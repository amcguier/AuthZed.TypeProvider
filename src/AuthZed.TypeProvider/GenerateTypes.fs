namespace AuthZed.TypeProvider

open System
open System.Collections.Generic
open System.Text
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Grpc.Core
open FSharp.Control.Tasks
open System.Reflection

module Types =
    open Authzed.Api.V1
    
    type BaseResource(prefix,typename, id : string, ?memberName : string ) =
        member _.Id with get() = id
        member _.TypeName with get() = typename
        member _.Prefix with get() = prefix
        member _.Member with get() = memberName
        member this.Format () = $"{this.Prefix}/{this.TypeName}"
        override _.ToString() = $"{prefix}/{typename} - {id}"

    type BaseRelation(name: string, obj : BaseResource, subject : BaseResource) =
        member _.Name with get() = name
        member _.Subject with get() = subject
        member _.Object with get() = obj
        override _.ToString() = $"{obj} to {subject}"    

    type BasePermissionCheck(relation : BaseRelation, permission : string) =
        member _.Permission with get() = permission
        member _.Relation with get()  = relation
        
    let encode64 (toEncode : string) =
        let bytes = Encoding.UTF8.GetBytes(toEncode)
        let base64Raw = Convert.ToBase64String(Encoding.UTF8.GetBytes(toEncode)).ToCharArray()
        seq {
            for c in base64Raw do
              match c with
              | '+' -> yield '-'
              | '/' -> yield '_'
              | '=' -> ()
              | x -> yield x
            }
        |> Array.ofSeq
        |> fun chars -> new String(chars)

    let decode64 (toDecode : string) =
        let pad (s : StringBuilder) =
            match s.Length % 4 with
                | 1 -> s.Append("===").ToString()
                | 2 -> s.Append("==").ToString()
                | 3 -> s.Append("=").ToString()
                | _ -> s.ToString()

        seq {
            for c in toDecode.ToCharArray() do
                match c with
                | '-' -> '+'
                | '_' -> '/'
                | x -> x
            }
        |> Array.ofSeq
        |> fun chars ->
            let sb = new StringBuilder()
            sb.Append(chars)
        |> pad
        |> Convert.FromBase64String
        |> Encoding.UTF8.GetString
    
    let private resourceToObject encoder (reference : BaseResource) =        
      new ObjectReference(ObjectId = (encoder reference.Id), ObjectType = reference.Format())

    let private resourceToSubject encoder (reference : BaseResource) =
        let obj = resourceToObject encoder reference
        let sub = new SubjectReference(Object = obj)
        match reference.Member with
            | Some(s) -> sub.OptionalRelation <- s
            | _ -> ()
        sub

    let private relationToRelationshipUpdate (encoder,decoder) (o : RelationshipUpdate.Types.Operation) (relation : BaseRelation) =
        let subj = resourceToSubject encoder relation.Subject
        let obj = resourceToObject encoder relation.Object
        let r = Relationship(Subject = subj, Resource = obj, Relation = relation.Name)
        new RelationshipUpdate(Operation = o, Relationship = r)        
        

    type Context(schemaString, connectionString, token, encodeKeys) =
        let client = AuthZedClient.V1.Context(connectionString,token)
        let encoders = if encodeKeys then (encode64,decode64) else (id,id)
        let sClient = lazy client.GetSchemaClient()
            
        let callOptions =
            let headers = Metadata()
            headers.Add("authorization", $"bearer {token}")
            Grpc.Core.CallOptions(headers = headers)            

        let relationshipUpdate req =
            task {
                try
                  let! resp = client.GetPermissionClient().WriteRelationshipsAsync(req,callOptions).ResponseAsync
                  return Ok <| resp
                with
                  | ex -> return Error(ex.Message)
            }

        member public _.ApplySchema str =
            let writeSchemaRequest = WriteSchemaRequest(Schema = str)
            writeSchemaRequest.Schema <- schemaString
            task {
                try
                  let! schemaResult = sClient.Value.WriteSchemaAsync(writeSchemaRequest, callOptions).ResponseAsync
                  return Ok(())
                with
                | ex -> return Error(ex.Message)
            }

        member public _.AddRelations (relations : BaseRelation seq) =
            let updates = Seq.map (relationToRelationshipUpdate encoders RelationshipUpdate.Types.Operation.Touch) relations
            let req = WriteRelationshipsRequest()
            req.Updates.AddRange(updates)
            relationshipUpdate req


        member public _.RemoveRelations (relations: BaseRelation seq) =
            let updates = Seq.map (relationToRelationshipUpdate encoders RelationshipUpdate.Types.Operation.Delete) relations
            let req = WriteRelationshipsRequest()
            req.Updates.AddRange(updates)
            relationshipUpdate req

            
module internal GenerateTypes =
    open Grammer
    open Types
    open Schema

    let formatResource r = $"{r.prefix}/{r.typeName}"

    (**
        ## Process source data

        First parse the data, then strip out the comments and the parser tokens
        ultimately we want a Map from resource to a combined list of
        relations and permissions
       **)        
    let processSourceData data =
        let parseResult =
            Parser.parse data
            |> function | Ok(r) -> r
                        | Error(s) -> failwith s

        // split out the different resource types and ignore the comments
        let splitTokens =
            List.fold (fun (rel , perm) tok ->
                match tok with
                | Token.Permission p -> (rel, p::perm)
                | Token.Relation r -> (r::rel, perm)
                | Token.Comment _ -> (rel,perm)) ([],[])
            >> function | (rel,perm) -> (List.rev rel, List.rev perm)

        parseResult
        |> List.map (function | Comment _ -> None
                              | Definition (r , tk) -> let (rel,perm) = splitTokens tk
                                                       (formatResource r , (r,rel,perm)) |> Some)
        |> List.choose id
        |> Map.ofList
        
    (**
        ## Add definition
        We need to generate all the definitions before we can create any of the resource/permission types since they refer to each other.
        We collect a map that contains the namespaces (which are ultimately added to the top level type) and we need to keep a map of the individual types
        for reference once we start building the permission structure
              **)
    let private addDefinition assembly nspace ((namespaces : Map<string,ProvidedTypeDefinition>), definitions : Map<string,ProvidedTypeDefinition>) (r :Schema.Resource) =

        //unpack the resource to pass it to the quotation. They can only accept primatives/strings
        let prefix = r.prefix
        let typeName : string = r.typeName


        // either the existing namespace or make a type to represent the namespace from the schema
        let rootType =
            namespaces.TryFind prefix
            |> Option.defaultWith (fun () -> (ProvidedTypeDefinition(assembly, nspace, prefix, None, isErased = true)))


        // define our type
        let definition = ProvidedTypeDefinition(assembly, r.prefix, r.typeName, Some typeof<BaseResource>, isErased = true)

        // We need to be able to accept an Id in from the user for the resources
        // TODO allow an option to automatically turn the passed id into/out of hex to avoid spice limitations
        let ctor = ProvidedConstructor([ProvidedParameter("id", typeof<string>)], fun args -> <@@ let id = (%%args.[0] : string)
                                                                                                  BaseResource(prefix,typeName,id)
                                                                                                 @@>)
        let ctorInfo = typeof<BaseResource>.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance,null,[|typeof<string>;typeof<string>;typeof<string>|], null)
        ctor.BaseConstructorCall <- (fun args ->
            let fullArgs = [ <@@ prefix @@>; <@@ typeName @@>; args.[0] ]
            ctorInfo, fullArgs)
        definition.AddMember(ctor)

        // The inherited type should have a getter for Id, but we hide the base class data since it's not relevant to the user
        definition.AddMember(ProvidedProperty("Id", typeof<string>, getterCode = fun args -> <@@ ((%%args.[0]) : BaseResource).Id @@>))
        rootType.AddMember(definition)

        // add our types to the 
        (namespaces.Add(prefix, rootType), definitions.Add($"{prefix}/{typeName}", definition))


    let rec findBaseResources (data : Map<string, Schema.Resource * Schema.Relation list *  _ >)  (resource : Schema.Resource) =        
        match resource.memberName with
            | None -> [resource]
            | Some(memName) ->
                let (_ ,relations, _) = data.[formatResource resource]
                let relation = List.find (fun (r : Schema.Relation) ->  r.name = memName) relations
                relation.allowedResources
                |> List.map (findBaseResources data)
                |> List.collect id
        

    let upperWord =
        function | null -> ""
                 | "" -> ""
                 | s -> System.String.Concat(s.[0].ToString().ToUpper(), s.Substring(1))

    (**
        ##Add Relation
        Add the relations for each definition. A relation can refer to another type, or another member
                                   **)
    let addRelation assembly nspace (types : Map<string,ProvidedTypeDefinition>) (data : Map<string, Schema.Resource * Schema.Relation list *  _ >) r (relations : Schema.Relation list) =        
        let mapResourceToMethod (nm : string) (obj : Schema.Resource) (subj : Schema.Resource) =
            // we need to change the method name so it's a valid .net method name
            let formattedNm =
                nm.Split([|'_'|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map upperWord
                |> fun x -> String.Join("_" ,x)
            
            let subName =
                subj.typeName.Split([|'_'|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map upperWord
                |> fun x -> String.Join("_" ,x)

            
            let methodName = $"{formattedNm}RelationFrom{subName}"
            let subType = types.[formatResource subj]
            let m = ProvidedMethod(methodName,
                                   [ProvidedParameter("subject",subType)],
                                   typeof<BaseRelation>,
                                   (fun args -> <@@
                                                    //the containing object
                                                    let self = (%%args.[0] : BaseResource)
                                                    // the parameter object
                                                    let r = (%%args.[1] : BaseResource)
                                                    BaseRelation(nm, self,r) @@>),
                                   false)
            m :> MemberInfo
            
        let mapResourceToBaseMethods nm (obj : Schema.Resource) (resources : Schema.Resource list) =
            resources
            |> List.map (findBaseResources data)
            |> List.collect id
            |> List.distinct
            |> List.map (mapResourceToMethod nm obj)
        
        // we need to generate a type for each relation
        let baseType = types.[formatResource r]

        relations
        |> List.map (fun (rel : Schema.Relation) ->  mapResourceToBaseMethods rel.name r rel.allowedResources)
        |> List.collect id
        |> List.iter (fun m -> baseType.AddMember(m))

        
    let generate (tp : ProvidedTypeDefinition) assembly nspace sourceString encodeIds =

        // add static method for constructing the context
        let getContext = ProvidedMethod("GetContext",
                                        [ProvidedParameter("connectionString", typeof<string>); ProvidedParameter("accesstoken",typeof<string>)],
                                        typeof<Context>,
                                        isStatic = true,
                                        invokeCode = fun args -> <@@ new Context(sourceString, (%%args.[0] : string), (%%args.[1] : string), encodeIds) @@>)
                                                        
        tp.AddMember(getContext)
       
        // first parse the schema 
        let data = processSourceData sourceString

        // generate the types for the definitions
        let (namespaces,typesDefinitions) =
            Map.fold (fun mp _ (r,_,_) -> addDefinition assembly nspace mp r)  (Map.empty, Map.empty) data

        let addR = addRelation assembly nspace typesDefinitions data
        Map.iter (fun _ (res,rels,_) -> addR res rels) data

        // add the namespaces to our global type namespace
        namespaces |> Map.iter (fun _ t -> tp.AddMember t)
