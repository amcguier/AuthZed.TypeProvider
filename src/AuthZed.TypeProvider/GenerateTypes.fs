namespace AuthZed.TypeProvider

open System
open System.Collections.Generic
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Grpc.Core
open FSharp.Control.Tasks
open System.Reflection

module Types =
    open Authzed.Api.V1
    
    type BaseResource(prefix,typename, id) =
        member _.Id with get() = id
        member _.TypeName with get() = typename
        member _.Prefix with get() = prefix
        override _.ToString() = $"{prefix}/{typename} - {id}"


    type Context(schemaString, connectionString, token) =
        let client = AuthZed.Api.AuthZedContext(connectionString,token)
        let sClient = lazy client.GetSchemaClient()
        let callOptions =
            let headers = Metadata()
            headers.Add("authorization", token)
            Grpc.Core.CallOptions(headers = headers)

        member _.ApplySchema () =
            let writeSchemaRequest = WriteSchemaRequest()
            writeSchemaRequest.Schema <- schemaString
            task {
                try
                  let! schemaResult = sClient.Value.WriteSchemaAsync(writeSchemaRequest).ResponseAsync
                  return Ok(())
                with
                | ex -> return Error(ex.Message)
            }

            
module internal GenerateTypes =
    open Grammer
    open Types
            

    let generate (tp : ProvidedTypeDefinition) assembly nspace sourceString  =
        let addResource (namespaces : Map<string,ProvidedTypeDefinition>)(r :Schema.Resource) =
            //unpack the resource to pass it to the quotation
            let prefix = r.prefix
            let typeName : string = r.typeName


            let rootType =
                namespaces.TryFind prefix
                |> Option.defaultWith (fun () -> (ProvidedTypeDefinition(assembly, nspace, prefix, None, isErased = true)))

            let definition = ProvidedTypeDefinition(assembly, r.prefix, r.typeName, Some typeof<BaseResource>, isErased = true)

            let ctor = ProvidedConstructor([ProvidedParameter("id", typeof<string>)], fun args -> <@@ let id = (%%args.[0] : string)
                                                                                                      BaseResource(prefix,typeName,id)
                                                                                                     @@>)
            let ctorInfo = typeof<BaseResource>.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance,null,[|typeof<string>;typeof<string>;typeof<string>|], null)

            ctor.BaseConstructorCall <- fun args ->
                let fullArgs = [ <@@ prefix @@>; <@@ typeName @@>; args.[0] ]
                ctorInfo, fullArgs
                
            definition.AddMember(ctor)
            definition.AddMember(ProvidedProperty("Id", typeof<string>, getterCode = fun args -> <@@ ((%%args.[0]) : BaseResource).Id @@>))
            rootType.AddMember(definition)
            namespaces.Add(prefix, rootType)
            

        // add static method for constructing the context
        let getContext = ProvidedMethod("getContext",
                                        [ProvidedParameter("connectionString", typeof<string>); ProvidedParameter("accesstoken",typeof<string>)],
                                        typeof<Context>,
                                        isStatic = true,
                                        invokeCode = fun args -> <@@ Context(sourceString, (%%args.[0] : string), (%%args.[1] : string)) @@>)
            
                                            
        tp.AddMember(getContext)
        

        let parseResult =
            Parser.parse sourceString
            |> function | Ok(r) -> r
                        | Error(s) -> failwith s


        
        
        parseResult
        |> List.map
            (function | AST.Comment _ -> None
                      | AST.Definition (r, _) ->  r |> Some )
        |> List.choose id
        |> List.fold (fun mp r -> addResource mp r) Map.empty
        |> Map.iter (fun _ t -> tp.AddMember t)


        

