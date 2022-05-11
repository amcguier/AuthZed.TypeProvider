namespace AuthZed.TypeProvider

open System
open System.Reflection
open FSharp.Core.CompilerServices
open FSharp.Quotations
open ProviderImplementation
open ProviderImplementation.ProvidedTypes


open System.IO

[<AutoOpen>]
module Providers =
    [<TypeProvider>]
    type AuthZedSchemaTypeProvider(config : TypeProviderConfig) as this =
        inherit TypeProviderForNamespaces(config,assemblyReplacementMap = ["AuthZed.TypeProvider.DesignTime","AuthZed.TypeProvider"; ], addDefaultProbingLocation = true)

            
        let assembly = Assembly.GetExecutingAssembly()

        let ns = "AuthZed.TypeProvider"

        
        let providerDefinition = ProvidedTypeDefinition(assembly,ns,"AuthZedSchemaProvider", Some typeof<obj>, hideObjectMethods=true, nonNullable=true, isErased = true)
        let parameters =
            [ ProvidedStaticParameter("SchemaText", typeof<string>, parameterDefaultValue = "")
              ProvidedStaticParameter("SchemaFile", typeof<string>, parameterDefaultValue = "")
              ProvidedStaticParameter("EncodeIds", typeof<bool>, parameterDefaultValue = true)
            ]
        let helpText =
            """<summary>Typed Representation of a AuthZed Schema</summary>
               <param name='SchemaText'>The schema to generate</param>
               <param name='SchemaFile'>The path to a file containing the AuthZed schema</param>
            """

        let generateTypesFromParams typeName (args : obj []) =
            let schemaText = args.[0] :?> string
            let schemaFile = args.[1] :?> string
            let encodeIds = args.[2] :?> bool
            if schemaFile.Length = 0 && schemaText.Length = 0 then
                failwith "You must provide either schema text or a file with your authzed schema" 
                        

            let definition = ProvidedTypeDefinition(assembly, ns, typeName, None, isErased = true)
            let types  = GenerateTypes.generate definition assembly ns schemaText encodeIds

            definition
            
        do providerDefinition.AddXmlDoc helpText
        do providerDefinition.DefineStaticParameters(parameters, generateTypesFromParams)
        do this.AddNamespace(ns,[providerDefinition])
        

[<TypeProviderAssembly>]
do ()
