namespace Authzed.TypeProvider


module Schema =

    type Resource =
        {
            prefix : string
            typeName : string
            memberName : string option
        }
    
    type Relation = {
        name : string
        allowedResources : Resource list
        }

    type PermissionOperator =
        | Union
        | Intersection
        | Exclusion
        | Arrow

    type PermissionRelation = PermissionOperator * string

    type Permission = {
        name : string
        relationName : string
        additionalRelations : PermissionRelation list
        }
        
    type definition = {
        resource : Resource
        relations : Relation list
        permissions : Permission list
        }

    type Comment = string

module Grammer =
    open FParsec
    open FParsec.Pipes
    open Schema

        
    type Token =
        | Comment of Schema.Comment
        | Permission of Schema.Permission
        | Relation of Schema.Relation

    type AST =
        | Comment of Schema.Comment
        | Definition of Schema.Resource * Token list


    // comments

    let blockComment = between (%"/*") (%"*/") (charsTillString "*/" false System.Int32.MaxValue) |>> Token.Comment 

    let lineComment = skipString "//" >>. (restOfLine true)  |>> Token.Comment

    let comment = blockComment <|> lineComment


    // resources and names
    
    let resourceRegex = "[A-z0-9_]+"
    let resourceName = regex resourceRegex
    
    let plainResource =
        pipe2
          (resourceName .>> %'/')
          (resourceName)
          (fun nmspace typeId ->
           { prefix = nmspace
             typeName = typeId
             memberName = None
           })

    let fullResource =
        pipe3
          (resourceName .>> %'/')
          resourceName
          (opt (%'#' >>. resourceName))
          (fun nmspace typeId memberId ->
           { prefix = nmspace
             typeName = typeId
             memberName = memberId
           })
           


    // relation entry
          
    let multipleResources =
        let resource = spaces >>. fullResource .>> spaces
        let additional = many (spaces >>. %'|' >>. spaces >>. fullResource .>> spaces)
        resource .>>. additional |>> fun (r,remain) -> r::remain
        
    let relation =
        let nameParser = %"relation" >>. spaces >>. resourceName
        let manyResources = %':' >>. spaces  >>. multipleResources
        nameParser .>>. manyResources |>> fun (name,resources) -> Relation { name = name ; allowedResources = resources }



    // permission entry
        
    let operator =
        (%'+' >>% PermissionOperator.Union)
        <|> (%'&' >>% PermissionOperator.Intersection)
        <|> (%"->" >>% PermissionOperator.Arrow)
        <|> (%'-' >>% PermissionOperator.Exclusion)

    let manyPermissions =
        let op = (spaces >>. operator .>> spaces)
        let rs = op .>>. resourceName .>> spaces |>> (fun tpl -> tpl : PermissionRelation)
        many rs
        
    let permission =
        let nameParser = %"permission" >>. spaces1 >>. (resourceName) .>> spaces1
        let resourceParser = %'=' >>. spaces1 >>. (resourceName) .>> spaces                
        pipe3
          nameParser
          resourceParser
          manyPermissions
          (fun nm resourceName additional -> Permission { name = nm; relationName = resourceName; additionalRelations = additional}
          )

    // Declaration
          
    let content =
        choice [permission; relation; comment]
        
    let declaration =
        let nameParser = %"definition" >>. spaces1 >>. plainResource .>> spaces .>> %'{' .>> spaces
        let contents = many (spaces >>. content .>> spaces)
        let finish = %'}' >>. spaces
        nameParser .>>. contents .>> finish |>> fun (resource, contents) -> Definition (resource,contents)


    // schema file


    let schema =        
        let commentParser =
            comment >>= (function | Token.Comment(s) -> preturn (AST.Comment(s)) | x -> fail $"Top level may only be comments or declaration you provided {x}")
        let fileContent = commentParser <|> declaration
        many1 (spaces >>. fileContent .>> spaces)

    
module Parser =
    open FParsec
    open FParsec.Primitives
    open FParsec.Pipes
    

    let parse = run Grammer.schema
        






    
