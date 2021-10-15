namespace AuthZed.TypeProvider.Tests

open System
open Expecto
open AuthZed.TypeProvider
open AuthZed.TypeProvider.Say

module SayTests =
    [<Tests>]
    let tests =
        testList "samples" [
            testCase "Add two integers" <| fun _ ->
                let subject = Say.add 1 2
                Expect.equal subject 3 "Addition works"
            testCase "Say nothing" <| fun _ ->
                let subject = Say.nothing ()
                Expect.equal subject () "Not an absolute unit"
            testCase "Say hello all" <| fun _ ->
                let person = {
                    Name = "Jean-Luc Picard"
                    FavoriteNumber = 4
                    FavoriteColor = Red
                    DateOfBirth = DateTimeOffset.Parse("July 13, 2305")
                }
                let subject = Say.helloPerson person
                Expect.equal subject "Hello Jean-Luc Picard. You were born on 2305/07/13 and your favorite number is 4. You like Red." "You didn't say hello" ]


module ParserTests =
    open FParsec


    let isOk msg x = Expect.isOk x msg
    
    let lineComment =
        testCase "only line comment" <| (fun _ ->
                AuthZed.TypeProvider.Parser.parse "// here's a comment"                                         
                |> isOk "multiline comments should parse"                    
                )

    let blockComment =
        testCase "one line block comment" <| (fun _ ->
            AuthZed.TypeProvider.Parser.parse "/* Here's a block comment on one line */"
            |> isOk "block comments should parse"                    
                )
    let multilineComment =
        testCase "multiline block comment" <| (fun _ ->
            AuthZed.TypeProvider.Parser.parse "/*  here's a long comment \n \n // embedded other comment \n \n */"
            |> isOk "Multi line comment should be a comment")

    let emptyResource =
        testCase "empty resource block parses" <| fun _ ->
            AuthZed.TypeProvider.Parser.parse "definition namespace/resourcename { }"
            |> isOk "basic definition should parse"


    let fullExample =
        """
/** user represents a user */
definition sysprefix/user {}

/** group represents a group **/
definition sysprefix/group {
    /** member is a member of a group, which can be a user or the membership of another group */
    relation member: sysprefix/user | sysprefix/group#member
}

/** document represents a document */
definition sysprefix/document {
    /** writer is a writer of the document */
    relation writer: sysprefix/user | sysprefix/group#member

    /** reader is a reader of the document */
    relation reader: sysprefix/user | sysprefix/group#member

    /** write indicates which user can write to the document */
    permission write = writer

    /** read indicates which user can read the document */
    permission read = reader + write
    
    permission exclude1 = reader - writer
    
    permission intersect = reader & writer

    permission arrIt = writer->member
}
        """

    let fullParse =
        testCase "full example should parse" <| fun _ ->
            AuthZed.TypeProvider.Parser.parse fullExample
            |> isOk "full example should parse"
        
    [<Tests>]
    let tests =
        testList "parsers" [
            lineComment
            blockComment
            multilineComment
            emptyResource
            fullParse
        ]
