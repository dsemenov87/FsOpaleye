module internal InternalGenerate

open InternalPrimQuery
open InternalSql

type NEL<'a> = FSharpx.Collections.NonEmptyList<'a>

type SqlGenerator = 
    {
     sqlUpdate      : TableName -> PrimExpr list -> Assoc -> SqlUpdate
     sqlDelete      : TableName -> PrimExpr list -> SqlDelete
     sqlInsert      : TableName -> Attribute list -> NEL<PrimExpr list> -> SqlInsert
     sqlExpr        : PrimExpr -> SqlExpr
     sqlLiteral     : Literal -> string
     /// Turn a string into a quoted string. Quote characters
     /// and any escaping are handled by this function.
     sqlQuote       : string -> string
    }

