/// Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
///                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
/// License     :  BSD-style

module internal InternalSqlDefault

open InternalPrimQuery
module PQ = InternalPrimQuery
open InternalSql
open InternalGenerate

module Sql = InternalSql
let tagWith = InternalTag.tagWith
type ByteString = FSharpx.Collections.ByteString
type Base16 = SimpleBase.Base16
type NEL<'a> = FSharpx.Collections.NonEmptyList<'a>
module NEL = FSharpx.Collections.NonEmptyList

//let mkSqlGenerator (gen : SqlGenerator) : SqlGenerator =
//    {
//     sqlUpdate      ->defaultSqlUpdate      gen
//     sqlDelete      ->defaultSqlDelete      gen
//     sqlInsert      ->defaultSqlInsert      gen
//     sqlExpr        ->defaultSqlExpr        gen
//     sqlLiteral     ->defaultSqlLiteral     gen
//     sqlQuote       ->defaultSqlQuote       gen
//    }

//defaultSqlGenerator :: SqlGenerator
//let defaultSqlGenerator ->mkSqlGenerator defaultSqlGenerator


let toSqlOrder (gen : SqlGenerator) ((OrderExpr (o, e)) : OrderExpr) : SqlExpr * SqlOrder =
  let o'= match o.orderDirection with
            | PQ.OpAsc  -> Sql.SqlAsc
            | PQ.OpDesc -> Sql.SqlDesc
  let orderNulls' = match o.orderNulls with
                    | PQ.NullsFirst -> Sql.SqlNullsFirst
                    | PQ.NullsLast  -> Sql.SqlNullsLast
  in
  (gen.sqlExpr e, { sqlOrderDirection  = o'
                    sqlOrderNulls      = orderNulls' }) 

let toSqlColumn (attr : Attribute) : SqlColumn = SqlColumn attr

let toSqlAssoc (gen : SqlGenerator) : Assoc -> (SqlColumn * SqlExpr) list =
  List.map (fun (attr, expr) -> (toSqlColumn attr, gen.sqlExpr expr))

let defaultSqlUpdate (gen : SqlGenerator)
                 (name : TableName)         /// Name of the table to update.
                 (criteria : PrimExpr list) /// Conditions which must all be true for a row
                                            /// to be updated.
                 (assigns : Assoc)          /// Update the data with this.
                 : SqlUpdate =
                 SqlUpdate ((SqlTable name), (toSqlAssoc gen assigns), (List.map gen.sqlExpr criteria))


let defaultSqlInsert (gen : SqlGenerator)
                 (name : TableName)
                 (attrs : Attribute list)
                 (exprs : NEL<PrimExpr list>)
                 : SqlInsert =
                 SqlInsert ((SqlTable name), (List.map toSqlColumn attrs), ((NEL.map (List.map gen.sqlExpr)) exprs))

let defaultSqlDelete (gen : SqlGenerator)
                 (name : TableName)         /// Name of the table
                 (criteria : PrimExpr list) /// Criteria which must all be true for a row
                                            /// to be deleted.
                 : SqlDelete =
                 SqlDelete ((SqlTable name), (List.map gen.sqlExpr criteria))

let showBinOp : BinOp -> string = function
  | OpEq         -> "="
  | OpLt         -> "<"
  | OpLtEq       -> "<="
  | OpGt         -> ">"
  | OpGtEq       -> ">="
  | OpNotEq      -> "<>"
  | OpAnd        -> "AND"
  | OpOr         -> "OR"
  | OpLike       -> "LIKE"
  | OpIn         -> "IN"
  | (OpOther s)  -> s
  | OpCat        -> "||"
  | OpPlus       -> "+"
  | OpMinus      -> "-"
  | OpMul        -> "*"
  | OpDiv        -> "/"
  | OpMod        -> "MOD"
  | OpBitNot     -> "~"
  | OpBitAnd     -> "&"
  | OpBitOr      -> "|"
  | OpBitXor     -> "^"
  | OpAsg        -> "->"

/// Escape characters that need escaping
/// FIXME: Escaping control characters probably doesn't work in SQLite
/// Need more tests
let escape : char -> string = function
  | '\000'->"\\0"
  | '\''  ->"''"
  | '"'   ->"\\\""
  | '\b'  ->"\\b"
  | '\n'  ->"\\n"
  | '\r'  ->"\\r"
  | '\t'  ->"\\t"
  | '\\'  ->"\\\\"
  | c     ->c.ToString()

let quote (s : string) : string =
  sprintf "'%s'" (s.ToCharArray() |> Array.map escape |> String.concat "")

type UnOpType = UnOpFun | UnOpPrefix | UnOpPostfix

let sqlUnOp : UnOp -> string * UnOpType = function
 | OpNot         -> ("NOT"        , UnOpPrefix)
 | OpIsNull      -> ("IS NULL"    , UnOpPostfix)
 | OpIsNotNull   -> ("IS NOT NULL", UnOpPostfix)
 | OpLength      -> ("LENGTH"     , UnOpFun)
 | OpAbs         -> ("ABS"        , UnOpFun)
 | OpNegate      -> ("-"          , UnOpFun)
 | OpLower       -> ("LOWER"      , UnOpFun)
 | OpUpper       -> ("UPPER"      , UnOpFun)
 | (UnOpOther s) -> (s            , UnOpFun)

let showAggrOp : AggrOp -> string = function
 | AggrCount          -> "COUNT"
 | AggrSum            -> "SUM"
 | AggrAvg            -> "AVG"
 | AggrMin            -> "MIN"
 | AggrMax            -> "MAX"
 | AggrStdDev         -> "StdDev"
 | AggrStdDevP        -> "StdDevP"
 | AggrVar            -> "Var"
 | AggrVarP           -> "VarP"
 | AggrBoolAnd        -> "BOOL_AND"
 | AggrBoolOr         -> "BOOL_OR"
 | AggrArr            -> "ARRAY_AGG"
 | (AggrStringAggr _) -> "STRING_AGG"
 | (AggrOther s)      -> s

let defaultSqlExpr (gen : SqlGenerator) (expr : PrimExpr) : SqlExpr =
  match expr with
  | AttrExpr (Symbol (a, t)) -> ColumnSqlExpr (SqlColumn (tagWith t a))
  | BaseTableAttrExpr a -> ColumnSqlExpr (SqlColumn a)
  | BinExpr (op, e1, e2) ->
    let leftE = gen.sqlExpr e1
    let rightE = gen.sqlExpr e2
    let paren = ParensSqlExpr
    let (expL, expR) =
      match (op, e1, e2) with
      | (OpAnd, BinExpr (OpOr, _, _), BinExpr (OpOr, _, _)) ->
        (paren leftE, paren rightE)
      | (OpOr, BinExpr (OpAnd, _, _), BinExpr (OpAnd, _, _)) ->
        (paren leftE, paren rightE)
      | (OpAnd, BinExpr (OpOr, _, _), _) ->
        (paren leftE, rightE)
      | (OpAnd, _, BinExpr (OpOr, _, _)) ->
        (leftE, paren rightE)
      | (OpOr, BinExpr (OpAnd, _, _), _) ->
        (paren leftE, rightE)
      | (OpOr, _, BinExpr (OpAnd, _, _)) ->
        (leftE, paren rightE)
      | (_, ConstExpr _, ConstExpr _) ->
        (leftE, rightE)
      | (_, _, ConstExpr _) ->
        (paren leftE, rightE)
      | (_, ConstExpr _, _) ->
        (leftE, paren rightE)
      | _ -> (paren leftE, paren rightE)
    in BinSqlExpr ((showBinOp op), expL, expR)

  | UnExpr (op, e) ->
    let (op',t) = sqlUnOp op
    let e' = gen.sqlExpr e
                        in match t with
                          | UnOpFun     -> FunSqlExpr (op', [e'])
                          | UnOpPrefix  -> PrefixSqlExpr (op', ParensSqlExpr e')
                          | UnOpPostfix -> PostfixSqlExpr (op', e')

//   TODO: The current arrangement whereby the delimeter parameter
//   of string_agg is in the AggrStringAggr constructor, but the
//   parameter being aggregated is not, seems unsatisfactory
//   because it leads to a non-uniformity of treatment, as seen
//   below.  Perhaps we should have just `AggrExpr AggrOp` and
//   always put the `PrimExpr` in the `AggrOp`.

  | AggrExpr (op, e) -> let op' = showAggrOp op
                        let e'  = gen.sqlExpr e
                        let moreAggrFunParams =
                          match op with
                            | AggrStringAggr primE -> [gen.sqlExpr primE]
                            | _ -> []
                        in AggrFunSqlExpr (op', e' :: moreAggrFunParams)
  | ConstExpr l      -> ConstSqlExpr (gen.sqlLiteral l)
  | CaseExpr (cs, e) -> let cs' = cs |> List.map (fun (c, x) -> (gen.sqlExpr c, gen.sqlExpr x))
                        let e'  = gen.sqlExpr e
                        in CaseSqlExpr (cs', e')
  | ListExpr es      -> ListSqlExpr (List.map gen.sqlExpr es)
  | ParamExpr (n, _) -> ParamSqlExpr (n, PlaceHolderSqlExpr)
  | FunExpr (n, ex)  -> FunSqlExpr (n, (List.map gen.sqlExpr ex))
  | CastExpr (typ, e1) -> CastSqlExpr (typ, (gen.sqlExpr e1))
  | DefaultInsertExpr -> DefaultSqlExpr

/// Quote binary literals using Postgresql's hex format.
let binQuote (s : ByteString) : string =
  sprintf "E'\\\\x%s'" (Base16.EncodeUpper s.Array)

let defaultSqlLiteral (_ : SqlGenerator) (l : Literal) : string =
    match l with
    | NullLit       -> "NULL"
    | DefaultLit    -> "DEFAULT"
    | BoolLit true  -> "1"
    | BoolLit false -> "0"
    | ByteStringLit s
                    -> binQuote s
    | StringLit s   -> quote s
    | IntegerLit i  -> i.ToString()
    | DoubleLit d   -> d.ToString()
    | OtherLit o    -> o

let defaultSqlQuote (_ : SqlGenerator) (s : string) = quote s
