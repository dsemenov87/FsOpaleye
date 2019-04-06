/// Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
///                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
/// License     :  BSD-style

module internal Print

open InternalSql
module NEL = FSharpx.Collections.NonEmptyList
open PPrint
open FSharpx.Collections.Seq

/// Silliness to avoid "ORDER BY 1" etc. meaning order by the first column
/// Any identity function will do
let deliteral : SqlExpr -> SqlExpr = function
  | ColumnSqlExpr (SqlColumn _) as c -> c
  | expr -> FunSqlExpr ("COALESCE", [expr; expr])

/// If we wanted to make the SQL slightly more readable this would be
/// one easy place to do it.  Currently we wrap all column references
/// in double quotes in case they are keywords.  However, we should be
/// sure that any column names we generate ourselves are not keywords,
/// so we only need to double quote base table column names.
let ppColumn ((SqlColumn s) : SqlColumn) : Doc =
  dquotes (txt s)

/// Postgres treats upper case letters in table names as lower case,
/// unless the name is quoted!
let ppTable ((SqlTable s) : SqlTable) : Doc = dquotes (txt s)

let commaH (f : ('a -> Doc)) : 'a list -> Doc =
  hcat << punctuate comma << List.map f

let commaV (f : ('a -> Doc)) : 'a list -> Doc =
  vcat << punctuate comma << List.map f

let rec ppSqlExpr (expr : SqlExpr) : Doc =
  match expr with
  | ColumnSqlExpr c         -> ppColumn c
  | ParensSqlExpr e         -> parens (ppSqlExpr e)
  | BinSqlExpr (op, e1, e2) -> ppSqlExpr e1 <+> txt op <+> ppSqlExpr e2
  | PrefixSqlExpr (op, e)   -> txt op <+> ppSqlExpr e
  | PostfixSqlExpr (op, e)  -> ppSqlExpr e <+> txt op
  | FunSqlExpr (f, es)      -> txt f <^> parens (commaH ppSqlExpr es)
  | AggrFunSqlExpr (f, es)  -> txt f <^> parens (commaH ppSqlExpr es)
  | ConstSqlExpr c          -> txt c
  | CaseSqlExpr (cs, el)    ->
    let ppWhen (w,t) = txt "WHEN" <+> ppSqlExpr w
                                  <+> txt "THEN" <+> ppSqlExpr t
    in txt "CASE" <+> vcat (List.map ppWhen cs)
                            <+> txt "ELSE" <+> ppSqlExpr el <+> txt "END"
        
  | ListSqlExpr es          -> parens (commaH ppSqlExpr es)
  | ParamSqlExpr (_, v)     -> ppSqlExpr v
  | PlaceHolderSqlExpr      -> txt "?"
  | CastSqlExpr (typ, e)    -> txt "CAST" <^> parens (ppSqlExpr e <+> txt "AS" <+> txt typ)
  | DefaultSqlExpr          -> txt "DEFAULT"

let ppWhere : SqlExpr list -> Doc = function
  | [] -> empty
  | es -> txt "WHERE"
             <+> (hsep (intersperse (txt "AND")
                       (List.map (parens << ppSqlExpr) es)))

let ppGroupBy (es : SqlExpr list) : Doc =
  let ppGroupAttrs = commaV (ppSqlExpr << deliteral)
  in txt "GROUP BY" <+> ppGroupAttrs es

let ppSqlDirection (x : SqlOrder) : Doc =
  match x.sqlOrderDirection with
  | SqlAsc  -> "ASC"
  | SqlDesc -> "DESC"
  |> txt

// FIXME: We haven't implemented NULL ordering properly
let ppSqlNulls (_ : SqlOrder) : Doc = empty
//ppSqlNulls _ = empty
//ppSqlNulls x = text $ case Sql.sqlOrderNulls x of
//        Sql.SqlNullsFirst -> "NULLS FIRST"
//        Sql.SqlNullsLast  -> "NULLS LAST"

let ppOrderBy : (SqlExpr * SqlOrder) list -> Doc = function
  | [] -> empty
  | ord ->
    // Silliness to avoid "ORDER BY 1" etc. meaning order by the first column
    // Any identity function will do
    // ppOrd (e,o) = ppSqlExpr e <+> ppSqlDirection o <+> ppSqlNulls o
    let ppOrd (e,o) = ppSqlExpr (deliteral e)
                      <+> ppSqlDirection o
                      <+> ppSqlNulls o
    in txt "ORDER BY" <+> commaV ppOrd ord

let ppAs (alias : string) (expr : Doc) : Doc =
  match (alias, expr) with
  | (null, alias) -> expr
  | _             -> expr <+> hsep [txt "as"; dquotes (txt alias)]
    
let ppUpdate (SqlUpdate (table, assigns, criteria) : SqlUpdate) : Doc =
  let ppAssign (c,e) = ppColumn c <+> equals <+> ppSqlExpr e
  in txt "UPDATE" <+> ppTable table
        <..> txt "SET" <+> commaV ppAssign assigns
        <..> ppWhere criteria
    
let ppDelete (SqlDelete (table, criteria) : SqlDelete) : Doc =
    txt "DELETE FROM" <+> ppTable table <..> ppWhere criteria    

let ppInsert (SqlInsert (table, names, values) : SqlInsert) : Doc =
    txt "INSERT INTO" <+> ppTable table
      <+> parens (commaV ppColumn names)
      <..> txt "VALUES" <+> commaV (fun v -> parens (commaV ppSqlExpr v))
                                  (NEL.toList values)
