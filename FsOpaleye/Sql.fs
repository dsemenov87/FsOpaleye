module internal InternalSql

type NEL<'a> = FSharpx.Collections.NonEmptyList<'a>

type SqlTable = SqlTable of string
type SqlColumn = SqlColumn of string

/// A valid SQL name for a parameter.
type SqlName = string
type SqlOrderNulls = SqlNullsFirst | SqlNullsLast
type SqlOrderDirection = SqlAsc | SqlDesc

type SqlOrder = { sqlOrderDirection : SqlOrderDirection
                  sqlOrderNulls     : SqlOrderNulls }

/// Expressions in SQL statements.
type SqlExpr = ColumnSqlExpr  of SqlColumn
             | BinSqlExpr     of string * SqlExpr * SqlExpr
             | PrefixSqlExpr  of string * SqlExpr
             | PostfixSqlExpr of string * SqlExpr
             | FunSqlExpr     of string * SqlExpr list
             | AggrFunSqlExpr of string * SqlExpr list // Aggregate functions separate from normal functions.
             | ConstSqlExpr   of string
             | CaseSqlExpr    of ((SqlExpr * SqlExpr) list) * SqlExpr
             | ListSqlExpr    of SqlExpr list
             | ParamSqlExpr   of (SqlName option) * SqlExpr
             | PlaceHolderSqlExpr
             | ParensSqlExpr of SqlExpr
             | CastSqlExpr of string * SqlExpr
             | DefaultSqlExpr

/// Data type for SQL UPDATE statements.
type SqlUpdate  = SqlUpdate of SqlTable * (SqlColumn * SqlExpr) list * SqlExpr list

/// Data type for SQL DELETE statements.
type SqlDelete  = SqlDelete of SqlTable * SqlExpr list

/// Data type for SQL INSERT statements.
type SqlInsert  = SqlInsert of SqlTable * (SqlColumn list) * NEL<SqlExpr list>
