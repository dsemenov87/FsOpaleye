module Column

open InternalColumn
module HPQ = InternalPrimQuery
module T = InternalPGTypes

/// A NULL of any type
let dbNull : Column<Nullable<'a>> = Column (HPQ.ConstExpr HPQ.NullLit)

let isDbNull : Column<Nullable<'a>> -> Column<T.PGBool> = C.unOp HPQ.OpIsNull