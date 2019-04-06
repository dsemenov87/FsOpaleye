module PGTypes

open FSharpPlus

open InternalColumn
module HPQ = InternalPrimQuery
module IPT = InternalPGTypes
let quote = InternalSqlDefault.quote

/// SQLite needs to be told that numeric literals without decimal
/// points are actual REAL
let private pgDouble (x : double) : Column<'PGFloat8> =
  unsafeCast "REAL" ^ IPT.literalColumn ^ HPQ.DoubleLit x

module PGBool =
  let pgFromInteger num = pgDouble ^ fromBigInt num



