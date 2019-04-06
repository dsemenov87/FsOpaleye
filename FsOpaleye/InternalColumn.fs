module internal InternalColumn

module HPQ = InternalPrimQuery

type Column<'a> = Column of HPQ.PrimExpr

type Nullable<'a> = Nullable of 'a

let unColumn (Column e : Column<'a>) : HPQ.PrimExpr = e

let unsafeCoerceColumn (Column e : Column<'a>) : Column<'b> = Column e

let binOp (op : HPQ.BinOp) (Column e) (Column e') : Column<'c> = Column (HPQ.BinExpr (op, e, e'))

let unOp (op : HPQ.UnOp) (Column e) : Column<'b> = Column (HPQ.UnExpr (op, e))

/// For import order reasons we can't make the return type PGBool
let unsafeCase (alts : (Column<'pgBool> * Column<'a>) list) (Column otherwise : Column<'a>) : Column<'a> =
  let unColumns = List.map (fun (Column e, Column e') -> (e, e'))
  in Column (HPQ.CaseExpr ((unColumns alts), otherwise))

let unsafeIfThenElse (cond : Column<'pgBool>) (t : Column<'a>) (f : Column<'a>) : Column<'a> =
  unsafeCase [(cond, t)] f

let unsafeGt (c : Column<'a>) : Column<'a> -> Column<'pgBool> = binOp HPQ.OpGt c

let unsafeEq (c : Column<'a>) : Column<'a> -> Column<'pgBool> = binOp HPQ.OpEq c

let unsafeCast (n : string) : Column<'a> -> Column<'b> =
  let cast n e = HPQ.CastExpr (n, e)
  let mapColumn primExpr = Column << primExpr << unColumn
  let call = mapColumn << cast
  call n

type IDbNum<'num> =
  abstract member FromInt32 : int32 -> ^num

type IDbFractional<'num> =
  inherit IDbNum<'num>
  abstract member FromRational : single -> ^num

module Operations =

  let inline (.*) (c1 : Column<IDbNum<'num>>) (c2: Column<IDbNum<'num>>) : Column<IDbNum<'num>> =
    binOp HPQ.OpMul c1 c2
  
  let inline (.+) (c1 : Column<IDbNum<'num>>) (c2: Column<IDbNum<'num>>) : Column<IDbNum<'num>> =
    binOp HPQ.OpPlus c1 c2
  
  let inline (.-) (c1 : Column<IDbNum<'num>>) (c2: Column<IDbNum<'num>>) : Column<IDbNum<'num>> =
    binOp HPQ.OpMinus c1 c2

  let inline abs (c1 : Column<IDbNum<'num>>) : Column<IDbNum<'num>> =
    unOp HPQ.OpAbs c1

  let inline negate (c1 : Column<IDbNum<'num>>) : Column<IDbNum<'num>> =
    unOp HPQ.OpNegate c1

  /// We can't use Postgres's 'sign' function because it returns only a
  /// numeric or a double
  let inline signum (c : Column<IDbNum<'num>>) : Column<IDbNum<'num>> =
    let zero = c .- c
    let one = c .* (negate c)
    unsafeCase [(unsafeGt c zero, zero); (unsafeEq c zero, zero)] (negate one)

  let inline (./) (c1 : Column<IDbNum<'num>>) (c2: Column<IDbNum<'num>>) : Column<IDbFractional<'num>> =
    binOp HPQ.OpDiv c1 c2
