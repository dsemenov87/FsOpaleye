module internal InternalOrder

open FSharpPlus

module HPQ = InternalPrimQuery
module IC = InternalColumn

/// An `Order` represents an expression to order on and a sort
/// direction. Multiple `Order`s can be composed with
/// `Data.Monoid.mappend`.  If two rows are equal according to the first
/// `Order`, the second is used, and so on.
/// Like the (columns -> RowParser haskells) field of QueryRunner this
/// type is "too big".  We never actually look at the 'a' (in the
/// QueryRunner case the 'colums') except to check the "structure".
/// This is so we can support a SumProfunctor instance.
type Order<'a> =
  | Order of ('a -> (HPQ.OrderOp * HPQ.PrimExpr) list)

let contramap f (Order g) = Order (lmap f g)

let empty = Order getEmpty
  
let append (Order o) (Order o') = Order (append o o')

let divide f o o' =
  append (contramap (fst << f) o) (contramap (snd << f) o')
  
let conquer = empty

//  let lose f = contramap f (Order Void.absurd)

let choose f (Order o) (Order o') = contramap f (Order (either o o'))

let order (op: HPQ.OrderOp) (f: 'a -> 'b) : Order<'a> =
  Order (map (fun column -> [(op, IC.unColumn column)]) f)

let orderByU (os: Order<'a>) (columns: 'a, primQ: PQ.PrimQuery, t:T.Tag) : 'a * PQ.PrimQuery * T.Tag
orderByU os  = (columns, primQ', t)
  where primQ' = PQ.Order orderExprs primQ
        Order sos = os
        orderExprs = map (uncurry HPQ.OrderExpr) (sos columns)