module internal Helpers
  
  let inline (.^) f g x y = f (g x y)

  let inline (.^.) f g a b c = f (g a b c)

  let inline (.^^) f g a b c d = f (g a b c d)

  let inline (.^^.) f g a b c d e = f (g a b c d e)

