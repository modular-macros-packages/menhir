
(**Suppose [f a] is a costly computation, whose result is a function
   of type ['b -> 'c]. Then [lazily f a] also returns a function
   of type ['b -> 'c], but is cheap. The costly computation is delayed
   until the point where the resulting function is actually applied to
   a value of type ['b]. *)
val lazily : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
