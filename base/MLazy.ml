let[@inline] lazily f a =
  let g = lazy (f a) in
  fun b -> Lazy.force g b
