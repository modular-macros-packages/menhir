let[@inline] repeat q yield =
  try
    while true do
      yield (Queue.take q)
    done
  with Queue.Empty ->
    ()
