let c = ref 0
let tick() =
  incr c
let current() =
  !c

let make e =
  tick();
  e

let rejects =
  ref 0

let reject () =
  incr rejects;
  MenhirGLR.GLR.reject()
