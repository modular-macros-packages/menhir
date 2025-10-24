type range =
  Range.range

type ranges =
  Range.ranges

type 'a located =
  'a Located.located

type ocamltype = OCamlType.ocamltype =
  | Declared of string located
  | Inferred of string
