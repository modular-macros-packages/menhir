(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module RowDisplacement = MenhirLib.RowDisplacement
module A = AtomicBitSet
module S = SparseBitSet

(* A cell is a pair of an index and a piece of data. *)
type 'a cell =
  int * 'a

(* A sparse row is a list of cells, whose indices must be pairwise distinct.
   A sparse row may have holes: that is, it may be that, for some index [i],
   there is no cell of the form [(i, v)]. *)
type 'a sparse_row =
  'a cell list

(* This fast packing algorithm proceeds in two steps:

   1. Determine which rows can be merged;
   2. Pack the merged rows into a one-dimensional array,
      without allowing any new merges.

   Two sparse rows are compatible (mergeable) if the values of the cells on
   which they overlap are equal. That is, if one row has a cell [(i, v1)]
   and the other row has a cell [(i, v2)] then the values [v1] and [v2] must
   be equal.

   The classic implementation in [RowDisplacement] performs both steps in a
   single pass. This implies checking for compatibility while fitting a row
   in the sparse table; this forbids the use of vectorised operations (i.e.,
   operations on bit sets) to speed things up.

   By separating the two steps, this algorithm enables the use of bit sets. *)

(* -------------------------------------------------------------------------- *)

(* The first step is to compute a merge plan, that is, a partition of the
   rows, grouped by compatibility.

   This is done in two passes over the rows and their cells:
   1. For each column, a set of rows defined in this column.
      For each cell, a set of "compatible" rows having this cell.
      (Rows and columns are represented by their index, a cell is represented as
      a pair of a column index and a value.)
   2. For each row, merging the next non-merged row that is compatible with this
      row (the step 1. allows compatibility to be quickly determined).
*)

(**A row is represented by its index in the array of rows that is passed
   to [merge_rows]. *)
type index =
  int

(**A group is a list of row indices. *)
 type group =
   index list

(**[merge_rows rows cols] determines which rows in the array [rows] are
   compatible and therefore can be merged. [cols] is the common logical
   width of all rows. *)

let merge_rows (type a) (rows : a sparse_row array) (cols : int) : group list =
  let module M = struct

  (* This function is quite long, so, for syntactic comfort,
     we write its body as a local module. *)

  (* A rich row, or ranked row, is a row that has been decorated with:

     - the index of this row in the original array of rows;
     - its rank, that is, the number of its nonempty cells.

     We sort the array [rows] in place, which is why a row must be
     explicitly decorated with its original index. Outside of this
     specific field, when we use row indices, these are indices
     into the *sorted* array [rows]. *)
  type 'a rrow = {
    index: int;
    rank: int;
    cells: 'a sparse_row;
  }

  (* Enrich each row with its index and rank. *)
  let enrich index cells =
    let rank = List.length cells in
    { index; rank; cells }
  let rows : a rrow array = Array.mapi enrich rows

  (* Sort the rows by decreasing rank. *)
  let decreasing_rank r1 r2 = Int.compare r2.rank r1.rank
  let () = Array.sort decreasing_rank rows

  (* Compute two mappings, [compatible] and [defined].

     The function [compatible] maps a cell to a set of all rows that
     have this cell. (This cell must exist in some row.)

     The array [defined] maps a column to the set of all rows that
     have a cell in this column. *)

  let
    (compatible : a cell -> S.t),
    (defined : S.t array)
  =

    (* We use a hash table whose keys are cells. Therefore the type [a]
       must support OCaml's polymorphic equality and hash functions. *)
    let compatibility = Hashtbl.create 7 in
    let defined = Array.make cols S.empty in

    (* [register i cell] registers the existence of cell [cell] in row [i]. *)
    let register i cell =
      (* Extract the column index [j]. *)
      let j, _ = cell in
      (* Record that this row is defined on column [j]. *)
      defined.(j) <- S.add i defined.(j);
      (* Record that the cell [cell] appears in row [i]. *)
      match Hashtbl.find_opt compatibility cell with
      | Some rows ->
          rows := S.add i !rows
      | None ->
          let rows = ref (S.singleton i) in
          Hashtbl.add compatibility cell rows
    in

    (* Treat each row in turn. [i] is the row's index. Within each row, treat
       each cell in turn. *)
    for i = Array.length rows - 1 downto 0 do
      List.iter (register i) rows.(i).cells
    done;

    let compatible cell = !(Hashtbl.find compatibility cell) in
    compatible, defined

  (* During the construction of a merge plan, a row is available until it
     becomes part of a group; then, it is no longer available. The mutable
     array [available] keeps track of which rows are still available. *)

  let available =
    Array.map (fun _ -> true) rows

  (* [build_group i] builds a new group, which contains at least the row [i].
     The array [available] is updated: the rows in the new group are marked as
     no longer available. *)

  let build_group (i : index) : group =

    (* Accumulate the rows that are candidates for inclusion in the current
       group. This set grows; but, because we are interested only in rows
       above the current row [i], we remove rows whose index is less than [i];
       and, because an incompatible row is not a suitable candidate, we also
       (lazily) remove incompatible rows. *)
    let candidates = ref S.empty in
    (* Accumulate the rows that are incompatible with the current group.
       This set grows; but, because we are interested only in rows above
       the current row [i], we remove rows whose index is less than [i]. *)
    let incompatible = ref S.empty in

    (* [update i cell] updates the sets [candidates] and [incompatible]
       so as to reflect the existence in row [i] of the cell [cell].
       This row has been included in the current group already. *)
    let update i cell =
      let (j, _) = cell in
      (* Restricting our attention to rows whose index is above [i], compute
         which rows are compatible / incompatible with this cell. *)
      let compatible_at_j   = S.above i (compatible cell)
      and defined_at_j      = S.above i defined.(j) in
      let incompatible_at_j = S.diff defined_at_j compatible_at_j in
      (* The compatible cells become new candidates for inclusion in this
         group. The incompatible cells are recorded. *)
      candidates := S.union compatible_at_j !candidates;
      incompatible := S.union incompatible_at_j !incompatible
    in

    (* [add_row group i] adds the row [i] to the group [group], then
       continues extending this group with rows whose indices lie above [i]. *)
    let rec add_row group i : group =
      (* Mark the row [i] as unavailable. *)
      assert available.(i);
      available.(i) <- false;
      (* Fetch the row whose index is [i]. Extend the group with its index.
         (A group is a list of *original* indices.) *)
      let row = rows.(i) in
      let group = row.index :: group in
      (* Update the sets [incompatible] and [candidates]. *)
      List.iter (update i) row.cells;
      candidates := S.diff !candidates !incompatible;
      (* Find the first (available) candidate, if there are any. *)
      match S.find_first_opt (Array.get available) !candidates with
      | None ->
          (* Done. *)
          group
      | Some i ->
          (* The next candidate is row [i]. Prune the sets [incompatible] and
             [candidate] by removing now-irrelevant rows. Then, add row [i] to
             the group and continue. *)
          incompatible := S.above i !incompatible;
          candidates := S.above i !candidates;
          add_row group i
    in

    (* Start with the row [i] and build a group for it. *)
    add_row [] i

  (* A merge plan is a partition of the rows; it is represented as a list of
     groups. The rows within a group are compatible with each other. We are
     now ready to build a merge plan.  *)

  let merge_plan : group list =

    (* Process each row in turn. For each available row, greedily build a new
       group, which contains at least this row. Because the arrays [rows] has
       been sorted by decreasing rank, we begin with the rows that have higher
       ranks. *)
    let merge_plan = ref [] in
    for i = 0 to Array.length rows - 1 do
      if available.(i) then
        let group = build_group i in
        merge_plan := group :: !merge_plan
    done;
    !merge_plan

  end (* M *)
  (* Return the merge plan. *)
  in M.merge_plan

(* -------------------------------------------------------------------------- *)

(* The second step is a variant of the fitting loop of [RowDisplacement]
   specialized to work on bitsets:
   - a row is now a [S].
   - table is now an array of [A].

   (The values are thus ignored, only the occupancy matters.)

   Rows are added to the table one after another, the offsets being computed on
   the fly.

   The core loop is still $O(n^2)$ ($n$ being the number of non-empty cells),
   but the merging and the packing make the $n$ smaller and the constant-factors
   much faster. *)
module Pack : sig
  type row
  val import : 'a sparse_row -> row

  type table
  val new_table : unit -> table
  val add_row : table -> row -> int

  val length : table -> int
end = struct
  type row =
    (* Offset of first element, and a sparse set of all elements where the first
       is at index [0]. *)
    int * S.t

  let import = function
    | [] -> (0, S.empty)
    | (index0, _) :: rest ->
      let add set (index, _) = S.add (index - index0) set in
      (* Reverse the list to avoid quadratic insertion *)
      let set = List.fold_left add S.empty (List.rev rest) in
      (index0, S.add 0 set)

  let compatible cells0 cells1 mask shift =
    let mask0, mask1 = A.shift mask shift in
    A.disjoint cells0 mask0 &&
    A.disjoint cells1 mask1

  type table = A.t array ref

  let rec fit_cells table oaddr oshift cells =
    match S.View.view cells with
    | S.View.N -> oshift
    | S.View.C (base, mask, cells') ->
      let addr = oaddr + base / A.bound in
      let len = Array.length table in
      if addr >= len then
        oshift
      else
        let cells0 = table.(addr) in
        let cells1 =
          if addr + 1 = len
          then A.empty
          else table.(addr + 1)
        in
        if compatible cells0 cells1 mask oshift then
          fit_cells table oaddr oshift cells'
        else (
          let oshift = ref (oshift + 1) in
          while !oshift < A.bound &&
                not (compatible cells0 cells1 mask !oshift)
          do incr oshift; done;
          !oshift
        )

  let grow_table table target =
    let len = Array.length table in
    if len > target then
      table
    else
      let rlen = ref len in
      while !rlen <= target do
        rlen := !rlen * 2
      done;
      let table' = Array.make !rlen A.empty in
      Array.blit table 0 table' 0 len;
      table'

  let rec write_cells table oaddr oshift cells =
    match S.View.view cells with
    | S.View.N -> table
    | S.View.C (base, mask, cells') ->
      let addr = oaddr + base / A.bound in
      let table = grow_table table (addr + 1) in
      let mask0, mask1 = A.shift mask oshift in
      let cells0 = table.(addr) in
      let cells1 = table.(addr + 1) in
      assert (A.disjoint cells0 mask0);
      assert (A.disjoint cells1 mask1);
      table.(addr + 0) <- A.union cells0 mask0;
      table.(addr + 1) <- A.union cells1 mask1;
      write_cells table oaddr oshift cells'

  let rec add_cells table oaddr oshift cells =
    let oshift' = fit_cells table oaddr oshift cells in
    if oshift' = oshift then
      let table = write_cells table oaddr oshift cells in
      (table, oaddr * A.bound + oshift)
    else if oshift' < A.bound then
      add_cells table oaddr oshift' cells
    else
      add_cells table (oaddr + 1) 0 cells

  let add_row rtable (index, cells) =
    let table, offset = add_cells !rtable 0 0 cells in
    rtable := table;
    offset - index

  let new_table () = ref [|A.empty|]

  let length rtable =
    let table = !rtable in
    let last = ref (Array.length table - 1) in
    while !last >= 0 && A.is_empty table.(!last) do
      decr last
    done;
    if !last < 0 then
      0
    else
      let msb = A.maximum table.(!last) in
      !last * A.bound + msb + 1

end (* Pack *)

(* -------------------------------------------------------------------------- *)

(* Connect the dots:
   - Take a matrix with empty cells,
   - Find and merge compatible rows,
   - Pack them
   - Construct the final tables *)

let compress (type a) insignificant dummy (t : a array array) : a RowDisplacement.table =

  if Array.length t = 0 then
    ([||], [||])
  else

    (* This turns a row-as-array into a row-as-sparse-list. *)

    let sparse (line : a array) : a sparse_row =

      let[@tail_mod_cons] rec loop (j : int) (row : a sparse_row) =
        if j < 0 then
          row
        else
          let x = line.(j) in
          if insignificant x then
            loop (j - 1) row
          else
            loop (j - 1) ((j, x) :: row)
      in

      loop (Array.length line - 1) []

    in

    (* Construct an array of all rows. *)

    let rows : a sparse_row array = Array.map sparse t in

    (* Group mergeable rows. *)

    let cols = Array.length t.(0) in
    let merge_plan = merge_rows rows cols in

    (* Merge rows, rank them (count the number of non-empty cells), and remember
       their indices. *)

    let merge_rows (indices : int list) : (int * int list * a sparse_row) =

      (* Gather cells of all rows *)

      let cells = List.map (Array.get rows) indices in

      (* Merge cells lists, rank result *)

      let compare_cell (i1,a1) (i2,a2) =
        match Int.compare i1 i2 with
        | 0 ->
          (* Safety check: only merge equal values *)
          assert (a1 = a2);
          0
        | n -> n
      in

      let cells, rank =
        match MList.reduce (MList.merge_uniq compare_cell) cells with
        | None -> ([], 0)
        | Some cells -> (cells, List.length cells)
      in

      (rank, indices, cells)

    in

    (* Merge and rank the rows (longest row first) *)

    let merged_rows =
      List.map merge_rows merge_plan
      |> List.sort (fun (r1, _, _) (r2, _, _) -> Int.compare r2 r1)
    in

    (* Compute an offset for each group of source rows *)

    let table = Pack.new_table () in

    let fit_merged_row (_rank, _rows, cells) =

      Pack.add_row table (Pack.import cells)

    in

    let offsets = List.map fit_merged_row merged_rows in

    (* Allocate and populate the final arrays *)

    let displacement = Array.make (Array.length t) 0 in
    let data = Array.make (Pack.length table) dummy in

    let store_rows offset (_rank, rows, cells) =

      (* Store encoded displacements *)
      let code = RowDisplacement.encode offset in
      List.iter (fun row -> displacement.(row) <- code) rows;

      (* Store cells *)
      List.iter (fun (i, v) -> data.(offset + i) <- v) cells

    in

    List.iter2 store_rows offsets merged_rows;

    (* Return the compressed tables. *)

    (displacement, data)
