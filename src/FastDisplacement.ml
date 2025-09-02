(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MenhirLib

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

module Merge : sig

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
  val merge_rows : 'a sparse_row array -> int -> group list

end = struct

  type index =
    int

  (**A rich row, or ranked row, is a row that has been decorated with:

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

  (* We begin by computing two mappings, [compatible] and [defined].

     The function [compatible] maps a cell to a set of all rows that
     have this cell. (This cell must exist in some row.)

     The array [defined] maps a column to the set of all rows that
     have a cell in this column. *)

  let precompute (type a) (rows : a rrow array) (cols : int)
  :
    (* compatible: *) (a cell -> SparseBitSet.t) *
    (* defined:    *) SparseBitSet.t array
  =

    (* We use a hash table whose keys are cells. Therefore the type [a]
       must support OCaml's polymorphic equality and hash functions. *)
    let compatibility = Hashtbl.create 7 in
    let defined = Array.make cols SparseBitSet.empty in

    (* [register i cell] registers the existence of cell [cell] in row [i]. *)
    let register i cell =
      (* Extract the column index [j]. *)
      let j, _ = cell in
      (* Record that this row is defined on column [j]. *)
      defined.(j) <- SparseBitSet.add i defined.(j);
      (* Record that the cell [cell] appears in row [i]. *)
      match Hashtbl.find_opt compatibility cell with
      | Some rows ->
          rows := SparseBitSet.add i !rows
      | None ->
          let rows = ref (SparseBitSet.singleton i) in
          Hashtbl.add compatibility cell rows
    in

    (* Treat each row in turn. [i] is the row's index. Within each row, treat
       each cell in turn. *)
    for i = Array.length rows - 1 downto 0 do
      List.iter (register i) rows.(i).cells
    done;

    let compatible cell = !(Hashtbl.find compatibility cell) in
    compatible, defined

  type group =
    index list

  (* Helper function to greedily pick a group of rows to merge. *)
  let build_merge_group
      rows compatible defined
      (* A row can be merged only once.
         Use [available] to pick only availabled rows and mark selected rows as
         unavailable. *)
      (available : bool array)
      (current_row : int)
    : group
    =
    (* Accumulate rows incompatible with the current group *)
    let incompatible = ref SparseBitSet.empty in

    (* Update incompatible candidates *)
    let select_cell row_index candidates (col_index, _ as cell) =
      (* [row_index] monotonically increases in a group,
         we are only interested about what happens to rows
         with greater indices *)
      let compatible_at_col =
        SparseBitSet.above row_index (compatible cell)
      and defined_at_col =
        SparseBitSet.above row_index defined.(col_index)
      in
      let incompatible_at_col =
        SparseBitSet.diff defined_at_col compatible_at_col
      in
      incompatible := SparseBitSet.union incompatible_at_col !incompatible;
      SparseBitSet.union compatible_at_col candidates
    in

    (* Add a row to the group *)
    let rec add_row group row_index candidates =
      (* Mark the row as unavailable *)
      assert available.(row_index);
      available.(row_index) <- false;
      (* Fetch row contents, update group, candidates and incompatible
         rows *)
      let row = rows.(row_index) in
      let group = row.index :: group in
      let candidates = List.fold_left (select_cell row_index) candidates row.cells in
      let candidates = SparseBitSet.diff candidates !incompatible in
      (* Find the first candidate that has not been merged yet *)
      match SparseBitSet.find_first_opt (Array.get available) candidates with
      | None -> group
      | Some next_row ->
        (* Prune irrelevant rows *)
        incompatible := SparseBitSet.above next_row !incompatible;
        let candidates = SparseBitSet.above next_row candidates in
        add_row group next_row candidates
    in

    (* Start with the current row and build its group *)
    add_row [] current_row SparseBitSet.empty

  (* [build_merge_plan] builds a merge plan. A merge plan is a partition of
     the rows; it is represented as a list of groups. The rows within a group
     are compatible with each other. *)

  let build_merge_plan rows compatible defined : group list =
    let count = Array.length rows in

    (* A row is available until it becomes part of a group. *)
    let available = Array.make count true in

    (* Process each row in turn. For each available row, greedily build a new
       group, which contains at least this row. Because the arrays [rows] has
       been sorted by decreasing rank, we begin with the rows that have higher
       ranks. *)
    let merge_plan = ref [] in
    for row = 0 to count - 1 do
      if available.(row) then
        let group = build_merge_group rows compatible defined available row in
        merge_plan := group :: !merge_plan
    done;
    !merge_plan

  (* The main function, [merge_rows], determines which rows can be merged. *)

  let merge_rows (type a) (rows : a sparse_row array) (cols : int) : group list =

    (* Enrich each row with its index and rank. *)
    let enrich index cells =
      let rank = List.length cells in
      { index; rank; cells }
    in
    let rows = Array.mapi enrich rows in

    (* Sort the rows by decreasing rank. *)
    let decreasing_rank r1 r2 = Int.compare r2.rank r1.rank in
    Array.sort decreasing_rank rows;

    (* Compute [compatible] and [defined]. *)
    let compatible, defined = precompute rows cols in

    (* Build a merge plan. *)
    build_merge_plan rows compatible defined

end (* Merge *)

(* -------------------------------------------------------------------------- *)

(* The second step is a variant of the fitting loop of [RowDisplacement]
   specialized to work on bitsets:
   - a row is now a [SparseBitSet].
   - table is now an array of [AtomicBitSet].

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
    int * SparseBitSet.t

  let import = function
    | [] -> (0, SparseBitSet.empty)
    | (index0, _) :: rest ->
      let add set (index, _) = SparseBitSet.add (index - index0) set in
      (* Reverse the list to avoid quadratic insertion *)
      let set = List.fold_left add SparseBitSet.empty (List.rev rest) in
      (index0, SparseBitSet.add 0 set)

  let compatible cells0 cells1 mask shift =
    let mask0, mask1 = AtomicBitSet.shift mask shift in
    AtomicBitSet.disjoint cells0 mask0 &&
    AtomicBitSet.disjoint cells1 mask1

  type table = AtomicBitSet.t array ref

  let rec fit_cells table oaddr oshift cells =
    match SparseBitSet.View.view cells with
    | SparseBitSet.View.N -> oshift
    | SparseBitSet.View.C (base, mask, cells') ->
      let addr = oaddr + base / AtomicBitSet.bound in
      let len = Array.length table in
      if addr >= len then
        oshift
      else
        let cells0 = table.(addr) in
        let cells1 =
          if addr + 1 = len
          then AtomicBitSet.empty
          else table.(addr + 1)
        in
        if compatible cells0 cells1 mask oshift then
          fit_cells table oaddr oshift cells'
        else (
          let oshift = ref (oshift + 1) in
          while !oshift < AtomicBitSet.bound &&
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
      let table' = Array.make !rlen AtomicBitSet.empty in
      Array.blit table 0 table' 0 len;
      table'

  let rec write_cells table oaddr oshift cells =
    match SparseBitSet.View.view cells with
    | SparseBitSet.View.N -> table
    | SparseBitSet.View.C (base, mask, cells') ->
      let addr = oaddr + base / AtomicBitSet.bound in
      let table = grow_table table (addr + 1) in
      let mask0, mask1 = AtomicBitSet.shift mask oshift in
      let cells0 = table.(addr) in
      let cells1 = table.(addr + 1) in
      assert (AtomicBitSet.disjoint cells0 mask0);
      assert (AtomicBitSet.disjoint cells1 mask1);
      table.(addr + 0) <- AtomicBitSet.union cells0 mask0;
      table.(addr + 1) <- AtomicBitSet.union cells1 mask1;
      write_cells table oaddr oshift cells'

  let rec add_cells table oaddr oshift cells =
    let oshift' = fit_cells table oaddr oshift cells in
    if oshift' = oshift then
      let table = write_cells table oaddr oshift cells in
      (table, oaddr * AtomicBitSet.bound + oshift)
    else if oshift' < AtomicBitSet.bound then
      add_cells table oaddr oshift' cells
    else
      add_cells table (oaddr + 1) 0 cells

  let add_row rtable (index, cells) =
    let table, offset = add_cells !rtable 0 0 cells in
    rtable := table;
    offset - index

  let new_table () = ref [|AtomicBitSet.empty|]

  let length rtable =
    let table = !rtable in
    let last = ref (Array.length table - 1) in
    while !last >= 0 && AtomicBitSet.is_empty table.(!last) do
      decr last
    done;
    if !last < 0 then
      0
    else
      let msb = AtomicBitSet.maximum table.(!last) in
      !last * AtomicBitSet.bound + msb + 1

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
    let merge_plan = Merge.merge_rows rows cols in

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
