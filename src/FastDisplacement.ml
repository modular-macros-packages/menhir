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

type cell = int * int

type sparse_row = cell list

(* The fast packing algorithm proceeds in two steps:
   1. Finding mergeable rows.
   2. Packing merged rows without allowing any new merge.

   Two sparse rows are mergeable if the values of the cells on which they
   overlap are equal. We say they have compatible cells.

   The classic [RowDisplacement] implementation does both in a single pass, but
   this implies checking for compatibility while fitting a row in the sparse
   table which prevents using shortcuts to speed-up the fitting tests.

   By separating both operations, [FastDisplacement] can use bitsets to
   tests whether cells fit by group of 63 or 31. *)

(* The first step is to compute a merge plan, a partition of rows grouped by
   compatibility.

   This is done in two passes over the rows and their cells:
   1. For each column, a set of rows defined in this column.
      For each cell, a set of "compatible" rows having this cell.
      (Rows and columns are represented by their index, a cell is represented as
      a pair of a column index and a value.)
   2. For each row, merging the next non-merged row that is compatible with this
      row (the step 1. allows compatibility to be quickly determined).
*)

module Merge :
sig

   type group = int list (* A group is a list of row represented by their index *)
  val merge_rows : sparse_row array -> cols:int -> group list

end =
struct
  type indexed_row = {
    source_index: int; (* Position of the row in the original matrix *)
    rank: int; (* Number of non-empty cells *)
    cells: sparse_row;
  }

  (* Index compatible and defined cells *)
  let build_mergeability_maps
      (rows : indexed_row array)
      (cols : int)
    :
      (* Maps a cell to all rows having this cell *)
      (cell -> SparseBitSet.t) *
      (* Maps a column to all rows defined in this column *)
      SparseBitSet.t array
    =
    let compatibility = Hashtbl.create 7 in
    let defined = Array.make cols SparseBitSet.empty in

    (* Register cells for each row *)
    for row_index = Array.length rows - 1 downto 0 do
      let register_cell (col_index, _ as cell) =
        (* Remember the row is defined on column [col_index] *)
        defined.(col_index) <-
          SparseBitSet.add row_index defined.(col_index);
        (* Remember the row has a specific cell *)
        match Hashtbl.find_opt compatibility cell with
        | Some compatible_rows ->
          compatible_rows := SparseBitSet.add row_index !compatible_rows
        | None ->
          Hashtbl.add compatibility cell
            (ref (SparseBitSet.singleton row_index))
      in
      List.iter register_cell rows.(row_index).cells
    done;
    let compatible cell = !(Hashtbl.find compatibility cell) in
    (compatible, defined)

  (* A group is set of compatible rows (represented as a list of source
     indices) *)
  type group = int list

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
        SparseBitSet.take_after row_index (compatible cell)
      and defined_at_col =
        SparseBitSet.take_after row_index defined.(col_index)
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
      let group = row.source_index :: group in
      let candidates = List.fold_left (select_cell row_index) candidates row.cells in
      let candidates = SparseBitSet.diff candidates !incompatible in
      (* Find the first candidate that has not been merged yet *)
      match SparseBitSet.find_first_opt (Array.get available) candidates with
      | None -> group
      | Some next_row ->
        (* Prune irrelevant rows *)
        incompatible := SparseBitSet.take_after next_row !incompatible;
        let candidates = SparseBitSet.take_after next_row candidates in
        add_row group next_row candidates
    in

    (* Start with the current row and build its group *)
    add_row [] current_row SparseBitSet.empty

  (* Helper function to build a merge plan.
     A merge plan is a partition of rows represented as a list of list of rows.
     All rows in the same class are compatible. *)
  let build_merge_plan rows compatible defined =
    let count = Array.length rows in
    let merge_plan = ref [] in

    (* A row is available until it is part of a merge group. *)
    let available = Array.make count true in

    (* Process each row to find compatible groups *)
    for row = 0 to count - 1 do
      if available.(row) then
        let group = build_merge_group rows compatible defined available row in
        merge_plan := group :: !merge_plan;
    done;

    !merge_plan

  (* Find sparse vectors that can be merged *)
  let merge_rows vectors ~cols =
    (* Step 1: Sort rows by size (largest first), remember original indices *)
    let index_row source_index cells =
      {source_index; rank = List.length cells; cells}
    in
    let rows = Array.mapi index_row vectors in
    Array.sort (fun r1 r2 -> Int.compare r2.rank r1.rank) rows;

    (* Step 2: Index compatible and defined cells *)
    let compatible, defined = build_mergeability_maps rows cols in

    (* Step 3: Build merge plan by greedily grouping compatible vectors *)
    build_merge_plan rows compatible defined
end

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
module Packer : sig
  type row
  val import : sparse_row -> row

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
end

(* Connect the dots:
   - Take a matrix with empty cells,
   - Find and merge compatible rows,
   - Pack them
   - Construct the final tables *)

let compress ~hole (t : int array array) : int RowDisplacement.table =

  if Array.length t = 0 then
    ([||], [||])
  else

    (* This turns a row-as-array into a row-as-sparse-list. *)

    let sparse (line : int array) : sparse_row (* index, rank, row *) =

      let rec loop (j : int) (row : sparse_row) =
        if j < 0 then
          row
        else
          let x = line.(j) in
          if x = hole then
            loop (j - 1) row
          else
            loop (j - 1) ((j, x) :: row)
      in

      loop (Array.length line - 1) []

    in

    (* Construct an array of all rows. *)

    let rows : sparse_row array = Array.map sparse t in

    (* Group mergeable rows. *)

    let merge_plan = Merge.merge_rows ~cols:(Array.length t.(0)) rows in

    (* Merge rows, rank them (count the number of non-empty cells), and remember
       their indices. *)

    let merge_rows (indices : int list) : (int * int list * sparse_row) =

      (* Gather cells of all rows *)

      let cells = List.map (Array.get rows) indices in

      (* Merge cells lists, rank result *)

      let compare_cell (i1,a1) (i2,a2) =
        match Int.compare i1 i2 with
        | 0 ->
          (* Safety check: only merge equal values *)
          assert (Int.equal a1 a2);
          0
        | n -> n
      in

      let cells, rank =
        match MList.reduce_list (MList.merge_uniq compare_cell) cells with
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

    let table = Packer.new_table () in

    let fit_merged_row (_rank, _rows, cells) =

      Packer.add_row table (Packer.import cells)

    in

    let offsets = List.map fit_merged_row merged_rows in

    (* Allocate and populate the final arrays *)

    let displacement = Array.make (Array.length t) 0 in
    let data = Array.make (Packer.length table) hole in

    let store_rows offset (_rank, rows, cells) =

      (* Store encoded displacements *)
      let code = RowDisplacement.encode_displacement offset in
      List.iter (fun row -> displacement.(row) <- code) rows;

      (* Store cells *)
      List.iter (fun (i, v) -> data.(offset + i) <- v) cells

    in

    List.iter2 store_rows offsets merged_rows;

    (* Return the compressed tables. *)

    (displacement, data)
