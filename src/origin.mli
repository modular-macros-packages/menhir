open Grammar

(* Reachability from the entry states. *)

(**The origin [SingleOrigin nt] indicates that the point of interest is
   reachable only via the start symbol [nt]. The origin [Dead] indicates
   that this point in unreachable. [MultipleOrigins] indicates that this
   point is reachable via several start symbols. *)
type origin =
  | Dead
  | SingleOrigin of Nonterminal.t
  | MultipleOrigins

(**[run s] determines via which start symbols the [run] function for state
   [s] is reachable. *)
val run: Lr1.node -> origin

(**[reduce prod] determines via which start symbols the [reduce] function
   for production [prod] is reachable. *)
val reduce: Production.t -> origin

(**[goto nt] determines via which start symbols the [goto] function for the
   nonterminal symbol [nt] is reachable. *)
val goto: Nonterminal.t -> origin
