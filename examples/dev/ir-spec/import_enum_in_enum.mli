type maybe_pair'0 = maybe_pair

and maybe_pair =
  | Maybe_pair'Really_pair of (int, int) Mod03.pair'2
  | Maybe_pair'Empty

val mk_pair'2 : int * int -> (int, int) Mod03.pair'2

val mk_ordered_pair'1 : (int, int) Mod03.pair'2 -> maybe_pair'0
