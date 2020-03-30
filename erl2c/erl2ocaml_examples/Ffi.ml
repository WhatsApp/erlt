type 'a tuple1 = Tuple1 of 'a

external same'2 : 'a * 'a -> unit = "same"

external to_string'1 : 'a -> string = "to_string"

external list_diff'2 : 'a list * 'a list -> 'a list = "list_diff"

type any = private Any
type atom = private Atom
type binary = private Binary
type bitstring = private Bitstring
type byte = private Byte
type identifier = private Identifier
type iodata = private Iodata
type iolist = private Iolist
type ('k, 'v) map = private Map of 'k * 'v
type neg_integer = private NegInteger
type none = private None
type non_neg_integer = private NonNegInteger
type number = private Number
type pid = private Pid
type port = private Port
type pos_integer = private PosInteger
type reference = private Ref
type term = private Term
type timeout = private Timeout

(* aliases *)
type node = atom
type no_return = none
