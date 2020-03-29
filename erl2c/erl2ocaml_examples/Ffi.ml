type 'a tuple1 = Tuple1 of 'a

external same'2 : 'a * 'a -> unit = "same"

external to_string'1 : 'a -> string = "to_string"

external list_diff'2 : 'a list * 'a list -> 'a list = "list_diff"
