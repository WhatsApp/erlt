type public_enum'0 = public_enum

and public_enum = Public_enum'Public_ctr

and private_enum'0 = private_enum

and private_enum = Private_enum'Private_ctr

and semi_private_enum'0 = semi_private_enum

and semi_private_enum = Semi_private_enum'Semi_private_ctr

and opaque_enum_alias'0 = semi_private_enum'0

let rec priv_id'1 : 'tA -> 'tA = function v_X -> v_X

let rec id'1 : 'tA -> 'tA = function v_X -> priv_id'1 v_X
