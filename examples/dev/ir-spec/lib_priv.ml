module Lib_priv : sig
  type public_enum'0 = Public_ctr

  and private_enum'0 = Private_ctr

  and semi_private_enum'0 = Semi_private_ctr

  and opaque_enum_alias'0 = semi_private_enum'0

  val id'1 : 'tA -> 'tA

  val priv_id'1 : 'tA -> 'tA
end = struct
  type public_enum'0 = Public_ctr

  and private_enum'0 = Private_ctr

  and semi_private_enum'0 = Semi_private_ctr

  and opaque_enum_alias'0 = semi_private_enum'0

  let rec priv_id'1 : 'tA -> 'tA = function v_X -> v_X

  let rec id'1 : 'tA -> 'tA = function v_X -> priv_id'1 v_X
end
