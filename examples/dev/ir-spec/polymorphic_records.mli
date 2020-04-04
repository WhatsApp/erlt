type rec1 = < > as 'row_tv__1

and 'tA rec2 = < get_a : 'tA ; set_a : 'tA -> 'row_tv__2 > as 'row_tv__2

and 'tR someRec = < .. > as 'tR

and 'tIdType idRec =
  < get_id : 'tIdType ; set_id : 'tIdType -> 'row_tv__3 > as 'row_tv__3

and ('tIdType, 'tRest) someRecWithId =
  < get_id : 'tIdType ; set_id : 'tIdType -> 'tRest ; .. > as 'tRest
