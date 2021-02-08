-module(records).

-compile([export_all, nowarn_export_all]).

-record(rec1, {}).
-record(rec2, {}).
-record(rec3, {}).

-spec mk_rec1_pos() -> #rec1{}.
mk_rec1_pos() -> #rec1{}.

-spec mk_rec1_neg() -> #rec1{}.
mk_rec1_neg() -> #rec2{}.

-spec mk_rec1a_pos() -> #rec1{}.
mk_rec1a_pos() ->
    R = #rec1{},
    R.

-spec mk_rec1a_neg() -> #rec1{}.
mk_rec1a_neg() ->
    R = #rec2{},
    R.

-spec mk_rec_pos(atom()) ->
    #rec1{} | #rec2{}.
mk_rec_pos(rec1) -> #rec1{};
mk_rec_pos(rec2) -> #rec2{}.

-spec mk_rec_neg(atom()) ->
    #rec1{} | #rec3{}.
mk_rec_neg(rec1) -> #rec1{};
mk_rec_neg(rec2) -> #rec2{}.
