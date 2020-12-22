-file("dev/src/bins1.erlt", 1).

-module(bins1).

-eqwalizer_unchecked([{bc, 0},
                      {match_bins, 1},
                      {mk_bins, 0},
                      {test_bins, 0}]).

-export([test_bins/0, bc/0]).

-spec test_bins() -> atom().

test_bins() -> match_bins(mk_bins()).

mk_bins() ->
    [<<>>,
     <<1>>,
     <<1:12>>,
     <<1.5:64/float>>,
     <<1:2/integer-unit:8>>,
     <<<<2>>/binary>>].

match_bins([<<>> | Rest]) -> match_bins(Rest);
match_bins([Next | Rest]) ->
    case Next of
        <<1>> -> ok;
        <<1:12>> -> ok;
        <<1.5:64/float>> -> ok;
        <<X:2/integer-unit:8>> when X =:= 1 -> ok;
        <<Bin/binary>> when Bin =:= <<2>> -> ok
    end,
    match_bins(Rest);
match_bins([]) -> ok.

bc() ->
    << <<A:4>>  || <<A:8>> <= <<1, 2, 3, 4, 5, 6>> >>.



