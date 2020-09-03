-lang([erl2, st]).
-module(n07).
-enum option(A) :: none{} | some{A}.

getOrElse(Opt, Alt) ->
  case Opt of
      option.some{X} -> X;
      option.none{} -> Alt
  end.

%% the type for rec cannot be expressed without recursion
unBox(Rec, Alt) ->
  getOrElse(Rec#(next), Rec).
