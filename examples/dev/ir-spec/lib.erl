-file("dev/src/lib.erlt", 1).

-module(lib).

-export([id/1]).

-export_type([public_enum/0, opaque_enum_alias/0]).

-type public_enum() :: {969696,
                        lib,
                        public_enum,
                        public_ctr}.

-type private_enum() :: {969696,
                         lib,
                         private_enum,
                         private_ctr}.

-type semi_private_enum() :: {969696,
                              lib,
                              semi_private_enum,
                              semi_private_ctr}.

-opaque opaque_enum_alias() :: semi_private_enum().

-spec id(A) -> A.

id(X) -> priv_id(X).

-spec priv_id(A) -> A.

priv_id(X) -> X.



