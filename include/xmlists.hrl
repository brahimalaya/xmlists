-define(map(V, L, X),
            [ X || V <- L]).

-define(value(K, P),
            proplists:get_value(K, P, "")).
