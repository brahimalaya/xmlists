%%% @author Per Melin <p@greendale.se>

-module(xmlists_entities).

-export([encode/1]).

%% encode("<foo>") -> "&lt;foo&gt;"
encode(Text) when is_binary(Text) -> bin(Text, Text, 0, <<>>);
encode(Text) when is_list(Text)   -> list(Text, []).

%% This should be safe for UTF-8, but not for UTF-16 or UTF-32.
bin(<<$&, R/binary>>, U, N, A)    -> bin(R, R, 0, <<A/binary, U:N/binary, "&amp;">>);
bin(<<$<, R/binary>>, U, N, A)    -> bin(R, R, 0, <<A/binary, U:N/binary, "&lt;">>);
bin(<<$>, R/binary>>, U, N, A)    -> bin(R, R, 0, <<A/binary, U:N/binary, "&gt;">>);
bin(<<$', R/binary>>, U, N, A)    -> bin(R, R, 0, <<A/binary, U:N/binary, "&apos;">>);
bin(<<$", R/binary>>, U, N, A)    -> bin(R, R, 0, <<A/binary, U:N/binary, "&quot;">>);
bin(<<_,  R/binary>>, U, N, A)    -> bin(R, U, N + 1, A);
bin(<<>>,             U, _, <<>>) -> U;
bin(<<>>,             U, _, A)    -> <<A/binary, U/binary>>.

list([$&|T], A) -> list(T, ";pma&"  ++ A);
list([$<|T], A) -> list(T, ";tl&"   ++ A);
list([$>|T], A) -> list(T, ";tg&"   ++ A);
list([$'|T], A) -> list(T, ";sopa&" ++ A);
list([$"|T], A) -> list(T, ";touq&" ++ A);
list([H|T],  A) -> list(T, [H|A]);
list([],     A) -> lists:reverse(A).
