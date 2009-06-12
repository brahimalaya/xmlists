%%% @author Per Melin <p@greendale.se>

-module(xmlists).

-export([render/1,
         render/2,
         encode/1]).

-define(XML(Encoding),
            [<<"<?xml version=\"1.0\" encoding=\"">>, Encoding|<<"\"?>\n">>]).

render(XMList) ->
    render(XMList, []).

render(XMList, Options) ->
    SelfClose = proplists:get_value(self_closing, Options, true),
    Encoding  = proplists:get_value(encoding, Options),
    [xml_declaration(Encoding)|to_iolist(XMList, SelfClose)].

xml_declaration(undefined) -> [];
xml_declaration(Encoding)  -> ?XML(Encoding).

%% Element with attributes, e.g: [user, [{id, 4711}]]
to_iolist([Name, [A|_] = Attribs|Content], O) when is_atom(Name), is_tuple(A) ->
    element(Name, Attribs, Content, O);
%% Element without attributes, e.g: [userid, 4711]
to_iolist([Name|Content], O) when is_atom(Name) ->
    element(Name, [], Content, O);
%% Something wrapped in a rendundant list, e.g:
%% [[userid, 4711]] or [[userid, 4711]|[userid, 1337]]
to_iolist([H|T], O) when is_list(H) ->
    [to_iolist(H, O)|to_iolist(T, O)];
to_iolist([], _) ->
    "";
to_iolist({cdata, Cdata}, _) ->
    [<<"<![CDATA[">>, Cdata|<<"]]>">>];
to_iolist({comment, Comment}, _) ->
    [<<"<!-- ">>, Comment|<<" -->">>];
to_iolist([{raw, Content}|T], O) ->
    [Content|to_iolist(T, O)];
to_iolist({raw, Content}, _) ->
    Content;
to_iolist(Content, _) ->
    to_iolist(Content).

to_iolist(Val) when is_list(Val)    -> encode(Val);
to_iolist(Val) when is_binary(Val)  -> encode(Val);
to_iolist(Val) when is_integer(Val) -> integer_to_list(Val);
to_iolist(Val) when is_float(Val)   -> io_lib:format("~f", [Val]);
to_iolist(Val) when is_atom(Val)    -> atom_to_list(Val).

%% element(foo, [], [], true)              -> "<foo />"
%% element(foo, [], [], false)             -> "<foo></foo>"
%% element(foo, [{id, 17}], [], true)      -> "<foo id=\"17\" />"
%% element(foo, [{id, 17}], ["bar"], true) -> "<foo id=\"17\">bar</foo>"
element(Name, Attribs, Content, O) ->
    case is_self_closing(Name, Content, O) of
        true ->
            self_closing_tag(Name, Attribs);
        false ->
            ContentIO = [ to_iolist(C, O) || C <- Content ],
            [start_tag(Name, Attribs), ContentIO|end_tag(Name)]
    end.

%% A self-closing tag is a single tag for an empty element, e.g: <br />
%% instead of <br></br>. In HTML not all elements are allowed to have
%% this form. The third parameter is either true, false or a list of
%% elements that can self-close.
is_self_closing(_,    [], Flag) when is_boolean(Flag) ->
    Flag;
is_self_closing(Name, [], Elements) ->
    lists:member(Name, Elements);
is_self_closing(_,    _,  _) ->
    false.

%% self_closing_tag(foo, [{id, 17}]) -> "<foo id=\"17\" />
self_closing_tag(Name, Attribs) ->
    [$<, atom_to_list(Name), attributes(Attribs), $\s, $/, $>].

%% start_tag(foo, [{id, 17}]) -> "<foo id=\"17\">"
start_tag(Name, Attribs) ->
    [$<, atom_to_list(Name), attributes(Attribs), $>].

%% end_tag(foo) -> "</foo>"
end_tag(Name) ->
    [$<, $/, atom_to_list(Name), $>].

%% attributes([{a, 1}, {b, 2}]) -> " a=\"1\" b=\"2\""
attributes([A|T]) -> [$\s, attribute(A)|attributes(T)];
attributes([])    -> [].

attribute({Attrib}) ->
    attribute({Attrib, Attrib});
attribute({Attrib, Value}) ->
    [atom_to_list(Attrib), $=, $", to_iolist(Value), $"].

%% encode("<foo>") -> "&lt;foo&gt;"
encode(Text) when is_binary(Text) -> encode_bin(Text, <<>>);
encode(Text) when is_list(Text)   -> encode_list(Text, []).

encode_bin(<<$&, R/binary>>, A) -> encode_bin(R, <<A/binary, "&amp;">>);
encode_bin(<<$<, R/binary>>, A) -> encode_bin(R, <<A/binary, "&lt;">>);
encode_bin(<<$>, R/binary>>, A) -> encode_bin(R, <<A/binary, "&gt;">>);
encode_bin(<<$', R/binary>>, A) -> encode_bin(R, <<A/binary, "&apos;">>);
encode_bin(<<$", R/binary>>, A) -> encode_bin(R, <<A/binary, "&quot;">>);
encode_bin(<<H,  R/binary>>, A) -> encode_bin(R, <<A/binary, H>>);
encode_bin(<<>>, A)             -> A.

encode_list([$&|T], A) -> encode_list(T, ";pma&"  ++ A);
encode_list([$<|T], A) -> encode_list(T, ";tl&"   ++ A);
encode_list([$>|T], A) -> encode_list(T, ";tg&"   ++ A);
encode_list([$'|T], A) -> encode_list(T, ";sopa&" ++ A);
encode_list([$"|T], A) -> encode_list(T, ";touq&" ++ A);
encode_list([H|T],  A) -> encode_list(T, [H|A]);
encode_list([],     A) -> lists:reverse(A).
