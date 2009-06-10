%%% @author Per Melin <p@greendale.se>

-module(xmlists).

-export([render/1,
         render/2,
         encode/1]).

-define(XML(Encoding),
            [<<"<?xml version=\"1.0\" encoding=\"">>, Encoding|<<"\"?>\n">>]).

-define(NAMED_ENTITIES, [{"&",  "\\&amp;"},
                         {"<",  "\\&lt;"},
                         {">",  "\\&gt;"},
                         {"'",  "\\&apos;"},
                         {"\"", "\\&quot;"}]).

render(XMList) ->
    render(XMList, []).

render(XMList, Options) ->
    SelfClose = proplists:get_value(self_closing, Options, true),
    Encoding  = proplists:get_value(encoding, Options),
    [xml_declaration(Encoding)|to_iolist(XMList, SelfClose)].

xml_declaration(undefined) -> [];
xml_declaration(Encoding)  -> ?XML(Encoding).

to_iolist([Name, [A|_] = Attribs|Content], O) when is_atom(Name), is_tuple(A) ->
    element(Name, Attribs, Content, O);
to_iolist([Name|Content], O) when is_atom(Name) ->
    element(Name, [], Content, O);
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

element(Name, Attribs, Content, O) ->
    case is_self_closing(Name, Content, O) of
        true ->
            self_closing_tag(Name, Attribs);
        false ->
            ContentIO = [ to_iolist(C, O) || C <- Content ],
            [start_tag(Name, Attribs), ContentIO|end_tag(Name)]
    end.

is_self_closing(_,    [], Flag) when is_boolean(Flag) ->
    Flag;
is_self_closing(Name, [], Elements) ->
    lists:member(Name, Elements);
is_self_closing(_,    _,  _) ->
    false.

self_closing_tag(Name, Attribs) ->
    [$<, atom_to_list(Name), attributes(Attribs), $\s, $/, $>].
start_tag(Name, Attribs) ->
    [$<, atom_to_list(Name), attributes(Attribs), $>].
end_tag(Name) ->
    [$<, $/, atom_to_list(Name), $>].

attributes([A|T]) -> [$\s, attribute(A)|attributes(T)];
attributes([])    -> [].

attribute({Attrib}) ->
    attribute({Attrib, Attrib});
attribute({Attrib, Value}) ->
    [atom_to_list(Attrib), $=, $", to_iolist(Value), $"].

encode(Text) ->
    lists:foldl(fun replace/2, Text, ?NAMED_ENTITIES).

replace({Find, Replace}, Text) ->
    re:replace(Text, Find, Replace, [global]).
