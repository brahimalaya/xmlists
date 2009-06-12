%%% @author Per Melin <p@greendale.se>

-module(xmlists_test).

-compile(export_all).

-include("xmlists.hrl").

-define(eq(X, Y), case (X) == (Y) of true -> ok; _ -> error(?LINE, X, Y) end).

%%%  I'm not using EUnit because its error output is truncated so short
%%%  that in many cases I can't see the difference between the expected
%%%  and the actual output.

test() ->
    up_to_date = make:all([load]),
    ?MODULE:run_all_tests().

element_test() ->
    L = [foo, [bar, "xyzzy"]],
    X = "<foo>
            <bar>xyzzy</bar>
         </foo>",
    ?eq(l(L), x(X)).

empty_element_test() ->
    L = [foo],
    X = "<foo />",
    ?eq(l(L), x(X)).

empty_optional_element_test() ->
    L = [[foo],
         [bar]],
    X = "<foo></foo>
         <bar />",
    ?eq(l(L, [{self_closing, [bar]}]), x(X)).

attrib_test() ->
    L = [foo, [{id, "foz"}],
            [bar, [{id, "baz"}]]],
    X = "<foo id=\"foz\">
            <bar id=\"baz\" />
         </foo>",
    ?eq(l(L), x(X)).

attrib_shorthand_test() ->
    L = [option, [{selected}]],
    X = "<option selected=\"selected\" />",
    ?eq(l(L), x(X)).

integers_test() ->
    L = [value, [{id, 1}], 2],
    X = "<value id=\"1\">2</value>",
    ?eq(l(L), x(X)).

atoms_test() ->
    L = [foo, [{id, bar}], xyzzy],
    X = "<foo id=\"bar\">xyzzy</foo>",
    ?eq(l(L), x(X)).

binaries_test() ->
    L = [foo, [{id, <<"bar">>}], <<"baz">>],
    X = "<foo id=\"bar\">baz</foo>",
    ?eq(l(L), x(X)).

content_test() ->
    L = [foo, "back", 2, <<"back">>],
    X = "<foo>back2back</foo>",
    ?eq(l(L), x(X)).

encoding_list_test() ->
    L = "a'<&>_\"o",
    X = "a&apos;&lt;&amp;&gt;_&quot;o",
    ?eq(l(L), x(X)).

encoding_binary_test() ->
    L = <<"a'<&>_\"o">>,
    X = <<"a&apos;&lt;&amp;&gt;_&quot;o">>,
    ?eq(l(L), x(X)).

deep_test() ->
    L = [[[foo,
            [[xoo]|[[bar]]]]]],
    X = "<foo>
            <xoo />
            <bar />
         </foo>",
    ?eq(l(L), x(X)).

cdata_test() ->
    L = [foo, {cdata, "<foo>"}],
    X = "<foo>
            <![CDATA[<foo>]]>
         </foo>",
    ?eq(l(L), x(X)).

comment_test() ->
    L = [foo, {comment, "<&>"}],
    X = <<"<foo><!-- <&> --></foo>">>,
    ?eq(l(L), X).

raw_content_test() ->
    L = [foo, {raw, "<foo>"}],
    X = "<foo>
            <foo>
         </foo>",
    ?eq(l(L), x(X)).

raw_unwrapped_test() ->
    L = [{raw, "<foo>"}],
    X = "<foo>",
    ?eq(l(L), x(X)).

%% HTML

doctype_test() ->
    L = [body],
    H = htmlists:render(L, [{doctype, transitional}]),
    X = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" "
        "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
        "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
            "<body></body>"
        "</html>",
    ?eq(iolist_to_binary(H), list_to_binary(X)).

doctype_xml_test() ->
    L = [body],
    H = htmlists:render(L, [{doctype, transitional}, {encoding, "UTF-8"}]),
    X = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" "
        "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
        "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
            "<body></body>"
        "</html>",
    ?eq(iolist_to_binary(H), list_to_binary(X)).

%% Macros

map_test() ->
    D = [17, 4711],
    L = [ul,
            ?map(V, D,
                [li, V])],
    X = "<ul>
            <li>17</li>
            <li>4711</li>
         </ul>",
    ?eq(l(L), x(X)).

%% Utility functions

run_all_tests() ->
    R = [ ?MODULE:F() || {F, 0} <- module_info(exports),
                                   lists:suffix("_test", atom_to_list(F)) ],
    T = length(R),
    E = length([ error || error <- R ]),
    io:format("Tests: ~b  Errors: ~b~n", [T, E]),
    if E == 0 -> ok;
       E > 0  -> error
    end.

error(Line, X, Y) ->
    io:format("Mismatch on line ~b:~n~s~n-~n~s~n~n", [Line, X, Y]),
    error.

l(XMList) ->
    iolist_to_binary(xmlists:render(XMList)).

l(XMList, Options) ->
    iolist_to_binary(xmlists:render(XMList, Options)).

x(XML0) ->
    XML1 = re:replace(XML0, "\\s+<", "<", [global]),
    XML2 = re:replace(XML1, ">\\s+", ">", [global]),
    iolist_to_binary(XML2).
