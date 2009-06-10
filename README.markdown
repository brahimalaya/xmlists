This is an experiment in using Erlang lists to represent XML and XHTML.

    1> XMList = [foo, [bar]].
    [foo,[bar]]
    2> io:format(xmlists:render(XMList)).
    <foo><bar /></foo>

Examples
========

    [table,
        [tr,
            [td, "foo"],
            [td, "bar"],
            [td, [{class, numeric}], 4711]]]

Becomes:

    <table>
        <tr>
            <td>foo</td>
            <td>bar</td>
            <td class="numeric">4711</td>
        </tr>
    </table>

_(Except without the line-breaks and indentation)_

Since they are just ordinary lists, you can construct them however you want. For example using a list comprehension:

    [ul,
        [[li, N] || N <- [1,2,3]]]

Becomes:

    <ul>
        <li>1</li>
        <li>2</li>
        <li>3</li>
    </ul>

The rules
=========
1. A list starting with an atom is an XML element.

        [foo] -> <foo />

2. Anything following that first atom is the content of the element.

        [foo, "bar"] -> <foo>bar</foo>

3. The exception from rule 2 is that a list with tuples directly following that first atom holds attributes.

        [foo, [{id, 4711}], "bar"] -> <foo id="4711">bar</foo>

4. The content can be strings, binaries, atoms or numbers.

        [foo, bar]       -> <foo>bar</foo>
        [foo, "bar"]     -> <foo>bar</foo>
        [foo, <<"bar">>] -> <foo>bar</foo>
        [foo, 4711]      -> <foo>4711</foo>

5. Multiple contents are concatenated together.

        [p, "bar", "baz", [span, "ooka"]] -> <p>barbaz<span>ooka</span></p>

Exported functions
==================

XML
---

    xmlists:render(List)          -> iolist()
    xmlists:render(List, Options) -> iolist()

    Options = [{encoding, Encoding}]

Converts an "xmlist" to a string (iolist). If Encoding (e.g "UTF-8") is specified the output is preceded by an XML declaration element. For now, that is the only thing the encoding option does.

HTML
----

    htmlists:render(List)          -> iolist()
    htmlists:render(List, Options) -> iolist()

    Options = [{encoding, Encoding} | {doctype, DocType}]
    DocType = strict | transitional | frameset | html5

A thin wrapper around xmlists that can optionally preceded the output with a doctype and wrap the content in an html root element.

It also knows which empty HTML elements are valid to self-close. For example:

    [br] -> <br />
    [p]  -> <p></p>

Output
======

The output is an iolist, which looks messy in itself.

    1> XMList = [foo, [bar]].
    [foo,[bar]]
    2> xmlists:render(XMList).
    [[60,"foo",[],62],[[60,"bar",[],32,47,62]],60,47,"foo",62]

But most functions that write output to the screen, files or sockets will take an iolist as input.

    3> io:format(xmlists:render(XMList)).
    <foo><bar /></foo>
