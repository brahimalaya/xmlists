%%% @author Per Melin <p@greendale.se>

-module(htmlists).

-export([render/1,
         render/2]).

-include("htmlists.hrl").

-define(SELF_CLOSING, [img, input, br, link, meta,
                       col, param, frame, area, hr, base, basefont]).

render(XMList) ->
    render(XMList, []).

render(XMList, Options) ->
    DocType  = proplists:get_value(doctype, Options),
    Encoding = proplists:get_value(encoding, Options),
    XmlOptions = [{encoding, Encoding}, {self_closing, ?SELF_CLOSING}],
    xmlists:render(wrap(XMList, DocType), XmlOptions).

wrap(XMList, undefined) ->
    XMList;
wrap(XMList, DocType) ->
    RootElement = [html, [{xmlns, ?XMLNS}], XMList],
    [{raw, doctype(DocType)}|RootElement].

doctype(strict)       -> ?DOCTYPE_STRICT;
doctype(transitional) -> ?DOCTYPE_TRANSITIONAL;
doctype(frameset)     -> ?DOCTYPE_FRAMESET;
doctype(html5)        -> ?DOCTYPE_HTML5.
