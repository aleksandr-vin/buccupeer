-module(xml).

-export([gen/1]).

%% =================================================
%% API
%% =================================================

gen(Tree) ->
    Printer = tree(new_printer(), Tree),
    format(Printer).

%% =================================================
%% Internals
%% =================================================

new_printer() ->
    [].

format(Printer) ->
    erlang:list_to_binary(lists:flatten(io_lib:format("~s", [lists:reverse(Printer)]))).

print(P, Format, Args) ->
    [io_lib:format(Format, Args)|P].

tree(P, []) ->
    P;
tree(P, [Tag|Rest]) when is_atom(Tag) orelse
			 is_tuple(Tag) ->
    fragment(P, Tag, Rest);
tree(P, Element) ->
    fragment(P, Element, []).

fragment(P, Text = [Char|_], Rest) when is_integer(Char) ->
    childs(print(P, "~s", [to_string(Text)]), Rest);
fragment(P, Bin, Rest) when is_binary(Bin) ->
    childs(print(P, "~s", [to_string(Bin)]), Rest);
fragment(P, Tag, Childs) ->
    case Childs of
	[] ->
	    element(P, Tag, closed);
	Rest ->
	    P2 = element(P, Tag, open),
	    P3 = childs(P2, Rest),
	    element(P3, Tag, close)
    end.

childs(P, []) ->
    P;
childs(P, [First|Rest]) ->
    childs(tree(P, First), Rest).

element(P, Element, close) ->
    P2 = print(P, "</", []),
    P3 = element_name(P2, Element),
    print(P3, ">", []);
element(P, Element, Type) ->
    P2 = print(P, "<", []),
    P3 = element_name(P2, Element),
    P4 = element_args(P3, Element),
    print(P4,
	  case Type of
	      closed -> " />";
	      open -> " >"
	  end,
	  []).

element_name(P, {N, _}) ->
    element_name(P, N);
element_name(P, N) ->
    print(P, "~s", [norm_atom(N)]).

element_args(P, {_, A}) ->
    lists:foldl(fun ({Name, Value}, PAcc) ->
			print(PAcc, " ~s=\"~s\"", [norm_atom(Name), to_string(Value)]);
		    (Name, PAcc) ->
			print(PAcc, " ~s", [norm_atom(Name)])
		end,
		P, A);
element_args(P, _) ->
    P.

norm_atom(A) when is_atom(A) ->
    case erlang:atom_to_list(A) of
	[$'|T] -> T -- "'";
	S -> S
    end.

to_string(V) when is_integer(V) -> erlang:integer_to_string(V);
to_string(V) when is_binary(V) -> erlang:binary_to_list(V);
to_string(V) when is_atom(V) -> erlang:atom_to_list(V);
to_string(V) when is_list(V) -> V.

%% =================================================
%% Tests
%% =================================================

-include_lib("eunit/include/eunit.hrl").

gen_test_() ->
    Tests = [{<<"<a ><b ><c /></b></a>">>,
	      [a, [b, [c]]]},

	     {<<"<a b=\"xxx\" c ><a /><b /><c /></a>">>,
	      [{a, [{b, "xxx"}, c]},
	       a, b, c]},

	     {<<"<a b=\"foo\" >Some text!</a>">>,
	      [{a, [{b, "foo"}]},
	       "Some text!"]},

	     {<<"<a b=\"foo\" >Some binary text!</a>">>,
	      [{a, [{b, "foo"}]},
	       <<"Some binary text!">>]}],
    
    [{V, ?_assertEqual(V, gen(R))} || {V, R} <- Tests].

special_atoms_test_() ->
    Tests = [{<<"<div />">>,
	     ['div']}],

    [{V, ?_assertEqual(V, gen(R))} || {V, R} <- Tests].
