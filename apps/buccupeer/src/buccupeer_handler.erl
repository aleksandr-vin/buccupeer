%% -*- encoding: utf-8 -*-
%% @doc Basic REST handler.
-module(buccupeer_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([to_html/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
	  {<<"text/html">>, to_html}
	], Req, State}.

to_html(Req, State) ->
	Content = content(),
	{Content, Req, State}.

%% =========================================================
%% Internals
%% =========================================================

content() ->
    <<"<html><body><h1>Write HTML here!</h1></body></html>">>.
