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
	Content = xml:gen(content()),
	{Content, Req, State}.

%% =========================================================
%% Internals
%% =========================================================

content() ->
    [html,
     [head,
      [{meta, [{charset, "utf-8"}]}],
      [title, <<"Buccupeer: локальный бэкап файлов на внешний USB диск">>],
      [{link, [{href, "static/buccupeer.css"}, {media,"screen"},
	       {rel,"stylesheet"}, {type,"text/css"}]}]],
     [body,
      [h1, <<"Buccupeer: локальный бэкап файлов на внешний USB диск">>],
      [hr],
      [h2, <<"Настройки">>],
      configuration(),
      [hr],
      [h2, <<"Лог">>],
      log()]].

configuration() ->
    [{'div', [{id, configuration}]},
     <<"Здесь будут настройки">>].

log() ->
    [{'div', [{id, log}]},
     <<"Здесь будут ссылки на лог-файлы">>].
