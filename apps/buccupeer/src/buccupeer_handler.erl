%% -*- encoding: utf-8 -*-
%% @doc Basic REST handler.
-module(buccupeer_handler).

-export([init/3]).
-export([content_types_provided/2, resource_exists/2, rest_init/2]).
-export([to_html/2]).

-include("log.hrl").

-record(state, {drives_info = []}).

-opaque state() :: #state{}.
-export_type([state/0]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

resource_exists(Req, State) ->
    {Req, DrivesInfo} = drives_info(Req),
    State2 = State#state{drives_info = DrivesInfo},
    {true, Req, State2}.

content_types_provided(Req, State) ->
	{[
	  {<<"text/html">>, to_html}
	], Req, State}.

to_html(Req, State) ->
	Content = xml:gen(content(State)),
	{Content, Req, State}.

%% =========================================================
%% Internals
%% =========================================================

-spec content(state()) -> xml:tree().
content(State) ->
    [html,
     [head,
      [{meta, [{charset, "utf-8"}]}],
      [title, <<"Buccupeer: локальный бэкап файлов на внешний USB диск">>],
      [{link, [{href, "static/buccupeer.css"}, {media,"screen"},
	       {rel,"stylesheet"}, {type,"text/css"}]}]],
     [body,
      [h1, <<"Buccupeer: локальный бэкап файлов на внешний USB диск">>],
      [hr],
      log(State)]].

-spec configuration(state()) -> xml:tree().
configuration(State) ->
    [{'div', [{id, configuration}]},
     [h3, <<"Доступные диски:">>],
     [ul] ++
      lists:map(fun ({DriveLetter, DriveInfo}) ->
			[li |
			 case DriveInfo of
			     undefined ->
				 [[{a, [{href, "link-showing-DriveLetter-info"}]},
				   DriveLetter]];
			     _ ->
				 [[{a, [{href, "link-closing-DriveLetter-info"}]},
				   DriveLetter],
				  markup(drive_info, DriveInfo)]
			 end]
		end,
		lists:keysort(1, State#state.drives_info))].

-spec log(state()) -> xml:tree().
log(_) ->
    [{'div', [{id, log}]}|
     format_results(buccupeer_srv:last_result())].

format_results({{{Year,Mon,Day},{Hr,Min,Sec}}, {Elapsed, Results}}) ->
    [
     [h2, io_lib:format("Последний бэкап был произведен "
                        "~b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b", [Year,Mon,Day,Hr,Min,Sec])],
     [h3, io_lib:format("Затраченное время: ~pс", [Elapsed / 1000000])],
     [ol | [format_result(X) || X <- Results]]
     ];
format_results(undefined) ->
    [h2, <<"Нет информации о последнем бэкапе">>].

format_result({{job,JobRef},Result,Descr}) ->
    [{'div', [{id, job_result}]},
     io_lib:format("Задача #~p: ~p -- ~p", [JobRef, Result, Descr])
    ];
format_result([]) ->
    [].

-type drive_info() :: term().

%% Finds drives available now, and lists info on ones selected by user.
%% Silently skips unavailable but user selected drives.
-spec drives_info(Req :: cowboy_req:req()) ->
			 {cowboy_req:req(),
			  Drives :: [{string(), drive_info() | undefined}]}.
drives_info(Req) ->
    {Req, buccupeer_srv:list_disks()}.

%% Markup code fragments
-spec markup(Section :: atom(), Data :: term()) -> xml:tree().
markup(drive_info, Props) ->
    PropsMap = [{note, <<"Заметка">>}],
    [{'div', [{class, "drive_info"}]}] ++
	[
	 ['div',
	  [{'div', [{class, "property_name"}]},
	   TextName],
	  <<":&nbsp;">>,
	  [{'div', [{class, "property_value"}]},
	   proplists:get_value(Name, Props)]]
	 || {Name, TextName} <- PropsMap].
