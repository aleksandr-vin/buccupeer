-module(buccupeer_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Utils
-export([get_env/2]).

-include("log.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = start_winmeserl_stuff(),
    ok = start_cowboy_stuff(),
    buccupeer_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internals
%% ===================================================================

get_env(Name, Default) ->
    case application:get_env(buccupeer, Name) of
	undefined ->
	    Default;
	{ok, V} -> V
    end.

start_winmeserl_stuff() ->
    ?info("Starting winmeserl stuff from buccupeer"),
    case buccupeer_winmeserl_handler:add_handler() of
        ok ->
            ?info("WM_DEVICECHANGE event handler added"),
            ok;
        E ->
            ?error("WM_DEVICECHANGE event handler not added: ~p", [E]),
            exit(E)
    end.

start_cowboy_stuff() ->
    ?info("Starting cowboy stuff from buccupeer"),
    Dispatch = [
		%% {URIHost, list({URIPath, Handler, Opts})}
		{'_', [{[<<"static">>, '...'], cowboy_static,
			[{directory, {priv_dir, buccupeer, [<<"www">>]}},
			 {mimetypes, [{<<".css">>, [<<"text/css">>]},
				      {<<".js">>, [<<"application/javascript">>]}]}]},
		       {'_', buccupeer_handler, []}]}
	       ],

    Port = get_env(port, 80),

    %% Name, NbAcceptors, TransOpts, ProtoOpts
    {ok, _} = cowboy:start_http(buccupeer_listener, 1,
				[{port, Port}],
				[{dispatch, Dispatch}]),
    ok.
