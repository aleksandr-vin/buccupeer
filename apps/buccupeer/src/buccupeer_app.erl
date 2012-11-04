-module(buccupeer_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Utils
-export([get_env/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
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
				[{dispatch, Dispatch}]
			       ),
    buccupeer_sup:start_link().

stop(_State) ->
    ok.

get_env(Name, Default) ->
    case application:get_env(buccupeer, Name) of
	undefined ->
	    Default;
	{ok, V} -> V
    end.
