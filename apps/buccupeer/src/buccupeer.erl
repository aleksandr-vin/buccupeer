-module(buccupeer).

%% Application callbacks
-export([start/0, stop/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

app_deps() ->
    [compiler,
     buccupeer].

start() ->
    ok = lists:foreach(fun application:start/1,
		       app_deps()).

stop() ->
    ok = lists:foreach(fun application:stop/1,
		       lists:reverse(app_deps())).
