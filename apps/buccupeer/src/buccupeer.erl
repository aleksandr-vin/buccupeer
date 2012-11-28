-module(buccupeer).

%% Application callbacks
-export([start/0, stop/0, run_all/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = lists:foreach(fun application:start/1,
		       app_deps()).

stop() ->
    ok = lists:foreach(fun application:stop/1,
		       lists:reverse(app_deps())).

run_all() ->
    BackupDisks = buccupeer_srv:list_disks(),
    Timeout = infinity,
    JobsResults = [buccupeer_srv:run_jobs(Disk, Timeout) || {Disk, _} <- BackupDisks],
    _ = os:cmd("start http://localhost/last-result"),
    JobsResults.

%% ===================================================================
%% Internals
%% ===================================================================

app_deps() ->
    [compiler,
     crypto,
     ranch,
     cowboy,
     winmeserl,
     buccupeer].
