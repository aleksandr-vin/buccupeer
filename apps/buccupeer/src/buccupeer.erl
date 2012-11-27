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
    JobsResults = [buccupeer_srv:run_jobs(Disk) || {Disk, _} <- BackupDisks],
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
