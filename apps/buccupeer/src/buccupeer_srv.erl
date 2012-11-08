%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012, Aleksandr Vinokurov
%%% @doc
%%%
%%% @end
%%% Created :  8 Nov 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(buccupeer_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, list_disks/0, add_job/1, remove_job/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("log.hrl").

-define(SERVER, ?MODULE). 

-record(state, {}).

-type disk_info_prop() :: {jobs, [job()]}.

-type disk_info() :: list(). % [buccupeer_disk_info, [disk_info_prop()]].

-type job() :: job_opts().

-type job_ref() :: term().

-type job_opts() :: [job_opt()].

-type job_opt() :: {src, file:filename()} |
		   {dest, file:filename()} |
		   {copies, integer()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of drives available for backupping with backup
%% jobs opts stored on them
%%
%% @end
%%--------------------------------------------------------------------
-spec list_disks() -> [disk_info()] | {error, Error :: any()}.
list_disks() ->
    [{"H:", []}].

%%--------------------------------------------------------------------
%% @doc
%% Adds a backup job
%%
%% The job record is saved to the meta-file on the disk used for
%% this job.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_job(Opts :: job_opts()) -> job_ref() | {error, Error :: any()}.
add_job(Opts) ->
    gen_server:call(?SERVER, {add_job, Opts}).

%%--------------------------------------------------------------------
%% @doc
%% Removes a backup job from the server
%%
%% The job record is removed from the meta-file stored on the disk
%% used for this job. No backups will be removed from the disk.
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_job(job_ref()) -> job_opts() | {error, Error :: any()}.
remove_job(_JobRef) ->
    {error, 'not-implemented'}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_job, Opts}, _From, State) ->
    Reply = {job, make_ref()},
    case disk_name(Opts) of
	undefined ->
	    {reply, {error, 'disk-name-undefined'}, State};
	DiskName ->
	    DiskInfo1 =
		case read_disk_info(DiskName) of
		    undefined ->
			?info("No disk info found for ~s", [DiskName]),
			new_disk_info(Opts);
		    {error, Why1} ->
			?error("Can't read current disk info for ~s: ~p", [DiskName, Why1]),
			?warning("Replacing with new disk info"),
			new_disk_info(Opts);
		    DiskInfo0 ->
			?info("Disk info found for ~s", [DiskName]),
			DiskInfo0
		end,
	    DiskInfo2 = merge_disk_info(DiskInfo1, Opts),
	    case write_disk_info(DiskName, DiskInfo2) of
		{error, Why2} = E ->
		    ?error("Can't write new disk info for ~s: ~p", [DiskName, Why2]),
		    {reply, E, State};
		ok ->
		    {reply, Reply, State}
	    end
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Return disk name of the 'dest' path of the job opts 
-spec disk_name(job_opts()) -> string() | undefined.
disk_name(Opts) ->
    case proplists:get_value(dest, Opts) of
	undefined ->
	    undefined;
	Path ->
	    case filename:pathtype(Path) of
		absolute ->
		    hd(filename:split(Path));
		_ ->
		    undefined
	    end
    end.

%% Return new disk info
-spec new_disk_info(job_opts()) -> disk_info().
new_disk_info(Opts) ->
    [buccupeer_disk_info, [{jobs, [Opts]}]].

%% Return disk info read from named disk
-spec read_disk_info(DiskName :: string()) ->
			    disk_info() |
			    undefined |
			    {error, Error :: any()}.
read_disk_info(DiskName) ->
    MetaFile = metafile(DiskName),
    case file:consult(MetaFile) of
	{ok, Terms} ->
	    case is_disk_info(Terms) of
		true ->
		    Terms;
		false ->
		    {error, 'not-disk-info'}
	    end;
	Other ->
	    Other
    end.

write_disk_info(DiskName, DiskInfo) ->
    MetaFile = metafile(DiskName),
    Data = lists:map(fun (E) ->
			     io_lib:format("~p.~n", [E])
		     end,
		     DiskInfo),
    file:write_file(MetaFile, Data, []).

merge_disk_info(DiskInfo0, Opts0) ->
    case DiskInfo0 of
	[buccupeer_disk_info, DIOpts1] ->
	    Jobs0 = proplists:get_value(jobs, DIOpts1),
	    JobsSet0 = sets:from_list(lists:map(fun (L) ->
							lists:keysort(1, L)
						end,
						Jobs0)),
	    Opts1 = lists:keysort(1, Opts0),
	    case sets:is_element(Opts1, JobsSet0) of
		true ->
		    ?warning("No merge of disk info needed"),
		    DiskInfo0;
		false ->
		    JobsSet1 = sets:add_element(Opts1, JobsSet0),
		    ?info("New job added during merge"),
		    Jobs1 = sets:to_list(JobsSet1),
		    DIOpts2 = [{jobs, Jobs1} | proplists:delete(jobs, DIOpts1)],
		    [buccupeer_disk_info, DIOpts2]
	    end;
	_ ->
	    {error, 'unknown-disk-info'}
    end.

is_disk_info(Terms) ->
    case Terms of
	[buccupeer_disk_info|_] ->
	    true;
	_ -> false
    end.

metafile(DiskName) ->
    filename:join(DiskName, metafilename()).

metafilename() ->
    ".buccupeer".