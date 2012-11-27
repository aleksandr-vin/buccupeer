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
-export([start_link/0, list_disks/0, add_job/1, remove_job/1,
	 run_jobs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("log.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

-record(job_run_state, {disk_name,
                        src, dest, copies,
			backup_dir}).

-type disk_name() :: string().

-type disk_info_prop() :: {jobs, [job()]}.

-type disk_info() :: list(). % [buccupeer_disk_info, [disk_info_prop()]].

-type job() :: job_opts().

-type job_ref() :: term().

-type job_opts() :: [job_opt()].

-type job_opt() :: {src, file:filename()} |
		   {dest, file:filename()} |
		   {copies, integer()}.

-type job_report() :: {job_ref(),
		       ok,
		       Info :: any()} |
		      {job_ref(),
		       {error, Error :: any()},
		       Info :: any()}.

-type removed_jobs_on_disk() :: {disk_name(), [job()]}.

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
-spec list_disks() -> [{disk_name(), disk_info()}] | {error, Error :: any()}.
list_disks() ->
    gen_server:call(?SERVER, list_disks).

%%--------------------------------------------------------------------
%% @doc
%% Adds a backup job
%%
%% The job record is saved to the meta-file on the disk used for
%% this job.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_job(Opts :: job_opts()) -> {ok, job_ref()} |
				     {'already-presents', job_ref()} |
				     {error, Error :: any()}.
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
-spec remove_job(job_ref()) -> [removed_jobs_on_disk()] |
			       'not-found' |
			       {error, Error :: any()}.
remove_job(JobRef) ->
    gen_server:call(?SERVER, {remove_job, JobRef}).

%%--------------------------------------------------------------------
%% @doc
%% Run jobs on disk if available
%%
%% @end
%%--------------------------------------------------------------------
-spec run_jobs(disk_name()) -> [job_report()] |
			       'not-found' |
			       {error, Error :: any()}.
run_jobs(DiskName) ->
    gen_server:call(?SERVER, {run_job, DiskName}).

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
handle_call(list_disks, _From, State) ->
    ?debug("Listing disks", []),
    Disks = list_disks_names(),
    Result = lists:map(fun (DiskName) ->
                               case read_disk_info(DiskName) of
                                   undefined ->
                                       ?debug("No disk info found for '~s'", [DiskName]),
                                       {false, DiskName};
                                   {error, enoent} = E ->
                                       ?debug("Can't read current disk info for '~s', "
                                              "skipping enoent error, "
                                              "assuming no drive attached",
                                              [DiskName]),
                                       {E, DiskName};
                                   {error, Why1} = E ->
                                       ?error("Can't read current disk info for '~s': ~p",
                                              [DiskName, Why1]),
                                       {E, DiskName};
                                   DiskInfo ->
                                       ?info("Disk info found for '~s'", [DiskName]),
                                       ?debug("Disk info: ~p", [DiskInfo]),
                                       {true, {DiskName, DiskInfo}}
                               end
		       end,
		       Disks),
    Reply = proplists:get_all_values(true, Result),
    {reply, Reply, State};
handle_call({add_job, Opts}, _From, State) ->
    ?debug("Adding job ~p", [Opts]),
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
			?debug("Current disk info: ~p", [DiskInfo0]),
			DiskInfo0
		end,
	    case merge_disk_info(DiskInfo1, Opts) of
		{ok, DiskInfo2} ->
		    case write_disk_info(DiskName, DiskInfo2) of
			{error, Why2} = E ->
			    ?error("Can't write new disk info for ~s: ~p", [DiskName, Why2]),
			    {reply, E, State};
			ok ->
			    JobRef = make_job_ref(Opts),
			    ?info("New job ~p: ~p", [JobRef, Opts]),
			    {reply, {ok, JobRef}, State}
		    end;
		'not-needed' ->
		    JobRef = make_job_ref(Opts),
		    ?info("Job ~p: ~p", [JobRef, Opts]),
		    {reply, {'already-presents', JobRef}, State};
		{error, Why3} = E->
		    ?error("Can't merge disk infos: ~p", [Why3]),
		    {reply, E, State}
	    end
    end;

handle_call({remove_job, JobRef}, _From, State) ->
    ?debug("Removing job ~p", [JobRef]),
    Disks = list_disks_names(),
    Result = lists:map(fun (DiskName) ->
			       case find_and_remove_job(DiskName, JobRef) of
				   false ->
				       {false, DiskName};
				   RemovedJobs ->
				       {true, {DiskName, RemovedJobs}}
			       end
		       end,
		       Disks),
    Reply = case proplists:get_all_values(true, Result) of
		[] -> 'not-found';
		List -> List
	    end,
    {reply, Reply, State};

handle_call({run_job, DiskName}, _From, State) ->
    ?debug("Trying to run jobs on disk ~s", [DiskName]),
    Reply =
	case read_disk_info(DiskName) of
	    undefined ->
		?info("No disk info found for '~s'", [DiskName]);
	    {error, Why1} = E ->
		?error("Can't read current disk info for '~s': ~p", [DiskName, Why1]),
		E;
	    DiskInfo ->
		?info("Disk info found for '~s'", [DiskName]),
		?debug("Disk info: '~p'", [DiskInfo]),
		run_jobs_by_disk_info(DiskName, DiskInfo)
	end,
    {reply, Reply, State};

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


-spec merge_disk_info(disk_info(), job()) -> 'not-needed' |
					     {ok, NewDiskInfo :: disk_info()} |
					     {error, Error :: any()}.
merge_disk_info(DiskInfo0, Opts0) ->
    true = is_disk_info(DiskInfo0),
    [_, DIOpts1] = DiskInfo0,
    Jobs00 = proplists:get_value(jobs, DIOpts1),
    Jobs0 = normalize_jobs(Jobs00),
    JobsSet0 = sets:from_list(lists:map(fun (L) ->
						lists:keysort(1, L)
					end,
					Jobs0)),
    Opts1 = lists:keysort(1, Opts0),
    case sets:is_element(Opts1, JobsSet0) of
	true ->
	    ?warning("No merge of disk info needed"),
	    'not-needed';
	false ->
	    JobsSet1 = sets:add_element(Opts1, JobsSet0),
	    ?info("New job added during merge"),
	    Jobs1 = sets:to_list(JobsSet1),
	    DIOpts2 = [{jobs, Jobs1} | proplists:delete(jobs, DIOpts1)],
	    {ok, [buccupeer_disk_info, DIOpts2]}
    end.

-spec remove_job(disk_info(), job_ref()) -> 'not-found' |
					    {ok,
					     NewDiskInfo :: disk_info(),
					     RemovedJobs :: [job()]} |
					    {error, Error :: any()}.
remove_job(DiskInfo0, JobRef) ->
    true = is_disk_info(DiskInfo0),
    [_, DIOpts1] = DiskInfo0,
    Jobs00 = proplists:get_value(jobs, DIOpts1),
    Jobs0 = normalize_jobs(Jobs00),
    {Kept,Removed} =
	lists:foldl(fun (Job, {Kept,Removed}) ->
			    case make_job_ref(Job) of
				JobRef -> {Kept, [Job|Removed]};
				_ -> {[Job|Kept], Removed}
			    end
		    end,
		    {[],[]},
		    Jobs0),
    case {Kept,Removed} of
	{_,[]} ->
	    'not-found';
	{Kept,Removed} ->
	    ?info("Jobs removed: ~p", [Removed]),
	    DIOpts2 = [{jobs, Kept} | proplists:delete(jobs, DIOpts1)],
	    {ok, [buccupeer_disk_info, DIOpts2], Removed}
    end.

normalize_jobs(Jobs) ->
    lists:map(fun normalize_job/1,
	      Jobs).

normalize_job(Opts) ->
    case is_case_sensitive_filesystem(disk_name(Opts)) of
	true ->
	    Opts
    end.

is_case_sensitive_filesystem(_) ->
    %% Not implemented, that's why
    %% any filesystem is case sensitive for now
    true.

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

make_job_ref(Opts) ->
    case proplists:get_value(dest, Opts) of
	Path = [H|_] when is_integer(H) andalso
			  0 < H andalso
			  H < 255 ->
	    case disk_name(Opts) of
		undefined ->
		    error('bad-job-opts');
		DiskName ->
		    {job, erlang:phash2({Path, DiskName})}
	    end
    end.

find_and_remove_job(DiskName, JobRef) ->
    case read_disk_info(DiskName) of
	undefined ->
	    false;
	{error, _} = E ->
	    E;
	DiskInfo0 ->
	    true = is_disk_info(DiskInfo0),
	    case remove_job(DiskInfo0, JobRef) of
		'not-found' ->
		    false;
		{error, _} = E ->
		    E;
		{ok, DiskInfo1, RemovedJobs} ->
		    case write_disk_info(DiskName, DiskInfo1) of
			{error, _} = E ->
			    E;
			ok ->
			    RemovedJobs
		    end
	    end
    end.

-spec run_jobs_by_disk_info(disk_name(), disk_info()) -> [job_report()] |
                                                         {error, Error :: any()}.
run_jobs_by_disk_info(DiskName, DiskInfo) ->
    true = is_disk_info(DiskInfo),
    [_, DIOpts] = DiskInfo,
    Jobs0 = proplists:get_value(jobs, DIOpts),
    Jobs = normalize_jobs(Jobs0),
    lists:map(fun (Job) -> run_job(DiskName, Job) end, Jobs).

run_job(DiskName, Job) when is_list(DiskName) ->
    ?debug("Running job: ~p", [Job]),
    [Src, Dest, Copies] =
	lists:map(fun (N) ->
			  proplists:get_value(N, Job)
		  end,
		  [src, dest, copies]),
    State = #job_run_state{disk_name = DiskName, src = Src, dest = Dest, copies = Copies},
    case run_job(State, 'base-dest') of
	{error, _} = E ->
	    {make_job_ref(Job), E, []};
	{Other, Info} ->
	    {make_job_ref(Job), Other, Info}
    end;

run_job(State0, 'base-dest') ->
    {BaseDest, State1} = base_dest_dir(State0),
    case filelib:ensure_dir(BaseDest) of
	{error, _} = E ->
	    {E, 'base-dest-dir-error'};
	ok ->
	    ?debug("Base destination directory exists: ~p", [BaseDest]),
	    run_job(State1, 'backup-dir')
    end;

run_job(State0, 'backup-dir') ->
    {BackupDir, State1} = backup_dir(State0),
    case filelib:ensure_dir(BackupDir) of
	{error, _} = E ->
	    {E, 'backup-dir-error'};
	ok ->
	    ?debug("Backup destination directory exists: ~p", [BackupDir]),
	    run_job(State1, 'decide-to-backup')
    end;

run_job(State0, 'decide-to-backup') ->
    case is_backup_needed(State0) of
	{{error, _} = E, _State1} ->
	    {E, 'decide-to-backup-error'};
        {'not-needed', _State1} ->
            ?debug("Decided not to backup now"),
            {'not-needed', "Decided not to backup now"};
	{'needed', State1} ->
	    ?debug("Decided to backup"),
	    run_job(State1, 'rotate-copies')
    end;

run_job(State0, 'rotate-copies') ->
    {BackupDir, State1} = backup_dir(State0),
    case rotate_backupfile(BackupDir ++ "copy", State1#job_run_state.copies) of
	{error, _} = E ->
	    {E, 'rotate-copies-error'};
	ok ->
            ?debug("Backups rotated"),
	    run_job(State1, 'new-copy')
    end;

run_job(State0, 'new-copy') ->
    {BackupName, State1} = backup_name(State0),
    Src = State0#job_run_state.src,
    case backupfile(Src, BackupName) of
	{error, _} = E ->
	    {E, 'new-copy-error'};
	{ok, BytesCopied} ->
            ?debug("New copy saved (~p bytes)", [BytesCopied]),
	    run_job(State1, 'commit')
    end;

run_job(_State0, 'commit') ->
    {ok, 'complete'};

run_job(_State0, NotImplemented) ->
    {ok, {'not-implemented', NotImplemented}}.

base_dest_dir(State0) ->
    DiskName = State0#job_run_state.disk_name,
    Dest0 = filename:split(State0#job_run_state.dest),
    Path = [DiskName,
            case filename:pathtype(Dest0) of
                volumerelative ->
                    filename:join([Dest0]);
                relative ->
                    ["/", filename:join([Dest0])];
                absolute ->
                    ["/", tl(filename:split(Dest0))]
            end,
            "/"],
    Dest1 = lists:flatten(Path),
    {Dest1, State0#job_run_state{dest = Dest1}}.

backup_dir(State0) ->
    Last = hd(lists:reverse(filename:split(State0#job_run_state.src))),
    Path = filename:join([State0#job_run_state.dest, Last])
	++ "/",
    {Path, State0#job_run_state{backup_dir = Path}}.

backup_name(State0) ->
    {BackupDir, State1} = backup_dir(State0),
    {BackupDir ++ "copy", State1}.

is_backup_needed(State0) ->
    Src = State0#job_run_state.src,
    case most_last_modified(Src) of
	undefined ->
	    ?warning("Source path not found: ~p", [Src]),
	    {'not-needed', State0};
	SrcMostLastModTime ->
	    {BackupName, State1} = backup_name(State0),
	    case most_last_modified(BackupName) of
		undefined ->
		    ?info("No latest backups found on ~p", [BackupName]),
		    {'needed', State1};
		LastBackupMostLastModTime ->
		    case calendar:time_difference(LastBackupMostLastModTime,
						  SrcMostLastModTime) of
			{D, _} when D < 0 ->
			    ?info("No modification time differences found "
				  "between source and last backup"),
			    {'not-needed', State1};
			_ ->
			    ?info("Modification time differences found "
				  "between source and last backup"),
			    {'needed', State1}
		    end
	    end
    end.

most_last_modified(Path) ->
    Times =
	fold_files(Path,
		   fun (point, file, Name, Acc) ->
%%			   ?debug(">>>> point file: ~p", [Name]),
			   [{filelib:last_modified(Name), Name}|Acc];
		       (open, dir, Name, Acc) ->
%%			   ?debug(">>>> open dir: ~p", [Name]),
			   [{filelib:last_modified(Name), Name}|Acc];
		       (close, dir, _Name, Acc) ->
%%			   ?debug(">>>> close dir: ~p", [_Name]),
			   Acc;
		       (point, other, _Name, Acc) ->
			   ?debug(">>>> point other: ~p", [_Name]),
			   Acc
		   end,
		   []),
    case length(Times) of
	0 -> undefined;
	_ ->
	    lists:foldl(fun ({TimeA, _}, TimeB) ->
				case calendar:time_difference(TimeA, TimeB) of
				    {D, _} when D < 0 -> TimeA;
				    _ -> TimeB
				end
			end,
			element(1, hd(Times)),
			Times)
    end.

%% Recursively delete directories
-spec fold_files(Path :: string(),
		 Fun :: fun((open | close | point,
			     file | dir | other,
			     Name :: string(),
			     Acc0 :: any()) -> Acc1 :: any()),
		 AccIn :: any()) -> AccOut :: any().
fold_files(Path, Fun, AccIn) ->
    case file:list_dir(Path) of
	{ok, Files} ->
	    AccIn1 = Fun(open, dir, Path, AccIn),
	    AccIn2 = lists:foldl(fun (File, Acc0) ->
					 fold_files(Path, File, Fun, Acc0)
				 end,
				 AccIn1,
				 Files),
	    Fun(close, dir, Path, AccIn2);
	_ ->
	    Fun(point, other, Path, AccIn)
    end.

-spec fold_files(Path :: string(),
		 File :: string(),
		 Fun :: fun((open | close | point,
			     file | dir | other,
			     Name :: string(),
			     Acc0 :: any()) -> Acc1 :: any()),
		 AccIn :: any()) -> AccOut :: any().
fold_files(Path, File, Fun, AccIn) ->
    FilePath = filename:join(Path, File),
    case filelib:is_dir(FilePath) of
        true  ->
            fold_files(FilePath, Fun, AccIn);
        false ->
            case filelib:is_file(FilePath) of
                true  ->
                    Fun(point, file, FilePath, AccIn);
                false ->
                    Fun(point, other, FilePath, AccIn)
            end
    end.    

%% renames and deletes failing are OK
rotate_backupfile(File, 0) ->
    delete_file_or_dir(File);
rotate_backupfile(File, 1) ->
    _Result = rename_dir(File, File++".0"),
    ?debug("Renaming on rotate: ~p", [_Result]),
    rotate_backupfile(File, 0);
rotate_backupfile(File, Count) ->
    _Result = rename_dir(File ++ "." ++ integer_to_list(Count - 2), File ++ "." ++
        integer_to_list(Count - 1)),
    ?debug("Renaming on rotate: ~p", [_Result]),
    rotate_backupfile(File, Count - 1).

rename_dir(Src, Dest) ->
    ok = delete_file_or_dir(Dest),
    file:rename(Src, Dest).

delete_file_or_dir(File) ->
    case file:delete(File) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, eperm} ->
            case recursive_delete(File) of
                {ok, _Path} ->
                    ok;
                Other -> Other
            end;
        Other -> Other
    end.

backupfile(Src, Dest) ->
    case file:copy(Src, Dest) of
        {error, eisdir} ->
            backupdir(Src, Dest);
        Other -> Other
    end.

backupdir(Src, Dest) ->
    case recursive_copy(Src, Dest) of
        {Result, _From} ->
            Result;
        Other -> Other
    end.

%% Recursively copy directories
-spec recursive_copy(list(), list()) -> ok.
recursive_copy(From, To) ->
    {ok, Files} = file:list_dir(From),
    Log = [rec_copy(From, To, X) || X <- Files],
    BytesTotal =
        lists:foldl(fun ({{ok, V}, _Name}, Acc) when is_integer(V) ->
                            ?debug("Copied ~p bytes: ~p", [V, _Name]),
                            V + Acc;
                        ({_Reason, _Name}, Acc) ->
                            ?debug("Not copied (~p): ~p ", [_Reason, _Name]),
                            Acc
                    end,
                    0,
                    Log),
    {{ok, BytesTotal}, From}.

-spec rec_copy(list(), list(), list()) -> {ok, list()} | {skipped, list()}.
rec_copy(From, To, File) ->
    NewFrom = filename:join(From, File),
    NewTo   = filename:join(To, File),
    case filelib:is_dir(NewFrom) of
        true  ->
            ok = filelib:ensure_dir(NewTo),
            recursive_copy(NewFrom, NewTo);
        false ->
            case filelib:is_file(NewFrom) of
                true  ->
                    ok = filelib:ensure_dir(NewTo),
                    {file:copy(NewFrom, NewTo), NewFrom};
                false ->
                    {skipped, NewFrom}
            end
    end.

%% Recursively delete directories
-spec recursive_delete(list()) -> ok.
recursive_delete(From) ->
    {ok, Files} = file:list_dir(From),
    Log = [rec_delete(From, X) || X <- Files],
    ?debug("Recursive deletion result: ~p", [Log]),
    {file:del_dir(From), From}.

-spec rec_delete(list(), list()) -> {ok, list()} |
                                    {skipped, list()} |
                                    {{error, any()}, list()}.
rec_delete(From, File) ->
    NewFrom = filename:join(From, File),
    case filelib:is_dir(NewFrom) of
        true  ->
            recursive_delete(NewFrom);
        false ->
            case filelib:is_file(NewFrom) of
                true  ->
                    {file:delete(NewFrom), NewFrom};
                false ->
                    {skipped, NewFrom}
            end
    end.


list_disks_names() ->
    {ok, RH} = win32reg:open([read]),
    Result =
        try
            ok = win32reg:change_key(RH, "\\hklm\\SYSTEM\\MountedDevices"),
            list_disks_names(RH)
        after
            ok = win32reg:close(RH)
        end,
    Result.

list_disks_names(RegHandle) ->
    {ok, Values} = win32reg:values(RegHandle),
    lists:foldl(fun match_reg_value/2, [], Values).

match_reg_value({"\\DosDevices\\" ++ DiskName, _Value}, Acc) -> [DiskName | Acc];
match_reg_value(_, Acc) -> Acc.
