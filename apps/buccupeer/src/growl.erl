%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012, Aleksandr Vinokurov
%%% @doc
%%% Growl notification server
%%% @end
%%% Created :  9 Nov 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(growl).

-behaviour(gen_server).

%% API
-export([start_link/0, info/2, warning/2, error/2, notify/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {util = "emacsclient"}).

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
%% Growl an info message
%%
%% @end
%%--------------------------------------------------------------------
-spec info(Title :: iolist(),
	   Message :: iolist()) -> ok | ignore | {error, Error :: any()}.
info(Title, Message) ->
    notify(info, Title, Message).

%%--------------------------------------------------------------------
%% @doc
%% Growl a warning message
%%
%% @end
%%--------------------------------------------------------------------
-spec warning(Title :: iolist(),
	      Message :: iolist()) -> ok | ignore | {error, Error :: any()}.
warning(Title, Message) ->
    notify(warning, Title, Message).

%%--------------------------------------------------------------------
%% @doc
%% Growl an error message
%%
%% @end
%%--------------------------------------------------------------------
-spec error(Title :: iolist(),
	    Message :: iolist()) -> ok | ignore | {error, Error :: any()}.
error(Title, Message) ->
    notify(error, Title, Message).

%%--------------------------------------------------------------------
%% @doc
%% Growl a message
%%
%% @end
%%--------------------------------------------------------------------
-spec notify(Type :: info | warning | error,
	     Title :: iolist(),
	     Message :: iolist()) -> ok | ignore | {error, Error :: any()}.
notify(Type, Title, Message) ->
    gen_server:cast(?SERVER, {notify, Type, Title, Message}).

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
handle_cast({notify, Type, Title, Message}, #state{util = Util} = State1) ->
    {Cmd, State2} = make_cmd(Util, Type, Title, Message, State1),
    io:format("cmd: ~s", [Cmd]),
    os:cmd(lists:flatten(Cmd)),
    {noreply, State2};
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

make_cmd("notifu" = Util, Type, Title, Message, State) ->
    Cmd = io_lib:format("~s /q /d 5000 /t ~s /p ~s /m ~s",
			[Util, notifu_type(Type), Title, Message]),
    {Cmd, State};

make_cmd("emacsclient" = Util, warning, Title, Message, State) ->
    Cmd = io_lib:format("~s --eval \"(warn \\\"%s: %s\\\" \\\"~s\\\" \\\"~s\\\")\"",
			[Util, Title, Message]),
    {Cmd, State};
make_cmd("emacsclient" = Util, error, Title, Message, State) ->
    Cmd = io_lib:format("~s --eval \"(warn \\\"[ERROR] %s: %s\\\" \\\"~s\\\" \\\"~s\\\")\"",
			[Util, Title, Message]),
    {Cmd, State};
make_cmd("emacsclient" = Util, Type, Title, Message, State) ->
    Cmd = io_lib:format("~s --eval \"(message \\\"[%s] %s: %s\\\" \\\"~s\\\" \\\"~s\\\" \\\"~s\\\")\"",
			[Util, Type, Title, Message]),
    {Cmd, State}.

notifu_type(info) -> "info";
notifu_type(warning) -> "warning";
notifu_type(error) -> "error".
