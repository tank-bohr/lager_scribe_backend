-module(lager_scribe_worker).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-behaviour(poolboy_worker).
-export([start_link/1]).

-type thrift_client() :: {tclient, Service :: atom(), Protocol :: any(), SeqId :: any()}.
-record(state, {
    client :: thrift_client()
}).

-include("scribe_types.hrl").
-include("lager_scribe.hrl").

-spec start_link(proplists:proplist()) ->
    {ok, pid()} | ignore | {error, any()}.
start_link(WorkerArgs) ->
    gen_server:start_link(?MODULE, WorkerArgs, []).

%% @private
init(ScribeOptions) ->
    [Host, Port] = [proplists:get_value(Opt, ScribeOptions) || Opt <- [host, port]],
    {ok, Client} = thrift_client_util:new(Host, Port, scribe_thrift, [
        {framed, true},
        {strict_read, false},
        {strict_write, false}
    ]),
    {ok, #state{client=Client}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast({log, Category, Message}, State) ->
    send_to_scribe(Category, Message, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internals

send_to_scribe(Category, Message, State=#state{client=Client}) ->
    LogEntry = #logEntry{
        category = unicode:characters_to_binary(Category),
        message  = unicode:characters_to_binary(Message)
    },
    {Client1, _Result} = thrift_client:call(Client, 'Log', [[LogEntry]]),
    ok = poolboy:checkin(?POOL_NAME, self()),
    {noreply, State#state{client=Client1}}.
