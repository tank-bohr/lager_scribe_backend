-module(lager_scribe_backend).

%% lager_scribe_backend: lager_scribe_backend library's entry point.
-behaviour(gen_event).
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type thrift_client() :: {tclient, Service :: atom(), Protocol :: any(), SeqId :: any()}.

-record(state, {
    level  :: {'mask', integer()},
    client :: thrift_client(),
    scribe_host :: string(),
    scribe_port :: non_neg_integer()
}).

-include("scribe_types.hrl").

%% API
%% gen_event callbacks

init(Options) ->
    LevelConfig = proplists:get_value(level, Options, debug),
    ScribeHost  = proplists:get_value(scribe_host, Options, "localhost"),
    ScribePort  = proplists:get_value(scribe_port, Options, 1463),
    {ok, Client} = connect_to_scribe(ScribeHost, ScribePort),
    case validate_loglevel(LevelConfig) of
        false ->
            {error, {fatal, bad_loglevel}};
        Level ->
            {ok, #state{
                level = Level,
                client = Client,
                scribe_host = ScribeHost,
                scribe_port = ScribePort
            }}
    end.


handle_event({log, Message}, State) ->
    log_message(Message, State);
handle_event(_Event, State) ->
    {ok, State}.

handle_call({set_loglevel, LevelConfig}, State) ->
    case validate_loglevel(LevelConfig) of
        false ->
            {ok, {error, bad_loglevel}, State};
        Level ->
            {ok, ok, State#state{level=Level}}
    end;
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call(_Request, _State) ->
    {remove_handler, {error, unknown_call}}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internals

log_message(Message, State=#state{level=Level}) ->
    case lager_util:is_loggable(Message, Level, ?MODULE) of
        true ->
            send_to_scribe(Message, State);
        false ->
            {ok, State}
    end.

send_to_scribe(Message, State=#state{client=Client}) ->
    MessageText = lists:flatten(Message:message()),
    Category    = log_category(Message:severity()),
    LogEntry = #logEntry{
        category = unicode:characters_to_binary(Category),
        message  = unicode:characters_to_binary(MessageText)
    },
    {Client1, _Result} = thrift_client:call(Client, 'Log', [[LogEntry]]),
    {ok, State#state{client=Client1}}.

connect_to_scribe(Host, Port) ->
    thrift_client_util:new(Host, Port, scribe_thrift, [
        {framed, true},
        {strict_read, false},
        {strict_write, false}
    ]).

validate_loglevel(Level) ->
    try lager_util:config_to_mask(Level) of
        Levels ->
            Levels
    catch
        _:_ ->
            false
    end.

log_category(Severity) ->
    atom_to_list(Severity).

%% End of Module.
