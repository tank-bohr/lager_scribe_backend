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
    level            :: {'mask', integer()}, %% Debug log level by default
    formatter        :: atom(),
    formatter_config :: any(),
    category         :: atom(),
    scribe_host      :: string(),
    scribe_port      :: non_neg_integer(),
    client           :: thrift_client()
}).

-include("scribe_types.hrl").

%% API
%% gen_event callbacks

init(Options) ->
    LevelConfig     = proplists:get_value(level, Options, debug),
    Formatter       = proplists:get_value(formatter, Options, lager_default_formatter),
    FormatterConfig = proplists:get_value(formatter_config, Options, []),
    ScribeHost      = proplists:get_value(scribe_host, Options, "localhost"),
    ScribePort      = proplists:get_value(scribe_port, Options, 1463),
    {ok, Client} = connect_to_scribe(ScribeHost, ScribePort),
    case validate_loglevel(LevelConfig) of
        false ->
            {error, {fatal, bad_loglevel}};
        Level ->
            {ok, #state{
                level  = Level,
                formatter = Formatter,
                formatter_config = FormatterConfig,
                scribe_host = ScribeHost,
                scribe_port = ScribePort,
                client = Client
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
    MessageText = message_text(Message, State),
    Category    = log_category(Message, State),
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

message_text(Message, #state{formatter=Formatter, formatter_config=FormatConfig}) ->
    Formatter:format(Message, FormatConfig).

log_category(Message, #state{category=undefined}) ->
    lager_scribe_category:default(Message);
log_category(Message, #state{category=Category}) ->
    case erlang:function_exported(Category, category, 1) of
        true  -> Category:category(Message);
        false -> lager_scribe_category:default(Message)
    end.

%% End of Module.
