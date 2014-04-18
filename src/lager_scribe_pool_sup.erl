-module(lager_scribe_pool_sup).

-export([start_link/3]).

-behaviour(supervisor).
-export([init/1]).

-include("lager_scribe.hrl").

-define(SERVER, ?MODULE).

-spec start_link(string(), non_neg_integer(), proplists:proplist()) ->
    {ok, pid()} | ignore | {error, any()}.
start_link(ScribeHost, ScribePort, SizeArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ScribeHost, ScribePort, SizeArgs]).

%% @private
init([ScribeHost, ScribePort, SizeArgs]) ->
    Name = ?POOL_NAME,
    WorkerArgs = [
        {host, ScribeHost},
        {port, ScribePort}
    ],
    PoolArgs = [
        {name, {local, Name}},
        {worker_module, lager_scribe_worker}
    ] ++ SizeArgs,
    ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, [ChildSpec]}}.
