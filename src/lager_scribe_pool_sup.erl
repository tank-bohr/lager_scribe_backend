-module(lager_scribe_pool_sup).

-export([start_link/2]).

-behaviour(supervisor).
-export([init/1]).

-include("lager_scribe.hrl").

-define(SERVER, ?MODULE).

start_link(ScribeHost, ScribePort) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ScribeHost, ScribePort]).

%% @private
init([ScribeHost, ScribePort]) ->
    Name = ?POOL_NAME,
    WorkerArgs = [ScribeHost, ScribePort],
    PoolArgs = [
        {name, {local, Name}},
        {worker_module, lager_scribe_worker},
        {size, 10},
        {max_overflow, 20}
    ],
    ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, [ChildSpec]}}.
