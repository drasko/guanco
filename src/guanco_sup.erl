%%%-------------------------------------------------------------------
%% @doc guanco top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(guanco_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    PoolSize = application:get_env(guanco, pool_size, 5),
    MaxOverflow = application:get_env(guanco, max_overflow, 10),

    WorkerPoolConfig = [
        {name, guanco_worker_pool},
        {worker_module, guanco_worker},
        {size, PoolSize},
        {max_overflow, MaxOverflow}
    ],

    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},

    %% Worker specification
    ChildSpecs = [
        wpool:child_spec(guanco_worker_pool, WorkerPoolConfig)
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
