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

    WorkerPoolConfig = [
        {worker, {guanco_worker, []}},
        {workers, PoolSize} %% Correct option for setting worker count
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
