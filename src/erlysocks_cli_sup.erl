-module(erlysocks_cli_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Socket) ->
        supervisor:start_child(?MODULE, [Socket]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Childs = [{erlysocks_cli, worker}],
    {ok, {{simple_one_for_one, 60, 120}, [?CHILD(I, Type) || {I, Type} <- Childs]}}.

