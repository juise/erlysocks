-module(erlysocks_app).

-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    application:start(erlysocks).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erlysocks_sup:start_link().

stop(_State) ->
    ok.
