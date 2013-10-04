-module(erlysocks_srv_tests).

-include_lib("eunit/include/eunit.hrl").

-define(APPLICATION, erlysocks).

start_link_test() ->
    [application:set_env(?APPLICATION, K, V) || {K, V} <- [{ip, {0,0,0,0}}, {port, 0}]],
    ?assertMatch({ok, _Pid}, erlysocks_srv:start_link()),
    ?assertMatch({error, {already_started, _Pid}}, erlysocks_srv:start_link()).

