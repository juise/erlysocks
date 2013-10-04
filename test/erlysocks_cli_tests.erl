-module(erlysocks_cli_tests).

-include_lib("eunit/include/eunit.hrl").

-define(IP4, <<93,184,216,119>>).
-define(rIP4, {93,184,216,119}).
-define(IP6, <<38,6,40,0,2,32,0,109,38,191,20,71,16,151,10,167>>).
-define(rIP6, {9734,10240,544,109,9919,5191,4247,2727}).
-define(DOMAIN, <<"example.com">>).

decode_ip_test() ->
    ?assertEqual({?rIP4, 1}, erlysocks_cli:decode_ip(?IP4)),
    ?assertEqual({?rIP6, 4}, erlysocks_cli:decode_ip(?IP6)),
    ?assertError(function_clause, erlysocks_cli:decode_ip({93,184,216,119})),
    ?assertError(function_clause, erlysocks_cli:decode_ip({38,6,40,0,2,32,0,109,38,191,20,71,16,151,10,167})).

decode_domain_test() ->
    ?assertEqual({?rIP4, 1}, erlysocks_cli:decode_domain(?DOMAIN)),
    ?assertError({badmatch, {error, nxdomain}}, erlysocks_cli:decode_domain(<<"stub">>)).

decode_address_test() ->
    ?assertEqual({?rIP4, 1}, erlysocks_cli:decode_address(?IP4)),
    ?assertEqual({?rIP6, 4}, erlysocks_cli:decode_address(?IP6)),
    ?assertEqual({?rIP4, 1}, erlysocks_cli:decode_address(?DOMAIN)),

    ?assertEqual({?rIP4, 1}, erlysocks_cli:decode_address(erlysocks_cli:encode_address(?rIP4))),
    ?assertEqual({?rIP6, 4}, erlysocks_cli:decode_address(erlysocks_cli:encode_address(?rIP6))),

    ?assertError(function_clause, erlysocks_cli:decode_address({93,184,216,119})),
    ?assertError({badmatch, {error, nxdomain}}, erlysocks_cli:decode_domain(<<"stub">>)).

encode_address_test() ->
    ?assertEqual(?IP4, erlysocks_cli:encode_address({93,184,216,119})),
    ?assertEqual(?IP6, erlysocks_cli:encode_address({9734,10240,544,109,9919,5191,4247,2727})),

    {IP4, 1} = erlysocks_cli:decode_address(?IP4),
    {IP6, 4} = erlysocks_cli:decode_address(?IP6),
    ?assertEqual(?IP4, erlysocks_cli:encode_address(IP4)),
    ?assertEqual(?IP6, erlysocks_cli:encode_address(IP6)),

    ?assertError(function_clause, erlysocks_cli:encode_address(<<93,184,216,119>>)).
