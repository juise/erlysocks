-module(erlysocks_srv).

%% API
-export([start_link/0]).
-export([init/0]).
-export([listen/2, accept/1]).

-define(APPLICATION, erlysocks).
-define(TCP_OPTIONS(IP), [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {ip, IP}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    case whereis(?MODULE) of
        undefined ->
            try register(?MODULE, proc_lib:spawn_link(?MODULE, init, [])) of
                true -> {ok, whereis(?MODULE)}
            catch
                error:_ -> {error, {already_started, whereis(?MODULE)}}
            end;
        Pid when is_pid(Pid) ->
            {error, {already_started, Pid}}
    end.

init() ->
    [{ok, IP}, {ok, Port}] = [application:get_env(?APPLICATION, Env) || Env <- [ip, port]],
    listen(IP, Port).

-spec listen(IP :: inet:ip_address(), Port :: inet:port_number()) -> no_return().
listen(IP, Port) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS(IP)) of
        {ok, Socket} ->
            accept(Socket);
        {error, Reason} ->
            lager:error("Can't listen socket on ~s:~w due ~p", [inet_parse:ntoa(IP), Port, Reason]),
            halt(2)
    end.

-spec accept(Socket :: inet:socket()) -> no_return().
accept(Socket) ->
    case gen_tcp:accept(Socket) of
        {ok, CSocket} ->
            client(CSocket);
        {error, Reason} ->
            lager:error("Can't accept a client connection due ~p", [Reason])
    end,
    ?MODULE:accept(Socket).

-spec client(Socket :: inet:socket()) -> 'ok'.
client(Socket) ->
    {ok, Pid} = erlysocks_cli_sup:start_child(Socket),
    gen_tcp:controlling_process(Socket, Pid),
    ok.

