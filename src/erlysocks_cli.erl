-module(erlysocks_cli).

-behaviour(gen_server).

-compile([{inline, [log_ll/2]}]).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    src_sock :: inet:socket(),
    dst_sock :: inet:socket()
}).

-define(TCP_OPTIONS, [binary, {active, true}, {reuseaddr, true}]).

-define(IPv4,   16#01).
-define(IPv6,   16#04).
-define(DOMAIN, 16#03).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Socket) ->
    inet:setopts(Socket, [{active, true}]),
    {ok, #state{src_sock = Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info({tcp, Socket, <<16#05:8, 16#01:8, _Methods:8>> = _Message}, #state{src_sock=Socket} = State) ->
    ok = gen_tcp:send(Socket, <<16#05, 16#00>>),
    {noreply, State};

handle_info({tcp, Socket, <<16#05:8, 16#01:8, _Reserved:8, ?IPv4:8, Address:32, Port:16>> = _Message}, #state{src_sock=Socket} = State) ->
    case connect(<<Address:32>>, Port, Socket) of
        {ok, CSocket} ->
            {noreply, State#state{dst_sock=CSocket}};
        _ ->
            {stop, normal, State}
    end;

handle_info({tcp, Socket, <<16#05:8, 16#01:8, _Reserved:8, ?IPv6:8, Address:128, Port:16>> = _Message}, #state{src_sock=Socket} = State) ->
    case connect(<<Address:128>>, Port, Socket) of
        {ok, CSocket} ->
            {noreply, State#state{dst_sock=CSocket}};
        _ ->
            {stop, normal, State}
    end;

handle_info({tcp, Socket, <<16#05:8, 16#01:8, _Reserved:8, ?DOMAIN:8, DomainLen:8, Domain:DomainLen/binary, Port:16>> = _Message}, #state{src_sock=Socket} = State) ->
    case connect(<<Domain:DomainLen/binary>>, Port, Socket) of
        {ok, CSocket} ->
            {noreply, State#state{dst_sock=CSocket}};
        _ ->
            {stop, normal, State}
    end;

handle_info({tcp, Socket, Message}, #state{src_sock=Socket} = State) ->
    log(Message, Socket),
    ok = gen_tcp:send(State#state.dst_sock, Message),
    {noreply, State};

handle_info({tcp, Socket, Message}, #state{dst_sock=Socket} = State) ->
    ok = gen_tcp:send(State#state.src_sock, Message),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    [is_port(Socket) andalso gen_tcp:close(Socket) || Socket <- [State#state.src_sock, State#state.dst_sock]],
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec connect(Address :: binary(), Port :: inet:port_number(), Socket :: inet:socket()) -> {'ok', inet:socket()} | {'error', inet:posix()}.
connect(Address, Port, Socket) ->
    {Address1, ATYP} = decode_address(Address),
    case gen_tcp:connect(Address1, Port, ?TCP_OPTIONS) of
        {ok, DstSocket} = Result ->
            {SelfDstAddress, SelfDstPort} = self(DstSocket),
            SelfDstAddress1 = encode_address(SelfDstAddress),
            ok = gen_tcp:send(Socket, <<16#05, 16#00, 16#00, ATYP, SelfDstAddress1/binary, SelfDstPort:16>>),
            Result;
        {error, ehostunreach} = Result ->
            {SrcAddress, SrcPort} = peer(Socket),
            lager:info("~s:~w host unreachable: ~s:~w", [printable(SrcAddress), SrcPort, printable(Address1), Port]),
            ok = gen_tcp:send(Socket, <<16#05, 16#04, 16#00, ATYP, Address/binary, Port:16>>),
            Result;
        {error, econnrefused} = Result ->
            {SrcAddress, SrcPort} = peer(Socket),
            lager:info("~s:~w connection refused: ~s:~w", [printable(SrcAddress), SrcPort, printable(Address1), Port]),
            ok = gen_tcp:send(Socket, <<16#05, 16#05, 16#00, ATYP, Address/binary, Port:16>>),
            Result;
        {error, timeout} = Result ->
            {SrcAddress, SrcPort} = peer(Socket),
            lager:info("~s:~w connection timed out: ~s:~w", [printable(SrcAddress), SrcPort, printable(Address1), Port]),
            ok = gen_tcp:send(Socket, <<16#05, 16#06, 16#00, ATYP, Address/binary, Port:16>>),
            Result;
        Result ->
            {SrcAddress, SrcPort} = peer(Socket),
            lager:error("~s:~w connection general error: ~s:~w, due ~p", [printable(SrcAddress), SrcPort, printable(Address1), Port, Result]),
            ok = gen_tcp:send(Socket, <<16#05, 16#01, 16#00, ATYP, Address/binary, Port:16>>),
            Result
    end.


-spec self(Socket :: inet:socket()) -> {binary(), inet:port_number()}.
self(Socket) ->
    {ok, {Address, Port}} = inet:sockname(Socket),
    {Address, Port}.

-spec peer(Socket :: inet:socket()) -> {binary(), inet:port_number()}.
peer(Socket) ->
    {ok, {Address, Port}} = inet:peername(Socket),
    {Address, Port}.


-spec encode_address(Address :: inet:port_number()) -> binary().
encode_address(Address) when is_tuple(Address) ->
    Size = size(Address) * 2,
    list_to_binary([<<Oct:Size>> || Oct <- tuple_to_list(Address)]).

-spec decode_address(Address :: binary()) -> {inet:ip_address(), ?IPv4 | ?IPv6}.
decode_address(Address) when is_binary(Address) ->
    case inet_parse:domain(binary_to_list(Address)) of
        true -> decode_domain(Address);
        false -> decode_ip(Address)
    end.


-spec decode_ip(Address :: binary()) -> {inet:ip_address(), ?IPv4 | ?IPv6}.
decode_ip(<<Address:32>>) ->
    Address1 = list_to_tuple([Oct || <<Oct:8>> <= <<Address:32>>]),
    {ok, IP} = inet:getaddr(Address1, inet),
    {IP, ?IPv4};

decode_ip(<<Address:128>>) ->
    Address1 = list_to_tuple([Oct || <<Oct:16>> <= <<Address:128>>]),
    {ok, IP} = inet:getaddr(Address1, inet6),
    {IP, ?IPv6}.

-spec decode_domain(Address :: binary()) -> {inet:ip_address(), ?IPv4 | ?IPv6}.
decode_domain(Address) ->
    Address1 = binary_to_list(Address),
    case inet:getaddr(Address1, inet) of
        {ok, IP} ->
            {IP, ?IPv4};
        {error, _} ->
            {ok, IP} = inet:getaddr(Address1, inet6),
            {IP, ?IPv6}
    end.


-spec printable(Address :: inet:ip_address()) -> string().
printable(Address) ->
    case size(Address) of
        4 -> string:join([integer_to_list(Oct) || Oct <- tuple_to_list(Address)], ".");
        8 -> string:join([integer_to_list(Oct, 16) || Oct <- tuple_to_list(Address)], ":")
    end.


log(<<"GET",     _Rest/binary>> = Message, Socket) -> log_ll(Message, Socket);
log(<<"PUT",     _Rest/binary>> = Message, Socket) -> log_ll(Message, Socket);
log(<<"POST",    _Rest/binary>> = Message, Socket) -> log_ll(Message, Socket);
log(<<"HEAD",    _Rest/binary>> = Message, Socket) -> log_ll(Message, Socket);
log(<<"DELETE",  _Rest/binary>> = Message, Socket) -> log_ll(Message, Socket);
log(<<"OPTIONS", _Rest/binary>> = Message, Socket) -> log_ll(Message, Socket);
log(_, _) -> ok.

log_ll(Message, Socket) ->
    [Method, Url, _, _, _, Address | _Rest] = re:split(Message, "[ \r\n]", [trim]),
    {SrcAddress, SrcPort} = self(Socket),
    lager:info("~s:~w ~s http://~s~s", [printable(SrcAddress), SrcPort, Method, Address, Url]).

