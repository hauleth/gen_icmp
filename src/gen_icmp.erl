-module(gen_icmp).

-behaviour(gen_server).

-export([open/0,
         open/1,
         ping/3,
         ping/4]).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_continue/2,
         handle_info/2]).

%% @equiv open([])
open() ->
    open([]).

%% @doc
%% Open new ICMP socket.
%%
%% Options:
%%
%% <ul>
%%      <li>`inet6' - use ICMPv6 instead of ICMP</li>
%%      <li>`raw' - use RAW socket instead of datagram, this provides better
%%      compatibility between different OSes, but may require to run Erlang
%%      process with additional privileges.</li>
%% </ul>
%% @end
open(Opts) ->
    gen_server:start_link(?MODULE, {self(), Opts}, []).

%% @equiv ping(Pid, Addr, Data, [])
ping(Pid, Addr, Data) -> ping(Pid, Addr, Data, []).

%% @doc
%% Send ICMP echo request to the `Addr' with `Data'.
%%
%% Options:
%%
%% <ul>
%%      <li>`{id, 0..255}' - stream identificator, defaults to `0'</li>
%%      <li>`{seq, 0..255}' - packet number in stream, defaults to `0'</li>
%% </ul>
%% @end
ping(Pid, Addr, Data, Opts) ->
    gen_server:cast(Pid, {echo_req, Addr, Data, Opts}).

%% gen_server handles =========================================================

-record(state, {owner, owner_mon, socket, module}).

%% @hidden
init({Pid, Opts}) ->
    Module = case proplists:get_bool(inet6, Opts) of
               true -> inet6_icmp;
               false -> inet_icmp
           end,
    Type = case proplists:get_bool(raw, Opts) of
               true -> raw;
               false -> dgram
           end,
    Ref = monitor(process, Pid),
    case Module:open(Type) of
        {ok, Socket} -> {ok, #state{socket=Socket, module=Module, owner=Pid, owner_mon=Ref}};
        Error -> Error
    end.

%% @hidden
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast({echo_req, Addr, Data, Opts}, #state{socket=Socket, module=Module} = State) ->
    Id = proplists:get_value(id, Opts, 0),
    Seq = proplists:get_value(seq, Opts, 0),

    Msg = Module:echo_req(Id, Seq, Data),

    ok = socket:sendto(Socket, Msg, Addr),

    {noreply, State, {continue, echo_reply}}.

%% @hidden
handle_continue(echo_reply, #state{socket=Socket, owner=Pid, module=Module} = State) ->
    case socket:recvfrom(Socket, [], nowait) of
        {ok, {From, Data}} ->
            Pid ! {icmp, From, Module:decode(Data)},
            {noreply, State, {continue, echo_reply}};
        {select, _Info} ->
            {noreply, State}
    end.

%% @hidden
handle_info({'$socket', Socket, select, _Info}, #state{socket=Socket} = State) ->
    {noreply, State, {continue, echo_reply}}.
