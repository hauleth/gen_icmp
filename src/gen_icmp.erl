-module(gen_icmp).

-behaviour(gen_server).

-export([open/0, open/1,
         echoreq/3, echoreq/4]).

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
%%
%% Controlling process will then receive messages in form of
%%
%% `{icmp, From, {MessageType, Data}}'
%%
%% Where `Data' is map with at least 2 fields:
%%
%% <ul>
%%      <li>`id'</li>
%%      <li>`seq'</li>
%% </ul>
%%
%% Other fields are `MessageType' dependant.
%% @end
open(Opts) ->
    gen_server:start(?MODULE, {self(), Opts}, []).

%% @equiv echoreq(Pid, Addr, Data, [])
echoreq(Pid, Addr, Data) -> echoreq(Pid, Addr, Data, []).

%% @doc
%% Send ICMP echo request to the `Addr' with `Data'.
%%
%% Options:
%%
%% <ul>
%%      <li>`{id, 0..255}' - stream identification number, defaults to `0'</li>
%%      <li>`{seq, 0..255}' - packet number in stream, defaults to `0'</li>
%% </ul>
%%
%% Beware that `Opts' are platform dependant and may not work as expected
%% between platforms (for example Linux will always ignore given values and will
%% self assign them).
%% @end
echoreq(Pid, Addr, Data, Opts) when is_binary(Data) ->
    gen_server:call(Pid, {echoreq, Addr, Data, Opts}).

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
        {ok, Socket} ->
            {ok, #state{socket=Socket, module=Module, owner=Pid, owner_mon=Ref}, {continue, reply}};
        Error -> Error
    end.

%% @hidden
handle_call({echoreq, Addr, Data, Opts}, _From, #state{socket=Socket, module=Module} = State) ->
    Id = proplists:get_value(id, Opts, 0),
    Seq = proplists:get_value(seq, Opts, 0),
    Msg = Module:encode(echoreq, 0, <<Id:16, Seq:16>>, Data),

    Resp = socket:sendto(Socket, Msg, Addr),

    {reply, Resp, State, {continue, reply}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_continue(reply, #state{socket=Socket, owner=Pid, module=Module} = State) ->
    case socket:recvfrom(Socket, [], nowait) of
        {ok, {From, Data}} ->
            Pid ! {icmp, From, Module:decode(Data)},
            {noreply, State, {continue, reply}};
        {select, _Info} ->
            {noreply, State}
    end.

%% @hidden
handle_info({'$socket', Socket, select, _Info}, #state{socket=Socket} = State) ->
    {noreply, State, {continue, reply}};
handle_info({'DOWN', Ref, process, Pid, _Info}, #state{owner=Pid, owner_mon=Ref} = State) ->
    {stop, normal, State}.
