%% @hidden
-module(gen_icmp_server).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_continue/2,
         handle_info/2]).

-record(state, {owner, owner_mon, socket, module}).

init({ControllingPid, Opts}) ->
    Module = case proplists:get_bool(inet6, Opts) of
               true -> inet6_icmp;
               false -> inet_icmp
             end,
    Type = case proplists:get_bool(raw, Opts) of
               true -> raw;
               false -> dgram
           end,
    If = proplists:get_value(ifaddr, Opts),
    Ref = monitor(process, ControllingPid),
    case Module:open(Type) of
        {ok, Socket} ->
            case try_bind(Module, Socket, If) of
                ok ->
                    {ok, #state{socket=Socket, module=Module, owner=ControllingPid, owner_mon=Ref}, {continue, reply}};
                Error ->
                    Error
            end;
        Error -> Error
    end.

try_bind(_Module, _Socket, undefined) -> ok;
try_bind(Module, Socket, If) ->
    Addr = Module:addr(If),
    case socket:bind(Socket, Addr) of
        {ok, _} -> ok;
        Error -> Error
    end.

handle_call({echoreq, Addr, Data, Opts}, _From, #state{socket=Socket, module=Module} = State) ->
    Id = proplists:get_value(id, Opts, 0),
    Seq = proplists:get_value(seq, Opts, 0),
    Dest = Module:addr(Addr),
    Msg = Module:encode(echoreq, 0, <<Id:16, Seq:16>>, Data),

    Resp = socket:sendto(Socket, Msg, Dest),

    {reply, Resp, State, {continue, reply}};
handle_call({controlling_process, Pid}, {ControllingPid, _}, State =
            #state{owner=ControllingPid, owner_mon=Ref}) ->
    true = demonitor(Ref, [flush]),
    NewRef = monitor(process, Pid),
    {reply, ok, State#state{owner=Pid, owner_mon=NewRef}, {continue, reply}};
handle_call({controlling_process, _Pid}, _Ref, State) ->
    {reply, {error, not_owner}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_continue(reply, #state{socket=Socket, owner=ControllingPid, module=Module} = State) ->
    case socket:recvfrom(Socket, [], nowait) of
        {ok, {#{addr:=From}, Data}} ->
            ControllingPid ! {icmp, self(), From, Module:decode(Data)},
            {noreply, State, {continue, reply}};
        {select, _Info} ->
            {noreply, State}
    end.

handle_info({'$socket', Socket, select, _Info}, #state{socket=Socket} = State) ->
    {noreply, State, {continue, reply}};
handle_info({'$socket', Socket, abort, Info}, #state{socket=Socket} = State) ->
    ?LOG_WARNING("Received abort: ~p~n", [Info]),
    {noreply, State, {continue, reply}};
handle_info({'DOWN', Ref, process, ControllingPid, _Info}, #state{owner=ControllingPid, owner_mon=Ref} = State) ->
    {stop, normal, State}.
