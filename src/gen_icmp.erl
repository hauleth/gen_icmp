-module(gen_icmp).

-export([open/0, open/1,
         echoreq/3, echoreq/4,
         controlling_process/2]).

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
%%      <li>`{ifaddr, ip_address()}' - address on which message should be
%%      listened on.</li>
%% </ul>
%%
%% Controlling process will then receive messages in form of
%%
%% `{icmp, Socket, From, {MessageType, Data}}'
%%
%% Where `Data' is map with at least 2 fields:
%%
%% <ul>
%%      <li>`id'</li>
%%      <li>`seq'</li>
%% </ul>
%%
%% Other fields are `MessageType' dependent.
%% @end
open(Opts) ->
    gen_server:start(gen_icmp_server, {self(), Opts}, []).

controlling_process(Socket, Pid) ->
    gen_server:call(Socket, {controlling_process, Pid}).

%% @equiv echoreq(Socket, Addr, Data, [])
echoreq(Socket, Addr, Data) -> echoreq(Socket, Addr, Data, []).

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
%% Beware that `Opts' are platform dependent and may not work as expected
%% between platforms (for example Linux will always ignore given values and will
%% self assign them).
%% @end
echoreq(Socket, Addr, Data, Opts) when is_binary(Data) ->
    gen_server:call(Socket, {echoreq, Addr, Data, Opts}).
