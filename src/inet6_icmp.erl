%% @hidden
%% TODO: Implementation of ICMPv6 protocol
-module(inet6_icmp).

-export([open/1,
         encode/4,
         decode/1]).

%% IANA number for ICMPv6 as `socket` do not support it OotB.
-define(ICMPv6, {raw, 58}).

open(Type) -> socket:open(inet6, Type, {raw, 58}).

encode(Type0, Code0, Meta, Data) ->
    {Type, Code} = encode_type(Type0, Code0),
    <<Type, Code, 0:16, Meta:4/binary, Data/binary>>.

encode_type(echoreq, _) -> {128, 0};
encode_type(Type, Code) -> {Type, Code}.

%% @doc
%% Decode ICMPv6 datagram, we skip first 40 bytes that are IPv6 header and aren't
%% needed for us. Then ICMP packet looks like:
%%
%%  0                   1                   2                   3
%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Type      |     Code      |          Checksum             |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                           Metadata                            |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                            Data
%% +------...
%% @end
decode(<<Type, Code, _CkSum:16, Meta:4/binary, Data/binary>>) ->
    do_decode(Type, Code, Meta, Data).

do_decode(1, Code, _Meta, Data) ->
    {unreach, #{code => Code, data => Data}};
do_decode(2, 0, <<Mtu:32>>, Data) ->
    {toobig, #{mtu => Mtu, data => Data}};
do_decode(3, Code, _Meta, Data) ->
    {timex, #{code => Code, data => Data}};
do_decode(128, 0, <<Id:16, Seq:16>>, Data) ->
    {echoreq, #{id => Id, seq => Seq, data => Data}};
do_decode(129, 0, <<Id:16, Seq:16>>, Data) ->
    {echorep, #{id => Id, seq => Seq, data => Data}};
do_decode(Type, Code, Meta, Data) ->
    {Type, #{code => Code, meta => Meta, data => Data}}.
