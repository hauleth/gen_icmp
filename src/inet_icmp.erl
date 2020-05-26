%% @hidden
%% Implementation of ICMP protocol
-module(inet_icmp).

-export([open/1,
         decode/1,
         echo_req/3]).

%% @doc Open new ICMP socket
open(Type) -> socket:open(inet, Type, icmp).

%% @doc Build Echo Req message
echo_req(Id, Seq, Data) ->
    Checksum = checksum(<<8, 0, 0:16, Id, Seq, Data/binary>>),
    <<8, 0, Checksum/binary, Id, Seq, Data/binary>>.

%% @doc
%% Decode ICMP datagram, we skip first 20 bytes that are IP header and aren't
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
decode(<<_IpHdr:20/binary, Type, Code, _CkSum:16, Meta:2/binary, Data/binary>>) ->
    do_decode(Type, Code, Meta, Data).

do_decode(0, 0, <<Id, Seq>>,  Data) ->
    {echo_reply, #{id => Id, seq => Seq, data => Data}};
do_decode(3, Code, _Meta, Data) when Code >= 0, Code =< 5 ->
    {dest_unreachable, Code, Data}.

%% Compute checksum of the package using Internet Checksum
checksum(Data) when is_binary(Data) -> checksum(Data, 0).

checksum(<<X:16, Rest/binary>>, Sum) -> checksum(Rest, Sum + X);
checksum(<<X>>, Sum) -> checksum(<<>>, Sum + X);
checksum(<<>>, Sum) ->
    <<A:16, B:16>> = <<Sum:32>>,
    <<bnot(A + B):16/big>>.
