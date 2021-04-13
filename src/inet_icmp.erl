%% @hidden
%% Implementation of ICMP protocol
-module(inet_icmp).

-export([open/1,
         addr/1,
         encode/4,
         decode/1]).

%% @doc Open new ICMP socket
open(Type) -> socket:open(inet, Type, icmp).

addr(Addr) ->
    #{addr => Addr, port => 0, family => inet}.

% TODO: Implement Extended Echo Request
encode(Type0, Code0, Meta, Data) ->
    {Type, Code} = encode_type(Type0, Code0),
    Checksum = checksum(<<Type, Code, 0:16, Meta/binary, Data/binary>>),
    <<Type, Code, Checksum/binary, Meta/binary, Data/binary>>.

encode_type(echoreq, _) -> {8, 0};
encode_type(Type, Code) -> {Type, Code}.

%% @doc
%% ICMP packet looks like:
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
decode(<<4:4, IHL:4, Rest/binary>>) ->
    Len = IHL * 4 - 1,
    % We need to ignore IPv4 header if present (it is a case on macOS)
    case Rest of
        <<_IpHdr:Len/binary, Type, Code, _CkSum:16, Meta:4/binary, Data/binary>> ->
            do_decode(Type, Code, Meta, Data);
        <<Type, Code, _CkSum:16, Meta:4/binary, Data/binary>> ->
            do_decode(Type, Code, Meta, Data)
    end;
decode(<<Type, Code, _CkSum:16, Meta:4/binary, Data/binary>>) ->
    do_decode(Type, Code, Meta, Data).

% TODO: Implement other ICMP messages decoding
do_decode(0, 0, <<Id:16, Seq:16>>, Data) ->
    {echorep, #{id => Id, seq => Seq, data => Data}};
do_decode(3, Code, _Meta, Data) ->
    {unreach, #{code => Code, data => Data}};
do_decode(5, Code, <<A, B, C, D>>, Data) ->
    {redirect, #{type => Code, gateway => {A, B, C, D}, data => Data}};
do_decode(8, 0, <<Id:16, Seq:16>>, Data) ->
    {echoreq, #{id => Id, seq => Seq, data => Data}};
do_decode(13, 0, <<Id:16, Seq:16>>, <<Orig:32, Recv:32, Trans:32>>) ->
    {tstamp, #{id => Id, seq => Seq,
               originate => Orig, 'receive' => Recv, transit => Trans}};
do_decode(14, 0, <<Id:16, Seq:16>>, <<Orig:32, Recv:32, Trans:32>>) ->
    Stamp = erlang:system_time(millisecond) rem 86400000,
    {tstamprep, #{id => Id, seq => Seq,
                  originate => Orig, back => Stamp,
                  'receive' => Recv, transit => Trans}};
do_decode(Type, Code, Meta, Data) ->
    {Type, #{code => Code, meta => Meta, data => Data}}.

%% Compute checksum of the package using Internet Checksum
checksum(Data) when is_binary(Data) -> checksum(Data, 0).

checksum(<<X:16, Rest/binary>>, Sum) -> checksum(Rest, Sum + X);
checksum(<<X>>, Sum) -> checksum(<<>>, Sum + X);
checksum(<<>>, Sum) ->
    <<A:16, B:16>> = <<Sum:32>>,
    <<bnot(A + B):16/big>>.
