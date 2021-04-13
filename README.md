# `gen_icmp`

ICMP implementation for Erlang using `socket` (so for now only Unix support).

## Usage

To send ICMP echo request as unprivileged user (works on macOS and Linux if
user group is within `sysctl net.ipv4.ping_group_range`):

```erlang
{ok, Socket} = gen_icmp:open().

Addr = {127, 0, 0, 1}.

ok = gen_icmp:echoreq(Socket, Addr, <<1,2,3,4>>).

receive
    {icmp, Addr, {echorep, #{data := <<1,2,3,4>>}}} -> ok
end.
```

If you want to be sure of message order you can use `seq` option:

```erlang
{ok, Socket} = gen_icmp:open().

Addr = {127, 0, 0, 1}.

ok = gen_icmp:echoreq(Socket, Addr, <<1,2,3,4>>, [{seq, 0}]).
ok = gen_icmp:echoreq(Socket, Addr, <<5,6,7,8>>, [{seq, 1}]).

receive
    {icmp, Addr, {echorep, #{seq := Seq, data := Data}}} ->
      io:write("Received reply seq=~B data=~p~n", [Seq, Data])
end.
```

## License

See [Apache 2.0](LICENSE).
