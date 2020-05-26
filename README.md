# `gen_icmp`

ICMP implementation for Erlang using `socket` (so for now only Unix support).

## Usage

```erlang
{ok, Socket} = gen_icmp:open().

Addr = #{family => inet, port => 0, addr => {127, 0, 0, 1}}.

ok = gen_icmp:ping(Socket, Addr, <<1,2,3,4>>).

receive
    {icmp, Addr, {echo_reply, #{data := <<1,2,3,4>>}}} -> ok
end.
```

## TODO

- [x] ICMP
- [ ] ICMPv6

## License

See [Apache 2.0](LICENSE).
