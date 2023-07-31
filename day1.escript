#!/usr/bin/env escript

lines(BinaryText) -> binary:split(BinaryText, [<<"\n">>], [trim_all, global]).
parse_address(Addr) -> {ok, IPAddr} = inet:parse_address(Addr), IPAddr.

main(_) ->
    {ok, BinaryText } = file:read_file("ips.txt"),
    lists:map(fun parse_address/1, lines(BinaryText)).
