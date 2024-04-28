# xerl

Samiur's experiment in erlang

## No file needed?

Use erl -eval. e.g.
```
erl -eval 'io:format("Memory: ~p~n", [erlang:memory(total)]).'\
-noshell -s init stop
```

## Compile a single file

Say you have x.erl.

Run in shell:

``` shell
erl
c(x).
x:start().
```

Or from command prompt

``` shell
erlc x.erl
erl -noshell -s x start -s init stop
```

## Need to make a release?

Hand written release is in templates/hello_supervisor

## TOC

| File | Concepts |
| ---- | -------- |
| colors | erlang maps |
| hif | high order funcs and comparators |
| month_length | BIF(trunc), if/case guards |
| weather | lists, io:format, tuples |

