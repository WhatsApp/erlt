# Well-typed calculator

(Inspired by http://raol.io/post/parsing-with-leex-and-yecc/)

In the [`src`](src) directory:

```
$ make
$ erl -pa ../ebin
Eshell V10.7.1  (abort with ^G)
1> calc:calculate([{x, 2}, {y, 11}], "x * y - 20").
2
```
