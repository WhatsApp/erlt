## Count Impl

count impl is a classic erlang project.

count impl shows the [server](./src/count_server.erl) and the [implementation](./src/count_server_impl.erl) of the server can live in two separate files.
The implementation module can then be passed to the server to create an instance.

```erlang
count_server:start(Name, InitArgs, count_server_impl)
```
