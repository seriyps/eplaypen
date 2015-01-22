A web interface for running Erlang code built using [playpen](https://github.com/thestinger/playpen)
and [cowboy](https://github.com/essen/cowboy).

It is hosted at <http://playerl.seriyps.ru/>.

JSON

```bash
curl -i -d '{"code":"main() -> ok.","release":"17.4","output_format":"beam"}' -H "Content-Type: application/json" http://playerl.seriyps.ru/api/compile
```

x-www-form-urlencoded

```bash
curl -i -d "code=main%28%29%20-%3E%20ok.&release=17.4&output_format=beam" http://playerl.seriyps.ru/api/compile
```
