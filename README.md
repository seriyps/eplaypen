A web interface for running Erlang code built using [playpen](https://github.com/thestinger/playpen)
and [cowboy](https://github.com/essen/cowboy).

It is hosted at <http://playerl.seriyps.ru/>.

JSON

```bash
curl -i -d '{"code":"main() -> ok.","release":"17.4","emit":"beam"}' -H "Content-Type: application/json" http://playerl.seriyps.ru/api/compile
```

x-www-form-urlencoded

```bash
curl -i -d "code=main%28%29%20-%3E%20ok.&release=17.4&emit=beam" http://playerl.seriyps.ru/api/compile
```

Add new release
---------------

1. Add it to `priv/RELEASES.txt`.
2. Run `./bin/build_erl.sh`
3. Connect to node and run `playpen:reload_releases()`
4. Add it as `<option>` to `priv/index.html`
