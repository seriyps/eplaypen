A web interface to run or compile Erlang code
=============================================

Built with [Docker](https://docker.com/), [PostgreSQL](https://postgresql.org/)
and [Cowboy](https://github.com/essen/cowboy).

It is hosted at <http://tryerl.seriyps.ru/>.


HTTP API
--------

It supports `x-www-form-urlencoded` as well as `json` payload.

There are 3 endpoints:

- evaluate
- compile
- pastebin

### Evaluate

Evaluates Erlang code and prints it's stdout and process return code.

JSON

```bash
curl -i -d '{"code":"-module(main). -export([main/0]). main() -> io:format(\"Hello, world!\").","release":"21.3"}' -H "Content-Type: application/json" http://tryerl.seriyps.ru/api/evaluate
```
x-www-form-urlencoded

```bash
curl -i -d 'code=-module(main).%20-export(%5Bmain%2F0%5D).%20main()%20-%3E%20io%3Aformat(%22Hello%2C%20world!%22).&release=21.3' http://tryerl.seriyps.ru/api/evaluate
```

### Compile

Compiles Erlang code to different forms and prints compilation result.

JSON

```bash
curl -i -d '{"code":"-module(main). -export([main/0]). main() -> io:format(\"Hello, world!\").","release":"21.3","emit":"beam"}' -H "Content-Type: application/json" http://tryerl.seriyps.ru/api/compile
```

x-www-form-urlencoded

```bash
curl -i -d 'code=-module(main).%20-export(%5Bmain%2F0%5D).%20main()%20-%3E%20io%3Aformat(%22Hello%2C%20world!%22).&release=21.3&emit=beam' http://tryerl.seriyps.ru/api/compile
```

### Pastebin

Basic pastebin to store and retrieve code snippets.

JSON

```bash
curl -i -d '{"code":"-module(main). -export([main/0]). main() -> io:format(\"Hello, world!\").","release":"21.3","emit":"beam"}' -H "Content-Type: application/json" http://tryerl.seriyps.ru/api/pastebin
```

x-www-form-urlencoded

```bash
curl -i -d 'code=-module(main).%20-export(%5Bmain%2F0%5D).%20main()%20-%3E%20io%3Aformat(%22Hello%2C%20world!%22).&release=21.3&emit=beam' http://tryerl.seriyps.ru/api/pastebin
```

New pastebin ID / URL will be returned in `Location` HTTP header.

Paste contents can be queried by this URL. HTTP `Accept` header is respected. Supported
values are `application/x-www-form-urlencoded` (default) and `application/json`.

```bash
curl -i -H "Accept: application/json" http://tryerl.seriyps.ru/api/pastebin/<paste id>
```

Deployment
----------

Create new [Ansible variables file](https://docs.ansible.com/ansible/latest/user_guide/playbooks_variables.html#defining-variables-in-files)

```
cp deploy/vars_example.yml deploy/vars.yml
nano deploy/vars.yml
```

Create [Ansible inventory file](https://docs.ansible.com/ansible/latest/user_guide/intro_inventory.html)

It should have `playpen` host.

```
nano deploy/hosts
```

Run ansible

```
ansible-playbook -D -K -i deploy/hosts deploy/main.yml
```

Be careful to not run it with `postgres` tag agin - it may wipe your pastebin database!
Use `--skip-tags postgres` for re-deployment / upgrades.
`--tags=src,conf,erl_version,release,start` for OTP version updates.
