# Tuple Space in Erlang (ts_step3)

Implementazione di un Tuple Space minimale in Erlang:
- **Interfaccia 1/3**: `new/1`, `out/2`, `rd/2`, `in/2`, `list/1`
- **Interfaccia 2/3 (timeout lato client)**: `rd/3`, `in/3` → `{ok,Tuple}` | `{err,timeout}`
- **Interfaccia 3/3 (multi-nodo senza replica)**: `addNode/2`, `removeNode/2`, `nodes/1` via proxy

> Semplificazioni: flat tuples, jolly `'\_'` (atomo), timeout con polling dal client, un master unico (niente replica).

## Avvio rapido

### Nodi
```bash
# Nodo B (master)
ERL_AFLAGS="-sname b1 -setcookie choco" rebar3 shell

# Nodo A (client)
ERL_AFLAGS="-sname a1 -setcookie choco" rebar3 shell
