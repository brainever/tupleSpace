-module(ts_runner).
-export([
  run_local/1,
  run_remote/3,
  recovery_ms/3          
]).

%% ---------- util ----------

avg(L) when is_list(L), L =/= [] ->
    (lists:sum(L) * 1.0) / length(L).

fmt_us(Label, US) ->
    io:format("~s = ~.3f us~n", [Label, US]).

%fmt_ms(Label, MS) -> io:format("~s = ~.1f ms~n", [Label, MS * 1.0]).

%% ---------- LOCALE ----------
run_local(N) ->
    %% se non esiste ancora il TS locale, crealo
    case whereis(ts1) of
        undefined -> {ok,_} = ts_step3:new(ts1);
        _ -> ok
    end,

    %% out locale (N tuple diverse)
    LocOutUS = avg([ element(1, timer:tc(ts_step3, out, [ts1, {k,I}]))
                     || I <- lists:seq(1, N) ]),

    ts_step3:out(ts1, {tag,42}),
    LocRdUS  = avg([ element(1, timer:tc(ts_step3, rd, [ts1, {tag,'_'}]))
                     || _ <- lists:seq(1, N) ]),

    [ ts_step3:out(ts1, {k,I}) || I <- lists:seq(1, N) ],
    LocInUS  = avg([ element(1, timer:tc(ts_step3, in, [ts1, {k,'_'}]))
                     || _ <- lists:seq(1, N) ]),

    io:format("LOCALE (n=~p)~n", [N]),
    fmt_us("out/2", LocOutUS),
    fmt_us("rd/2 ", LocRdUS),
    fmt_us("in/2 ", LocInUS),
    ok.

%% ---------- REMOTO ----------
run_remote(N, HostB, TimeoutMs) ->
    TSRef = {ts1, HostB},

    case net_adm:ping(HostB) of
        pong -> ok;
        pang -> error({cannot_reach, HostB})
    end,

    RemOutUS = avg([ element(1, timer:tc(ts_step3, out, [TSRef, {r,I}]))
                     || I <- lists:seq(1, N) ]),

    ok = ts_step3:out(TSRef, {rtag,99}),
    RemRdUS = avg([ element(1, timer:tc(ts_step3, rd, [TSRef, {rtag,'_'}, TimeoutMs]))
                    || _ <- lists:seq(1, N) ]),

    [ ts_step3:out(TSRef, {r,I}) || I <- lists:seq(1, N) ],
    RemInUS = avg([ element(1, timer:tc(ts_step3, in, [TSRef, {r,'_'}, TimeoutMs]))
                    || _ <- lists:seq(1, N) ]),

    io:format("REMOTO A->B (n=~p, timeout=~p ms)~n", [N, TimeoutMs]),
    fmt_us("out/2", RemOutUS),
    fmt_us("rd/3 ", RemRdUS),
    fmt_us("in/3 ", RemInUS),
    ok.

%% ---------- RECOVERY ----------
%% Misura "tempo percepito lato API" in millisecondi:
%% 1) da B, spegne A con rpc:halt()
%% 2) poi valuta ogni StepMs se A sparisce da nodes/1
recovery_ms(Name, NodeA, StepMs) ->
    %% tenta di spegnere A tra 1ms
    spawn(fun() ->
        timer:sleep(1),
        catch rpc:call(NodeA, erlang, halt, [])
    end),
    T0 = erlang:monotonic_time(millisecond),
    wait_until_gone(Name, NodeA, StepMs),
    T1 = erlang:monotonic_time(millisecond),
    T1 - T0.

wait_until_gone(Name, NodeA, StepMs) ->
    Nodes = ts_step3:nodes(Name),
    case lists:member(NodeA, Nodes) of
        true  -> timer:sleep(StepMs), wait_until_gone(Name, NodeA, StepMs);
        false -> ok
    end.
