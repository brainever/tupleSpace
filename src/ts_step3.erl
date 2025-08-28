-module(ts_step3).
-export([new/1, out/2, rd/2, in/2, rd/3, in/3, list/1, addNode/2, removeNode/2, nodes/1,
         start_proxy/2]).

%%%% ========== Creazione (master) e debug ==========

new(Name) ->
    Pid = spawn(fun() -> loop([], []) end),  %% stato: Tuples=[], Proxies=[]
    register(Name, Pid),
    {ok, Name}.

list(TS) ->
    TS ! {list, self()},
    receive
        Tuples ->
            Tuples
    end.

%%%% ========== Interfaccia 1/3 ==========

out(TS, Tuple) ->
    TS ! {out, Tuple},
    ok.

rd(TS, Pattern) ->
    TS ! {rd, self(), Pattern},
    receive
        {ok, T} ->
            T;
        not_found ->
            receive after 10 ->
                rd(TS, Pattern)
            end
    end.

in(TS, Pattern) ->
    TS ! {in, self(), Pattern},
    receive
        {ok, T} ->
            T;
        not_found ->
            receive after 10 ->
                in(TS, Pattern)
            end
    end.

%%%% ========== Interfaccia 2/3 (timeout ms) ==========

rd(TS, Pattern, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    rd_until(TS, Pattern, Timeout).

in(TS, Pattern, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    in_until(TS, Pattern, Timeout).

rd_until(_TS, _P, 0) ->
    {err, timeout};
rd_until(TS, Pattern, Timeout) ->
    TS ! {rd, self(), Pattern},
    receive
        {ok, T} ->
            {ok, T};
        not_found ->
            Step =
                case Timeout >= 10 of
                    true ->
                        10;
                    false ->
                        Timeout
                end,
            receive after Step ->
                New = Timeout - Step,
                case New =< 0 of
                    true ->
                        {err, timeout};
                    false ->
                        rd_until(TS, Pattern, New)
                end
            end
    end.

in_until(_TS, _P, 0) ->
    {err, timeout};
in_until(TS, Pattern, Timeout) ->
    TS ! {in, self(), Pattern},
    receive
        {ok, T} ->
            {ok, T};
        not_found ->
            Step =
                case Timeout >= 10 of
                    true ->
                        10;
                    false ->
                        Timeout
                end,
            receive after Step ->
                New = Timeout - Step,
                case New =< 0 of
                    true ->
                        {err, timeout};
                    false ->
                        in_until(TS, Pattern, New)
                end
            end
    end.

%%%% ========== Interfaccia 3/3 (nodi) ==========

%% Aggiunge visibilità locale su 'Node' (crea il proxy registrato lì)
addNode(TS, Node) ->
    TS ! {add_node, self(), Node},
    receive
        ok ->
            ok;
        {err, Reason} ->
            {err, Reason}
    end.

%% Rimuove il proxy su 'Node' (se c’è)
removeNode(TS, Node) ->
    TS ! {remove_node, self(), Node},
    receive
        ok ->
            ok
    end.

%% Elenca i nodi su cui il TS è visibile (master + proxy)
nodes(TS) ->
    TS ! {nodes, self()},
    receive
        Ns ->
            Ns
    end.

%%%% ========== SERVER MASTER (stato = Tuples, Proxies) ==========
%% Proxies = [{Node, ProxyPid}, ...]

loop(Tuples, Proxies) ->
    receive
        {out, T} ->
            loop([T | Tuples], Proxies);
        {list, From} ->
            From ! lists:reverse(Tuples),
            loop(Tuples, Proxies);
        {rd, From, Pattern} ->
            case find_match(Pattern, Tuples) of
                {ok, Match} ->
                    From ! {ok, Match},
                    loop(Tuples, Proxies);
                not_found ->
                    From ! not_found,
                    loop(Tuples, Proxies)
            end;
        {in, From, Pattern} ->
            case take_match(Pattern, Tuples) of
                {ok, Match, Rest} ->
                    From ! {ok, Match},
                    loop(Rest, Proxies);
                not_found ->
                    From ! not_found,
                    loop(Tuples, Proxies)
            end;
        %% ====== 3/3: gestione nodi ======
        {add_node, From, Node} ->
            Name = registered_name(),            
            case Node of
                N when N =:= node() ->
                    From ! ok,
                    loop(Tuples, Proxies);
                _ ->
                    case lists:keyfind(Node, 1, Proxies) of
                        {Node, _Pid} ->
                            From ! ok,  
                            loop(Tuples, Proxies);
                        false ->
                            Res = rpc:call(Node, ?MODULE, start_proxy, [Name, self()]),
                            case Res of
                                {ok, ProxyPid} ->
                                    From ! ok,
                                    NewProxies = [{Node, ProxyPid} | Proxies],
                                    erlang:monitor_node(Node, true),
                                    loop(Tuples, NewProxies);
                                {error, name_in_use} ->
                                    From ! {err, name_in_use},
                                    loop(Tuples, Proxies);
                                {badrpc, Reason} ->
                                    From ! {err, {badrpc, Reason}},
                                    loop(Tuples, Proxies)
                            end
                    end
            end;
        {nodedown, Node} ->
            case lists:keytake(Node, 1, Proxies) of
                {value, {_Node, _Pid}, NewProxies} ->
                    loop(Tuples, NewProxies);
                false ->
                    loop(Tuples, Proxies)
            end;
        {remove_node, From, Node} ->
            case lists:keytake(Node, 1, Proxies) of
                {value, {Node, ProxyPid}, NewProxies} ->
                    ProxyPid ! stop,
                    From ! ok,
                    loop(Tuples, NewProxies);
                false ->
                    From ! ok,   
                    loop(Tuples, Proxies)
            end;
        {nodes, From} ->
            Ns = [node() | [N || {N, _} <- Proxies]],
            From ! Ns,
            loop(Tuples, Proxies)
    end.

registered_name() ->
    case erlang:process_info(self(), registered_name) of
        {registered_name, Name} when is_atom(Name) ->
            Name;
        _ ->
            undefined
    end.

%%%% ========== PROXY (vive sul nodo remoto) ==========

start_proxy(Name, MasterPid) when is_atom(Name), is_pid(MasterPid) ->
    case whereis(Name) of
        undefined ->
            Pid = spawn(fun() ->
                           register(Name, self()),
                           proxy_loop(MasterPid)
                        end),
            {ok, Pid};
        _Already ->
            {error, name_in_use}
    end.

proxy_loop(MasterPid) ->
    receive
        stop ->
            ok;   %% uscita: la registrazione locale si libera
        Msg ->
            MasterPid ! Msg,   %% inoltra tutto al master
            proxy_loop(MasterPid)
    end.

%%%% ========== Funzioni ausiliari ==========

find_match(Pattern, [H | T]) ->
    case matches(Pattern, H) of
        true ->
            {ok, H};
        false ->
            find_match(Pattern, T)
    end;
find_match(_Pattern, []) ->
    not_found.

take_match(Pattern, [H | T]) ->
    case matches(Pattern, H) of
        true ->
            {ok, H, T};
        false ->
            case take_match(Pattern, T) of
                {ok, Match, Rest} ->
                    {ok, Match, [H | Rest]};
                not_found ->
                    not_found
            end
    end;
take_match(_Pattern, []) ->
    not_found.

matches(Pattern, Tuple)
    when is_tuple(Pattern), is_tuple(Tuple), tuple_size(Pattern) =:= tuple_size(Tuple) ->
    match_lists(tuple_to_list(Pattern), tuple_to_list(Tuple));
matches(_, _) ->
    false.

match_lists([P | Ps], [X | Xs]) ->
    case match_field(P, X) of
        true ->
            match_lists(Ps, Xs);
        false ->
            false
    end;
match_lists([], []) ->
    true.

match_field('_', _Any) ->
    true;
match_field(Value, Value) ->
    true;
match_field(_, _) ->
    false.
