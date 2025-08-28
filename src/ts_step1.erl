-module(ts_step1).
-export([new/1, out/2, rd/2, in/2, list/1]).

%%%% ========== Creazione e debug ==========

new(Name) ->
    Pid = spawn(fun() -> loop([]) end),  %% processo server;
    register(Name, Pid),
    {ok, Name}.

list(TS) ->
    TS ! {list, self()},
    receive
        Tuples -> Tuples
    end.

%%%% ========== (Interfaccia 1/3) ==========

out(TS, Tuple) ->
    TS ! {out, Tuple},
    ok.

%% rd/2: BLOCCANTE finché non trova, NON consuma; ritorna direttamente la tupla
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

%% in/2: BLOCCANTE finché non trova, CONSUMA; ritorna direttamente la tupla
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

%%%% ========== Server (stato = lista di tuple) ==========

loop(Tuples) ->
    receive
        {out, T} ->
            loop([T | Tuples]);                       %% aggiungi in testa

        {list, From} ->
            From ! lists:reverse(Tuples),             %% solo debug
            loop(Tuples);

        {rd, From, Pattern} ->
            case find_match(Pattern, Tuples) of
                {ok, Match} -> From ! {ok, Match}, loop(Tuples);   %% NON consuma
                not_found   -> From ! not_found,   loop(Tuples)
            end;

        {in, From, Pattern} ->
            case take_match(Pattern, Tuples) of
                {ok, Match, Rest} -> From ! {ok, Match}, loop(Rest); %% CONSUMA
                not_found         -> From ! not_found,   loop(Tuples)
            end
    end.

%%%% ========== Funzioni ausiliari ==========

%% Cerca senza consumare
find_match(Pattern, [H|T]) ->
    case matches(Pattern, H) of
        true  -> {ok, H};
        false -> find_match(Pattern, T)
    end;
find_match(_Pattern, []) ->
    not_found.

%% Cerca con consumo (restituisce anche la lista senza la tupla trovata)
take_match(Pattern, [H|T]) ->
    case matches(Pattern, H) of
        true  -> {ok, H, T};
        false ->
            case take_match(Pattern, T) of
                {ok, Match, Rest} -> {ok, Match, [H | Rest]};  %% ricostruisci
                not_found         -> not_found
            end
    end;
take_match(_Pattern, []) ->
    not_found.

%% MATCH: entrambe tuple e stessa arità, poi confronto campo-per-campo
matches(Pattern, Tuple)
  when is_tuple(Pattern), is_tuple(Tuple),
       tuple_size(Pattern) =:= tuple_size(Tuple) ->
    match_lists(tuple_to_list(Pattern), tuple_to_list(Tuple));
matches(_, _) ->
    false.

%% Confronto elemento per elemento
match_lists([P|Ps], [X|Xs]) ->
    case match_field(P, X) of
        true  -> match_lists(Ps, Xs);
        false -> false
    end;
match_lists([], []) ->
    true.

match_field('_', _Any) -> true;        %% jolly
match_field(Value, Value) -> true;     %% uguali
match_field(_, _) -> false.
