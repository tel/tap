%% @doc Erlang VM diagnostics
-module(tap).
-author('Joseph Abrahamson <me@jspha.com>').

-export([start/0, refresh/0]).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-define(SUP, tap_sup).
-export([init/1]).

%% ----------
%% Public API

refresh() ->
    OldChildren = supervisor:which_children(?SUP),
    lager:info("Killing ~b diagnostics.", [length(OldChildren)]),
    lists:map(
      fun (C) -> supervisor:terminate_child(?SUP, C) end,
      OldChildren),
    NewChildren = childspecs(),
    lager:info("Starting ~b new diagnostics.", [length(NewChildren)]),
    lists:map(
      fun (C) -> supervisor:start_child(?SUP, C) end,
      NewChildren),
    ok.


%% ---------------------
%% Application callbacks

start() ->
    _ = lists:map(fun(A) -> ok = estart(A) end,
		  [syntax_tools, compiler, lager, 
		   sasl, crypto, protobuffs, zeta]),
    application:start(tap).

start(_StartType, _Args) ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

stop(_State) -> ok.

%% --------------------
%% Supervisor callbacks

init(_Args) ->
    {ok, {{one_for_one, 5, 10},
	  childspecs()}}.

childspecs() ->
    {ok, Diags} = application:get_env(tap, diagnostics),
    {ok, Timeout} = application:get_env(tap, timeout),
    lists:map(
      fun ({D, T}) -> make_childspec(D, T);
	  (D) -> make_childspec(D, Timeout)
      end, Diags).

make_childspec(Diag, Timeout) ->
    Name = list_to_atom("tap_" ++ atom_to_list(Diag) ++ "_obs"),
    {Name, {tap_observer, start_link, [Name, Diag, Timeout]},
     permanent, brutal_kill, worker, [tap_observer]}.

%% ---------
%% Utilities 

estart(App) ->
    case application:start(App) of
	{error, {already_started, _}} -> ok;
	ok -> ok;
	Else -> Else
    end.
