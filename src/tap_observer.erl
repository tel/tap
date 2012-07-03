%% @doc An observer of a particular VM statistic
-module(tap_observer).
-author('Joseph Abrahamson <me@jspha.com>').

-export([start_link/3, init/4]).
-export([system_continue/3, system_terminate/4, system_code_change/4, dbg/3]).

-record(st, {diagnostic :: atom(),
	     function :: function(),
	     debug :: _,
	     timeout :: pos_integer(),
	     parent :: pid()}).

%% Observer

-spec
start_link(atom(), atom(), integer()) -> {ok, pid()}.
start_link(Name, Diagnostic, Timeout) ->
    proc_lib:start_link(?MODULE, init, [Name, self(), Diagnostic, Timeout]).

-spec
init(atom(), pid(), atom(), non_neg_integer()) -> no_return().
init(Name, Parent, Diagnostic, Timeout) ->
    register(Name, self()),
    Deb = sys:debug_options([]),
    Fn = get_diagnostic_function(Diagnostic),
    proc_lib:init_ack(Parent, {ok, self()}),
    %% On restart, do the job immediately
    lager:info("Starting tap observer for: ~s", [Diagnostic]),
    beat(#st{parent = Parent, debug = Deb,
	     diagnostic = Diagnostic,
	     function = Fn, timeout = Timeout}).

-spec
loop(#st{}) -> no_return().
loop(St = #st{parent = Parent, debug = Deb, timeout = Timeout}) ->
    receive
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, 
				  ?MODULE, Deb, St)
    after Timeout ->
	    beat(St)
    end.

-spec
beat(#st{}) -> no_return().
beat(St = #st{function = Fn, diagnostic = Diagnostic, timeout = Timeout}) -> 
    zeta:svh(Diagnostic, Fn(), ok, [{tags, [erl_vm]},
				    {t, timestamp()},
				    {ttl, 1.1*(Timeout/1000)}]),
    loop(St).

dbg(Device, Event, Extra) ->
    io:format(Device, "~p event = ~p~n", [Extra, Event]).

system_continue(_Parent, _Deb, St) ->
    _ = lager:info("System sent continue to heartbeat"),
    loop(St).

-spec
system_terminate(_, _, _, _) -> no_return().
system_terminate(Reason, _Parent, _Deb, _Timer) ->
    _ = lager:info("System sent terminate to tap_observer"),
    exit(Reason).

system_code_change(Timer, _Module, _OldVsn, _Extra) -> 
    _ = lager:info("System sent code change to tap_observer"),
    {ok, Timer}.


%%
%% Utilities

get_diagnostic_function({M, F, A}) -> fun () -> apply(M, F, A) end;
get_diagnostic_function(idle_time) -> 
    fun() -> 
	    {_, Since} = erlang:statistics(wall_clock), Since
    end;
get_diagnostic_function(proc_count) -> fun () -> length(erlang:processes()) end.

timestamp() ->
    {MegS, S, MuS} = erlang:now(),
    MegS*1000000 + S + trunc(MuS/1000000).
