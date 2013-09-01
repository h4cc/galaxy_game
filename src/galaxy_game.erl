%% @doc
%% Implementation module for the galactic battle simulator.
%% The following example shows the expected behavior of the simulator:
%%
%% Planets=[mercury,uranus,venus, earth]
%% Shields=[mercury,uranus]
%% Alliances=[{mercury, uranus}, {venus, earth}]
%% Actions=[{nuclear,mercury},{laser,venus}, {laser, uranus}]
%%
%% ExpectedSurvivors = [uranus]
%% In order to produce this expected results, the following calls will be tested:
%% * ok = setup_universe(Planets, Shields, Alliances)
%% * [uranus] = simulate_attack(Planets, Actions)
%% * ok = teardown_universe(Planets)
%%
%% All the 3 calls will be tested in order to check they produce the expected
%% side effects (setup_universe/3 creates a process per planet, etc)
%% @end

-module(galaxy_game).

-include_lib("eunit/include/eunit.hrl").

-type planet()::atom().
-type shield()::planet().
-type alliance()::{planet(), planet()}.
-type attack()::{laser | nuclear, planet()}.

-export([setup_universe/3, teardown_universe/1, simulate_attack/2]).

% State for the planet proc.
% The planet knows its own name so far.
-record(planet, {
  name::planet()
}).

%% @doc Set up a universe described by the input.
%% The imput is asumed to be minimal and non redundant (i.e. if there is an
%% alliance {a, b} there won't be an alliance {b, a}).
%% Once this function returns, the universe is expected to be fully ready,
%% shields, alliances and all.
-spec setup_universe([planet()], [shield()], [alliance()]) -> ok.
%% @end
setup_universe(Planets, Shields, Alliances) ->
    % Create planets
    lists:foreach( 
      fun(Name) ->
          Pid = spawn(fun() -> 
              IsShielded = lists:member(Name, Shields),
              process_flag(trap_exit, IsShielded),
              planet_proc(#planet{name = Name}) 
          end),
          register(Name, Pid),
          % Check that planet exists with a ping pong
          Pid ! {self(), ping},
          receive
            pong -> ok
          end
      end, 
      Planets
    ),
    % Create alliances
    lists:foreach( 
      fun({Partner1, Partner2}) ->
          Partner1 ! {self(), link_to, whereis(Partner2)},
          % Wait for ack.
          receive 
            ack -> ok
          end
      end, 
      Alliances
    ),
    ok.

%% @doc Clean up a universe simulation.
%% This function will only be called after calling setup_universe/3 with the
%% same set of planets.
%% Once this function returns, all the processes spawned by the simulation
%% should be gone.
-spec teardown_universe([planet()]) -> ok.
%% @end
teardown_universe(Planets) ->
    % Shutdown planets
    lists:foreach( 
      fun(Name) ->
          case whereis(Name) of
              % Planet does not exist.
              undefined ->
                  ok;
              % Tear planet down.
              Pid -> 
                  Pid ! teardown,
                  unregister(Name)
          end
      end, 
      Planets
    ),
    ok.

%% @doc Simulate an attack.
%% This function will only be called after setting up a universe with the same
%% set of planets.
%% It returns the list of planets that have survived the attack
-spec simulate_attack([planet()], [attack()]) -> Survivors::[planet()].
%% @end
simulate_attack(Planets, Actions) ->
    % Send attacks.
    lists:foreach(
        fun({Type, Planet}) ->
            case Type of
                % A nuclear attack will simply destroy that planet.
                nuclear ->
                    exit(whereis(Planet), kill);
                % Send a special laser attack, that only shielded planets will survive.
                laser ->
                    exit(whereis(Planet), laser)   
            end
        end,
        Actions
    ),
    % Need to wait a short time to have the kills be evaluated.
    timer:sleep(1),
    % Filter planet processes that are still alive.
    Survivors = lists:filter(
        fun(Planet) ->
            PPid = whereis(Planet),
            PPid /= undefined andalso erlang:is_process_alive(PPid)
        end,
        Planets
    ),
    Survivors.

%--- Implementation ---

% @doc Process function for a planet.
-spec planet_proc(#planet{}) -> ok.
planet_proc(#planet{name = Name} = Planet) ->
    receive 
        % Offer ping pong.
        {From, ping} ->
            From ! pong,
            planet_proc(Planet);
        % Will stop the server without usage of exit().
        teardown ->
            ok;
        % Synchronous command to link this process to another.
        {FromPid, link_to, Pid} ->
            link(Pid),
            FromPid ! ack,
            planet_proc(Planet);
        % If this process  get a killed message, it is shielded, so simply ignore it.
        {'EXIT', _RemotePid, killed} ->
            planet_proc(Planet);
        % If this process  get a laser message, it is shielded, so simply ignore it.
        {'EXIT', _RemotePid, laser} ->
            planet_proc(Planet);
        % Uhh, errors! Why?
        UnknownMsg ->
            io:format("Planet ~p: Unknown Message recieved: ~p~n", [Name, UnknownMsg]),
            planet_proc(Planet)
    end.
