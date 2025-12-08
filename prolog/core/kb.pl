%═══════════════════════════════════════════════════════════
% PRIMAL LOGIC KNOWLEDGE BASE
% Core facts, constants, and rules for the MotorHandPro system
%═══════════════════════════════════════════════════════════

:- module(kb, [
    constant/2,
    semantic_bound/2,
    stability_bound/2,
    within_bounds/1,
    validate_constants/0,
    system_state/2,
    update_state/3
]).

:- dynamic system_state/2.

%═══════════════════════════════════════════════════════════
% PRIMAL LOGIC CONSTANTS
%═══════════════════════════════════════════════════════════

% Core mathematical constants
constant(lightfoot, 0.16905).
constant(donte, 149.9992314).
constant(i3, 6.4939394023).
constant(scaling, 23.0983417165).
constant(lipschitz, 0.000129931830).

% Semantic bounds for valid operation
semantic_bound(lower, -6.5).
semantic_bound(upper, 6.5).

% Physical constants
constant(speed_of_light, 299792458).
constant(planck_constant, 6.62607015e-34).
constant(planck_length, 1.616255e-35).
constant(planck_time, 5.391247e-44).

% Mission constants
constant(mars_mission_min_days, 180).
constant(mars_mission_max_days, 860).
constant(gcr_rate_gy_per_day, 0.0002).
constant(spe_rate_gy_per_day, 0.002).

%═══════════════════════════════════════════════════════════
% STABILITY RULES
%═══════════════════════════════════════════════════════════

% Lipschitz constant must be < 1 for contractivity
stability_bound(lipschitz, L) :-
    constant(lipschitz, L),
    L < 1.

% System is stable if all conditions met
system_stable :-
    stability_bound(lipschitz, L),
    L < 1,
    constant(lightfoot, Lambda),
    Lambda > 0,
    constant(donte, D),
    D > 149,
    D < 150.

%═══════════════════════════════════════════════════════════
% SEMANTIC BOUNDS CHECKING
%═══════════════════════════════════════════════════════════

% Check if value is within semantic bounds
within_bounds(Value) :-
    number(Value),
    semantic_bound(lower, Lower),
    semantic_bound(upper, Upper),
    Value >= Lower,
    Value =< Upper.

% Check if quantum field state is valid
valid_field_state(state(Position, Velocity, Acceleration)) :-
    within_bounds(Position),
    number(Velocity),
    number(Acceleration).

%═══════════════════════════════════════════════════════════
% EXPONENTIAL DECAY
%═══════════════════════════════════════════════════════════

% Apply Lightfoot exponential decay
% lightfoot_decay(+Time, +InitialValue, -DecayedValue)
lightfoot_decay(Time, Initial, Decayed) :-
    constant(lightfoot, Lambda),
    Decayed is Initial * exp(-Lambda * Time).

% Compute decay factor for time period
decay_factor(Time, Factor) :-
    constant(lightfoot, Lambda),
    Factor is exp(-Lambda * Time).

%═══════════════════════════════════════════════════════════
% PRIMAL LOGIC ITERATION
%═══════════════════════════════════════════════════════════

% Single iteration of F(x) = D - I3 * ln(S - x)
primal_step(X, NextX) :-
    constant(donte, D),
    constant(i3, I3),
    constant(scaling, S),
    Arg is S - X,
    Arg > 0,  % Must be positive for log
    NextX is D - I3 * log(Arg).

% Iterate N times
primal_iterate(0, X, X) :- !.
primal_iterate(N, X, Result) :-
    N > 0,
    primal_step(X, NextX),
    N1 is N - 1,
    primal_iterate(N1, NextX, Result).

% Check convergence (within epsilon)
converged(X, Y, Epsilon) :-
    abs(X - Y) < Epsilon.

% Iterate until convergence
primal_converge(X, Result) :-
    primal_converge(X, Result, 0.0001, 100).

primal_converge(X, X, _, 0) :- !.
primal_converge(X, Result, Epsilon, MaxIter) :-
    MaxIter > 0,
    primal_step(X, NextX),
    (   converged(X, NextX, Epsilon)
    ->  Result = NextX
    ;   MaxIter1 is MaxIter - 1,
        primal_converge(NextX, Result, Epsilon, MaxIter1)
    ).

%═══════════════════════════════════════════════════════════
% QUANTUM RESONANCE FIELD
%═══════════════════════════════════════════════════════════

% Initialize quantum field at fixed point
init_quantum_field(state(D, 0, 0)) :-
    constant(donte, D).

% Update quantum field with new observation
update_quantum_field(state(Pos, Vel, _Acc), Obs, NewState) :-
    constant(lightfoot, Lambda),
    NewPos is Pos + exp(-Lambda) * (Obs - Pos),
    NewVel is NewPos - Pos,
    NewAcc is NewVel - Vel,
    NewState = state(NewPos, NewVel, NewAcc).

% Check if field state is within semantic bounds
field_within_bounds(state(Pos, _, _)) :-
    within_bounds(Pos).

%═══════════════════════════════════════════════════════════
% SYSTEM STATE MANAGEMENT
%═══════════════════════════════════════════════════════════

% Initialize system state
init_system_state :-
    retractall(system_state(_, _)),
    init_quantum_field(Field),
    assertz(system_state(quantum_field, Field)),
    assertz(system_state(last_update, 0)),
    assertz(system_state(stability_index, 1.0)).

% Update specific state component
update_state(Key, OldValue, NewValue) :-
    retract(system_state(Key, OldValue)),
    assertz(system_state(Key, NewValue)).

% Get current state value
get_state(Key, Value) :-
    system_state(Key, Value).

%═══════════════════════════════════════════════════════════
% VALIDATION AND TESTING
%═══════════════════════════════════════════════════════════

% Validate all constants
validate_constants :-
    format('~n═══════════════════════════════════════════════~n'),
    format('   PRIMAL LOGIC CONSTANTS VALIDATION~n'),
    format('═══════════════════════════════════════════════~n~n'),

    % Check Lipschitz bound
    constant(lipschitz, L),
    (   L < 1
    ->  format('✓ Lipschitz constant: ~w (< 1)~n', [L])
    ;   format('✗ Lipschitz constant: ~w (>= 1 - FAIL)~n', [L])
    ),

    % Check Lightfoot positive
    constant(lightfoot, Lambda),
    (   Lambda > 0
    ->  format('✓ Lightfoot constant: ~w (> 0)~n', [Lambda])
    ;   format('✗ Lightfoot constant: ~w (<= 0 - FAIL)~n', [Lambda])
    ),

    % Check Donte in range
    constant(donte, D),
    (   D > 149, D < 150
    ->  format('✓ Donte constant: ~w (149 < D < 150)~n', [D])
    ;   format('✗ Donte constant: ~w (out of range - FAIL)~n', [D])
    ),

    % Check semantic bounds
    semantic_bound(lower, Lower),
    semantic_bound(upper, Upper),
    BoundsSum is abs(Lower + Upper),
    (   BoundsSum < 0.01
    ->  format('✓ Semantic bounds: [~w, ~w] (symmetric)~n', [Lower, Upper])
    ;   format('✗ Semantic bounds: [~w, ~w] (asymmetric)~n', [Lower, Upper])
    ),

    format('~n═══════════════════════════════════════════════~n~n').

% Run comprehensive tests
run_tests :-
    format('~nRunning Primal Logic KB tests...~n'),

    % Test 1: Convergence
    primal_converge(0, Result),
    constant(donte, D),
    Diff is abs(Result - D),
    (   Diff < 0.01
    ->  format('Test 1 - Convergence: PASS~n')
    ;   format('Test 1 - Convergence: FAIL (error: ~w)~n', [Diff])
    ),

    % Test 2: Stability bounds
    (   system_stable
    ->  format('Test 2 - Stability: PASS~n')
    ;   format('Test 2 - Stability: FAIL~n')
    ),

    % Test 3: Semantic bounds
    (   within_bounds(0)
    ->  format('Test 3 - Bounds check: PASS~n')
    ;   format('Test 3 - Bounds check: FAIL~n')
    ),

    % Test 4: Quantum field update
    init_quantum_field(Field),
    update_quantum_field(Field, 150, NewField),
    (   field_within_bounds(NewField)
    ->  format('Test 4 - Quantum field: PASS~n')
    ;   format('Test 4 - Quantum field: FAIL~n')
    ),

    format('~n').

%═══════════════════════════════════════════════════════════
% QUERY HELPERS
%═══════════════════════════════════════════════════════════

% Get all constants as list
all_constants(Constants) :-
    findall(Name-Value, constant(Name, Value), Constants).

% Display constant
show_constant(Name) :-
    constant(Name, Value),
    format('~w = ~w~n', [Name, Value]).

% Display all constants
show_all_constants :-
    format('~n🔷 Primal Logic Constants:~n'),
    constant(lightfoot, L), format('  Lightfoot (λ): ~w~n', [L]),
    constant(donte, D), format('  Donte (D): ~w~n', [D]),
    constant(i3, I3), format('  I3: ~w~n', [I3]),
    constant(scaling, S), format('  Scaling (S): ~w~n', [S]),
    constant(lipschitz, Lip), format('  Lipschitz: ~w~n', [Lip]),
    format('~n').

%═══════════════════════════════════════════════════════════
% MODULE INITIALIZATION
%═══════════════════════════════════════════════════════════

:- initialization(init_system_state).
