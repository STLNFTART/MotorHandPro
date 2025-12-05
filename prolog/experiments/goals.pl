%═══════════════════════════════════════════════════════════
% EXPERIMENT GOAL MANAGEMENT
% Track research goals, dependencies, and progress
%═══════════════════════════════════════════════════════════

:- module(goals, [
    define_goal/3,
    track_goal/3,
    goal_status/2,
    check_dependencies/2,
    update_progress/3,
    find_blockers/2,
    goal_tree/2
]).

:- use_module('../core/kb').
:- dynamic goal/4.
:- dynamic goal_dependency/2.
:- dynamic goal_progress/3.

%═══════════════════════════════════════════════════════════
% GOAL DEFINITION
%═══════════════════════════════════════════════════════════

% Goal states
goal_state(pending).
goal_state(in_progress).
goal_state(blocked).
goal_state(completed).
goal_state(failed).

% Define a new goal
% define_goal(+GoalID, +Description, +Dependencies)
define_goal(GoalID, Description, Dependencies) :-
    \+ goal(GoalID, _, _, _),  % Not already defined
    assertz(goal(GoalID, Description, pending, 0.0)),
    forall(
        member(Dep, Dependencies),
        assertz(goal_dependency(GoalID, Dep))
    ).

% Update goal description
update_goal_description(GoalID, NewDescription) :-
    retract(goal(GoalID, _, Status, Progress)),
    assertz(goal(GoalID, NewDescription, Status, Progress)).

%═══════════════════════════════════════════════════════════
% GOAL STATUS MANAGEMENT
%═══════════════════════════════════════════════════════════

% Get current goal status
goal_status(GoalID, Status) :-
    goal(GoalID, _, Status, _).

% Change goal status
set_goal_status(GoalID, NewStatus) :-
    goal_state(NewStatus),
    retract(goal(GoalID, Desc, _, Progress)),
    assertz(goal(GoalID, Desc, NewStatus, Progress)),
    log_status_change(GoalID, NewStatus).

% Automatically update status based on dependencies and progress
auto_update_status(GoalID) :-
    goal(GoalID, _, _, Progress),
    (   Progress >= 1.0
    ->  set_goal_status(GoalID, completed)
    ;   \+ all_dependencies_met(GoalID)
    ->  set_goal_status(GoalID, blocked)
    ;   Progress > 0
    ->  set_goal_status(GoalID, in_progress)
    ;   set_goal_status(GoalID, pending)
    ).

%═══════════════════════════════════════════════════════════
% DEPENDENCY MANAGEMENT
%═══════════════════════════════════════════════════════════

% Check if all dependencies are completed
all_dependencies_met(GoalID) :-
    forall(
        goal_dependency(GoalID, DepID),
        goal_status(DepID, completed)
    ).

% Get list of dependencies
check_dependencies(GoalID, Dependencies) :-
    findall(
        dep(DepID, Status),
        (goal_dependency(GoalID, DepID), goal_status(DepID, Status)),
        Dependencies
    ).

% Find goals that are blocking this goal
find_blockers(GoalID, Blockers) :-
    findall(
        blocker(DepID, Status),
        (
            goal_dependency(GoalID, DepID),
            goal_status(DepID, Status),
            Status \= completed
        ),
        Blockers
    ).

% Add dependency to existing goal
add_dependency(GoalID, DepID) :-
    goal(GoalID, _, _, _),
    goal(DepID, _, _, _),
    \+ goal_dependency(GoalID, DepID),
    assertz(goal_dependency(GoalID, DepID)),
    auto_update_status(GoalID).

%═══════════════════════════════════════════════════════════
% PROGRESS TRACKING
%═══════════════════════════════════════════════════════════

% Update goal progress (0.0 to 1.0)
update_progress(GoalID, Progress, Notes) :-
    Progress >= 0.0,
    Progress =< 1.0,
    retract(goal(GoalID, Desc, Status, _)),
    assertz(goal(GoalID, Desc, Status, Progress)),
    get_time(Timestamp),
    assertz(goal_progress(GoalID, Timestamp, progress(Progress, Notes))),
    auto_update_status(GoalID).

% Get progress history
progress_history(GoalID, History) :-
    findall(
        entry(Time, Prog),
        goal_progress(GoalID, Time, Prog),
        History
    ).

% Compute progress velocity (rate of change)
progress_velocity(GoalID, Velocity) :-
    findall(
        Time-Progress,
        (
            goal_progress(GoalID, Time, progress(Progress, _)),
            number(Progress)
        ),
        Entries
    ),
    length(Entries, N),
    N >= 2,
    sort(Entries, Sorted),
    nth0(0, Sorted, FirstTime-FirstProg),
    last(Sorted, LastTime-LastProg),
    TimeDelta is LastTime - FirstTime,
    TimeDelta > 0,
    Velocity is (LastProg - FirstProg) / TimeDelta.

progress_velocity(_, 0.0).  % Default if insufficient data

%═══════════════════════════════════════════════════════════
% GOAL TRACKING
%═══════════════════════════════════════════════════════════

% Track goal and return status with recommendations
track_goal(GoalID, Status, Recommendations) :-
    goal(GoalID, _, Status, Progress),
    find_blockers(GoalID, Blockers),
    progress_velocity(GoalID, Velocity),
    generate_recommendations(GoalID, Blockers, Velocity, Progress, Recommendations).

% Generate recommendations based on goal state
generate_recommendations(GoalID, Blockers, Velocity, Progress, Recommendations) :-
    findall(
        Rec,
        recommendation_rule(GoalID, Blockers, Velocity, Progress, Rec),
        Recommendations
    ).

% Recommendation rules
recommendation_rule(_, [], _, Progress, "Goal is on track") :-
    Progress > 0.5.

recommendation_rule(_, Blockers, _, _, Rec) :-
    length(Blockers, N),
    N > 0,
    format(atom(Rec), 'Resolve ~w blocking dependencies', [N]).

recommendation_rule(_, [], Velocity, Progress, "Progress is too slow") :-
    Velocity < 0.01,
    Progress < 0.5.

recommendation_rule(GoalID, [], Velocity, Progress, Rec) :-
    Velocity > 0,
    Progress > 0,
    Progress < 1.0,
    RemainingProgress is 1.0 - Progress,
    EstimatedTime is RemainingProgress / Velocity,
    format(atom(Rec), 'Estimated completion: ~2f time units', [EstimatedTime]).

%═══════════════════════════════════════════════════════════
% GOAL HIERARCHIES
%═══════════════════════════════════════════════════════════

% Build goal dependency tree
goal_tree(GoalID, Tree) :-
    goal(GoalID, Desc, Status, Progress),
    findall(
        SubTree,
        (goal_dependency(GoalID, DepID), goal_tree(DepID, SubTree)),
        SubTrees
    ),
    Tree = node(GoalID, Desc, Status, Progress, SubTrees).

% Get all leaf goals (no dependencies)
leaf_goals(Leaves) :-
    findall(
        GoalID,
        (goal(GoalID, _, _, _), \+ goal_dependency(_, GoalID)),
        Leaves
    ).

% Get all root goals (not a dependency of anything)
root_goals(Roots) :-
    findall(
        GoalID,
        (goal(GoalID, _, _, _), \+ goal_dependency(GoalID, _)),
        Roots
    ).

%═══════════════════════════════════════════════════════════
% GOAL SEARCH AND QUERIES
%═══════════════════════════════════════════════════════════

% Find goals matching description pattern
find_goals_by_description(Pattern, Goals) :-
    findall(
        goal(GoalID, Desc, Status, Progress),
        (
            goal(GoalID, Desc, Status, Progress),
            sub_atom(Desc, _, _, _, Pattern)
        ),
        Goals
    ).

% Find goals by status
goals_by_status(Status, Goals) :-
    findall(
        GoalID,
        goal_status(GoalID, Status),
        Goals
    ).

% Find stalled goals (in progress but low velocity)
stalled_goals(Stalled) :-
    findall(
        GoalID,
        (
            goal_status(GoalID, in_progress),
            progress_velocity(GoalID, V),
            V < 0.001
        ),
        Stalled
    ).

%═══════════════════════════════════════════════════════════
% LOGGING AND REPORTING
%═══════════════════════════════════════════════════════════

:- dynamic goal_log/3.

% Log status change
log_status_change(GoalID, NewStatus) :-
    get_time(Timestamp),
    assertz(goal_log(Timestamp, GoalID, status_change(NewStatus))).

% Generate goal report
goal_report(GoalID) :-
    goal(GoalID, Desc, Status, Progress),
    format('~n═══════════════════════════════════════════════~n'),
    format('   GOAL REPORT: ~w~n', [GoalID]),
    format('═══════════════════════════════════════════════~n~n'),
    format('Description: ~w~n', [Desc]),
    format('Status: ~w~n', [Status]),
    format('Progress: ~2f%~n', [Progress * 100]),

    check_dependencies(GoalID, Deps),
    length(Deps, NumDeps),
    format('Dependencies: ~w~n', [NumDeps]),
    forall(
        member(dep(DepID, DepStatus), Deps),
        format('  - ~w: ~w~n', [DepID, DepStatus])
    ),

    find_blockers(GoalID, Blockers),
    length(Blockers, NumBlockers),
    (   NumBlockers > 0
    ->  format('~nBlockers: ~w~n', [NumBlockers]),
        forall(
            member(blocker(BID, BStatus), Blockers),
            format('  - ~w: ~w~n', [BID, BStatus])
        )
    ;   true
    ),

    progress_velocity(GoalID, Velocity),
    format('~nProgress velocity: ~4f/time unit~n', [Velocity]),

    track_goal(GoalID, _, Recommendations),
    format('~nRecommendations:~n'),
    forall(
        member(Rec, Recommendations),
        format('  • ~w~n', [Rec])
    ),
    format('~n═══════════════════════════════════════════════~n~n').

% Summary report for all goals
summary_report :-
    format('~n═══════════════════════════════════════════════~n'),
    format('   GOAL TRACKING SUMMARY~n'),
    format('═══════════════════════════════════════════════~n~n'),

    goals_by_status(pending, Pending),
    length(Pending, NPending),
    format('Pending: ~w~n', [NPending]),

    goals_by_status(in_progress, InProgress),
    length(InProgress, NInProgress),
    format('In Progress: ~w~n', [NInProgress]),

    goals_by_status(blocked, Blocked),
    length(Blocked, NBlocked),
    format('Blocked: ~w~n', [NBlocked]),

    goals_by_status(completed, Completed),
    length(Completed, NCompleted),
    format('Completed: ~w~n', [NCompleted]),

    Total is NPending + NInProgress + NBlocked + NCompleted,
    (   Total > 0
    ->  CompletionRate is NCompleted / Total,
        format('~nCompletion rate: ~2f%~n', [CompletionRate * 100])
    ;   format('~nNo goals defined~n')
    ),

    format('~n═══════════════════════════════════════════════~n~n').

%═══════════════════════════════════════════════════════════
% TESTING
%═══════════════════════════════════════════════════════════

run_tests :-
    format('~nRunning goal management tests...~n'),

    % Clean slate
    retractall(goal(_, _, _, _)),
    retractall(goal_dependency(_, _)),
    retractall(goal_progress(_, _, _)),

    % Test 1: Define goal
    define_goal(test_goal_1, "Test goal 1", []),
    (   goal(test_goal_1, _, pending, 0.0)
    ->  format('Test 1 - Define goal: PASS~n')
    ;   format('Test 1 - Define goal: FAIL~n')
    ),

    % Test 2: Update progress
    update_progress(test_goal_1, 0.5, "Halfway done"),
    (   goal(test_goal_1, _, in_progress, 0.5)
    ->  format('Test 2 - Update progress: PASS~n')
    ;   format('Test 2 - Update progress: FAIL~n')
    ),

    % Test 3: Dependencies
    define_goal(test_goal_2, "Test goal 2", [test_goal_1]),
    (   goal_dependency(test_goal_2, test_goal_1),
        goal_status(test_goal_2, blocked)
    ->  format('Test 3 - Dependencies: PASS~n')
    ;   format('Test 3 - Dependencies: FAIL~n')
    ),

    % Test 4: Complete goal
    update_progress(test_goal_1, 1.0, "Complete"),
    (   goal_status(test_goal_1, completed),
        goal_status(test_goal_2, pending)  % Unblocked
    ->  format('Test 4 - Completion: PASS~n')
    ;   format('Test 4 - Completion: FAIL~n')
    ),

    format('~n').
