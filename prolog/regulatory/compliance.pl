% Regulatory Compliance Engine - Prolog Implementation
% FDA, NHTSA, FAA rule checking and validation

:- dynamic device/4.
:- dynamic drug/4.
:- dynamic vehicle/4.
:- dynamic aircraft/4.
:- dynamic recall/5.
:- dynamic adverse_event/5.

% ============================================================================
% FDA REGULATIONS
% ============================================================================

% Device Classes (21 CFR Part 860)
device_class(class_i, low_risk, general_controls).
device_class(class_ii, moderate_risk, special_controls).
device_class(class_iii, high_risk, premarket_approval).

% device(DeviceID, Name, Class, Status)
device(dev_001, 'Motor Hand Prosthetic', class_ii, approved).
device(dev_002, 'Cardiac Monitor AI', class_iii, pending).
device(dev_003, 'Blood Glucose Sensor', class_ii, approved).

% Drug Approval Status (21 CFR Part 314)
drug_status(investigational_new_drug, phase_1).
drug_status(investigational_new_drug, phase_2).
drug_status(investigational_new_drug, phase_3).
drug_status(new_drug_application, under_review).
drug_status(approved, marketed).

% drug(DrugID, Name, Status, Phase)
drug(drug_001, 'Primal-Logic-Enhanced Antiarrhythmic', investigational_new_drug, phase_2).
drug(drug_002, 'Quantum Memory Optimizer', investigational_new_drug, phase_1).

% Check if device requires FDA approval
requires_fda_approval(DeviceID) :-
    device(DeviceID, _, Class, _),
    device_class(Class, Risk, _),
    member(Risk, [moderate_risk, high_risk]).

% Validate device compliance
device_compliant(DeviceID) :-
    device(DeviceID, _, Class, Status),
    device_class(Class, _, Control),
    (Status = approved ; Status = cleared),
    has_required_controls(DeviceID, Control).

has_required_controls(_, general_controls).
has_required_controls(DeviceID, special_controls) :-
    has_510k_clearance(DeviceID).
has_required_controls(DeviceID, premarket_approval) :-
    has_pma_approval(DeviceID).

has_510k_clearance(dev_001).
has_pma_approval(dev_002).

% Adverse Event Reporting (21 CFR Part 803)
% adverse_event(EventID, DeviceID, Severity, Date, Reported)
adverse_event(ae_001, dev_001, serious, '2024-11-15', true).
adverse_event(ae_002, dev_003, minor, '2024-12-01', false).

% Check if adverse event requires reporting
requires_reporting(EventID) :-
    adverse_event(EventID, _, Severity, Date, Reported),
    member(Severity, [serious, life_threatening]),
    Reported = false,
    days_since(Date, Days),
    Days < 30.  % Must report within 30 days

% ============================================================================
% NHTSA REGULATIONS (Vehicle Safety)
% ============================================================================

% vehicle(VehicleID, Make, Model, Year)
vehicle(veh_001, tesla, model_s, 2024).
vehicle(veh_002, spacex, cybertruck, 2025).

% FMVSS (Federal Motor Vehicle Safety Standards)
fmvss_standard(fmvss_208, occupant_crash_protection).
fmvss_standard(fmvss_226, ejection_mitigation).
fmvss_standard(fmvss_305, electric_vehicle_safety).

% Check vehicle compliance
vehicle_compliant(VehicleID) :-
    vehicle(VehicleID, Make, Model, Year),
    meets_fmvss(VehicleID, fmvss_208),
    meets_fmvss(VehicleID, fmvss_226),
    (is_electric(Make, Model) -> meets_fmvss(VehicleID, fmvss_305) ; true).

meets_fmvss(veh_001, fmvss_208).
meets_fmvss(veh_001, fmvss_226).
meets_fmvss(veh_001, fmvss_305).
is_electric(tesla, _).

% Recall Management
% recall(RecallID, VehicleID, Reason, Date, Status)
recall(rec_001, veh_001, 'Autopilot software issue', '2024-10-01', active).
recall(rec_002, veh_002, 'Brake calibration', '2024-11-15', remedied).

% Check if vehicle has active recalls
has_active_recall(VehicleID) :-
    recall(_, VehicleID, _, _, active).

% Safety rating (NCAP)
safety_rating(VehicleID, Rating) :-
    vehicle(VehicleID, _, _, _),
    ncap_score(VehicleID, Score),
    (Score >= 4.5 -> Rating = five_star ;
     Score >= 3.5 -> Rating = four_star ;
     Score >= 2.5 -> Rating = three_star ;
     Rating = below_standard).

ncap_score(veh_001, 4.8).
ncap_score(veh_002, 4.2).

% ============================================================================
% FAA REGULATIONS (Aviation)
% ============================================================================

% aircraft(AircraftID, Manufacturer, Model, Category)
aircraft(air_001, spacex, starship, experimental).
aircraft(air_002, nasa, x57_maxwell, experimental).

% Airworthiness Certification (14 CFR Part 21)
airworthiness_category(standard, normal).
airworthiness_category(standard, utility).
airworthiness_category(special, experimental).

% Check aircraft certification
aircraft_certified(AircraftID) :-
    aircraft(AircraftID, _, _, Category),
    airworthiness_category(_, Category),
    has_airworthiness_certificate(AircraftID).

has_airworthiness_certificate(air_001).
has_airworthiness_certificate(air_002).

% Flight Operations (14 CFR Part 91)
flight_authorized(AircraftID, Pilot, Weather) :-
    aircraft_certified(AircraftID),
    pilot_qualified(Pilot, AircraftID),
    weather_acceptable(Weather).

pilot_qualified(Pilot, AircraftID) :-
    pilot_license(Pilot, License),
    aircraft(AircraftID, _, _, Category),
    license_covers_category(License, Category).

pilot_license(john_doe, atp).  % Airline Transport Pilot
pilot_license(jane_smith, commercial).

license_covers_category(atp, _).
license_covers_category(commercial, normal).
license_covers_category(commercial, utility).

weather_acceptable(Weather) :-
    weather_visibility(Weather, Visibility),
    weather_ceiling(Weather, Ceiling),
    Visibility >= 3,  % 3 statute miles
    Ceiling >= 1000.  % 1000 feet AGL

weather_visibility(vfr, 5).
weather_ceiling(vfr, 3000).

% ============================================================================
% CROSS-REGULATORY COMPLIANCE
% ============================================================================

% Check if autonomous vehicle meets all regulations
autonomous_vehicle_compliant(VehicleID) :-
    vehicle_compliant(VehicleID),
    \+ has_active_recall(VehicleID),
    safety_rating(VehicleID, Rating),
    member(Rating, [five_star, four_star]),
    software_certified(VehicleID).

software_certified(veh_001).

% Check if medical device in vehicle meets FDA + NHTSA
medical_device_in_vehicle_compliant(DeviceID, VehicleID) :-
    device_compliant(DeviceID),
    vehicle_compliant(VehicleID),
    device_vehicle_integration_safe(DeviceID, VehicleID).

device_vehicle_integration_safe(dev_003, veh_001).  % Glucose monitor in Tesla

% Space flight compliance (FDA + FAA)
space_mission_compliant(AircraftID, CrewHealth) :-
    aircraft_certified(AircraftID),
    aircraft(AircraftID, _, _, experimental),
    crew_health_certified(CrewHealth),
    radiation_exposure_acceptable(CrewHealth).

crew_health_certified(CrewHealth) :-
    crew_health_status(CrewHealth, Status),
    Status = fit_for_duty.

radiation_exposure_acceptable(CrewHealth) :-
    crew_radiation_dose(CrewHealth, Dose),
    Dose < 1.0.  % 1 Sv NASA limit

crew_health_status(crew_001, fit_for_duty).
crew_radiation_dose(crew_001, 0.35).

% ============================================================================
% RULE VALIDATION & VERIFICATION
% ============================================================================

% Validate all rules are consistent
validate_rules :-
    \+ inconsistent_rule.

inconsistent_rule :-
    device(DeviceID, _, class_iii, approved),
    \+ has_pma_approval(DeviceID).

inconsistent_rule :-
    adverse_event(EventID, _, serious, _, false),
    days_since(_, Days),
    Days > 30.

% Check compliance for all entities
check_all_compliance :-
    writeln('=== FDA Device Compliance ==='),
    forall(device(ID, Name, _, _),
           (device_compliant(ID) ->
               format('✓ ~w (~w) compliant~n', [Name, ID]) ;
               format('✗ ~w (~w) NON-COMPLIANT~n', [Name, ID]))),
    writeln(''),
    writeln('=== NHTSA Vehicle Compliance ==='),
    forall(vehicle(ID, Make, Model, _),
           (vehicle_compliant(ID) ->
               format('✓ ~w ~w (~w) compliant~n', [Make, Model, ID]) ;
               format('✗ ~w ~w (~w) NON-COMPLIANT~n', [Make, Model, ID]))),
    writeln(''),
    writeln('=== FAA Aircraft Compliance ==='),
    forall(aircraft(ID, Mfg, Model, _),
           (aircraft_certified(ID) ->
               format('✓ ~w ~w (~w) certified~n', [Mfg, Model, ID]) ;
               format('✗ ~w ~w (~w) NOT CERTIFIED~n', [Mfg, Model, ID]))).

% ============================================================================
% AUTOMATED REPORTING
% ============================================================================

% Generate compliance report
generate_compliance_report(Report) :-
    findall(device_issue(ID, Reason),
            (device(ID, _, _, _), \+ device_compliant(ID), failure_reason(ID, Reason)),
            DeviceIssues),
    findall(vehicle_issue(ID, Reason),
            (vehicle(ID, _, _, _), \+ vehicle_compliant(ID), failure_reason(ID, Reason)),
            VehicleIssues),
    findall(aircraft_issue(ID, Reason),
            (aircraft(ID, _, _, _), \+ aircraft_certified(ID), failure_reason(ID, Reason)),
            AircraftIssues),
    Report = [devices=DeviceIssues, vehicles=VehicleIssues, aircraft=AircraftIssues].

failure_reason(ID, missing_approval) :-
    device(ID, _, class_iii, Status),
    Status \= approved.

failure_reason(ID, active_recall) :-
    has_active_recall(ID).

failure_reason(ID, no_certificate) :-
    aircraft(ID, _, _, _),
    \+ has_airworthiness_certificate(ID).

% ============================================================================
% PRIMAL LOGIC COMPLIANCE SCORING
% ============================================================================

% Use exponential memory weighting for compliance history
compliance_score(EntityID, CurrentScore, Time, NewScore) :-
    lightfoot_lambda(0.16905),
    Lambda = 0.16905,
    target_score(100),
    Target = 100,
    Error is Target - CurrentScore,
    Gain is 0.5,
    DeltaScore is -Lambda * CurrentScore + Gain * Error,
    NewScore is CurrentScore + DeltaScore * Time.

target_score(100).

% ============================================================================
% UTILITY PREDICATES
% ============================================================================

days_since(Date, Days) :-
    get_time(Now),
    parse_time(Date, Timestamp),
    Days is (Now - Timestamp) / 86400.

% ============================================================================
% EXPORTS & INITIALIZATION
% ============================================================================

:- initialization(writeln('Regulatory Compliance Engine loaded successfully')).

% Query examples:
% ?- device_compliant(dev_001).
% ?- vehicle_compliant(veh_001).
% ?- aircraft_certified(air_001).
% ?- check_all_compliance.
% ?- generate_compliance_report(Report).
