/**
 * Motor Hand Robotic Control - D Language Implementation
 * High-performance multi-actuator control with Primal Logic
 */

module motor_hand;

import std.math : abs, sin, cos, PI;
import std.datetime.stopwatch : StopWatch, AutoStart;
import std.stdio : writeln, writefln;
import std.algorithm : min, max;
import core.time : Duration, msecs, usecs;

// Import Primal Logic kernel
public import primal_kernel;

/// Number of fingers in robotic hand
enum size_t NUM_FINGERS = 5;

/// Number of joints per finger
enum size_t JOINTS_PER_FINGER = 3;

/// Total number of actuators
enum size_t TOTAL_ACTUATORS = NUM_FINGERS * JOINTS_PER_FINGER;

/// Motor control frequency (Hz)
enum double CONTROL_FREQUENCY = 1000.0;  // 1 kHz

/// Control timestep (seconds)
enum double CONTROL_TIMESTEP = 1.0 / CONTROL_FREQUENCY;

/// Maximum motor torque (N·m)
enum double MAX_TORQUE = 5.0;

/// Position limits (radians)
enum double MIN_POSITION = 0.0;
enum double MAX_POSITION = PI / 2.0;  // 90 degrees

/**
 * Finger enumeration
 */
enum Finger : size_t
{
    Thumb = 0,
    Index = 1,
    Middle = 2,
    Ring = 3,
    Pinky = 4
}

/**
 * Joint enumeration
 */
enum Joint : size_t
{
    Proximal = 0,    // Base joint
    Intermediate = 1, // Middle joint
    Distal = 2       // Tip joint
}

/**
 * Actuator State
 */
struct ActuatorState
{
    double position;      // radians
    double velocity;      // rad/s
    double torque;        // N·m
    double target_position;
    double psi;          // Primal Logic state
    PrimalState primal;

    this(double initial_position)
    {
        position = initial_position;
        velocity = 0.0;
        torque = 0.0;
        target_position = initial_position;
        psi = 100.0;
        primal = PrimalState(100.0, 0.0, 1.0);
    }
}

/**
 * Motor Hand Controller
 */
class MotorHandController
{
private:
    ActuatorState[TOTAL_ACTUATORS] actuators;
    MultiActuatorController multi_controller;
    StopWatch timer;
    size_t control_cycle;
    double[] position_history;
    double[] torque_history;

public:
    this()
    {
        // Initialize all actuators
        foreach (ref actuator; actuators)
        {
            actuator = ActuatorState(0.0);
        }

        multi_controller = MultiActuatorController(TOTAL_ACTUATORS);
        timer = StopWatch(AutoStart.yes);
        control_cycle = 0;

        position_history = [];
        torque_history = [];
    }

    /// Get actuator index from finger and joint
    static size_t getActuatorIndex(Finger finger, Joint joint) pure nothrow @nogc
    {
        return finger * JOINTS_PER_FINGER + joint;
    }

    /// Set target position for specific actuator
    void setTargetPosition(Finger finger, Joint joint, double target)
    {
        immutable size_t idx = getActuatorIndex(finger, joint);
        actuators[idx].target_position = clampPosition(target);
    }

    /// Set target positions for entire finger
    void setFingerTarget(Finger finger, double proximal, double intermediate, double distal)
    {
        setTargetPosition(finger, Joint.Proximal, proximal);
        setTargetPosition(finger, Joint.Intermediate, intermediate);
        setTargetPosition(finger, Joint.Distal, distal);
    }

    /// Clamp position to valid range
    static double clampPosition(double pos) pure nothrow @nogc
    {
        return max(MIN_POSITION, min(MAX_POSITION, pos));
    }

    /// Clamp torque to valid range
    static double clampTorque(double torque) pure nothrow @nogc
    {
        return max(-MAX_TORQUE, min(MAX_TORQUE, torque));
    }

    /// Update all actuators (main control loop)
    void update()
    {
        double[TOTAL_ACTUATORS] states;
        double[TOTAL_ACTUATORS] errors;

        // Gather current states and errors
        foreach (i, ref actuator; actuators)
        {
            states[i] = actuator.psi;
            errors[i] = actuator.target_position - actuator.position;
        }

        // Compute multi-actuator control
        auto control_signals = multi_controller.computeControl(states[], errors[]);

        // Apply control to each actuator
        foreach (i, ref actuator; actuators)
        {
            // Update Primal Logic state
            actuator.primal.error = errors[i];
            immutable double dpsi_dt = primalControl(
                actuator.psi,
                errors[i],
                actuator.primal.control_gain,
                actuator.primal.lambda
            );

            actuator.psi = eulerStep(actuator.psi, dpsi_dt, CONTROL_TIMESTEP);

            // Convert control signal to torque
            actuator.torque = clampTorque(control_signals[i]);

            // Simple motor dynamics: τ = I·α (assuming unit inertia)
            immutable double acceleration = actuator.torque;
            actuator.velocity += acceleration * CONTROL_TIMESTEP;

            // Update position
            actuator.position += actuator.velocity * CONTROL_TIMESTEP;
            actuator.position = clampPosition(actuator.position);

            // Damping
            actuator.velocity *= 0.95;

            // Record history
            position_history ~= actuator.position;
            torque_history ~= actuator.torque;
        }

        control_cycle++;
    }

    /// Get current position of actuator
    double getPosition(Finger finger, Joint joint) const pure nothrow @nogc
    {
        immutable size_t idx = getActuatorIndex(finger, joint);
        return actuators[idx].position;
    }

    /// Get current torque of actuator
    double getTorque(Finger finger, Joint joint) const pure nothrow @nogc
    {
        immutable size_t idx = getActuatorIndex(finger, joint);
        return actuators[idx].torque;
    }

    /// Check if all actuators have converged
    bool hasConverged(double tolerance = 0.01) const pure nothrow @nogc
    {
        foreach (ref actuator; actuators)
        {
            if (abs(actuator.position - actuator.target_position) > tolerance)
                return false;
        }
        return true;
    }

    /// Get elapsed time
    double getElapsedTime() const
    {
        return timer.peek.total!"usecs" / 1_000_000.0;
    }

    /// Get control cycle count
    size_t getCycleCount() const pure nothrow @nogc
    {
        return control_cycle;
    }

    /// Print status
    void printStatus() const
    {
        writefln("=== Motor Hand Status (Cycle: %d, Time: %.3f s) ===",
                 control_cycle, getElapsedTime());

        foreach (finger; 0 .. NUM_FINGERS)
        {
            writefln("Finger %d:", finger);
            foreach (joint; 0 .. JOINTS_PER_FINGER)
            {
                immutable size_t idx = finger * JOINTS_PER_FINGER + joint;
                writefln("  Joint %d: pos=%.3f rad, target=%.3f rad, torque=%.3f N·m",
                         joint,
                         actuators[idx].position,
                         actuators[idx].target_position,
                         actuators[idx].torque);
            }
        }
    }
}

/**
 * Predefined Grasp Patterns
 */
struct GraspPattern
{
    static void powerGrasp(MotorHandController controller)
    {
        // Close all fingers for strong grip
        foreach (finger; 0 .. NUM_FINGERS)
        {
            controller.setFingerTarget(
                cast(Finger)finger,
                PI / 3.0,  // 60 degrees
                PI / 2.5,  // ~72 degrees
                PI / 3.0   // 60 degrees
            );
        }
    }

    static void precisionGrasp(MotorHandController controller)
    {
        // Thumb and index finger form circle
        controller.setFingerTarget(Finger.Thumb, PI / 4.0, PI / 3.0, PI / 4.0);
        controller.setFingerTarget(Finger.Index, PI / 3.0, PI / 2.5, PI / 3.0);

        // Other fingers relaxed
        controller.setFingerTarget(Finger.Middle, 0.1, 0.1, 0.1);
        controller.setFingerTarget(Finger.Ring, 0.1, 0.1, 0.1);
        controller.setFingerTarget(Finger.Pinky, 0.1, 0.1, 0.1);
    }

    static void pointingGesture(MotorHandController controller)
    {
        // Index finger extended, others closed
        controller.setFingerTarget(Finger.Thumb, PI / 3.0, PI / 3.0, PI / 3.0);
        controller.setFingerTarget(Finger.Index, 0.0, 0.0, 0.0);  // Extended
        controller.setFingerTarget(Finger.Middle, PI / 2.0, PI / 2.0, PI / 2.0);
        controller.setFingerTarget(Finger.Ring, PI / 2.0, PI / 2.0, PI / 2.0);
        controller.setFingerTarget(Finger.Pinky, PI / 2.0, PI / 2.0, PI / 2.0);
    }

    static void openHand(MotorHandController controller)
    {
        // All fingers fully extended
        foreach (finger; 0 .. NUM_FINGERS)
        {
            controller.setFingerTarget(cast(Finger)finger, 0.0, 0.0, 0.0);
        }
    }

    static void restPosition(MotorHandController controller)
    {
        // Natural resting position (slightly curved)
        foreach (finger; 0 .. NUM_FINGERS)
        {
            controller.setFingerTarget(cast(Finger)finger, 0.1, 0.15, 0.1);
        }
    }
}

/**
 * Trajectory Generator
 */
class TrajectoryGenerator
{
    /// Generate sinusoidal trajectory
    static double[] generateSinusoid(double amplitude, double frequency, double duration, double dt)
    {
        immutable size_t steps = cast(size_t)(duration / dt);
        double[] trajectory = new double[steps];

        foreach (i; 0 .. steps)
        {
            immutable double t = i * dt;
            trajectory[i] = amplitude * sin(2.0 * PI * frequency * t);
        }

        return trajectory;
    }

    /// Generate smooth interpolation between two positions
    static double[] generateInterpolation(double start, double end, double duration, double dt)
    {
        immutable size_t steps = cast(size_t)(duration / dt);
        double[] trajectory = new double[steps];

        foreach (i; 0 .. steps)
        {
            immutable double alpha = cast(double)i / (steps - 1);
            // Smooth interpolation using cosine
            immutable double smooth_alpha = (1.0 - cos(alpha * PI)) / 2.0;
            trajectory[i] = start + smooth_alpha * (end - start);
        }

        return trajectory;
    }
}

/**
 * Performance Benchmark
 */
struct MotorHandBenchmark
{
    size_t control_cycles;
    Duration elapsed_time;
    double frequency;  // Hz
    double avg_latency;  // microseconds

    static MotorHandBenchmark run(size_t cycles)
    {
        auto controller = new MotorHandController();
        GraspPattern.powerGrasp(controller);

        auto sw = StopWatch(AutoStart.yes);

        foreach (i; 0 .. cycles)
        {
            controller.update();
        }

        sw.stop();

        MotorHandBenchmark result;
        result.control_cycles = cycles;
        result.elapsed_time = sw.peek;
        result.frequency = cast(double)cycles / (sw.peek.total!"usecs" / 1_000_000.0);
        result.avg_latency = cast(double)sw.peek.total!"usecs" / cycles;

        return result;
    }
}

void main()
{
    writeln("=== Motor Hand Robotic Control System ===");
    writeln();

    // Create controller
    auto controller = new MotorHandController();

    writeln("Initial state:");
    controller.printStatus();
    writeln();

    // Demonstrate grasp patterns
    writeln("Executing power grasp...");
    GraspPattern.powerGrasp(controller);

    foreach (i; 0 .. 100)
    {
        controller.update();

        if ((i + 1) % 25 == 0)
        {
            writefln("Update %d:", i + 1);
            controller.printStatus();
            writeln();
        }

        if (controller.hasConverged())
        {
            writefln("Converged after %d cycles!", i + 1);
            break;
        }
    }

    // Benchmark performance
    writeln("Running performance benchmark...");
    auto benchmark = MotorHandBenchmark.run(10000);
    writefln("Control Cycles: %d", benchmark.control_cycles);
    writefln("Elapsed Time: %s", benchmark.elapsed_time);
    writefln("Control Frequency: %.2f Hz", benchmark.frequency);
    writefln("Avg Latency: %.2f μs (target: < 100 μs)", benchmark.avg_latency);

    if (benchmark.avg_latency < 100.0)
        writeln("✓ Real-time performance achieved!");
    else
        writeln("⚠ Warning: Latency exceeds 100 μs target");

    writeln();
    writeln("=== Motor Hand Controller Ready ===");
}
