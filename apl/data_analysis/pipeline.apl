⍝ Data Processing Pipeline - APL Implementation
⍝ High-performance array-based data analytics

⍝ ============================================================================
⍝ TIME SERIES PROCESSING
⍝ ============================================================================

⍝ Moving Average Filter
⍝ Arguments: data vector, window size
MovingAverage ← {
    data window ← ⍵
    n ← ⍴ data
    result ← (n - window + 1) ⍴ 0
    :For i :In ⍳(n - window + 1)
        result[i] ← (+/ data[i + ⍳window]) ÷ window
    :EndFor
    result
}

⍝ Exponential Weighted Moving Average
⍝ EWMA(t) = α·x(t) + (1-α)·EWMA(t-1)
EWMA ← {
    data alpha ← ⍵
    n ← ⍴ data
    ewma ← n ⍴ 0
    ewma[1] ← data[1]
    :For i :In 1 ↓ ⍳n
        ewma[i] ← (alpha × data[i]) + ((1 - alpha) × ewma[i - 1])
    :EndFor
    ewma
}

⍝ Standard Deviation
StdDev ← {
    data ← ⍵
    mean_val ← (+/ data) ÷ ⍴ data
    variance ← (+/ (data - mean_val) * 2) ÷ ⍴ data
    variance * 0.5
}

⍝ Z-Score Normalization
ZScore ← {
    data ← ⍵
    mean_val ← (+/ data) ÷ ⍴ data
    std_val ← StdDev data
    (data - mean_val) ÷ std_val
}

⍝ Min-Max Normalization
MinMaxNorm ← {
    data ← ⍵
    min_val ← ⌊/ data
    max_val ← ⌈/ data
    (data - min_val) ÷ (max_val - min_val)
}

⍝ ============================================================================
⍝ MATRIX OPERATIONS
⍝ ============================================================================

⍝ Covariance Matrix
CovarianceMatrix ← {
    X ← ⍵  ⍝ Matrix: rows=observations, cols=features
    n ← ⊃⍴ X
    X_centered ← X - (+⌿ X) ÷ n
    (X_centered +.× X_centered) ÷ n
}

⍝ Correlation Matrix
CorrelationMatrix ← {
    X ← ⍵
    features ← 1 ↓ ⍴ X
    corr ← features features ⍴ 0
    :For i :In ⍳features
        :For j :In ⍳features
            xi ← X[;i]
            xj ← X[;j]
            corr[i;j] ← (+/ (xi - (+/xi) ÷ ⍴xi) × (xj - (+/xj) ÷ ⍴xj)) ÷
                        ((StdDev xi) × (StdDev xj) × ⍴xi)
        :EndFor
    :EndFor
    corr
}

⍝ Matrix Transpose
Transpose ← {⍉⍵}

⍝ Matrix Determinant (2x2)
Det2x2 ← {
    M ← ⍵
    (M[1;1] × M[2;2]) - (M[1;2] × M[2;1])
}

⍝ ============================================================================
⍝ SIGNAL PROCESSING
⍝ ============================================================================

⍝ Discrete Derivative
Derivative ← {
    data ← ⍵
    1 ↓ data - ¯1 ⌽ data
}

⍝ Discrete Integration (Cumulative Sum)
Integrate ← {+\⍵}

⍝ Peak Detection
⍝ Returns indices of local maxima
PeakDetection ← {
    data threshold ← ⍵
    n ← ⍴ data
    peaks ← 0⍴0
    :For i :In 2 ↓ ⍳(n - 1)
        :If (data[i] > data[i - 1]) ∧ (data[i] > data[i + 1]) ∧ (data[i] > threshold)
            peaks ← peaks, i
        :EndIf
    :EndFor
    peaks
}

⍝ Low-Pass Filter (Simple RC filter simulation)
LowPassFilter ← {
    data cutoff_freq dt ← ⍵
    RC ← 1 ÷ (2 × ○1 × cutoff_freq)  ⍝ ○1 is π
    alpha ← dt ÷ (RC + dt)
    EWMA data alpha
}

⍝ High-Pass Filter
HighPassFilter ← {
    data cutoff_freq dt ← ⍵
    lowpass ← LowPassFilter data cutoff_freq dt
    data - lowpass
}

⍝ ============================================================================
⍝ FOURIER ANALYSIS (Simplified DFT)
⍝ ============================================================================

⍝ Discrete Fourier Transform (magnitude only, simplified)
⍝ Note: APL doesn't have native complex numbers, so this is magnitude approximation
DFT_Magnitude ← {
    data ← ⍵
    N ← ⍴ data
    freq_bins ← ⌊N ÷ 2
    magnitudes ← freq_bins ⍴ 0
    :For k :In ⍳freq_bins
        real_part ← +/ data × 2○(¯2 × (○1) × k × (⍳N) ÷ N)  ⍝ cos
        imag_part ← +/ data × 1○(¯2 × (○1) × k × (⍳N) ÷ N)  ⍝ sin
        magnitudes[k] ← ((real_part * 2) + (imag_part * 2)) * 0.5
    :EndFor
    magnitudes
}

⍝ ============================================================================
⍝ STATISTICAL ANALYSIS
⍝ ============================================================================

⍝ Quartiles
Quartiles ← {
    data ← ⍵
    sorted ← data[⍋data]  ⍝ Sort ascending
    n ← ⍴ sorted
    q1_idx ← ⌊n ÷ 4
    q2_idx ← ⌊n ÷ 2
    q3_idx ← ⌊(3 × n) ÷ 4
    sorted[q1_idx], sorted[q2_idx], sorted[q3_idx]
}

⍝ Interquartile Range
IQR ← {
    q ← Quartiles ⍵
    q[3] - q[1]
}

⍝ Outlier Detection (IQR method)
OutlierDetection ← {
    data ← ⍵
    q ← Quartiles data
    iqr ← IQR data
    lower_bound ← q[1] - (1.5 × iqr)
    upper_bound ← q[3] + (1.5 × iqr)
    outliers ← (data < lower_bound) ∨ (data > upper_bound)
    outliers
}

⍝ Skewness
Skewness ← {
    data ← ⍵
    n ← ⍴ data
    mean_val ← (+/ data) ÷ n
    std_val ← StdDev data
    ((+/ ((data - mean_val) ÷ std_val) * 3) ÷ n)
}

⍝ Kurtosis
Kurtosis ← {
    data ← ⍵
    n ← ⍴ data
    mean_val ← (+/ data) ÷ n
    std_val ← StdDev data
    ((+/ ((data - mean_val) ÷ std_val) * 4) ÷ n) - 3
}

⍝ ============================================================================
⍝ ANOMALY DETECTION
⍝ ============================================================================

⍝ Z-Score Anomaly Detection
ZScoreAnomaly ← {
    data threshold ← ⍵
    z_scores ← ZScore data
    |z_scores > threshold
}

⍝ Moving Average Anomaly Detection
MAAnomaly ← {
    data window threshold ← ⍵
    ma ← MovingAverage data window
    deviation ← |data[window ↓ ⍳⍴data] - ma
    deviation > threshold
}

⍝ ============================================================================
⍝ DATA AGGREGATION
⍝ ============================================================================

⍝ Group By and Aggregate
⍝ Simple binning aggregation
BinAggregate ← {
    data bin_size agg_func ← ⍵
    n ← ⍴ data
    num_bins ← ⌈n ÷ bin_size
    result ← num_bins ⍴ 0
    :For i :In ⍳num_bins
        start_idx ← ((i - 1) × bin_size) + 1
        end_idx ← ⌊(i × bin_size) ⌊ n
        bin_data ← data[start_idx + ⍳(end_idx - start_idx + 1)]
        result[i] ← agg_func bin_data
    :EndFor
    result
}

⍝ Downsample by averaging
Downsample ← {
    data factor ← ⍵
    BinAggregate data factor {(+/⍵) ÷ ⍴⍵}
}

⍝ ============================================================================
⍝ PRIMAL LOGIC DATA INTEGRATION
⍝ ============================================================================

⍝ Apply Primal Logic exponential weighting to data stream
PrimalWeightedData ← {
    data lambda ← ⍵
    n ← ⍴ data
    weights ← *(-lambda × ⍳n)
    weighted_data ← data × weights
    weighted_data
}

⍝ Exponential Weighted Statistics
PrimalMean ← {
    data lambda ← ⍵
    weighted_data ← PrimalWeightedData data lambda
    weights ← *(-lambda × ⍳⍴data)
    (+/ weighted_data) ÷ (+/ weights)
}

⍝ ============================================================================
⍝ PIPELINE COMPOSITION
⍝ ============================================================================

⍝ Complete Data Processing Pipeline
DataPipeline ← {
    raw_data ← ⍵

    ⍝ Step 1: Normalization
    normalized ← MinMaxNorm raw_data

    ⍝ Step 2: Noise reduction (moving average)
    filtered ← MovingAverage normalized 5

    ⍝ Step 3: Anomaly detection
    anomalies ← ZScoreAnomaly filtered 3

    ⍝ Step 4: Statistical summary
    mean_val ← (+/ filtered) ÷ ⍴ filtered
    std_val ← StdDev filtered
    quartiles ← Quartiles filtered

    ⍝ Return results
    filtered, anomalies, mean_val, std_val, quartiles
}

⍝ Real-Time Stream Processor
⍝ Maintains state for online processing
StreamProcessor ← {
    new_data buffer_size lambda ← ⍵

    ⍝ Simulated circular buffer
    buffer ← new_data

    ⍝ Online statistics
    online_mean ← (+/ buffer) ÷ ⍴ buffer
    online_ewma ← EWMA buffer 0.1

    ⍝ Primal Logic weighted mean
    primal_mean ← PrimalMean buffer lambda

    online_mean, (⊃⌽online_ewma), primal_mean
}

⍝ ============================================================================
⍝ BENCHMARK
⍝ ============================================================================

BenchmarkPipeline ← {
    iterations ← ⍵
    test_data ← 1000 ⍴ 0
    test_data ← test_data + (? 1000 ⍴ 0) × 100

    start_time ← ⎕AI[3]
    :For i :In ⍳iterations
        result ← DataPipeline test_data
    :EndFor
    end_time ← ⎕AI[3]

    elapsed_ms ← end_time - start_time
    throughput ← iterations ÷ (elapsed_ms ÷ 1000)
    elapsed_ms, throughput
}

⍝ ============================================================================
⍝ EXPORTS
⍝ ============================================================================

∇ Z ← FFI_DataPipeline data
  Z ← DataPipeline data
∇

∇ Z ← FFI_MovingAverage params
  Z ← MovingAverage params
∇

∇ Z ← FFI_EWMA params
  Z ← EWMA params
∇

⍝ Example Usage:
⍝ data ← 100 ⍴ 0
⍝ data ← data + (? 100 ⍴ 0) × 50
⍝ result ← DataPipeline data
⍝ ma ← MovingAverage data 5
⍝ anomalies ← ZScoreAnomaly data 2.5
