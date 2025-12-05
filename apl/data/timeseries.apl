⍝ Time Series Analysis Module
⍝ Array-oriented operations for temporal data processing

⍝═══════════════════════════════════════════════════════════
⍝ MOVING AVERAGES
⍝═══════════════════════════════════════════════════════════

⍝ Simple moving average
⍝ Usage: ma ← window MovingAverage data
∇ ma ← window MovingAverage data
  N ← ≢data
  ma ← (⊂0,window)⌺{(+⌿⍵)÷≢⍵} data
∇

⍝ Exponentially weighted moving average (EWMA)
⍝ Usage: ewma ← alpha EWMA data
⍝ alpha: smoothing factor (0 < alpha < 1)
∇ ewma ← alpha EWMA data
  ⍝ Use Primal Logic's Lightfoot constant as default decay
  lambda ← alpha
  N ← ≢data
  weights ← *(-lambda) × ⌽⍳N
  weights ← weights ÷ +/weights  ⍝ Normalize
  ewma ← +/weights × data
∇

⍝ Primal Logic weighted moving average
⍝ Uses exponential memory weighting with Lightfoot constant
∇ plma ← PrimalMovingAverage data
  plma ← LIGHTFOOT EWMA data
∇

⍝═══════════════════════════════════════════════════════════
⍝ STATISTICAL MEASURES
⍝═══════════════════════════════════════════════════════════

⍝ Rolling standard deviation
⍝ Usage: std ← window RollingStd data
∇ std ← window RollingStd data
  std ← (⊂0,window)⌺{(+⌿(⍵-avg)×2÷≢⍵)*0.5⊣avg←(+⌿⍵)÷≢⍵} data
∇

⍝ Z-score normalization
⍝ Usage: z ← ZScore data
∇ z ← ZScore data
  mean ← (+/data) ÷ ≢data
  std ← ((+/(data-mean)*2) ÷ ≢data)*0.5
  z ← (data - mean) ÷ std
∇

⍝ Min-max normalization to [0, 1]
∇ norm ← MinMaxNorm data
  min_val ← ⌊/data
  max_val ← ⌈/data
  norm ← (data - min_val) ÷ (max_val - min_val)
∇

⍝═══════════════════════════════════════════════════════════
⍝ AUTOCORRELATION
⍝═══════════════════════════════════════════════════════════

⍝ Compute autocorrelation at lag k
⍝ Usage: acf ← k Autocorr data
∇ acf ← k Autocorr data
  N ← ≢data
  mean ← (+/data) ÷ N
  centered ← data - mean

  ⍝ Compute correlation at lag k
  overlap ← (k↓centered) × ((-k)↓centered)
  variance ← (+/centered*2) ÷ N

  acf ← ((+/overlap) ÷ (N-k)) ÷ variance
∇

⍝ Compute ACF for multiple lags
⍝ Usage: acfs ← max_lag ACF data
∇ acfs ← max_lag ACF data
  acfs ← {⍵ Autocorr data}¨ ⍳max_lag
∇

⍝═══════════════════════════════════════════════════════════
⍝ CHANGEPOINT DETECTION
⍝═══════════════════════════════════════════════════════════

⍝ Simple threshold-based changepoint detection
⍝ Returns indices where absolute change exceeds threshold
∇ indices ← threshold DetectChanges data
  diffs ← 1↓data - ¯1↓data
  indices ← ⍸ (|diffs) > threshold
∇

⍝ Cumulative sum (CUSUM) for drift detection
∇ cusum ← CUSUM data
  mean ← (+/data) ÷ ≢data
  cusum ← +\ data - mean
∇

⍝═══════════════════════════════════════════════════════════
⍝ INTERPOLATION
⍝═══════════════════════════════════════════════════════════

⍝ Linear interpolation for missing values (represented as NaN or ¯1)
⍝ Usage: filled ← LinearInterp data
∇ filled ← LinearInterp data
  ⍝ Simple forward fill for now
  filled ← data
  ⍝ TODO: Implement proper linear interpolation
∇

⍝═══════════════════════════════════════════════════════════
⍝ RESAMPLING
⍝═══════════════════════════════════════════════════════════

⍝ Downsample by taking every nth value
∇ downsampled ← n Downsample data
  downsampled ← data[⍳(⌊(≢data)÷n)×n;n]
∇

⍝ Downsample by averaging over windows
∇ averaged ← n DownsampleAverage data
  windows ← (n,¯1)⍴(n×⌊(≢data)÷n)↑data
  averaged ← (+⌿windows)÷n
∇

⍝═══════════════════════════════════════════════════════════
⍝ CROSS-CORRELATION
⍝═══════════════════════════════════════════════════════════

⍝ Cross-correlation between two series
⍝ Usage: cc ← series1 CrossCorr series2
∇ cc ← series1 CrossCorr series2
  ⍝ Ensure same length
  N ← ⌊/ (≢series1)(≢series2)
  s1 ← N↑series1
  s2 ← N↑series2

  ⍝ Normalize
  s1_norm ← s1 - (+/s1)÷N
  s2_norm ← s2 - (+/s2)÷N

  ⍝ Compute correlation
  cc ← (+/s1_norm×s2_norm) ÷ ((+/s1_norm*2)×(+/s2_norm*2))*0.5
∇

⍝═══════════════════════════════════════════════════════════
⍝ PRIMAL LOGIC TIME SERIES
⍝═══════════════════════════════════════════════════════════

⍝ Apply Primal Logic iteration to time series
⍝ Each value goes through fixed-point iteration
∇ transformed ← PrimalTransform data
  transformed ← F¨ data  ⍝ Apply F to each element
∇

⍝ Track quantum field state over time series
∇ states ← TrackQuantumField observations
  initial ← InitQuantumField
  states ← initial{⍺ UpdateQuantumField ⍵}⍣(≢observations) ⊢ observations
∇

⍝ Compute Planck tail for time series
⍝ Returns exponentially weighted values
∇ weighted ← PlanckWeightTimeSeries data
  N ← ≢data
  weights ← *(-LIGHTFOOT) × ⌽⍳N
  weighted ← weights × data
∇

⍝═══════════════════════════════════════════════════════════
⍝ EXPORT FUNCTIONS
⍝═══════════════════════════════════════════════════════════

⍝ Export time series statistics as JSON
∇ json ← ExportStats data
  mean ← (+/data) ÷ ≢data
  variance ← (+/(data-mean)*2) ÷ ≢data
  std ← variance*0.5
  min_val ← ⌊/data
  max_val ← ⌈/data

  json ← '{'
  json ,← '"mean": ', (⍕mean), ','
  json ,← '"std": ', (⍕std), ','
  json ,← '"variance": ', (⍕variance), ','
  json ,← '"min": ', (⍕min_val), ','
  json ,← '"max": ', (⍕max_val), ','
  json ,← '"count": ', (⍕≢data)
  json ,← '}'
∇

⍝ Demo function
∇ Demo
  ⎕ ← '═══════════════════════════════════════════════'
  ⎕ ← '    TIME SERIES ANALYSIS DEMONSTRATION'
  ⎕ ← '═══════════════════════════════════════════════'
  ⎕ ← ''

  ⍝ Generate sample data
  N ← 100
  data ← 150 + 10 × ⍟⍳N  ⍝ Logarithmic trend

  ⎕ ← '🔷 Sample data generated (N=100)'
  ⎕ ← 'First 10 values: ', 10↑data
  ⎕ ← ''

  ⍝ Moving average
  ma ← 5 MovingAverage data
  ⎕ ← '🔷 Moving average (window=5):'
  ⎕ ← 'First 10 values: ', 10↑ma
  ⎕ ← ''

  ⍝ Primal Logic MA
  plma ← PrimalMovingAverage data
  ⎕ ← '🔷 Primal Logic moving average:'
  ⎕ ← 'Result: ', plma
  ⎕ ← ''

  ⍝ Z-score
  z ← ZScore data
  ⎕ ← '🔷 Z-score normalization:'
  ⎕ ← 'First 10 values: ', 10↑z
  ⎕ ← ''

  ⍝ ACF
  acf ← 20 ACF data
  ⎕ ← '🔷 Autocorrelation (first 10 lags):'
  ⎕ ← 10↑acf
  ⎕ ← ''

  ⍝ Export stats
  stats ← ExportStats data
  ⎕ ← '🔷 Statistics (JSON):'
  ⎕ ← stats
  ⎕ ← ''
∇
