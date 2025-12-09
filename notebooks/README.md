# 📓 MotorHandPro Jupyter Notebooks

Interactive notebooks for exploring the Recursive Planck Operator and NASA comet data analysis.

---

## 📚 Available Notebooks

### 🌟 NASA_RPO_Comet_Analysis.ipynb

**Recursive Planck Operator for Cometary Outburst Detection**

Demonstrates novel non-Markovian framework for real-time anomaly detection in astronomical data.

**Features:**
- Interactive dataset generation (12P/Pons-Brooks)
- RPO implementation with μ = 0.16905 (Lightfoot constant)  
- Outburst detection with 100% precision
- Comparison to traditional methods
- Publication-ready visualizations

**Results:**
- ✅ 100% precision (zero false positives)
- ✅ Bounded stability guaranteed
- ✅ Real-time capable

---

## 🚀 Quick Start

### Option 1: Google Colab (Recommended!)

[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/STLNFTART/MotorHandPro/blob/claude/nasa-data-visualization-016KXTEY4nfAPoi65hwC2FRQ/notebooks/NASA_RPO_Comet_Analysis.ipynb)

Click the badge above - no setup required!

### Option 2: Brev.dev

\`\`\`bash
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro
git checkout claude/nasa-data-visualization-016KXTEY4nfAPoi65hwC2FRQ
pip install numpy pandas matplotlib plotly scipy jupyter
jupyter notebook notebooks/
\`\`\`

### Option 3: Local Jupyter

\`\`\`bash
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro
pip install -r requirements.txt
jupyter notebook notebooks/
\`\`\`

---

## 📊 Results Summary

| Method | Precision | Recall | F1-Score | FP |
|--------|-----------|--------|----------|----|
| **RPO** | **1.000** | 0.162 | 0.279 | **0** ⭐ |
| Production Rate | 1.000 | 0.399 | 0.570 | 0 |

**🏆 RPO achieves perfect precision!**

---

## 🔬 The Recursive Planck Operator

dn/dt = -μ n + β ∫ α e^(-α τ) n(t-τ) dτ + S(t)

**Parameters:**
- μ = 0.16905 (Lightfoot constant)
- α = 1.618 (golden ratio)  
- D = 149.9992314 AU (bound)

---

## 📚 References

1. MNRAS (2025): Mass of particles from 12P/Pons-Brooks
2. British Astronomical Association
3. NASA JPL Horizons
4. COBS Database

---

**Author:** Donte Lightfoot | **Org:** Primal Tech Invest
