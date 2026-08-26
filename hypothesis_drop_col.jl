using DataFrames
using MixedModels
using MixedModelsDatasets
using MixedModelsSim

lmm(@formula(score ~ 1 + Test + (1|Child)), dataset(:fggk21);
            contrasts=Dict(:Test => hyp))

# ────────────────────────────────────────────────────────
#                     Coef.  Std. Error        z  Pr(>|z|)
# ────────────────────────────────────────────────────────
# (Intercept)      3.74214     0.203883    18.35    <1e-74
# Test: Run     1000.21        0.285852  3499.05    <1e-99
# Test: S20_r      0.776042    0.28512      2.72    0.0065
# Test: SLJ      121.892       0.284392   428.61    <1e-99
# Test: Star_r    -1.71462     0.285863    -6.00    <1e-08
# ────────────────────────────────────────────────────────

# julia> sort(unique(dataset(:fggk21).Test))
# 5-element Vector{String}:
#  "BPT"
#  "Run"
#  "S20_r"
#  "SLJ"
#  "Star_r"

hyp = HypothesisCoding(["Run" => [0, 1, 0, 0, 0],
                       "S20_r" => [0, 0, 1, 0, 0],
                       "SLJ" => [0, 0, 0, 1, 0],
                        "Star_r"  => [0, 0, 0, 0, 0]]; 
                       levels=["BPT", "Run", "S20_r", "SLJ", "Star_r"])

mhyp = lmm(@formula(score ~ 1 + Test + (1|Child)), dataset(:fggk21);
            contrasts=Dict(:Test => hyp))
