# MRP Sensitivity Power Analysis
# What effect sizes could the MRP study detect given its sample size?
#
# Context: The MRP study (n=9, 4 trained / 5 untrained) was originally powered using
# Coyle et al. (1984) detraining data (d=1.41). Since no detraining control was used,
# a sensitivity analysis is more appropriate for the actual design.

library(pwr)

cat("========================================\n")
cat("MRP Sensitivity Power Analysis\n")
cat("========================================\n\n")

# --- Overall sample (paired comparisons, n=9) ---
cat("--- Within-Subject Comparisons (n=9 paired) ---\n\n")

for (power in c(0.80, 0.90, 0.95)) {
  result <- pwr.t.test(n = 9, sig.level = 0.05, power = power, type = "paired")
  cat(sprintf("  Power = %.0f%%: minimum detectable d = %.2f\n", power * 100, result$d))
}

cat("\n  Interpretation: Only LARGE effects (d > 1.0) are reliably detectable.\n")
cat("  This is appropriate for a feasibility/proof-of-concept study.\n")
cat("  Results should be reported primarily through effect sizes and CIs.\n\n")

# --- Between-group comparisons (trained n=4 vs untrained n=5) ---
cat("--- Between-Group Comparisons (trained n=4 vs untrained n=5) ---\n\n")

for (power in c(0.80, 0.90)) {
  result <- pwr.t2n.test(n1 = 4, n2 = 5, sig.level = 0.05, power = power)
  cat(sprintf("  Power = %.0f%%: minimum detectable d = %.2f\n", power * 100, result$d))
}

cat("\n  Interpretation: Between-group comparisons require VERY large effects (d > 2).\n")
cat("  Trained vs. untrained analysis should be treated as EXPLORATORY.\n")
cat("  This addresses JSCR Reviewer #3's concern about splitting n=9 into subgroups.\n\n")

# --- What power did we have for the observed effects? ---
cat("--- Observed Effect Context ---\n\n")
cat("  Typical observed Hedges' g in this study: 0.5 - 2.0 (strength measures)\n")
cat("  For the median observed g ~ 1.0:\n")

result_observed <- pwr.t.test(n = 9, d = 1.0, sig.level = 0.05, type = "paired")
cat(sprintf("    Power at d=1.0: %.1f%%\n", result_observed$power * 100))

result_small <- pwr.t.test(n = 9, d = 0.5, sig.level = 0.05, type = "paired")
cat(sprintf("    Power at d=0.5: %.1f%%\n", result_small$power * 100))

cat("\n  This confirms the study was well-powered for large within-subject effects\n")
cat("  (strength gains) but underpowered for moderate effects (VO2, body comp).\n")

# --- A priori context ---
cat("\n--- Original A Priori Calculation ---\n\n")
cat("  Based on Coyle et al. (1984): d = 1.41 for VO2max detraining\n")
cat("  Required n for paired t-test at 80% power: ")
result_apriori <- pwr.t.test(d = 1.41, sig.level = 0.05, power = 0.80, type = "paired")
cat(sprintf("%.0f\n", ceiling(result_apriori$n)))
cat("  Note: This reference assumed a detraining control group that was not completed.\n")
cat("  The sensitivity analysis above reflects the actual study design.\n")
