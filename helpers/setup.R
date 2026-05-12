# setup.R -- Shared preamble for MRP analysis Rmd scripts
#
# Source this at the top of every analysis script:
#   source("helpers/setup.R")
#
# Provides: all packages loaded, shared functions, NPG + MRP colors,
# output directory, namespace conflict resolution, reproducibility seed.
#
# paths.R (sourced below) captures an absolute PROJECT_ROOT, so no
# setwd() or opts_knit$set(root.dir=...) is needed.

# ---- Project paths (single source of truth) ----
if (file.exists("helpers/paths.R")) {
  source("helpers/paths.R")
} else if (file.exists("../helpers/paths.R")) {
  source("../helpers/paths.R")
} else {
  stop("setup.R: cannot locate helpers/paths.R from ", getwd())
}
if (interactive()) cat("Project root:", PROJECT_ROOT, "\n")

# ---- Reproducibility ----
set.seed(0308)

# ---- Load shared functions ----
source(file.path(HELPERS_DIR, "functions.R"))

# ---- Required packages ----
required_packages <- c(
  # Data handling
  "tidyverse", "readxl", "writexl", "openxlsx", "dplyr", "tidyr",
  "stringr", "purrr",
  # Statistical analysis
  "car", "broom", "broom.mixed", "lme4", "lmerTest", "emmeans",
  "effectsize", "pwr",
  # Visualization
  "ggplot2", "ggpubr", "ggsignif", "cowplot", "gridExtra", "ggrepel",
  "ggnewscale", "corrplot", "ggtext", "ggsci", "viridis",
  # Reporting
  "knitr", "kableExtra", "flextable",
  # Namespace management
  "conflicted"
)
required_packages <- unique(required_packages)
install_and_load_packages(required_packages)

# ---- Namespace conflict resolution ----
if (requireNamespace("MASS", quietly = TRUE)) library(MASS)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::recode)
conflicted::conflicts_prefer(dplyr::count)
conflicted::conflicts_prefer(dplyr::summarize)
conflicted::conflicts_prefer(dplyr::summarise)
conflicted::conflicts_prefer(dplyr::rename)
conflicted::conflicts_prefer(dplyr::mutate)
conflicted::conflicts_prefer(dplyr::arrange)
conflicted::conflicts_prefer(dplyr::desc)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(dplyr::between)
conflicted::conflicts_prefer(dplyr::slice)
conflicted::conflicts_prefer(tidyr::expand)
conflicted::conflicts_prefer(tidyr::pack)
conflicted::conflicts_prefer(tidyr::unpack)
conflicted::conflicts_prefer(purrr::map)
conflicted::conflicts_prefer(purrr::discard)
conflicted::conflicts_prefer(base::union)
conflicted::conflicts_prefer(base::intersect)
conflicted::conflicts_prefer(base::setdiff)
conflicted::conflicts_prefer(lmerTest::lmer)
conflicted::conflicts_prefer(stats::sigma)
conflicted::conflicts_prefer(scales::alpha)

# ggpubr vs flextable vs cowplot vs lubridate
conflicted::conflicts_prefer(ggpubr::rotate)
conflicted::conflicts_prefer(purrr::some)
conflicted::conflicts_prefer(lubridate::stamp)
conflicted::conflicts_prefer(stats::step)

# ---- Audit unresolved conflicts ----
remaining <- tryCatch(conflicted::conflict_scout(), error = function(e) NULL)
if (!is.null(remaining) && length(remaining) > 0) {
  message("Unresolved conflicts after setup.R preferences:")
  print(remaining)
}

# ---- NPG color palette (for table formatting) ----
npg_colors <- get_npg_colors()

# ---- Conversion constants ----
lbs_to_kg <- 0.45359237

if (interactive()) {
  cat(sprintf("Output directory: %s\n", output_dir))
  cat("MRP setup complete.\n")
}
