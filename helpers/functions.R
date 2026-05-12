# functions.R -- Shared helper functions for MRP analysis
#
# Adapted from the Physiological_Data project's functions_v2.R for the
# MRP study's measures and conventions. Sourced by setup.R.
#
# Contents:
#   - Package management: install_and_load_packages()
#   - NPG colors: get_npg_colors()
#   - MRP custom colors: dark_blue_color, light_blue_color, third_color,
#     mrp_time_colors, mrp_training_colors, scale_fill/color helpers
#   - Number formatters: fmt0-fmt3, fmt_pct
#   - Table formatters: fmt_p(), fmt_p_text(), fmt_g(), fmt_mean_sd(),
#     create_styled_kable()
#   - Effect sizes: calculate_hedges_g(), hedges_g_paired(),
#     hedges_g_independent(), interpret_hedges_g()
#   - Analysis: calculate_percent_change()
#   - Measure metadata: get_measure_digits(), get_formatted_label()
#   - Data helpers: add_training_status()


# ============================================================================
# Package management
# ============================================================================

install_and_load_packages <- function(packages) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  total_packages <- length(packages); current_package <- 0
  update_progress <- function(pkg, status) {
    current_package <<- current_package + 1
    if (interactive()) {
      cat(sprintf("\r[%d/%d] %s: %s", current_package, total_packages, pkg, status))
      flush.console()
    }
  }
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    message("\nInstalling missing packages...")
    for (pkg in missing_packages) {
      update_progress(pkg, "Installing")
      tryCatch(install.packages(pkg, dependencies = TRUE, quiet = TRUE, ask = FALSE),
               error = function(e) {
                 update_progress(pkg, "Failed")
                 warning(sprintf("Failed to install: %s\n%s", pkg, e$message))
               })
    }
  }
  failed_packages <- character(0)
  for (pkg in packages) {
    update_progress(pkg, "Loading")
    if (!require(pkg, character.only = TRUE, quietly = TRUE))
      failed_packages <- c(failed_packages, pkg)
  }
  if (interactive()) cat("\nPackage loading complete.\n")
  if (length(failed_packages) > 0)
    warning(sprintf("Failed packages: %s", paste(failed_packages, collapse = ", ")))
  invisible(list(success = packages[!packages %in% failed_packages],
                 failed = failed_packages))
}


# ============================================================================
# Color palettes
# ============================================================================

#' NPG color palette (for table cell_spec formatting)
get_npg_colors <- function() {
  ggsci::pal_npg()(9)
}
# Indices: [1] Red  [2] Cyan  [3] Green  [4] Blue  [5] Salmon
#          [6] Lavender  [7] Teal  [8] DarkRed  [9] Brown

#' MRP custom blue palette (for figures)
dark_blue_color  <- "#00205B"
light_blue_color <- "#6699CC"
third_color      <- "#A0C9E0"

#' Named color vectors for ggplot scales
mrp_time_colors <- c(
  "PRE"  = dark_blue_color,
  "MID"  = light_blue_color,
  "POST" = third_color
)

mrp_training_colors <- c(
  "trained"   = dark_blue_color,
  "untrained" = light_blue_color
)

#' Convenience ggplot scale functions
scale_fill_mrp_time <- function(...) {
  ggplot2::scale_fill_manual(values = mrp_time_colors, ...)
}

scale_fill_mrp_training <- function(...) {
  ggplot2::scale_fill_manual(values = mrp_training_colors, ...)
}

scale_color_mrp_training <- function(...) {
  ggplot2::scale_color_manual(values = mrp_training_colors, ...)
}


# ============================================================================
# Number formatters
# ============================================================================

fmt0 <- function(x) formatC(x, format = "f", digits = 0)
fmt1 <- function(x) formatC(x, format = "f", digits = 1)
fmt2 <- function(x) formatC(x, format = "f", digits = 2)
fmt3 <- function(x) formatC(x, format = "f", digits = 3)
fmt_pct <- function(x, total) sprintf("%s (%s%%)", fmt0(x), fmt0(100 * x / total))


# ============================================================================
# Table formatting (kableExtra)
# ============================================================================

#' Format p-value with cell_spec for kable tables.
#' Returns HTML string: blue + bold if significant, black otherwise.
#' Use in mutate() before passing to create_styled_kable(escape = FALSE).
fmt_p <- function(p, threshold = 0.05) {
  npg_colors <- get_npg_colors()
  sapply(p, function(val) {
    if (is.na(val)) return("")
    label <- if (val < 0.001) "< 0.001" else sprintf("%.3f", val)
    kableExtra::cell_spec(label, format = "html",
                          bold = val < threshold,
                          color = ifelse(val < threshold, npg_colors[4], "black"))
  })
}

#' Format p-value as plain text (for plots and inline prose).
#' No HTML, no color -- just the string.
fmt_p_text <- function(p) {
  sapply(p, function(val) {
    if (is.na(val)) return("NA")
    if (val < 0.001) return("< 0.001")
    if (val < 0.01) return(sprintf("%.3f", val))
    if (val < 0.05) return(sprintf("%.3f", val))
    return(sprintf("%.3f", val))
  })
}

#' Format Hedges' g with cell_spec for kable tables.
#' Returns HTML string: red + bold if |g| >= 0.5 (moderate+), black otherwise.
fmt_g <- function(g) {
  npg_colors <- get_npg_colors()
  sapply(g, function(val) {
    if (is.na(val)) return("")
    kableExtra::cell_spec(sprintf("%.2f", val), format = "html",
                          bold = abs(val) >= 0.5,
                          color = ifelse(abs(val) >= 0.5, npg_colors[8], "black"))
  })
}

#' Styled kable wrapper with project defaults.
#' Forwards all additional arguments to kable().
#' Usage: create_styled_kable(df, caption = "My table", escape = FALSE)
#' In results='asis' chunks: cat(as.character(create_styled_kable(...)))
create_styled_kable <- function(data, caption = NULL, ...) {
  dots <- list(...)
  if (is.null(dots$format)) dots$format <- "html"
  do.call(kableExtra::kable, c(list(data, caption = caption), dots)) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE
    )
}


# ============================================================================
# Effect size calculations
# ============================================================================

#' Hedges' g for paired/repeated-measures data.
#' Uses SD of differences (appropriate when same subjects at both timepoints).
#' This is the MRP's original implementation.
calculate_hedges_g <- function(group1, group2) {
  diffs <- group2 - group1
  n <- sum(!is.na(diffs))
  if (n < 2) return(NA)
  m_diff <- mean(diffs, na.rm = TRUE)
  sd_diff <- sd(diffs, na.rm = TRUE)
  if (is.na(sd_diff) || sd_diff == 0) return(NA)
  d <- m_diff / sd_diff
  correction <- 1 - (3 / (4 * n - 1))
  g <- correction * d
  return(if (is.finite(g)) g else NA)
}

#' Alias for clarity in new code
hedges_g_paired <- function(x, y) {
  complete_cases <- complete.cases(x, y)
  x <- x[complete_cases]; y <- y[complete_cases]
  calculate_hedges_g(x, y)
}

#' Hedges' g for independent groups (between-group comparisons).
hedges_g_independent <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(NA)
  n1 <- length(x); n2 <- length(y)
  s1 <- sd(x); s2 <- sd(y)
  if (is.na(s1) || is.na(s2)) return(NA)
  pooled_sd <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  if (is.na(pooled_sd) || pooled_sd == 0) return(NA)
  J <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
  g <- J * ((mean(x) - mean(y)) / pooled_sd)
  return(if (is.finite(g)) g else NA)
}

#' Interpret Hedges' g magnitude
interpret_hedges_g <- function(g) {
  abs_g <- abs(g)
  if (is.na(abs_g)) return(NA)
  else if (abs_g < 0.2) return("Trivial")
  else if (abs_g < 0.5) return("Small")
  else if (abs_g < 0.8) return("Moderate")
  else return("Large")
}


# ============================================================================
# Analysis helpers
# ============================================================================

#' Calculate percent change across three timepoints
calculate_percent_change <- function(pre, mid, post) {
  data.frame(
    PRE_to_MID  = ((mid - pre) / pre) * 100,
    MID_to_POST = ((post - mid) / mid) * 100,
    PRE_to_POST = ((post - pre) / pre) * 100
  )
}

#' Add training status column based on subject ID
add_training_status <- function(data,
                                trained = paste0("SUB ", c(8, 17, 18, 19)),
                                untrained = paste0("SUB ", c(2, 3, 10, 11, 14))) {
  data %>%
    dplyr::mutate(
      TrainingStatus = ifelse(Subject %in% trained, "trained", "untrained")
    )
}


# ============================================================================
# Measure-specific metadata
# ============================================================================

#' Decimal precision by measure (instrument-appropriate rounding).
#' Returns the number of decimal places for a given measure.
get_measure_digits <- function(measure, default = 2) {
  digits_dict <- list(
    # Strength -- kg, 1 decimal (plates are 2.5 lb increments -> ~1.1 kg)
    "1RM_OHP"        = 1, "1RM_Lat Pull"   = 1, "1RM_Seated Row" = 1,
    "1RM_Leg Press"  = 1, "1RM_Deadlift"   = 1,
    "10RM_OHP"       = 1, "10RM_Lat Pull"  = 1, "10RM_Seated Row"= 1,
    "10RM_Leg Press" = 1, "10RM_Deadlift"  = 1,
    # VO2 -- metabolic cart reports to 1-2 decimals
    "VO2Max"         = 1, "VO2Peak"        = 1,
    # Body composition
    "Body fat (%)"   = 1, "Weight (Kg)"    = 1,
    # Performance
    "Wmax"           = 0, "Vertical Jump"  = 1, "YYIR"           = 0,
    # Running economy (mL/kg/min at given speed)
    "6.5km*hr-1"     = 1, "12km*hr-1"      = 1, "16km*hr-1"      = 1
  )
  if (measure %in% names(digits_dict)) digits_dict[[measure]] else default
}

#' Format mean +/- SD with measure-appropriate decimal places.
fmt_mean_sd <- function(x, measure) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("\u2014")
  d <- get_measure_digits(measure)
  if (length(x) == 1) {
    return(sprintf(paste0("%.", d, "f \u00B1 \u2014"), x))
  }
  sprintf(paste0("%.", d, "f \u00B1 %.", d, "f"), mean(x), sd(x))
}

#' Publication-quality labels for ggplot and kable.
#' format = "expression" returns plotmath expression for axis labels.
#' format = "text" returns Unicode string for table headers.
get_formatted_label <- function(measure, format = "expression") {

  # Plotmath expressions for ggplot axis labels
  expression_dict <- list(
    # Strength measures
    "1RM_OHP"        = expression(paste("1RM Overhead Press (kg)")),
    "1RM_Lat Pull"   = expression(paste("1RM Lat Pulldown (kg)")),
    "1RM_Seated Row" = expression(paste("1RM Seated Row (kg)")),
    "1RM_Leg Press"  = expression(paste("1RM Leg Press (kg)")),
    "1RM_Deadlift"   = expression(paste("1RM Deadlift (kg)")),
    "10RM_OHP"       = expression(paste("10RM Overhead Press (kg)")),
    "10RM_Lat Pull"  = expression(paste("10RM Lat Pulldown (kg)")),
    "10RM_Seated Row"= expression(paste("10RM Seated Row (kg)")),
    "10RM_Leg Press" = expression(paste("10RM Leg Press (kg)")),
    "10RM_Deadlift"  = expression(paste("10RM Deadlift (kg)")),
    # Physiological measures
    "VO2Max"         = expression(paste(dot(V), O[2], "max (mL\u00B7", kg^-1, "\u00B7", min^-1, ")")),
    "VO2Peak"        = expression(paste(dot(V), O[2], "peak (mL\u00B7", kg^-1, "\u00B7", min^-1, ")")),
    "Body fat (%)"   = expression(paste("Body Fat (%)")),
    "Weight (Kg)"    = expression(paste("Body Mass (kg)")),
    "Wmax"           = expression(paste(W[max], " (W)")),
    "Vertical Jump"  = expression(paste("Vertical Jump (cm)")),
    "YYIR"           = expression(paste("YoYo IR1 (m)")),
    # Running economy
    "6.5km*hr-1"     = expression(paste("RE at 6.5 km\u00B7", h^-1, " (mL\u00B7", kg^-1, "\u00B7", min^-1, ")")),
    "12km*hr-1"      = expression(paste("RE at 12 km\u00B7", h^-1, " (mL\u00B7", kg^-1, "\u00B7", min^-1, ")")),
    "16km*hr-1"      = expression(paste("RE at 16 km\u00B7", h^-1, " (mL\u00B7", kg^-1, "\u00B7", min^-1, ")"))
  )

  # Unicode plain text for kable table headers
  text_dict <- list(
    "1RM_OHP"        = "1RM OHP (kg)",
    "1RM_Lat Pull"   = "1RM Lat Pull (kg)",
    "1RM_Seated Row" = "1RM Seated Row (kg)",
    "1RM_Leg Press"  = "1RM Leg Press (kg)",
    "1RM_Deadlift"   = "1RM Deadlift (kg)",
    "10RM_OHP"       = "10RM OHP (kg)",
    "10RM_Lat Pull"  = "10RM Lat Pull (kg)",
    "10RM_Seated Row"= "10RM Seated Row (kg)",
    "10RM_Leg Press" = "10RM Leg Press (kg)",
    "10RM_Deadlift"  = "10RM Deadlift (kg)",
    "VO2Max"         = "V\u0307O\u2082max (mL\u00B7kg\u207B\u00B9\u00B7min\u207B\u00B9)",
    "VO2Peak"        = "V\u0307O\u2082peak (mL\u00B7kg\u207B\u00B9\u00B7min\u207B\u00B9)",
    "Body fat (%)"   = "Body Fat (%)",
    "Weight (Kg)"    = "Body Mass (kg)",
    "Wmax"           = "W\u2098\u2090\u2093 (W)",
    "Vertical Jump"  = "Vertical Jump (cm)",
    "YYIR"           = "YoYo IR1 (m)",
    "6.5km*hr-1"     = "RE 6.5 km\u00B7h\u207B\u00B9 (mL\u00B7kg\u207B\u00B9\u00B7min\u207B\u00B9)",
    "12km*hr-1"      = "RE 12 km\u00B7h\u207B\u00B9 (mL\u00B7kg\u207B\u00B9\u00B7min\u207B\u00B9)",
    "16km*hr-1"      = "RE 16 km\u00B7h\u207B\u00B9 (mL\u00B7kg\u207B\u00B9\u00B7min\u207B\u00B9)"
  )

  dict <- if (format == "text") text_dict else expression_dict
  if (measure %in% names(dict)) return(dict[[measure]]) else return(measure)
}
