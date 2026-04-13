# paths.R -- Single source of truth for MRP project paths.
#
# Sourced by setup.R. No package dependencies, no data loading, no side
# effects beyond defining path constants and creating today's output directory.
#
# Defines:
#   PROJECT_ROOT   absolute path to MRP/ (the directory containing MRP.Rproj)
#   DATA_DIR       <PROJECT_ROOT>/MRP  (where MRP.xlsx lives)
#   HELPERS_DIR    <PROJECT_ROOT>/MRP/helpers
#   output_dir     <PROJECT_ROOT>/MRP/<MMDDYYYY>_output  (created if missing)
#   file_prefix    paste0(output_dir, "/")
#   proj_path(...) helper: file.path(PROJECT_ROOT, ...)
#
# Project root is anchored on MRP.Rproj -- a file committed to git that
# exists in any clone. We do NOT anchor on generated directories.
#
# Why absolute paths: knitr restores the chunk WD around every chunk.
# Relative paths break silently. Absolute paths are invariant.

.find_project_root <- function() {
  candidate <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(10)) {
    if (file.exists(file.path(candidate, "MRP.Rproj"))) return(candidate)
    parent <- dirname(candidate)
    if (parent == candidate) break
    candidate <- parent
  }
  stop("paths.R: could not locate MRP.Rproj walking up from ", getwd(),
       ". Run scripts from inside the project tree.")
}

PROJECT_ROOT <- .find_project_root()
DATA_DIR     <- PROJECT_ROOT
HELPERS_DIR  <- file.path(PROJECT_ROOT, "helpers")

# Date-stamped output directory, absolute, idempotent.
output_dir <- file.path(PROJECT_ROOT,
                        paste0(format(Sys.Date(), "%m%d%Y"), "_output"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Trailing slash for easy paste0(file_prefix, "filename.pdf") usage.
file_prefix <- paste0(output_dir, "/")

# Convenience helper for callers that prefer functional access.
proj_path <- function(...) file.path(PROJECT_ROOT, ...)
