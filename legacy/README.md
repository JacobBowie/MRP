# Legacy / archive

Scripts and Rmds from earlier stages of the MRP analysis. **Preserved as historical record, not part of the canonical pipeline.** The canonical analysis lives in `../01_MRP_Analysis.Rmd` at the repo root.

## What's here and why

These files were the working analysis surface before the Phase 0–10 refactor (commits `80adf92` through `4cc0f0f`, Apr 2026). Each represents either a chronological snapshot of the Rmd or an exploratory side-script that informed the final canonical pipeline.

### Dated Rmd snapshots
| File | Notes |
|---|---|
| `03142025 MRP_Markdown.Rmd` | First dated snapshot — earliest end-to-end pass. |
| `06252025 MRP_Markdown.Rmd` | Mid-stage rewrite. |
| `07112025 MRP_Markdown.Rmd` | Stable baseline pre-refactor; useful for diffing what changed in Phases 0–10. |
| `MRP_Markdown.Rmd` | Unstamped working copy; superseded. |
| `MRP_Analysis.Rmd` | Pre-`01_` naming convention; superseded by `01_MRP_Analysis.Rmd`. |

### Exploratory R scripts
| File | What it did |
|---|---|
| `script.R` | Bulk exploratory script — strength, VO₂, plots; cannibalized into `01_MRP_Analysis.Rmd`. |
| `Session_script.R` | Session-level analyses (HIIT / HIFT / RT). |
| `session_RT_TvsU.R` | Trained vs untrained session-level RT contrasts. |
| `TvU Script.R` | Trained vs untrained outcome contrasts. |
| `chatGPT_script.R` | Scratchpad from an LLM-assisted exploration. |
| `Exercise_time_figure.R` | One-off figure for exercise time. |
| `Exercise_time_plot.R` | Sibling of the above; different layout. |
| `MRP_sensitivity_power.R` | Post-hoc sensitivity / power analysis used to anchor the revision plan's framing. |

## Reproducibility caveats

These scripts contain hard-coded local paths (`C:/MRP`, `G:/My Drive/...`) from the original Windows + Google Drive workflow. They are **not expected to run on a clean clone**. They also predate the `helpers/` namespace + `renv` lockfile pinning, so package versions are not guaranteed.

If you need a working pipeline, use `../01_MRP_Analysis.Rmd` with the project-root `renv.lock`.
