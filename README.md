# Maximal Returns Protocol (MRP)

Analysis of a time-efficient concurrent training intervention. Part of Dr. Jacob Bowie's dissertation work at UConn.

## Study design

- **Intervention**: 12 weeks, 1 hour/week combined training (HIIT + resistance + HIFT plyometrics)
- **Participants**: n = 9 (4 aerobically trained, 5 untrained), PRE / MID / POST design
- **Funding**: NASA Connecticut Space Grant Consortium (P-1708)

## Key findings

- Significant strength gains across all 12 strength measures (PRE->POST *p* < 0.001)
- Effect sizes ranged from moderate to very large (Hedges' *g* = 0.49-1.54)
- Lower-body exercises showed the largest adaptations (10RM Deadlift *g* = 1.54)
- VO2Max improved significantly in trained participants (*p* = 0.016, *g* = 0.68)
- Maximal power output (Wmax) increased progressively throughout the intervention
- Running economy and body composition were maintained

## Repo layout

```
MRP/
├── MRP/
│   ├── 01_MRP_Analysis.Rmd        # Main analysis (refactored, canonical)
│   ├── 07112025 MRP_Markdown.Rmd  # Historical baseline (preserved)
│   ├── helpers/
│   │   ├── paths.R                # Absolute-path anchoring (MRP.Rproj)
│   │   ├── setup.R                # Packages, namespace conflicts, NPG palette
│   │   └── functions.R            # fmt_p, fmt_g, create_styled_kable,
│   │                              # Hedges' g, MRP color scales, labels
│   ├── MRP.xlsx                   # Source data (Strength, vo2data, Session)
│   ├── MRP.Rproj                  # RStudio project anchor
│   └── renv.lock                  # Dependency lock file
├── DATA/                          # Raw subject-level data (not tracked in git)
├── .gitignore
└── README.md
```

## Rendering the analysis

```r
# From the MRP/MRP/ directory (or open MRP.Rproj in RStudio)
renv::restore()  # restore package versions
rmarkdown::render("01_MRP_Analysis.Rmd")
```

The rendered HTML (`01_MRP_Analysis.html`) is self-contained and opens in any browser. Parameters (`show_all_plots`, `save_pdfs`, `save_outputs`, `cache`) can be adjusted via the YAML header or passed to `render(params = ...)`.

## Design notes

- Tables use `cell_spec` conditional formatting: significant *p*-values in blue, moderate+ effect sizes in red.
- Figures use a consistent custom blue palette (`#00205B` / `#6699CC` / `#A0C9E0`) for PRE/MID/POST.
- Prose statistics are preserved as hand-written paragraphs alongside auto-generated versions (blue-bordered divs) for comparison. See the "Summary of Training Adaptations," "Trained vs. Untrained," and "Percent Change" sections.
- Caching is disabled project-wide (`cache: FALSE`) per repeated issues with stale cache masking real bugs.

## Status

Manuscript was submitted to the *Journal of Strength and Conditioning Research* (JSCR-S-25-01430) as a Research Note in September 2025; decision was reject (February 2026) with reviewer feedback to reframe as a feasibility/pilot study. Currently targeting an alternative journal for resubmission with the analyses in this repo as supplementary material.
