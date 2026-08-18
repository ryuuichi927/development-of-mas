# Output of the August 2026 run

These files come from running the re-analysis on the real 2021 data. They are
committed as a record of what the analysis produced, which is why the folder is
not empty in a repository that ships no data. First run 1 August 2026,
regenerated 18 August 2026 with `MSI.MT` corrected; see `../scr/fix_msi_mt.R`.
The factor results did not move, and `efa_trimmed_loadings.csv` and
`efa_fit_log.csv` are byte for byte what the August run produced.

Regenerate with `MAS_DATA=... Rscript 2026-reanalysis/make_out.R` from the
repository root. Three scripts write here and two of them write the canonical
run's file names under different options, so the order matters and that script
fixes it. Running them by hand in another order leaves the main four files
describing a sensitivity pass rather than the canonical run.

Nothing here is participant-level. `efa_trimmed_loadings.csv` is keyed by item,
`cor_mas_factors_external.csv` is an eight by eight correlation matrix, the
`OLD_vs_NEW_*` and `compare_*` files hold per-measure summary rows, and
`efa_fit_log.csv` is the run's parameters and fit indices.

Running the pipeline on the synthetic file writes to `out/synthetic/` instead,
which is not committed. A demonstration must not be able to overwrite a result.

| File | What |
|---|---|
| `efa_fit_log.csv` | sample sizes, KMO, TLI, RMSEA, and the options the run used |
| `efa_trimmed_loadings.csv` | loadings for the twenty retained items |
| `cor_mas_factors_external.csv` | factors and cores against HUMS and Gold-MSI |
| `corrplot_mas_clean.pdf` | the same matrix as a figure |
| `OLD_vs_NEW_fit.csv` | 2021 pipeline against the 2026 one, fit indices |
| `OLD_vs_NEW_loading_cor.csv` | how far the loadings moved between them |
| `OLD_vs_NEW_comparison.csv`, `OLD_efa_trim_print.txt` | the full comparison |
| `compare_legacy_vs_clean.csv` | 2021 composite formulas against consistent ones |
| `compare_listwise_vs_roughfix.csv` | complete cases against median imputation |
| `run_log_2026-08-18.txt` | console transcript of the run |
| `sessionInfo.txt` | R version and package versions |
