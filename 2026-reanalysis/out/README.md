# Output of the August 2026 run

These files come from running the re-analysis on the real 2021 data on
1 August 2026. They are committed as a record of what the analysis produced,
which is why the folder is not empty in a repository that ships no data.

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
| `OLD_vs_NEW_comparison.csv`, `OLD_vs_NEW_run_log.txt`, `OLD_efa_trim_print.txt` | the full comparison |
| `compare_legacy_vs_clean.csv` | 2021 composite formulas against consistent ones |
| `compare_listwise_vs_roughfix.csv` | complete cases against median imputation |
| `run_log_2026-08-01.txt` | console transcript of the run |
| `sessionInfo.txt` | R version and package versions |
