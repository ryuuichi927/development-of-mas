# Development of MAS

Can music use be measured the way behavioural addictions are measured, and if it
can, what does the resulting scale actually separate? The Music Addiction Scale
takes twenty-eight items adapted from the behavioural addiction template and asks
whether they hold together, on 249 respondents, trimmed to twenty items in an
addictive core and an engagement core.

The scale was developed for the thesis below, and this repository holds the
analysis code from that work and its re-examination five years later.

> Hamakawa, R. (2022). *Music and Addiction The development of Music Addiction
> Scale (MAS)*. Master of Arts (Research), Durham University.
> [e-theses 14555](https://etheses.durham.ac.uk/id/eprint/14555/)

The thesis reported a result rather than a process. Its factor structure rests on
a single exploratory factor analysis, with one rotation and one missing-data
policy, and none of those choices were examined. The 2026 folder examines them.
Under an oblique rotation the two cores correlate at 0.34, where the 2021
analysis had assumed them uncorrelated.

## What is here

| Folder | When | What |
|---|---|---|
| `2021-thesis/` | Feb – Nov 2021 | The pipeline as it was run for the thesis, kept verbatim. |
| `2022-corrections/` | Aug 2022 | Post-submission corrections: severity grouping over the composites, and one-way ANOVAs against HUMS and Gold-MSI. |
| `2026-reanalysis/` | Jul – Aug 2026 | The re-examination, and the runnable part of the repository. |
| `data/` | — | The data policy and a synthetic stand-in. |
| `tools/` | — | The synthetic data generator. |

`2021-thesis/` is an archive. It is not maintained and not corrected, and
`contents.R` does not run to completion.

## Running it

Requires R with `psych`, `dplyr`, `tidyr` and `ggplot2`, and for some options
`GPArotation` and `randomForest`. Developed against R 4.6.1.

Open `development-of-mas.Rproj`, or set the working directory to the repository
root yourself. Either way:

```r
source("tools/make_synthetic_data.R")   # writes data/synthetic/mas_synthetic.tsv
source("2026-reanalysis/RUN_ME.R")      # munge, EFA, composites, fit log
```

No participant data are distributed here, so the default run uses a synthetic
file with the same columns and response options and none of the real structure.
See `data/README.md`. To run against a real export, set `MAS_DATA`.

`2026-reanalysis/out/` holds the committed record of the run on the real data,
and `2026-reanalysis/out/README.md` describes each file and how to regenerate
them. A synthetic run writes to `out/synthetic/` instead, so it cannot overwrite
that record.

`2026-reanalysis/trials/` and `2026-reanalysis/bimodality/` need the saved
workspace from the 2021 run and stop with an explanation if `MAS_WORKSPACE` is
unset. They are included as record, not as an entry point.
`2026-reanalysis/bimodality/README.md` reports what that side question found.

## Attribution

The 2021 pipeline is built on
[R_template](https://github.com/tuomaseerola/R_template/), which supplies the
`contents.R` entry point and the split into `munge/` and `scr/`. Files derived
from it carry its header on line 2. `LICENSE` sets out what this repository's
licence does and does not cover, and `CITATION.cff` holds the citation.

## Future work

The 2026 folder re-examines the exploratory structure; it does not confirm it.
Confirmation is the next step, and it needs a new sample. The twenty items were
selected on these 249 respondents, so testing them again on the same data would
recover the selection rather than test it. A confirmatory factor analysis on
roughly 300 to 350 new responses would establish whether the two-factor solution
holds outside the sample it came from.

Two questions raised here would follow from that sample. Whether the addictive
and engagement poles behave differently against external criteria, rather than
only loading differently within the scale. And whether the withdrawal facet
belongs where the instrument puts it, since items 21, 23 and 24 sit on the
engagement side in both the 2021 solution and the 2026 one.

Further out, the scale measures a disposition and says nothing about episodes.
Whether a high score corresponds to what actually happens during a given hour of
listening is a question for repeated momentary measurement, not for a
questionnaire. That is the direction of the doctoral proposal rather than of this
repository.

## History

The repository was created on 16 August 2026 from files that had never been under
version control. Commits are grouped by year and dated from the files' own
modification times, so the timeline is reconstructed from filesystem evidence
rather than kept as the work happened. Everything from before that date carries
the same author and committer date, because both were set from the same source.

Absolute paths from the machine the work was done on were shortened throughout,
including in the historical commits. They appear in comments and console logs,
never in anything the analysis depends on.
