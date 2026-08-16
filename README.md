# Development of MAS

The Music Addiction Scale, from the 2021 master's work at Durham to the 2026
re-analysis, kept as a dated record rather than a tidied result.

The scale was developed for the thesis *Music and Addiction: The development of
Music Addiction Scale (MAS)* (Durham University, 2022,
[e-theses 14555](https://etheses.durham.ac.uk/id/eprint/14555/)). Twenty-eight
items adapted from the behavioural addiction template were trimmed to twenty on
249 respondents, splitting into an addictive core and an engagement core.

This repository exists because the thesis reported a result and not a process.
The factor structure rests on a single exploratory factor analysis with one
rotation and one missing-data policy, and none of those choices were tested.
The 2026 folder is that test.

## How to read it

The folders are years, in the order the work happened.

| Folder | When | What |
|---|---|---|
| `2021-thesis/` | Feb – Nov 2021 | The pipeline as it was run for the thesis. Kept verbatim, including its faults. |
| `2022-corrections/` | Aug 2022 | Post-submission minor corrections. Supervisor-authored analyses. |
| `2023-2025-hiatus/` | — | No work. The note says why the gap is here. |
| `2026-reanalysis/` | Jul – Aug 2026 | Re-examination of the item structure. The runnable part of the repository. |

`2021-thesis/` is an archive. It is not maintained, it is not corrected, and
two of its scripts do not run. `2021-thesis/NOTES.md` lists the faults found
when the code was re-read in 2026, with line numbers. Leaving them visible is
the point: the 2026 work is only meaningful against an honest record of what it
started from.

## Running it

Requires R with `psych`, `dplyr`, `tidyr`, `ggplot2` and, for some options,
`GPArotation` and `randomForest`. Developed against R 4.6.1.

Open `development-of-mas.Rproj`, or set the working directory to the repository
root yourself. Either way:

```r
# from the repository root
source("tools/make_synthetic_data.R")   # writes data/synthetic/mas_synthetic.tsv
source("2026-reanalysis/RUN_ME.R")      # munge, EFA, composites, fit log
```

Output lands in `2026-reanalysis/out/`.

No participant data are distributed here, so the default run uses a synthetic
file with the same columns and response options and no substantive content.
See `data/README.md`. To run against a real export, set `MAS_DATA`.

Two sets of scripts, `2026-reanalysis/trials/` and
`2026-reanalysis/bimodality/`, need the saved workspace from the 2021 run and
will stop with an explanation if `MAS_WORKSPACE` is unset. They are included as
record, not as an entry point.

## Authorship

Part of the 2021 code is not mine. The pipeline is built on
[Tuomas Eerola's R_template](https://github.com/tuomaseerola/R_template/), and
he wrote the first factor analysis script and the 2022 additions to the
descriptive statistics directly. `AUTHORSHIP.md` gives the file and line ranges.

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

This repository was created on 16 August 2026 by importing files that had never
been under version control. The commits are grouped by year and their dates are
taken from the files' own modification times, so the timeline is a
reconstruction from filesystem evidence rather than a record kept as the work
happened. Everything from before that date has one author date and one
committer date because both were set from the same source.

Two mechanical redactions were applied throughout, including to the historical
commits. Absolute paths from the machine the work was done on were shortened,
and one folder was renamed because it carried a person's nickname. Both appear
in comments and in console logs, never in anything the analysis depends on.
Working notes that named a colleague were left out rather than edited.
