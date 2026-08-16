# Profile shape

28 July 2026. A side question, asked once and closed.

The scale reports two cores, and the 2021 analysis found them by looking at how
items group. This asks the same thing about people instead: given the seven
facet scores as a profile, is there a direction along which respondents fall
into two groups, and does that direction resemble the addictive-minus-engagement
contrast the instrument assumes?

Method: fit Gaussian mixtures to the seven-dimensional profiles, then repeat in
the subspace orthogonal to the total score, so that the answer cannot simply be
overall severity. Test the resulting projection for bimodality with the dip
test. N = 241 complete profiles of 249.

## What it found

The raw mixture recovers severity. Its axis correlates 0.977 with the total
score and 0.988 with the first principal component, so left to itself the method
separates people by how high they score, which is not the question.

Orthogonal to the total, the axis becomes interpretable: it correlates 0.83 with
the old addictive-minus-engagement contrast. The weights load positively on
conflict, relapse and problems and negatively on salience, mood modification and
tolerance, which is the theoretical split. Withdrawal sits on the engagement
side, negative at -0.357, the same anomaly the factor analyses show.

The dip test does not support bimodality on any projection tried, including a
plane chosen specifically to maximise separation. So the contrast is real and
recoverable without labels, but respondents form a continuum rather than two
kinds of people. A scale can report two poles; it should not report two types.

## Entry points

| Path | What |
|---|---|
| `scripts/run_7d_bimodality.R` | the trial |
| `scripts/compare_20_vs_28.R` | the same question on the trimmed twenty items |
| `results/console_summary.txt` | weights, cosines, correlations and the dip results |
| `results/meta.json` | the same numbers, machine-readable |
| `figures/` | three figures |

Both scripts read the saved workspace from the 2021 run rather than rebuilding
it, so they need `MAS_WORKSPACE` and will stop with an explanation without it.
See `../../data/README.md`.

## Not distributed

The per-person score files, because they are participant-level.

Most of the figures and two index notes, because their labels and headings are
in the mixture of Chinese and shorthand this was worked in, and a public
repository should not carry text that its reader cannot check. The three kept
figures are the ones that stand on their own: the density panel, the axis
weights, and the scatter of the recovered axis against the old contrast. The
numbers behind all of them are in `results/`.
