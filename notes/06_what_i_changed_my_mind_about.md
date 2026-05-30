# What this note is

Paper 1, in its original form, made a specific empirical claim: that Philadelphia's 2023 reassessment was associated with a roughly 13 percent reduction in within-tract assessment uncertainty, identified by an interrupted time series fit at the tract-quarter level. The claim was the paper's headline finding and survived the December 2025 grading process intact.

The corrected analysis in this repository reaches a more careful version of that claim, after a methodological journey that took most of the spring of 2026 to work through. This note documents that journey: what the original specification was, what was wrong with it, what the corrected specification does, what it finds at three reassessment events rather than one, why the standard errors required coefficient-specific treatment, and what the corrected analysis is and is not entitled to claim. This is the longest note in the repository and also the one I am most uncertain about, in the specific sense that the methodological choices it documents are choices I made under pressure of trying to figure out what could be honestly said with the data I have.

## What the original specification was

The original Paper 1 ran a hedonic regression of log sale price on structural characteristics, condition codes, and time effects, then extracted residuals and computed dispersion measures at the tract-quarter level. The outcome variable was a measure of "uncertainty" that the paper described as the dispersion of unexplained sale prices within a tract and quarter. The analysis then fit an interrupted time series with a single break point at 2023Q1, the effective date of the TY2023 reassessment, and reported the level shift at the break as evidence that the reassessment had reduced within-tract uncertainty.

The 13 percent figure came from this level shift expressed as a fraction of the pre-2023 mean.
Three things about the original specification were defensible. The choice of tract-quarter as the unit of analysis was appropriate to the question. The choice of hedonic-residual dispersion as the outcome was appropriate to the question. The use of an interrupted time series to detect a level shift at a known institutional event was a reasonable identification strategy.

Three things about the original specification, in retrospect, were not defensible. First, the hedonic predictors included one of the data-quality flags whose construction we now know to have been circular (note 03). Second, the analysis tested only the 2023 break, even though the 2014 to 2025 panel contained two other major reassessments (TY2019 and TY2025) that the same identification strategy could have tested. Third, the standard errors were tract-clustered without consideration of cross-tract spatial dependence, even though the cross-sectional analysis at the GMA level (note 04) had documented substantial spatial autocorrelation that the tract-quarter analysis ought also to have acknowledged.
The corrected analysis fixes all three.

## What the corrected specification does

The corrected hedonic, implemented in analysis/06, regresses log sale price on log livable area, bedrooms, bathrooms, stories, age, age squared, and full sets of dummies for exterior condition, interior condition, and quality grade, with tract and year-quarter fixed effects. The data-period dummy that flexes the intercept across the 2014–2019 versus 2020–2025 OPA snapshot regimes is automatically dropped by the estimator because it is collinear with the year-quarter fixed effects. The hedonic fits 174,470 transactions across 308 tracts and 48 quarters, with an adjusted R-squared of 0.707 and a within-R-squared (after absorbing fixed effects) of 0.222.

Residuals from the hedonic are aggregated at the tract-quarter level. For each tract-quarter cell with at least five transactions, I compute the interquartile range of the log-price residuals. The minimum-n filter produces 11,205 tract-quarter cells with a mean residual IQR of 0.495. This is the outcome series for the interrupted time series.
The ITS itself includes three break points rather than one: 2019Q1, 2023Q1, and 2025Q1. Each break contributes a level shift and a slope change to the model. The full specification is:

> iqr_res = α + β₀·time + β₁·post_2019 + β₂·time_since_2019 + β₃·post_2023 + β₄·time_since_2023 + β₅·post_2025 + β₆·time_since_2025  + tract_FE + ε

This is a segmented regression with three predetermined break points. The model can capture both a discontinuous change at each event (the level shift) and a change in trajectory rate following each event (the slope change). Standard errors are two-way clustered at the tract and year-quarter level for most coefficients and tract-clustered for the 2025 coefficients, for reasons documented below.

## Why three breaks rather than one

The original paper tested only the 2023 break, presenting the result as informative about the TY2023 reassessment specifically. The corrected analysis fits all three breaks simultaneously, which has two advantages.

First, it disciplines the inference. If the model finds a level shift at 2023 of similar magnitude to level shifts it also finds at 2019 and 2025, the 2023 finding can no longer be attributed specifically to features of the 2023 reassessment — it is part of a recurring pattern. Conversely, if the 2023 shift is distinctively larger than the others, that distinctiveness is itself informative.
Second, it lets the data speak about the institutional sequence. The three reassessments occurred under different institutional regimes. TY2019 was the second full reassessment under the Actual Value Initiative, fit pre-CAMA. TY2023 was the first reassessment after a multi-year pandemic-era pause, fit with the new CAMA system and with the newly created sales validation unit. TY2025 was fit during a period of greater institutional maturity, with an external audit commissioned alongside the internal modeling. The three events are not comparable in a way that allows clean causal claims, but they are part of a sequence we can describe.

## What the corrected analysis finds at each break
The corrected analysis finds statistically significant level shifts at all three reassessment events. The level shift at 2019Q1 is -0.062 (about 12.5 percent of the pre-event mean), at 2023Q1 is -0.034 (about 7 percent), and at 2025Q1 is -0.045 (about 9 percent). All three are significant under appropriate standard-error methods.

The slope changes are more variable. The 2019 slope change is +0.005 per quarter (p = 0.012), indicating that the rate of decline in residual IQR slowed after the 2019 reassessment. The 2023 slope change is +0.003 per quarter (p = 0.036), a smaller version of the same pattern. The 2025 slope change is statistically unidentifiable (p = 0.68), which is expected: we have only four post-2025 quarters in the panel, which is not enough data to detect a trend change.
The pre-event time trend is -0.006 per quarter and highly significant. The series was already declining before any of the reassessments. The level shifts at each event are step changes on top of an underlying downward drift.

The original paper's 13 percent reduction at 2023 becomes a 7 percent reduction in the corrected analysis. The reduction is real and statistically significant, but it is smaller than the original specification reported and, critically, it is not unique to 2023. The pattern of level shifts at reassessment events is recurring across the panel.

## Why standard errors required coefficient-specific treatment

One of the more interesting methodological episodes of the corrected analysis concerned standard errors. The original specification used tract-clustered standard errors, the standard panel-data choice. After fitting the corrected ITS, I decided that two-way clustering by tract and year-quarter was more appropriate, because the cross-sectional analysis at the GMA level (note 04) had documented substantial spatial autocorrelation that single-dimension tract clustering does not absorb.

Two-way clustering produced sensible results for five of the seven coefficients (the time trend and the 2019 and 2023 level shifts and slope changes). For these coefficients, two-way clustered standard errors are modestly larger than tract-clustered standard errors, which is the expected behavior: adding a clustering dimension allows for more correlation in the residuals and properly widens the standard errors.

For the 2025 coefficients, two-way clustering produced standard errors that were smaller than tract-clustered standard errors, and the estimator returned a warning that the variance-covariance matrix was not positive semi-definite and had been corrected. The 2025 level-shift t-statistic under two-way clustering came out at -10.5, an implausibly large value that would correspond to a p-value on the order of 10⁻¹⁴.

I worked through the algebra. The Cameron-Gelbach-Miller two-way cluster variance estimator computes V_two_way = V_tract + V_quarter - V_intersection, with the subtraction correcting for the double-counting at the tract-quarter intersection cells. Under normal conditions the intersection variance is the smallest of the three terms and the subtraction produces a sensible result. Under conditions where one cluster dimension is degenerate for a particular coefficient — meaning the regressor has no within-cluster variation along that dimension — the subtraction can produce a variance smaller than either of the two single-dimension variances. The post-2025 dummy is constant within every year-quarter cluster (it equals 0 for every observation in any pre-2025 quarter and 1 for every observation in any post-2025 quarter), and we have only four post-2025 quarters in the data. The quarter dimension is degenerate for the 2025 coefficients in the precise sense the algebra requires for the pathology to occur.

I confirmed the diagnosis by computing standard errors four ways for each coefficient: HC1 (no clustering), tract-only, quarter-only, and two-way. For five of the seven coefficients, the standard errors ascend monotonically from HC1 through the more permissive methods, which is the expected pattern. For the two 2025 coefficients, they descend — quarter-only is smaller than tract-only, and two-way is smaller still. This confirms that the quarter-clustering variance contribution is structurally too small for these specific coefficients, and that the two-way subtraction is overcorrecting as a result.

The principled response is to report two-way clustered standard errors for the coefficients where the method behaves well, and tract-clustered standard errors for the 2025 coefficients where the method does not. The choice is driven by data structure, not by which method gives the more convenient p-values. With tract-clustered standard errors, the 2025 level shift has t = -3.24 and p = 0.001 — still highly significant, but appropriately precise rather than spuriously so.

This is not a problem unique to my data. It is a known pathology of the Cameron-Gelbach-Miller estimator when the number of clusters in one dimension is small relative to the post-treatment observations the estimator is identifying off of. The textbook-rigorous response would be a wild cluster bootstrap, which does not depend on the asymptotic approximation that fails here. I did not implement the bootstrap because the diagnostic-and-fallback approach is more interpretable in a portfolio context and the substantive conclusion does not depend on the choice between them. A future version of this analysis would use the bootstrap.

## What this analysis can and cannot identify

The corrected analysis documents an empirical pattern: at three institutional events in the 2014–2025 period, within-tract hedonic-residual dispersion shifted downward in measurable amounts. The pattern is robust across the three events, statistically significant, and not specific to the 2023 reassessment as the original paper had implied.

The analysis cannot identify the causal effect of any reassessment on assessment quality. There are two structural reasons for this and a third that compounds them.

First, the hedonic-residual outcome measures market pricing heterogeneity conditional on observed characteristics. Reassessments change assessed values; they do not change sale prices or property characteristics. There is no mechanism by which a reassessment would directly affect our outcome. The level shifts the analysis detects are shifts in what the market was doing around the timing of the reassessment, not shifts the reassessment itself produced.

Second, each reassessment was bundled with other administrative changes. The 2023 event in particular was accompanied by the implementation of the CAMA system, the creation of the sales validation unit, an external data-collection contract, and several methodology revisions. The data does not allow us to separate the effect of "the reassessment" from the effects of the package of institutional changes that surrounded it.

Third, the data itself was produced by the institution undergoing these changes. The OPA records I aggregate to compute residual IQR reflect the institution's record-keeping practices at the moment of data extraction. As OPA reprocesses validation outcomes, refreshes characteristic records, and adjusts classification, our historical observations of past sales can change retrospectively. The institutional environment is not a neutral backdrop to the analysis; it is part of the analytical fabric, and the autocorrelation in our outcome series likely includes a component arising from administrative reprocessing that no standard time-series correction handles.

These limitations do not make the analysis worthless. They make it the kind of analysis that is most honestly described as documenting a recurring pattern at three institutional events, with the recognition that the pattern's causal interpretation is structurally underdetermined by the data and the framing.

## What this whole exercise was actually about

The deeper realization, which I want to be honest about for parallel reasons to the previous notes, is that the original Paper 1 had been written in a register that the data does not quite support.

The register was something like: here is a finding about the effect of a policy event, identified by an interrupted time series, with the standard inferential machinery that backs such claims in policy research. The register positioned the paper within an established empirical genre: the policy-event ITS, of which there are many examples in the health, education, and tax-policy literatures.

The corrected analysis sits in a different register. It documents what happened at three policy events without claiming to identify the effect of any of them. It acknowledges that the outcome variable is several steps removed from anything the policy events directly act on. It surfaces the institutional environment in which the data is produced as part of the substantive object of study, not as a backdrop. It treats the residual spatial structure that none of the model specifications fully absorbs as a substantive feature of how assessment outcomes are distributed in Philadelphia, not as a nuisance for the inference.

This register is harder to summarize in a one-line abstract. It does not produce a "the reassessment caused" headline. The cost is real: the corrected version of the paper would not be as publishable in the same venue as the original, because the original genre rewards clean causal claims and the corrected version refuses to make them. The benefit is also real: the corrected version is something I can defend.

The notes in this repository, taken together, document a movement from one register to the other. Note 01 framed the project. Note 02 documented the choices required to turn raw administrative records into an analytical sample. Note 03 traced a circular flag through the original Paper 1 and showed what the corrected falsification logit actually finds. Note 04 worked through the Paper 2 framework and what survives when spatial structure is acknowledged. Note 05 surfaced the metaphysical decision hiding in the choice of dispersion measure. This note has been about the corrected Paper 1 itself, methodologically the most fraught of the analyses and the one whose corrections cost the most in terms of how confidently the result can be stated.
What runs through all six notes is the same shape. There was a question. The data appeared to give a clean answer. Working through the methodology made the answer less clean and more honest. The institutional environment producing the data, which the original analyses had treated as a backdrop, turned out to be part of what the analyses needed to be about. The corrected analyses are smaller in their substantive ambition than the originals and more careful about what the data can support.

If the project has a thesis, that is the thesis: a year of careful work on this dataset has been a year of slowly making my claims smaller. I think that is what serious empirical work tends to produce. I have tried to document the process honestly enough that someone reading this in 2027 or 2030 can audit not only the analyses but the choices behind them.

