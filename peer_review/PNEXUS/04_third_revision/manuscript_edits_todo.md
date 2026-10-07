# Manuscript edits to match the third-revision response to reviewers

Action list of changes the response promises but that are **not yet reflected**
in the manuscript / analysis code. Each item notes the file to edit and the
response passage it must satisfy. Items already satisfied are listed at the
bottom for reference so we don't redo them.

Status legend: `[ ]` to do · `[x]` done · `[~]` partially done

---

## Reviewer #3 — adjusted proportion (female-author ratio)

- [ ] **Methods: state the exclusion rule for the female-author ratio.**
  `manuscript/sections/20-methods.Rmd` (Women Authors Cited).
  The active formula is already the unadjusted share $f/(f+m)$, but the text
  does not say what happens when a reading list has no gender-identified
  authors. Add one sentence: these courses are **undefined and dropped**
  (listwise), as for the other reading-based outcomes.
  *Response claim:* "we now compute the empirical share ... and exclude only
  the courses for which it is genuinely undefined."

- [x] Remove the Agresti–Caffo / Beta-prior justification from Methods.
  (Already commented out in `20-methods.Rmd`; verify it stays removed in the
  rendered PDF.)

- [ ] **Align the robustness-check text to the unadjusted ratio.**
  `manuscript/sections/50-supporting.Rmd` (Robustness) + confirm the
  robustness figures are regenerated from the edited
  `scripts/05_plot_robustness_checks.R` (now using the raw share). Make sure no
  SI sentence still references the "+1/+2" adjustment or Agresti.

- [ ] **Regenerate affected figures/tables** so the main-text and SI
  female-author numbers come from the unadjusted definition everywhere
  (`make heterogeneity`, robustness checks). Confirm the forest-plot /
  temporal-stability values are unchanged in substance (sign, ordering,
  significance), as the response asserts.

## Reviewer #3 — course-level "unknown"

- [ ] **Drop unknown-level courses in the models that condition on course level.**
  `scripts/03_fit_models.R` currently includes `course_level` as a factor with
  the `unknown`/`NA` category. Filter to basic/advanced/graduate (drop unknown
  and un-parseable) before fitting, then re-run and regenerate the main figure.
  *Response claim:* "in the revised analysis we **drop** courses lacking a
  determinate level rather than carrying them as a separate category."

- [ ] **Report the course-level coverage and the drop in the manuscript.**
  Add to SI (Course Level) and/or Results: among estimation-sample courses,
  ~0.2% have an explicit `unknown` code, ~7% have no parseable code, and ~93%
  are classified (basic 49% / advanced 28% / graduate 16%). State that the
  "20%" figure does not correspond to the analysed sample.

- [ ] **Add a robustness sentence** that dropping undefined-level courses does
  not change the substantive results (document composition in SI).

## Reviewer #4 — age / seniority & mechanisms

- [x] Results subsection "Age and seniority composition" with the within-level
  (Task 1) and temporal (Task 2) analyses and SI figures `fig:age-level`,
  `fig:age-trend`. (Already added to `30-results.Rmd`.)

- [ ] **Discussion: add an explicit age/seniority paragraph.**
  `manuscript/sections/40-discussion.Rmd` currently only mentions "cohort
  effects" in passing. Add a paragraph that (a) states the age mechanism runs
  through course level, (b) summarises that the gaps survive within level and
  do not track the female share over time, and (c) cross-references the new
  Results subsection. *Response claim:* "we have revised the Discussion so the
  conclusions are stated as associational and are explicit about this residual
  uncertainty."

- [ ] **Discussion/Results: reflect the homophily field-balance finding.**
  State that the mixed-team shortfall is driven by a field's gender *balance*
  (r ≈ 0.47), not its female-richness (≈0 once balance is controlled),
  interpreted as a symmetric same-gender preference. Currently homophily is
  mentioned but the balance-vs-richness result from the field-level analysis is
  not in the paper. (Source: `notebooks/field_drivers_mixed_teams.Rmd`;
  consider promoting it into the Monte Carlo results or SI.)

- [~] **Scope framing as associational.** Discussion/Limitations already say
  "results should be interpreted as correlations." Verify the Abstract and
  Significance Statement are consistent (soften any causal-sounding language).
  `manuscript/main.Rmd` (abstract) and `manuscript/sections/00-significance.Rmd`.

## Reviewer #4 — additional comments

- [ ] **SI: explain the 2005–2008 mixed-team fluctuation with underlying counts.**
  Add the decomposition (stable #institutions, steadily rising #syllabi, all 69
  fields present each year; mixed-team growth plateaus 2005–2008 against a
  rising denominator). Currently this only exists as a comment in a legacy
  script. Consider a small SI figure of the per-configuration counts over time.
  *Response claim:* "We now note this explicitly in the SI and show the
  underlying counts."

- [x] Interdisciplinarity = multidisciplinary orientation, with the
  `@van2025circling` citation. (Already in `20-methods.Rmd`.)
  - [ ] Minor: verify the surrounding Results prose for interdisciplinarity uses
    the tempered wording (orientation/breadth), not "integrative"
    interdisciplinarity.

- [x] Novelty / Uzzi measure caveats and recency-vs-novelty orthogonality.
  (Already in `20-methods.Rmd`, Recency/Conventionality/Atypicality.)
  - [ ] Minor: add one Discussion sentence flagging novelty as the outcome least
    consistent with a simple textbook/age story (as conceded in the response).

- [x] Gender vs sex clarification sentence + footnote.
  (Already in `20-methods.Rmd`, Data.)

## Cross-cutting / submission hygiene

- [ ] **Re-render the manuscript** after the above edits and confirm all figure
  cross-references resolve and the page count stays within the 12-page limit.
- [ ] **Marked (diff) copy** regenerated so the editor can see the changes
  (`manuscript/Makefile` diff target).
- [ ] **Figure alt-text** for each figure (PNAS Nexus accessibility requirement).
- [ ] **Data availability statement** confirmed to cover the new analyses
  (age/seniority, field-balance) and the released code
  (`scripts/09_fit_heterogeneity.R`, `scripts/10_plot_heterogeneity.R`).
- [ ] Keep the response bib key consistent: response uses
  `@vandenbesselaar2025circling` (self-contained `refs.bib`), manuscript uses
  `@van2025circling`. Fine as-is, but note if the two are ever merged.

---

## Already satisfied (verified in current manuscript) — no action

- Methods female-ratio formula is the unadjusted share; Agresti passage commented out.
- Interdisciplinarity "multidisciplinary orientation" clarification + van den Besselaar cite.
- Novelty/atypicality definition, limitations footnote, recency-vs-novelty orthogonality.
- Gender-vs-sex sentence and footnote.
- "Age and seniority composition" Results subsection + SI figures.
- Discussion/Limitations state results are correlational.
