
# render: 
# 	Rscript scripts/10_render_manuscript.R

# review: peer_review/PNEXUS/2026-03-28-response-to-reviewers.Rmd
# 	Rscript scripts/12_render_response.R

# diff: submission/diff.pdf
# 	cd submission; latexdiff revision-v2/_main.tex master/_main.tex > diff.tex && pdflatex diff.tex

# ---- Step 1. Montecarlo ---- 
montecarlo: output/montecarlo/summary.log
output/montecarlo/summary.log: scripts/01_plot_montecarlo_analysis.R
	@echo "Rendering $@ ..."
	@mkdir -p "$(dir $@)"
	@Rscript $< > $@ 2>&1

# ---- Step 2. Covid ---- 
covid: output/covid/summary.log
output/covid/summary.log: scripts/02_plot_covid_analysis.R
	@echo "Rendering $@ ..."
	@mkdir -p "$(dir $@)"
	@Rscript $< > $@ 2>&1

# ---- Setp 3. Associations ---- 
regressions: output/regressions/summary.log
output/regressions/summary.log: scripts/04_plot_regressions.R
	@echo "Rendering $@ ..."
	@mkdir -p "$(dir $@)"
	@Rscript $< > $@ 2>&1
