
# render: 
# 	Rscript scripts/10_render_manuscript.R

# review: peer_review/PNEXUS/2026-03-28-response-to-reviewers.Rmd
# 	Rscript scripts/12_render_response.R

# diff: submission/diff.pdf
# 	cd submission; latexdiff revision-v2/_main.tex master/_main.tex > diff.tex && pdflatex diff.tex


montecarlo: data/results/montecarlo/summary.log
data/results/montecarlo/summary.log: scripts/01_plot_montecarlo_analysis.R
	@echo "Rendering $@ ..."
	@Rscript $< > $@ 2>&1
