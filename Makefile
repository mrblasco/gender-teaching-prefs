.PHONY: all montecarlo covid models plots heterogeneity fit-heterogeneity plot-heterogeneity

all: montecarlo covid models plots heterogeneity


# ---- Analysis ---- 
montecarlo: output/01_plot_montecarlo_analysis/.completed
models: output/03_fit_models/.completed
covid: output/02_plot_covid_analysis/.completed
plots: output/04_make_plots_v2/.completed

# Heterogeneity is split into a slow fitting step and a fast plotting
# step, so figures can be re-rendered without refitting the models.
fit-heterogeneity: output/09_fit_heterogeneity/.completed
plot-heterogeneity: output/09_heterogeneity_analysis/.completed
heterogeneity: plot-heterogeneity

# Generic rule: scripts/<name>.R -> output/<name>/ (one output-dir arg).
output/%/.completed: scripts/%.R
	@echo "Rendering $@ ..."
	@mkdir -p "$(dir $@)"
	@Rscript $< "$(dir $@)" \
		|| { echo "ERROR" ; exit 1; }
	@touch "$@"
	@echo "Done!"

# Plotting reads the fitted coefficients, so it depends on the fit step
# and takes two args: <out_dir> <fit_dir>.
output/09_heterogeneity_analysis/.completed: scripts/10_plot_heterogeneity.R output/09_fit_heterogeneity/.completed
	@echo "Rendering $@ ..."
	@mkdir -p "$(dir $@)"
	@Rscript $< "$(dir $@)" "output/09_fit_heterogeneity" \
		|| { echo "ERROR" ; exit 1; }
	@touch "$@"
	@echo "Done!"
