.PHONY: all montecarlo

all: montecarlo covid plots


# ---- Analysis ---- 
montecarlo: output/01_plot_montecarlo_analysis/summary.log
models: output/03_fit_models/summary.log
covid: output/02_plot_covid_analysis/summary.log
plots: output/04_make_plots_v2/summary.log


output/%/summary.log: scripts/%.R
	@echo "Rendering $@ ..."
	@mkdir -p "$(dir $@)"
	@Rscript $< "$(dir $@)" > "$@" 2>&1 || echo "ERROR"