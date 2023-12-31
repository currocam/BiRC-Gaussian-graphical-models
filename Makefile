snakemake_threads := $(or $(snakemake_threads), 'all')
all: .singularities/workflow.sif
	snakemake all --cores $(snakemake_threads) --use-singularity -s src/Snakefile -p --rerun-triggers mtime --rerun-incomplete --keep-going --keep-incomplete

dry: .singularities/workflow.sif
	snakemake all --cores $(snakemake_threads) -n --use-singularity -s src/Snakefile -p --rerun-triggers mtime --rerun-incomplete --keep-going --keep-incomplete


touch: .singularities/workflow.sif
	snakemake all --cores $(snakemake_threads) --use-singularity -s src/Snakefile -p --touch

.singularities/workflow.sif: src/Singularity.def
	mkdir -p .singularities && sudo singularity build .singularities/workflow.sif src/Singularity.def

clean:
	# Remove all files *.time recursively in data	
	find data -name "*.time" -type f -delete
