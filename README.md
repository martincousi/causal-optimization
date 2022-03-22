# causal-optimization

This is the code and results used by the Cousineau et al. (2022) paper. *Note that some adjustments were made to this code since the real code was run on a cluster.*

The `src` folder contains the R code. In particular,

- The `methods` folder contains the R scripts to obtain the results of the various methods tested. The 7700 iterations should be parallelized on a cluster in order to obtain the results in a reasonable time.
- The `analyze_res.R` file contains the code to analyze the various result files as in the paper. To execute this code, you will also need to download the raw results from the 2016 ACIC competition at [https://github.com/vdorie/aciccomp/tree/master/bakeoff/results_orig](https://github.com/vdorie/aciccomp/tree/master/bakeoff/results_orig). You may also need to run some methods to obtain the zip files.
- The `plot_covariate_balance.R` file contains the code to reproduce the supplementary material results.
- The `tools.R` file contains some functions reused by several of these scripts.

**Reference**

Cousineau M, Verter V, Murphy SA, Pineau J.  Estimating causal effects with optimization-based methods: A review and empirical comparison. To appear in *European Journal of Operational Research*. 2022. doi: [10.1016/j.ejor.2022.01.046](https://doi.org/10.1016/j.ejor.2022.01.046)

