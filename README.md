# Leveraging Static Analysis to Accelerate Dynamic Race Detection in RMA Programs - Supplemental Material
This is supplemental material for the paper "Leveraging Static Analysis to Accelerate Dynamic Race Detection in RMA Programs" submitted to the C3PO workshop.

## Repository Structure

- [classification_quality](classification_quality/): Results of RMARaceBench of MUST-RMA with the different filters applied
- [MUST](MUST/): Source code of MUST-RMA
- [RMAOptimizerPlugin](RMAOptimizerPlugin/): Static analysis passes that are presented in the paper
- [performance_evaluation](performance_evaluation): Source codes, results, and plotting scripts of the performance evaluation


## Classification Quality Results
We used the set of test cases provided by RMARaceBench and extended them with a `misc` category that contains several test cases that are challenging to be understood for static analysis tools (due to aliasing, nesting of functions etc.). The codes of the misc category are available at [classification_quality/rmaracebench/MPIRMA/misc](classification_quality/rmaracebench/MPIRMA/misc).

We ran the RMARaceBench tests in three variants: 1) without any optimization, 2) with BDX(10), 3) with BDX(∞). The classification quality results are available here:
  - [Results with no optimization applied](classification_quality/results-noopt/results_parsed)
  - [Results for run with BDX(10),CLUSTER](classification_quality/results-clusteral10/results_parsed)
  - [Results for run with BDX(∞),CLUSTER](classification_quality/results-clusteral1000/results_parsed)

If no optimization is applied, then MUST-RMA has a recall of 1 for the `misc` category. For BDX(∞), it is 0.87, since function pointer aliasing is not detected. For BDX(10), it is 0.81, since the test cases with a deep nesting of pointer aliasing are not correctly detected. The precision for all variants is 1.


## Performance Evaluation
The results of the performance evaluation for the different benchmarks are available at:
- [performance_evaluation/benchmark_results/PRK_stencil/result](performance_evaluation/benchmark_results/PRK_stencil/result)
- [performance_evaluation/benchmark_results/PRK_transpose/result](performance_evaluation/benchmark_results/PRK_transpose/result)
- [performance_evaluation/benchmark_results/miniMD/result](performance_evaluation/benchmark_results/miniMD/result)
- [performance_evaluation/benchmark_results/lulesh/result](performance_evaluation/benchmark_results/lulesh/result)
- [performance_evaluation/benchmark_results/BT-RMA/result](performance_evaluation/benchmark_results/BT-RMA/result)
- [performance_evaluation/benchmark_results/miniVite/result](performance_evaluation/benchmark_results/miniVite/result)

Our benchmark suite is based on the [JUBE benchmarking environment](https://apps.fz-juelich.de/jsc/jube/jube2/docu/) and can be used to reproduce our experiments. The setup can be found at [performance_evaluation/rma_codes](performance_evaluation/rma_codes).

The sources of different codes are available at
- PRK_Stencil: [performance_evaluation/rma_codes/benchmarks/PRK_stencil/prk](performance_evaluation/rma_codes/benchmarks/PRK_stencil/prk)
- PRK_Transpose: [performance_evaluation/rma_codes/benchmarks/PRK_transpose](performance_evaluation/rma_codes/benchmarks/PRK_transpose/prk)
- miniMD (RMA port): [performance_evaluation/rma_codes/benchmarks/miniMD/miniMD](performance_evaluation/rma_codes/benchmarks/miniMD/miniMD)
- LULESH (RMA port): [performance_evaluation/rma_codes/benchmarks/lulesh/lulesh](performance_evaluation/rma_codes/benchmarks/lulesh/lulesh)
- BT-RMA (RMA port): [performance_evaluation/rma_codes/benchmarks/BT-RMA/npb](performance_evaluation/rma_codes/benchmarks/BT-RMA/npb)
- miniVite: [performance_evaluation/rma_codes/benchmarks/miniVite](performance_evaluation/rma_codes/benchmarks/miniVite)