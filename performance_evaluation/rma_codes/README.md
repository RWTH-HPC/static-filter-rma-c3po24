# RMA Benchmark Collection

## Description

The RMA Benchmark Collection consists of a set of MPI-RMA benchmarks, including both adapted benchmarks like lulesh and miniMD, and unmodified benchmarks like miniVite that were cloned from their original GitHub repositories. 
We use the [JUBE benchmarking environment](https://apps.fz-juelich.de/jsc/jube/jube2/docu/introduction.html) to manage, automate, and ensure reproducibility of benchmark executions.

The main purpose of the benchmark collection is to evaluate the performance impact of [MUST](https://itc.rwth-aachen.de/must/) on MPI-RMA programs. For this we run the benchmarks with different configurations of instrumentations. We distinguish between `compile_mode` and `measurement_mode`.  `compile_mode` consists of the instrumentations that are added during compilation, which are currently `base` (no instrumentation), and `tsan` (ThreadSanitizer). While `measurement_mode` consists of runtime analyses, which are currently `base` (no analysis), and `must` (MUST). Each combination of `compile_mode` and `measurement_mode` is tested.


This readme covers the usage using the CLAIX-18 compute cluster with the Rocky operating system as an example. If you are using a different system, it may be necessary to adjust certain commands, such as module loading, to suit the requirements of your particular system.

## Available Benchmarks
- [lulesh](https://github.com/LLNL/LULESH)
- [miniMD](https://github.com/Mantevo/miniMD)
- [miniVite](https://github.com/ECP-ExaGraph/miniVite)
- PRK
    - stencil
    - transpose
- [NAS Parallel Benchmarks (npb) v3.4.2](https://www.nas.nasa.gov/software/npb.html)
    - BT-RMA based on a [previous adapation of v2.4](https://git-ce.rwth-aachen.de/hpc-research/benchmarks/code-npb-bt-rma)

## Preparation
We use multiple git calls to update/check repositories during the preparation and usage, so it might be helpful to use an ssh-key-manager so you do not have to provide your passphrase everytime, e.g.
```
eval "$(ssh-agent -s)"          
ssh-add
```

First, clone the repository (with ssh)
```
git clone git@git-ce.rwth-aachen.de:hpc-research/benchmarks/rma-codes/jube.git
```

We use submodules to manage the benchmark codes. Before you can run a benchmark you have to initialize and update the submodules of the benchmark codes:
```
git submodule update --init --recursive
```
This clones the submodules from their respective repositories, checks out the specified versions of the code, and integrates them into the main project. This  ensures that you have all the necessary benchmark code and that it is up to date with the correct version specified in the project. This is an important step to ensure that the benchmarks can run properly and produce accurate results.

## Basic Usage

First, you need to have JUBE operationable, on CLAIX-18 you can load the JUBE module with:
```
module load Python
module load JUBE
```
Sometimes it seems to be necessary to tell JUBE to use the correct shell environment. If benchmark runs fail during steps such as compilation, it is possible that JUBE is executing commands in the wrong shell environment and may fail to load the necessary modules. So, make sure that JUBE uses the correct shell, for CLAIX-18 this is "/bin/zsh".
```
export JUBE_EXEC_SHELL="/bin/zsh"
```

The benchmarks are stored in the "/benchmarks" directory. To run a specific benchmark, you need to navigate to its corresponding directory and then use the `jube run` command with the benchmark's XML file:
```
jube run <benchmark_name>.xml 
```

To check the status of each step in the benchmark run, you can use the  `jube continue` command and passing it the directory where the benchmark runs are located. By default this is /\<benchmark_name\>.benchmarks. The command will show the steps that have already been completed and the steps that are still pending. This can be useful for monitoring the progress of a benchmark run and identifying any issues or errors that may have occurred during the run.
```
jube continue <benchmark_name>.benchmarks
```
<p><sub>
    Note: On CLAIX-18 the execute steps which use tsan always segfault and are reported to have encountered an error. This is currently the "normal" behaviour of ThreadSanitizer provided by the default clang compiler (version 13.0.1). However, the benchmarks should still complete normally and error-free.
</sub></p>


To obtain the results and print the result table, use the `jube result` command and passing it the directory where the benchmark runs are located. Normally the directory names end with *.benchmarks*:
```
jube result <benchmark_name>.benchmarks
```

## Enabling Additional Features via Tagging
Tagging can be used to enable or disable additional features of a benchmark run.

The available tags are: `isl`, `layout`, `no_tsan`, `tsan`, `tsan-opt`, `nodelist`, `pnmpi`, `rebuild`, `rebuild_must`, `rebuild_source`, `no_must`, `must`, `must::verbose`, `test`, `toolprocess`, and `vtune`

To use tags in a benchmark run, use the `--tag` parameter with the `jube run` command and pass it the desired tags.

```
jube run <benchmark_name>.xml --tag <tags>
```
 e.g. if you want to run a benchmark with the `test` and `nodelist` tags enabled:
```
jube run <benchmark_name>.xml --tag test nodelist
```

Following is a description of the available tags:

- **isl** &mdash; The `isl` tag prompts MUST to use the interval skip list by using the 'must:rma-mode isl' option. If this tag is not used MUST uses the 'must:rma-mode shadow' option instead.

- **layout** &mdash; The `layout` tag prompts MUST to use the 'must:layout <layout>' option. The specific layout that is used is specified in the must.xml. If this tag is not used MUST creates its own layout using the 'must:rma-only' option instead.

- **no_tsan** &mdash; The `no_tsan` tag determines that the benchmark is compiled without any instrumentation. Both, `no_tsan` and `tsan`, can be used at the same time. If neither `no_tsan` nor `tsan` is used then the benchmark runs with both enabled by default.

- **tsan** &mdash; The `tsan` tag determines that the benchmark is run with ThreadSanitizer. Both, `no_tsan` and `tsan`, can be used at the same time. If neither `no_tsan` nor `tsan` is used then the benchmark runs with both enabled by default.

- **tsan-opt** &mdash; The `tsan-opt` tag enables the [RMAOptimizerPlugin](https://git-ce.rwth-aachen.de/hpc-research/correctness/RMAOptimizerPlugin) on top of ThreadSanitizer. This will only run if explicitly requested, and has no effect on the aforementioned tags.

- **nodelist** &mdash;
The nodelist is always printed in job.out file. Therefore, using the `nodelist` tag only enables the output of the result table. This, however, comes with the benefit that the tag can be used retroactively with the `jube result` or `jube analize` commands. To do this you need to use the upadate parameter *-u* and pass the \<benchmark\>.xml.
```
jube result <benchmark_name>.benchmarks -u <benchmark_name>.xml --tag nodelist
```

- **pnmpi** &mdash; The `pnmpi` tag creates an additional job for each configuration, which does not use tsan or must. It also generates a separate table that shows how many times certain MPI-calls were executed during the benchmark run. To count other MPI-calls, you need to add them to both the patternset *PnMPI_pattern* and result table *PnMPI_result* in the /common/pnmpi.xml file. However, please note that any MPI-calls that were added will only be counted in future benchmark runs and cannot be retroactively counted in benchmark runs that were executed without those changes.

- **rebuild** &mdash; The `rebuild` tag is a combination of `rebuild_must` and `rebuild_source`.

- **rebuild_must** &mdash; The `rebuild_must` tag forces must to be rebuild from scratch.

- **rebuild_source** &mdash; The `rebuild_source` tag forces the benchmarked executable to be rebuild, i.e., the cached executable gets replaced with a newly build executable.

- **no_must** &mdash; The `no_must` tag determines that the benchmark is run without MUST. Both, `no_must` and `must`, can be used at the same time. If neither `no_must` nor `must` is used then the benchmark runs with both enabled by default.

- **must** &mdash; The `must` tag determines that the benchmark is run with MUST. Both, `no_must` and `must`, can be used at the same time. If neither `no_must` nor `must` is used then the benchmark runs with both enabled by default.

- **must::verbose** &mdash; The `must::verbose` tag activates the verbose output of MUST.

- **test** &mdash; The `test`  tag is used to indicate that a benchmark run is a test run and does not enforce the jobs to be run exclusively on the nodes or ensure that the used nodes are on the same switch. This is useful for verifying that a benchmark is running correctly before scheduling a larger, more comprehensive benchmark run.

- **memusage** &mdash; The `memusage` tag is used to additionally measure the total memory consumption (max_rss) at the end of the program execution (in MPI_Finalize). It will be printed out as additional column of the result table. 

- **toolprocess** &mdash; The `toolprocess` tag changes the loaded must layout to "must_layout_toolprocess.xml.in", which uses toolprocesses for additional analysis.

- **filterstats** &mdash; The `filterstats` tag outputs statistics on the static filtering with the MUST compiler wrapper.


- **vtune** &mdash; Starts the regular jobs with intel vtune.

### Input Presets
 - **C** &mdash; The `C` tag runs the benchmark with the user defined input values. The user defined input values are also used if none of the `S`, `M`, and `L` tags are used. 
 - **S** &mdash; The `S` tag runs the benchmark with predefined values such that the runtime for 24 tasks is appoximately 10s.
 - **M** &mdash; The `M` tag runs the benchmark with predefined values such that the runtime for 24 tasks is appoximately 60s.
 - **L** &mdash; The `L` tag runs the benchmark with predefined values such that the runtime for 24 tasks is appoximately 300s.

## Plotting

Load the python module
``` 
module load Python
```
Change directory to jube/utils/plots/

### Positional Arguments
| argument | description |
|---|---|
|`<benchmark_name>`| Name of the benchmark|

### Optional Arguments

| argument | description |
|---|---|
| `-e` or `--err`       | Error column|
| `-i` or `--id`        | If not specified the script will use the most current benchmark run by default. |
| `-t` or `--target`    | Data column|
| `-h` or `--help`      | Display a help message.|

### Usage

```
python plot.py [-h] [-i ID] [-t TARGET] [-e ERR] [-p PATH] benchmark_name
```
