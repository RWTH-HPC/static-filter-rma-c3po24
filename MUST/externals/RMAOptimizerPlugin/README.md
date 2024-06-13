# TSanRMAOptimizerPlugin

## Overview
This LLVM Pass plugin attempts to optimize ThreadSanitizer execution for data race detection on RMA/PGAS programs.
Currently, only MPI RMA on C and C++ is supported, with support for further RMA/PGAS frameworks and programming languages planned.

This is done using four optimization methods:

1. Do not instrument irrelevant memory locations (AL)
2. Do not instrument loads that cannot lead to a data race (ALX)
3. Delay race detection until required (DELAY)
4. Cluster ThreadSanitizer annotations inside a BasicBlock to at most two calls per memory location; one read, one write (CLUSTER)

## Usage

First, the program must be turned into a representation parseable by `opt`; either bytecode (`.bc`) or LLVM IR (`.ll`).

Depending on programming language, this can be done using:

- `clang` for C programs
- `clang++` for C++ programs

Use flags `<infile> -S -emit-llvm -o <outfile>` to generate LLVM IR using the correct tool.

The `opt` tool can then be used to apply the optimizations provided by this plugin.
Depending on need, different optimizations may need to be used, requiring different passes and arguments to be used. Those can be generated automatically using a provided CMake function, or manually.

After the specific passes are known, they can be set using the `-passes=<passlist>` argument.

Finally, in both cases the plugin must be loaded using `-load-pass-plugin <Path-to-Plugin>`.

### Automatic passlist and arguments using `rmaopt_get_config.sh`

The correct configuration for `opt` can be retrieved using `rmaopt_get_config.sh`.
Pass the needed optimizations (space-separated) as well as what information to retrieve using `--get-config passes` or `--get-config args` to get the passlist or arguments for `opt` respectively.

Additionally, for the allowlist optimizations, the depth value can be configured by appending it to the optimization, such as `AL10` to have the depth up to 10.

### Automatic passlist and arguments using CMake

If this plugin is part of a larger CMake project, the required passes and options for `opt` can be generated using the provided `RMAOPT_GET_REQUIREMENTS_FOR_OPTIMIZATIONS`
CMake function, which will set `RMAOPT_REQUIRED_PASSES_FOR_OPT` and `RMAOPT_REQUIRED_OPTIONS_FOR_OPT` depending on the requested optimizations passed as an argument.
This could be done like in the following:
```
set(opts "AL" "CLUSTER")
RMAOPT_GET_REQUIREMENTS_FOR_OPTIMIZATIONS("${opts}")
# RMAOPT_REQUIRED_PASSES_FOR_OPT: tsanMOD-module,function(tsanMOD),tsanMOD-cluster-shim,function(tsanMOD-clusteringopt)
# RMAOPT_REQUIRED_OPTIONS_FOR_OPT: -tsanMOD-use-optallowlist=1
```

### Manual passlist and arguments

To use ThreadSanitizer without optimizations, only use these passes: `-passes=tsanMOD-module,function(tsanMOD)`. Note that this base configuration is not very useful, as ThreadSanitizer requires the `sanitize_thread` attribute on each function to apply any instrumentation, which is not done when invoked manually. Instead, prepend `function(tsanMOD-attrpass)` in order for this attribute to be added.
The specific optimizations may or may not require this, see below for details.

Then, `opt` can be used to enable the different optimization methods:

- AL: Add option `-tsanMOD-use-optallowlist=1`. This will also apply the `sanitize_thread` attribute only to the relevant functions; the `tsanMOD-attrpass` should **NOT** be used.
- ALX: Add option `-tsanMOD-use-optallowlist-remoteaccesstypeext=1`. Depends on method 1 and cannot run separately.
- DELAY: **Prepend** `function(tsanMOD-activatoropt)` to the pass list. If not using any allowlist method, the `sanitize_thread` attribute must still be applied!
- CLUSTER: **Append** `tsanMOD-cluster-shim,function(tsanMOD-clusteringopt)` to the pass list. If not using any allowlist method, the `sanitize_thread` attribute must still be applied!

# TODO Usage with modified runtime

## Compiling

Clone the repo **recursively**.

This project does not require external dependencies. However, a compiler with support for C++17 is required.

Compile using CMake:

- `mkdir build && cd build`
- `cmake ..`
- `cmake --build . -j$(nproc)`

The binaries will be in `build/bin/`.
