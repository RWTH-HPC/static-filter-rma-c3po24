MUST (Marmot Umpire Scalable Tool)

MUST automatically checks parallel applications for correct use of MPI. 
It operates at runtime and intercepts all MPI calls that an application 
issues to verify their correctness. It uses the Generic Tool 
Infrastructure (GTI), and PnMPI as base packages.

The MUST documentation is located at doc/manual/manual.pdf.
The License file is located directly in this package, named LICENSE.txt

MUST uses CMake as a build system to enable compatibility to multiple
operating systems. See the CMake documentation as well as the MUST 
documentation for further details on how to build and install with 
CMake.


----------------------------
CHANGE-LOG
----------------------------
v1.9.2 (November 2023):
- Add option to send error reports via mail
- Add option to enable / disable TSan report functionality
- Fix several bugs in mustrun when using prebuilt infrastructure
- Fix build error in PnMPI due to wrong install locations in CMake
- Add missing predefined MPI_Ops (MPI_REPLACE and MPI_NO_OP)
- Change minimum required CMake version from 3.9 to 3.13.4 and fix compatibility with latest CMake

v1.9.1 (July 2023):
- Fix compatibility with older versions of CMake
- Fix some issues with backward, allow to disable specific debugging libraries in backward
- Mustrun returns an error code for all output formats
- Fix several bugs in analyses

v1.9.0 (May 2023):
- Added support for partitioned P2P communication
- Added support for MPI 4 functions
- Added JSON as new output format
- Reports use of MPI functions where MUST does not apply any analysis
- Added OMPT-based OpenMP analyses
- Added User Cache for reuse of customized tool infrastructure builds (default: ~/.must/cache)
- Integrated LLVM testing infrastructure to allow stand-alone testing

v1.8.0 (Nov 2022):
- Integrated ThreadSanitizer to detect data races involving MPI function calls in hybrid applications
- Employ TypeART to check for type inconsistencies in MPI function calls
- Allow filtering of MUST's output, splitting of HTML output across multiple files, and specification of the output path for HTML output
- Move from CTests to LLVM's lit for the test suite

v1.7.0 (March 2021):
- Enable builiding with OpenMPI 4
- Stabilized support for hybrid (OpenMPI + MPI) programs
- Fixed several reported issues

v1.6.0 (December 2019):
- Added support for non-blocking collectives and missing MPI-3 base datatypes, and multi-threaded applications
- Prebuilt configurations to speed up tool startup
- MUST, GTI, and PnMPI are delivered in a single archive file
- Fixed installation and compatibility issues

v1.5.0 (July 2016):
-

v1.4.0-rc1 (November 2014):
- Capture and replay mode allow MUST to store and replay correctness errors (for deterministic applications)
- Allinea DDT integration
-- Uses DDT manual launch mode to start an application with mustrun and to connect it to a waiting instance of DDT
-- A DDT plugin automatically sets breakpoint to stop the debugger when an error is detected
-- Capture and replay enables breakpoints for correctness errors that MUST detects asynchronously
- Improved unfoldable HTML reports
- Adapted handling for MPI_Test_cancelled to widen the range of errors MUST can detect
- Smaller bug fixes and portability improvements

v1.3.0 (February 2014):
- Largely distributed deadlock detection scheme that only uses a centralized graph detectionafter a timeout (scalability ~10,000 processes)
- Optimizations and corrections for intralayer based type matching used to handle irregular MPI collective communications (e.g., MPI_Alltoallv)
- Checks for MPI communication buffer attachment (MPI_Buffer_attach and related functions)
- New partial MPI Request tracking module that only tracks persitent requests enables performance improvements for several MUST modules
- Distributed CollectiveMatch module uses the stride representations of GTI's channel identifiers to provide better performance for 
   comb-shape communicators
- Basic checks for thread level usage (MPI_Init_thread)

v1.2.0 (November 2012):
- Application crash handling to allow efficient MUST internal communication even if the target application crashs
- Distributed checks for lost message detection, point-to-point and collective type matching, and collective checking
-- The only check that is still only available as a centralized version is the actual deadlock detection
- Extended deadlock view provides details for non-blocking communications and for potential message mismatch situations
- Visualization of path expressions provide detailed insight into communication buffer overlap situations and for type mismatches
- Completely reworked mustrun command
- Numerous bug fixes and portability improvements

v1.1.0 (April 2012):
- New deadlock display (if a DOT installation is available, this comes with the graphviz package on most linux systems)
- Wait-for graphs now show message tags for blocked P2P messages
- Checkpoint & Restart functionality for deadlock detection
-- Adds higher accuracy and support for complex corner cases
-- Some corner cases may require imprecise decisions by MUST, this will be hinted upon in its output file
- Bugfix to support MPI_STATUS[ES]_IGNORE
- Various small bugfixes and portability improvements
- All remainders of the deprecated OTF tracing functionality were removed

v1.10 (November 2011):
- Initial MUST version
