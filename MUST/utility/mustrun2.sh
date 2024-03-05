#! @BASH@
# -*- sh-shell: bash; -*-

# Part of the MUST Project, under BSD-3-Clause License
# See https://hpc.rwth-aachen.de/must/LICENSE for license information.
# SPDX-License-Identifier: BSD-3-Clause
#
#@@
# mustrun - tool to run applications with the MPI correctness tool MUST
#

#----------------- Function ----------------
#name: printHelp
#purpose: prints the help for this script
#-------------------------------------------

_MUST_WDIR=$PWD

printHelp()
{
  if MYTPUT=$(which tput)
  then
      WIDTH=$($MYTPUT cols) || WIDTH=80
  elif MYSTTY=$(which stty)
  then
      WIDTH=$($MYSTTY size | cut -d' ' -f2) || WIDTH=80
  else
      WIDTH=80
  fi
  if test $WIDTH -lt 80
  then
    WIDTH=80
  elif test $WIDTH -gt 130
  then
    WIDTH=130
  fi
  if ! MYFMT="$(which fmt) -w $WIDTH"
  then
      MYFMT="cat"
  fi
  (echo "\"mustrun\" from MUST @MUST_VERSION@"
  echo "Prepares and runs an application that is linked with P^nMPI for runtime analysis"
  echo "        with the MPI correctness tool MUST."
  echo ""
  echo "Replace your regular mpiexec/mpirun command with mustrun, usage:"
  echo ""
  echo "mustrun [--help|-help|--must:help] [--must:nocrash] [--must:layout <LAYOUT.xml>]"
  echo "        [--must:mode {prepare|run|preparerun}] [--must:mpiexec <RUNCOMMAND>]"
  echo "        [--must:np <NP-SWITCH>] [--must:temp <DIR>] [--must:user-cache-dir <DIR>]"
  echo "        [--must:no-user-cache] [--must:apis <API-XMLS>]"
  echo "        [--must:analyses <ANALYSES-XMLS>] [--must:verbose] [--must:quiet]"
  echo "        [--must:exportflag <MPIEXEC-EXPORT-FLAG>] [--must:wait] <seconds>"
  echo "        [--must:hybrid] [--must:stacktrace {none|dyninst|backward}]"
  echo "        [--must:mpimode {SPMD|MPMD}] [--must:typeart] [--must:errorcode] <code>"
  echo "        <MPIRUNARGS> <COMMAND> <ARGS>"
  echo ""
  (echo "--help,-help,--must:help:"
  echo "        Prints this information."
  echo "--must:nocrash:"
  echo "        When using the default layout, switches on asynchronous communication to"
  echo "        improve tool performance, only use this option if your applications"
  echo "        suffers from no crash."
  echo "--must:layout <LAYOUT.xml>:"
  echo "        Overrides the default MUST layout with the given layout."
  echo "--must:nodesize <NUMBER>:"
  echo "        Specifies the systems number of processes per core, required for"
  echo "        application crash handling. See the MUST manual for details on the"
  echo "        resulting layouts."
  echo "--must:close <NUMBER>:"
  echo "        Place first layer tool processes close to the application processes"
  echo "        The number specifies the fanin for the first layer."
  echo "--must:distributed:"
  echo "        Distributed correctness checks, requires multiple layers of extra"
  echo "        processes."
  echo "--must:hybrid:"
  echo "        Enable analysis of multi-threaded applications. Requires an MPI library"
  echo "        supporting MPI_THREAD_MULTIPLE. Not necessary up to MPI_THREAD_FUNNELED."
  echo "--must:tsan:"
  echo "        Enable support for race detection with ThreadSanitzier. Implicitly"
  echo "        enables the \"hybrid\" option."
  echo "--must:rma:"
  echo "        Enable support for MPI RMA race detection. Implicitly"
  echo "        enables the \"hybrid\" option."
  echo "--must:rma-mode {shadow|isl}:"
  echo "        Select shadow memory (shadow) (default) or interval skip list (isl) for"
  echo "        RMA race detection."
  echo "--must:rma-only:"
  echo "        Load only necessary analysis for MPI RMA race detection,"
  echo "        all other analyses (deadlocks, parameter checks) are disabled. The output"
  echo "        mode is fixed to stdout. Only tool threads are started, no tool processes."
  echo "        This mode is in particular helpful for overhead measurements. Implicitly "
  echo "        enables the \"rma\" option."
  echo "--must:nodl:"
  echo "        Disables deadlock detection if --must:distributed is specified."
  echo "--must:fanin <NUMBER>:"
  echo "        Gives the maximal fanin for distributed layout. Default: 16. See"
  echo "        the MUST manual for details, automatically sets \"--must:distributed\"."
  echo "--must:fillnodes:"
  echo "        Forces MUST to fill all used nodes (helps for some batch systems)."
  echo "        Only use if necessary, requires --must:distributed and --must:nodesize."
  echo "--must:language {c|fortran}:"
  echo "        Specifies in which programming language the MPI calls should be"
  echo "        intercepted. Default: @MUST_DEFAULT_MPI_INTERFACE@"
  echo "--must:openmp"
  echo "        Apply OpenMP analysis for your program. Implicitly"
  echo "        enables the \"hybrid\" option."
  echo "--must:mode {prepare|run|preparerun}:"
  echo "        Operation mode of mustrun, \"prepare\" only creates the intermediate files"
  echo "        needed by MUST, \"run\" only runs an application that was previously"
  echo "        prepared with mustrun, \"preparerun\" performs a \"prepare\" first and a"
  echo "        \"run\" afterwards, default is \"preparerun\"."
  echo "--must:info:"
  echo "        Print the size for the (explicit or implicit) specified layout."
  echo "--must:np <NP-SWITCH>:"
  echo "        Tells MUST to search for <NP-SWITCH> in <MPIRUNARGS> for determining the"
  echo "        number of application processes, example: \"--must:np -np\"."
  echo "--must:mpiexec <RUNCOMMAND>:"
  echo "        Tells MUST to use the given command instead of mpiexec."
  echo "--must:temp <DIR>:"
  echo "        Specifies which directory should be used to store intermediate files, or"
  echo "        in which directory the preparation of this run was made, default is"
  echo "        \"must_temp\"."
  echo "--must:user-cache-dir <DIR>:"
  echo "        Specifies which directory should be used to store cached files. Defaults"
  echo "        to the environment variable XDG_CACHE_HOME if the variable is set and to"
  echo "        @MUST_USER_CACHE_DIR@ otherwise."
  echo "--must:no-user-cache:"
  echo "        Do not store files in the user cache directory."
  echo "--must:clean:"
  echo "        Removes all intermediate module and layout files generated by MUST after"
  echo "        the run completes. Does not remove MUSTs output files."
  echo "--must:cleanshm:"
  echo "        Removes shared memory fragments remaining after crash of application."
  echo "        Could raise problems when run multiple instances of mustrun on a host."
  echo "--must:analyses <ANALYSES-XMLS>:"
  echo "        Gives extra analysis specifications to the weaver, multiple xmls need to"
  echo "        be separated with a colon."
  echo "--must:apis <API-XMLS>:"
  echo "        Gives extra api specifications to the weaver, multiple xmls need to be"
  echo "        separated with a colon."
  echo "--must:verbose:"
  echo "        Prints extensive information, use this when submitting bug reports."
  echo "--must:partitioned_verbose_report:"
  echo "        Prints extensive information when for partitioned communication operations."
  echo "        Can print the location where a partition was first activated."
  echo "--must:timing:"
  echo "        Adds a \"time\" to the actual mpirun call, used to discern mustrun"
  echo "        overheads from the actual runtime."
  echo "--must:quiet:"
  echo "        Causes mustrun to give no output at all (if successful)."
  echo "--must:output {html|stdout|json}:"
  echo "        Specifies the output format of MUST correctness messages. Default is"
  echo "        \"html\" which provides a correctness report in a file named"
  echo "        \"MUST_Output.html\". Use \"stdout\" if you prefer to get MUSTs output"
  echo "        printed to std out. The \"json\" option gives MUSTs output in a"
  echo "        machine-readable JSON file named \"MUST_Output.json\". Note that MUST"
  echo "        will usually print this output on an extra task, so it may be placed"
  echo "        oddly."
  echo "--must:output-dir <PATH>"
  echo "        Declares the path to the output files."
  echo "--must:output-email-report <EMAIL-ADDRESS>:"
  echo "        Sends all the generated output files to your email address."
  echo "--must:filter-file <PATH>"
  echo "        Declares the path to the filterfile."
  echo "--must:ddt:"
  echo "        Enables the MUST-DDT integration. Must adds ddt-client (or the value"
  echo "        of --must:precmdddt) in front of the binary to link the starting"
  echo "        application with a DDT instance in the manual launch mode."
  echo "        This disables default output types (html/stdout) and rather issues"
  echo "        a function that DDT traps with breakpoints."
  echo "--must:precmdddt:"
  echo "        Specifies the command that MUST puts in front of the binary to support"
  echo "        DDT's manual launch mode, default is \"ddt-client\"."
  echo "--must:capture:"
  echo "        Stores correctness messages of MUST in a format that allows MUST to"
  echo "        reproduce the messages in a latter run. Usage intention is for the"
  echo "        MUST-DDT integration to trigger breakpoints on calls that cause errors"
  echo "        (Especially for errors that MUST detects remotely)."
  echo "        Default is off, is automatically enabled if --must:ddt is present."
  echo "--must:reproduce:"
  echo "        Causes MUST to trigger error messages that have been recorded with"
  echo "        --must:capture in a previous run. Use this to trigger breakpoints"
  echo "        for non-local correctness errors in the MUST-DDT integration."
  echo "        IMPORTANT: only for applications that use the same"
  echo "        deterministic MPI operations in all executions."
  echo "--must:exportflag:"
  echo "        flag needed to tell 'mpiexec' to export environmental variables."
  echo "        E.g. if you need mpiexec -x LD_PRELOAD to have LD_PRELOAD available"
  echo "        on compute node, give '-x' as exportflag"
  echo "--must:pnmpi-linked:"
  echo "        tells mustrun that pnmpi is already linked to the executable, per"
  echo "        default MUST tries to detect this automatically, use this, to override"
  echo "        its decission."
  echo "--must:wait:"
  echo "        Load the PnMPI debug module to print the PID of each rank and wait the"
  echo "        given number of seconds, to attach with a debugger in this time."
  echo "--must:mpimode:"
  echo "        Run MUST in SPMD (default) or MPMD MPI mode."
  echo "--must:typeart:"
  echo "        Enable typeart support to check for type inconsistencies in MPI calls."
  echo "--must:errorcode <code>"
  echo "        Tell MUST to return with errorcode <code>."
  echo "--must:instant-logging {info|warning|error|fatal}:"
  echo "        Tells MUST to immediately log <level> messages that may cause the application"
  echo "        and MUST to crash."
  echo "<MPIRUNARGS>:"
  echo "        Arguments you usually pass to mpirun/mpiexec, mustrun searches for a"
  echo "        \"-n\" \"-np\" \"-p\" in the given order to determine how many tasks"
  echo "        should be used for the application."
  echo "<COMMAND>:"
  echo "        Application to execute."
  echo "<ARGS>: Application arguments."
  echo ""
  echo "Note that the default layout used for MUST requires one additional process,"
  echo "mustrun will execute an mpiexec with the number of application processes you"
  echo "specified plus 1, make sure to allocate the sufficient resources."
  echo ""
  echo "Example: mustrun -np 4 myapp.exe"
  echo "(Prepares and runs \"maypp.exe\" with default layout)"
  echo ""
  echo "Contact: must-feedback@lists.rwth-aachen.de"
  echo ""
  for i in "$@"
  do
    echo "$i"
  done) | $MYFMT )>&2
  exit 1
}

export _LOCKED=0
export _LOCKDIR=""

#----------------- Function ----------------
# name: assert_dir_is_locked(dir, location)
# purpose: assert that we have the lock for the given directory
#-------------------------------------------
function assert_dir_is_locked {
  if [ ${_LOCKED} -ne 1 ]; then
    musterror "Assertion failed in $2: Directory $1 is expected to be locked but isn't."
    exit 10
  fi
  if [ ${_LOCKDIR} != $1 ]; then
    musterror "Assertion failed in $2: Directory $1 is expected to be locked but ${_LOCKDIR} is locked."
    exit 11
  fi
}

#----------------- Function ----------------
# name: assert_not_locked(location)
# purpose: assert that we do not hold a lock
#-------------------------------------------
function assert_not_locked {
  if [ ${_LOCKED} -ne 0 ]; then
    musterror "Assertion failed in $1: No lock expected, but ${_LOCKDIR} is locked"
    exit 12
  fi
}

#----------------- Function ----------------
# name: try_lock_dir(dir)
# purpose: try to get a lock for the given directory
#          the directory must exist
#-------------------------------------------
function try_lock_dir {
  assert_not_locked "${BASH_SOURCE}:${LINENO}"
  if [ $do_verbose -eq 1 ]
  then
    mustecho "Trying to lock the directory ${1}"
  fi
  mkdir -- "${1}/lockdir" 2>/dev/null
  local retval=$?
  if [ ${retval} == 0 ]
  then
    _LOCKED=1
    _LOCKDIR="${1}"
    # Ensure to release the lock on exit.
    # shellcheck disable=SC2064
    trap "release_dir ${1}" 0
  fi
  return ${retval}
}

#----------------- Function ----------------
# name: assert_not_locked(location)
# purpose: assert that we do not hold a lock
#-------------------------------------------
function release_dir {
  assert_dir_is_locked ${1} "${BASH_SOURCE}:${LINENO}"
  if [ $do_verbose -eq 1 ]
  then
    mustecho "Releasing locked dir ${1}"
  fi
  trap 0
  rm --recursive --force -- "${1}/lockdir"
}

#----------------- Function ----------------
#name: printVersions
#purpose: generates a prefix for echo output
#-------------------------------------------

printVersions()
{
  echo "MUST @MUST_VERSION@"
  echo "based on"
  echo "GTI v@GTI_VERSION@"
  echo "and"
  echo "PnMPI v@PNMPI_VERSION@"
}

#----------------- Function ----------------
#name: mustecho
#purpose: generates a prefix for echo output
#-------------------------------------------
NO_ECHO_NL=0
mustecho()
{
    # if this is not a continued echo
    if [ $NO_ECHO_NL -ne 1 ]
    then
        # prefix
        echo -n "[MUST] "
    fi
    NO_ECHO_NL=0
    # record echo to be continued
    OPTIND=1
    if getopts "n" opt
    then
        NO_ECHO_NL=1
    fi
    echo "$@"
}

#----------------- Function ----------------
#name: musterror
#purpose: generates a prefix for error output
#-------------------------------------------
musterror()
{
    # if there is a continuing echo
    if [ $NO_ECHO_NL -eq 1 ]
    then
        echo
    fi
    # prefix
    echo -n "[MUST-ERROR] " >&2
    echo "$@" >&2
}

#----------------- Function ----------------
#name: cmaketrue
#purpose: tests whether cmake var is true
#-------------------------------------------
cmaketrue()
{
    t=$(echo $1 | tr "[:upper:]" "[:lower:]")
    case $t in
        *-notfound)
        return 1
        ;;
    esac
    if [ -n "$t" ] && [ "$t" == "true" -o "$t" == "on" -o "$t" == "1" ]
    then
        return 0
    fi
    return 1
}

#----------------- Function ----------------
#name: remove_single_opt
#purpose: removes a must option from input
#-------------------------------------------
remove_single_opt()
{
    input=$(echo "$input" | sed -e "s/--must:$1//")
}

#----------------- Function ----------------
# name: get_opt_arg(option)
# purpose: get the argument associated with --must:option
# return: argument via stdout
#-------------------------------------------
get_opt_arg()
{
    local ret=$(echo "$input" | sed -e "s/.*--must:$1[[:space:]]\([^[:space:]]\{1,\}\).*/\1/")
    # found an argument?
    if test "$ret" == "$input"
    then
        printHelp "Missing argument for option --must:$opt"
    fi
    # argument should not start with '-'
    if [[ "$1" != "np" && "$1" != "exportflag" && "$ret" == -* ]]
    then
        printHelp "Missing argument for option --must:$opt"
    fi
    echo $ret
}

#----------------- Function ----------------
#name: remove_opt_and_arg
#purpose: removes a must option and argument from input
#-------------------------------------------
remove_opt_and_arg()
{
    input=$(echo "$input" | sed -e "s/--must:$1[[:space:]][^[:space:]]\{1,\}//")
}

#----------------- Function ----------------
# name: min( integers ... )
# purpose: get the minimum number passed to the function
# return: argument via stdout
#-------------------------------------------
min()
{
    local min=$1
    shift
    while [ $# -gt 0 ]
    do
    	if [ $min -gt $1 ]
    	then
    	    min=$1
    	fi
    	shift
    done
    echo $min
}

#----------------- Function ----------------
# name: max( integers ... )
# purpose: get the maximum number passed to the function
# return: argument via stdout
#-------------------------------------------
max()
{
    local max=$1
    shift
    while [ $# -gt 0 ]
    do
    	if [ $max -lt $1 ]
    	then
    	    max=$1
    	fi
    	shift
    done
    echo $max
}

#----------------- Function ----------------
#name: check_opt_arg
# args: ($1=option, $2=argument)
#purpose: validate option arguments
#-------------------------------------------
check_opt_arg()
{
    out=$1
    shift
    arg=$@
    case $out in
    layout)
        absolut_path arg
        if [ ! -f "$arg" ]
        then
            musterror "Can't find the given layout file \""$arg"\"!"
            exit 1
        fi
        ;;
    mode)
        case "$arg" in run | preparerun | prepare | build | config | auto);; #ok, do nothing
        *) # unknown
            printHelp "Unknown argument for option --must:mode : $arg" \
                        "                                         (valid: run | prepare | preparerun | build | config | auto)"
            ;;
        esac
        ;;
    language)
        case "$arg" in c | fortran);; #ok, do nothing
        *) # unknown
            printHelp "Unknown argument for option --must:language : $arg" \
                        "                                             (valid: c | fortran)"
            ;;
        esac
        ;;
    output)
        case "$arg" in html | stdout | json);; #ok, do nothing
        *) # unknown
            printHelp "Unknown argument for option --must:output : $arg" \
                        "                                           (valid: html | stdout | json)"
        esac
        ;;
    stacktrace)
        case "$arg" in
        none);; #ok, do nothing
        dyninst)
            if ! cmaketrue "@USE_CALLPATH@"
            then
                printHelp "Argument for option --must:stacktrace : $2 not supported with this build of MUST"
            fi
            ;; #ok, do nothing
        backward)
            if ! cmaketrue "@USE_BACKWARD@"
            then
                printHelp "Argument for option --must:stacktrace : $2 not supported with this build of MUST"
            fi
            ;; #ok, do nothing
        *) # unknown
            printHelp "Unknown argument for option --must:stacktrace : $2" \
                      "                                      (valid: none | dyninst | backward)"
        esac
        ;;
    esac
    echo $arg
}

#----------------- Function ----------------
#name: absolut_path(path-variable-name)
#purpose: makes a path absolut
#-------------------------------------------
absolut_path()
{
    local result=$1
    eval local val=\$$1
    if [[ "$val" != /* ]]
    then
        val=${_MUST_WDIR}/$val
    fi
    eval $result="'$val'"
}

#----------------- Function ----------------
#name: get_num_procs
#purpose: get the number of procs given on commandline (e.g.: -np [n])
#return: number of procs via stdout
#-------------------------------------------
get_num_procs()
{
    # Split the input if the "end-of-command-options" marker is present
    if [[ "$input" == *" -- "* ]]
    then
      local options_pre_marker="${input%% -- *}"
    else
      local options_pre_marker="$input"
    fi
    local switch
    for switch in $np_arg
    do
      local pTemp=`echo " ${options_pre_marker}" | sed -e "s/[[:space:]]${switch}[[:space:]]\([^[:space:]]\{1,\}\).*\|./\1/g"`

      if [ -n "$pTemp" ]
      then
          p=$pTemp
          switchToUse=$switch
          return 0
      fi
    done
    return 1
}

#----------------- Function ----------------
# name: get_checksum()
# purpose: get a checksum for run relevant arguments
# return: argument via stdout
#-------------------------------------------
get_checksum()
{
    if [ "x@MD5SUM_FOUND@" == "xTRUE" ]
    then
        md5sum="@MD5SUM@"
    else
        md5sum="cat"
    fi
    (
        # number of processes
        if ! get_num_procs
        then
            # error, not re-use build
            date
        else
            echo $p
        fi
        # explicit layout-file or layoutmode
        if [ -n "${layout_arg}" ];
        then
            cat ${layout_arg}
        else
            echo $layoutMode
        fi
        if [ -n "$preInstalled" ];
        then
            echo "Using prebuild $preInstalled"
        else
            echo "Using prebuild ${inst_arg}"
        fi
#        ls -l $inst_arg/modules/
        ls -l ${mustModulesDir}/
        ls -l @CMAKE_INSTALL_PREFIX@/modules/
        echo ${nodesize_arg} ${close_arg} ${fanin_arg} ${firstfanin_arg} ${do_dl} ${do_reproduce} ${do_ddt} ${do_capture} ${do_hybrid} ${do_tsan} ${do_rma} ${rma_mode_arg} ${do_rma_only} ${stacktrace_arg} ${do_typeart} ${do_openmp} ${do_user_cache}
        # environment variables
        echo $oldenv

        if [ -n "${wait_arg}" ]
        then
            echo 'wait';
        fi
    ) #| $md5sum
}

#----------------- Function ----------------
# name: get_prebuild_hash()
# purpose: identifies the prebuild with a hash
# return: the hash via stdout
#-------------------------------------------
get_prebuild_hash()
{
    local content=$(
	# explicit layout-file or layoutmode
        if [ -n "${layout_arg}" ]
        then
            cat ${layout_arg}
        else
            echo $layoutMode-layer${layoutNumLayers}
        fi
	if [ -n "${analyses_arg}" ]
	then
	    cat ${analyses_arg}
	fi
	# we pipe with cat into md5sum to prevent filepaths in its output
	cat @CMAKE_INSTALL_PREFIX@/bin/mustrun | md5sum -
	# sort the filenames in hope to achieve determinism
	local SPEC_DIR="@CMAKE_INSTALL_PREFIX@/specifications"
	ls ${SPEC_DIR} | grep '.*\.xml$' | LC_ALL=C sort | sed -e s@^@${SPEC_DIR}/@g | xargs cat | md5sum -

        echo ${do_dl} ${do_reproduce} ${do_ddt} ${do_capture} ${do_hybrid} ${do_tsan} ${do_rma} ${rma_mode_arg} ${do_rma_only} ${do_typeart} ${do_openmp}
        "@MPI_C_COMPILER@" -show
	  )
    local checksum=$(echo "$content" | md5sum | awk '{print $1}')
    echo -n ${checksum}
}

#----------------- Function ----------------
#name: get_config_from_layout
#purpose: scans the given layout for properties
#-------------------------------------------
get_config_from_layout()
{
    if [ -z "${do_hybrid+x}" ] && grep "thread_app" ${layout_arg} > /dev/null ; then
        do_hybrid=1
    fi
}

#----------------- Function ----------------
#name: gen_layout
#purpose: tests the given layout or generates a new one
#-------------------------------------------
gen_layout()
{
#####################################################################
## 2) What layout are we using (--must:layout)

if [ -n "${layout_arg}" ];
then
    absolut_path layout_arg
    layoutFile=$layout_arg
    printLayoutFile=$layout_arg
else
    layoutFile=${src_arg}/layout.xml
    printLayoutFile=$layoutFile
    mkdir -p ${src_arg}
    mkdir -p ${inst_arg}
    if ! get_num_procs
    then
        musterror "Could not determine number of MPI tasks, use \"--must:np <NP-SWITCH>\" to tip me."
        exit 1
    fi

    strategy="CStratThreadedUp" ## Strategy between layer 0 and 1
    strategyDefault="CStratThreadedUp" ## Strategy between other layers
    intraStrategy="CStratThreadedIntra"
    place="mpi_place"

    if [ $layoutMode -eq 1 ]
    then
      strategy="CStratSimpleUp"
      strategyDefault="CStratSimpleUp"
    fi

    if [ $do_distributed -eq 1 ]
    then
      if [ $do_dl -eq 1 ]
      then
        strategy="CStratPRecvUp"
        strategyDefault="CStratPRecvUp"
        intraStrategy="CStratPRecvIntra"
      fi
    fi
    if [ $do_rma -ne 0 ]
    then
      strategy="CStratSimpleUp"
      strategyDefault="CStratSimpleUp"
      intraStrategy="CStratIsendIntra"
    fi
    
    #Set whether we need a case for level 1 (for distributed layouts with more than one layer)
    layer1CaseLabel=999
    if [ $layoutNumLayers -gt 2 ] || [ $do_rma_only -ne 0 ]
    then
        layer1CaseLabel=1
    fi

    cat <<End-of-File > $printLayoutFile
<layout-specification>
  <levels>
End-of-File

    for (( i=0; i <= $[$layoutNumLayers-1]; i++))
    do
        place="mpi_place"
        if [ $i -eq 0 ]
        then
            place="mpi_app"
        fi
        if [ ${do_hybrid} -ne 0 ]
        then
            if [ $i -eq 0 ]
            then
                place="thread_app"
            fi
            if [ $i -eq 1 ]
            then
                place="thread_place"
            fi
        fi
        echo >> $printLayoutFile "<level order=\"$i\" size=\"${layoutLayerSizes[$i]}\" place-name=\"$place\"><analyses>"
        case $i in
# application level analyses
        0)
            #We have a "reproduce" errors mode in which we don't run checks, but rather read a previoud error log instead
            #So if we aren't doing the reproduce mode, we apply checks, and otherwise we apply the error reproducer
            if [ $do_reproduce -eq 0 ]
            then
                if [ $do_rma_only -eq 0 ]
                then
                    cat <<End-of-File >> $printLayoutFile
                        <analysis name="CollectUnmapped" group="MUST_CollectUnmapped"/>
                        <analysis name="DatatypePredefs" group="MUST_Resource"/>
                        <analysis name="CommPredefs" group="MUST_Resource"/>
                        <analysis name="FinalizeNotify" group="MUST_Base"/>
                        <analysis name="DCollectiveInitNotify" group="MUST_DistributedDeadlock"/>
                        <analysis name="OpSplitter" group="MUST_Deadlock"/>
                        <analysis name="WcUpdate" group="MUST_Deadlock"/>
                        <analysis name="IntegerChecks" group="MUST_BasicChecks"/>
                        <analysis name="BasicIntegrities" group="MUST_BasicChecks"/>
End-of-File
                else
                    cat <<End-of-File >> $printLayoutFile
                        <analysis name="DatatypePredefs" group="MUST_Resource"/>
                        <analysis name="CommPredefs" group="MUST_Resource"/>
End-of-File
                fi

                #If we have OpenMP support we add the ELP module
#                if cmaketrue "@OPENMP_FOUND@"
#                then
#                    echo >> $printLayoutFile "                    <analysis name=\"ThreadSanity\" group=\"MUST_ELPBasicChecks\"/>"
#                fi

                if cmaketrue "@ENABLE_TSAN@" && [ $do_tsan -ne 0 ]
                then
                    #echo >> $printLayoutFile "                    <analysis name=\"TSanMessages\" group=\"MUST_TSanMessages\"/>"
                    echo >> $printLayoutFile "                    <analysis name=\"MpiTSanAnnotations\" group=\"MUST_TSan\"/>"
                fi

                if [ $layoutMode -ne 2 ] && [ $do_rma_only -eq 0 ]
                then
                    cat <<End-of-File >> $printLayoutFile
                    <analysis name="OperationChecks" group="MUST_BasicChecks"/>
                    <analysis name="CommChecks" group="MUST_CommChecks"/>
                    <analysis name="RequestChecks" group="MUST_RequestChecks"/>
                    <analysis name="BufferChecks" group="MUST_BufferChecks"/>
                    <analysis name="DatatypeChecks" group="MUST_DatatypeChecks"/>
                    <analysis name="OverlapChecks" group="MUST_OverlapChecks"/>
                    <analysis name="GroupChecks" group="MUST_GroupChecks"/>
                    <analysis name="LeakChecks" group="MUST_LeakChecks"/>
                    <analysis name="MessageFileManager" group="MUST_Base"/>
End-of-File
                fi
                if [ $do_typeart -eq 1 ]
                then
                    echo >> $printLayoutFile "<analysis name=\"MpiTypeArt\" group=\"MUST_MpiTypeChecks\"/>"
                fi

                if [ $do_rma -ne 0 ]
                then
                    echo '<analysis name="InitTSanSyncClock" group="MUST_TSan_Annotations"/>' >> $printLayoutFile
                    if [ "$rma_mode_arg" == "shadow" ]
                    then
                        echo '<analysis name="AppThrAnn" group="MUST_OneSidedChecks"/>' >> $printLayoutFile
                    elif [ "$rma_mode_arg" == "isl" ]
                    then
                        echo '<analysis name="TSanInterceptor" group="MUST_TSanInterceptor"/>' >> $printLayoutFile
                    fi
                fi

                if [ $do_rma_only -eq 0 ]
                then
                cat <<End-of-File >> $printLayoutFile
                    <analysis name="RequestCondition" group="MUST_RequestCondition"/>
                    <analysis name="CollectiveCondition" group="MUST_CollectiveCondition"/>
                    <analysis name="OnlyOnRootCondition" group="MUST_OnlyOnRootCondition"/>
                    <analysis name="CompletionCondition" group="MUST_CompletionCondition"/>
End-of-File
                fi

                if cmaketrue "@OMPT_FOUND@" && [ ${do_openmp} -ne 0 ]
                then
                    cat <<EOF >> $printLayoutFile
                    <analysis name="OpenMPadapter"   group="OpenMP"/>
                    <analysis name="OpenMPsanity"    group="OpenMPsanity"/>
                    <analysis name="OpenMPlocks"     group="OpenMPlocks"/>
                    <analysis name="OpenMPnestlocks" group="OpenMPlocks"/>
EOF
                fi
            else
                #We do reproduce errors, so add the reproducer module
                echo >> $printLayoutFile "<analysis name=\"MessageReproducer\" group=\"MUST_Base\"/>"
            fi

            # Add DDT Logger if we run with DDT (irrespective of whether we reproduce or not)
            if [ $do_ddt -eq 1 ]
            then
                echo >> $printLayoutFile "<analysis name=\"MsgLoggerDdt\" group=\"MUST_Base\"/>"
            fi
        ;;
# first level analyses
         $layer1CaseLabel)
            if [ $do_reproduce -eq 0 ]
            then
                # Distributed layouts need either DP2PMatch or DWaitState (the latter drags in p2p-matching) on first non-application layer
                if [ $layoutMode -eq 2 ] || ! [ -n $do_reproduce ]
                then
                    cat <<End-of-File >> $printLayoutFile
                    <analysis name="OperationChecks" group="MUST_BasicChecks"/>
                    <analysis name="CommChecks" group="MUST_CommChecks"/>
                    <analysis name="RequestChecks" group="MUST_RequestChecks"/>
                    <analysis name="BufferChecks" group="MUST_BufferChecks"/>
                    <analysis name="DatatypeChecks" group="MUST_DatatypeChecks"/>
                    <analysis name="OverlapChecks" group="MUST_OverlapChecks"/>
                    <analysis name="GroupChecks" group="MUST_GroupChecks"/>
                    <analysis name="LeakChecks" group="MUST_LeakChecks"/>
End-of-File
                fi
                if [ $do_typeart -eq 1 ]
                then
                    echo >> $printLayoutFile "<analysis name=\"MpiTypeArt\" group=\"MUST_MpiTypeChecks\"/>"
                fi
                if [ $layoutMode -gt 3 ]
                then
                    if [ $do_dl -eq 1 ]
                    then
                    echo >> $printLayoutFile "<analysis name=\"DWaitState\" group=\"MUST_DistributedDeadlock\"/>"
                    else
                    echo >> $printLayoutFile "<analysis name=\"DP2PMatch\" group=\"MUST_DistributedDeadlock\"/>"
                    fi
                fi

                # MPI RMA race analysis
                if [ $do_rma -ne 0 ]
                then
                    echo '<analysis name="RMATrack" group="MUST_OneSidedChecks" />' >> $printLayoutFile
                    echo '<analysis name="VectorClockWrapper" group="MUST_VectorClock" />' >> $printLayoutFile
                    if [ "$rma_mode_arg" == "shadow" ]
                    then
                        echo '<analysis name="TSanMessages" group="MUST_TSanMessages"/>' >> $printLayoutFile
                        echo '<analysis name="TSanSyncClockRecorder" group="MUST_OneSidedChecks" />' >> $printLayoutFile
                        echo '<analysis name="OriginChecks" group="MUST_OneSidedChecks" />' >> $printLayoutFile
                        echo '<analysis name="TargetChecks" group="MUST_OneSidedChecks" />' >> $printLayoutFile
                    elif [ "$rma_mode_arg" == "isl" ]
                    then
                        echo '<analysis name="RaceChecksList" group="MUST_OneSidedChecks" />' >> $printLayoutFile
                    fi
                fi

                # Add stdout logger for MUST-RMA only mode on tool thread layer
                if [ $do_rma_only -ne 0 ]
                then
                    echo >> $printLayoutFile "<analysis name=\"MsgLoggerStdOut\" group=\"MUST_Base\"/>"
                fi

                if cmaketrue "@OMPT_FOUND@" && [ ${do_openmp} -ne 0 ]
                then
                    cat <<EOF >> $printLayoutFile
                    <analysis name="OpenMPbarriers"  group="OpenMPbarriers"/>
EOF
                fi
            fi #reproduce ?
         ;;
# root level analyses
        $[$layoutNumLayers-1])
            if [ $do_reproduce -eq 0 ] && [ $do_rma_only -eq 0 ]
            then
                if [ $layoutMode -lt 4 ]
                then
                    #centralized modes (1-3) use blocking state (DL + P2P + Coll)
                    echo >> $printLayoutFile "<analysis name=\"BlockingState\" group=\"MUST_Deadlock\"/>"
                else
                    #distributed modes (4-5) use distributed collective match
                    echo >> $printLayoutFile "<analysis name=\"DCollectiveMatchRoot\" group=\"MUST_DistributedDeadlock\"/>"

                    #If we do deadlock detection, we need the central wait state modules
                    if [ $do_dl -eq 1 ]
                    then
                        echo >> $printLayoutFile "<analysis name=\"DWaitStateCollMgr\" group=\"MUST_DistributedDeadlock\"/>"
                        echo >> $printLayoutFile "<analysis name=\"DWaitStateWfgMgr\" group=\"MUST_DistributedDeadlock\"/>"
                    fi

                    #we need to add our layer 1 modules if we only have one layer
                    if [ "x$layoutNumLayers" == "x2" ]
                    then
                        if [ $do_dl -eq 1 ]
                        then
                            echo >> $printLayoutFile "<analysis name=\"DWaitState\" group=\"MUST_DistributedDeadlock\"/>"
                        else
                            echo >> $printLayoutFile "<analysis name=\"DP2PMatch\" group=\"MUST_DistributedDeadlock\"/>"
                        fi
                    fi
                fi
            fi

            # Logger base selection: StdOut | DDT+Reproduce | HTML
            if [ "$output_arg" == "stdout" ]
            then
                echo >> $printLayoutFile "<analysis name=\"MsgLoggerStdOut\" group=\"MUST_Base\"/>"
            elif [ "$output_arg" == "json" ]
            then
                echo >> $printLayoutFile "<analysis name=\"MsgLoggerJson\" group=\"MUST_Base\"/>"
            elif [ $do_ddt -eq 1 ]
            then
                #For DDT, only add the logger modules if we aren't reproducing, otherwise the application layer DDT Logger module does the complete job
                if [ $do_reproduce -eq 0 ]
                then
                    echo >> $printLayoutFile "<analysis name=\"MsgLoggerDdt\" group=\"MUST_Base\"/>"
                    do_capture=1
                fi
            else
                echo >> $printLayoutFile "<analysis name=\"MsgLoggerHtml\" group=\"MUST_Base\"/>"
            fi

            # Logger addition: ScoreP Format
            if [ $do_scorep -eq 1 ]
            then
                # for --must:scorep insert the scorep logger:
                echo >> $printLayoutFile "<analysis name=\"MsgLoggerScoreP\" group=\"MUST_Base\"/>"
            fi

            # Logger addition: ScoreP Format
            if [ $do_capture -eq 1 ]
            then
                # for --must:scorep insert the scorep logger:
                echo >> $printLayoutFile "<analysis name=\"MsgLoggerReproduce\" group=\"MUST_Base\"/>"
            fi

        ;;
# other level analyses
#         *)
#         ;;
        esac
    echo >> $printLayoutFile "</analyses></level>"
    done
        cat <<End-of-File >> $printLayoutFile
  </levels>
  <communications>
    <default>
      <comm-strategy name="$strategyDefault">
        <settings></settings>
      </comm-strategy>
      <comm-protocol name="CProtMpiSplited">
        <settings></settings>
      </comm-protocol>
    </default>
    <connection top-level="1" bottom-level="0"
End-of-File
    if [ -n "${nodesize_arg}" ]
    then
        cat <<End-of-File >> $printLayoutFile
    distribution="by-block"
        blocksize="$[${nodesize_arg}-1]">
        <comm-strategy name="CStratIsendUp">
            <settings></settings>
          </comm-strategy>
          <comm-protocol name="CProtIpcSM">
            <settings></settings>
          </comm-protocol
End-of-File
    elif [ -n "${close_arg}" ]
    then
        cat <<End-of-File >> $printLayoutFile
    distribution="by-block"
        blocksize="${close_arg}">
        <comm-strategy name="$strategy">
            <settings></settings>
          </comm-strategy>
          <comm-protocol name="CProtMpiSplited">
            <settings></settings>
          </comm-protocol
End-of-File
    elif [ "${do_hybrid}" -ne 0 ]
    then
        cat <<End-of-File >> $printLayoutFile
>
<!--        <comm-strategy name="CStratIsendUp">-->
        <comm-strategy name="CStratSimpleUp">
            <settings></settings>
          </comm-strategy>
          <comm-protocol name="CProtSharedMemory">
            <settings></settings>
          </comm-protocol
End-of-File
    else
        cat <<End-of-File >> $printLayoutFile
>
        <comm-strategy name="$strategy">
            <settings></settings>
          </comm-strategy>
          <comm-protocol name="CProtMpiSplited">
            <settings></settings>
          </comm-protocol
End-of-File
    fi
    echo >> $printLayoutFile "></connection>"

    for (( i=2; i <= $[$layoutNumLayers-1]; i++ ))
    do
        echo >>$printLayoutFile "<connection top-level=\"$i\" bottom-level=\"$[$i-1]\"></connection>"
    done

    # Distributed modes need intra-layer connection on first non-application layer
    if [ $layoutMode -gt 3 ] || [ $do_rma -ne 0 ]
    then
        cat <<End-of-File >> $printLayoutFile
    <connection top-level="1" bottom-level="1">
        <comm-strategy name="$intraStrategy">
            <settings></settings>
        </comm-strategy>
        <comm-protocol name="CProtMpiSplited">
            <settings></settings>
        </comm-protocol>
    </connection>
End-of-File
    fi

    echo >> $printLayoutFile "</communications></layout-specification>"
fi
if [ $do_verbose -eq 1 ]
then
    mustecho "Using layout file: $printLayoutFile"
fi

layoutSize=0
for j in $(grep '\bsize' $printLayoutFile | grep -v thread_app | sed -e 's|.*size="\([^"]*\)".*|\1|')
do
        layoutSize=$[$layoutSize+$j]
done

cd ${_MUST_WDIR}

}

#----------------- Function ----------------
#name: save_checksum
#purpose: save the checksum for a successful run
#-------------------------------------------
save_checksum()
{
get_checksum > ${temp_arg}/lastrun
}

#----------------- Function ----------------
#name: run_weaver
# args: ($1=weaver binary)
#purpose: run the gti-weaver to analyse the input xml and configure the tool-chain
#-------------------------------------------
run_weaver()
{

OpenMPspec=
if cmaketrue "@OMPT_FOUND@"
then
    OpenMPspec="
        ${mustSpecDir}/OpenMP_api.xml
        ${mustSpecDir}/OpenMP_adapter.xml
        ${mustSpecDir}/OpenMP_sanity.xml
        ${mustSpecDir}/OpenMP_barriers.xml
        ${mustSpecDir}/OpenMP_locks.xml
    "
fi

#MPI_Finalize is in an extra specification, if we do not do distribtued deadlock detection,
#it is finalizing the tool, if we do distributed deadlock detection it is not finalizing the
#tool; In that case DWaitState will issue the tool finalization
mpiFinSpec=${mustSpecDir}/mpi_specification_finalize_fin.xml

if [ $do_distributed -eq 1 -a $do_reproduce -eq 0 ]
then
  if [ $do_dl -eq 1 ]
  then
    mpiFinSpec=${mustSpecDir}/mpi_specification_finalize_nofin.xml
  fi
fi

tsanspecs=""
if cmaketrue "@ENABLE_TSAN@"
then
    tsanspecs=$(for file in                   \
      ${mustSpecDir}/must_tsan.xml            \
      ${mustSpecDir}/must_tsan_messages.xml   \
      ${mustSpecDir}/mpi_tsan_annotations.xml
        do
            [[ -f $file ]] && echo $file || true
        done
    )
fi

tsan_interceptor_specs=${mustSpecDir}/must_tsan_interceptor.xml
tsan_interceptor_apis=${mustSpecDir}/must_tsan_interceptor_api.xml

optionalSpecApis=$(for file in               \
        ${mustSpecDir}/euro_mpi_2011_api.xml \
        ${mustSpecDir}/ipdps_2012_api.xml
    do
        [[ -f $file ]] && echo $file || true
    done
)

optionalSpecs=$(for file in                         \
        ${mustSpecDir}/coll_reduction_prototype.xml \
        ${mustSpecDir}/euro_mpi_2011.xml            \
        ${mustSpecDir}/ipdps_2012.xml
    do
        [[ -f $file ]] && echo $file || true
    done
)

#run the weaver
set +e

mpi_4_xml_file="${mustSpecDir}/mpi_4_specification.xml"
#mpi_4_xml_file=""
#_mpi_version_str="@MPI_C_VERSION@"
#if [[ ${_mpi_version_str%.*} -ge 4 ]]
#then
#    mpi_4_xml_file="${mustSpecDir}/mpi_4_specification.xml"
#fi

if ! $1 \
  $layoutFile \
  @GTI_SPECIFICATION@ \
  ${mustSpecDir}/mpi_specification.xml \
  $mpiFinSpec \
  ${mustSpecDir}/mpi_2_specification.xml \
  ${mustSpecDir}/mpi_3_specification_nbc.xml \
  ${mustSpecDir}/mpi_3_specification_rma.xml \
  ${mustSpecDir}/mpi_3_specification_unmapped.xml \
  $mpi_4_xml_file \
  ${mustSpecDir}/must_annotation_api_spec.xml \
  ${mustSpecDir}/must_base_api_spec.xml \
  ${mustSpecDir}/must_resource_track_api.xml \
  ${mustSpecDir}/must_deadlock_detection_api.xml \
  ${mustSpecDir}/request_condition_api.xml \
  ${mustSpecDir}/collective_condition_api.xml \
  ${mustSpecDir}/onlyOnRoot_condition_api.xml \
  ${mustSpecDir}/completion_condition_api.xml \
  ${mustSpecDir}/must_onesided_checks_api.xml \
  ${mustSpecDir}/must_distributed_deadlock_detection_api.xml \
  ${tsan_interceptor_apis} \
  $optionalSpecApis \
  $apis \
  ${mustSpecDir}/mpi_base_specification.xml \
  ${mustSpecDir}/must_base_specification.xml \
  ${mustSpecDir}/must_resource_track.xml \
  ${mustSpecDir}/must_basic_checks.xml \
  ${mustSpecDir}/must_buffer_checks.xml \
  ${mustSpecDir}/must_collect_unmapped.xml \
  ${mustSpecDir}/must_comm_checks.xml \
  ${mustSpecDir}/must_request_checks.xml \
  ${mustSpecDir}/must_datatype_checks.xml \
  ${mustSpecDir}/must_overlap_checks.xml \
  ${mustSpecDir}/must_typeart_checks.xml \
  ${mustSpecDir}/must_group_checks.xml \
  ${mustSpecDir}/must_leak_checks.xml \
  ${mustSpecDir}/request_condition.xml \
  ${mustSpecDir}/collective_condition.xml \
  ${mustSpecDir}/must_deadlock_detection.xml \
  ${mustSpecDir}/onlyOnRoot_condition.xml \
  ${mustSpecDir}/completion_condition.xml \
  ${mustSpecDir}/must_tsan.xml \
  ${mustSpecDir}/must_onesided_checks.xml \
  ${mustSpecDir}/must_distributed_deadlock_detection.xml \
  ${mustSpecDir}/must_vectorclock_wrapper.xml \
  $optionalSpecs \
  ${tsanspecs} \
  ${tsan_interceptor_specs} \
  ${omptspec} \
  ${OpenMPspec} \
  $analyses > output.out 2>output.err
  #TODO: add XMLS for further checks
then
  musterror "Running the weaver failed with the following output:"
  cat output.out
  cat output.err
  exit 1
else
  if [ $do_verbose -eq 1 ]
  then
    musterror "==> Running weaver:"
    cat output.out
    cat output.err
  else
    if [ $do_quiet -ne 1 ]
    then
      mustecho "Weaver ... success"
    fi
  fi
fi

}

#----------------- Function ----------------
#name: run_codegen
#purpose: generates the code for the given layout
#-------------------------------------------
run_codegen()
{
#####################################################################
## 3) Run the weaver and code generators

if [ $do_verbose -eq 1 ]
then
  mustecho "========= Step 2) Generation ========="
fi

#change to temp dir
mkdir -p $src_arg
cd $src_arg

#run the Generators
export PATH=@GTI_HOME@/bin:$PATH
must_exports="${must_exports} PATH"

run_weaver "$<TARGET_FILE_NAME:@GTI_WEAVER@>"

if ! @BASH@ weaver-run-generators.sh > output.out 2>output.err
then
  musterror "Code generation failed with the following output:"
  cat output.out
  cat output.err
  exit 1
fi

if [ $do_verbose -eq 1 ]
then
  mustecho "==> Generating source code:"
  cat output.out
  cat output.err
else
  if [ $do_quiet -ne 1 ]
  then
    mustecho "Code generation ... success"
  fi
fi

#run the build file generation


if ! "$<TARGET_FILE_NAME:@GTI_BUILDGEN@>" weaver-buildgen.xml > output.out 2>output.err
then
  musterror "Build file generation failed with the following output:"
  cat output.out
  cat output.err
  exit 1
fi

if [ $do_verbose -eq 1 ]
then
  mustecho "==> Generating build files:"
  cat output.out
  cat output.err
else
  if [ $do_quiet -ne 1 ]
  then
    mustecho "Build file generation ... success"
  fi
fi
set -e

#the cat is superfluous, but is used to remove a possible error exit from the grep, so do not remove it!

if errorgenenv=$(printenv | grep ERRORGEN_)
then
    errorgendefs=$(for i in $errorgenenv; do echo -n "-D$i " | sed 's/= / /'; done)
    cp @CMAKE_INSTALL_PREFIX@/src/error_generator.c .
    cat <<End-of-File >> CMakeLists.txt
    ADD_DEFINITIONS( $errorgendefs )
    GTI_MAC_ADD_MODULE (errorGen "error_generator.c;${BASE_SOURCES}" "C")

    ###TARGET_LINK_LIBRARIES (errorGen)
End-of-File

fi

#cp $src_arg/weaver-layout-info.xml $temp_arg/weaver-layout-info.xml
#cp $src_arg/weaver-mod-conf-input.xml $temp_arg/weaver-mod-conf-input.xml

cd ${_MUST_WDIR}

}

#----------------- Function ----------------
#name: build_modules
#purpose: compiles modules from the generated code
#-------------------------------------------
build_modules()
{
#####################################################################
## 4) Build the intermediate modules

cd $src_arg

if [ -d BUILD ]
then
  rm -rf BUILD
fi

mkdir BUILD
cd BUILD

set +e
## Configure with cmake
GTI_TOOL_INCLUDE_PATH=@CMAKE_INSTALL_PREFIX@/include
GTI_TOOL_LIB_PATH=@CMAKE_INSTALL_PREFIX@/lib
if [ $do_inbuilddir -eq 1 ]
then
  GTI_TOOL_INCLUDE_PATH="@CMAKE_BINARY_DIR@/include;@CMAKE_INSTALL_PREFIX@/include;$MUSTRUN_INCLUDE_PATH"
  GTI_TOOL_LIB_PATH="@CMAKE_BINARY_DIR@/lib;@CMAKE_INSTALL_PREFIX@/lib"
fi
assert_dir_is_locked ${inst_arg} "${BASH_SOURCE}:${LINENO}"
CC=@MUST_C_COMPILER@ CXX=@MUST_CXX_COMPILER@ FC=@MUST_Fortran_COMPILER@ LDFLAGS="@CMAKE_MODULE_LINKER_FLAGS@" "@CMAKE_COMMAND@" -G"Unix Makefiles" ../ -DCMAKE_LINKER=@CMAKE_LINKER@ -DCMAKE_SHARED_LINKER_FLAGS="@CMAKE_SHARED_LINKER_FLAGS@" -DCMAKE_SHARED_LINKER_FLAGS_DEBUG="@CMAKE_SHARED_LINKER_FLAGS_DEBUG@" -DCMAKE_SHARED_LINKER_FLAGS_RELEASE="@CMAKE_SHARED_LINKER_FLAGS_RELEASE@" -DCMAKE_C_FLAGS="@CMAKE_C_FLAGS@" -DCMAKE_C_FLAGS_DEBUG="@CMAKE_C_FLAGS_DEBUG@" -DCMAKE_C_FLAGS_RELEASE="@CMAKE_C_FLAGS_RELEASE@" -DCMAKE_CXX_FLAGS="@CMAKE_CXX_FLAGS@" -DCMAKE_CXX_FLAGS_DEBUG="@CMAKE_CXX_FLAGS_DEBUG@" -DCMAKE_CXX_FLAGS_RELEASE="@CMAKE_CXX_FLAGS_RELEASE@" -DCMAKE_BUILD_TYPE=@CMAKE_BUILD_TYPE@ -DCMAKE_INSTALL_PREFIX=$inst_arg -DGTI_TOOL_INCLUDE_PATH="${GTI_TOOL_INCLUDE_PATH}" -DGTI_TOOL_LIB_PATH="${GTI_TOOL_LIB_PATH}"> output.out 2>output.err

if [ $? -ne 0 ]
then
  musterror "Configuration of intermediate build failed with the following output:"
  cat output.out
  cat output.err
  exit 1
fi

if [ $do_verbose -eq 1 ]
then
  mustecho "==> Configuring intermediate build:"
  cat output.out
  cat output.err
else
  if [ $do_quiet -ne 1 ]
  then
    mustecho "Configuring intermediate build ... success"
  fi
fi

makej=8
if [ $makej -gt $layoutSize ]
then
    makej=$layoutSize
fi
if [ $do_inbuilddir -eq 1 ]
then
    makej=1
fi

## Make
if [ $do_verbose -ne 1 -a $do_quiet -ne 1 ]
then
    mustecho -n "Building intermediate sources ... "
fi


if ! make -j${makej} > output.out 2>output.err
then
  musterror "Building intermediate sources failed with the following output:"
  cat output.out
  cat output.err
  exit 1
fi

if [ $do_verbose -eq 1 ]
then
  musterror "==> Building intermediate sources:"
  cat output.out
  cat output.err
else
  if [ $do_quiet -ne 1 ]
  then
    mustecho "success"
  fi
fi

## Make install

if ! make -j${makej} install > output.out 2>output.err
then
  musterror "Installing intermediate modules failed with the following output:"
  cat output.out
  cat output.err
  exit 1
fi

if [ $do_verbose -eq 1 ]
then
  mustecho "==> Installing intermediate modules:"
  cat output.out
  cat output.err
else
  if [ $do_quiet -ne 1 ]
  then
    mustecho "Installing intermediate modules ... success"
  fi
fi

set -e
cd ${_MUST_WDIR}
}

#----------------- Function ----------------
#name: pnmpi_config
#purpose: generates a pnmpi.config for the given layout
#-------------------------------------------
pnmpi_config()
{
#####################################################################
## 5) Create the P^nMPI configuration file
cd $temp_arg

#if [ -z "$layoutSize" ]
#then
#    layoutSize=0
#    for j in $(grep '\bsize' $inst_arg/layout.xml | sed -e 's|.*size="\([^"]*\)".*|\1|')
#    do
#            layoutSize=$[$layoutSize+$j]
#    done
#fi

## Get information on the layers we had in the layout
numLayers=`grep "num-layers" $src_arg/weaver-layout-info.xml | sed -e s/.*num-layers=\"// -e s/[^0-9].*//`
totalSize=0

for (( i = 0 ; i < $numLayers ; i++ ))
do
  size[$i]=`grep "id=\"$i\"" $src_arg/weaver-layout-info.xml | sed -e s"/.*size=\"//" -e s"/[^0-9].*//"`
  to[$i]=`grep "id=\"$i\"" $src_arg/weaver-layout-info.xml | sed -e s"/[^>]*[>]//" -e s"/[<][/]layer[>]//" -e s"/[<]to[>]/ /" -e s"/[<][/]to[>]/ /"`
  mpi[$i]=$(grep "id=\"$i\"" $src_arg/weaver-layout-info.xml | grep -v thread_app | cat)

  if [ -n "${mpi[$i]}" ]; then
    totalSize=$(($totalSize+${size[$i]}))
  fi
done

if [ $totalSize -ne $layoutSize ]
then
  mustecho "totalSize != layoutSize ($totalSize != $layoutSize)"
  exit 1
fi


## Store the total size in an info file
echo "$totalSize" > mustrun.info

## Print the merge file
cat <<End-of-File > level_merge.xml
<level-merge>
<set>
End-of-File

for (( i = 0 ; i < $numLayers ; i++ ))
do
  echo -e "\t\t<order>$i</order>" >> level_merge.xml
done

cat <<End-of-File >> level_merge.xml
</set>
</level-merge>
End-of-File

## Run the module configurator
set +e

if ! "$<TARGET_FILE_NAME:@GTI_MODCONFGEN@>" $src_arg/weaver-mod-conf-input.xml level_merge.xml > output.out 2>output.err
then
  musterror "Generating P^nMPI configuration failed:"
  cat output.out
  cat output.err
  exit 1
fi

if [ $do_verbose -eq 1 ]
then
  mustecho "==> Generating P^nMPI configuration:"
  cat output.out
  cat output.err
else
  if [ $do_quiet -ne 1 ]
  then
    mustecho "Generating P^nMPI configuration ... success"
  fi
fi
set -e

echo -n "" > pnmpi.conf

cat <<EOF >> pnmpi.conf
globalpcontrol typed 4042 pointer
EOF


# Add configuration for PnMPI thread limiting module.

echo "module limit-threading" >> pnmpi.conf
if [ $do_hybrid -eq 1 ]
then
  # we need MPI-thread-multiple in case we execute hybrid:
  # Communication of the tools thread is not synchronized against the application
  echo "argument force-thread-level multiple" >> pnmpi.conf
else
  # with non-hybrid execution we support not more then MPI-thread-funneled:
  # Only the master thread has knowledge about MPI opaque data
  echo "argument thread-level funneled" >> pnmpi.conf
fi

if [ $do_printmem -eq 1 ]
then
  echo "module libXPrintMemUsage" >> pnmpi.conf
fi

if [ $do_printmem -eq 1 ]
then
  echo "module libXPrintMemUsage" >> pnmpi.conf
fi

##Add configuration for split module to the beginning of the config file
if [ $stacktrace_arg == "dyninst" ]
then
  cat <<End-of-File >> pnmpi.conf
module libcallpathModule
End-of-File
else
      echo -n "" >> pnmpi.conf
fi

  if cmaketrue "@HAVE_MPI_STATUS_IGNORE@" || cmaketrue "@HAVE_MPI_STATUSES_IGNORE@"
  then
      cat <<End-of-File >> pnmpi.conf
module libstatusWrapper
End-of-File
  else
      echo -n "" >> pnmpi.conf
  fi

  if [ "$errorgenenv" ]
then
cat <<End-of-File >> pnmpi.conf
module liberrorGen
End-of-File
fi

cat <<End-of-File >> pnmpi.conf
module libcProtMpiSplitComm
argument num_sets $numLayers
$nodesizearg
End-of-File

confOut="weaver-mod-conf"
for (( i = 0 ; i < $numLayers ; i++ ))
do
  echo "argument size_$i ${size[$i]}" >> pnmpi.conf
  echo "argument stack_$i level_$i" >> pnmpi.conf
  if [ -n "${mpi[$i]}" ]; then
    echo "argument place_$i mpi_place" >> pnmpi.conf
  else
    echo "argument place_$i thread_place" >> pnmpi.conf
  fi

  confOut=$confOut.$i
done

index=0
commId=1
echo -n > pnmpi.conf_temp
for (( i = 0 ; i < $numLayers ; i++ ))
do
  for t in ${to[$i]}
  do
    echo "argument mapping$(($index)) $i:$commId:$t" >> pnmpi.conf_temp
    echo "argument mapping$(($index+1)) $t:$commId:$i" >> pnmpi.conf_temp
    index=$(($index+2))
    commId=$(($commId+1))
  done
done

echo "argument num_mappings $index" >> pnmpi.conf
cat < pnmpi.conf_temp >> pnmpi.conf
echo "" >> pnmpi.conf

if [ $do_hybrid -ne 0 ]
then
  echo "module libthreadedAppStartup" >>pnmpi.conf
fi

cat < $confOut >>pnmpi.conf

# append the renaming of MPI_COMM_WORLD to end of stack level 0
cat pnmpi.conf | sed -e 's/stack[[:space:]]level_0/module libcProtMpiCommRewrite\
stack level_0/' > pnmpi.conf2
mv pnmpi.conf2 pnmpi.conf

if [ $stacktrace_arg == "dyninst" ]
then
  cat pnmpi.conf | sed -e 's/module libinitLocationId/module libinitLocationIdCallpath/' > pnmpi.conf2
  mv pnmpi.conf2 pnmpi.conf
elif [ $stacktrace_arg == "backward" ]
then
  cat pnmpi.conf | sed -e 's/module libinitLocationId/module libinitLocationIdBackward/' > pnmpi.conf2
  mv pnmpi.conf2 pnmpi.conf
fi

#TODO disabled for the moment, needs further testing
#if [ $do_scorep -eq 1 ]
#then
#    # for --must:scorep insert the scorep_adapter_mpi at begin of stack 0:
## Version Frank had localy
#    cat pnmpi.conf | sed -e 's/stack[[:space:]]level_0/stack level_0\nmodule libscorep_adapter_mpi_patch\nmodule libscorep_mpp_mpi_patch/'  > pnmpi.conf2
## Original version
#    cat pnmpi.conf | sed -e 's/stack[[:space:]]level_0/stack level_0\
#module libscorep_adapter_mpi/'  > pnmpi.conf2
#    mv pnmpi.conf2 pnmpi.conf
#fi

if [ $do_isp -eq 1 ]
then
    # for --must:isp insert the isp_prof module at begin of stack 0:
    cat pnmpi.conf | sed -e 's/stack[[:space:]]level_0/stack level_0\
module libisp_prof/'  > pnmpi.conf2
    #                                           at end of stack 0:
#     cat pnmpi.conf | sed -e 's/stack[[:space:]]level_1/module libisp_prof\
#stack level_1/'  > pnmpi.conf2
    mv pnmpi.conf2 pnmpi.conf
fi

if [ $do_hybrid -ne 0 ]
then
    sed -i'' -e $'s/stack[[:space:]]level_0/module libthreadedAppStartup\\\nstack level_0/' pnmpi.conf
fi

if [ -n "${wait_arg}" ]
then
    sed -i'' -e $'s/stack[[:space:]]level_1/module wait-for-debugger\\\nstack level_1/' pnmpi.conf
fi

cat pnmpi.conf | sed -e 's/module libweaver-wrapp-gen-output-0/module libweaver-wrapp-gen-output-0\npcontrol on/' > pnmpi.conf2
mv pnmpi.conf2 pnmpi.conf


cd ${_MUST_WDIR}
}


#####################################################################
## Prepare the must environment
#####################################################################
setup()
{

#####################################################################
## 1) Prepare a temp directory

if [ -d $temp_arg ]
then
    if [ ${do_quiet} -ne 1 ]
    then
        mustecho "Information: overwritting old intermediate data in directory \""$temp_arg"\"!"
    fi
    # no re-run for broken prepare
    rm -f ${temp_arg}/lastrun
else
    mkdir -p $temp_arg
fi

#Print result
if [ ${do_verbose} -eq 1 ]
then
    mustecho "Using intermediate directory \""$temp_arg"\"."
fi

#####################################################################
## 2) What layout are we using (--must:layout)
#Set runOnBackend Variable
export runOnBackend=
if [ $mode_arg == "preparerun" ]
then
  runOnBackend="$mpiexec_arg $switchToUse 1"
fi

if [ $mode_arg == "autorun" -o $mode_arg == "prepareautorun" ] && [ -n "$preInstalled" ]
then
    if [ -d $preInstalled ]
    then
        if [ ${mode_arg} == "prepareautorun" ]
        then
            mode_arg=configpreinstalled
    	else
            mode_arg=preinstalled
        fi
        inst_arg=$preInstalled
        if [ $do_quiet -ne 1 ]
        then
            mustecho "Using prebuilt infrastructure at ${inst_arg}"
        fi
    fi
fi

gen_layout

if [ $mode_arg == "configpreinstalled" ]
then
    rm -f $src_arg/weaver-layout-info.xml $src_arg/weaver-mod-conf-input.xml
fi

if [ $mode_arg == "config" -o $mode_arg == "preinstalled" -o $mode_arg == "configpreinstalled" -o -n "$layout_arg" ]
then
  if [ ! -f $src_arg/weaver-layout-info.xml -o ! -f $src_arg/weaver-mod-conf-input.xml ]
  then
    mkdir -p $src_arg
    cd $src_arg
    run_weaver "$<TARGET_FILE_NAME:@GTI_WEAVER@-no-build>"
  fi
fi
} # end setup()



#####################################################################
## Prepare the must environment
#####################################################################
build()
{

#####################################################################
## 3) Run the weaver and code generators
run_codegen

#####################################################################
## 4) Build the intermediate modules
build_modules

} # end build()

#####################################################################
## Configure the must environment
#####################################################################
config()
{

#####################################################################
## 5) Create the P^nMPI configuration file
pnmpi_config

#####################################################################
## 6) Save checksum of current run
save_checksum

} # end config()


#----------------- Function ----------------
#name: calclayers
#purpose: info mode
#-------------------------------------------
calclayers()
{
preInstalled=
if [ -n "${layout_arg}" ]
then
    layoutTotalSize=0
    layoutNumLayers=$(grep '\bsize' $layout_arg | wc -l)
    i=0
    for j in $(grep '\bsize' $layout_arg | sed -e 's|.*size="\([^"]*\)".*|\1|')
    do
            layoutTotalSize=$[$layoutTotalSize+$j]
            layoutLayerSizes[$i]=$j
            i=$[$i+1]
    done

    # Set number of application processes according to passed layout
    local appProcs
    if [ "$do_hybrid" -eq 0 ]
    then
        appProcs=${layoutLayerSizes[0]} # mpi_app
    else
        appProcs=${layoutLayerSizes[1]} # thread_place
        layoutTotalSize=$[${layoutTotalSize}-${layoutLayerSizes[0]}] # subtract thread_app
    fi

    # Sanity check: If there is -n / -np, it should be equal to the derived number of MPI processes
    if get_num_procs && [ $p -ne $appProcs ]
    then
        musterror "Number of passed MPI tasks ($p) is not equal to number of MPI tasks derived from layout ($appProcs)."
        exit 1
    fi

    p=$appProcs
else
    #If "fanin" is set, automatically set distributed!
    if [ "x$fanin_given" == "x1" ]
    then
        do_distributed=1;
    fi

    layoutNumLayers=1
    if ! get_num_procs
    then
        musterror "Could not determine number of MPI tasks, use \"--must:np <NP-SWITCH>\" to tip me."
        exit 1
    fi
    layoutLayerSizes[0]=$p
    layoutTotalSize=$p
    local threads=1
    if [ $do_distributed -ne 1 ]
    then
        if [ -n "$nodesize_arg" ]
        then
            # Mode 2
            layoutMode=2
            layoutLayerSizes[1]=$[($p-1)/($nodesize_arg-1)+1]
            layoutLayerSizes[2]=1
            layoutTotalSize=$[$layoutTotalSize+${layoutLayerSizes[1]}+1]
            layoutNumLayers=3
        elif [ -n "$close_arg" ]
        then
            # Mode 2
            layoutMode=1
            if [ "x$do_nocrash" == "x1" ]
            then
                layoutMode=3
            fi
            layoutLayerSizes[1]=$[($p-1)/($close_arg)+1]
            layoutLayerSizes[2]=1
            layoutTotalSize=$[$layoutTotalSize+${layoutLayerSizes[1]}+1]
            layoutNumLayers=3
        elif [ "$do_hybrid" -ne 0 ]
        then
            layoutMode=1
            threads=512
            if [ "x$do_nocrash" == "x1" ]
            then
                layoutMode=3
            fi
            # Mode 1+3
            layoutLayerSizes[0]=$[$p*$threads]
            layoutLayerSizes[1]=$p
            layoutLayerSizes[2]=1
            layoutTotalSize=$[$layoutTotalSize+1]
            layoutNumLayers=3
        else
            layoutMode=1
            if [ "x$do_nocrash" == "x1" ]
            then
                layoutMode=3
            fi
            # Mode 1+3
            layoutLayerSizes[1]=1
            layoutTotalSize=$[$layoutTotalSize+1]
            layoutNumLayers=2
            fanin_arg=$p
        fi
    else
        layoutMode=5
        firstLayer=1
        if [ "$do_hybrid" -ne 0 ]
        then
            threads=$(min 512 $firstfanin_arg $fanin_arg)
            layoutLayerSizes[0]=$[$p*$threads]
            layoutLayerSizes[1]=$p
            layoutNumLayers=2
        fi
        if [ -n "$nodesize_arg" ]
        then
            # Extra layer for mode 4
            layoutMode=4
            layoutLayerSizes[$layoutNumLayers]=$[($p-1)/($nodesize_arg-1)+1]
            layoutTotalSize=$[$layoutTotalSize+${layoutLayerSizes[$layoutNumLayers]}]
            layoutNumLayers=$[$layoutNumLayers+1]
            firstLayer=0
        fi
        if [ -n "$close_arg" ]
        then
            layoutLayerSizes[$layoutNumLayers]=$[($p-1)/($close_arg)+1]
            layoutTotalSize=$[$layoutTotalSize+${layoutLayerSizes[$layoutNumLayers]}]
            layoutNumLayers=$[$layoutNumLayers+1]
            firstLayer=0
        fi
        # Mode 4+5
        while [ ${layoutLayerSizes[$[$layoutNumLayers-1]]} -gt 1 ]
        do
            useFanIn=$fanin_arg
            if [ "x$firstLayer" == "x1" ]
            then
              useFanIn=$firstfanin_arg
            fi
            firstLayer=0
            layoutLayerSizes[$layoutNumLayers]=$[(${layoutLayerSizes[$[$layoutNumLayers-1]]}-1)/$useFanIn+1]
            layoutTotalSize=$[$layoutTotalSize+${layoutLayerSizes[$layoutNumLayers]}]
            layoutNumLayers=$[$layoutNumLayers+1]
        done

        # IF we do distributed deadlock detection:
        ## we currently have bug in shutdown that requires us to not run DWaitState on the root, so we add an extra layer if needed
        if [ $do_dl -eq 1 ]
        then
            if [ $layoutNumLayers -eq 2 ]
            then
                layoutLayerSizes[$layoutNumLayers]=1
                layoutTotalSize=$[$layoutTotalSize+${layoutLayerSizes[$layoutNumLayers]}]
                layoutNumLayers=$[$layoutNumLayers+1]
            fi
        fi
    fi
#    maxFanin=$(( $firstfanin_arg > $fanin_arg ? ( $nodesize_arg-1 > $firstfanin_arg ? $nodesize_arg-1 : $firstfanin_arg ) : ( $nodesize_arg-1 > $fanin_arg ? $nodesize_arg-1 : $fanin_arg ) ))
    maxFanin=$(max $firstfanin_arg $fanin_arg $(($nodesize_arg-1)) $close_arg $threads )
    if [ $maxFanin -le $(( 2 ** ( 31 / $layoutNumLayers ) - 2 )) ]
    then
        if [ $do_hybrid -eq 1 ]
        then
	    preInstalled=mode${layoutMode}-layer${layoutNumLayers}-hybrid
        else
	    preInstalled=mode${layoutMode}-layer${layoutNumLayers}
        fi
    fi

    if [ $do_rma_only -ne 0 ]
    then
        # RMA performance measurement, use only two layers (thread_app and thread_place), cut all other layers
        layoutNumLayers=2
        layoutTotalSize=$[${layoutLayerSizes[0]}+${layoutLayerSizes[1]}]
    fi
fi

if [ $do_noprebuilt -eq 1 -o $do_reproduce -eq 1 -o $do_ddt -eq 1 -o $do_scorep -eq 1 -o $do_capture -eq 1 ]
then
    # with --must:noprebuild, --must:reproduce, --must:capture, --must:ddt, --must:scorep, we don't use preinstalled communication
    preInstalled=
else
    # lookup for matching prebuild using the hash
    local PREBUILD_PATH="$(get_prebuild_path)"
    local preinstalled_hash="$(get_prebuild_hash)"
    local OLDIFS="${IFS}"
    IFS=":"
    for dir in ${PREBUILD_PATH}
    do
      if [ -d "${dir}/${preinstalled_hash}/modules" ]
      then
  	    preInstalled="${dir}/${preinstalled_hash}"
  	    break
      fi
    done
    IFS="${OLDIFS}"
fi
#if [ -n "${preInstalled}" ] && { cmaketrue "@REDUCED_PREBUILDS@"; } && [ $layoutNumLayers -gt 3 ] ;
#then
#    # with reduced prebuilds we only generate Layouts for 3 layers
#    preInstalled=
#fi


}

#----------------- Function ----------------
#name: fillnodes
#purpose: to be called after calclayers if
#         all used nodes should be filled
#-------------------------------------------
fillnodes()
{
    #Is the mode fitting?
    if [ "$layoutMode" -ne "4" ]
    then
        musterror "The --must:fillnodes option requires that --must:nodesize and --must:distributed (or --must:fanin is specified)."
        exit 1
    fi

    numNodes=$[($layoutTotalSize-1)/($nodesize_arg)+1]
    openSlots=$[$numNodes*$nodesize_arg-$layoutTotalSize]

    #We must not change #app procs
    minlayer=1

    #We must not change #1-layer procs if we do app crash handling
    if [ -n "$nodesize_arg" ]
    then
        minlayer=2
    fi

    ## First try to fill remaining slots to existing layers (without exceeding the set fan-in)
    redo=1
    while [ $redo -gt 0 ]
    do
        redo=0
        for (( i=$layoutNumLayers-1; i > $minlayer; i-- ))
        do
            useFanIn=$fanin_arg
                       if [ "x$i" == "x2" ]
                       then
                             useFanIn=$firstfanin_arg
                       fi

            if [ $[${layoutLayerSizes[$i]}*$useFanIn] -gt ${layoutLayerSizes[$i-1]} ]
            then
                if [ ${layoutLayerSizes[$i-2]} -gt ${layoutLayerSizes[$i-1]} ]
                then
                    if [ $openSlots -gt 0 ]
                    then
                        layoutLayerSizes[i-1]=$[${layoutLayerSizes[$i-1]}+1]
                        openSlots=$[$openSlots-1]
                        layoutTotalSize=$[$layoutTotalSize+1]
                        redo=1
                    fi
                fi
            fi
        done
    done

    ## If we couldn't distribute all slots yet, add extra layers of one process each
    while [ $openSlots -gt 0 ]
    do
        layoutLayerSizes[$layoutNumLayers]=1
        layoutNumLayers=$[$layoutNumLayers+1]
        openSlots=$[$openSlots-1]
        layoutTotalSize=$[$layoutTotalSize+1]
    done
}

#----------------- Function ----------------
#name: print_info
#purpose: print info
#-------------------------------------------
print_info()
{
    mustecho -n "MUST configuration ... "

    if [ -n "${layout_arg}" ]
    then
        if [ $do_verbose -eq 1 -o $do_info -eq 1 ]
        then
             mustecho " with given layout: ${layout_arg}"
        else
             mustecho " with given layout.xml"
        fi
    else
        if [ $layoutMode -lt 4 ]
        then
            mustecho -n "centralized checks"
        else
            mustecho -n "distributed checks"
        fi

        case $layoutMode in
            1)
                mustecho " with fall-back application crash handling (very slow)"
            ;;
            2)
                mustecho " with application crash handling"
            ;;
            3)
                mustecho " without application crash handling"
            ;;
            4)
                mustecho " with application crash handling"
            ;;
            5)
                mustecho " without application crash handling"
            ;;
        esac
    fi

    if [ $do_info -eq 1 ]
    then
        mustecho "Required total number of processes ... $layoutTotalSize"
        if [ $do_hybrid -eq 1 ]
        then
            mustecho "Number of application processes ... ${layoutLayerSizes[1]}"
            mustecho "Number of tool processes ... $[$layoutTotalSize - ${layoutLayerSizes[1]}]"
        else
            mustecho "Number of application processes ... ${layoutLayerSizes[0]}"
            mustecho "Number of tool processes ... $[$layoutTotalSize - ${layoutLayerSizes[0]}]"
        fi

        if [ -n "$nodesize_arg" ]
        then
            mustecho "Total number of required nodes or node partitions ... $[($layoutTotalSize-1)/($nodesize_arg)+1]"
        fi

        mustecho -n "Tool layers sizes ... "
        for (( i=0; i < $layoutNumLayers; i++ ))
        do
            if [ $i -gt 0 ]
            then
                mustecho -n ":"
                mustecho -n "${layoutLayerSizes[i]}"
            else
                if [ $do_hybrid -eq 1 ]
                then
                    mustecho -n "(${layoutLayerSizes[i]})"
                else
                    mustecho -n "${layoutLayerSizes[i]}"
                fi
            fi
        done
        mustecho ""
    if [ -n "$preInstalled" -a -d $preInstalled ]
    then
            mustecho "Using prebuilt infrastructure at $preInstalled"
    else
            mustecho "No prebuilt infrastructure available for the applied settings. Use \"--must:mode prepare\" to prepare the tool for batch system use."
    fi
    fi
}

#----------------- Function ----------------
#name: get_executable(string)
#purpose: find the executable in an argument string
#-------------------------------------------
get_executable()
{
local i
for i in $1;
do
    if [[ $i != -* && -f $i && -x $i ]]
    then
        echo $i
        return 0
    fi
done
return 1
}

#----------------- Function ----------------
#name: run
#purpose: start the run mode
#-------------------------------------------
run()
{

save_checksum

mpiexec_command=${mpiexec_arg}

if [ ! -d $temp_arg ]
then
    musterror "temp directory not found, run prepare mode first!"
    exit 1
fi

if [ -f $temp_arg/mustrun.info ]
then
  totalSize=`cat $temp_arg/mustrun.info`
else
  musterror "Error: the \"mustrun.info\" file could not be found in the temp directory \"$temp_arg\", did you really prepare with this temporary directory?"
  exit 1
fi

if ! executable=$(get_executable "$input" )
then
  musterror "Missing program to execute (parameter COMMAND, see '$0 --help' for more information)"
  exit 1
fi

if [ -z "$switchToUse" ] && ! get_num_procs
then
    if [ "@MPIEXEC_NUMPROC_FLAG@" ]
    then
        switchToUse="@MPIEXEC_NUMPROC_FLAG@"
    else
        switchToUse="-np"
    fi
fi
#####################################################################
## 1) Which programming language the program is using (no --must:language given)

myNM=@CMAKE_NM@
if [ -z "$language_arg" ]
then
    if [ -n "$myNM" ] && myNM=$(which $myNM 2> /dev/null) || myNM=$(which nm 2> /dev/null)
    then
        if test -z "$($myNM $executable 2>/dev/null | grep MAIN__)"
        then
            language_arg="c"
        else
            language_arg="fortran"
        fi
    fi
fi

# #####################################################################
# ## 2) Do we want to ld_preload the pnmpi-lib?
myldd=@LDD@
myotool=@OTOOL@
havepnmpi=0

if [ $do_pnmpi_linked -ne 1 ]
then
    if [ $do_quiet -ne 1 ]
    then
        mustecho -n "Search for linked P^nMPI ... "
    fi
    if myldd=$(which $myldd)
    then
        if test -z "$($myldd $executable | grep pnmpi)"
        then
            if [ $do_quiet -ne 1 ]
            then
                mustecho -n "not found ... "
            fi
        else
            havepnmpi=1
            if [ $do_quiet -ne 1 ]
            then
                mustecho -n "found ... "
            fi
        fi
    elif myotool=$(which $myotool)
    then
        if test -z "$($myotool -L $executable | grep pnmpi)"
        then
            if [ $do_quiet -ne 1 ]
            then
                mustecho -n "not found ... "
            fi
        else
            havepnmpi=1
            if [ $do_quiet -ne 1 ]
            then
                mustecho -n "found ... "
            fi
        fi
    fi
else
    havepnmpi=1
fi

#additional options for valgrind
massifopt=""
memcheckopt=""
#memcheckopt="--log-file=valgrind/memcheck.out.%p --leak-check=full --track-origins=yes"

#export LD_PRELOAD=@GTI_HOME@/lib/libGtiTLS.so

# insert pnmpize if needed
if test ${havepnmpi} -eq 0
then
    temp=""
    found=0
    for i in $input;
    do
        # already found the executable?
        if test $found -eq 0
        then
            if [ "$i" -ef "$executable" ]
            then
                absolut_path executable
                if [ $do_massif -eq 1 ]
                then
                    # use 'valgrind' with massif tool
                    executable="valgrind --tool=massif $massifopt $executable"
                fi
                if [ $do_memcheck -eq 1 ]
                then
                    # use 'valgrind' with memcheck tool
                    executable="valgrind --tool=memcheck $memcheckopt $executable"
                fi
                if [ $do_ddt -eq 1 ]
                then
                    # use ddt-client
                    if ! executable="$(which $precmdddt_arg) $executable"
                    then
                        musterror "$precmdddt_arg not found in PATH, please load ddt environment"
                        exit 1
                    fi
                fi

                if [ $language_arg == "fortran" ]
                then
                    temp="${temp} $<TARGET_FILE_NAME:@PnMPI_PnMPIze@> --fortran $executable"
                else
                    temp="${temp} $<TARGET_FILE_NAME:@PnMPI_PnMPIze@> $executable"
                fi

                found=1
                if [ $do_quiet -ne 1 ]
                then
                    mustecho -n "using LD_PRELOAD to load P^nMPI ... "
                fi
                continue
            fi
        fi
        temp="${temp} $i"
    done
    input=${temp}
else
    if [ $do_massif -eq 1 ]
    then
        # use 'valgrind' with massif tool
        mpiexec_command="$mpiexec_command valgrind --tool=massif $massifopt"
    fi
    if [ $do_memcheck -eq 1 ]
    then
        # use 'valgrind' with memcheck tool
        mpiexec_command="$mpiexec_command valgrind --tool=memcheck $memcheckopt"
    fi
    if [ $do_ddt -eq 1 ]
    then
        # use ddt-client
        if ! executable="$(which $precmdddt_arg) $executable"
        then
            musterror "$precmdddt_arg not found in PATH, please load ddt environment"
            exit 1
        fi
    fi
fi

if [ $do_pnmpi_linked -ne 1 -a $do_quiet -ne 1 ]
then
    mustecho "success"
fi


######################################################################
## 3) Set TSAN_OPTIONS

# copy suppressions file to must_temp directory
tsan_suppressions_file="${temp_arg}/tsan_suppressions.txt"
cp "@CMAKE_INSTALL_PREFIX@/share/tsan_suppressions.txt" "$tsan_suppressions_file"

# default TSAN_OPTIONS key-value pairs
tsan_option_ignore_noninstrumented_modules=1
tsan_option_exitcode=0
tsan_option_log_path=stdout
tsan_option_suppressions="$tsan_suppressions_file"

# options set by user take precedence over our defaults
while IFS='\n' read -r line; do
    opt=${line%%=*}
    arg=${line#*=}
    case $opt in suppressions | ignore_noninstrumented_modules | exitcode | log_path)
        # special case: external suppression file will be merged with our own
        if [[ "$opt" == "suppressions" ]]; then
            if [[ -f "$arg" ]]; then
                # append contents of external suppressions file to our suppressions
                cat "$arg" >> "$tsan_suppressions_file"
            fi
        else
            # overwrite our default with user value
            case "$arg" in
                *\ *)
                    # escape arguments with spaces
                    eval "tsan_option_${opt}=\"'${arg}'\""
                    ;;
                *)
                    eval "tsan_option_${opt}=\"${arg}\""
                    ;;
            esac
        fi

        # remove option set by user
        TSAN_OPTIONS=$(echo $TSAN_OPTIONS | sed -E "s/${opt}=('(.*?)'|\"(.*?)\"|[^\"']\S*)\s+?//g")
        ;;
    esac
done <<< "$(echo $TSAN_OPTIONS | xargs -n 1)"

export TSAN_OPTIONS="$TSAN_OPTIONS ignore_noninstrumented_modules=$tsan_option_ignore_noninstrumented_modules \
                                   exitcode=$tsan_option_exitcode \
                                   suppressions=$tsan_option_suppressions \
                                   log_path=$tsan_option_log_path"
must_exports="${must_exports} TSAN_OPTIONS"



#####################################################################
## 4) Run (finally :D)

if cmaketrue "@MUST_SANITIZE_UBSAN@" && [ $do_tsan -eq 0 ]
then
    LD_PRELOAD="@UBSAN_LIBRARIES@:${LD_PRELOAD}"
    export LSAN_OPTIONS="external_symbolizer_path=/usr/bin/llvm-symbolizer:suppressions=@CMAKE_CURRENT_SOURCE_DIR@/LSan.supp:${LSAN_OPTIONS}"
    must_exports="${must_exports} LD_PRELOAD LSAN_OPTIONS"
fi

if cmaketrue "@MUST_SANITIZE_ASAN@" && [ $do_tsan -eq 0 ]
then
    LD_PRELOAD="@ASAN_LIBRARIES@:${LD_PRELOAD}"
    export ASAN_OPTIONS="detect_leaks=0:${ASAN_OPTIONS}"
    export LSAN_OPTIONS="external_symbolizer_path=/usr/bin/llvm-symbolizer:suppressions=@CMAKE_CURRENT_SOURCE_DIR@/LSan.supp:${LSAN_OPTIONS}"
    must_exports="${must_exports} LD_PRELOAD LSAN_OPTIONS"
fi

if [ $do_timing -eq 1 ]
then
    # use 'time' to get the runtime of mpiexec
    mpiexec_command="time $mpiexec_command"
fi

#TODO: add cmake magic to only export for MPICH and derivatives
export MPIR_CVAR_COREDUMP_ON_ABORT=1
must_exports="${must_exports} MPIR_CVAR_COREDUMP_ON_ABORT"


export THIS_RUN_SEED=$$
must_exports="${must_exports} THIS_RUN_SEED"

output_dir_arg=${output_dir_arg:=${_MUST_WDIR}}
output_dir_arg="$(readlink -f ${output_dir_arg})" # expand path to absolute path (for example "." for current dir)

if [ -n "${output_email_report_arg}" ]
then
    export MUST_OUTPUT_EMAIL_REPORT="${output_email_report_arg}"
    must_exports="${must_exports} MUST_OUTPUT_EMAIL_REPORT"
    do_unique=1
fi

if [ $do_unique -eq 1 ]
then
    output_dir_arg="${output_dir_arg}/MUST-Output-$(date +%Y.%m.%d-%H.%M.%S)"
fi

if [ -n "${output_dir_arg}" ]
then
  export MUST_OUTPUT_PATH="${output_dir_arg}"
  must_exports="${must_exports} MUST_OUTPUT_PATH"
fi

if [ -z "${errorcode_arg}" ]
then
    must_returncode_file="${temp_arg}/return${THIS_RUN_SEED}"
    export MUST_RETURNCODE_FILE="${must_returncode_file}"
    must_exports="${must_exports} MUST_RETURNCODE_FILE"
fi

if [ -n "${filter_file_arg}" ]
then
  export MUST_FILTER_FILE="${filter_file_arg}"
  must_exports="${must_exports} MUST_FILTER_FILE"
fi

if [ $do_partitioned_verbose_report -ne 0 ]
then
    export MUST_PARTITIONED_VERBOSE_REPORT=1
    must_exports="${must_exports} MUST_PARTITIONED_VERBOSE_REPORT"
fi

if [ $do_tsan -ne 0 ]
then
    mustecho -n "Note: Full ThreadSanitizer support requires the application to be built with either "
    mustecho -n "a GNU-based compiler in version 9 to 11 or "
    mustecho "an LLVM-based compiler in version 6 to 13."
fi

if [ $do_rma -ne 0 ]
then
    if [ $rma_mode_arg == "shadow" ]; then
        mustecho -n "Note: MPI RMA support requires the application to be built with Clang >= 15."
        if test -z "$($myNM $executable 2>/dev/null | grep AnnotateHappensBefore)"; then
            musterror "Running in RMA shadow mode, but TSan runtime / annotation functions are missing. Rebuild your application with the correct TSan library."
            # still allow running the application, no exit here
            # exit 1
        fi
    fi
    if [ $rma_mode_arg == "isl" ] && test -z "$($myNM $executable 2>/dev/null | grep __must_isl_tsan_interceptor)"
    then
        musterror "Running in RMA ISL mode, but ISL interceptor library is missing. Rebuild your application with the correct interceptor library."
        exit 1
    fi
fi

if ! cmaketrue "@OMPT_FOUND@" && [ $do_openmp -ne 0 ]
then
    mustecho "Warning: Your toolchain has no support for the OMPT interface. Ignoring the --must:openmp argument."
fi

if [ $do_verbose -eq 1 ]
then
  mustecho "========= Step 3) Execution ========="
else
  if [ $do_quiet -ne 1 -a "$mode_arg" != "run" ]
  then
    mustecho "Executing application:"
  fi

  #Tell P^nMPI to be quiet
  export PNMPI_BE_SILENT=1
  must_exports="${must_exports} PNMPI_BE_SILENT"
fi

# Tell PnMPI to enable the debug module.
if [ -n "${wait_arg}" ]
then
  export WAIT_AT_STARTUP="${wait_arg}"
  must_exports="${must_exports} WAIT_AT_STARTUP"
fi

if [ "${instant_logging_arg}" != "none" ]
then
  export MUST_INSTANT_LOGGING="${instant_logging_arg}"
  must_exports="${must_exports} MUST_INSTANT_LOGGING"
fi

set +e #we ignore the final exit status, this simplifies regular expressions for tests

STACKWALKLIB=""
if cmaketrue "@CALLPATH_STACKWALKER_LIB_PATH@"
then
    LD_LIBRARY_PATH=@CALLPATH_STACKWALKER_LIB_PATH@:$LD_LIBRARY_PATH
    DYLD_LIBRARY_PATH=@CALLPATH_STACKWALKER_LIB_PATH@:$DYLD_LIBRARY_PATH
fi

if cmaketrue "@OMPT_FOUND@" && [ ${do_openmp} -ne 0 ]
then
    export OMP_TOOL_LIBRARIES=@CMAKE_INSTALL_PREFIX@/modules/libOpenMPadapter.so
    export MUST_TOOL_LIBRARIES=$MUST_TOOL_LIBRARIES:libarcher.so
    must_exports="${must_exports} OMP_TOOL_LIBRARIES MUST_TOOL_LIBRARIES"
fi

# export PNMPI_LIB_PATH=$temp_arg/module_temp
export PNMPI_LIB_PATH="${PNMPI_LIB_PATH:+${PNMPI_LIB_PATH}:}$inst_arg/modules/:${mustModulesDir}/:@GTI_MODULES_DIR@/"
must_exports="${must_exports} PNMPI_LIB_PATH"
export LD_LIBRARY_PATH=@CMAKE_INSTALL_PREFIX@/lib:$LD_LIBRARY_PATH

# !! don't export LD_LIBRARY_PATH/DYLD_LIBRARY_PATH with env, as they might be
# !! modified by mpiexec command and we would reset this modification.

#must_exports="${must_exports} LD_LIBRARY_PATH"
export DYLD_LIBRARY_PATH=@CMAKE_INSTALL_PREFIX@/lib:$DYLD_LIBRARY_PATH
#must_exports="${must_exports} DYLD_LIBRARY_PATH"
export PNMPI_CONF=$temp_arg/pnmpi.conf
must_exports="${must_exports} PNMPI_CONF"
if cmaketrue "@ENABLE_TSAN@" && [ $do_tsan -ne 0 -o $do_rma -ne 0 ]
then
    export LD_PRELOAD=@CMAKE_INSTALL_PREFIX@/modules/libTSanMessages.so:$LD_PRELOAD
    must_exports="${must_exports} LD_PRELOAD"
fi

if [ $do_rma -ne 0 ] && [ "$rma_mode_arg" == "isl" ]
then
    export LD_PRELOAD=@CMAKE_INSTALL_PREFIX@/modules/libTSanInterceptor.so:$LD_PRELOAD
    must_exports="${must_exports} LD_PRELOAD"
fi

local TIMEOUT_COMMAND=""
if test $timeout_arg -ne 0
then
    TIMEOUT_COMMAND="@TIMEOUT_COMMAND@ $timeout_arg"
fi

# Split the input if the "end-of-command-options" marker is present
if [[ "$input" == *" -- "* ]]
then
    local options_pre_marker="${input%% -- *}"
    local options_post_marker="${input#* -- }"
else
    local options_pre_marker="$input"
    local options_post_marker=""
fi

# Remove the given number of tasks
options_pre_marker=$(echo "$options_pre_marker" | sed -e "s/$switchToUse[[:space:]]\([^[:space:]]*\)//")
input="$options_pre_marker $options_post_marker"


local EXEC_EXPORTS=""
if [ -n "${must_exports}" ] && cmaketrue "@MUSTRUN_INJECT_ENV@"
then
    # $mpiexec_command should e.g look like this after this looping
    # construct: mpiexec /usr/bin/env "PATH=/usr/bin" "FANCY_ENVVAR=value of fancy envvar" ...
    EXEC_EXPORTS="$(which env)"
    for exp in ${must_exports}
    do
	EXEC_EXPORTS="${EXEC_EXPORTS} \"${exp}=${!exp}\""
    done
fi

input="@MPIEXEC_PREFLAGS@ $input @MPIEXEC_POSTFLAGS@"

if [[ "$mpimode_arg" != "MPMD" ]]
then
    mpiexec_arg="$mpiexec_arg $switchToUse $totalSize"
    mpiexec_command="$mpiexec_command $switchToUse $totalSize ${EXEC_EXPORTS}"

else
    mpiexec_arg="$mpiexec_arg $switchToUse $p"
    mpiexec_command="$mpiexec_command $switchToUse $p ${EXEC_EXPORTS}"

    input="$input : $switchToUse $[$totalSize-$p] ${EXEC_EXPORTS} ${TIMEOUT_COMMAND} $<TARGET_FILE_NAME:@PnMPI_PnMPIze@> $<TARGET_FILE_NAME:must_tool_process>"
fi

mpiexec_command="${mpiexec_command} ${TIMEOUT_COMMAND}"

#Verbose
if [ $do_verbose -eq 1 ]
then
  for e in ${must_exports}
  do
    mustecho export ${e}=$(eval echo \$${e})
  done
  mustecho "Going to execute run with:"
  mustecho "@BASH@ -c \"$mpiexec_command $input\""
fi

@BASH@ -c "$mpiexec_command $input"

if [ $do_cleanshm -eq 1 ]
then
    set +e
    $mpiexec_arg ${mustBinDir}/cleanup_shm >/dev/null 2>&1
    set -e
fi

# Remove the temporary return code file in any case
# but only report if we aren't quiet
if [ -n "${must_returncode_file}" -a -f "${must_returncode_file}" ]
then
    if [ -z "${errorcode_arg}" ]
    then
        errorcode_arg=`tail -n 1 ${must_returncode_file}`
    fi
    rm ${must_returncode_file}
fi

if [ -z "${errorcode_arg}" ]
then
    errorcode_arg=0
fi

if [ $do_clean -eq 1 -a ! "$temp_arg" -ef "${_MUST_WDIR}" ]
    then
    if [ $do_verbose -eq 1 ]
        then
        mustecho "Information: remove intermediate data in directory \""$temp_arg"\"!"
    fi
    rm -fr "$temp_arg";
fi

if [ -d $output_dir_arg/MUST_Output-files ] && dot=$(which "@DOT@")
    then
    if dotfiles=$(find $output_dir_arg/MUST_Output-files -name '*.dot')
        then
        for dotfile in $dotfiles
            do
            pngfile=${dotfile%.dot}.png
            if [ ! -f $pngfile -o $pngfile -ot $dotfile ]
                then
                dot -Tpng $dotfile -o $pngfile
            fi
        done
    fi
fi

if [ $output_arg == "json" ]
then
    # Replace ugly formatted output with pretty printed json
    output_name="${output_dir_arg}/MUST_Output.json"

    temp_output_name="${output_dir_arg}/MUST_Output.temp.json"
    if python3 -m json.tool < "${output_name}" > "${temp_output_name}"
    then
        mv "${temp_output_name}" "${output_name}"
    else
        rm -f "${temp_output_name}"
    fi
fi

if [ $do_quiet -ne 1 ]
then
    final_message="Execution finished."
    if [ $output_arg == "html" -o $output_arg == "json" ] && [ $do_ddt -eq 0 ] && [ -n "$output_email_report_arg" ]
    then
        output_name="${output_dir_arg}/MUST_Output.${output_arg}"
        if [ -f ${output_name} ]
        then
            final_message="Execution finished, inspect \""${output_name}"\"!"
        else
            musterror "Execution finished, but no output found at ${output_name}"
            exit 2;
        fi
    fi

    mustecho "${final_message}"
fi
# Only html or json supported for email report
if [ -n "$output_email_report_arg" ] && [ $output_arg == "html" -o $output_arg == "json" ]
then
    mustecho "Sending email report to $output_email_report_arg"

    if ! command -v mail &>/dev/null; then
        musterror "The 'mail' command is not available"
        musterror "Check the generated output located in ${output_dir_arg}"
    else
        tar -czf report.tar.gz -C "$output_dir_arg/../" $(basename "$output_dir_arg")

        message_body_file=$(mktemp)
        echo '' > $message_body_file

        mail -s 'MUST Report' -a 'report.tar.gz' $output_email_report_arg < $message_body_file

        if [ $? -ne 0 ]
        then
            musterror "Sending the email report (probably) failed."
            musterror "Please ensure your system is properly set up to send mail via the \"mail\" command."
            musterror "Check the generated output located in ${output_dir_arg}"
        else
            if [[ "$_MUST_WDIR" != "$output_dir_arg" ]]
            then
                rm -rf $output_dir_arg
            fi
        fi

        rm $message_body_file
        rm -f report.tar.gz
    fi
fi

exit ${errorcode_arg};

} # end run()

#----------------- Function ----------------
# name: get_must_cache_dir
# purpose: Determines the cache directory for must. It tries to follow the XDG Base Directory specification.
# return: prints the path to stdout.
# modifies: nothing
#-------------------------------------------
get_must_cache_dir () {
    local result
    if [ -n "${XDG_CACHE_HOME}" ] && [ "${XDG_CACHE_HOME:0:1}" = "/" ]  # relative paths are invalid per spec
    then
        result="${XDG_CACHE_HOME}/must"
    else
        result=@MUST_USER_CACHE_DIR@
    fi
    echo -n "${result}"
}

#----------------- Function ----------------
#name: get_prebuild_path
#returns: The directories where to look for prebuilds. The names are concatenated with a colon similar to the famous
# PATH variable.
# modifies: nothing
#-------------------------------------------
get_prebuild_path () {
    local PREBUILD_PATH="${mustModulesDir}/prebuilds"
    if [ -n "${user_cache_dir_arg}" ]
    then
        PREBUILD_PATH="${user_cache_dir_arg}/prebuilds:${PREBUILD_PATH}"
    fi
    if [ -n "${INTERNAL_MUST_PREBUILD_PATH}" ]
    then
        PREBUILD_PATH="${INTERNAL_MUST_PREBUILD_PATH}:${PREBUILD_PATH}"
    fi

    echo "${PREBUILD_PATH}"
}

#Exit if something goes wrong
set -e

#####################################################################
## 1) clean up the input (replace any sequence of (white)spaces [:space:] by a single space,
##    remove leading, tailing spaces)
input=`echo "$*" | sed -e 's/[[:space:]]\{1,\}/ /g' -e 's/^[[:space:]]*//g' -e 's/[[:space:]]*$//g'`

oldenv=$(env)

#####################################################################
## 2) Option parsing
##

if opt="$(echo " $input " | sed -e 's/.*[[:space:]]\(-h\)[[:space:]].*/\1/')" && test "$opt" != " $input "
then
    printHelp
elif opt="$(echo " $input " | sed -e 's/.*[[:space:]]\(--help\)[[:space:]].*/\1/')" && test "$opt" != " $input "
then
    printHelp
elif opt="$(echo " $input " | sed -e 's/.*[[:space:]]\(-help\)[[:space:]].*/\1/')" && test "$opt" != " $input "
then
    printHelp
fi

fanin_given=

envs=$(env | grep ^MUST | grep -v MUST_ROOT | grep -v MUST_INC | grep -v MUST_LIB | grep -v MUST_TOOL_ | cat)
while  IFS= read -r i ; do
    if [ -z "$i" ]; then
    	continue
    fi
    i=${i#MUST_}
    opt=${i%%=*}
    opt=$(echo $opt | tr A-Z a-z)
    case $opt in
    verbose | quiet | timing | massif | memcheck | nocrash | clean | cleanshm | hybrid | tsan | typeart | pnmpi_linked | partitioned_verbose_report | unused | info | distributed | dl | fillnodes | isp | scorep | ddt | reproduce | nodl | capture | unique | inbuilddir | noprebuilt | rma | rma_only | printmem | user_cache | openmp)
        envvars="${i%%=*} $envvars"
#       replace '-' by '_' in var names
        eval "do_${opt//-/_}=1"
        if [ "$opt" == "rma-only" ]
        then
            do_rma=1
            do_hybrid=1
        fi 
        case $opt in
            tsan | rma | openmp)
                do_hybrid=1
            ;;
        esac
        ;;

    # options with argument:
    mode | layout | np | mpiexec | temp | analyses | apis | language | nodesize | close | timeout | output | fanin | firstfanin | precmdddt | exportflag | src | inst | wait | stacktrace | output_dir | output_email_report | filter_file | mpimode | rma_mode | user_cache_dir | errorcode | instant_logging )
        envvars="${i%%=*} $envvars"
        arg=${i#*=}
        # optionspecific tests:
        arg="$(check_opt_arg $opt $arg)"
        if [[ $opt == "nodesize" ]]; then nodesizearg="argument nodesize $arg"; fi
        if [[ $opt == "close" ]]; then nodesizearg="argument nodesize $[$arg+1]"; fi
        if [[ $opt == "output_email_report" ]]; then do_unique=1; fi
        eval "${opt//-/_}_arg=\"$arg\""
        case $opt in
            fanin)
                fanin_given=1
            ;;
            instant_logging)
                instant_logging_arg=$(echo $instant_logging_arg | tr "[:upper:]" "[:lower:]")
            ;;
            rma-mode)
                if ! [ "$rma_mode_arg" == "shadow" ] && ! [ "$rma_mode_arg" == "isl" ]
                then
                    musterror "rma-mode requires 'isl' or 'shadow', got '$rma_mode_arg'"
                    exit 1
                fi
            ;;
        esac
        ;;
    *)
        mustecho "Unknown environment option MUST_$i is ignored"
    esac
done <<< "$envs"

while opt=$(echo " $input" | sed -e 's/.*[[:space:]]--must:\([^[:space:]]\{1,\}\).*/\1/') && test "$opt" != " $input" ;do

    if [[ $opt = no-* ]]
    then
        NEGATIVE_FLAG=1
        opt=${opt#no-}
    fi

    case $opt in
    verbose | quiet | timing | massif | memcheck | nocrash | clean | cleanshm | hybrid | tsan | typeart | pnmpi-linked | partitioned_verbose_report | unused | info | distributed | dl | fillnodes | isp | scorep | ddt | reproduce | nodl | capture | unique | inbuilddir | noprebuilt | rma | rma-only | printmem | user-cache | openmp )

        if [[ "${NEGATIVE_FLAG}" == 1 ]]
        then
            eval "do_${opt//-/_}=0"
            remove_single_opt no-$opt
        else
            # replace '-' by '_' in var names
            eval "do_${opt//-/_}=1"
            remove_single_opt $opt
            if [ "$opt" == "rma-only" ]
            then
                do_rma=1
                do_hybrid=1
            fi 
            case $opt in
                tsan | rma | openmp)
                    do_hybrid=1
                ;;
            esac
        fi
        ;;

    # options with argument:
    mode | layout | np | mpiexec | temp | analyses | apis | language | nodesize | close | timeout | output | fanin | firstfanin | precmdddt | exportflag | src | inst | wait | stacktrace | output-dir | output-email-report | filter-file | mpimode | rma-mode | user-cache-dir | errorcode | instant-logging )
        if [[ "${NEGATIVE_FLAG}" == 1 ]]
        then
            remove_single_opt no-$opt
            unset ${opt//-/_}_arg
            if [[ $opt == "nodesize" || $opt == "close" ]]
            then
                unset nodesizearg
            fi
            case $opt in
                fanin)
                    unset fanin_given
                    ;;
            esac
        else
            arg="$(get_opt_arg $opt)"
            # optionspecific tests:
            arg="$(check_opt_arg $opt $arg)"
            remove_opt_and_arg $opt
            if [[ $opt == "nodesize" ]]; then nodesizearg="argument nodesize $arg"; fi
            if [[ $opt == "close" ]]; then nodesizearg="argument nodesize $[$arg+1]"; fi
            if [[ $opt == "output-email-report" ]]; then do_unique=1; fi
            eval "${opt//-/_}_arg=$arg"
            case $opt in
                fanin)
                    fanin_given=1
                ;;
                instant-logging)
                    instant_logging_arg=$(echo $instant_logging_arg | tr "[:upper:]" "[:lower:]")
                ;;
                rma-mode)
                if ! [ "$rma_mode_arg" == "shadow" ] && ! [ "$rma_mode_arg" == "isl" ]
                then
                    musterror "rma-mode requires 'isl' or 'shadow', got '$rma_mode_arg'"
                    exit 1
                fi
                ;;
            esac
        fi
        ;;
    help)
        printHelp
        ;;
    version)
        printVersions
        exit 1
        ;;
    *)
        printHelp "Unknown option --must:$opt"
    esac

    unset NEGATIVE_FLAG
done

#####################################################################
## 2a) Scan for unused Options
if [ ${do_unused:=0} -eq 1 ]
then
    case ${mode_arg:=auto} in
    run)
        for i in nocrash layout analyses apis nodesize close output
        do
            if eval "temp=\"\${do_$i}\${${i}_arg}\""; [ "$temp" ]
            then
                mustecho "Unused Option --must:$i"
            fi
        done
        ;;
    prepare)
        for i in timing massif memcheck clean cleanshm pnmpi-linked language timeout
        do
            if eval "temp=\"\${do_$i}\${$i_arg}\""; [ "$temp" ]
            then
                mustecho "Unused Option --must:$i"
            fi
        done
        ;;
    esac
fi


#####################################################################
## 3) Set default values

temp=${layout_arg:=}
if [ -n "${layout_arg}" ]; then
    get_config_from_layout
fi


temp=${do_quiet:=0}
temp=${do_rma:=0}
temp=${do_rma_only:=0}
temp=${do_verbose:=0}
temp=${do_partitioned_verbose_report:=0}
temp=${do_timing:=0}
temp=${do_massif:=0}
temp=${do_memcheck:=0}
temp=${do_nocrash:=0}
temp=${do_clean:=0}
temp=${instant_logging_arg:="none"}
temp=${do_fillnodes:=0}
temp=${do_distributed:=0}
temp=${do_dl:=1}
temp=${do_nodl:=0}
temp=${do_info:=0}
temp=${do_isp:=0}
temp=${do_ddt:=0}
temp=${do_printmem:=0}
temp=${do_unique:=0}
temp=${do_reproduce:=0}
temp=${do_capture:=0}
temp=${do_scorep:=0}
temp=${do_inbuilddir:=0}
temp=${do_cleanshm:=0}
temp=${do_noprebuilt:=0}
temp=${do_hybrid:=0}
temp=${do_openmp:=0}
temp=${do_typeart:=0}
temp=${do_tsan:=0}
temp=${do_pnmpi_linked:=0}
temp=${mpimode_arg:=SPMD}
temp=${rma_mode_arg:=shadow}
temp=${do_user_cache:=1}
temp=${temp_arg:=must_temp}
temp=${src_arg:=$temp_arg/src}
temp=${inst_arg:=$temp_arg/install}
temp=${mode_arg:=auto}
temp=${np_arg:="-np -n -p @MPIEXEC_NUMPROC_FLAG@"}
MPIEXEC=${MPIEXEC:-"@MPIEXEC@"}
temp=${mpiexec_arg:=$MPIEXEC}
temp=${analyses_arg:=""}
# temp=${apis_arg:=}
temp=${language_arg:=@MUST_DEFAULT_MPI_INTERFACE@}
#temp=${nodesize_arg:=""}
temp=${timeout_arg:="0"}
temp=${output_arg:="html"}
temp=${fanin_arg:=16}
temp=${firstfanin_arg:=$fanin_arg}}
temp=${precmdddt_arg:="ddt-client"}
temp=${wait_arg:=""}
temp=${user_cache_dir_arg:=$(get_must_cache_dir)}
temp=${exitcode_arg:=""}
temp=${output_email_report_arg:=""}

if cmaketrue "@USE_BACKWARD@"
then
    temp=${stacktrace_arg:="backward"}
else
    temp=${stacktrace_arg:="none"}
fi

if [ $do_rma_only -ne 0 ]
then
    output_arg="stdout"
fi


# Deactivate user cache whenever using prepare mode
if [ "$mode_arg" = "prepare" ]
then
  do_user_cache=0
fi

if [ ${do_user_cache} -eq 0 ]
then
  user_cache_dir_arg=""
fi

if ! cmaketrue "@ENABLE_TSAN@" && [ ${do_rma} -eq 1 ]
then
  printHelp "Cannot run MUST-RMA race checks without enabled TSan support."
fi

if [ "$mpimode_arg" = "MPMD" ] && [ -n "$nodesize_arg" -o -n "$close_arg" ]
then
  printHelp "Cannot use nodesize or close argument together with MPMD mode, use --must:mpimode SPMD"
fi

if [ "$mpimode_arg" != "MPMD" -a $do_openmp -eq 1 ]
then
  printHelp "Running OpenMP analysis without MPMD mode is unsafe, use --must:mpimode MPMD"
fi

#run the Generators
export PATH=@GTI_HOME@/bin:$PATH
must_exports="${must_exports} PATH"

if ! cmaketrue "@ENABLE_PREBUILD@"
then
    do_noprebuilt=1
fi

if [ $do_nodl -eq 1 ]
then
    do_dl=0
fi

if [ $do_inbuilddir -eq 1 ]
then
    mustModulesDir=@CMAKE_BINARY_DIR@/modules
    mustSpecDir=@CMAKE_BINARY_DIR@/specifications
    mustBinDir=@CMAKE_BINARY_DIR@/utility
else
    mustModulesDir=@CMAKE_INSTALL_PREFIX@/modules
    mustSpecDir=@CMAKE_INSTALL_PREFIX@/specifications
    mustBinDir=@CMAKE_INSTALL_PREFIX@/bin
fi

if [ $do_quiet -eq 0 -a -n "$envvars" ]
then
    mustecho "Using environment variable(s) $envvars"
fi
#####################################################################
## 3b) Make paths absolut

absolut_path temp_arg
absolut_path src_arg
absolut_path inst_arg
analyses_arg=${analyses_arg//:/ }
analyses=""
for i in $analyses_arg; do
    absolut_path i
    analyses="$analyses $i"
done
apis_arg=${apis_arg//:/ }
apis=""
for i in $apis_arg; do
    absolut_path i
    apis="$apis $i"
done

if [ ${do_info} -ne 0 ]
then
#    setup
    calclayers

    if [ $do_fillnodes -eq 1 ]
    then
        fillnodes
    fi

    print_info

    exit 0
fi

#####################################################################
## 3c) Auto mode

if [ ${mode_arg} == "auto" ]
then
    if [ ! -d ${temp_arg} -o ! -f ${temp_arg}/lastrun ]
    then
        mode_arg="prepareautorun"
    else
        calclayers

        if [ $do_fillnodes -eq 1 ]
        then
            fillnodes
        fi

        if get_checksum | diff - ${temp_arg}/lastrun 2>/dev/null 1>&2
        then
            mode_arg="autorun"
        else
            mode_arg="prepareautorun"
        fi
    fi
fi

#####################################################################
## 4) if prepare mode, run prepare()

i_am_builder="0"
case $mode_arg in prepare*|build|config|autorun)
    calclayers

    if [ -f ${temp_arg}/lastrun -a -n "$preInstalled" ] && grep "Using prebuild" ${temp_arg}/lastrun && ! grep "Using prebuild $preInstalled" ${temp_arg}/lastrun;
    then
    	rm -f ${temp_arg}/lastrun
    	if [ $mode_arg = "autorun" ];
    	then
    	    mode_arg = "prepareautorun"
    	fi
    fi

    if [ $do_fillnodes -eq 1 ]
    then
        fillnodes
    fi

    if [ $do_quiet -eq 0 ]
    then
        print_info
    fi

    if [ $do_inbuilddir -eq 1 ] && [ ${mode_arg} == "prepare" ]
    then
        # We're generating the prebuilds with the cmake
        # install-prebuilds target. Place them into a single well-known
        # directory so we do not have to know the hashes to install the
        # prebuilds with CMake.
        inst_arg="${INTERNAL_MUST_PREBUILD_DEST:-@CMAKE_BINARY_DIR@/prebuild/prebuilds}/$(get_prebuild_hash)"
        if [ -d "${inst_arg}/modules" ]
        then
            if [ $do_quiet -ne 1 ]
            then
                mustecho "The prebuilt \"${inst_arg}\" is already present. Nothing to do here. Exiting."
            fi
            exit 0
        fi

        mkdir -p "${inst_arg}"
        if ! try_lock_dir "${inst_arg}"
        then
            assert_not_locked "${BASH_SOURCE}:${LINENO}"
            if [ $do_quiet -ne 1 ]
            then
                mustecho "The prebuilt \"${inst_arg}\" is built by another instance right now. Nothing to do here. Exiting."
            fi
            exit 0
        else
            # Check again since we could have been outpaced by another process
            # between checking and acquiring the lock. (For the unlikely case
            # that the other process acquires and releases the lock again in the
            # meantime.)
            if [ -d "${inst_arg}/modules" ]
            then
                if [ $do_quiet -ne 1 ]
                then
                    mustecho "The prebuilt \"${inst_arg}\" is already present. Nothing to do here. Exiting."
                fi
                exit 0
            fi
	    i_am_builder="1"
        fi
    elif [ -n "${user_cache_dir_arg}" ] && ( [ -z "$preInstalled" ] || [ ! -d "$preInstalled" ] )
    then
        inst_arg="${user_cache_dir_arg}/prebuilds/$(get_prebuild_hash)"

        # Lock inst_arg directory in case that parallel runs of mustrun generate the same prebuild hash and collide.
        mkdir -p "${inst_arg}"

        # acquire an exclusive lock on the install directory
        while ! try_lock_dir "${inst_arg}"
        do
          # Retry if the lock could not be acquired.
          sleep 1
        done

        # Force update of inode metadata
        ls "${inst_arg}" > /dev/null

        if [ -d "${inst_arg}/modules" ]
        then
            # The prebuild is already installed from a previous run.
            release_dir "${inst_arg}"
            preInstalled=${inst_arg}
        else
            i_am_builder="1"
        fi
    elif [ ${do_user_cache} -eq 0 ]
    then
        mkdir -p "${inst_arg}"
        # acquire an exclusive lock on the install directory
        while ! try_lock_dir "${inst_arg}"
        do
          # Retry if the lock could not be acquired.
          sleep 1
        done
        i_am_builder="1"
    fi

    setup
    ;;
esac

if [ "${i_am_builder}" -eq 1 ]
then
#####################################################################
## 4b) build gti infrasturcture
    case $mode_arg in prepare*|build)
        build;;
    esac

    release_dir "${inst_arg}"
fi

#####################################################################
## 4c) create pnmpi config

case $mode_arg in prepare*|config*|autorun)
    config;;
*)
# update html/stdout
    if [ $output_arg == "stdout" ] && \
           # specifiying libmsgLoggerStdOut twice is not allowed
           ! grep --quiet "module libmsgLoggerStdOut" $temp_arg/pnmpi.conf && \
           [ -n "${preInstalled}" ]
    then
      cat $temp_arg/pnmpi.conf | sed -e 's/module libmsgLoggerHtml/module libmsgLoggerStdOut/g' \
                                     -e 's/module libmsgLoggerJson/module libmsgLoggerStdOut/g' > $temp_arg/pnmpi.conf2
      mv $temp_arg/pnmpi.conf2 $temp_arg/pnmpi.conf
    fi

    if [ $output_arg == "html" ] && \
           # specifiying libmsgLoggerHtml twice is not allowed
           ! grep --quiet "module libmsgLoggerHtml" $temp_arg/pnmpi.conf && \
           [ -n "${preInstalled}" ]
    then
      cat $temp_arg/pnmpi.conf | sed -e 's/module libmsgLoggerStdOut/module libmsgLoggerHtml/g' \
                                     -e 's/module libmsgLoggerJson/module libmsgLoggerHtml/g' > $temp_arg/pnmpi.conf2
      mv $temp_arg/pnmpi.conf2 $temp_arg/pnmpi.conf
    fi

    if [ $output_arg == "json" ] && \
               # specifiying libmsgLoggerHtml twice is not allowed
               ! grep --quiet "module libmsgLoggerJson" $temp_arg/pnmpi.conf && \
               [ -n "${preInstalled}" ]
        then
          cat $temp_arg/pnmpi.conf | sed -e 's/module libmsgLoggerStdOut/module libmsgLoggerJson/g' \
                                         -e 's/module libmsgLoggerHtml/module libmsgLoggerJson/g' > $temp_arg/pnmpi.conf2
          mv $temp_arg/pnmpi.conf2 $temp_arg/pnmpi.conf
        fi
esac

#####################################################################
## 5) if run mode, run run()

case $mode_arg in *run|*preinstalled)
    # If path to infrastructure does not exist, try to find prebuilds
    if [ ! -d "${inst_arg}/modules" ]
    then
        calclayers

        # Check if installed prebuild or user-cache prebuild exists and set it (if available)
        check_prebuild_dir="${INTERNAL_MUST_PREBUILD_DEST:-@CMAKE_BINARY_DIR@/prebuild/prebuilds}/$(get_prebuild_hash)"
        if [ -d "${check_prebuild_dir}/modules" ] # installed prebuilds
        then
            inst_arg="${check_prebuild_dir}"
        elif [ ${do_user_cache} -ne 0 ] && [ -d "${user_cache_dir_arg}/prebuilds/$(get_prebuild_hash)/modules" ] # user-cache prebuilds
        then
            inst_arg="${user_cache_dir_arg}/prebuilds/$(get_prebuild_hash)"
        else
            musterror "No built infrastructure is present. Try to run with '--must:prepare' or '--must:preparerun' to build an infrastructure."
            exit 1
        fi
    fi

    mustecho "Infrastructure in \"${inst_arg}\" is present and used."
    run;;
esac



