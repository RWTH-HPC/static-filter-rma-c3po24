##
## Help Function, Script usage
##

available_opts="AL ALX CLUSTER"
available_opts_help=\
"Opt     | Description
--------|------------
AL      | Allowlist optimization, base version
ALX     | Allowlist optimization, using remote access type
CLUSTER | Cluster TSan calls inside basic blocks"

function print_help() {
    name=$(basename $0)
    printf "$name is a helper script to allow for dynamically getting the required passes and options for opt"
    printf "\nfor use with RMAOptimizerPlugin"
    printf "\n\nUsage: $0 [--opts] [--opts-help] [--get-config <arg>] <optimizations>"
    printf "\n\t --opts: Print available optimizations and exit. Minimal output for automatic parsing."
    printf "\n\t --opts-help: Print available optimizations and exit, including short descriptions of each."
    printf "\n\t --get-config: Print the required configuration for opt. Argument 'passes' for the passes, 'args' for the arguments."
    printf "\n\t               Only one printed at a time, call script twice for both, as both are required."
    printf "\n\t               Passes are string separated, arguments space separated, so forwarding directly to opt is possible."
    printf "\n\t --help: Print this help text and exit"
    printf "\n"
}

function parse_cmd() {
optimizations=""
mode="None"

while [[ $# -gt 0 ]]; do
  case $1 in
    --help)
      print_help
      exit 0
      ;;
    --opts)
      echo ${available_opts}
      exit 0
      ;;
    --opts-help)
      echo "${available_opts_help}"
      exit 0
      ;;
    --get-config)
      mode="$2"
      shift # past argument
      shift # past value
      ;;
    *)
      optimizations="$optimizations $1"
      shift # past argument
      ;;
  esac
done
}

parse_cmd $@

##
## Sanity checks
##

# Check mode validity
if [[ "${mode}" != "passes" ]] && [[ "${mode}" != "args" ]]; then
    echo "Invalid configuration data requested: ${mode}" 1>&2
    exit 1
fi

# Check optimizations
recog_opt=0
for opt in ${optimizations}; do
    for real_opt in ${available_opts}; do
        if [[ "${opt}" =~ "${real_opt}"[0-9]*$ ]]; then
            recog_opt=1
        fi
    done
    if [[ $recog_opt -eq 0 ]]; then
        echo "Invalid optimization requested: ${opt}" 1>&2
        exit 1
    fi
done

##
## Parse requested optimizations
##

RMAOPT_AL=0
RMAOPT_AL_EXTENDED=0
RMAOPT_DELAY=0
RMAOPT_CLUSTER=0

unset depth_param

# Check optimizations
for opt in ${optimizations}; do
    if [[ "${opt}" =~ "AL"[0-9]* ]]; then
        RMAOPT_AL=1
        depth_param="$(echo ${opt} | sed 's/AL//')"
    fi
    if [[ "${opt}" =~ "ALX"[0-9]* ]]; then
        RMAOPT_AL=1
        RMAOPT_AL_EXTENDED=1
        depth_param="$(echo ${opt} | sed 's/ALX//')"
    fi
    if [[ "${opt}" == "DELAY" ]]; then
        RMAOPT_DELAY=1
    fi
    if [[ "${opt}" == "CLUSTER" ]]; then
        RMAOPT_CLUSTER=1
    fi
done

##
## Generate config
##

need_attrpass=true
passlist_string="tsanMOD-module,function(tsanMOD)"

# Use allowlist?
if [[ $RMAOPT_AL -eq 1 ]]; then
    need_attrpass=false
    args_needed="${args_needed} -tsanMOD-use-optallowlist=1"
    if [[ $RMAOPT_AL_EXTENDED -eq 1 ]]; then
        args_needed="${args_needed} -tsanMOD-use-optallowlist-remoteaccesstypeext=1" 
    fi
    if [[ -n "$depth_param" ]]; then
        args_needed="${args_needed} -tsanMOD-allowlist-depth=${depth_param}"
    fi
fi

# Use ActivatorPass?
if [[ $RMAOPT_TSANDELAY -eq 1 ]]; then
    passlist_string="function(tsanMOD-activatoropt),${passlist_string}"
fi

# Use ClusteringPass?
if [[ $RMAOPT_CLUSTER -eq 1 ]]; then
    passlist_string="${passlist_string},tsanMOD-cluster-shim,function(tsanMOD-clusteringopt)"
fi

# Add AttrPass if required
if $need_attrpass; then
    passlist_string="function(tsanMOD-attrpass),${passlist_string}"
fi

##
## Output config
##

if [[ "${mode}" == "passes" ]]; then
    printf '%s' "${passlist_string}"
fi
if [[ "${mode}" == "args" ]]; then
    printf '%s' "${args_needed}"
fi
