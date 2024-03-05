import json
import argparse
import copy


# This map is not to be used directly -- it is copied and used as the
# basis for multiple other kind maps that fill in proper types for the
# POLY* types.

BASE_C_KIND_MAP = {
    # Pointers
    'BUFFER'              : 'void',
    'C_BUFFER'            : 'void',
    'C_BUFFER2'           : 'void',
    'C_BUFFER3'           : 'void',
    'C_BUFFER4'           : 'void',
    'EXTRA_STATE'         : 'void', # The '*' is added in bindingc.py
    'EXTRA_STATE2'        : 'void',
    'FUNCTION_SMALL'      : None,   # Every function pointer type is different
    'FUNCTION'            : None,
    'POLYFUNCTION'        : None,

    # Callback-functions for MPI_T events
    'EVENT_CB_FUNCTION'      : 'MPI_T_event_cb_function',
    'EVENT_FREE_CB_FUNCTION' : 'MPI_T_event_free_cb_function',
    'EVENT_DROP_CB_FUNCTION' : 'MPI_T_event_dropped_cb_function',

    'STRING'              : 'char', # The '*' is added in bindingc.py
    'STRING_ARRAY'        : 'char',
    'STRING_2DARRAY'      : 'char',

    'ARGUMENT_COUNT'      : 'int',
    'ARGUMENT_LIST'       : 'char',

    # Various types of integers
    'ARRAY_LENGTH'        : 'int',
    'ARRAY_LENGTH_NNI'    : 'int',
    'ARRAY_LENGTH_PI'     : 'int',
    'ATTRIBUTE_VAL_10'    : 'void', # From MPI-1.0
    'ATTRIBUTE_VAL'       : 'void', # Current version of MPI
    'BLOCKLENGTH'         : 'int',
    'COLOR'               : 'int',
    'COORDINATE'          : 'int',
    'COORDINATE_NNI'      : 'int',
    'DEGREE'              : 'int',
    'DIMENSION'           : 'int',
    'ENUM'                : 'int',
    'FILE_DESCRIPTOR'     : 'int',
    'KEY'                 : 'int',
    'KEYVAL'              : 'int',
    'INDEX'               : 'int',
    'LOGICAL'             : 'int',
    'LOGICAL_OPTIONAL'    : 'int',
    'LOGICAL_BOOLEAN'     : 'int',
    'MATH'                : 'int',
    'NUM_DIMS'            : 'int',
    'RANK'                : 'int',
    'RANK_NNI'            : 'int',
    'COMM_SIZE'           : 'int',
    'COMM_SIZE_PI'        : 'int',
    'STRING_LENGTH'       : 'int',
    'STRIDE_BYTES'        : 'MPI_Aint',
    'STRIDE_ELEM'         : 'int',
    'TAG'                 : 'int',
    'VERSION'             : 'int',
    'WEIGHT'              : 'int',
    'OFFSET'              : 'MPI_Offset',
    'PROFILE_LEVEL'       : 'int',
    'WINDOW_SIZE'         : 'MPI_Aint',
    'INFO_VALUE_LENGTH'   : 'int',
    'ACCESS_MODE'         : 'int',
    'UPDATE_MODE'         : 'int',
    'KEY_INDEX'           : 'int',
    'TOOLENUM_INDEX'      : 'int',
    'TOOLENUM_SIZE'       : 'int',
    'TOOL_VAR_VERBOSITY'  : 'int',
    'TOOL_VAR_VALUE'      : 'int',
    'CVAR_INDEX'          : 'int',
    'CVAR_INDEX_SPECIAL'  : 'int',
    'PVAR_INDEX'          : 'int',
    'PVAR_CLASS'          : 'int',
    'SOURCE_INDEX'        : 'int',
    'TOOLS_TICK_COUNT'    : 'MPI_Count',
    'EVENT_INDEX'         : 'int',
    'CAT_INDEX'           : 'int',
    'UPDATE_NUMBER'       : 'int',
    'DROPPED_COUNT'       : 'MPI_Count',
    'TYPECLASS_SIZE'      : 'int',
    'GENERIC_DTYPE_INT'   : 'int',
    'GENERIC_DTYPE_COUNT' : 'MPI_Count',
    'PROCESS_GRID_SIZE'   : 'int',
    'DTYPE_DISTRIBUTION'  : 'int',

    # These are special.  See note in LIS_KIND_MAP for details.
    'ALLOC_MEM_NUM_BYTES' : 'MPI_Aint',
    'PACK_EXTERNAL_SIZE'  : 'MPI_Aint',
    'WIN_ATTACH_SIZE'     : 'MPI_Aint',

    # See notes about these types in LIS_KIND_MAP.
    'DISPLACEMENT_SMALL'  : 'int',
    'DISPLACEMENT'        : 'MPI_Aint',
    'DISPLACEMENT_NNI'    : 'MPI_Aint',

    'RMA_DISPLACEMENT_SMALL': 'int',
    'RMA_DISPLACEMENT'    : 'MPI_Aint',

    'XFER_NUM_ELEM_SMALL' : 'int',
    'XFER_NUM_ELEM'       : 'MPI_Count',

    'NUM_BYTES_SMALL'     : 'int',
    'NUM_BYTES'           : 'MPI_Count',

    'NUM_BYTES_NNI_SMALL' : 'int',
    'NUM_BYTES_NNI'       : 'MPI_Count',

    # Enums
    'ERROR_CODE'          : 'int',
    'ERROR_CODE_SHOW_INTENT': 'int',
    'ERROR_CLASS'         : 'int',
    'ORDER'               : 'int',
    'THREAD_LEVEL'        : 'int',
    'COMBINER'            : 'int',
    'LOCK_TYPE'           : 'int',
    'TOOLS_ENUM'          : 'MPI_T_enum',
    'BIND_TYPE'           : 'int',
    'SOURCE_ORDERING'     : 'MPI_T_source_order',
    'CALLBACK_SAFETY'     : 'MPI_T_cb_safety',
    'VARIABLE_SCOPE'      : 'int',
    'ASSERT'              : 'int',
    'TYPECLASS'           : 'int',
    'GROUP_COMPARISON'    : 'int',
    'COMM_COMPARISON'     : 'int',
    'SPLIT_TYPE'          : 'int',
    'TOPOLOGY_TYPE'       : 'int',
    'DISTRIB_ENUM'  : 'int',

    'DISPOFFSET_SMALL'    : 'MPI_Aint',
    'DISPOFFSET'          : 'MPI_Count',


    'DTYPE_NUM_ELEM_NNI_SMALL': 'int',
    'DTYPE_NUM_ELEM_NNI'      : 'MPI_Count',


    'DTYPE_NUM_ELEM_SMALL': 'int',
    'DTYPE_NUM_ELEM'      : 'MPI_Count',


    # Polymorphic types and their corresponding non-polymorphic types.
    # Anything that is POLY* means that it has one type in <=MPI-3.1
    # and a different type in >=MPI-4.0.
    'POLYDISPLACEMENT'    : None,
    'POLYRMA_DISPLACEMENT': None,
    'POLYDISPOFFSET'      : None,
    'POLYDTYPE_NUM_ELEM_NNI': None,
    'POLYDTYPE_NUM_ELEM'    : None,
    'POLYDTYPE_NUM_ELEM_PI' : None,
    'POLYTOOLS_NUM_ELEM'  : None,
    'POLYNUM_BYTES'       : None,
    'POLYNUM_BYTES_NNI'   : None,
    'POLYXFER_NUM_ELEM'   : None,
    'POLYXFER_NUM_ELEM_NNI' : None,
    'POLYDTYPE_STRIDE_BYTES': None,
    'POLYDISPLACEMENT_COUNT': None,
    'POLYDISPLACEMENT_AINT_COUNT': None,
    'POLYDTYPE_PACK_SIZE'   : None,
    'POLYRMA_DISPLACEMENT_NNI'  : None,
    'POLYLOCATION'  : None,

    'DTYPE_STRIDE_BYTES_SMALL': 'MPI_Aint',
    'DTYPE_STRIDE_BYTES'      : 'MPI_Count',

    'DTYPE_NUM_ELEM_PI_SMALL': 'int',
    'DTYPE_NUM_ELEM_PI'      : 'MPI_Count',

    'DTYPE_NUM_ELEM_SMALL': 'int',
    'DTYPE_NUM_ELEM'      : 'MPI_Count',

    'TOOLS_NUM_ELEM_SMALL': 'int',
    'TOOLS_NUM_ELEM'      : 'MPI_Count',

    'XFER_NUM_ELEM_NNI_SMALL': 'int',
    'XFER_NUM_ELEM_NNI'      : 'MPI_Count',

    'DISPLACEMENT_COUNT_SMALL': 'int',
    'DISPLACEMENT_COUNT'      : 'MPI_Count',

    'DISPLACEMENT_AINT_COUNT_SMALL': 'MPI_Aint',
    'DISPLACEMENT_AINT_COUNT'      : 'MPI_Count',

    'DTYPE_PACK_SIZE_SMALL': 'MPI_Aint',
    'DTYPE_PACK_SIZE'      : 'MPI_Count',

    'RMA_DISPLACEMENT_NNI_SMALL': 'int',
    'RMA_DISPLACEMENT_NNI'      : 'MPI_Aint',

    'LOCATION_SMALL'  : 'MPI_Aint',
    'LOCATION'        : 'MPI_Count',

    'NUM_PARAM_VALUES_SMALL': 'int',
    'POLYNUM_PARAM_VALUES': None,
    'NUM_PARAM_VALUES': 'MPI_Count',

    # MPI partitioned communication
    'PARTITION'           : 'int',

    # MPI handles
    'COMMUNICATOR'        : 'MPI_Comm',
    'DATATYPE'            : 'MPI_Datatype',
    'ERRHANDLER'          : 'MPI_Errhandler',
    'FILE'                : 'MPI_File',
    'GROUP'               : 'MPI_Group',
    'INFO'                : 'MPI_Info',
    'MESSAGE'             : 'MPI_Message',
    'REQUEST'             : 'MPI_Request',
    'SESSION'             : 'MPI_Session',
    'STATUS'              : 'MPI_Status',
    'WINDOW'              : 'MPI_Win',
    'OPERATION'           : 'MPI_Op',
    'CVAR'                : 'MPI_T_cvar_handle',
    'PVAR'                : 'MPI_T_pvar_handle',
    'PVAR_SESSION'        : 'MPI_T_pvar_session',
    'EVENT_REGISTRATION'  : 'MPI_T_event_registration',
    'EVENT_INSTANCE'      : 'MPI_T_event_instance',
    'TOOL_MPI_OBJ'        : 'void',

    # Special handles (needed for handle conversion bindings)
    'F90_STATUS'          : 'MPI_Fint',
    'F08_STATUS'          : 'MPI_F08_status',

    'F90_COMM'            : 'MPI_Fint',
    'F90_DATATYPE'        : 'MPI_Fint',
    'F90_GROUP'           : 'MPI_Fint',
    'F90_REQUEST'         : 'MPI_Fint',
    'F90_FILE'            : 'MPI_Fint',
    'F90_WIN'             : 'MPI_Fint',
    'F90_OP'              : 'MPI_Fint',
    'F90_INFO'            : 'MPI_Fint',
    'F90_ERRHANDLER'      : 'MPI_Fint',
    'F90_MESSAGE'         : 'MPI_Fint',
    'F90_SESSION'         : 'MPI_Fint',

    # Special handle for VARARGS in MPI_Pcontrol
    'VARARGS'             : '\\ldots',

    # Specials for return types
    'WALL_TIME'           : 'double',
    'TICK_RESOLUTION'     : 'double',
    'NOTHING'             : 'void'
}

# These 2 maps are meant to be used.  They have types filled in for POLLY*.

SMALL_C_KIND_MAP = copy.deepcopy(BASE_C_KIND_MAP)
SMALL_C_KIND_MAP.update({
    'POLYDISPLACEMENT'    : 'int',
    'POLYRMA_DISPLACEMENT': 'int',
    'POLYRMA_DISPLACEMENT_NNI'  : 'int',
    'POLYDISPOFFSET'      : 'MPI_Aint',
    'POLYDTYPE_NUM_ELEM_NNI': 'int',
    'POLYDTYPE_NUM_ELEM'    : 'int',
    'POLYDTYPE_NUM_ELEM_PI' : 'int',
    'POLYTOOLS_NUM_ELEM'  : 'int',
    'POLYNUM_BYTES'       : 'int',
    'POLYNUM_BYTES_NNI'       : 'int',
    'POLYXFER_NUM_ELEM'   : 'int',
    'POLYXFER_NUM_ELEM_NNI' : 'int',
    'POLYDTYPE_STRIDE_BYTES': 'MPI_Aint',
    'POLYDISPLACEMENT_COUNT': 'int',
    'POLYDISPLACEMENT_AINT_COUNT': 'MPI_Aint',
    'POLYDTYPE_PACK_SIZE'   : 'MPI_Aint',
    'POLYLOCATION'  : 'MPI_Aint',
    'POLYNUM_PARAM_VALUES': 'int',
})

#BIG_C_KIND_MAP = copy.deepcopy(BASE_C_KIND_MAP)
#BIG_C_KIND_MAP.update({
#    'POLYDISPLACEMENT'    : 'MPI_Aint',
#    'POLYRMA_DISPLACEMENT': 'MPI_Aint',
#    'POLYRMA_DISPLACEMENT_NNI'  : 'MPI_Aint',
#    'POLYDISPOFFSET'      : 'MPI_Count',
#    'POLYDTYPE_NUM_ELEM_NNI': 'MPI_Count',
#    'POLYDTYPE_NUM_ELEM'    : 'MPI_Count',
#    'POLYDTYPE_NUM_ELEM_PI' : 'MPI_Count',
#    'POLYTOOLS_NUM_ELEM'  : 'MPI_Count',
#    'POLYNUM_BYTES'       : 'MPI_Count',
#    'POLYNUM_BYTES_NNI'       : 'MPI_Count',
#    'POLYXFER_NUM_ELEM'   : 'MPI_Count',
#    'POLYXFER_NUM_ELEM_NNI' : 'MPI_Count',
#    'POLYDTYPE_STRIDE_BYTES': 'MPI_Count',
#    'POLYDISPLACEMENT_COUNT': 'MPI_Count',
#    'POLYDISPLACEMENT_AINT_COUNT': 'MPI_Count',
#    'POLYDTYPE_PACK_SIZE'   : 'MPI_Count',
#    'POLYLOCATION'  : 'MPI_Count',
#    'POLYNUM_PARAM_VALUES': 'MPI_Count',
#})

BIG_C_KIND_MAP = copy.deepcopy(BASE_C_KIND_MAP)
BIG_C_KIND_MAP.update({
    'POLYDISPLACEMENT'    : 'int',
    'POLYRMA_DISPLACEMENT': 'int',
    'POLYRMA_DISPLACEMENT_NNI'  : 'int',
    'POLYDISPOFFSET'      : 'MPI_Aint',
    'POLYDTYPE_NUM_ELEM_NNI': 'int',
    'POLYDTYPE_NUM_ELEM'    : 'int',
    'POLYDTYPE_NUM_ELEM_PI' : 'int',
    'POLYTOOLS_NUM_ELEM'  : 'int',
    'POLYNUM_BYTES'       : 'int',
    'POLYNUM_BYTES_NNI'       : 'int',
    'POLYXFER_NUM_ELEM'   : 'int',
    'POLYXFER_NUM_ELEM_NNI' : 'int',
    'POLYDTYPE_STRIDE_BYTES': 'MPI_Aint',
    'POLYDISPLACEMENT_COUNT': 'int',
    'POLYDISPLACEMENT_AINT_COUNT': 'MPI_Aint',
    'POLYDTYPE_PACK_SIZE'   : 'MPI_Aint',
    'POLYLOCATION'  : 'MPI_Aint',
    'POLYNUM_PARAM_VALUES': 'int',
})


def get_pointer_attribute(parameter):
    """
    Emits the pointer attribute of the parameter expression.
    """

    # Add a star if:
    # - This is a BUFFER or C_BUFFER, or
    # - This is a STRING, or
    # - This is a STATUS, or
    # - This is a TOOL_MPI_OBJ, or
    # - This is a F90_STATUS or F08_STATUS, or
    # - This is an EXTRA_STATE, or
    # - This is an IN FUNCTION, or
    # - This is an INOUT or OUT and there is no "length" inidicated, or
    # - "pointer" is True

    if parameter['pointer'] is not None and not parameter['pointer'] and parameter['kind'] == 'ARGUMENT_LIST':
        return '*'

    if parameter['pointer'] is not None and not parameter['pointer']:
        return '*'

    if parameter['kind'] == 'STRING_2DARRAY':
        return '**'

    if parameter['kind'] == 'ARGUMENT_LIST':
        return '***'

    # needed for MPI_UNPACK_EXTERNAL[_size]
    if (parameter['kind'] == 'STRING' and
            parameter['length'] == '*' and
            not parameter['pointer']):
        return ''

    if parameter['kind'] in ('BUFFER', 'C_BUFFER', 'C_BUFFER2', 'C_BUFFER3',
                             'C_BUFFER4', 'STRING', 'EXTRA_STATE',
                             'EXTRA_STATE2', 'ATTRIBUTE_VAL', 'STATUS',
                             'ATTRIBUTE_VAL_10', 'STRING_ARRAY',
                             'FUNCTION', 'FUNCTION_SMALL', 'POLYFUNCTION',
                             'TOOL_MPI_OBJ', 'F08_STATUS', 'F90_STATUS'):
        return '*'

    if (parameter['param_direction'] == 'inout' or
            parameter['param_direction'] == 'out' or
            parameter['pointer']) and parameter['length'] is None:
        return '*'

    if parameter['length'] is not None:
        return '*'


    return ''


def get_const_attribute(parameter):
    """
    Emits the const attribute of the parameter expression.
    """

    return 'const ' if parameter['constant'] else ''


def get_type_str(param):
    postfix = ''
    if param['kind'] in ('FUNCTION', 'FUNCTION_SMALL'):
        return f"{param['func_type']}"
    elif param['kind'] == 'POLYFUNCTION':
        return f"{param['func_type']}{postfix}"

    return BIG_C_KIND_MAP[param['kind']]


def get_param_name(param):
    if param['kind'] == 'VARARGS':
        return ''
    return f' {param["name"]}'


def get_deref_paramter(name, param):
    #if param['kind'] in ['INDEX'] and param['param_direction']:
    if param['param_direction']:
        for p in data[name]['parameters']:
            if p['kind'] == 'ARRAY_LENGTH':
                return p['name']

    return None


def get_param_postfix(name, param, param_idx, pointer_attr):
    if pointer_attr == '':
        return ''
    postfix = ' {'
    isArray = param['length'] is not None
    if isArray:
        postfix += 'ARRAY_'
    else:
        postfix += 'SINGLE_'

    if param['kind'] in ['BUFFER'] or param['param_direction'] == 'in':
        postfix += 'IN'
    elif param['param_direction'] == 'out':
        postfix += 'OUT'
    elif param['param_direction'] == 'inout':
        postfix += 'IO'

    if isArray:
        postfix += '|'
        if param['length'] in [x['name'] for x in data[name]['parameters']]:
            postfix += f'ARG:{param["length"]}'
        elif get_deref_paramter(name, param):
            postfix += f'OP:deref:{get_deref_paramter(name, param)}'
        else:
            postfix += 'OP:comm_size:comm'

    postfix += '}'
    return postfix


def get_param_str(name, param, param_idx):
    if param['name'] == 'ierror':
        return ''

    param_str = get_type_str(param)
    pointer_attr = get_pointer_attribute(param)
    param_str += pointer_attr

    param_str += get_param_name(param)
    param_str += get_param_postfix(name, param, param_idx, pointer_attr)

    return param_str


def get_func_signature(name):
    # Ignore callback functions (MPI_<name>_function)
    if data[name]['attributes']['callback'] == True:
        raise KeyError('Callback.')

    exclude_strings = ["c2f", "f2c", "f2f08", "f082c", "f082f", "typedef", "mpi_t_", "mpi_comm_spawn"]
    for xs in exclude_strings:
        if xs in name:
            raise KeyError('Ignore')


    signature = ''
    signature += BIG_C_KIND_MAP[data[name]['return_kind']]
    signature += ' '
    signature += data[name]['name']
    signature += '('
    signature += ', '.join([f'{get_const_attribute(p)}{get_param_str(name, p, i)}' for i, p in enumerate(data[name]['parameters']) if p['name'] not in  ['ierror', 'ierr']])
    signature += ');'

    return signature


def get_signatures():
    for func in func_names:
        try:
            print(get_func_signature(func.lower()))
        except KeyError:
            None


parser = argparse.ArgumentParser(description='Process some integers.')
parser.add_argument('--func-file', '-ff', type=str,
                    help='File with functions to consider, one function per line')

args = parser.parse_args()


with open ('apis.json', 'r+') as apis_file, open(f'{args.func_file}', 'r') as func_file:
    data = json.load(apis_file)
    func_names = func_file.read().splitlines()
    get_signatures()
