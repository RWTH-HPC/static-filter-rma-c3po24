#!/bin/bash

filenames="mpi_specification.xml mpi_2_specification.xml mpi_3_specification_nbc.xml mpi_3_specification_rma.xml optional/mpi_specification_finalize_fin.xml"
#filenames="mpi_specification.xml"
#header_filenames="gen_api_mpi_standard_1_2.h"
header_filenames="gen_api_mpi_standard_3_nbc.h"
must_dir="MUST"

get_xml_names()
{
    grep -h -E -o 'name=\"MPI_\S*\"' $1 | sed -e 's/name=//g' -e 's/\"//g' | sort -u 
}

get_mapped_function_names()
{
    cd $must_dir/specifications
    get_xml_names "${filenames}"
}

get_funcs_from_header()
{
    grep -h -o -E 'MPI_\S*\(' $1 | tr -d '(' | sed -e 's/\s//g' | sort -u
}

get_funcs_from_mpi_header()
{
    grep -o 'MPI_[A-Z][a-z]\+_\S*\s\?(' $1 | tr -d '(,)' | sed -e 's/\s//g' | sort -u
}

get_exclusive_functions()
{
    comm -13 <(get_funcs_from_mpi_header $1) <(get_funcs_from_mpi_header $2)
}

get_shared_functions()
{
    comm -12 <(get_funcs_from_mpi_header $1) <(get_funcs_from_mpi_header $2)
}

get_mapped_function_names
#get_funcs_from_header "${header_filenames}"
#get_exclusive_functions "mpi31.h" "mpi4.h"
#get_shared_functions "mpi31.h" "mpi4.h"
