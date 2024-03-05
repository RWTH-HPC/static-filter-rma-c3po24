/* This file is part of GTI (Generic Tool Infrastructure)
 *
 * Copyright (C)
 *  2008-2019 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2008-2019 Lawrence Livermore National Laboratories, United States of America
 *  2013-2023 RWTH Aachen University, Federal Republic of Germany
 *
 * See the LICENSE file in the package base directory for details
 */

/**
 * @file CProtMpiRenameWorld.w
 * This is the source for the generated part included by CProtMpiRenameWorld.cpp. 
 */

{{fn fn_name MPI_Init MPI_Init_thread}} {
  int err;
  err = {{callfn}}
  assert (err == MPI_SUCCESS);
    PNMPI_modHandle_t handle;
    PNMPI_Service_descriptor_t service;

    //We need to check whether MPI_COMM_WORLD was splited
    err = PNMPI_Service_GetModuleByName("split_processes", &handle);
    if (err == PNMPI_SUCCESS)
    {

        err = PNMPI_Service_GetServiceByName(handle, "SplitMod_getMySetComm", "p", &service);
        assert (err == PNMPI_SUCCESS);
        ((int(*)(void*)) service.fct) (&fakeComm);

    }
} {{endfn}}

/*
 * -------------------------------------------
 */
/*GENERATED FUNCTIONS*/
/*
 * -------------------------------------------
 */
/* automatically generated wrapper code */
/*
 * MPI_Attr_get and MPI_Attr_put are in this list since an MPI_Attr_get on a replaced MPI_COMM_WORLD may not provide
 * the default attributes that are attached with MPI_COMM_WORLD. Since we also do not wrap the MPI_Attr_put this
 * should be fine.
 */

{{fnalltype fn_name MPI_Comm MPI_Attr_get MPI_Comm_get_attr MPI_Attr_put MPI_Comm_set_attr MPI_Comm_delete_attr}} {
  {{apply_to_type MPI_Comm MACRO_MPI_Comm}}
WRAP_MPI_CALL_PREFIX
  {{ret_val}} = P{{fn_name}}({{args}});
WRAP_MPI_CALL_POSTFIX
}{{endfnalltype}}
