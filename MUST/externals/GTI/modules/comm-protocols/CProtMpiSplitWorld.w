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
 * @file CProtMpiSplitWorld.w
 * This is the source for the generated part included by CProtMpiSplitWorld.cpp. 
 */

static int gModuleInitialized = 0;

{{fn fn_name MPI_Init MPI_Init_thread}} {

  if( gModuleInitialized )
   return MPI_SUCCESS;
  
  gModuleInitialized = 1;
  int err;
  const char *inp;
  int split_mod;
  int num_sets, nodesize=0;
  int world_size, rank;
  int curr_set_start = 0;
  PNMPI_Service_descriptor_t service;
  realCommWorld = MPI_COMM_WORLD;
  char buf[512];
  PNMPI_modHandle_t stack=0;

  err = PNMPI_Service_GetModuleSelf(&split_mod);
  assert (err==PNMPI_SUCCESS);

  sprintf (buf,"stack_0");
  err=PNMPI_Service_GetArgument(split_mod,buf,&inp);
  assert (err==PNMPI_SUCCESS);
  err=PNMPI_Service_GetStackByName("level_0",&stack);
  assert (err==PNMPI_SUCCESS);



  /* Get self module */
  extern int X{{fn_name}}_NewStack ({{ list "int stack" {{formals}} }});
  err =  X{{fn_name}}_NewStack ({{ list stack {{args}} }});
//  err = {{callfn}}  

#ifdef MUST_TIME
  gettimeofday(&gStart, NULL);
#endif

  assert (err == MPI_SUCCESS);
  err = XMPI_Comm_size_NewStack(stack, MPI_COMM_WORLD, &world_size);
  assert (err == MPI_SUCCESS);
  err = XMPI_Comm_rank_NewStack(stack, MPI_COMM_WORLD, &rank);
  assert (err == MPI_SUCCESS);
  err = XMPI_Comm_dup_NewStack(stack, MPI_COMM_WORLD, &realCommWorld);
  assert (err == MPI_SUCCESS);

  //enable a sleep to allow attaching with a debugger
  if (getenv("MUST_WAIT_AT_STARTUP") != NULL)
  {
//        std::cout << "Rank " << rank << " has pid " << getpid() << std::endl;
// Stop random interleaving when printing
        printf("Rank %i has pid %i\n",rank,getpid());
	  sleep (atoi(getenv("MUST_WAIT_AT_STARTUP")));
  }

  // print out memory statistics if requested
  if((getenv("GTI_VERBOSE") != NULL &&
      atoi(getenv("GTI_VERBOSE")) > 0) ||
     (getenv("INTERNAL_GTI_PRINT_MEMORY_CONSUMPTION") != NULL &&
      atoi(getenv("INTERNAL_GTI_PRINT_MEMORY_CONSUMPTION")) == 1))
  {
      gPrintMemoryConsumption = true;
  }
  
  /*
   * EXPERIMENTAL, might be of use in the future. 
   * Idea is to automatically connect a gdbserver to some ranks to allow remote debugging
   * in unfriendly environments.
   */  
  if (getenv("MUST_START_GDBSERVER_BEGIN") != NULL && getenv("MUST_START_GDBSERVER_END") != NULL)
  {
  	  int begin = atoi(getenv("MUST_START_GDBSERVER_BEGIN"));
  	  int end = atoi(getenv("MUST_START_GDBSERVER_END"));
  
  	  if (rank >= begin && rank <= end)
  	  {  
	      char host[512];
	      gethostname(host, 512);
	      std::stringstream stream;
	      stream
	      	<< "gdbserver "
	      	<< host
	      	<< ":"
	      	<< rank + 10000
	      	<< " --attach "
	      	<< getpid ()
	      	<< " &"
	      	<< std::endl;
	      if (system (stream.str().c_str())==0)
	         std::cout << "GDBSERVER for rank " << rank << " listens on " << host << ":" << rank + 10000 << std::endl;
	      else
	         std::cout << "Starting GDBSERVER for rank " << rank << " failed!" << std::endl << "Command was: " << stream.str() << std::endl;
	  }
  }

  /* Query for nodesize */
  err=PNMPI_Service_GetArgument(split_mod,"nodesize",&inp);
  if (err==PNMPI_SUCCESS)
    nodesize=atoi(inp);

  /* Query for number of sets and their stacks */
  err=PNMPI_Service_GetArgument(split_mod,"num_sets",&inp);
  assert (err==PNMPI_SUCCESS);
  num_sets=atoi(inp);
  g_sets.resize(num_sets);

/*   int in_sets = 0; */
  int found_app=0;
  for (int i = 0; i < num_sets; i++)
  {
	  int  size;

	  sprintf (buf,"size_%d",i);
	  err=PNMPI_Service_GetArgument(split_mod,buf,&inp);
	  assert (err==PNMPI_SUCCESS);
	  size=atoi(inp);

	if(i>0){
	  sprintf (buf,"stack_%d",i);
	  err=PNMPI_Service_GetArgument(split_mod,buf,&inp);
	  assert (err==PNMPI_SUCCESS);
	  err=PNMPI_Service_GetStackByName(inp,&stack);
	  assert (err==PNMPI_SUCCESS);
	}

	/* Get the place for the set. */
	sprintf (buf, "place_%d",i);
        inp = PNMPI_Service_GetArgumentSelf(buf);
	assert(inp != NULL);
        g_sets[i].app_place=0;
	if (strncmp("mpi_place", inp, 10)==0)
	{
	  g_sets[i].mpi_place=1;
	  if (!found_app)
	  {
	    g_sets[i].app_place=1;
	    found_app=1;
	  }
	}
	else
	{
	  g_sets[i].mpi_place=0;
	  g_sets[i].set_comm = MPI_COMM_SELF;
	  continue;
	}

	  if( size > 0 )
            g_sets[i].start_rank = curr_set_start;
          else
            g_sets[i].start_rank = g_sets[0].start_rank;

      int last_rank = curr_set_start + size - 1;
      g_sets[i].in_set = 0;
	  g_sets[i].set_index = i;
	  g_sets[i].stack = stack;

      if (nodesize>0 && i<2 && num_sets>1)
      {/* for given nodesize, distribute the lower levels: 1 tool rank + nodesize-1 app ranks per node */
        if(i==0)
        {
            g_sets[0].start_rank=1;
            g_sets[0].size = size;
            /* one tool node per computenode */
            /* \lfloor size / (nodesize-1) \rfloor  */
            g_sets[1].size = (size - 1) / (nodesize - 1) + 1;
            last_rank = g_sets[0].size + g_sets[1].size;
            if (rank < last_rank && (rank % nodesize) != 0 && size > 0)
                g_sets[i].in_set = 1;
        }
        else /* i==1 */
        {
            g_sets[1].start_rank=0;
            assert(g_sets[i].size == size);
            if (rank < last_rank && (rank % nodesize) == 0)
                g_sets[i].in_set = 1;
        }
      }
      else /* old split behaviour for nodesize = 0 or higher levels */
      {
        g_sets[i].size = size;
        if (rank >= g_sets[i].start_rank && rank <= last_rank && size > 0)
            g_sets[i].in_set = 1;
      }

	  err = XMPI_Comm_split_NewStack (stack, MPI_COMM_WORLD, g_sets[i].in_set, 0, &g_sets[i].set_comm);
	  assert (err == MPI_SUCCESS);

/*	  //MUST-DDT Integration Begin
	  if (g_sets[i].in_set)
	  {
	    if (i == 0)
	    {
	      int myNewAppRank;
	      XMPI_Comm_rank_NewStack (stack, g_sets[i].set_comm, &myNewAppRank);
	  	  MUST_application_rank = myNewAppRank;
	  	}
	  	else
	  	{
	  	  MUST_application_rank = -1;
	  	}
	  }
      //MUST-DDT Integration END*/

	  curr_set_start += size;
	  assert (world_size >= curr_set_start); //Sets must require no more processes than available!
  }
  /* Query for mappings */
  err=PNMPI_Service_GetArgument(split_mod,"num_mappings",&inp);
  if (err==PNMPI_SUCCESS)
  {
	  int numMappings = atoi(inp);

	  for (int i = 0; i < numMappings; i++)
	  {
		  char buf[512];

		  sprintf (buf,"mapping%d",i);
		  err=PNMPI_Service_GetArgument(split_mod,buf,&inp);
		  assert (err==PNMPI_SUCCESS);

		  std::string mapping (inp);

		  int ownSetId, commId, setIdToUse;
		  std::string ownSetIdStr, commIdStr, setIdToUseStr;

		  //Own set id
		  size_t pos = mapping.find_first_of (':');
		  assert (pos != std::string::npos);
		  ownSetIdStr = mapping.substr (0, pos);

		  //comm id
		  size_t pos2 = mapping.find_first_of (':',pos+1);
		  assert (pos2 != std::string::npos);
		  commIdStr = mapping.substr (pos+1, pos2-pos-1);

		  //Set id to use
		  setIdToUseStr = mapping.substr (pos2+1, mapping.length()-pos2-1);

		  ownSetId = atoi (ownSetIdStr.c_str());
		  commId = atoi (commIdStr.c_str());
		  setIdToUse = atoi (setIdToUseStr.c_str());

		  g_mappings.insert (std::make_pair(std::make_pair(ownSetId, commId), setIdToUse));
	  }
  }

  /* Provide services to query for set information */
  sprintf(service.name,"SplitMod_getMySetSize");
  service.fct=(PNMPI_Service_Fct_t) getMySetSize;
  sprintf(service.sig,"p");
  err=PNMPI_Service_RegisterService(&service);
  assert (err == PNMPI_SUCCESS);

  sprintf(service.name,"SplitMod_getMySetComm");
  service.fct=(PNMPI_Service_Fct_t) getMySetComm;
  sprintf(service.sig,"p");
  err=PNMPI_Service_RegisterService(&service);
  assert (err == PNMPI_SUCCESS);

  sprintf(service.name,"SplitMod_getRealCommWorld");
  service.fct=(PNMPI_Service_Fct_t) getRealCommWorld;
  sprintf(service.sig,"p");
  err=PNMPI_Service_RegisterService(&service);
  assert (err == PNMPI_SUCCESS);

  sprintf(service.name,"SplitMod_getSetInfo");
  service.fct=(PNMPI_Service_Fct_t) getSetInfo;
  sprintf(service.sig,"ipp");
  err=PNMPI_Service_RegisterService(&service);
  assert (err == PNMPI_SUCCESS);

  /* Call MPI_Init for the correct stack */
  MUST_InitComplete (); //We are all set up and ready when now; MPI_Init was invoked and we decided who is application and who is tool
  for (int i = 0; i < num_sets; i++)
  {
	  if (/*!g_sets[i].app_place &&*/ g_sets[i].mpi_place && g_sets[i].in_set == 1)
	  {
		  //APPLICATION
		  if (/*i == 0 && 
		     getenv("GTI_MPI_SPLIT_NO_APPLICATION") == NULL*/ g_sets[i].app_place)
		  {
//		      err =  P{{fn_name}}_NewStack ({{ list g_sets[i].stack {{args}} }});
		      extern int X{{fn_name}} ( {{formals}} );
		      err =  X{{fn_name}} ( {{args}} );
			  assert (err == MPI_SUCCESS);
                      break;
		  }
		  //Tool Processes
		  else
		  {
			  /*Init the MPI tool place, when it returns finalize and exit*/
			  extern int X{{fn_name}}_NewStack ({{ list "int stack" {{formals}} }} );
			  err =  X{{fn_name}}_NewStack ({{ list g_sets[i].stack {{args}} }});
			  assert (err == MPI_SUCCESS);

			  if (gPrintMemoryConsumption)
			      printSetMemoryConsumption(i);

#ifdef MUST_TIME
			  gettimeofday(&gEnd, NULL);

                          if (rank == world_size -1)
			    std::cout << "Time Post Init - Pre Finalize of Tool " << rank << " (usec): " << ((gEnd.tv_sec * 1000000 + gEnd.tv_usec) - (gStart.tv_sec * 1000000 + gStart.tv_usec)) << std::endl;
#endif

              /*
               * This does not calls the real MPI_Finalize,
               * it just passes the finalize to all modules of
               * the stack, we must still call the actual finalize
               * afterwards.
               */
			  err = XMPI_Finalize_NewStack(g_sets[i].stack);
			  assert (err == MPI_SUCCESS);
			  exit(0);
		  }
	  }
  }

  return err;
} {{endfn}}
