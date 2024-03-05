/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*
 *   Heat conduction demo program
 *
 *  solves the heat equation on a 2D grid
 *
 *  March 2009
 *  Matthias.Lieber@tu-dresden.de
 *  Tobias.Hilbrich@tu-dresden.de
 *
 *  Adapted: Jan 2013
 *
 *  Header for both OpenMP and serial version.
 */

#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* This type represents the grid and its description */
typedef struct {
    /* current theta array */
    double** theta;
    /* new theta array */
    double** thetanew;
    /* domain size (number of grid cells) in x and y */
    int xsize;
    int ysize;
    /* size of a grid cell */
    double dx;
    double dy;
    /* "heat equation constant" */
    double k;
} heatGrid;

void heatAllocate(heatGrid* grid, int xsize, int ysize);
void heatDeallocate(heatGrid* grid);
void heatInitialize(heatGrid* grid);
double heatInitFunc(double x);
void heatPrint(heatGrid* grid);
void heatTimestep(heatGrid* grid, double dt, double* dthetamax);
void heatBoundary(heatGrid* grid);
void heatTotalEnergy(heatGrid* grid, double* energy);
