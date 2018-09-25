/*=====================================================================*
 *                                                                     *
 *   Software Name : ppohFEM                                           *
 *         Version : 1.0                                               *
 *                                                                     *
 *   License                                                           *
 *     This file is part of ppohFEM.                                   *
 *     ppohFEM is a free software, you can use it under the terms      *
 *     of The MIT License (MIT). See LICENSE file and User's guide     *
 *     for more details.                                               *
 *                                                                     *
 *   ppOpen-HPC project:                                               *
 *     Open Source Infrastructure for Development and Execution of     *
 *     Large-Scale Scientific Applications on Post-Peta-Scale          *
 *     Supercomputers with Automatic Tuning (AT).                      *
 *                                                                     *
 *   Organizations:                                                    *
 *     The University of Tokyo                                         *
 *       - Information Technology Center                               *
 *       - Atmosphere and Ocean Research Institute (AORI)              *
 *       - Interfaculty Initiative in Information Studies              *
 *         /Earthquake Research Institute (ERI)                        *
 *       - Graduate School of Frontier Science                         *
 *     Kyoto University                                                *
 *       - Academic Center for Computing and Media Studies             *
 *     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
 *                                                                     *
 *   Sponsorship:                                                      *
 *     Japan Science and Technology Agency (JST), Basic Research       *
 *     Programs: CREST, Development of System Software Technologies    *
 *     for post-Peta Scale High Performance Computing.                 *
 *                                                                     *
 *   Copyright (c) 2015 The University of Tokyo                        *
 *                       - Graduate School of Frontier Science         *
 *                                                                     *
 *=====================================================================*/




/*
	Index Sorting for FSTR
	2004.10.18 by N.Imai
	-------------------------
	[Fortran]
	integer(kind=4) :: index_data(2,:), n
	call fstr_sort_index( index_data, n )
*/



#include <stdlib.h>


static int my_comp( const void* a, const void* b)
{
	return (*(int*)a - *(int*)b);
}


void c_fstr_sort_index( int* index_data, int n)
{
	qsort( index_data, n, sizeof(int)*2, my_comp);
}



/*----------- Fortran Interface ---------------*/

void fstr_sort_index( int* index_data, int* n)
{
	c_fstr_sort_index( index_data, *n);
}

void fstr_sort_index_( int* index_data, int* n)
{
	c_fstr_sort_index( index_data, *n);
}

void fstr_sort_index__( int* index_data, int* n)
{
	c_fstr_sort_index( index_data, *n);
}

void FSTR_SORT_INDEX( int* index_data, int* n)
{
	c_fstr_sort_index( index_data, *n);
}

void FSTR_SORT_INDEX_( int* index_data, int* n)
{
	c_fstr_sort_index( index_data, *n);
}

void FSTR_SORT_INDEX__( int* index_data, int* n)
{
	c_fstr_sort_index( index_data, *n);
}

