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



#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define Table232_Size 6
#define Table342_Size 10
#define Table352_Size 15

/* for HECMW to FSTR */
int Table232[] = { 1,2,3,  6,4,5 };
int Table342[] = { 1,2,3,4, 7,5,6,  8,9,10 };
int Table352[] = { 1,2,3,4,5,6,  9,7,8,  12,10,11,  13,14,15 };
/*int Table232[] = { 1,2,3,  4,5,6 };
int Table342[] = { 1,2,3,4, 5,6,7,  8,9,10 };
int Table352[] = { 1,2,3,4,5,6,  7,8,9,  10,11,12,  13,14,15 };*/


#define MaxItemNumber 15


/*======================================================================*/
/*                                                                      */
/* HECMW to FSTR Mesh Data Converter                                    */
/* Convering Conectivity of Element Type 232, 342 and 352               */
/*                                                                      */
/*======================================================================*/


void c_hecmw2fstr_connect_conv( int n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	int i, j;
	int start_no;
	int type;
	int table_n;
	int* table;
	int* item;
	int buff[MaxItemNumber];

	for(i=0; i<n_elem; i++ ){
		type = elem_type[i];
		if( type != 232 && type != 2322 && type != 342 && type != 3422 && type != 352 )
			continue;

		switch(type){
		case 232:
        case 2322:
			table_n = Table232_Size;
			table = Table232;
			break;
		case 342:
		case 3422:
			table_n = Table342_Size;
			table = Table342;
			break;
		case 352:
			table_n = Table352_Size;
			table = Table352;
			break;
		}
		start_no = elem_node_index[i];
		item = &elem_node_item[start_no];
		memcpy( buff, item, sizeof(int)*table_n );

		for(j=0; j<table_n; j++){
			item[j] = buff[table[j]-1];
		}
	}
}


/*======================================================================*/
/*                                                                      */
/* FSTR to HECMW Mesh Data Converter                                    */
/* Convering Conectivity of Element Type 232, 342 and 352               */
/*                                                                      */
/*======================================================================*/


void c_fstr2hecmw_connect_conv( int n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	int i, j;
	int start_no;
	int type;
	int table_n;
	int* table;
	int* item;
	int buff[MaxItemNumber];

	for(i=0; i<n_elem; i++ ){
		type = elem_type[i];
		if( type != 232 && type != 2322 && type != 342 && type != 3422 && type != 352 )
			continue;

		switch(type){
		case 232:
        case 2322:
			table_n = Table232_Size;
			table = Table232;
			break;
		case 342:
		case 3422:
			table_n = Table342_Size;
			table = Table342;
			break;
		case 352:
			table_n = Table352_Size;
			table = Table352;
			break;
		}
		start_no = elem_node_index[i];
		item = &elem_node_item[start_no];
		memcpy( buff, item, sizeof(int)*table_n );

		for(j=0; j<table_n; j++){
			item[table[j]-1] = buff[j];
		}
	}
}


void c_fstr2hecmw_elem_conv( int type, int node[] )
{
	int j;
	int table_n;
	int* table;
	int buff[MaxItemNumber];

	if( type != 232 && type != 2322 && type != 342 && type != 3422 && type != 352 ) return;

	switch(type){
	case 232:
    case 2322:
		table_n = Table232_Size;
		table = Table232;
		break;
	case 342:
	case 3422:
		table_n = Table342_Size;
		table = Table342;
		break;
	case 352:
		table_n = Table352_Size;
		table = Table352;
		break;
	}
	memcpy( buff, node, sizeof(int)*table_n );

	for(j=0; j<table_n; j++){
		node[table[j]-1] = buff[j];
	}
}



/********************************************************************************************************/
/* Fortran Interface */

void hecmw2fstr_connect_conv( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_hecmw2fstr_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}

void hecmw2fstr_connect_conv_( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_hecmw2fstr_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}

void hecmw2fstr_connect_conv__( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_hecmw2fstr_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}

void HECMW2FSTR_CONNECT_CONV( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_hecmw2fstr_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}

void HECMW2FSTR_CONNECT_CONV_( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_hecmw2fstr_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}

void HECMW2FSTR_CONNECT_CONV__( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_hecmw2fstr_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}

/*---------------------------------------------------------------------------------------------------------*/


void fstr2hecmw_connect_conv( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_fstr2hecmw_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}

void fstr2hecmw_connect_conv_( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_fstr2hecmw_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}

void fstr2hecmw_connect_conv__( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_fstr2hecmw_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}

void FSTR2HECMW_CONNECT_CONV( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_fstr2hecmw_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}

void FSTR2HECMW_CONNECT_CONV_( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_fstr2hecmw_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}

void FSTR2HECMW_CONNECT_CONV__( int* n_elem, int elem_type[], int elem_node_index[], int elem_node_item[] )
{
	c_fstr2hecmw_connect_conv( *n_elem, elem_type, elem_node_index, elem_node_item );
}
