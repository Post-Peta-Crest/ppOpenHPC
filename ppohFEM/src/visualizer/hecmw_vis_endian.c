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

#include "hecmw_vis_endian.h"

static long _TestEndian=1;

int IsLittleEndian(void) {
	return *(char*)&_TestEndian;
}

/******************************************************************************
  FUNCTION: SwapEndian
  PURPOSE: Swap the byte order of a structure
  EXAMPLE: float F=123.456;; SWAP_FLOAT(F);
 ******************************************************************************/

void *SwapEndian(void* Addr, const int Nb) {
	static char Swapped[16];
	switch (Nb) {
	case 2:	Swapped[0]=*((char*)Addr+1);
	Swapped[1]=*((char*)Addr  );
	break;
	case 3:	/* As far as I know, 3 is used only with RGB images */
		Swapped[0]=*((char*)Addr+2);
		Swapped[1]=*((char*)Addr+1);
		Swapped[2]=*((char*)Addr  );
		break;
	case 4:	Swapped[0]=*((char*)Addr+3);
	Swapped[1]=*((char*)Addr+2);
	Swapped[2]=*((char*)Addr+1);
	Swapped[3]=*((char*)Addr  );
	break;
	case 8:	Swapped[0]=*((char*)Addr+7);
	Swapped[1]=*((char*)Addr+6);
	Swapped[2]=*((char*)Addr+5);
	Swapped[3]=*((char*)Addr+4);
	Swapped[4]=*((char*)Addr+3);
	Swapped[5]=*((char*)Addr+2);
	Swapped[6]=*((char*)Addr+1);
	Swapped[7]=*((char*)Addr  );
	break;
	case 16:Swapped[0]=*((char*)Addr+15);
	Swapped[1]=*((char*)Addr+14);
	Swapped[2]=*((char*)Addr+13);
	Swapped[3]=*((char*)Addr+12);
	Swapped[4]=*((char*)Addr+11);
	Swapped[5]=*((char*)Addr+10);
	Swapped[6]=*((char*)Addr+9);
	Swapped[7]=*((char*)Addr+8);
	Swapped[8]=*((char*)Addr+7);
	Swapped[9]=*((char*)Addr+6);
	Swapped[10]=*((char*)Addr+5);
	Swapped[11]=*((char*)Addr+4);
	Swapped[12]=*((char*)Addr+3);
	Swapped[13]=*((char*)Addr+2);
	Swapped[14]=*((char*)Addr+1);
	Swapped[15]=*((char*)Addr  );
	break;
	}
	return (void*)Swapped;
}


