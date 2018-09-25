/*=====================================================================*
 *                                                                     *
 *   Software Name : ppohVIS_base                                      *
 *         Version : 0.2.0                                             *
 *                                                                     *
 *   License                                                           *
 *     This file is part of ppohVIS_FDM3D                              *
 *     ppohVIS_FDM3D is a free software, you can use it under the      *
 *     termas of The MIT License (MIT). See LICENSE file and User's    *
 *     guide for more details.                                         *
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
 *       - Graduate School of Interdisciplinary Information Studies    *
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
 *   Copyright (c) 2013 <Kengo Nakajima, The University of Tokyo       *
 *                       nakajima(at)cc.u-tokyo.ac.jp           >      *
 *=====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <omp.h>

#include "ppohVIS_BASE_Config.h"
#include "ppohVIS_BASE_Error.h"
#include "ppohVIS_BASE_Mesh.h"
#include "ppohVIS_BASE_Geometry.h"
#include "ppohVIS_BASE_UCD.h"
#include "ppohVIS_BASE_Voxel.h"
#include "ppohVIS_BASE_InitialVoxel.h"
#include "ppohVIS_BASE_VoxelCount.h"
#include "ppohVIS_BASE_VoxelUtil.h"
#include "ppohVIS_BASE_VoxelUtilSearchInside.h"

#define PI (3.141592653589793238462)
#define EPS (1.0E-8)


/*******************************************************************************
 * Set voxel type
 ******************************************************************************/
/*
 * Point is inside of the voxel
 */
static int
CheckPointInside(
	double X1, double Y1, double Z1, double OX, double OY, double OZ,
	double PX, double PY, double PZ)
{
	if((OX <= X1 && X1 <= PX) &&
	   (OY <= Y1 && Y1 <= PY) &&
	   (OZ <= Z1 && Z1 <= PZ)) {
		return 1;
	} else {
		return 0;
	};
};


/*
 * Line passes through inside of the voxel
 */
static int
CheckPassThroughInside(
	double X1, double Y1, double Z1, double X2, double Y2, double Z2,
	double OX, double OY, double OZ, double PX, double PY, double PZ)
{
	double XA, XB, YA, YB, ZA, ZB, YC, ZC, YD, ZD;
	double DX12, DY12, DZ12, DYAB, DZAB;
	double t1, t2, t3, t4;

	/* Simple check */
	if(	((X1 < OX && X2 < OX) || (X1 > PX && X2 > PX)) ||
		((Y1 < OY && Y2 < OY) || (Y1 > PY && Y2 > PY)) ||
		((Z1 < OZ && Z2 < OZ) || (Z1 > PZ && Z2 > PZ))) {
		return 0;
	};

	DX12 = X1 - X2;
	DY12 = Y1 - Y2;
	DZ12 = Z1 - Z2;

	/* Point */
	if(abs(DX12) < PPOHVIS_BASE_EPS_METRIC &&
	   abs(DY12) < PPOHVIS_BASE_EPS_METRIC &&
	   abs(DZ12) < PPOHVIS_BASE_EPS_METRIC) {
		if((OX <= X1 && X1 <= PX) &&
		   (OY <= Y1 && Y1 <= PY) &&
		   (OZ <= Z1 && Z1 <= PZ)) {
			return 1;
		} else {
			return 0;
		};
	};

	/* Parallel to an axis */
	if(abs(DX12) < PPOHVIS_BASE_EPS_METRIC &&
	   abs(DY12) < PPOHVIS_BASE_EPS_METRIC) {	/* Z-axis */
		if((OX <= X1 && X1 <= PX) && (OY <= Y1 && Y1 <= PY)) {
			return 1;
		} else {
			return 0;
		};

	} else if(abs(DY12) < PPOHVIS_BASE_EPS_METRIC &&
	          abs(DZ12) < PPOHVIS_BASE_EPS_METRIC) {	/* X-axis */
		if((OY <= Y1 && Y1 <= PY) && (OZ <= Z1 && Z1 <= PZ)) {
			return 1;
		} else {
			return 0;
		};

	} else if(abs(DZ12) < PPOHVIS_BASE_EPS_METRIC &&
	          abs(DX12) < PPOHVIS_BASE_EPS_METRIC) {	/* Y-axis */
		if((OZ <= Z1 && Z1 <= PZ) && (OX <= X1 && X1 <= PX)) {
			return 1;
		} else {
			return 0;
		};
	};

	/* Parallel to a surface */
	if(abs(DX12) < PPOHVIS_BASE_EPS_METRIC) {
		YA = OY;
		YB = PY;
		t1 = (YA - Y2) / DY12;
		t2 = (YB - Y2) / DY12;
		ZA = DZ12 * t1 + Z2;
		ZB = DZ12 * t2 + Z2;

		if((ZA < OZ && ZB < OZ) || (ZA > PZ && ZB > PZ)) {
			return 0;
		} else {
			return 1;
		};

	} else if(abs(DY12) < PPOHVIS_BASE_EPS_METRIC) {
		ZA = OZ;
		ZB = PZ;
		t1 = (ZA - Z2) / DZ12;
		t2 = (ZB - Z2) / DZ12;
		XA = DX12 * t1 + X2;
		XB = DX12 * t2 + X2;

		if((XA < OX && XB < OX) || (XA > PX && XB > PX)) {
			return 0;
		} else {
			return 1;
		};

	} else if(abs(DZ12) < PPOHVIS_BASE_EPS_METRIC) {
		XA = OX;
		XB = PX;
		t1 = (XA - X2) / DX12;
		t2 = (XB - X2) / DX12;
		YA = DY12 * t1 + Y2;
		YB = DY12 * t2 + Y2;

		if((YA < OY && YB < OY) || (YA > PY && YB > PY)) {
			return 0;
		} else {
			return 1;
		};
	};

	/* Others */
	XA = OX;
	XB = PX;
	t1 = (XA - X2) / DX12;
	t2 = (XB - X2) / DX12;
	YA = DY12 * t1 + Y2;
	YB = DY12 * t2 + Y2;
	ZA = DZ12 * t1 + Z2;
	ZB = DZ12 * t2 + Z2;

	DYAB = YA - YB;
	DZAB = ZA - ZB;

	YC = OY;
	YD = PY;
	t3 = (YC - YB) / DYAB;
	t4 = (YD - YB) / DYAB;
	ZC = DZAB * t3 + ZB;
	ZD = DZAB * t4 + ZB;

	if((ZC < OZ && ZD < OZ) || (ZC > PZ && ZD > PZ)) {
		return 0;
	} else {
		return 1;
	};
};


/*
 *
 */
static int
CheckPointOnTriangle(
	double X0, double Y0, double Z0, double X1, double Y1, double Z1,
	double X2, double Y2, double Z2, double XP, double YP, double ZP)
{
	double P0X, P0Y, P0Z, P1X, P1Y, P1Z, P2X, P2Y, P2Z;
	double VL0, VL1, VL2;
	double DP01, DP12, DP20;
	double COS01, COS12, COS20;
	double T01, T12, T20, T;

	/* Point-P on vertex */
	if((XP == X0 && YP == Y0 && ZP == Z0) ||
	   (XP == X1 && YP == Y1 && ZP == Z1) ||
	   (XP == X2 && YP == Y2 && ZP == Z2)) {
		return 1;
	};

	/* Vector from Point-P to vertex */
	P0X = X0 - XP; P0Y = Y0 - YP; P0Z = Z0 - ZP;
	P1X = X1 - XP; P1Y = Y1 - YP; P1Z = Z1 - ZP;
	P2X = X2 - XP; P2Y = Y2 - YP; P2Z = Z2 - ZP;

	/* Length of vector */
	VL0 = sqrt(P0X * P0X + P0Y * P0Y + P0Z * P0Z);
	VL1 = sqrt(P1X * P1X + P1Y * P1Y + P1Z * P1Z);
	VL2 = sqrt(P2X * P2X + P2Y * P2Y + P2Z * P2Z);

	/* Dot product */
	DP01 = P0X * P1X + P0Y * P1Y + P0Z * P1Z;
	DP12 = P1X * P2X + P1Y * P2Y + P1Z * P2Z;
	DP20 = P2X * P0X + P2Y * P0Y + P2Z * P0Z;

	/* Cosine between two vectors */
	COS01 = DP01 / (VL0 * VL1);
	COS12 = DP12 / (VL1 * VL2);
	COS20 = DP20 / (VL2 * VL0);

	if(COS01 > +1.0) COS01 = +1.0;
	if(COS01 < -1.0) COS01 = -1.0;
	if(COS12 > +1.0) COS12 = +1.0;
	if(COS12 < -1.0) COS12 = -1.0;
	if(COS20 > +1.0) COS20 = +1.0;
	if(COS20 < -1.0) COS20 = -1.0;

	/* Angle[rad] between two vectors */
	T01 = acos(COS01);
	T12 = acos(COS12);
	T20 = acos(COS20);

	/* Sum of angles [rad] */
	T = T01 + T12 + T20;

	/* Point-P inside triangle or not */
	if(abs(T - 2.0 * PI) < EPS) {
		if(abs(T01 - PI) < EPS ||
		   abs(T12 - PI) < EPS ||
		   abs(T20 - PI) < EPS) {
			return 2;
		} else {
			return 1;
		};
	} else {
		return 0;
	};
};


static int
CheckPointOnLine2D(
	double X0, double Y0, double X1, double Y1,
	double X2, double Y2, double XP, double YP)
{
	double XA, YA, XB, YB;
	double ABX, ABY, APX, APY;
	double VLAB, VLAP;
	double DP;
	double COS;
	double T;

	/* Bounding box */
	XA = X0; YA = Y0;
	XB = X0; YB = Y0;
	if(X1 < XA) {
		XA = X1; YA = Y1;
	};
	if(X2 < XA) {
		XA = X2; YA = Y2;
	};
	if(X1 > XB) {
		XB = X1; YB = Y1;
	};
	if(X2 > XB) {
		XB = X2; YB = Y2;
	};

	/* Outside of bounding box */
	if(XP < XA || XB < XP || YP < YA || YB < YP) return 0;

	/* On Vertex */
	if((XP == XA && YP == YA) || (XP == XB && YP == YB)) return 1;

	/* Vector from Point-A to Point-B,P */
	ABX = XB - XA;
	ABY = YB - YA;

	APX = XP - XA;
	APY = YP - YA;

	/* Length of vector */
	VLAB = sqrt(ABX * ABX + ABY * ABY);
	VLAP = sqrt(APX * APX + APY * APY);

	/* Dot product */
	DP = ABX * APX + ABY * APY;

	/* Cosine between two vectors */
	COS = DP / (VLAB * VLAP);

	if(COS > +1.0) COS = +1.0;
	if(COS < -1.0) COS = -1.0;

	/* Angle[rad] between two vectors */
	T = acos(COS);

	/* Point-P on edge or not */
	if(abs(T) < EPS) {
		return 1;
	} else {
		return 0;
	};
};


static int
CheckCrossTriangle(
	double XO, double YO, double ZO, double XA, double YA, double ZA,
	double XB, double YB, double ZB, double X1, double Y1)
{
	double OAX, OAY, OAZ, OBX, OBY, OBZ;
	double VNX, VNY, VNZ;
	double Z1;
	int Rc;

	/* Vector from Point-O to Point-A,B */
	OAX = XA - XO;
	OAY = YA - YO;
	OAZ = ZA - ZO;

	OBX = XB - XO;
	OBY = YB - YO;
	OBZ = ZB - ZO;

	/* Cross product */
	VNX = OAY * OBZ - OBY * OAZ;
	VNY = OAZ * OBX - OBZ * OAX;
	VNZ = OAX * OBY - OBX * OAY;

	if(VNZ != 0.0) {
		Z1 = ZO + (VNX * (XO - X1) + VNY * (YO - Y1)) / VNZ;
		Rc = CheckPointOnTriangle(
			XO, YO, ZO, XA, YA, ZA, XB, YB, ZB, X1, Y1, Z1);
		if(Rc) {
			return Rc;
		} else {
			return 0;
		};
	} else {
		if(CheckPointOnLine2D(XO, YO, XA, YA, XB, YB, X1, Y1)) {
			return 1;
		} else {
			return 0;
		};
	};
};


static int
GetZDepth(
	double XO, double YO, double ZO, double XA, double YA, double ZA,
	double XB, double YB, double ZB, double X1, double Y1, double Z1)
{
	double OAX, OAY, OAZ, OBX, OBY, OBZ;
	double VNX, VNY, VNZ;
	double ZP;
	int Rc;

	/* Vector from Point-O to Point-A,B */
	OAX = XA - XO;
	OAY = YA - YO;
	OAZ = ZA - ZO;

	OBX = XB - XO;
	OBY = YB - YO;
	OBZ = ZB - ZO;

	/* Cross product */
	VNX = OAY * OBZ - OBY * OAZ;
	VNY = OAZ * OBX - OBZ * OAX;
	VNZ = OAX * OBY - OBX * OAY;

	if(VNZ != 0.0) {
		ZP = ZO + (VNX * (XO - X1) + VNY * (YO - Y1)) / VNZ;
		Rc = CheckPointOnTriangle(XO, YO, ZO, XA, YA, ZA,
		                          XB, YB, ZB, X1, Y1, ZP);
		if(Rc) {
			if(ZP > Z1) {
				if(Rc == 1) {
					return 1;
				} else {
					return 2;
				};
			} else if(ZP < Z1) {
				if(Rc == 1) {
					return -1;
				} else {
					return -2;
				};
			} else {
				return 0;
			};
		} else {
			return 0;
		};
	} else {
		if(CheckPointOnLine2D(XO, YO, XA, YA, XB, YB, X1, Y1)) {
			if(ZO > Z1) {
				return 2;
			} else if(ZO < Z1) {
				return -2;
			} else {
				return 0;
			};
		} else {
			return 0;
		};
	};
};


static int
GetInside(
	double XO, double YO, double ZO, double XA, double YA, double ZA,
	double XB, double YB, double ZB,
	double X1, double Y1, double ZMin, double ZMax)
{
	double OAX, OAY, OAZ, OBX, OBY, OBZ;
	double VNX, VNY, VNZ;
	double ZP;
	int Rc;

	/* Vector from Point-O to Point-A,B */
	OAX = XA - XO;
	OAY = YA - YO;
	OAZ = ZA - ZO;

	OBX = XB - XO;
	OBY = YB - YO;
	OBZ = ZB - ZO;

	/* Cross product */
	VNX = OAY * OBZ - OBY * OAZ;
	VNY = OAZ * OBX - OBZ * OAX;
	VNZ = OAX * OBY - OBX * OAY;

	if(VNZ != 0.0) {
		ZP = ZO + (VNX * (XO - X1) + VNY * (YO - Y1)) / VNZ;
		Rc = CheckPointOnTriangle(XO, YO, ZO, XA, YA, ZA,
		                          XB, YB, ZB, X1, Y1, ZP);
		if(Rc) {
			if(ZMin <= ZP && ZP <= ZMax) {
				return 1;
			} else {
				return 0;
			};
		} else {
			return 0;
		};
	} else {
		if(CheckPointOnLine2D(XO, YO, XA, YA, XB, YB, X1, Y1)) {
			if(ZMin <= ZO && ZO <= ZMax) {
				return 1;
			} else {
				return 0;
			};
		} else {
			return 0;
		};
	};
};


/*
 * Mask free surfaces which pass through inside of the voxel
 */
/*
 * ##### currently not used #####
 *
static int
MaskPassThrough(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf, int *TsufMask)
{
	double OX, OY, OZ, PX, PY, PZ;
	double X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3;
	int Node1, Node2, Node3;
	int iTsuf;

	OX = pVoxel->OX;
	OY = pVoxel->OY;
	OZ = pVoxel->OZ;
	PX = OX + pVoxel->DX;
	PY = OY + pVoxel->DY;
	PZ = OZ + pVoxel->DZ;

	for(iTsuf=0; iTsuf<pFreeSurf->Count; iTsuf++) {
		if(TsufMask[iTsuf] == 1) {
			Node1 = pFreeSurf->Node[3*iTsuf];
			Node2 = pFreeSurf->Node[3*iTsuf+1];
			Node3 = pFreeSurf->Node[3*iTsuf+2];

			X1 = pMesh->Node->Coords[3*Node1-3];
			Y1 = pMesh->Node->Coords[3*Node1-2];
			Z1 = pMesh->Node->Coords[3*Node1-1];

			X2 = pMesh->Node->Coords[3*Node2-3];
			Y2 = pMesh->Node->Coords[3*Node2-2];
			Z2 = pMesh->Node->Coords[3*Node2-1];

			X3 = pMesh->Node->Coords[3*Node3-3];
			Y3 = pMesh->Node->Coords[3*Node3-2];
			Z3 = pMesh->Node->Coords[3*Node3-1];


			if(CheckPointInside(X1, Y1, Z1, OX, OY, OZ,
			                    PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(CheckPointInside(X2, Y2, Z2, OX, OY, OZ,
			                    PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(CheckPointInside(X3, Y3, Z3, OX, OY, OZ,
			                    PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};

			if(CheckPassThroughInside(X1, Y1, Z1, X2, Y2, Z2,
			                          OX, OY, OZ, PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(CheckPassThroughInside(X2, Y2, Z2, X3, Y3, Z3,
			                          OX, OY, OZ, PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(CheckPassThroughInside(X3, Y3, Z3, X1, Y1, Z1,
			                          OX, OY, OZ, PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};

			if(CheckCrossTriangle(X1, Y1, Z1, X2, Y2, Z2,
			                      X3, Y3, Z3, OX, OY)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(CheckCrossTriangle(X1, Y1, Z1, X2, Y2, Z2,
			                      X3, Y3, Z3, PX, OY)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(CheckCrossTriangle(X1, Y1, Z1, X2, Y2, Z2,
			                      X3, Y3, Z3, OX, PY)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(CheckCrossTriangle(X1, Y1, Z1, X2, Y2, Z2,
			                      X3, Y3, Z3, PX, PY)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
		};
	};

	return 0;
};
*/


/*
 * Mask free surfaces which pass through inside of the voxel
 */
static int
MaskInside(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf, int *TsufMask)
{
	double OX, OY, OZ, PX, PY, PZ;
	double X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3;
	int Node1, Node2, Node3;
	int iTsuf;

	OX = pVoxel->OX;
	OY = pVoxel->OY;
	OZ = pVoxel->OZ;
	PX = OX + pVoxel->DX;
	PY = OY + pVoxel->DY;
	PZ = OZ + pVoxel->DZ;

#pragma omp parallel for private(Node1,Node2,Node3,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3)
	for(iTsuf=0; iTsuf<pFreeSurf->Count; iTsuf++) {
		if(TsufMask[iTsuf] == 1) {
			Node1 = pFreeSurf->Node[3*iTsuf];
			Node2 = pFreeSurf->Node[3*iTsuf+1];
			Node3 = pFreeSurf->Node[3*iTsuf+2];

			X1 = pMesh->Node->Coords[3*Node1-3];
			Y1 = pMesh->Node->Coords[3*Node1-2];
			Z1 = pMesh->Node->Coords[3*Node1-1];

			X2 = pMesh->Node->Coords[3*Node2-3];
			Y2 = pMesh->Node->Coords[3*Node2-2];
			Z2 = pMesh->Node->Coords[3*Node2-1];

			X3 = pMesh->Node->Coords[3*Node3-3];
			Y3 = pMesh->Node->Coords[3*Node3-2];
			Z3 = pMesh->Node->Coords[3*Node3-1];


			if(CheckPointInside(X1, Y1, Z1, OX, OY, OZ,
			                    PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(CheckPointInside(X2, Y2, Z2, OX, OY, OZ,
			                    PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(CheckPointInside(X3, Y3, Z3, OX, OY, OZ,
			                    PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};

			if(CheckPassThroughInside(X1, Y1, Z1, X2, Y2, Z2,
			                          OX, OY, OZ, PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(CheckPassThroughInside(X2, Y2, Z2, X3, Y3, Z3,
			                          OX, OY, OZ, PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(CheckPassThroughInside(X3, Y3, Z3, X1, Y1, Z1,
			                          OX, OY, OZ, PX, PY, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};

			if(GetInside(X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
			             OX, OY, OZ, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(GetInside(X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
			             PX, OY, OZ, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(GetInside(X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
			             OX, PY, OZ, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
			if(GetInside(X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
			             PX, PY, OZ, PZ)) {
				TsufMask[iTsuf] = 2;
				continue;
			};
		};
	};

	return 0;
};


/*
 * Search triangular (free) surfaces which pass through inside the voxel
 */
extern int
ppohVIS_BASE_SearchFreeSurfaceInsideByParent(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf, int *TsufMask)
{
	struct ppohVIS_BASE_stVoxelItem *pChild;
	int TsufID;
	int iCount, iTsuf, i;

	if(pVoxel->FreeSurfaceCount <= 0) { return 0; };
	if(pVoxel->Children == NULL) { return 0; };

	for(i=0; i<8; i++) {
		pChild = pVoxel->Children[i];

		memset(TsufMask, 0, pFreeSurf->Count*sizeof(int));
		for(iTsuf=0; iTsuf<pVoxel->FreeSurfaceCount; iTsuf++) {
			TsufID = pVoxel->FreeSurfaceID[iTsuf];
			TsufMask[TsufID-1] = 1;
		};

/*		MaskPassThrough(pChild, pMesh, pFreeSurf, TsufMask); */
		MaskInside(pChild, pMesh, pFreeSurf, TsufMask);

		iCount = 0;
		for(iTsuf=0; iTsuf<pFreeSurf->Count; iTsuf++) {
			if(TsufMask[iTsuf] == 2) iCount++;
		};

		pChild->FreeSurfaceCount = iCount;

		if(iCount > 0) {
			pChild->FreeSurfaceID = (int *)calloc(iCount, sizeof(int));
			if(pChild->FreeSurfaceID == NULL) {
				ppohVIS_BASE_SetError(
					ppohVIS_BASE_ErrorCode_AllocationError,
					 "%s [%s]",
					strerror(errno), "free surface ID");
				goto error;
			};

			iCount = 0;
			for(iTsuf=0; iTsuf<pVoxel->FreeSurfaceCount; iTsuf++) {
				TsufID = pVoxel->FreeSurfaceID[iTsuf];
				if(TsufMask[TsufID-1] == 2) {
					pChild->FreeSurfaceID[iCount] = TsufID;
					iCount++;
				};
			};
		} else {
			pChild->FreeSurfaceID = NULL;
		};
	};

	return 0;

error:
	return 1;
};


/*
 *
 */
extern int
ppohVIS_BASE_SearchFreeSurfaceInside(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf, int *TsufMask)
{
	int iCount, iTsuf;

	MaskInside(pVoxel, pMesh, pFreeSurf, TsufMask);

	iCount = 0;
#pragma omp parallel for
	for(iTsuf=0; iTsuf<pFreeSurf->Count; iTsuf++) {
		if(TsufMask[iTsuf] == 2) iCount++;
	};

	pVoxel->FreeSurfaceCount = iCount;

	if(iCount > 0) {
		pVoxel->FreeSurfaceID = (int *)calloc(iCount, sizeof(int));
		if(pVoxel->FreeSurfaceID == NULL) {
			ppohVIS_BASE_SetError(
				ppohVIS_BASE_ErrorCode_AllocationError,
				"%s [%s]", strerror(errno), "free surface ID");
			goto error;
		};

		iCount = 0;
		for(iTsuf=0; iTsuf<pFreeSurf->Count; iTsuf++) {
			if(TsufMask[iTsuf] == 2) {
				pVoxel->FreeSurfaceID[iCount] = iTsuf + 1;
				iCount++;
			};
		};
	} else {
		pVoxel->FreeSurfaceID = NULL;
	};

	return 0;

error:
	return -1;
};


/*******************************************************************************
 *
 ******************************************************************************/
/* Mask candidate free surfaces */
static int
MaskCandidateFreeSurface(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf, int *TsufMask)
{
	struct ppohVIS_BASE_stVoxelItem *pRoot, *pRootZ;
	int Tsuf;
	int NX, NY, NZ;
	int iX, iY, iZ, iPosX, iPosY, iPosZ, i, iTsuf;

	pRoot = pVoxel;
	for(;;) {
		if(pRoot->Level == 1) break;
		pRoot = pRoot->Parent;
	};


	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	iPosX = -1;
	iPosY = -1;
	iPosZ = -1;
	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;

		if(&pIniVoxel->Voxels[i] == pRoot) {
			iPosX = iX;
			iPosY = iY;
			iPosZ = iZ;
			break;
		};
	};
	};
	};

	if(iPosX < 0 || iPosY < 0 || iPosZ < 0) {
		fprintf(stderr, "Warning: Cannot Find Root Voxel\n");
		goto error;
	};

	memset(TsufMask, 0, pFreeSurf->Count*sizeof(int));
	for(iZ=0; iZ<NZ; iZ++) {
		i = NX * NY * iZ + NX * iPosY + iPosX;

		pRootZ = &pIniVoxel->Voxels[i];
		if(iZ < iPosZ) {
			for(iTsuf=0; iTsuf<pRootZ->FreeSurfaceCount; iTsuf++) {
				Tsuf = pRootZ->FreeSurfaceID[iTsuf];
				TsufMask[Tsuf-1] = -1;
			};
		} else if(iZ > iPosZ) {
			for(iTsuf=0; iTsuf<pRootZ->FreeSurfaceCount; iTsuf++) {
				Tsuf = pRootZ->FreeSurfaceID[iTsuf];
				TsufMask[Tsuf-1] = 1;
			};
		} else {
			for(iTsuf=0; iTsuf<pRootZ->FreeSurfaceCount; iTsuf++) {
				Tsuf = pRootZ->FreeSurfaceID[iTsuf];
				TsufMask[Tsuf-1] = 2;
			};
		};
	};

	return 0;

error:
	return -1;
};


static void
PrintCross(
	FILE *fp, int iTsuf, int iRc, int Node1, int Node2, int Node3,
	double X1, double Y1, double Z1, double X2, double Y2, double Z2,
	double X3, double Y3, double Z3, double PX, double PY, double PZ)
{
	fprintf(fp, "  Bound: %d, %d\n", iTsuf, iRc);
	fprintf(fp, "    Node1: %d, %le, %le, %le\n", Node1, X1, Y1, Z1);
	fprintf(fp, "    Node2: %d, %le, %le, %le\n", Node2, X2, Y2, Z2);
	fprintf(fp, "    Node3: %d, %le, %le, %le\n", Node3, X3, Y3, Z3);
	fprintf(fp, "    Point:     %le, %le, %le\n",        PX, PY, PZ);
};


static enum ppohVIS_BASE_eVoxelType
CheckZDepth(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf)
{
	int *TsufMask = NULL;
	double PX, PY, PZ, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3;
	int Node1, Node2, Node3;
	int UpperCount, LowerCount, Rc;
	int UpperOnEdge, LowerOnEdge;
	int ModUpper, ModLower;
	int iTsuf;


	TsufMask = (int *)calloc(pFreeSurf->Count, sizeof(int));
	if(TsufMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]",
			strerror(errno), "mask array for triangular surface");
		goto error;
	};


	MaskCandidateFreeSurface(pIniVoxel, pVoxel, pFreeSurf, TsufMask);

	PX = pVoxel->OX + pVoxel->DX * 0.5;
	PY = pVoxel->OY + pVoxel->DY * 0.5;
	PZ = pVoxel->OZ + pVoxel->DZ * 0.5;


	UpperCount = 0;
	LowerCount = 0;
	UpperOnEdge = 0;
	LowerOnEdge = 0;
	for(iTsuf=0; iTsuf<pFreeSurf->Count; iTsuf++) {
		if(TsufMask[iTsuf] == -1) {
			Node1 = pFreeSurf->Node[3*iTsuf];
			Node2 = pFreeSurf->Node[3*iTsuf+1];
			Node3 = pFreeSurf->Node[3*iTsuf+2];

			X1 = pMesh->Node->Coords[3*Node1-3];
			Y1 = pMesh->Node->Coords[3*Node1-2];
			Z1 = pMesh->Node->Coords[3*Node1-1];

			X2 = pMesh->Node->Coords[3*Node2-3];
			Y2 = pMesh->Node->Coords[3*Node2-2];
			Z2 = pMesh->Node->Coords[3*Node2-1];

			X3 = pMesh->Node->Coords[3*Node3-3];
			Y3 = pMesh->Node->Coords[3*Node3-2];
			Z3 = pMesh->Node->Coords[3*Node3-1];

			Rc = CheckCrossTriangle(X1, Y1, Z1, X2, Y2, Z2,
			                        X3, Y3, Z3, PX, PY);
			if(Rc == 1) {
				LowerCount++;
				PrintCross(stderr, iTsuf, Rc,
				           Node1, Node2, Node3,
				           X1, Y1, Z1, X2, Y2, Z2,
				           X3, Y3, Z3, PX, PY, PZ);
			} else if(Rc == 2) {
				LowerOnEdge++;
				PrintCross(stderr, iTsuf, Rc,
				           Node1, Node2, Node3,
				           X1, Y1, Z1, X2, Y2, Z2,
				           X3, Y3, Z3, PX, PY, PZ);
			};

		} else if(TsufMask[iTsuf] == 1) {
			Node1 = pFreeSurf->Node[3*iTsuf];
			Node2 = pFreeSurf->Node[3*iTsuf+1];
			Node3 = pFreeSurf->Node[3*iTsuf+2];

			X1 = pMesh->Node->Coords[3*Node1-3];
			Y1 = pMesh->Node->Coords[3*Node1-2];
			Z1 = pMesh->Node->Coords[3*Node1-1];

			X2 = pMesh->Node->Coords[3*Node2-3];
			Y2 = pMesh->Node->Coords[3*Node2-2];
			Z2 = pMesh->Node->Coords[3*Node2-1];

			X3 = pMesh->Node->Coords[3*Node3-3];
			Y3 = pMesh->Node->Coords[3*Node3-2];
			Z3 = pMesh->Node->Coords[3*Node3-1];

			Rc = CheckCrossTriangle(X1, Y1, Z1, X2, Y2, Z2,
			                        X3, Y3, Z3, PX, PY);
			if(Rc == 1) {
				UpperCount++;
				PrintCross(stderr, iTsuf, Rc,
				           Node1, Node2, Node3,
				           X1, Y1, Z1, X2, Y2, Z2,
				           X3, Y3, Z3, PX, PY, PZ);
			} else if(Rc == 2) {
				UpperOnEdge++;
				PrintCross(stderr, iTsuf, Rc,
				           Node1, Node2, Node3,
				           X1, Y1, Z1, X2, Y2, Z2,
				           X3, Y3, Z3, PX, PY, PZ);
			};

		} else if(TsufMask[iTsuf] == 2) {
			Node1 = pFreeSurf->Node[3*iTsuf];
			Node2 = pFreeSurf->Node[3*iTsuf+1];
			Node3 = pFreeSurf->Node[3*iTsuf+2];

			X1 = pMesh->Node->Coords[3*Node1-3];
			Y1 = pMesh->Node->Coords[3*Node1-2];
			Z1 = pMesh->Node->Coords[3*Node1-1];

			X2 = pMesh->Node->Coords[3*Node2-3];
			Y2 = pMesh->Node->Coords[3*Node2-2];
			Z2 = pMesh->Node->Coords[3*Node2-1];

			X3 = pMesh->Node->Coords[3*Node3-3];
			Y3 = pMesh->Node->Coords[3*Node3-2];
			Z3 = pMesh->Node->Coords[3*Node3-1];

			Rc = GetZDepth(X1, Y1, Z1, X2, Y2, Z2,
			               X3, Y3, Z3, PX, PY, PZ);
			if(Rc == -1) {
				LowerCount++;
				PrintCross(stderr, iTsuf, Rc,
				           Node1, Node2, Node3,
				           X1, Y1, Z1, X2, Y2, Z2,
				           X3, Y3, Z3, PX, PY, PZ);
			} else if(Rc == -2) {
				LowerOnEdge++;
				PrintCross(stderr, iTsuf, Rc,
				           Node1, Node2, Node3,
				           X1, Y1, Z1, X2, Y2, Z2,
				           X3, Y3, Z3, PX, PY, PZ);
			} else if(Rc == 1) {
				UpperCount++;
				PrintCross(stderr, iTsuf, Rc,
				           Node1, Node2, Node3,
				           X1, Y1, Z1, X2, Y2, Z2,
				           X3, Y3, Z3, PX, PY, PZ);
			} else if(Rc == 2) {
				UpperOnEdge++;
				PrintCross(stderr, iTsuf, Rc,
				           Node1, Node2, Node3,
				           X1, Y1, Z1, X2, Y2, Z2,
				           X3, Y3, Z3, PX, PY, PZ);
			};
		};
	};

	if(LowerOnEdge % 2 == 0) {
		LowerCount += LowerOnEdge / 2;
	} else {
		LowerCount += (LowerOnEdge + 1) / 2;
	};
	if(UpperOnEdge % 2 == 0) {
		UpperCount += UpperOnEdge / 2;
	} else {
		UpperCount += (UpperOnEdge + 1) / 2;
	};

	if(LowerCount == 0 || UpperCount == 0) {
		return ppohVIS_BASE_VoxelOuter;
	} else {
		ModUpper = UpperCount % 2;
		ModLower = LowerCount % 2;

		if(ModUpper == 0 && ModLower == 0) {
			return ppohVIS_BASE_VoxelOuter;
		} else {
			return ppohVIS_BASE_VoxelInner;
		};	
	};

error:
	if(TsufMask) {
		free(TsufMask);
		TsufMask = NULL;
	};

	return (enum ppohVIS_BASE_eVoxelType)(-1);
};


extern int
ppohVIS_BASE_CheckVoxelType(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf)
{
	enum ppohVIS_BASE_eVoxelType Type;
	int NX, NY, NZ;
	int iX, iY, iZ, i;

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;

		if(pIniVoxel->Voxels[i].Type == ppohVIS_BASE_VoxelUnknown) {
			Type = CheckZDepth(pIniVoxel, &pIniVoxel->Voxels[i],
			                   pMesh, pFreeSurf);
			pIniVoxel->Voxels[i].Type = Type;
		};
		if(Type == ppohVIS_BASE_VoxelInner) {
			fprintf(stderr, "  Result: INNER\n");
		} else if(Type == ppohVIS_BASE_VoxelOuter) {
			fprintf(stderr, "  Result: OUTER\n");
		} else if(Type == ppohVIS_BASE_VoxelBoundary) {
			fprintf(stderr, "  Result: BOUNDARY\n");
		} else {
			fprintf(stderr, "  Result: UNKNOWN\n");
		};
	};
	};
	};

	return 0;
};



static int
SetVoxelType(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf, int *TsufMask)
{
	int i;

	if(pVoxel == NULL) { return 0; };

	if(pVoxel->NodeCount > 0 ||
	   pVoxel->ElementCount > 0 ||
	   pVoxel->FreeSurfaceCount > 0) {
		if(pVoxel->FreeSurfaceCount > 0) {
			pVoxel->Type = ppohVIS_BASE_VoxelBoundary;
		} else {
			pVoxel->Type = ppohVIS_BASE_VoxelInner;
		};
	} else {
		pVoxel->Type = CheckZDepth(pIniVoxel, pVoxel, pMesh, pFreeSurf);
	};

	if(pVoxel->Children != NULL) {
		for(i=0; i<8; i++) {
			SetVoxelType(pIniVoxel, pVoxel->Children[i],
			             pMesh, pFreeSurf, TsufMask);
		};
	};

	return 0;
};


/*
extern int
ppohVIS_BASE_SetVoxelType(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf)
{
	int *TsufMask = NULL;
	int NX, NY, NZ;
	int iX, iY, iZ, i;

	TsufMask = (int *)calloc(pFreeSurf->Count, sizeof(int));
	if(TsufMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "surface mask array");
		goto error;
	};


	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		if(SetVoxelType(pIniVoxel, &pIniVoxel->Voxels[i],
		                pMesh, pFreeSurf, TsufMask)) goto error;
	};
	};
	};

	free(TsufMask);

	return 0;

error:
	if(TsufMask) {
		free(TsufMask);
		TsufMask = NULL;
	};

	return -1;
};
*/

extern int
ppohVIS_BASE_SetVoxelType(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	struct ppohVIS_BASE_stMesh *pMesh,
	struct ppohVIS_BASE_stGeometryItem *pFreeSurf)
{
	int *TsufMask = NULL;
	int NXYZ;
	int i;
	int iRc = 0;

	TsufMask = (int *)calloc(pFreeSurf->Count, sizeof(int));
	if(TsufMask == NULL) {
		ppohVIS_BASE_SetError(
			ppohVIS_BASE_ErrorCode_AllocationError,
			"%s [%s]", strerror(errno), "surface mask array");
		goto error;
	};


	NXYZ = pIniVoxel->NX * pIniVoxel->NY * pIniVoxel->NZ;
#pragma omp parallel for reduction(+:iRc)
	for(i=0; i<NXYZ; i++) {
		if(SetVoxelType(pIniVoxel, &pIniVoxel->Voxels[i],
		                pMesh, pFreeSurf, TsufMask)) {
			iRc++;
		};
	};
	if(iRc) {
		goto error;
	};

	free(TsufMask);

	return 0;

error:
	if(TsufMask) {
		free(TsufMask);
		TsufMask = NULL;
	};

	return -1;
};


/*******************************************************************************
 * Convert voxel information to UCD data
 ******************************************************************************/
static int
ConvertVoxel2UCD(
	struct ppohVIS_BASE_stVoxelItem *pVoxel,
	struct ppohVIS_BASE_stUCDData *pUCD,
	int *VoxelCount, const int OmitOuter, const int OmitBoundary)
{
	double OX, OY, OZ, DX, DY, DZ;
	int NodeCount, ElemCount, ValueSum;
	int i;

	if(pVoxel == NULL) { return 0; };

	if(pVoxel->Children == NULL) {
		if(OmitOuter) {
			if(pVoxel->Type == ppohVIS_BASE_VoxelOuter) {
				return 0;
			}
			if(OmitBoundary) {
				if(pVoxel->Type == ppohVIS_BASE_VoxelBoundary) {
					return 0;
				};
			};
		};

		OX = pVoxel->OX;
		OY = pVoxel->OY;
		OZ = pVoxel->OZ;
		DX = pVoxel->DX;
		DY = pVoxel->DY;
		DZ = pVoxel->DZ;

		/* Node */
		NodeCount = 8 * (*VoxelCount);

		pUCD->Node->ID[NodeCount+0] = NodeCount+1;
		pUCD->Node->ID[NodeCount+1] = NodeCount+2;
		pUCD->Node->ID[NodeCount+2] = NodeCount+3;
		pUCD->Node->ID[NodeCount+3] = NodeCount+4;
		pUCD->Node->ID[NodeCount+4] = NodeCount+5;
		pUCD->Node->ID[NodeCount+5] = NodeCount+6;
		pUCD->Node->ID[NodeCount+6] = NodeCount+7;
		pUCD->Node->ID[NodeCount+7] = NodeCount+8;

		pUCD->Node->Coords[3*NodeCount+ 0] = OX;
		pUCD->Node->Coords[3*NodeCount+ 1] = OY;
		pUCD->Node->Coords[3*NodeCount+ 2] = OZ;

		pUCD->Node->Coords[3*NodeCount+ 3] = OX + DX;
		pUCD->Node->Coords[3*NodeCount+ 4] = OY;
		pUCD->Node->Coords[3*NodeCount+ 5] = OZ;

		pUCD->Node->Coords[3*NodeCount+ 6] = OX + DX;
		pUCD->Node->Coords[3*NodeCount+ 7] = OY + DY;
		pUCD->Node->Coords[3*NodeCount+ 8] = OZ;

		pUCD->Node->Coords[3*NodeCount+ 9] = OX;
		pUCD->Node->Coords[3*NodeCount+10] = OY + DY;
		pUCD->Node->Coords[3*NodeCount+11] = OZ;

		pUCD->Node->Coords[3*NodeCount+12] = OX;
		pUCD->Node->Coords[3*NodeCount+13] = OY;
		pUCD->Node->Coords[3*NodeCount+14] = OZ + DZ;

		pUCD->Node->Coords[3*NodeCount+15] = OX + DX;
		pUCD->Node->Coords[3*NodeCount+16] = OY;
		pUCD->Node->Coords[3*NodeCount+17] = OZ + DZ;

		pUCD->Node->Coords[3*NodeCount+18] = OX + DX;
		pUCD->Node->Coords[3*NodeCount+19] = OY + DY;
		pUCD->Node->Coords[3*NodeCount+20] = OZ + DZ;

		pUCD->Node->Coords[3*NodeCount+21] = OX;
		pUCD->Node->Coords[3*NodeCount+22] = OY + DY;
		pUCD->Node->Coords[3*NodeCount+23] = OZ + DZ;

		/* Element */
		ElemCount = (*VoxelCount);

		pUCD->Element->ID[ElemCount] = ElemCount+1;
		pUCD->Element->MaterialID[ElemCount] = 1;
		pUCD->Element->Type[ElemCount] = ppohVIS_BASE_UCDTopology_Hexa8;
		pUCD->Element->NodeIndex[ElemCount+1] = NodeCount + 8;
		pUCD->Element->Node[NodeCount+0] = NodeCount+1;
		pUCD->Element->Node[NodeCount+1] = NodeCount+2;
		pUCD->Element->Node[NodeCount+2] = NodeCount+3;
		pUCD->Element->Node[NodeCount+3] = NodeCount+4;
		pUCD->Element->Node[NodeCount+4] = NodeCount+5;
		pUCD->Element->Node[NodeCount+5] = NodeCount+6;
		pUCD->Element->Node[NodeCount+6] = NodeCount+7;
		pUCD->Element->Node[NodeCount+7] = NodeCount+8;

		/* Element value */
		ValueSum = pUCD->ElementValue->Sum;

		pUCD->ElementValue->ID[ElemCount] = ElemCount+1;
		pUCD->ElementValue->Value[ValueSum*ElemCount+0] = pVoxel->Value[0];
		pUCD->ElementValue->Value[ValueSum*ElemCount+1] = pVoxel->Cost;

		(*VoxelCount)++;

	} else {
		for(i=0; i<8; i++) {
			ConvertVoxel2UCD(pVoxel->Children[i], pUCD,
			                 VoxelCount, OmitOuter, OmitBoundary);
		};
	};

	return 0;
};


extern struct ppohVIS_BASE_stUCDData *
ppohVIS_BASE_ConvertVoxel2UCD(
	struct ppohVIS_BASE_stInitialVoxel *pIniVoxel,
	const int OmitOuter, const int OmitBoundary)
{
	struct ppohVIS_BASE_stUCDData *pUCD = NULL;
	int NX, NY, NZ;
	int VoxelCount, i, iX, iY, iZ, iCount;


	if(OmitOuter) {
		if(OmitBoundary) {
			VoxelCount = ppohVIS_BASE_GetInnerVoxelCount(pIniVoxel);
		} else {
			VoxelCount = ppohVIS_BASE_GetVoxelCountWithoutOuter(pIniVoxel);
		};
	} else {
		VoxelCount = ppohVIS_BASE_GetVoxelCount(pIniVoxel);
	};

	NX = pIniVoxel->NX;
	NY = pIniVoxel->NY;
	NZ = pIniVoxel->NZ;

	if((pUCD = ppohVIS_BASE_AllocateUCD()) == NULL) {
		goto error;
	};
	if((pUCD->Node = ppohVIS_BASE_AllocateUCDNode(VoxelCount*8)) == NULL) {
		goto error;
	};
	if((pUCD->Element = ppohVIS_BASE_AllocateUCDElement(VoxelCount)) == NULL) {
		goto error;
	};
	if((pUCD->NodeValue = ppohVIS_BASE_AllocateUCDValue(VoxelCount*8, 0, 0)) == NULL) {
		goto error;
	};
	if((pUCD->ElementValue = ppohVIS_BASE_AllocateUCDValue(VoxelCount, 2, 2)) == NULL) {
		goto error;
	};


	for(i=0; i<pUCD->ElementValue->Count; i++) {
		pUCD->ElementValue->DOF[i] = 1;
		pUCD->ElementValue->Label[i] = (char *)calloc(PPOHVIS_BASE_LABEL_LEN, sizeof(char));
		pUCD->ElementValue->Unit[i] = (char *)calloc(PPOHVIS_BASE_LABEL_LEN, sizeof(char));
	};
	strncpy(pUCD->ElementValue->Label[0], "value", PPOHVIS_BASE_LABEL_LEN);
	strncpy(pUCD->ElementValue->Label[1], "cost", PPOHVIS_BASE_LABEL_LEN);

	iCount = 0;
	for(iZ=0; iZ<NZ; iZ++) {
	for(iY=0; iY<NY; iY++) {
	for(iX=0; iX<NX; iX++) {
		i = NX * NY * iZ + NX * iY + iX;
		ConvertVoxel2UCD(&pIniVoxel->Voxels[i], pUCD, &iCount, OmitOuter, OmitBoundary);
	};
	};
	};


	return pUCD;

error:
	ppohVIS_BASE_FreeUCD(pUCD);
	return NULL;
};

