/*=====================================================================*
 *                                                                     *
 *   Software Name : ppOpen-AT                                         *
 *         Version : 1.0.0                                             *
 *                                                                     *
 *   License                                                           *
 *     This file is part of ppOpen-AT.                                 *
 *     ppOpen-AT is a free software, you can use it under the terms    *
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
 *   Copyright (c) 2012                                                *
 *      Information Technology Center, The University of Tokyo         *
 *                                                                     *
 *=====================================================================*/
 //---------------------------------------------------------------------------

#ifndef VisualH
#define VisualH
//---------------------------------------------------------------------------
#include "TuneRegion.h"

/*----------------------------------------------------------------------------*/
//  1.ﾆ・ﾜｸ・ｾ
//
//  2.ｳｵﾍﾗ
//    ･ﾓ･ｸ･螂｢･・ｽ･ﾇ｡ｼ･ｿ･筵ｸ･蝪ｼ･・//    ･ﾓ･ｸ･螂｢･・ｽ､ﾎ･ｳ｡ｼ･ﾉﾀｸﾀｮ､ﾋｻﾈﾍﾑ､ｵ､・・｣
//
//  3.ｵ｡ﾇｽﾀ篶ﾀ
//
//  4.ﾈﾍ
//
/*----------------------------------------------------------------------------*/
class TVisualDM
{
private:	// ･譯ｼ･ｶ｡ｼﾀ・ﾀ
	string DirName;
	TList *TuneRegionList;
    int RegionIndent;

	void MakeIndexHTML();
	void MakeResultHTML(TTuneRegion *TuneRegion);
	string StrToHTMLStr(string s);
	void ResetLogDataFile(TTuneRegion *TuneRegion);
	void CopyDataForHTML();

public:		// ･譯ｼ･ｶ｡ｼﾀ・ﾀ
	TVisualDM();
	void Exec();
};

#endif
