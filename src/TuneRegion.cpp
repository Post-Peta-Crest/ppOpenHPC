/* =====================================================================*
 *                                                                     *
 *   Software Name : ppOpen-AT                                         *
 *         Version : 0.1                                               *
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
 *===================================================================== */
/* ---------------------------------------------------------------------------- */
//
// 概要
// チューニングリージョンクラス
//
// 作成者
//
/* ---------------------------------------------------------------------------- */

#include "TuneRegion.h"
#include "main.h"
#include "pass5.h"

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// aPass4  呼び出し元パス４
// Pos 開始位置
//
// 3.概要
// TRuneReginクラスの生成
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
TTuneRegion::TTuneRegion(TPass4 *aPass4, int Pos) {
	TScript *Script;
	TToken *Token;
	int i, Nest;
	int j;
	TScValData *ScValData;

	if (MainF->SrcCodeType == MainF->sctFortran77) {
		Comment = 'c';
	}
	else {
		Comment = '!';
	}
	Pass4 = aPass4;
	TokenStartPos = Pos;
	TokenList = Pass4->TokenList;
	ValDataList = Pass4->ValDataList;
	ArgValList = new TStringList; // サブルーチンにする場合の引数リスト
	UndefinedArgValList = new TList; // 定義が見つからない引数データ
	BaseValList = new TStringList; // 基本パラメータ名の変数リスト
	ParamValList = new TStringList; // パラメータ文の変数リスト
	ValBitsList = new TList;
	SubRegionList = new TList; // サブリージョンリスト TSubRegion クラス
	RotaionOrderList = new TList;
	ReplaceStrList = new TStringList;	// replace指定子で定義された文字列リスト
	TargetStrList = new TStringList;	// targer指定子で定義された文字列リスト。最後の()を含む場合あり。
	ListSrcStrList = new TStringList;	// list指定子で定義された置換え元変数名リスト
	ListReplaceStrList = new TStringList;	// list指定子で定義された置換えリスト ListSrcList*N個の要素
	ListDSrcStrList = new TStringList;	// list指定子で定義された置換え元変数名リスト
	ListDReplaceStrList = new TStringList;	// list指定子で定義された置換えリスト ListSrcList*N個の要素
	GWV_ListList = new TList;				// GWV変換用のリストへのリスト
	GWV_TargetStrList = new TStringList;	// targer指定子で定義された文字列リスト。最後の()を含む場合あり。
	UseAccordingF = false;
	UsedDynPefThis = false; // DynPefThisスクリプトが使用された。
	Number = 0;
	AccordingStr = "";
	OffsetStr = "";

	// d-spline用コード追加部分
	FittingDspline = 0;
	FittingDynamic = 0;
	// d-spline用コード追加部分　ここまで

	FittingType = 0; // Type = 0:なし、1:least-square,2:user_defined
	FittingDegree = 0; // 多項式の次数
	SampledList = NULL; // intの形で Sampledの値がセット（実体は、Scriptのデータ）
	GPUOption = gpu_option_None; // GPUオプション なし
	SplitPointCopyDef_StartPos = 0;
	SplitPointCopyDef_EndPos = 0;
	OptionStr = "";

	/** ************************************** */
	//
	// Kogakuin Irie
	// TuneRegion.h内の配列を初期化する処理を追加
	//
	for (i = 0; i < 32; i++) {
		DoStartSPos[i] = 0;
		DoStartEPos[i] = 0;
		DoEndSPos[i] = 0;
		DoEndEPos[i] = 0;
		DoStepSPos[i] = 0;
		DoStepEPos[i] = 0;
	}
	//
	// ここまで
	//
	/** ************************************** */

	for (int kk = 0; kk < 32; kk++) {
		DoToken[kk] = NULL;
		DoValToken[kk] = NULL;
	}
	Token = (TToken*)TokenList->Items[Pos];
	Script = (TScript*)Token->Script;
	variedCount = 0;
	variedDCount = 0;

	if (Script->ScType == sct_install) {
		TuneGroup = tgInstall;
		TuneGroupName = "Install";
	}
	else if (Script->ScType == sct_static) {
		TuneGroup = tgStatic;
		TuneGroupName = "Static";
	}
	else if (Script->ScType == sct_dynamic) {
		TuneGroup = tgDynamic;
		TuneGroupName = "Dynamic";
	}
	//
	// region end を探す。（そこまでが、ブロックになる）
	// Nestしている場合は、内側は別とする。
	//
	TokenEndPos = -1;
	Nest = 0;
	for (i = Pos; i < TokenList->Count; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->Script == NULL) {
			continue;
		}
		Script = (TScript*)Token->Script;
		if (Script->ScRegion == scr_start) {
			Nest++;
		}
		else if (Script->ScRegion == scr_end) {
			Nest--;
			if (Nest == 0) {
				TokenEndPos = i;
				break;
			}
		}
	}
	if (TokenEndPos == -1) {
		TokenEndPos = Pos; // Loop回避のため
		// MainF->err(Pos,"region end が見つかりません。");
		MainF->ErrMessage(Pos, "region end is not found.");
		BaseValList->AddObject("UndefinedBPSet", NULL);
		return;
	}
	if (Script->ScAction == sca_unroll) {
		TuneKind = tkUnroll;
		MakeUnrollData(Pos); // Unrollのデータを作成する。
	}
	else if (Script->ScAction == sca_select) {
		TuneKind = tkSelect;
		MakeSelectData(Pos); // Selectのデータを作成する。
	}
	else if (Script->ScAction == sca_loopfusionsplit) {
		TuneKind = tkLoopFusionSplit;
		MakeLoopFusionSplitData(Pos); // LoopFusionのデータを作成する。
	}
	else if (Script->ScAction == sca_loopfusion) {
		TuneKind = tkLoopFusion;
		MakeLoopFusionSplitData(Pos); // LoopFusionのデータを作成する。
	}
	else if (Script->ScAction == sca_loopsplit) {
		TuneKind = tkLoopSplit;
		MakeLoopFusionSplitData(Pos); // LoopFusionのデータを作成する。
	}
	else if (Script->ScAction == sca_rotationorder) {
		TuneKind = tkRotationOrder;
		MakeLoopFusionSplitData(Pos); // LoopFusionのデータを作成する。
	}
	else if (Script->ScAction == sca_list) {
		TuneKind = tkList;
		MakeSelectData(Pos); // Selectのデータを作成する。
	}
	else if (Script->ScAction == sca_variableD) {
		TuneKind = tkVariableD;
		MakeSelectData(Pos); // Selectのデータを作成する。
	}
	else if (Script->ScAction == sca_listD) {
		TuneKind = tkListD;
		MakeSelectData(Pos); // Selectのデータを作成する。
	}
	else if (Script->ScAction == sca_replace) {
		TuneKind = tkReplace;
		MakeSelectData(Pos); // Selectのデータを作成する。
	}
	else if (Script->ScAction == sca_GWV) {
		TuneKind = tkGWV;
		MakeSelectData(Pos); // Selectのデータを作成する。
	}
	else {
		TuneKind = tkSelect;
		MakeSelectData(Pos); // Selectのデータを作成する。
	}
	//
	// リージョン開始直前のスクリプト　BPsetを探す。
	// BPset (ValNameList)から BaseValNameListを作成する。
	// もし、定義がない場合は、以前との互換性のために N,NX,NY,NZとなる。
	//
	for (i = TokenEndPos; i >= 0; i--) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->Script == NULL) {
			continue;
		}
		Script = (TScript*)Token->Script;
		if ((Script->ScType == sct_BPset) && (Script->ScValDataList != NULL)) {
			for (j = 0; j < Script->ScValDataList->Count; j++) {
				ScValData = (TScValData*)Script->ScValDataList->Items[j];
				BaseValList->AddObject(ScValData->Str, (void*)ScValData);
			}
			break;
		}
	}
	//
	// 定義がない場合はソースコード上で UndefinedBPSetとしてエラーとする。
	// Select等の場合は定義がなくてもエラーでないため。
	// 検索は行わない。　2012/02/28
	//
	if (BaseValList->Count == 0) {
		MainF->ErrMessage(TokenEndPos, "oat_bpset が見つかりません。");
		BaseValList->AddObject("UndefinedBPSet", NULL);
	}
	//
	// #Pragme OAT allocate()を探して見つかれば設定を行う。
	//
	for (i = 0; i <= TokenEndPos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->Script == NULL) {
			continue;
		}
		Script = (TScript*)Token->Script;
		if ((Script->ScType == sct_Allocate) && (Script->ScValDataList != NULL)
		) {
			if (MainF->cc_option_str == "") {
				GPUOption = gpu_option_None; // -cc 指定なしの場合は、GPUオプションなし。
				break;
			}
			for (j = 0; j < Script->ScValDataList->Count; j++) {
				ScValData = (TScValData*)Script->ScValDataList->Items[j];
				if (ScValData->Str == "cpu") {
					GPUOption = gpu_option_CPU; // GPUオプション CPU
				}
				else if (ScValData->Str == "gpu") {
					GPUOption = gpu_option_GPU; // GPUオプション GPU
				}
				else if (ScValData->Str == "auto") {
					GPUOption = gpu_option_auto; // GPUオプション auto
				}
				else {
					MainF->ErrMessage(i,
						"allocate の指定が CPU,GPU,autoのいずれでもありません。");
				}
			}
		}
	}
	//
	// GPUOptionがautoの場合は、Case数を２倍にする。 2010/12/29
	//
	if (GPUOption == gpu_option_auto) { // GPUオプション auto
		CaseCount *= 2;
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// TRuneReginクラスの破棄
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
TTuneRegion::~TTuneRegion() {

	delete ArgValList; // サブルーチンにする場合の引数リスト
	delete UndefinedArgValList; // 定義が見つからない引数データ
	delete BaseValList; // 基本パラメータ名の変数リスト
	delete ParamValList; // パラメータ文の変数リスト
	delete ValBitsList;
	for (int i = 0; i < SubRegionList->Count; i++) {
		delete(TSubRegion*)SubRegionList->Items[i];
	}
	delete SubRegionList;
	delete RotaionOrderList;
	delete ReplaceStrList;
	delete TargetStrList;
	delete ListSrcStrList;
	delete ListReplaceStrList;
	delete ListDSrcStrList;
	delete ListDReplaceStrList;
	for(int i = 0 ; i < GWV_ListList->Count ; i++){
		if(GWV_ListList->Items[i] != NULL){
			delete ((TStringList *)GWV_ListList->Items[i]);
			GWV_ListList->Items[i] = NULL;
		}
	}
	delete GWV_ListList;
	delete GWV_TargetStrList;

}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// sPos    開始位置
//
// 3.概要
// Selectの解析を行う。以前のバージョン
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::MakeSelectData_Old(int sPos) {
	int i, j, k, k2;
	TToken *Token;
	TScript *Script;
	string s;
	TScValData *ScValData;
	string ScValName;
	int Nest;
	TSubRegion *SubRegion;
	bool InSubRegionF = false;

	Token = (TToken*)TokenList->Items[sPos];

	//
	// 対象変数名を所得
	//
	ScValData = NULL;
	ScValName = "";
	//
	// 名前、参照変数（Setが先の場合は、不要）を求める
	//
	// Name指定なし時の対策 Add 2004/08/26
	Name = IntToStr(MainF->TuneRegionList->Count + 1);
	if (TuneGroup == tgInstall) {
		FuncName = "OAT_Install" + Name;
	}
	else if (TuneGroup == tgStatic) {
		FuncName = "OAT_Static" + Name;
	}
	else if (TuneGroup == tgDynamic) {
		FuncName = "OAT_Dynamic" + Name;
	}
	Nest = 0;
	ArgValList->Clear();
	for (i = sPos; i <= TokenEndPos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Nest != 1) {
			//
			// TRのネストは、Skipするが、
			// 変数に関しては、チェック対象とする。
			// 複数のSubRegionでの変数の参照・代入は統一していることが前提。
			//
			if (Token->Script != NULL) {
				Script = (TScript*)Token->Script;
				if (Script->ScRegion == scr_start) {
					Nest++;
				}
				else if (Script->ScRegion == scr_end) {
					Nest--;
				}
			}
			continue;
		}
		if (Token->Script != NULL) {
			Script = (TScript*)Token->Script;
			if (Script->ScRegion == scr_start) { // region Start
				Nest++; // TR ネスト部分の処理
				continue;
			}
			else if (Script->ScType == sct_name) { // name
				Name = Script->TokStrList->Strings[2];
				if (TuneGroup == tgInstall) {
					FuncName = "OAT_Install" + Name;
				}
				else if (TuneGroup == tgStatic) {
					FuncName = "OAT_Static" + Name;
				}
				else if (TuneGroup == tgDynamic) {
					FuncName = "OAT_Dynamic" + Name;
				}
			}
			else if (Script->ScType == sct_Number) { // number
				Number = atoi(Script->TokStrList->Strings[2].c_str());
			}
			else if (Script->ScType == sct_according) { // Acoording
				if (!InSubRegionF) { // SubRegion内の acordingは、SubRegion内のみ有効
					TToken *Token2;

					UseAccordingF = true;
					AccordingStr = "";
					for (j = i; j <= TokenEndPos; j++) {
						Token2 = (TToken*)TokenList->Items[j];
						if (Token2->TokId == tid_LineEnd) {
							break;
						}
						AccordingStr += Token2->OrgStr;
					}
				}
			}
			else if (Script->ScType == sct_varied) {
				//
				// varried処理を行う。
				//
				if (!InSubRegionF) { // SubRegion内の variedは、SubRegion内のみ有効
					if (Script->ScValDataList == NULL) {
						MainF->ErrMessage(i, "varied の変数指定が見つかりません。");
						return;
					}
					if (Script->ScValDataList->Count == 0) {
						MainF->ErrMessage(i, "varied の変数指定が見つかりません。");
						return;
					}
					for (k = 0; k < Script->ScValDataList->Count; k++) {
						ScValData = (TScValData*)Script->ScValDataList->Items
						[k];
						variedValName[variedCount] = ScValData->Str;
						variedValData[variedCount] = ScValData;
						DoToken[variedCount] = NULL;
						for (j = 0; j < Script->TokStrList->Count - 1; j++) {
							s = Script->TokStrList->Strings[j];
							if (s == "from") {
								s = Script->TokStrList->Strings[j + 1];
								/** *********************************************************************** */
								//
								// Kogakuin Irie
								// PPの変動範囲を実数にも対応
								// 既存コードはコメントアウト
								//
								// variedFromValue[variedCount] = atoi(s.c_str());
								variedFromValue[variedCount] = (float)atof
								(s.c_str());
								//
								// ここまで
								//
								/** *********************************************************************** */
							}
							else if (s == "to") {
								s = Script->TokStrList->Strings[j + 1];
								/** *********************************************************************** */
								//
								// Kogakuin Irie
								// PPの変動範囲を実数にも対応
								// 既存コードはコメントアウト
								//
								// variedToValue[variedCount] = atoi(s.c_str());
								variedToValue[variedCount] = (float)atof
								(s.c_str());
								//
								// ここまで
								//
								/** *********************************************************************** */

								/** ************************************** */
								//
								// Kogakuin Irie
								// step（増分値の任意設定）対応のための処理
								//
								variedStepValue[variedCount] = 1;
								//
								// ここまで
								//
								/** ************************************** */
							}
							/** ********************************************************* */
							//
							// Kogakuin Irie
							// step（増分値の任意設定）対応のための処理
							//
							else if (s == "step") {
								s = Script->TokStrList->Strings[j + 1];
								variedStepValue[variedCount] = (float)atof
								(s.c_str());
							}
							//
							// ここまで
							//
							/** ********************************************************* */
						}
						variedCount++;
					}
				}
			}
			else if (Script->ScType == sct_fitting) {
				//
				// Fitting指定子の処理を行う。（TuneRegionへデータ複写のみ)
				//

				// d-spline用コード追加部分
				FittingDspline = Script->FittingDspline;
				FittingDynamic = Script->FittingDynamic;
				// d-spline用コード追加部分　ここまで

				FittingType = Script->FittingType;
				FittingDegree = Script->FittingDegree;
				SampledList = Script->SampledList;
				// }else if(Script->ScRegion == scr_substart){	// Sub region Start
			}
			else if ((Script->ScRegion == scr_substart) &&
				(Script->ScType == sct_select)) { // Sub region Start
				InSubRegionF = true;
				SubRegion = new TSubRegion();
				SubRegion->TokenStartPos = i;
				SubRegion->ScType = Script->ScType;
				SubRegion->AccordingStr = AccordingStr;
				SubRegionList->Add((void*)SubRegion);

				for (j = 0; j < variedCount; j++) { // varidCountを継承
					SubRegion->variedValName[j] = variedValName[j];
					SubRegion->variedValData[j] = variedValData[j];
					SubRegion->variedFromValue[j] = variedFromValue[j];
					/** ********************************************************* */
					//
					// Kogakuin Irie
					// step（増分値の任意設定）対応のための処理
					//
					SubRegion->variedStepValue[j] = variedStepValue[j];
					//
					// ここまで
					//
					/** ********************************************************* */
					SubRegion->variedToValue[j] = variedToValue[j];
				}
				SubRegion->variedCount = variedCount;

				for (j = i; j <= TokenEndPos; j++) {
					Token = (TToken*)TokenList->Items[j];
					if (Token->Script != NULL) {
						Script = (TScript*)Token->Script;
						if (Script->ScRegion == scr_start) {
							// Nestしている場合は、region end まで Skipする。
							for (; j <= TokenEndPos; j++) {
								Token = (TToken*)TokenList->Items[j];
								if (Token->Script != NULL) {
									Script = (TScript*)Token->Script;
									if (Script->ScRegion == scr_end) {
										break;
									}
								}
							}
						}
						else if (Script->ScRegion == scr_subend) {
							break;
						}
						else if (Script->ScType == sct_according) { // Acoording
							TToken *Token2;

							UseAccordingF = true;
							SubRegion->AccordingStr = "";
							for (int k = j; k <= TokenEndPos; k++) {
								Token2 = (TToken*)TokenList->Items[k];
								if (Token2->TokId == tid_LineEnd) {
									break;
								}
								SubRegion->AccordingStr += Token2->OrgStr;
							}
						}
						else if (Script->ScType == sct_varied) {
							//
							// varried処理を行う。
							//
							if (Script->ScValDataList == NULL) {
								MainF->ErrMessage(i, "varied の変数指定が見つかりません。");
								return;
							}
							if (Script->ScValDataList->Count == 0) {
								MainF->ErrMessage(i, "varied の変数指定が見つかりません。");
								return;
							}
							for (k = 0; k < Script->ScValDataList->Count; k++) {
								ScValData = (TScValData*)
								Script->ScValDataList->Items[k];
								SubRegion->variedValName[variedCount]
								= ScValData->Str;
								SubRegion->variedValData[variedCount]
								= ScValData;
								for (k2 = 0;
									k2 < Script->TokStrList->Count - 1; k2++) {
									s = Script->TokStrList->Strings[k2];
									if (s == "from") {
										s = Script->TokStrList->Strings[k2 + 1];
										/** *********************************************************************** */
										//
										// Kogakuin Irie
										// PPの変動範囲を実数にも対応
										// 既存コードはコメントアウト
										//
										// SubRegion->variedFromValue[variedCount] = atoi(s.c_str());
										SubRegion->variedFromValue[variedCount]
										= (float)atof(s.c_str()); // 実数に対応
										//
										// ここまで
										//
										/** *********************************************************************** */
									}
									else if (s == "to") {
										s = Script->TokStrList->Strings[k2 + 1];
										/** *********************************************************************** */
										//
										// Kogakuin Irie
										// PPの変動範囲を実数にも対応
										// 既存コードはコメントアウト
										//
										// SubRegion->variedToValue[variedCount] = atoi(s.c_str());
										SubRegion->variedToValue[variedCount] =
										(float)atof(s.c_str()); // 実数に対応
										//
										// ここまで
										//
										/** *********************************************************************** */

										/** **************************************** */
										//
										// Kogakuin Irie
										// step（増分値の任意設定）対応のための処理
										//
										SubRegion->variedStepValue[variedCount]
										= 1;
										//
										// ここまで
										//
										/** **************************************** */
									}
									/** ******************************************************************** */
									//
									// Kogakuin Irie
									// step（増分値の任意設定）対応のための処理
									//
									else if (s == "step") {
										s = Script->TokStrList->Strings[k2 + 1];
										SubRegion->variedStepValue[variedCount]
										= (float)atof(s.c_str());
									}
									//
									// ここまで
									//
									/** ******************************************************************** */
								}
								SubRegion->variedCount++;
							}
						}
					}
				}
				SubRegion->TokenEndPos = j;
				if (j >= TokenEndPos) {
					MainF->ErrMessage(i, "sub region endが見つかりません。");
				}

			}
			else if (Script->ScRegion == scr_subend) {
				InSubRegionF = false;
			}
		}
	}
	MakeArgValList(); // TRのサブルーチンに渡す引数リストを生成する。
	//
	// Case数を計算する。
	// SubRegionがある場合は、各SubReguionの varied数だけのCaseの総和
	// SubRegionがない場合は、varied数だけのCase
	//
	if (SubRegionList->Count > 0) {
		int SubCaseCount;

		CaseCount = 0;
		for (j = 0; j < SubRegionList->Count; j++) {
			SubRegion = (TSubRegion*)SubRegionList->Items[j];
			SubCaseCount = 1;
			for (k = 0; k < SubRegion->variedCount; k++) { // SubRegionがない場合の
				/** ******************************************************************************************************************** */
				//
				// Kogakuin Irie
				// PPの実数対応とstep対応のための処理
				// 既存コードはコメントアウト
				//
				// SubCaseCount *= (SubRegion->variedToValue[k] - SubRegion->variedFromValue[k] + 1);
				SubCaseCount *= (int)
				((SubRegion->variedToValue[k] - SubRegion->variedFromValue[k]
					) / SubRegion->variedStepValue[k] + 1);
				//
				// ここまで
				//
				/** ******************************************************************************************************************** */
			}
			SubRegion->CaseCount = SubCaseCount;
			CaseCount += SubCaseCount;
		}
	}
	else {
		// SubRegionがない場合の処理
		CaseCount = 1;
		for (k = 0; k < variedCount; k++) { // SubRegionがない場合の
			/** **************************************************************************************************** */
			//
			// Kogakuin Irie
			// PPの実数対応とstep対応のための処理
			// 既存コードはコメントアウト
			//
			// CaseCount *= (variedToValue[k] - variedFromValue[k] + 1);
			CaseCount *= (int)((variedToValue[k] - variedFromValue[k])
				/ variedStepValue[k] + 1);
			//
			// ここまで
			//
			/** **************************************************************************************************** */
		}
	}
	//
	// varied from to で定数に置換される変数は、引数リストから参照扱いをCUT
	// 現状、同一名で、片側は変数として使用、もう一方は引数と指定使用は出来ない。
	// 最終的には、エラーメッセージが必要？
	//
	for (i = 0; i < SubRegionList->Count; i++) {
		SubRegion = (TSubRegion*)SubRegionList->Items[i];
		for (k = 0; k < SubRegion->variedCount; k++) {
			s = SubRegion->variedValName[k];
			for (j = 0; j < ArgValList->Count; j++) {
				if (ArgValList->Strings[j] == s) {
					ArgValList->Strings[j] = "";
				}
			}
		}
	}
	for (k = 0; k < variedCount; k++) {
		s = variedValName[k];
		for (j = 0; j < ArgValList->Count; j++) {
			if (ArgValList->Strings[j] == s) {
				ArgValList->Strings[j] = "";
			}
		}
	}
	//
	// ArgValListを追加 ""(最初に代入で使用)も、必要なので、削除はしないこと
	//
	AddArgValListToATExecArgList();
	SaveAndResetValBits(TokenStartPos, TokenEndPos); // 変数識別を、保存してリセット
}
/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// sPos    開始位置
//
// 3.概要
// Replace等の置き換え用データの解析にも対応した新しいMakeSelectData
// MakeSelectDataに機能拡張した形になり、replace,variableD等のリージョンで呼ばれる。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::MakeSelectData(int sPos)
{
	int i, j, k, k2;
	TToken *Token;
	TScript *Script;
	string s;
	TScValData *ScValData;
	string ScValName;
	int Nest;
	TSubRegion *SubRegion;
	bool InSubRegionF = false;

	Token = (TToken*)TokenList->Items[sPos];

	//
	// 対象変数名を所得
	//
	ScValData = NULL;
	ScValName = "";
	//
	// 名前、参照変数（Setが先の場合は、不要）を求める
	//
	// Name指定なし時の対策 Add 2004/08/26
	Name = IntToStr(MainF->TuneRegionList->Count + 1);
	if (TuneGroup == tgInstall) {
		FuncName = "OAT_Install" + Name;
	}
	else if (TuneGroup == tgStatic) {
		FuncName = "OAT_Static" + Name;
	}
	else if (TuneGroup == tgDynamic) {
		FuncName = "OAT_Dynamic" + Name;
	}
	Nest = 0;
	ArgValList->Clear();
	for (i = sPos; i <= TokenEndPos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Nest != 1) {
			//
			// TRのネストは、Skipするが、
			// 変数に関しては、チェック対象とする。
			// 複数のSubRegionでの変数の参照・代入は統一していることが前提。
			//
			if (Token->Script != NULL) {
				Script = (TScript*)Token->Script;
				if (Script->ScRegion == scr_start) {
					Nest++;
				}
				else if (Script->ScRegion == scr_end) {
					Nest--;
				}
			}
			continue;
		}
		if (Token->Script != NULL) {
			Script = (TScript*)Token->Script;
			if (Script->ScRegion == scr_start) { // region Start
				Nest++; // TR ネスト部分の処理
				continue;
			}
			else if (Script->ScType == sct_name) { // name
				Name = Script->TokStrList->Strings[2];
				if (TuneGroup == tgInstall) {
					FuncName = "OAT_Install" + Name;
				}
				else if (TuneGroup == tgStatic) {
					FuncName = "OAT_Static" + Name;
				}
				else if (TuneGroup == tgDynamic) {
					FuncName = "OAT_Dynamic" + Name;
				}
			}
			else if (Script->ScType == sct_Replace) {
				int BrNest = 0;
				TToken *Token2;
				string s,s2;

				s = "";
				for(k = 0 ; k < Script->TokStrList->Count ; k++){
					Token2 = (TToken *)TokenList->Items[Script->TokPos+k];
					s2 = Token2->Str;
					if(s2 == "("){
						BrNest++;
					}
					else if(s2 == ")"){
						if(BrNest == 1){
							ReplaceStrList->Add(s);
							s = "";
						}
						BrNest--;
					}
					else if(BrNest == 1){
						s += Token2->OrgStr;	// 空白も含めた文字列として置換。
					}
				}
			}
			else if (Script->ScType == sct_Target) {
				TToken *Token2;
				string s;

				s = "";
				for(k = 2 ; k < Script->TokStrList->Count ; k++){
					Token2 = (TToken *)TokenList->Items[Script->TokPos+k];
					s += Token2->OrgStr;	// 空白も含めた文字列
				}
				s = Trim(s);	// 前後の空白は対象外とする。
				TargetStrList->AddObject(s,(void *)Script); // Scripへの参照を含めてセット
			}
			else if (Script->ScType == sct_List) {
				int BrNest = 0;
				TToken *Token2;
				string s,s2;
				int BrCount = 0;

				s = "";
				for(k = 0 ; k < Script->TokStrList->Count ; k++){
					// Token解析、TokIdはコメントなので文字列でのみ解析する。
					Token2 = (TToken *)TokenList->Items[Script->TokPos+k];
					s2 = Token2->Str;
//					DP("["+s2+"] + tok_Id = "+IntToStr(Token2->TokId));
					if(s2 == "("){
						BrNest++;
					}
					else if(s2 == ")"){
						if(BrNest == 1){
							if(BrCount == 0){
								ListSrcStrList->Add(s);
							}else{
								ListReplaceStrList->Add(s);
							}
							s = "";
							BrCount++;
						}
						BrNest--;
					}
					else if(BrNest == 1){
						if(s2 == ","){	// カンマ
							if(BrCount == 0){
								ListSrcStrList->Add(s);
							}else{
								ListReplaceStrList->Add(s);
							}
							s = "";
						}else{
							s += Token2->Str;
						}
					}
				}
			}
			else if (Script->ScType == sct_ListD) {
				int BrNest = 0;
				TToken *Token2;
				string s,s2;
				int BrCount = 0;

				s = "";
				for(k = 0 ; k < Script->TokStrList->Count ; k++){
					// Token解析、TokIdはコメントなので文字列でのみ解析する。
					Token2 = (TToken *)TokenList->Items[Script->TokPos+k];
					s2 = Token2->Str;
//					DP("["+s2+"] + tok_Id = "+IntToStr(Token2->TokId));
					if(s2 == "("){
						BrNest++;
					}
					else if(s2 == ")"){
						if(BrNest == 1){
							if(BrCount == 0){
								ListDSrcStrList->Add(s);
							}else{
								ListDReplaceStrList->Add(s);
							}
							s = "";
							BrCount++;
						}
						BrNest--;
					}
					else if(BrNest == 1){
						if(s2 == ","){	// カンマ
							if(BrCount == 0){
								ListDSrcStrList->Add(s);
							}else{
								ListDReplaceStrList->Add(s);
							}
							s = "";
						}else{
							s += Token2->Str;
						}
					}
				}
			}
			else if (Script->ScType == sct_GWV_list) {
				TToken *Token2;
				string s,s2,LabelStr;
				int BrNest = 0;			// ( )
				int KagiBrNest = 0;		// [ ]
				TStringList *GWV_LabelStrList;
				TStringList *GWV_ReplaceStrList;

				GWV_LabelStrList = new TStringList;
				GWV_ReplaceStrList = new TStringList;
				GWV_ListList->Add(GWV_LabelStrList);
				GWV_ListList->Add(GWV_ReplaceStrList);
//				GWV_ListList->Insert(0,GWV_ReplaceStrList);
//				GWV_ListList->Insert(0,GWV_LabelStrList);

				s = "";
				for(k = 4 ; k < Script->TokStrList->Count ; k++){
					// Token解析、TokIdはコメントなので文字列でのみ解析する。
					Token2 = (TToken *)TokenList->Items[Script->TokPos+k];
					s2 = Token2->Str;
//					DP("["+s2+"] + tok_Id = "+IntToStr(Token2->TokId));
					if(s2 == "["){
						KagiBrNest++;
					}
					else if(s2 == "]"){
						if(s != ""){
							GWV_LabelStrList->Add(s);
							s = "";
						}
						KagiBrNest--;
					}
					else if(s2 == "("){
						if(s != ""){
							GWV_LabelStrList->Add(s);
							s = "";
						}
						BrNest++;
					}
					else if(s2 == ")"){
						if(BrNest == 1){
							GWV_ReplaceStrList->Add(s);
							s = "";
							while((GWV_ReplaceStrList->Count % 3) != 0){
								GWV_ReplaceStrList->Add(""); // GWVの3*n個にする。
							}
						}
						BrNest--;
					}
					else if(BrNest == 1){
						if(s2 == ","){	// カンマ
							GWV_ReplaceStrList->Add(s);
							s = "";
						}else{
							s += Token2->Str;
						}
					}else{	// ()内以外で[]()以外の文字はラベルとして追加する。
						if(s2 == ","){	// カンマ
							if(s != ""){
								GWV_LabelStrList->Add(s);
								s = "";
							}
						}else{
							s += Token2->Str;
						}
					}
				}
			}
			else if (Script->ScType == sct_GWV_target) {
				TToken *Token2;
				string s;

				s = "";
				for(k = 4 ; k < Script->TokStrList->Count ; k++){
					Token2 = (TToken *)TokenList->Items[Script->TokPos+k];
					s += Token2->OrgStr;	// 空白も含めた文字列
				}
				s = Trim(s);	// 前後の空白は対象外とする。
				GWV_TargetStrList->AddObject(s,(void *)Script); // Scripへの参照を含めてセット
			}
			else if (Script->ScType == sct_Number) { // number
				Number = atoi(Script->TokStrList->Strings[2].c_str());
			}
			else if (Script->ScType == sct_according) { // Acoording
				if (!InSubRegionF) { // SubRegion内の acordingは、SubRegion内のみ有効
					TToken *Token2;

					UseAccordingF = true;
					AccordingStr = "";
					for (j = i; j <= TokenEndPos; j++) {
						Token2 = (TToken*)TokenList->Items[j];
						if (Token2->TokId == tid_LineEnd) {
							break;
						}
						AccordingStr += Token2->OrgStr;
					}
				}
			}
			else if (Script->ScType == sct_varied) {
				//
				// varried処理を行う。
				//
				if (!InSubRegionF) { // SubRegion内の variedは、SubRegion内のみ有効
					if (Script->ScValDataList == NULL) {
						MainF->ErrMessage(i, "varied の変数指定が見つかりません。");
						return;
					}
					if (Script->ScValDataList->Count == 0) {
						MainF->ErrMessage(i, "varied の変数指定が見つかりません。");
						return;
					}
					for (k = 0; k < Script->ScValDataList->Count; k++) {
						ScValData = (TScValData*)Script->ScValDataList->Items[k];
						variedValName[variedCount] = ScValData->Str;
						variedValData[variedCount] = ScValData;
						DoToken[variedCount] = NULL;
						for (j = 0; j < Script->TokStrList->Count - 1; j++) {
							s = Script->TokStrList->Strings[j];
							if (s == "from") {
								s = Script->TokStrList->Strings[j + 1];
								variedFromValue[variedCount] = (float)atof(s.c_str());
							}
							else if (s == "to") {
								s = Script->TokStrList->Strings[j + 1];
								variedToValue[variedCount] = (float)atof(s.c_str());
								variedStepValue[variedCount] = 1;
							}
							else if (s == "step") {
								s = Script->TokStrList->Strings[j + 1];
								variedStepValue[variedCount] = (float)atof(s.c_str());
							}
						}
						variedCount++;
					}
				}
			}
			else if (Script->ScType == sct_variedD) {
				//
				// varriedD処理を行う。 コメントや#pragma用の置換
				//
				if (!InSubRegionF) { // SubRegion内の variedは、SubRegion内のみ有効
					if (Script->ScValDataList == NULL) {
						MainF->ErrMessage(i, "variedD の変数指定が見つかりません。");
						return;
					}
					if (Script->ScValDataList->Count == 0) {
						MainF->ErrMessage(i, "variedD の変数指定が見つかりません。");
						return;
					}
					for (k = 0; k < Script->ScValDataList->Count; k++) {
						ScValData = (TScValData*)Script->ScValDataList->Items[k];
						variedDValName[variedDCount] = ScValData->Str;
						variedDValData[variedDCount] = ScValData;
						DoToken[variedDCount] = NULL;
						for (j = 0; j < Script->TokStrList->Count - 1; j++) {
							s = Script->TokStrList->Strings[j];
							if (s == "from") {
								s = Script->TokStrList->Strings[j + 1];
								variedDFromValue[variedDCount] = (float)atof(s.c_str());
							}
							else if (s == "to") {
								s = Script->TokStrList->Strings[j + 1];
								variedDToValue[variedDCount] = (float)atof(s.c_str());
								variedDStepValue[variedDCount] = 1;
							}
							else if (s == "step") {
								s = Script->TokStrList->Strings[j + 1];
								variedDStepValue[variedDCount] = (float)atof(s.c_str());
							}
						}
						variedDCount++;
					}
				}
			}
			else if (Script->ScType == sct_fitting) {
				//
				// Fitting指定子の処理を行う。（TuneRegionへデータ複写のみ)
				//

				// d-spline用コード追加部分
				FittingDspline = Script->FittingDspline;
				FittingDynamic = Script->FittingDynamic;
				// d-spline用コード追加部分　ここまで

				FittingType = Script->FittingType;
				FittingDegree = Script->FittingDegree;
				SampledList = Script->SampledList;
				// }else if(Script->ScRegion == scr_substart){	// Sub region Start
			}
			else if ((Script->ScRegion == scr_substart) &&
				(Script->ScType == sct_select)) { // Sub region Start
				InSubRegionF = true;
				SubRegion = new TSubRegion();
				SubRegion->TokenStartPos = i;
				SubRegion->ScType = Script->ScType;
				SubRegion->AccordingStr = AccordingStr;
				SubRegionList->Add((void*)SubRegion);

				for (j = 0; j < variedCount; j++) { // varidCountを継承
					SubRegion->variedValName[j] = variedValName[j];
					SubRegion->variedValData[j] = variedValData[j];
					SubRegion->variedFromValue[j] = variedFromValue[j];
					SubRegion->variedStepValue[j] = variedStepValue[j];
					SubRegion->variedToValue[j] = variedToValue[j];
				}
				SubRegion->variedCount = variedCount;

				for (j = i; j <= TokenEndPos; j++) {
					Token = (TToken*)TokenList->Items[j];
					if (Token->Script != NULL) {
						Script = (TScript*)Token->Script;
						if (Script->ScRegion == scr_start) {
							// Nestしている場合は、region end まで Skipする。
							for (; j <= TokenEndPos; j++) {
								Token = (TToken*)TokenList->Items[j];
								if (Token->Script != NULL) {
									Script = (TScript*)Token->Script;
									if (Script->ScRegion == scr_end) {
										break;
									}
								}
							}
						}
						else if (Script->ScRegion == scr_subend) {
							break;
						}
						else if (Script->ScType == sct_according) { // Acoording
							TToken *Token2;

							UseAccordingF = true;
							SubRegion->AccordingStr = "";
							for (int k = j; k <= TokenEndPos; k++) {
								Token2 = (TToken*)TokenList->Items[k];
								if (Token2->TokId == tid_LineEnd) {
									break;
								}
								SubRegion->AccordingStr += Token2->OrgStr;
							}
						}
						else if (Script->ScType == sct_varied) {
							//
							// varried処理を行う。
							//
							if (Script->ScValDataList == NULL) {
								MainF->ErrMessage(i, "varied の変数指定が見つかりません。");
								return;
							}
							if (Script->ScValDataList->Count == 0) {
								MainF->ErrMessage(i, "varied の変数指定が見つかりません。");
								return;
							}
							for (k = 0; k < Script->ScValDataList->Count; k++) {
								ScValData = (TScValData*)
								Script->ScValDataList->Items[k];
								SubRegion->variedValName[variedCount]
								= ScValData->Str;
								SubRegion->variedValData[variedCount]
								= ScValData;
								for (k2 = 0;
									k2 < Script->TokStrList->Count - 1; k2++) {
									s = Script->TokStrList->Strings[k2];
									if (s == "from") {
										s = Script->TokStrList->Strings[k2 + 1];
										SubRegion->variedFromValue[variedCount]
											= (float)atof(s.c_str()); // 実数に対応
									}
									else if (s == "to") {
										s = Script->TokStrList->Strings[k2 + 1];
										SubRegion->variedToValue[variedCount] =
											(float)atof(s.c_str()); // 実数に対応
										SubRegion->variedStepValue[variedCount]	= 1;
									}
									else if (s == "step") {
										s = Script->TokStrList->Strings[k2 + 1];
										SubRegion->variedStepValue[variedCount]
										= (float)atof(s.c_str());
									}
								}
								SubRegion->variedCount++;
							}
						}
					}
				}
				SubRegion->TokenEndPos = j;
				if (j >= TokenEndPos) {
					MainF->ErrMessage(i, "sub region endが見つかりません。");
				}

			}
			else if (Script->ScRegion == scr_subend) {
				InSubRegionF = false;
			}
		}
	}
	MakeArgValList(); // TRのサブルーチンに渡す引数リストを生成する。
	//
	// Case数を計算する。
	// SubRegionがある場合は、各SubReguionの varied数だけのCaseの総和
	// SubRegionがない場合は、varied数だけのCase
	//
	if (SubRegionList->Count > 0) {
		int SubCaseCount;

		CaseCount = 0;
		for (j = 0; j < SubRegionList->Count; j++) {
			SubRegion = (TSubRegion*)SubRegionList->Items[j];
			SubCaseCount = 1;
			for (k = 0; k < SubRegion->variedCount; k++) { // SubRegionがない場合の
				/** ******************************************************************************************************************** */
				//
				// Kogakuin Irie
				// PPの実数対応とstep対応のための処理
				// 既存コードはコメントアウト
				//
				// SubCaseCount *= (SubRegion->variedToValue[k] - SubRegion->variedFromValue[k] + 1);
				SubCaseCount *= (int)
				((SubRegion->variedToValue[k] - SubRegion->variedFromValue[k]
					) / SubRegion->variedStepValue[k] + 1);
				//
				// ここまで
				//
				/** ******************************************************************************************************************** */
			}
			SubRegion->CaseCount = SubCaseCount;
			CaseCount += SubCaseCount;
		}
	}
	else {
		// SubRegionがない場合の処理
		CaseCount = 1;
		for (k = 0; k < variedCount; k++) { // SubRegionがない場合の
			/** **************************************************************************************************** */
			//
			// Kogakuin Irie
			// PPの実数対応とstep対応のための処理
			// 既存コードはコメントアウト
			//
			// CaseCount *= (variedToValue[k] - variedFromValue[k] + 1);
			CaseCount *= (int)((variedToValue[k] - variedFromValue[k])
				/ variedStepValue[k] + 1);
			//
			// ここまで
			//
			/** **************************************************************************************************** */
		}
		for (k = 0; k < variedDCount; k++) { // SubRegionがない場合の
			CaseCount *= (int)((variedDToValue[k] - variedDFromValue[k])
				/ variedDStepValue[k] + 1);
		}
		// Replace等はSubregionがない場合で使用する事を想定。
		if(ReplaceStrList->Count > 1){
			CaseCount *= ReplaceStrList->Count;
		}
		if((ListSrcStrList->Count != 0)&&(ListReplaceStrList->Count / ListSrcStrList->Count > 1)){
			CaseCount *= ListReplaceStrList->Count / ListSrcStrList->Count;
		}
		if((ListDSrcStrList->Count != 0)&&(ListDReplaceStrList->Count / ListDSrcStrList->Count > 1)){
			CaseCount *= ListDReplaceStrList->Count / ListDSrcStrList->Count;
		}
		if(GWV_ListList->Count != 0){
			// 種類数の積を求める。
			TStringList *GWV_LabelStrList;
			TStringList *GWV_ReplaceStrList;
			int TypeCount;

			TypeCount = 1;
			for(int k2 = 0; k2 < GWV_ListList->Count ; k2 +=2){
				GWV_LabelStrList = (TStringList *)GWV_ListList->Items[k2];
				GWV_ReplaceStrList = (TStringList *)GWV_ListList->Items[k2+1];
				TypeCount *= (GWV_ReplaceStrList->Count/3)/GWV_LabelStrList->Count;
			}
			CaseCount *= TypeCount;
		}
	}
	//
	// varied from to で定数に置換される変数は、引数リストから参照扱いをCUT
	// 現状、同一名で、片側は変数として使用、もう一方は引数と指定使用は出来ない。
	// 最終的には、エラーメッセージが必要？
	//
	for (i = 0; i < SubRegionList->Count; i++) {
		SubRegion = (TSubRegion*)SubRegionList->Items[i];
		for (k = 0; k < SubRegion->variedCount; k++) {
			s = SubRegion->variedValName[k];
			for (j = 0; j < ArgValList->Count; j++) {
				if (ArgValList->Strings[j] == s) {
					ArgValList->Strings[j] = "";
				}
			}
		}
	}
	for (k = 0; k < variedCount; k++) {
		s = variedValName[k];
		for (j = 0; j < ArgValList->Count; j++) {
			if (ArgValList->Strings[j] == s) {
				ArgValList->Strings[j] = "";
			}
		}
	}
	//
	// ArgValListを追加 ""(最初に代入で使用)も、必要なので、削除はしないこと
	//
	AddArgValListToATExecArgList();
	SaveAndResetValBits(TokenStartPos, TokenEndPos); // 変数識別を、保存してリセット
}
/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
//
// チューニングリージョンのサブルーチンに渡す引数リストを生成する。
// チューニングリージョン外との連携が必要な変数の一覧を作成する。
// チューニングリージョン内で最初に参照、チューニングリージョン外で後で参照(S
// UBROTIONの引数を含む）をチェックする。
//
// TokenEndPosの後までチェック（tid_end)して、参照があるかチェックする。
//
// DefPosS == -1 の外部定義（グローバル）変数は引数に追加しない形とした。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::MakeArgValList() {
	int i, Idx, j;
	TToken *Token, *Token2;
	TValData *ValData, *ValData2;
	string s;
	int DefValInsertIdx; // 配列等の定義に使用される引数は前に持って来る。

	for (i = TokenStartPos; i <= TokenEndPos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->ValData != NULL) {
			ValData = (TValData*)Token->ValData;
			if (ValData->DefPosS == -1) { // 未定義の変数
				// DP(ValData->Str);
				if (UndefinedArgValList->IndexOf(ValData) == -1) {
					UndefinedArgValList->Add(ValData);
				}
				continue; // 定義なしは、対象外。 モジュールやグローバル変数
			}
			if (ValData->ArrayOrFuncF && (ValData->ArrayLevel == 0)) {
				continue; // 関数は、対象外
			}
			Token2 = (TToken*)TokenList->Items[i + 1];
			if (Token2->TokId == tid_Kakko) {
				if (ValData->ArrayLevel == 0) {
					// 配列定義なしでのValName( 呼び出しも対象外
					continue;
				}
			}
			// 名前の一致で判定。
			if (MainF->SrcCodeType != MainF->sctC) {
				for (j = 0; j < ArgValList->Count; j++) {
					ValData2 = (TValData*)ArgValList->Objects[j];
					if (LowerCase(ValData->Str) == LowerCase(ValData2->Str)) {
						break;
					}
				}
			}
			else {
				for (j = 0; j < ArgValList->Count; j++) {
					ValData2 = (TValData*)ArgValList->Objects[j];
					if (ValData->Str == ValData2->Str) {
						break;
					}
				}
			}
			if (j < ArgValList->Count) {
				continue; // 追加済
			}
			// MainF->print(Token->Str);
			if (ValData->ArgF) { // サブルーチンの引数は、常に対象とする。
				ArgValList->AddObject(ValData->Str, (void*)ValData);
			}
			else if ((Token->RefType == vrf_ref) ||
				(Token->RefType == vrf_refset)) {
				// 最初に参照で使用されている変数
				ArgValList->AddObject(ValData->Str, (void*)ValData);
			}
			else if (Token->RefType == vrf_set) {
				// 最初に代入で使用されている変数（ＴＲ外での参照がなければ不要)
				ArgValList->AddObject("*", (void*)ValData);
			}
		}
	}
#if 1
	//
	// 引数リストの変数が配列で、その定義に変数を参照(Exp: Real D(1:nx))の場合
	// その変数も、引数リストに追加する。 Add 2007/1/20
	// これは、最初に代入で使用される変数も対象となる。
	//
	DefValInsertIdx = 0; // 配列等の定義に使用される引数は前に持って来る。
	for (i = 0; i < ArgValList->Count; i++) {
		if (ArgValList->Strings[i] == "") {
			continue;
		}
		ValData = (TValData*)ArgValList->Objects[i];
		if (!ValData->ArrayOrFuncF || (ValData->ArrayLevel == 0)) {
			continue; // 配列以外は、対象外
		}
		if (MainF->SrcCodeType == MainF->sctFortran90) {
			//
			// 配列等の定義に使われる変数を先に持って来る。
			//
			for (j = ValData->DefPosS; j < ValData->DefPosE; j++) {
				Token = (TToken*)TokenList->Items[j];
				ValData2 = (TValData*)Token->ValData;
				if (ValData2 == NULL) {
					continue; // 変数以外のトークン
				}
				Idx = ArgValList->IndexOfObject((void*)ValData2);
				if (Idx != -1) { // 追加済
					if (Idx <= DefValInsertIdx) {
						continue;
					}
					ArgValList->Delete(Idx);
				}
				ArgValList->InsertObject(DefValInsertIdx, ValData2->Str,
					(void*)ValData2);
				DefValInsertIdx++;
			}
			for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
				Token = (TToken*)TokenList->Items[j];
				ValData2 = (TValData*)Token->ValData;
				if (ValData2 == NULL) {
					continue; // 変数以外のトークン
				}
				Idx = ArgValList->IndexOfObject((void*)ValData2);
				if (Idx != -1) { // 追加済
					if (Idx <= DefValInsertIdx) {
						continue;
					}
					ArgValList->Delete(Idx);
				}
				ArgValList->InsertObject(DefValInsertIdx, ValData2->Str,
					(void*)ValData2);
				DefValInsertIdx++;
			}
		}
		else {
			for (j = ValData->DefPosS; j < ValData->DefPosE; j++) {
				Token = (TToken*)TokenList->Items[j];
				ValData2 = (TValData*)Token->ValData;
				if (ValData2 == NULL) {
					continue; // 変数以外のトークン
				}
				Idx = ArgValList->IndexOfObject((void*)ValData2);
				if (Idx != -1) { // 追加済
					continue;
				}
				ArgValList->AddObject(ValData2->Str, (void*)ValData2);
			}
			for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
				Token = (TToken*)TokenList->Items[j];
				ValData2 = (TValData*)Token->ValData;
				if (ValData2 == NULL) {
					continue; // 変数以外のトークン
				}
				Idx = ArgValList->IndexOfObject((void*)ValData2);
				if (Idx != -1) { // 追加済
					continue;
				}
				ArgValList->AddObject(ValData2->Str, (void*)ValData2);
			}
		}
	}
#endif
	//
	// ENDまで検索して、参照があるかを調べる。
	// リージョン内で Sum = 0 ... Loop外で参照なら Sumは引数になる。
	//
#if 1
	for (i = TokenEndPos; i < TokenList->Count; i++) {
		Token = (TToken*)TokenList->Items[i];

		/** ************************************************************************************************* */
		//
		// Kogakuin Irie
		// チューニング領域が複数あるときの対処
		// 2つ目以降のチューニング領域内は，検索対象外とする
		//
		/* ----------------------------------------------- */
		// region start の文字列を探す処理
		if (Token->Str == "region") {
			Token2 = (TToken*)TokenList->Items[++i];
			if (Token2->Str == "start") {
				/* ----------------------------------------------- */
				/* -------------------------------------------------------------------------------- */
				// 対応する region end を探す処理
				for (j = ++i; j < TokenList->Count; j++) {
					Token2 = (TToken*)TokenList->Items[j];
					if (Token2->Str == "region") {
						Token2 = (TToken*)TokenList->Items[++j];
						if (Token2->Str == "end") {
							// end の次のトークンを取得し，参照の有無を調べる処理に戻る
							i = ++j;
							Token = (TToken*)TokenList->Items[i];
							break;
						}
					}
				}
				/* -------------------------------------------------------------------------------- */
			}
		}
		/** ************************************************************************************************* */

		if (Token->NestLevel == 0) {
			break;
		}
		if (Token->ValData != NULL) {
			ValData = (TValData*)Token->ValData;

			Idx = ArgValList->IndexOfObject((void*)ValData);
			if (Idx == -1) {
				continue; // 対象外の変数
			}
			if (ArgValList->Strings[Idx] == "*") {
				if ((Token->RefType == vrf_ref) ||
					(Token->RefType == vrf_refset)) {
					ArgValList->Strings[Idx] = ValData->Str; // 使用にセット
				}
				else if (Token->RefType == vrf_set) { // 最初に代入で使用
					ArgValList->Strings[Idx] = ""; // 未使用にセット
				}
			}
		}
	}
#endif
	//
	// ArgValListの "*"[TR内で最初に代入で使用されて、その後参照されていない変数]
	// を "" としてクリアする。
	//
	for (i = 0; i < ArgValList->Count; i++) {
		ValData = (TValData*)ArgValList->Objects[i];
		// MainF->print("Arg["+ArgValList->Strings[i]+","+ValData->Str+"]");
		if (ArgValList->Strings[i] == "*") {
			ArgValList->Strings[i] = "";
		}
	}
#if 0
	//
	// 引数リストの変数が配列で、その定義に変数を参照(Exp: Real D(1:nx))の場合
	// その変数も、引数リストに追加する。 Add 2007/1/20
	// これは、最初に代入で使用される変数も対象となる。
	//
	for (i = 0; i < ArgValList->Count; i++) {
		if (ArgValList->Strings[i] == "") {
			continue;
		}
		ValData = (TValData*)ArgValList->Objects[i];
		if (!ValData->ArrayOrFuncF || (ValData->ArrayLevel == 0)) {
			continue; // 配列以外は、対象外
		}
		//
		//
		//
		for (j = ValData->DefPosS; j < ValData->DefPosE; j++) {
			Token = (TToken*)TokenList->Items[j];
			ValData2 = (TValData*)Token->ValData;
			if (ValData2 == NULL) {
				continue; // 変数以外のトークン
			}
			if (ArgValList->IndexOfObject((void*)ValData2) != -1) {
				continue; // 処理済み
			}
			ArgValList->AddObject(ValData2->Str, (void*)ValData2);
		}
		for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
			Token = (TToken*)TokenList->Items[j];
			ValData2 = (TValData*)Token->ValData;
			if (ValData2 == NULL) {
				continue; // 変数以外のトークン
			}
			if (ArgValList->IndexOfObject((void*)ValData2) != -1) {
				continue; // 処理済み
			}
			ArgValList->AddObject(ValData2->Str, (void*)ValData2);
		}
	}
#endif
	/*
	for(i = 0 ; i < ArgValList->Count ; i++){
	ValData = (TValData *)ArgValList->Objects[i];
	MainF->print("Arg["+ArgValList->Strings[i]+","+ValData->Str+"]");
	}
	 */
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// sPos    開始位置
//
// 3.概要
// UnRollの解析を行う。
// Type,Name,Do LoopEnd 式,iusw の変数の作成が行われる。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::MakeUnrollData(int sPos) {
	int i, j, k;
	TToken *Token;
	TScript *Script;
	string s;
	TScValData *ScValData;
	string ScValName;
	int Nest;
	int StepS, StepE;

	Token = (TToken*)TokenList->Items[sPos];

	//
	// UnRollの対象変数名を所得
	//
	ScValData = NULL;
	ScValName = "";

	//
	// 名前、参照変数（Setが先の場合は、不要）、DO変数、DOの終わり値を求める
	// とりあえず、一番外側のunrollのみ。複数は出力サンプルが必要。
	//
	// OAT_SetParam()
	// OAT_InsetallXXX() の呼び出しを追加する。
	//
	// この２つで、メイン側の呼び出しは終わり。
	//
	TValData *ValData;

	// Name指定なし時の対策 Add 2004/08/26
	Name = IntToStr(MainF->TuneRegionList->Count + 1);
	if (TuneGroup == tgInstall) {
		FuncName = "OAT_Install" + Name;
	}
	else if (TuneGroup == tgStatic) {
		FuncName = "OAT_Static" + Name;
	}
	else if (TuneGroup == tgDynamic) {
		FuncName = "OAT_Dynamic" + Name;
	}
	Nest = 0;
	ArgValList->Clear();
	for (i = sPos; i <= TokenEndPos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->Script != NULL) {
			Script = (TScript*)Token->Script;
			if (Script->ScRegion == scr_start) { // region Start
				Nest++; // TR ネスト部分の処理
				if (Nest > 1) {
					MainF->ErrMessage(i, "Unroll内のRegionネストは、サポートしていません。");
				}
				continue;
			}
			else if (Script->ScType == sct_name) { // name
				Name = Script->TokStrList->Strings[2];
				if (TuneGroup == tgInstall) {
					FuncName = "OAT_Install" + Name;
				}
				else if (TuneGroup == tgStatic) {
					FuncName = "OAT_Static" + Name;
				}
				else if (TuneGroup == tgDynamic) {
					FuncName = "OAT_Dynamic" + Name;
				}
			}
			else if (Script->ScType == sct_Number) { // number
				Number = atoi(Script->TokStrList->Strings[2].c_str());
			}
			else if (Script->ScType == sct_varied) {
				//
				// varried処理を行う。
				//
				if (Script->ScValDataList->Count == 0) {
					MainF->ErrMessage(i, "varied の変数指定が見つかりません。");
					return;
				}
				for (k = 0; k < Script->ScValDataList->Count; k++) {
					ScValData = (TScValData*)Script->ScValDataList->Items[k];
					variedValName[variedCount] = ScValData->Str;
					variedValData[variedCount] = ScValData;
					DoToken[variedCount] = NULL;
					for (j = 0; j < Script->TokStrList->Count - 1; j++) {
						s = Script->TokStrList->Strings[j];
						if (s == "from") {
							s = Script->TokStrList->Strings[j + 1];
							/** ************************************************************************ */
							//
							// Kogakuin Irie
							// PPの変動範囲を実数にも対応
							// 既存コードはコメントアウト
							//
							// variedFromValue[variedCount] = atoi(s.c_str());
							variedFromValue[variedCount] = (float)atof
							(s.c_str());
							//
							// ここまで
							//
							/** ************************************************************************ */
						}
						else if (s == "to") {
							s = Script->TokStrList->Strings[j + 1];
							/** ************************************************************************ */
							//
							// Kogakuin Irie
							// PPの変動範囲を実数にも対応
							// 既存コードはコメントアウト
							//
							// variedToValue[variedCount] = atoi(s.c_str());
							variedToValue[variedCount] = (float)atof(s.c_str());
							//
							// ここまで
							//
							/** ************************************************************************ */

							/** ************************************** */
							//
							// Kogakuin Irie
							// step（増分値の任意設定）対応のための処理
							//
							variedStepValue[variedCount] = 1;
							//
							// ここまで
							//
							/** ************************************** */
						}
						else if (s == "step") {
							s = Script->TokStrList->Strings[j + 1];
							variedStepValue[variedCount] = (float)atof
							(s.c_str());
						}
					}
					variedCount++;
				}
			}
			else if (Script->ScType == sct_fitting) {
				//
				// Fitting指定子の処理を行う。（TuneRegionへデータ複写のみ)
				//

				// d-spline用追加部分
				FittingDspline = Script->FittingDspline;
				FittingDynamic = Script->FittingDynamic;
				// d-spline用追加部分　ここまで

				FittingType = Script->FittingType;
				FittingDegree = Script->FittingDegree;
				SampledList = Script->SampledList;
			}
		}
		else if (Token->TokId == tid_DO) {
			//
			// DOの終わりの式を求める。Fortran用
			// DOの終わりの式は カンマとカンマの間（関数がある可能性があるので
			// ()のレベルチェックは行うこと！（関数作成）
			//
			TToken *Token1, *Token2;
			int kk, DoSPos;

			Token1 = Token; // DOのトークン
			DoSPos = i + 1;
			for (kk = 0; kk < 100; kk++) {
				Token = (TToken*)TokenList->Items[DoSPos++];
				if (Token->TokId == tid_Set) {
					break;
				}
			}
			//
			// スクリプトの変数名と一致するかを調べる。
			//
			Token2 = (TToken*)TokenList->Items[DoSPos - 2];
			for (kk = 0; kk < variedCount; kk++) {
				if (LowerCase(Token2->Str) == LowerCase(variedValName[kk])) {
					DoToken[kk] = Token1;
					DoValToken[kk] = Token2;
					ValData = (TValData*)Token2->ValData;
					ValData->DoValBits = (1 << kk); // 変数Bitsをセットする。
					ValData->RefDoValBits = (1 << kk); // 変数Bitsをセットする。
					DoStartSPos[kk] = DoSPos;
					DoStartEPos[kk] = GetNextKanmaPos(i);
					DoEndSPos[kk] = GetNextKanmaPos(i) + 1;
					DoEndEPos[kk] = GetNextKanmaPos(DoEndSPos[kk]);

					if (((TToken*)TokenList->Items[DoEndEPos[kk]])
						->TokId == tid_Kannma) {
						DoStepSPos[kk] = GetNextKanmaPos(DoEndEPos[kk]) + 1;
						DoStepEPos[kk] = GetNextKanmaPos(DoStepSPos[kk]);
					}
					else {
						DoStepSPos[kk] = -1;
						DoStepEPos[kk] = -1;
					}
					break;
				}
			}
		}
		else if (Token->TokId == tid_for) {
			//
			// Forの終りの式を求める。C言語用
			// forの終わりの式は １つめのセミコロンと２つめの間
			// ()のレベルチェックは行うこと！（関数作成）
			//
			TToken *Token1, *Token2;
			int kk, DoSPos;

			Token1 = Token; // Forのトークン
			DoSPos = i + 1;
			for (kk = 0; kk < 100; kk++) {
				Token = (TToken*)TokenList->Items[DoSPos++];
				if (Token->TokId == tid_Set) {
					break;
				}
			}
			//
			// スクリプトの変数名と一致するかを調べる。
			//
			Token2 = (TToken*)TokenList->Items[DoSPos - 2];
			for (kk = 0; kk < variedCount; kk++) {
				if (Token2->Str == variedValName[kk]) {
					DoToken[kk] = Token1;
					DoValToken[kk] = Token2;
					ValData = (TValData*)Token2->ValData;
					ValData->DoValBits = (1 << kk); // 変数Bitsをセットする。
					ValData->RefDoValBits = (1 << kk); // 変数Bitsをセットする。
					DoStartSPos[kk] = DoSPos;
					// DoStartEPos[kk] = GetNextSemikoronPos(i);
					// DoEndSPos[kk] = GetNextSemikoronPos(i) + 1;
					DoStartEPos[kk] = GetNextSemikoronPos(DoStartSPos[kk]);
					DoEndSPos[kk] = DoStartEPos[kk] + 1;
					DoEndEPos[kk] = GetNextSemikoronPos(DoEndSPos[kk]);

					if ((((TToken*)TokenList->Items[DoEndSPos[kk]])
							->TokId != tid_Val) ||
						(((TToken*)TokenList->Items[DoEndSPos[kk] + 1])
							->Str != "<")) {
						MainF->ErrMessage(i,
							"forのC for文の継続条件式には、変数<式 以外は使えません。");
						break;
					}
					// StepS = GetNextSemikoronPos(DoEndEPos[kk]) + 1;
					StepS = DoEndEPos[kk] + 1;
					StepE = GetNextSemikoronPos(StepS);
					// if(StepE != StepS+3){
					if (StepE != StepS + 2) {
						MainF->ErrMessage(i, "forの増分には、変数++ 以外は使えません。");
						break;
					}
					if (ValData->Str != ((TToken*)TokenList->Items[StepS])
						->Str) {
						MainF->ErrMessage(i, "forの増分には、変数++ 以外は使えません。");
						break;
					}
					// if((((TToken *)TokenList->Items[StepS+1])->Str != "+")||
					// (((TToken *)TokenList->Items[StepS+2])->Str != "+")){
					if (((TToken*)TokenList->Items[StepS + 1])->Str != "++") {
						MainF->ErrMessage(i, "forの増分には、変数++ 以外は使えません。");
						break;
					}
					DoStepSPos[kk] = -1;
					DoStepEPos[kk] = -1;
					break;
				}
			}
		}
	}
	MakeArgValList(); // TRのサブルーチンに渡す引数リストを生成する。
	//
	// 全アンロールのCase数を計算する。
	//
	CaseCount = 1;
	for (k = 0; k < variedCount; k++) {
		/** **************************************************************************************************** */
		//
		// Kogakuin Irie
		// PPの実数対応とstep対応のための処理
		// 既存コードはコメントアウト
		//
		// CaseCount *= (variedToValue[k] - variedFromValue[k] + 1);
		CaseCount *= (int)((variedToValue[k] - variedFromValue[k])
			/ variedStepValue[k] + 1); // step追加に対応
		//
		// ここまで
		//
		/** **************************************************************************************************** */
	}
	//
	for (k = 0; k < variedCount; k++) {
		//
		// 修正 8/26: Doの終わり値を式でもＯＫな形に修正。
		//
		for (int k2 = DoEndSPos[k]; k2 < DoEndEPos[k]; k2++) {
			Token = (TToken*)TokenList->Items[k2];
			ValData = (TValData*)Token->ValData;
			if (ValData != NULL) {
				ValData->DoEndValBits |= (1 << k); // Doの終わり値を示す変数を確定
			}
		}
	}
	//
	// ArgValListを追加 ""(最初に代入で使用)も、必要なので、削除はしないこと
	//
	AddArgValListToATExecArgList();

	//
	// 各変数がDo変数からの依存かどうかをチェックする。
	// Do依存の変数は、UnRollの数だけ、別変数に置き換えられる。
	//
	// DO i = 1,100 で da = b(i); の場合は、 daは、Do変数依存
	// a(i) = b(i) の場合は、 aに(Do変数 i)があるために依存でない。
	// da = b(i) で daがDo変数依存の場合に dc = 2*da での dcは、Do依存
	// da = b(i) で da = da + c(i) は、 daは Do依存だが、元々Do依存
	//
	//
	ChackRefDoValF(TokenStartPos, TokenEndPos);
	SaveAndResetValBits(TokenStartPos, TokenEndPos); // DoVal関連の識別を、保存してリセット
	// 使用時は、RestoreValBitsを行うこと。
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// sPos    開始位置
//
// 3.概要
// LoopFusionの解析を行う。
// Type,Name,Do LoopEnd 式,iusw の変数の作成が行われる。
// 現状では、Fortranのみに対応している。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::MakeLoopFusionSplitData(int sPos) {
	int i, j, k;
	TToken *Token;
	TScript *Script;
	string s;
	string ScValName;
	int DoNest;
	int StepS, StepE;
	TSubRegion *SubRegion;
	TToken *Token1, *Token2;
	int NestLevelOfDoNest[100];
	TValData *ValData;

	Token = (TToken*)TokenList->Items[sPos];
	ScValName = "";

	//
	// 名前、参照変数（Setが先の場合は、不要）、DO変数、DOの終わり値を求める
	// とりあえず、一番外側のunrollのみ。複数は出力サンプルが必要。
	//
	// OAT_SetParam()
	// OAT_InsetallXXX() の呼び出しを追加する。
	//
	// この２つで、メイン側の呼び出しは終わり。
	//
	// Name指定なし時の対策 Add 2004/08/26
	Name = IntToStr(MainF->TuneRegionList->Count + 1);
	if (TuneGroup == tgInstall) {
		FuncName = "OAT_Install" + Name;
	}
	else if (TuneGroup == tgStatic) {
		FuncName = "OAT_Static" + Name;
	}
	else if (TuneGroup == tgDynamic) {
		FuncName = "OAT_Dynamic" + Name;
	}
	ArgValList->Clear();
	DoNest = 0;
	for (i = sPos; i <= TokenEndPos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->Script != NULL) {
			Script = (TScript*)Token->Script;
			if ((Script->ScAction == sca_loopfusionsplit) ||
				(Script->ScAction == sca_loopfusion) ||
				(Script->ScAction == sca_loopsplit)) {
				if ((Script->ScRegion == scr_start) && (i > sPos)) {
					MainF->ErrMessage(i, "LoopFusionSpiltのネストは、サポートしていません。");
				}
				continue;
			}
			else if (Script->ScType == sct_name) { // name
				Name = Script->TokStrList->Strings[2];
				if (TuneGroup == tgInstall) {
					FuncName = "OAT_Install" + Name;
				}
				else if (TuneGroup == tgStatic) {
					FuncName = "OAT_Static" + Name;
				}
				else if (TuneGroup == tgDynamic) {
					FuncName = "OAT_Dynamic" + Name;
				}
			}
			else if (Script->ScType == sct_Number) { // number
				Number = atoi(Script->TokStrList->Strings[2].c_str());
			}
			else if (Script->ScType == sct_varied) {
				// LoopFusionSplitには variedなし。
			}
			else if (Script->ScType == sct_fitting) {
				// LoopFusionSplitには fittingなし
			}
			else if (Script->ScType == sct_SplitPoint) {
				// スプリット位置。 SplitPoint ( I,J,K )等の記述もあり。
				// １つのリージョンに複数ある場合あり。組み合わせで分割される。
				// SubRegionListとして追加される
				TStringList ScriptArgNameList;
				int k2;

				SubRegion = new TSubRegion();
				SubRegion->TokenStartPos = i;
				SubRegion->ScType = Script->ScType; // sct_SplitPoint
				//
				// SubRegion->SplitBitsOfFusionIdx[0]に切断可能DO変数Bitsをセットする。
				//
				SubRegion->SplitBitsOfFusionIdx[0] = 0;
				// FusionIdxによる Split有効無効Bits
				if (Script->TokStrList->Count >= 3) {
					for (int kk = 3; kk < Script->TokStrList->Count; kk++) {
						s = Script->TokStrList->Strings[kk];
						if ((s != "(") && (s != ")") && (s != ",") && (s != "")
							&& (s[0] > ' ')) {
							ScriptArgNameList.Add(LowerCase(s));
						}
					}
				}
				else { // DoVal変数指定なしの場合は、全てが対象。
					for (int kk = 0; kk < DoNest; kk++) { // 外側のLoopから
						SubRegion->SplitBitsOfFusionIdx[0] |= (1 << kk);
					}
				}
				for (int kk = 0; kk < DoNest; kk++) { // 外側のLoopから
					s = DoValName[kk];
					for (k2 = 0; k2 < ScriptArgNameList.Count; k2++) {
						// 名前指定がある場合は名前の一致するLoopのみ切断する。
						if (ScriptArgNameList.Strings[k2] == LowerCase(s)) {
							SubRegion->SplitBitsOfFusionIdx[0] |= (1 << kk);
						}
					}
				}
				SubRegionList->Add((void*)SubRegion);
			}
			else if (Script->ScType == sct_SplitPointCopyDef) {
				// スプリットCOPY元。 Resion StartとEndあり
				// 現状は名前がないので、１つのリージョンに１つのみ
				if (Script->ScRegion == scr_substart) { // region Start
					for (j = i + 1; j < TokenEndPos; j++) {
						Token = (TToken*)TokenList->Items[j];
						if (Token->TokId == tid_LineEnd) {
							break;
						}
					}
					SplitPointCopyDef_StartPos = j + 1; // Scriptの次から開始
				}
				else if (Script->ScRegion == scr_subend) { // region End
					SplitPointCopyDef_EndPos = i;
				}
				else {
					MainF->ErrMessage(i, "LoopPointCopyDef region err.");
				}
			}
			else if (Script->ScType == sct_SplitPointCopyInsert) {
				// スプリットCOPY先。CopyDefとInsertが別ループになった場合にのみ
				// コピーする。各スプリット後に複数存在可能。２回スプリットの場合は
				// ２回コピーされる場合もあり。
				// これについては、Split出力中に行われる。
			}
			else if (Script->ScType == sct_RotationOrder) {
				if (Script->ScRegion == scr_substart) {
					if ((RotaionOrderList->Count % 2) != 0) {
						MainF->ErrMessage(i, "RotationOrder sub region err.");
					}
					RotaionOrderList->Add((void*)((long)i));
				}
				else if (Script->ScRegion == scr_subend) {
					if ((RotaionOrderList->Count % 2) != 1) {
						MainF->ErrMessage(i, "RotationOrder sub region err.");
					}
					RotaionOrderList->Add((void*)((long)i));
				}
				else {
					MainF->ErrMessage(i, "RotationOrder sub region err.");
				}
			}
		}
		else if (Token->TokId == tid_DO) {
			//
			// DOの終わりの式を求める。Fortran用
			// DOの終わりの式は カンマとカンマの間（関数がある可能性があるので
			// ()のレベルチェックは行うこと！（関数作成）
			//
			int kk, DoSPos;

			NestLevelOfDoNest[DoNest] = Token->NestLevel;
			DoNest++;
			Token1 = Token; // DOのトークン
			DoSPos = i + 1;
			for (kk = 0; kk < 100; kk++) {
				Token = (TToken*)TokenList->Items[DoSPos++];
				if (Token->TokId == tid_Set) {
					break;
				}
			}
			//
			// スクリプトの変数名と一致するかを調べる。
			// LoopFusionSplitの場合は、variedなしで全てのDoが対象となる。
			// 同じDO変数名でのLoopはなしとする。（変数名で区別する）
			//
			Token2 = (TToken*)TokenList->Items[DoSPos - 2];
			for (kk = 0; kk < variedCount; kk++) {
				if (LowerCase(Token2->Str) == LowerCase(variedValName[kk])) {
					MainF->ErrMessage(DoSPos, "LoopFusionSplitで複数回、同じDoがあります。");
					break;
				}
			}
			if (kk >= variedCount) {
				kk = variedCount++;
				variedValName[kk] = Token2->Str;
				variedFromValue[kk] = 0;
				variedToValue[kk] = 0;
				/** ************************************** */
				//
				// Kogakuin Irie
				// step（増分値の任意設定）対応のための処理
				//
				variedStepValue[kk] = 1;
				//
				// ここまで
				//
				/** ************************************** */
				DoToken[kk] = Token1;
				DoValToken[kk] = Token2;
				ValData = (TValData*)Token2->ValData;
				if (ValData == NULL) {
					MainF->ErrMessage(DoSPos, "Do変数が不明です。");
					return;
				}
				DoValName[kk] = Token2->Str;
				ValData->DoValBits = (1 << kk); // 変数Bitsをセットする。
				ValData->RefDoValBits = (1 << kk); // 変数Bitsをセットする。
				DoStartSPos[kk] = DoSPos;
				DoStartEPos[kk] = GetNextKanmaPos(i);
				DoEndSPos[kk] = GetNextKanmaPos(i) + 1;
				DoEndEPos[kk] = GetNextKanmaPos(DoEndSPos[kk]);

				if (((TToken*)TokenList->Items[DoEndEPos[kk]])
					->TokId == tid_Kannma) {
					DoStepSPos[kk] = GetNextKanmaPos(DoEndEPos[kk]) + 1;
					DoStepEPos[kk] = GetNextKanmaPos(DoStepSPos[kk]);
				}
				else {
					DoStepSPos[kk] = -1;
					DoStepEPos[kk] = -1;
				}
			}
		}
		else if (Token->TokId == tid_ENDDO) {
			if (DoNest > 0) {
				DoNest--;
			}
		}
		else if (Token->TokId == tid_for) {
			//
			// Forの終りの式を求める。C言語用
			// forの終わりの式は １つめのセミコロンと２つめの間
			// ()のレベルチェックは行うこと！（関数作成）
			//
			TToken *Token1, *Token2;
			int kk, DoSPos;

			NestLevelOfDoNest[DoNest] = Token->NestLevel;
			DoNest++;
			Token1 = Token; // Forのトークン
			DoSPos = i + 1;
			for (kk = 0; kk < 100; kk++) {
				Token = (TToken*)TokenList->Items[DoSPos++];
				if (Token->TokId == tid_Set) {
					break;
				}
			}
			//
			// スクリプトの変数名と一致するかを調べる。
			//
			Token2 = (TToken*)TokenList->Items[DoSPos - 2];
			for (kk = 0; kk < variedCount; kk++) {
				if (Token2->Str == variedValName[kk]) {
					MainF->ErrMessage(DoSPos, "LoopFusionSplitで複数回、同じDoがあります。");
					break;
				}
			}
			if (kk >= variedCount) {
				kk = variedCount++;
				variedValName[kk] = Token2->Str;
				variedFromValue[kk] = 0;
				variedToValue[kk] = 0;
				/** ************************************** */
				//
				// Kogakuin Irie
				// step（増分値の任意設定）対応のための処理
				//
				variedStepValue[kk] = 1;
				//
				// ここまで
				//
				/** ************************************** */
				DoToken[kk] = Token1;
				DoValToken[kk] = Token2;
				ValData = (TValData*)Token2->ValData;
				if (ValData == NULL) {
					MainF->ErrMessage(DoSPos, "for変数が不明です。");
					return;
				}
				DoValName[kk] = Token2->Str;
				ValData->DoValBits = (1 << kk); // 変数Bitsをセットする。
				ValData->RefDoValBits = (1 << kk); // 変数Bitsをセットする。
				DoStartSPos[kk] = DoSPos;
				DoStartEPos[kk] = GetNextSemikoronPos(DoStartSPos[kk]);
				DoEndSPos[kk] = DoStartEPos[kk] + 1;
				DoEndEPos[kk] = GetNextSemikoronPos(DoEndSPos[kk]);

				if ((((TToken*)TokenList->Items[DoEndSPos[kk]])
						->TokId != tid_Val) ||
					(((TToken*)TokenList->Items[DoEndSPos[kk] + 1])
						->Str != "<")) {
					MainF->ErrMessage(i, "forのC for文の継続条件式には、変数<式 以外は使えません。");
					break;
				}
				StepS = DoEndEPos[kk] + 1;
				StepE = GetNextSemikoronPos(StepS);
				if (StepE != StepS + 2) {
					MainF->ErrMessage(i, "forの増分には、変数++ 以外は使えません。");
					break;
				}
				if (ValData->Str != ((TToken*)TokenList->Items[StepS])->Str) {
					MainF->ErrMessage(i, "forの増分には、変数++ 以外は使えません。");
					break;
				}
				if (((TToken*)TokenList->Items[StepS + 1])->Str != "++") {
					MainF->ErrMessage(i, "forの増分には、変数++ 以外は使えません。");
					break;
				}
				DoStepSPos[kk] = -1;
				DoStepEPos[kk] = -1;
				// break;
			}
		}
		else if ((DoNest > 0) && (Token->NestLevel < NestLevelOfDoNest
				[DoNest - 1])) {
			DoNest--;
		}
	}
	MakeArgValList(); // TRのサブルーチンに渡す引数リストを生成する。
	//
	// Case数を計算する。Fusion（ループのネスト数-1) * Split数となる。
	//
	// フィージョンありの場合は、ループネスト数のCaseが発生する。
	//
	if ((TuneKind == tkLoopFusionSplit) || (TuneKind == tkLoopFusion)) {
		FusionCount = variedCount;
#if 0
		// 常にネストなしでDo変数１つごとの分割のみは、FusionCount = 1とする。
		FusionCount = 1;
#endif
	}
	else {
		FusionCount = 1;
	}
	//
	// SubRegion(SplitPoint)のFusionIdxごとの有効・無効を設定する。
	//
	CaseCount = FusionCount;
	if ((TuneKind == tkLoopFusionSplit) || (TuneKind == tkLoopSplit)) {
		unsigned int BaseSplitBits; // 切断対象Do変数Bits
//		unsigned int LoopDoValBits; // Fusion後に有効なDo変数。 K_J フィージョンの場合は Kのみ有効。
		int DoValCount = variedCount;
		int SubCaseCount;
		int CaseCountOfFusuinIdx;
		string LoopDoValName;
		int SplitCount;

		for (k = 0; k < SubRegionList->Count; k++) {
			SubCaseCount = 0; // 2016/02/23
			SubRegion = (TSubRegion*)SubRegionList->Items[k];
			SubRegion->CaseCount = 0; // FusionIdxごとのCase数
			BaseSplitBits = SubRegion->SplitBitsOfFusionIdx[0];
			//
			// LoopFusionSplitのCase数を求める。
			// FusionIdx == 0 の場合は、SplitBitsOfFusionIdx[0] のビット数だけの位置＋１の数。
			// それ以外は、全体に対して Splitありとなしの２種類とする。
			//
#if 1
			//
			// Fusion なしの場合の Splitなし＋指定位置のSplit設定でのCase数に修正
			// 2013/08/06
			// Fusion SplitのSplit数の計算が間違っていたので修正した。2016/02/21
			//
			//
			SplitCount = 1;
			s = "[";
			for (DoValIdx = 0; DoValIdx < DoValCount; DoValIdx++) {
				s += DoValName[DoValIdx] + ",";
				if ((BaseSplitBits & (1 << DoValIdx)) != 0) {
					SplitCount++;
				}
			}
			SubCaseCount += SplitCount;
			s += "]";
			SubRegion->SplitCaseCountOfFusionIdx[0] = SubCaseCount;
			SubRegion->variedValName[0] = s;
			for (int FusionIdx = 1; FusionIdx < FusionCount; FusionIdx++) {
				s = "[";
				SplitCount = 1;
				for (DoValIdx = 0; DoValIdx < DoValCount - FusionIdx;
					DoValIdx++) {
					if ((BaseSplitBits & (1 << DoValIdx)) != 0) {
						SplitCount++;
					}
				}
				for (DoValIdx = 0; DoValIdx < DoValCount; DoValIdx++) {
					if (DoValIdx == DoValCount - 1) {
						s += DoValName[DoValIdx];
					}
					else if (DoValIdx < DoValCount - FusionIdx - 1) {
						s += DoValName[DoValIdx] + ",";
					}
					else {
						s += DoValName[DoValIdx] + "_";
					}
				}

				s += "]";
				SubRegion->SplitBitsOfFusionIdx[FusionIdx] = 1; // このFusionで有効。
				SubRegion->SplitCaseCountOfFusionIdx[FusionIdx] = SplitCount;
				SubCaseCount += SplitCount;
				SubRegion->variedValName[FusionIdx] = s;
			}
			SubRegion->CaseCount += SubCaseCount; // FusionIdxごとのCase数
#else
			for (int FusionIdx = 0; FusionIdx < FusionCount; FusionIdx++) {
				SplitBits = BaseSplitBits;
				LoopDoValBits = 0;
				LoopDoValName = "";
				for (DoValIdx = 0; DoValIdx < DoValCount; DoValIdx++) {
					if (DoValIdx < DoValCount - FusionIdx) {
						LoopDoValBits |= (1 << DoValIdx); // 切断可能Do変数。
						LoopDoValName += ",";
					}
					else {
						LoopDoValName += "_" + DoValName[DoValIdx];
					}
				}
				SubRegion->SplitBitsOfFusionIdx[FusionIdx] = SplitBits &
				LoopDoValBits;
				s = " [" + LoopDoValName + "] ";
				SubCaseCount = 1;
				for (DoValIdx = 0; DoValIdx < DoValCount; DoValIdx++) {
					if ((SubRegion->SplitBitsOfFusionIdx[FusionIdx] &
							(1 << DoValIdx)) != 0) {
						s += DoValName[DoValIdx] + ",";
						SubCaseCount++;
					}
				}
				SubRegion->variedValName[FusionIdx] = s;
				SubRegion->SplitCaseCountOfFusionIdx[FusionIdx] = SubCaseCount;
				SubRegion->CaseCount += SubCaseCount; // FusionIdxごとのCase数
			}
#endif
			SubRegion->variedCount = FusionCount;
			CaseCount *= SubCaseCount;
		}
		//
		// 全体のCase数を求める。
		//
		CaseCount = 0;
		for (int FusionIdx = 0; FusionIdx < FusionCount; FusionIdx++) {
			CaseCountOfFusuinIdx = 1;
			for (k = 0; k < SubRegionList->Count; k++) {
				SubRegion = (TSubRegion*)SubRegionList->Items[k];
				CaseCountOfFusuinIdx *= SubRegion->SplitCaseCountOfFusionIdx
				[FusionIdx];
			}
			CaseCount += CaseCountOfFusuinIdx;
		}
	}
	if (RotaionOrderList->Count != 0) {
		CaseCount *= 2;
	}
#if 1
	for (k = 0; k < variedCount; k++) {
		for (int k2 = DoEndSPos[k]; k2 < DoEndEPos[k]; k2++) {
			Token = (TToken*)TokenList->Items[k2];
			ValData = (TValData*)Token->ValData;
			if (ValData != NULL) {
				ValData->DoEndValBits |= (1 << k); // Doの終わり値を示す変数を確定
			}
		}
	}
	//
	// ArgValListを追加 ""(最初に代入で使用)も、必要なので、削除はしないこと
	//
	AddArgValListToATExecArgList();

	//
	// 各変数がDo変数からの依存かどうかをチェックする。
	// Do依存の変数は、UnRollの数だけ、別変数に置き換えられる。
	//
	// DO i = 1,100 で da = b(i); の場合は、 daは、Do変数依存
	// a(i) = b(i) の場合は、 aに(Do変数 i)があるために依存でない。
	// da = b(i) で daがDo変数依存の場合に dc = 2*da での dcは、Do依存
	// da = b(i) で da = da + c(i) は、 daは Do依存だが、元々Do依存
	//
	//
	ChackRefDoValF(TokenStartPos, TokenEndPos);
	SaveAndResetValBits(TokenStartPos, TokenEndPos); // DoVal関連の識別を、保存してリセット
	// 使用時は、RestoreValBitsを行うこと。
#endif
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// チューニングリージョン情報を改行入り文字列で所得（デバッグ用）
//
// 4.機能説明
//
// 5.戻り値
// 情報文字列
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
string TTuneRegion::GetInfo() {
	int i;
	TSubRegion *SubRegion;
	string s = "";
	string s2;
	TToken *Token;
	string grpName[] = {
		"Install", "Static", "Dynamic", "???"
	};
	string kindName[] = {
		"Define", "Variable", "Select", "Unroll", "LoopFusionSplit",
		"LoopFusion", "LoopSplit", "RotationOrder",
		"List", "VariableD","ListD", "Replace","GWV",
		"???"
	};
	TValData *ValData;

	s += grpName[TuneGroup] + " ";
	s += kindName[TuneKind] + " ";
	s += Name + "\n";
	if (ArgValList->Count == 0) {
		s += FuncName + "(";
	}
	else {
		for (i = 0; i < ArgValList->Count; i++) {
			if (i == 0) {
				s += FuncName + "(" + ArgValList->Strings[i];
			}
			else {
				s += "," + ArgValList->Strings[i];
			}
		}
	}
	s += ") \n";
	if (UndefinedArgValList->Count != 0) {
		s += "Undefined Val ( use module ) = (";
		for (i = 0; i < UndefinedArgValList->Count; i++) {
			ValData = (TValData*)UndefinedArgValList->Items[i];
			if (ValData != NULL) {
				s += ValData->Str + " ";
			}
		}
		s += ") \n";
	}
	if(ReplaceStrList->Count != 0){
		s2 = "  Replace ";
		for (int j = 0 ; j < ReplaceStrList->Count; j++) {
			s2 += "("+ReplaceStrList->Strings[j]+")";
		}
		s += s2 + "\n";
	}
	if(TargetStrList->Count != 0){
		for (int j = 0 ; j < TargetStrList->Count; j++) {
			TScript *Script = (TScript *)TargetStrList->Objects[j];
			s += "    Target "+TargetStrList->Strings[j]+" (TokPos="+IntToStr(Script->TokPos)+")\n";
		}
	}
	if(ListSrcStrList->Count != 0){
		s2 = "  List (";
		for (int j = 0 ; j < ListSrcStrList->Count; j++) {
			if(j != 0){
				s2 += ",";
			}
			s2 += ListSrcStrList->Strings[j];
		}
		s2 += ")";
		s2 += " with ";
		for (int j = 0 ; j < ListReplaceStrList->Count; j++) {
			if(j % ListSrcStrList->Count == 0){
				s2 += "(";
			}else{
				s2 += ",";
			}
			s2 += ListReplaceStrList->Strings[j];
			if(j % ListSrcStrList->Count == ListSrcStrList->Count-1){
				s2 += ")";
			}
		}
		s += s2 + "\n";
	}
	if(ListDSrcStrList->Count != 0){
		s2 = "  ListD (";
		for (int j = 0 ; j < ListDSrcStrList->Count; j++) {
			if(j != 0){
				s2 += ",";
			}
			s2 += ListDSrcStrList->Strings[j];
		}
		s2 += ")";
		s2 += " with ";
		for (int j = 0 ; j < ListDReplaceStrList->Count; j++) {
			if(j % ListDSrcStrList->Count == 0){
				s2 += "(";
			}else{
				s2 += ",";
			}
			s2 += ListDReplaceStrList->Strings[j];
			if(j % ListDSrcStrList->Count == ListDSrcStrList->Count-1){
				s2 += ")";
			}
		}
		s += s2 + "\n";
	}
	//
	//	GWV-Listの表示。ラベル列、置換え列が複数ある場合あり。
	//
	if(GWV_ListList->Count != 0){
		TStringList *GWV_LabelStrList;
		TStringList *GWV_ReplaceStrList;

		for(int i = 0 ; i < GWV_ListList->Count ; i+=2){
			GWV_LabelStrList = (TStringList *)GWV_ListList->Items[i];
			GWV_ReplaceStrList = (TStringList *)GWV_ListList->Items[i+1];

			s2 = "  GWV-List [";
			for(int j = 0 ; j < GWV_LabelStrList->Count ; j++){
				s2 += GWV_LabelStrList->Strings[j];
				if(j < GWV_LabelStrList->Count-1){
					s2 += ",";
				}
			}
			s2 += "]";
			for(int j = 0 ; j < GWV_ReplaceStrList->Count ; j+=3){
				if((j/3) % GWV_LabelStrList->Count == 0){
					if(j != 0){
						s2 += "]";

					}
					s2 += "[";
				}
				s2 += "("+GWV_ReplaceStrList->Strings[j];
				s2 += ","+GWV_ReplaceStrList->Strings[j+1];
				s2 += ","+GWV_ReplaceStrList->Strings[j+2]+")";
			}
			s2 += "]";
			s += s2 + "\n";
		}
	}
	if(GWV_TargetStrList->Count != 0){
		for (int j = 0 ; j < GWV_TargetStrList->Count; j++) {
			TScript *Script = (TScript *)GWV_TargetStrList->Objects[j];
			s += "    GWV-Target "+GWV_TargetStrList->Strings[j]+" (TokPos="+IntToStr(Script->TokPos)+")\n";
		}
	}
	for (i = 0; i < variedCount; i++) {
		s2 = "";
		for (int j = DoStartSPos[i]; j < DoStartEPos[i]; j++) {
			Token = (TToken*)MainF->TokenList->Items[j];
			s2 += Token->Str;
		}
		s2 += ",";
		for (int j = DoEndSPos[i]; j < DoEndEPos[i]; j++) {
			Token = (TToken*)MainF->TokenList->Items[j];
			s2 += Token->Str;
		}
		s2 += ",";
		for (int j = DoStepSPos[i]; j < DoStepEPos[i]; j++) {
			Token = (TToken*)MainF->TokenList->Items[j];
			s2 += Token->Str;
		}
		/** **************************************************** */
		//
		// Kogakuin Irie
		// PPの実数対応とstep対応のための処理
		// 既存コードはコメントアウト
		//
		/* s+= "    varied "+ variedValName[i] + " ("+s2+")"+
		" from " +IntToStr(variedFromValue[i])+
		" to " + IntToStr(variedToValue[i])  +"\n"; */
		s += "    varied " + variedValName[i] + " (" + s2 + ")" + " from " +
			FloatToStr(variedFromValue[i]) + " to " + FloatToStr
			(variedToValue[i]) + " step " + FloatToStr(variedStepValue[i])
			+ "\n";
		//
		// ここまで
		//
		/** **************************************************** */
	}
	for (i = 0; i < variedDCount; i++) {	// variedD
		s2 = "";
		s += "    variedD " + variedDValName[i] + " (" + s2 + ")" + " from " +
			FloatToStr(variedDFromValue[i]) + " to " + FloatToStr
			(variedDToValue[i]) + " step " + FloatToStr(variedDStepValue[i])
			+ "\n";
	}
	s += "  RefValStr = ( " + RefValStr + " )\n"; // デバッグ用（変数参照文字列リスト）
	s += "  Case Count = " + IntToStr(CaseCount) + "\n";

	// s+= "    SubRegion Count = "+ IntToStr(SubRegionList->Count)+"\n";
	for (i = 0; i < SubRegionList->Count; i++) {
		SubRegion = (TSubRegion*)SubRegionList->Items[i];
		if (SubRegion->ScType == sct_select) {
			s += "    Select    AccordingStr(" + IntToStr(i)
				+ ") = " + SubRegion->AccordingStr + "\n";
		}
		else if (SubRegion->ScType == sct_SplitPoint) {
			s2 = "";
			for (int k = 0; k < SubRegion->variedCount; k++) {
				s2 += SubRegion->variedValName[k];
				s2 += "#" + IntToStr(SubRegion->SplitCaseCountOfFusionIdx[k])
					+ " ";
			}
			s += "    SplitPoint (" + IntToStr(i) + ") = " + s2 + "\n";
		}
	}
	if (RotaionOrderList->Count != 0) {
		s += "    RotationOrder #2 \n";
	}
#if 0
	for (i = TokenStartPos; i <= TokenEndPos; i++) {
		s += IntToStr(i) + " : [" + ((TToken*)TokenList->Items[i])
			->OrgStr + "]\n";
	}
#endif
	return s;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// sPos    開始位置
//
// 3.概要
// 同レベルでの次のカンマまたは、行末を探して、その位置を返す。
// カンマがない場合は、行の最後を返す。
//
// 4.機能説明
//
// 5.戻り値
// カンマか行末のトークン位置
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::GetNextKanmaPos(int sPos) {
	int i;
	int Nest = 0;
	TToken *Token;

	for (i = sPos; i < TokenList->Count; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->TokId == tid_Kakko) {
			Nest++;
		}
		else if (Token->TokId == tid_Kokka) {
			Nest--;
		}
		else if ((Token->TokId == tid_Kannma) && (Nest == 0)) {
			return i;
		}
		else if (Token->LineEndF) {
			return i;
		}
	}
	return -1;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// sPos    開始位置
//
// 3.概要
// 次のセミコロンカンマまたは、)を探しして、その位置を返す。
//
// 4.機能説明
//
// 5.戻り値
// セミコロンか)のトークン位置
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::GetNextSemikoronPos(int sPos) {
	int i;
	int Nest = 0;
	TToken *Token;

	for (i = sPos; i < TokenList->Count; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->TokId == tid_Kakko) {
			Nest++;
		}
		else if (Token->TokId == tid_Kokka) {
			Nest--;
			if (Nest < 0) {
				return i;
			}
		}
		else if (Token->TokId == tid_Semikoron) {
			return i;
		}
		else if (Token->TokId == tid_Semikoron2) {
			return i;
		}
	}
	return -1;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// sPos    開始位置
// ePos    終了位置
//
// 3.概要
// 各変数がDo変数からの依存かどうかをチェックする。
// ValData->RefDoValがセットされる。
// Do依存の変数は、UnRollの数だけ、別変数に置き換えられる。
//
// DO i = 1,100 で da = b(i); の場合は、 daは、Do変数依存
// a(i) = b(i) の場合は、 aに(Do変数 i)があるために依存でない。
// da = b(i) で daがDo変数依存の場合に dc = 2*da での dcは、Do依存
// da = b(i) で da = da + c(i) は、 daは Do依存だが、元々Do依存
// Ｃ言語の場合は、DOでなく for(ValName=s;ValName<e;ValName++)に
// になるが、動作としてはFortranの場合と同じになる。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::ChackRefDoValF(int sPos, int ePos) {
	int i, j;
	TToken *Token;
	TValData *ValData, *ValData2;
	unsigned int RefDoValBits;
	unsigned int RefDoValBitsInArray;
	int DoValIdx;
	TValData *DoValData;

	for (i = sPos; i <= ePos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->ValData != NULL) {
			ValData = (TValData*)Token->ValData;
			if ((Token->RefType != vrf_set) && (Token->RefType != vrf_refset)) {
				continue;
			}
			RefDoValBits = 0;
			RefDoValBitsInArray = 0;
			//
			// 変数への代入が発生、参照変数を調べる。
			//
			i++; // Skip ValToken
			//
			// 配列への代入の場合は、添え字に、Do依存変数がないかを調べる。
			// もし、Do依存変数があれば、それでＯＫとなる。
			//
			for (; i < ePos; i++) {
				Token = (TToken*)TokenList->Items[i];
				if ((Token->TokId == tid_Set) || (Token->LineEndF)) {
					break;
				}
				if (Token->ValData != NULL) {
					ValData2 = (TValData*)Token->ValData;
					RefDoValBitsInArray |= ValData2->RefDoValBits;
				}
			}
			//
			// 行末まで、調べて参照変数にDO依存があるかをチェックする、
			//
			for (; i < ePos; i++) {
				Token = (TToken*)TokenList->Items[i];
				if (Token->LineEndF) {
					break;
				}
				if (Token->ValData != NULL) {
					ValData2 = (TValData*)Token->ValData;
					RefDoValBits |= ValData2->RefDoValBits;
				}
			}
			//
			// Do依存の結果を ValDataにセット
			//
			RefDoValBits &= ~RefDoValBitsInArray;
			/*
			//
			//	参照＆設定のトークン (Exp. S = S + A(I) )の場合は、
			//	ePosまでの間の後の式で参照があるかをチェックする。
			//	参照があれば、RefTypeを vrf_setとして扱い、UnRoll対象とする。
			//		Add 2009/02/24
			//
			if((RefDoValBits != 0)&&(SetValTokenRefType == vrf_refset)){
			for( ; i < ePos ; i++){
			Token = (TToken *)TokenList->Items[i];
			if((Token->ValData == ValData)&&(Token->RefType == vrf_ref)){
			break;
			}
			}
			if(i == ePos){
			// vrf_refset後の式での参照がなかったので対象外とする。
			// S = S + A(i) でのみ使用として解釈した。
			continue;
			}
			}
			 */
			ValData->RefDoValBits |= RefDoValBits;
		}
	}
	//
	// もし、Unroll 範囲外（UnrollDoValBitとの＆が０）で
	// UnrollDo変数を含まない式での参照がある変数はUnroll対象外とする。
	// ValData->RefDoValBitsの該当するBitをクリアする。
	// 参照＆設定についての処理はこちらになった。Add 2010/08/26
	//
	// 例えば、 Unroll k の状態で
	//
	// da1 = 0;
	// for(k = xxxl; ... ){  da1 = da1 + xxx; } の場合。
	// da1は Unrollの束縛以外での使用となる。
	//
	// for(k = xxxl; ... ){  da1 = 0 ; da1 = da1 + xxx; } の場合は
	// da1は Unroll内なので、多重化される。
	//

	//
	// DoValIdxのDo変数出現前にRefDoValBitsで参照ありの変数が出ていた場合
	// RedDoValBitsをリセットする。
	//
	for (DoValIdx = 0; DoValIdx < variedCount; DoValIdx++) {
		bool FindDoValF;

		DoValData = (TValData*)DoValToken[DoValIdx]->ValData;
		RefDoValBits = (1 << DoValIdx); // 変数Bits

		for (i = sPos; i <= ePos; i++) {
			Token = (TToken*)TokenList->Items[i];
			if (Token->ValData != NULL) {
				ValData = (TValData*)Token->ValData;
				if (ValData == DoValData) { // Do変数出現後はなにもしない。
					break;
				}
				if ((ValData->RefDoValBits & RefDoValBits) != 0) {
					//
					// 該当Ｄｏ変数を参照している変数あり。
					// 行末までチェックして、ＤＯ変数がなければリセットする。
					//
					FindDoValF = false;
					for (j = i; j <= ePos; j++) {
						Token = (TToken*)TokenList->Items[j];
						if (Token->LineEndF) {
							break;
						}
						if (Token->ValData != NULL) {
							if ((TValData*)Token->ValData == DoValData) {
								FindDoValF = true;
								break; // 行末までにＤｏ変数があった。
							}
						}
					}
					if (!FindDoValF) {
						//
						// 式の行末までにＤｏ変数が出現していない。
						// 参照をリセットする。
						//
						ValData->RefDoValBits &= ~RefDoValBits;
					}
				}
			}
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// sPos    開始位置
// ePos    終了位置
//
// 3.概要
//
// Val関連の状態を、一時的に保存してリセットする。RestoreValBitsによって復帰
// を行う。
// この時、デバッグ情報として、参照変数の文字列も作成する。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::SaveAndResetValBits(int sPos, int ePos) {
	int i;
	TToken *Token;
	TValData *ValData;

	RefValStr = "";
	ValBitsList->Clear();
	for (i = sPos; i <= ePos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->ValData != NULL) {
			ValData = (TValData*)Token->ValData;
			ValBitsList->Add((void*)(long)ValData->DoValBits);
			ValBitsList->Add((void*)(long)ValData->DoEndValBits);
			ValBitsList->Add((void*)(long)ValData->RefDoValBits);
		}
	}
	//
	// Bitsをリセットする。
	//
	for (i = sPos; i <= ePos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->ValData != NULL) {
			ValData = (TValData*)Token->ValData;
			if (ValData->RefDoValBits != 0) {
				RefValStr += Token->Str + ",";
			}
			ValData->DoValBits = 0;
			ValData->DoEndValBits = 0;
			ValData->RefDoValBits = 0;
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// sPos    開始位置
// ePos    終了位置
//
// 3.概要
// SaveAndResetValBitsで保存しておいた ValBits(ValData->xxxBits)を戻す。
// 新たな対象の解析の開始前に呼び出す。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::RestoreValBits(int sPos, int ePos) {
	int i;
	TToken *Token;
	TValData *ValData;
	int idx = 0;

	for (i = sPos; i <= ePos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->ValData != NULL) {
			ValData = (TValData*)Token->ValData;
			if (idx < ValBitsList->Count) {
				ValData->DoValBits = (long)ValBitsList->Items[idx++];
				ValData->DoEndValBits = (long)ValBitsList->Items[idx++];
				ValData->RefDoValBits = (long)ValBitsList->Items[idx++];
			}
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
// 引数リストのATExec用への追加
//
// 2.パラメタ説明
//
// 3.概要
// ArgValListをATExecの引数に一致すれば、一致する順に並べる。
// ない場合は、MainF->Call_ATExec_ArgListとScript->TokListに追加する。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::AddArgValListToATExecArgList() {
	int i, j;
	TValData *ValData, *ValData2;
	string MinStr;
	TScript *Script;
	int ArgCount;
	TToken *Token;
	TStringList StrList;
	TStringList NewArgValList;
	TStringList *ATExec_ArgValList; // ATExec()の３個目からの引数リスト（全体で１つ）
	string ValName;
	int idx;

	//
	// 最初に、ATExec_Scriptの引数があれば追加する。
	//
	if ((MainF->Call_ATExec_Script != NULL) &&
		(MainF->Call_ATExec_ArgList->Count == 0)) {
		ArgCount = 0;
		Script = MainF->Call_ATExec_Script;
		// 名前の一致する変数名が ArgValListにあるかを調べる。
		for (j = 0; j < Script->TokStrList->Count; j++) {
			Token = (TToken*)Script->TokStrList->Objects[j];
			// if(Token->TokId == tid_Kannma){
			if (Token->Str == ",") {
				ArgCount++;
			}
			if (ArgCount < 2) {
				continue; // 先頭の２つはSkip。
			}
			idx = ArgValList->IndexOf(Token->Str);
			if (idx != -1) {
				ValData2 = (TValData*)ArgValList->Objects[idx];
				MainF->Call_ATExec_ArgList->AddObject(ValData2->Str, ValData2);
			}
			// if(Token->ValData == NULL){
			// continue;
			// }
			// ValData2 = (TValData *)Token->ValData;
			// MainF->Call_ATExec_ArgList->AddObject(ValData2->Str,ValData2);
		}
	}
	ATExec_ArgValList = MainF->Call_ATExec_ArgList;
#if 0
	//
	// ABC順に並べる。call ATExec() 指定なしの場合の対応。
	// また、変数名をリストに追加する。
	//
	for (i = 0; i < ArgValList->Count; i++) {
		if (ArgValList->Strings[i] == "") {
			continue;
		}
		ValData = (TValData*)ArgValList->Objects[i];
		StrList.Add(ValData->Str);
		MinStr = "";
		MinIdx = -1;
		for (j = i + 1; j < ArgValList->Count; j++) {
			if (ArgValList->Strings[j] == "") {
				continue;
			}
			ValData = (TValData*)ArgValList->Objects[j];
			if (MinIdx == -1) {
				MinStr = ValData->Str;
				MinIdx = j;
			}
			if (ValData->Str < MinStr) {
				MinStr = ValData->Str;
				MinIdx = j;
			}
		}
		if (MinIdx != -1) {
			ArgValList->Exchange(i, MinIdx);
		}
	}
#endif
	//
	// 名前の一致する変数名が ATExec_ArgValListにあるかを調べる。
	// 並びもATExec_ArgValListと同じ順番にする。
	//
	for (i = 0; i < ArgValList->Count; i++) {
		ValData = (TValData*)ArgValList->Objects[i];
		ValName = ArgValList->Strings[i];
		if (ValName == "") {
			NewArgValList.AddObject("", ValData);
			continue;
		}
		// 名前の一致する変数名が ArgValListにあるかを調べる。
		for (j = 0; j < ATExec_ArgValList->Count; j++) {
			ValData2 = (TValData*)ATExec_ArgValList->Objects[j];
			if (ValData2->Str == ValName) {
				NewArgValList.AddObject(ValData2->Str, ValData2);
				break; // 名前の一致する変数名が Call_ATExec_Scriptに見つかった。
			}
		}
	}
	// 見つからない場合は、自動で追加する。
	for (i = 0; i < ArgValList->Count; i++) {
		ValData = (TValData*)ArgValList->Objects[i];
		ValName = ArgValList->Strings[i];
		if (ValName == "") {
			continue;
		}
		if (NewArgValList.IndexOf(ValName) == -1) {
			ATExec_ArgValList->AddObject(ValData->Str, ValData);
			NewArgValList.AddObject(ValData->Str, ValData);
		}
	}
	ArgValList->Clear();
	for (i = 0; i < NewArgValList.Count; i++) {
		ArgValList->AddObject(NewArgValList.Strings[i],
			NewArgValList.Objects[i]);
	}
	/*
	MainF->print("ArgValList->Strings #2 ");
	for(i = 0 ; i < ArgValList->Count ; i++){
	ValData = (TValData *)ArgValList->Objects[i];
	MainF->print(ArgValList->Strings[i] + ",["+ ValData->Str+"]");
	}
	 */
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// TempLineNo  作成する行番号のベース
//
// 3.概要
// OAT_Coontrol.f の ABCLob_SetParam()のコードを出力する。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputSetParamCode_Fortran(FILE *fp, int TempLineNo,FILE *fpAdd) {
	const char *F90Char;

	if (MainF->SrcCodeType == MainF->sctFortran90) {
		F90Char = " &";
	}
	else {
		F90Char = "";
	}

	fprintf(fp, "\n");
	fprintf(fp, "\n");
	fprintf(fp, "        if (index(OAT_Routine,'%s') .ne. 0) then\n",
		Name.c_str());

	// Fortran77でも有効へ　ATEXECと違いOAT.hと無関係なので問題なし。
//	if (MainF->SrcCodeType == MainF->sctFortran90) {
	if (true) {
		fprintf(fp, "\n");
		fprintf(fp, "      if (iusw1_%s_flag .eq. 0) then\n", Name.c_str());
		fprintf(fp, "\n");
		fprintf(fp, "        iusw1_%s_flag = 1\n", Name.c_str());
	}
	if (MainF->OMP_InnerF) {
		fprintf(fp, "!$omp flush(iusw1_%s_flag)\n", Name.c_str());
	}
	fprintf(fp, "\n");

	if (FittingType == 0) { // Fittingでない場合
		if (MainF->SrcCodeType == MainF->sctFortran90) {
			fprintf(fp, "          isw = 1\n");
		}
		else {
			fprintf(fp, "          isw = -1\n");
		}
		fprintf(fp, "          ibsw = 1\n");
		fprintf(fp, "%c         ---- file create\n", Comment);
		fprintf(fp, "%c         -----------------------------------------\n",
			Comment);
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "          if (oat_myid .eq. 0) then\n");
//		else {
//			fprintf(fp, "          if (myid .eq. 0) then\n");
//		}
		if (MainF->SrcCodeType == MainF->sctFortran90) {
			fprintf(fp,
				"!            print *, \"open OAT_Install%sParam.dat\"\n",
				Name.c_str());
		}
		fprintf(fp, "            open(21, status = 'old',%s\n", F90Char);
		fprintf(fp, "     &         file = 'OAT_%s%sParam.dat',%s\n",
			TuneGroupName.c_str(), Name.c_str(), F90Char);
		fprintf(fp, "     &         action = 'read', pad= 'yes', err =%d)\n",
			TempLineNo + 2);
		fprintf(fp, "\n");
		fprintf(fp, "%c           --- Seek object file\n", Comment);
		fprintf(fp, "            read(21, *) cbuf\n");
		fprintf(fp, "            do while (index(cbuf,'%s') .eq. 0)\n",
			Name.c_str());
		fprintf(fp, "              read(21, *) cbuf\n");
		fprintf(fp, "            enddo\n");
		fprintf(fp, "\n");
		fprintf(fp, "            do\n");
		fprintf(fp, "%c             --- Find problemsize\n", Comment);
		//
		// コンパイル時のワーニング回避のために、 FMT='A A'を FMT='(A A)' に修正した。
		// g95 コンパイラでのワーニング
		// 2007/1/18
		//
		// fprintf(fp,"              read(UNIT=21, FMT='A A', END=%d) cbuf\n",TempLineNo);
		/** ***************************************************************************** */
		//
		// Kogakuin Irie
		// 書式識別子の書き方を変更（コンパイルエラーが起きたため）
		// 既存コードはコメントアウト
		//
		// fprintf(fp,"              read(UNIT=21, FMT='(A A)', END=%d) cbuf\n",TempLineNo);
		fprintf(fp, "              read(UNIT=21, FMT='(A,A)', END=%d) cbuf\n",
			TempLineNo);
		//
		// ここまで
		//
		/** ***************************************************************************** */
		fprintf(fp,
			"              do while (index(cbuf,'OAT_PROBSIZE') .eq. 0)\n");
		// fprintf(fp,"                read(UNIT=21, FMT='A A', END=%d) cbuf\n",TempLineNo);
		/** ***************************************************************************** */
		//
		// Kogakuin Irie
		// 書式識別子の書き方を変更（コンパイルエラーが起きたため）
		// 既存コードはコメントアウト
		//
		// fprintf(fp,"                read(UNIT=21, FMT='(A A)', END=%d) cbuf\n",TempLineNo);
		fprintf(fp,
			"                read(UNIT=21, FMT='(A,A)', END=%d) cbuf\n"
			, TempLineNo);
		//
		// ここまで
		//
		/** ***************************************************************************** */
		fprintf(fp, "              enddo\n");
		fprintf(fp,
			"%c             -------------------------------------------\n",
			Comment);
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c             ---- find space\n", Comment);
		fprintf(fp,
//			"              i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')\n"
			"              oat_i = index(cbuf,'OAT_PROBSIZE')+len('OAT_PROBSIZE')\n"
			);
//		fprintf(fp, "              do while(cbuf(i:i) .eq. ' ')\n");
		fprintf(fp, "              do while(cbuf(oat_i:oat_i) .eq. ' ')\n");
//		fprintf(fp, "                i = i + 1\n");
		fprintf(fp, "                oat_i = oat_i + 1\n");
		fprintf(fp, "              enddo\n");
		fprintf(fp, "%c             ---------------------------------\n",
			Comment);
		fprintf(fp, "\n");
		fprintf(fp,
			"%c             ---- store digit and change it to integer\n", Comment);
//		fprintf(fp, "              j = 1\n");
		fprintf(fp, "              oat_j = 1\n");
//		fprintf(fp, "              do while(cbuf(i:i) .ne. ' ')\n");
		fprintf(fp, "              do while(cbuf(oat_i:oat_i) .ne. ' ')\n");
//		fprintf(fp, "                digit(j:j) = cbuf(i:i)\n");
		fprintf(fp, "                digit(oat_j:oat_j) = cbuf(oat_i:oat_i)\n");
//		fprintf(fp, "                i = i + 1\n");
		fprintf(fp, "                oat_i = oat_i + 1\n");
//		fprintf(fp, "                j = j + 1\n");
		fprintf(fp, "                oat_j = oat_j + 1\n");
		fprintf(fp, "              enddo\n");
		fprintf(fp, "%c             ---------------------------------\n",
			Comment);
//		fprintf(fp, "              digit(j:j) = ' '\n");
		fprintf(fp, "              digit(oat_j:oat_j) = ' '\n");
		if(fpAdd == NULL){
//			fprintf(fp, "              call OATCharToNum(digit, inum)\n");
			fprintf(fp, "              call OATCharToNum(digit, oat_inum)\n");
		}else{
//			fprintf(fp, "              call OATCharToNum_OAT(digit, inum)\n");
			fprintf(fp, "              call OATCharToNum_OAT(digit, oat_inum)\n");
		}
		fprintf(fp,
			"%c             -----------------------------------------\n", Comment);
		fprintf(fp, "\n");
		fprintf(fp, "%c             --- Find parameter\n", Comment);
		// fprintf(fp,"              read(UNIT=21, FMT='A A', END=%d) cbuf\n",TempLineNo);
		/** ***************************************************************************** */
		//
		// Kogakuin Irie
		// 書式識別子の書き方を変更（コンパイルエラーが起きたため）
		// 既存コードはコメントアウト
		//
		// fprintf(fp,"              read(UNIT=21, FMT='(A A)', END=%d) cbuf\n",TempLineNo);
		fprintf(fp, "              read(UNIT=21, FMT='(A,A)', END=%d) cbuf\n",
			TempLineNo);
		//
		// ここまで
		//
		/** ***************************************************************************** */
		fprintf(fp, "              do while (index(cbuf, '%s_I') .eq. 0)\n",
			Name.c_str());
		// fprintf(fp,"                 read(UNIT=21, FMT='A A', END=%d) cbuf\n",TempLineNo);
		/** ***************************************************************************** */
		//
		// Kogakuin Irie
		// 書式識別子の書き方を変更（コンパイルエラーが起きたため）
		// 既存コードはコメントアウト
		//
		// fprintf(fp,"                 read(UNIT=21, FMT='(A A)', END=%d) cbuf\n",TempLineNo);
		fprintf(fp,
			"                 read(UNIT=21, FMT='(A,A)', END=%d) cbuf\n", TempLineNo);
		//
		// ここまで
		//
		/** ***************************************************************************** */
		fprintf(fp, "              enddo\n");
		fprintf(fp,
			"%c             -------------------------------------------\n",
			Comment);
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c             ---- find space\n", Comment);
//		fprintf(fp, "              i = index(cbuf, '%s_I')+%d\n", Name.c_str(),
		fprintf(fp, "              oat_i = index(cbuf, '%s_I')+%d\n", Name.c_str(),
			(int)Name.length() + 2);
//		fprintf(fp, "              do while(cbuf(i:i) .eq. ' ')\n");
		fprintf(fp, "              do while(cbuf(oat_i:oat_i) .eq. ' ')\n");
//		fprintf(fp, "                i = i + 1\n");
		fprintf(fp, "                oat_i = oat_i + 1\n");
		fprintf(fp, "              enddo\n");
		fprintf(fp, "%c             ---------------------------------\n",
			Comment);
		fprintf(fp, "\n");
		fprintf(fp,
			"%c             ---- store digit and change it to integer\n", Comment);
//		fprintf(fp, "              j = 1\n");
		fprintf(fp, "              oat_j = 1\n");
//		fprintf(fp, "              do while(cbuf(i:i) .ne. ' ')\n");
		fprintf(fp, "              do while(cbuf(oat_i:oat_i) .ne. ' ')\n");
//		fprintf(fp, "                digit(j:j) = cbuf(i:i)\n");
		fprintf(fp, "                digit(oat_j:oat_j) = cbuf(oat_i:oat_i)\n");
//		fprintf(fp, "                i = i + 1\n");
		fprintf(fp, "                oat_i = oat_i + 1\n");
//		fprintf(fp, "                j = j + 1\n");
		fprintf(fp, "                oat_j = oat_j + 1\n");
		fprintf(fp, "              enddo\n");
		fprintf(fp, "%c             ---------------------------------\n",
			Comment);
//		fprintf(fp, "              digit(j:j) = ' '\n");
		fprintf(fp, "              digit(oat_j:oat_j) = ' '\n");
		if(fpAdd == NULL){
			fprintf(fp, "              call OATCharToNum(digit, ibsw)\n");
		}else{
			fprintf(fp, "              call OATCharToNum_OAT(digit, ibsw)\n");
		}
		fprintf(fp, "\n");
		/*
		if(BaseValList->Count > 0){
		fprintf(fp,-"%c             --- inputed %s is less than filed num?\n",BaseValList->Strings[0].c_str());
		fprintf(fp,"              if (%s .le. inum) then\n",BaseValList->Strings[0].c_str());
		}else{
		fprintf(fp,-"%c             --- inputed n is less than filed num?\n");
		fprintf(fp,"              if (n .le. inum) then\n");
		}
		 */
		// fprintf(fp,"%c             --- inputed n is less than filed num?\n",Comment);
		// fprintf(fp,"              if (n .le. inum) then\n");

		if (MainF->SrcCodeType == MainF->sctFortran90) {
			fprintf(fp,
				"%c             --- inputed n_bpset is less than filed num?\n",
				Comment);
//			fprintf(fp, "              if (n_bpset .le. inum) then\n");
			fprintf(fp, "              if (n_bpset .le. oat_inum) then\n");
		}
		else {
			fprintf(fp,
//				"c             --- inputed n is less than filed num?\n");
				"c             --- inputed n_bpset is less than filed num?\n");
//			fprintf(fp, "              if (n .le. inum) then\n");
//			fprintf(fp, "              if (n .le. oat_inum) then\n");
			fprintf(fp, "              if (n_bpset .le. oat_inum) then\n");
		}
		fprintf(fp, "                isw = ibsw\n");
		fprintf(fp, "                goto %d\n", TempLineNo);
		fprintf(fp, "              endif\n");
		fprintf(fp, "            enddo\n");
		fprintf(fp, "%c           === end of seeking loop for n\n", Comment);
		fprintf(fp, " %-6d     continue\n", TempLineNo);
		fprintf(fp, "\n");
		fprintf(fp, "%c           --- File close\n", Comment);
		fprintf(fp, "            close(21, status = 'keep')\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c           --- This is last parameter\n", Comment);
		fprintf(fp, " %-6d     if (isw .eq. -1) then\n", TempLineNo + 2);
		fprintf(fp, "              isw = ibsw\n");
		fprintf(fp, "            endif\n");
		fprintf(fp, "\n");
		fprintf(fp, "          endif\n");
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "%c         === end of oat_myid == 1\n", Comment);
//			fprintf(fp, "%c         === end of myid == 1\n", Comment);
		fprintf(fp, "\n");
	}
	else { // Fitting用の出力

		fprintf(fp, "%c         !!!!!fitting用処理\n", Comment);
		fprintf(fp, "%c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
			Comment);
		fprintf(fp, "\n");
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "          if (oat_myid .eq. 0) then\n");
//			fprintf(fp, "          if (myid .eq. 0) then\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c           === file open for LSM\n", Comment);
		// fprintf(fp,"            open(21, status = 'old', file = 'MyMM_I_LSM.dat',\n");
		fprintf(fp,
			"            open(21, status = 'old', file = '%s_%c_LSM.dat',%s\n",
			Name.c_str(), TuneGroupName[0], F90Char);
		fprintf(fp, "     &           action = 'read', pad= 'yes')\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c           !!! varidから分かるパラメタの総数\n", Comment);
		fprintf(fp, "            nparm = 8\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c           !!! polynolial x の値\n", Comment);
		fprintf(fp, "            m_lsm = 3\n");
		fprintf(fp, "\n");
		fprintf(fp, "            do isw=1, nparm\n");
		fprintf(fp, "              read (21, %d) dtemp,%s\n", TempLineNo,
			F90Char);
		fprintf(fp, "     &            (a_lsm(iii, isw-1), iii=0, m_lsm)\n");
		fprintf(fp, " %-6d       format(D20.10, 20D20.10)\n", TempLineNo);
		fprintf(fp, "            enddo\n");
		fprintf(fp, "\n");
		fprintf(fp, "            close(21, status = 'keep')\n");
		fprintf(fp, "\n");
//		fprintf(fp, "%c           do i=1, nparm\n", Comment);
		fprintf(fp, "%c           do oat_i=1, nparm\n", Comment);
		fprintf(fp, "%c              write (*, %d) dble(i),\n", Comment,
			TempLineNo);
//		fprintf(fp, "%c     &              (a_lsm(iii, i-1), iii=0, m_lsm)\n",
		fprintf(fp, "%c     &              (a_lsm(iii, oat_i-1), iii=0, m_lsm)\n",
			Comment);
		fprintf(fp, "%c           enddo\n", Comment);
		fprintf(fp, "\n");
		fprintf(fp, "          endif\n");
		fprintf(fp, "\n");
		fprintf(fp, "\n");

		if (MainF->MPIF) {
			fprintf(fp, "          call MPI_BCAST(a_lsm,%s\n", F90Char);
			fprintf(fp,
				"     &         (OATLSM_MAX_M+1)*OATLSM_MAX_NPARM,%s\n",
				F90Char);
			fprintf(fp, "     &          MPI_DOUBLE_PRECISION,0,%s\n", F90Char);
			fprintf(fp, "     &          MPI_COMM_WORLD,ierr)\n");
		}
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c          ==== Tuned Parameters do not exist.\n",
			Comment);
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "%c           if (oat_myid .eq. 0) then\n", Comment);
//			fprintf(fp, "%c           if (myid .eq. 0) then\n", Comment);
		fprintf(fp,
			"%c              print *, \"I estimate parameters by LSM.\"\n",
			Comment);
		fprintf(fp, "%c           endif\n", Comment);
		fprintf(fp, "\n");
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "%c           if (oat_myid .eq. 0) then\n", Comment);
//			fprintf(fp, "%c           if (myid .eq. 0) then\n", Comment);
		fprintf(fp, "%c             do i=1, nparm\n", Comment);
		fprintf(fp, "%c                write (*, %d) dble(i),\n", Comment,
			TempLineNo + 1);
		fprintf(fp,
			"%c     &              (a_lsm_TrdUd(iii, i-1), iii=0, OATLSM_MAX_M)\n"
			, Comment);
		fprintf(fp, "%c             enddo\n", Comment);
		fprintf(fp, "%c             print *, \"\"\n", Comment);
		fprintf(fp, "%c %-6d      format(D20.10, 20D20.10)\n", Comment,
			TempLineNo + 1);
		fprintf(fp, "%c           endif\n", Comment);
		fprintf(fp, "\n");
		fprintf(fp, "          call OATLSM_Est_Param(n, nparm, m_lsm,%s\n",
			F90Char);
		fprintf(fp, "     &               a_lsm, isw)\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c          print *, \"isw=\",isw\n", Comment);
		fprintf(fp, "\n");
		fprintf(fp, "%c         !!!!!fitting用処理 の終り\n", Comment);
		fprintf(fp, "%c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
			Comment);
		fprintf(fp, "\n");
	}
	fprintf(fp, "%c         === braodcast best param\n", Comment);
	if (MainF->MPIF) {
		fprintf(fp,
			"          call MPI_BCAST(isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)\n");
	}
	if (MainF->OMP_InnerF) {
		fprintf(fp, "!$omp flush(isw)\n");
	}
	else {
		fprintf(fp, "\n");
	}
	// Fortran77でも有効へ　ATEXECと違いOAT.hと無関係なので問題なし。
//	if (MainF->SrcCodeType == MainF->sctFortran90) {
	if (true) {
		fprintf(fp, "        endif\n");
	}
	fprintf(fp, "        return\n");	// Add 2016/02/26
	fprintf(fp, "        endif\n");
	fprintf(fp, "%c       === end of %s\n", Comment, Name.c_str());
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// TempLineNo  作成する行番号のベース
//
// 3.概要
// OAT_Coontrol.f の ABCLob_SetParam()のコードを出力する。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputSetParamCode_C(FILE *fp, int TempLineNo)
{
	fprintf(fp, "\t\tif(strstr(OAT_Routines,\"%s\") != 0){\n", Name.c_str());
	fprintf(fp, "\t\tif(OAT_iusw1_%s_flag == 0){\n", Name.c_str());
	fprintf(fp, "\t\t\tOAT_iusw1_%s_flag = 1;\n", Name.c_str());
//	fprintf(fp, "\t\tif((strstr(OAT_Routines,\"%s\") != 0)&&", Name.c_str());
//	fprintf(fp, "(iusw1_%s_flag == 0)){\n", Name.c_str());
//	fprintf(fp, "\t\t\tiusw1_%s_flag = 1;\n", Name.c_str());
	if (FittingType == 0) { // Fittingでない場合
		fprintf(fp, "\t\t\t*isw = -1;\n");
		fprintf(fp, "\t\t\tibsw = 1;\n");
		fprintf(fp, "//---- file create\n");
		fprintf(fp, "//-----------------------------------------\n");
		fprintf(fp, "\t\t\tif(myid == 0){\n");
		fprintf(fp, "\t\t\t\tfp = fopen(\"OAT_%s%sParam.dat\",\"rt\");\n",
			TuneGroupName.c_str(), Name.c_str());
		fprintf(fp, "\t\t\t\tif(fp == NULL){\n");
		fprintf(fp, "\t\t\t\t\tgoto L%d;\n", TempLineNo + 2);
		fprintf(fp, "\t\t\t\t}\n");
		/*
		fprintf(fp,"            open(21, status = 'old',\n");
		fprintf(fp,"     &         file = 'OAT_%s%sParam.dat',\n",TuneGroupName,Name);
		fprintf(fp,"     &         action = 'read', pad= 'yes', err =%d)\n",TempLineNo+2);
		fprintf(fp,"\n");
		 */
		fprintf(fp, "//--- Seek object file\n");
		fprintf(fp, "\t\t\t\tif(fgets(cbuf,sizeof(cbuf),fp) == NULL){\n");
		fprintf(fp, "\t\t\t\t\tgoto L%d;\n", TempLineNo);
		fprintf(fp, "\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\twhile(strstr(cbuf,\"%s\") == 0){\n", Name.c_str());
		fprintf(fp, "\t\t\t\t\tif(fgets(cbuf,sizeof(cbuf),fp) == NULL){\n");
		fprintf(fp, "\t\t\t\t\t\tgoto L%d;\n", TempLineNo);
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\twhile(1){\n");
		fprintf(fp, "//--- Find problemsize\n");
		fprintf(fp, "\t\t\t\t\tif(fgets(cbuf,sizeof(cbuf),fp) == NULL){\n");
		fprintf(fp, "\t\t\t\t\t\tgoto L%d;\n", TempLineNo);
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\t\twhile (strstr(cbuf,\"%s\") == 0){\n",
			"OAT_PROBSIZE");
		fprintf(fp, "\t\t\t\t\t\tif(fgets(cbuf,sizeof(cbuf),fp) == NULL){\n");
		fprintf(fp, "\t\t\t\t\t\t\tgoto L%d;\n", TempLineNo);
		fprintf(fp, "\t\t\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "// -------------------------------------------\n");
		fprintf(fp, "\n");
		fprintf(fp, "//---- find space\n");
		fprintf(fp,
//			"\t\t\t\t\ti = (char *)strstr(cbuf,\"OAT_PROBSIZE\")-cbuf+%d;\n",
			"\t\t\t\t\toat_i = (char *)strstr(cbuf,\"OAT_PROBSIZE\")-cbuf+%d;\n",
			(int)strlen("OAT_PROBSIZE"));
//		fprintf(fp, "\t\t\t\t\twhile(cbuf[i] == ' '){\n");
		fprintf(fp, "\t\t\t\t\twhile(cbuf[oat_i] == ' '){\n");
//		fprintf(fp, "\t\t\t\t\t\ti = i + 1;\n");
		fprintf(fp, "\t\t\t\t\t\toat_i = oat_i + 1;\n");
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "//---------------------------------\n");
		fprintf(fp, "\n");
		fprintf(fp, "//---- store digit and change it to integer\n");
//		fprintf(fp, "\t\t\t\t\tj = 0;\n");
		fprintf(fp, "\t\t\t\t\toat_j = 0;\n");
//		fprintf(fp, "\t\t\t\t\twhile(cbuf[i] > ' '){\n");
		fprintf(fp, "\t\t\t\t\twhile(cbuf[oat_i] > ' '){\n");
//		fprintf(fp, "\t\t\t\t\t\tdigit[j] = cbuf[i];\n");
		fprintf(fp, "\t\t\t\t\t\tdigit[oat_j] = cbuf[oat_i];\n");
//		fprintf(fp, "\t\t\t\t\t\ti = i + 1;\n");
		fprintf(fp, "\t\t\t\t\t\toat_i = oat_i + 1;\n");
//		fprintf(fp, "\t\t\t\t\t\tj = j + 1;\n");
		fprintf(fp, "\t\t\t\t\t\toat_j = oat_j + 1;\n");
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "//---------------------------------\n");
//		fprintf(fp, "\t\t\t\t\tdigit[j] = '\\0';\n");
		fprintf(fp, "\t\t\t\t\tdigit[oat_j] = '\\0';\n");
//		fprintf(fp, "\t\t\t\t\tinum = atoi(digit);\n");
		fprintf(fp, "\t\t\t\t\toat_inum = atoi(digit);\n");
		fprintf(fp, "//-----------------------------------------\n");
		fprintf(fp, "\n");
		fprintf(fp, "//--- Find parameter\n");
		fprintf(fp, "\t\t\t\t\tif(fgets(cbuf,sizeof(cbuf),fp) == NULL){\n");
		fprintf(fp, "\t\t\t\t\t\tgoto L%d;\n", TempLineNo);
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\t\twhile (strstr(cbuf,\"%s_I\") == 0){\n",
			Name.c_str());
		fprintf(fp, "\t\t\t\t\t\tif(fgets(cbuf,sizeof(cbuf),fp) == NULL){\n");
		fprintf(fp, "\t\t\t\t\t\t\tgoto L%d;\n", TempLineNo);
		fprintf(fp, "\t\t\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "//-------------------------------------------\n");
		fprintf(fp, "\n");
		fprintf(fp, "//---- find space\n");
//		fprintf(fp, "\t\t\t\t\ti = (char *)strstr(cbuf,\"%s_I\")-cbuf+%d;\n",
		fprintf(fp, "\t\t\t\t\toat_i = (char *)strstr(cbuf,\"%s_I\")-cbuf+%d;\n",
			Name.c_str(), (int)Name.length() + 2);
//		fprintf(fp, "\t\t\t\t\twhile(cbuf[i] == ' '){\n");
		fprintf(fp, "\t\t\t\t\twhile(cbuf[oat_i] == ' '){\n");
//		fprintf(fp, "\t\t\t\t\t\ti = i + 1;\n");
		fprintf(fp, "\t\t\t\t\t\toat_i = oat_i + 1;\n");
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "//---------------------------------\n");
		fprintf(fp, "\n");
		fprintf(fp, "//---- store digit and change it to integer\n");
//		fprintf(fp, "\t\t\t\t\tj = 0;\n");
		fprintf(fp, "\t\t\t\t\toat_j = 0;\n");
//		fprintf(fp, "\t\t\t\t\twhile(cbuf[i] > ' '){\n");
		fprintf(fp, "\t\t\t\t\twhile(cbuf[oat_i] > ' '){\n");
//		fprintf(fp, "\t\t\t\t\t\tdigit[j] = cbuf[i];\n");
		fprintf(fp, "\t\t\t\t\t\tdigit[oat_j] = cbuf[oat_i];\n");
//		fprintf(fp, "\t\t\t\t\t\ti = i + 1;\n");
		fprintf(fp, "\t\t\t\t\t\toat_i = oat_i + 1;\n");
//		fprintf(fp, "\t\t\t\t\t\tj = j + 1;\n");
		fprintf(fp, "\t\t\t\t\t\toat_j = oat_j + 1;\n");
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "//---------------------------------\n");
//		fprintf(fp, "\t\t\t\t\tdigit[j] = '\\0';\n");
		fprintf(fp, "\t\t\t\t\tdigit[oat_j] = '\\0';\n");
		fprintf(fp, "\t\t\t\t\tibsw = atoi(digit);\n");
		fprintf(fp, "\n");
		// fprintf(fp,"//--- inputed n is less than filed num?\n");
		// fprintf(fp,"\t\t\t\t\tif (n <= inum){\n"); // 2012/09/27
#if 0
		if (BaseValList->Count > 0) {
			fprintf(fp, "//--- inputed %s is less than filed num?\n",
				BaseValList->Strings[0].c_str());
//			fprintf(fp, "\t\t\t\t\tif (%s <= inum){\n",
			fprintf(fp, "\t\t\t\t\tif (%s <= oat_inum){\n",
				BaseValList->Strings[0].c_str());
		}
		else {
			// fprintf(fp,"//--- inputed n is less than filed num?\n");
			// fprintf(fp,"\t\t\t\t\tif (n <= inum){\n");
			fprintf(fp, "//--- inputed n_bpset is less than filed num?\n");
//			fprintf(fp, "\t\t\t\t\tif (n_bpset <= inum){\n");
			fprintf(fp, "\t\t\t\t\tif (n_bpset <= oat_inum){\n");
		}
#else
		// 常に引数の n_bpsetを使用。fortranと合わせる。
		fprintf(fp, "//--- inputed n_bpset is less than filed num?\n");
		fprintf(fp, "\t\t\t\t\tif (n_bpset <= oat_inum){\n");
#endif
		fprintf(fp, "\t\t\t\t\t\t*isw = ibsw;\n");
		fprintf(fp, "\t\t\t\t\t\tgoto L%d;\n", TempLineNo);
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\t}\n");
		fprintf(fp, "//=== end of seeking loop for n\n");
		fprintf(fp, "L%d:\n", TempLineNo);
		fprintf(fp, "\n");
		fprintf(fp, "//--- File close\n");
		fprintf(fp, "\t\t\t\tfclose(fp);\n");
		fprintf(fp, "\n");
		fprintf(fp, "//--- This is last parameter\n");
		fprintf(fp, "L%d:\n", TempLineNo + 2);
		fprintf(fp, "\t\t\t\tif (*isw == -1){\n");
		fprintf(fp, "\t\t\t\t\t*isw = ibsw;\n");
		fprintf(fp, "\t\t\t\t}\n");
		fprintf(fp, "\n");
		fprintf(fp, "\t\t\t}\n");
		fprintf(fp, "//=== end of myid == 1\n");
		fprintf(fp, "\n");
	}
	else { // Fitting用の出力

		fprintf(fp, "//!!!!!fitting用処理\n");
		fprintf(fp, "//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
		fprintf(fp, "\n");
		fprintf(fp, "          if(myid == 0){\n");
		fprintf(fp, "\n");
		fprintf(fp, "//          === file open for LSM\n");
		// fprintf(fp,"            fp = fopen(\"OAT_%s%sParam.dat\",\"rt\");\n",TuneGroupName,Name);
		fprintf(fp, "            fp = fopen(\"%s_%c_LSM.dat\",\"rt\");\n",
			Name.c_str(), *TuneGroupName.c_str());
		fprintf(fp, "\n");
		fprintf(fp, "//          !!! varidから分かるパラメタの総数\n");
		// fprintf(fp,"            nparm = 5;\n");
		fprintf(fp, "            nparm = %d;\n", CaseCount);
		fprintf(fp, "\n");
		fprintf(fp, "//           !!! polynolial x の値\n");
		fprintf(fp, "            m_lsm = 3;\n");
		fprintf(fp, "\n");
//		fprintf(fp, "            for(j = 0 ; j < nparm ; j++){\n");
		fprintf(fp, "            for(oat_j = 0 ; oat_j < nparm ; oat_j++){\n");
		fprintf(fp, "              fgets(cbuf,sizeof(cbuf),fp);\n");
		fprintf(fp, "              dtemp = atof(cbuf);\n");
		fprintf(fp, "              for(iii=0 ; iii <= m_lsm ; iii++){\n");
//		fprintf(fp, "                a_lsm[iii][j] = atof(cbuf+20*(iii+1));\n");
		fprintf(fp, "                a_lsm[iii][oat_j] = atof(cbuf+20*(iii+1));\n");
		fprintf(fp, "              }\n");
		fprintf(fp, "            }\n");
		fprintf(fp, "            fclose(fp);\n");
		fprintf(fp, "\n");
		fprintf(fp, "//           do i=1, nparm\n");
		fprintf(fp, "//              write (*, %d) dble(i),\n", TempLineNo);
		fprintf(fp, "//     &              (a_lsm(iii, i-1), iii=0, m_lsm)\n");
		fprintf(fp, "//           enddo");
		fprintf(fp, "\n");
		fprintf(fp, "          }\n");
		fprintf(fp, "\n");
		fprintf(fp, "\n");

		if (MainF->MPIF) {
			fprintf(fp, "      MPI_BCAST(a_lsm,\n");
			fprintf(fp, "              (OATLSM_MAX_M+1)*OATLSM_MAX_NPARM,\n");
			fprintf(fp, "               MPI_DOUBLE_PRECISION,0,\n");
			fprintf(fp, "               MPI_COMM_WORLD,ierr)\n");
		}
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "//          ==== Tuned Parameters do not exist.\n");
		fprintf(fp, "//           if (oat_myid .eq. 0) then\n");
		fprintf(fp,
			"//              print *, \"I estimate parameters by LSM.\"\n");
		fprintf(fp, "//           endif\n");
		fprintf(fp, "\n");
		fprintf(fp, "//           if (oat_myid .eq. 0) then\n");
		fprintf(fp, "//             do i=1, nparm\n");
		fprintf(fp, "//                write (*, %d) dble(i),\n",
			TempLineNo + 1);
		fprintf(fp,
			"//     &              (a_lsm_TrdUd(iii, i-1), iii=0, OATLSM_MAX_M)\n");
		fprintf(fp, "//            enddo\n");
		fprintf(fp, "//             print *, \"\"\n");
		fprintf(fp, "// %-6d      format(D20.10, 20D20.10)\n", TempLineNo + 1);
		fprintf(fp, "//           endif\n");
		fprintf(fp, "\n");
		fprintf(fp,
			"          OATLSM_Est_Param(n, nparm, m_lsm,a_lsm, isw);\n");
		fprintf(fp, "\n");
		fprintf(fp, "//          print *, \"isw=\",*isw\n");
		fprintf(fp, "\n");
		fprintf(fp, "//!!!!!fitting用処理 の終り\n");
		fprintf(fp, "//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
		fprintf(fp, "\n");
	}
	fprintf(fp, "//=== braodcast best param\n");
	if (MainF->MPIF) {
		fprintf(fp,
			"\t\tMPI_BCAST(*isw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)\n");
	}
	fprintf(fp, "\n");
	fprintf(fp, "\t\t}\n");

	fprintf(fp, "\t\treturn;\n");
	fprintf(fp, "\t\t}\n");
	fprintf(fp, "//=== end of %s\n", Name.c_str());
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
//
// 3.概要
// 時間計測を行うための ATexec サブルーチンのコードの生成を行う。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputAutoExecCode_Fortran(FILE *fp) {
	int idx;
	int i;
	string s;
	TValData *ValData;
	int CaseArrayCount;
	string ValName;
	string BaseValName;
	string ArgStr;
	bool VisualF = MainF->VisualF;
	const char *F90Char;

	if (MainF->SrcCodeType == MainF->sctFortran90) {
		F90Char = " &";
	}
	else {
		F90Char = "";
	}

	BaseValName = BaseValList->Strings[0];
	CaseArrayCount = CaseCount;
	if (FittingType != 0) {
		CaseArrayCount = SampledList->Count;
	}
	fprintf(fp, "%c     ==== %s Optimization Routines\n", Comment,
		TuneGroupName.c_str());
	fprintf(fp,
		"%c     ==============================================================\n"
		, Comment);
	/** ************************************************************************************************** */
	//
	// Kogakuin Irie
	// 引数が不足しているため修正
	// 既存コードはコメントアウト
	//
	// if(MainF->SrcCodeType == MainF->sctFortran90){
	// if(TuneGroup != tgDynamic){
	// ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false,"","");
	// s = "      subroutine OAT_ATexec"+TuneGroupName+Name+"(OAT_Routines";
	// s += ArgStr + ")";
	// s = SepLongStr(s);
	// fprintf(fp,"%s\n",s.c_str());
	// }else{
	// ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false,"","");
	// s = "      subroutine OAT_ATexec"+TuneGroupName+Name+"(OAT_Routines";
	// s += ArgStr + ")";
	// s = SepLongStr(s);
	// fprintf(fp,"%s\n",s.c_str());
	// }
	// if(MainF->MPIF){
	// fprintf(fp,"      use mpi\n");
	// }
	// fprintf(fp,"      character*%d OAT_Routines\n",MainF->CharMaxLen);
	// }else{
	// if(TuneGroup != tgDynamic){
	// ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false,"","");
	// s = "      subroutine OAT_ATexec"+TuneGroupName+Name+"(OAT_Routines";
	// s += ArgStr + ")";
	// s = SepLongStr(s);
	// fprintf(fp,"%s\n",s.c_str());
	// fprintf(fp,"      character*%d OAT_Routines\n",MainF->CharMaxLen);
	// }else{
	// s = "      subroutine OAT_ATexec"+TuneGroupName+Name+"(OAT_Routines,"+
	// BaseValName+",iBestSw1";
	// s += ")";
	// s = SepLongStr(s);
	// fprintf(fp,"%s\n",s.c_str());
	// fprintf(fp,"      character*%d OAT_Routines\n",MainF->CharMaxLen);
	// fprintf(fp,"      integer %s\n",BaseValName.c_str());
	// fprintf(fp,"      integer iBestSw1\n");
	// }
	// }
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		if (TuneGroup != tgDynamic) {
			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				ArgStr = Script->GetATExecArgStr(false, "", "");
				s = "      subroutine OAT_ATexec" + TuneGroupName + Name +
					"(OAT_Routines";
				s += ArgStr + ")";
				s = SepLongStr(s);
				fprintf(fp, "%s\n", s.c_str());
				delete Script;
			}
			else {
				ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "",
					"");
				s = "      subroutine OAT_ATexec" + TuneGroupName + Name +
					"(OAT_Routines";
				s += ArgStr + ")";
				s = SepLongStr(s);
				fprintf(fp, "%s\n", s.c_str());
			}
		}
		else {
			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				ArgStr = Script->GetATExecArgStr(false, "", "");
				s = "      subroutine OAT_ATexec" + TuneGroupName + Name +
					"(OAT_Routines,iBestSw1";
				s += ArgStr + ")";
				s = SepLongStr(s);
				fprintf(fp, "%s\n", s.c_str());
				delete Script;
			}
			else {
				ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "",
					"");
				s = "      subroutine OAT_ATexec" + TuneGroupName + Name +
					"(OAT_Routines,iBestSw1";
				s += ArgStr + ")";
				s = SepLongStr(s);
				fprintf(fp, "%s\n", s.c_str());
			}
		}
		fprintf(fp, "      character*%d OAT_Routines\n", MainF->CharMaxLen);
		if (TuneGroup == tgDynamic) {
			fprintf(fp, "      integer iBestSw1\n");
		}
	}
	else {
		if (TuneGroup != tgDynamic) {
			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				ArgStr = Script->GetATExecArgStr(false, "", "");
				s = "      subroutine OAT_ATexec" + TuneGroupName + Name +
					"(OAT_Routines";
				s += ArgStr + ")";
				s = SepLongStr(s);
				fprintf(fp, "%s\n", s.c_str());
				delete Script;
			}
			else {
				ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "",
					"");
				s = "      subroutine OAT_ATexec" + TuneGroupName + Name +
					"(OAT_Routines";
				s += ArgStr + ")";
				s = SepLongStr(s);
				fprintf(fp, "%s\n", s.c_str());
			}
			fprintf(fp, "      character*%d OAT_Routines\n", MainF->CharMaxLen);
		}
		else {
			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				ArgStr = Script->GetATExecArgStr(false, "", "");
				s = "      subroutine OAT_ATexec" + TuneGroupName + Name +
					"(OAT_Routines,iBestSw1";
				s += ArgStr + ")";
				s = SepLongStr(s);
				fprintf(fp, "%s\n", s.c_str());
				delete Script;
			}
			else {
				ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "",
					"");
				s = "      subroutine OAT_ATexec" + TuneGroupName + Name +
					"(OAT_Routines,iBestSw1";
				s += ArgStr + ")";
				s = SepLongStr(s);
				fprintf(fp, "%s\n", s.c_str());
			}
			fprintf(fp, "      character*%d OAT_Routines\n", MainF->CharMaxLen);
			fprintf(fp, "      integer iBestSw1\n");
		}
	}
	//
	// ここまで
	//
	/** ************************************************************************************************** */

	//
	// チューニングに使用する引数宣言
	// 配列の添え字部分が、N 一致する場合は、NN に置換（変数名固定）
	// 配列の添え字部分が、基本パラメタ名(N等) 一致する場合は、
	// 基本パラメタ名*2(NN等） に置換（変数名固定->可変 2009/03/05）
	//
	idx = Pass4->OAT_ValList->IndexOf("OAT_ENDTUNESIZE");
	if (idx == -1) {
		//
		// OAT_ENDTUNESIZEがない場合は、 PROB_VALN,PROB_VALNNの変数を検索して
		// その Parm値を使用する。（N,NN固定が前提となる）
		//
		int kk;

		ValName = BaseValList->Strings[0];
		for (kk = 0; kk < ValDataList->Count; kk++) {
			ValData = (TValData*)ValDataList->Items[kk];
			if (ValData == NULL) {
				continue;
			}
			if (ValData->ParamValF == false) {
				continue;
			}
			if (LowerCase(ValData->Str) == LowerCase(ValName)) {
				break;
			}
			// Add 2004/11/12 NN=xxx でもＯＫとした。Fittingサンプル
			if (LowerCase(ValData->Str) == LowerCase(ValName + ValName)) {
				break;
			}
		}
		if (kk >= ValDataList->Count) {
			MainF->ErrMessage(-1, "parameter (" + ValName + "=)が設定されていません。");
			return;
		}
	}
	else {
		// EndTuneSizeData = (int)Pass4->OAT_ValList->Objects[idx];
	}
	//
	// ArgValでの引数リストを追加。Fortran用は、改行コードを含む。
	// ここの呼び出しの引数までは、AtExec()の並びと合わせる。2013/03/17
	//
	// if(MainF->SrcCodeType == MainF->sctFortran90){
	if (true) { // 常に引数を渡す形に修正。2015/03/25
		if (MainF->Call_ATExec_Script != NULL) {
			ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true, "", "");
			fprintf(fp, "%s", ArgStr.c_str());
		}
		/** ******************************************************************** */
		//
		// Kogakuin Irie
		// 仮引数の宣言部分を追加
		//
		else {
			TScript *Script = new TScript(MainF->TokenList, 0, NULL,
				ValDataList);
			ArgStr = Script->GetATExecArgStr(true, "", "");
			fprintf(fp, "%s", ArgStr.c_str());
			delete Script;
		}
		//
		// ここまで
		//
		/** ******************************************************************** */
	}
	else {

		/** ******************************************************************** */
		//
		// Kogakuin Irie
		// 仮引数の宣言部分を追加
		//
		if (MainF->Call_ATExec_Script == NULL) {
			TScript *Script = new TScript(MainF->TokenList, 0, NULL,
				ValDataList);
			ArgStr = Script->GetATExecArgStr(true, "", "");
			fprintf(fp, "%s", ArgStr.c_str());
			delete Script;
		}
		//
		// ここまで
		//
		/** ******************************************************************** */
		int DefPosS;
		int ParamValIdx;
		int BaseValIdx;
		TToken *Token;
		int j;

		DefPosS = -1;
		s = "";
		for (i = 0; i < ArgValList->Count; i++) {
			if (TuneGroup == tgDynamic) {
				break; // Dynamic
			}
			if (ArgValList->Strings[i] == "") {
				continue;
			}
			ValData = (TValData*)ArgValList->Objects[i];
			ValName = ValData->Str;
			//
			// パラメータ文として追加済みと同名の変数は対象外とする。（名前で比較）
			// N->NN の場合の Nは置き換えを行っているため対象外となる。
			//
			for (ParamValIdx = 0; ParamValIdx < ParamValList->Count;
				ParamValIdx++) {
				if (LowerCase(ParamValList->Strings[ParamValIdx]) == LowerCase
					(ValName)) {
					break;
				}
			}
			if (ParamValIdx < ParamValList->Count) {
				continue;
			}
			for (BaseValIdx = 0; BaseValIdx < BaseValList->Count; BaseValIdx++)
			{
				if (LowerCase(BaseValList->Strings[BaseValIdx]) == LowerCase
					(ValName)) {
					break;
				}
			}
			if ((BaseValIdx < BaseValList->Count) && (BaseValIdx != 0)) {
				continue;
			}
			if (TuneGroup == tgDynamic) {
				if (BaseValIdx < BaseValList->Count) {
					continue;
				}
			}
			if ((DefPosS == -1) || (ValData->DefPosS != DefPosS)) { // 違う定義行
				if (s != "") {
					s = SepLongStr(s);
					fprintf(fp, "%s\n", s.c_str());
					s = "";
				}
				//
				// Dimension属性 に対応するための N->NN への置換を定義文字列まで拡張した
				// 2007/1/19
				//
				int j;

				s = "";
				if (ValData->DefPosS != -1) {
					for (j = ValData->DefPosS; j < ValData->DefPosE; j++) {
						Token = (TToken*)TokenList->Items[j];
						ValName = Token->OrgStr;
						if ((Token->TokId == -1) || (Token->TokId == tid_Val)) {
							for (BaseValIdx = 0;
								BaseValIdx < BaseValList->Count;
								BaseValIdx++) {
								if (LowerCase(BaseValList->Strings[BaseValIdx])
									== LowerCase(ValName)) {
									break;
								}
							}
							/* 引数渡しになったので、基本パラメータの名前変更は、不要のはず。
							if(BaseValIdx < BaseValList->Count){
							ValName = BaseValList->Strings[0]+BaseValList->Strings[0]; // Exp. N->NN
							}
							 */
						}
						s += ValName;
					}
				}
				else {
					s = ValData->GetDefStr_Fortran();
				}
				if (s == "") {
					continue;
				}
				DefPosS = ValData->DefPosS;
			}
			else {
				s += ",";
			}
			s += " " + ValData->Str;
			for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
				Token = (TToken*)TokenList->Items[j];
				// Change for N or n 2007/1/19
				ValName = Token->OrgStr;
				if ((Token->TokId == -1) || (Token->TokId == tid_Val)) {
					for (BaseValIdx = 0; BaseValIdx < BaseValList->Count;
						BaseValIdx++) {
						if (LowerCase(BaseValList->Strings[BaseValIdx])
							== LowerCase(ValName)) {
							break;
						}
					}
					/* 引数渡しになｔったので、基本パラメータの名前変更は、不要のはず。
					if(BaseValIdx < BaseValList->Count){
					ValName = BaseValList->Strings[0]+BaseValList->Strings[0]; // Exp. N->NN
					}
					 */
				}
				s += ValName;
			}
		}
		if (s != "") {
			s = SepLongStr(s);
			fprintf(fp, "%s\n", s.c_str());
		}
	}

	fprintf(fp, "\n");
	fprintf(fp, "      include 'OAT.h'\n");
	/** ******************************************* */
	//
	// Kogakuin Irie
	// mpif.hをインクルードさせる処理を追加
	//
	if (MainF->MPIF) {
		fprintf(fp, "      include 'mpif.h'\n");
	}
	//
	// ここまで
	//
	/** ******************************************* */
	fprintf(fp, "\n");

	if (FittingType != 0) {
		fprintf(fp, "%c     !!!!!! fitting用\n", Comment);
		fprintf(fp, "%c     === for estimation using Least Square Method\n",
			Comment);
		fprintf(fp, "%c        ===  for sumipling data\n", Comment);
		fprintf(fp,
			"      real*8  xDim(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_NPARM-1)\n");
		fprintf(fp,
			"      real*8  yEst(0:OATLSM_MAX_N-1, 0:OATLSM_MAX_NPARM-1)\n");
		fprintf(fp, "      real*8  x(0:OATLSM_MAX_N-1)\n");
		fprintf(fp, "      real*8  y(0:OATLSM_MAX_N-1)\n");
		fprintf(fp, "%c        === for target coefficients\n", Comment);
		fprintf(fp, "      real*8  a_lsm(0:OATLSM_MAX_M)\n");
		fprintf(fp,
			"      real*8  aa_lsm(0:OATLSM_MAX_M, 0:OATLSM_MAX_NPARM-1)\n");
		fprintf(fp, "%c     !!!!!! fitting用終り\n", Comment);
	}
	fprintf(fp, "\n");
	fprintf(fp, "      integer iusw1\n");
	// fprintf(fp,"      integer F1(%d)\n",CaseCount);
	fprintf(fp, "      integer F1(%d)\n", CaseArrayCount);
//	if (MainF->SrcCodeType == MainF->sctFortran90) {
	if (MainF->SrcCodeType != MainF->sctC) {
		/** *************************************************************************************** */
		//
		// Kogakuin Irie
		// install以外にも対応できるよう修正
		// 既存コードはコメントアウト
		//
		fprintf(fp, "      integer iloop_%s,iloop_iter,iloop_n\n",
			LowerCase(TuneGroupName).c_str());
		//
		// ここまで
		//
		/** *************************************************************************************** */
		fprintf(fp, "\n");
		if (TuneGroup != tgDynamic) {
			fprintf(fp, "      integer iBestSw1\n");
		}
		fprintf(fp, "\n");
	}
	else {
		if (TuneGroup != tgDynamic) {
			fprintf(fp, "      integer iBestSw1\n");
		}
		fprintf(fp, "\n");
	}
	fprintf(fp, "\n");
	fprintf(fp, "      real*8  t1, t2, t_all, bt\n");
	fprintf(fp, "      real*8  dBestTime1\n");
//	if (MainF->SrcCodeType == MainF->sctFortran90) {
	if (MainF->SrcCodeType != MainF->sctC) {
		if (!MainF->IncludeCodeInContainsF) {
			// Includeの位置でread*4と混在するため必要
			// Contains内に配置する場合は不要。2013/03/22
			/** ************************************************** */
			//
			// Kogakuin Irie
			// エラーの原因となったのでプログラム内でコメントアウト
			//
			fprintf(fp, "!      real*8 OAT_Wtime\n");
			//
			// ここまで
			//
			/** ************************************************** */
		}
		fprintf(fp, "\n");
		fprintf(fp, "      integer ierr\n");
	}
	else {
		fprintf(fp, "      real*8 OAT_Wtime\n");
	}
	fprintf(fp, "\n");
	fprintf(fp, "\n");
	if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		// この定義がないと do While等での型チェックで問題発生していた
		fprintf(fp, "      integer OAT_Eecntl_NextIndex\n");
		fprintf(fp, "      logical OAT_Eecntl_Continue\n");
	}
	if (VisualF) { // Visualize = ON
		// 格納用の ExecState() と読込み用の cbufを確保する。
		fprintf(fp, "      integer inum,iloop_v,LoopCount\n");
		fprintf(fp, "      integer ExecState(%d)\n",
			MainF->TuneRegionList->Count);
		fprintf(fp, "      character*100 cbuf\n");
		fprintf(fp, "      real*8 t_all_sum\n");
	}

	fprintf(fp, "\n");
	fprintf(fp, "%c     ---- file create\n", Comment);
	fprintf(fp, "%c     -----------------------------------------\n", Comment);
//	if (MainF->SrcCodeType == MainF->sctFortran90) {	// 以前の互換で f90のみ oat_myid
	fprintf(fp, "      if (oat_myid .eq. 0) then\n");
//	else {
//		fprintf(fp, "      if (myid .eq. 0) then\n");
//	}
	fprintf(fp, "        open(11, status = 'replace',%s\n", F90Char);
	fprintf(fp, "     &     file = 'OAT_%s%sParam.dat',%s\n",
		TuneGroupName.c_str(), Name.c_str(), F90Char);
	fprintf(fp, "     &     action = 'write', pad= 'yes')\n");
	fprintf(fp, "\n");
	fprintf(fp, "        write (11, *) \"(%s\"\n", Name.c_str());
	if (TuneGroup != tgDynamic) {
		fprintf(fp,
			"        write (11, *) \"  (OAT_NUMPROCS \", OAT_NUMPROCS,\")\"\n");
		fprintf(fp,
			"        write (11, *) \"  (OAT_SAMPDIST \", OAT_SAMPDIST,\")\"\n");
	}

	if (VisualF) { // Visualize = ON
		// 格納用の ExecState() の現在の値を読み込む。
		fprintf(fp, "\n");

		if (TuneGroup != tgDynamic) {
			fprintf(fp,
				"        LoopCount = OAT_ENDTUNESIZE-OAT_STARTTUNESIZE\n");
			fprintf(fp, "        LoopCount = LoopCount+OAT_SAMPDIST\n");
			fprintf(fp, "        LoopCount = LoopCount/OAT_SAMPDIST\n");
		}
		else {
			fprintf(fp, "        LoopCount = 1\n");
		}
		fprintf(fp, "\n");

		fprintf(fp, "        open(12, status = 'old',%s\n", F90Char);
		fprintf(fp, "     &     file = 'OATATLog.dat',%s\n", F90Char);
		fprintf(fp, "     &     action = 'read', pad= 'yes')\n");
		fprintf(fp, "\n");
		fprintf(fp, "        do iloop_v = 1, %d\n",
			MainF->TuneRegionList->Count);
		fprintf(fp, "          read(12, *) cbuf\n");
		fprintf(fp, "          call OATCharToNum(cbuf,inum)\n");
		fprintf(fp, "          ExecState(iloop_v) = inum\n");
		fprintf(fp, "        enddo\n");
		fprintf(fp, "        close(12, status = 'keep')\n");
		fprintf(fp, "\n");

		// 結果Logの書き出し用ファイルをOpen
		fprintf(fp, "        open(13, status = 'replace',%s\n", F90Char);
		fprintf(fp, "     &     file = 'OATATLog_%s.dat',%s\n", Name.c_str(),
			F90Char);
		fprintf(fp, "     &     action = 'write', pad= 'yes')\n");
		fprintf(fp, "        write (13, *) OAT_NUMPROCS , \"NUMPROCS\"\n");
		if (TuneGroup != tgDynamic) {
			fprintf(fp,
				"        write (13, *) OAT_STARTTUNESIZE, \"OAT_STARTTUNESIZE\"\n");
			fprintf(fp,
				"        write (13, *) OAT_ENDTUNESIZE, \"OAT_ENDTUNESIZE\"\n");
			fprintf(fp,
				"        write (13, *) OAT_SAMPDIST, \"OAT_SAMPDIST\"\n");
		}
		else {
			fprintf(fp, "        write (13, *) N, \"OAT_STARTTUNESIZE\"\n");
			fprintf(fp, "        write (13, *) N, \"OAT_ENDTUNESIZE\"\n");
			fprintf(fp, "        write (13, *) 100, \"OAT_SAMPDIST\"\n");
		}
		fprintf(fp, "        write (13, *) -1, \"EndOfHeader\"\n");
		fprintf(fp, "\n");
	}

	fprintf(fp, "      endif\n");
//	if (MainF->SrcCodeType == MainF->sctFortran90) {
	if (true) {
		fprintf(fp, "\n");
		fprintf(fp, "      if (oat_myid .eq. 0) then\n");
		fprintf(fp, "        open(12, status = 'replace', %s\n",F90Char);
		fprintf(fp, "     &  file = 'OAT_%s%sTuneLog.dat', %s\n",
			TuneGroupName.c_str(), Name.c_str(),F90Char);
		fprintf(fp, "     &  action = 'write', pad= 'yes')\n");
		fprintf(fp, "\n");
		fprintf(fp, "      endif\n");
		fprintf(fp, "!     ----------------------------------------\n");
		fprintf(fp, "\n");
#if 1
		fprintf(fp, "      if (OAT_DEBUG .ge. 1)then\n");
		fprintf(fp, "        if (oat_myid .eq. 0) then\n");
		fprintf(fp, "           print *, \"AT region: %s\"\n", Name.c_str());
		fprintf(fp, "        endif\n");
		fprintf(fp, "      endif\n");
#else
		fprintf(fp, "      if (oat_myid .eq. 0) then\n");
		fprintf(fp, "        print *, \"AT region: %s\"\n", Name.c_str());
		fprintf(fp, "      endif\n");
#endif
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "      if (oat_myid .eq. 0) then\n");
//			fprintf(fp, "      if (myid .eq. 0) then\n");
		// fprintf(fp,"     		write (12,\"(A)\") \"AT region: ppohBEMresidual_direct\"\n");
		fprintf(fp, "        write (12,\"(A)\") \"AT region: %s\"\n",
			Name.c_str());
		fprintf(fp, "      endif\n");
		fprintf(fp, "\n");
		fprintf(fp, "\n");
	}
	else {
		fprintf(fp, "%c     ----------------------------------------\n",
			Comment);
	}
	fprintf(fp, "%c     ---- Start tune\n", Comment);
	fprintf(fp, "%c     -----------------------------------------\n", Comment);
	if (FittingType != 0) {
		for (i = 0; i < SampledList->Count; i++) {
			fprintf(fp, "      F1(%d)=%d\n", i + 1,
				(int)(long)SampledList->Items[i]);
		}

		fprintf(fp, "\n");
		fprintf(fp, "%c     !!!!!! fitting用変数\n", Comment);
		fprintf(fp, "%c     !!! variedから推定する最大パラメタ組合せ数\n", Comment);
		fprintf(fp, "      nparm = %d\n", CaseCount);
		fprintf(fp, "\n");
		fprintf(fp, "%c     !!! パラメタの変化数 / sampled 指定子から算出\n", Comment);
		fprintf(fp, "      n_lsm = %d\n", SampledList->Count);
		fprintf(fp, "\n");
		fprintf(fp, "%c     !!! 行列サイズに関するサンプル点の個数\n", Comment);
		fprintf(fp, "      nsamp = 0\n");
		fprintf(fp, "      do iloop_n=OAT_STARTTUNESIZE,%s\n", F90Char);
		fprintf(fp, "     &           OAT_ENDTUNESIZE,%s\n", F90Char);
		fprintf(fp, "     &           OAT_SAMPDIST\n");
		fprintf(fp, "         nsamp = nsamp + 1\n");
		fprintf(fp, "      enddo\n");
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c     !!! fitting least squares で指定した次元数\n", Comment);
		fprintf(fp, "      m_lsm = %d\n", FittingDegree);
		fprintf(fp, "\n");
		fprintf(fp, "%c     !!! サンプリング点インデックス初期化\n", Comment);
		fprintf(fp, "      isamp_indx = 0\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c      print *, \"nparm = \", nparm\n", Comment);
		fprintf(fp, "%c      print *, \"nsamp = \", nsamp\n", Comment);
		fprintf(fp, "%c      print *, \"m_lsm = \", m_lsm\n", Comment);
		fprintf(fp, "\n");
	}
	else {
		for (i = 1; i <= CaseCount; i++) {
			fprintf(fp, "      F1(%d)=%d\n", i, i);
		}
	}
	if (VisualF) { // Visualize = ON
		fprintf(fp, "      t_all_sum = 0\n"); // １秒以上のチェック
	}
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		if (TuneGroup != tgDynamic) {
			fprintf(fp, "      do iloop_n=OAT_STARTTUNESIZE,%s\n", F90Char);
			fprintf(fp, "     &           OAT_ENDTUNESIZE,%s\n", F90Char);
			fprintf(fp, "     &           OAT_SAMPDIST\n");
		}
		else {
			/** ********************************************************************* */
			//
			// Kogakuin Irie
			// BaseValList->String[0]の変数が宣言されていないため変更
			// 既存コードをコメントアウトにして対処
			//
			// if(BaseValList->Count > 0){
			// fprintf(fp,"      iloop_n=%s\n",BaseValList->Strings[0].c_str());
			// }else{
			// fprintf(fp,"      iloop_n=1\n");
			// }
			fprintf(fp, "      iloop_n=1\n");
			//
			// ここまで
			//
			/** ********************************************************************* */
		}
	}
	else {
		if (TuneGroup != tgDynamic) {
			fprintf(fp, "      do iloop_n=OAT_STARTTUNESIZE,%s\n", F90Char);
			fprintf(fp, "     &           OAT_ENDTUNESIZE,%s\n", F90Char);
			fprintf(fp, "     &           OAT_SAMPDIST\n");
		}
		else {
			/** ********************************************************************* */
			//
			// Kogakuin Irie
			// BaseValList->String[0]の変数が宣言されていないため変更
			// 既存コードをコメントアウトにして対処
			//
			// if(BaseValList->Count > 0){
			// fprintf(fp,"      iloop_n=%s\n",BaseValList->Strings[0].c_str());
			// }else{
			// fprintf(fp,"      iloop_n=1\n");
			// }
			fprintf(fp, "      iloop_n=1\n");
			//
			// ここまで
			//
			/** ********************************************************************* */
		}
	}
	fprintf(fp, "\n");
	if (!MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "        do iloop_%s=1, %d\n",
			LowerCase(TuneGroupName).c_str(), CaseArrayCount);
		fprintf(fp, "\n");
		fprintf(fp, "          iusw1 = F1(iloop_%s)\n",
			LowerCase(TuneGroupName).c_str());
		fprintf(fp, "\n");
	}
	else {
		fprintf(fp, "        call OAT_Eecntl_Init(F1,%d)\n", CaseArrayCount);
		fprintf(fp, "        iBestSw1 = 0\n");
		fprintf(fp, "        do while ( .true.)\n");
		fprintf(fp, "          iusw1 = OAT_Eecntl_NextIndex()\n");
		fprintf(fp, "          if (iusw1 < 1 ) exit\n");
		fprintf(fp, "\n");
	}
	//
	// NがParameterの場合に実行時エラーになるので、 iloop_nを引数に入れる形に修正した。
	// 2013/03/15
	// 従来との互換性のため .f の場合のみ有効に戻した。2013/08/11
	// .fに対しても引数で持って来る形になったので、どちらもなしとした。 2015/03/01
	//
#if 0
	if (MainF->SrcCodeType == MainF->sctFortran90) {

	}
	else {
		if (TuneGroup != tgDynamic) {
			for (i = 0; i < BaseValList->Count; i++) {
				ValName = BaseValList->Strings[i];
				fprintf(fp, "          %s = iloop_n\n", ValName.c_str());
			}
		}
		fprintf(fp, "\n");
	}
#endif
	/** ****************************************************************************************************************** */
	//
	// Kogakuin Irie
	// Fortran版d-Spline用追加部分
	// インストール時自動チューニング用追加部分
	//

	if (TuneGroup == tgInstall) {
		if (FittingDspline == 1) {
			fprintf(fp, "          iBestSw1 = dspgiv%s%s(%d",
				TuneGroupName.c_str(), Name.c_str(), CaseArrayCount);

			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				string ArgStr = Script->GetATExecArgStr(false, "", "");
				fprintf(fp, "%s);\n", ArgStr.c_str());
				delete Script;
			}
			else {
				string ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr
					(false, "", "");
				fprintf(fp, "%s);\n", ArgStr.c_str());
			}

			fprintf(fp, "          exit\n");
		}
		// 以下は2次元の場合
		if (FittingDspline == 2) {

			int CaseArrayCount2[2];
			CaseArrayCount2[0] = (int)((variedToValue[0] - variedFromValue[0])
				/ variedStepValue[0] + 1);
			CaseArrayCount2[1] = (int)((variedToValue[1] - variedFromValue[1])
				/ variedStepValue[1] + 1);

			fprintf(fp, "          iBestSw1 = dsp2giv%s%s(%d,%d,%d",
				TuneGroupName.c_str(), Name.c_str(), CaseArrayCount,
				CaseArrayCount2[0], CaseArrayCount2[1]);

			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				string ArgStr = Script->GetATExecArgStr(false, "", "");
				fprintf(fp, "%s);\n", ArgStr.c_str());
				delete Script;
			}
			else {
				string ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr
					(false, "", "");
				fprintf(fp, "%s);\n", ArgStr.c_str());
			}

			fprintf(fp, "          exit\n");
		}
	}
	//
	// ここまで
	//
	/** ****************************************************************************************************************** */

	s = GetPrePostSubregionStr(0);
	if (s != "") {
		fprintf(fp, "%s", s.c_str());
		fprintf(fp, "\n");
	}
	if (MainF->MPIF) {
		fprintf(fp, "          call MPI_BARRIER(MPI_COMM_WORLD, ierr)\n");
	}
	if (MainF->my_timer_start != "") {
		fprintf(fp, "          call %s()\n", MainF->my_timer_start.c_str());
	}
	fprintf(fp, "          t1 = %s()\n", MainF->TimeFunc.c_str());
	fprintf(fp, "\n");

	/** ******************************************************** */
	//
	// Kogakuin Irie
	// 宣言されていない変数（OAT_MAXSAMPITER）が登場
	// 出力プログラム内ではコメントアウトされるよう処理
	//
	// fprintf(fp,"          do iloop_iter=1, OAT_MAXSAMPITER\n");
	fprintf(fp, "!          do iloop_iter=1, OAT_MAXSAMPITER\n");
	//
	// ここまで
	//
	/** ******************************************************** */
	fprintf(fp, "\n");

	if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "          iloop_inner = 0\n");
		fprintf(fp, "          do while (OAT_Eecntl_Continue())\n");
		s = "              call " + FuncName + "(";
	}
	else {
		s = "            call " + FuncName + "(";
	}
	for (i = 0; i < ArgValList->Count; i++) {
		if (ArgValList->Strings[i] == "") {
			continue;
		}
		ValData = (TValData*)ArgValList->Objects[i];

		if (MainF->SrcCodeType == MainF->sctFortran90) {
			if ((BaseValList->Count > 0) && (LowerCase(ValData->Str)
					== LowerCase(BaseValList->Strings[0]))) {
				s += "iloop_n, ";
			}
			else {
				s += ValData->Str + ", ";
			}
		}
		else {
			// s += ValData->Str + ", ";	// 2015/03/01 F90と同じコードになるように修正。
			if ((BaseValList->Count > 0) && (LowerCase(ValData->Str)
					== LowerCase(BaseValList->Strings[0]))) {
				s += "iloop_n, ";
			}
			else {
				s += ValData->Str + ", ";
			}
		}
	}
	s += "iusw1)";
	s = SepLongStr(s);
	fprintf(fp, "%s\n", s.c_str());
	if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "            iloop_inner = iloop_inner + 1\n");
		fprintf(fp, "          end do\n");
	}

	fprintf(fp, "\n");

	/** ***************************************************** */
	//
	// Kogakuin Irie
	// 上の追加処理と関係
	// do文をコメントアウトしているため，end doもコメントアウト
	//
	// fprintf(fp,"          end do\n");
	fprintf(fp, "!          end do\n");
	//
	// ここまで
	//
	/** ***************************************************** */

	fprintf(fp, "\n");
	if (MainF->MPIF) {
		fprintf(fp, "          call MPI_BARRIER(MPI_COMM_WORLD, ierr)\n");
	}
	if (MainF->my_timer_stop != "") {
		fprintf(fp, "          call %s()\n", MainF->my_timer_stop.c_str());
	}
	// fprintf(fp,"          t2 = MPI_WTIME()\n");
	fprintf(fp, "          t2 = %s()\n", MainF->TimeFunc.c_str());

	fprintf(fp, "          t_all = t2 - t1\n");
	if (MainF->MPIF) {
		fprintf(fp,
			"          call MPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION,%s\n"
			, F90Char);
		fprintf(fp, "     &           MPI_MAX, MPI_COMM_WORLD, ierr)\n");
	}
	else {
		fprintf(fp, "          bt = t_all\n");
	}
	if (!MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "          t_all = bt\n");
	}
	else {
		fprintf(fp, "          t_all = bt / iloop_inner\n");
		fprintf(fp, "          call OAT_Eecntl_Repperf(iusw1, t_all)\n");
	}
	fprintf(fp, "\n");

	s = GetPrePostSubregionStr(1);
	if (s != "") {
		fprintf(fp, "%s", s.c_str());
		fprintf(fp, "\n");
	}

	if (VisualF) { // Visualize = ON
		// 0から100(%)までの進行状態の値をセットする。
		if (MainF->SrcCodeType == MainF->sctFortran90) {
			fprintf(fp, "          if (oat_myid .eq. 0) then\n");
		}
		else {
			fprintf(fp, "          if (myid .eq. 0) then\n");
		}
		fprintf(fp, "            t_all_sum = t_all_sum + t_all\n"); // １秒以上のチェック
		fprintf(fp, "            if(t_all_sum .ge. 1.0) then\n"); // １秒以上のチェック
		fprintf(fp, "              t_all_sum = 0\n"); // １秒以上のチェック
		fprintf(fp,
			"              inum = (iloop_n-OAT_STARTTUNESIZE)/OAT_SAMPDIST\n");

		fprintf(fp, "              ExecState(%d) = (100*inum+%s\n",
			MainF->TuneRegionList->IndexOf(this) + 1, F90Char);
		fprintf(fp,
			"     &                    100*(iloop_%s-1)/%d)/LoopCount\n", LowerCase(TuneGroupName).c_str(), CaseCount);

		// 格納用の ExecState() の現在の値を書き込む。
		fprintf(fp, "              open(12, status = 'replace',%s\n", F90Char);
		fprintf(fp, "     &           file = 'OATATLog.dat',%s\n", F90Char);
		fprintf(fp, "     &           action = 'write', pad= 'yes')\n");
		fprintf(fp, "\n");
		fprintf(fp, "              do iloop_v = 1, %d\n",
			MainF->TuneRegionList->Count);
		fprintf(fp, "                write (12, *) ExecState(iloop_v)\n");
		fprintf(fp, "              enddo\n");
		fprintf(fp, "              close(12, status = 'keep')\n");

		fprintf(fp, "            endif\n"); // １秒以上のチェック
		fprintf(fp, "          endif\n");
	}

	//
	// According 指定がある場合は、計測時間を置き換える。
	// 条件指定の場合（estimatedなし）には、条件が一致すれば time = 0
	// 条件が一一致しない場合は、かかった時間（0より大きい値)
	// Accordingと varidの２重の場合には、 範囲で選択となる。
	// 時間変数があると便利？ 例例えば、 OAT_TIME  ？？？
	//
	if (UseAccordingF) {
		string aStr;
		TSubRegion *SubRegion;
		int cp1, cp2;
		int CaseIdx = 1;

		if (SubRegionList->Count != 0) {
			fprintf(fp, "          select case(iusw1)\n");
			for (i = 0; i < SubRegionList->Count; i++) {
				SubRegion = (TSubRegion*)SubRegionList->Items[i];
				aStr = SubRegion->AccordingStr;
				if (aStr == "") {
					continue;
				}
				if (SubRegion->CaseCount <= 1) {
					fprintf(fp, "            case(%d)\n", CaseIdx++);
				}
				else {
					fprintf(fp, "            case(%d:%d)\n", CaseIdx,
						CaseIdx + SubRegion->CaseCount - 1);
					CaseIdx += SubRegion->CaseCount;
				}
				cp1 = aStr.find("according") + strlen("according");
				cp2 = aStr.find("estimated");
				if (cp2 != 0) {
					cp1 = cp2 + strlen("estimated");
					aStr = Trim(aStr.substr(cp1, aStr.length()));
					fprintf(fp, "              t_all = dble(%s)\n",
						aStr.c_str());
				}
				else { // 条件式
					aStr = Trim(aStr.substr(cp1, aStr.length()));
					fprintf(fp, "              if (%s) then\n", aStr.c_str());
					fprintf(fp, "                t_all = 0\n");
					fprintf(fp, "              endif\n");
				}
			}
			fprintf(fp, "          end select\n");
		}
		else {
			aStr = AccordingStr;
			cp1 = aStr.find("according") + strlen("according");
			cp2 = aStr.find("estimated");
			if (cp2 != 0) {
				cp1 = cp2 + strlen("estimated");
				aStr = Trim(aStr.substr(cp1, aStr.length()));
				fprintf(fp, "            t_all = dble(%s)\n", aStr.c_str());
			}
			else { // 条件式
				aStr = Trim(aStr.substr(cp1, aStr.length()));
				fprintf(fp, "            if (%s) then\n", aStr.c_str());
				fprintf(fp, "              t_all = 0\n");
				fprintf(fp, "            endif\n");
			}
		}
		fprintf(fp, "\n");
	}

	if (VisualF) { // Visualize = ON
		//
		// ビジュアル化のための結果出力
		// Sampledでとびとびの場合は、空白行を入れる。
		//
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "          if (oat_myid .eq. 0) then\n");
//			fprintf(fp, "          if (myid .eq. 0) then\n");
		fprintf(fp, "            if (iloop .eq. 1) then\n");
		fprintf(fp, "              do iloop_vi = 1, F1(iloop_%s)-1\n",
			LowerCase(TuneGroupName).c_str());
		fprintf(fp, "                write (13, *)\n");
		fprintf(fp, "              enddo\n");
		fprintf(fp, "            endif\n");
		fprintf(fp, "            write (13, *) t_all\n");
		fprintf(fp, "            if (iloop .lt. %d) then\n", CaseArrayCount);
		fprintf(fp,
			"              do iloop_vi = F1(iloop_%s)+1, F1(iloop_%s+1)-1\n",
			LowerCase(TuneGroupName).c_str(), LowerCase(TuneGroupName).c_str());
		fprintf(fp, "                write (13, *)\n");
		fprintf(fp, "              enddo\n");
		fprintf(fp, "            else\n");
		fprintf(fp, "              do iloop_vi = F1(iloop_%s)+1,%d\n",
			LowerCase(TuneGroupName).c_str(), CaseCount);
		fprintf(fp, "                write (13, *)\n");
		fprintf(fp, "              enddo\n");
		fprintf(fp, "            endif\n");
		fprintf(fp, "          endif\n");
	}
	if (MainF->DebugF) {
		// fprintf(fp,"          if (OAT_DEBUG .ge. 2)then\n");
		fprintf(fp, "          if (OAT_DEBUG .ge. 1)then\n");
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "            if (oat_myid .eq. 0) then\n");
//			fprintf(fp, "            if (myid .eq. 0) then\n");
		fprintf(fp,
			"              print *, \"N=\",iloop_n, \"iusw1=\", iusw1, t_all\n"
			);
		fprintf(fp, "            endif\n");
		fprintf(fp, "          endif\n");
	}
	else {
		// fprintf(fp,"%c          if (OAT_DEBUG .ge. 2)then\n",Comment);
		fprintf(fp, "%c          if (OAT_DEBUG .ge. 1)then\n", Comment);
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "%c            if (oat_myid .eq. 0) then\n", Comment);
//			fprintf(fp, "%c            if (myid .eq. 0) then\n", Comment);
		fprintf(fp,
			"%c              print *, \"N=\",iloop_n, \"iusw1=\", iusw1, t_all\n"
			, Comment);
		fprintf(fp, "%c            endif\n", Comment);
		fprintf(fp, "%c          endif\n", Comment);
	}
	fprintf(fp, "\n");
//	if (MainF->SrcCodeType == MainF->sctFortran90) {
	fprintf(fp, "      if (oat_myid .eq. 0) then\n");
	fprintf(fp,
		"         write(12, \"(A,I6,A,I6,A,F9.4,A)\") \"N=\",iloop_n, \" / iusw1=\", iusw1, \" : \",t_all, \" [sec.]\"\n");
	fprintf(fp, "      endif\n");
	fprintf(fp, "\n");
//	}

	fprintf(fp, "\n");
	if (!MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "          if (iloop_%s .eq. 1) then\n",
			LowerCase(TuneGroupName).c_str());
	}
	else {
		fprintf(fp, "          if (iBestSw1 == 0) then\n");
	}
	fprintf(fp, "            dBestTime1 = t_all\n");
	if (!MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "            iBestSw1 = F1(1)\n");
	}
	else {
		fprintf(fp, "            iBestSw1 = iusw1\n");
	}
	fprintf(fp, "          else\n");
	fprintf(fp, "            if (t_all .lt. dBestTime1) then\n");
	fprintf(fp, "              dBestTime1 = t_all\n");
	if (!MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "              iBestSw1 = F1(iloop_%s)\n",
			LowerCase(TuneGroupName).c_str());
	}
	else {
		fprintf(fp, "              iBestSw1 = iusw1\n");

	}
	fprintf(fp, "            endif\n");
	fprintf(fp, "          endif\n");
	fprintf(fp, "\n");
	if (FittingType != 0) {

		fprintf(fp, "%c         !!!!!! fitting用変数設定\n", Comment);
		fprintf(fp, "          x(iloop_install-1) = dble(F1(iloop_install))\n");
		fprintf(fp, "          y(iloop_install-1) = t_all\n");
		fprintf(fp, "\n");
		fprintf(fp,
			"%c          print *, iloop_install-1,x(iloop_install-1),y(iloop_install-1)\n", Comment);
		fprintf(fp, "\n");
	}
	fprintf(fp, "        enddo\n");
	if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "        call OAT_Eecntl_Fin\n");
	}
	fprintf(fp, "\n");

	if (MainF->DebugF) {
		fprintf(fp, "        if (OAT_DEBUG .ge. 1)then\n");
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "          if (oat_myid .eq. 0) then\n");
//			fprintf(fp, "          if (myid .eq. 0) then\n");
		fprintf(fp,
			"             print *, \"N=\",iloop_n, \"BestSw=\",iBestSw1\n");
		fprintf(fp, "          endif\n");
		fprintf(fp, "        endif\n");
	}
	if (FittingType != 0) {
		fprintf(fp, "%c       ------ fitting処理\n", Comment);
		fprintf(fp, "\n");
		fprintf(fp,
			"%c       !!!! 以下のF1(%d) と %d は、sampledの個数と、variedの個数から判断\n", Comment, CaseArrayCount, CaseCount);
		fprintf(fp, "%c           -> sampledの点がvariedの全領域を調べているか判断し、\n",
			Comment);
		fprintf(fp, "%c              全数判断していれば、最適値を選択\n", Comment);
		fprintf(fp, "        if (F1(%d) .eq. %d) then\n", CaseArrayCount,
			CaseCount);
		fprintf(fp, "%c         !!! sampled 指定子なし\n", Comment);
		fprintf(fp,
			"%c         === if all parameters are mesured or communication optimization\n", Comment);
		fprintf(fp,
			"%c                then this selects the mesured parameter.\n",
			Comment);
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "           if (oat_myid .eq. 0) then\n");
//			fprintf(fp, "           if (myid .eq. 0) then\n");
		fprintf(fp,
			"              print *, \"All parameters are mesured. \"\n");
		fprintf(fp,
			"              print *, \"So, I will select the measured pararameter.\"\n");
		fprintf(fp, "           endif\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n", Comment);
		fprintf(fp, "%c         ! 変更点                         !\n", Comment);
		fprintf(fp, "%c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n", Comment);
		fprintf(fp, "%c         === using LSM\n", Comment);
		fprintf(fp,
			"%c         === Parameter Estimated routine for fixing dimansion\n"
			, Comment);
		fprintf(fp,
			"%c                 and add estimated costs for all range of the parameter\n", Comment);
		fprintf(fp,
			"          call OATLSM_Est_ParamFxDim(x(0), y(0), n_lsm, m_lsm,%s\n"
			, F90Char);
		fprintf(fp,
			"     &           a_lsm(0),  iloop_n, nparm, isamp_indx,%s\n",
			F90Char);
		fprintf(fp, "     &           xDim(0,0), yEst(0,0), idummy)\n");
		fprintf(fp, "          isamp_indx = isamp_indx + 1\n");
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "          if (oat_myid .eq. 0) then\n");
//			fprintf(fp, "          if (myid .eq. 0) then\n");
		fprintf(fp, "             print *, \"Best Parameter: \", iBestSW1\n");
		fprintf(fp, "          endif\n");
		fprintf(fp, "%c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n", Comment);
		fprintf(fp, "%c         ! 変更点の終り                   !\n", Comment);
		fprintf(fp, "%c         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n", Comment);
		fprintf(fp, "\n");
		fprintf(fp, "        else\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c         !!! sampled 指定子あり\n", Comment);
		fprintf(fp, "%c         === using LSM\n", Comment);
		fprintf(fp,
			"%c         === Parameter Estimated routine for fixing dimansion\n"
			, Comment);
		fprintf(fp,
			"%c                 and add estimated costs for all range of the parameter\n", Comment);
		fprintf(fp,
			"          call OATLSM_Est_ParamFxDim(x(0), y(0), n_lsm, m_lsm,%s\n"
			, F90Char);
		fprintf(fp,
			"     &           a_lsm(0),  iloop_n, nparm, isamp_indx,%s\n",
			F90Char);
		fprintf(fp, "     &           xDim(0,0), yEst(0,0), iBestSW1)\n");
		fprintf(fp, "          isamp_indx = isamp_indx + 1\n");
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "          if (oat_myid .eq. 0) then\n");
//			fprintf(fp, "          if (myid .eq. 0) then\n");
		fprintf(fp,
			"             print *, \"Estimated Best Parameter: \", iBestSW1\n");
		fprintf(fp, "          endif\n");
		fprintf(fp, "        endif\n");
		fprintf(fp, "%c       -----------------------------------------\n",
			Comment);
		fprintf(fp, "\n");
	}

//	if (MainF->SrcCodeType == MainF->sctFortran90) {
	fprintf(fp, "      if (oat_myid .eq. 0) then\n");
	fprintf(fp,
		"        write(12, \"(A,I6,A,I6)\") \"N=\",iloop_n, \" BestSw=\",iBestSW1\n");
	fprintf(fp, "      endif\n");
	fprintf(fp, "\n");
	fprintf(fp, "\n");
//	}

	fprintf(fp, "%c       --- file write\n", Comment);
//	if (MainF->SrcCodeType == MainF->sctFortran90) {
	fprintf(fp, "        if (oat_myid .eq. 0) then\n");
//		fprintf(fp, "        if (myid .eq. 0) then\n");
	fprintf(fp,
		"          write (11, *) \"  (OAT_PROBSIZE \", iloop_n, \" \"\n");
	fprintf(fp, "          write (11, *) \"     (%s_I \", iBestSw1,\")\"\n",
		Name.c_str());
	fprintf(fp, "          write (11, *) \"  )\"\n");
	fprintf(fp, "        endif\n");
	fprintf(fp, "%c       -----------------------------------------\n",
		Comment);
	if (TuneGroup != tgDynamic) {
		fprintf(fp, "      enddo\n");
	}
	fprintf(fp, "\n");
	fprintf(fp, "%c     --- file close\n", Comment);
//	if (MainF->SrcCodeType == MainF->sctFortran90) {
	fprintf(fp, "      if (oat_myid .eq. 0) then\n");
//		fprintf(fp, "      if (myid .eq. 0) then\n");
	fprintf(fp, "        write (11, *) \")\"\n");
	fprintf(fp, "        close(11, status = 'keep')\n");

	if (VisualF) { // Visualize = ON
		// 0から100(%)までの進行状態の値をセットする。
		fprintf(fp, "              ExecState(%d) = 100\n",
			MainF->TuneRegionList->IndexOf(this) + 1);
		// 格納用の ExecState() の現在の値を書き込む。
		fprintf(fp, "              open(12, status = 'replace',%s\n", F90Char);
		fprintf(fp, "     &           file = 'OATATLog.dat',%s\n", F90Char);
		fprintf(fp, "     &           action = 'write', pad= 'yes')\n");
		fprintf(fp, "\n");
		fprintf(fp, "              do iloop_v = 1, %d\n",
			MainF->TuneRegionList->Count);
		fprintf(fp, "                write (12, *) ExecState(iloop_v)\n");
		fprintf(fp, "              enddo\n");
		fprintf(fp, "              close(12, status = 'keep')\n");

		//
		// 結果をClose
		//
		fprintf(fp, "              close(13, status = 'keep')\n");
	}

	fprintf(fp, "      endif\n");
	fprintf(fp, "\n");
//	if (MainF->SrcCodeType == MainF->sctFortran90) {
	fprintf(fp, "      if (oat_myid .eq. 0) then\n");
	fprintf(fp, "        close(12, status = 'keep')\n");
	fprintf(fp, "      endif\n");
//	}
	fprintf(fp, "%c     ---------------------------------------------\n",
		Comment);
	fprintf(fp, "\n");

	if (FittingType != 0) {

		fprintf(fp, "%c     !!!!!!!! fitting処理\n", Comment);
		fprintf(fp, "%c     === for LSM to estimate variable dimension\n",
			Comment);
		fprintf(fp, "%c       === Input xDim, yEst\n", Comment);
		fprintf(fp, "\n");
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "%c      if (oat_myid. eq. 0) then\n", Comment);
//			fprintf(fp, "%c      if (myid. eq. 0) then\n", Comment);
		fprintf(fp,
			"%c        print *, \"nsamp=\",nsamp, \"/ nparm=\",nparm\n",
			Comment);
		fprintf(fp, "%c        do i=0, nsamp-1\n", Comment);
		fprintf(fp,
			"%c          write(6, 1919) (xDim(i,isw),isw=0, nparm-1)\n",
			Comment);
		fprintf(fp, "%c        enddo\n", Comment);
		fprintf(fp, "%c        print *, " "\n", Comment);
		fprintf(fp, "%c        do i=0, nsamp-1\n", Comment);
		fprintf(fp,
			"%c          write(6, 1919) (yEst(i,isw),isw=0, nparm-1)\n",
			Comment);
		fprintf(fp, "%c        enddo\n", Comment);
		fprintf(fp, "%c 1919   format(' ', 20F10.5)\n", Comment);
		fprintf(fp, "%c      endif\n", Comment);
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c       === 全組合せ数がデータの総数となる。\n", Comment);
		fprintf(fp, "        n_lsm = nparm\n");
		fprintf(fp, "\n");
		fprintf(fp, "        call OATLSM_lsm_DimEst(xDim(0,0), yEst(0,0),%s\n",
			F90Char);
		fprintf(fp,
			"     &               n_lsm, m_lsm, aa_lsm(0,0), nparm, nsamp)\n");
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "      do isw=1, nparm\n");
		fprintf(fp, "        do iii=0, nsamp-1\n");
		fprintf(fp, "           xDim(iii, isw-1)=0.0d0\n");
		fprintf(fp, "        enddo\n");
		fprintf(fp, "      enddo\n");
		fprintf(fp, "\n");
		fprintf(fp, "      do isw=1, nparm\n");
		fprintf(fp, "        do iii=0, nsamp-1\n");
		fprintf(fp, "          yEst(iii, isw-1)=0.0d0\n");
		fprintf(fp, "        enddo\n");
		fprintf(fp, "      enddo\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c     === Output is aa_lsm.\n", Comment);
		fprintf(fp, "%c       ===  Output\n", Comment);
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "      if (oat_myid .eq. 0) then\n");
//			fprintf(fp, "      if (myid .eq. 0) then\n");

		// fprintf(fp,"         open(10, status = 'replace', file = 'MyMM_I_LSM.dat',\n");
		fprintf(fp,
			"         open(10, status = 'replace', file = '%s_%c_LSM.dat',%s\n"
			, Name.c_str(), TuneGroupName[0], F90Char);
		fprintf(fp, "     &       action = 'write', pad= 'yes')\n");
		fprintf(fp, "      endif\n");
		fprintf(fp, "      do isw=1, nparm\n");
		fprintf(fp, "         write (10, 1001) dble(isw),%s\n", F90Char);
		fprintf(fp, "     &       (aa_lsm(iii, isw-1), iii=0, m_lsm)\n");
		fprintf(fp, " 1001    format(D20.10, 20D20.10)\n");
		fprintf(fp, "      enddo\n");
		fprintf(fp, "      close(10, status = 'keep')\n");
		fprintf(fp, "\n");
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "      if (oat_myid .eq. 0) then\n");
//			fprintf(fp, "      if (myid .eq. 0) then\n");
		fprintf(fp,
			"        print *, \"Output Parameters ==============================\"\n");
		fprintf(fp, "        do isw=1, nparm\n");
		fprintf(fp, "          print *, \"Parameter No.: \", isw\n");
		fprintf(fp,
			"          print *, \"Sample Points: \", n_lsm, \" / Formula Order: \", m_lsm\n");
		fprintf(fp, "          print *, \"Calculated Coefficients: \"\n");
		fprintf(fp, "          do iii=0, m_lsm\n");
		fprintf(fp,
			"            print *, aa_lsm(iii, isw-1), \" * x^\", m_lsm-iii\n");
		fprintf(fp, "          enddo\n");
		fprintf(fp, "        enddo\n");
		fprintf(fp, "%c       === End of Output\n", Comment);
		fprintf(fp, "      endif\n");
		fprintf(fp, "%c     !!!!!!!! fitting処理の終り\n", Comment);
		fprintf(fp,
			"%c     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
			, Comment);
		fprintf(fp, "\n");

	}

	fprintf(fp, "      return\n");
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		s = "OAT_ATexec" + TuneGroupName + Name;
		fprintf(fp, "      end subroutine %s\n", s.c_str());
	}
	else {
		fprintf(fp, "      end\n"); // Change 2013/06/20
	}

	fprintf(fp, "%c     ==== End of %s Optimization Routines\n", Comment,
		TuneGroupName.c_str());
	fprintf(fp,
		"%c     ==============================================================\n"
		, Comment);
	fprintf(fp, "\n");
	fprintf(fp, "\n");
	/** **************************************************************************************************** */
	//
	// Kogakuin Irie
	// Fortran90対応のd-Spline用追加部分---------------------------------------------------------------------
	//
	// 1次元用d-Spline
	if (FittingDspline == 1) {
		// 1次元用d-Spline（インストール時自動チューニング用）
		if (TuneGroup == tgInstall) {
			int CaseNums = (int)((variedToValue[0] - variedFromValue[0])
				/ variedStepValue[0] + 1);

			// 関数名部分
			fprintf(fp, "function dspgiv%s%s( npN", TuneGroupName.c_str(),
				Name.c_str());

			// ここから流用　変数名出力のため------

			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				ArgStr = Script->GetATExecArgStr(false, "", "");
				fprintf(fp, "%s", ArgStr.c_str());
				delete Script;
			}
			else {
				ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "",
					"");
				fprintf(fp, "%s", ArgStr.c_str());
			}

			// ここまで流用　変数名出力用------
			fprintf(fp, ")\n");

			// use文の宣言
			fprintf(fp, "      use omp_lib\n");

			// implicit noneの宣言
			fprintf(fp, "      implicit none\n");

			// 仮引数宣言部分
			fprintf(fp, "      integer npN\n");

			// ここから流用　変数名出力のため------

			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				ArgStr = Script->GetATExecArgStr(true, "", "");
				fprintf(fp, "%s", ArgStr.c_str());
				delete Script;
			}
			else {
				ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true, "",
					"");
				fprintf(fp, "%s", ArgStr.c_str());
			}

			// ここまで流用　変数名出力用------

			fprintf(fp, "      integer :: dspgiv%s%s\n", TuneGroupName.c_str(),
				Name.c_str());
			fprintf(fp, "      integer :: DEBUG = 1\n");
			fprintf(fp, "      integer nn\n");
			fprintf(fp, "      double precision DE(%d+1,%d+1)\n", CaseNums * 3,
				CaseNums * 3);
			fprintf(fp, "      double precision R(%d+1,%d+1)\n", CaseNums * 3,
				CaseNums * 3);
			fprintf(fp, "      double precision G(%d+1,%d+1)\n", CaseNums * 3,
				CaseNums * 3);
			fprintf(fp, "      double precision radius, cosine, sine\n");
			fprintf(fp, "      integer p, q, q2\n");
			fprintf(fp, "      double precision time\n");
			fprintf(fp, "      integer count\n");
			fprintf(fp,
				"      double precision, allocatable, dimension(:) :: x\n");
			fprintf(fp, "      double precision temp\n");
			fprintf(fp, "      integer temp2, bestP, nextP, prebestP\n");
			fprintf(fp, "      double precision select2(npN,2)\n");
			fprintf(fp, "      integer h(npN)\n");
			fprintf(fp, "      integer dspline_isw(npN)\n");
			fprintf(fp, "      integer kk\n");
			fprintf(fp, "      double precision, parameter :: alfa = 0.1\n");
			fprintf(fp, "      double precision t1, t2\n");
			fprintf(fp, "      integer F2(4)\n");
			fprintf(fp, "      integer i, j, k, l, i4\n\n");

			fprintf(fp, "      nn = npN * 3\n");
			fprintf(fp, "      count = 1\n");
			fprintf(fp, "      bestP = 0\n");
			fprintf(fp, "      kk = 1\n\n");

			fprintf(fp, "      allocate(x(nn))\n\n");

			// エクセル書き込み用部分
			fprintf(fp, "      open( 11, file = 'd-SplineData.csv' )\n\n");

			// DE初期化部分
			fprintf(fp,
				"      do i = 1, nn + 1\n         do j = 1, nn + 1\n            DE(i,j) = 0\n            R(i,j) = 0\n         end do\n      end do\n\n");

			// DE1,-2,1セット部分
			fprintf(fp,
				"      do i = 1, nn - 2\n         DE(i,i) = 1 * alfa\n         DE(i,i+1) = -2 * alfa\n         DE(i,i+2) = 1 * alfa\n      end do\n\n");

			// h初期化部分
			fprintf(fp,
				"      do i = 1, npN\n         h(i) = npN + 1\n      end do\n\n"
				);

			// 初期4点決定部分
			fprintf(fp,
				"      F2(1) = 1\n      F2(2) = ( 2 + npN ) / 3\n      F2(3) = ( 1 + 2 * npN ) / 3\n      F2(4) = npN\n\n      p = nn + 1\n\n");

			// パラメータの取りうる値が,d-Spline関数中ではどの点に対応するかを設定
			fprintf(fp,
				"      do i = 1, npN\n         dspline_isw(i) = 3 * i - 2\n      end do\n\n");

			// 初期4点の計算用部分-------------------------------------------------------
			fprintf(fp,
				"      do i4 = 1, 4\n         q = F2(i4)\n         q2 = dspline_isw(F2(i4))\n\n         t1 = omp_get_wtime()\n");

			// 計算する関数に飛ぶ部分	基本的に既存部分の流用
			if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
				fprintf(fp, "         iloop_inner = 0;\n");
				fprintf(fp, "         while(OAT_Eecntl_Continue()){\n");
				s = "            call " + FuncName + "(";
			}
			else {
				s = "         call " + FuncName + "(";
			}
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str + ", ";
			}
			s += "q)";
			s = SepLongStr(s);
			fprintf(fp, "%s\n", s.c_str());

			fprintf(fp,
				"         t2 = omp_get_wtime()\n\n         time = t2 - t1\n\n");
			fprintf(fp,
				"         print '( /, \"---para is \", I0, \", time is \", G14.7, \"---\", / )', q, time\n\n");
			fprintf(fp,
				"         h(kk) = q\n\n         DE(p,q2) = 1\n         DE(p,nn+1) = time\n\n");

			// ------初期4点ギブンス変換do文部分ここから------
			fprintf(fp,
				"         do q2 = q2, nn\n\n            do i = 1, nn + 1\n               G(i,i) = 0\n            end do\n\n");
			fprintf(fp, "            G(nn+1,nn+1) = 1\n");

			// G用計算部分
			fprintf(fp,
				"            radius = dsqrt( ( DE(q2,q2) ) * ( DE(q2,q2) ) + ( DE(p,q2) ) * ( DE(p,q2) ) )\n\n");
			fprintf(fp,
				"            if ( radius .eq. 0 ) then\n               exit\n            endif\n\n");
			fprintf(fp,
				"            cosine = DE(q2,q2) / radius\n            sine = DE(p,q2) / radius\n\n            G(p,p) = cosine\n            G(q2,q2) = cosine\n\n            G(q2,p) = sine\n            G(p,q2) = -1 * sine\n\n\n");

			// R=G*DEギブンス変換部分
			fprintf(fp,
				"            R(q2,q2) = R(q2,q2) + G(q2,q2) * DE(q2,q2)\n            R(q2,q2) = R(q2,q2) + G(q2,nn+1) * DE(nn+1,q2)\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               R(q2,q2+1) = R(q2,q2+1) + G(q2,q2) * DE(q2,q2+1)\n               R(q2,q2+1) = R(q2,q2+1) + G(q2,nn+1) * DE(nn+1,q2+1)\n            endif\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               R(q2,q2+2) = R(q2,q2+2) + G(q2,q2) * DE(q2,q2+2)\n               R(q2,q2+2) = R(q2,q2+2) + G(q2,nn+1) * DE(nn+1,q2+2)\n            endif\n\n");
			fprintf(fp,
				"            R(q2,nn+1) = R(q2,nn+1) + G(q2,q2) * DE(q2,nn+1)\n            R(q2,nn+1) = R(q2,nn+1) + G(q2,nn+1) * DE(nn+1,nn+1)\n\n\n");

			fprintf(fp,
				"            R(nn+1,q2) = R(nn+1,q2) + G(nn+1,q2) * DE(q2,q2)\n            R(nn+1,q2) = R(nn+1,q2) + G(nn+1,nn+1) * DE(nn+1,q2)\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               R(nn+1,q2+1) = R(nn+1,q2+1) + G(nn+1,q2) * DE(q2,q2+1)\n               R(nn+1,q2+1) = R(nn+1,q2+1) + G(nn+1,nn+1) * DE(nn+1,q2+1)\n            endif\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               R(nn+1,q2+2) = R(nn+1,q2+2) + G(nn+1,q2) * DE(q2,q2+2)\n               R(nn+1,q2+2) = R(nn+1,q2+2) + G(nn+1,nn+1) * DE(nn+1,q2+2)\n            endif\n\n");
			fprintf(fp,
				"            R(nn+1,nn+1) = R(nn+1,nn+1) + G(nn+1,q2) * DE(q2,nn+1)\n            R(nn+1,nn+1) = R(nn+1,nn+1) + G(nn+1,nn+1) * DE(nn+1,nn+1)\n\n\n");

			fprintf(fp,
				"            DE(q2,q2) = R(q2,q2)\n\n            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               DE(q2,q2+1) = R(q2,q2+1)\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               DE(q2,q2+2) = R(q2,q2+2)\n            endif\n\n            DE(q2,nn+1) = R(q2,nn+1)\n\n\n");
			fprintf(fp,
				"            DE(nn+1,q2) = R(nn+1,q2)\n\n            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               DE(nn+1,q2+1) = R(nn+1,q2+1)\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               DE(nn+1,q2+2) = R(nn+1,q2+2)\n            endif\n\n            DE(nn+1,nn+1) = R(nn+1,nn+1)\n\n\n");
			fprintf(fp,
				"            R(q2,q2) = 0\n\n            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               R(q2,q2+1) = 0\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               R(q2,q2+2) = 0\n            endif\n\n            R(q2,nn+1) = 0\n\n\n");
			fprintf(fp,
				"            R(nn+1,q2) = 0\n\n            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               R(nn+1,q2+1) = 0\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               R(nn+1,q2+2) = 0\n            endif\n\n            R(nn+1,nn+1) = 0\n\n");

			fprintf(fp,
				"         end do\n\n         kk = kk + 1\n      end do\n\n");
			// ------初期4点ギブンス変換do文部分ここまで------

			// ------初期4点計算後，ベストパラメタと次のパラメタ決定ここから------

			// 推定値代入初期化部分
			fprintf(fp,
				"      do i = 1, nn\n         x(i) = DE(i,nn+1)\n      end do\n\n");

			// 後退代入で推定値計算部分
			fprintf(fp,
				"      do i = nn, 1, -1\n         do j = i + 1, nn\n            x(i) = x(i) - DE(i,j) * x(j)\n         end do\n\n         x(i) = x(i) / DE(i,i)\n      end do\n\n");

			// デバッグ用，推定値表示部分
			fprintf(fp,
				"      if ( DEBUG .gt. 0 ) then\n         print '( /, \"estimation\" )'\n\n         do i = 1, nn\n            print *, '[', i, ']', x(i)\n         end do\n\n         print *, ''\n      endif\n\n");

			// 推定値が最小となる番号をベストパラメタに設定する部分
			fprintf(fp,
				"      temp = x(1)\n      bestP = 1\n\n      do i = 1, npN\n         if ( x(dspline_isw(i)) .lt. temp ) then\n            temp = x(dspline_isw(i))\n            bestP = i\n         endif\n      end do\n\n");
			fprintf(fp,
				"      print *, 'best para = ', bestP, ' time = ', x(dspline_isw(bestP))\n\n");

			// 選択基準2用配列初期化部分
			fprintf(fp,
				"      do i = 1, npN\n         select2(i,2) = i\n         select2(i,1) = 0\n      end do\n\n");

			// 変化率計算部分
			fprintf(fp,
				"      do i = 2, npN - 1\n         select2(i,1) = dabs( x(dspline_isw(i-1)) - 2 * x(dspline_isw(i)) + x(dspline_isw(i+1)) )\n      end do\n\n");

			// select2ソート部分
			fprintf(fp,
				"      do i = 1, npN - 1\n         do j = npN, i + 1, -1\n            if ( select2(j-1,1) .lt. select2(j,1) ) then\n               temp = select2(j,1)\n               temp2 = select2(j,2)\n               select2(j,1) = select2(j-1,1)\n               select2(j,2) = select2(j-1,2)\n               select2(j-1,1) = temp\n               select2(j-1,2) = temp2\n            endif\n         end do\n      end do\n\n");

			// 推定したベストPがすでに標本点にあるなら選択基準2,ないなら選択基準1　選択する部分
			fprintf(fp,
				"      do i = 1, npN\n         if ( bestP .eq. h(i) ) then\n            do j = 1, npN\n               do k = 1, npN\n                  if ( select2(j,2) .eq. h(k) ) then\n                     exit\n                  endif\n                  if ( k + 1 .eq. npN + 1 ) then\n                     nextP = select2(j,2)\n                     goto 111\n                  endif\n               end do\n            end do\n\n            exit\n         else\n            nextP = bestP\n         endif\n      end do\n\n111   print '( \" nextP =\", I0, \" bestP = \", I0, / )', nextP, bestP\n\n");

			// prebestPに4点計算終了時のベストパラメタを代入する部分
			fprintf(fp, "      prebestP = bestP\n\n");

			// 推定値をエクセルに書き込みする部分
			fprintf(fp,
				"      do i = 1, nn\n         write( 11, * ) x(i)\n      end do\n      write( 11, '(A3,/)') 'end'\n\n");

			fprintf(fp,
				"      print '( /, \"----------end 4 point----------\", / )'\n\n");

			// --------4点後のdo文ここから--------
			fprintf(fp, "      do l = kk, npN\n");
			fprintf(fp, "         q = nextP\n");
			fprintf(fp, "         q2 = dspline_isw(nextP)\n\n");
			fprintf(fp, "         t1 = omp_get_wtime()\n");

			// 計算する関数に飛ぶ部分	基本的に既存部分の流用
			if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
				fprintf(fp, "         iloop_inner = 0;\n");
				fprintf(fp, "         while(OAT_Eecntl_Continue()){\n");
				s = "            call " + FuncName + "(";
			}
			else {
				s = "         call " + FuncName + "(";
			}
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str + ", ";
			}
			s += "q)";
			s = SepLongStr(s);
			fprintf(fp, "%s\n", s.c_str());

			fprintf(fp, "         t2 = omp_get_wtime()\n\n");
			fprintf(fp, "         time = t2 - t1\n\n");
			fprintf(fp,
				"         print '( /, \"---para is \", I0, \", time is \", G14.7, \"---\", / )', q, time\n\n");
			fprintf(fp,
				"         h(kk) = q\n         DE(p,q2) = 1\n         DE(p,nn+1) = time\n\n");

			// --------ギブンス変換do文ここから--------
			fprintf(fp,
				"         do q2 = q2, nn\n\n            do i = 1, nn + 1\n               G(i,i) = 0\n            end do\n\n");
			fprintf(fp, "            G(nn+1,nn+1) = 1\n");

			// G用計算部分
			fprintf(fp,
				"            radius = dsqrt( ( DE(q2,q2) ) * ( DE(q2,q2) ) + ( DE(p,q2) ) * ( DE(p,q2) ) )\n\n");
			fprintf(fp,
				"            if ( radius .eq. 0 ) then\n               exit\n            endif\n\n");
			fprintf(fp,
				"            cosine = DE(q2,q2) / radius\n            sine = DE(p,q2) / radius\n\n            G(p,p) = cosine\n            G(q2,q2) = cosine\n\n            G(q2,p) = sine\n            G(p,q2) = -1 * sine\n\n\n");

			// R=G*DEギブンス変換部分
			fprintf(fp,
				"            R(q2,q2) = R(q2,q2) + G(q2,q2) * DE(q2,q2)\n            R(q2,q2) = R(q2,q2) + G(q2,nn+1) * DE(nn+1,q2)\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               R(q2,q2+1) = R(q2,q2+1) + G(q2,q2) * DE(q2,q2+1)\n               R(q2,q2+1) = R(q2,q2+1) + G(q2,nn+1) * DE(nn+1,q2+1)\n            endif\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               R(q2,q2+2) = R(q2,q2+2) + G(q2,q2) * DE(q2,q2+2)\n               R(q2,q2+2) = R(q2,q2+2) + G(q2,nn+1) * DE(nn+1,q2+2)\n            endif\n\n");
			fprintf(fp,
				"            R(q2,nn+1) = R(q2,nn+1) + G(q2,q2) * DE(q2,nn+1)\n            R(q2,nn+1) = R(q2,nn+1) + G(q2,nn+1) * DE(nn+1,nn+1)\n\n\n");

			fprintf(fp,
				"            R(nn+1,q2) = R(nn+1,q2) + G(nn+1,q2) * DE(q2,q2)\n            R(nn+1,q2) = R(nn+1,q2) + G(nn+1,nn+1) * DE(nn+1,q2)\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               R(nn+1,q2+1) = R(nn+1,q2+1) + G(nn+1,q2) * DE(q2,q2+1)\n               R(nn+1,q2+1) = R(nn+1,q2+1) + G(nn+1,nn+1) * DE(nn+1,q2+1)\n            endif\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               R(nn+1,q2+2) = R(nn+1,q2+2) + G(nn+1,q2) * DE(q2,q2+2)\n               R(nn+1,q2+2) = R(nn+1,q2+2) + G(nn+1,nn+1) * DE(nn+1,q2+2)\n            endif\n\n");
			fprintf(fp,
				"            R(nn+1,nn+1) = R(nn+1,nn+1) + G(nn+1,q2) * DE(q2,nn+1)\n            R(nn+1,nn+1) = R(nn+1,nn+1) + G(nn+1,nn+1) * DE(nn+1,nn+1)\n\n\n");

			fprintf(fp,
				"            DE(q2,q2) = R(q2,q2)\n\n            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               DE(q2,q2+1) = R(q2,q2+1)\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               DE(q2,q2+2) = R(q2,q2+2)\n            endif\n\n            DE(q2,nn+1) = R(q2,nn+1)\n\n\n");
			fprintf(fp,
				"            DE(nn+1,q2) = R(nn+1,q2)\n\n            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               DE(nn+1,q2+1) = R(nn+1,q2+1)\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               DE(nn+1,q2+2) = R(nn+1,q2+2)\n            endif\n\n            DE(nn+1,nn+1) = R(nn+1,nn+1)\n\n\n");
			fprintf(fp,
				"            R(q2,q2) = 0\n\n            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               R(q2,q2+1) = 0\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               R(q2,q2+2) = 0\n            endif\n\n            R(q2,nn+1) = 0\n\n\n");
			fprintf(fp,
				"            R(nn+1,q2) = 0\n\n            if ( ( ( q2 + 1 ) .ne. nn + 1 ) .and. ( ( q2 + 1 ) .le. nn + 1 ) ) then\n               R(nn+1,q2+1) = 0\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn + 1 ) .and. ( ( q2 + 2 ) .le. nn + 1 ) ) then\n               R(nn+1,q2+2) = 0\n            endif\n\n            R(nn+1,nn+1) = 0\n\n");

			fprintf(fp, "         end do\n\n");
			// ------ギブンス変換do文部分ここまで------

			// ギブンス変換後,推定値計算,次の標本点決定部分
			// 推定値代入初期化部分
			fprintf(fp,
				"         do i = 1, nn\n            x(i) = DE(i,nn+1)\n         end do\n\n");

			// 後退代入で推定値計算部分
			fprintf(fp,
				"         do i = nn, 1, -1\n            do j = i + 1, nn\n               x(i) = x(i) - DE(i,j) * x(j)\n            end do\n\n            x(i) = x(i) / DE(i,i)\n         end do\n\n");

			// デバッグ用，推定値表示部分
			fprintf(fp,
				"         if ( DEBUG .gt. 0 ) then\n            print '( /, \"estimation\" )'\n\n            do i = 1, nn\n               print *, '[', i, ']', x(i)\n            end do\n\n            print *, ''\n         endif\n\n");

			// 推定値が最小となる番号をベストパラメタに設定する部分
			fprintf(fp,
				"         temp = x(1)\n         bestP = 1\n\n         do i = 1, npN\n            if ( x(dspline_isw(i)) .lt. temp ) then\n               temp = x(dspline_isw(i))\n               bestP = i\n            endif\n         end do\n\n");
			fprintf(fp,
				"         print *, 'best para = ', bestP, ' time = ', x(dspline_isw(bestP))\n\n");

			// 選択基準2用配列初期化部分
			fprintf(fp,
				"         do i = 1, npN\n            select2(i,2) = i\n            select2(i,1) = 0\n         end do\n\n");

			// 変化率計算部分
			fprintf(fp,
				"         do i = 2, npN - 1\n            select2(i,1) = dabs( x(dspline_isw(i-1)) - 2 * x(dspline_isw(i)) + x(dspline_isw(i+1)) )\n         end do\n\n");

			// select2ソート部分
			fprintf(fp,
				"         do i = 1, npN - 1\n            do j = npN, i + 1, -1\n               if ( select2(j-1,1) .lt. select2(j,1) ) then\n                  temp = select2(j,1)\n                  temp2 = select2(j,2)\n                  select2(j,1) = select2(j-1,1)\n                  select2(j,2) = select2(j-1,2)\n                  select2(j-1,1) = temp\n                  select2(j-1,2) = temp2\n               endif\n            end do\n         end do\n\n");

			// 推定したベストPがすでに標本点にあるなら選択基準2,ないなら選択基準1　選択する部分
			fprintf(fp,
				"         do i = 1, npN\n            if ( bestP .eq. h(i) ) then\n               do j = 1, npN\n                  do k = 1, npN\n                     if ( select2(j,2) .eq. h(k) ) then\n                        exit\n                     endif\n                     if ( k + 1 .eq. npN + 1 ) then\n                        nextP = select2(j,2)\n                        goto 222\n                     endif\n                  end do\n               end do\n\n               exit\n            else\n               nextP = bestP\n            endif\n         end do\n\n222      print '( \" nextP = \", I0, \" bestP = \", I0, / )', nextP, bestP\n\n");

			// 前回推定したパラメタと同じならカウント+1,違うならカウントを1に初期化する部分
			fprintf(fp,
				"         if ( prebestP .eq. bestP ) then\n            count = count + 1\n         else\n            count = 1\n         endif\n\n");

			// 今回推定したベストパラメタをprebestPに設定
			fprintf(fp, "         prebestP = bestP\n\n");

			// 推定値をエクセルに書き込みする部分
			fprintf(fp,
				"      do i = 1, nn\n         write( 11, * ) x(i)\n      end do\n      write( 11, '(A3,/)') 'end'\n\n");

			fprintf(fp, "         kk = kk + 1\n\n");

			fprintf(fp, "         print *, 'count = ', count\n");

			fprintf(fp,
				"         if ( count .eq. 3 ) then\n            print *, 'count end'\n            print *, 'usedParaNums = ', kk - 1\n            exit\n         endif\n\n");

			fprintf(fp, "      end do\n\n");
			// --------4点後のdo文ここまで--------

			// 戻り値の設定
			fprintf(fp, "      dspgiv%s%s = bestP\n\n", TuneGroupName.c_str(),
				Name.c_str());

			fprintf(fp, "end function dspgiv%s%s\n\n\n", TuneGroupName.c_str(),
				Name.c_str());
		}
		// 1次元用d-Spline（インストール時自動チューニング用）ここまで

		// 1次元用d-Spline（実行時自動チューニング用）
		if (TuneGroup == tgDynamic) {
			int CaseNums = (int)((variedToValue[0] - variedFromValue[0])
				/ variedStepValue[0] + 1);

			// 関数名部分
			fprintf(fp,
				"subroutine dynamicDspline%s%s( npN, isw, Dswitch, time )\n",
				TuneGroupName.c_str(), Name.c_str());

			// 仮引数宣言部分
			fprintf(fp, "      integer npN, isw, Dswitch\n");
			fprintf(fp, "      double precision time\n\n");

			// 変数宣言部分
			fprintf(fp, "      integer :: DEBUG = 1\n");
			fprintf(fp, "      integer nn\n");
			fprintf(fp, "      double precision, save :: DE(%d+1,%d+1)\n",
				CaseNums * 3 - 2, CaseNums * 3 - 2);
			fprintf(fp, "      double precision, save :: R(%d+1,%d+1)\n",
				CaseNums * 3 - 2, CaseNums * 3 - 2);
			fprintf(fp, "      double precision, save :: G(%d+1,%d+1)\n",
				CaseNums * 3 - 2, CaseNums * 3 - 2);
			fprintf(fp, "      double precision radius, cosine, sine\n");
			fprintf(fp, "      integer p, q, q2\n");
			fprintf(fp, "      integer, save :: count1 = 1\n");
			fprintf(fp, "      integer, save :: count2 = 0\n");
			fprintf(fp,
				"      double precision, allocatable, dimension(:) :: x\n");
			fprintf(fp, "      double precision temp\n");
			fprintf(fp, "      integer temp2\n");
			fprintf(fp, "      integer bestP\n");
			fprintf(fp, "      integer nextP\n");
			fprintf(fp, "      integer, save :: prebestP = 0\n");
			fprintf(fp, "      double precision select2(npN,2)\n");
			fprintf(fp, "      integer, save :: h(%d+1)\n", CaseNums);
			fprintf(fp, "      integer, save :: kk = 1\n");
			fprintf(fp, "      double precision, parameter :: alfa = 0.1\n");
			fprintf(fp, "      integer, save :: Fcount = 0\n");
			fprintf(fp, "      integer i, j, k\n\n");

			fprintf(fp, "      nn = npN * 3 - 2\n");
			fprintf(fp, "      allocate(x(nn))\n");
			fprintf(fp, "      bestP = 0\n\n");

			// エクセル書き込み用部分
			fprintf(fp, "      open( 11, file = 'd-SplineData.csv' )\n\n");

			// パラメタ探索後の終了部分
			fprintf(fp,
				"      if ( count2 .eq. 1 ) then\n         return\n      endif\n\n");
			fprintf(fp,
				"      if ( count1 .eq. 3 ) then\n         print *, 'count1=3 end'\n         print *, 'usedParaNums = ', Fcount-1\n         isw = prebestP\n         count2 = 1\n         return\n      endif\n\n");

			// h初期化部分
			fprintf(fp,
				"      if ( Fcount .eq. 0 ) then\n         do i = 1, npN + 1\n               h(i) = npN + 1\n         end do\n      endif\n\n");
			fprintf(fp, "      p = nn + 1\n\n");

			// Dswitch1部分ここから--------------------
			fprintf(fp, "      if ( Dswitch .eq. 1 ) then\n\n");

			// switch部分ここから
			fprintf(fp, "         select case (Fcount)\n");
			fprintf(fp,
				"            case (0)\n               isw = 1\n            case (1)\n               isw = npN\n            case (2)\n               isw = ( 2 + npN ) / 3\n            case (3)\n               isw = ( 1 + 2 * npN ) / 3\n");

			// default部分
			fprintf(fp, "            case default\n\n");

			// 推定値初期化部分
			fprintf(fp,
				"               do i = 1, nn\n                  x(i) = DE(i,nn+1)\n               end do\n\n");

			// 後退代入計算部分
			fprintf(fp,
				"               do i = nn, 1, -1\n                  do j = i + 1, nn\n                     x(i) = x(i) - DE(i,j) * x(j)\n                  end do\n\n                  x(i) = x(i) / DE(i,i)\n               end do\n\n");

			// デバッグ用推定値表示部分
			fprintf(fp,
				"               if ( DEBUG .gt. 0 ) then\n                  print '( /, \"estimation\" )'\n\n                  do i = 1, npN\n                     j = 3 * i - 2\n                     print *, '[', i, ']', x(j)\n                  end do\n\n                  print *, ''\n               endif\n\n");

			// 最小推定値探索部分
			fprintf(fp,
				"               temp = x(1)\n               bestP = 1\n\n               do i = 1, npN\n                  j = 3 * i - 2\n\n                  if ( x(j) .lt. temp ) then\n                     temp = x(j)\n                     bestP = i\n                  endif\n               end do\n\n");

			// ベストパラメタ表示部分

			fprintf(fp,
				"               print *, 'best para = ', bestP, ' time = ', x(bestP*3)\n\n");

			// 選択基準2初期化部分
			fprintf(fp,
				"               do i = 1, npN\n                  select2(i,2) = i\n                  select2(i,1) = 0\n               end do\n\n");

			// 変化率計算部分
			fprintf(fp,
				"               do i = 2, npN - 1\n                  j = 3 * i - 2\n\n                  select2(i,1) = dabs( x(j-3) - 2 * x(j) + x(j+3) )\n               end do\n\n");

			// select2ソート部分
			fprintf(fp,
				"               do i = 1, npN - 1\n                  do j = npN, i + 1, -1\n                     if ( select2(j-1,1) .lt. select2(j,1) ) then\n                        temp = select2(j,1)\n                        temp2 = select2(j,2)\n                        select2(j,1) = select2(j-1,1)\n                        select2(j,2) = select2(j-1,2)\n                        select2(j-1,1) = temp\n                        select2(j-1,2) = temp2\n                     endif\n                  end do\n               end do\n\n");

			// 選択基準1or2選択部分
			fprintf(fp,
				"               do i = 1, npN\n                  if ( bestP .eq. h(i) ) then\n                     do j = 1, npN\n                        do k = 1, npN\n                           if ( select2(j,2) .eq. h(k) ) then\n                              exit\n                           endif\n                           if ( k + 1 .eq. npN + 1 ) then\n                              nextP = select2(j,2)\n                              goto 111\n                           endif\n                        end do\n                     end do\n\n                     exit\n                  else\n                     nextP = bestP\n                  endif\n               end do\n\n");

			// ベストパラメタと次のパラメタを表示
			fprintf(fp,
				"111               print '( \" nextP = \", I0, \" bestP = \", I0, / )', nextP, bestP\n\n");

			// iswに次に使うパラメタをセットする部分
			fprintf(fp, "               isw = nextP\n\n");

			// 3回カウント用部分
			fprintf(fp,
				"               if ( prebestP .eq. bestP ) then\n                  count1 = count1 + 1\n               else\n                  count1 = 1\n               endif\n\n");

			// default部分ここまで
			fprintf(fp, "               prebestP = bestP\n\n");

			// 推定値をエクセルに書き込みする部分
			fprintf(fp,
				"               do i = 1, nn\n                  write( 11, * ) x(i)\n               end do\n");
			fprintf(fp, "               write( 11, '(A3,/)' ) 'end'\n\n");

			// switchここまで
			fprintf(fp,
				"         end select\n\n         Fcount = Fcount + 1\n\n");

			// Dswitch1ここまで------------
			fprintf(fp, "      endif\n\n");

			// Dswitch1ここから----------------
			fprintf(fp, "      if ( Dswitch .eq. 2 ) then\n\n");

			// DE初期化部分
			fprintf(fp, "         if ( Fcount .eq. 1 ) then\n");
			fprintf(fp,
				"            do i = 1, nn + 1\n               do j = 1, nn + 1\n                  DE(i,j) = 0\n                  R(i,j) = 0\n               end do\n            end do\n\n");
			fprintf(fp,
				"            do i = 1, nn - 2\n               DE(i,i) = 1 * alfa\n               DE(i,i+1) = -2 * alfa\n               DE(i,i+2) = 1*alfa\n            end do\n         endif\n\n");

			// 標本点とその実測値のセット部分
			fprintf(fp,
				"         q = isw\n         q2 = 3 * q - 2\n         h(kk) = q\n\n");
			fprintf(fp,
				"         DE(p,q2) = 1\n         DE(p,nn+1) = time\n\n");

			// ギブンス変換ループ部分--------------------
			fprintf(fp, "         do q2 = q2, nn\n\n");

			// G初期化
			fprintf(fp,
				"            do i = 1, nn + 1\n               G(i,i) = 0\n            end do\n\n");
			fprintf(fp, "            G(nn+1,nn+1) = 1\n");

			// G用計算部分
			fprintf(fp,
				"            radius = dsqrt( ( DE(q2,q2) ) * ( DE(q2,q2) ) + ( DE(p,q2) ) * ( DE(p,q2) ) )\n\n");
			fprintf(fp,
				"            if ( radius .eq. 0 ) then\n               exit\n            endif\n\n");
			fprintf(fp,
				"            cosine = DE(q2,q2) / radius\n            sine = DE(p,q2) / radius\n\n");
			fprintf(fp,
				"            G(p,p) = cosine\n            G(q2,q2) = cosine\n\n            G(q2,p) = sine\n            G(p,q2) = -1*sine\n\n");

			// R=G*DEギブンス変換部分
			fprintf(fp,
				"            R(q2,q2) = R(q2,q2) + G(q2,q2) * DE(q2,q2)\n            R(q2,q2) = R(q2,q2) + G(q2,nn+1) * DE(nn+1,q2)\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 1 ) .ne. nn+1 ) .and. ( ( q2 + 1 ) .le. nn+1 ) ) then\n               R(q2,q2+1) = R(q2,q2+1) + G(q2,q2) * DE(q2,q2+1)\n               R(q2,q2+1) = R(q2,q2+1) + G(q2,nn+1) * DE(nn+1,q2+1)\n            endif\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 2 ) .ne. nn+1 ) .and. ( ( q2 + 2 ) .le. nn+1 ) ) then\n               R(q2,q2+2) = R(q2,q2+2) + G(q2,q2) * DE(q2,q2+2)\n               R(q2,q2+2) = R(q2,q2+2) + G(q2,nn+1) * DE(nn+1,q2+2)\n            endif\n\n");
			fprintf(fp,
				"            R(q2,nn+1) = R(q2,nn+1) + G(q2,q2) * DE(q2,nn+1)\n            R(q2,nn+1) = R(q2,nn+1) + G(q2,nn+1) * DE(nn+1,nn+1)\n\n");
			fprintf(fp,
				"            R(nn+1,q2) = R(nn+1,q2) + G(nn+1,q2) * DE(q2,q2)\n            R(nn+1,q2) = R(nn+1,q2) + G(nn+1,nn+1) * DE(nn+1,q2)\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 1 ) .ne. nn+1 ) .and. ( ( q2 + 1 ) .le. nn+1 ) ) then\n               R(nn+1,q2+1) = R(nn+1,q2+1) + G(nn+1,q2) * DE(q2,q2+1)\n               R(nn+1,q2+1) = R(nn+1,q2+1) + G(nn+1,nn+1) * DE(nn+1,q2+1)\n            endif\n\n");
			fprintf(fp,
				"            if ( ( ( q2 + 2 ) .ne. nn+1 ) .and. ( ( q2 + 2 ) .le. nn+1 ) ) then\n               R(nn+1,q2+2) = R(nn+1,q2+2) + G(nn+1,q2) * DE(q2,q2+2)\n               R(nn+1,q2+2) = R(nn+1,q2+2) + G(nn+1,nn+1) * DE(nn+1,q2+2)\n            endif\n\n");
			fprintf(fp,
				"            R(nn+1,nn+1) = R(nn+1,nn+1) + G(nn+1,q2) * DE(q2,nn+1)\n            R(nn+1,nn+1) = R(nn+1,nn+1) + G(nn+1,nn+1) * DE(nn+1,nn+1)\n\n");
			fprintf(fp,
				"            DE(q2,q2) = R(q2,q2)\n\n            if ( ( ( q2 + 1 ) .ne. nn+1 ) .and. ( ( q2 + 1 ) .le. nn+1 ) ) then\n               DE(q2,q2+1) = R(q2,q2+1)\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn+1 ) .and. ( ( q2 + 2 ) .le. nn+1 ) ) then\n               DE(q2,q2+2) = R(q2,q2+2)\n            endif\n\n            DE(q2,nn+1) = R(q2,nn+1)\n\n");
			fprintf(fp,
				"            DE(nn+1,q2) = R(nn+1,q2)\n\n            if ( ( ( q2 + 1 ) .ne. nn+1 ) .and. ( ( q2 + 2 ) .le. nn+1 ) ) then\n               DE(nn+1,q2+1) = R(nn+1,q2+1)\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn+1 ) .and. ( ( q2 + 2 ) .le. nn+1 ) ) then\n               DE(nn+1,q2+2) = R(nn+1,q2+2)\n            endif\n\n            DE(nn+1,nn+1) = R(nn+1,nn+1)\n\n");
			fprintf(fp,
				"            R(q2,q2) = 0\n\n            if ( ( ( q2 + 1 ) .ne. nn+1 ) .and. ( ( q2 + 1 ) .le. nn+1 ) ) then\n               R(q2,q2+1) = 0\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn+1 ) .and. ( ( q2 + 2 ) .le. nn+1 ) ) then\n               R(q2,q2+2) = 0\n            endif\n\n            R(q2,nn+1) = 0\n\n            R(nn+1,q2) = 0\n\n            if ( ( ( q2 + 1 ) .ne. nn+1 ) .and. ( ( q2 + 1 ) .le. nn+1 ) ) then\n               R(nn+1,q2+1) = 0\n            endif\n\n            if ( ( ( q2 + 2 ) .ne. nn+1 ) .and. ( ( q2 + 2 ) .le. nn+1 ) ) then\n               R(nn+1,q2+2) = 0\n            endif\n\n            R(nn+1,nn+1) = 0\n         end do\n\n");

			// ギブンス変換ループここまで------------------

			// Dswitch2ここまで
			fprintf(fp, "         kk=kk+1\n\n      endif\n\n");

			// 関数ここまで
			fprintf(fp,
				"      return\n\nend subroutine dynamicDspline%s%s\n\n"
				, TuneGroupName.c_str(), Name.c_str());
		}
		// 1次元用d-Spline（実行時自動チューニング用）ここまで
	}
	// 1次元用d-Spline　ここまで

	// 2次元用d-Spline
	if (FittingDspline == 2) {
		// 2次元用d-Spline（インストール時自動チューニング用）
		if (TuneGroup == tgInstall) {
			int CaseNums[2];
			CaseNums[0] = (int)((variedToValue[0] - variedFromValue[0])
				/ variedStepValue[0] + 1);
			CaseNums[1] = (int)((variedToValue[1] - variedFromValue[1])
				/ variedStepValue[1] + 1);
			int nnn = (3 * CaseNums[0] - 2) * (3 * CaseNums[1] - 2);

			// 関数名部分
			fprintf(fp, "function dsp2giv%s%s( npN, para1st, para2nd",
				TuneGroupName.c_str(), Name.c_str());

			// ここから流用　変数名出力のため------

			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				ArgStr = Script->GetATExecArgStr(false, "", "");
				fprintf(fp, "%s", ArgStr.c_str());
				delete Script;
			}
			else {
				ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "",
					"");
				fprintf(fp, "%s", ArgStr.c_str());
			}

			// ここまで流用　変数名出力用------
			fprintf(fp, ")\n");

			// use文の宣言
			fprintf(fp, "      use omp_lib\n");

			// implicit noneの宣言
			fprintf(fp, "      implicit none\n");

			// 仮引数宣言部分
			fprintf(fp, "      integer npN\n");
			fprintf(fp, "      integer para1st, para2nd\n");

			// ここから流用　変数名出力のため------

			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				ArgStr = Script->GetATExecArgStr(true, "", "");
				fprintf(fp, "%s", ArgStr.c_str());
				delete Script;
			}
			else {
				ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true, "",
					"");
				fprintf(fp, "%s", ArgStr.c_str());
			}

			// ここまで流用　変数名出力用------

			fprintf(fp, "      integer :: dsp2giv%s%s\n",
				TuneGroupName.c_str(), Name.c_str());
			fprintf(fp, "      integer :: DEBUG = 1\n");
			fprintf(fp, "      integer p3_1st, p3_2nd, p3_2nd_2\n");
			fprintf(fp, "      integer nn\n");
			fprintf(fp, "      double precision DE(%d+1,%d+1)\n", nnn, nnn);
			fprintf(fp, "      double precision R(%d+1,%d+1)\n", nnn, nnn);
			fprintf(fp, "      double precision radius, cosine, sine\n");
			fprintf(fp, "      integer p, q, q2\n");
			fprintf(fp, "      double precision time\n");
			fprintf(fp, "      integer count\n");
			fprintf(fp,
				"      double precision, allocatable, dimension(:) :: x\n");
			fprintf(fp, "      double precision temp\n");
			fprintf(fp, "      integer temp2, bestP, nextP, prebestP\n");
			fprintf(fp, "      double precision select2(npN,2)\n");
			fprintf(fp, "      integer h(npN)\n");
			fprintf(fp, "      integer dspline_isw(npN)\n");
			fprintf(fp, "      integer kk\n");
			fprintf(fp, "      double precision, parameter :: alfa = 0.1\n");
			fprintf(fp, "      double precision t1, t2\n");
			fprintf(fp, "      integer F2(16)\n");
			fprintf(fp, "      integer i, j, k, l, i4\n\n");

			fprintf(fp, "      p3_1st = para1st * 3\n");
			fprintf(fp, "      p3_2nd = para2nd * 3\n");
			fprintf(fp, "      p3_2nd_2 = p3_2nd - 2\n");
			fprintf(fp, "      nn = ( p3_1st - 2 ) * p3_2nd_2\n");
			fprintf(fp, "      count = 1\n");
			fprintf(fp, "      bestP = 0\n");
			fprintf(fp, "      kk = 1\n\n");

			fprintf(fp, "      allocate(x(nn))\n\n");

			// エクセル書き込み用部分
			fprintf(fp, "      open( 11, file = 'd-SplineData.csv' )\n\n");

			// DE初期化部分
			fprintf(fp,
				"      do i = 1, nn + 1\n         do j = 1, nn + 1\n            DE(i,j) = 0\n            R(i,j) = 0\n         end do\n      end do\n\n");

			// DE 1,-2,1   1,1,-4,1,1 セット部分
			fprintf(fp, "      j = 1\n\n");
			fprintf(fp,
				"      do i = 1, nn - 4\n         if ( i .le. p3_2nd_2 - 2 ) then\n            DE(i,i) = 1 * alfa\n            DE(i,i+1) = -2 * alfa\n            DE(i,i+2) = 1 * alfa\n");
			fprintf(fp,
				"         else if ( ( nn - p3_2nd ) .lt. i ) then\n            DE(i,i+2) = 1 * alfa\n            DE(i,i+3) = -2 * alfa\n            DE(i,i+4) = 1 * alfa\n");
			fprintf(fp,
				"         else\n            if ( ( MOD(i+1,p3_2nd_2) .eq. 0 ) .or. ( MOD(i+2,p3_2nd_2) .eq. 0 ) ) then\n               DE(i,j) = 1 * alfa\n               DE(i,j+p3_2nd_2) = -2 * alfa\n               DE(i,j+2*p3_2nd_2) = 1 * alfa\n");
			fprintf(fp,
				"            else\n               DE(i,j) = 1 * alfa\n               DE(i,j+p3_2nd_2-1) = 1 * alfa\n               DE(i,j+p3_2nd_2) = -4 * alfa\n               DE(i,j+p3_2nd_2+1) = 1 * alfa\n               DE(i,j+2*p3_2nd_2) = 1 * alfa\n            endif\n\n");
			fprintf(fp,
				"            j = j + 1\n         endif\n      end do\n\n");

			/* DEをあらかじめ上三角行列に整形 */
			fprintf(fp,
				"      q = 0\n\n      do i = ( p3_2nd_2 - 2 ) + 1, nn - p3_2nd\n         do j = 1, p3_2nd_2 - 2\n");

			// G用計算部分
			fprintf(fp,
				"            radius = dsqrt( ( DE(q+j,q+j) ) * ( DE(q+j,q+j) ) + ( DE(i,q+j) ) * ( DE(i,q+j) ) )\n\n            if ( radius .eq. 0 ) then\n               cycle\n            end if\n\n            cosine = DE(q+j,q+j) / radius\n            sine = DE(i,q+j) / radius\n\n");

			// R=回転行列*DEギブンス変換部分
			fprintf(fp,
				"            do k = j, 2 * p3_2nd_2 + 1\n               if ( k .eq. nn ) then\n                  exit\n               end if\n\n               R(q+j,q+k) = R(q+j,q+k) + cosine * DE(q+j,q+k)\n               R(q+j,q+k) = R(q+j,q+k) + sine * DE(i,q+k)\n\n               R(i,q+k) = R(i,q+k) + (-1) * sine * DE(q+j,q+k)\n               R(i,q+k) = R(i,q+k) + cosine * DE(i,q+k)\n\n               DE(q+j,q+k) = R(q+j,q+k)\n               DE(i,q+k) = R(i,q+k)\n\n               R(q+j,q+k) = 0\n               R(i,q+k) = 0\n");

			fprintf(fp,
				"            end do\n         end do\n\n         q = q + 1\n      end do\n\n");
			/* 整形終了 */

			// h初期化部分
			fprintf(fp,
				"      do i = 1, npN\n         h(i) = npN + 1\n      end do\n\n"
				);

			// 初期16点決定部分
			fprintf(fp,
				"      F2(1) = 1\n      F2(2) = ( 2 + para2nd ) / 3\n      F2(3) = ( 1 + 2 * para2nd ) / 3\n      F2(4) = para2nd\n");
			fprintf(fp,
				"      F2(5) = ( ( 2 + para1st ) / 3 - 1 ) * para2nd + 1\n");
			fprintf(fp,
				"      F2(6) = ( ( 2 + para1st ) / 3 - 1 ) * para2nd + ( 2 + para2nd ) / 3\n");
			fprintf(fp,
				"      F2(7) = ( ( 2 + para1st ) / 3 - 1 ) * para2nd + ( 1 + 2 * para2nd ) / 3\n");
			fprintf(fp,
				"      F2(8) = ( ( 2 + para1st ) / 3 - 1 ) * para2nd + para2nd\n");
			fprintf(fp,
				"      F2(9) = ( ( 1 + 2 * para1st ) / 3 - 1 ) * para2nd + 1\n"
				);
			fprintf(fp,
				"      F2(10) = ( ( 1 + 2 * para1st ) / 3 - 1 ) * para2nd + ( 2 + para2nd ) / 3\n");
			fprintf(fp,
				"      F2(11) = ( ( 1 + 2 * para1st ) / 3 - 1 ) * para2nd + ( 1 + 2 * para2nd ) / 3\n");
			fprintf(fp,
				"      F2(12) = ( ( 1 + 2 * para1st ) / 3 - 1 ) * para2nd + para2nd\n");
			fprintf(fp, "      F2(13) = ( para1st - 1 ) * para2nd + 1\n");
			fprintf(fp,
				"      F2(14) = ( para1st - 1 ) * para2nd + ( 2 + para2nd ) / 3\n");
			fprintf(fp,
				"      F2(15) = ( para1st - 1 ) * para2nd + ( 1 + 2 * para2nd ) / 3\n");
			fprintf(fp,
				"      F2(16) = ( para1st - 1 ) * para2nd + para2nd\n\n");
			fprintf(fp, "      p = nn + 1\n\n");

			// パラメータの取りうる値が,d-Spline関数中ではどの点に対応するかを設定
			fprintf(fp,
				"      do i = 1, npN\n         dspline_isw(i) = ( 3 * i - 2 ) + int( ( i - 1 ) / para2nd ) * 2 * ( p3_2nd - 3 )\n      end do\n\n");

			// 初期16点の計算用部分-------------------------------------------------------
			fprintf(fp,
				"      do i4 = 1, 16\n         q = F2(i4)\n         q2 = dspline_isw(F2(i4))\n\n         t1 = omp_get_wtime()\n");

			// 計算する関数に飛ぶ部分	基本的に既存部分の流用
			if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
				fprintf(fp, "         iloop_inner = 0;\n");
				fprintf(fp, "         while(OAT_Eecntl_Continue()){\n");
				s = "            call " + FuncName + "(";
			}
			else {
				s = "         call " + FuncName + "(";
			}
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str + ", ";
			}
			s += "q)";
			s = SepLongStr(s);
			fprintf(fp, "%s\n", s.c_str());

			fprintf(fp,
				"         t2 = omp_get_wtime()\n\n         time = t2 - t1\n\n");
			fprintf(fp,
				"         print '( /, \"---para is \", I0, \", time is \", G14.7, \"---\", / )', q, time\n\n");
			fprintf(fp,
				"         h(kk) = q\n\n         DE(p,q2) = 1\n         DE(p,nn+1) = time\n\n");

			// ------初期16点ギブンス変換do文部分ここから------
			fprintf(fp, "         do q2 = q2, nn\n\n");

			// G用計算部分
			fprintf(fp,
				"            radius = dsqrt( ( DE(q2,q2) ) * ( DE(q2,q2) ) + ( DE(p,q2) ) * ( DE(p,q2) ) )\n\n");
			fprintf(fp,
				"            if ( radius .eq. 0 ) then\n               cycle\n            endif\n\n");
			fprintf(fp,
				"            cosine = DE(q2,q2) / radius\n            sine = DE(p,q2) / radius\n\n");

			// R=回転行列*DEギブンス変換部分
			fprintf(fp,
				"            do j = q2, q2 + 2 * p3_2nd_2 + 1\n               if ( j .eq. p + 1 ) then\n                  exit\n               endif\n\n");
			fprintf(fp,
				"               R(q2,j) = R(q2,j) + cosine * DE(q2,j)\n               R(q2,j) = R(q2,j) + sine * DE(p,j)\n\n               R(p,j) = R(p,j) + (-1) * sine * DE(q2,j)\n               R(p,j) = R(p,j) + cosine * DE(p,j)\n\n");
			fprintf(fp,
				"               DE(q2,j) = R(q2,j)\n               DE(p,j) = R(p,j)\n\n               R(q2,j) = 0\n               R(p,j) = 0\n\n");

			fprintf(fp,
				"               if ( j .eq. q2 + 2 * p3_2nd_2 + 1 ) then\n                  R(q2,p) = R(q2,p) + cosine * DE(q2,p)\n                  R(q2,p) = R(q2,p) + sine * DE(p,p)\n\n                   R(p,p) = R(p,p) + (-1) * sine * DE(q2,p)\n                  R(p,p) = R(p,p) + cosine * DE(p,p)\n\n");
			fprintf(fp,
				"                  DE(q2,p) = R(q2,p)\n                  DE(p,p) = R(p,p)\n                  R(q2,p) = 0\n                  R(p,p) = 0\n");

			fprintf(fp,
				"               endif\n            end do\n         end do\n\n         kk = kk + 1\n      end do\n\n");
			// ------初期16点ギブンス変換do文部分ここまで------

			// ------初期16点計算後，ベストパラメタと次のパラメタ決定ここから------

			// 推定値代入初期化部分
			fprintf(fp,
				"      do i = 1, nn\n         x(i) = DE(i,nn+1)\n      end do\n\n");

			// 後退代入で推定値計算部分
			fprintf(fp,
				"      do i = nn, 1, -1\n         do j = i + 1, nn\n            x(i) = x(i) - DE(i,j) * x(j)\n         end do\n\n         x(i) = x(i) / DE(i,i)\n      end do\n\n");

			// デバッグ用，推定値表示部分
			fprintf(fp,
				"      if ( DEBUG .gt. 0 ) then\n         print '( /, \"estimation\" )'\n\n         do i = 1, npN\n            print *, '[', i, ']', x(dspline_isw(i))\n         end do\n\n         print *, ''\n      endif\n\n");

			// 推定値が最小となる番号をベストパラメタに設定する部分
			fprintf(fp,
				"      temp = x(1)\n      bestP = 1\n\n      do i = 1, npN\n         if ( x(dspline_isw(i)) .lt. temp ) then\n            temp = x(dspline_isw(i))\n            bestP = i\n         endif\n      end do\n\n");
			fprintf(fp,
				"      print *, 'best para = ', bestP, ' time = ', x(dspline_isw(bestP))\n\n");

			// 選択基準2用配列初期化部分
			fprintf(fp,
				"      do i = 1, npN\n         select2(i,2) = i\n         select2(i,1) = 0\n      end do\n\n");

			// 変化率計算部分
			fprintf(fp, "      k = p3_2nd_2 * 3\n\n");
			fprintf(fp,
				"      do i = 2, npN - 1\n         j = dspline_isw(i)\n\n         if ( ( i .lt. para2nd ) .or. ( ( para1st - 1 ) * para2nd + 1 .lt. i ) ) then\n            select2(i,1) = dabs( x(j-3) - 2 * x(j) + x(j+3) )\n");
			fprintf(fp,
				"         else if ( ( ( MOD(i,para2nd) .eq. 0 ) .and. ( para2nd .lt. i ) ) .or. &\n          ( ( MOD(i-1,para2nd) .eq. 0 ) .and. ( i .lt. ( para1st - 1 ) * para2nd ) ) ) then\n            select2(i,1) = dabs( x(j-k) - 2 * x(j) + x(j+k) )\n");
			fprintf(fp,
				"         else if ( ( i .ne. para2nd ) .and. ( i .ne. ( para1st - 1 ) * para2nd + 1 ) ) then\n            select2(i,1) = dabs( x(j-3) + x(j-k) - 4 * x(j) + x(j+k) + x(j+3) )\n         endif\n      end do\n\n");

			// select2ソート部分
			fprintf(fp,
				"      do i = 1, npN - 1\n         do j = npN, i + 1, -1\n            if ( select2(j-1,1) .lt. select2(j,1) ) then\n               temp = select2(j,1)\n               temp2 = select2(j,2)\n               select2(j,1) = select2(j-1,1)\n               select2(j,2) = select2(j-1,2)\n               select2(j-1,1) = temp\n               select2(j-1,2) = temp2\n            endif\n         end do\n      end do\n\n");

			// 推定したベストPがすでに標本点にあるなら選択基準2,ないなら選択基準1　選択する部分
			fprintf(fp,
				"      do i = 1, npN\n         if ( bestP .eq. h(i) ) then\n            do j = 1, npN\n               do k = 1, npN\n                  if ( select2(j,2) .eq. h(k) ) then\n                     exit\n                  endif\n                  if ( k + 1 .eq. npN + 1 ) then\n                     nextP = select2(j,2)\n                     goto 111\n                  endif\n               end do\n            end do\n\n            exit\n         else\n            nextP = bestP\n         endif\n      end do\n\n111   print '( \" nextP = \", I0, \" bestP = \", I0, / )', nextP, bestP\n\n");

			// prebestPに16点計算終了時のベストパラメタを代入する部分
			fprintf(fp, "      prebestP = bestP\n\n");

			// 推定値をエクセルに書き込みする部分
			fprintf(fp,
				"      do i = 1, nn\n         write( 11, * ) x(i)\n      end do\n      write( 11, '(A3,/)') 'end'\n\n");

			fprintf(fp,
				"      print '( /, \"----------end 16 point----------\", / )'\n\n");

			// --------16点後のdo文ここから--------
			fprintf(fp, "      do l = kk, npN\n");
			fprintf(fp, "         q = nextP\n");
			fprintf(fp, "         q2 = dspline_isw(nextP)\n\n");
			fprintf(fp, "         t1 = omp_get_wtime()\n");

			// 計算する関数に飛ぶ部分	基本的に既存部分の流用
			if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
				fprintf(fp, "         iloop_inner = 0;\n");
				fprintf(fp, "         while(OAT_Eecntl_Continue()){\n");
				s = "            call " + FuncName + "(";
			}
			else {
				s = "         call " + FuncName + "(";
			}
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str + ", ";
			}
			s += "q)";
			s = SepLongStr(s);
			fprintf(fp, "%s\n", s.c_str());

			fprintf(fp, "         t2 = omp_get_wtime()\n\n");
			fprintf(fp, "         time = t2 - t1\n\n");
			fprintf(fp,
				"         print '( /, \"---para is \", I0, \", time is \", G14.7, \"---\", / )', q, time\n\n");
			fprintf(fp,
				"         h(kk) = q\n         DE(p,q2) = 1\n         DE(p,nn+1) = time\n\n");

			// --------ギブンス変換do文ここから--------
			// G用計算部分
			fprintf(fp, "      do q2 = q2, nn\n");
			fprintf(fp,
				"            radius = dsqrt( ( DE(q2,q2) ) * ( DE(q2,q2) ) + ( DE(p,q2) ) * ( DE(p,q2) ) )\n\n");
			fprintf(fp,
				"            if ( radius .eq. 0 ) then\n               exit\n            endif\n\n");
			fprintf(fp,
				"            cosine = DE(q2,q2) / radius\n            sine = DE(p,q2) / radius\n\n");

			// R=回転行列*DEギブンス変換部分
			fprintf(fp,
				"            do j = q2, q2 + 2 * p3_2nd_2 + 1\n               if ( j .eq. p + 1 ) then\n                  exit\n               endif\n\n");
			fprintf(fp,
				"               R(q2,j) = R(q2,j) + cosine * DE(q2,j)\n               R(q2,j) = R(q2,j) + sine * DE(p,j)\n\n               R(p,j) = R(p,j) + (-1) * sine * DE(q2,j)\n               R(p,j) = R(p,j) + cosine * DE(p,j)\n\n");
			fprintf(fp,
				"               DE(q2,j) = R(q2,j)\n               DE(p,j) = R(p,j)\n\n               R(q2,j) = 0\n               R(p,j) = 0\n\n");

			fprintf(fp,
				"               if ( j .eq. q2 + 2 * p3_2nd_2 + 1 ) then\n                  R(q2,p) = R(q2,p) + cosine * DE(q2,p)\n                  R(q2,p) = R(q2,p) + sine * DE(p,p)\n\n                  R(p,p) = R(p,p) + (-1) * sine * DE(q2,p)\n                  R(p,p) = R(p,p) + cosine * DE(p,p)\n\n");
			fprintf(fp,
				"                  DE(q2,p) = R(q2,p)\n                  DE(p,p) = R(p,p)\n                  R(q2,p) = 0\n                  R(p,p) = 0\n");

			fprintf(fp,
				"               endif\n            end do\n         end do\n\n"
				);
			// ------ギブンス変換do文部分ここまで------

			// ギブンス変換後,推定値計算,次の標本点決定部分
			// 推定値代入初期化部分
			fprintf(fp,
				"         do i = 1, nn\n            x(i) = DE(i,nn+1)\n         end do\n\n");

			// 後退代入で推定値計算部分
			fprintf(fp,
				"         do i = nn, 1, -1\n            do j = i + 1, nn\n               x(i) = x(i) - DE(i,j) * x(j)\n            end do\n\n            x(i) = x(i) / DE(i,i)\n         end do\n\n");

			// デバッグ用，推定値表示部分
			fprintf(fp,
				"         if ( DEBUG .gt. 0 ) then\n            print '( /, \"estimation\" )'\n\n            do i = 1, npN\n               print *, '[', i, ']', x(dspline_isw(i))\n            end do\n\n            print *, ''\n         endif\n\n");

			// 推定値が最小となる番号をベストパラメタに設定する部分
			fprintf(fp,
				"         temp = x(1)\n         bestP = 1\n\n         do i = 1, npN\n            if ( x(dspline_isw(i)) .lt. temp ) then\n               temp = x(dspline_isw(i))\n               bestP = i\n            endif\n         end do\n\n");
			fprintf(fp,
				"         print *, 'best para = ', bestP, ' time = ', x(dspline_isw(bestP))\n\n");

			// 選択基準2用配列初期化部分
			fprintf(fp,
				"         do i = 1, npN\n            select2(i,2) = i\n            select2(i,1) = 0\n         end do\n\n");

			// 変化率計算部分
			fprintf(fp, "      k = p3_2nd_2 * 3\n\n");
			fprintf(fp,
				"      do i = 2, npN - 1\n         j = dspline_isw(i)\n\n         if ( ( i .lt. para2nd ) .or. ( ( para1st - 1 ) * para2nd + 1 .lt. i ) ) then\n            select2(i,1) = dabs( x(j-3) - 2 * x(j) + x(j+3) )\n");
			fprintf(fp,
				"         else if ( ( ( MOD(i,para2nd) .eq. 0 ) .and. ( para2nd .lt. i ) ) .or. &\n          ( ( MOD(i-1,para2nd) .eq. 0 ) .and. ( i .lt. ( para1st - 1 ) * para2nd ) ) ) then\n            select2(i,1) = dabs( x(j-k) - 2 * x(j) + x(j+k) )\n");
			fprintf(fp,
				"         else if ( ( i .ne. para2nd ) .and. ( i .ne. ( para1st - 1 ) * para2nd + 1 ) ) then\n            select2(i,1) = dabs( x(j-3) + x(j-k) - 4 * x(j) + x(j+k) + x(j+3) )\n         endif\n      end do\n\n");

			// select2ソート部分
			fprintf(fp,
				"         do i = 1, npN - 1\n            do j = npN, i + 1, -1\n               if ( select2(j-1,1) .lt. select2(j,1) ) then\n                  temp = select2(j,1)\n                  temp2 = select2(j,2)\n                  select2(j,1) = select2(j-1,1)\n                  select2(j,2) = select2(j-1,2)\n                  select2(j-1,1) = temp\n                  select2(j-1,2) = temp2\n               endif\n            end do\n         end do\n\n");

			// 推定したベストPがすでに標本点にあるなら選択基準2,ないなら選択基準1　選択する部分
			fprintf(fp,
				"         do i = 1, npN\n            if ( bestP .eq. h(i) ) then\n               do j = 1, npN\n                  do k = 1, npN\n                     if ( select2(j,2) .eq. h(k) ) then\n                        exit\n                     endif\n                     if ( k + 1 .eq. npN + 1 ) then\n                        nextP = select2(j,2)\n                        goto 222\n                     endif\n                  end do\n               end do\n\n               exit\n            else\n               nextP = bestP\n            endif\n         end do\n\n222      print '( \" nextP = \", I0, \" bestP = \", I0, / )', nextP, bestP\n\n");

			// 前回推定したパラメタと同じならカウント+1,違うならカウントを1に初期化する部分
			fprintf(fp,
				"         if ( prebestP .eq. bestP ) then\n            count = count + 1\n         else\n            count = 1\n         endif\n\n");

			// 今回推定したベストパラメタをprebestPに設定
			fprintf(fp, "         prebestP = bestP\n\n");

			// 推定値をエクセルに書き込みする部分
			fprintf(fp,
				"      do i = 1, nn\n         write( 11, * ) x(i)\n      end do\n      write( 11, '(A3,/)') 'end'\n\n");

			fprintf(fp, "         kk = kk + 1\n\n");

			fprintf(fp, "         print *, 'count = ', count\n");

			fprintf(fp,
				"         if ( count .eq. 3 ) then\n            print *, 'count end'\n            print *, 'usedParaNums = ', kk - 1\n            exit\n         endif\n\n");

			fprintf(fp, "      end do\n\n");
			// --------4点後のdo文ここまで--------

			// 戻り値の設定
			fprintf(fp, "      dsp2giv%s%s = bestP\n\n", TuneGroupName.c_str(),
				Name.c_str());

			fprintf(fp, "end function dsp2giv%s%s\n\n\n",
				TuneGroupName.c_str(), Name.c_str());
		}
		// 2次元用d-Spline（インストール時自動チューニング用）ここまで

		// 2次元用d-spline（実行時自動チューニング用）
		if (TuneGroup == tgDynamic) {
			int CaseNums[2];
			CaseNums[0] = (int)((variedToValue[0] - variedFromValue[0])
				/ variedStepValue[0] + 1);
			CaseNums[1] = (int)((variedToValue[1] - variedFromValue[1])
				/ variedStepValue[1] + 1);
			int nnn = (3 * CaseNums[0] - 2) * (3 * CaseNums[1] - 2);

			// 関数名部分
			fprintf(fp,
				"subroutine dynamicDspline2%s%s( npN, para1st, para2nd, isw, Dswitch, time )\n", TuneGroupName.c_str(), Name.c_str());

			// 仮引数宣言部分
			fprintf(fp, "      integer npN, para1st, para2nd, isw, Dswitch\n");
			fprintf(fp, "      double precision time\n\n");

			// 変数宣言部分
			fprintf(fp, "      integer :: DEBUG = 1\n");
			fprintf(fp, "      integer p3_1st, p3_2nd, p3_2nd_2\n");
			fprintf(fp, "      integer nn\n");
			fprintf(fp, "      double precision, save :: DE(%d+1,%d+1)\n", nnn,
				nnn);
			fprintf(fp, "      double precision, save :: R(%d+1,%d+1)\n", nnn,
				nnn);
			fprintf(fp, "      double precision radius, cosine, sine\n");
			fprintf(fp, "      integer p, q, q2\n");
			fprintf(fp, "      integer, save :: count1 = 1\n");
			fprintf(fp, "      integer, save :: count2 = 0\n");
			fprintf(fp,
				"      double precision, allocatable, dimension(:) :: x\n");
			fprintf(fp, "      double precision temp\n");
			fprintf(fp, "      integer temp2\n");
			fprintf(fp, "      integer bestP\n");
			fprintf(fp, "      integer nextP\n");
			fprintf(fp, "      integer, save :: prebestP = 0\n");
			fprintf(fp, "      double precision select2(npN,2)\n");
			fprintf(fp, "      integer, save :: h(%d+1)\n",
				CaseNums[0] * CaseNums[1]);
			fprintf(fp, "      integer, save :: dspline_isw(%d)\n",
				CaseNums[0] * CaseNums[1]);
			fprintf(fp, "      integer, save :: kk = 1\n");
			fprintf(fp, "      double precision, parameter :: alfa = 0.1\n");
			fprintf(fp, "      integer, save :: Fcount = 0\n");
			fprintf(fp, "      integer i, j, k\n\n");

			fprintf(fp, "      p3_1st = para1st * 3\n");
			fprintf(fp, "      p3_2nd = para2nd * 3\n");
			fprintf(fp, "      p3_2nd_2 = p3_2nd - 2\n");
			fprintf(fp, "      nn = ( p3_1st - 2 ) * ( p3_2nd - 2 )\n");
			fprintf(fp, "      allocate(x(nn))\n");
			fprintf(fp, "      bestP = 0\n");

			// エクセル書き込み用部分
			fprintf(fp, "      open( 11, file = 'd-SplineData.csv' )\n");

			// パラメタ探索後の終了部分
			fprintf(fp,
				"      if ( count2 .eq. 1 ) then\n         return\n      endif\n\n");
			fprintf(fp,
				"      if ( count1 .eq. 3 ) then\n         print *, 'count1=3 end'\n         print *, 'used paraNums=', Fcount-1\n         isw = prebestP\n         count2 = 1\n         return\n      endif\n\n");

			// h初期化部分
			fprintf(fp,
				"      if ( Fcount .eq. 0 ) then\n         do i = 1, npN + 1\n            h(i) = npN + 1\n         end do\n      endif\n\n");
			fprintf(fp, "      p = nn + 1\n\n");

			// Dswitch1部分ここから
			fprintf(fp, "      if ( Dswitch .eq. 1 ) then\n\n");

			// switch部分ここから
			fprintf(fp, "         select case (Fcount)\n");
			fprintf(fp,
				"            case (0)\n               isw = 1\n            case (1)\n               isw = ( 2 + para2nd ) / 3\n            case (2)\n               isw = ( 1 + 2 * para2nd ) / 3\n            case (3)\n               isw = para2nd\n            case (4)\n               isw = ( ( 2 + para1st ) / 3 - 1 ) * para2nd + 1\n            case (5)\n               isw = ( ( 2 + para1st ) / 3 - 1 ) * para2nd + ( 2 + para2nd ) / 3\n            case (6)\n               isw = ( ( 2 + para1st ) / 3 - 1 ) * para2nd + ( 1 + 2 * para2nd ) / 3\n            case (7)\n               isw = ( ( 2 + para1st ) / 3 - 1 ) * para2nd + para2nd\n            case (8)\n               isw = ( ( 1 + 2 * para1st ) / 3 - 1 ) * para2nd + 1\n            case (9)\n               isw = ( ( 1 + 2 * para1st ) / 3 - 1 ) * para2nd + ( 2 + para2nd ) / 3\n            case (10)\n               isw = ( ( 1 + 2 * para1st ) / 3 - 1 ) * para2nd + ( 1 + 2 * para2nd ) / 3\n            case (11)\n               isw = ( ( 1 + 2 * para1st ) / 3 - 1 ) * para2nd + para2nd\n            case (12)\n               isw = ( para1st - 1 ) * para2nd + 1\n            case (13)\n               isw = ( para1st - 1 ) * para2nd + ( 2 + para2nd ) / 3\n            case (14)\n               isw = ( para1st - 1 ) * para2nd + ( 1 + 2 * para2nd ) / 3\n            case (15)\n               isw = ( para1st - 1 ) * para2nd + para2nd\n");

			// default部分
			fprintf(fp, "            case default\n\n");

			// 推定値初期化部分
			fprintf(fp,
				"               do i = 1, nn\n                  x(i) = DE(i,nn+1)\n               end do\n\n");

			// 後退代入計算部分
			fprintf(fp,
				"               do i = nn, 1, -1\n                  do j = i + 1, nn\n                     x(i) = x(i) - DE(i,j) * x(j)\n                  end do\n\n                  x(i) = x(i) / DE(i,i)\n               end do\n\n");

			// デバッグ推定値表示部分
			fprintf(fp,
				"               if ( DEBUG .gt. 0 ) then\n                  print '( /, \"estimation\" )'\n\n                  do i = 1, npN\n                     print *, '[', i, '] ', x(dspline_isw(i))\n                  end do\n\n                  print *, ''\n               endif\n\n");

			// 最小推定値探索部分
			fprintf(fp,
				"               temp = x(dspline_isw(1))\n               bestP = 1\n\n               do i = 1, npN\n                  j = dspline_isw(i)\n\n                  if ( x(j) .lt. temp ) then\n                     temp = x(j)\n                     bestP = i\n                  endif\n               end do\n\n");

			// ベストパラメタ部分
			fprintf(fp,
				"               print *, 'best para = ', bestP, ' time = ', x(dspline_isw(bestP))\n\n");

			// 選択基準2初期化部分
			fprintf(fp,
				"               do i = 1, npN\n                  select2(i,2) = i\n                  select2(i,1) = 0\n               end do\n\n");

			// 変化率計算部分
			fprintf(fp, "               k = p3_2nd_2 * 3\n\n");
			fprintf(fp,
				"               do i = 2, npN - 1\n                  j = dspline_isw(i)\n\n                  if ( ( i .lt. para2nd ) .or. ( ( para1st - 1 ) * para2nd + 1 .lt. i ) ) then\n                     select2(i,1) = dabs( x(j-3) - 2 * x(j) + x(j+3) )\n                  else if ( ( ( MOD(i,para2nd) .eq. 0 ) .and. ( para2nd .lt. i ) ) .or. &\n                   ( ( MOD(i-1,para2nd) .eq. 0 ) .and. ( i .lt. ( para1st - 1 ) * para2nd ) ) ) then\n                     select2(i,1) = dabs( x(j-k) - 2 * x(j) + x(j+k) )\n                  else if ( ( i .ne. para2nd ) .and. ( i .ne. ( para1st - 1 ) * para2nd + 1 ) ) then\n                     select2(i,1) = dabs( x(j-3) + x(j-k) - 4 * x(j) + x(j+k) + x(j+3) )\n                  endif\n               end do\n\n");

			// select2ソート部分
			fprintf(fp,
				"               do i = 1, npN - 1\n                  do j = npN, i + 1, -1\n                     if ( select2(j-1,1) .lt. select2(j,1) ) then\n                        temp = select2(j,1)\n                        temp2 = select2(j,2)\n                        select2(j,1) = select2(j-1,1)\n                        select2(j,2) = select2(j-1,2)\n                        select2(j-1,1) = temp\n                        select2(j-1,2) = temp2\n                     endif\n                  end do\n               end do\n\n");

			// 選択基準1or2選択部分
			fprintf(fp,
				"               do i = 1, npN\n                  if ( bestP .eq. h(i) ) then\n                     do j = 1, npN\n                        do k = 1, npN\n                           if ( select2(j,2) .eq. h(k) ) then\n                              exit\n                           endif\n                           if ( k + 1 .eq. npN + 1 ) then\n                              nextP = select2(j,2)\n                              goto 111\n                           endif\n                        end do\n                     end do\n\n                     exit\n                  else\n                     nextP = bestP\n                  endif\n               end do\n\n");

			// ベストパラメタと次のパラメタをセットする部分
			fprintf(fp,
				"111               print '( \" nextP = \", I0, \" bestP = \", I0, / )', nextP, bestP\n\n               isw = nextP\n\n");

			// 3回カウント用部分
			fprintf(fp,
				"               if ( prebestP .eq. bestP ) then\n                  count1 = count1 + 1\n               else\n                  count1 = 1\n               endif\n\n");

			// default部分ここまで
			fprintf(fp, "               prebestP = bestP\n\n");

			// 推定値をエクセルに書き込みする部分
			fprintf(fp,
				"               do i = 1, nn\n                  write( 11, * ) x(i)\n               end do\n");
			fprintf(fp, "               write( 11, '(A3,/)' ) 'end'\n\n");

			// switchここまで
			fprintf(fp,
				"         end select\n\n         Fcount = Fcount + 1\n\n");

			// Dswitch1ここまで------------
			fprintf(fp, "      endif\n\n");

			// Dswitch2ここから-----------------
			fprintf(fp, "      if ( Dswitch .eq. 2 ) then\n\n");

			// DE初期化部分
			fprintf(fp, "         if ( Fcount .eq. 1 ) then\n");
			fprintf(fp,
				"            do i = 1, nn + 1\n               do j = 1, nn + 1\n                  DE(i,j) = 0\n                  R(i,j) = 0\n               end do\n            end do\n\n");
			fprintf(fp,
				"            j=1\n\n            do i = 1, nn - 4\n               if ( i .le. p3_2nd_2 - 2 ) then\n                  DE(i,i) = 1 * alfa\n                  DE(i,i+1) = -2 * alfa\n                  DE(i,i+2) = 1 * alfa\n               else if ( ( nn - p3_2nd ) .lt. i ) then\n                  DE(i,i+2) = 1 * alfa\n                  DE(i,i+3) = -2 * alfa\n                  DE(i,i+4) = 1 * alfa\n               else\n                  if ( ( MOD(i+1,p3_2nd_2) .eq. 0 ) .or. ( MOD(i+2,p3_2nd_2) .eq. 0 ) ) then\n                     DE(i,j) = 1 * alfa\n                     DE(i,j+p3_2nd_2) = -2 * alfa\n                     DE(i,j+2*p3_2nd_2) = 1 * alfa\n                  else\n                     DE(i,j) = 1 * alfa\n                     DE(i,j+p3_2nd_2-1) = 1 * alfa\n                     DE(i,j+p3_2nd_2) = -4 * alfa\n                     DE(i,j+p3_2nd_2+1) = 1 * alfa\n                     DE(i,j+2*p3_2nd_2) = 1 * alfa\n                  endif\n\n                  j = j + 1\n               endif\n            end do\n\n");

			// パラメタの取りうる値が，d-Spline関数中ではどの点に対応するかを設定
			fprintf(fp,
				"            do i = 1, npN\n               dspline_isw(i) = ( 3 * i - 2 ) + int( ( i - 1 ) / para2nd ) * 2 * ( p3_2nd - 3 )\n            end do\n\n");

			/* DEをあらかじめ上三角行列に整形 */
			fprintf(fp,
				"            q = 0\n\n            do i = ( p3_2nd_2 - 2 ) + 1, nn - p3_2nd\n               do j = 1, p3_2nd_2 - 2\n");

			// G用計算部分
			fprintf(fp,
				"                  radius = dsqrt( ( DE(q+j,q+j) ) * ( DE(q+j,q+j) ) + ( DE(i,q+j) ) * ( DE(i,q+j) ) )\n\n                  if ( radius .eq. 0 ) then\n                     cycle\n                  end if\n\n                  cosine = DE(q+j,q+j) / radius\n                  sine = DE(i,q+j) / radius\n\n");

			// R=回転行列*DEギブンス変換部分
			fprintf(fp,
				"                  do k = j, 2 * p3_2nd_2 + 1\n                     if ( k .eq. nn ) then\n                        exit\n                     end if\n\n                     R(q+j,q+k) = R(q+j,q+k) + cosine * DE(q+j,q+k)\n                     R(q+j,q+k) = R(q+j,q+k) + sine * DE(i,q+k)\n\n                     R(i,q+k) = R(i,q+k) + (-1) * sine * DE(q+j,q+k)\n                     R(i,q+k) = R(i,q+k) + cosine * DE(i,q+k)\n\n                     DE(q+j,q+k) = R(q+j,q+k)\n                     DE(i,q+k) = R(i,q+k)\n\n                     R(q+j,q+k) = 0\n                     R(i,q+k) = 0\n");

			fprintf(fp,
				"                  end do\n               end do\n               q = q + 1\n            end do\n         end if\n\n");
			/* 整形終了 */

			// 標本点とその実測値のセット部分
			fprintf(fp,
				"         q = isw\n         q2 = dspline_isw(q)\n         h(kk) = q\n\n");
			fprintf(fp,
				"         DE(p,q2) = 1\n         DE(p,nn+1) = time\n\n");

			// ギブンス変換ループ部分------------------

			fprintf(fp, "         do q2 = q2, nn\n\n");

			// G用計算部分
			fprintf(fp,
				"            radius = dsqrt( ( DE(q2,q2) ) * ( DE(q2,q2) ) + ( DE(p,q2) ) * ( DE(p,q2) ) )\n\n");
			fprintf(fp,
				"            if ( radius .eq. 0 ) then\n               cycle\n            endif\n\n");
			fprintf(fp,
				"            cosine = DE(q2,q2) / radius\n            sine = DE(p,q2) / radius\n\n");

			// R=回転行列*DEギブンス変換部分
			fprintf(fp,
				"            do i = q2, q2 + 2 * p3_2nd_2 + 1\n               if ( i .eq. p + 1 ) then\n                  exit\n               end if\n\n");
			fprintf(fp,
				"               R(q2,i) = R(q2,i) + cosine * DE(q2,i)\n               R(q2,i) = R(q2,i) + sine * DE(p,i)\n\n               R(p,i) = R(p,i) + (-1) * sine * DE(q2,i)\n               R(p,i) = R(p,i) + cosine * DE(p,i)\n\n               DE(q2,i) = R(q2,i)\n               DE(p,i) = R(p,i)\n\n               R(q2,i) = 0\n               R(p,i) = 0\n\n");
			fprintf(fp,
				"               if ( i .eq. q2 + 2 * p3_2nd_2 + 1 ) then\n                  R(q2,p) = R(q2,p) + cosine * DE(q2,p)\n                  R(q2,p) = R(q2,p) + sine * DE(p,p)\n\n                  R(p,p) = R(p,p) + (-1) * sine * DE(q2,p)\n                  R(p,p) = R(p,p) + cosine * DE(p,p)\n\n                  DE(q2,p) = R(q2,p)\n                  DE(p,p) = R(p,p)\n\n                  R(q2,p) = 0\n                  R(p,p) = 0\n               end if\n");

			fprintf(fp, "            end do\n");

			// ギブンス変換ループここまで

			fprintf(fp, "         end do\n\n         kk = kk + 1\n");

			// Dswitch2ここまで
			fprintf(fp, "      end if\n\n");

			// 関数ここまで
			fprintf(fp,
				"      return\n\nend subroutine dynamicDspline2%s%s\n\n", TuneGroupName.c_str(), Name.c_str());
		}
		// 2次元用d-Spline（実行時自動チューニング用）ここまで
	}
	// 2次元用d-Spline　ここまで
	//
	// Fortran90対応のd-spline用追加部分---------------------------------------------------------------------
	// ここまで
	//
	/** **************************************************************************************************** */

}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
//
// 3.概要
// 時間計測を行うための ATexec サブルーチンのコードの生成を行う。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputAutoExecCode_C(FILE *fp, FILE *fpOutHeader) {
	int i, j, k;
	string s;
	TValData *ValData;
	TToken *Token;
	int CaseArrayCount;
	int BaseValIdx;
	string ValName;
	string BaseValName;
	string iBsetSw1Str;
	TStringList *DefValNameList;
	string ArgStr;

	bool VisualF = MainF->VisualF;

	DefValNameList = new TStringList;
	BaseValName = BaseValList->Strings[0];
	CaseArrayCount = CaseCount;
	if (FittingType != 0) {
		CaseArrayCount = SampledList->Count;
	}
	fprintf(fp,
		"//==============================================================\n");
	fprintf(fp, "//==== %s Optimization Routines\n", TuneGroupName.c_str());
	fprintf(fp,
		"//==============================================================\n");
	if (TuneGroup != tgDynamic) {
		if (MainF->Call_ATExec_Script == NULL) {
			TScript *Script = new TScript(MainF->TokenList, 0, NULL,
				ValDataList);
			ArgStr = Script->GetATExecArgStr(true, "", "");
			fprintf(fp, "int OAT_ATexec%s%s(char *OAT_Routines%s)\n",
				TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());
			fprintf(fpOutHeader, "int OAT_ATexec%s%s(char *OAT_Routines%s);\n",
				TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());
			delete Script;
			// fprintf(fp,"OAT_ATexec%s%s(char *OAT_Routines)\n",TuneGroupName.c_str(),Name.c_str());
		}
		else {
			ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true, "", "");
			fprintf(fp, "int OAT_ATexec%s%s(char *OAT_Routines%s)\n",
				TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());
			fprintf(fpOutHeader, "int OAT_ATexec%s%s(char *OAT_Routines%s);\n",
				TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());
		}
		iBsetSw1Str = "iBestSw1";
	}
	else {
		if (MainF->Call_ATExec_Script == NULL) {
			TScript *Script = new TScript(MainF->TokenList, 0, NULL,
				ValDataList);
			ArgStr = Script->GetATExecArgStr(true, "", "");
			/*
			fprintf(fp,"int OAT_ATexec%s%s(char *OAT_Routines,int %s,int *iBestSw1)\n",
			TuneGroupName.c_str(),Name.c_str(),BaseValName.c_str());
			fprintf(fpOutHeader,"int OAT_ATexec%s%s(char *OAT_Routines,int %s,int *iBestSw1);\n",
			TuneGroupName.c_str(),Name.c_str(),BaseValName.c_str());
			 */
			fprintf(fp,
				"int OAT_ATexec%s%s(char *OAT_Routines,int *iBestSw1%s)\n",
				TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());
			fprintf(fpOutHeader,
				"int OAT_ATexec%s%s(char *OAT_Routines,int *iBestSw1%s);\n",
				TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());
			delete Script;
		}
		else {
			ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true, "", "");
			/*
			fprintf(fp,"int OAT_ATexec%s%s(char *OAT_Routines,int %s,int *iBestSw1%s)\n",
			TuneGroupName.c_str(),Name.c_str(),BaseValName.c_str(),ArgStr.c_str());
			fprintf(fpOutHeader,"int OAT_ATexec%s%s(char *OAT_Routines,int %s,int *iBestSw1%s);\n",
			TuneGroupName.c_str(),Name.c_str(),BaseValName.c_str(),ArgStr.c_str());
			 */
			fprintf(fp,
				"int OAT_ATexec%s%s(char *OAT_Routines,int *iBestSw1%s)\n",
				TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());
			fprintf(fpOutHeader,
				"int OAT_ATexec%s%s(char *OAT_Routines,int *iBestSw1%s);\n",
				TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());

		}
		iBsetSw1Str = "*iBestSw1";
	}

	fprintf(fp, "{\n");

	//
	// 使用される変数の定義を行う。call OAT_ATexe()で明示された引数は
	// 対象外となる。
	//
	s = "";
	for (i = 0; i < ArgValList->Count; i++) {
		/*
		if(MainF->Call_ATExec_Script != NULL){
		ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false,"","");
		if(ArgStr != ""){
		break;	// ATExecでの引数指定がある場合は定義済として扱う。
		}
		}
		 */
		if (ArgValList->Strings[i] == "") {
			continue;
		}
		ValData = (TValData*)ArgValList->Objects[i];
		ValName = ValData->Str;
		for (BaseValIdx = 0; BaseValIdx < BaseValList->Count; BaseValIdx++) {
			if (BaseValList->Strings[BaseValIdx] == ValName) {
				break;
			}
		}
		if ((BaseValIdx < BaseValList->Count) && (BaseValIdx != 0)) {
			continue;
		}
		if (TuneGroup == tgDynamic) {
			if (BaseValIdx < BaseValList->Count) {
				continue;
			}
		}
		if (IsATExecArg(ValData)) {
			continue; // ATExec() の引数
		}
		s = "\t" + ValData->GetDefStr_C() + ValData->Str;
		// s += " " + ValData->Str;
		DefValNameList->Add(ValData->Str);
		for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
			Token = (TToken*)TokenList->Items[j];
			// Change for N or n 2007/1/19
			ValName = Token->OrgStr;
			if ((Token->TokId == -1) || (Token->TokId == tid_Val)) {
				for (BaseValIdx = 0; BaseValIdx < BaseValList->Count;
					BaseValIdx++) {
					if (BaseValList->Strings[BaseValIdx] == ValName) {
						break;
					}
				}
				/* 引数渡しになｔったので、基本パラメータの名前変更は、不要のはず。
				if(BaseValIdx < BaseValList->Count){
				ValName = BaseValList->Strings[0]+BaseValList->Strings[0]; // Exp. N->NN
				}
				 */
			}
			s += ValName;
		}
		if (s != "") {
			fprintf(fp, "%s;\n", s.c_str()); // １行ごとに出力
		}
		// fprintf(fp,"%s;\n",s.c_str());
	}
	if (FittingType != 0) {
		fprintf(fp, "//     !!!!!! fitting用\n");
		fprintf(fp, "//     === for estimation using Least Square Method\n");
		fprintf(fp, "//        ===  for sumipling data\n");
		fprintf(fp, "\tdouble xDim[OATLSM_MAX_N][OATLSM_MAX_NPARM];\n");
		fprintf(fp, "\tdouble yEst[OATLSM_MAX_N][OATLSM_MAX_NPARM];\n");
		fprintf(fp, "\tdouble x[OATLSM_MAX_N];\n");
		fprintf(fp, "\tdouble y[OATLSM_MAX_N];\n");
		fprintf(fp, "//        === for target coefficients\n");
		fprintf(fp, "\tdouble a_lsm[OATLSM_MAX_M+1];\n");
		fprintf(fp, "\tdouble aa_lsm[OATLSM_MAX_M+1][OATLSM_MAX_NPARM];\n");
		fprintf(fp, "\tint nparm,n_lsm,nsamp,m_lsm,isamp_indx;\n");
		fprintf(fp, "\tint isw,iii,idummy;\n");
		fprintf(fp, "\tFILE *fp10;\n");

		fprintf(fp, "//     !!!!!! fitting用終り\n");
	}
	fprintf(fp, "\tint iusw1;\n");
	fprintf(fp, "\tint F1[%d];\n", CaseArrayCount);
	fprintf(fp, "\tFILE *fp11;\n");
	fprintf(fp, "\tFILE *fp12;\n");
	fprintf(fp, "\tFILE *fp13;\n");
	fprintf(fp, "\tint iloop_n,iloop_%s;\n", LowerCase(TuneGroupName).c_str());
	/*
	//
	//	BaseValNameの定義を追加
	//	引数として渡す形なので不要。
	//
	if(TuneGroup != tgDynamic){
	for(i = 0 ; i < BaseValList->Count ; i++){
	ValName = BaseValList->Strings[i];
	ValName = LowerCase(ValName);	// 小文字に変換
	if(DefValNameList->IndexOf(ValName) != -1){ // すでに定義済み
	continue;
	}
	DefValNameList->Add(ValName);
	for(k = 0 ; k < ValDataList->Count ; k++){
	ValData = (TValData *)ValDataList->Items[k];
	if(ValData == NULL){
	continue;
	}
	if(ValName == ValData->Str){	// 変数で使用ありの場合のみ
	fprintf(fp,"\tint %s;\n",ValName.c_str());
	break;
	}
	}
	}
	}
	 */
	fprintf(fp, "\n");
	if (TuneGroup != tgDynamic) {
		fprintf(fp, "\tint iBestSw1;\n");
	}
	fprintf(fp, "\n");
	fprintf(fp, "\tdouble t1, t2, t_all, bt;\n");
	fprintf(fp, "\tdouble dBestTime1;\n");
	fprintf(fp, "\n");
	if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		// この定義がないと do While等での型チェックで問題発生していた
		fprintf(fp, "\tint OAT_Eecntl_NextIndex:\n");
		fprintf(fp, "\tint OAT_Eecntl_Continue;\n");
	}

	if (VisualF) { // Visualize = ON
		// 格納用の ExecState() と読込み用の cbufを確保する。
		fprintf(fp, "\tint inum,iloop_vi,LoopCount;\n");
		fprintf(fp, "\tint ExecState[%d];\n", MainF->TuneRegionList->Count);
		fprintf(fp, "\tchar cbuf[100];\n");
		fprintf(fp, "\tdouble t_all_sum;\n");
	}
#if 0
	//
	// FTN95で値が入っていない場合にエラーになったので値のセットを追加 2009/03/10
	// 本来は PrePro指定等で行う方が正解と思われるが省略時でも動作可能とするために
	// 追加した。パラメータ指定の変数は除く
	//
	// Cの場合は不要なのでCurt、#include <xxx.h> 等で定義されている場合が多い。2011/09/08
	//
	fprintf(fp, "\n");
	for (i = 0; i < ArgValList->Count; i++) {
		if (ArgValList->Strings[i] == "") {
			continue;
		}
		ValData = (TValData*)ArgValList->Objects[i];
		ValName = ValData->Str;
		//
		// パラメータ文として追加済みと同名の変数は対象外とする。（名前で比較）
		// N->NN の場合の Nは置き換えを行っているため対象外となる。
		//
		for (ParamValIdx = 0; ParamValIdx < ParamValList->Count; ParamValIdx++)
		{
			if (CompareText(ParamValList->Strings[ParamValIdx], ValName) == 0) {
				break;
			}
		}
		if (ParamValIdx < ParamValList->Count) {
			continue;
		}
		for (BaseValIdx = 0; BaseValIdx < BaseValList->Count; BaseValIdx++) {
			if (CompareText(BaseValList->Strings[BaseValIdx], ValName) == 0) {
				break;
			}
		}
		if (BaseValIdx < BaseValList->Count) {
			continue;
		}
		// fprintf(fp,"\t%s = 1;\n",ValName);
		if (ValData->ArrayLevel == 0) {
			fprintf(fp, "\t%s = 0;\n", ValName.c_str());
		}
		else {
			fprintf(fp, "\tmemset(%s,sizeof(%s),0);\n", ValName.c_str(),
				ValName.c_str());
		}
	}
#endif
	fprintf(fp, "\n");
	fprintf(fp, "//---- file create\n");
	fprintf(fp, "//-----------------------------------------\n");
	fprintf(fp, "\tif(myid == 0){\n");
	fprintf(fp, "\t\tfp11 = fopen(\"OAT_%s%sParam.dat\",\"wt\");\n",
		TuneGroupName.c_str(), Name.c_str());
	fprintf(fp, "\t\tfprintf(fp11,\"(%s\\n\");\n", Name.c_str());
	if (TuneGroup != tgDynamic) {
		fprintf(fp,
			"\t\tfprintf(fp11,\"  (OAT_NUMPROCS %%d)\\n\",OAT_NUMPROCS);\n");
		fprintf(fp,
			"\t\tfprintf(fp11,\"  (OAT_SAMPDIST %%d)\\n\",OAT_SAMPDIST);\n");
	}
	if (VisualF) { // Visualize = ON
		// 格納用の ExecState() の現在の値を読み込む。
		fprintf(fp, "\n");

		if (TuneGroup != tgDynamic) {
			fprintf(fp, "\t\tLoopCount = OAT_ENDTUNESIZE-OAT_STARTTUNESIZE;\n");
			fprintf(fp, "\t\tLoopCount = LoopCount+OAT_SAMPDIST;\n");
			fprintf(fp, "\t\tLoopCount = LoopCount/OAT_SAMPDIST;\n");
		}
		else {
			fprintf(fp, "\t\tLoopCount = 1;\n");
		}
		fprintf(fp, "\n");
		fprintf(fp, "\t\tfp12 = fopen(\"OATATLog.dat\",\"rt\");\n");
		fprintf(fp, "\t\tif(fp12 != NULL){\n");
		fprintf(fp, "\t\t\tfor(iloop_vi = 0 ; iloop_vi < %d ; iloop_vi++){\n",
			MainF->TuneRegionList->Count);
		fprintf(fp, "\t\t\t\tfgets(cbuf,sizeof(cbuf),fp12);\n");
		fprintf(fp, "\t\t\t\tinum = atoi(cbuf);\n");
		fprintf(fp, "\t\t\t\tExecState[iloop_vi] = inum;\n");
		fprintf(fp, "\t\t\t}\n");
		fprintf(fp, "\t\t\tfclose(fp12);\n");
		fprintf(fp, "\t\t}else{\n");
		fprintf(fp, "\t\t\tfor(iloop_vi = 0 ; iloop_vi < %d ; iloop_vi++){\n",
			MainF->TuneRegionList->Count);
		fprintf(fp, "\t\t\t\tExecState[iloop_vi] = 0;\n");
		fprintf(fp, "\t\t\t}\n");
		fprintf(fp, "\t\t}\n");
		fprintf(fp, "\n");

		// 結果Logの書き出し用ファイルをOpen
		fprintf(fp, "\t\tfp13 = fopen(\"OATATLog_%s.dat\",\"wt\");\n",
			Name.c_str());
		fprintf(fp, "\t\tfprintf(fp13,\"%%d NUMPROCS\\n\",OAT_NUMPROCS);\n");
		if (TuneGroup != tgDynamic) {
			fprintf(fp,
				"\t\tfprintf(fp13,\"%%d OAT_STARTTUNESIZE\\n\",OAT_STARTTUNESIZE);\n");
			fprintf(fp,
				"\t\tfprintf(fp13,\"%%d OAT_ENDTUNESIZE\\n\",OAT_ENDTUNESIZE);\n");
			fprintf(fp,
				"\t\tfprintf(fp13,\"%%d OAT_SAMPDIST\\n\",OAT_SAMPDIST);\n");
		}
		else {
			fprintf(fp, "\t\tfprintf(fp13,\"%%d OAT_STARTTUNESIZE\\n\",N);\n");
			fprintf(fp, "\t\tfprintf(fp13,\"%%d OAT_ENDTUNESIZE\\n\",N);\n");
			fprintf(fp, "\t\tfprintf(fp13,\"%%d OAT_SAMPDIST\\n\",100);\n");
		}
		fprintf(fp, "\t\tfprintf(fp13,\"%%d EndOfHeader\\n\",-1);\n");
		fprintf(fp, "\n");
	}
	fprintf(fp, "\t}\n");
	fprintf(fp, "//----------------------------------------\n");
	fprintf(fp, "\n");
	fprintf(fp, "//---- Start tune\n");
	fprintf(fp, "//-----------------------------------------\n");
	if (FittingType != 0) {
		for (i = 0; i < SampledList->Count; i++) {
			// fprintf(fp,"      F1[%d]=%d\n",i+1,(int)SampledList->Items[i]);
			fprintf(fp, "\t\tF1[%d]=%d;\n", i,
				(int)(long)SampledList->Items[i]);
		}
		fprintf(fp, "\n");
		fprintf(fp, "//    !!!!!! fitting用変数\n");
		fprintf(fp, "//    !!! variedから推定する最大パラメタ組合せ数\n");
		fprintf(fp, "      nparm = %d;\n", CaseCount);
		fprintf(fp, "\n");
		fprintf(fp, "//    !!! パラメタの変化数 / sampled 指定子から算出\n");
		fprintf(fp, "      n_lsm = %d;\n", SampledList->Count);
		fprintf(fp, "\n");
		fprintf(fp, "//    !!! 行列サイズに関するサンプル点の個数\n");
		fprintf(fp, "      nsamp = 0;\n");
		fprintf(fp,
			"      for(iloop_n=OAT_STARTTUNESIZE;iloop_n<=OAT_ENDTUNESIZE;iloop_n+=OAT_SAMPDIST){\n");
		fprintf(fp, "         nsamp = nsamp + 1;\n");
		fprintf(fp, "      }");
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "//    !!! fitting least squares で指定した次元数\n");
		fprintf(fp, "      m_lsm = %d;\n", FittingDegree);
		fprintf(fp, "\n");
		fprintf(fp, "//    !!! サンプリング点インデックス初期化\n");
		fprintf(fp, "      isamp_indx = 0;\n");
		fprintf(fp, "\n");
		fprintf(fp, "//    print *, \"nparm = \", nparm\n");
		fprintf(fp, "//    print *, \"nsamp = \", nsamp\n");
		fprintf(fp, "//    print *, \"m_lsm = \", m_lsm\n");
		fprintf(fp, "\n");
	}
	else {
		for (i = 1; i <= CaseCount; i++) {
			// fprintf(fp,"\tF1[%d]=%d;\n",i,i);
			fprintf(fp, "\tF1[%d] = %d;\n", i - 1, i);
		}
	}
	fprintf(fp, "\n");
	//
	// キャッシュミスヒット対応のために１回動作させてからの実行を追加 2011/08/02
	//
	fprintf(fp, "\tfor(iloop_%s=0;iloop_%s<%d;iloop_%s++){\n",
		LowerCase(TuneGroupName).c_str(), LowerCase(TuneGroupName).c_str(),
		CaseArrayCount, LowerCase(TuneGroupName).c_str());
	fprintf(fp, "\n");
	fprintf(fp, "\t\tiusw1 = F1[iloop_%s];\n",
		LowerCase(TuneGroupName).c_str());
	fprintf(fp, "\n");
	s = "\t\t" + FuncName + "(";
	for (i = 0; i < ArgValList->Count; i++) {
		if (ArgValList->Strings[i] == "") {
			continue;
		}
		ValData = (TValData*)ArgValList->Objects[i];
		ValName = ValData->Str;
		if ((BaseValList->Count > 0) && (BaseValList->Strings[0] == ValName)) {
			s += "OAT_STARTTUNESIZE,";
		}
		else {
#if 0
			string s2 = ValData->GetDefStr_C();
			for (int j = 0; j < s2.length(); j++) {
				if (s2[j] > ' ') {
					s2.erase(j, s2.length());
					break;
				}
			}
			s += s2 + ValData->Str + ",";
#else
			s += ValData->Str + ",";
#endif
		}
	}
	s += "iusw1);";
	s = SepLongStr(s);
	fprintf(fp, "%s\n", s.c_str());
	fprintf(fp, "\t}\n");
	fprintf(fp, "\n");

	if (VisualF) { // Visualize = ON
		fprintf(fp, "\tt_all_sum = 0;\n"); // １秒以上のチェック
	}
	if (TuneGroup != tgDynamic) {
		fprintf(fp,
			"\tfor(iloop_n=OAT_STARTTUNESIZE;iloop_n<=OAT_ENDTUNESIZE;iloop_n+=OAT_SAMPDIST){\n");
	}
	else {
		fprintf(fp, "\tiloop_n=0;\n");
	}
	fprintf(fp, "\n");
	if (!MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "\t\tfor(iloop_%s=0;iloop_%s<%d;iloop_%s++){\n",
			LowerCase(TuneGroupName).c_str(),
			LowerCase(TuneGroupName).c_str(), CaseArrayCount,
			LowerCase(TuneGroupName).c_str());
		fprintf(fp, "\n");
		fprintf(fp, "\t\t\tiusw1 = F1[iloop_%s];\n",
			LowerCase(TuneGroupName).c_str());
		fprintf(fp, "\n");
	}
	else {
		fprintf(fp, "\t\tOAT_Eecntl_Init(F1,%d);\n", CaseArrayCount);
		fprintf(fp, "\t\t%s = 0;\n", iBsetSw1Str.c_str());
		fprintf(fp, "\t\twhile (1){\n");
		fprintf(fp, "\t\t\tiusw1 = OAT_Eecntl_NextIndex();\n");
		fprintf(fp, "\t\t\tif (iusw1 < 1 ){ break; }\n");
		fprintf(fp, "\n");
	}

	if (TuneGroup != tgDynamic) {
		for (i = 0; i < BaseValList->Count; i++) {
			ValName = BaseValList->Strings[i];
			// ValName = LowerCase(ValName);	// 小文字に変換
			for (k = 0; k < ValDataList->Count; k++) {
				ValData = (TValData*)ValDataList->Items[k];
				if (ValData == NULL) {
					continue;
				}
				if (ValName == ValData->Str) { // 変数で使用ありの場合のみ
					fprintf(fp, "\t\t\t%s = iloop_n;\n", ValName.c_str());
					break;
				}
			}
			// fprintf(fp,"\t\t\t%s = iloop_n;\n",ValName.c_str());
		}
	}
	fprintf(fp, "\n");

	// d-spline用追加部分---------------
	printf("tune %d \n", FittingDspline);
	/** **************************************************************************************************** */
	//
	// Kogakuin Irie
	// 実行時d-Splineとその他のd-Splineの判断方法を変更，および2次元用追加部分
	// region start 文からチューニングタイミングを取得
	//
	// 既存コードはコメントアウト
	//
	// if(FittingDspline == 1){
	// fprintf(fp,"\t\t\tiBestSw1 = dspgiv(%d",CaseArrayCount);
	//
	//
	//
	// if(MainF->Call_ATExec_Script == NULL){
	// TScript *Script = new TScript(MainF->TokenList,0,NULL,ValDataList);
	// string ArgStr = Script->GetATExecArgStr(false,"","");
	// fprintf(fp,"%s);\n",ArgStr.c_str());
	// delete Script;
	// }else{
	// string ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false,"","");
	// fprintf(fp,"%s);\n",ArgStr.c_str());
	// }
	//
	// fprintf(fp,"\t\t\tbreak;\n");
	//
	// }

	// d-spline用追加部分　ここまで---------------

	if (TuneGroup != tgDynamic) {
		if (FittingDspline == 1) {
			fprintf(fp, "\t\t\tiBestSw1 = dspgiv(%d", CaseArrayCount);

			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				string ArgStr = Script->GetATExecArgStr(false, "", "");
				fprintf(fp, "%s);\n", ArgStr.c_str());
				delete Script;
			}
			else {
				string ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr
					(false, "", "");
				fprintf(fp, "%s);\n", ArgStr.c_str());
			}

			fprintf(fp, "\t\t\tbreak;\n");
		}

		if (FittingDspline == 2) {

			int CaseArrayCount2[2];
			CaseArrayCount2[0] = (int)((variedToValue[0] - variedFromValue[0])
				/ variedStepValue[0] + 1);
			CaseArrayCount2[1] = (int)((variedToValue[1] - variedFromValue[1])
				/ variedStepValue[1] + 1);

			fprintf(fp, "\t\t\tiBestSw1 = dsp2giv%s%s(%d,%d,%d",
				TuneGroupName.c_str(), Name.c_str(), CaseArrayCount,
				CaseArrayCount2[0], CaseArrayCount2[1]);

			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				string ArgStr = Script->GetATExecArgStr(false, "", "");
				fprintf(fp, "%s);\n", ArgStr.c_str());
				delete Script;
			}
			else {
				string ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr
					(false, "", "");
				fprintf(fp, "%s);\n", ArgStr.c_str());
			}

			fprintf(fp, "\t\t\tbreak;\n");
		}
	}
	//
	// ここまで
	//
	/** **************************************************************************************************** */

	s = GetPrePostSubregionStr(0, 1);
	if (s != "") {
		fprintf(fp, "%s", s.c_str());
		fprintf(fp, "\n");
	}
	if (MainF->MPIF) {
		fprintf(fp, "\t\t\tMPI_BARRIER(MPI_COMM_WORLD, ierr)\n");
	}
	if (MainF->my_timer_start != "") {
		fprintf(fp, "\t\t\t %s();\n", MainF->my_timer_start.c_str());
	}
	// fprintf(fp,"          t1 = MPI_WTIME()\n");
	fprintf(fp, "\t\t\tt1 = %s();\n", MainF->TimeFunc.c_str());
	fprintf(fp, "\n");

	if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "\t\t\tiloop_inner = 0;\n");
		fprintf(fp, "\t\t\twhile(OAT_Eecntl_Continue()){\n");
		s = "\t\t\t\t" + FuncName + "(";
	}
	else {
		s = "\t\t\t" + FuncName + "(";
	}
	for (i = 0; i < ArgValList->Count; i++) {
		if (ArgValList->Strings[i] == "") {
			continue;
		}
		ValData = (TValData*)ArgValList->Objects[i];
#if 0
		string s2 = ValData->GetDefStr_C();
		for (int j = 0; j < s2.length(); j++) {
			if (s2[j] > ' ') {
				s2.erase(j, s2.length());
				break;
			}
		}
		s += s2 + ValData->Str + ",";
#else
		s += ValData->Str + ",";
#endif
	}
	// Mod 2007/1/19
	// fprintf(fp,"%siusw1)\n",s.c_str());
	s += "iusw1);";
	s = SepLongStr(s);
	fprintf(fp, "%s\n", s.c_str());
	if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "\t\t\t\tiloop_inner = iloop_inner + 1;\n");
		fprintf(fp, "\t\t\t}\n");
	}

	fprintf(fp, "\n");
	if (MainF->MPIF) {
		fprintf(fp, "\t\t\tMPI_BARRIER(MPI_COMM_WORLD,ierr);\n");
	}
	if (MainF->my_timer_stop != "") {
		fprintf(fp, "\t\t\t%s();\n", MainF->my_timer_stop.c_str());
	}
	// fprintf(fp,"          t2 = MPI_WTIME()\n");
	fprintf(fp, "\t\t\tt2 = %s();\n", MainF->TimeFunc.c_str());

	fprintf(fp, "\t\t\tt_all = t2 - t1;\n");
	if (MainF->MPIF) {
		fprintf(fp,
			"\t\t\tMPI_ALLREDUCE(t_all, bt, 1, MPI_DOUBLE_PRECISION,MPI_MAX, MPI_COMM_WORLD, ierr)\n");
	}
	else {
		fprintf(fp, "\t\t\tbt = t_all;\n");
	}
	if (!MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "\t\t\tt_all = bt;\n");
	}
	else {
		fprintf(fp, "\t\t\tt_all = bt / iloop_inner;\n");
		fprintf(fp, "\t\t\tOAT_Eecntl_Repperf(iusw1, t_all);\n");
	}
	fprintf(fp, "\n");

	s = GetPrePostSubregionStr(1, 1);
	if (s != "") {
		fprintf(fp, "%s", s.c_str());
		fprintf(fp, "\n");
	}

	if (VisualF) { // Visualize = ON
		// 0から100(%)までの進行状態の値をセットする。
		fprintf(fp, "\t\t\tif(myid == 0){\n");
		fprintf(fp, "\t\t\t\tt_all_sum = t_all_sum + t_all;\n"); // １秒以上のチェック
		fprintf(fp, "\t\t\t\tif(t_all_sum >= 1.0){\n"); // １秒以上のチェック
		fprintf(fp, "\t\t\t\t\tt_all_sum = 0;\n"); // １秒以上のチェック
		fprintf(fp,
			"\t\t\t\t\tinum = (iloop_n-OAT_STARTTUNESIZE)/OAT_SAMPDIST;\n");
		// fprintf(fp,"\t\t\t\t\tExecState[%d] = (100*inum+",MainF->TuneRegionList->IndexOf(this)+1);
		fprintf(fp, "\t\t\t\t\tExecState[%d] = (100*inum+",
			MainF->TuneRegionList->IndexOf(this));
		fprintf(fp, "100*(iloop_%s-1)/%d)/LoopCount;\n",
			LowerCase(TuneGroupName).c_str(), CaseCount);

		// 格納用の ExecState() の現在の値を書き込む。
		fprintf(fp, "\t\t\t\t\tfp12 = fopen(\"OATATLog.dat\",\"wt\");\n");
		fprintf(fp,
			"\t\t\t\t\tfor(iloop_vi = 0 ; iloop_vi < %d ; iloop_vi++){\n",
			MainF->TuneRegionList->Count);
		fprintf(fp, "\t\t\t\t\t\tfprintf(fp12,\"%%d\",ExecState[iloop_vi]);\n");
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\t\tfclose(fp12);\n");

		fprintf(fp, "\t\t\t\t}\n"); // １秒以上のチェック
		fprintf(fp, "\t\t\t}\n");
	}

	//
	// According 指定がある場合は、計測時間を置き換える。
	// 条件指定の場合（estimatedなし）には、条件が一致すれば time = 0
	// 条件が一致しない場合は、かかった時間（0より大きい値)
	// Accordingと varidの２重の場合には、 範囲で選択となる。
	// 時間変数があると便利？ 例えば、 OAT_TIME ？？？
	//
	if (UseAccordingF) {
		string aStr;
		TSubRegion *SubRegion;
		int cp1, cp2;
		int CaseIdx = 1;

		if (SubRegionList->Count != 0) {
			fprintf(fp, "\t\t\tswitch(iusw1){\n");
			for (i = 0; i < SubRegionList->Count; i++) {
				SubRegion = (TSubRegion*)SubRegionList->Items[i];
				aStr = SubRegion->AccordingStr;
				if (aStr == "") {
					continue;
				}
				if (SubRegion->CaseCount <= 1) {
					fprintf(fp, "\t\t\tcase %d:\n", CaseIdx++);
				}
				else {
					for (j = CaseIdx; j < CaseIdx + SubRegion->CaseCount; j++) {
						fprintf(fp, "\t\t\tcase %d:\n", j);
					}
					CaseIdx += SubRegion->CaseCount;
				}
				cp1 = aStr.find("according") + strlen("according");
				cp2 = aStr.find("estimated");
				if (cp2 != 0) {
					cp1 = cp2 + strlen("estimated");
					aStr = Trim(aStr.substr(cp1 + 1, aStr.length()));
					fprintf(fp, "\t\t\t\tt_all = %s;\n", aStr.c_str());
				}
				else { // 条件式
					aStr = Trim(aStr.substr(cp1 + 1, aStr.length()));
					fprintf(fp, "\t\t\t\tif(%s){\n", aStr.c_str());
					fprintf(fp, "\t\t\t\t\tt_all = 0;\n");
					fprintf(fp, "\t\t\t\t}\n");
				}
				fprintf(fp, "\t\t\t\tbreak;\n");
			}
			fprintf(fp, "\t\t\t}\n");
		}
		else {
			aStr = AccordingStr;
			cp1 = aStr.find("according") + strlen("according");
			cp2 = aStr.find("estimated");
			if (cp2 != 0) {
				cp1 = cp2 + strlen("estimated");
				aStr = Trim(aStr.substr(cp1 + 1, aStr.length()));
				fprintf(fp, "\t\t\t\tt_all = %s;\n", aStr.c_str());
			}
			else { // 条件式
				aStr = Trim(aStr.substr(cp1 + 1, aStr.length()));
				fprintf(fp, "\t\t\t\tif(%s){\n", aStr.c_str());
				fprintf(fp, "\t\t\t\t\tt_all = 0;\n");
				fprintf(fp, "\t\t\t\t}\n");
			}
		}
		fprintf(fp, "\n");
	}

	if (VisualF) { // Visualize = ON
		//
		// ビジュアル化のための結果出力
		// Sampledでとびとびの場合は、空白行を入れる。
		//
		fprintf(fp, "\t\t\tif(myid == 0){\n");
		fprintf(fp, "\t\t\t\tif(iloop_%s == 0){\n",
			LowerCase(TuneGroupName).c_str());
		fprintf(fp,
			"\t\t\t\t\tfor(iloop_vi = 0; iloop_vi < F1[iloop_%s]-1;iloop_vi++){\n"
			, LowerCase(TuneGroupName).c_str());
		fprintf(fp, "\t\t\t\t\t\tfprintf(fp13,\"\\n\");\n");
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\tfprintf(fp13,\"%%f\\n\",t_all);\n");
		fprintf(fp, "\t\t\t\tif(iloop_%s < %d-1){\n",
			LowerCase(TuneGroupName).c_str(), CaseArrayCount);
		fprintf(fp,
			"\t\t\t\t\tfor(iloop_vi = F1[iloop_%s]+1;iloop_vi < F1[iloop_%s+1];iloop_vi++){\n", LowerCase(TuneGroupName).c_str(), LowerCase(TuneGroupName).c_str());
		fprintf(fp, "\t\t\t\t\t\tfprintf(fp13,\"\\n\");\n");
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\t}else{\n");
		fprintf(fp,
			"\t\t\t\t\tfor(iloop_vi = F1[iloop_%s];iloop_vi < %d;iloop_vi++){\n"
			, LowerCase(TuneGroupName).c_str(), CaseCount);
		fprintf(fp, "\t\t\t\t\t\tfprintf(fp13,\"\\n\");\n");
		fprintf(fp, "\t\t\t\t\t}\n");
		fprintf(fp, "\t\t\t\t}\n");
		fprintf(fp, "\t\t\t}\n");
	}
	if (MainF->DebugF) {
		fprintf(fp, "\t\t\tif(OAT_DEBUG >= 1){\n");
		fprintf(fp, "\t\t\t\tif(myid == 0){\n");
		fprintf(fp,
			"\t\t\t\t\tprintf(\"N=%%d iusw1=%%d %%f\\n\",iloop_n, iusw1, t_all);\n");
		fprintf(fp, "\t\t\t\t}\n");
		fprintf(fp, "\t\t\t}\n");
	}
	else {
		fprintf(fp, "//\t\t\tif(OAT_DEBUG >= 1){\n");
		fprintf(fp, "//\t\t\t\tif(myid == 0){\n");
		fprintf(fp,
			"//\t\t\t\t\tprintf(\"N=%%d iusw1=%%d %%f\\n\",iloop_n, iusw1, t_all);\n");
		fprintf(fp, "//\t\t\t\t}\n");
		fprintf(fp, "//\t\t\t}\n");
	}
	fprintf(fp, "\n");
	if (!MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "\t\t\tif (iloop_%s == 0){\n",
			LowerCase(TuneGroupName).c_str());
	}
	else {
		fprintf(fp, "\t\t\tif (%s == 0){\n", iBsetSw1Str.c_str());
	}
	fprintf(fp, "\t\t\t\tdBestTime1 = t_all;\n");
	if (!MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "\t\t\t\t%s = F1[0];\n", iBsetSw1Str.c_str());
	}
	else {
		fprintf(fp, "\t\t\t\t%s = iusw1;\n", iBsetSw1Str.c_str());
	}
	fprintf(fp, "\t\t\t}else{\n");
	fprintf(fp, "\t\t\t\tif(t_all < dBestTime1){\n");
	fprintf(fp, "\t\t\t\t\tdBestTime1 = t_all;\n");
	if (!MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "\t\t\t\t\t%s = F1[iloop_%s];\n", iBsetSw1Str.c_str(),
			LowerCase(TuneGroupName).c_str());
	}
	else {
		fprintf(fp, "\t\t\t\t\t%s = iusw1;\n", iBsetSw1Str.c_str());

	}
	fprintf(fp, "\t\t\t\t}\n");
	fprintf(fp, "\t\t\t}\n");
	fprintf(fp, "\n");
	if (FittingType != 0) {

		fprintf(fp, "//        !!!!!! fitting用変数設定\n");
		fprintf(fp, "          x[iloop_install] = F1[iloop_install];\n");
		fprintf(fp, "          y[iloop_install] = t_all;\n");
		fprintf(fp, "\n");
		fprintf(fp,
			"//        print *, iloop_install-1,x(iloop_install-1),y(iloop_install-1)\n");
		fprintf(fp, "\n");
	}
	fprintf(fp, "\t\t}\n");
	if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
		fprintf(fp, "\t\t\t\tOAT_Eecntl_Fin();\n");
	}
	fprintf(fp, "\n");

	if (MainF->DebugF) {
		fprintf(fp, "\t\tif(OAT_DEBUG >= 1){\n");
		fprintf(fp, "\t\t\tif(myid == 0){\n");
		fprintf(fp, "\t\t\t\tprintf(\"N=%%d BestSw=%%d\\n\",iloop_n,%s);\n",
			iBsetSw1Str.c_str());
		fprintf(fp, "\t\t\t}\n");
		fprintf(fp, "\t\t}\n");
	}
	if (FittingType != 0) {
		fprintf(fp, "//      ------ fitting処理\n");
		fprintf(fp, "\n");
		fprintf(fp, "//     !!!! 以下のF1(%d) と %d は、sampledの個数と、variedの個数から判断\n",
			CaseArrayCount - 1, CaseCount);
		fprintf(fp, "//          -> sampledの点がvariedの全領域を調べているか判断し、\n");
		fprintf(fp, "//             全数判断していれば、最適値を選択\n");
		fprintf(fp, "        if (F1[%d] == %d){", CaseArrayCount - 1,
			CaseCount);
		fprintf(fp, "//         !!! sampled 指定子なし\n");
		fprintf(fp,
			"//         === if all parameters are mesured or communication optimization\n");
		fprintf(fp,
			"//                then this selects the mesured parameter.\n");
		fprintf(fp, "           if (myid == 0){\n");
		fprintf(fp,
			"              printf(\"All parameters are mesured. \\n\");\n");
		fprintf(fp,
			"              printf(\"So, I will select the measured pararameter.\\n\");\n");
		fprintf(fp, "           }\n");
		fprintf(fp, "\n");
		fprintf(fp, "//         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
		fprintf(fp, "//         ! 変更点                         !\n");
		fprintf(fp, "//         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
		fprintf(fp, "//         === using LSM\n");
		fprintf(fp,
			"//         === Parameter Estimated routine for fixing dimansion\n"
			);
		fprintf(fp,
			"//                 and add estimated costs for all range of the parameter\n");
		fprintf(fp, "           OATLSM_Est_ParamFxDim(x, y, n_lsm, m_lsm,\n");
		fprintf(fp, "                a_lsm,  iloop_n, nparm, isamp_indx,\n");
		fprintf(fp, "                xDim, yEst, &idummy);\n");
		fprintf(fp, "          isamp_indx = isamp_indx + 1;\n");
		fprintf(fp, "          if (myid == 0){\n");
		fprintf(fp, "             printf(\"Best Parameter: %%d\\n\", %s);\n",
			iBsetSw1Str.c_str());
		fprintf(fp, "          }\n");
		fprintf(fp, "//         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
		fprintf(fp, "//         ! 変更点の終り                   !\n");
		fprintf(fp, "//         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
		fprintf(fp, "\n");
		fprintf(fp, "        }else{\n");
		fprintf(fp, "\n");
		fprintf(fp, "//         !!! sampled 指定子あり\n");
		fprintf(fp, "//         === using LSM\n");
		fprintf(fp,
			"//         === Parameter Estimated routine for fixing dimansion\n"
			);
		fprintf(fp,
			"//                 and add estimated costs for all range of the parameter\n");
		fprintf(fp, "          OATLSM_Est_ParamFxDim(x, y, n_lsm, m_lsm,\n");
		fprintf(fp, "                a_lsm,  iloop_n, nparm, isamp_indx,\n");
		if (iBsetSw1Str[0] == '*') {
			fprintf(fp, "                xDim, yEst, %s);\n",
				iBsetSw1Str.c_str());
		}
		else {
			fprintf(fp, "                xDim, yEst, &%s);\n",
				iBsetSw1Str.c_str());
		}
		fprintf(fp, "          isamp_indx = isamp_indx + 1;\n");
		fprintf(fp, "          if (myid == 0){\n");
		fprintf(fp,
			"             printf(\"Estimated Best Parameter: %%d\\n\", %s);\n",
			iBsetSw1Str.c_str());
		fprintf(fp, "          }\n");
		fprintf(fp, "        }\n");
		fprintf(fp, "//       -----------------------------------------\n");
		fprintf(fp, "\n");
	}

	fprintf(fp, "//--- file write\n");
	fprintf(fp, "\t\tif(myid == 0){\n");
	fprintf(fp, "\t\t\tfprintf(fp11,\"  (OAT_PROBSIZE %%d\\n\", iloop_n);\n");
	fprintf(fp, "\t\t\tfprintf(fp11,\"     (%s_I %%d)\\n\",%s);\n",
		Name.c_str(), iBsetSw1Str.c_str());
	fprintf(fp, "\t\t\tfprintf(fp11,\"  )\\n\");\n");
	fprintf(fp, "\t\t}\n");
	fprintf(fp, "//-----------------------------------------\n");
	if (TuneGroup != tgDynamic) {
		fprintf(fp, "\t}\n");
	}
	fprintf(fp, "\n");
	fprintf(fp, "//--- file close\n");
	fprintf(fp, "\tif (myid == 0){\n");
	fprintf(fp, "\t\tfprintf(fp11,\")\\n\");\n");
	fprintf(fp, "\t\tfclose(fp11);\n");

	if (VisualF) { // Visualize = ON
		// 0から100(%)までの進行状態の値をセットする。
		fprintf(fp, "\t\tExecState[%d] = 100;\n",
			MainF->TuneRegionList->IndexOf(this));
		// 格納用の ExecState() の現在の値を書き込む。
		fprintf(fp, "\t\tfp12 = fopen(\"OATATLog.dat\",\"wt\");\n");
		fprintf(fp, "\t\tfor(iloop_vi = 0 ; iloop_vi < %d ; iloop_vi++){\n",
			MainF->TuneRegionList->Count);
		// fprintf(fp,"\t\t\tfprintf(fp12,\"%d\\n\",ExecState[iloop_vi]);\n");
		s = "\t\t\tfprintf(fp12,\"%d\\n\",ExecState[iloop_vi]);\n";
		fprintf(fp, "%s", s.c_str());
		fprintf(fp, "\t\t}\n");
		fprintf(fp, "\t\tfclose(fp12);\n");
		//
		// 結果をClose
		//
		fprintf(fp, "\t\tfclose(fp13);\n");
	}

	fprintf(fp, "\t}\n");
	fprintf(fp, "//---------------------------------------------\n");
	fprintf(fp, "\n");

	if (FittingType != 0) {

		fprintf(fp, "//     !!!!!!!! fitting処理\n");
		fprintf(fp, "//     === for LSM to estimate variable dimension\n");
		fprintf(fp, "//       === Input xDim, yEst\n");
		fprintf(fp, "//\n");
		fprintf(fp, "//      if (oat_myid. eq. 0) then\n");
		fprintf(fp,
			"//        print *, \"nsamp=\",nsamp, \"/ nparm=\",nparm\n");
		fprintf(fp, "//        do i=0, nsamp-1\n");
		fprintf(fp,
			"//          write(6, 1919) (xDim(i,isw),isw=0, nparm-1)\n");
		fprintf(fp, "//        enddo\n");
		fprintf(fp, "//        print *, " "\n");
		fprintf(fp, "//        do i=0, nsamp-1\n");
		fprintf(fp,
			"//          write(6, 1919) (yEst(i,isw),isw=0, nparm-1)\n");
		fprintf(fp, "//        enddo\n");
		fprintf(fp, "// 1919   format(' ', 20F10.5)\n");
		fprintf(fp, "//      endif\n");
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "//       === 全組合せ数がデータの総数となる。\n");
		fprintf(fp, "        n_lsm = nparm;\n");
		fprintf(fp, "\n");
		fprintf(fp, "        OATLSM_lsm_DimEst(xDim, yEst,\n");
		fprintf(fp,
			"                    n_lsm, m_lsm, aa_lsm, nparm, nsamp);\n");
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "      for(isw=0;isw < nparm;isw++){\n");
		fprintf(fp, "        for(iii=0 ; iii < nsamp ; iii++){\n");
		fprintf(fp, "           xDim[iii][isw]=0.0;\n");
		fprintf(fp, "        }\n");
		fprintf(fp, "      }\n");
		fprintf(fp, "\n");
		fprintf(fp, "      for(isw=0;isw < nparm;isw++){\n");
		fprintf(fp, "        for(iii=0 ; iii < nsamp ; iii++){\n");
		fprintf(fp, "           yEst[iii][isw]=0.0;\n");
		fprintf(fp, "        }\n");
		fprintf(fp, "      }\n");
		fprintf(fp, "\n");
		fprintf(fp, "//     === Output is aa_lsm.\n");
		fprintf(fp, "//       ===  Output\n");
		fprintf(fp, "      if (myid == 0){\n");
		fprintf(fp, "         fp10 = fopen(\"%s_%c_LSM.dat\",\"wt\");\n",
			Name.c_str(), *TuneGroupName.c_str());
		fprintf(fp, "      }\n");
		fprintf(fp, "      for(isw=1 ; isw <= nparm ; isw++){\n");
		fprintf(fp, "        fprintf(fp10,\"%%20.10f \",(double)isw);\n");
		fprintf(fp, "        for(iii=0 ; iii <= m_lsm ; iii++){\n");
		fprintf(fp,
			"          fprintf(fp10,\"%%20.10f \",aa_lsm[iii][isw-1]);\n");
		fprintf(fp, "      	}\n");
		fprintf(fp, "      }\n");
		fprintf(fp, "      fclose(fp10);\n");
		fprintf(fp, "\n");
		fprintf(fp, "      if (myid == 0){\n");
		fprintf(fp,
			"        printf(\"Output Parameters ==============================\");\n");
		fprintf(fp, "        for(isw=1;isw<=nparm;isw++){\n");
		fprintf(fp, "          printf(\"Parameter No.: %%d\\n\", isw);\n");
		fprintf(fp,
			"          printf(\"Sample Points: %%d / Formula Order: %%d\\n\", n_lsm, m_lsm);\n");
		fprintf(fp, "          printf(\"Calculated Coefficients: \");\n");
		fprintf(fp, "          for(iii=0 ; iii <= m_lsm ; iii++){\n");
		fprintf(fp,
			"            printf(\"%%f * x^%%d\\n\",aa_lsm[iii][isw-1], m_lsm-iii);\n");
		fprintf(fp, "          }\n");
		fprintf(fp, "        }\n");
		fprintf(fp, "//       === End of Output\n");
		fprintf(fp, "      }\n");
		fprintf(fp, "//     !!!!!!!! fitting処理の終り\n");
		fprintf(fp,
			"//     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
		fprintf(fp, "\n");

	}

	fprintf(fp, "\treturn 0;\n");
	fprintf(fp, "}\n");

	fprintf(fp, "//==== End of %s Optimization Routines\n",
		TuneGroupName.c_str());
	fprintf(fp,
		"//==============================================================\n");
	fprintf(fp, "\n");
	fprintf(fp, "\n");
	// d-spline用追加部分---------------------------------------------------------------------

	if (FittingDspline == 1) {
		/** *********************************************** */
		//
		// Kogakuin Irie
		// 実行時d-Splineとその他のd-Splineの判断方法を変更
		// region start 文からチューニングタイミングを取得
		// 対応する } は，現在は6468行目
		//
		if (TuneGroup == tgInstall) {
			//
			// ここまで
			//
			/** *********************************************** */
			fprintf(fp, "#include <stdio.h>\n");
			fprintf(fp, "#include <stdlib.h>\n");
			fprintf(fp, "\n#define DEBUG 2\n\n");
			fprintf(fp, "int dspgiv(int npN");
			// ここから流用　変数名出力のため------

			if (TuneGroup != tgDynamic) {
				if (MainF->Call_ATExec_Script == NULL) {
					TScript *Script = new TScript(MainF->TokenList, 0, NULL,
						ValDataList);
					ArgStr = Script->GetATExecArgStr(true, "", "");
					fprintf(fp, "%s", ArgStr.c_str());
					delete Script;
					// fprintf(fp,"OAT_ATexec%s%s(char *OAT_Routines)\n",TuneGroupName.c_str(),Name.c_str());
				}
				else {
					ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true,
						"", "");
					fprintf(fp, "%s", ArgStr.c_str());
				}
				iBsetSw1Str = "iBestSw1";
			} // else{
			// if(MainF->Call_ATExec_Script == NULL){
			// TScript *Script = new TScript(MainF->TokenList,0,NULL,ValDataList);
			// ArgStr = Script->GetATExecArgStr(true,"","");
			// fprintf(fp,"%s",ArgStr.c_str());
			// delete Script;
			// }else{
			// ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true,"","");
			// fprintf(fp,"%s",ArgStr.c_str());
			//
			// }
			// iBsetSw1Str = "*iBestSw1";
			// }

			// ここまで流用　変数名出力用------
			fprintf(fp, ")\n{\n");

			// 変数宣言部分
			fprintf(fp, "\tint nn=npN*3;\n");
			fprintf(fp, "\tdouble DE[nn+1][nn+1];\n");
			fprintf(fp, "\tdouble R[nn+1][nn+1];\n");
			fprintf(fp, "\tdouble G[nn+1][nn+1];\n");
			fprintf(fp, "\tdouble r,c,s;\n");
			/** ****************************** */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// fprintf(fp,"\tint p,q;\n");
			fprintf(fp, "\tint p,q,q2;\n");
			//
			// ここまで
			//
			/** ****************************** */
			fprintf(fp, "\tdouble data;\n");
			fprintf(fp, "\tint count=1;\n");
			fprintf(fp, "\tdouble x[n];\n");
			fprintf(fp, "\tdouble temp;\n");
			fprintf(fp, "\tint temp2,bestP=0,nextP,prebestP;\n");
			fprintf(fp, "\tdouble select2[npN][2];\n");
			fprintf(fp, "\tint h[npN],kk=0;\n");
			fprintf(fp, "\tdouble alfa=0.1;\n");
			fprintf(fp, "\tdouble t1,t2;\n");
			fprintf(fp, "\tint F2[4];\n");
			fprintf(fp, "\tint i,j,k,i4;\n\n");

			// エクセル書き込み用部分
			fprintf(fp, "\tFILE *fpA;\n");
			fprintf(fp, "\tchar *fname = \"d-SplineData.csv\";\n");
			fprintf(fp, "\tfpA = fopen(fname,\"w\");\n\n");

			// DE初期化部分
			fprintf(fp,
				"\tfor(i=0;i<nn+1;i++){\n\t\tfor(j=0;j<nn+1;j++){\n\t\t\tDE[i][j]=0;\n\t\t\tR[i][j]=0;\n\t\t}\n\t}\n\n");

			// DE1,-2,1セット部分
			fprintf(fp,
				"\tfor(i=0;i<nn-2;i++){\n\t\tDE[i][i]=1*alfa;\n\t\tDE[i][i+1]=-2*alfa;\n\t\tDE[i][i+2]=1*alfa;\n\t}\n\n");

			// h初期化部分
			fprintf(fp, "\tfor(i=0;i<=npN;i++){\n\t\th[i]=npN+1;\n\t}\n\n");

			// 初期4点決定部分
			fprintf(fp,
				"\tF2[0]=1;\n\tF2[1]=(2+npN)/3;\n\tF2[2]=(1+2*npN)/3;\n\tF2[3]=npN;\n\n \tp=nn;\n\n");

			// 初期4点の計算用部分------------------------------------------------------
			fprintf(fp, "\tfor(i4=0;i4<4;i4++){\n\t\tq=F2[i4];\n\n");
			fprintf(fp, "\t\tt1 = OAT_Wtime();\n");
			// 計算する関数に飛ぶ部分	基本的に既存部分の流用
			if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
				fprintf(fp, "\t\t\tiloop_inner = 0;\n");
				fprintf(fp, "\t\t\twhile(OAT_Eecntl_Continue()){\n");
				s = "\t\t\t" + FuncName + "(";
			}
			else {
				s = "\t\t" + FuncName + "(";
			}
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str + ", ";
			}
			s += "q);";
			fprintf(fp, "%s\n", s.c_str());

			fprintf(fp, "\n\t\tt2 = OAT_Wtime();\n\t\tdata = t2 - t1;\n\n");
			fprintf(fp,
				"\t\tprintf(\"\\n---para is %%d time is %%lf--- \\n\",q,data);\n\n");
			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// fprintf(fp,"\t\tq--;\n\t\th[kk]=q;\n\n\t\tDE[p][q]=1;\n\t\tDE[p][nn]=data;\n\n");
			fprintf(fp,
				"\t\tq--;\n\t\tq2=q*3;\n\t\th[kk]=q;\n\n\t\tDE[p][q2]=1;\n\t\tDE[p][nn]=data;\n\n");
			//
			// ここまで
			//
			/** ******************************************************************************************* */

			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// ------初期4点ギブンス変換for文部分ここから------
			// fprintf(fp,"\t\tfor(q=q;q<nn;q++){\n\n");
			//
			// G初期化部分(不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tG[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n\n");
			 */
			// G初期化
			// fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tG[i][i]=0;\n\t\t\t}\n\n");
			// fprintf(fp,"\t\t\tG[nn][nn]=1;\n");
			//
			// G用計算部分
			// fprintf(fp,"\t\t\tr=sqrt( (DE[q][q])*(DE[q][q]) + (DE[p][q])*(DE[p][q]) );\n\n\t\t\tif(r==0)\n\t\t\t\tbreak;\n\n\t\t\tc=DE[q][q]/r;\n\t\t\ts=DE[p][q]/r;\n\n\t\t\tG[p][p]=c;\n\t\t\tG[q][q]=c;\n\n\t\t\tG[q][p]=s;\n\t\t\tG[p][q]=-1*s;\n\n");
			//
			//
			// R=G*DEギブンス変換部分
			// fprintf(fp,"\t\t\tR[q][q]+=G[q][q]*DE[q][q];\n\t\t\tR[q][q]+=G[q][nn]*DE[nn][q];\n\t\t\tif( (q+1)!=nn && (q+1)<=nn ){\n\t\t\tR[q][q+1]+=G[q][q]*DE[q][q+1];\n\t\t\tR[q][q+1]+=G[q][nn]*DE[nn][q+1];\n\t\t\t}\n\n\t\t\tif( (q+2)!=nn && (q+2)<=nn ){\n\t\t\tR[q][q+2]+=G[q][q]*DE[q][q+2];\n\t\t\tR[q][q+2]+=G[q][nn]*DE[nn][q+2];\n\t\t\t}\n\n\t\t\tR[q][nn]+=G[q][q]*DE[q][nn];\n\t\t\tR[q][nn]+=G[q][nn]*DE[nn][nn];\n\n\t\t\tR[nn][q]+=G[nn][q]*DE[q][q];\n\t\t\tR[nn][q]+=G[nn][nn]*DE[nn][q];\n\n\t\t\t");
			// fprintf(fp,"if( (q+1)!=nn && (q+1)<=nn ){\n\t\t\tR[nn][q+1]+=G[nn][q]*DE[q][q+1];\n\t\t\tR[nn][q+1]+=G[nn][nn]*DE[nn][q+1];\n\t\t\t}\n\n\t\t\tif( (q+2)!=nn && (q+2)<=nn ){\n\t\t\tR[nn][q+2]+=G[nn][q]*DE[q][q+2];\n\t\t\tR[nn][q+2]+=G[nn][nn]*DE[nn][q+2];\n\t\t\t}\n\n\t\t\tR[nn][nn]+=G[nn][q]*DE[q][nn];\n\t\t\tR[nn][nn]+=G[nn][nn]*DE[nn][nn];\n\n\t\t\t");
			// fprintf(fp,"DE[q][q]=R[q][q];\n\t\t\tDE[q][q+1]=R[q][q+1];\n\t\t\tDE[q][q+2]=R[q][q+2];\n\t\t\tDE[q][nn]=R[q][nn];\n\n\t\t\tDE[nn][q]=R[nn][q];\n\t\t\tDE[nn][q+1]=R[nn][q+1];\n\t\t\tDE[nn][q+2]=R[nn][q+2];\n\t\t\tDE[nn][nn]=R[nn][nn];\n\n\t\t\tR[q][q]=0;\n\t\t\tR[q][q+1]=0;\n\t\t\tR[q][q+2]=0;\n\t\t\tR[q][nn]=0;\n\n\t\t\tR[nn][q]=0;\n\t\t\tR[nn][q+1]=0;\n\t\t\tR[nn][q+2]=0;\n\t\t\tR[nn][nn]=0;\n");
			//
			// R=G*DEギブンス変換部分(不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tfor(k=0;k<nn+1;k++){\n\t\t\t\t\t\tR[i][j]+=G[i][k]*DE[k][j];\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\n");
			 */
			// DEに次の計算のために計算結果Rを代入する部分（不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tDE[i][j]=R[i][j];\n\t\t\t\t\tR[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n");
			 */
			// fprintf(fp,"\t\t}\n\t\tkk++;\n\n\n\t}\n");
			//
			fprintf(fp, "\t\tfor(q2=q2;q2<nn;q2++){\n\n");

			// G初期化部分(不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tG[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n\n");
			 */
			// G初期化
			fprintf(fp,
				"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tG[i][i]=0;\n\t\t\t}\n\n");
			fprintf(fp, "\t\t\tG[nn][nn]=1;\n");

			// G用計算部分
			fprintf(fp,
				"\t\t\tr=sqrt( (DE[q2][q2])*(DE[q2][q2]) + (DE[p][q2])*(DE[p][q2]) );\n\n\t\t\tif(r==0)\n\t\t\t\tbreak;\n\n\t\t\tc=DE[q2][q2]/r;\n\t\t\ts=DE[p][q2]/r;\n\n\t\t\tG[p][p]=c;\n\t\t\tG[q2][q2]=c;\n\n\t\t\tG[q2][p]=s;\n\t\t\tG[p][q2]=-1*s;\n\n");

			// R=G*DEギブンス変換部分
			fprintf(fp,
				"\t\t\tR[q2][q2]+=G[q2][q2]*DE[q2][q2];\n\t\t\tR[q2][q2]+=G[q2][nn]*DE[nn][q2];\n\t\t\tif( (q2+1)!=nn && (q2+1)<=nn ){\n\t\t\tR[q2][q2+1]+=G[q2][q2]*DE[q2][q2+1];\n\t\t\tR[q2][q2+1]+=G[q2][nn]*DE[nn][q2+1];\n\t\t\t}\n\n\t\t\tif( (q2+2)!=nn && (q2+2)<=nn ){\n\t\t\tR[q2][q2+2]+=G[q2][q2]*DE[q2][q2+2];\n\t\t\tR[q2][q2+2]+=G[q2][nn]*DE[nn][q2+2];\n\t\t\t}\n\n\t\t\tR[q2][nn]+=G[q2][q2]*DE[q2][nn];\n\t\t\tR[q2][nn]+=G[q2][nn]*DE[nn][nn];\n\n\t\t\tR[nn][q2]+=G[nn][q2]*DE[q2][q2];\n\t\t\tR[nn][q2]+=G[nn][nn]*DE[nn][q2];\n\n\t\t\t");
			fprintf(fp,
				"if( (q2+1)!=nn && (q2+1)<=nn ){\n\t\t\tR[nn][q2+1]+=G[nn][q2]*DE[q2][q2+1];\n\t\t\tR[nn][q2+1]+=G[nn][nn]*DE[nn][q2+1];\n\t\t\t}\n\n\t\t\tif( (q2+2)!=nn && (q2+2)<=nn ){\n\t\t\tR[nn][q2+2]+=G[nn][q2]*DE[q2][q2+2];\n\t\t\tR[nn][q2+2]+=G[nn][nn]*DE[nn][q2+2];\n\t\t\t}\n\n\t\t\tR[nn][nn]+=G[nn][q2]*DE[q2][nn];\n\t\t\tR[nn][nn]+=G[nn][nn]*DE[nn][nn];\n\n\t\t\t");
			fprintf(fp,
				"DE[q2][q2]=R[q2][q2];\n\t\t\tDE[q2][q2+1]=R[q2][q2+1];\n\t\t\tDE[q2][q2+2]=R[q2][q2+2];\n\t\t\tDE[q2][nn]=R[q2][nn];\n\n\t\t\tDE[nn][q2]=R[nn][q2];\n\t\t\tDE[nn][q2+1]=R[nn][q2+1];\n\t\t\tDE[nn][q2+2]=R[nn][q2+2];\n\t\t\tDE[nn][nn]=R[nn][nn];\n\n\t\t\tR[q2][q2]=0;\n\t\t\tR[q2][q2+1]=0;\n\t\t\tR[q2][q2+2]=0;\n\t\t\tR[q2][nn]=0;\n\n\t\t\tR[nn][q2]=0;\n\t\t\tR[nn][q2+1]=0;\n\t\t\tR[nn][q2+2]=0;\n\t\t\tR[nn][nn]=0;\n");

			// R=G*DEギブンス変換部分(不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tfor(k=0;k<nn+1;k++){\n\t\t\t\t\t\tR[i][j]+=G[i][k]*DE[k][j];\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\n");
			 */
			// DEに次の計算のために計算結果Rを代入する部分（不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tDE[i][j]=R[i][j];\n\t\t\t\t\tR[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n");
			 */
			fprintf(fp, "\t\t}\n\n\t\tkk++;\n\t}\n\n");
			// ------初期4点ギブンス変換for文部分ここまで------
			//
			// ここまで
			//
			/** ******************************************************************************************* */

			// ------初期4点計算後、ベストパラメタと次のパラメタ決定ここから------

			// 推定値代入初期化部分
			fprintf(fp, "\tfor(i=0;i<nn;i++){\n\t\tx[i]=DE[i][nn];\n\t}\n\n");

			// 後退代入で推定値計算部分
			fprintf(fp,
				"\tfor(i=nn-1; i>=0;i--){\n\t\tfor(j=i+1;j<nn;j++){\n\t\t\tx[i]-=DE[i][j]*x[j];\n\t\t}\n\t\tx[i] /= DE[i][i];\n\t}\n\n");

			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// デバッグ用、推定値表示部分
			// fprintf(fp,"\tif(DEBUG>0){\n\t\tprintf(\"estimation\\n\");\n\t\tfor(i=0;i<npN;i++){\n\t\t\tprintf(\"[%%d],%%lf\\n\",i+1,x[i]);\n\t\t}\n\t\tprintf(\"\\n\");\n\t}\n\n");
			fprintf(fp,
				"\t\tif(DEBUG>0){\n\t\t\tprintf(\"\\nestimation\\n\");\n\t\t\tfor(i=0;i<npN;i++){\n\t\t\t\tprintf(\"[%%d],%%lf\\n\",i+1,x[i*3]);\n\t\t\t\t}printf(\"\\n\");\n\t\t}\n\n");
			//
			// ここまで
			//
			/** ******************************************************************************************* */

			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// 推定値が最小となる番号をベストパラメタに設定する部分
			// fprintf(fp,"\ttemp=x[0];\n\tfor(i=0;i<npN;i++){\n\t\tif(x[i]<temp){\n\t\t\ttemp=x[i];\n\t\t\tbestP=i;\n\t\t}\n\t}\n\n");
			// fprintf(fp,"\tprintf(\"\\nbest para = %%d time = %%lf\\n\",bestP+1,x[bestP]);\n\n");
			fprintf(fp,
				"\ttemp=x[0];\n\tbestP=0;\n\n\tfor(i=0;i<npN;i++){\n\t\tif(x[i*3]<temp){\n\t\t\ttemp=x[i*3];\n\t\t\tbestP=i;\n\t\t}\n\t}\n\n");
			fprintf(fp,
				"\tprintf(\"\\nbest para = %%d time = %%lf\\n\",bestP+1,x[bestP*3]);\n\n");
			//
			// ここまで
			//
			/** ******************************************************************************************* */

			// 選択基準2用配列初期化部分
			fprintf(fp,
				"\tfor(i=0;i<npN;i++){\n\t\tselect2[i][1]=i;\n\t\tselect2[i][0]=0;\n\t}\n\n");

			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// 変化率計算部分
			fprintf(fp,
				"\tfor(i=1;i<npN-1;i++){\n\t\tselect2[i][0]=fabs( x[i-1] - 2*x[i] + x[i+1] );\n\t}\n\n");
			fprintf(fp,
				"\tfor(i=1;i<npN-1;i++){\n\t\tselect2[i][0]=fabs( x[i-3] - 2*x[i] + x[i+3] );\n\t}\n\n");
			//
			// ここまで
			//
			/** ******************************************************************************************* */

			// select2ソート部分
			fprintf(fp,
				"\tfor(i=0 ; i<npN-1 ; i++){\n\t\tfor(j=npN-1 ; j>i ; j--){\n\t\t\tif(select2[j-1][0] < select2[j][0]){\n\t\t\t\ttemp=select2[j][0];\n\t\t\t\ttemp2=select2[j][1];\n\t\t\t\tselect2[j][0]=select2[j-1][0];\n\t\t\t\tselect2[j][1]=select2[j-1][1];\n\t\t\t\tselect2[j-1][0]=temp;\n\t\t\t\tselect2[j-1][1]=temp2;\n\t\t\t}\n\t\t}\n\t}\n\n");

			// 推定したベストPがすでに標本点にあるなら選択基準2、ないなら選択基準1　選択する部分
			fprintf(fp, "\tfor(i=0;i<npN;i++){\n");
			fprintf(fp,
				"\t\tif(bestP==h[i]){\n\t\t\tfor(j=0;j<npN;j++){\n\t\t\t\tfor(k=0;k<npN;k++){\n\t\t\t\t\tif(select2[j][1]==h[k]){\n\t\t\t\t\t\tbreak;\n\t\t\t\t\t}\n\t\t\t\t\tif(k+1==npN){\n\t\t\t\t\t\tnextP=select2[j][1];\n\t\t\t\t\t\tgoto OUT1;\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\t\t\tbreak;\n\t\t}\n");
			fprintf(fp, "\t\telse{\n\t\t\tnextP=bestP;\n\t\t}\n\t}\n\tOUT1:\n");
			fprintf(fp,
				"\tprintf(\"nextP=%%d bestP=%%d \\n\\n\",nextP+1,bestP+1);\n\n"
				);

			// prebestPに4点計算終了時のベストパラメタを代入する部分
			fprintf(fp, "\tprebestP=bestP;\n\n\n");

			// 推定値をエクセルに書き込みする部分
			fprintf(fp,
				"\tfor(i=0;i<nn;i++){\n\t\tfprintf(fpA,\"%%lf\\n\",x[i]);\n\t}\n\tfprintf(fpA,\"end\\n\\n\");\n\n");

			fprintf(fp,
				"\tprintf(\"\\n----------end 4 point---------- \\n\");\n\n");

			// --------4点後のdo while文ここから--------
			fprintf(fp, "\tdo{\n\n");
			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// fprintf(fp,"\t\tq=nextP;\n");
			// fprintf(fp,"\t\tt1 = OAT_Wtime();\n");
			fprintf(fp, "\t\tq=nextP;\n\t\tq2=q*3;\n\n");
			fprintf(fp, "\t\tt1 = omp_get_wtime();\n");
			//
			// ここまで
			//
			/** ******************************************************************************************* */
			// 計算する関数に飛ぶ部分	基本的に既存部分の流用
			if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
				fprintf(fp, "\t\t\tiloop_inner = 0;\n");
				fprintf(fp, "\t\t\twhile(OAT_Eecntl_Continue()){\n");
				s = "\t\t\t" + FuncName + "(";
			}
			else {
				s = "\t\t" + FuncName + "(";
			}
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str + ", ";
			}
			s += "q+1);";
			fprintf(fp, "%s\n", s.c_str());

			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// fprintf(fp,"\t\tt2 = OAT_Wtime();\n");
			fprintf(fp, "\t\tt2 = omp_get_wtime();\n\n");
			//
			// ここまで
			//
			/** ******************************************************************************************* */
			fprintf(fp, "\t\tdata = t2 - t1;\n\n");
			fprintf(fp,
				"\t\tprintf(\"\\n---para is %%d time is %%lf--- \\n\",q+1,data);\n\n");
			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// fprintf(fp,"\t\th[kk]=q;\n\t\tDE[p][q]=1;\n\t\tDE[p][nn]=data;\n\n");
			fprintf(fp,
				"\t\th[kk]=q;\n\t\tDE[p][q2]=1;\n\t\tDE[p][nn]=data;\n\n");
			//
			// ここまで
			//
			/** ******************************************************************************************* */

			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// --------ギブンス変換for文ここから--------
			// fprintf(fp,"\t\tfor(q=q;q<nn;q++){\n\n");
			//
			// G初期化部分(不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tG[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n\n");
			 */
			// G初期化
			// fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tG[i][i]=0;\n\t\t\t}\n\n");
			// fprintf(fp,"\t\t\tG[nn][nn]=1;\n");
			//
			// G用計算部分
			// fprintf(fp,"\t\t\tr=sqrt( (DE[q][q])*(DE[q][q]) + (DE[p][q])*(DE[p][q]) );\n\n\t\t\tif(r==0)\n\t\t\t\tbreak;\n\n\t\t\tc=DE[q][q]/r;\n\t\t\ts=DE[p][q]/r;\n\n\t\t\tG[p][p]=c;\n\t\t\tG[q][q]=c;\n\n\t\t\tG[q][p]=s;\n\t\t\tG[p][q]=-1*s;\n\n");
			//
			// R=G*DEギブンス変換部分
			// fprintf(fp,"\t\t\tR[q][q]+=G[q][q]*DE[q][q];\n\t\t\tR[q][q]+=G[q][nn]*DE[nn][q];\n\t\t\tif( (q+1)!=nn && (q+1)<=nn ){\n\t\t\tR[q][q+1]+=G[q][q]*DE[q][q+1];\n\t\t\tR[q][q+1]+=G[q][nn]*DE[nn][q+1];\n\t\t\t}\n\n\t\t\tif( (q+2)!=nn && (q+2)<=nn ){\n\t\t\tR[q][q+2]+=G[q][q]*DE[q][q+2];\n\t\t\tR[q][q+2]+=G[q][nn]*DE[nn][q+2];\n\t\t\t}\n\n\t\t\tR[q][nn]+=G[q][q]*DE[q][nn];\n\t\t\tR[q][nn]+=G[q][nn]*DE[nn][nn];\n\n\t\t\tR[nn][q]+=G[nn][q]*DE[q][q];\n\t\t\tR[nn][q]+=G[nn][nn]*DE[nn][q];\n\n\t\t\t");
			// fprintf(fp,"if( (q+1)!=nn && (q+1)<=nn ){\n\t\t\tR[nn][q+1]+=G[nn][q]*DE[q][q+1];\n\t\t\tR[nn][q+1]+=G[nn][nn]*DE[nn][q+1];\n\t\t\t}\n\n\t\t\tif( (q+2)!=nn && (q+2)<=nn ){\n\t\t\tR[nn][q+2]+=G[nn][q]*DE[q][q+2];\n\t\t\tR[nn][q+2]+=G[nn][nn]*DE[nn][q+2];\n\t\t\t}\n\n\t\t\tR[nn][nn]+=G[nn][q]*DE[q][nn];\n\t\t\tR[nn][nn]+=G[nn][nn]*DE[nn][nn];\n\n\t\t\t");
			// fprintf(fp,"DE[q][q]=R[q][q];\n\t\t\tDE[q][q+1]=R[q][q+1];\n\t\t\tDE[q][q+2]=R[q][q+2];\n\t\t\tDE[q][nn]=R[q][nn];\n\n\t\t\tDE[nn][q]=R[nn][q];\n\t\t\tDE[nn][q+1]=R[nn][q+1];\n\t\t\tDE[nn][q+2]=R[nn][q+2];\n\t\t\tDE[nn][nn]=R[nn][nn];\n\n\t\t\tR[q][q]=0;\n\t\t\tR[q][q+1]=0;\n\t\t\tR[q][q+2]=0;\n\t\t\tR[q][nn]=0;\n\n\t\t\tR[nn][q]=0;\n\t\t\tR[nn][q+1]=0;\n\t\t\tR[nn][q+2]=0;\n\t\t\tR[nn][nn]=0;\n");
			//
			/*
			//R=G*DEギブンス変換部分(不要なため削除）
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tfor(k=0;k<nn+1;k++){\n\t\t\t\t\t\tR[i][j]+=G[i][k]*DE[k][j];\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\n");

			//DEに次の計算のために計算結果Rを代入する部分(不要なため削除）
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tDE[i][j]=R[i][j];\n\t\t\t\t\tR[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n");
			 */
			//
			// fprintf(fp,"\n\t\t}\n\n\n");

			fprintf(fp, "\t\tfor(q2=q2;q2<nn;q2++){\n\n");

			// G初期化部分(不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tG[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n\n");
			 */
			// G初期化
			fprintf(fp,
				"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tG[i][i]=0;\n\t\t\t}\n\n");
			fprintf(fp, "\t\t\tG[nn][nn]=1;\n");

			// G用計算部分
			fprintf(fp,
				"\t\t\tr=sqrt( (DE[q2][q2])*(DE[q2][q2]) + (DE[p][q2])*(DE[p][q2]) );\n\n\t\t\tif(r==0)\n\t\t\t\tbreak;\n\n\t\t\tc=DE[q2][q2]/r;\n\t\t\ts=DE[p][q2]/r;\n\n\t\t\tG[p][p]=c;\n\t\t\tG[q2][q2]=c;\n\n\t\t\tG[q2][p]=s;\n\t\t\tG[p][q2]=-1*s;\n\n");

			// R=G*DEギブンス変換部分
			fprintf(fp,
				"\t\t\tR[q2][q2]+=G[q2][q2]*DE[q2][q2];\n\t\t\tR[q2][q2]+=G[q2][nn]*DE[nn][q2];\n\t\t\tif( (q2+1)!=nn && (q2+1)<=nn ){\n\t\t\tR[q2][q2+1]+=G[q2][q2]*DE[q2][q2+1];\n\t\t\tR[q2][q2+1]+=G[q2][nn]*DE[nn][q2+1];\n\t\t\t}\n\n\t\t\tif( (q2+2)!=nn && (q2+2)<=nn ){\n\t\t\tR[q2][q2+2]+=G[q2][q2]*DE[q2][q2+2];\n\t\t\tR[q2][q2+2]+=G[q2][nn]*DE[nn][q2+2];\n\t\t\t}\n\n\t\t\tR[q2][nn]+=G[q2][q2]*DE[q2][nn];\n\t\t\tR[q2][nn]+=G[q2][nn]*DE[nn][nn];\n\n\t\t\tR[nn][q2]+=G[nn][q2]*DE[q2][q2];\n\t\t\tR[nn][q2]+=G[nn][nn]*DE[nn][q2];\n\n\t\t\t");
			fprintf(fp,
				"if( (q2+1)!=nn && (q2+1)<=nn ){\n\t\t\tR[nn][q2+1]+=G[nn][q2]*DE[q2][q2+1];\n\t\t\tR[nn][q2+1]+=G[nn][nn]*DE[nn][q2+1];\n\t\t\t}\n\n\t\t\tif( (q2+2)!=nn && (q2+2)<=nn ){\n\t\t\tR[nn][q2+2]+=G[nn][q2]*DE[q2][q2+2];\n\t\t\tR[nn][q2+2]+=G[nn][nn]*DE[nn][q2+2];\n\t\t\t}\n\n\t\t\tR[nn][nn]+=G[nn][q2]*DE[q2][nn];\n\t\t\tR[nn][nn]+=G[nn][nn]*DE[nn][nn];\n\n\t\t\t");
			fprintf(fp,
				"DE[q2][q2]=R[q2][q2];\n\t\t\tDE[q2][q2+1]=R[q2][q2+1];\n\t\t\tDE[q2][q2+2]=R[q2][q2+2];\n\t\t\tDE[q2][nn]=R[q2][nn];\n\n\t\t\tDE[nn][q2]=R[nn][q2];\n\t\t\tDE[nn][q2+1]=R[nn][q2+1];\n\t\t\tDE[nn][q2+2]=R[nn][q2+2];\n\t\t\tDE[nn][nn]=R[nn][nn];\n\n\t\t\tR[q2][q2]=0;\n\t\t\tR[q2][q2+1]=0;\n\t\t\tR[q2][q2+2]=0;\n\t\t\tR[q2][nn]=0;\n\n\t\t\tR[nn][q2]=0;\n\t\t\tR[nn][q2+1]=0;\n\t\t\tR[nn][q2+2]=0;\n\t\t\tR[nn][nn]=0;\n");

			/*
			//R=G*DEギブンス変換部分(不要なため削除）
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tfor(k=0;k<nn+1;k++){\n\t\t\t\t\t\tR[i][j]+=G[i][k]*DE[k][j];\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\n");

			//DEに次の計算のために計算結果Rを代入する部分(不要なため削除）
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tDE[i][j]=R[i][j];\n\t\t\t\t\tR[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n");
			 */

			fprintf(fp, "\t\t}\n\n\n");
			// --------ギブンス変換for文ここまで--------
			//
			// ここまで
			//
			/** ******************************************************************************************* */

			// ギブンス変換後、推定値計算、次の標本点決定部分
			// 推定値代入初期化部分
			fprintf(fp,
				"\t\tfor(i=0;i<nn;i++){\n\t\t\tx[i]=DE[i][nn];\n\t\t}\n\n");

			// 後退代入で推定値計算部分
			fprintf(fp,
				"\t\tfor(i=nn-1; i>=0;i--){\n\t\t\tfor(j=i+1;j<nn;j++){\n\t\t\t\tx[i]-=DE[i][j]*x[j];\n\t\t\t}\n\t\t\tx[i] /= DE[i][i];\n\t\t}\n\n");

			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// デバッグ用、推定値表示部分
			// fprintf(fp,"\t\tif(DEBUG>0){\n\t\t\tprintf(\"estimation\\n\");\n\t\t\tfor(i=0;i<npN;i++){\n\t\t\t\tprintf(\"[%%d],%%lf\\n\",i+1,x[i]);\n\t\t\t\t}printf(\"\\n\");\n\t\t}\n\n");
			fprintf(fp,
				"\t\tif(DEBUG>0){\n\t\t\tprintf(\"\\nestimation\\n\");\n\t\t\tfor(i=0;i<npN;i++){\n\t\t\t\tprintf(\"[%%d],%%lf\\n\",i+1,x[i*3]);\n\t\t\t}\n\t\t\tprintf(\"\\n\");\n\t\t}\n\n");
			//
			// ここまで
			//
			/** ******************************************************************************************* */

			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// 推定値が最小となる番号をベストパラメタに設定する部分
			// fprintf(fp,"\ttemp=x[0];\n\tfor(i=0;i<npN;i++){\n\t\tif(x[i]<temp){\n\t\t\ttemp=x[i];\n\t\t\tbestP=i;\n\t\t}\n\t}\n\n");
			// fprintf(fp,"\tprintf(\"\\nbest para = %%d time = %%lf\\n\",bestP+1,x[bestP]);\n\n");
			fprintf(fp,
				"\t\ttemp=x[0];\n\t\tbestP=0;\n\n\t\tfor(i=0;i<npN;i++){\n\t\t\tif(x[i*3]<temp){\n\t\t\t\ttemp=x[i*3];\n\t\t\t\tbestP=i;\n\t\t\t}\n\t\t}\n\n");
			fprintf(fp,
				"\t\tprintf(\"\\nbest para = %%d time = %%lf\\n\",bestP+1,x[bestP*3]);\n\n");
			//
			// ここまで
			//
			/** ******************************************************************************************* */

			// 選択基準2用配列初期化部分
			fprintf(fp,
				"\t\tfor(i=0;i<npN;i++){\n\t\t\tselect2[i][1]=i;\n\t\t\tselect2[i][0]=0;\n\t\t}\n\n");

			/** ******************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// 変化率計算部分
			// fprintf(fp,"\tfor(i=1;i<npN-1;i++){\n\t\tselect2[i][0]=fabs( x[i-1] - 2*x[i] + x[i+1] );\n\t}\n\n");
			fprintf(fp,
				"\t\tfor(i=1;i<npN-1;i++){\n\t\t\tselect2[i][0]=fabs( x[i-3] - 2*x[i] + x[i+3] );\n\t\t}\n\n");
			//
			// ここまで
			//
			/** ******************************************************************************************* */

			// select2ソート部分
			fprintf(fp,
				"\t\tfor(i=0 ; i<npN-1 ; i++){\n\t\t\tfor(j=npN-1 ; j>i ; j--){\n\t\t\t\tif(select2[j-1][0] < select2[j][0]){\n\t\t\t\t\ttemp=select2[j][0];\n\t\t\t\t\ttemp2=select2[j][1];\n\t\t\t\t\tselect2[j][0]=select2[j-1][0];\n\t\t\t\t\tselect2[j][1]=select2[j-1][1];\n\t\t\t\t\tselect2[j-1][0]=temp;\n\t\t\t\t\tselect2[j-1][1]=temp2;\n\t\t\t}\n\t\t\t}\n\t\t}\n\n");

			// 推定したベストPがすでに標本点にあるなら選択基準2、ないなら選択基準1　選択する部分
			fprintf(fp, "\t\tfor(i=0;i<npN;i++){\n");
			fprintf(fp,
				"\t\t\tif(bestP==h[i]){\n\t\t\t\tfor(j=0;j<npN;j++){\n\t\t\t\t\tfor(k=0;k<npN;k++){\n\t\t\t\t\t\tif(select2[j][1]==h[k]){\n\t\t\t\t\t\t\tbreak;\n\t\t\t\t\t\t}\n\t\t\t\t\t\tif(k+1==npN){\n\t\t\t\t\t\t\tnextP=select2[j][1];\n\t\t\t\t\t\t\tgoto OUT2;\n\t\t\t\t\t\t}\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t\tbreak;\n\t\t\t}\n");
			fprintf(fp,
				"\t\t\telse{\n\t\t\t\tnextP=bestP;\n\t\t\t}\n\t\t}\n\t\tOUT2:\n"
				);
			fprintf(fp,
				"\t\tprintf(\"nextP=%%d bestP=%%d \\n\\n\",nextP+1,bestP+1);\n\n");

			// 前回推定したパラメタと同じならカウント+1違うならカウント+1に初期化する部分
			fprintf(fp,
				"\t\tif(prebestP==bestP){\n\t\t\tcount++;\n\t\t}\n\t\telse{\n\t\t\tcount=1;\n\t\t}\n\n");

			// 今回推定したベストパラメタをprebestPに設定
			fprintf(fp, "\t\tprebestP=bestP;\n\n");

			fprintf(fp, "\t\tkk++;\n\n");

			// 推定値をエクセルに書き込みする部分
			fprintf(fp,
				"\t\tfor(i=0;i<nn;i++){\n\t\t\tfprintf(fpA,\"%%lf\\n\",x[i]);\n\t\t}\n\t\tfprintf(fpA,\"end\\n\\n\");\n\n");

			fprintf(fp, "\t\tprintf(\"count = %%d \\n\",count);\n\n");

			// while文
			fprintf(fp,
				"\t}while(count<3);\n\n\tprintf(\"count end\\n\");\n\tprintf(\"usedParaNums = %%d\\n\",kk);\n\n");

			// --------4点後のdo while文ここまで--------

			fprintf(fp, "\tfclose(fpA);\n\n");
			fprintf(fp, "\treturn bestP+1;\n}\n");
		}

		// d-spline用追加部分　ここまで---------------------------------------------------------------------

		// d-spline用追加部分----------------dynamicDspline--------------------------------------

		/** *********************************************** */
		//
		// Kogakuin Irie
		// 実行時d-Splineとその他のd-Splineの判断方法を変更
		// region start 文からチューニングタイミングを取得
		// 対応する } は，現在は6786行目
		//
		// 既存コードはコメントアウト
		//
		// if(FittingDynamic == 1){
		if (TuneGroup == tgDynamic) {
			//
			// ここまで
			//
			/** *********************************************** */
			int CaseNums = (int)((variedToValue[0] - variedFromValue[0])
				/ variedStepValue[0] + 1);

			// デバッグ用と関数名部分
			fprintf(fp, "#define DEBUG 1\n");
			/** ********************************************************************************************************************************** */
			//
			// Kogakuin Irie
			// d-Spline関数名に「自動チューニング種類名」と「チューニング領域名」を後ろにつける
			// また，ヘッダファイルに関数定義を出力
			// 既存コードはコメントアウト
			//
			// fprintf(fp,"int dynamicDspline(int npN , int *isw , int Dswitch , double data)\n{\n");
			fprintf(fp,
				"int dynamicDspline%s%s(int npN , int *isw , int Dswitch , double data)\n{\n", TuneGroupName.c_str(), Name.c_str());
			fprintf(fpOutHeader,
				"int dynamicDspline%s%s(int npN , int *isw , int Dswitch , double data);\n"
				, TuneGroupName.c_str(), Name.c_str());
			//
			// ここまで
			//
			/** ********************************************************************************************************************************** */

			// 変数宣言部分
			fprintf(fp, "\tint nn=npN*3-2;\n");
			/** ****************************************************************************************************************************************************************************************** */
			//
			// Kogakuin Irie
			// 配列の要素数を，パラメータの取りうる数により変更させる
			// 既存コードはコメントアウト
			//
			// fprintf(fp,"\tstatic double DE[100+1][100+1];\n\tstatic double R[100+1][100+1];\n\tstatic double G[100+1][100+1];\n");
			fprintf(fp,
				"\tstatic double DE[%d+1][%d+1];\n\tstatic double R[%d+1][%d+1];\n\tstatic double G[%d+1][%d+1];\n", 3 * CaseNums - 2, 3 * CaseNums - 2, 3 * CaseNums - 2, 3 * CaseNums - 2, 3 * CaseNums - 2, 3 * CaseNums - 2);
			//
			// ここまで
			//
			/** ****************************************************************************************************************************************************************************************** */

			/** ************************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// fprintf(fp,"\tdouble r,c,s;\n\tint p,q;\n\tstatic int count=1;\n\t");
			fprintf(fp,
				"\tdouble r,c,s;\n\tint p,q,q2;\n\tstatic int count=1;\n\t");
			//
			// ここまで
			//
			/** ************************************************************************************************* */
			fprintf(fp,
				"static int count2=0;\n\tdouble x[nn];\n\tdouble temp;\n\tint temp2;\n");
			fprintf(fp,
				"\tint bestP=0;\n\tint nextP;\n\tstatic int prebestP=0;\n\tdouble select2[npN][2];\n");

			/** *********************************************************************************************************** */
			//
			// Kogakuin Irie
			// 配列の要素数を，パラメータの取りうる数により変更させる
			// 既存コードはコメントアウト
			//
			// fprintf(fp,"\tstatic int h[100];\n\tstatic int kk=0;\n\tdouble alfa=0.1;\n\tstatic int Fcount=0;\n");
			fprintf(fp,
				"\tstatic int h[%d+1];\n\tstatic int kk=0;\n\tdouble alfa=0.1;\n\tstatic int Fcount=0;\n", CaseNums);
			//
			// ここまで
			//
			/** *********************************************************************************************************** */

			fprintf(fp, "\tint i,j,k;\n\n");

			// エクセル書き込み用部分
			fprintf(fp, "\tFILE *fpA;\n");
			fprintf(fp, "\tchar *fname = \"d-SplineData.csv\";\n");
			fprintf(fp, "\tfpA = fopen(fname,\"a\");\n\n");

			// パラメタ探索後の終了部分
			fprintf(fp, "\tif(count2==1){\n\t\treturn 0;\n\t}\n\n");

			fprintf(fp,
				"\tif(count==3){\n\t\tprintf(\"count=3 end\\n\");\n\t\tprintf(\"usedParaNums=%%d\\n\", Fcount-1);\n\t\t*isw = prebestP+1;\n");
			fprintf(fp, "\t\tcount2=1;\n\t\treturn 0;\n\t}\n\n");

			// h初期化部分
			fprintf(fp,
				"\tif(Fcount==0){\n\t\tfor(i=0;i<=npN;i++){\n\t\t\th[i]=npN+1;\n\t\t}\n\t}\n\n\tp=nn;\n\n");

			// Dswitch1部分ここから--------------------
			fprintf(fp, "\tif(Dswitch==1){\n\n");

			// switch部分ここから
			fprintf(fp,
				"\t\tswitch(Fcount){\n\t\tcase 0:\n\t\t\t*isw = 1;\n\t\t\tbreak;\n");
			fprintf(fp, "\t\tcase 1:\n\t\t\t*isw = npN;\n\t\t\tbreak;\n");
			fprintf(fp, "\t\tcase 2:\n\t\t\t*isw = (2+npN)/3;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 3:\n\t\t\t*isw = (1+2*npN)/3;\n\t\t\tbreak;\n");

			// default部分
			fprintf(fp, "\t\tdefault:\n\n");
			// 推定値初期化部分
			fprintf(fp,
				"\t\t\tfor(i=0;i<nn;i++){\n\t\t\t\tx[i]=DE[i][nn];\n\t\t\t}\n\n"
				);

			// 後退代入計算部分
			fprintf(fp,
				"\t\t\tfor(i=nn-1; i>=0;i--){\n\t\t\t\tfor(j=i+1;j<nn;j++){\n\t\t\t\t\tx[i]-=DE[i][j]*x[j];\n\t\t\t\t}\n\t\t\t\tx[i] /= DE[i][i];\n\t\t\t}\n\n");

			// デバッグ用推定値表示部分
			fprintf(fp,
				"\t\t\tif(DEBUG>0){\n\t\t\t\tprintf(\"\\nestimation\\n\");\n\t\t\t\tfor(i=0;i<npN;i++){\n");
			/** ************************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// fprintf(fp,"\t\t\t\t\tprintf(\"[%%d],%%lf\\n\",i+1,x[i]);\n\t\t\t\t\t}printf(\"\\n\");\n\t\t\t\t}\n\n");
			fprintf(fp,
				"\t\t\t\t\tprintf(\"[%%d],%%lf\\n\",i+1,x[i*3]);\n\t\t\t\t\t}printf(\"\\n\");\n\t\t\t\t}\n\n");
			//
			// ここまで
			//
			/** ************************************************************************************************* */

			/** ************************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// 最小推定値探索部分
			// fprintf(fp,"\t\t\ttemp=x[0];\n\t\t\tfor(i=0;i<npN;i++){\n\t\t\t\tif(x[i]<temp){\n");
			// fprintf(fp,"\t\t\t\t\ttemp=x[i];\n\t\t\t\t\tbestP=i;\n\t\t\t\t}\n\t\t\t}\n");
			fprintf(fp,
				"\t\t\ttemp=x[0];\n\t\t\tbestP=0;\n\n\t\t\tfor(i=0;i<npN;i++){\n\t\t\t\tj=i*3;\n\n\t\t\t\tif(x[j]<temp){\n");
			fprintf(fp,
				"\t\t\t\t\ttemp=x[j];\n\t\t\t\t\tbestP=i;\n\t\t\t\t}\n\t\t\t}\n\n");
			//
			// ここまで
			//
			/** ************************************************************************************************* */

			/** ************************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// ベストパラメタ表示部分
			// fprintf(fp,"\t\t\tprintf(\"\\nbest para = %%d time = %%lf\\n\",bestP+1,x[bestP]);\n\n");
			fprintf(fp,
				"\t\t\tprintf(\"\\nbest para = %%d time = %%lf\\n\",bestP+1,x[bestP*3]);\n\n");
			//
			// ここまで
			//
			/** ************************************************************************************************* */

			// 選択基準2初期化部分
			fprintf(fp,
				"\t\t\tfor(i=0;i<npN;i++){\n\t\t\t\tselect2[i][1]=i;\n\t\t\t\tselect2[i][0]=0;\n\t\t\t}\n\n");

			/** ************************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// 変化率計算部分
			// fprintf(fp,"\t\t\tfor(i=1;i<npN-1;i++){\n\t\t\t\tselect2[i][0]=fabs( x[i-1] - 2*x[i] + x[i+1] );\n\t\t\t}\n\n");
			fprintf(fp,
				"\t\t\tfor(i=1;i<npN-1;i++){\n\t\t\t\tj=i*3;\n\t\t\t\tselect2[i][0]=fabs( x[j-3] - 2*x[j] + x[j+3] );\n\t\t\t}\n\n");
			//
			// ここまで
			//
			/** ************************************************************************************************* */

			// select2ソート部分
			fprintf(fp,
				"\t\t\tfor(i=0 ; i<npN-1 ; i++){\n\t\t\t\tfor(j=npN-1 ; j>i ; j--){\n\t\t\t\t\tif(select2[j-1][0] < select2[j][0]){\n\t\t\t\t\t\ttemp=select2[j][0];\n\t\t\t\t\t\ttemp2=select2[j][1];\n\t\t\t\t\t\tselect2[j][0]=select2[j-1][0];\n\t\t\t\t\t\tselect2[j][1]=select2[j-1][1];\n\t\t\t\t\t\tselect2[j-1][0]=temp;\n\t\t\t\t\t\tselect2[j-1][1]=temp2;\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\n");

			// 選択基準1or2選択部分
			fprintf(fp,
				"\t\t\tfor(i=0;i<npN;i++){\n\t\t\t\tif(bestP==h[i]){\n\t\t\t\t\tfor(j=0;j<npN;j++){\n\t\t\t\t\t\tfor(k=0;k<npN;k++){\n\t\t\t\t\t\t\tif(select2[j][1]==h[k]){\n\t\t\t\t\t\t\t\tbreak;\n\t\t\t\t\t\t\t}\n\t\t\t\t\t\t\tif(k+1==npN){\n\t\t\t\t\t\t\t\tnextP=select2[j][1];\n\t\t\t\t\t\t\t\tgoto OUT1;\n\t\t\t\t\t\t\t}\n\t\t\t\t\t\t}\n\t\t\t\t\t}\n\t\t\t\t\tbreak;\n\t\t\t\t}\n\t\t\t\telse{\n\t\t\t\t\tnextP=bestP;\n\t\t\t\t}\n\t\t\t}\n\t\t\tOUT1:\n");
			// ベストパラメタと次のパラメタを表示
			fprintf(fp,
				"\t\t\tprintf(\"nextP=%%d bestP=%%d \\n\\n\",nextP+1,bestP+1);\n");

			// iswに次使うパラメタをセットする部分
			fprintf(fp, "\t\t\t*isw = nextP+1;\n\n");

			// 3回カウント用部分
			fprintf(fp,
				"\t\t\tif(prebestP==bestP){\n\t\t\t\tcount++;\n\t\t\t}\n");
			fprintf(fp,
				"\t\t\telse{\n\t\t\t\tcount=1;\n\t\t\t}\n\t\t\tprebestP=bestP;\n\n");

			// エクセルに書き込みをする部分
			fprintf(fp,
				"\t\t\tfor(i=0;i<nn;i++){\n\t\t\t\tfprintf(fpA,\"%%lf\\n\",x[i]);\n\t\t\t}\n\t\t\tfprintf(fpA,\"end\\n\\n\");\n\n");

			// default部分ここまで
			fprintf(fp, "\t\t\tbreak;\n");

			// switchここまで
			fprintf(fp, "\t\t}\n\t\tFcount++;\n");

			// Dswitch1ここまで------------
			fprintf(fp, "\t}\n\n");

			// Dswitch2ここから-----------------

			fprintf(fp, "\tif(Dswitch == 2){\n\n");

			// DE初期化部分
			fprintf(fp,
				"\t\tif(Fcount == 1){\n\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tDE[i][j]=0;\n\t\t\t\t\tR[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n\n");
			fprintf(fp,
				"\t\t\tfor(i=0;i<nn-2;i++){\n\t\t\t\tDE[i][i]=1*alfa;\n\t\t\t\tDE[i][i+1]=-2*alfa;\n\t\t\t\tDE[i][i+2]=1*alfa;\n\t\t\t}\n\t\t}\n\n");

			/** ************************************************************************************************* */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// 標本点とその実測値のセット部分
			// fprintf(fp,"\t\tq = *isw;\n\t\tq--;\n\t\th[kk]=q;\n\n\t\tDE[p][q]=1;\n\t\tDE[p][nn]=data;\n\n");
			fprintf(fp,
				"\t\tq = *isw;\n\t\tq--;\n\t\tq2=q*3;\n\t\th[kk]=q;\n\n\t\tDE[p][q2]=1;\n\t\tDE[p][nn]=data;\n\n");
			//
			// ここまで
			//
			/** ************************************************************************************************* */

			/** **************************************************************************************************************************************************************************** */
			//
			// Kogakuin Irie
			// d-Splineのバグ修正のための変更
			// 既存コードはコメントアウト
			//
			// ギブンス変換ループ部分-------------------

			// fprintf(fp,"\t\tfor(q=q;q<nn;q++){\n\n");

			// G初期化部分（不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tG[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n\n");
			 */
			// G初期化
			// fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tG[i][i]=0;\n\t\t\t}\n\n");
			// fprintf(fp,"\t\t\tG[nn][nn]=1;\n");

			// G用計算部分
			// fprintf(fp,"\t\t\tr=sqrt( (DE[q][q])*(DE[q][q]) + (DE[p][q])*(DE[p][q]) );\n\n\t\t\tif(r==0)\n\t\t\t\tbreak;\n\n\t\t\tc=DE[q][q]/r;\n\t\t\ts=DE[p][q]/r;\n\n\t\t\tG[p][p]=c;\n\t\t\tG[q][q]=c;\n\n\t\t\tG[q][p]=s;\n\t\t\tG[p][q]=-1*s;\n\n");

			// R=G*DEギブンス変換部分
			// fprintf(fp,"\t\t\tR[q][q]+=G[q][q]*DE[q][q];\n\t\t\tR[q][q]+=G[q][nn]*DE[nn][q];\n\t\t\tif( (q+1)!=nn && (q+1)<=nn ){\n\t\t\tR[q][q+1]+=G[q][q]*DE[q][q+1];\n\t\t\tR[q][q+1]+=G[q][nn]*DE[nn][q+1];\n\t\t\t}\n\n\t\t\tif( (q+2)!=nn && (q+2)<=nn ){\n\t\t\tR[q][q+2]+=G[q][q]*DE[q][q+2];\n\t\t\tR[q][q+2]+=G[q][nn]*DE[nn][q+2];\n\t\t\t}\n\n\t\t\tR[q][nn]+=G[q][q]*DE[q][nn];\n\t\t\tR[q][nn]+=G[q][nn]*DE[nn][nn];\n\n\t\t\tR[nn][q]+=G[nn][q]*DE[q][q];\n\t\t\tR[nn][q]+=G[nn][nn]*DE[nn][q];\n\n\t\t\t");
			// fprintf(fp,"if( (q+1)!=nn && (q+1)<=nn ){\n\t\t\tR[nn][q+1]+=G[nn][q]*DE[q][q+1];\n\t\t\tR[nn][q+1]+=G[nn][nn]*DE[nn][q+1];\n\t\t\t}\n\n\t\t\tif( (q+2)!=nn && (q+2)<=nn ){\n\t\t\tR[nn][q+2]+=G[nn][q]*DE[q][q+2];\n\t\t\tR[nn][q+2]+=G[nn][nn]*DE[nn][q+2];\n\t\t\t}\n\n\t\t\tR[nn][nn]+=G[nn][q]*DE[q][nn];\n\t\t\tR[nn][nn]+=G[nn][nn]*DE[nn][nn];\n\n\t\t\t");
			// fprintf(fp,"DE[q][q]=R[q][q];\n\t\t\tDE[q][q+1]=R[q][q+1];\n\t\t\tDE[q][q+2]=R[q][q+2];\n\t\t\tDE[q][nn]=R[q][nn];\n\n\t\t\tDE[nn][q]=R[nn][q];\n\t\t\tDE[nn][q+1]=R[nn][q+1];\n\t\t\tDE[nn][q+2]=R[nn][q+2];\n\t\t\tDE[nn][nn]=R[nn][nn];\n\n\t\t\tR[q][q]=0;\n\t\t\tR[q][q+1]=0;\n\t\t\tR[q][q+2]=0;\n\t\t\tR[q][nn]=0;\n\n\t\t\tR[nn][q]=0;\n\t\t\tR[nn][q+1]=0;\n\t\t\tR[nn][q+2]=0;\n\t\t\tR[nn][nn]=0;\n");

			// R=G*DEギブンス変換部分（不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tfor(k=0;k<nn+1;k++){\n\t\t\t\t\t\tR[i][j]+=G[i][k]*DE[k][j];\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\n");

			//DEに次の計算のために計算結果Rを代入する部分（不要なため削除）
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tDE[i][j]=R[i][j];\n\t\t\t\t\tR[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n");
			 */
			// fprintf(fp,"\n\t\t}\n");

			// ギブンス変換ループここまで------------------

			// ギブンス変換ループ部分-------------------

			fprintf(fp, "\t\tfor(q2=q2;q2<nn;q2++){\n\n");

			// G初期化部分（不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tG[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n\n");
			 */
			// G初期化
			fprintf(fp,
				"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tG[i][i]=0;\n\t\t\t}\n\n");
			fprintf(fp, "\t\t\tG[nn][nn]=1;\n");

			// G用計算部分
			fprintf(fp,
				"\t\t\tr=sqrt( (DE[q2][q2])*(DE[q2][q2]) + (DE[p][q2])*(DE[p][q2]) );\n\n\t\t\tif(r==0)\n\t\t\t\tbreak;\n\n\t\t\tc=DE[q2][q2]/r;\n\t\t\ts=DE[p][q2]/r;\n\n\t\t\tG[p][p]=c;\n\t\t\tG[q2][q2]=c;\n\n\t\t\tG[q2][p]=s;\n\t\t\tG[p][q2]=-1*s;\n\n");

			// R=G*DEギブンス変換部分
			fprintf(fp,
				"\t\t\tR[q2][q2]+=G[q2][q2]*DE[q2][q2];\n\t\t\tR[q2][q2]+=G[q2][nn]*DE[nn][q2];\n\t\t\tif( (q2+1)!=nn && (q2+1)<=nn ){\n\t\t\tR[q2][q2+1]+=G[q2][q2]*DE[q2][q2+1];\n\t\t\tR[q2][q2+1]+=G[q2][nn]*DE[nn][q2+1];\n\t\t\t}\n\n\t\t\tif( (q2+2)!=nn && (q2+2)<=nn ){\n\t\t\tR[q2][q2+2]+=G[q2][q2]*DE[q2][q2+2];\n\t\t\tR[q2][q2+2]+=G[q2][nn]*DE[nn][q2+2];\n\t\t\t}\n\n\t\t\tR[q2][nn]+=G[q2][q2]*DE[q2][nn];\n\t\t\tR[q2][nn]+=G[q2][nn]*DE[nn][nn];\n\n\t\t\tR[nn][q2]+=G[nn][q2]*DE[q2][q2];\n\t\t\tR[nn][q2]+=G[nn][nn]*DE[nn][q2];\n\n\t\t\t");
			fprintf(fp,
				"if( (q2+1)!=nn && (q2+1)<=nn ){\n\t\t\tR[nn][q2+1]+=G[nn][q2]*DE[q2][q2+1];\n\t\t\tR[nn][q2+1]+=G[nn][nn]*DE[nn][q2+1];\n\t\t\t}\n\n\t\t\tif( (q2+2)!=nn && (q2+2)<=nn ){\n\t\t\tR[nn][q2+2]+=G[nn][q2]*DE[q2][q2+2];\n\t\t\tR[nn][q2+2]+=G[nn][nn]*DE[nn][q2+2];\n\t\t\t}\n\n\t\t\tR[nn][nn]+=G[nn][q2]*DE[q2][nn];\n\t\t\tR[nn][nn]+=G[nn][nn]*DE[nn][nn];\n\n\t\t\t");
			fprintf(fp,
				"DE[q2][q2]=R[q2][q2];\n\t\t\tDE[q2][q2+1]=R[q2][q2+1];\n\t\t\tDE[q2][q2+2]=R[q2][q2+2];\n\t\t\tDE[q2][nn]=R[q2][nn];\n\n\t\t\tDE[nn][q2]=R[nn][q2];\n\t\t\tDE[nn][q2+1]=R[nn][q2+1];\n\t\t\tDE[nn][q2+2]=R[nn][q2+2];\n\t\t\tDE[nn][nn]=R[nn][nn];\n\n\t\t\tR[q2][q2]=0;\n\t\t\tR[q2][q2+1]=0;\n\t\t\tR[q2][q2+2]=0;\n\t\t\tR[q2][nn]=0;\n\n\t\t\tR[nn][q2]=0;\n\t\t\tR[nn][q2+1]=0;\n\t\t\tR[nn][q2+2]=0;\n\t\t\tR[nn][nn]=0;\n");

			// R=G*DEギブンス変換部分（不要なため削除）
			/*
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tfor(k=0;k<nn+1;k++){\n\t\t\t\t\t\tR[i][j]+=G[i][k]*DE[k][j];\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\n");

			//DEに次の計算のために計算結果Rを代入する部分（不要なため削除）
			fprintf(fp,"\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tDE[i][j]=R[i][j];\n\t\t\t\t\tR[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n");
			 */
			fprintf(fp, "\n\t\t}\n");

			// ギブンス変換ループここまで------------------
			//
			// ここまで
			//
			/** **************************************************************************************************************************************************************************** */

			fprintf(fp, "\t\tkk++;\n\n");

			// Dswitch2ここまで
			fprintf(fp, "\t}\n");

			fprintf(fp, "\tfclose(fpA);\n\n");

			// 関数ここまで
			fprintf(fp, "\treturn 0;\n}\n\n");

		}
	}

	// d-spline用追加部分　ここまで----------------dynamicDspline--------------------------------------
	//

	/** ********************************************************************************************************************** */
	//
	// Kogakuin Irie
	// 2次元対応のd-Spline用追加部分
	//
	if (FittingDspline == 2) {
		// 2次元用d-spline（インストール時自動チューニング用）
		if (TuneGroup == tgInstall) {
			// インストール時自動チューニングに使用するdspline2の定義
			fprintf(fp, "#include <stdio.h>\n");
			fprintf(fp, "#include <stdlib.h>\n");
			fprintf(fp, "\n#define DEBUG 2\n\n");

			// ここから流用　関数出力のため------

			if (TuneGroup != tgDynamic) {
				if (MainF->Call_ATExec_Script == NULL) {
					TScript *Script = new TScript(MainF->TokenList, 0, NULL,
						ValDataList);
					ArgStr = Script->GetATExecArgStr(true, "", "");
					fprintf(fp,
						"int dsp2giv%s%s(int npN,int para1st,int para2nd%s)\n",
						TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());
					fprintf(fpOutHeader,
						"int dsp2giv%s%s(int npN,int para1st,int para2nd%s);\n"
						, TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());
					delete Script;
					// fprintf(fp,"OAT_ATexec%s%s(char *OAT_Routines)\n",TuneGroupName.c_str(),Name.c_str());
				}
				else {
					ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true,
						"", "");
					fprintf(fp,
						"int dsp2giv%s%s(int npN,int para1st,int para2nd%s)\n",
						TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());
					fprintf(fpOutHeader,
						"int dsp2giv%s%s(int npN,int para1st,int para2nd%s);\n"
						, TuneGroupName.c_str(), Name.c_str(), ArgStr.c_str());
				}
				iBsetSw1Str = "iBestSw1";
			} // else{
			// if(MainF->Call_ATExec_Script == NULL){
			// TScript *Script = new TScript(MainF->TokenList,0,NULL,ValDataList);
			// ArgStr = Script->GetATExecArgStr(true,"","");
			/*
			fprintf(fp,"int dsp2giv%s%s(char *OAT_Routines,int %s,int *iBestSw1)\n",
			TuneGroupName.c_str(),Name.c_str(),BaseValName.c_str());
			fprintf(fpOutHeader,"int dsp2giv%s%s(char *OAT_Routines,int %s,int *iBestSw1);\n",
			TuneGroupName.c_str(),Name.c_str(),BaseValName.c_str());
			 */
			// fprintf(fp,"int dsp2giv%s%s(int npN,int para1st,int para2nd%s)\n",
			// TuneGroupName.c_str(),Name.c_str(),ArgStr.c_str());
			// fprintf(fpOutHeader,"int dsp2giv%s%s(int npN,int para1st,int para2nd%s);\n",
			// TuneGroupName.c_str(),Name.c_str(),ArgStr.c_str());
			// delete Script;
			// }else{
			// ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true,"","");
			/*
			fprintf(fp,"int dsp2giv%s%s(char *OAT_Routines,int %s,int *iBestSw1%s)\n",
			TuneGroupName.c_str(),Name.c_str(),BaseValName.c_str(),ArgStr.c_str());
			fprintf(fpOutHeader,"int dsp2giv%s%s(char *OAT_Routines,int %s,int *iBestSw1%s);\n",
			TuneGroupName.c_str(),Name.c_str(),BaseValName.c_str(),ArgStr.c_str());
			 */
			// fprintf(fp,"int dsp2giv%s%s(int npN,int para1st,int para2nd%s)\n",
			// TuneGroupName.c_str(),Name.c_str(),ArgStr.c_str());
			// fprintf(fpOutHeader,"int dsp2giv%s%s(int npN,int para1st,int para2nd%s);\n",
			// TuneGroupName.c_str(),Name.c_str(),ArgStr.c_str());
			//
			// }
			// iBsetSw1Str = "*iBestSw1";
			// }

			// ここまで流用　関数出力用------
			fprintf(fp, "{\n");

			// 変数宣言部分
			fprintf(fp, "\tint p3_1st=para1st*3;\n");
			fprintf(fp, "\tint p3_2nd=para2nd*3;\n");
			fprintf(fp, "\tint p3_2nd_2 = p3_2nd-2;\n");
			fprintf(fp, "\tint nn=(p3_1st-2)*p3_2nd_2;\n");
			fprintf(fp, "\tdouble DE[nn+1][nn+1];\n");
			fprintf(fp, "\tdouble R[nn+1][nn+1];\n");
			fprintf(fp, "\tdouble radius,cosine,sine;\n");
			fprintf(fp, "\tint p,q,q2;\n");
			fprintf(fp, "\tdouble data;\n");
			fprintf(fp, "\tint count=1;\n");
			fprintf(fp, "\tdouble x[nn];\n");
			fprintf(fp, "\tdouble temp;\n");
			fprintf(fp, "\tint temp2,bestP=0,nextP,prebestP;\n");
			fprintf(fp, "\tdouble select2[npN][2];\n");
			fprintf(fp, "\tint h[npN];\n");
			fprintf(fp, "\tint dspline_isw[npN];\n");
			fprintf(fp, "\tint kk=0;\n");
			fprintf(fp, "\tdouble alfa=0.1;\n");
			fprintf(fp, "\tdouble t1,t2;\n");
			fprintf(fp, "\tint F2[16];\n");
			fprintf(fp, "\tint i,j,k,i4;\n\n");

			// エクセル書き込み用部分
			fprintf(fp, "\tFILE *fpA;\n");
			fprintf(fp, "\tchar *fname = \"d-SplineData.csv\";\n");
			fprintf(fp, "\tfpA = fopen(fname,\"w\");\n\n");

			// DE初期化部分
			fprintf(fp,
				"\tfor(i=0;i<nn+1;i++){\n\t\tfor(j=0;j<nn+1;j++){\n\t\t\tDE[i][j]=0;\n\t\t\tR[i][j]=0;\n\t\t}\n\t}\n\n");

			// DE1,-2,1(1,1,-4,1,1)セット部分
			fprintf(fp, "\tj=0;\n\n");
			fprintf(fp,
				"\tfor(i=0;i<nn-4;i++){\n\t\tif(i<p3_2nd_2-2){\n\t\t\tDE[i][i]=1*alfa;\n\t\t\tDE[i][i+1]=-2*alfa;\n\t\t\tDE[i][i+2]=1*alfa;\n\t\t}\n\t\telse if(i>=nn-p3_2nd){\n\t\t\tDE[i][i+2]=1*alfa;\n\t\t\tDE[i][i+3]=-2*alfa;\n\t\t\tDE[i][i+4]=1*alfa;\n\t\t}\n\t\telse{\n\t\t\tif(j%%p3_2nd_2==0||(j+1)%%p3_2nd_2==0){\n\t\t\t\tDE[i][j]=1*alfa;\n\t\t\t\tDE[i][j+p3_2nd_2]=-2*alfa;\n\t\t\t\tDE[i][j+2*p3_2nd_2]=1*alfa;\n\t\t\t}\n\t\t\telse{\n\t\t\t\tDE[i][j]=1*alfa;\n\t\t\t\tDE[i][j+p3_2nd_2-1]=1*alfa;\n\t\t\t\tDE[i][j+p3_2nd_2]=-4*alfa;\n\t\t\t\tDE[i][j+p3_2nd_2+1]=1*alfa;\n\t\t\t\tDE[i][j+2*p3_2nd_2]=1*alfa;\n\t\t\t}\n\n\t\t\tj++;\n\t\t}\n\t}\n\n");

			/* DEをあらかじめ上三角行列に整形 */
			fprintf(fp,
				"\tq=0;\n\n\tfor(i=p3_2nd_2-2;i<nn-p3_2nd;i++){\n\t\tfor(j=0;j<p3_2nd_2-2;j++){\n");

			// G用計算部分
			fprintf(fp,
				"\t\t\tradius=sqrt( (DE[q+j][q+j])*(DE[q+j][q+j]) + (DE[i][q+j])*(DE[i][q+j]) );\n\n\t\t\tif(radius==0)\n\t\t\t\tcontinue;\n\n\t\t\tcosine=DE[q+j][q+j]/radius;\n\t\t\tsine=DE[i][q+j]/radius;\n\n");

			// R=回転行列*DEギブンス変換部分
			fprintf(fp,
				"\t\t\tfor(k=j;k<2*p3_2nd_2+1;k++){\n\t\t\t\tif(k==nn)\n\t\t\t\t\tbreak;\n\n\t\t\t\tR[q+j][q+k]+=cosine*DE[q+j][q+k];\n\t\t\t\tR[q+j][q+k]+=sine*DE[i][q+k];\n\n\t\t\t\tR[i][q+k]+=(-1)*sine*DE[q+j][q+k];\n\t\t\t\tR[i][q+k]+=cosine*DE[i][q+k];\n\n\t\t\t\tDE[q+j][q+k]=R[q+j][q+k];\n\t\t\t\tDE[i][q+k]=R[i][q+k];\n\n\t\t\t\tR[q+j][q+k]=0;\n\t\t\t\tR[i][q+k]=0;\n\t\t\t}\n");

			fprintf(fp, "\t\t}\n\n\t\tq++;\n\t}\n\n");
			/* 整形終了 */

			// h初期化部分
			fprintf(fp, "\tfor(i=0;i<=npN;i++){\n\t\th[i]=npN+1;\n\t}\n\n");

			// 初期16点決定部分
			fprintf(fp,
				"\tF2[0]=1;\n\tF2[1]=(2+para2nd)/3;\n\tF2[2]=(1+2*para2nd)/3;\n\tF2[3]=para2nd;\n\tF2[4]=((2+para1st)/3-1)*para2nd+1;\n\tF2[5]=((2+para1st)/3-1)*para2nd+(2+para2nd)/3;\n\tF2[6]=((2+para1st)/3-1)*para2nd+(1+2*para2nd)/3;\n\tF2[7]=((2+para1st)/3-1)*para2nd+para2nd;\n\tF2[8]=((1+2*para1st)/3-1)*para2nd+1;\n\tF2[9]=((1+2*para1st)/3-1)*para2nd+(2+para2nd)/3;\n\tF2[10]=((1+2*para1st)/3-1)*para2nd+(1+2*para2nd)/3;\n\tF2[11]=((1+2*para1st)/3-1)*para2nd+para2nd;\n\tF2[12]=(para1st-1)*para2nd+1;\n\tF2[13]=(para1st-1)*para2nd+(2+para2nd)/3;\n\tF2[14]=(para1st-1)*para2nd+(1+2*para2nd)/3;\n\tF2[15]=(para1st-1)*para2nd+para2nd;\n\n\tp=nn;\n\n");

			// パラメータの取りうる値が，d-Spline関数中ではどの点に対応するかを設定
			fprintf(fp,
				"\tfor(i=0;i<npN;i++)\n\t{\n\t\tdspline_isw[i]=3*i+(p3_2nd-3)*((int)(i/para2nd)*2);\n\t}\n\n");

			// 初期16点の計算用部分------------------------------------------------------
			fprintf(fp,
				"\tfor(i4=0;i4<16;i4++){\n\t\tq=F2[i4];\n\t\tq2=dspline_isw[q-1];\n\n");
			fprintf(fp, "\t\tt1 = omp_get_wtime();\n");
			// 計算する関数に飛ぶ部分	基本的に既存部分の流用
			if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
				fprintf(fp, "\t\t\tiloop_inner = 0;\n");
				fprintf(fp, "\t\t\twhile(OAT_Eecntl_Continue()){\n");
				s = "\t\t\t" + FuncName + "(";
			}
			else {
				s = "\t\t" + FuncName + "(";
			}
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str + ", ";
			}
			s += "q);";
			fprintf(fp, "%s\n", s.c_str());

			fprintf(fp, "\t\tt2 = omp_get_wtime();\n\n\t\tdata = t2 - t1;\n\n");
			fprintf(fp,
				"\t\tprintf(\"\\n---para is %%d time is %%lf--- \\n\",q,data);\n\n");
			fprintf(fp,
				"\t\tq--;\n\t\th[kk]=q;\n\n\t\tDE[p][q2]=1;\n\t\tDE[p][nn]=data;\n\n");

			// ------初期16点ギブンス変換for文部分ここから------
			fprintf(fp, "\t\tfor(q2=q2;q2<nn;q2++){\n\n");

			// G用計算部分
			fprintf(fp,
				"\t\t\tradius=sqrt( (DE[q2][q2])*(DE[q2][q2]) + (DE[p][q2])*(DE[p][q2]) );\n\n\t\t\tif(radius==0)\n\t\t\t\tcontinue;\n\n\t\t\tcosine=DE[q2][q2]/radius;\n\t\t\tsine=DE[p][q2]/radius;\n\n");

			// R=回転行列*DEギブンス変換部分
			fprintf(fp,
				"\t\t\tfor(i=q2;i<q2+2*p3_2nd_2+1;i++){\n\t\t\t\tif(i==p+1)\n\t\t\t\t\tbreak;\n\n\t\t\t\tR[q2][i]+=cosine*DE[q2][i];\n\t\t\t\tR[q2][i]+=sine*DE[p][i];\n\n\t\t\t\tR[p][i]+=(-1)*sine*DE[q2][i];\n\t\t\t\tR[p][i]+=cosine*DE[p][i];\n\n\t\t\t\tDE[q2][i]=R[q2][i];\n\t\t\t\tDE[p][i]=R[p][i];\n\n\t\t\t\tR[q2][i]=0;\n\t\t\t\tR[p][i]=0;\n\n\t\t\t\tif(i==q2+2*p3_2nd_2){\n\t\t\t\t\tR[q2][p]+=cosine*DE[q2][p];\n\t\t\t\t\tR[q2][p]+=sine*DE[p][p];\n\n\t\t\t\t\tR[p][p]+=(-1)*sine*DE[q2][p];\n\t\t\t\t\tR[p][p]+=cosine*DE[p][p];\n\n\t\t\t\t\tDE[q2][p]=R[q2][p];\n\t\t\t\t\tDE[p][p]=R[p][p];\n\n\t\t\t\t\tR[q2][p]=0;\n\t\t\t\t\tR[p][p]=0;\n\t\t\t\t}\n\t\t\t}\n");

			fprintf(fp, "\t\t}\n\n\t\tkk++;\n\t}\n\n");
			// ------初期16点ギブンス変換for文部分ここまで------

			// ------初期16点計算後、ベストパラメタと次のパラメタ決定ここから------

			// 推定値代入初期化部分
			fprintf(fp, "\tfor(i=0;i<nn;i++){\n\t\tx[i]=DE[i][nn];\n\t}\n\n");

			// 後退代入で推定値計算部分
			fprintf(fp,
				"\tfor(i=nn-1; i>=0;i--){\n\t\tfor(j=i+1;j<nn;j++){\n\t\t\tx[i]-=DE[i][j]*x[j];\n\t\t}\n\t\tx[i] /= DE[i][i];\n\t}\n\n");

			// デバッグ用、推定値表示部分
			fprintf(fp,
				"\tif(DEBUG>0){\n\t\tprintf(\"\\nestimation\\n\");\n\t\tfor(i=0;i<npN;i++){\n\t\t\tprintf(\"[%%d],%%lf\\n\",i+1,x[dspline_isw[i]]);\n\t\t}\n\t\tprintf(\"\\n\");\n\t}\n\n");

			// 推定値が最小となる番号をベストパラメタに設定する部分
			fprintf(fp,
				"\ttemp=x[0];\n\tbestP=0;\n\n\tfor(i=0;i<npN;i++){\n\t\tif(x[dspline_isw[i]]<temp){\n\t\t\ttemp=x[dspline_isw[i]];\n\t\t\tbestP=i;\n\t\t}\n\t}\n\n");
			fprintf(fp,
				"\tprintf(\"\\nbest para = %%d time = %%lf\\n\",bestP+1,x[dspline_isw[bestP]]);\n\n");

			// 選択基準2用配列初期化部分
			fprintf(fp,
				"\tfor(i=0;i<npN;i++){\n\t\tselect2[i][1]=i;\n\t\tselect2[i][0]=0;\n\t}\n\n");

			// 変化率計算部分
			fprintf(fp, "\tk=p3_2nd_2*3;\n\n");
			fprintf(fp,
				"\tfor(i=1;i<npN-1;i++){\n\t\tj=dspline_isw[i];\n\n\t\tif(i<para2nd-1||(para1st-1)*para2nd<i){\n\t\t\tselect2[i][0]=fabs( x[j-3] - 2*x[j] + x[j+3] );\n\t\t}\n\t\telse if((i%%para2nd==0)&&(i<(para1st-1)*para2nd)||((i+1)%%para2nd==0)&&i>para2nd){\n\t\t\tselect2[i][0]=fabs( x[j-k] - 2*x[j] + x[j+k] );\n\t\t}\n\t\telse if( i!=para2nd-1&&i!=(para1st-1)*para2nd){\n\t\t\tselect2[i][0]=fabs( x[j-3] + x[j-k] - 4*x[j] + x[j+k] + x[j+3] );\n\t\t}\n\t}\n\n");

			// select2ソート部分
			fprintf(fp,
				"\tfor(i=0 ; i<npN-1 ; i++){\n\t\tfor(j=npN-1 ; j>i ; j--){\n\t\t\tif(select2[j-1][0] < select2[j][0]){\n\t\t\t\ttemp=select2[j][0];\n\t\t\t\ttemp2=select2[j][1];\n\t\t\t\tselect2[j][0]=select2[j-1][0];\n\t\t\t\tselect2[j][1]=select2[j-1][1];\n\t\t\t\tselect2[j-1][0]=temp;\n\t\t\t\tselect2[j-1][1]=temp2;\n\t\t\t}\n\t\t}\n\t}\n\n");

			// 推定したベストPがすでに標本点にあるなら選択基準2、ないなら選択基準1　選択する部分
			fprintf(fp, "\tfor(i=0;i<npN;i++){\n");
			fprintf(fp,
				"\t\tif(bestP==h[i]){\n\t\t\tfor(j=0;j<npN;j++){\n\t\t\t\tfor(k=0;k<npN;k++){\n\t\t\t\t\tif(select2[j][1]==h[k]){\n\t\t\t\t\t\tbreak;\n\t\t\t\t\t}\n\t\t\t\t\tif(k+1==npN){\n\t\t\t\t\t\tnextP=select2[j][1];\n\t\t\t\t\t\tgoto OUT1;\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\t\t\tbreak;\n\t\t}\n");
			fprintf(fp, "\t\telse{\n\t\t\tnextP=bestP;\n\t\t}\n\t}\n\tOUT1:\n");
			fprintf(fp,
				"\tprintf(\"nextP=%%d bestP=%%d \\n\\n\",nextP+1,bestP+1);\n\n"
				);

			// prebestPに16点計算終了時のベストパラメタを代入する部分
			fprintf(fp, "\tprebestP=bestP;\n\n\n");

			// 推定値をエクセルに書き込みする部分
			fprintf(fp,
				"\tfor(i=0;i<nn;i++){\n\t\tfprintf(fpA,\"%%lf\\n\",x[i]);\n\t}\n\tfprintf(fpA,\"end\\n\\n\");\n\n");

			fprintf(fp,
				"\tprintf(\"\\n----------end 16 point---------- \\n\");\n\n");

			// --------16点後のdo while文ここから--------
			fprintf(fp, "\tdo{\n");
			fprintf(fp, "\t\tq=nextP;\n\t\tq2=dspline_isw[q];\n\n");
			fprintf(fp, "\t\tt1 = omp_get_wtime();\n");
			// 計算する関数に飛ぶ部分	基本的に既存部分の流用
			if (MainF->EECntlF) { // -eectrlのフラグ(ON時は、コントロール部分を置き換え）
				fprintf(fp, "\t\t\tiloop_inner = 0;\n");
				fprintf(fp, "\t\t\twhile(OAT_Eecntl_Continue()){\n");
				s = "\t\t\t" + FuncName + "(";
			}
			else {
				s = "\t\t" + FuncName + "(";
			}
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str + ", ";
			}
			s += "q+1);";
			fprintf(fp, "%s\n", s.c_str());

			fprintf(fp, "\t\tt2 = omp_get_wtime();\n\n");
			fprintf(fp, "\t\tdata = t2 - t1;\n\n");
			fprintf(fp,
				"\t\tprintf(\"\\n---para is %%d time is %%lf--- \\n\",q+1,data);\n\n");
			fprintf(fp,
				"\t\th[kk]=q;\n\n\t\tDE[p][q2]=1;\n\t\tDE[p][nn]=data;\n\n");

			// --------ギブンス変換for文ここから--------
			fprintf(fp, "\t\tfor(q=q;q<nn;q++){\n\n");

			// G用計算部分
			fprintf(fp,
				"\t\t\tradius=sqrt( (DE[q2][q2])*(DE[q2][q2]) + (DE[p][q2])*(DE[p][q2]) );\n\n\t\t\tif(radius==0)\n\t\t\t\tcontinue;\n\n\t\t\tcosine=DE[q2][q2]/radius;\n\t\t\tsine=DE[p][q2]/radius;\n\n");

			// R=回転行列*DEギブンス変換部分
			fprintf(fp,
				"\t\t\tfor(i=q2;i<q2+2*p3_2nd_2+1;i++){\n\t\t\t\tif(i==p+1)\n\t\t\t\t\tbreak;\n\n\t\t\t\tR[q2][i]+=cosine*DE[q2][i];\n\t\t\t\tR[q2][i]+=sine*DE[p][i];\n\n\t\t\t\tR[p][i]+=(-1)*sine*DE[q2][i];\n\t\t\t\tR[p][i]+=cosine*DE[p][i];\n\n\t\t\t\tDE[q2][i]=R[q2][i];\n\t\t\t\tDE[p][i]=R[p][i];\n\n\t\t\t\tR[q2][i]=0;\n\t\t\t\tR[p][i]=0;\n\n\t\t\t\tif(i==q2+2*p3_2nd_2){\n\t\t\t\t\tR[q2][p]+=cosine*DE[q2][p];\n\t\t\t\t\tR[q2][p]+=sine*DE[p][p];\n\n\t\t\t\t\tR[p][p]+=(-1)*sine*DE[q2][p];\n\t\t\t\t\tR[p][p]+=cosine*DE[p][p];\n\n\t\t\t\t\tDE[q2][p]=R[q2][p];\n\t\t\t\t\tDE[p][p]=R[p][p];\n\n\t\t\t\t\tR[q2][p]=0;\n\t\t\t\t\tR[p][p]=0;\n\t\t\t\t}\n\t\t\t}\n");

			fprintf(fp, "\n\t\t}\n\n\n");
			// --------ギブンス変換for文ここまで--------

			// ギブンス変換後、推定値計算、次の標本点決定部分
			// 推定値代入初期化部分
			fprintf(fp,
				"\t\tfor(i=0;i<nn;i++){\n\t\t\tx[i]=DE[i][nn];\n\t\t}\n\n");

			// 後退代入で推定値計算部分
			fprintf(fp,
				"\t\tfor(i=nn-1; i>=0;i--){\n\t\t\tfor(j=i+1;j<nn;j++){\n\t\t\t\tx[i]-=DE[i][j]*x[j];\n\t\t\t}\n\t\t\tx[i] /= DE[i][i];\n\t\t}\n\n");

			// デバッグ用、推定値表示部分
			fprintf(fp,
				"\t\tif(DEBUG>0){\n\t\t\tprintf(\"\\nestimation\\n\");\n\t\t\tfor(i=0;i<npN;i++){\n\t\t\t\tprintf(\"[%%d],%%lf\\n\",i+1,x[dspline_isw[i]]);\n\t\t\t\t}printf(\"\\n\");\n\t\t}\n\n");

			// 推定値が最小となる番号をベストパラメタに設定する部分
			fprintf(fp,
				"\t\ttemp=x[0];\n\t\tbestP=0;\n\n\t\tfor(i=0;i<npN;i++){\n\t\t\tif(x[dspline_isw[i]]<temp){\n\t\t\t\ttemp=x[dspline_isw[i]];\n\t\t\t\tbestP=i;\n\t\t\t}\n\t\t}\n\n");
			fprintf(fp,
				"\t\tprintf(\"\\nbest para = %%d time = %%lf\\n\",bestP+1,x[dspline_isw[bestP]]);\n\n");

			// 選択基準2用配列初期化部分
			fprintf(fp,
				"\t\tfor(i=0;i<npN;i++){\n\t\t\tselect2[i][1]=i;\n\t\t\tselect2[i][0]=0;\n\t\t}\n\n");

			// 変化率計算部分
			fprintf(fp, "\t\tk=p3_2nd_2*3;\n\n");
			fprintf(fp,
				"\t\tfor(i=1;i<npN-1;i++){\n\t\t\tj=dspline_isw[i];\n\n\t\t\tif(i<para2nd-1||(para1st-1)*para2nd<i){\n\t\t\t\tselect2[i][0]=fabs( x[j-3] - 2*x[j] + x[j+3] );\n\t\t\t}\n\t\t\telse if((i%%para2nd==0)&&(i<(para1st-1)*para2nd)||((i+1)%%para2nd==0)&&i>para2nd){\n\t\t\t\tselect2[i][0]=fabs( x[j-k] - 2*x[j] + x[j+k] );\n\t\t\t}\n\t\t\telse if( i!=para2nd-1&&i!=(para1st-1)*para2nd){\n\t\t\t\tselect2[i][0]=fabs( x[j-3] + x[j-k] - 4*x[j] + x[j+k] + x[j+3] );\n\t\t\t}\n\t\t}\n\n");

			// select2ソート部分
			fprintf(fp,
				"\t\tfor(i=0 ; i<npN-1 ; i++){\n\t\t\tfor(j=npN-1 ; j>i ; j--){\n\t\t\t\tif(select2[j-1][0] < select2[j][0]){\n\t\t\t\t\ttemp=select2[j][0];\n\t\t\t\t\ttemp2=select2[j][1];\n\t\t\t\t\tselect2[j][0]=select2[j-1][0];\n\t\t\t\t\tselect2[j][1]=select2[j-1][1];\n\t\t\t\t\tselect2[j-1][0]=temp;\n\t\t\t\t\tselect2[j-1][1]=temp2;\n\t\t\t\t}\n\t\t\t}\n\t\t}\n\n");

			// 推定したベストPがすでに標本点にあるなら選択基準2、ないなら選択基準1　選択する部分
			fprintf(fp, "\t\tfor(i=0;i<npN;i++){\n");
			fprintf(fp,
				"\t\t\tif(bestP==h[i]){\n\t\t\t\tfor(j=0;j<npN;j++){\n\t\t\t\t\tfor(k=0;k<npN;k++){\n\t\t\t\t\t\tif(select2[j][1]==h[k]){\n\t\t\t\t\t\t\tbreak;\n\t\t\t\t\t\t}\n\t\t\t\t\t\tif(k+1==npN){\n\t\t\t\t\t\t\tnextP=select2[j][1];\n\t\t\t\t\t\t\tgoto OUT2;\n\t\t\t\t\t\t}\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t\tbreak;\n\t\t\t}\n");
			fprintf(fp,
				"\t\t\telse{\n\t\t\t\tnextP=bestP;\n\t\t\t}\n\t\t}\n\t\tOUT2:\n"
				);
			fprintf(fp,
				"\t\tprintf(\"nextP=%%d bestP=%%d \\n\\n\",nextP+1,bestP+1);\n\n");

			// 前回推定したパラメタと同じならカウント+1違うならカウント+1に初期化する部分
			fprintf(fp,
				"\t\tif(prebestP==bestP){\n\t\t\tcount++;\n\t\t}\n\t\telse{\n\t\t\tcount=1;\n\t\t}\n\n");

			// 今回推定したベストパラメタをprebestPに設定
			fprintf(fp, "\t\tprebestP=bestP;\n\n");

			fprintf(fp, "\t\tkk++;\n\n");

			// 推定値をエクセルに書き込みする部分
			fprintf(fp,
				"\t\tfor(i=0;i<nn;i++){\n\t\t\tfprintf(fpA,\"%%lf\\n\",x[i]);\n\t\t}\n\t\tfprintf(fpA,\"end\\n\\n\");\n\n");

			fprintf(fp, "\t\tprintf(\"count = %%d \\n\",count);\n\n");

			// while文
			fprintf(fp,
				"\t}while(count<3);\n\n\tprintf(\"count end\\n\");\n\tprintf(\"usedParaNum = %%d\\n\",kk);\n\n");

			// --------16点後のdo while文ここまで--------

			fprintf(fp, "\tfclose(fpA);\n\n");
			fprintf(fp, "\treturn bestP+1;\n}\n");
		}
		// 2次元用d-spline（インストール時自動チューニング用）ここまで

		// 2次元用d-spline（実行時自動チューニング用）
		if (TuneGroup == tgDynamic) {

			// デバッグ用と関数名部分
			fprintf(fp, "#define DEBUG 1\n");

			fprintf(fp,
				"int dynamicDspline2%s%s(int npN, int para1st, int para2nd, int *isw, int Dswitch, double data)\n", TuneGroupName.c_str(), Name.c_str());
			fprintf(fpOutHeader,
				"int dynamicDspline2%s%s(int npN, int para1st, int para2nd, int *isw, int Dswitch, double data);\n", TuneGroupName.c_str(), Name.c_str());

			fprintf(fp, "{\n");

			int CaseArrayCount2[2];
			CaseArrayCount2[0] = (int)((variedToValue[0] - variedFromValue[0])
				/ variedStepValue[0] + 1);
			CaseArrayCount2[1] = (int)((variedToValue[1] - variedFromValue[1])
				/ variedStepValue[1] + 1);
			int nnn = (3 * CaseArrayCount2[0] - 2) *
				(3 * CaseArrayCount2[1] - 2);

			// 変数宣言部分
			fprintf(fp,
				"\tint p3_1st=para1st*3;\n\tint p3_2nd=para2nd*3;\n\tint p3_2nd_2=p3_2nd-2;\n\n");
			fprintf(fp, "\tint nn=(p3_1st-2)*p3_2nd_2;\n");
			fprintf(fp,
				"\tstatic double DE[%d+1][%d+1];\n\tstatic double R[%d+1][%d+1];\n", nnn, nnn, nnn, nnn);
			fprintf(fp,
				"\tdouble radius,cosine,sine;\n\tint p,q,q2;\n\tstatic int count=1;\n\t");
			fprintf(fp,
				"static int count2=0;\n\tdouble x[nn];\n\tdouble temp;\n\tint temp2;\n");
			fprintf(fp,
				"\tint bestP=0;\n\tint nextP;\n\tstatic int prebestP=0;\n\tdouble select2[npN][2];\n");
			fprintf(fp,
				"\tstatic int h[%d+1];\n\tstatic int dspline_isw[%d];\n\tstatic int kk=0;\n\tdouble alfa=0.1;\n\tstatic int Fcount=0;\n", CaseArrayCount2[0] * CaseArrayCount2[1], CaseArrayCount2[0] * CaseArrayCount2[1]);
			fprintf(fp, "\tint i,j,k;\n\n");

			// エクセル書き込み用部分
			fprintf(fp, "\tFILE *fpA;\n");
			fprintf(fp, "\tchar *fname = \"d-SplineData.csv\";\n");
			fprintf(fp, "\tfpA = fopen(fname,\"a\");\n\n");

			// パラメタ探索後の終了部分
			fprintf(fp, "\tif(count2==1){\n\t\treturn 0;\n\t}\n\n");

			fprintf(fp,
				"\tif(count==3){\n\t\tprintf(\"count=3 end\\n\");\n\t\tprintf(\"usedParaNums=%%d\\n\", Fcount-1);\n\t\t*isw = prebestP+1;\n");
			fprintf(fp, "\t\tcount2=1;\n\t\treturn 0;\n\t}\n\n");

			// h初期化部分
			fprintf(fp,
				"\tif(Fcount==0){\n\t\tfor(i=0;i<=npN;i++){\n\t\t\th[i]=npN+1;\n\t\t}\n\t}\n\n\tp=nn;\n\n");

			// Dswitch1部分ここから--------------------
			fprintf(fp, "\tif(Dswitch==1){\n\n");

			// switch部分ここから
			// 1  9 13   5
			// 16  3  7  12
			// 11  8  4  15
			// 6 14 10   2
			fprintf(fp,
				"\t\tswitch(Fcount){\n\t\tcase 0:\n\t\t\t*isw = 1;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 1:\n\t\t\t*isw = (2+para2nd)/3;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 2:\n\t\t\t*isw = (1+2*para2nd)/3;\n\t\t\tbreak;\n");
			fprintf(fp, "\t\tcase 3:\n\t\t\t*isw = para2nd;\n\t\t\tbreak;\n");

			fprintf(fp,
				"\t\tcase 4:\n\t\t\t*isw = ((2+para1st)/3-1)*para2nd+1;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 5:\n\t\t\t*isw = ((2+para1st)/3-1)*para2nd+(2+para2nd)/3;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 6:\n\t\t\t*isw = ((2+para1st)/3-1)*para2nd+(1+2*para2nd)/3;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 7:\n\t\t\t*isw = ((2+para1st)/3-1)*para2nd+para2nd;\n\t\t\tbreak;\n");

			fprintf(fp,
				"\t\tcase 8:\n\t\t\t*isw = ((1+2*para1st)/3-1)*para2nd+1;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 9:\n\t\t\t*isw = ((1+2*para1st)/3-1)*para2nd+(2+para2nd)/3;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 10:\n\t\t\t*isw = ((1+2*para1st)/3-1)*para2nd+(1+2*para2nd)/3;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 11:\n\t\t\t*isw = ((1+2*para1st)/3-1)*para2nd+para2nd;\n\t\t\tbreak;\n");

			fprintf(fp,
				"\t\tcase 12:\n\t\t\t*isw = (para1st-1)*para2nd+1;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 13:\n\t\t\t*isw = (para1st-1)*para2nd+(2+para2nd)/3;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 14:\n\t\t\t*isw = (para1st-1)*para2nd+(1+2*para2nd)/3;\n\t\t\tbreak;\n");
			fprintf(fp,
				"\t\tcase 15:\n\t\t\t*isw = (para1st-1)*para2nd+para2nd;\n\t\t\tbreak;\n");

			// default部分
			fprintf(fp, "\t\tdefault:\n\n");
			// 推定値初期化部分
			fprintf(fp,
				"\t\t\tfor(i=0;i<nn;i++){\n\t\t\t\tx[i]=DE[i][nn];\n\t\t\t}\n\n"
				);

			// 後退代入計算部分
			fprintf(fp,
				"\t\t\tfor(i=nn-1; i>=0;i--){\n\t\t\t\tfor(j=i+1;j<nn;j++){\n\t\t\t\t\tx[i]-=DE[i][j]*x[j];\n\t\t\t\t}\n\n\t\t\t\tx[i] /= DE[i][i];\n\t\t\t}\n\n");

			// デバッグ用推定値表示部分
			fprintf(fp,
				"\t\t\tif(DEBUG>0){\n\t\t\t\tprintf(\"\\nestimation\\n\");\n\n\t\t\t\tfor(i=0;i<npN;i++){\n");
			fprintf(fp,
				"\t\t\t\t\tprintf(\"[%%d],%%lf\\n\",i+1,x[dspline_isw[i]]);\n\t\t\t\t}\n\n\t\t\t\tprintf(\"\\n\");\n\t\t\t}\n\n");

			// 最小推定値探索部分
			fprintf(fp,
				"\t\t\ttemp=x[dspline_isw[0]];\n\t\t\tbestP=0;\n\n\t\t\tfor(i=0;i<npN;i++){\n\t\t\t\tj=dspline_isw[i];\n\n\t\t\t\tif(x[j]<temp){\n");
			fprintf(fp,
				"\t\t\t\t\ttemp=x[j];\n\t\t\t\t\tbestP=i;\n\t\t\t\t}\n\t\t\t}\n"
				);

			// ベストパラメタ表示部分
			fprintf(fp,
				"\t\t\tprintf(\"\\nbest para = %%d time = %%lf\\n\",bestP+1,x[dspline_isw[bestP]]);\n\n");

			// 選択基準2初期化部分
			fprintf(fp,
				"\t\t\tfor(i=0;i<npN;i++){\n\t\t\t\tselect2[i][1]=i;\n\t\t\t\tselect2[i][0]=0;\n\t\t\t}\n\n");

			// 変化率計算部分
			fprintf(fp, "\t\t\tk = p3_2nd_2*3;\n\n");
			fprintf(fp,
				"\t\t\tfor(i=1;i<npN-1;i++){\n\t\t\t\tj=dspline_isw[i];\n\n\t\t\t\tif(i<para2nd-1||(para1st-1)*para2nd<i){\n\t\t\t\t\tselect2[i][0]=fabs( x[j-3] - 2*x[j] + x[j+3] );\n\t\t\t\t}\n\t\t\t\telse if((i%%para2nd==0)&&(i<(para1st-1)*para2nd)||((i+1)%%para2nd==0)&&i>para2nd){\n\t\t\t\t\tselect2[i][0]=fabs( x[j-k] - 2*x[j] + x[j+k] );\n\t\t\t\t}\n\t\t\t\telse if(i!=para2nd-1&&i!=(para1st-1)*para2nd){\n\t\t\t\t\tselect2[i][0]=fabs( x[j-3] + x[j-k] - 4*x[j] + x[j+k] + x[j+3] );\n\t\t\t\t}\n\t\t\t}\n\n");

			// select2ソート部分
			fprintf(fp,
				"\t\t\tfor(i=0 ; i<npN-1 ; i++){\n\t\t\t\tfor(j=npN-1 ; j>i ; j--){\n\t\t\t\t\tif(select2[j-1][0] < select2[j][0]){\n\t\t\t\t\t\ttemp=select2[j][0];\n\t\t\t\t\t\ttemp2=select2[j][1];\n\t\t\t\t\t\tselect2[j][0]=select2[j-1][0];\n\t\t\t\t\t\tselect2[j][1]=select2[j-1][1];\n\t\t\t\t\t\tselect2[j-1][0]=temp;\n\t\t\t\t\t\tselect2[j-1][1]=temp2;\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\n");

			// 選択基準1or2選択部分
			fprintf(fp,
				"\t\t\tfor(i=0;i<npN;i++){\n\t\t\t\tif(bestP==h[i]){\n\t\t\t\t\tfor(j=0;j<npN;j++){\n\t\t\t\t\t\tfor(k=0;k<npN;k++){\n\t\t\t\t\t\t\tif(select2[j][1]==h[k]){\n\t\t\t\t\t\t\t\tbreak;\n\t\t\t\t\t\t\t}\n\t\t\t\t\t\t\tif(k+1==npN){\n\t\t\t\t\t\t\t\tnextP=select2[j][1];\n\t\t\t\t\t\t\t\tgoto OUT1;\n\t\t\t\t\t\t\t}\n\t\t\t\t\t\t}\n\t\t\t\t\t}\n\t\t\t\t\tbreak;\n\t\t\t\t}\n\t\t\t\telse{\n\t\t\t\t\tnextP=bestP;\n\t\t\t\t}\n\t\t\t}\n\t\t\tOUT1:\n");
			// ベストパラメタと次のパラメタを表示
			fprintf(fp,
				"\t\t\tprintf(\"nextP=%%d bestP=%%d \\n\\n\",nextP+1,bestP+1);\n");

			// iswに次使うパラメタをセットする部分
			fprintf(fp, "\t\t\t*isw = nextP+1;\n\n");

			// 3回カウント用部分
			fprintf(fp,
				"\t\t\tif(prebestP==bestP){\n\t\t\t\tcount++;\n\t\t\t}\n");
			fprintf(fp,
				"\t\t\telse{\n\t\t\t\tcount=1;\n\t\t\t}\n\t\t\tprebestP=bestP;\n\n");

			// エクセルに書き込みをする部分
			fprintf(fp,
				"\t\t\tfor(i=0;i<nn;i++){\n\t\t\t\tfprintf(fpA,\"%%lf\\n\",x[i]);\n\t\t\t}\n\t\t\tfprintf(fpA,\"end\\n\\n\");\n\n");

			// default部分ここまで
			fprintf(fp, "\t\t\tbreak;\n");

			// switchここまで
			fprintf(fp, "\t\t}\n\t\tFcount++;\n");

			// Dswitch1ここまで------------
			fprintf(fp, "\t}\n\n");

			// Dswitch2ここから-----------------

			fprintf(fp, "\tif(Dswitch == 2){\n\n");

			// DE初期化部分
			fprintf(fp,
				"\t\tif(Fcount == 1){\n\t\t\tfor(i=0;i<nn+1;i++){\n\t\t\t\tfor(j=0;j<nn+1;j++){\n\t\t\t\t\tDE[i][j]=0;\n\t\t\t\t\tR[i][j]=0;\n\t\t\t\t}\n\t\t\t}\n\n\t\t\tj=0;\n\n");
			fprintf(fp,
				"\t\t\tfor(i=0;i<nn-4;i++){\n\t\t\t\tif(i<p3_2nd_2-2){\n\t\t\t\t\tDE[i][i]=1*alfa;\n\t\t\t\t\tDE[i][i+1]=-2*alfa;\n\t\t\t\t\tDE[i][i+2]=1*alfa;\n\t\t\t\t}\n");
			fprintf(fp,
				"\t\t\t\telse if(i>=nn-p3_2nd){\n\t\t\t\t\tDE[i][i+2]=1*alfa;\n\t\t\t\t\tDE[i][i+3]=-2*alfa;\n\t\t\t\t\tDE[i][i+4]=1*alfa;\n\t\t\t\t}\n");
			fprintf(fp,
				"\t\t\t\telse{\n\t\t\t\t\tif((i+2)%%p3_2nd_2==0||(i+3)%%p3_2nd_2==0){\n\t\t\t\t\t\tDE[i][j]=1*alfa;\n\t\t\t\t\t\tDE[i][j+p3_2nd_2]=-2*alfa;\n\t\t\t\t\t\tDE[i][j+2*p3_2nd_2]=1*alfa;\n\t\t\t\t\t}\n");
			fprintf(fp,
				"\t\t\t\t\telse{\n\t\t\t\t\t\tDE[i][j]=1*alfa;\n\t\t\t\t\t\tDE[i][j+p3_2nd_2-1]=1*alfa;\n\t\t\t\t\t\tDE[i][j+p3_2nd_2]=-4*alfa;\n\t\t\t\t\t\tDE[i][j+p3_2nd_2+1]=1*alfa;\n\t\t\t\t\t\tDE[i][j+2*p3_2nd_2]=1*alfa;\n\t\t\t\t\t}\n\n");
			fprintf(fp, "\t\t\t\t\tj++;\n\t\t\t\t}\n\t\t\t}\n\n");

			// パラメータの取りうる値が，d-Spline関数中ではどの点に対応するかを設定
			fprintf(fp,
				"\t\t\tfor(i=0;i<npN;i++)\n\t\t\t{\n\t\t\t\tdspline_isw[i]=3*i+(p3_2nd-3)*((int)(i/para2nd)*2);\n\t\t\t}\n\n");

			/* DEをあらかじめ上三角行列に整形 */
			fprintf(fp,
				"\t\t\tq=0;\n\n\t\t\tfor(i=p3_2nd_2-2;i<nn-p3_2nd;i++){\n\t\t\t\tfor(j=0;j<p3_2nd_2-2;j++){\n");

			// G用計算部分
			fprintf(fp,
				"\t\t\t\t\tradius=sqrt( (DE[q+j][q+j])*(DE[q+j][q+j]) + (DE[i][q+j])*(DE[i][q+j]) );\n\n\t\t\t\t\t\tif(radius==0)\n\t\t\t\t\t\tcontinue;\n\n\t\t\t\t\tcosine=DE[q+j][q+j]/radius;\n\t\t\t\t\tsine=DE[i][q+j]/radius;\n\n");

			// R=回転行列*DEギブンス変換部分
			fprintf(fp,
				"\t\t\t\t\tfor(k=j;k<2*p3_2nd_2+1;k++){\n\t\t\t\t\t\tif(k==nn)\n\t\t\t\t\t\t\tbreak;\n\n\t\t\t\t\t\tR[q+j][q+k]+=cosine*DE[q+j][q+k];\n\t\t\t\t\t\tR[q+j][q+k]+=sine*DE[i][q+k];\n\n\t\t\t\t\t\tR[i][q+k]+=(-1)*sine*DE[q+j][q+k];\n\t\t\t\t\t\tR[i][q+k]+=cosine*DE[i][q+k];\n\n\t\t\t\t\t\tDE[q+j][q+k]=R[q+j][q+k];\n\t\t\t\t\t\tDE[i][q+k]=R[i][q+k];\n\n\t\t\t\t\t\tR[q+j][q+k]=0;\n\t\t\t\t\t\tR[i][q+k]=0;\n\t\t\t\t\t}\n");

			fprintf(fp, "\t\t\t\t}\n\n\t\t\t\tq++;\n\t\t\t}\n\n");
			/* 整形終了 */

			fprintf(fp, "\t\t}\n\n");

			// 標本点とその実測値のセット部分
			fprintf(fp,
				"\t\tq = *isw;\n\t\tq--;\n\t\tq2=dspline_isw[q];\n\t\th[kk]=q;\n\n\t\tDE[p][q2]=1;\n\t\tDE[p][nn]=data;\n\n");

			// ギブンス変換ループ部分-------------------

			fprintf(fp, "\t\tfor(q2=q2;q2<nn;q2++){\n\n");

			// G用計算部分
			fprintf(fp,
				"\t\t\tradius=sqrt( (DE[q2][q2])*(DE[q2][q2]) + (DE[p][q2])*(DE[p][q2]) );\n\n\t\t\tif(radius==0)\n\t\t\t\tcontinue;\n\n\t\t\tcosine=DE[q2][q2]/radius;\n\t\t\tsine=DE[p][q2]/radius;\n\n");

			// R=回転行列*DEギブンス変換部分
			fprintf(fp,
				"\t\t\tfor(i=q2;i<q2+2*p3_2nd_2+1;i++){\n\t\t\t\tif(i==p+1)\n\t\t\t\t\tbreak;\n\n\t\t\t\tR[q2][i]+=cosine*DE[q2][i];\n\t\t\t\tR[q2][i]+=sine*DE[p][i];\n\n\t\t\t\tR[p][i]+=(-1)*sine*DE[q2][i];\n\t\t\t\tR[p][i]+=cosine*DE[p][i];\n\n\t\t\t\tDE[q2][i]=R[q2][i];\n\t\t\t\tDE[p][i]=R[p][i];\n\n\t\t\t\tR[q2][i]=0;\n\t\t\t\tR[p][i]=0;\n\n");
			fprintf(fp,
				"\t\t\t\tif(i==q2+2*p3_2nd_2){\n\t\t\t\t\tR[q2][p]+=cosine*DE[q2][p];\n\t\t\t\t\tR[q2][p]+=sine*DE[p][p];\n\n\t\t\t\t\tR[p][p]+=(-1)*sine*DE[q2][p];\n\t\t\t\t\tR[p][p]+=cosine*DE[p][p];\n\n\t\t\t\t\tDE[q2][p]=R[q2][p];\n\t\t\t\t\tDE[p][p]=R[p][p];\n\n\t\t\t\t\tR[q2][p]=0;\n\t\t\t\t\tR[p][p]=0;\n\t\t\t\t}\n\t\t\t}\n\n");

			// ギブンス変換ループここまで------------------

			fprintf(fp, "\t\t}\n\n\t\tkk++;\n");

			// Dswitch2ここまで
			fprintf(fp, "\t}\n\n");

			fprintf(fp, "\tfclose(fpA);\n\n");

			// 関数ここまで
			fprintf(fp, "\treturn 0;\n}\n\n");
		}
		// 2次元用d-spline（実行時自動チューニング用）ここまで
	}
	//
	// ここまで
	//
	/** ********************************************************************************************************************** */

	delete DefValNameList;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
//
// 3.概要
// OAT_GGGFFF(...) のswich()付きの実行コードを生成する。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputExecCode_Fortran(FILE *fp) {
	string s, s2;
	int i, j;
	TValData *ValData;
	TToken *Token;
	int DefPosS;
	char cs[1024];
	string ValDefStr;

	RestoreValBits(TokenStartPos, TokenEndPos);
	s = "      subroutine " + FuncName + "(";
	for (i = 0; i < ArgValList->Count; i++) {
		if (ArgValList->Strings[i] == "") {
			continue;
		}
		ValData = (TValData*)ArgValList->Objects[i];
		s2 = s + ValData->Str + ", ";
		if (s2.length() > 72) {
			if (MainF->SrcCodeType == MainF->sctFortran77) {
				fprintf(fp, "%s\n", SepLongStr(s).c_str());
			}
			else {
				fprintf(fp, "%s &\n", SepLongStr(s).c_str());
			}
			s = "     &" + ValData->Str + ", ";
		}
		else {
			s = s2;
		}
	}
	s += "iusw1)";
	fprintf(fp, "%s", SepLongStr(s).c_str());
	fprintf(fp, "\n");
	//
	// 使用される引数の定義を出力する。
	// DefPosS,E,ArrayDefPosS,Eを使用する。
	// DefPosSが同じ変数は、１つにまとめること。
	//
	DefPosS = -1;
	s = "";
	for (i = 0; i < ArgValList->Count; i++) {
		if (ArgValList->Strings[i] == "") {
			continue;
		}
		ValData = (TValData*)ArgValList->Objects[i];
		if ((DefPosS == -1) || (ValData->DefPosS != DefPosS)) { // 違う定義行
			if (s != "") {
				s = SepLongStr(s);
				fprintf(fp, "%s\n", s.c_str());
				s = "";
			}
			s = ValData->GetDefStr_Fortran();
			if (s == "") {
				continue;
			}
			DefPosS = ValData->DefPosS;
		}
		else {
			s += ",";
		}
		s += " " + ValData->Str;
		for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
			Token = (TToken*)TokenList->Items[j];
			s += Token->OrgStr;
		}
	}
	if (s != "") {
		s = SepLongStr(s);
		fprintf(fp, "%s\n", s.c_str());
		s = "";
	}
	fprintf(fp, "      integer iusw1\n");

	/** ***************************************************************************************************************************************************************** */
	//
	// Kogakuin Irie
	// チューニング領域内で使用されている変数で，自動宣言されない変数を自動宣言させる処理
	// character型の変数に自動宣言されない傾向が見られる
	// しかし，上記の可能性は低いと思われるので，不要になったら以下はコメントアウトにする
	// ppOpen-AT内のコードを引用
	//
#if 0	// variedD(G,V) のサンプルで定義が複数回出るので、コメントアウトした。2016/03/06

	TToken *Token2;
	int k, l, tokPoint;
	int p, strPoint;
	bool paraDef, paraDef2, display;
	char c;

	// 以下のfor文はregion start から region end までの範囲のトークンを取得
	for (i = TokenStartPos; i <= TokenEndPos; i++) {
		Token2 = (TToken*)TokenList->Items[i];

		// 以下のfor文はプログラムの先頭からregion startまでの範囲のトークンを取得
		// Tokenに入れたいトークンは，変数宣言部分のトークン
		for (j = 0; j < TokenStartPos; j++) {
			Token = (TToken*)TokenList->Items[j];

			// 2つのトークン名が一致し，それがtid_Valならば実行
			// このif文でしたいことは，チューニング対象内の変数の型を，プログラムの宣言部から見つけること
			//
			/** ***********************************注意************************************ */
			// このif文を通り抜けるトークンは「変数名」と「関数名」と「サブルーチン名」の3つ
			// そのため，「変数名」のトークンだけを取得する処理が必要
			/** ***********************************注意************************************ */
			// すでに自動宣言されている変数もあるため，二重宣言を回避する必要がある
			/** ***********************************注意************************************ */
			// varied で指定されている変数も，if文を通り抜ける（対処が必要）
			if (Token->OrgStr == Token2->OrgStr && Token->TokId == tid_Val) {

				tokPoint = j;
				strPoint = 0;
				paraDef = false;
				paraDef2 = false;
				display = false;

				/* ------------------------------------------- */
				// variedの変数名との比較を行う
				// 一致したら「paraDef」を「true」に
				for (k = 0; k < variedCount; k++) {
					if (variedValName[k] == Token->Str) {
						k = variedCount;
						paraDef = true;
					}
				}
				/* ------------------------------------------- */

				/* --------------------------------------------------------------------------------------------- */
				// 自動宣言されている変数名との比較を行う
				// 一致したら「paraDef2」を「true」に

				// 引数の変数との比較
				if (MainF->Call_ATExec_Script != NULL) {
					s = MainF->Call_ATExec_Script->GetATExecArgStr(false, "",
						"");
				}
				else {
					TScript *Script = new TScript(MainF->TokenList, 0, NULL,
						ValDataList);
					s = Script->GetATExecArgStr(false, "", "");
					delete Script;
				}

				for (p = 1; p < (int)s.length(); p++) {
					// s[0]には","が入っているため，ループ変数は1からスタート

					c = s[p];
					if (c == ',') {
						// 1つ前の','の後の文字から，次の','の前の文字までを抜き出す
						s2 = s.substr(strPoint + 1, p - strPoint - 1);

						if (s2 == Token->Str) {
							paraDef2 = true;
						}

						strPoint = p;
					}
					else if (p == (int)s.length() - 1) {
						// 1つ前の','の後の文字から，最後の文字までを抜き出す
						s2 = s.substr(strPoint + 1);

						if (s2 == Token->Str) {
							paraDef2 = true;
						}
					}
				}
				// 引数の変数との比較　ここまで

				// 引数以外の変数との比較
				for (k = 0; k < ArgValList->Count; k++) {
					if (ArgValList->Strings[k] != "") {
						continue;
					}
					ValData = (TValData*)ArgValList->Objects[k];

					if (ValData->Str == Token->Str) {
						k = ArgValList->Count;
						paraDef2 = true;
					}
				}
				// 引数以外の変数との比較　ここまで
				/* --------------------------------------------------------------------------------------------- */

				// このif文の中で自動宣言を行う
				if (!paraDef && !paraDef2) {
					// 現在のトークンの前にある改行を探すため，デクリメントしていく
					// k > 0 という終了条件だが，"\n"が見つかったらループを抜ける
					for (k = j - 1; k > 0; k--) {
						Token = (TToken*)TokenList->Items[k];

						if (Token->OrgStr == "\n") {
							s = "";

							// 改行が見つかったら，改行後のトークンからTokenまでのトークンを
							// 取得して1つの文字列とする（型の宣言から取得したいため）
							for (l = k + 1; l <= tokPoint; l++) {
								Token = (TToken*)TokenList->Items[l];

								// 「変数名」以外のトークンを除去
								// 「変数名」のトークンは，最初に型宣言があるため，型のトークンかどうかで判断
								// if( Token->Str == "integer" || Token->Str == "real" || Token->Str == "double" || Token->Str == "doubleprecision"
								// || Token->Str == "character" || Token->Str == "logical" || Token->Str == "complex" ){
								if
									(Token->TokId == tid_INTEGER ||
									Token->TokId == tid_REAL ||
									Token->TokId == tid_DOUBLEPRECISION ||
									Token->TokId == tid_CHARACTER ||
									Token->TokId == tid_LOGICAL ||
									Token->TokId == tid_COMPLEX) {
									display = true;
								}
								if (display) {
									if (l == k + 1) {
										s += Token->Str;
									}
									else {
										s += Token->OrgStr;
									}
								}
							}

							// Tokenの次に','がある場合,変数が続くことがわかる
							// その変数もまとめて宣言させるため,このタイミングでは宣言させない
							Token = (TToken*)TokenList->Items[l];
							if (Token->Str == ",")
								break;

							if (display) {
								fprintf(fp, "      %s\n", s.c_str());
							}
							break;
						}
					}
				}

				// TokenとToken2の比較が終了したため，次のToken2を取得する
				// そのため，jのforループを強制的に終わらせる
				j = TokenStartPos;
			}
		}
	}
	//
	// ここまで
	//
	/** ***************************************************************************************************************************************************************** */
#endif
	//
	// 引数以外の変数の変数宣言を追加する。 Selectの場合のみ有効 Add 2013/03/10
	// DO変数は対象外。DO依存変数は、名前を変えて複数にする。
	//
//	if (TuneKind == tkSelect || TuneKind == tkVariable) { // Select
	if ((TuneKind == tkSelect) || (TuneKind == tkVariable)||
		((TuneKind >= tkList) && (TuneKind <= tkList_End)) ) { // Select
		fprintf(fp, "\n");
		DefPosS = -1;
		s = "";
		ValDefStr = "";

		for (i = 0; i < ArgValList->Count; i++) {
			if (ArgValList->Strings[i] != "") { // 引数の変数は対象外
				continue;
			}
			ValData = (TValData*)ArgValList->Objects[i];
			if (ValData->DefPosS == -1) { // 定義なし
				continue;
			}
			if (ValData->DefPosS != DefPosS) { // 違う定義行
				if (s != "") {
					s = SepLongStr(s);
					fprintf(fp, "%s\n", SepLongStr(s).c_str());
					s = "";
				}
				for (j = ValData->DefPosS; j < ValData->DefPosE; j++) {
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
				}
				DefPosS = ValData->DefPosS;
				ValDefStr = s;
			}
			else {
				if (s == "") {
					s = ValDefStr;
				}
				else {
					s += ",";
				}
			}
			if (s == "") {
				s = ValDefStr;
			}
			s += " " + ValData->Str;
			for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
				Token = (TToken*)TokenList->Items[j];
				s += Token->OrgStr;
			}
		}
		if (s != "") {
			s = SepLongStr(s);
			fprintf(fp, "%s\n", SepLongStr(s).c_str());
			s = "";
		}
	}
	fprintf(fp, "\n");
	fprintf(fp, "      select case(iusw1)\n");

	for (i = 1; i <= CaseCount; i++) {
		if ((i % 10) == 0) {
			// MainF->print("Output Code "+IntToStr(i)+" / "+IntToStr(CaseCount));
			if (MainF->CloseReqF) {
				return;
			}
		}
		fprintf(fp, "        case(%d)\n", i);
		s = "";
		for (j = 0; j < ArgValList->Count; j++) {
			if (ArgValList->Strings[j] == "") {
				continue;
			}
			ValData = (TValData*)ArgValList->Objects[j];
			if (s != "") {
				s += ", ";
			}
			s += ValData->Str;
		}
		if ((TuneKind == tkSelect) ||
			((TuneKind >= tkList) && (TuneKind <= tkList_End)) ) { // Select
			// Select時は、ソースコード（NestしたRegionがあれば置換する）
			// さらに、先に別のネストがある可能性もあるので注意
			OutputSelectCaseExecCode_Fortran(fp, i);
		}
		else { // それ以外のUnRollやフィージョン等は、サブルーチン集となる
			sprintf(cs, "           call %s_%d(", FuncName.c_str(), i);
			s = cs + s + ")";
			s = SepLongStr(s);
			fprintf(fp, "%s\n", s.c_str());
		}
	}
	fprintf(fp, "      end select\n");
	fprintf(fp, "\n");
	fprintf(fp, "      return\n");
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "      end subroutine %s\n", FuncName.c_str());
	}
	else {
		fprintf(fp, "      end\n"); // Change 2013/06/20
	}
	fprintf(fp, "\n");
	fprintf(fp, "\n");

	switch(TuneKind) {
	case tkUnroll: // UnRoll時は、サブルーチン集となる
		OutputUnrollExecCode_Fortran(fp);
		break;
	case tkLoopFusionSplit: // LoopFusionSplit
	case tkLoopFusion:
	case tkLoopSplit:
	case tkRotationOrder:
		OutputLoopFusionExecCode_Fortran(fp);
		break;
	default:
		break;
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
//
// 3.概要
// OAT_GGGFFF(...) のswich()付きの実行コードを生成する。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputExecCode_C(FILE *fp, FILE *fpOutHeader) {
	string s, s2, ValName;
	int i, j;
	TValData *ValData;
	TToken *Token;
	TStringList *DefValStrList;

	DefValStrList = new TStringList;
	RestoreValBits(TokenStartPos, TokenEndPos);
	fprintf(fp, "\n");
	s = "int " + FuncName + "(";
	for (i = 0; i < ArgValList->Count; i++) {
		if (ArgValList->Strings[i] == "") {
			continue;
		}
		ValData = (TValData*)ArgValList->Objects[i];
		/*
		Cの場合は、グローバル変数であっても、引数で渡す形に修正。 2012/03/20
		if((ValData->ArgF == false)&&((ValData->BrNestLevel == 0)||(ValData->DefPosS == -1))){
		//			MainF->print("グローバル変数1 =  "+ValData->Str);
		continue;	// グローバル変数は定義済みとしてSkip 2011/07/28
		}
		 */
		DefValStrList->Add(ValData->Str);
		// s += GetDefStr_C(ValData)+ " "+ValData->Str;
		s += ValData->GetDefStr_C() + ValData->Str;
		for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
			Token = (TToken*)TokenList->Items[j];
			s += Token->OrgStr;
		}
		s += ",";
	}
	s += "int iusw1)";
	fprintf(fp, "%s\n", s.c_str());
	fprintf(fpOutHeader, "%s;\n", s.c_str()); // ヘッダ
	fprintf(fp, "{\n");

	//
	// UnRoll時以外は、対象内の変数を定義。
	// ただし、引数で定義されている変数は対象外となる。
	//
	if ((TuneKind == tkSelect) || (TuneKind == tkVariable)||
		((TuneKind >= tkList) && (TuneKind <= tkList_End)) ) { // Select
		TValData *ValData;

		for (i = TokenStartPos; i <= TokenEndPos; i++) {
			ValData = NULL;
			Token = (TToken*)TokenList->Items[i];
			if (Token->TokId == tid_Val) {
				ValData = (TValData*)Token->ValData;
			}
			if (ValData == NULL) {
				continue;
			}
			if (DefValStrList->IndexOf(ValData->Str) != -1) {
				continue;
			}
			/*
			Cの場合は、グローバル変数であっても、引数で渡す形に修正。 2012/03/20
			if((ValData->BrNestLevel == 0)||(ValData->DefPosS == -1)){
			//				MainF->print("グローバル変数2 =  "+ValData->Str);
			continue;	// グローバル変数は定義済みとしてSkip 2011/07/28
			}
			 */
			DefValStrList->Add(ValData->Str);
			// s = GetDefStr_C(ValData)+ " "+ValData->Str;
			s = ValData->GetDefStr_C() + ValData->Str;
			for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
				Token = (TToken*)TokenList->Items[j];
				s += Token->OrgStr;
			}
			fprintf(fp, "\t%s;\n", s.c_str());
		}
		fprintf(fp, "\n");
	}
	//
	// for文の変数は、グローバル変数であっても再定義する形とする。Add 2011/09/07
	// for( の後の 変数を探して、定義なしであれば定義を行う。
	//
	// if(TuneKind != tkUnroll){
	if ((TuneKind == tkSelect) || (TuneKind == tkVariable)||
		((TuneKind >= tkList) && (TuneKind <= tkList_End)) ) { // Select
		TValData *ValData;
		TToken *Token_2, *Token_1;

		for (i = TokenStartPos + 2; i <= TokenEndPos; i++) {
			ValData = NULL;
			Token = (TToken*)TokenList->Items[i];
			if (Token->TokId == tid_Val) {
				ValData = (TValData*)Token->ValData;
			}
			if (ValData == NULL) {
				continue;
			}
			if (DefValStrList->IndexOf(ValData->Str) != -1) {
				continue;
			}
			Token_2 = (TToken*)TokenList->Items[i - 2];
			Token_1 = (TToken*)TokenList->Items[i - 1];
			if ((Token_2->TokId != tid_for) || (Token_1->TokId != tid_Kakko)) {
				// for ( の直後の変数のみが対象。
				continue;
			}
			DefValStrList->Add(ValData->Str);
			// s = GetDefStr_C(ValData)+ " "+ValData->Str;
			s = ValData->GetDefStr_C() + ValData->Str;
			for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
				Token = (TToken*)TokenList->Items[j];
				s += Token->OrgStr;
			}
			MainF->print("for( 直後の変数 =  " + ValData->Str);
			fprintf(fp, "\t%s;\n", s.c_str());
		}
		fprintf(fp, "\n");
	}
	fprintf(fp, "\tswitch(iusw1){\n");

	for (i = 1; i <= CaseCount; i++) {
		if ((i % 10) == 0) {
			// MainF->print("Output Code "+IntToStr(i)+" / "+IntToStr(CaseCount));
			if (MainF->CloseReqF) {
				return;
			}
		}
		fprintf(fp, "\tcase %d:\n", i);
		s = "";
		for (j = 0; j < ArgValList->Count; j++) {
			if (ArgValList->Strings[j] == "") {
				continue;
			}
			ValData = (TValData*)ArgValList->Objects[j];
			if (s != "") {
				s += ", ";
			}
			s += ValData->Str;
		}
#if 1  // 2016/02/23
//		if (TuneKind == tkSelect) { // Select
		if ((TuneKind == tkSelect) ||
			((TuneKind >= tkList) && (TuneKind <= tkList_End)) ) { // Select
			// Select時は、ソースコード（NestしたRegionがあれば置換する）
			// さらに、先に別のネストがある可能性もあるので注意
			OutputSelectCaseExecCode_C(fp, i);
		}
		else { // それ以外のUnRollやフィージョン等は、サブルーチン集となる
			s2 = "\t\t" + FuncName + "_" + IntToStr(i) + "(";
			s = s2 + s + ");";
			fprintf(fp, "%s\n", s.c_str());
			/*
			sprintf(cs,"           call %s_%d(",FuncName.c_str(),i);
			s = cs + s + ")";
			s = SepLongStr(s);
			fprintf(fp,"%s\n",s.c_str());
			 */
		}
#else
		if (TuneKind == tkUnroll) { // UnRoll時は、サブルーチン集となる
			// s2.printf("\t\t%s_%d(",FuncName,i);
			s2 = "\t\t" + FuncName + "_" + IntToStr(i) + "(";
			s = s2 + s + ");";
			fprintf(fp, "%s\n", s.c_str());
		}
		else {
			// if(TuneKind == tkSelect){ // 当面 Unroll以外は、Selectで処理
			// Select時は、ソースコード（NestしたRegionがあれば置換する）
			// さらに、先に別のネストがある可能性もあるので注意
			OutputSelectCaseExecCode_C(fp, i);
		}
#endif
		fprintf(fp, "\t\tbreak;\n");
	}
	fprintf(fp, "\t}\n");
	fprintf(fp, "\treturn 0;\n");
	fprintf(fp, "}\n");
	fprintf(fp, "\n");

#if 0	// 2016/02/23
	if (TuneKind == tkUnroll) { // UnRoll時は、サブルーチン集となる
		OutputUnrollExecCode_C(fp, fpOutHeader);
	}
#else
	switch(TuneKind) {
	case tkUnroll: // UnRoll時は、サブルーチン集となる
		OutputUnrollExecCode_C(fp, fpOutHeader);
		break;
	case tkLoopFusionSplit: // LoopFusionSplit
	case tkLoopFusion:
	case tkLoopSplit:
	case tkRotationOrder:
		OutputLoopFusionExecCode_C(fp, fpOutHeader);
		break;
	default:
		break;
	}
#endif
	delete DefValStrList;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// iusw1   対象iusw1
//
// 3.概要
// SelectのCase()内に入るコードを出力する
// Case()内のコード生成要求時に呼ばれる。
// 複数のsub regionから選択か、varidの数値から計算される。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputSelectCaseExecCode_Fortran(FILE *fp, int iusw1)
{
	int i, idx;
	TSubRegion *SubRegion = NULL;
	bool Skip_Pragma_ACCF = false;

	idx = iusw1 - 1;
	if (GPUOption == gpu_option_auto) {
		if (idx >= CaseCount / 2) {
			idx -= CaseCount / 2;
		}
	}
	//
	// autoモードにおいてCPUコードでは, #Pragma accを消す。
	//
	if ((GPUOption == gpu_option_auto) && (iusw1 <= CaseCount / 2)) {
		Skip_Pragma_ACCF = true;
	}
	//
	// GPUに対応する #Pragmaを挿入	2010/12/29
	//
	if ((GPUOption == gpu_option_GPU) || ((GPUOption == gpu_option_auto) &&
			(iusw1 > CaseCount / 2))) {
		if (MainF->cc_option_str == "PGI") {
			fprintf(fp, "#pragma acc region\n");
			fprintf(fp, "{ // #pragma allocate region start.\n");
		}
		else if (MainF->cc_option_str == "OMPCUDA") {
			fprintf(fp, "#pragma OMPCUDA gpu region\n");
			fprintf(fp, "{ // #pragma allocate region start.\n");
		}
	}
	if (SubRegionList->Count != 0) {
		for (i = 0; i < SubRegionList->Count; i++) {
			SubRegion = (TSubRegion*)SubRegionList->Items[i];
			if (idx < SubRegion->CaseCount) {
				break;
			}
			idx -= SubRegion->CaseCount;
		}
		OutputReplaceSrc(fp, SubRegion, idx, Skip_Pragma_ACCF);
	}
	else {
		OutputReplaceSrc(fp, NULL, idx, Skip_Pragma_ACCF);
	}
	//
	// GPUに対応する #Pragma ... { に対応する }閉じるを挿入	2011/1/18
	//
	if ((GPUOption == gpu_option_GPU) || ((GPUOption == gpu_option_auto) &&
			(iusw1 > CaseCount / 2))) {
		if (MainF->cc_option_str == "PGI") {
			fprintf(fp, "} // #pragma allocate region end. \n");
		}
		else if (MainF->cc_option_str == "OMPCUDA") {
			fprintf(fp, "} // #pragma allocate region end. \n");
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// iusw1   対象iusw1
//
// 3.概要
// SelectのCase()内に入るコードを出力する
// Case()内のコード生成要求時に呼ばれる。
// 複数のsub regionから選択か、varidの数値から計算される。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputSelectCaseExecCode_C(FILE *fp, int iusw1) {
	int i, idx;
	TSubRegion *SubRegion = NULL;
	bool Skip_Pragma_ACCF = false;

	idx = iusw1 - 1;
	if (GPUOption == gpu_option_auto) {
		if (idx >= CaseCount / 2) {
			idx -= CaseCount / 2;
		}
	}
	//
	// autoモードにおいてCPUコードでは, #Pragma accを消す。
	//
	if ((GPUOption == gpu_option_auto) && (iusw1 <= CaseCount / 2)) {
		Skip_Pragma_ACCF = true;
	}
	//
	// GPUに対応する #Pragmaを挿入	2010/12/29
	//
	if ((GPUOption == gpu_option_GPU) || ((GPUOption == gpu_option_auto) &&
			(iusw1 > CaseCount / 2))) {
		if (MainF->cc_option_str == "PGI") {
			fprintf(fp, "#pragma acc region\n");
			fprintf(fp, "{ // #pragma allocate region start.\n");
		}
		else if (MainF->cc_option_str == "OMPCUDA") {
			fprintf(fp, "#pragma OMPCUDA gpu region\n");
			fprintf(fp, "{ // #pragma allocate region start.\n");
		}
	}
	if (SubRegionList->Count != 0) {
		for (i = 0; i < SubRegionList->Count; i++) {
			SubRegion = (TSubRegion*)SubRegionList->Items[i];
			if (idx < SubRegion->CaseCount) {
				break;
			}
			idx -= SubRegion->CaseCount;
		}
		OutputReplaceSrc(fp, SubRegion, idx, Skip_Pragma_ACCF);
	}
	else {
		OutputReplaceSrc(fp, NULL, idx, Skip_Pragma_ACCF);
	}
	//
	// GPUに対応する #Pragma ... { に対応する }閉じるを挿入	2011/1/18
	//
	if ((GPUOption == gpu_option_GPU) || ((GPUOption == gpu_option_auto) &&
			(iusw1 > CaseCount / 2))) {
		if (MainF->cc_option_str == "PGI") {
			fprintf(fp, "} // #pragma allocate region end. \n");
		}
		else if (MainF->cc_option_str == "OMPCUDA") {
			fprintf(fp, "} // #pragma allocate region end. \n");
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
//
// 3.概要
// LoopFusionSplitサブルーチンを出力する
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
// LoopFusionSplitサブルーチンは順番に出力と必要な部分の置換や挿入で行う
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputLoopFusionExecCode_Fortran(FILE *fp) {
	int TokPos;
	int i, j, k, DefPosS;
	TValData *ValData;
	string s, s2, ValDefStr;
	TToken *Token;
	char cs[1024];
	int FusionIdx, SplitIdx, SplitIdxTbl[32]; // Fusionと各サブリージョンのSplitのIndex
	int SplitSubRegionIdx;
	string FusionValName;
	unsigned int FusionDoValBits;
	int DoNest;
	TScript *Script;
	int RotationOrderIdx;
	int RotationOrderCount;
	int FusionCaseIdx, FusionCaseCount;
	TList *SrcTokenList;

	DoValCount = variedCount;
	//
	// Doで使用する変数名を作成する。
	//
	for (DoValIdx = 0; DoValIdx < DoValCount; DoValIdx++) {
		TValData *DoValData = (TValData*)DoValToken[DoValIdx]->ValData;

		DoTokPos[DoValIdx] = TokenList->IndexOf(DoToken[DoValIdx]);
		EndDoTokPos[DoValIdx] = FindEndDo2(TokenList, DoTokPos[DoValIdx]);
		DoValName[DoValIdx] = DoValData->Str;
		DoValStr[DoValIdx] = DoValData->Str;
	}
	//
	// 指定種類だけのアンロールを行ったサブルーチンを作成する。
	// Sub名 . 引数 , 使用変数 , [コード] がセットされる。
	// コードによって、一時変数が必要となるので、それも追加される。
	//
	RotationOrderCount = 1;
	if (RotaionOrderList->Count != 0) {
		RotationOrderCount = 2; // ソースコードの順番を変えた２種類が作成される。
	}
	FusionCaseCount = CaseCount / RotationOrderCount;

	for (RotationOrderIdx = 0; RotationOrderIdx < RotationOrderCount;
		RotationOrderIdx++) {
		FusionIdx = 0; // 最初はフュージョンなしで開始。
		SplitIdx = -1; // 最初はスプリットなしで開始。
		// SplitIdx = 0;	// 最初はスプリットなしで開始。
		SrcTokenList = TokenList;
		if (RotationOrderIdx != 0) {
			SrcTokenList = MakeRotaionOrderSrcTokenList();
		}
		for (FusionCaseIdx = 1; FusionCaseIdx <= FusionCaseCount;
			FusionCaseIdx++) {
			CaseIdx = RotationOrderIdx * FusionCaseCount + FusionCaseIdx;
			if ((CaseIdx % 10) == 0) {
				if (MainF->CloseReqF) {
					return;
				}
			}
			//
			// FusionIdx,SplitIdx[]を設定
			//
			int wIdx;
			TSubRegion *SubRegion;

			wIdx = ++SplitIdx;
			//
			// 現在のSplitが終了しているかをチェックする。
			//
			if (SplitIdx != 0) { // Splitを更新
				for (k = 0; k < SubRegionList->Count; k++) {
					SubRegion = (TSubRegion*)SubRegionList->Items[k];
					if (SubRegion->SplitCaseCountOfFusionIdx[FusionIdx] == 0) {
						SplitIdxTbl[k] = 0;
					}
					else {
						SplitIdxTbl[k] = wIdx %
							SubRegion->SplitCaseCountOfFusionIdx[FusionIdx];
						wIdx = wIdx / SubRegion->SplitCaseCountOfFusionIdx
							[FusionIdx];
					}
				}
				if (wIdx != 0) { // FusionIdx部分でのSplitが終わったので次のFusionIdxへ
					FusionIdx++;
					SplitIdx = 0;
				}
			}
			if (SplitIdx == 0) { // Splitなし（新しいFusionの開始)
				for (k = 0; k < SubRegionList->Count; k++) {
					SubRegion = (TSubRegion*)SubRegionList->Items[k];
					SplitIdxTbl[k] = 0; // 全て０に設定 （不要な場合もあるが念のため）
				}
			}
			//
			// Fusion（Loop融合）ありの場合は、Loop変数を融合した変数定義を作成する。
			// DoVal1_OoVal2_DoVal3 等の名前とする。
			// 内側のLoopがFusinIdx数+1だけフィージョンされる。
			//
			FusionValName = "";
			FusionDoValBits = 0; // Fusion対象の DoValBits （外側がLSB)
			if (FusionIdx != 0) {
				// for(DoValIdx = 0 ; DoValIdx <= FusionIdx ; DoValIdx++){
				for (DoValIdx = DoValCount - FusionIdx - 1;
					DoValIdx < DoValCount; DoValIdx++) {
					TValData *DoValData = (TValData*)
						DoValToken[DoValIdx]->ValData;
					if (FusionValName == "") {
						FusionValName = DoValData->Str;
					}
					else {
						FusionValName += "_" + DoValData->Str;
					}
					// FusionDoValBits |= (3 << DoValIdx); // Fusion対象の DoValBits （外側がLSB)
					FusionDoValBits |= (1 << DoValIdx);
					// Fusion対象の DoValBits （外側がLSB)
				}
			}
			//
			// 宣言部分
			//
			s = "";
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				if (s != "") {
					s += ", ";
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str;
			}
			sprintf(cs, "      subroutine %s_%d(%s)", FuncName.c_str(),
				CaseIdx, s.c_str());
			fprintf(fp, "%s\n", SepLongStr(cs).c_str());

			//
			// 使用される引数の定義を出力する。
			// DefPosS,E,ArrayDefPosS,Eを使用する。
			// DefPosSが同じ変数は、１つにまとめること。
			//
			DefPosS = -1;
			s = "";
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				if ((DefPosS == -1) || (ValData->DefPosS != DefPosS)) { // 違う定義行
					if (s != "") {
						fprintf(fp, "%s\n", SepLongStr(s).c_str());
						s = "";
					}
					s = ValData->GetDefStr_Fortran();
					if (s == "") {
						continue;
					}
					DefPosS = ValData->DefPosS;
				}
				else {
					s += ",";
				}
				s += " " + ValData->Str;
				for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++)
				{
					Token = (TToken*)SrcTokenList->Items[j];
					s += Token->OrgStr;
				}
			}
			if (s != "") {
				fprintf(fp, "%s\n", SepLongStr(s).c_str());
				s = "";
			}
			//
			// 引数以外の変数の変数宣言を追加する。
			// DO変数は対象外。DO依存変数は、名前を変えて複数にする。
			//
			DefPosS = -1;
			s = "";
			ValDefStr = "";
			DoNest = 0; // LoopFusion内のDoの数。
			SplitSubRegionIdx = 0; // 出現SplitPoint （サブリージョンの数と一致する）
			for (i = 0; i < ArgValList->Count; i++) {
				ValData = (TValData*)ArgValList->Objects[i];
				if (ArgValList->Strings[i] != "") { // 引数の変数は対象外
					continue;
				}
				if (ValData->DefPosS == -1) { // 定義なし
					continue;
				}
				if (ValData->DefPosS != DefPosS) { // 違う定義行
					if (s != "") {
						fprintf(fp, "%s\n", SepLongStr(s).c_str());
						s = "";
					}
					for (j = ValData->DefPosS; j < ValData->DefPosE; j++) {
						Token = (TToken*)SrcTokenList->Items[j];
						s += Token->OrgStr;
					}
					DefPosS = ValData->DefPosS;
					ValDefStr = s;
				}
				else {
					if (s == "") {
						s = ValDefStr;
					}
					else {
						s += ",";
					}
				}
				if (s == "") {
					s = ValDefStr;
				}
				s += " " + ValData->Str;
				for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++)
				{
					Token = (TToken*)SrcTokenList->Items[j];
					s += Token->OrgStr;
				}
			}
			if (s != "") {
				fprintf(fp, "%s\n", SepLongStr(s).c_str());
				s = "";
			}
			if (FusionValName != "") { // フィージョン用の変数
				fprintf(fp, "      integer %s\n", FusionValName.c_str());
			}
			fprintf(fp, "\n");

			//
			// コードを出力する。基本的に範囲をトークンでスキャンして動く。
			// 必要に応じて文字列の置き換えや、必要コードのセットを行う。
			//
			for (TokPos = TokenStartPos; TokPos < TokenEndPos; TokPos++) {
				Token = (TToken*)SrcTokenList->Items[TokPos];
				s = Token->OrgStr;
				if (Token->Script != NULL) {
					Script = (TScript*)Token->Script;
					// スクリプトは、Skip。スクリプトの後の改行もスキップ。
					while (TokPos + 1 < TokenEndPos) {
						Token = (TToken*)SrcTokenList->Items[TokPos + 1];
						if (Token->TokId == tid_LineEnd) {
							break;
						}
						s += Token->OrgStr;
						TokPos++;
					}
					if (Script->ScType == sct_SplitPoint) {
						unsigned int SplitBits;
						int SplitBitsIdx, SplitDoValIdx, BitsCount;

						if (MainF->SrcCodeType == MainF->sctFortran77) {
							fprintf(fp, "c%s\n", SepLongStr(s).c_str());
						}
						else {
							fprintf(fp, "!%s\n", SepLongStr(s).c_str());
						}
						//
						// 有効なSplitか調べる。また、どのDoValIdx(DoNestと同等）を
						// 切断するかを求める。
						//
						SubRegion = (TSubRegion*)
							SubRegionList->Items[SplitSubRegionIdx];
						SplitBits = SubRegion->SplitBitsOfFusionIdx[FusionIdx];
						SplitBitsIdx = SplitIdxTbl[SplitSubRegionIdx];

						if (SplitBitsIdx == 0) {
							SplitDoValIdx = -1;
						}
						else {
							BitsCount = 0;
							for (SplitDoValIdx = 0; SplitDoValIdx < DoValCount;
								SplitDoValIdx++) {
								if ((SplitBits & (1 << SplitDoValIdx)) != 0) {
									BitsCount++;
								}
								if (BitsCount >= SplitBitsIdx) {
									break;
								}
							}
						}
						if (SplitDoValIdx != -1) { // Splitコードを出力する。
							//
							// SplitDoValIdxからDoNestだけの END DO を出力する。
							// 対象DOのEND DOを対象として、インデントを合わせる。
							//
							for (DoValIdx = DoNest - 1;
								DoValIdx >= SplitDoValIdx; DoValIdx--) {
								if (DoValIdx == DoValCount - FusionIdx - 1) {
									// 外側のFusionDoVal

								}
								else if
									((FusionDoValBits & (1 << DoValIdx))
									!= 0) {
									continue;
								}
								Token = (TToken*)SrcTokenList->Items[EndDoTokPos[DoValIdx]];
								s = Token->OrgStr;
								fprintf(fp, "%s\n", SepLongStr(s).c_str());
//								fprintf(fp, "%s", SepLongStr(s).c_str());
							}
							//
							// ここでリージョン内での最後のenddoの後のコメント行を入れてから、
							// 最初のdoの前のコメント行を入れる。!omp 等の行が入る形になる。
							// ただし、Doの内部でのSplitの場合は入れない。
							//
							if (SplitDoValIdx == 0) {
								TToken *Token2;
								int j;

								for (j = EndDoTokPos[0]+1; j < TokenEndPos; j++) {
									Token2 = (TToken*)SrcTokenList->Items[j];
									if (Token2->Script != NULL) { // Scriptのスキップ
										for (; j < TokenEndPos; j++) {
											Token2 = (TToken*)
											SrcTokenList->Items[j];
											if (Token2->TokId == tid_LineEnd) {
												break;
											}
										}
										continue;
									}
									fprintf(fp, "%s",Token2->OrgStr.c_str());
								}
								fprintf(fp, "\n");
								for (j = TokenStartPos; j < DoTokPos[0]; j++) {
									Token2 = (TToken*)SrcTokenList->Items[j];
									if (Token2->Script != NULL) { // Scriptのスキップ
										for (; j < TokenEndPos; j++) {
											Token2 = (TToken*)
											SrcTokenList->Items[j];
											if (Token2->TokId == tid_LineEnd) {
												break;
											}
										}
										continue;
									}
									fprintf(fp, "%s",Token2->OrgStr.c_str());
								}
							}
							//
							// SplitDoValIdxからDoNestだけの DOを再度出力する。
							// Fusionも有効なので、Fusion中のDOはFusionでの出力となる。
							// Fusionの内部のDOに関してのSplitはCaseから除外済。
							//
							for (DoValIdx = SplitDoValIdx; DoValIdx < DoNest;
								DoValIdx++) {
								if ((FusionDoValBits & (1 << DoValIdx)) != 0) {
									// OutputFusionDo(FusionValName,fp,DoValIdx-1,FusionIdx);
									OutputFusionDo_Fortran(FusionValName, fp,
										DoValIdx, FusionIdx);
								}
								else { // Fusion対象外であれば、そのまま出力
									s = "";
									for (j = DoTokPos[DoValIdx];
										j < TokenEndPos; j++) {
										Token = (TToken*)SrcTokenList->Items[j];
										s += Token->OrgStr;
										if (Token->TokId == tid_LineEnd) {
											break;
										}
									}
									fprintf(fp, "%s", SepLongStr(s).c_str());
								}
							}
						}
						SplitSubRegionIdx++; // 出現SplitPoint （サブリージョンの数と一致する）
					}
					else if (Script->ScType == sct_SplitPointCopyInsert) {
						//
						// SplitPointCopyDefのリージョン内をCOPYして配置する。
						//
						if (MainF->SrcCodeType == MainF->sctFortran77) {
							fprintf(fp, "c%s\n", SepLongStr(s).c_str());
						}
						else {
							fprintf(fp, "!%s\n", SepLongStr(s).c_str());
						}
						if (SplitIdx != 0) {
							for (k = SplitPointCopyDef_StartPos;
								k < SplitPointCopyDef_EndPos; k++) {
								Token = (TToken*)SrcTokenList->Items[k];
								s = Token->OrgStr;
								fprintf(fp, "%s", SepLongStr(s).c_str());
							}
						}
					}
					else {
						// スクリプトは、Skip。スクリプトの後の改行もスキップ。
						while (TokPos + 1 < TokenEndPos) {
							Token = (TToken*)SrcTokenList->Items[TokPos + 1];
							if (Token->TokId == tid_LineEnd) {
								break;
							}
							s += Token->OrgStr;
							TokPos++;
						}
						if (MainF->SrcCodeType == MainF->sctFortran77) {
							fprintf(fp, "c%s", SepLongStr(s).c_str());
						}
						else {
							fprintf(fp, "!%s", SepLongStr(s).c_str());
						}
					}
					continue;
				}
				else if (Token->TokId == tid_Comment) {
					fprintf(fp, "%s", Token->OrgStr.c_str()); // コメントは、そのまま
					continue;
				}
				else if (Token->TokId == tid_LineEnd) {
					fprintf(fp, "%s", Token->OrgStr.c_str()); // 改行も、そのまま
					continue;
				}
				else if ((Token->TokId == tid_CALL)&&(i < TokenEndPos-1)) {	// 2016/02/26
					//
					// 	_OATへの置換え SUBROUTINかをチェックする。
					//
					TToken *Token2;
					string SubroutineName;
					int Idx;
					TPass5 *Pass5;

					Token2 = (TToken*)SrcTokenList->Items[TokPos+1];
					SubroutineName = Token2->Str;

					Idx = -1;
					Pass5 = (TPass5 *)MainF->Pass5;
					for(int k = 0 ; k < Pass5->Call_SubroutineNameInRegionList->Count ; k++){
#ifdef _WIN32
						if(stricmp(Pass5->Call_SubroutineNameInRegionList->Strings[k].c_str(),
#else
						if(strcasecmp(Pass5->Call_SubroutineNameInRegionList->Strings[k].c_str(),
#endif
							SubroutineName.c_str()) == 0){
							Idx = k;
						}
					}
					if(Idx != -1){	// 一致
						fprintf(fp, "%s%s_OAT", Token->OrgStr.c_str(),Token2->OrgStr.c_str());
						TokPos++;
					}else{
						fprintf(fp, "%s",  Token->OrgStr.c_str());
					}
					continue;
				}
				else if (FusionCaseIdx == 1) {
					// Fusion CaseIdx == 1 の場合は、そのまま出力
					s = "";
					for (; TokPos < TokenEndPos; TokPos++) {
						Token = (TToken*)SrcTokenList->Items[TokPos];
						s += Token->OrgStr;
						if (Token->TokId == tid_LineEnd) {
							break;
						}
					}
					fprintf(fp, "%s", SepLongStr(s).c_str());
					continue;
				}
				if ((Token->TokId == tid_DO) || (Token->TokId == tid_for)) {
					//
					//
					// Fusion対象DOかをチェックする。
					// 外側FusionDoValBitsは、Fusion用変数を使ったDoと変数設定。
					// それ以外のFusion対象はコメントにする。EndDoも同様に処理する。
					//
					// K = (KK-1)/(NY*NX) + 1
					// J = mod((KK-1)/NX,NY) + 1
					// I = mod(KK-1,NX) + 1
					//
					//
					if ((FusionDoValBits & (1 << DoNest)) != 0) {
						OutputFusionDo_Fortran(FusionValName, fp, DoNest,
							FusionIdx);
						if (MainF->SrcCodeType == MainF->sctFortran77) {
							s = "c";
						}
						else {
							s = "!";
						}
						for (j = TokPos; j < TokenEndPos; j++) {
							Token = (TToken*)SrcTokenList->Items[j];
							s += Token->OrgStr;
							if (Token->TokId == tid_LineEnd) {
								break;
							}
						}
						fprintf(fp, "%s", SepLongStr(s).c_str());
						TokPos = j;
					}
					else { // Fusion対象外であれば、そのまま出力
						s = "";
						for (j = TokPos; j < TokenEndPos; j++) {
							Token = (TToken*)SrcTokenList->Items[j];
							s += Token->OrgStr;
							if (Token->TokId == tid_LineEnd) {
								break;
							}
						}
						fprintf(fp, "%s", SepLongStr(s).c_str());
						TokPos = j;
					}
					DoNest++;

				}
				else if (Token->TokId == tid_ENDDO) {
					// if(DoNest > DoValCount-FusionIdx){	// 内側のFusionDoVal
					// if((FusionDoValBits & (1 << DoNest)) != 0){
					// Fusion用のenddoを追加する。
					if ((FusionDoValBits & (1 << (DoNest - 1))) != 0) {
						if (MainF->SrcCodeType == MainF->sctFortran77) {
							fprintf(fp, "c%s", SepLongStr(s).c_str());
						}
						else {
							fprintf(fp, "!%s", SepLongStr(s).c_str());
						}
					}
					else {
						fprintf(fp, "%s", SepLongStr(s).c_str());
					}
					//
					// Fusion Loop用の enddoを出力する。
					//
					if ((FusionDoValBits & (1 << (DoNest - 1))) != 0) {
						if
							((DoNest == 1) ||
							((FusionDoValBits & (1 << (DoNest - 2))) == 0)) {
							// enddo i 等のenddoの後を出力してからenddoを出力する。
							s = "";
							for (j = TokPos + 1; j < TokenEndPos; j++) {
								Token = (TToken*)SrcTokenList->Items[j];
								s += Token->OrgStr;
								if (Token->TokId == tid_LineEnd) {
									TokPos = j;
									break;
								}

							}
							fprintf(fp, "%s", SepLongStr(s).c_str());
							fprintf(fp, "      enddo\n");
						}
					}
					DoNest--;
				}
				else {
					s = "";
					for (j = TokPos; j < TokenEndPos; j++) {
						Token = (TToken*)SrcTokenList->Items[j];
						s += Token->OrgStr;
						if (Token->TokId == tid_LineEnd) {
							TokPos = j;
							break;
						}

					}
					fprintf(fp, "%s", SepLongStr(s).c_str());
				}
			}
			fprintf(fp,"\n");
			fprintf(fp, "      return\n");
			if (MainF->SrcCodeType == MainF->sctFortran90) {
				fprintf(fp, "      end subroutine %s_%d\n", FuncName.c_str(),
					CaseIdx);
			}
			else {
				fprintf(fp, "      end\n");
			}
			fprintf(fp, "\n");
			// サブルーチン終了（Case回繰り返される）
		}
		if (SrcTokenList != TokenList) {
			delete SrcTokenList;
			SrcTokenList = NULL;
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
//
// 3.概要
// LoopFusionSplitサブルーチンを出力する
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
// LoopFusionSplitサブルーチンは順番に出力と必要な部分の置換や挿入で行う
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputLoopFusionExecCode_C(FILE *fp, FILE *fpOutHeader) {
	int TokPos;
	int i, j, k, DefPosS;
	TValData *ValData;
	string s, s2, ValDefStr;
	TToken *Token;
	int FusionIdx, SplitIdx, SplitIdxTbl[32]; // Fusionと各サブリージョンのSplitのIndex
	int SplitSubRegionIdx;
	string FusionValName;
	unsigned int FusionDoValBits;
	int DoNest;
	TScript *Script;
	int RotationOrderIdx;
	int RotationOrderCount;
	int FusionCaseIdx, FusionCaseCount;
	TList *SrcTokenList;
	string ArgValName;

	DoValCount = variedCount;
	//
	// Doで使用する変数名を作成する。
	//
	for (DoValIdx = 0; DoValIdx < DoValCount; DoValIdx++) {
		TValData *DoValData = (TValData*)DoValToken[DoValIdx]->ValData;

		DoTokPos[DoValIdx] = TokenList->IndexOf(DoToken[DoValIdx]);
		EndDoTokPos[DoValIdx] = FindEndDo2(TokenList, DoTokPos[DoValIdx]);
		DoValName[DoValIdx] = DoValData->Str;
		DoValStr[DoValIdx] = DoValData->Str;
	}
	//
	// 指定種類だけのアンロールを行ったサブルーチンを作成する。
	// Sub名 . 引数 , 使用変数 , [コード] がセットされる。
	// コードによって、一時変数が必要となるので、それも追加される。
	//
	RotationOrderCount = 1;
	if (RotaionOrderList->Count != 0) {
		RotationOrderCount = 2; // ソースコードの順番を変えた２種類が作成される。
	}
	FusionCaseCount = CaseCount / RotationOrderCount;

	for (RotationOrderIdx = 0; RotationOrderIdx < RotationOrderCount;
		RotationOrderIdx++) {
		FusionIdx = 0; // 最初はフュージョンなしで開始。
		SplitIdx = -1; // 最初はスプリットなしで開始。
		SrcTokenList = TokenList;
		if (RotationOrderIdx != 0) {
			SrcTokenList = MakeRotaionOrderSrcTokenList();
		}
		for (FusionCaseIdx = 1; FusionCaseIdx <= FusionCaseCount;
			FusionCaseIdx++) {
			CaseIdx = RotationOrderIdx * FusionCaseCount + FusionCaseIdx;
			if ((CaseIdx % 10) == 0) {
				if (MainF->CloseReqF) {
					return;
				}
			}
			//
			// FusionIdx,SplitIdx[]を設定
			//
			int wIdx;
			TSubRegion *SubRegion;

			wIdx = ++SplitIdx;
			//
			// 現在のSplitが終了しているかをチェックする。
			//
			if (SplitIdx != 0) { // Splitを更新
				for (k = 0; k < SubRegionList->Count; k++) {
					SubRegion = (TSubRegion*)SubRegionList->Items[k];
					if (SubRegion->SplitCaseCountOfFusionIdx[FusionIdx] == 0) {
						SplitIdxTbl[k] = 0;
					}
					else {
						SplitIdxTbl[k] = wIdx %
							SubRegion->SplitCaseCountOfFusionIdx[FusionIdx];
						wIdx = wIdx / SubRegion->SplitCaseCountOfFusionIdx
							[FusionIdx];
					}
				}
				if (wIdx != 0) { // FusionIdx部分でのSplitが終わったので次のFusionIdxへ
					FusionIdx++;
					SplitIdx = 0;
				}
			}
			if (SplitIdx == 0) { // Splitなし（新しいFusionの開始)
				for (k = 0; k < SubRegionList->Count; k++) {
					SubRegion = (TSubRegion*)SubRegionList->Items[k];
					SplitIdxTbl[k] = 0; // 全て０に設定 （不要な場合もあるが念のため）
				}
			}
			//
			// Fusion（Loop融合）ありの場合は、Loop変数を融合した変数定義を作成する。
			// DoVal1_OoVal2_DoVal3 等の名前とする。
			// 内側のLoopがFusinIdx数+1だけフィージョンされる。
			//
			FusionValName = "";
			FusionDoValBits = 0; // Fusion対象の DoValBits （外側がLSB)
			if (FusionIdx != 0) {
				for (DoValIdx = DoValCount - FusionIdx - 1;
					DoValIdx < DoValCount; DoValIdx++) {
					TValData *DoValData = (TValData*)
						DoValToken[DoValIdx]->ValData;
					if (FusionValName == "") {
						FusionValName = DoValData->Str;
					}
					else {
						FusionValName += "_" + DoValData->Str;
					}
					FusionDoValBits |= (1 << DoValIdx);
					// Fusion対象の DoValBits （外側がLSB)
				}
			}
			//
			// 宣言部分
			//
			s = "";
			for (i = 0; i < ArgValList->Count; i++) {
				ArgValName = ArgValList->Strings[i];
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				if (s != "") {
					s += ",";
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->GetDefStr_C() + ValData->Str;
				for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++)
				{
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
				}
			}
			// s2= FuncName + "_"+IntToStr(CaseIdx_for_Loop)+"("+s+")";
			s2 = FuncName + "_" + IntToStr(CaseIdx) + "(" + s + ")";
			fprintf(fp, "int %s\n", s2.c_str());
			fprintf(fpOutHeader, "int %s;\n", s2.c_str());
			fprintf(fp, "{\n");
			//
			// 使用される引数の定義を出力する。
			// DefPosS,E,ArrayDefPosS,Eを使用する。
			// DefPosSが同じ変数は、１つにまとめること。
			//
			//
			// 引数以外の変数の変数宣言を追加する。
			// DO変数は対象外。DO依存変数は、名前を変えて複数にする。
			//
			DefPosS = -1;
			s = "";
			ValDefStr = "";
			DoNest = 0; // LoopFusion内のDoの数。
			SplitSubRegionIdx = 0; // 出現SplitPoint （サブリージョンの数と一致する）
			for (i = 0; i < ArgValList->Count; i++) {
				ValData = (TValData*)ArgValList->Objects[i];
				if (ArgValList->Strings[i] != "") { // 引数の変数は対象外
					continue;
				}
				if (ValData->DefPosS == -1) { // 定義なし
					continue;
				}
				if (ValData->DefPosS != DefPosS) { // 違う定義行
					if (s != "") {
						fprintf(fp, "%s;\n", s.c_str());
						s = "";
					}
					for (j = ValData->DefPosS; j < ValData->DefPosE; j++) {
						Token = (TToken*)SrcTokenList->Items[j];
						s += Token->OrgStr;
					}
					DefPosS = ValData->DefPosS;
					ValDefStr = s;
				}
				else {
					if (s == "") {
						s = ValDefStr;
					}
					else {
						s += ",";
					}
				}
				if (s == "") {
					s = ValDefStr;
				}
				s += " " + ValData->Str;
				for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++)
				{
					Token = (TToken*)SrcTokenList->Items[j];
					s += Token->OrgStr;
				}
			}
			if (s != "") {
				fprintf(fp, "%s;\n", s.c_str());
				s = "";
			}
			if (FusionValName != "") { // フィージョン用の変数
				// fprintf(fp,"      integer %s\n",FusionValName.c_str());
				fprintf(fp, "  int %s;\n", FusionValName.c_str());
			}
			fprintf(fp, "\n");

			//
			// コードを出力する。基本的に範囲をトークンでスキャンして動く。
			// 必要に応じて文字列の置き換えや、必要コードのセットを行う。
			//
			for (TokPos = TokenStartPos; TokPos < TokenEndPos; TokPos++) {
				Token = (TToken*)SrcTokenList->Items[TokPos];
				s = Token->OrgStr;
				if (Token->Script != NULL) {
					Script = (TScript*)Token->Script;
					// スクリプトは、Skip。スクリプトの後の改行もスキップ。
					while (TokPos + 1 < TokenEndPos) {
						Token = (TToken*)SrcTokenList->Items[TokPos + 1];
						if (Token->TokId == tid_LineEnd) {
							break;
						}
						s += Token->OrgStr;
						TokPos++;
					}
					if (Script->ScType == sct_SplitPoint) {
						unsigned int SplitBits;
						int SplitBitsIdx, SplitDoValIdx, BitsCount;

						fprintf(fp, "//%s\n", SepLongStr(s).c_str());
						//
						// 有効なSplitか調べる。また、どのDoValIdx(DoNestと同等）を
						// 切断するかを求める。
						//
						SubRegion = (TSubRegion*)
							SubRegionList->Items[SplitSubRegionIdx];
						SplitBits = SubRegion->SplitBitsOfFusionIdx[FusionIdx];
						SplitBitsIdx = SplitIdxTbl[SplitSubRegionIdx];

						if (SplitBitsIdx == 0) {
							SplitDoValIdx = -1;
						}
						else {
							BitsCount = 0;
							for (SplitDoValIdx = 0; SplitDoValIdx < DoValCount;
								SplitDoValIdx++) {
								if ((SplitBits & (1 << SplitDoValIdx)) != 0) {
									BitsCount++;
								}
								if (BitsCount >= SplitBitsIdx) {
									break;
								}
							}
						}
						if (SplitDoValIdx != -1) { // Splitコードを出力する。
							//
							// SplitDoValIdxからDoNestだけの END DO を出力する。
							// 対象DOのEND DOを対象として、インデントを合わせる。
							//
							for (DoValIdx = DoNest - 1;
								DoValIdx >= SplitDoValIdx; DoValIdx--) {
								if (DoValIdx == DoValCount - FusionIdx - 1) {
									// 外側のFusionDoVal

								}
								else if
									((FusionDoValBits & (1 << DoValIdx))
									!= 0) {
									continue;
								}
								Token = (TToken*)
									SrcTokenList->Items
									[EndDoTokPos[DoValIdx]];
								s = Token->OrgStr;
								fprintf(fp, "%s\n", SepLongStr(s).c_str());
//								fprintf(fp, "%s", SepLongStr(s).c_str());
							}
							// fprintf(fp,"\n");
							//
							// ここでリージョン内での最後のenddoの後のコメント行を入れてから、
							// 最初のdoの前のコメント行を入れる。!omp 等の行が入る形になる。
							// ただし、Doの内部でのSplitの場合は入れない。
							//
							if (SplitDoValIdx == 0) {
								TToken *Token2;
								int j;

								for (j = EndDoTokPos[0]+1; j < TokenEndPos; j++) {
									Token2 = (TToken*)SrcTokenList->Items[j];
									if (Token2->Script != NULL) { // Scriptのスキップ
										for (; j < TokenEndPos; j++) {
											Token2 = (TToken*)
											SrcTokenList->Items[j];
											if (Token2->TokId == tid_LineEnd) {
												break;
											}
										}
										continue;
									}
									fprintf(fp, "%s",Token2->OrgStr.c_str());
								}
								fprintf(fp, "\n");
								for (j = TokenStartPos; j < DoTokPos[0]; j++) {
									Token2 = (TToken*)SrcTokenList->Items[j];
									if (Token2->Script != NULL) { // Scriptのスキップ
										for (; j < TokenEndPos; j++) {
											Token2 = (TToken*)
											SrcTokenList->Items[j];
											if (Token2->TokId == tid_LineEnd) {
												break;
											}
										}
										continue;
									}
									fprintf(fp, "%s",Token2->OrgStr.c_str());
								}
							}
							//
							// SplitDoValIdxからDoNestだけの DOを再度出力する。
							// Fusionも有効なので、Fusion中のDOはFusionでの出力となる。
							// Fusionの内部のDOに関してのSplitはCaseから除外済。
							//
							for (DoValIdx = SplitDoValIdx; DoValIdx < DoNest;
								DoValIdx++) {
								if ((FusionDoValBits & (1 << DoValIdx)) != 0) {
									// OutputFusionDo(FusionValName,fp,DoValIdx-1,FusionIdx);
									OutputFusionDo_C(FusionValName, fp,
										DoValIdx, FusionIdx);
								}
								else { // Fusion対象外であれば、そのまま出力
									s = "";
									for (j = DoTokPos[DoValIdx];
										j < TokenEndPos; j++) {
										Token = (TToken*)SrcTokenList->Items[j];
										s += Token->OrgStr;
										if (Token->TokId == tid_LineEnd) {
										break;
										}
									}
									fprintf(fp, "%s", SepLongStr(s).c_str());
								}
							}
						}
						SplitSubRegionIdx++; // 出現SplitPoint （サブリージョンの数と一致する）
					}
					else if (Script->ScType == sct_SplitPointCopyInsert) {
						//
						// SplitPointCopyDefのリージョン内をCOPYして配置する。
						//
						fprintf(fp, "//%s\n", SepLongStr(s).c_str());
						if (SplitIdx != 0) {
							for (k = SplitPointCopyDef_StartPos;
								k < SplitPointCopyDef_EndPos; k++) {
								Token = (TToken*)SrcTokenList->Items[k];
								s = Token->OrgStr;
								fprintf(fp, "%s", SepLongStr(s).c_str());
							}
						}
					}
					else {
						// スクリプトは、Skip。スクリプトの後の改行もスキップ。
						while (TokPos + 1 < TokenEndPos) {
							Token = (TToken*)SrcTokenList->Items[TokPos + 1];
							if (Token->TokId == tid_LineEnd) {
								break;
							}
							s += Token->OrgStr;
							TokPos++;
						}
						fprintf(fp, "//%s", SepLongStr(s).c_str());
					}
					continue;
				}
				else if (Token->TokId == tid_Comment) {
					fprintf(fp, "%s", Token->OrgStr.c_str()); // コメントは、そのまま
					continue;
				}
				else if (Token->TokId == tid_LineEnd) {
					fprintf(fp, "%s", Token->OrgStr.c_str()); // 改行も、そのまま
					continue;
				}
				else if (FusionCaseIdx == 1) {
					// Fusion CaseIdx == 1 の場合は、そのまま出力
					s = "";
					for (; TokPos < TokenEndPos; TokPos++) {
						Token = (TToken*)SrcTokenList->Items[TokPos];
						s += Token->OrgStr;
						if (Token->TokId == tid_LineEnd) {
							break;
						}
					}
					fprintf(fp, "%s", s.c_str());
					continue;
				}
				if ((Token->TokId == tid_DO) || (Token->TokId == tid_for)) {
					//
					//
					// Fusion対象DOかをチェックする。
					// 外側FusionDoValBitsは、Fusion用変数を使ったDoと変数設定。
					// それ以外のFusion対象はコメントにする。EndDoも同様に処理する。
					//
					// K = (KK-1)/(NY*NX) + 1
					// J = mod((KK-1)/NX,NY) + 1
					// I = mod(KK-1,NX) + 1
					//
					//
					if ((FusionDoValBits & (1 << DoNest)) != 0) {
						OutputFusionDo_C(FusionValName, fp, DoNest, FusionIdx);
						s = "//";
						for (j = TokPos; j < TokenEndPos; j++) {
							Token = (TToken*)SrcTokenList->Items[j];
							s += Token->OrgStr;
							if (Token->TokId == tid_LineEnd) {
								break;
							}
						}
						fprintf(fp, "%s", s.c_str());
						TokPos = j;
					}
					else { // Fusion対象外であれば、そのまま出力
						s = "";
						for (j = TokPos; j < TokenEndPos; j++) {
							Token = (TToken*)SrcTokenList->Items[j];
							s += Token->OrgStr;
							if (Token->TokId == tid_LineEnd) {
								break;
							}
						}
						fprintf(fp, "%s", s.c_str());
						TokPos = j;
					}
					DoNest++;

				}
				else if (Token->TokId == tid_DaiKokka) {
					if ((FusionDoValBits & (1 << (DoNest - 1))) != 0) {
						fprintf(fp, "//%s", SepLongStr(s).c_str());
					}
					else {
						fprintf(fp, "%s", SepLongStr(s).c_str());
					}
					//
					// Fusion Loop用の enddoを出力する。
					//
					if ((FusionDoValBits & (1 << (DoNest - 1))) != 0) {
						if((DoNest == 1) ||
							((FusionDoValBits & (1 << (DoNest - 2))) == 0)) {
							// enddo i 等のenddoの後を出力してからenddoを出力する。
							s = "";
							for (j = TokPos + 1; j < TokenEndPos; j++) {
								Token = (TToken*)SrcTokenList->Items[j];
								s += Token->OrgStr;
								if (Token->TokId == tid_LineEnd) {
									TokPos = j;
									break;
								}

							}
							fprintf(fp, "%s", SepLongStr(s).c_str());
							fprintf(fp, "      }\n");	// Loop終了
						}
					}
					DoNest--;
				}
				else {
					s = "";
					for (j = TokPos; j < TokenEndPos; j++) {
						Token = (TToken*)SrcTokenList->Items[j];
						s += Token->OrgStr;
						if (Token->TokId == tid_LineEnd) {
							TokPos = j;
							break;
						}

					}
					fprintf(fp, "%s", SepLongStr(s).c_str());
				}
			}
			// fprintf(fp,"\n");
			fprintf(fp, "  return 0;\n");
			fprintf(fp, "}\n");
			// サブルーチン終了（Case回繰り返される）
		}
		if (SrcTokenList != TokenList) {
			delete SrcTokenList;
			SrcTokenList = NULL;
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// RotaionOrder sub regionに合わせて順番を変えたTokenListを作成して返す。
//
// 4.機能説明
// 元のTokenListを参照して参照の順番を変えたTokenListを作成して返す。
// 参照の順番を変えただけなのでTokData等の実体はTokList内と同じ。
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
TList * TTuneRegion::MakeRotaionOrderSrcTokenList() {
	TList *NewTokenList;
	int i;
	int RotIdx;
	int *RotationOrderSPos, *RotationOrderEPos;
	int *RotationOrderCurPos;
	TToken *Token;
	bool AddNewTokenF;

	NewTokenList = new TList;

	RotationOrderSPos = new int[RotaionOrderList->Count / 2 + 1];
	RotationOrderEPos = new int[RotaionOrderList->Count / 2 + 1];
	RotationOrderCurPos = new int[RotaionOrderList->Count / 2 + 1];
	RotationOrderSPos[0] = 0;
	RotationOrderEPos[0] = 0;
	RotationOrderCurPos[0] = 0;
	for (i = 0; i < RotaionOrderList->Count / 2; i++) {
		RotationOrderSPos[i] = (long)RotaionOrderList->Items[2 * i];
		RotationOrderEPos[i] = (long)RotaionOrderList->Items[2 * i + 1];
		RotationOrderCurPos[i] = RotationOrderSPos[i];
	}
	//
	// 最初のRotationOrder sub regionまで複写
	//
	for (i = 0; i < RotationOrderSPos[0]; i++) {
		NewTokenList->Add(TokenList->Items[i]);
	}
	//
	// それぞれの RotationOrder sub region を行ごとに順番に出力
	// 出力終了（NewTokenListへの追加がなくなった場合）まで繰り返す。
	//
	AddNewTokenF = true;
	while (AddNewTokenF) {
		AddNewTokenF = false;
		for (RotIdx = 0; RotIdx < RotaionOrderList->Count / 2; RotIdx++) {
			for (i = RotationOrderCurPos[RotIdx];
				i < RotationOrderEPos[RotIdx]; i++) {
				AddNewTokenF = true;
				NewTokenList->Add(TokenList->Items[i]);
				Token = (TToken*)TokenList->Items[i];
				if (Token->TokId == tid_LineEnd) {
					i++;
					break;
				}
			}
			RotationOrderCurPos[RotIdx] = i;
		}
	}
	//
	// 残りのTokenListをRotateionOrder sub region の間を除いて複写
	//
	for (i = RotationOrderEPos[0]; i < TokenList->Count; i++) {
		for (RotIdx = 0; RotIdx < RotaionOrderList->Count / 2; RotIdx++) {
			if ((i >= RotationOrderSPos[RotIdx]) &&
				(i < RotationOrderEPos[RotIdx])) {
				break;
			}
		}
		if (RotIdx < RotaionOrderList->Count / 2) {
			continue; // Skip対象
		}
		NewTokenList->Add(TokenList->Items[i]);
	}
	delete[]RotationOrderSPos;
	delete[]RotationOrderEPos;
	return NewTokenList;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
//
// 3.概要
// Unrollのサブルーチンを出力する
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputUnrollExecCode_Fortran(FILE *fp) {
	int TokPos;
	int i, j, DefPosS;
	TValData *ValData;
	string s, s2, ValDefStr;
	TToken *Token;
	int iusw;
	int RollCount;
	int EndTokPos;
	char cs[1024];
	string CommentStrBeforeDo;

	DoValCount = variedCount;
	//
	// Doで使用する変数名を作成する。
	//
	for (DoValIdx = 0; DoValIdx < DoValCount; DoValIdx++) {
		TValData *DoValData = (TValData*)DoValToken[DoValIdx]->ValData;

		DoTokPos[DoValIdx] = TokenList->IndexOf(DoToken[DoValIdx]);
		EndDoTokPos[DoValIdx] = FindEndDo2(TokenList, DoTokPos[DoValIdx]);
		DoValName[DoValIdx] = DoValData->Str;
		DoValStr[DoValIdx] = DoValData->Str;
		DoVal_m[DoValIdx] = GetNewValName(DoValData->Str, "m", ArgValList);
		DoVal_i[DoValIdx] = GetNewValName(DoValData->Str, "i", ArgValList);
		DoVal_l[DoValIdx] = GetNewValName(DoValData->Str, "l", ArgValList);
	}
	//
	// 指定種類だけのアンロールを行ったサブルーチンを作成する。
	// Sub名 . 引数 , 使用変数 , [コード] がセットされる。
	// コードによって、一時変数が必要となるので、それも追加される。
	//
	for (CaseIdx = 1; CaseIdx <= CaseCount; CaseIdx++) {
		//
		// DoValCountに合わせた、UnRoll数を求める。
		// 現状では、 ２段にLoopは対応していない。（RefBitsのクリアが必要）
		// また、DoValになる変数のUnrollDoRefValBitsを求める。
		//
		int idx2, c;

		if ((CaseIdx % 10) == 0) {
			// MainF->print("Output Code "+IntToStr(CaseIdx)+" / "+IntToStr(CaseCount));
			if (MainF->CloseReqF) {
				return;
			}
		}
		UnRollDoRefValBits = 0;
		idx2 = CaseIdx - 1;
		for (int kk = DoValCount - 1; kk >= 0; kk--) {
			/** *************************************************** */
			//
			// Kogakuin Irie
			// PPの変動範囲を実数にも対応
			// 既存コードはコメントアウト
			//
			// c = (variedToValue[kk] - variedFromValue[kk] + 1);
			c = (int)(variedToValue[kk] - variedFromValue[kk] + 1);
			//
			// ここまで
			//
			/** *************************************************** */

			// UnrollCount[kk] = (idx2 % c)+1;

			/** *************************************************** */
			//
			// Kogakuin Irie
			// PPの変動範囲を実数にも対応
			// 既存コードはコメントアウト
			//
			// UnrollCount[kk] = (idx2 % c)+variedFromValue[kk];	// 2012/05/17
			//
//			UnrollCount[kk] = (int)((idx2 % c) + variedFromValue[kk]);
			// Add StepValue 2016/03/05
			UnrollCount[kk] = (int)(variedStepValue[kk]*(idx2 % c) + variedFromValue[kk]);
			// ここまで
			//
			/** *************************************************** */

			if (UnrollCount[kk] > 1) {
				UnRollDoRefValBits |= (1 << kk); // UnRollされるDoのBits
			}
			idx2 /= c;
		}
		//
		// 宣言部分
		//
		s = "";
		for (i = 0; i < ArgValList->Count; i++) {
			if (ArgValList->Strings[i] == "") {
				continue;
			}
			if (s != "") {
				s += ", ";
			}
			ValData = (TValData*)ArgValList->Objects[i];
			s += ValData->Str;
		}
		sprintf(cs, "      subroutine %s_%d(%s)", FuncName.c_str(), CaseIdx,
			s.c_str());
		fprintf(fp, "%s\n", SepLongStr(cs).c_str());

		//
		// 使用される引数の定義を出力する。
		// DefPosS,E,ArrayDefPosS,Eを使用する。
		// DefPosSが同じ変数は、１つにまとめること。
		//
		DefPosS = -1;
		s = "";
		for (i = 0; i < ArgValList->Count; i++) {
			if (ArgValList->Strings[i] == "") {
				continue;
			}
			ValData = (TValData*)ArgValList->Objects[i];
			if ((DefPosS == -1) || (ValData->DefPosS != DefPosS)) { // 違う定義行
				if (s != "") {
					fprintf(fp, "%s\n", SepLongStr(s).c_str());
					s = "";
				}
				s = ValData->GetDefStr_Fortran();
				if (s == "") {
					continue;
				}
				DefPosS = ValData->DefPosS;
			}
			else {
				s += ",";
			}
			s += " " + ValData->Str;
			for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
				Token = (TToken*)TokenList->Items[j];
				s += Token->OrgStr;
			}
		}
		if (s != "") {
			fprintf(fp, "%s\n", SepLongStr(s).c_str());
			s = "";
		}
		//
		// 引数以外の変数の変数宣言を追加する。
		// DO変数は対象外。DO依存変数は、名前を変えて複数にする。
		//
		DefPosS = -1;
		s = "";
		ValDefStr = "";
		for (i = 0; i < ArgValList->Count; i++) {
			if (ArgValList->Strings[i] != "") { // 引数の変数は対象外
				continue;
			}
			ValData = (TValData*)ArgValList->Objects[i];
			if (ValData->DefPosS == -1) { // 定義なし
				continue;
			}
			if (ValData->DefPosS != DefPosS) { // 違う定義行
				if (s != "") {
					fprintf(fp, "%s\n", SepLongStr(s).c_str());
					s = "";
				}
				for (j = ValData->DefPosS; j < ValData->DefPosE; j++) {
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
				}
				DefPosS = ValData->DefPosS;
				ValDefStr = s;
			}
			else {
				if (s == "") {
					s = ValDefStr;
				}
				else {
					s += ",";
				}
			}
//			if ((ValData->DoValBits != 0) &&
//				(MainF->SrcCodeType == MainF->sctFortran90)) {
			if (ValData->DoValBits != 0){
				// if(ValData->DoValBits != 0){
				// Do変数
				if (s == "") {
					s = ValDefStr;
				}
				s += " " + ValData->Str;
				for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++)
				{
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
				}
			}
			else if ((ValData->RefDoValBits & UnRollDoRefValBits) == 0) {
				// UnrollするDo変数の束縛を受けない変数。
				if (s == "") {
					s = ValDefStr;
				}
				s += " " + ValData->Str;
				for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++)
				{
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
				}
			}
			else {
				//
				// Do依存変数は、Ｎ個に多重化する。
				// これは、どのDo変数に依存しているかによってUnroll数が
				// 変化するので、それに合わせての形となる。
				// 束縛されているDo変数における、もっとも大きい数？
				//
				RollCount = GetValCountOfBit
					(ValData->RefDoValBits & UnRollDoRefValBits, UnrollCount);
				for (iusw = 1; iusw <= RollCount; iusw++) {
					if ((iusw % 4) == 1) {
						if (iusw == 1) { // ４変数ごとに改行する。
							s = ValDefStr + " " + ValData->Str;
						}
						else {
							if (ValDefStr.find('\n') != string::npos) {
								fprintf(fp, "%s", SepLongStr(s).c_str());
							}
							else {
								fprintf(fp, "%s\n", SepLongStr(s).c_str());
							}
							s = ValDefStr + " " + GetNewValName(ValData->Str,
								iusw, ArgValList);
						}
					}
					else {
						s += ", " + GetNewValName(ValData->Str, iusw,
							ArgValList);
					}
					for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE;
						j++) {
						Token = (TToken*)TokenList->Items[j];
						s += Token->OrgStr;
					}
				}
				if (s != "") {
					fprintf(fp, "%s\n", SepLongStr(s).c_str());
					s = "";
				}
			}
		}
		if (s != "") {
			fprintf(fp, "%s\n", SepLongStr(s).c_str());
			s = "";
		}
		//
		// Unrollで使う変数定義を行う。DoValCount全てでなく、Unrollが有効な変数のみ。
		// implicit対応のため、使用変数を全て対象に変更 2016/03/05
		//
//		if (MainF->SrcCodeType == MainF->sctFortran90) {
			for (int j = 0; j < DoValCount; j++) {
				if (UnrollCount[j] != 1) {
					s = "      integer " + DoVal_m[j] + "," + DoVal_i[j]
						+ "," + DoVal_l[j];
					fprintf(fp, "%s\n", SepLongStr(s).c_str());
				}
			}
//		}
		fprintf(fp,"\n");
		//
		// コードを出力する。基本的に範囲をトークンでスキャンして動く。
		// 必要に応じて文字列の置き換えや、必要コードのセットを行う。
		// 行単位で処理する。
		// 複数のＤＯに対応するために、スクリプトでの処理となる。
		// Doの出現順は、順番（ネスト）になっている（コンパイルエラーなし）
		// よって、DoValIdx = 0,1... 1, 0 の順に処理される。
		//
		DoValIdx = 0;
		// 共有変数 UnRollDoRefValBitsは、UnRollの深さごとにSet/Reset
		UnRollDoRefValBits = 0;
		CommentStrBeforeDo = "";
		OptionStr = "";
		for (TokPos = TokenStartPos; TokPos < TokenEndPos; TokPos++) {
			Token = (TToken*)TokenList->Items[TokPos];
			s = Token->OrgStr;
			//
			// Doの前のコメント群をひとまとめにして、Do側で処理する。
			// EndDoの後のコメント処理はDoコード生成部分で行う。
			//
			if ((Token->TokId == tid_Comment)&&(Token->Script == NULL)) {
				CommentStrBeforeDo += Token->OrgStr;
				continue;
			}else if ((Token->TokId == tid_LineEnd)&&(CommentStrBeforeDo != "")){
				CommentStrBeforeDo += Token->OrgStr;
				continue;
			}else if (Token->TokId != tid_DO){
				if(CommentStrBeforeDo != ""){
					fprintf(fp, "%s", CommentStrBeforeDo.c_str()); // コメント
					CommentStrBeforeDo = "";
				}
			}
			if (Token->Script != NULL) {
				// スクリプトは、Skip。スクリプトの後の改行もスキップ。
				if(((TScript *)Token->Script)->ScType == sct_Option){
					for(int k2 = TokPos + 1 ; k2 < TokenEndPos ; k2++) {
						Token = (TToken*)TokenList->Items[k2];
						OptionStr += Token->OrgStr;
						if (Token->TokId == tid_LineEnd) {
							break;
						}
					}
				}
//				while (TokPos + 1 < TokenEndPos) {
//					Token = (TToken*)TokenList->Items[TokPos + 1];
				while (TokPos < TokenEndPos) {
					Token = (TToken*)TokenList->Items[TokPos];
					if (Token->TokId == tid_LineEnd) {
						break;
					}
					TokPos++;
				}
				continue;
			}
			else if (Token->TokId == tid_LineEnd) {
				fprintf(fp, "%s", Token->OrgStr.c_str()); // 改行
				continue;
			}
			//
			// UnRool対象のDo変数を含む式およびDOブロックの多重化コードを出力する。
			// ブロックの場合は、ブロック終了まで解析をＳｋｉｐする。
			// かなりの、引数を渡すことになるので、クラスで共有する形とする。
			// DoRefValBitsは、多重化対象をクリアした形で渡され、内部でさらに
			// OR が行われる形で使用されることになる。
			// 多重化数(cc)の回数のブロックまたは行に多重化する。
			// RollCount = 1の場合もあるが、そのままのブロック・行となる。
			//
			if (Token->TokId == tid_DO) {
				// DOのブロックを出力(END_DO,Moduloの処理まで行う)
				// ここで行った多重化をマスクした DoRefValBitsを渡す。
//				EndTokPos = OutputUnroll_DoBlock_Fortran(fp, TokPos, 0, 0);
				EndTokPos = OutputUnroll_DoBlock_Fortran(fp, TokPos, 0, 0,CommentStrBeforeDo);
				CommentStrBeforeDo = "";
			}
			else if (Token->TokId == tid_IF) {
				// IFのブロックを出力 if .. endifが対象
				EndTokPos = OutputUnroll_IfBlock_Fortran(fp, TokPos, 0, 0);
			}
			else if (Token->ValData != NULL) {
				// １行を出力
				EndTokPos = OutputUnroll_Line_Fortran(fp, TokPos, 0, 0);
			}
			else {
				// DO,IF,代入文以外
				int j;
				s = "";
				for (j = TokPos; j < TokenEndPos; j++) {
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
					if (Token->TokId == tid_LineEnd) {
						break;
					}
				}
				fprintf(fp, "%s", SepLongStr(s).c_str());
				EndTokPos = j;
			}
			TokPos = EndTokPos;
		}
		if(CommentStrBeforeDo != ""){
//			fprintf(fp, "\n%s", Trim(CommentStrBeforeDo).c_str()); // コメント
			fprintf(fp, "%s", Trim(CommentStrBeforeDo).c_str()); // コメント
			CommentStrBeforeDo = "";
		}
		fprintf(fp,"\n");
		fprintf(fp, "      return\n");
		if (MainF->SrcCodeType == MainF->sctFortran90) {
			fprintf(fp, "      end subroutine %s_%d\n", FuncName.c_str(),
				CaseIdx);
		}
		else {
			fprintf(fp, "      end\n");
		}
		fprintf(fp, "\n");
		// サブルーチン終了（Case回繰り返される）
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
//
// 3.概要
// Unrollのサブルーチンを出力する
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputUnrollExecCode_C(FILE *fp, FILE *fpOutHeader) {
	int TokPos;
	int i, j, DefPosS;
	TValData *ValData;
	string s, s2, ValDefStr;
	string ArgValName;
	TToken *Token;
	string BaseValName;
	int iusw;
	int RollCount;
	int EndTokPos;
	int CaseIdx_for_Loop;
	string CommentStrBeforeDo;

	DoValCount = variedCount;
	BaseValName = BaseValList->Strings[0];
	//
	// Doで使用する変数名を作成する。
	//
	for (DoValIdx = 0; DoValIdx < DoValCount; DoValIdx++) {
		TValData *DoValData = (TValData*)DoValToken[DoValIdx]->ValData;

		DoTokPos[DoValIdx] = TokenList->IndexOf(DoToken[DoValIdx]);
		EndDoTokPos[DoValIdx] = FindEndDo2(TokenList, DoTokPos[DoValIdx]);
		DoValName[DoValIdx] = DoValData->Str;
		DoValStr[DoValIdx] = DoValData->Str;
		DoVal_m[DoValIdx] = GetNewValName(DoValData->Str, "m", ArgValList);
		DoVal_i[DoValIdx] = "???"; // Cでは未使用
		DoVal_l[DoValIdx] = GetNewValName(DoValData->Str, "l", ArgValList);
	}
	//
	// 指定種類だけのアンロールを行ったサブルーチンを作成する。
	// Sub名 . 引数 , 使用変数 , [コード] がセットされる。
	// コードによって、一時変数が必要となるので、それも追加される。
	//
	// for(CaseIdx = 1 ; CaseIdx <= CaseCount ; CaseIdx++){
	for (CaseIdx_for_Loop = 1; CaseIdx_for_Loop <= CaseCount;
		CaseIdx_for_Loop++) {
		//
		// DoValCountに合わせた、UnRoll数を求める。
		// 現状では、 ２段にLoopは対応していない。（RefBitsのクリアが必要）
		// また、DoValになる変数のUnrollDoRefValBitsを求める。
		//
		int idx2, c;
		TStringList *DefValStrList;
		TStringList *NewValStrList; // 多重化により新規に作成した変数。 temp2等

		//
		// GPUオプションを考慮したCaseIdxに修正。 2010/12/29
		//
		CaseIdx = CaseIdx_for_Loop;
		if (GPUOption == gpu_option_auto) {
			if (CaseIdx > CaseCount / 2) {
				CaseIdx -= CaseCount / 2;
			}
		}
		DefValStrList = new TStringList;
		NewValStrList = new TStringList;
		if ((CaseIdx % 10) == 0) {
			// MainF->print("Output Code "+IntToStr(CaseIdx)+" / "+IntToStr(CaseCount));
			if (MainF->CloseReqF) {
				return;
			}
		}
		UnRollDoRefValBits = 0;
		idx2 = CaseIdx - 1;
		for (int kk = DoValCount - 1; kk >= 0; kk--) {
			/** **************************************************** */
			//
			// Kogakuin Irie
			// PPの変動範囲を実数にも対応
			// 既存コードはコメントアウト
			//
			// c = (variedToValue[kk] - variedFromValue[kk] + 1);
			c = (int)(variedToValue[kk] - variedFromValue[kk] + 1);
			//
			// ここまで
			//
			/** **************************************************** */

			// UnrollCount[kk] = (idx2 % c)+1;

			/** **************************************************** */
			//
			// Kogakuin Irie
			// PPの変動範囲を実数にも対応
			// 既存コードはコメントアウト
			//
			// UnrollCount[kk] = (idx2 % c)+variedFromValue[kk];	// 2012/05/17
//			UnrollCount[kk] = (int)((idx2 % c) + variedFromValue[kk]);
			// Add StepValue 2016/03/05
			UnrollCount[kk] = (int)(variedStepValue[kk]*(idx2 % c) + variedFromValue[kk]);
			//
			// ここまで
			//
			/** **************************************************** */

			if (UnrollCount[kk] > 1) {
				UnRollDoRefValBits |= (1 << kk); // UnRollされるDoのBits
			}
			idx2 /= c;
		}
		//
		// 宣言部分
		//
		s = "";
		for (i = 0; i < ArgValList->Count; i++) {
			ArgValName = ArgValList->Strings[i];
			if (ArgValList->Strings[i] == "") {
				continue;
			}
			if (s != "") {
				s += ",";
			}
			ValData = (TValData*)ArgValList->Objects[i];
			s += ValData->GetDefStr_C() + ValData->Str;
			for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++) {
				Token = (TToken*)TokenList->Items[j];
				s += Token->OrgStr;
			}
		}
		s2 = FuncName + "_" + IntToStr(CaseIdx_for_Loop) + "(" + s + ")";
		fprintf(fp, "int %s\n", s2.c_str());
		fprintf(fpOutHeader, "int %s;\n", s2.c_str());
		fprintf(fp, "{\n");
		//
		// 引数以外の変数の変数宣言を追加する。
		// DO変数は対象外。DO依存変数は、名前を変えて複数にする。
		//
		DefPosS = -1;
		s = "";
		ValDefStr = "";
		DefValStrList->Clear();
		NewValStrList->Clear();
		for (i = 0; i < ArgValList->Count; i++) {
			if (ArgValList->Strings[i] != "") { // 引数の変数は対象外
				continue;
			}
			ValData = (TValData*)ArgValList->Objects[i];
			if (ValData->DefPosS == -1) { // 定義なし
				continue;
			}
			if (DefValStrList->IndexOf(ValData->Str) != -1) {
				continue;
			}
			DefValStrList->Add(ValData->Str);

			if (ValData->DefPosS != DefPosS) { // 違う定義行
				if (s != "") {
					fprintf(fp, "%s;\n", s.c_str());
					s = "";
				}
				ValDefStr = ValData->DefStr1;
				DefPosS = ValData->DefPosS;
			}

			if ((ValData->RefDoValBits & UnRollDoRefValBits) == 0) {
				// UnrollするDo変数の束縛を受けない変数。
				if (s == "") {
					s = ValDefStr;
				}
				else {
					s += ",";
				}
				s += ValData->DefStr2 + " " + ValData->Str;
				for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++)
				{
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
				}
			}
			else if (ValData->DoValBits != 0) {
				// Do Val変数自体は多重化しない。 Add 2010/08/14
				if (s == "") {
					s = ValDefStr;
				}
				else {
					s += ",";
				}
				// s += " " + ValData->Str;
				s += ValData->DefStr2 + " " + ValData->Str;
				for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE; j++)
				{
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
				}
			}
			else {
				//
				// Do依存変数は、Ｎ個に多重化する。
				// これは、どのDo変数に依存しているかによってUnroll数が
				// 変化するので、それに合わせての形となる。
				// 束縛されているDo変数における、もっとも大きい数？
				//
				string NewValName;
				int ValCount = 0;

				if (s != "") {
					fprintf(fp, "%s;\n", s.c_str());
					s = "";
				}
				RollCount = GetValCountOfBit
					(ValData->RefDoValBits & UnRollDoRefValBits, UnrollCount);
				for (iusw = 1; iusw <= RollCount; iusw++) {
					if (iusw == 1) {
						NewValName = ValData->Str;
					}
					else {
						NewValName = GetNewValName(ValData->Str, iusw,
							ArgValList);
						if (DefValStrList->IndexOf(NewValName) != -1) {
							continue;
						}
						DefValStrList->Add(NewValName);
						NewValStrList->Add(ValData->Str);
						NewValStrList->Add(NewValName);
					}
					if (s == "") {
						s = ValDefStr + ValData->DefStr2 + " " + NewValName;
					}
					else {
						s += "," + ValData->DefStr2 + NewValName;
					}
					for (j = ValData->ArrayDefPosS; j < ValData->ArrayDefPosE;
						j++) {
						Token = (TToken*)TokenList->Items[j];
						s += Token->OrgStr;
					}
					if ((ValCount++ % 8) == 7) { // ８個ごとに改行
						if (s != "") {
							fprintf(fp, "%s;\n", s.c_str());
							s = "";
						}
					}
				}
				if (s != "") {
					fprintf(fp, "%s;\n", s.c_str());
					s = "";
				}
			}
		}
		if (s != "") {
			fprintf(fp, "%s;\n", s.c_str());
			s = "";
		}
		//
		// Unrollで使う変数定義を行う。DoValCount全てでなく、Unrollが有効な変数のみ。
		// これは、Cの場合は、変数なしで埋め込みとした、Fortranは以前と同じ。
		//
#if 1   // cut im = xxx 2012/09/27 ,add im = xxx 2012/10/05
		for (int j = 0; j < DoValCount; j++) {
			if (UnrollCount[j] != 1) {
				s = OffsetStr + "\tint " + DoVal_m[j];
				fprintf(fp, "%s;\n", s.c_str());
			}
		}
		fprintf(fp, "\n");
		for (int j = 0; j < DoValCount; j++) {
			if (UnrollCount[j] != 1) {
				s = OffsetStr + "\t" + DoVal_m[j] + " = (" + BaseValName +
					" / " + IntToStr(UnrollCount[j]) + ") * " + IntToStr
					(UnrollCount[j]);
				fprintf(fp, "%s;\n", s.c_str());
			}
		}
#endif
		fprintf(fp, "\n");
		//
		// GPUに対応する #Pragmaを挿入	2010/12/29
		//
		if ((GPUOption == gpu_option_GPU) || ((GPUOption == gpu_option_auto) &&
				(CaseIdx_for_Loop > CaseIdx))) {
			if (MainF->cc_option_str == "PGI") {
				fprintf(fp, "#pragma acc region\n");
				fprintf(fp, "{ // #pragma allocate region start.\n");
			}
			else if (MainF->cc_option_str == "OMPCUDA") {
				fprintf(fp, "#pragma OMPCUDA gpu region\n");
				fprintf(fp, "{ // #pragma allocate region start.\n");
			}
		}
		//
		// コードを出力する。基本的に範囲をトークンでスキャンして動く。
		// 必要に応じて文字列の置き換えや、必要コードのセットを行う。
		// 行単位で処理する。
		// 複数のＤＯに対応するために、スクリプトでの処理となる。
		// Doの出現順は、順番（ネスト）になっている（コンパイルエラーなし）
		// よって、DoValIdx = 0,1... 1, 0 の順に処理される。
		//
		DoValIdx = 0;
		// 共有変数 UnRollDoRefValBitsは、UnRollの深さごとにSet/Reset
		UnRollDoRefValBits = 0;
		CommentStrBeforeDo = "";
		OptionStr = "";
		for (TokPos = TokenStartPos; TokPos < TokenEndPos; TokPos++) {
			Token = (TToken*)TokenList->Items[TokPos];
			s = Token->OrgStr;
			//
			// forの前の#Paragma群をひとまとめにして、for側で処理する。
			// forの}の後のコメント処理はDoコード生成部分で行う。
			//
			if ((Token->TokId == tid_SharpPragma)&&(Token->Script == NULL)) {
				// #pragme （OATのスクリプト以外）
				int j;

				for (j = TokPos + 1; j < TokenEndPos; j++) {
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
					if (Token->LineEndF) {
						break;
					}
				}
				EndTokPos = j;
				s = ChangePragmaStr(s, TokPos, EndTokPos, NewValStrList);
				CommentStrBeforeDo += s;
//				fprintf(fp, "%s", s.c_str());
				TokPos = EndTokPos;
				continue;
			}else if ((Token->TokId == tid_LineEnd)&&(CommentStrBeforeDo != "")){
				CommentStrBeforeDo += Token->OrgStr;
				continue;
			}else if (Token->TokId != tid_for){
				if(CommentStrBeforeDo != ""){
					fprintf(fp, "%s", CommentStrBeforeDo.c_str()); // コメント
					CommentStrBeforeDo = "";
				}
			}
			if (Token->Script != NULL) {
				// スクリプトは、Ｓｋｉｐ
				// スクリプトは、Skip。スクリプトの後の改行もスキップ。
				if(((TScript *)Token->Script)->ScType == sct_Option){
					for(int k2 = TokPos + 1 ; k2 < TokenEndPos ; k2++) {
						Token = (TToken*)TokenList->Items[k2];
						OptionStr += Token->OrgStr;
						if (Token->TokId == tid_LineEnd) {
							break;
						}
					}
				}
				while (TokPos < TokenEndPos) {
					Token = (TToken*)TokenList->Items[TokPos];
					if (Token->TokId == tid_LineEnd) {
						break;
					}
					TokPos++;
				}
				continue;
			}
			else if (Token->TokId == tid_Comment) {
				fprintf(fp, "%s", Token->OrgStr.c_str()); // コメントは、そのまま
				continue;
			}
			else if (Token->TokId == tid_LineEnd) {
				fprintf(fp, "%s", Token->OrgStr.c_str()); // 改行も、そのまま
				continue;
			}
			//
			// UnRool対象のDo変数を含む式およびDOブロックの多重化コードを出力する。
			// ブロックの場合は、ブロック終了まで解析をＳｋｉｐする。
			// かなりの、引数を渡すことになるので、クラスで共有する形とする。
			// DoRefValBitsは、多重化対象をクリアした形で渡され、内部でさらに
			// OR が行われる形で使用されることになる。
			// 多重化数(cc)の回数のブロックまたは行に多重化する。
			// RollCount = 1の場合もあるが、そのままのブロック・行となる。
			//
			// if(Token->TokId == tid_DO){
			if (Token->TokId == tid_for) {
				// DOのブロックを出力(END_DO,Moduloの処理まで行う)
				// ここで行った多重化をマスクした DoRefValBitsを渡す。
//				EndTokPos = OutputUnroll_DoBlock_C(fp, TokPos, 0, 0);
//				fprintf(fp, "%s", CommentStrBeforeDo.c_str()); // コメント
				EndTokPos = OutputUnroll_DoBlock_C(fp, TokPos, 0, 0,CommentStrBeforeDo);
				CommentStrBeforeDo = "";
			}
			else if (Token->TokId == tid_if) {
				// IFのブロックを出力 if .. endifが対象
				EndTokPos = OutputUnroll_IfBlock_C(fp, TokPos, 0, 0);
			}
			else if (Token->ValData != NULL) {
				// １行を出力
				EndTokPos = OutputUnroll_Line_C(fp, TokPos, 0, 0);
			}
			else {
				// DO,IF,代入文以外
				int j;
				s = "";
				for (j = TokPos; j < TokenEndPos; j++) {
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
					if (Token->LineEndF) {
						break;
					}
				}
				fprintf(fp, "%s", s.c_str());
				EndTokPos = j;
			}
			TokPos = EndTokPos;
		}
		if(CommentStrBeforeDo != ""){
//			fprintf(fp, "\n%s", Trim(CommentStrBeforeDo).c_str()); // コメント
			fprintf(fp, "%s", Trim(CommentStrBeforeDo).c_str()); // コメント
			CommentStrBeforeDo = "";
		}
		//
		// GPUに対応する #Pragma ... { に対応する }閉じるを挿入	2011/1/18
		//
		if ((GPUOption == gpu_option_GPU) || ((GPUOption == gpu_option_auto) &&
				(CaseIdx_for_Loop > CaseIdx))) {
			if (MainF->cc_option_str == "PGI") {
				fprintf(fp, "} // #pragma allocate region end. \n");
			}
			else if (MainF->cc_option_str == "OMPCUDA") {
				fprintf(fp, "} // #pragma allocate region end. \n");
			}
		}
		fprintf(fp, "\n");
		fprintf(fp, "\treturn 0;\n");
		fprintf(fp, "}\n");
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		// サブルーチン終了（Case回繰り返される）
		delete DefValStrList;
		delete NewValStrList;
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// TokPos  トークン位置
// UseDoRefValBits 使用中ＤＯ参照変数Ｂｉｔｓ
// RefValIdx   参照変数インデックス
//
// 3.概要
// 多重化したコードを１行出力する。
// UseDoRefValBitsとRefValIdxによるDo参照変数部分の置換を行った行を出力する。
// RefValIdxは、Countに対しての番号となる。（呼び出し側から指定される）
// ここから、別の解析を呼ぶことはない。
// 最後のトークン（通常はtid_LineEnd)の位置を返す。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::OutputUnroll_Line_Fortran(FILE *fp, int TokPos,
	unsigned int UseDoRefValBits, int RefValIdx) {
	string s;
	int Idx;
	int AddValue; // 変数への加算数
	TValData *ValData;
	TToken *Token, *TokenB, *TokenF;
	int j;
	string OprCheckStr = "(),";
	string NewValName;

	s = "";
	if (UseDoRefValBits == 0) { // UnRollなしの場合は、そのまま出力
		for (; TokPos < TokenEndPos; TokPos++) {
			Token = (TToken*)TokenList->Items[TokPos];
			s += Token->OrgStr;
			if (Token->TokId == tid_LineEnd) {
				break;
			}
		}
		if ((s != "") && (s[1] == ' ')) {
			s = OffsetStr + s;
		}
		s = SepLongStr(s);
		fprintf(fp, "%s", s.c_str());
		return TokPos;
	}
	//
	// DO依存変数または、Do変数を含んだ式！
	// 変数部分の置換を行う
	//
	for (j = TokPos; j < TokenEndPos; j++) { // 行の先頭から、最後
		Token = (TToken*)TokenList->Items[j];
		if (Token->TokId == tid_LineEnd) {
			TokPos = j;
			break;
		}
		ValData = (TValData*)Token->ValData;
		if (ValData != NULL) {
			if (ValData->DoValBits != 0) {
				//
				// DO変数の置き換え
				// 前後が オペレータでなければ、()を付けない。
				// 加算する値は、UnrollCount[] と kk によって決まる
				//
				Idx = GetBitIdx(ValData->DoValBits);
				TokenB = (TToken*)TokenList->Items[j - 1];
				TokenF = (TToken*)TokenList->Items[j + 1];

				// fprintf(fp,"AddValue %s[RefValIdx=%d:DoValBits %04X] DoValIdx = %d UnRollBits = %04X \n",ValData->Str.c_str(),
				// RefValIdx,ValData->DoValBits,DoValIdx,UnRollDoRefValBits);

				AddValue = GetDoValAddValue(RefValIdx,
					ValData->DoValBits & UnRollDoRefValBits, DoValIdx,
					UnrollCount);
				if (AddValue == 0) {
					s += ChangeFromOrgStr(Token->OrgStr, "");
					s += DoValName[Idx];
				}
				else if ((OprCheckStr.find(TokenB->Str) != string::npos) &&
					(OprCheckStr.find(TokenF->Str) != string::npos)) {
					s += ChangeFromOrgStr(Token->OrgStr, "");
					s += DoValName[Idx] + "+" + IntToStr(AddValue);
				}
				else {
					s += ChangeFromOrgStr(Token->OrgStr, "");
					s += "(" + DoValName[Idx] + "+" + IntToStr(AddValue) + ")";
				}
			}
			else if (((ValData->RefDoValBits & UnRollDoRefValBits) != 0) &&
				(ValData->ArrayLevel == 0)) {
				//
				// Do依存変数の置き換え
				// ・配列は、対象外とする。(Doの変数が含まれると想定)
				// ・Do依存変数のAddValueを求める（Idx)
				//
				AddValue = GetValAddValue(RefValIdx,
					ValData->RefDoValBits & UnRollDoRefValBits, DoValIdx,
					UnrollCount);
				if (AddValue == 0) {
					NewValName = ValData->Str;
				}
				else {
					NewValName = GetNewValName(ValData->Str, AddValue + 1,
						ArgValList);
				}
				s += ChangeFromOrgStr(Token->OrgStr, NewValName);
			}
			else {
				s += Token->OrgStr;
			}
		}
		else {
			s += Token->OrgStr;
		}
	}
	if ((s != "") && (s[1] == ' ')) {
		s = OffsetStr + s;
	}
	s = SepLongStr(s);
	fprintf(fp, "%s\n", s.c_str());
	return TokPos;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// TokPos  トークン位置
// UseDoRefValBits 使用中ＤＯ参照変数Ｂｉｔｓ
// RefValIdx   参照変数インデックス
//
// 3.概要
// 多重化したコードを１行出力する。
// UseDoRefValBitsとRefValIdxによるDo参照変数部分の置換を行った行を出力する。
// RefValIdxは、Countに対しての番号となる。（呼び出し側から指定される）
// ここから、別の解析を呼ぶことはない。
// 最後のトークン（通常はtid_LineEnd)の位置を返す。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::OutputUnroll_Line_C(FILE *fp, int TokPos,
	unsigned int UseDoRefValBits, int RefValIdx) {
	string s;
	int Idx;
	int AddValue; // 変数への加算数
	TValData *ValData;
	TToken *Token, *TokenB, *TokenF;
	int j;
	string OprCheckStr = "(),";
	string NewValName;
	string NewLastOutputStr;
	int StartNestLevel;
	static long int LastOutputSPos = 0;
	static long int LastOutputEPos = 0;
	static string BaseOutputStr = "";
	static string LastOutputStr = "";
	long int CurOutputPos;
	int AddOprStrPos = -1;
	int StartTokPos;

	Token = (TToken*)TokenList->Items[TokPos];
	if (Token->TokId == tid_DaiKokka) {
		s = OffsetStr + Token->OrgStr;
		fprintf(fp, "%s", s.c_str());
		return TokPos;
	}
	StartTokPos = TokPos;
	StartNestLevel = Token->BrNestLevel;
	s = "";
	if (UseDoRefValBits == 0) { // UnRollなしの場合は、そのまま出力
		for (; TokPos < TokenEndPos; TokPos++) {
			Token = (TToken*)TokenList->Items[TokPos];
			s += Token->OrgStr;
			if (Token->LineEndF) {
				break;
			}
		}
		s = OffsetStr + s;
		fprintf(fp, "%s", s.c_str());
		return TokPos;
	}
	//
	// DO依存変数または、Do変数を含んだ式！
	// 変数部分の置換を行う
	//
	for (j = TokPos; j < TokenEndPos; j++) { // 行の先頭から、最後
		Token = (TToken*)TokenList->Items[j];
		if (Token->TokId == tid_Semikoron) { // ; が同じレベルで出現時
			if (Token->BrNestLevel >= StartNestLevel) {
				s += TrimRight(Token->OrgStr);
				TokPos = j;
				break;
			}
		}
		if ((StartNestLevel == Token->BrNestLevel) && (AddOprStrPos == -1)) {
			if ((Token->Str == "+") || (Token->Str == "-")) {
				//
				// 最初の+-の出現位置。合計計算の式のマージをチェックする。
				// １つ前の = まで戻って、それが一致しているかをチェック。
				//
				int k;
				int SetPos = -1;

				AddOprStrPos = -2;
				for (k = j - 1; k > 0; k--) {
					TokenB = (TToken*)TokenList->Items[k];
					if (TokenB->TokId == tid_Set) {
						SetPos = k;
						break;
					}
				}
				if (SetPos != -1) {
					for (k = j - 1; k > SetPos; k--) {
						TokenB = (TToken*)TokenList->Items[k];
						TokenF = (TToken*)
							TokenList->Items[k - (j - 1) + SetPos - 1];
						if (TokenB->Str != TokenF->Str) {
							break; // 不一致
						}
					}
					if (k == SetPos) { // 一致 Val = Val + の形。
						if (k - (j - 1) + SetPos == StartTokPos) {
							AddOprStrPos = s.length();
						}
					}
				}
			}
		}

		ValData = (TValData*)Token->ValData;
		if (ValData != NULL) {
			if (ValData->DoValBits != 0) {
				//
				// DO変数の置き換え
				// 前後が オペレータでなければ、()を付けない。
				// 加算する値は、UnrollCount[] と kk によって決まる
				//
				Idx = GetBitIdx(ValData->DoValBits);
				TokenB = (TToken*)TokenList->Items[j - 1];
				TokenF = (TToken*)TokenList->Items[j + 1];

				// fprintf(fp,"AddValue %s[RefValIdx=%d:DoValBits %04X] DoValIdx = %d UnRollBits = %04X \n",ValData->Str.c_str(),
				// RefValIdx,ValData->DoValBits,DoValIdx,UnRollDoRefValBits);

				AddValue = GetDoValAddValue(RefValIdx,
					ValData->DoValBits & UnRollDoRefValBits, DoValIdx,
					UnrollCount);
				if (AddValue == 0) {
					s += ChangeFromOrgStr(Token->OrgStr, "");
					s += DoValName[Idx];
				}
				else if ((OprCheckStr.find(TokenB->Str) != string::npos) &&
					(OprCheckStr.find(TokenF->Str) != string::npos)) {
					s += ChangeFromOrgStr(Token->OrgStr, "");
					s += DoValName[Idx] + "+" + IntToStr(AddValue);
				}
				else {
					s += ChangeFromOrgStr(Token->OrgStr, "");
					s += "(" + DoValName[Idx] + "+" + IntToStr(AddValue) + ")";
				}
			}
			else if (((ValData->RefDoValBits & UnRollDoRefValBits) != 0) &&
				(ValData->ArrayLevel == 0)) {
				//
				// Do依存変数の置き換え
				// ・配列は、対象外とする。(Doの変数が含まれると想定)
				// ・Do依存変数のAddValueを求める（Idx)
				//
				AddValue = GetValAddValue2(RefValIdx, ValData->RefDoValBits,
					UnRollDoRefValBits, DoValIdx, UnrollCount);
				if (AddValue == 0) {
					NewValName = ValData->Str;
				}
				else {
					NewValName = GetNewValName(ValData->Str, AddValue + 1,
						ArgValList);
				}
				s += ChangeFromOrgStr(Token->OrgStr, NewValName);
			}
			else {
				s += Token->OrgStr;
			}
		}
		else {
			s += Token->OrgStr;
		}
	}
	//
	// 出力行がマージ可能( Val = Val + ... ）ならばマージする。
	//
	CurOutputPos = ftell(fp);
	if ((AddOprStrPos >= 0) && (CurOutputPos == LastOutputEPos) &&
		(LastOutputSPos < LastOutputEPos)) {
		NewLastOutputStr = Merge_Unroll_Line_C(&BaseOutputStr, LastOutputStr,
			&s, AddOprStrPos);
		if (NewLastOutputStr != LastOutputStr) {
			fseek(fp, LastOutputSPos, SEEK_SET);
			fprintf(fp, "%s\n", NewLastOutputStr.c_str());
			CurOutputPos = ftell(fp);
		}
		else {
			BaseOutputStr = s;
		}
	}
	else {
		BaseOutputStr = s;
	}
	LastOutputSPos = CurOutputPos;
	s = OffsetStr + s;
	LastOutputStr = s;
	fprintf(fp, "%s\n", s.c_str());
	LastOutputEPos = ftell(fp);
	return TokPos;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// BaseOutputStr  基準になる元の式
// LastOutputStr  最後に出力された式
// CurLineStr  今回出力する式
// AddOprStrPos 最初に+が出現した位置。この手前までがカットされる。
//
// 3.概要
// Unrollでの加算の複数回の式を１つにマージする。
// 式の先頭が Val[xx] = Val[xx] + (or - ）かどうかを調べて、合計型ならば
// 1回目と2回目以降を合わせた１式に変更する。 1回目は最後の　; をカット
// 2回目以降は先頭のVal[xx] = Val[xx]と最後の ; をカット
// 最後は先頭のVal[xx] = Val[xx]をカットして、最後の ;  は残す。
// 途中で カンマやセミコロンが2回ある場合や変数の前に(がある場合は対象外。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
string TTuneRegion::Merge_Unroll_Line_C(string *BaseOutputStr,
	string LastOutputStr, string *CurLineStr, int AddOprStrPos) {
	int i;
	string BaseStr = *BaseOutputStr;
	string CurStr = *CurLineStr;

	// 元の式と今回の式が文字列として規定の位置まで一致しているかを調べる。
	for (i = 0; i < (int)BaseStr.length(); i++) {
		if (i >= (int)CurStr.length()) {
			break;
		}
		if (BaseStr[i] != CurStr[i]) {
			break;
		}
	}
	//
	// 最初の加算の前まで一致していれば、合計の計算としてマージする。
	//
	if (i > AddOprStrPos) {
		// CurLieneStrの先頭部分をスペースにする。
		for (i = 0; i < AddOprStrPos; i++) {
			CurStr[i] = ' ';
		}
		*CurLineStr = CurStr;
		// 最後の ; をスペースにする。
		for (i = LastOutputStr.length() - 1; i >= 0; i--) {
			if (LastOutputStr[i] == ';') {
				LastOutputStr[i] = ' ';
				break;
			}
		}
	}
	return LastOutputStr;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// TokPos  トークン位置
// UsedDoRefValBits    使用中ＤＯ参照変数Ｂｉｔｓ
// RefValIdx   参照変数インデックス
//
// 3.概要
// 多重化したDO_Blockを出力する。
// RefValIdxは、Countに対しての番号となる。（呼び出し側から指定される）
// 最後のトークン（通常はtid_LineEnd)の位置を返す。
// 多重化に合わせた変数の置換も行われる。
//
// UseDoRefValBitsは、このブロックで分割対象となっているDoRefValのbitを示す。
// RefValIdxは、UseDoRefValBitsで1になっているBitのCount総和における番号を示
// す。
// 呼び出し時のUseDoRefValBitsから新たに1になるBitsだけが繰り返し対象となる。
// その場合のRefValIdxは、新たなUseDoRefValBitsでのIdxに再計算され先に渡され
// る。
//
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::OutputUnroll_DoBlock_Fortran(FILE *fp, int TokPos,
	unsigned int UsedDoRefValBits, int RefValIdx,string CommentStrBeforeDo) {
	int EndTokPos;
	TToken *Token, *Token2;
	TValData *ValData;
	string s;
	int i, j;
	int RollCount;
	int bkDoValIdx = DoValIdx;
	bool UnRollDoF = false;
	bool StartValueIs1F;
	unsigned int DoRefValBits;
	unsigned int NextUsedDoRefValBits; // 次に渡す Do変数参照(処理済)bit
	unsigned int NowUseDoRefValBits; // 今回の分割数となる UseDoRefValBits
	int NewRefValIdx; // 次に渡すRefValIdx (of NextUseDoRefValBits)
	unsigned int bkUnRollDoRefValBits = UnRollDoRefValBits;
	int AddValue;
	string CommentStrAfterDo;
	int EndTokPosAfterComment = -1;
	bool OptionInheritF;

	OptionInheritF = (LowerCase(OptionStr).find("inherit") != string::npos); // Option
	EndTokPos = TokenEndPos;
	//
	// DO文の入り口の処理を行う。
	//
	Token = (TToken*)TokenList->Items[TokPos];
	if ((Token != DoToken[DoValIdx]) || (UnrollCount[DoValIdx] <= 1)) {
		//
		// UnRoll対象でない場合は、DO文の行は、DO参照変数の置換のみを行って戻る。
		//
		if (Token == DoToken[DoValIdx]) {
			DoValIdx++;
		}
		if(Trim(CommentStrBeforeDo) != ""){
			fprintf(fp, "%s\n", Trim(CommentStrBeforeDo).c_str());
		}
		TokPos = OutputUnroll_Line_Fortran(fp, TokPos, UsedDoRefValBits,
			RefValIdx);
		TokPos++; // LineEndをSkipする
	}
	else {
		//
		// UnRoll対象ＤＯ文の入り口の処理を行う。
		// DOの前処理の式を挿入する。（DO依存変数を含まない)
		// この場合も、DO参照変数の置換は、対象とすること。
		//
		// Exp.       im =  N/2  // Unrollした変数
		// i = 1		 // 参照変数
		// do ii=1,im // doの実行回数も変更
		//
		// [       im = N/2]

		UnRollDoF = true;
		UnRollDoRefValBits |= (1 << DoValIdx); // 変数Bitsをセットする。

		s = DoVal_m[DoValIdx] + " = ";
		s = ChangeFromOrgStr(DoToken[DoValIdx]->OrgStr, s); // Doの変数を置換

		// 開始が１かチェック
		StartValueIs1F = false;
		if (DoStartSPos[DoValIdx] + 1 == DoStartEPos[DoValIdx]) {
			Token = (TToken*)TokenList->Items[DoStartSPos[DoValIdx]];
			if (Trim(Token->OrgStr) == "1") {
				StartValueIs1F = true;
			}
		}

		if ((DoStartSPos[DoValIdx] + 1 != DoStartEPos[DoValIdx]) ||
			(!StartValueIs1F)) {
			s += "(";
		}
		for (i = DoEndSPos[DoValIdx]; i < DoEndEPos[DoValIdx]; i++) {
			Token = (TToken*)TokenList->Items[i];
			// ここも、変数変更
			ValData = (TValData*)Token->ValData;
			AddValue = 0;
			if ((ValData != NULL) &&
				((ValData->RefDoValBits & UnRollDoRefValBits) != 0)) {
				AddValue = GetValAddValue(RefValIdx,
					ValData->RefDoValBits & UnRollDoRefValBits, DoValIdx,
					UnrollCount);
			}
			if (AddValue != 0) {
				s += "(" + Token->OrgStr + "+" + IntToStr(AddValue) + ")";
			}
			else {
				s += Token->OrgStr;
			}
		}
		if (!StartValueIs1F) {
			s += "-";
			for (i = DoStartSPos[DoValIdx]; i < DoStartEPos[DoValIdx]; i++) {
				Token = (TToken*)TokenList->Items[i];
				// ここも、変数変更
				ValData = (TValData*)Token->ValData;
				AddValue = 0;
				if ((ValData != NULL) &&
					((ValData->RefDoValBits & UnRollDoRefValBits) != 0)) {
					AddValue = GetValAddValue(RefValIdx,
						ValData->RefDoValBits & UnRollDoRefValBits, DoValIdx,
						UnrollCount);
				}
				if (AddValue != 0) {
					s += "(" + Token->OrgStr + "+" + IntToStr(AddValue) + ")";
				}
				else {
					s += Token->OrgStr;
				}
			}
			s += "+1";
		}
		if ((DoStartSPos[DoValIdx] + 1 != DoStartEPos[DoValIdx]) ||
			(!StartValueIs1F)) {
			s += ")";
		}
		if (DoStepSPos[DoValIdx] == DoStepEPos[DoValIdx]) {
			s += "/" + IntToStr(UnrollCount[DoValIdx]);
		}
		else {
			s += "/(" + IntToStr(UnrollCount[DoValIdx]) + "*(";
			for (i = DoStepSPos[DoValIdx]; i < DoStepEPos[DoValIdx]; i++) {
				Token = (TToken*)TokenList->Items[i];
				// ここも、変数変更
				ValData = (TValData*)Token->ValData;
				AddValue = 0;
				if ((ValData != NULL) &&
					((ValData->RefDoValBits & UnRollDoRefValBits) != 0)) {
					AddValue = GetValAddValue(RefValIdx,
						ValData->RefDoValBits & UnRollDoRefValBits, DoValIdx,
						UnrollCount);
				}
				if (AddValue != 0) {
					s += "(" + Token->OrgStr + "+" + IntToStr(AddValue) + ")";
				}
				else {
					s += Token->OrgStr;
				}
				// s += Token->OrgStr;
			}
			s += "))";
		}
		s = SepLongStr(s);
		fprintf(fp, "%s\n", s.c_str());
		// [       i = 1]
		s = DoValStr[DoValIdx] + " = ";
		s = ChangeFromOrgStr(DoToken[DoValIdx]->OrgStr, s); // Doの変数を置換
		for (i = DoStartSPos[DoValIdx]; i < DoStartEPos[DoValIdx]; i++) {
			Token = (TToken*)TokenList->Items[i];
			// ここも、変数変更
			ValData = (TValData*)Token->ValData;
			AddValue = 0;
			if ((ValData != NULL) &&
				((ValData->RefDoValBits & UnRollDoRefValBits) != 0)) {
				AddValue = GetValAddValue(RefValIdx,
					ValData->RefDoValBits & UnRollDoRefValBits, DoValIdx,
					UnrollCount);
			}
			if (AddValue != 0) {
				s += "(" + Token->OrgStr + "+" + IntToStr(AddValue) + ")";
			}
			else {
				s += Token->OrgStr;
			}
			// s += Token->OrgStr;
		}
		s = SepLongStr(s);
		fprintf(fp, "%s\n", s.c_str());
		if(Trim(CommentStrBeforeDo) != ""){
			fprintf(fp, "%s\n", Trim(CommentStrBeforeDo).c_str());
		}
		// Do文の出力を行う
		s = "";
		for (i = TokPos; i < DoEndEPos[DoValIdx]; i++) {
			Token = (TToken*)TokenList->Items[i];
			if (Token == DoValToken[DoValIdx]) {
				s += ChangeFromOrgStr(Token->OrgStr, DoVal_i[DoValIdx]);
				// Doの変数を置換
				continue;
			}
			if (i == DoStartSPos[DoValIdx]) {
				s += "1"; // 開始値は、常に１
				continue;
			}
			else if ((i > DoStartSPos[DoValIdx]) && (i < DoStartEPos[DoValIdx])
				) {
				continue; // 開始値をSkip
			}
			if (i == DoEndSPos[DoValIdx]) {
				s += DoVal_m[DoValIdx]; // Doの終了式を置換
				i = DoEndEPos[DoValIdx] - 1;
				continue;
			}
			s += Token->OrgStr;
			if (Token->TokId == tid_LineEnd) {
				break;
			}
		}
		s = SepLongStr(s);
		fprintf(fp, "%s", s.c_str());
		// Do文の終わりまで TokPosをSkip
		TokPos = DoEndEPos[DoValIdx];
		DoValIdx++; // Next Nest Do （Doの最後で戻る）
	}
	// DoValIdx++; // Next Nest Do （Doの最後で戻る）

	//
	// EndDoまでスキャンする。
	//
	for (; TokPos < TokenEndPos; TokPos++) {
		Token = (TToken*)TokenList->Items[TokPos];
		if (Token->Script != NULL) {
			// スクリプトは、Ｓｋｉｐ
			continue;
		}
		else if (Token->TokId == tid_Comment) {
			fprintf(fp, "%s", Token->OrgStr.c_str()); // コメントは、そのまま
			continue;
		}
		else if (Token->TokId == tid_LineEnd) {
			fprintf(fp, "%s", Token->OrgStr.c_str()); // 改行も、そのまま
			continue;
		}
		else if (Token->TokId == tid_ENDDO) {
			// EndDoの処理へ
			break;
		}
		//
		// 行内で、UnRoll有効中のDo変数を参照しているかを調べる。
		// もし、使用中であれば、多重化の対象となる。
		// 多重化は、代入文とDo文とする。(2004/08/26)
		//
		DoRefValBits = 0;
		for (j = TokPos; j < TokenEndPos; j++) {
			Token2 = (TToken*)TokenList->Items[j];
			if (Token2->TokId == tid_LineEnd) {
				break;
			}
			if (Token2->TokId == tid_DO) {
				// Doの後の変数をスキップ(除外)　DO 100などはサポート外
				j++;
			}
			if (Token2->ValData == NULL) {
				continue;
			}
			ValData = (TValData*)Token2->ValData;
			DoRefValBits |= ValData->RefDoValBits;
		}
		DoRefValBits &= UnRollDoRefValBits;
		// 次に渡す Do変数参照(処理済)bitを決定（OR）
		NextUsedDoRefValBits = DoRefValBits | UsedDoRefValBits;
		NowUseDoRefValBits = DoRefValBits & (~UsedDoRefValBits);
		RollCount = GetValCountOfBit(NowUseDoRefValBits, UnrollCount);
		for (int AndRefValIdx = 0; AndRefValIdx < RollCount; AndRefValIdx++) {
			//
			// NewRefValIdxを計算する。
			//
			if (RollCount <= 1) { // 分割が発生しない場合は、呼び出し元がそのまま
				NewRefValIdx = RefValIdx;
			}
			else {
				// 新たに分割が発生する場合には、計算が必要
				// RefValIdx : UseDoRefValBits
				// kk ; AndUseDoRefValBits の２つを合成した NewRefValIdxを計算。
				// [32] の要素で合成する。
				//
				NewRefValIdx = CalNewRefValIdx(RefValIdx, UsedDoRefValBits,
					AndRefValIdx, NowUseDoRefValBits, UnrollCount);
			}
			if (Token->TokId == tid_DO) {
				// DOのブロックを出力(END_DO,Moduloの処理まで行う)
				EndTokPos = OutputUnroll_DoBlock_Fortran(fp, TokPos,
					NextUsedDoRefValBits, NewRefValIdx);
				if (AndRefValIdx < RollCount - 1) { // 継続がある場合は改行
					fprintf(fp, "\n");
				}
			}
			else if (Token->TokId == tid_IF) {
				// IFのブロックを出力 if .. endifが対象
				EndTokPos = OutputUnroll_IfBlock_Fortran(fp, TokPos,
					NextUsedDoRefValBits, NewRefValIdx);
				if (AndRefValIdx < RollCount - 1) { // 継続がある場合は改行
					fprintf(fp, "\n");
				}
			}
			else if (Token->ValData != NULL) {
				// 代入文１行を出力
				EndTokPos = OutputUnroll_Line_Fortran(fp, TokPos,
					NextUsedDoRefValBits, NewRefValIdx);
			}
			else {
				// それ以外の文は、そのまま出力する
				int j;

				s = "";
				for (j = TokPos; j < TokenEndPos; j++) {
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
					if (Token->TokId == tid_LineEnd) {
						break;
					}
				}
				if ((s != "") && (s[1] == ' ')) {
					s = OffsetStr + s;
				}
				s = SepLongStr(s);
				fprintf(fp, "%s", s.c_str());
				EndTokPos = j;
			}
		}
		TokPos = EndTokPos;
	}
	if (TokPos >= TokenEndPos) { // EndDOが見つからなかった。
		MainF->ErrMessage(-1, "文法解析エラー。ENDDOが見つからなかった。");
		return 99999;
	}
	//
	// EndDoの処理を行う
	//
	DoValIdx = bkDoValIdx;
	UnRollDoRefValBits = bkUnRollDoRefValBits;
	Token = (TToken*)TokenList->Items[TokPos];
	if (!UnRollDoF) {
		//
		// UnRoll対象でない場合は、ENDDOの行は、DO参照変数の置換のみを行う。
		//
		//
		TokPos = OutputUnroll_Line_Fortran(fp, TokPos, UsedDoRefValBits,
			RefValIdx);
	}
	else if (Token->TokId == tid_ENDDO) {
		//
		// EndDoの処理 (DO変数を含まない、これも含む場合あり)
		// EndDoの前に、変数の加算が入る。
		// EndDoの後に、Modulo()の処理が入る
		//
		string s2, s3;
		string BaseStr;
		int TokPos3;

		s3 = Token->OrgStr; // End DO Token
		BaseStr = ChangeFromOrgStr(DoToken[DoValIdx]->OrgStr, "");
		if (UnrollCount[DoValIdx] != 1) {
			s2 = BaseStr + "  ";
			s2 += DoValStr[DoValIdx] + " = " + DoValStr[DoValIdx] + "+";
			s2 += IntToStr(UnrollCount[DoValIdx]);

			if (DoStepSPos[DoValIdx] != DoStepEPos[DoValIdx]) {
				s2 += "*(";
				for (int k3 = DoStepSPos[DoValIdx]; k3 < DoStepEPos[DoValIdx];
					k3++) {
					Token = (TToken*)TokenList->Items[k3];
					s2 += Token->Str;
				}
				s2 += ")";
			}
			s2 = SepLongStr(s2);
			fprintf(fp, "%s\n", s2.c_str());
		}
		s3 = SepLongStr(s3);
		fprintf(fp, "%s", s3.c_str());
		fprintf(fp, "\n");
		//
		//	enddoの後のコメント部分を所得し、コメント後のToken位置を求める。
		//
		CommentStrAfterDo = "";
		for (TokPos3 = TokPos+1; TokPos3 < TokenEndPos; TokPos3++) {
			Token = (TToken*)TokenList->Items[TokPos3];
			s = Token->OrgStr;
			//
			// Doの前のコメント群をひとまとめにして、Do側で処理する。
			// EndDoの後のコメント処理はDoコード生成部分で行う。
			//
			if ((Token->TokId == tid_Comment)&&(Token->Script == NULL)) {
				for (; TokPos3 < TokenEndPos; TokPos3++) {
					Token = (TToken*)TokenList->Items[TokPos3];
					CommentStrAfterDo += Token->OrgStr;
					if (Token->TokId == tid_LineEnd){
						break;
					}
				}
				continue;
			}else if (Token->TokId == tid_LineEnd){
				CommentStrAfterDo += Token->OrgStr;
				continue;
			}else{
				break;
			}
		}
		CommentStrAfterDo = Trim(CommentStrAfterDo);
		EndTokPosAfterComment = TokPos3-1;
		if(CommentStrAfterDo != ""){
			fprintf(fp, "%s\n",CommentStrAfterDo.c_str());
		}
		//
		// 余り部分の処理を行う。
		// modulo, if を追加する。
		//
		BaseStr = ChangeFromOrgStr(DoToken[DoValIdx]->OrgStr, "");
		s = BaseStr;
		s += DoVal_l[DoValIdx] + " = modulo(";

		StartValueIs1F = false;
		if (DoStartSPos[DoValIdx] + 1 == DoStartEPos[DoValIdx]) {
			Token = (TToken*)TokenList->Items[DoStartSPos[DoValIdx]];
			if (Trim(Token->OrgStr) == "1") {
				StartValueIs1F = true;
			}
		}
		if ((DoStartSPos[DoValIdx] + 1 != DoStartEPos[DoValIdx]) ||
			(!StartValueIs1F)) {
			s += "(";
		}
		for (i = DoEndSPos[DoValIdx]; i < DoEndEPos[DoValIdx]; i++) {
			Token = (TToken*)TokenList->Items[i];
			// ここも、変数変更
			ValData = (TValData*)Token->ValData;
			AddValue = 0;
			if ((ValData != NULL) &&
				((ValData->RefDoValBits & UnRollDoRefValBits) != 0)) {
				AddValue = GetValAddValue(RefValIdx,
					ValData->RefDoValBits & UnRollDoRefValBits, DoValIdx,
					UnrollCount);
			}
			if (AddValue != 0) {
				s += "(" + Token->OrgStr + "+" + IntToStr(AddValue) + ")";
			}
			else {
				s += Token->OrgStr;
			}
			// s += Token->OrgStr;
		}
		if (!StartValueIs1F) {
			s += "-";
			for (i = DoStartSPos[DoValIdx]; i < DoStartEPos[DoValIdx]; i++) {
				Token = (TToken*)TokenList->Items[i];
				// ここも、変数変更
				ValData = (TValData*)Token->ValData;
				AddValue = 0;
				if ((ValData != NULL) &&
					((ValData->RefDoValBits & UnRollDoRefValBits) != 0)) {
					AddValue = GetValAddValue(RefValIdx,
						ValData->RefDoValBits & UnRollDoRefValBits, DoValIdx,
						UnrollCount);
				}
				if (AddValue != 0) {
					s += "(" + Token->OrgStr + "+" + IntToStr(AddValue) + ")";
				}
				else {
					s += Token->OrgStr;
				}
				// s += Token->OrgStr;
			}
			s += "+1";
		}
		if ((DoStartSPos[DoValIdx] + 1 != DoStartEPos[DoValIdx]) ||
			(!StartValueIs1F)) {
			s += ")";
		}

		s += ",";
		if (DoStepSPos[DoValIdx] == DoStepEPos[DoValIdx]) {
			s += IntToStr(UnrollCount[DoValIdx]);
		}
		else {
			s += IntToStr(UnrollCount[DoValIdx]) + "*(";
			for (i = DoStepSPos[DoValIdx]; i < DoStepEPos[DoValIdx]; i++) {
				Token = (TToken*)TokenList->Items[i];
				// ここも、変数変更
				ValData = (TValData*)Token->ValData;
				AddValue = 0;
				if ((ValData != NULL) &&
					((ValData->RefDoValBits & UnRollDoRefValBits) != 0)) {
					AddValue = GetValAddValue(RefValIdx,
						ValData->RefDoValBits & UnRollDoRefValBits, DoValIdx,
						UnrollCount);
				}
				if (AddValue != 0) {
					s += "(" + Token->OrgStr + "+" + IntToStr(AddValue) + ")";
				}
				else {
					s += Token->OrgStr;
				}
				// s += Token->OrgStr;
			}
			s += ")";
		}
		s += ")";
		s = SepLongStr(s);
		fprintf(fp, "%s\n", s.c_str());

		s = BaseStr;
		s += "if (" + DoVal_l[DoValIdx] + " .ne. 0) then";
		s = SepLongStr(s);
		fprintf(fp, "%s\n", s.c_str());

		//
		// 余りの処理のためのDo部分の出力
		// これに対しては、内部のUnrollは行わない。（プログラムが長くなるため）
		//
		int TokPos2; // 内部Do用のTokPos

		if(OptionInheritF){
			if(Trim(CommentStrBeforeDo) != ""){
				fprintf(fp, "%s\n", Trim(CommentStrBeforeDo).c_str());
			}
		}
		OffsetStr += "  ";
		for (TokPos2 = DoTokPos[DoValIdx]; TokPos2 <= TokPos; TokPos2++) {
			Token = (TToken*)TokenList->Items[TokPos2];
			s = Token->OrgStr;
			if (Token->Script != NULL) {
				// スクリプトは、ここでは、Ｓｋｉｐ
				continue;
			}
			else if (Token->TokId == tid_Comment) {
				fprintf(fp, "%s", Token->OrgStr.c_str()); // コメント
				continue;
			}
			else if (Token->TokId == tid_LineEnd) {
				fprintf(fp, "%s", Token->OrgStr.c_str()); // 改行も、そのまま
				continue;
			}
			else if (Token == DoToken[DoValIdx]) { // 該当Do文の出現
				//
				// 初期値を置換した　Doを生成
				//
				s = "  ";
				for (; TokPos2 < TokPos; TokPos2++) {
					Token = (TToken*)TokenList->Items[TokPos2];
					if (TokPos2 == DoStartEPos[DoValIdx] - 1) {
						s += Token->OrgStr;
						s += "+" + DoVal_m[DoValIdx] + "*" + IntToStr
							(UnrollCount[DoValIdx]); // Doの変数を置換
						if (DoStepSPos[DoValIdx] != DoStepEPos[DoValIdx]) {
							s += "*(";
							for (i = DoStepSPos[DoValIdx];
								i < DoStepEPos[DoValIdx]; i++) {
								Token = (TToken*)TokenList->Items[i];
								s += Token->OrgStr;
							}
							s += ")";
						}
					}
					else {
						s += Token->OrgStr;
					}
					if (Token->TokId == tid_LineEnd) {
						break;
					}
				}
				fprintf(fp, "%s", s.c_str());
				continue;
			}
			//
			// 行内で、UnRoll有効中のDo変数を参照しているかを調べる。
			// もし、使用中であれば、多重化の対象となる。
			//
			// Modulo内での多重化対象は、UnRollしたDO変数自体は対象外とする。
			// ようは、UnRoll対象での外側のUseBitのみが多重化対象となる。
			//
			DoRefValBits = 0;
			for (j = TokPos2; j < TokenEndPos; j++) {
				Token2 = (TToken*)TokenList->Items[j];
				if (Token2->TokId == tid_LineEnd) {
					break;
				}
				if (Token2->TokId == tid_DO) {
					// Doの後の変数をスキップ(除外)　DO 100などはサポート外
					j++;
				}
				if (Token2->ValData == NULL) {
					continue;
				}
				ValData = (TValData*)Token2->ValData;
				DoRefValBits |= ValData->RefDoValBits;
			}
			DoRefValBits &= UnRollDoRefValBits; // UnRoll対象(今のDoを除く)
			// 次に渡す Do変数参照(処理済)bitを決定（OR）
			NextUsedDoRefValBits = DoRefValBits | UsedDoRefValBits;
			NowUseDoRefValBits = DoRefValBits & (~UsedDoRefValBits);

			// fprintf(fp,"DoRefValBits %04X UnRollDoRefValBits %04X\n",
			// DoRefValBits , UnRollDoRefValBits);
			// fprintf(fp,"UsedDoRefValBits %04X NextUseDoRefValBits %04X NowUseDoRefValBits %04X\n",
			// UsedDoRefValBits, NextUsedDoRefValBits , NowUseDoRefValBits);

			RollCount = GetValCountOfBit(NowUseDoRefValBits, UnrollCount);
			for (int AndRefValIdx = 0; AndRefValIdx < RollCount;
				AndRefValIdx++) {
				//
				// NewRefValIdxを計算する。
				//
				if (RollCount <= 1) { // 分割が発生しない場合は、呼び出し元がそのまま
					NewRefValIdx = RefValIdx;
				}
				else {
					// 新たに分割が発生する場合には、計算が必要
					// RefValIdx : UseDoRefValBits
					// kk ; AndUseDoRefValBits の２つを合成した NewRefValIdxを計算。
					// [32] の要素で合成する。
					//
					NewRefValIdx = CalNewRefValIdx(RefValIdx, UsedDoRefValBits,
						AndRefValIdx, NowUseDoRefValBits, UnrollCount);
				}
				// fprintf(fp,"  NewRefValIdx %d RefValIdx %d\n",
				// NewRefValIdx,RefValIdx);
				if (Token->TokId == tid_DO) {
					// DOのブロックを出力(END_DO,Moduloの処理まで行う)
					EndTokPos = OutputUnroll_DoBlock_Fortran(fp, TokPos2,
						NextUsedDoRefValBits, NewRefValIdx);
				}
				else if (Token->TokId == tid_IF) {
					// IFのブロックを出力 if .. endifが対象
					EndTokPos = OutputUnroll_IfBlock_Fortran(fp, TokPos2,
						NextUsedDoRefValBits, NewRefValIdx);
				}
				else if (Token->ValData != NULL) {
					// 代入文１行を出力
					EndTokPos = OutputUnroll_Line_Fortran(fp, TokPos2,
						NextUsedDoRefValBits, NewRefValIdx);
				}
				else {
					// それ以外の文は、そのまま出力する
					int j;

					s = "";
					for (j = TokPos2; j < TokenEndPos; j++) {
						Token = (TToken*)TokenList->Items[j];
						s += Token->OrgStr;
						if (Token->TokId == tid_LineEnd) {
							break;
						}
					}
					if ((s != "") && (s[1] == ' ')) {
						s = OffsetStr + s;
					}
					s = SepLongStr(s);
					fprintf(fp, "%s", s.c_str());
					EndTokPos = j;
				}
			}
			TokPos2 = EndTokPos;
		}
		if(OptionInheritF){
			if(CommentStrAfterDo != ""){
				fprintf(fp, "%s\n",CommentStrAfterDo.c_str());
			}
		}
		s = BaseStr + "endif";
//		fprintf(fp, "%s", s.c_str());
		fprintf(fp, "%s\n", s.c_str());
		if (OffsetStr.length() >= 3) {
			OffsetStr = OffsetStr.substr(3, OffsetStr.length());
		}
	}
	if(EndTokPosAfterComment != -1){
		TokPos = EndTokPosAfterComment;
	}
	return TokPos;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// TokPos  トークン位置
// UsedDoRefValBits    使用中ＤＯ参照変数Ｂｉｔｓ
// RefValIdx   参照変数インデックス
//
// 3.概要
// 多重化したDO_Blockを出力する。
// RefValIdxは、Countに対しての番号となる。（呼び出し側から指定される）
// 最後のトークン（通常はtid_LineEnd)の位置を返す。
// 多重化に合わせた変数の置換も行われる。
//
// UseDoRefValBitsは、このブロックで分割対象となっているDoRefValのbitを示す。
// RefValIdxは、UseDoRefValBitsで1になっているBitのCount総和における番号を示
// す。
// 呼び出し時のUseDoRefValBitsから新たに1になるBitsだけが繰り返し対象となる。
// その場合のRefValIdxは、新たなUseDoRefValBitsでのIdxに再計算され先に渡され
// る。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::OutputUnroll_DoBlock_C(FILE *fp, int TokPos,
	unsigned int UsedDoRefValBits, int RefValIdx,string CommentStrBeforeDo) {
	int EndTokPos;
	TToken *Token, *Token2;
	TValData *ValData;
	string s, s2;
	int i, j;
	int RollCount;
	int bkDoValIdx = DoValIdx;
	bool UnRollDoF = false;
	bool StartValueIs0F;
	unsigned int DoRefValBits;
	unsigned int NextUsedDoRefValBits; // 次に渡す Do変数参照(処理済)bit
	unsigned int NowUseDoRefValBits; // 今回の分割数となる UseDoRefValBits
	int NewRefValIdx; // 次に渡すRefValIdx (of NextUseDoRefValBits)
	unsigned int bkUnRollDoRefValBits = UnRollDoRefValBits;
	int AddValue;
	int EndDoPos;
	string BaseValName;
	string DoEndValReplaseStr = ""; // DOの終了変数の置換の式
	string CommentStrAfterDo;
	int EndTokPosAfterComment = -1;
	bool OptionInheritF;

	OptionInheritF = (LowerCase(OptionStr).find("inherit") != string::npos); // Option
	//
	// DO文の入り口の処理を行う。
	//
	EndTokPos = TokenEndPos;
	EndDoPos = FindEndDo2(TokenList, TokPos);
	Token = (TToken*)TokenList->Items[TokPos];
	if ((Token != DoToken[DoValIdx]) || (UnrollCount[DoValIdx] <= 1)) {
		//
		// UnRoll対象でない場合は、DO文の行は、DO参照変数の置換のみを行って戻る。
		//
		if (Token == DoToken[DoValIdx]) {
			DoValIdx++;
		}
		if(Trim(CommentStrBeforeDo) != ""){
			fprintf(fp, "%s\n", Trim(CommentStrBeforeDo).c_str());
		}
		TokPos = OutputUnroll_Line_C(fp, TokPos, UsedDoRefValBits, RefValIdx);
		TokPos++; // LineEndをSkipする
	}
	else {
		//
		// UnRoll対象ＤＯ文の入り口の処理を行う。
		// DOの前処理の式を挿入する。（DO依存変数を含まない)
		// この場合合も、DO参照変数の置換は、対象とすること。
		//
		// Exp.       im =  N/2  // Unrollした変数
		// i = 1		 // 参照変数
		// do ii=1,im // doの実行回数も変更
		//
		// [       im = N/2]

		UnRollDoF = true;
		UnRollDoRefValBits |= (1 << DoValIdx); // 変数Bitsをセットする。

		s = ChangeFromOrgStr(DoToken[DoValIdx]->OrgStr, s); // Doの変数を置換
		// 開始が0かチェック
		StartValueIs0F = false;
		if (DoStartSPos[DoValIdx] + 1 == DoStartEPos[DoValIdx]) {
			Token = (TToken*)TokenList->Items[DoStartSPos[DoValIdx]];
			if (Trim(Token->Str) == "0") {
				StartValueIs0F = true;
			}
		}
		if ((DoStartSPos[DoValIdx] + 1 != DoStartEPos[DoValIdx]) ||
			(!StartValueIs0F)) {
			s += "(";
		}
		AddValue = 0;
		i = DoEndSPos[DoValIdx];
		Token = (TToken*)TokenList->Items[i++];
		// ここも、変数変更
		ValData = (TValData*)Token->ValData;
		if ((ValData != NULL) && ((ValData->RefDoValBits & UnRollDoRefValBits)
				!= 0)) {
			AddValue = GetValAddValue(RefValIdx,
				ValData->RefDoValBits & UnRollDoRefValBits, DoValIdx,
				UnrollCount);
		}
		Token = (TToken*)TokenList->Items[i++];
		if (Token->Str == "<=") {
			AddValue += 1; // 最後の数が＋１までならばLoop数も増える。
		}
		s2 = "";
		for (; i < DoEndEPos[DoValIdx]; i++) {
			Token = (TToken*)TokenList->Items[i];
			s2 += Token->Str;
		}
		if (AddValue != 0) {
			s += "(" + s2 + "+" + IntToStr(AddValue) + ")";
		}
		else if (DoEndSPos[DoValIdx] + 2 > DoEndEPos[DoValIdx]) {
			s += "(" + s2 + ")";
		}
		else {
			s += s2;
		}
		if (!StartValueIs0F) {
			s += "-";
			for (i = DoStartSPos[DoValIdx]; i < DoStartEPos[DoValIdx]; i++) {
				Token = (TToken*)TokenList->Items[i];
				// ここも、変数変更
				ValData = (TValData*)Token->ValData;
				AddValue = 0;
				if ((ValData != NULL) &&
					((ValData->RefDoValBits & UnRollDoRefValBits) != 0)) {
					AddValue = GetValAddValue(RefValIdx,
						ValData->RefDoValBits & UnRollDoRefValBits, DoValIdx,
						UnrollCount);
				}
				if (AddValue != 0) {
					s += "(" + Token->OrgStr + "+" + IntToStr(AddValue) + ")";
				}
				else {
					s += Token->OrgStr;
				}
			}
			s += "+1";
		}
		if ((DoStartSPos[DoValIdx] + 1 != DoStartEPos[DoValIdx]) ||
			(!StartValueIs0F)) {
			s += ")";
		}
		if (DoStepSPos[DoValIdx] == DoStepEPos[DoValIdx]) {
			s += "/" + IntToStr(UnrollCount[DoValIdx]);
		}
		else {
			/* 現状のＣでのStepは Val++ 以外はエラーとしている。Fortranからそのままのため。
			 */
		}
		DoEndValReplaseStr = Trim(s);
		// s = SepLongStr(s);
		// fprintf(fp,"%s;\n",s.c_str());
		//
		// Do文の出力を行う
		//
		if(Trim(CommentStrBeforeDo) != ""){
			fprintf(fp, "%s\n", Trim(CommentStrBeforeDo).c_str());
		}
		s = "";
		for (i = TokPos; i <= DoEndEPos[DoValIdx]; i++) {
			Token = (TToken*)TokenList->Items[i];
			// ベース変数名については、Unroll数の整数倍に丸めた変数に置き換える。2012/04/05
#if 1       // cut im = xxx 2012/09/27 , add im = xxx 2012/10/05
			if ((Token->Str == BaseValList->Strings[0]) &&
				(UnrollCount[DoValIdx] > 1)) {
				s += " " + DoVal_m[DoValIdx];
			}
			else {
				s += Token->OrgStr;
			}
#else
			s += Token->OrgStr;
#endif
		}
		if (UnrollCount[DoValIdx] == 1) {
			s += " " + DoValStr[DoValIdx] + "++";
		}
		else {
			s += " " + DoValStr[DoValIdx] + "+=" + IntToStr
				(UnrollCount[DoValIdx]);
		}
		fprintf(fp, "%s", s.c_str());
		// Do文の終わりまで TokPosをSkip
		// TokPos = FindEndDo2(TokenList,TokPos);
		int Nest = 0;

		for (; TokPos < TokenEndPos; TokPos++) {
			Token = (TToken*)TokenList->Items[TokPos];
			if (Token->TokId == tid_Kakko) {
				Nest++;
			}
			if (Token->TokId == tid_Kokka) {
				Nest--;
				if (Nest <= 0) {
					break;
				}
			}
		}
		DoValIdx++; // Next Nest Do （Doの最後で戻る）
	}
	//
	// EndDoまでスキャンする。
	//
	////	EndDoPos = FindEndDo2(TokenList,TokPos-1);
	for (; TokPos < TokenEndPos; TokPos++) {
		Token = (TToken*)TokenList->Items[TokPos];
		if (Token->Script != NULL) {
			// スクリプトは、Ｓｋｉｐ
			continue;
		}
		else if (Token->TokId == tid_Comment) {
			fprintf(fp, "%s", Token->OrgStr.c_str()); // コメントは、そのまま
			continue;
		}
		else if (Token->TokId == tid_LineEnd) {
			fprintf(fp, "%s", Token->OrgStr.c_str()); // 改行も、そのまま
			continue;
		}
		else if (TokPos >= EndDoPos) {
			// EndDoの処理へ
			break;
		}
		//
		// 行内で、UnRoll有効中のDo変数を参照しているかを調べる。
		// もし、使用中であれば、多重化の対象となる。
		// 多重化は、代入文とDo文とする。(2004/08/26)
		//
		DoRefValBits = 0;
		for (j = TokPos; j < TokenEndPos; j++) {
			Token2 = (TToken*)TokenList->Items[j];
			if (Token2->LineEndF) {
				break;
			}
			if (Token2->TokId == tid_for) {
				// Doの後の変数をスキップ(除外)　DO 100などはサポート外
				j++;
			}
			if (Token2->ValData == NULL) {
				continue;
			}
			ValData = (TValData*)Token2->ValData;
			DoRefValBits |= ValData->RefDoValBits;
		}
		/*
		if((DoRefValBits != 0)&&((DoRefValBits & UnRollDoRefValBits) == 0)){
		// Do束縛変数への参照がDOLoop外で行われている場合は、
		// 一時的（現在のUnrollが終わるまで）Do束縛変数でなくする。
		// 関数から抜ける時には、元に戻す。
		DP(ValData->Str);
		}
		 */
		DoRefValBits &= UnRollDoRefValBits;
		// 次に渡す Do変数参照(処理済)bitを決定（OR）
		NextUsedDoRefValBits = DoRefValBits | UsedDoRefValBits;
		NowUseDoRefValBits = DoRefValBits & (~UsedDoRefValBits);
		RollCount = GetValCountOfBit(NowUseDoRefValBits, UnrollCount);
		for (int AndRefValIdx = 0; AndRefValIdx < RollCount; AndRefValIdx++) {
			//
			// NewRefValIdxを計算する。
			//
			if (RollCount <= 1) { // 分割が発生しない場合は、呼び出し元がそのまま
				NewRefValIdx = RefValIdx;
			}
			else {
				// 新たに分割が発生する場合には、計算が必要
				// RefValIdx : UseDoRefValBits
				// kk ; AndUseDoRefValBits の２つを合成した NewRefValIdxを計算。
				// [32] の要素で合成する。
				//
				NewRefValIdx = CalNewRefValIdx(RefValIdx, UsedDoRefValBits,
					AndRefValIdx, NowUseDoRefValBits, UnrollCount);
			}
			if (Token->TokId == tid_for) {
				// DOのブロックを出力(END_DO,Moduloの処理まで行う)
				EndTokPos = OutputUnroll_DoBlock_C(fp, TokPos,
					NextUsedDoRefValBits, NewRefValIdx);
				if (AndRefValIdx < RollCount - 1) { // 継続がある場合は改行
					fprintf(fp, "\n");
				}
			}
			else if (Token->TokId == tid_if) {
				// IFのブロックを出力 if .. endifが対象
				EndTokPos = OutputUnroll_IfBlock_C(fp, TokPos,
					NextUsedDoRefValBits, NewRefValIdx);
			}
			else if (Token->ValData != NULL) {
				// 代入文１行を出力
				// EndTokPos = OutputUnroll_Line_C(fp,TokPos,NextUsedDoRefValBits,NewRefValIdx);
				EndTokPos = OutputUnroll_Line_C(fp, TokPos,
					NextUsedDoRefValBits, NewRefValIdx);
			}
			else {
				// それ以外の文は、そのまま出力する
				int j;

				s = "";
				for (j = TokPos; j < TokenEndPos; j++) {
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
					if ((Token->TokId == tid_DaiKakko) ||
						(Token->TokId == tid_DaiKokka) ||
						(Token->TokId == tid_Semikoron)) {
						break;
					}
				}
				s = OffsetStr + s;
				fprintf(fp, "%s", s.c_str());
				EndTokPos = j;
			}
		}
		TokPos = EndTokPos;
	}
	if (TokPos > TokenEndPos) { // EndDOが見つからなかった。
		MainF->ErrMessage(1, "文法解析エラー。ENDDOが見つからなかった。");
		return 99999;
	}
	//
	// EndDoの処理を行う
	//
	DoValIdx = bkDoValIdx;
	UnRollDoRefValBits = bkUnRollDoRefValBits;
	Token = (TToken*)TokenList->Items[TokPos];
	if (!UnRollDoF) {
		//
		// UnRoll対象でない場合は、ENDDOの行は、DO参照変数の置換のみを行う。
		//
		TokPos = OutputUnroll_Line_C(fp, TokPos, UsedDoRefValBits, RefValIdx);

		// }else if(Token->TokId == tid_ENDDO){
	}
	else if (TokPos >= EndDoPos) {
		//
		// EndDoの処理 (DO変数を含まない、これも含む場合あり)
		// EndDoの前に、変数の加算が入る。
		// EndDoの後に、Modulo()の処理が入る
		//
		string s2, s3;
		string BaseStr;
		int TokPos3;

		s3 = Token->OrgStr; // End DO Token
		fprintf(fp, "%s\n", s3.c_str()); // ここは、次のLineEndの変わりに改行をつける。

		//
		//	enddoの後の#pragma部分を所得し、#pragma後のToken位置を求める。
		//
		CommentStrAfterDo = "";
		for (TokPos3 = TokPos+1; TokPos3 < TokenEndPos; TokPos3++) {
			Token = (TToken*)TokenList->Items[TokPos3];
			s = Token->OrgStr;
			//
			// Doの前のコメント群をひとまとめにして、Do側で処理する。
			// EndDoの後のコメント処理はDoコード生成部分で行う。
			//
			if ((Token->TokId == tid_SharpPragma)&&(Token->Script == NULL)) {
				// #pragme （OATのスクリプト以外）
				for (; TokPos3 < TokenEndPos; TokPos3++) {
					Token = (TToken*)TokenList->Items[TokPos3];
					CommentStrAfterDo += Token->OrgStr;
					if (Token->TokId == tid_LineEnd){
						break;
					}
				}
				continue;
			}else if (Token->TokId == tid_LineEnd){
				CommentStrAfterDo += Token->OrgStr;
				continue;
			}else{
				break;
			}
		}
		CommentStrAfterDo = Trim(CommentStrAfterDo);
		EndTokPosAfterComment = TokPos3-1;
		if(CommentStrAfterDo != ""){
			fprintf(fp, "%s\n",CommentStrAfterDo.c_str());
		}
		//
		// 余り部分の処理を行う。
		//
		s = BaseStr;
		BaseValName = BaseValList->Strings[0];
		s += "if ((" + BaseValName + " % " + IntToStr(UnrollCount[DoValIdx])
			+ ") != 0){";
		fprintf(fp, "\t%s\n", s.c_str());
		//
		// 余りの処理のためのDo部分の出力
		// これに対しては、内部のUnrollは行わない。（プログラムが長くなるため）
		//
		int TokPos2; // 内部Do用のTokPos

		if(OptionInheritF){
			if(Trim(CommentStrBeforeDo) != ""){
				fprintf(fp, "%s\n", Trim(CommentStrBeforeDo).c_str());
			}
		}
		OffsetStr += "\t";
		for (TokPos2 = DoTokPos[DoValIdx]; TokPos2 <= TokPos; TokPos2++) {
			Token = (TToken*)TokenList->Items[TokPos2];
			s = Token->OrgStr;
			if (Token->Script != NULL) {
				// スクリプトは、ここでは、Ｓｋｉｐ
				continue;
			}
			else if (Token->TokId == tid_Comment) {
				fprintf(fp, "%s", Token->OrgStr.c_str()); // コメント
				continue;
			}
			else if (Token->TokId == tid_LineEnd) {
				fprintf(fp, "%s", Token->OrgStr.c_str()); // 改行も、そのまま
				continue;
			}
			else if (Token == DoToken[DoValIdx]) { // 該当Do文の出現
				//
				// 初期値を置換した　Doを生成
				//
				s = OffsetStr;
				for (; TokPos2 < TokPos; TokPos2++) {
					Token = (TToken*)TokenList->Items[TokPos2];
					if (TokPos2 == DoStartEPos[DoValIdx] - 1) {
						if (Token->Str == "0") {
							// s += " "+DoVal_m[DoValIdx] + "*" + IntToStr(UnrollCount[DoValIdx]);	// Doの変数を置換
							s += "(" + BaseValName + "/" + IntToStr
								(UnrollCount[DoValIdx]) + ")*" + IntToStr
								(UnrollCount[DoValIdx]); // Doの変数を置換
						}
						else {
							s += Token->Str;
							s += "+(" + BaseValName + "/" + IntToStr
								(UnrollCount[DoValIdx])
								+ ")*" + IntToStr(UnrollCount[DoValIdx]);
							// Doの変数を置換
						}
						/*
						if(DoStepSPos[DoValIdx] != DoStepEPos[DoValIdx]){
						s += "*(";
						for(i = DoStepSPos[DoValIdx] ; i < DoStepEPos[DoValIdx] ; i++){
						Token = (TToken *)TokenList->Items[i];
						s += Token->OrgStr;
						}
						s += ")";
						}
						 */
					}
					else {
						s += Token->OrgStr;
					}
					if (Token->LineEndF) {
						break;
					}
				}
				fprintf(fp, "%s", s.c_str());
				continue;
			}
			//
			// 行内で、UnRoll有効中のDo変数を参照しているかを調べる。
			// もし、使用中であれば、多重化の対象となる。
			//
			// Modulo内での多重化対象は、UnRollしたDO変数自体は対象外とする。
			// ようは、UnRoll対象での外側のUseBitのみが多重化対象となる。
			//
			DoRefValBits = 0;
			for (j = TokPos2; j < TokenEndPos; j++) {
				Token2 = (TToken*)TokenList->Items[j];
				if (Token2->LineEndF) {
					break;
				}
				if (Token2->TokId == tid_for) {
					// Doの後の変数をスキップ(除外)　DO 100などはサポート外
					j++;
				}
				if (Token2->ValData == NULL) {
					continue;
				}
				ValData = (TValData*)Token2->ValData;
				DoRefValBits |= ValData->RefDoValBits;
			}
			DoRefValBits &= UnRollDoRefValBits; // UnRoll対象(今のDoを除く)
			// 次に渡す Do変数参照(処理済)bitを決定（OR）
			NextUsedDoRefValBits = DoRefValBits | UsedDoRefValBits;
			NowUseDoRefValBits = DoRefValBits & (~UsedDoRefValBits);

			// fprintf(fp,"DoRefValBits %04X UnRollDoRefValBits %04X\n",
			// DoRefValBits , UnRollDoRefValBits);
			// fprintf(fp,"UsedDoRefValBits %04X NextUseDoRefValBits %04X NowUseDoRefValBits %04X\n",
			// UsedDoRefValBits, NextUsedDoRefValBits , NowUseDoRefValBits);

			RollCount = GetValCountOfBit(NowUseDoRefValBits, UnrollCount);
			for (int AndRefValIdx = 0; AndRefValIdx < RollCount;
				AndRefValIdx++) {
				//
				// NewRefValIdxを計算する。
				//
				if (RollCount <= 1) { // 分割が発生しない場合は、呼び出し元がそのまま
					NewRefValIdx = RefValIdx;
				}
				else {
					// 新たに分割が発生する場合には、計算が必要
					// RefValIdx : UseDoRefValBits
					// kk ; AndUseDoRefValBits の２つを合成した NewRefValIdxを計算。
					// [32] の要素で合成する。
					//
					NewRefValIdx = CalNewRefValIdx(RefValIdx, UsedDoRefValBits,
						AndRefValIdx, NowUseDoRefValBits, UnrollCount);
				}
				// fprintf(fp,"  NewRefValIdx %d RefValIdx %d\n",
				// NewRefValIdx,RefValIdx);
				if (Token->TokId == tid_for) {
					// DOのブロックを出力(END_DO,Moduloの処理まで行う)
					EndTokPos = OutputUnroll_DoBlock_C(fp, TokPos2,
						NextUsedDoRefValBits, NewRefValIdx);
				}
				else if (Token->TokId == tid_if) {
					// IFのブロックを出力 if .. endifが対象
					EndTokPos = OutputUnroll_IfBlock_C(fp, TokPos2,
						NextUsedDoRefValBits, NewRefValIdx);
				}
			  	else if (Token->ValData != NULL) {
					// 代入文１行を出力
					EndTokPos = OutputUnroll_Line_C(fp, TokPos2,
						NextUsedDoRefValBits, NewRefValIdx);
				}
				else {
					// それ以外の文は、そのまま出力する
					int j;

					s = "";
					for (j = TokPos2; j < TokenEndPos; j++) {
						Token = (TToken*)TokenList->Items[j];
						s += Token->OrgStr;
						if (Token->LineEndF) {
							break;
						}
					}
					// if((s != "")&&(s[1] == ' ')){
					s = OffsetStr + s;
					// }
					fprintf(fp, "%s", s.c_str());
					EndTokPos = j;
				}
			}
			TokPos2 = EndTokPos;
		}
		if(OptionInheritF){
			if(CommentStrAfterDo != ""){
				fprintf(fp, "%s\n",CommentStrAfterDo.c_str());
			}
		}
		s = BaseStr + "}";
		fprintf(fp, "\t%s\n", s.c_str());
		// fprintf(fp,"%s",s.c_str());
		OffsetStr = OffsetStr.erase(0, 1);
	}
	if(EndTokPosAfterComment != -1){
		TokPos = EndTokPosAfterComment;
	}
	return TokPos;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// TokPos  トークン位置
// UsedDoRefValBits    使用中ＤＯ参照変数Ｂｉｔｓ
// RefValIdx   参照変数インデックス
//
// 3.概要
// 多重化したIf_Blockを出力する。
// 最後のトークン（通常はtid_LineEnd)の位置を返す。
// 多重化に合わせた変数の置換も行われる。
//
// １）ifの構造 EndPos,CondSPpos,CondEPos,BlockSPos,BlockEPos,ElseSPos,ElseEP
// osを求める
// ２）条件式がDO依存ならば、ブロック多重化、そうでなければ行単位での多重化を
// 行う
// ３）終了トークン位置を返す。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::OutputUnroll_IfBlock_Fortran(FILE *fp, int TokPos,
	unsigned int UsedDoRefValBits, int RefValIdx) {
	int SPos, EPos;
	int IfNest = 0;
	TToken *Token;
	int AndRefValIdx;
	int cc;
	unsigned int DoRefValBits;
	unsigned int NextUsedDoRefValBits; // 次に渡す Do変数参照(処理済)bit
	unsigned int NowUseDoRefValBits; // 今回の分割数となる UseDoRefValBits
	TValData *ValData;
	int NewRefValIdx, EndTokPos;
	string s;

	//
	// IF構造の終了位置を検索する。
	// 対象とするＩＦは、現在のTopLevelでありNestではない
	//
	EPos = -1;
	SPos = TokPos;
	EndTokPos = TokenEndPos;
	for (; TokPos < TokenEndPos; TokPos++) {
		Token = (TToken*)TokenList->Items[TokPos];
		if (Token->TokId == tid_IF) {
			IfNest++;
		}
		else if (Token->TokId == tid_ENDIF) {
			IfNest--;
			if (IfNest == 0) {
				EPos = TokPos;
				break;
			}
		}
	}
	//
	// IFを出力する。ネストしたＩＦやＤＯに対しての再帰呼び出しも行う。
	// ブロック多重化数だけの出力を行う
	//
	for (TokPos = SPos; TokPos <= EPos; TokPos++) {
		//
		// 多重化を式にUnRoll変数が含まれているかでチェックする。
		// IF文も対象
		//
		DoRefValBits = 0;
		for (int kk = TokPos; kk < EPos; kk++) {
			Token = (TToken*)TokenList->Items[kk];
			if (Token->TokId == tid_LineEnd) {
				break;
			}
			if (Token->ValData == NULL) {
				continue;
			}
			//
			// 行内で、UnRoll有効中のDo変数を参照しているかを調べる。
			// もし、使用中であれば、多重化の対象となる。
			// 多重化は、代入文とDo文とする。(2004/08/26)
			//
			ValData = (TValData*)Token->ValData;
			DoRefValBits |= ValData->RefDoValBits;
		}
		DoRefValBits &= UnRollDoRefValBits;
		// 次に渡す Do変数参照(処理済)bitを決定（OR）
		NextUsedDoRefValBits = DoRefValBits | UsedDoRefValBits;
		NowUseDoRefValBits = DoRefValBits & (~UsedDoRefValBits);
		cc = GetValCountOfBit(NowUseDoRefValBits, UnrollCount); // ブロック多重化数が確定
		for (AndRefValIdx = 0; AndRefValIdx < cc; AndRefValIdx++) {
			//
			// NewRefValIdxを計算する。
			//
			Token = (TToken*)TokenList->Items[TokPos];
			if (cc <= 1) { // 分割が発生しない場合は、呼び出し元がそのまま
				NewRefValIdx = RefValIdx;
			}
			else {
				// 新たに分割が発生する場合には、計算が必要
				// RefValIdx : UseDoRefValBits
				// kk ; AndUseDoRefValBits の２つを合成した NewRefValIdxを計算。
				// [32] の要素で合成する。
				//
				NewRefValIdx = CalNewRefValIdx(RefValIdx, UsedDoRefValBits,
					AndRefValIdx, NowUseDoRefValBits, UnrollCount);
			}
			if (Token->TokId == tid_DO) {
				// DOのブロックを出力(END_DO,Moduloの処理まで行う)
				EndTokPos = OutputUnroll_DoBlock_Fortran(fp, TokPos,
					NextUsedDoRefValBits, NewRefValIdx);
				if (AndRefValIdx < cc - 1) { // 継続がある場合は改行
					fprintf(fp, "\n");
				}
			}
			else if (Token->TokId == tid_IF) {
				// IFのブロックを出力 if .. endifが対象
				if (TokPos == SPos) {
					// 対象自体のＩＦ
					EndTokPos = OutputUnroll_Line_Fortran(fp, TokPos,
						NextUsedDoRefValBits, NewRefValIdx);
				}
				else {
					// これは、ネストしたＩＦが対象となる
					EndTokPos = OutputUnroll_IfBlock_Fortran(fp, TokPos,
						NextUsedDoRefValBits, NewRefValIdx);
					if (AndRefValIdx < cc - 1) { // 継続がある場合は改行
						fprintf(fp, "\n");
					}
				}
			}
			else if (Token->ValData != NULL) {
				// 代入文１行を出力
				EndTokPos = OutputUnroll_Line_Fortran(fp, TokPos,
					NextUsedDoRefValBits, NewRefValIdx);
			}
			else {
				// それ以外の文は、そのまま出力する
				int j;

				s = "";
				for (j = TokPos; j < TokenEndPos; j++) {
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
					if (Token->TokId == tid_LineEnd) {
						break;
					}
				}
				if ((s != "") && (s[1] == ' ')) {
					s = OffsetStr + s;
				}
				s = SepLongStr(s);
				fprintf(fp, "%s", s.c_str());
				EndTokPos = j;
			}
		}
		TokPos = EndTokPos;
	}
	return EPos;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// TokPos  トークン位置
// UsedDoRefValBits    使用中ＤＯ参照変数Ｂｉｔｓ
// RefValIdx   参照変数インデックス
//
// 3.概要
// 多重化したIf_Blockを出力する。
// 最後のトークン（通常はtid_LineEnd)の位置を返す。
// 多重化に合わせた変数の置換も行われる。
//
// １）ifの構造 EndPos,CondSPpos,CondEPos,BlockSPos,BlockEPos,ElseSPos,ElseEP
// osを求める
// ２）条件式がDO依存ならば、ブロック多重化、そうでなければ行単位での多重化を
// 行う
// ３）終了トークン位置を返す。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::OutputUnroll_IfBlock_C(FILE *fp, int TokPos,
	unsigned int UsedDoRefValBits, int RefValIdx) {
	int SPos, EPos;
	TToken *Token;
	int AndRefValIdx;
	int cc;
	unsigned int DoRefValBits;
	unsigned int NextUsedDoRefValBits; // 次に渡す Do変数参照(処理済)bit
	unsigned int NowUseDoRefValBits; // 今回の分割数となる UseDoRefValBits
	TValData *ValData;
	int NewRefValIdx, EndTokPos;
	string s;

	//
	// IF構造の終了位置を検索する。{から}を探す。
	//
	EPos = -1;
	SPos = TokPos;
	EndTokPos = TokenEndPos;
	for (; TokPos < TokenEndPos; TokPos++) {
		Token = (TToken*)TokenList->Items[TokPos];
		if (Token->TokId == tid_DaiKakko) {
			EPos = FindEndIf(TokenList, TokPos);
			break;
		}
	}
	//
	// IFを出力する。ネストしたＩＦやＤＯに対しての再帰呼び出しも行う。
	// ブロック多重化数だけの出力を行う
	//
	for (TokPos = SPos; TokPos <= EPos; TokPos++) {
		//
		// 多重化を式にUnRoll変数が含まれているかでチェックする。
		// IF文も対象
		//
		DoRefValBits = 0;
		for (int kk = TokPos; kk < EPos; kk++) {
			Token = (TToken*)TokenList->Items[kk];
			if (Token->LineEndF) {
				break;
			}
			if (Token->ValData == NULL) {
				continue;
			}
			//
			// 行内で、UnRoll有効中のDo変数を参照しているかを調べる。
			// もし、使用中であれば、多重化の対象となる。
			// 多重化は、代入文とDo文とする。(2004/08/26)
			//
			ValData = (TValData*)Token->ValData;
			DoRefValBits |= ValData->RefDoValBits;
		}
		DoRefValBits &= UnRollDoRefValBits;
		// 次に渡す Do変数参照(処理済)bitを決定（OR）
		NextUsedDoRefValBits = DoRefValBits | UsedDoRefValBits;
		NowUseDoRefValBits = DoRefValBits & (~UsedDoRefValBits);
		cc = GetValCountOfBit(NowUseDoRefValBits, UnrollCount); // ブロック多重化数が確定
		for (AndRefValIdx = 0; AndRefValIdx < cc; AndRefValIdx++) {
			//
			// NewRefValIdxを計算する。
			//
			Token = (TToken*)TokenList->Items[TokPos];
			if (cc <= 1) { // 分割が発生しない場合は、呼び出し元がそのまま
				NewRefValIdx = RefValIdx;
			}
			else {
				// 新たに分割が発生する場合には、計算が必要
				// RefValIdx : UseDoRefValBits
				// kk ; AndUseDoRefValBits の２つを合成した NewRefValIdxを計算。
				// [32] の要素で合成する。
				//
				NewRefValIdx = CalNewRefValIdx(RefValIdx, UsedDoRefValBits,
					AndRefValIdx, NowUseDoRefValBits, UnrollCount);
			}
			if (Token->TokId == tid_for) {
				// DOのブロックを出力(END_DO,Moduloの処理まで行う)
				EndTokPos = OutputUnroll_DoBlock_C(fp, TokPos,
					NextUsedDoRefValBits, NewRefValIdx);
				if (AndRefValIdx < cc - 1) { // 継続がある場合は改行
					fprintf(fp, "\n");
				}
			}
			else if (Token->TokId == tid_if) {
				// IFのブロックを出力 if .. endifが対象
				if (TokPos == SPos) {
					// 対象自体のＩＦ
					EndTokPos = OutputUnroll_Line_C(fp, TokPos,
						NextUsedDoRefValBits, NewRefValIdx);
				}
				else {
					// これは、ネストしたＩＦが対象となる
					EndTokPos = OutputUnroll_IfBlock_C(fp, TokPos,
						NextUsedDoRefValBits, NewRefValIdx);
				}
			}
			else if (Token->ValData != NULL) {
				// 代入文１行を出力
				EndTokPos = OutputUnroll_Line_C(fp, TokPos,
					NextUsedDoRefValBits, NewRefValIdx);
			}
			else {
				// それ以外の文は、そのまま出力する
				int j;

				s = "";
				for (j = TokPos; j < TokenEndPos; j++) {
					Token = (TToken*)TokenList->Items[j];
					s += Token->OrgStr;
					if (Token->LineEndF) {
						break;
					}
				}
				s = OffsetStr + s;
				fprintf(fp, "%s", s.c_str());
				EndTokPos = j;
			}
		}
		TokPos = EndTokPos;
	}
	return EPos;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// BaseName    基本名
// Idx インデックス
// ArgValList  引数変数リスト
//
// 3.概要
// Idx番目の新しい名前を作成する。語尾の数字を取った名前＋Idxの名前とするが、
// ArgValList->Objects[] の ValDataに既に数字を取って同じ名前がある場合には、
// 元の変数名＋"_"＋数値とする。さらに一致する場合は、"_"を増やして対応する。
//
// 4.機能説明
//
// 5.戻り値
// 新しい変数名
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
string TTuneRegion::GetNewValName(string BaseName, int Idx,
	TStringList *ArgValList) {
	int i, j, k;
	string s, s2, BaseName2, BaseName3;
	TValData *ValData;

	for (i = BaseName.length() - 1; i >= 0; i--) {
		if ((BaseName[i] >= '0') && (BaseName[i] <= '9')) {
			continue;
		}
		BaseName2 = BaseName.substr(0, i + 1);
		break;
	}
	s = BaseName2 + IntToStr(Idx);
	for (j = 0; j < 100; j++) {
		for (i = 0; i < ArgValList->Count; i++) {
			ValData = (TValData*)ArgValList->Objects[i];
			BaseName3 = ValData->Str;
			for (k = BaseName3.length() - 1; k >= 0; k--) {
				if ((BaseName3[k] >= '0') && (BaseName3[k] <= '9')) {
					continue;
				}
				BaseName3 = BaseName3.substr(0, k + 1);
				break;
			}
			if (BaseName3 == BaseName2) {
				if (BaseName != ValData->Str) { // 完全一致は別。
					i = -1;
					break;
				}
			}
		}
		if (i != -1) {
			return s;
		}
	}
	//
	// BaseNameが同じ変数がある場合は元の変数名_Idxとする・
	// 以前は元の変数名の番号を変えた変数名_ としていたが
	// 元の変数名の番号を変えた変数名も多重化する場合に問題があったので修正した。
	//
	s2 = "_";
	for (j = 0; j < 100; j++) {
		s = BaseName + s2 + IntToStr(Idx);
		for (i = 0; i < ArgValList->Count; i++) {
			ValData = (TValData*)ArgValList->Objects[i];
			if (ValData->Str == s) {
				i = -1;
				break;
			}
		}
		if (i != -1) {
			return s;
		}
		else {
			s2 += "_";
		}
	}
	return s;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// ValData  変数データ
// UnRollDoRefValBits 現在までにUnroll中のValBit
// Idx インデックス
// ArgValList  引数変数リスト
//
// 3.概要
// Idx番目の新しい名前を作成する。
// ArgValList->Objects[] の ValDataに既にある名前と一致する場合は、
// "_"を数値の後に必要なだけ挿入する
//
// 4.機能説明
//
// 5.戻り値
// 新しい変数名
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
string TTuneRegion::GetNewValNameForValData(TValData *ValData,
	unsigned int UnRollDoRefValBits, int Idx, TStringList *ArgValList) {
	int i, j;
	string s;
	TValData *ValData2;
	string BaseName;
	int Idx3;

	//
	// IdxをDo束縛ビット(ValData->DoRefValBits)で調整する。
	// UnRollDoRefValBitsの１部分に対してIdxが変化しているので、
	// もし、ValData->DoRefValBits[]が０部分があれば、その部分のUnRollCount[]で割る。
	//
	Idx3 = Idx;
	Idx--;
	for (i = 0; i < 32; i++) {
		if ((UnRollDoRefValBits & (2 << i)) != 0) {
			if ((ValData->RefDoValBits & (2 << i)) == 0) {
				Idx /= UnrollCount[i];
			}
		}
	}
	Idx++;

	BaseName = ValData->Str;
	for (i = BaseName.length() - 1; i >= 0; i--) {
		if ((BaseName[i] >= '0') && (BaseName[i] <= '9')) {
			continue;
		}
		BaseName = BaseName.substr(1, i);
		break;
	}
	s = BaseName + IntToStr(Idx);
	for (j = 0; j < 100; j++) {
		for (i = 0; i < ArgValList->Count; i++) {
			ValData2 = (TValData*)ArgValList->Objects[i];
			if (ValData2->Str == s) {
				i = -1;
				break;
			}
		}
		if (i != -1) {
			break;
		}
		else {
			s += "_";
		}
	}
	s += "(" + IntToStr((int)Idx3) + ",";
	s += "" + IntToStr((int)UnRollDoRefValBits) + ")";
	return s;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// BaseName    元の名前
// AddStr  追加文字列
// ArgValList  引数変数リスト
//
// 3.概要
// AddStrを追加した新しい名前を作成する。
// ArgValList->Objects[] の ValDataに既にある名前と一致する場合は、
// "_"を数値の後に必要なだけ挿入する
//
// 4.機能説明
//
// 5.戻り値
// 新しい変数名
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
string TTuneRegion::GetNewValName(string BaseName, string AddStr,
	TStringList *ArgValList) {
	int i, j;
	string s;
	TValData *ValData;

	s = BaseName + AddStr;
	for (j = 0; j < 100; j++) {
		for (i = 0; i < ArgValList->Count; i++) {
			ValData = (TValData*)ArgValList->Objects[i];
			if (ValData->Str == s) {
				i = -1;
				break;
			}
		}
		if (i != -1) {
			break;
		}
		else {
			s += "_";
		}
	}
	return s;

}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// Bits    チェックするＢｉｔｓ
//
// 3.概要
// 最初に１になっているBitの位置を返す。
//
// 4.機能説明
//
// 5.戻り値
// Bit位置
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::GetBitIdx(unsigned int Bits) {
	int i;
	for (i = 0; i < 32; i++) {
		if ((Bits & (1 << i)) != 0) {
			return i;
		}
	}
	return 0;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// Bits    有効・無効を示すＢｉｔ列
// Count   各変数のカウント配列
//
// 3.概要
// Bitsが１部分の配列のCountを掛けた値を返す。
// 変数がUnroll中に、何個必要かを計算する。
//
// 4.機能説明
//
// 5.戻り値
// Bitsが１部分の配列のCountを掛けた値を返す。
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::GetValCountOfBit(unsigned int Bits, int *Count) {
	int i, Total;

	Total = 1;
	for (i = 0; i < 32; i++) {
		if ((Bits & (1 << i)) != 0) {
			Total *= Count[i];
		}
	}
	return Total;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// RefValIdx　現在の変数Idx
// UseDoRefValBits ＤＯのUnRoll対象となっているかのBits
// AndRefValIdx    今回変化させるＩｄｘ
// AndUseDoRefValBits  今回変化させ変数の対象Bits
// Count   各変数のカウント数の配列
//
// 3.概要
// 新たに分割が発生する場合のNewRefValIdxを求める。
//
// 4.機能説明
//
// 5.戻り値
// 新たに求めた RefValIdx
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::CalNewRefValIdx(int RefValIdx, unsigned int UseDoRefValBits,
	int AndRefValIdx, unsigned int AndUseDoRefValBits, int *Count) {
	int i, Total, NewRefValIdx;
	int cc[32];
	unsigned int Bits;

	// RefValIdxでの各変数Idxの計算
	Bits = UseDoRefValBits;
	for (i = 31; i >= 0; i--) {
		if ((Bits & (1 << i)) != 0) {
			cc[i] = RefValIdx % Count[i];
			RefValIdx /= Count[i];
		}
	}
	// AndRefValIdxでの各変数Idxの計算
	Bits = AndUseDoRefValBits;
	for (i = 31; i >= 0; i--) {
		if ((Bits & (1 << i)) != 0) {
			cc[i] = AndRefValIdx % Count[i];
			AndRefValIdx /= Count[i];
		}
	}
	// NewRefValIdxの生成
	Bits = UseDoRefValBits | AndUseDoRefValBits;
	Total = 1;
	NewRefValIdx = 0;
	for (i = 31; i >= 0; i--) {
		if ((Bits & (1 << i)) != 0) {
			NewRefValIdx += cc[i] * Total;
			Total *= Count[i];
		}
	}
	return NewRefValIdx;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// OrgStr
// Str
//
// 3.概要
// OrgStrの改行やタブ、スペースなどは、そのままの形として、Strを追加した文字
// 列を返す。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
string TTuneRegion::ChangeFromOrgStr(string OrgStr, string Str) {
	int i;
	string s;

	s = "";
	for (i = 0; i < (int)OrgStr.length(); i++) {
		if (OrgStr[i] <= ' ') {
			s += OrgStr[i];
		}
		else if (Str != "") {
			s += Str;
			Str = "";
		}
	}
	if (Str != "") {
		s += Str;
	}
	return s;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// RefValIdx   参照変数Ｉｄｘ
// UseValBits  使用中Ｂｉｔｓ
// DoValIdx    Ｄｏ変数Ｉｄｘ
// Count   各変数のカウント配列
//
// 3.概要
// Do変数に Unroll中の行での加算する値を計算して返す。
//
// 外側のLoop で、UnRoll = 2
// 中で　UnRoll = 2 の場合には、中側が先に変化する形の計算となる。
// kk = 0 : A(i+0,j+0);
// kk = 1 : A(i+0,j+1);
// kk = 2 : A(i+1,j+0);
// kk = 3 : A(i+1,j+1);
//
// 4.機能説明
//
// 5.戻り値
// 加算する値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::GetDoValAddValue(int RefValIdx, unsigned int UseValBits,
	int DoValIdx, int *Count) {
	int AddValue = 0;
	int i;
	int Idx[32];

	// 各桁[Do変数ごと]のIdxを求める。
	for (i = DoValIdx - 1; i >= 0; i--) {
		Idx[i] = RefValIdx % Count[i];
		RefValIdx /= Count[i];
	}
	// UseValBitsの立っているIdxを加算して計算する。
	for (i = 0; i < DoValIdx; i++) {
		if ((UseValBits & (1 << i)) != 0) {
			AddValue *= Count[i];
			AddValue += Idx[i];
		}
	}
	return AddValue;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// RefValIdx   参照変数インデックス
// UseDoValBits    使用ＤＯ変数Ｂｉｔｓ
// DoValIdx    ＤＯ変数インデックス
// Count   各変数のカウント配列
//
// 3.概要
// 通常変数に、Unroll中の行で加算する値（Ｉｄｘ）を計算して返す。
// どのDo変数に依存しているかによって決定される。
//
// ・有効なDoが何かは、DoValIdxとUseDoValBitsで決定されている。
// UseDoValBitsは、kkの繰り返し回数（複数行数）の決定に使用される
// ・変数がどのDo変数に対して束縛（複数化）されているかは、UseValBits。
// ・もし、束縛を受けていない変数のDoが有効中の場合は、
// その部分の増加はSkip(繰り返し参照)対象となる
//
// 複数行数の計算に使用されながら、束縛には、有効でない変数Ｂｉｔ部分の変化が
// マスクされる形になるが、
// Bitが詰まったデータなので、計算する必要あり。
// 各bitでのどのIdxを示すかを求めてから、該当UseValBitsでのIdxを求める。
//
// 4.機能説明
//
// 5.戻り値
// 変数名に追加される番号
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::GetValAddValue(int RefValIdx, unsigned int UseDoValBits,
	int DoValIdx, int *Count) {
	int AddValue = 0;
	int i, Total;
	int IdxTbl[32];

	//
	// kk が　各Ｂｉｔに対して、どのＩｄｘを持つかを計算する。
	//
	Total = RefValIdx;
	for (i = DoValIdx; i >= 0; i--) {
		if ((UseDoValBits & (1 << i)) != 0) {
			IdxTbl[i] = Total % Count[i];
			Total /= Count[i];
		}
	}
	//
	// 有効BitのIdxだけを集めた Idx(from kk) を再構築
	//
	for (i = 0; i < DoValIdx; i++) {
		if ((UseDoValBits & (1 << i)) != 0) {
			AddValue *= Count[i];
			AddValue += IdxTbl[i];
		}
	}
	return AddValue;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// RefValIdx   参照変数インデックス
// RefDoValBits    変数が参照しているＤＯ変数Ｂｉｔｓ
// UnRollDoRefValBits   UnRollに使用しているＤＯ変数Ｂｉｔｓ
// DoValIdx    ＤＯ変数インデックス
// Count   各変数のカウント配列
//
// 3.概要
// 通常変数に、Unroll中の行で加算する値（Ｉｄｘ）を計算して返す。
// どのDo変数に依存しているかによって決定される。
// UnRoll中の多重化のBitによってのRefValIdxが変数のRefDoValBitsに
// 合わせて調整される。
//
// 4.機能説明
//
// 5.戻り値
// 変数名に追加される番号
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::GetValAddValue2(int RefValIdx, unsigned int RefDoValBits,
	unsigned int UnRollDoRefValBits, int DoValIdx, int *Count) {
	int AddValue = 0;
	int i, Total;
	int IdxTbl[32];

	//
	// kk が　各Ｂｉｔに対して、どのＩｄｘを持つかを計算する。
	//
	Total = RefValIdx;
	for (i = DoValIdx; i >= 0; i--) {
		if ((UnRollDoRefValBits & (1 << i)) != 0) {
			IdxTbl[i] = Total % Count[i];
			Total /= Count[i];
		}
		else {
			IdxTbl[i] = 0;
		}
	}
	//
	// 有効BitのIdxだけを集めた Idx(from kk) を再構築
	//
	for (i = 0; i < DoValIdx; i++) {
		if ((RefDoValBits & (1 << i)) != 0) {
			AddValue *= Count[i];
			AddValue += IdxTbl[i];
		}
	}
	return AddValue;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// IndentStr   インデント文字列
//
// 3.概要
// SetParmコードを出力
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputSetParmSrcCode(FILE *fp, string IndentStr,int Use_OAT_functuonNameF) {
	string s, ValName;
	string BaseValName;
	string ArgStr;
	string s1;
	TTuneRegion *TuneRegion;

	//
	// Dynamic以外はSetParamの引数は最小にする。2016/03/12
	// call OAT_xxx_OAT()での呼び出し時に、その先からSetParama_OATを呼ぶ必要が出てくるため。
	//
	bool DynamicRegionF = false;
	for (int i = 0; i < ((TPass5 *)MainF->Pass5)->TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)(((TPass5 *)MainF->Pass5)->TuneRegionList->Items[i]);
		if (TuneRegion->TuneGroup == tgDynamic) {
			DynamicRegionF = true;
			break;
		}
	}
	if(DynamicRegionF){
		if (MainF->Call_ATExec_Script == NULL) {
			TScript *Script = new TScript(MainF->TokenList, 0, NULL, ValDataList);
			ArgStr = Script->GetATExecArgStr(false, "", "");
			delete Script;
		}
		else {
			ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "", "");
		}
	}else{
		ArgStr = "";
    }
	BaseValName = "0";
	if (BaseValList->Count > 0) {
		ValName = BaseValList->Strings[0];
		if (Trim(ValName) != "") {
			BaseValName = ValName;
		}
	}
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		s = "      ctmp = \"" + Name + "\"\n";
		s1 = SepLongStr(s);
		if(Use_OAT_functuonNameF == 0){
			s = "      call OAT_SetParm(" + IntToStr(TuneGroup + 1);
		}else{
			s = "      call OAT_SetParm_OAT(" + IntToStr(TuneGroup + 1);
		}
		s += ",ctmp";
		s += "," + BaseValName;
		s += ",iusw1_" + Name + ArgStr + ")\n"; // 複数変数対応の unroll
		s = s1 + SepLongStr(s);
	}
	else if (MainF->SrcCodeType == MainF->sctC) {
		s = IndentStr + "\tOAT_SetParm(" + IntToStr(TuneGroup + 1);
		s += ",\"" + Name + "\"";
		s += "," + BaseValName;	// Add 2016/03/14
		s += ",&OAT_iusw1_" + Name + ArgStr + ");\n"; // 複数変数対応の unrollとなる。
		s = SepLongStr(s);
	}
	else {
		if(Use_OAT_functuonNameF == 0){
			s = "      call OAT_SetParm(" + IntToStr(TuneGroup + 1);
		}else{
			s = "      call OAT_SetParm_OAT(" + IntToStr(TuneGroup + 1);
		}
		s += ",\"" + Name + "\"";
		s += "," + BaseValName;
		// s += ",iusw1_"+Name+");\n";	// 複数変数対応の unrollとなる。
		s += ",iusw1_" + Name + ArgStr + ")\n"; // 複数変数対応の unroll 2015/02/28
		s = SepLongStr(s);
	}
#if 1
	// Pre BPset , Post BPset の行を前後に挿入する。TuneRegion範囲が対象
	s = GetPrePostBPSetSubregionStr(0) + s;
	s = s + GetPrePostBPSetSubregionStr(1);
#endif
	// d-spline用追加部分
	int CaseArrayCount;
	CaseArrayCount = CaseCount;

	/** ******************************************************************************************************************************************************** */
	//
	// Kogakuin Irie
	// Fortran90にd-Splineを対応させるための変更，および2次元用追加部分
	// 既存のコードはコメントアウト
	//
	// if(FittingDynamic == 1){
	// fprintf(fp,"\tdouble t1,t2;\n//%s\tdynamicDspline(%d,&OAT_iusw1_",s.c_str(),CaseArrayCount);
	//
	// s = Name;
	// fprintf(fp,"%s,1,0);\n\tt1 = OAT_Wtime();\n",s.c_str() );
	//
	//
	// }

	if (TuneGroup == tgDynamic && FittingDspline != 0) {
		if (FittingDspline == 1) {
			if (MainF->SrcCodeType == MainF->sctC) {
				fprintf(fp,
					"\tdouble t1,t2;\n/*%s*/\tdynamicDspline%s%s(%d,&OAT_iusw1_"
					, s.c_str(), TuneGroupName.c_str(), Name.c_str(),
					CaseArrayCount);

				s = Name;
				fprintf(fp, "%s,1,0);\n\tt1 = OAT_Wtime();\n", s.c_str());
			}
			else if (MainF->SrcCodeType == MainF->sctFortran90) {
				int index;
				/* OAT_SetParmをコメントアウトするための処理 */
				index = s.find("\n");
				s.replace(index, 2, "\n!");
				/* OAT_SetParmをコメントアウトここまで */
				fprintf(fp, "!%s      call dynamicDspline%s%s(%d,iusw1_",
					s.c_str(), TuneGroupName.c_str(), Name.c_str(),
					CaseArrayCount);

				s = Name;
				fprintf(fp, "%s,1,0.0d0)\n      t1 = omp_get_wtime()\n",
					s.c_str());
			}
		}
		else if (FittingDspline == 2) {
			int CaseArrayCount2[2];
			CaseArrayCount2[0] = (int)((variedToValue[0] - variedFromValue[0])
				/ variedStepValue[0] + 1);
			CaseArrayCount2[1] = (int)((variedToValue[1] - variedFromValue[1])
				/ variedStepValue[1] + 1);

			if (MainF->SrcCodeType == MainF->sctC) {
				fprintf(fp,
					"\tdouble t1,t2;\n/*%s*/\tdynamicDspline2%s%s(%d,%d,%d,&OAT_iusw1_",
					s.c_str(), TuneGroupName.c_str(), Name.c_str(),
					CaseArrayCount, CaseArrayCount2[0], CaseArrayCount2[1]);

				s = Name;
				fprintf(fp, "%s,1,0);\n\tt1 = OAT_Wtime();\n", s.c_str());
			}
			else if (MainF->SrcCodeType == MainF->sctFortran90) {
				int index;
				/* OAT_SetParmをコメントアウトするための処理 */
				index = s.find("\n");
				s.replace(index, 2, "\n!");
				/* OAT_SetParmをコメントアウトここまで */
				fprintf(fp,
					"!%s      call dynamicDspline2%s%s(%d,%d,%d,iusw1_",
					s.c_str(), TuneGroupName.c_str(), Name.c_str(),
					CaseArrayCount, CaseArrayCount2[0], CaseArrayCount2[1]);

				s = Name;
				fprintf(fp, "%s,1,0.0d0)\n      t1 = omp_get_wtime()\n",
					s.c_str());
			}
		}
	}
	//
	// ここまで
	//
	/** ******************************************************************************************************************************************************** */
	else {
		fprintf(fp, "%s", s.c_str()); // 既存部分
	}
	// d-spline用追加部分　ここまで　既存の部分をelseで囲っている
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// IndentStr   インデント文字列
// OutMode 出力モード
// Use_OAT_functuonNameF: _OATの関数名を使うかどうかの指定
//
// 3.概要
//
// 置き換えコードを出力
// Call SetParam()
// Call xxxx()
// コメントに置き換える。
//
// Modeの設定で、出力対象（範囲）を変える。 Add 2004/09/14
//
// OutMode=0 : SetParam + Call Func() + Comment
// OutMode=1 : SetParam + Call Func()
// OutMode=2 : SetParam
// OutMode=3 : Call Func() + Comment
//
// Use_OAT_functuonNameF: OAT_SetParamをOAT_SetParam_OATに置き換える。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputSrcCode(FILE *fp, string IndentStr, int OutMode,int Use_OAT_functuonNameF) {
	int i;
	TToken *Token;
	TValData *ValData;
	string s, s2;
	TScript *Script;
	string BaseValName;

	BaseValName = BaseValList->Strings[0];
	// Param OAT_INSTALL = 1  が ABCScript.h　にあるが、
	// モジュールが異なるため、参照できてないので、値自体をセットしている
	// 2004/05/28
	if (OutMode != 3) {
		if (MainF->Call_SetParam_Script == NULL) { // 指定なしの場合のみ
			OutputSetParmSrcCode(fp, IndentStr,Use_OAT_functuonNameF);
		}
	}
	if (OutMode != 2) {
		s = GetPrePostSubregionStr(0, 0);
		fprintf(fp, "%s", s.c_str());

		// if(MainF->SrcCodeType != MainF->sctC){
		if (MainF->SrcCodeType == MainF->sctFortran90) {
			s = "      call " + FuncName + "(";
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str + ",";
			}
			s += "iusw1_" + Name + ")\n"; // 当面は１変数だけの unrollとなる。
		}
		else if (MainF->SrcCodeType == MainF->sctFortran77) {
			s = "      call " + FuncName + "(";
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str + ",";
			}
			s += "iusw1_" + Name + ");\n"; // 当面は１変数だけの unrollとなる。
		}
		else {
			s = IndentStr + "\t" + FuncName + "(";
			for (i = 0; i < ArgValList->Count; i++) {
				if (ArgValList->Strings[i] == "") {
					continue;
				}
				ValData = (TValData*)ArgValList->Objects[i];
				s += ValData->Str + ",";
			}
			s += "OAT_iusw1_" + Name + ");\n"; // 当面は１変数だけの unrollとなる。

		}
		s = SepLongStr(s);
		fprintf(fp, "%s", s.c_str());
		s = GetPrePostSubregionStr(1, 0);
		fprintf(fp, "%s", s.c_str());
	}
	//
	// 対象ブロックをコメントにして出力する。
	// case()内の場合,SetParamだけの場合は、Skip
	//
	// d-spline用追加部分

	int CaseArrayCount;
	CaseArrayCount = CaseCount;

	/** ***************************************************************************************************************************** */
	//
	// Kogakuin Irie
	// Fortran90用のd-Spline用追加部分，および2次元用追加部分
	// 既存のコードはコメントアウト
	/* if(FittingDynamic == 1){
	fprintf(fp,"\tt2 = OAT_Wtime();\n");
	fprintf(fp,"\tdynamicDspline(%d,&OAT_iusw1_",CaseArrayCount);

	s = Name;
	fprintf(fp,"%s,2,t2-t1);\n",s.c_str() );


	}
	//d-spline用追加部分ここまで
	 */
	if (TuneGroup == tgDynamic) {
		if (FittingDspline == 1) {
			if (MainF->SrcCodeType == MainF->sctC) {
				fprintf(fp, "\tt2 = OAT_Wtime();\n");
				fprintf(fp, "\tdynamicDspline%s%s(%d,&OAT_iusw1_",
					TuneGroupName.c_str(), Name.c_str(),
					CaseArrayCount);

				s = Name;
				fprintf(fp, "%s,2,t2-t1);\n", s.c_str());
			}
			else if (MainF->SrcCodeType == MainF->sctFortran90) {
				fprintf(fp, "      t2 = omp_get_wtime()\n");
				fprintf(fp, "      call dynamicDspline%s%s(%d,iusw1_",
					TuneGroupName.c_str(), Name.c_str(), CaseArrayCount);

				s = Name;
				fprintf(fp, "%s,2,t2-t1)\n", s.c_str());
			}
		}
		if (FittingDspline == 2) {
			int CaseArrayCount2[2];
			CaseArrayCount2[0] = (int)((variedToValue[0] - variedFromValue[0])
				/ variedStepValue[0] + 1);
			CaseArrayCount2[1] = (int)((variedToValue[1] - variedFromValue[1])
				/ variedStepValue[1] + 1);

			if (MainF->SrcCodeType == MainF->sctC) {
				fprintf(fp, "\tt2 = OAT_Wtime();\n");

				fprintf(fp, "\tdynamicDspline2%s%s(%d,%d,%d,&OAT_iusw1_",
					TuneGroupName.c_str(), Name.c_str(), CaseArrayCount,
					CaseArrayCount2[0], CaseArrayCount2[1]);

				s = Name;
				fprintf(fp, "%s,2,t2-t1);\n", s.c_str());
			}
			else if (MainF->SrcCodeType == MainF->sctFortran90) {
				fprintf(fp, "      t2 = omp_get_wtime();\n");

				fprintf(fp, "      call dynamicDspline2%s%s(%d,%d,%d,iusw1_",
					TuneGroupName.c_str(), Name.c_str(), CaseArrayCount,
					CaseArrayCount2[0], CaseArrayCount2[1]);

				s = Name;
				fprintf(fp, "%s,2,t2-t1);\n", s.c_str());
			}
		}
	}
	//
	// ここまで
	//
	/** ***************************************************************************************************************************** */

	if ((OutMode != 0) && (OutMode != 3)) {
		return;
	}
	s = "";
	for (i = TokenStartPos; i <= TokenEndPos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->Script != NULL) {
			Script = (TScript*)Token->Script;
			s = Token->OrgStr;
			i++;
			for (; i < TokenEndPos; i++) {
				Token = (TToken*)TokenList->Items[i];
				s += Token->OrgStr;
				if (Token->LineEndF) {
					break;
				}
			}
			if (MainF->SrcCodeType == MainF->sctC) {
				fprintf(fp, "//%s", s.c_str());
			}
			else if (MainF->SrcCodeType == MainF->sctFortran77) {
				fprintf(fp, "c%s", s.c_str());
			}
			else {
				fprintf(fp, "!%s", s.c_str());
			}

			//
			// !OAT$ debug()　スクリプトの処理
			//
			if (Script->ScType == sct_debug) { // number
				string cs;

				if (MainF->SrcCodeType == MainF->sctC) {
					if (MainF->DebugF) {
						s = IndentStr + "\t";
						cs = "";
					}
					else {
						s = "//" + IndentStr + "\t";
						cs = "//";
					}
					s2 = s + "if(OAT_DEBUG >= 1){";
					fprintf(fp, "%s\n", s2.c_str());
					s2 = s + "\tprintf(\"myid: %d\\n\",myid);";
					fprintf(fp, "%s\n", s2.c_str());
					s2 = s + "\tprintf(\"" + TuneGroupName + " Routine: " +
						Name + "\\n\");";
					fprintf(fp, "%s\n", s2.c_str());

					if (Script->ScValDataList != NULL) {
						for (int k = 0; k < Script->ScValDataList->Count; k++) {
							TScValData *ScValData;
							ScValData = (TScValData*)
								Script->ScValDataList->Items[k];
							if (ScValData->Str == "bp") {
								s2 = s +
									"\tprintf(\"OAT_STARTTUNESIZE: %d\\n\",OAT_STARTTUNESIZE);";
								fprintf(fp, "%s\n", s2.c_str());
								s2 = s +
									"\tprintf(\"OAT_ENDTUNESIZE: %d\\n\",OAT_ENDTUNESIZE);";
								fprintf(fp, "%s\n", s2.c_str());
								s2 = s +
									"\tprintf(\"OAT_SAMPDIST: %d\\n\",OAT_SAMPDIST);";
								fprintf(fp, "%s\n", s2.c_str());
							}
							else if (ScValData->Str == "pp") {
								//
								// Change iusw1 to iusw1_+Name 2005/02/28
								// s2 = s+"  print *, \"iusw1: \",iusw1";

								// s2 = s+"  print *, \"iusw1: \",iusw1_"+Name;
								s2 = s + "  print *, \"iusw1: \",OAT_iusw1_" +
									Name;
								s2 = s +
									"\tprintf(\"iusw1: %d\\n\",OAT_iusw1_" + Name + ");";
								fprintf(fp, "%s\n", s2.c_str());
							}
							else {
								if (ScValData->Str.find("OAT") != string::npos)
								{
									s2 = s + "\tprintf(\"" + ScValData->Str +
										": %d\\n\"," + ScValData->Str + ");";
									fprintf(fp, "%s\n", s2.c_str());
								}
								else {
									s2 = s + "\tprintf(\"Error! : " +
										ScValData->Str + " \");";
									fprintf(fp, "%s\n", s2.c_str());
									s2 = s +
										"\tprintf(\"You can only specify the system parameters\");";
									fprintf(fp, "%s\n", s2.c_str());
									s2 = s +
										"\tprintf(\"   in the co-operator of debug. \");";
									fprintf(fp, "%s\n", s2.c_str());
								}
							}
						}
						s2 = s + "}";
						fprintf(fp, "%s\n", s2.c_str());
					}
				}
				else {
					if (MainF->SrcCodeType == MainF->sctFortran90) {
						if (MainF->DebugF) {
							fprintf(fp, "        if (OAT_DEBUG .ge. 2)then\n");
							fprintf(fp,
								"          print *, \'oat_myid: \',oat_myid\n");
							s = "          print *, \'" + TuneGroupName +
								" Routine: " + Name + "=\',iusw1_" + Name;
							fprintf(fp, "%s\n", s.c_str());
							fprintf(fp, "        endif\n");
						}
						else {
							fprintf(fp, "!        if (OAT_DEBUG .ge. 2)then\n");
							fprintf(fp,
								"!          print *, \'oat_myid: \',oat_myid\n"
								);
							s = "!          print *, \'" + TuneGroupName +
								" Routine: " + Name + "=\',iusw1_" + Name;
							fprintf(fp, "%s\n", s.c_str());
							fprintf(fp, "!        endif\n");
						}
					}
					else {
						if (MainF->DebugF) {
							fprintf(fp, "        if (OAT_DEBUG .ge. 1)then\n");
							fprintf(fp, "          print *, \'myid: \',myid\n");
							s = "          print *, \'" + TuneGroupName +
								" Routine: " + Name + "\'";
							fprintf(fp, "%s\n", s.c_str());
							fprintf(fp, "        endif\n");
						}
						else {
							fprintf(fp, "c        if (OAT_DEBUG .ge. 1)then\n");
							fprintf(fp,
								"c          print *, \'myid: \',myid\n");
							s = "c          print *, \'" + TuneGroupName +
								" Routine: " + Name + "\'";
							fprintf(fp, "%s\n", s.c_str());
							fprintf(fp, "c        endif\n");
						}
					}
				}
			}
			s = "\n";
		}
		else { // not Script
			if (MainF->SrcCodeType == MainF->sctC) {
				s = "//";
				for (; i < TokenEndPos; i++) {
					Token = (TToken*)TokenList->Items[i];
					s += Token->OrgStr;
					if (Token->LineEndF) {
						break;
					}
					if (s.find('\n') != string::npos) {
						// 改行が文字列の最後でなければ、改行の後に//を入れる。
						if ((int)s.find('\n') != (int)s.length() - 1) {
							s.insert(s.find('\n') + 1, "//");
						}
						break;
					}
				}
			}
			else {
				if (MainF->SrcCodeType == MainF->sctFortran77) {
					s = "c";
				}
				else {
					s = "!";
				}
				for (; i < TokenEndPos; i++) {
					Token = (TToken*)TokenList->Items[i];
					s += Token->OrgStr;
					if (Token->LineEndF) {
						break;
					}
				}
				s = SepLongStr(s);
				for (int j = s.length() - 2; j >= 0; j--) {
					if (s[j] == '\n') {
						// 改行が文字列の最後でなければ、改行の後にcまたは!を入れる。
						if (MainF->SrcCodeType == MainF->sctFortran77) {
							s.insert(j + 1, "c");
						}
						else {
							s.insert(j + 1, "!");
						}
					}
				}
			}
			fprintf(fp, "%s", s.c_str());
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp  出力先ファイルポインタ
// TargetSubRegion 対象サブリージョン
// iusw1   対象iusw1
// Skip_Pragma_ACCF pragma_accのスキップをおこなうかどうか
//
// 3.概要
// ソースの指定範囲を出力（コメントにはしない、Scriptは出力しない）
// NestしたScriptの置換も行う
// UnRoll以外でのvarid指定も、変換対象となる。
// また、TragetSubRegionと一致しないSubRegionは、Ｓｋｉｐする。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputReplaceSrc(FILE *fp, void *TargetSubRegion, int iusw1,
	bool Skip_Pragma_ACCF) {
	int i, j, k;
	TToken *Token;
	string s, IndentStr;
	TScript *Script;
	int SubRegionIdx;
	TSubRegion *SubRegion;
	//
	// 対象ブロックを置換して出力する。
	//
	s = "";
	IndentStr = "    ";
	SubRegionIdx = 0;
	for (i = TokenStartPos; i <= TokenEndPos; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->Script != NULL) {
			Script = (TScript*)Token->Script;
			if (Script->ScType == sct_GWV_target){
				// GWV-target一致ラベルがあれば次行を置換する。
				int Idx = -1;
				int Idx1,LabelIdx;
				string LabelStr;
				string OutStr;
				TStringList *GWV_LabelStrList;
				TStringList *GWV_ReplaceStrList;

				Idx = GWV_TargetStrList->IndexOfObject(Script);
				if(Idx != -1){
					LabelIdx = -1;
					LabelStr = GWV_TargetStrList->Strings[Idx];
					//
					//	ラベルに一致する置換用文字列を探す。
					//
					for(Idx1 = 0; Idx1 < GWV_ListList->Count ; Idx1+=2){
						GWV_LabelStrList = (TStringList *)GWV_ListList->Items[Idx1];
						GWV_ReplaceStrList = (TStringList *)GWV_ListList->Items[Idx1+1];
						LabelIdx = GWV_LabelStrList->IndexOf(LabelStr);
						if(LabelIdx != -1){
							break;	// 一致ラベルが見つかった。
						}
					}
					if(LabelIdx != -1){
						//
						//	置換えに使う文字列を求める。
						//
						int TypeCount,TypeIdx,ReplaceStrIdx;
						string GangWorkerVectorStr[3];

						// １つ手前までの種類数の積を求める。
						TypeCount = 1;
#if 1
						for(int k2 = Idx1+2; k2 < GWV_ListList->Count ; k2 +=2){
							GWV_LabelStrList = (TStringList *)GWV_ListList->Items[k2];
							GWV_ReplaceStrList = (TStringList *)GWV_ListList->Items[k2+1];
							TypeCount *= (GWV_ReplaceStrList->Count/3)/GWV_LabelStrList->Count;
						}
						TypeIdx = iusw1 / TypeCount; // Idx1以後の数で割る。
#else
						for(int k2 = 0; k2 < Idx1 ; k2 +=2){
							GWV_LabelStrList = (TStringList *)GWV_ListList->Items[k2];
							GWV_ReplaceStrList = (TStringList *)GWV_ListList->Items[k2+1];
							TypeCount *= (GWV_ReplaceStrList->Count/3)/GWV_LabelStrList->Count;
						}
						TypeIdx = iusw1 / TypeCount; // Idx1以前の数で割る。
#endif
						GWV_LabelStrList = (TStringList *)GWV_ListList->Items[Idx1];
						GWV_ReplaceStrList = (TStringList *)GWV_ListList->Items[Idx1+1];
						TypeCount = (GWV_ReplaceStrList->Count/3)/GWV_LabelStrList->Count;
						TypeIdx = TypeIdx % TypeCount; // Idx1の数での余り
						ReplaceStrIdx = TypeIdx * GWV_LabelStrList->Count + LabelIdx;

						GangWorkerVectorStr[0] = GWV_ReplaceStrList->Strings[3*ReplaceStrIdx];
						GangWorkerVectorStr[1] = GWV_ReplaceStrList->Strings[3*ReplaceStrIdx+1];
						GangWorkerVectorStr[2] = GWV_ReplaceStrList->Strings[3*ReplaceStrIdx+2];

						//
						//	Scriptの次の行を置換する。行の geng() work() Vector()部分
						//	置換え、または追加。
						//
						// スクリプトの行末までトークンを進める。
						for (; i <= TokenEndPos; i++) {
							Token = (TToken*)TokenList->Items[i];
							if (Token->TokId == tid_LineEnd) {
								break;
							}
						}
						//
						//　次行の置換え文字列を所得する。
						//
						i++;
						OutStr = "";
						for (; i <= TokenEndPos; i++) {
							Token = (TToken*)TokenList->Items[i];
							if (Token->TokId == tid_LineEnd) {
								break;
							}
							OutStr += Token->OrgStr;
						}
						int cp1,cp2;
						const char *KeyWord[] = {"gang","worker","vector"};
						string s3;
						int iData;

						for(int KeyIdx = 0 ; KeyIdx < 3 ; KeyIdx++){
							cp1 = LowerCase(OutStr).find(KeyWord[KeyIdx]);
							if(cp1 != -1){
								cp2 = cp1+strlen(KeyWord[KeyIdx]);
								while(OutStr[cp2] == ' '){
									cp2++;
								}
								if(OutStr[cp2] == '('){
									while((cp2 < (int)OutStr.length())&&(OutStr[cp2++] != ')')){
										;
									}
								}
								if((cp1 > 0)&&(OutStr[cp1-1] == ' ')){
									cp1--;	// １つ前のスペースがあれば削除
								}
								OutStr.erase(cp1,cp2-cp1);
							}
						}
						//
						//	gang worker vector を追加する。
						//
						for(int KeyIdx = 0 ; KeyIdx < 3 ; KeyIdx++){
							s3 = GangWorkerVectorStr[KeyIdx];
							iData = atof(s3.c_str());
							if(iData > 0){
								OutStr += (string)" "+KeyWord[KeyIdx]+"("+s3+")";
							}else if(s3 == "0"){
							}else{
								OutStr += (string)" "+KeyWord[KeyIdx];
							}
						}
						fprintf(fp, "%s\n", OutStr.c_str()); // 出力する
					}
				}
			}
			else if ((Script->ScRegion == scr_start) && (i != TokenStartPos)) {
				// NestしたTuneRegionあり
				TTuneRegion *TuneRegion;

				TuneRegion = (TTuneRegion*)Script->TuneRegion;
				TuneRegion->OutputSrcCode(fp, "    ", 1);
				i = TuneRegion->TokenEndPos;
				// }else if(Script->ScRegion == scr_substart){
			}
			else if ((Script->ScRegion == scr_substart) &&
				(Script->ScType == sct_select)) { // Sub region Start
				// SubRegion開始
				// もし、TargetSubRegionでなければ、sub region endまで
				// skipする。TargetSubRegionの場合は、そのまま次へ
				//
				if (SubRegionIdx < SubRegionList->Count) {
					SubRegion = (TSubRegion*)
						SubRegionList->Items[SubRegionIdx++];
					if (SubRegion != TargetSubRegion) {
						i = SubRegion->TokenEndPos; // Skip To End of SubRegion
					}
				}
			}
			else if (Script->ScRegion == scr_substart) {
				//
				// Pre,PostのサブリージョンのＳｋｉｐ
				// End_SubReguinまでSkipする。
				//
				int Nest = 0;
				for (; i <= TokenEndPos; i++) {
					Token = (TToken*)TokenList->Items[i];
					if (Token->Script != NULL) {
						Script = (TScript*)Token->Script;
						if (Script->ScRegion == scr_substart) {
							Nest++;
						}
						else if (Script->ScRegion == scr_subend) {
							Nest--;
							if (Nest == 0) {
								break;
							}
						}
					}
				}
			}
			// 行末までトークンを進める。
			for (; i <= TokenEndPos; i++) {
				Token = (TToken*)TokenList->Items[i];
				if (Token->TokId == tid_LineEnd) {
					break;
				}
			}
		}
		else if ((TuneKind != tkUnroll) && ((Token->TokId == tid_Val) ||
				(Token->TokId == -1))) {
			//
			// variedによる変数の置換（Unrollのvariedは対象外)
			//
			s = Token->OrgStr;
			// checkPoint
			if (TargetSubRegion == NULL) {
				for (j = 0; j < variedCount; j++) {
					if (Token->Str == variedValName[j]) {
						/** ************************ */
						//
						// Kogakuin Irie
						// PPの変動範囲を実数にも対応
						// 既存コードはコメントアウト
						//
						// 定数に置換する。
						// int Idx,Value;
						int Idx;
						float Value; // 実数に対応
						//
						// ここまで
						//
						/** ************************ */

						Idx = iusw1;
						for (k = j + 1; k < variedCount; k++) {
							/** ************************************************************************** */
							//
							// Kogakuin Irie
							// PPの実数対応とstep対応のための処理
							// 既存コードはコメントアウト
							//
							// Idx /= (variedToValue[k] - variedFromValue[k]+1);
							Idx /= (int)
								((variedToValue[k] - variedFromValue[k])
								/ variedStepValue[k] + 1);
							//
							// ここまで
							//
							/** ************************************************************************** */
						}
						/** ************************************************************************************** */
						//
						// Kogakuin Irie
						// PPの実数対応とstep対応のための処理
						// 既存コードはコメントアウト
						//
						// Idx = Idx % (variedToValue[j] - variedFromValue[j]+1); // 余り
						Idx = Idx % (int)
							((variedToValue[j] - variedFromValue[j])
							/ variedStepValue[j] + 1); // 余り
						// Value = variedFromValue[j] + Idx;
						Value = variedFromValue[j] + Idx * variedStepValue[j];
#if 1
						//
						// ここまで
						//
						/** ************************************************************************************** */
						if (s == Token->Str) {
							/** ************************ */
							//
							// Kogakuin Irie
							// PPの変動範囲を実数にも対応
							// 既存コードはコメントアウト
							//
							// s = IntToStr(Value);
							s = FloatToStr(Value);
							//
							// ここまで
							//
							/** ************************ */
						}
						else {
							/** *********************** */
							//
							// Kogakuin Irie
							// PPの変動範囲を実数にも対応
							// 既存コードはコメントアウト
							//
							// s = " "+IntToStr(Value);
							s = " " + FloatToStr(Value);
							//
							// ここまで
							//
							/** *********************** */
						}
#else
						if(Value == (int)Value){
							s = IntToStr(Value);
						}else{
							s = FloatToStr(Value);
						}
						if (Token->Str != Token->OrgStr) {
							int cp1 = Token->OrgStr.find(Token->Str);

							if(cp1 != -1){
								s = Token->OrgStr.substr(0,cp1) + s;
							}
						}
#endif
						break;
					}
				}
				//
				//	List指定による変数の置換えのチェック
				//
				if(ListSrcStrList->Count != 0){
					int Idx,Idx2;

					Idx = ListSrcStrList->IndexOf(Token->Str);
					if(Idx != -1){
						Idx2 = iusw1 * ListSrcStrList->Count + Idx;
						if(Idx2 < ListReplaceStrList->Count){
							s = ListReplaceStrList->Strings[Idx2];
						}
					}
				}
			}
			else {
				//
				// SubRegionありの場合の varidの置換
				// SubRegionごとの varidCountを使用する。
				// iusw1も、SubRegion単位でのIdxで呼ばれている。（上位から）
				//
				SubRegion = (TSubRegion*)TargetSubRegion;
				for (j = 0; j < SubRegion->variedCount; j++) {
					if (Token->Str == SubRegion->variedValName[j]) {
						/** ************************* */
						//
						// Kogakuin Irie
						// PPの変動範囲を実数にも対応
						// 既存コードはコメントアウト
						//
						// 定数に置換する。
						// int Idx,Value;
						int Idx;
						float Value;
						//
						// ここまで
						//
						/** ************************* */

						Idx = iusw1;
						for (k = j + 1; k < SubRegion->variedCount; k++) {
							/** ************************************************************************ */
							//
							// Kogakuin Irie
							// PPの実数対応とstep対応のための処理
							// 既存コードはコメントアウト
							//
							// Idx /= (SubRegion->variedToValue[k]
							// - SubRegion->variedFromValue[k]+1);
							Idx /= (int)
								((SubRegion->variedToValue[k]
									- SubRegion->variedFromValue[k])
								/ SubRegion->variedStepValue[k] + 1);
							//
							// ここまで
							//
							/** ************************************************************************ */
						}
						/** ********************************************************************************* */
						//
						// Kogakuin Irie
						// PPの実数対応とstep対応のための処理
						// 既存コードはコメントアウト
						//
						// Idx = Idx % (SubRegion->variedToValue[j]
						// - SubRegion->variedFromValue[j]+1); // 余り
						Idx = Idx % (int)
							((SubRegion->variedToValue[j]
								- SubRegion->variedFromValue[j])
							/ SubRegion->variedStepValue[j] + 1); // 余り
						// Value = SubRegion->variedFromValue[j] + Idx;
						Value = SubRegion->variedFromValue[j]
							+ Idx * SubRegion->variedStepValue[j];
						//
						// ここまで
						//
						/** ********************************************************************************* */
						if (s == Token->Str) {
							/** ************************ */
							//
							// Kogakuin Irie
							// PPの変動範囲を実数にも対応
							// 既存コードはコメントアウト
							//
							// s = IntToStr(Value);
							s = FloatToStr(Value);
							//
							// ここまで
							//
							/** ************************ */
						}
						else {
							/** ************************ */
							//
							// Kogakuin Irie
							// PPの変動範囲を実数にも対応
							// 既存コードはコメントアウト
							//
							// s = " "+IntToStr(Value);
							s = " " + FloatToStr(Value);
							//
							// ここまで
							//
							/** ************************ */
						}
						break;
					}
				}
			}
			/** *************************************************************** */
			//
			// Kogakuin Irie
			// 実数対応用追加部分
			// 変動範囲の内，整数で表現できる値は整数で表現させる
			//
			int index, strSize;
			index = s.find_first_of(".");

			if (index != (int)string::npos) {
				if (s.find_first_of("123456789",
						index + 1) == string::npos && index != (int)s.size() - 1) {
					strSize = s.size();

					for (j = index; j < strSize; j++) {
						s.replace(index, 1, "");
					}
				}
			}
			//
			// 実数対応用追加成分　ここまで
			//
			/** *************************************************************** */
			fprintf(fp, "%s%s", IndentStr.c_str(), s.c_str());
			IndentStr = "";
		}
		else if (Token->TokId == tid_Comment) {
			// コメントは、インデントなしで出力
			if(variedDCount != 0){	// variedDの指定があれば、置換を実行する。
				int k2;
				for(k2 = 0 ; k2 < variedDCount ; k2++){
					if(variedDValName[k2] == Token->Str){
						break;
					}
				}
				if(k2 < variedDCount){
					int Idx;
					float Value; // 実数に対応
					Idx = iusw1;

					for (k = k2 + 1; k < variedDCount; k++) {
						Idx /= (int)((variedDToValue[k] - variedDFromValue[k])
								/ variedDStepValue[k] + 1);
					}
					Idx = Idx % (int)((variedDToValue[k2] - variedDFromValue[k2])
							/ variedDStepValue[k2] + 1); // 余り
					Value = variedDFromValue[k2] + Idx * variedDStepValue[k2];
					if(Value == (int)Value){
						s = IntToStr(Value);
					}else{
						s = FloatToStr(Value);
					}
					if (Token->Str != Token->OrgStr) {
						int cp1 = Token->OrgStr.find(Token->Str);

						if(cp1 != -1){
							s = Token->OrgStr.substr(0,cp1) + s;
						}
					}
					fprintf(fp, "%s", s.c_str());
				}else{
					fprintf(fp, "%s", Token->OrgStr.c_str());
				}
				continue;
			}
			if(ListDSrcStrList->Count != 0){
				int Idx,Idx2;

				s = Token->OrgStr;
				Idx = ListDSrcStrList->IndexOf(Token->Str);
				if(Idx != -1){
					Idx2 = iusw1 * ListDSrcStrList->Count + Idx;
					if(Idx2 < ListDReplaceStrList->Count){
						s = ListDReplaceStrList->Strings[Idx2];
						if (Token->Str != Token->OrgStr) {
							int cp1 = Token->OrgStr.find(Token->Str);

							if(cp1 != -1){
								s = Token->OrgStr.substr(0,cp1) + s;
							}
						}
					}
				}
				fprintf(fp, "%s", s.c_str());
				continue;
			}
			fprintf(fp, "%s", Token->OrgStr.c_str());
			IndentStr = "    ";
		}
		else if (Token->TokId == tid_LineEnd) {
			fprintf(fp, "%s", Token->OrgStr.c_str()); // 改行も、そのまま
//			fprintf(fp, "%s[Line End]", Token->OrgStr.c_str()); // 改行も、そのまま
			//
			//	もし、次の行がTargetStrListの文字列と先頭から一致する場合には、
			//	行を置き換えて次の行まで進める。行としての置換が行われる。
			//
			if(TargetStrList->Count != 0){
				TToken *Token2;
				string LineStr = "";
				string NewLineStr;
				string TargetStr;
				int k,k2;
				int cp1,cp2,cp3,len;

				for(k = i+1 ; k < TokenEndPos ; k++){
					Token2 = (TToken*)TokenList->Items[k];
					if (Token2->TokId == tid_LineEnd) {
						break;
					}
					LineStr += Token2->OrgStr;
				}
				LineStr = Trim(LineStr);	// 前後の空白は対象外とする。
				//
				//	文字列の一致を検索する。
				//
				for(k2 = 0 ; k2 < TargetStrList->Count ;k2++){
					TargetStr = TargetStrList->Strings[k2];
					len = TargetStr.length();
					if((int)LineStr.length() < len){
						// 行の文字列が一致を検索する文字列より短い場合は一致しない。
						continue;
					}
					for(cp1 = 0 ; cp1 < len ; cp1++){
						if(LineStr[cp1] != TargetStr[cp1]){
							break;	// cp1の手前まで一致
						}
					}
					for(cp2 = cp1 ; cp2 < (int)LineStr.length() ; cp2++){
						if(LineStr[cp2] == TargetStr[cp1]){
							break;	// cp3の手前までLineStrの文字と一致
						}
					}
					cp3 = cp1;
					for(; cp2 < (int)LineStr.length() ; cp2++){
						if(LineStr[cp2] != TargetStr[cp3]){
							break;	// cp2がcp1の文字と一致
						}
						if(cp3 < len){
							cp3++;
						}else{
                            break;
                        }
					}
					//
					//	TargetStr[0::cp1-1] までと [cp1::] までの文字が一致した。
					//	置換え行の文字列を作成する。ReplaceCaseIdxの番号の文字列をセットする。
					//
					if(cp3 == (int)TargetStr.length()){
						NewLineStr = TargetStr.substr(0,cp1);
//						NewLineStr += IntToStr(iusw1);
						if(iusw1 < ReplaceStrList->Count){
							NewLineStr += ReplaceStrList->Strings[iusw1];
						}
						NewLineStr += TargetStr.substr(cp1,TargetStr.length());
						break;
					}
				}
				if(NewLineStr != ""){
					fprintf(fp, "%s", NewLineStr.c_str()); // 改行も、そのまま
					i = k-1;	// 使用したトークン分をスキップ
					continue;
				}
			}
			continue;
		}
		else if (Token->TokId == tid_SharpPragma) {
			TToken *Token2;

			// #Pragmaは、インデントなしで出力する。
			if (Skip_Pragma_ACCF) {
				TToken *Token2;

				if (i < TokenEndPos) {
					Token2 = (TToken*)TokenList->Items[i + 1];

					if (LowerCase(Trim(Token2->OrgStr)) == "acc") {
						for (i = i + 1; i < TokenEndPos; i++) {
							Token2 = (TToken*)TokenList->Items[i];
							if (Token2->LineEndF) {
								break;
							}
						}
						continue;
					}
				}
			}
			// variableDの置換を行う。
			if(variedDCount != 0){
				int k2;
				string s;

				for (; i < TokenEndPos; i++) {
					Token2 = (TToken*)TokenList->Items[i];

					for(k2 = 0 ; k2 < variedDCount ; k2++){
						if(variedDValName[k2] == Token2->Str){
							break;
						}
					}
					if(k2 < variedDCount){
						int Idx;
						float Value; // 実数に対応
						Idx = iusw1;

						for (k = k2 + 1; k < variedDCount; k++) {
							Idx /= (int)((variedDToValue[k] - variedDFromValue[k])
									/ variedDStepValue[k] + 1);
						}
						Idx = Idx % (int)((variedDToValue[k2] - variedDFromValue[k2])
								/ variedDStepValue[k2] + 1); // 余り
						Value = variedDFromValue[k2] + Idx * variedDStepValue[k2];
						/*
							char cs[256];
							sprintf(cs,"%g",Value);
							s = cs;
						*/
						if(Value == (int)Value){
							s = IntToStr(Value);
						}else{
							s = FloatToStr(Value);
						}
						if (Token->Str != Token->OrgStr) {
							int cp1 = Token->OrgStr.find(Token->Str);

							if(cp1 != -1){
								s = Token->OrgStr.substr(0,cp1) + s;
							}
						}
						fprintf(fp, "%s", s.c_str());
					}else{
						fprintf(fp, "%s", Token2->OrgStr.c_str());
					}
					if (Token2->LineEndF) {
						break;
					}
				}
				continue;	// 置換終了
			}
			for (; i < TokenEndPos; i++) {
				Token2 = (TToken*)TokenList->Items[i];
				if(ListDSrcStrList->Count != 0){
					int Idx,Idx2;

					s = Token2->OrgStr;
					Idx = ListDSrcStrList->IndexOf(Token2->Str);
					if(Idx != -1){
						Idx2 = iusw1 * ListDSrcStrList->Count + Idx;
						if(Idx2 < ListDReplaceStrList->Count){
							s = ListDReplaceStrList->Strings[Idx2];
							if (Token2->Str != Token2->OrgStr) {
								int cp1 = Token2->OrgStr.find(Token2->Str);

								if(cp1 != -1){
									s = Token2->OrgStr.substr(0,cp1) + s;
								}
							}
						}
					}
					fprintf(fp, "%s", s.c_str());
				}else{
					fprintf(fp, "%s", Token2->OrgStr.c_str());
                }
				if (Token2->LineEndF) {
					break;
				}
			}
			IndentStr = "";
			continue;
		}
		else if ((Token->TokId == tid_CALL)&&(i < TokenEndPos-1)) {	// 2016/02/26
			//
			// 	_OATへの置換え SUBROUTINかをチェックする。
			//
			TToken *Token2;
			string SubroutineName;
			int Idx;
			TPass5 *Pass5;

			Token2 = (TToken*)TokenList->Items[i+1];
			SubroutineName = Token2->Str;

			Idx = -1;
			Pass5 = (TPass5 *)MainF->Pass5;
			for(int k = 0 ; k < Pass5->Call_SubroutineNameInRegionList->Count ; k++){
#ifdef _WIN32
				if(stricmp(Pass5->Call_SubroutineNameInRegionList->Strings[k].c_str(),
#else
				if(strcasecmp(Pass5->Call_SubroutineNameInRegionList->Strings[k].c_str(),
#endif
					SubroutineName.c_str()) == 0){
					Idx = k;
				}
			}
			if(Idx != -1){	// 一致
				fprintf(fp, "%s%s%s_OAT", IndentStr.c_str(), Token->OrgStr.c_str(),Token2->OrgStr.c_str());
				IndentStr = "";
				i++;
			}else{
				fprintf(fp, "%s%s", IndentStr.c_str(), Token->OrgStr.c_str());
				IndentStr = "";
			}
		}
		else {
			fprintf(fp, "%s%s", IndentStr.c_str(), Token->OrgStr.c_str());
			IndentStr = "";
		}
		if (Token->LineEndF) {
			IndentStr = "    ";
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// Mode　0:Prepro部分、1:PostPro部分を対象とする。
// InControlCode　0:SrcoCode部分 、1:ControlCode部分が作成象とする。
//
// 3.概要
// TuneRegion内の PrePro Sub RegionとPostPro Sub Righonの文字列を所得する。
//
// 4.機能説明
//
// 5.戻り値
// PrePro Sub RegionとPostPro Sub Righonの文字列を返す。この文字列は先頭と最
// 後にコメントを含んだ複数行の文字列となる。
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
string TTuneRegion::GetPrePostSubregionStr(int Mode, int InControlCode) {
	int i, j;
	TToken *Token;
	TScript *Script;
	// AnsiString rStr = "";
	string rStr = "";
	bool InRegionF;
	string s;

	for (i = TokenStartPos; i < TokenEndPos; i++) {
		Token = (TToken*)TokenList->Items[i];
		Script = (TScript*)Token->Script;
		if (Script == NULL) {
			continue;
		}
		if (Script->ScRegion == scr_substart) {
			if ((Mode == 0) && (Script->ScType == sct_prepro)) {
				// Sub region Start
				i = Script->LineEndTokPos;
				InRegionF = true;
			}
			else if ((Mode == 1) && (Script->ScType == sct_postpro)) {
				// Sub region Start
				i = Script->LineEndTokPos;
				InRegionF = true;
			}
			else {
				InRegionF = false;
			}
			// SubRegion Endまで検索する。
			for (j = i; j <= TokenEndPos; j++) {
				Token = (TToken*)TokenList->Items[j];
				if (Token->Script != NULL) {
					Script = (TScript*)Token->Script;
					if (Script->ScRegion == scr_start) {
						// Nestしている場合は、region end まで Skipする。
						for (; j <= TokenEndPos; j++) {
							Token = (TToken*)TokenList->Items[j];
							if (Token->Script != NULL) {
								Script = (TScript*)Token->Script;
								if (Script->ScRegion == scr_end) {
									break;
								}
							}
						}
					}
					else if (Script->ScRegion == scr_subend) {
						break;
					}
					else {
						if (InControlCode) {
							j = j + 1; // #pragma OAT の部分のみ取り除いて配置する。
						}
						else if (InRegionF) {
							rStr += Token->OrgStr;
						}
					}
				}
				else if (InRegionF) {
					rStr += Token->OrgStr;
				}
			}
			if (j >= TokenEndPos) {
				MainF->ErrMessage(i, "sub region endが見つかりません。");
			}
			i = j; // iをSubRegion End位置まで移動
		}

	}
	if (rStr != "") {
		if (MainF->SrcCodeType == MainF->sctC) {
			if (Mode == 0) {
				rStr = "// OAT prepro sub region start\n" + rStr;
				rStr += "// OAT prepro sub region end\n";
			}
			else {
				rStr = "// OAT postpro sub region start\n" + rStr;
				rStr += "// OAT postpro sub region end\n";
			}
		}
		else if (MainF->SrcCodeType == MainF->sctFortran77) {
			if (Mode == 0) {
				rStr = "c OAT prepro sub region start\n" + rStr;
				rStr += "c OAT prepro sub region end\n";
			}
			else {
				rStr = "c OAT postpro sub region start\n" + rStr;
				rStr += "c OAT postpro sub region end\n";
			}
		}
		else {
			if (Mode == 0) {
				rStr = "! OAT prepro sub region start\n" + rStr;
				rStr += "! OAT prepro sub region end\n";
			}
			else {
				rStr = "! OAT postpro sub region start\n" + rStr;
				rStr += "! OAT postpro sub region end\n";
			}
		}
	}
	return rStr;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// Mode　0:Prepro部分、1:PostPro部分を対象とする。
//
// 3.概要
// TuneRegion内の Pre BPset RegionとPost BPSet Righonの文字列を所得する。
//
// 4.機能説明
//
// 5.戻り値
// Pre BPSet RegionとPost BPSet Righonの文字列を返す。
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
string TTuneRegion::GetPrePostBPSetSubregionStr(int Mode) {
	int i, j;
	TToken *Token;
	TScript *Script;
	string rStr = "";
	bool InRegionF;
	string s;

	for (i = TokenStartPos; i < TokenEndPos; i++) {
		Token = (TToken*)TokenList->Items[i];
		Script = (TScript*)Token->Script;
		if (Script == NULL) {
			continue;
		}
		if (Script->ScRegion == scr_substart) {
			if ((Mode == 0) && (Script->ScType == sct_pre_BPset)) {
				// Sub region Start
				i = Script->LineEndTokPos;
				InRegionF = true;
			}
			else if ((Mode == 1) && (Script->ScType == sct_post_BPset)) {
				// Sub region Start
				i = Script->LineEndTokPos;
				InRegionF = true;
			}
			else {
				InRegionF = false;
			}
			// SubRegion Endまで検索する。
			for (j = i; j <= TokenEndPos; j++) {
				Token = (TToken*)TokenList->Items[j];
				if (Token->Script != NULL) {
					Script = (TScript*)Token->Script;
					if (Script->ScRegion == scr_start) {
						// Nestしている場合は、region end まで Skipする。
						for (; j <= TokenEndPos; j++) {
							Token = (TToken*)TokenList->Items[j];
							if (Token->Script != NULL) {
								Script = (TScript*)Token->Script;
								if (Script->ScRegion == scr_end) {
									break;
								}
							}
						}
					}
					else if (Script->ScRegion == scr_subend) {
						break;
					}
					else {
						j = j + 1; // #pragma OAT の部分のみ取り除いて配置する。
					}
				}
				else if (InRegionF) {
					rStr += Token->OrgStr;
				}
			}
			if (j >= TokenEndPos) {
				MainF->ErrMessage(i, "sub region endが見つかりません。");
			}
			i = j; // iをSubRegion End位置まで移動
		}

	}
#if 0	// pre post BPset は、コメントなし。
	if (rStr != "") {
		if (Mode == 0) {
			rStr = "// OAT pre BPset region start\n" + rStr;
			rStr += "// OAT pre BPSet region end\n";
		}
		else {
			rStr = "// OAT post BPSet region start\n" + rStr;
			rStr += "// OAT post BPSet region end\n";
		}
	}
#endif
	return rStr;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// サブリージョンクラスの生成
// スクリプトで定義されるサブリージョンのデータが格納される。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
TSubRegion::TSubRegion() {
	CaseCount = 1;
	variedCount = 0;
	AccordingStr = ""; // Accordingで指定された　数式 継承も可能
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// TokenList   トークンリスト
// sPos    開始位置
//
// 3.概要
// Cの場合は、forの後のネストが０の } か ; を探してその位置を返す。
// Fortranの場合は、tid_endoのネストが０を探してその位置を返す。
// 見つからない場合は -1を返す行の先頭トークン位置で返す。
//
// 4.機能説明
//
// 5.戻り値
// forに対応する }の位置。見つからない場合は 開始位置sPosを返す。
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::FindEndDo2(TList *TokenList, int sPos) {
	int i;
	TToken *Token;
	int Nest;

	Nest = 0;
	if (MainF->SrcCodeType == MainF->sctC) {
		for (i = sPos; i < TokenList->Count; i++) {
			Token = (TToken*)TokenList->Items[i];
			if (Token->TokId == tid_Kakko) {
				Nest++;
			}
			else if (Token->TokId == tid_Kokka) {
				Nest--;
			}
			else if (Token->TokId == tid_DaiKakko) {
				Nest++;
			}
			else if (Token->TokId == tid_DaiKokka) {
				Nest--;
				if (Nest <= 0) {
					return i;
				}
			}
			else if (Token->TokId == tid_Semikoron) {
				if (Nest <= 0) {
					return i;
				}
			}
		}
	}
	else {
		for (i = sPos; i < TokenList->Count; i++) {
			Token = (TToken*)TokenList->Items[i];
			if (Token->TokId == tid_DO) {
				Nest++;
			}
			else if (Token->TokId == tid_ENDDO) {
				Nest--;
				if (Nest <= 0) {
					return i;
				}
			}
		}
	}
	// return -1;
	return sPos;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// TokenList   トークンリスト
// sPos    開始位置
//
// 3.概要
// { }を調べてネストが０の場合の位置を返す。
// if()の後の{の位置からの検索に使用される。
// ifの直後からの}の検索にはFindEndDo2を使用する。
// 見つからない場合は -1を返す行の先頭トークン位置で返す。
//
// 4.機能説明
//
// 5.戻り値
// 対応する ENDIFDOの位置。見つからない場合は 開始位置 sPosを返す。
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TTuneRegion::FindEndIf(TList *TokenList, int sPos) {
	int i;
	TToken *Token;
	int Nest = 0;

	for (i = sPos; i < TokenList->Count; i++) {
		Token = (TToken*)TokenList->Items[i];
		if ((Token->TokId == tid_if) || (Token->TokId == tid_else)) {

		}
		else if (Token->TokId == tid_DaiKakko) {
			Nest++;
		}
		else if (Token->TokId == tid_DaiKokka) {
			Nest--;
			if (Nest == 0) {
				return i;
			}
		}
		else {
			if (Nest == 0) {
				return i;
			}
		}
	}
	// return -1;
	return sPos;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// Mode　0:Pripro部分、1:PostPro部分を対象とする。
//
// 3.概要
// TuneRegion内の PriPro Sub RegionとPostPro Sub Righonの文字列を所得する。
//
// 4.機能説明
//
// 5.戻り値
// PriPro Sub RegionとPostPro Sub Righonの文字列を返す。この文字列は先頭と最
// 後にコメントを含んだ複数行の文字列となる。
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
string TTuneRegion::GetPrePostSubregionStr(int Mode) {
	int i, j;
	TToken *Token;
	TScript *Script;
	string rStr = "";
	bool InRegionF;

	for (i = TokenStartPos; i < TokenEndPos; i++) {
		Token = (TToken*)TokenList->Items[i];
		Script = (TScript*)Token->Script;
		if (Script == NULL) {
			continue;
		}
		if (Script->ScRegion == scr_substart) {
			if ((Mode == 0) && (Script->ScType == sct_prepro)) {
				// Sub region Start
				InRegionF = true;
			}
			else if ((Mode == 1) && (Script->ScType == sct_postpro)) {
				// Sub region Start
				InRegionF = true;
			}
			else {
				InRegionF = false;
			}
			// SubRegion Endまで検索する。
			for (j = i; j <= TokenEndPos; j++) {
				Token = (TToken*)TokenList->Items[j];
				if (Token->Script != NULL) {
					Script = (TScript*)Token->Script;
					if (Script->ScRegion == scr_start) {
						// Nestしている場合は、region end まで Skipする。
						for (; j <= TokenEndPos; j++) {
							Token = (TToken*)TokenList->Items[j];
							if (Token->Script != NULL) {
								Script = (TScript*)Token->Script;
								if (Script->ScRegion == scr_end) {
									break;
								}
							}
						}
					}
					else if (Script->ScRegion == scr_subend) {
						break;
					}
				}
				else if (InRegionF) {
					rStr += Token->OrgStr;
				}
			}
			if (j >= TokenEndPos) {
				MainF->ErrMessage(i, "sub region endが見つかりません。");
			}
			i = j; // iをSubRegion End位置まで移動
		}

	}
	if (rStr != "") {
		if (Mode == 0) {
			if (MainF->SrcCodeType == MainF->sctC) {
				rStr = "// OAT pripro sub region start\n" + rStr;
				rStr += "// OAT pripro sub region end\n";
			}
			else if (MainF->SrcCodeType == MainF->sctFortran77) {
				rStr = "c!OAT$ pripro sub region start\n" + rStr;
				rStr += "c!OAT$ pripro sub region end\n";
			}
			else {
				rStr = "!!OAT$ pripro sub region start\n" + rStr;
				rStr += "!!OAT$ pripro sub region end\n";
			}
		}
		else {
			if (MainF->SrcCodeType == MainF->sctC) {
				rStr = "// OAT postpro sub region start\n" + rStr;
				rStr += "// OAT postpro sub region end\n";
			}
			else if (MainF->SrcCodeType == MainF->sctFortran77) {
				rStr = "c!OAT$ postpro sub region start\n" + rStr;
				rStr += "c!OAT$ postpro sub region end\n";
			}
			else {
				rStr = "!!OAT$ postpro sub region start\n" + rStr;
				rStr += "!!OAT$ postpro sub region end\n";
			}
		}
	}

	return rStr;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// string OrgStr 元の文字列
// int TokPos,int EndTokPos トークン範囲
// TStringList *NewValStrList 元と新規追加した変数名が交互に入ったリスト
//
// 3.概要
// #pragmaの文字列を変更する。#pragme omp xxx () の ()内の変数を増やす。
//
// 4.機能説明
// #pragmaの文字列を変更する。#pragme omp xxx () の ()内の変数を増やす。
// 例えば、#pragma omp (temp)で tempが多重化された場合は
// #pragma omp (temp,temp2)に置き換えられる。
//
// 5.戻り値
// 置き換え文字列。
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
string TTuneRegion::ChangePragmaStr(string OrgStr, int TokPos, int EndTokPos,
	TStringList *NewValStrList) {
	int i, j, Idx;
	string s, ValName;
	TToken *Token;
	int BrNest = 0;

	if (TokPos + 1 >= TokenEndPos) {
		return OrgStr;
	}
	Token = (TToken*)TokenList->Items[TokPos + 1];
	if (LowerCase(Token->Str) != "omp") {
		return OrgStr; // #pragma omp 以外は対象外
	}
	if (NewValStrList->Count == 0) {
		return OrgStr;
	}
	s = "";
	for (j = TokPos; j < EndTokPos; j++) {
		Token = (TToken*)TokenList->Items[j];
		s += Token->OrgStr;
		if (Token->Str == "(") {
			BrNest++;
		}
		else if (Token->Str == ")") {
			BrNest--;
		}
		else if (Token->Str == ",") {;
		}
		else if (BrNest > 0) { // ()内。
			ValName = Token->Str;
			Idx = NewValStrList->IndexOf(ValName);
			if (Idx != -1) {
				for (i = 0; i < NewValStrList->Count; i += 2) {
					if (ValName == NewValStrList->Strings[i]) {
						s += "," + NewValStrList->Strings[i + 1]; // 変数多重化
					}
				}
			}
		}
	}
	return s + "\n";
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// ValData 検索対象のValData
//
// 3.概要
// ValDataがATExec()のスクリプトの引数に含まれているかを調べれる。
//
// 4.機能説明
//
// 5.戻り値
// 含まれていればtrueを返す。
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
bool TTuneRegion::IsATExecArg(void *ValData) {
	TStringList *ATExec_ArgValList;

	ATExec_ArgValList = MainF->Call_ATExec_ArgList;
	if (ATExec_ArgValList->IndexOf(((TValData*)ValData)->Str) != -1) {
		return true;
	}
	return false;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// LoopFusionに対応したDo出力を行う。Fortran用
//
// 4.機能説明
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputFusionDo_Fortran(string FusionValName, FILE *fp,
	int DoNest, int FusionIdx) {
	string s, s1, s2, s3, s4, s5;
	int k, k2, kk;
	TToken *Token;
	int TokCount;

	// fprintf(fp,"OutputFusionDo DoNest %d FusionIdx %d \n",DoNest,FusionIdx);
	// if(DoNest == FusionIdx){	// 外側のFusionDoVal
	// if(DoNest == 0){	// 外側のFusionDoVal
	if (DoNest == DoValCount - FusionIdx - 1) { // 外側のFusionDoVal
		s = "      DO ";
		s += FusionValName + " = 1 , ";
		for (k = DoNest; k < DoValCount; k++) {
			s2 = "";
			s3 = "";
			TokCount = 0;
			for (kk = DoEndSPos[k]; kk < DoEndEPos[k]; kk++) {
				Token = (TToken*)TokenList->Items[kk];
				s2 += Token->Str;
				TokCount++;
			}
			for (kk = DoStartSPos[k]; kk < DoStartEPos[k]; kk++) {
				Token = (TToken*)TokenList->Items[kk];
				s3 += Token->Str;
			}
			if (Trim(s3) != "1") {
				s4 = "(" + s2 + "-" + s3 + "+1)";
			}
			else {
				s4 = s2;
				if(TokCount != 1){
					s4 = "("+s4+")";
				}
			}
			if (k == DoNest) {
				s += s4;
			}
			else {
				s += "*" + s4;
			}
		}
		fprintf(fp, "%s\n", SepLongStr(s).c_str());
		// 元のDo変数をFusion用変数から作成する式を追加する。
		/*
		do k_j = 1, (NZ01-NZ00+1)*(NY01-NY00+1)
		k = (k_j-1)/(NY01-NY00+1) + NZ00
		j = mod((k_j-1),(NY01-NY00+1)) + NY00
		j = mod((k_j-1),(NY01-NY00+1)) + NY00
		 */
		/*
		k = (k_j-1)/(NY01-NY00+1) + NZ00

		 */
		for (k = DoNest; k < DoValCount; k++) {
			s = "      ";
			s += DoValName[k] + " = ";
			s1 = "(" + FusionValName + "-1)";
			// 割る数を求める。
			s2 = "";
			TokCount = 0;
			for (k2 = k + 1; k2 < DoValCount; k2++) {
				if (s2 != "") {
					s2 += "*";
				}
				s3 = "";
				s4 = "";
				for (kk = DoEndSPos[k2]; kk < DoEndEPos[k2]; kk++) {
					Token = (TToken*)TokenList->Items[kk];
					s3 += Token->Str;
					TokCount++;
				}
				for (kk = DoStartSPos[k2]; kk < DoStartEPos[k2]; kk++) {
					Token = (TToken*)TokenList->Items[kk];
					s4 += Token->Str;
				}
				if (Trim(s4) != "1") {
					s5 = "(" + s3 + "-" + s4 + "+1)";
				}
				else {
					s5 = s3;
					if(TokCount != 1){
						s5 = "("+s5+")";
					}
				}
				s2 += s5;
			}
			if (s2.find('*') != string::npos) {
				s2 = "(" + s2 + ")";
			}
			s3 = ""; // 余りの計算用
			if (k != 0) {
				k2 = k;
				s3 = "";
				s4 = "";
				for (kk = DoEndSPos[k2]; kk < DoEndEPos[k2]; kk++) {
					Token = (TToken*)TokenList->Items[kk];
					s3 += Token->Str;
				}
				for (kk = DoStartSPos[k2]; kk < DoStartEPos[k2]; kk++) {
					Token = (TToken*)TokenList->Items[kk];
					s4 += Token->Str;
				}
				if (Trim(s4) != "1") {
					s3 = "(" + s3 + "-" + s4 + "+1)";
				}
			}
			if (s2 != "") {
				s1 = s1 + "/" + s2;
			}
//			if (s3 != "") {
			if ((s3 != "")&&(DoValIdx != DoNest)){	// 一番外側以外は余りの計算が必要
				s1 = "mod(" + s1 + "," + s3 + ")";
			}
			s4 = "";
			for (kk = DoStartSPos[k]; kk < DoStartEPos[k]; kk++) {
				Token = (TToken*)TokenList->Items[kk];
				s4 += Token->Str;
			}
			s += s1 + " + " + s4;
			fprintf(fp, "%s\n", SepLongStr(s).c_str());
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// LoopFusionに対応したDo出力を行う。C用
//
// 4.機能説明
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TTuneRegion::OutputFusionDo_C(string FusionValName, FILE *fp, int DoNest,
	int FusionIdx) {
	string s, s1, s2, s3, s4, s5;
	int kk;
	TToken *Token;
	int AddValue; // 変数への加算数
	int i;
	int DoValIdx,DoValIdx2;
	int TokCount;

	// fprintf(fp,"OutputFusionDo DoNest %d FusionIdx %d \n",DoNest,FusionIdx);
	// if(DoNest == FusionIdx){	// 外側のFusionDoVal
	// if(DoNest == 0){	// 外側のFusionDoVal
	if (DoNest == DoValCount - FusionIdx - 1) { // 外側のFusionDoVal
		s = "      for(";
		s += FusionValName + " = 0 ; " + FusionValName + " < ";
		for (DoValIdx = DoNest; DoValIdx < DoValCount; DoValIdx++) {
			s2 = "";
			s3 = "";
			TokCount = 0;
			AddValue = 0;
			i = DoEndSPos[DoValIdx];
			Token = (TToken*)TokenList->Items[i++];	// Do変数部分
			Token = (TToken*)TokenList->Items[i++];	// < 部分
			if (Token->Str == "<=") {
				AddValue += 1; // 最後の数が＋１までならばLoop数も増える。
			}
			for (; i < DoEndEPos[DoValIdx]; i++) {
				Token = (TToken*)TokenList->Items[i];
				s2 += Token->Str;
				TokCount++;
			}
			if (AddValue != 0) {
				s2 += "(" + s2 + "+" + IntToStr(AddValue) + ")";
			}
			else if (DoEndSPos[DoValIdx] + 2 > DoEndEPos[DoValIdx]) {
				s2 += "(" + s2 + ")";
			}
			for (kk = DoStartSPos[DoValIdx]; kk < DoStartEPos[DoValIdx]; kk++) {
				Token = (TToken*)TokenList->Items[kk];
				s3 += Token->Str;
			}
			if (Trim(s3) != "0") {
				s4 = "(" + s2 + "-" + s3 + ")";
			}
			else {
				s4 = s2;
				if(TokCount != 1){
					s4 = "("+s4+")";
				}
			}
			if (DoValIdx == DoNest) {
				s += s4;
			}
			else {
				s += "*" + s4;
			}

		}
		s += " ; " + FusionValName + "++){";
		fprintf(fp, "%s\n", s.c_str());
		// 元のDo変数をFusion用変数から作成する式を追加する。
		/*
		do k_j = 1, (NZ01-NZ00+1)*(NY01-NY00+1)
		k = (k_j-1)/(NY01-NY00+1) + NZ00
		j = mod((k_j-1),(NY01-NY00+1)) + NY00
		j = mod((k_j-1),(NY01-NY00+1)) + NY00
		 */
		/*
		k = (k_j-1)/(NY01-NY00+1) + NZ00

		 */
		for (DoValIdx = DoNest; DoValIdx < DoValCount; DoValIdx++) {
			s = "      ";
			s += DoValName[DoValIdx] + " = ";
			s1 = FusionValName;
			// 割る数を求める。
			s2 = "";
			for (DoValIdx2 = DoValIdx + 1; DoValIdx2 < DoValCount; DoValIdx2++) {
				if (s2 != "") {
					s2 += "*";
				}
				s3 = "";
				s4 = "";
				TokCount = 0;
				AddValue = 0;
				i = DoEndSPos[DoValIdx2];
				Token = (TToken*)TokenList->Items[i++];	// Do変数部分
				Token = (TToken*)TokenList->Items[i++];	// < 部分
				if (Token->Str == "<=") {
					AddValue += 1; // 最後の数が＋１までならばLoop数も増える。
				}
				for (; i < DoEndEPos[DoValIdx2]; i++) {
					Token = (TToken*)TokenList->Items[i];
					s3 += Token->Str;
					TokCount++;
				}
				if (AddValue != 0) {
					s3 += "(" + s3 + "+" + IntToStr(AddValue) + ")";
				}
				else if (DoEndSPos[DoValIdx2] + 2 > DoEndEPos[DoValIdx2]) {
					s3 += "(" + s3 + ")";
				}
				for (kk = DoStartSPos[DoValIdx2]; kk < DoStartEPos[DoValIdx2]; kk++) {
					Token = (TToken*)TokenList->Items[kk];
					s4 += Token->Str;
				}
				if (Trim(s4) != "0") {
					s5 = "(" + s3 + "-" + s4 + ")";
				}
				else {
					s5 = s3;
					if(TokCount != 1){
						s5 = "("+s5+")";
					}
				}
				s2 += s5;
			}
			if (s2.find('*') != string::npos) {
				s2 = "(" + s2 + ")";
			}
			s3 = ""; // 余りの計算用
			TokCount = 0;
			if (DoValIdx != 0) {
				DoValIdx2 = DoValIdx;
				s3 = "";
				s4 = "";
				AddValue = 0;
				i = DoEndSPos[DoValIdx2];
				Token = (TToken*)TokenList->Items[i++];	// Do変数部分
				Token = (TToken*)TokenList->Items[i++];	// < 部分
				if (Token->Str == "<=") {
					AddValue += 1; // 最後の数が＋１までならばLoop数も増える。
				}
				for (; i < DoEndEPos[DoValIdx2]; i++) {
					Token = (TToken*)TokenList->Items[i];
					s3 += Token->Str;
					TokCount++;
				}
				if (AddValue != 0) {
					s3 += "(" + s3 + "+" + IntToStr(AddValue) + ")";
				}
				else if (DoEndSPos[DoValIdx2] + 2 > DoEndEPos[DoValIdx2]) {
					s3 += "(" + s3 + ")";
				}
				for (kk = DoStartSPos[DoValIdx2]; kk < DoStartEPos[DoValIdx2]; kk++) {
					Token = (TToken*)TokenList->Items[kk];
					s4 += Token->Str;
				}
				if (Trim(s4) != "0") {
					s3 = "(" + s3 + "-" + s4 + ")";
				}else{
					if(TokCount != 1){
						s3 = "("+s3+")";
					}
				}
			}
			if (s2 != "") {
				s1 = s1 + "/" + s2;
			}
//			if (s3 != "") {
			if ((s3 != "")&&(DoValIdx != DoNest)){	// 一番外側以外は余りの計算が必要
				s1 = s1 + "%" + s3;
			}
			s4 = "";
			for (kk = DoStartSPos[DoValIdx]; kk < DoStartEPos[DoValIdx]; kk++) {
				Token = (TToken*)TokenList->Items[kk];
				s4 += Token->Str;
			}
			if(Trim(s4) != "0"){
				s += s1 + " + " + s4;
			}else{
				s += s1;
			}
			fprintf(fp, "%s;\n", s.c_str());
		}
	}
}
