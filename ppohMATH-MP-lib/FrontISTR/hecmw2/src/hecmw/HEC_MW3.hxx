/*
 ----------------------------------------------------------
|
| Software Name :HEC-MW Ver 4.2 beta
|
|   ./src/HEC_MW3.hxx
|
|                     Written by T.Takeda,    2013/03/26
|                                Y.Sato,      2013/03/26
|                                K.Goto,      2010/01/12
|                                K.Matsubara, 2010/06/01
|
|   Contact address : IIS, The University of Tokyo CISS
|
 ----------------------------------------------------------
*/
#include "TypeDef.h"
#include "HEC_MPI.h"
#include "FileIO.h"
#include "Logger.h"
#include "SolutionType.h"
#include "CodeType.h"
#include "MeshFactory.h"
#include "GMGModel.h"
#include "AssyMatrix.h"
#include "ShapeHexa.h"
#include "ShapeHexaNic.h"
#include "ShapeTetra.h"
#include "ShapePrism.h"
#include "ShapeQuad.h"
#include "ShapeTriangle.h"
#include "ShapeLine.h"
#include "ShapeFunctionCatalog.h"
#include "ISTR2Edge.h"
#include "Edge2ISTR.h"
#include "Jacobian.h"

#ifdef REVOCAP_REFINE
#include "rcapRefiner.h"
#include "rcapRefinerMacros.h"
#endif

#include <cstdio>  // printf
#include <cstdlib> // calloc

#include <cstdarg>
#include <iomanip>
#include <cstring>
typedef pair<uiint,uiint> integPair;

namespace pmw{
#ifndef PMW_MAIN_HH_C8955A70_0EE9_4f3e_82D1_FB32C9626513
#define PMW_MAIN_HH_C8955A70_0EE9_4f3e_82D1_FB32C9626513
class CMW
{
public:
    __declspec(dllexport) static CMW*  Instance(){
	static CMW moMW;
	return &moMW;
    }
private:
    __declspec(dllexport) CMW();
public:
    __declspec(dllexport) virtual ~CMW();
protected:
    FileIO::CFileIO   *mpFileIO;
    Utility::CLogger  *mpLogger;

    uiint mnSolutionType;//----

    bool mb_file;

    string msInputFileName;  
    string msOutputFileName; 
    string msResFileName;    
    string msRltFileName;    
    string msPartOutFileName;

    CGMGModel     *mpGMGModel;
    CMeshFactory  *mpFactory;
    CHecMPI       *mpMPI;

    CAssyModel  *mpAssy;
    CAssyMatrix *mpAssyMatrix;
    CAssyVector *mpRHSAssyVector;
    CAssyVector *mpSolAssyVector;

    CMesh       *mpMesh;
    CElement    *mpElement;
    CCommMesh2  *mpComMesh;
    CContactMesh *mpConMesh;

    CShapeHexa     *mpShapeHexa;
    CShapeHexaNic  *mpShapeHexaNic;
    CShapeTetra    *mpShapeTetra;
    CShapePrism    *mpShapePrism;
    CShapeQuad     *mpShapeQuad;
    CShapeTriangle *mpShapeTriangle;
    CShapeLine     *mpShapeLine;

    CShapeFunctionCatalog *mpShapeCatalog;

    CISTR2Edge *mpISTR2Edge;
    CEdge2ISTR *mpEdge2ISTR;

    vvvdouble mvdNdx;

    vuint mv_ItemL;
    vuint mv_ItemU;

    uiint BaseName_BCast(uiint& nLength, string& sName, uiint nType);

    bool mbRefine_Use;//--------- リファイン使用判定

    // API_Fortran 定数
    MPI_Datatype mnMPI_UIINT;
    MPI_Datatype mnMPI_IINT;
    int  mnMyRank;
    int  mnNumOfProcess;

public:
    __declspec(dllexport) uiint Initialize( int argc, char** argv); 
    __declspec(dllexport) uiint Initialize_fstr(int argc, char** argv, string& ctrlname);
    __declspec(dllexport) uiint Finalize();  
    __declspec(dllexport) void  Banner_Display();


#ifdef REVOCAP_REFINE //==========================================REVOCAP_REFINE
    //----
    // REVOCAP_Refiner
    //----
    __declspec(dllexport) uiint RevocapRefine(string& filename, const uiint& nRefine);
private:
    void Rcap_BndParamSetup(uiint nElemType, uiint& nNumOfVol,vuint& vBndID, vstring& vBndName, vuint& vBndBType,
        vuint& vBndDOF, vvuint& vvBndDOF, vuint& vBndType, vuint& vBndCount, vuint& vBndTypeSeq, uiint& nSeq,
        CBoundaryVolumeMesh *pVolMesh);//境界条件管理変数セットアップ:BndVolume
    
    void Rcap_CrsElemNodes(size_t nNNode, vector<CElement*> vElem, CIndexBucket *pBucket, int32_t *elemNodes);

    void Rcap_NodeGene(uiint iLevel, uiint nMaxLevel, size_t crsNumOfNode, size_t refineNodeCount, float64_t* resultCoord, CMesh *pCrsMesh, CMesh *pProgMesh);
    void Rcap_ElemGene(int8_t nType, uiint& nIDCount, size_t refineCount, int32_t *refineNodes, CMesh *pProgMesh);

    void Rcap_SetBFaceMesh(uiint iLevel, uiint nMaxLevel, vuint vBndID, vuint vBndBType, vstring vBndName, vuint vNumDOF, vvuint vvDOF, CMesh *pProgMesh, CMesh *pMesh);
    void Rcap_SetBEdgeMesh(uiint iLevel, uiint nMaxLevel, vuint vBndID, vuint vBndBType, vstring vBndName, vuint vNumDOF, vvuint vvDOF, CMesh *pProgMesh, CMesh *pMesh);
    void Rcap_SetBVolMesh(uiint iLevel, uiint nMaxLevel, vuint vBndID, vuint vBndBType, vstring vBndName, vuint vNumDOF, vvuint vvDOF, CMesh *pProgMesh, CMesh *pMesh);

    vvuint Rcap_NodeIX_SortMerge(size_t nNumOfID, size_t *nEntityCount, size_t nNumOfEntityNode, int32_t **refineNodes);
    void Rcap_BNodeGene_VolMesh(uiint iLevel, uiint nMaxLevel, vuint vBndID, CMesh *pMesh, CMesh *pProgMesh, vvuint& vvNodeIndex);
    void Rcap_BNodeGene_FaceMesh(uiint iLevel, uiint nMaxLevel, vuint vBndID, CMesh *pMesh, CMesh *pProgMesh, vvuint& vNodeIndex);
    void Rcap_BNodeGene_EdgeMesh(uiint iLevel, uiint nMaxLevel, vuint vBndID, CMesh *pMesh, CMesh *pProgMesh, vvuint& vNodeIndex);
    void Rcap_NodeNum2Index(vector<map<uiint, uiint> >& vmNodeNum2Index, vvuint& NodeIndex);//逆引き生成
    
    void Rcap_BVolGene(uiint iLevel, vuint& vBndID, vector<map<uiint, uiint> >& vmID2Index, size_t *nVolCount, uiint nShape, size_t& nNumOfVolNode,
                        int32_t **refineNodes, CMesh *pProgMesh, CMesh *pCrsMesh);
    void Rcap_BFaceGene(uiint iLevel, vuint& vBndID, vector<map<uiint, uiint> >& vmID2Index, size_t *nFaceCount, uiint nShape, size_t& nNumOfFaceNode,
                        int32_t **refineNodes, CMesh *pProgMesh, CMesh *pCrsMesh);
    void Rcap_BEdgeGene(uiint iLevel, vuint& vBndID, vector<map<uiint, uiint> >& vmID2Index, size_t *nEdgeCount, uiint nShape, size_t& nNumOfEdgeNode,
                        int32_t **refineNodes, CMesh *pProgMesh, CMesh *pCrsMesh);

    void Rcap_ElemSearch(uiint& nElementID, vector<CNode*>& vNode, CMesh *pProgMesh);
    void Rcap_ElemFaceSearch(uiint& nElementID, uiint& nFaceID, vector<CNode*>& vNode, CMesh *pProgMesh);
    void Rcap_ElemEdgeSearch(uiint& nElementID, uiint& nEdgeID, vector<CNode*>& vNode, CMesh *pProgMesh);
    void Rcap_ElemPointSearch(uiint& nElementID, uiint& nLocalID, CNode* pNode, CMesh *pProgMesh);

    void Rcap_BNodeValueDist(uiint iLevel, CBoundaryVolumeMesh *pBVolMesh, vdouble& vVal, vuint& vBNodeIndex, map<uiint,uiint>& NodeNum2BNodeNum, uiint nNumOfEntityNode, int32_t *crsNodes);// Dirichlet値 分配
    void Rcap_BNodeValueDist(uiint iLevel, CBoundaryFaceMesh *pBFaceMesh, vdouble& vVal, vuint& vBNodeIndex, map<uiint,uiint>& NodeNum2BNodeNum, uiint nNumOfEntityNode, int32_t *crsNodes);//Dirichlet値 分配
    void Rcap_BNodeValueDist(uiint iLevel, CBoundaryEdgeMesh *pBEdgeMesh, vdouble& vVal, vuint& vBNodeIndex, map<uiint,uiint>& NodeNum2BNodeNum, uiint nNumOfEntityNode, int32_t *crsNodes);// Dirichlet値 分配
    void Rcap_EquivalentNodalForce(uiint iLevel, uiint dof, CBoundaryFaceMesh *pBFaceMesh, CBoundaryFace *pBFace);
    void Rcap_EquivalentNodalForce(uiint iLevel, uiint dof, CBoundaryEdgeMesh *pBEdgeMesh, CBoundaryEdge *pBEdge);
    void Rcap_EquivalentNodalForce(uiint iLevel, uiint dof, CBoundaryVolumeMesh *pBVolMesh, CBoundaryVolume *pBVol);

    void Rcap_BFaceMeshDebug(vuint& vBndID, CMesh *pProgMesh);

    //
    // Comm
    //
    void Rcap_SetCommMesh2(uiint iLevel, map<uiint,uiint> mComID, vvuint& vvNum4EachType, CMesh *pCrsMesh, CMesh *pProgMesh);
    void Rcap_NodesPoint2vuint(map<uiint,uiint>& mComID, uiint nNumOfFaceNode, size_t *nFaceCount, int32_t **refineNodes, vvuint& vvRefineNodes);
    
    // Comm:形状別リファイン・ノード配列を一つにまとめる
    void Rcap_SumRefineNodes( vvuint& vvRefineNodes, vvuint& vvQuad, vvuint& vvQuad2, vvuint& vvTri, vvuint& vvTri2, vvuint& vvBeam, vvuint& vvBeam2, vvuint& vvPoint);
    void Rcap_CommFaceCount( vvuint& vvNum4EachType, map<uiint,uiint> mComID, size_t* commQuadCount, size_t* commQuad2Count,
                              size_t* commTriCount, size_t* commTri2Count, size_t* commBeamCount, size_t* commBeam2Count,
                              size_t* commPointCount);
    void Rcap_CommNodeGene( map<uiint,uiint> mComID, vuint& vvNum4EachType, vvvuint& vvvElemNum, vector<map<uiint, vuint> >& vmaElemNIndex,
                            CMesh *pProgMesh, vvuint& vvRefineNodes, vector<map<uiint, uiint> >& maNNum2CommNNum);
    void Rcap_CommFaceGene( uiint iLevel, map<uiint,uiint>& mComID, vvvuint& vvvElemNum, vector<map<uiint, vuint> >& vmaElemNIndex,
                            vvuint& vvRefineNodes, CMesh *pProgMesh, vector<map<uiint, uiint> >& maNNum2CommNNum );

#endif  //========================================================REVOCAP_REFINE
private:
    void SortMerge(vuint& vec);

public:
    __declspec(dllexport) void clearMW3(const uiint& mgLevel);


    __declspec(dllexport) uiint FileRead(string& basename, bool bBinary);
    __declspec(dllexport) uiint FileRead_fstr(bool bBinary);             
    __declspec(dllexport) uiint FileDebugWrite();

    __declspec(dllexport) uiint FileWriteRes(const uiint& nStep, bool bBinary);
    __declspec(dllexport) uiint SetRestart(const uiint& nStep, uiint& nAppNumOfLevel, uiint& nAppNumEquation, bool bBinary);
    
    __declspec(dllexport) void PrintRlt_Start(const uiint& nStep, bool bBinary);
    __declspec(dllexport) void PrintRlt(const uiint& width, const char* format, ... );
    __declspec(dllexport) void PrintRlt_End();
    
    __declspec(dllexport) void PrintMicroAVS_Basis(const uiint& ieq);
    __declspec(dllexport) void recAVS_Label(const uiint& iMesh, char* cLabel, char* cUnit, const uiint& nNumOfDOF);
    __declspec(dllexport) void recAVS_Variable(const uiint& iMesh, const uiint& nNumOfNode, char* cLabel, double* pvValue);
    __declspec(dllexport) void PrintMicroAVS_FEM();

    __declspec(dllexport) void recVTK_Label(const uiint& iMesh, char* cLabel, char* cUnit, const uiint& nNumOfDOF);
    __declspec(dllexport) void recVTK_Variable(const uiint& iMesh, const uiint& nNumOfNode, char* cLabel, double* pvValue);
    __declspec(dllexport) void PrintVTK_FEM();

    __declspec(dllexport) void recUNS_Label(const uiint& iMesh, char* cLabel, char* cUnit, const uiint& nNumOfDOF);
    __declspec(dllexport) void recUNS_Variable(const uiint& iMesh, const uiint& nNumOfNode, char* cLabel, double* pvValue);
    __declspec(dllexport) void PrintUNS_FEM();


    __declspec(dllexport) string getFstr_FileName_Mesh();
    __declspec(dllexport) string getFstr_FileName_Control();
    __declspec(dllexport) string getFstr_FileName_Result();
    __declspec(dllexport) string getFstr_FileName_Restart();
    __declspec(dllexport) string getFstr_FileName_PartIn();
    __declspec(dllexport) string getFstr_FileName_PartOut();
    __declspec(dllexport) string getFstr_FileName_VisMesh();
    __declspec(dllexport) string getFstr_FileName_VisIn();
    __declspec(dllexport) string getFstr_FileName_VisOut();
    __declspec(dllexport) string getFstr_RefineCADFitName();
    __declspec(dllexport) uiint  getFstr_RefineNum();
    __declspec(dllexport) uiint  getFstr_RefineType();

    __declspec(dllexport) void GeneLinearAlgebra(const uiint& nNumOfAlgebra, const uiint& nGlobalNumOfMesh, uiint** vvDOF, double** vvTransCoeff);
    __declspec(dllexport) void GeneLinearAlgebra(const uiint& nNumOfAlgebra, const uiint& nGlobalNumOfMesh, uiint** vvDOF);
    __declspec(dllexport) void SelectAlgebra(const uiint& iequ);

    __declspec(dllexport) uiint Matrix_Add_Elem(const uiint& iMesh, const uiint& iElem, double *ElemMatrix);
    __declspec(dllexport) uiint Matrix_Add_Node(const uiint& iMesh, const uiint& inode, const uiint& jnode, double *NodalMatrix);
    __declspec(dllexport) void Matrix_Clear(const uiint& iMesh);
    __declspec(dllexport) void Vector_Clear(const uiint& iMesh);
    __declspec(dllexport) void AssyMatrix_Clear();//----------------------------------
    __declspec(dllexport) void AssyVector_Clear();//----------------------------------
    __declspec(dllexport) uiint Set_BC_Mat_RHS2(uiint& iMesh, uiint& iNodeID, uiint& nDOF, double& diag_value, double& sol_value);
    __declspec(dllexport) uiint Set_BC_Mat_RHS(uiint& iMesh, uiint& iNodeID, uiint& nDOF, double& diag_value, double& rhs_value); 

    __declspec(dllexport) uiint Set_BC_RHS(uiint& iMesh, uiint& iNodeID, uiint& nDOF, double& value);
    __declspec(dllexport) uiint Add_BC_RHS(uiint& iMesh, uiint& iNodeID, uiint& nDOF, double& value);
    __declspec(dllexport) uiint Set_BC_NL_RHS(uiint& iMesh, uiint& iNodeID, uiint& nDOF, double& value);//---- 節点集中荷重として扱う:Rank大には値は入らない.
    __declspec(dllexport) uiint Add_BC_NL_RHS(uiint& iMesh, uiint& iNodeID, uiint& nDOF, double& value);//---- 節点集中荷重として扱う:Rank大には値は入らない.

    __declspec(dllexport) uiint Solve(iint& iter_max, double& tolerance, iint& method, iint& precondition);

    __declspec(dllexport) void GetSolution_Vector(double* buf, const uiint& imesh);
    __declspec(dllexport) void GetSolution_AssyVector(double* buf);
    __declspec(dllexport) void GetRHS_Vector(double* buf, const uiint& imesh);
    __declspec(dllexport) void GetRHS_AssyVector(double* buf);
    __declspec(dllexport) void GetRHS_Load(double* buf, const uiint& imesh);//--- 非線形構造解析の残差力:右辺ベクトルをsumupしたベクトル
    __declspec(dllexport) void GetRHS_AssyLoad(double* buf);//------------------- 非線形構造解析の残差力:右辺ベクトルをsumupしたベクトル
    __declspec(dllexport) double& GetSolutionVector_Val(const uiint& imesh, const uiint& inode, const uiint& idof);
    __declspec(dllexport) double& GetRHSVector_Val(const uiint& imesh, const uiint& inode, const uiint& idof);
    __declspec(dllexport) uiint& GetSolutionVector_DOF(const uiint& imesh);
    __declspec(dllexport) uiint& GetRHSVector_DOF(const uiint& imesh);
    __declspec(dllexport) uiint& GetSolutionVector_DOF(const uiint& iLevel, const uiint& imesh, const uiint& ieq);
    __declspec(dllexport) uiint& GetRHSVector_DOF(const uiint& iLevel, const uiint& imesh, const uiint& ieq);

    __declspec(dllexport) void dump_AssyMatrix();//debug用 行列ダンプ(CRT表示)
    __declspec(dllexport) void dump_RHSAssyVector();//debug用 行列ダンプ(CRT表示)


private:
////    __declspec(dllexport) void Update_Add(const uiint& iLevel, const uiint& imesh, double* vec, const uiint& nDOF);
    void Update( const uiint& iLevel, const uiint& imesh, double* vec, const uiint& nDOF );
    void Sumup( const uiint& iLevel, const uiint& imesh, double* vec, const uiint& nDOF );
////    __declspec(dllexport) void Update_Average(const uiint& iLevel, const uiint& imesh, double* vec, const uiint& nDOF);
////
////    __declspec(dllexport) void Update_Assy_Add(const uiint& iLevel, double* assy_vec, uiint* vDOF);
////    __declspec(dllexport) void Update_Assy(const uiint& iLevel, double* assy_vec, uiint* vDOF);
////    __declspec(dllexport) void Sumup_Assy(const uiint& iLevel, double* assy_vec, uiint* vDOF);
////    __declspec(dllexport) void Update_Assy_Average(const uiint& iLevel, double* assy_vec, uiint* vDOF);


public:
    // __declspec(dllexport) void MatVec(const uiint& iLevel, const uiint& imesh, double* mat, double* x, double* y, const uiint& nDOF);//汎用版
    __declspec(dllexport) void MatVec_Assy(const uiint& iLevel, const uiint& ieq, double* assy_x, double* assy_y);     //AssyMatrix版 Ax=y : AssyMatrix::multVector
    __declspec(dllexport) void MatVec(const uiint& iLevel, const uiint& imesh, const uiint& ieq, double* x, double* y);//MatrixBCRS版 Ax=y : xベクトルのupdate後、行列ベクトル演算、演算後にyベクトルをsumup

    

    __declspec(dllexport) uiint Refine(const uiint& nNumOfRefine);


    __declspec(dllexport) uiint GetNumOfAssembleModel();
    __declspec(dllexport) void SelectAssembleModel(const uiint& mgLevel);
    __declspec(dllexport) uiint GetNumOfMeshPart();
    __declspec(dllexport) void SelectMeshPart_ID(const uiint& mesh_id);
    __declspec(dllexport) void SelectMeshPart_IX(const uiint& index);
    __declspec(dllexport) uiint GetMeshID_Num(const uiint& index);//---------- Mesh 通し番号(Index)から、ID
    __declspec(dllexport) uiint GetMeshIndex_Num(const uiint& id);//---------- Mesh ID番号から、Index

    __declspec(dllexport) void SelectElement_ID(const uiint& elem_id);
    __declspec(dllexport) void SelectElement_IX(const uiint& index);
    __declspec(dllexport) uiint GetElementType();
    __declspec(dllexport) uiint GetNumOfElementVert();
    __declspec(dllexport) void GetElementVertNodeID(iint* vNodeID);
    __declspec(dllexport) void GetElementVertNodeIndex(iint* vNodeIndex);//----------------------------------11.11.01
    __declspec(dllexport) uiint GetNumOfElementEdge();
    __declspec(dllexport) uiint GetNumOfElementFace();
    __declspec(dllexport) void GetElementEdgeNodeID(iint* vNodeID);
    __declspec(dllexport) uiint GetElementFaceElementID(uiint faceIndex);
    __declspec(dllexport) uiint GetNumOfElementEdgeElement(uiint edgeIndex);
    __declspec(dllexport) uiint GetElementEdgeElementID(uiint edgeIndex, uiint i);
    __declspec(dllexport) void GetNodeCoord(const uiint& node_id, double& x, double& y, double& z);
    __declspec(dllexport) uiint GetNumOfDOF(const uiint& node_id);
    __declspec(dllexport) uiint GetNumOfScalar(const uiint& node_id);
    __declspec(dllexport) uiint GetNumOfVector(const uiint& node_id);
    __declspec(dllexport) uiint& GetNodeType(const uiint& node_id);
    __declspec(dllexport) uiint  getNodeSize();
    __declspec(dllexport) uiint  getElementSize();
    __declspec(dllexport) uiint  getNodeSize(uiint iMesh);
    __declspec(dllexport) uiint  getElementSize(uiint iMesh);
    __declspec(dllexport) uiint& getNodeID(const uiint& index);
    __declspec(dllexport) uiint& getElementID(const uiint& index);
    __declspec(dllexport) uiint& getNodeIndex(const uiint& id);   
    __declspec(dllexport) uiint& getElementIndex(const uiint& id);

    __declspec(dllexport) uiint  getNumOfParentNode(const uiint& id, const uiint& nLevel);
    __declspec(dllexport) uiint  getParentNodeID(const uiint& id, const uiint& nLevel, const uiint& index);

    __declspec(dllexport) void constructNodeConnectFEM(const uiint& node_id);
    __declspec(dllexport) void getNodeConnectFEM_Size(uiint& nNumOfItemU, uiint& nNumOfItemL);
    __declspec(dllexport) void getNodeConnectFEM_Item(uiint itemU[], uiint itemL[]);
    __declspec(dllexport) void getNodeConnectFEM_Item_F(iint itemU[], iint itemL[]);
    __declspec(dllexport) uiint getNumOfAggregateElement(const uiint& node_id);
    __declspec(dllexport) uiint& getAggregateElementID(const uiint& node_id, const uiint& ielem);

    __declspec(dllexport) void setupNeighbors();

    __declspec(dllexport) void FinalizeRefine();
    
    __declspec(dllexport) uiint nodetype_s();
    __declspec(dllexport) uiint nodetype_v();
    __declspec(dllexport) uiint nodetype_sv();

    __declspec(dllexport) uiint elemtype_hexa();
    __declspec(dllexport) uiint elemtype_hexa2();
    __declspec(dllexport) uiint elemtype_tetra();
    __declspec(dllexport) uiint elemtype_tetra2();
    __declspec(dllexport) uiint elemtype_prism();
    __declspec(dllexport) uiint elemtype_prism2();
    __declspec(dllexport) uiint elemtype_quad();
    __declspec(dllexport) uiint elemtype_quad2();
    __declspec(dllexport) uiint elemtype_triangle();
    __declspec(dllexport) uiint elemtype_triangle2();
    __declspec(dllexport) uiint elemtype_line();
    __declspec(dllexport) uiint elemtype_line2();

    __declspec(dllexport) uiint fistr_elemtype_hexa();
    __declspec(dllexport) uiint fistr_elemtype_hexa2();
    __declspec(dllexport) uiint fistr_elemtype_tetra();
    __declspec(dllexport) uiint fistr_elemtype_tetra2();
    __declspec(dllexport) uiint fistr_elemtype_prism();
    __declspec(dllexport) uiint fistr_elemtype_prism2();
    __declspec(dllexport) uiint fistr_elemtype_quad();
    __declspec(dllexport) uiint fistr_elemtype_quad2();
    __declspec(dllexport) uiint fistr_elemtype_triangle();
    __declspec(dllexport) uiint fistr_elemtype_triangle2();
    __declspec(dllexport) uiint fistr_elemtype_line();
    __declspec(dllexport) uiint fistr_elemtype_line2();

    __declspec(dllexport) uiint fistr_elemtype_to_mw3_elemtype(const uiint& fistr_elemtype);
    __declspec(dllexport) uiint mw3_elemtype_to_fistr_elemtype(const uiint& mw3_elemtype);

    __declspec(dllexport) uiint& NumOfIntegPoint(const uiint& shapeType);
    __declspec(dllexport) void ShapeFunc_on_pt(const uiint& shapeType, const uiint& igauss, vdouble& N);
    __declspec(dllexport) void ShapeFunc_on_pt(uiint shapeType, uiint igauss, double N[]);
    __declspec(dllexport) void ShapeFunc(const uiint& shapeType, vvdouble& N);
    __declspec(dllexport) double& ShapeFunc_Hexa81(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Hexa82(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Hexa201(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Hexa202(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Hexa203(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Tetra41(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Tetra101(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Tetra104(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Tetra1015(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Prism62(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Prism156(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Prism159(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Prism1518(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Quad41(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Quad84(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Quad89(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Triangle31(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Triangle63(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Line21(uiint igauss, uiint ishape);
    __declspec(dllexport) double& ShapeFunc_Line32(uiint igauss, uiint ishape);

    __declspec(dllexport) void dNdr_on_pt(const uiint& shapeType, const uiint& igauss, vvdouble& dNdr);
    __declspec(dllexport) void dNdr(const uiint& shapeType, vvvdouble& dNdr);
    __declspec(dllexport) void dNdr(const uiint& shapeType, double dNdr[]);

    __declspec(dllexport) double& dNdr_Hexa81_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Hexa82_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Hexa201_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Hexa202_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Hexa203_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Tetra41_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Tetra101_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Tetra104_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Tetra1015_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Prism62_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Prism156_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Prism159_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Prism1518_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Quad41_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Quad84_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Quad89_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Tri31_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Tri63_on_pt_on_shape(uiint igauss, uiint ishape, uiint iaxis);
    __declspec(dllexport) double& dNdr_Line21_on_pt_on_shape(uiint igauss, uiint ishape);
    __declspec(dllexport) double& dNdr_Line32_on_pt_on_shape(uiint igauss, uiint ishape);

    __declspec(dllexport) void Calculate_dNdx(const uiint& elemType, const uiint& numOfInteg, const uiint& elem_index);

    __declspec(dllexport) void dNdx_on_pt(const uiint& igauss, vvdouble& dNdx);
    __declspec(dllexport) void dNdx(const uiint& elemType, const uiint& numOfInteg, const uiint& elem_index, vvvdouble& dNdx);
    __declspec(dllexport) void dNdx(const uiint& elemType, const uiint& numOfInteg, const uiint& ielem, double dNdx[]);

    __declspec(dllexport) void detJacobian(const uiint& elemType, const uiint& numOfInteg, const uiint& igauss, double& detJ);
    __declspec(dllexport) void Weight(const uiint& elemType, const uiint& numOfInteg, const uiint& igauss, double& w);

    __declspec(dllexport) uiint shapetype_hexa81();
    __declspec(dllexport) uiint shapetype_hexa82();
    __declspec(dllexport) uiint shapetype_hexa201();
    __declspec(dllexport) uiint shapetype_hexa202();
    __declspec(dllexport) uiint shapetype_hexa203();
    __declspec(dllexport) uiint shapetype_tetra41();
    __declspec(dllexport) uiint shapetype_tetra101();
    __declspec(dllexport) uiint shapetype_tetra104();
    __declspec(dllexport) uiint shapetype_tetra1015();
    __declspec(dllexport) uiint shapetype_prism62();
    __declspec(dllexport) uiint shapetype_prism156();
    __declspec(dllexport) uiint shapetype_prism159();
    __declspec(dllexport) uiint shapetype_prism1518();
    __declspec(dllexport) uiint shapetype_quad41();
    __declspec(dllexport) uiint shapetype_quad84();
    __declspec(dllexport) uiint shapetype_quad89();
    __declspec(dllexport) uiint shapetype_tri31();
    __declspec(dllexport) uiint shapetype_tri63();
    __declspec(dllexport) uiint shapetype_line21();
    __declspec(dllexport) uiint shapetype_line32();

    __declspec(dllexport) uiint GetNumOfBoundaryNodeMesh();
    __declspec(dllexport) uiint GetNumOfBoundaryFaceMesh();
    __declspec(dllexport) uiint GetNumOfBoundaryEdgeMesh();
    __declspec(dllexport) uiint GetNumOfBoundaryVolumeMesh();
    __declspec(dllexport) uiint GetBNDType_BNodeMesh(const uiint& ibmesh);
    __declspec(dllexport) uiint GetBNDType_BFaceMesh(const uiint& ibmesh);
    __declspec(dllexport) uiint GetBNDType_BEdgeMesh(const uiint& ibmesh);
    __declspec(dllexport) uiint GetBNDType_BVolumeMesh(const uiint& ibmesh);
    __declspec(dllexport) uiint getNeumannType();
    __declspec(dllexport) uiint getDirichletType();
    __declspec(dllexport) uiint GetNumOfBNode_BNodeMesh(const uiint& ibmesh);
    __declspec(dllexport) uiint GetNumOfBNode_BFaceMesh(const uiint& ibmesh);
    __declspec(dllexport) uiint GetNumOfBNode_BEdgeMesh(const uiint& ibmesh);
    __declspec(dllexport) uiint GetNumOfBNode_BVolumeMesh(const uiint& ibmesh);
    __declspec(dllexport) uiint GetNumOfDOF_BNodeMesh(const uiint& ibmesh, const uiint& ibnode);
    __declspec(dllexport) uiint GetNumOfDOF_BFaceMesh(const uiint& ibmesh);
    __declspec(dllexport) uiint GetNumOfDOF_BEdgeMesh(const uiint& ibmesh);
    __declspec(dllexport) uiint GetNumOfDOF_BVolumeMesh(const uiint& ibmesh);
    __declspec(dllexport) uiint GetDOF_BNodeMesh(const uiint& ibmesh, const uiint& ibnode, const uiint& idof);
    __declspec(dllexport) uiint GetDOF_BFaceMesh(const uiint& ibmesh, const uiint& idof);
    __declspec(dllexport) uiint GetDOF_BEdgeMesh(const uiint& ibmesh, const uiint& idof);
    __declspec(dllexport) uiint GetDOF_BVolumeMesh(const uiint& ibmesh, const uiint& idof);
    __declspec(dllexport) double& GetBNodeValue_BNodeMesh(const uiint& ibmesh, const uiint& ibnode, const uiint& dof);
    __declspec(dllexport) double& GetBNodeValue_BFaceMesh(const uiint& ibmesh, const uiint& ibnode, const uiint& dof, const uiint& mgLevel);
    __declspec(dllexport) double& GetBNodeValue_BEdgeMesh(const uiint& ibmesh, const uiint& ibnode, const uiint& dof, const uiint& mgLevel);
    __declspec(dllexport) double& GetBNodeValue_BVolumeMesh(const uiint& ibmesh, const uiint& ibnode, const uiint& dof, const uiint& mgLevel);
    __declspec(dllexport) uiint& GetNodeID_BNode_BNodeMesh(const uiint& ibmesh, const uiint& ibnode);
    __declspec(dllexport) uiint& GetNodeID_BNode_BFaceMesh(const uiint& ibmesh, const uiint& ibnode);
    __declspec(dllexport) uiint& GetNodeID_BNode_BEdgeMesh(const uiint& ibmesh, const uiint& ibnode);
    __declspec(dllexport) uiint& GetNodeID_BNode_BVolumeMesh(const uiint& ibmesh, const uiint& ibnode);
    __declspec(dllexport) uiint GetNumOfBFace(const uiint& ibmesh);
    __declspec(dllexport) double& GetBFaceValue(const uiint& ibmesh, const uiint& ibface, const uiint& dof);
    __declspec(dllexport) uiint GetNumOfBEdge(const uiint& ibmesh);
    __declspec(dllexport) double& GetBEdgeValue(const uiint& ibmesh, const uiint& ibedge, const uiint& dof);
    __declspec(dllexport) uiint GetNumOfBVolume(const uiint& ibmesh);
    __declspec(dllexport) double& GetBVolumeValue(const uiint& ibmesh, const uiint& ibvol, const uiint& dof);
    __declspec(dllexport) uiint GetNumOfNode_BFace(const uiint& ibmesh, const uiint& ibface);
    __declspec(dllexport) uiint& GetNodeID_BFace(const uiint& ibmesh, const uiint& ibface, const uiint& ibnode);
    __declspec(dllexport) uiint GetNumOfNode_BEdge(const uiint& ibmesh, const uiint& ibedge);
    __declspec(dllexport) uiint& GetNodeID_BEdge(const uiint& ibmesh, const uiint& ibedge, const uiint& ibnode);
    __declspec(dllexport) uiint GetNumOfNode_BVolume(const uiint& ibmesh, const uiint& ibvol);
    __declspec(dllexport) uiint& GetNodeID_BVolume(const uiint& ibmesh, const uiint& ibvol, const uiint& ibnode);
    __declspec(dllexport) uiint GetBNodeMesh_NameLength(const uiint& ibmesh);
    __declspec(dllexport) string& GetBNodeMesh_Name(const uiint& ibmesh);
    __declspec(dllexport) uiint GetBFaceMesh_NameLength(const uiint& ibmesh);
    __declspec(dllexport) string& GetBFaceMesh_Name(const uiint& ibmesh);
    __declspec(dllexport) uiint GetBVolumeMesh_NameLength(const uiint& ibmesh);
    __declspec(dllexport) string& GetBVolumeMesh_Name(const uiint& ibmesh);
    __declspec(dllexport) uiint GetBEdgeMesh_NameLength(const uiint& ibmesh);
    __declspec(dllexport) string& GetBEdgeMesh_Name(const uiint& ibmesh);
    __declspec(dllexport) uiint GetEdgeID_BEdge(const uiint& ibmesh, const uiint& ibedge);
    __declspec(dllexport) uiint GetElemID_BEdge(const uiint& ibmesh, const uiint& ibedge);
    __declspec(dllexport) uiint GetFaceID_BFace(const uiint& ibmesh, const uiint& ibface);
    __declspec(dllexport) uiint GetElemID_BFace(const uiint& ibmesh, const uiint& ibface);
    __declspec(dllexport) uiint GetElemID_BVolume(const uiint& ibmesh, const uiint& ibvol);

    //--
    // MPI
    //--
    __declspec(dllexport) int GetRank();
    __declspec(dllexport) int GetNumOfProcess();
    __declspec(dllexport) iint AllReduce(void* sendbuf, void* recvbuf, iint buf_size, MPI_Datatype datatype, MPI_Op op, MPI_Comm commworld);
    __declspec(dllexport) iint Barrier(MPI_Comm commworld);
    __declspec(dllexport) iint Abort(MPI_Comm commworld, int error);
    __declspec(dllexport) iint AllGather(void* sendbuf, iint sendcnt, MPI_Datatype sendtype, void* recvbuf, iint recvcnt, MPI_Datatype recvtype, MPI_Comm comm);
    __declspec(dllexport) iint Gather(void* sendbuf , iint sendcnt, MPI_Datatype sendtype, void* recvbuf, iint recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm);
    __declspec(dllexport) iint Scatter(void* sendbuf, iint sendcnt, MPI_Datatype sendtype, void* recvbuf, iint recvcnt, MPI_Datatype recvtype, int root, MPI_Comm comm);
    __declspec(dllexport) iint Recv(void* buf, iint count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Status *status);
    __declspec(dllexport) iint Send(void* buf, iint count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);
    __declspec(dllexport) iint Bcast(void* buf, iint cnt, MPI_Datatype type, int root, MPI_Comm comm);
    __declspec(dllexport) iint GetNumOfNeibPE(const uiint& imesh);
    __declspec(dllexport) iint GetTransRank(const uiint& imesh, const uiint& ipe);
    __declspec(dllexport) void Send_Recv_R(double* buf, const iint& num_of_node, const iint& dof_size, const int& trans_rank);
    __declspec(dllexport) void Send_Recv_I(iint* buf, const iint& num_of_node, const iint& dof_size, const int& trans_rank );
    __declspec(dllexport) void Sendrecv_R(double* buf, const iint& num_of_node, const iint& dof_size, const int& trans_rank);
    __declspec(dllexport) void Sendrecv_I(iint* buf, const iint& num_of_node, const iint& dof_size, const int& trans_rank);
    __declspec(dllexport) MPI_Datatype MPI_UIINT();
    __declspec(dllexport) MPI_Datatype MPI_IINT();

    
    //--
    // CommMesh2
    //--
    __declspec(dllexport) uiint GetNumOfCommMesh();
    __declspec(dllexport) uiint GetNumOfCommNode(const uiint& icmesh);
    __declspec(dllexport) uiint& GetNodeID_CommNode(const uiint& icmesh, const uiint& icnode);

    
    //--
    // 接合面:ContactMesh
    //--
    __declspec(dllexport) uiint GetNumOfContactMesh();
    __declspec(dllexport) uiint GetContactMeshID(const uiint& icont);



    
    __declspec(dllexport) uiint GetNumOfElementGroup();
    __declspec(dllexport) uiint GetNumOfElementID(const uiint& iGrp);
    __declspec(dllexport) uiint& GetElementID_with_ElementGroup(const uiint& iGrp, const uiint& index);
    __declspec(dllexport) uiint GetElementGroupName_Length(const uiint& iGrp);
    __declspec(dllexport) string& GetElementGroupName(const uiint& iGrp);


    __declspec(dllexport) void LoggerMode(const uiint& mode);
    __declspec(dllexport) void LoggerDevice(const uiint& mode, const uiint& device);
    __declspec(dllexport) void LoggerInfoMssg(const uiint& mode, const char* message);
    __declspec(dllexport) void LoggerInfo(const uiint& mode, const char* format, ... );
    __declspec(dllexport) uiint getErrorMode();
    __declspec(dllexport) uiint getWarnMode();
    __declspec(dllexport) uiint getInfoMode();
    __declspec(dllexport) uiint getDebugMode();
    __declspec(dllexport) uiint getDiskDevice();
    __declspec(dllexport) uiint getDisplayDevice();
};
#endif
}
