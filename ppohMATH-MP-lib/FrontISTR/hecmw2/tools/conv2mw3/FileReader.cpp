/*
 ----------------------------------------------------------
|
| Software Name :conv2mw3 Ver 0.1 beta
|
|   FileReader.cpp
|
|                     Written by T.Takeda,    2012/06/01
|                                Y.Sato,      2012/06/01
|
|   Contact address : IIS, The University of Tokyo CISS
|
 ----------------------------------------------------------
*/
#include "FileReader.h"
using namespace boost::xpressive;

CFileReader::CFileReader()
{
	msHeaderTitle="";
	mnLineNum=0;
	mbOpFlag=false;
}
CFileReader::~CFileReader()
{
}
//--
// �v�f�^�C�v�ʂ̐ߓ_��
//--
size_t CFileReader::getElementNNum(size_t nType)
{
	switch(nType){
		case(FistrElementType::Beam):  return 2;
		case(FistrElementType::Beam2): return 3;

		case(FistrElementType::Triangle):  return 3;
		case(FistrElementType::Triangle2): return 6;

		case(FistrElementType::Quad):  return 4;
		case(FistrElementType::Quad2): return 8;

		case(FistrElementType::Tetra):  return 4;
		case(FistrElementType::Tetra2): return 10;

		case(FistrElementType::Prism):  return 6;
		case(FistrElementType::Prism2): return 15;

		case(FistrElementType::Hexa):  return 8;
		case(FistrElementType::Hexa2): return 20;

		case(FistrElementType::IFaceQuad):  return 4;

		case(FistrElementType::TriShell):  return 3;
		case(FistrElementType::QuadShell): return 4;

		default:
			return 0;
	}
}
//--
// Mesh == Parts
//--
CMesh* CFileReader::getgenMesh(CAssyModel* pAssyModel, string sPartsName, string sTagName)
{
	CMessage* pMsg=CMessage::Instance();

	CMesh* pMesh=NULL;

	if( !sPartsName.empty() ){
		if( pAssyModel->existPartsName(sPartsName) ){
			pMesh= pAssyModel->getMesh(sPartsName);
		}else{
			pMesh = new CMesh;//------------------------- ���� Mesh
			pMesh->setPartsName(sPartsName);

			size_t nNMesh=pAssyModel->getNumOfMesh();
			pMesh->setID(nNMesh);//---------------------- ID

			pAssyModel->addMesh(pMesh);
		}
	}else{
		pMsg->error("Partname is not exist in "+sTagName);
		exit(0);//----------------------------------------------exit(0)
	}
	
	return pMesh;
}
//--
// GroupPair : MPC 
//--
CGroupPair* CFileReader::getgenMPCPair(CAssyModel* pAssyModel, string sPairName, string sTagName)
{
	CMessage* pMsg=CMessage::Instance();

	CGroupPair* pMPCPair=NULL;

	if(!sPairName.empty() ){
		if( pAssyModel->existMPCPairName(sPairName) ){
			pMPCPair= pAssyModel->getMPCPair(sPairName);
		}else{
			pMPCPair= new CMPCPair;//--------------------- ���� MPCPair
			pMPCPair->setName(sPairName);

			size_t nNMPC=pAssyModel->getNumOfMPCPair();
			pMPCPair->setID(nNMPC);//--------------------- ID

			pAssyModel->addMPCPair(pMPCPair);
		}
	}else{
		pMsg->error("AssemblyPair Name is not exist in " + sTagName);
		exit(0);//----------------------------------------------exit(0)
	}
	return pMPCPair;
}
//--
// GroupPair : Contact
//--
CGroupPair* CFileReader::getgenConPair(CAssyModel* pAssyModel, string sPairName, string sTagName)
{
	CMessage* pMsg=CMessage::Instance();

	CGroupPair* pConPair=NULL;

	if( !sPairName.empty() ){
		if( pAssyModel->existConPairName(sPairName) ){
			pConPair= pAssyModel->getConPair(sPairName);
		}else{
			pConPair= new CConPair;//-------------------- ���� ConPair
			pConPair->setName(sPairName);

			size_t nNCon=pAssyModel->getNumOfConPair();
			pConPair->setID(nNCon);//-------------------- ID

			pAssyModel->addConPair(pConPair);
		}
	}else{
		pMsg->error("ContactPair Name is not exist in "+ sTagName);
		exit(0);//----------------------------------------------exit(0)
	}

	return pConPair;
}

//--
// Ngroup
//--
CNgroup* CFileReader::getgenNgrp(CMesh* pMesh, string sNgrpName, string sTagName)
{
	CMessage* pMsg=CMessage::Instance();

	CNgroup* pNgrp=NULL;

	if(!sNgrpName.empty()){
		if( pMesh->existNgrpName(sNgrpName) ){
			pNgrp= pMesh->getNgrp(sNgrpName);
		}else{
			pNgrp = new CNgroup;//----------------------- ���� Ngroup
			pNgrp->setGroupName(sNgrpName);

			size_t nNNgrp=pMesh->getNumOfNgrp();
			pNgrp->setID(nNNgrp);//---------------------- ID

			pMesh->addNgroup(pNgrp);
		}
	}else{
		pMsg->error("NGROUP Name is not exist in "+ sTagName);
		exit(0);//----------------------------------------------exit(0)
	}

	return pNgrp;
}
//--
// Sgroup
//--
CSgroup* CFileReader::getgenSgrp(CMesh* pMesh, string sSgrpName, string sTagName)
{
	CMessage* pMsg=CMessage::Instance();

	CSgroup* pSgrp=NULL;

	if(!sSgrpName.empty()){
		if( pMesh->existSgrpName(sSgrpName) ){
			pSgrp= pMesh->getSgrp(sSgrpName);
		}else{
			pSgrp = new CSgroup;//----------------------- ���� Sgroup
			pSgrp->setGroupName(sSgrpName);

			size_t nNSgrp=pMesh->getNumOfSgrp();
			pSgrp->setID(nNSgrp);//---------------------- ID

			pMesh->addSgroup(pSgrp);
		}
	}else{
		pMsg->error("SGROUP Name is not exist in "+ sTagName);
		exit(0);//----------------------------------------------exit(0)
	}

	return pSgrp;
}
//--
// Lgroup
//--
CLgroup* CFileReader::getgenLgrp(CMesh* pMesh, string sLgrpName, string sTagName)
{
	CMessage* pMsg=CMessage::Instance();

	CLgroup* pLgrp=NULL;

	if(!sLgrpName.empty()){
		if( pMesh->existLgrpName(sLgrpName) ){
			pLgrp= pMesh->getLgrp(sLgrpName);
		}else{
			pLgrp = new CLgroup;//----------------------- ���� Lgroup
			pLgrp->setGroupName(sLgrpName);

			size_t nNLgrp=pMesh->getNumOfLgrp();
			pLgrp->setID(nNLgrp);//---------------------- ID

			pMesh->addLgroup(pLgrp);
		}
	}else{
		pMsg->error("SGROUP(EDGE) Name is not exist in "+ sTagName);//------- FrontISTR�͕��ʗv�f�̕ӂ�SGROUP
		exit(0);//----------------------------------------------exit(0)
	}

	return pLgrp;
}
//--
// Egroup
//--
CEgroup* CFileReader::getgenEgrp(CMesh* pMesh, string sEgrpName, string sTagName)
{
	CMessage* pMsg=CMessage::Instance();

	CEgroup* pEgrp=NULL;

	if(!sEgrpName.empty()){
		if( pMesh->existEgrpName(sEgrpName)){
			pEgrp= pMesh->getEgrp(sEgrpName);
		}else{
			pEgrp= new CEgroup;//---------------------- ���� Egroup
			pEgrp->setGroupName(sEgrpName);

			size_t nNEgrp=pMesh->getNumOfEgrp();
			pEgrp->setID(nNEgrp);//-------------------- ID

			pMesh->addEgroup(pEgrp);
		}
	}else{
		pMsg->error("EGROUP Name is not exist in "+ sTagName);
		exit(0);//----------------------------------------------exit(0)
	}

	return pEgrp;
}

//--
// Element
//--
CElement* CFileReader::getgenElement(CMesh* pMesh, size_t nID, size_t nType)
{
	CMessage* pMsg=CMessage::Instance();

	CElement* pElem=NULL;

	if( pMesh->existElementID(nID) ){
		pElem= pMesh->getElement_id(nID);
	}else{
		//---------------------------------------- ���� Element
		switch(nType){
			case(FistrElementType::Beam): pElem= new CBeam;  break;
			case(FistrElementType::Beam2): pElem= new CBeam2; break;

			case(FistrElementType::Triangle): pElem= new CTriangle; break;
			case(FistrElementType::Triangle2): pElem= new CTriangle2; break;

			case(FistrElementType::Quad):  pElem= new CQuad; break;
			case(FistrElementType::Quad2): pElem= new CQuad2; break;

			case(FistrElementType::Tetra):  pElem= new CTetra; break;
			case(FistrElementType::Tetra2): pElem= new CTetra2; break;

			case(FistrElementType::Prism):  pElem= new CPrism; break;
			case(FistrElementType::Prism2): pElem= new CPrism2; break;

			case(FistrElementType::Hexa):  pElem= new CHexa; break;
			case(FistrElementType::Hexa2): pElem= new CHexa2; break;

			case(FistrElementType::IFaceQuad):  pElem= new CIFaceQuad; break;

			case(FistrElementType::TriShell):  pElem= new CTriShell; break;
			case(FistrElementType::QuadShell): pElem= new CQuadShell; break;

			default: pElem=NULL; pMsg->error("CFileReader::getgenElement, element type is not exist."); break;
		}
		// id�ԍ��̃Z�b�g
		if(pElem) pElem->setID(nID);
	}
	return pElem;
}
//--
// !HEADER
//--
bool CFileReader::ReadHeader(ifstream& ifs, string& sLine)
{
	// !HEADER �^�O�E�`�F�b�N
	if( !existStr(sLine, FistrTag::Header()) ) return false;

	bool bVer(false);

	CMessage* pMsg=CMessage::Instance();

	//--
	// !HEADER�p�����[�^: �^�O�E�`�F�b�N�ς�
	//--
	vstring vStr= SplitToken(sLine);
	for(vstring::iterator it=vStr.begin(); it != vStr.end(); ++it){
		string str = *it;

		// �p�����[�^:VER
		if( existStr(str, FistrTag::Ver()) ){
			bVer=true;
			string sVerNum= getRearStr(str, "=");

			//�o�[�W�����ԍ��m�F
			if(sVerNum != FistrTag::VerNumber()){
				pMsg->error("data format version "+sVerNum);
				exit(0);//------------------------------------------------------------------- exit(0)
			}
		}//if(VER)

	};//tokenizer end
	

	// regex
	string strRegex= FistrTag::regAllHeaders();// "!HEADER,!NODE,!ELEMENT,!..."
	sregex regHead = sregex::compile( strRegex );
	smatch res;

	string sLine2;
	//--
	// Title ���
	//--
	if(bVer){
		ifstream::pos_type nbef = ifs.tellg();
		getline(ifs, sLine2); mnLineNum++;
		
		if( regex_search(sLine2, res, regHead) ){// Error����:�ʃ^�O
			ifs.seekg(nbef,ios::beg); mnLineNum--;
			msHeaderTitle="non title";
			pMsg->warn("input data title:"+msHeaderTitle);
			return false;
		}else if(sLine2==""){
			msHeaderTitle="non title";
			pMsg->warn("input data title:"+msHeaderTitle);
		}else{
			msHeaderTitle=sLine2;
			pMsg->info("input data title:"+msHeaderTitle);
		}
	}else{
		pMsg->error("VER parameter not exist.");
		exit(0);//-------------------------------------------exit(0)
	}

	return true;
}

//--
// !NODE
//--
bool CFileReader::ReadNode(CAssyModel *pAssyModel, ifstream& ifs, string& sLine)
{
	// !NODE �^�O�E�`�F�b�N
	if( !existStr(sLine, FistrTag::Node()) ) return false;

	bool bPartsName(false), bNum(false), bNgrp(false);

	string sPartsName("");//-- Parts��
	string sNNum("");//---- Node��(������)
	string sNgrp("");//---- Ngrp��

	CMessage* pMsg=CMessage::Instance();

	//--
	// !NODE�p�����[�^: �^�O�E�`�F�b�N�ς�
	//--
	vstring vStr= SplitToken(sLine);
	for(vstring::iterator it=vStr.begin(); it != vStr.end(); ++it){
		string str = *it;

		// �p�����[�^:PARTNAME
		if( existStr(str, FistrTag::PartsName()) ){
			bPartsName=true;
			sPartsName=getRearStr(str, "=");
			if( sPartsName.empty() ) pMsg->error("!NODE, Name is not exist in PARTNAME");
		}

		// �p�����[�^:NUM
		if( existStr(str, FistrTag::Num()) ){
			bNum=true;
			sNNum= getRearStr(str, "=");
			if( sNNum.empty() ) pMsg->error("!NODE, Value is not exist in NUM");
		}

		// �I�v�V����:NGRP
		if( existStr(str, FistrTag::Ngrp()) ){
			bNgrp=true;
			sNgrp=getRearStr(str, "=");
			if( sNgrp.empty() ) pMsg->error("!NODE, Name is not exist in NGRP");
		}

	};//tokenizer end


	if(!bPartsName || !bNum ){
		string sInfoMsg;
		if(!bPartsName) sInfoMsg = " PARTNAME";
		if(!bNum)       sInfoMsg += " NUM";

		pMsg->error("Required parameters are missing." + sInfoMsg);
		exit(0);//------------------------------------------------------exit(0);
	}

	//--
	// Mesh == Parts
	//--
	CMesh* pMesh= getgenMesh(pAssyModel, sPartsName, FistrTag::Node());
	//--
	// Ngrp : �I�v�V�����Ȃ̂Ŗ����ꍇ����
	//--
	CNgroup* pNgrp=NULL;
	if(pMesh && bNgrp)
		pNgrp= getgenNgrp(pMesh, sNgrp, FistrTag::Node());


	// regex
	string strRegex= FistrTag::regAllHeaders();// "!HEADER,!NODE,!ELEMENT,!..."
	sregex regHead = sregex::compile(strRegex);
	smatch res;

	std::string sLine2;
	//--
	// �ߓ_�f�[�^
	//--
	if(bNum){
		size_t nNNum= lexical_cast<size_t>(sNNum);//Node��

		size_t nNodeID;
		double X,Y,Z;

		// Node�f�[�^
		for(size_t i=0; i < nNNum; i++){
			ifstream::pos_type nbef= ifs.tellg();
			getline(ifs, sLine2); mnLineNum++;
			
			if( regex_search(sLine2, res, regHead) ){// Error����:�ʃ^�O
				ifs.seekg(nbef, ios::beg); mnLineNum--;
				string sErrLine=lexical_cast<string>(mnLineNum+1);
				pMsg->error("Line:"+sErrLine+" "+res.str()+" in !NODE");
				return false;//----------------------------------------------�ǂݍ��ݏ����I��
			}

			X=0.0; Y=0.0; Z=0.0;//�t�@�C���ɒl�������ꍇ:0.0

			vstring vParam= SplitToken(sLine2);
			for(size_t ii=0; ii < vParam.size(); ii++){
				if(0==ii) nNodeID = lexical_cast<size_t>(vParam[0]);
				if(1==ii) X = lexical_cast<double>(vParam[1]);
				if(2==ii) Y = lexical_cast<double>(vParam[2]);
				if(3==ii) Z = lexical_cast<double>(vParam[3]);
			};

			if(!vParam.empty()){
				CNode *pNode= new CNode;//-------------------- ���� Node
				pNode->setID(nNodeID);
				pNode->setCoord(X,Y,Z);

				pMesh->addNode(pNode);//-------- �p�[�c(Mesh)��Node��ǉ�
				// Ngrp������ꍇ
				if(pNgrp) pNgrp->addNode(pNode);
			}else{
				string sErrLine=lexical_cast<string>(mnLineNum);
				pMsg->warn("Line:"+sErrLine+" empty.");
			}//if(!vParam.empty)
		};
	}else{
		pMsg->error("!NODE, Num is not exist.");
		exit(0);//------------------------------------------ exit(0)
	}

	return true;
}

//--
// !ELEMENT
//--
bool CFileReader::ReadElement(CAssyModel *pAssyModel, ifstream& ifs, string& sLine)
{
	// !ELEMENT �^�O�E�`�F�b�N
	if( !existStr(sLine, FistrTag::Element()) ) return false;

	bool bPartsNameFlg(false), bENumFlg(false), bTypeFlg(false), bEgrpFlg(false);

	CMessage* pMsg=CMessage::Instance();

	string sPartsName;
	string sENum;
	string sType;//-- �v�f�^�C�v
	string sEgrp;//-- Egrp��

	//--
	// !ELEMENT�p�����[�^: �^�O�E�`�F�b�N�ς�
	//--
	vstring vStr= SplitToken(sLine);
	for(vstring::iterator it=vStr.begin(); it != vStr.end(); ++it){
		string str = *it;

		// PARTNAME
		if( existStr(str, FistrTag::PartsName()) ){
			bPartsNameFlg=true;
			sPartsName=getRearStr(str, "=");
			if( sPartsName.empty() ){
				pMsg->error("!ELEMENT, Name is not exist in PARTNAME");
			}
		}
		// NUM
		if( existStr(str, FistrTag::Num()) ){
			bENumFlg=true;
			sENum= getRearStr(str, "=");
			if( sENum.empty() ){
				pMsg->error("!ELEMENT, Value is not exist in NUM");
			}
		}
		// TYPE
		if( existStr(str, FistrTag::Type()) ){
			bTypeFlg=true;
			sType= getRearStr(str, "=");
			if( sType.empty() ){
				pMsg->error("!ELEMENT, Value is not exist in Type");
			}
		}
		// EGRP
		if( existStr(str, FistrTag::Egrp()) ){
			bEgrpFlg=true;
			sEgrp=getRearStr(str, "=");
			if( sEgrp.empty() ){
				pMsg->error("!ELEMENT, Name is not exist in Egrp");
			}
		}

	};//tokenizer end


	if(!bPartsNameFlg || !bENumFlg || !bTypeFlg){
		string sInfoMsg;
		if(!bPartsNameFlg) sInfoMsg = " PARTNAME";
		if(!bENumFlg)      sInfoMsg += " NUM";
		if(!bTypeFlg)      sInfoMsg += " TYPE";

		pMsg->error("Required parameters are missing." + sInfoMsg);
		exit(0);//-------------------------------------------------exit(0);
	}

	//--
	// Mesh == Parts
	//--
	CMesh* pMesh=getgenMesh(pAssyModel, sPartsName, FistrTag::Element());
	//--
	// Egroup
	//--
	CEgroup* pEgrp=NULL;
	if(pMesh && bEgrpFlg)//���I�v�V�����Ȃ̂Ŗ����ꍇ����.
		pEgrp= getgenEgrp(pMesh, sEgrp, FistrTag::Element());


	// regex
	string strRegex= FistrTag::regAllHeaders();// "!HEADER,!NODE,!ELEMENT,!..."
	sregex regHead = sregex::compile(strRegex);
	smatch res;

	if(!bENumFlg || !bTypeFlg){ pMsg->error("Type is not exist in !ELEMENT"); exit(0);}//------------------ exit(0)
	
	size_t nNNum, nENum, nType;
	nENum= lexical_cast<size_t>(sENum);
	nType= lexical_cast<size_t>(sType);
	nNNum= getElementNNum(nType);
	//--
	// �v�f�f�[�^
	//--	
	for(size_t i=0; i < nENum; i++){		
		size_t nID;
		vector<size_t> vNID;
		// �v�f�f�[�^�͕����s�ɓn�� : vNID���A�v�f�ߓ_���ɂȂ�܂Ŏ擾
		size_t nCount=0;
		while( nCount < nNNum ){
			string sLine2;
			ifstream::pos_type nbef=ifs.tellg();
			getline(ifs, sLine2); mnLineNum++;
			
			if( regex_search(sLine2, res, regHead) ){// Error����:�ʃ^�O
				ifs.seekg(nbef, ios::beg); mnLineNum--;
				string sErrLine=lexical_cast<string>(mnLineNum+1);
				pMsg->error("Line:"+sErrLine+" "+res.str()+" in !ELEMENT");
				return false;//-----------------------------------------------�ǂݍ��ݏ����I��
			}

			vstring vParam= SplitToken(sLine2);
			for(size_t ii=0; ii < vParam.size(); ii++){
				if(nCount==0 && ii==0) nID=lexical_cast<size_t>( vParam[ii] );
				if(nCount==0 && ii!=0) vNID.push_back( lexical_cast<size_t>(vParam[ii]) );

				if(nCount!=0) vNID.push_back( lexical_cast<size_t>(vParam[ii]) );
			};
			nCount= vNID.size();

			if(vParam.empty()){
				string sErrLine=lexical_cast<string>(mnLineNum);
				pMsg->warn("Line:"+sErrLine+" empty.");
			}//if(vParam.empty)

		};//while end

		CElement* pElem= getgenElement(pMesh, nID, nType);//----------- �v�f���� �a �擾

		pMesh->addElement(pElem);//-------- �p�[�c(Mesh)��Element��ǉ�
		if(pEgrp)
			pEgrp->addElement(pElem);//----------- Egroup ��Element��ǉ�

		CNode* pNode;
		for(size_t ii=0; ii < nNNum; ii++){
			if( pMesh->existNodeID(vNID[ii]) ){
				pNode=pMesh->getNode_id(vNID[ii]);
				pElem->setNode(ii, pNode);//------------------------------- Node�Z�b�g

			}else{
				string sNID=lexical_cast<string>(vNID[ii]);
				string sErrLine=lexical_cast<string>(mnLineNum);
				pMsg->error("Line:"+sErrLine+" !ELEMENT, "+sNID+" is not exist");
				exit(0);//------------------------------------------------- exit(0)
			}
		};//for( nNNum )		
	};//for( nENum )

	return true;
}

//--
// !EGROUP
//--
bool CFileReader::ReadEgrp(CAssyModel *pAssyModel, ifstream& ifs, string& sLine)
{
	// !EGROUP �^�O�E�`�F�b�N
	if( !existStr(sLine, FistrTag::Egroup()) ) return false;

	bool bPartsName(false), bENum(false), bGrpName(false), bGenerate(false);

	CMessage* pMsg=CMessage::Instance();

	string sPartsName;
	string sENum;
	string sGrpName;//-- Egrp��

	//--
	// !EGROUP�p�����[�^: �^�O�E�`�F�b�N�ς�
	//--
	vstring vStr= SplitToken(sLine);
	for(vstring::iterator it=vStr.begin(); it != vStr.end(); ++it){
		string str = *it;

		// PARTNAME
		if( existStr(str, FistrTag::PartsName()) ){
			bPartsName=true;
			sPartsName=getRearStr(str, "=");
			if( sPartsName.empty() )pMsg->error("!EGROUP, Name is not exist in PARTNAME");
		}
		// NUM
		if( existStr(str, FistrTag::Num()) ){
			bENum=true;
			sENum= getRearStr(str, "=");
			if( sENum.empty() ) pMsg->error("!EGROUP, Value is not exist in NUM");
		}
		// EGRP : �O���[�v��
		if( existStr(str, FistrTag::Egrp()) ){
			bGrpName=true;
			sGrpName=getRearStr(str, "=");
			if( sGrpName.empty() ) pMsg->error("!EGROUP, Name is not exist in EGRP");
		}
		// GENERATE�I�v�V����
		if( existStr(str, FistrTag::Generate()) ) bGenerate=true;

	};//tokenizer end

	if(!bGenerate){
		// GENERATE�I�v�V�����Ȃ��F�G���[
		if(!bPartsName || !bENum || !bGrpName){
			string sInfoMsg;
			if(!bPartsName) sInfoMsg = " PARTNAME";
			if(!bENum)      sInfoMsg += " NUM";
			if(!bGrpName)   sInfoMsg += " EGRP";

			pMsg->error("Required parameters are missing." + sInfoMsg);
			exit(0);//-------------------------------------------------exit(0);
		}
	}else{
		// GENERATE�I�v�V��������:�G���[
		if(!bPartsName || !bGrpName){
			string sInfoMsg;
			if(!bPartsName) sInfoMsg = " PARTNAME";
			if(!bGrpName)   sInfoMsg += " EGRP";

			pMsg->error("Required parameters are missing." + sInfoMsg);
			exit(0);//-------------------------------------------------exit(0);
		}
		// GENERATE�I�v�V��������FENum���Z�b�g����Ă���ꍇ
		if(bENum){
			pMsg->error("Unnecessary parameter NUM.");
			exit(0);//-------------------------------------------------exit(0);
		}
	}

	//--
	// Mesh == Parts
	//--
	CMesh* pMesh=getgenMesh(pAssyModel, sPartsName, FistrTag::Egroup());
	//--
	// Egroup 
	//--
	CEgroup* pEgrp=NULL;
	if(pMesh)
		pEgrp= getgenEgrp(pMesh, sGrpName, FistrTag::Egroup());
	//--
	// conv2mw3 �I�v�V����
	//--
	if(mbOpFlag){
		string sBndType;
		pMsg->user_in("GroupName="+sGrpName+"; enter D or N (\"D\" means dirichlet,\"N\" means neumann)=> ", sBndType);
		transform(sBndType.begin(), sBndType.end(), sBndType.begin(), ::tolower);

		if(string::npos != sBndType.find("d") && sBndType.length()==1){ pEgrp->setBndType(BndType::Dirichlet);}else
		if(string::npos != sBndType.find("n") && sBndType.length()==1){	pEgrp->setBndType(BndType::Neumann);  }
		else{ pMsg->error("set to default. \"N\"."); pEgrp->setBndType(BndType::Neumann);}
	}

	// regex
	string strRegex= FistrTag::regAllHeaders();// "!HEADER,!NODE,!ELEMENT,!..."
	sregex regHead = sregex::compile(strRegex);
	smatch res;

	size_t nCount=0, nENum;
	if(bENum) nENum= lexical_cast<size_t>(sENum);//�v�f��
	//--
	// EGROUP �ǂݍ���
	// # nENum�ɖ��֌W�Ƀ^�O�Ń`�F�b�N
	//--
	while(!ifs.eof()){
		string sLine2;
		ifstream::pos_type nbef= ifs.tellg();
		getline(ifs, sLine2); mnLineNum++;

		// �ʃ^�O�̏o��
		if( regex_search(sLine2, res, regHead) ){
			ifs.seekg(nbef, ios::beg); mnLineNum--;
			break;//----------------------------------�ǂݍ��ݏI���F���̃^�O���o���D
		}

		vector<size_t> vEID;
		size_t id1, id2, step=1;
		vstring vParam=SplitToken(sLine2);
		if(!vParam.empty()){
			// �v�fID
			for(size_t i=0; i < vParam.size(); i++){
				if(bGenerate){//GENERATE�I�v�V����
					if(i==0) id1= lexical_cast<size_t>(vParam[i]);
					if(i==1) id2= lexical_cast<size_t>(vParam[i]);
					if(i==2) step=lexical_cast<size_t>(vParam[i]);
				}else{
					vEID.push_back( lexical_cast<size_t>(vParam[i]) );
				}
			};

			if(bGenerate)//GENERATE�I�v�V���� : �v�fID��W�J
				for(size_t i=id1; i <= id2; i+=step) vEID.push_back(i);
		}else{
			string sErrLine=lexical_cast<string>(mnLineNum);
			pMsg->warn("Line:"+sErrLine+" empty.");
		}


		//�v�f�Q�ɗv�f��ǉ�
		for(vector<size_t>::iterator it=vEID.begin(); it!=vEID.end(); it++){
			if(pMesh->existElementID(*it)){
				CElement *pElem= pMesh->getElement_id(*it);
				pEgrp->addElement(pElem);
			}else{
				//Error:�v�fID�����݂��Ȃ�
				string sNumb=lexical_cast<string>(*it);
				string sErrLine=lexical_cast<string>(mnLineNum);
				pMsg->error("Line:"+sErrLine+" !EGROUP id:" + sNumb + " is not exist.");
			}
		};

		//�v�f���̃`�F�b�N
		nCount += vEID.size();//�v�f���J�E���g
		if(!bGenerate && bENum && nCount > nENum){//GENERATE�Ȃ��AENum���̃`�F�b�N
			string sErrLine=lexical_cast<string>(mnLineNum);
			pMsg->error("Line:"+sErrLine+" exceeded "+sENum);
			return false;
		}

	};//while(!eof) end

	return true;
}

//--
// !SGROUP
//--
bool CFileReader::ReadSgrp(CAssyModel *pAssyModel, ifstream& ifs, string& sLine)
{
	// !SGROUP �^�O�E�`�F�b�N
	if( !existStr(sLine, FistrTag::Sgroup()) ) return false;

	bool bPartsName(false), bSNum(false), bGrpName(false);

	CMessage* pMsg=CMessage::Instance();

	string sPartsName;
	string sSNum;
	string sGrpName;//-- Sgrp��

	//--
	// !SGROUP�p�����[�^: �^�O�E�`�F�b�N�ς�
	//--
	vstring vStr= SplitToken(sLine);
	for(vstring::iterator it=vStr.begin(); it != vStr.end(); ++it){
		string str = *it;

		// PARTNAME
		if( existStr(str, FistrTag::PartsName()) ){
			bPartsName=true;
			sPartsName=getRearStr(str, "=");
			if( sPartsName.empty() ) pMsg->error("!SGROUP, Name is not exist in PARTNAME");
		}
		// NUM
		if( existStr(str, FistrTag::Num()) ){
			bSNum=true;
			sSNum= getRearStr(str, "=");
			if( sSNum.empty() ) pMsg->error("!SGROUP, Value is not exist in NUM");
		}
		// SGRP : �O���[�v��
		if( existStr(str, FistrTag::Sgrp()) ){
			bGrpName=true;
			sGrpName=getRearStr(str, "=");
			if( sGrpName.empty() ) pMsg->error("!SGROUP, Name is not exist in Sgrp");
		}
	};//tokenizer end

	if(!bPartsName || !bSNum || !bGrpName){
		string sInfoMsg;
		if(!bPartsName) sInfoMsg = " PARTNAME";
		if(!bSNum)      sInfoMsg += " NUM";
		if(!bGrpName)   sInfoMsg += " SGRP";

		pMsg->error("Required parameters are missing." + sInfoMsg);
		exit(0);//-------------------------------------------------exit(0);
	}

	//--
	// Mesh == Parts
	//--
	CMesh* pMesh=getgenMesh(pAssyModel, sPartsName, FistrTag::Sgroup());
	//--
	// conv2mw3 �I�v�V����
	//--
	size_t nOption;
	if(mbOpFlag){
		string sBndType;
		pMsg->user_in("GroupName="+sGrpName+"; enter D or N (\"D\" means dirichlet,\"N\" means neumann)=> ", sBndType);
		transform(sBndType.begin(), sBndType.end(), sBndType.begin(), ::tolower);

		if(string::npos != sBndType.find("d") && sBndType.length()==1){ nOption = BndType::Dirichlet;}else
		if(string::npos != sBndType.find("n") && sBndType.length()==1){	nOption = BndType::Neumann;  }
		else{ pMsg->warn("set to default. \"N\"."); nOption = BndType::Neumann;}
	}

	// regex
	string strRegex= FistrTag::regAllHeaders();// "!HEADER,!NODE,!ELEMENT,!..."
	sregex regHead = sregex::compile(strRegex);
	smatch res;

	string strPlate= FistrElemTypeS::regPlate();// "TriShell Triangle ..."
	sregex regPlate= sregex::compile(strPlate);
	smatch resplt;

	string strLine= FistrElemTypeS::regLine();// "Beam .."
	sregex regLine= sregex::compile(strLine);
	smatch resline;
	
	//--
	// �ʌQ
	//--
	size_t nCount=0, nSNum=lexical_cast<size_t>(sSNum);
	while(nCount < nSNum){
		string sLine2;
		ifstream::pos_type nbef=ifs.tellg();
		getline(ifs, sLine2); mnLineNum++;

		//Error �ʃ^�O�̏o��
		if( regex_search(sLine2, res, regHead) ){
			ifs.seekg(nbef, ios::beg); mnLineNum--;
			string sErrLine=lexical_cast<string>(mnLineNum+1);
			pMsg->error("Line:"+sErrLine+" "+res.str()+" in !SGROUP");
			return false;// ------------------------------------------------ �ǂݍ��ݏI�� : ���̃^�O���o��.
		}

		vector<size_t> vElemID, vFace;
		vstring vsParam= SplitToken(sLine2);
		for(size_t i=0; i < vsParam.size(); i++){
			size_t ndiv = i % 2;
			if(ndiv==0) vElemID.push_back(lexical_cast<size_t>(vsParam[i]));//--- �v�fID
			if(ndiv==1) vFace.push_back(lexical_cast<size_t>(vsParam[i]));  //--- �ʔԍ�
		};//for(vsParam)

		if(vsParam.empty()){
			string sErrLine=lexical_cast<string>(mnLineNum);
			pMsg->warn("Line:"+sErrLine+" empty.");
		}//if(vsParam.empty)

		//Error �y�A��������Ȃ�
		if(vElemID.size() != vFace.size()){
			string sErrLine=lexical_cast<string>(mnLineNum);
			pMsg->error("Line:"+sErrLine+" Number of elements and faces do not match in !SGROUP");
			exit(0);//-------------------------------------------exit(0)
		}

		nCount += vElemID.size();

		// Sgrp�ɖʌQ�f�[�^��ǉ�
		for(size_t i=0; i < vElemID.size(); i++){

			if(pMesh->existElementID(vElemID[i])){
				CElement *pElem=pMesh->getElement_id(vElemID[i]);

				string sEType= pElem->getCType();
				if( regex_search(sEType, resplt, regPlate) ){
					// PLATE�v�f
					if(vFace[i] > pElem->getNumOfEdge()){
						// Error����:���̗v�f�ɑ��݂��Ȃ�"�Ӕԍ�"
						string sErrLine=lexical_cast<string>(mnLineNum);
						string sFace=lexical_cast<string>(vFace[i]);
						string sElem=lexical_cast<string>(vElemID[i]);
						pMsg->error("Line:"+sErrLine+", "+sFace+" is not exist in this element:"+sElem);
					}else{
						//--
						// Lgrp�ւ̕ӌQ�̒ǉ�
						//--
						CLgroup* pLgrp= getgenLgrp(pMesh, sGrpName, FistrTag::Sgroup());//--------------- Lgroup ����||�擾
						pLgrp->addElemEdgeID(vElemID[i], vFace[i]);//--------------------- �ӌQ�ɒǉ�
						if(mbOpFlag) pLgrp->setBndType(nOption);//------------------------ conv2mw3�I�v�V����
					}
				}else if( !regex_search(sEType, resline, regLine) ){
					// SOLID�v�f
					if(vFace[i] > pElem->getNumOfFace()){
						// Error����:���̗v�f�ɑ��݂��Ȃ�"�ʔԍ�"
						string sErrLine=lexical_cast<string>(mnLineNum);
						string sFace=lexical_cast<string>(vFace[i]);
						string sElem=lexical_cast<string>(vElemID[i]);
						pMsg->error("Line:"+sErrLine+", "+sFace+" is not exist in this element:"+sElem);
					}else{
						//--
						// Sgrp�ւ̖ʌQ�̒ǉ�
						//--
						CSgroup* pSgrp=getgenSgrp(pMesh, sGrpName, FistrTag::Sgroup());//---------------- Sgroup ����||�擾
						pSgrp->addElemFaceID(vElemID[i], vFace[i]);//-------------------- �ʌQ�ɒǉ�
						if(mbOpFlag) pSgrp->setBndType(nOption);//----------------------- conv2mw3�I�v�V����
					}
				}//if(regex)
			}else{
				// Error����:���̃p�[�c�ɗv�f�����݂��Ȃ�
				string sErrLine=lexical_cast<string>(mnLineNum);
				string sElem=lexical_cast<string>(vElemID[i]);
				pMsg->error("Line:"+sErrLine+", "+sElem+" is not exist in this parts:"+sPartsName);
			}//if(existElementID)
		}

	};//while(nSNum)

	return true;
}

//--
// !NGROUP
//--
bool CFileReader::ReadNgrp(CAssyModel *pAssyModel, ifstream& ifs, string& sLine)
{
	// !NGROUP �^�O�E�`�F�b�N
	if( !existStr(sLine, FistrTag::Ngroup()) ) return false;

	bool bPartsName(false), bNNum(false), bGrpName(false), bGenerate(false);

	CMessage* pMsg=CMessage::Instance();

	string sPartsName;
	string sNNum;
	string sGrpName;//-- Ngrp��

	//--
	// !NGROUP�p�����[�^: �^�O�E�`�F�b�N�ς�
	//--
	vstring vStr= SplitToken(sLine);
	for(vstring::iterator it=vStr.begin(); it != vStr.end(); ++it){
		string str = *it;

		// PARTNAME
		if( existStr(str, FistrTag::PartsName()) ){
			bPartsName=true;
			sPartsName=getRearStr(str, "=");
			if( sPartsName.empty() ) pMsg->error("!NGROUP, Name is not exist in PARTNAME");
		}
		// NUM
		if( existStr(str, FistrTag::Num()) ){
			bNNum=true;
			sNNum= getRearStr(str, "=");
			if( sNNum.empty() ) pMsg->error("!NGROUP, Value is not exist in NUM");
		}
		// NGRP : �O���[�v��
		if( existStr(str, FistrTag::Ngrp()) ){
			bGrpName=true;
			sGrpName=getRearStr(str, "=");
			if( sGrpName.empty() ) pMsg->error("!NGROUP, Name is not exist in Ngrp");
		}
		// GENERATE�I�v�V����
		if( existStr(str, FistrTag::Generate()) ) bGenerate=true;

	};//tokenizer end

	if(!bGenerate){
		// GENERATE�I�v�V�����Ȃ�:�G���[
		if(!bPartsName || !bNNum || !bGrpName){
			string sInfoMsg;
			if(!bPartsName) sInfoMsg = " PARTNAME";
			if(!bNNum)      sInfoMsg += " NUM";
			if(!bGrpName)   sInfoMsg += " NGRP";

			pMsg->error("Required parameters are missing." + sInfoMsg);
			exit(0);//-------------------------------------------------exit(0);
		}
	}else{
		// GENERATE�I�v�V��������:�G���[
		if(!bPartsName || !bGrpName){
			string sInfoMsg;
			if(!bPartsName) sInfoMsg = " PARTNAME";
			if(!bGrpName)   sInfoMsg += " NGRP";

			pMsg->error("Required parameters are missing." + sInfoMsg);
			exit(0);//-------------------------------------------------exit(0);
		}
		// GENERATE�I�v�V��������FNNum���Z�b�g����Ă���ꍇ
		if(bNNum){
			pMsg->error("Unnecessary parameter NUM.");
			exit(0);//-------------------------------------------------exit(0);
		}
	}

	//--
	// Mesh == Parts
	//--
	CMesh* pMesh=getgenMesh(pAssyModel, sPartsName, FistrTag::Ngroup());
	//--
	// Ngroup 
	//--
	CNgroup* pNgrp=NULL;
	if(pMesh)
		pNgrp= getgenNgrp(pMesh, sGrpName, FistrTag::Ngroup());
	//--
	// conv2mw3 �I�v�V����
	//--
	if(mbOpFlag){
		string sBndType;
		pMsg->user_in("GroupName="+sGrpName+"; enter D or N (\"D\" means dirichlet,\"N\" means neumann)=> ", sBndType);
		transform(sBndType.begin(), sBndType.end(), sBndType.begin(), ::tolower);

		if(string::npos != sBndType.find("d") && sBndType.length()==1){ pNgrp->setBndType(BndType::Dirichlet);}else
		if(string::npos != sBndType.find("n") && sBndType.length()==1){	pNgrp->setBndType(BndType::Neumann);  }
		else{ pMsg->error("set to default. \"D\"."); pNgrp->setBndType(BndType::Dirichlet);}
	}

	// regex
	string strRegex= FistrTag::regAllHeaders();// "!HEADER,!NODE,!ELEMENT,!..."
	sregex regHead = sregex::compile(strRegex);
	smatch res;

	
	size_t nCount=0, nNNum;
	if(bNNum) nNNum=lexical_cast<size_t>(sNNum);
	//--
	// NGROUP �ǂݍ���
	// # nNNum���ɖ��֌W�Ƀ^�O�Ń`�F�b�N
	//--
	while(!ifs.eof()){
		string sLine2;
		ifstream::pos_type nbef=ifs.tellg();
		getline(ifs, sLine2); mnLineNum++;

		// �ʃ^�O�̏o�� : break 
		if( regex_search(sLine2, res, regHead) ){
			ifs.seekg(nbef, ios::beg); mnLineNum--;
			break;//-------------------------- �ǂݍ��ݏI�� : ���̃^�O���o��.
		}

		vector<size_t> vID;
		size_t id1,id2,step=1;
		vstring vsParam=SplitToken(sLine2);
		if(!vsParam.empty()){
			for(size_t i=0; i < vsParam.size(); i++){
				if(bGenerate){
					//GENERATE�I�v�V��������
					if(i==0) id1=lexical_cast<size_t>(vsParam[i]);
					if(i==1) id2=lexical_cast<size_t>(vsParam[i]);
					if(i==2) step=lexical_cast<size_t>(vsParam[i]);
				}else{
					//GENERATE�I�v�V�����Ȃ�
					size_t nID= lexical_cast<size_t>(vsParam[i]);
					vID.push_back(nID);
				}
			};

			//GENERATE�I�v�V��������: �ߓ_ID�W�J
			if(bGenerate)
				for(size_t i=id1; i <= id2; i+=step) vID.push_back(i);
		}else{
			string sErrLine=lexical_cast<string>(mnLineNum);
			pMsg->warn("Line:"+sErrLine+" empty.");
		}
			
		//�ߓ_�Q��Node��ǉ�
		for(vector<size_t>::iterator it=vID.begin(); it!=vID.end(); it++){
			if( pMesh->existNodeID(*it) ){
				CNode* pNode=pMesh->getNode_id(*it);
				pNgrp->addNode(pNode);
			}else{
				//Error:���݂��Ȃ�NodeID
				string sNumb=lexical_cast<string>(*it);
				string sErrLine=lexical_cast<string>(mnLineNum);
				pMsg->error("Line:"+sErrLine+" !NGROUP id:" + sNumb + " is not exist.");
			}
		};
		
		//�ߓ_���̃`�F�b�N
		nCount += vID.size();//�ߓ_���J�E���^�[
		if(!bGenerate && bNNum && nCount > nNNum){//GENERATE�Ȃ��ANNum���̃`�F�b�N
			string sErrLine=lexical_cast<string>(mnLineNum);
			pMsg->error("Line:"+sErrLine+" exceeded "+sNNum);
			return false;
		}

	};//while(!eof) end

	return true;
}

//--
// !ASSEMBLY_PAIR & !CONTACT_PAIR
//--
bool CFileReader::ReadGrpPair(CAssyModel *pAssyModel, ifstream& ifs, string& sLine)//Assembly_Pair
{
	// !ASSEMBLY_PAIR && !CONTACT_PAIR �^�O�E�`�F�b�N
	if( !existStr(sLine, FistrTag::AssemblyPair()) && !existStr(sLine, FistrTag::ContactPair()) ) return false;

	// regex
	string strRegex= FistrTag::regAllHeaders();// "!HEADER,!NODE,!ELEMENT,!..."
	sregex regHead = sregex::compile(strRegex);
	smatch res;

	regex_search(sLine, res, regHead);
	string sTagName= res.str();//ASSEMBLY_PAIR || CONTACT_PAIR

	bool bPairName(false),  bPairNum(false);
	string sPairName, sPairNum;

	CMessage* pMsg=CMessage::Instance();

	//--
	// !ASSEMBLY_PAIR & !CONTACT_PAIR �p�����[�^
	//--
	vstring vStr= SplitToken(sLine);
	for(vstring::iterator it=vStr.begin(); it != vStr.end(); ++it){
		string str = *it;

		// NAME
		if( existStr(str, FistrTag::Name()) ){
			bPairName=true;
			sPairName=getRearStr(str, "=");
			if( sPairName.empty() ) pMsg->error(sTagName+", Name is not exist in NAME");
		}
		// NUM
		if( existStr(str, FistrTag::Num()) ){
			bPairNum=true;
			sPairNum= getRearStr(str, "=");
			if( sPairNum.empty() ) pMsg->error(sTagName+", Value is not exist in NUM");
		}

	};//tokenizer end

	if(!bPairName || !bPairNum){
		string sInfoMsg;
		if(!bPairName) sInfoMsg = " NAME";
		if(!bPairNum)  sInfoMsg += " NUM";

		pMsg->error("Required parameters are missing." + sInfoMsg);
		exit(0);//-------------------------------------------------exit(0);
	}

	//--
	// GroupPair����
	//--
	CGroupPair* pGrpPair=NULL;
	if(sTagName==FistrTag::AssemblyPair() ){
		pGrpPair= getgenMPCPair(pAssyModel,sPairName, sTagName);// MPCPari
	}
	if(sTagName==FistrTag::ContactPair() ){
		pGrpPair= getgenConPair(pAssyModel,sPairName, sTagName);// ConPair
	}
	
	//--
	// Master, Slave Sgrp
	//--
	size_t nPairNum=lexical_cast<size_t>(sPairNum), nCount=0;
	while(nPairNum > nCount){
		string sLine2;
		ifstream::pos_type nbef=ifs.tellg();
		getline(ifs, sLine2); mnLineNum++;

		//Error �ʃ^�O�̏o��
		if( regex_search(sLine2, res, regHead) ){
			ifs.seekg(nbef, ios::beg); mnLineNum--;
			string sErrLine=lexical_cast<string>(mnLineNum+1);
			pMsg->error("Line:"+sErrLine+" "+res.str()+" in "+sTagName);
			return false;// ------------------------------------------ �ǂݍ��ݏI�� : ���̃^�O���o��.
		}

		string sSSgrpName, sMSgrpName, sSPartsName, sMPartsName;
		vstring vsParam=SplitToken(sLine2);
		if(!vsParam.empty()){
			for(size_t i=0; i < vsParam.size(); i++){
				if(i==0) sSSgrpName = vsParam[i];
				if(i==1) sMSgrpName = vsParam[i];
				if(i==2) sSPartsName = vsParam[i];
				if(i==3) sMPartsName = vsParam[i];
			};//for(vsParam)

			// Mesh == Parts
			CMesh* pSMesh=getgenMesh(pAssyModel, sSPartsName, sTagName);
			CMesh* pMMesh=getgenMesh(pAssyModel, sMPartsName, sTagName);
			// Sgroup 
			CSgroup *pSSgrp, *pMSgrp;
			if(pSMesh) pSSgrp= getgenSgrp(pSMesh, sSSgrpName, sTagName);
			if(pMMesh) pMSgrp= getgenSgrp(pMMesh, sMSgrpName, sTagName);

			if(!pSSgrp || !pMSgrp){//Error
				pMsg->error("SGROUP name is not exist in "+sTagName);
				string sErrLine=lexical_cast<string>(mnLineNum);
				return false;//-------------------------- �ǂݍ��ݏI��
			}

			pGrpPair->setPair(pMSgrp, pSSgrp);//------- GroupPair �Ƀ}�X�^�[�A�X���[�u��Grp���Z�b�g
			pGrpPair->setMeshPair(pMMesh, pSMesh);

			nCount++;//-------------------------------- �J�E���^�[
		}else{
			string sErrLine=lexical_cast<string>(mnLineNum);
			pMsg->warn("Line:"+sErrLine+" empty.");
		}//if(vParam.empty)

	};//while(nPairNum)

	return true;
}


//--
// FrontISTR ver4 ���b�V���t�@�C��
//--
void CFileReader::ReadMesh_FISTR4(CAssyModel *pAssyModel, string filename, bool opflag)
{
	CMessage* pMsg=CMessage::Instance();
	mbOpFlag = opflag;
	// regex
	string strRegex= FistrTag::regAllHeaders();
	sregex regHead = sregex::compile(strRegex);
	smatch res;

	ifstream ifs;
	ifs.open(filename.c_str());// Open

	if(ifs){
		pMsg->info("file open:\""+filename+"\"");

		while(true){
			string sLine;//------- "!�^�O"�s
			getline(ifs,sLine); mnLineNum++;

			if(ifs.eof()) break;
		
			regex_search(sLine, res, regHead);
			string sTag= res.str();

			if(sTag.empty()){
				string sErrLine=lexical_cast<string>(mnLineNum);
				pMsg->warn("Line:"+sErrLine+" \""+sLine+"\"");
			}else{
				pMsg->info("read :" + sTag);

				if(sTag==FistrTag::Header()){ ReadHeader(ifs, sLine); }else               //!HEADER
				if(sTag==FistrTag::Node()){ ReadNode(pAssyModel, ifs, sLine); }else       //!NODE
				if(sTag==FistrTag::Element()){ ReadElement(pAssyModel, ifs, sLine); }else //!ELEMENT
				if(sTag==FistrTag::Egroup()){ ReadEgrp(pAssyModel, ifs, sLine); }else     //!EGROUP
				if(sTag==FistrTag::Sgroup()){ ReadSgrp(pAssyModel, ifs, sLine);}else      //!SGROUP
				if(sTag==FistrTag::Ngroup()){ ReadNgrp(pAssyModel, ifs, sLine);}else      //!NGROUP
				if(sTag==FistrTag::AssemblyPair()){ ReadGrpPair(pAssyModel, ifs, sLine);}else //!ASSEMBLY_PAIR
				if(sTag==FistrTag::ContactPair()){ ReadGrpPair(pAssyModel, ifs, sLine);}else  //!CONTACT_PAIR
				if(sTag==FistrTag::End()){  break;}//!END ---------------------------------------------- break;
			}

		};//while(!eof)
		pMsg->info("file close:\""+filename+"\"\n");
	}else{
		pMsg->error("file \""+filename+"\" not found.");
		exit(0);//-------------------------------------------exit(0)
	}

	ifs.close();// Close
}


