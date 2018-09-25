/*
 ----------------------------------------------------------
|
| Software Name :conv2mw3 Ver 0.1 beta
|
|   FileWriter.cpp
|
|                     Written by T.Takeda,    2012/06/01
|                                Y.Sato,      2012/06/01
|
|   Contact address : IIS, The University of Tokyo CISS
|
 ----------------------------------------------------------
*/
#include "FileWriter.h"
using namespace boost::xpressive;
using namespace boost;
CFileWriter::CFileWriter()
{
	msDirichlet= "Dirichlet";
	msNeumann  = "Neumann";
}
CFileWriter::~CFileWriter()
{
}

//--
// Header && mesh version 0.4
//--
bool CFileWriter::WriteHeader(ofstream& ofs)
{
	string sVer = MW3_Tag::VerNumber();// �o�[�W�����ԍ�

	ofs << "//----------------------------------------      \n"
		     "//      converted mesh data                     \n"
		     "// output format  \"MW3 mesh ver " + sVer + "\" \n"
		     "//----------------------------------------      \n" << endl;
	
	//--
	// Version 
	//--
	ofs << "Version \n"      
	       "  MW3_" + sVer + " \n"
	       "End \n" 
			<< endl;

	return true;
}
//--
// AssyModel
//--
bool CFileWriter::WriteAssyModel(CAssyModel *pAssyModel, ofstream& ofs)
{
	if(!pAssyModel->existMesh()) return false;//Mesh������.

	CMessage *pMsg=CMessage::Instance();
	
	/*
	���b�V���p�[�c��  �ő�ID�@�ŏ�ID
	���b�V���p�[�cID  �����ԍ��@
	�c
	�ʐM�e�[�u����
	���b�V���p�[�cID  �����N�E�y�A1st  �����N�E�y�A2nd
	�c
	*/

	ofs << "AssyModel" << endl;
	//���b�V����  MaxID  MinID
	ofs << format(" %lu %lu %lu") %pAssyModel->getNumOfMesh() %pAssyModel->getMaxIDinMesh() %pAssyModel->getMinIDinMesh() << endl;
	//MeshID  �����ԍ�
	for(size_t i=0; i < pAssyModel->getNumOfMesh(); i++){
		CMesh* pMesh=pAssyModel->getMesh(i);
		ofs << format(" %3lu %lu") %pMesh->getID()  %0 << endl;
	};
	//�ʐM�e�[�u����(�V���O��:���"0")
	ofs << format(" %3lu") %0 << endl;
	ofs << "End\n" << endl;


	//���b�Z�[�W�\��
	pMsg->info("wrote :1 block \"AssyModel\"");
	
	return true;
}
//--
// Node
//--
bool CFileWriter::WriteNode(CAssyModel *pAssyModel, ofstream& ofs)
{
	if(!pAssyModel->existMesh()) return false;//Mesh������

	CMessage *pMsg=CMessage::Instance();

	size_t nNumMesh= pAssyModel->getNumOfMesh();

	for(size_t i=0; i < nNumMesh; i++){
		CMesh *pMesh= pAssyModel->getMesh(i);
		/*
		�ߓ_��  ���b�V���p�[�cID�@�ő�ߓ_ID  �ŏ��ߓ_ID
		���(V,S,SV)�@�X�J���[�ϐ��̐�  �x�N�g���ϐ��̐�  ID  X  Y  Z
		�c
		*/
		ofs << "Node" << endl;
		ofs << format(" %lu %lu %lu %lu") %pMesh->getNumOfNode() %pMesh->getID() %pMesh->getMaxNodeID() %pMesh->getMinNodeID() << endl;
		
		for(size_t ii=0; ii < pMesh->getNumOfNode(); ii++){
			CNode *pNode= pMesh->getNode(ii);
			ofs << format(" %s  %9lu  %23.15e %23.15e %23.15e") %"V 0 3"  %pNode->getID() %pNode->getX() %pNode->getY() %pNode->getZ() << endl;
		};
		ofs << "End\n" << endl;
	};


	// ���b�Z�[�W�\��
	string sNumMesh= lexical_cast<string>(nNumMesh);
	pMsg->info("wrote :"+sNumMesh+" block \"Node\"");

	return true;
}
//--
// Element
//--
bool CFileWriter::WriteElement(CAssyModel *pAssyModel, ofstream& ofs)
{
	if(!pAssyModel->existMesh()) return false;//Mesh������

	CMessage *pMsg=CMessage::Instance();

	// regex
	string strRegex= FistrElemTypeS::regStandard();// MW3�ɑ��݂���v�f�^�C�v
	sregex regBasis = sregex::compile(strRegex);
	string strRegex2= FistrElemTypeS::regShellType();// �V�F���v�f
	sregex regShell= sregex::compile(strRegex2);
	smatch res;

	size_t nNumMesh= pAssyModel->getNumOfMesh();

	for(size_t i=0; i < nNumMesh; i++){
		CMesh *pMesh= pAssyModel->getMesh(i);
		/*
		�v�f��  ���b�V���p�[�cID�@�ő�v�fID  �ŏ��v�fID
		���(Hexa,Tetra,etc.)�@ID  �ߓ_ID  �ߓ_ID  �ߓ_ID  �ߓ_ID ... ...(�v�f�ߓ_�������J��Ԃ�)
		�c
		*/
		ofs << "Element" << endl;
		ofs << format(" %lu %lu %lu %lu") %pMesh->getNumOfElement() %pMesh->getID() %pMesh->getMaxElementID() %pMesh->getMinElementID() << endl;
		for(size_t ii=0; ii < pMesh->getNumOfElement(); ii++){
			
			CElement* pElem= pMesh->getElement(ii);
			string sType=pElem->getCType();
			size_t nType=pElem->getNType();

			//if( regex_search(sType, res, regBasis) ){
			if( regex_match(sType, regBasis)){
				//�v�f�^�C�v�@ID  �ߓ_ID  �ߓ_ID  �ߓ_ID  �ߓ_ID ... ...
				//ofs << format(" %s %9lu ") %res.str()  %pElem->getID();
				ofs << format(" %s %9lu ") %sType  %pElem->getID();
				for(size_t iii=0; iii < pElem->getNumOfNode(); iii++){
					ofs << format(" %9lu")  %pElem->getNodeID_Fistr2MW3(iii);
				};
				ofs << endl;
			}else if( regex_search(sType, res, regShell) ){
				//�V�F���v�f��Quad,Triangle�ɕϊ�
				string sRepType;
				switch(nType){
				case(FistrElementType::TriShell):   sRepType=FistrElemTypeS::Triangle();   break;
				case(FistrElementType::TriShell2):  sRepType=FistrElemTypeS::Triangle2();  break;
				case(FistrElementType::QuadShell):  sRepType=FistrElemTypeS::Quad();  break;
				case(FistrElementType::QuadShell2): sRepType=FistrElemTypeS::Quad2(); break;
				default:
					pMsg->error(res.str()+" ? ");//regex����������΁A���̏����ɂ͓���Ȃ�.
					break;
				}
				//�v�f�^�C�v(�u��)�@ID  �ߓ_ID  �ߓ_ID  �ߓ_ID  �ߓ_ID ... ...
				ofs << format(" %s %9lu ") %sRepType %pElem->getID();
				for(size_t iii=0; iii < pElem->getNumOfNode(); iii++){
					ofs << format(" %9lu") %pElem->getNodeID_Fistr2MW3(iii);
				};
				ofs << endl;
			}else{
				pMsg->error( res.str()+" not supported");//Error --- IFaceQuad���܂�.
			}
		};
		ofs << "End\n" << endl;
	};

	// ���b�Z�[�W�\��
	string sNumMesh= lexical_cast<string>(nNumMesh);
	pMsg->info("wrote :"+sNumMesh+" block \"Element\"");

	return true;
}
//--
// BoundaryNodeMesh : �f�t�H���g��Dirichlet�^�ŏo��(���R:FrontISTR�͋��E�l���g�p���Ȃ�)
//--
bool CFileWriter::WriteBndNodeMesh(CAssyModel *pAssyModel, ofstream& ofs)
{
	if(!pAssyModel->existMesh()) return false;//Mesh������

	CMessage *pMsg=CMessage::Instance();

	size_t nNMesh=pAssyModel->getNumOfMesh();
	size_t nSumGrpN=0;
	string sBndType;

	for(size_t i=0; i < nNMesh; i++){
		CMesh *pMesh=pAssyModel->getMesh(i);
		/*
		�^�O�FBoundaryNodeMesh
		����
		���b�V���p�[�cID  ���E������
		�ߓ_���E���b�V��ID  ���E������ށ@���� ������(DOF��) DOF ���� DOF ���� ... # ����:ver0.4�ȍ~
		*/
		size_t nNNgrp=pMesh->getNumOfNgrp();
		nSumGrpN += nNNgrp;

		if(nNNgrp > 0){
			ofs << "BoundaryNodeMesh" << endl;
			ofs << format(" %lu %lu") %pMesh->getID() %nNNgrp << endl;
		
			for(size_t ii=0; ii < nNNgrp; ii++){
				CNgroup *pNgrp= pMesh->getNgrp(ii);

				switch(pNgrp->getBndType()){ //���E��ނ���H
				case( BndType::Dirichlet ): sBndType = msDirichlet; break;
				case( BndType::Neumann ): sBndType = msNeumann; break;
				case( BndType::NotUse ): sBndType = msDirichlet; break;
				default: 
					sBndType = msDirichlet;
					break;
				}
			
				ofs << format(" %lu %s %s %lu") %pNgrp->getID() %sBndType %pNgrp->getGroupName() %0 << endl;//������:0(���f�[�^�ɖ���)
			};
			ofs << "End\n" << endl;
		}
	};

	
	for(size_t i=0; i < nNMesh; i++){
		CMesh *pMesh=pAssyModel->getMesh(i);
		/*
		�^�O�FBoundaryNode
		����( �f�B���N��&�m�C�}�� ���� )
		�ߓ_���E���b�V��ID�@���E�������  ���b�V���p�[�cID  ���E�ߓ_��
		���E�ߓ_ID  �ߓ_ID  X  Y  Z  ���R�x�ԍ� ���E�l(FrontISTR�͒l���g�p���Ȃ��ˏ��0.0)
		*/
		size_t nNNgrp=pMesh->getNumOfNgrp();
		for(size_t ii=0; ii < nNNgrp; ii++){
			CNgroup *pNgrp= pMesh->getNgrp(ii);

			switch(pNgrp->getBndType()){ //���E��ނ���H
			case( BndType::Dirichlet ): sBndType = msDirichlet; break;
			case( BndType::Neumann ): sBndType = msNeumann; break;
			case( BndType::NotUse ): sBndType = msDirichlet; break;
			default: 
				sBndType = msDirichlet;
				break;
			}

			ofs << "BoundaryNode" << endl;
			ofs << format(" %lu %s %lu %lu") %pNgrp->getID() %sBndType %pMesh->getID() %pNgrp->getNumOfNode() << endl;
			for(size_t iii=0; iii < pNgrp->getNumOfNode(); iii++){
				CNode* pNode=pNgrp->getNode(iii);
				ofs << format(" %9lu %9lu %23.15e %23.15e %23.15e %lu   %lf") %iii %pNode->getID() %pNode->getX() %pNode->getY() %pNode->getZ() %0 %0.0 << endl;
			};
			ofs << "End\n" << endl;
		};
	};


	// ���b�Z�[�W�\��
	string sSumGrpN= lexical_cast<string>(nSumGrpN);
	pMsg->info("wrote :"+ sSumGrpN +" block \"BoundaryNode\"");

	return true;
}
//--
// BoundaryFaceMesh : �f�t�H���g��Neumann�^�ŏo��(���R:FrontISTR�͋��E�l���g�p���Ȃ�)
//--
bool CFileWriter::WriteBndFaceMesh(CAssyModel *pAssyModel, ofstream& ofs)
{
	if(!pAssyModel->existMesh()) return false;//Mesh������

	CMessage *pMsg=CMessage::Instance();

	size_t nNMesh=pAssyModel->getNumOfMesh();
	size_t nSumGrpN=0;
	string sBndType;
	/*
	�^�O�FBoundaryFaceMesh
	����
	���b�V���p�[�cID  ���E������
	�ʋ��E���b�V��ID  ���E������ށ@����  ���R�x�� ���R�x�ԍ� ���R�x�ԍ� ...(�J��Ԃ�) ������(DOF��) DOF ���� DOF ���� ...
	*/
	for(size_t i=0; i < nNMesh; i++){
		CMesh *pMesh=pAssyModel->getMesh(i);

		size_t nNSgrp=pMesh->getNumOfSgrp();
		nSumGrpN += nNSgrp;

		if(nNSgrp > 0){
			ofs << "BoundaryFaceMesh" << endl;
			ofs << format(" %lu %lu") %pMesh->getID() %nNSgrp << endl;
			for(size_t ii=0; ii < nNSgrp; ii++){
				CSgroup* pSgrp=pMesh->getSgrp(ii);

				switch(pSgrp->getBndType()){ //���E��ނ���H
				case( BndType::Dirichlet ): sBndType = msDirichlet; break;
				case( BndType::Neumann ): sBndType = msNeumann; break;
				case( BndType::NotUse ): sBndType = msNeumann; break;
				default: 
					sBndType = msNeumann;
					break;
				}

				ofs << format(" %lu %s %s %lu %lu %lu") %pSgrp->getID() %sBndType %pSgrp->getGroupName() %1 %0 %0 << endl;//���R�x��:1,���R�x:0,������:0
			};
			ofs << "End\n" << endl;
		}
	};
	//--
	// BoundaryFace
	//--
	for(size_t i=0; i < nNMesh; i++){
		CMesh *pMesh=pAssyModel->getMesh(i);
		size_t nNSgrp=pMesh->getNumOfSgrp();
		
		for(size_t ii=0; ii < nNSgrp; ii++){
			CSgroup *pSgrp= pMesh->getSgrp(ii);

			if(pSgrp->getBndType()!=BndType::Dirichlet){
				/*
				�^�O�FBoundaryFace
				����( �m�C�}���^ )
				�ʋ��E���b�V��ID�@���E�������  ���b�V���p�[�cID  ���E�ߓ_�� Face��
				���E�ߓ_ID  �ߓ_ID  
				�c
				�ʌ`��^�C�v ���E��ID  �v�fID  �ʔԍ�  ���R�x�ԍ� ���E�ߓ_ID...(�J��Ԃ�)  ���E�l
				*/
				ofs << "BoundaryFace" << endl;
				ofs << format(" %lu %s %5lu %5lu %5lu") %pSgrp->getID() %msNeumann %pMesh->getID() %pSgrp->getNumOfNode() %pSgrp->getNumOfFace() << endl;
				// BNode 
				size_t nNNode=pSgrp->getNumOfNode();
				for(size_t inode=0; inode < nNNode; inode++){
					CNode* pNode = pSgrp->getNode(inode);
					ofs << format(" %5lu %5lu") %inode %pNode->getID() << endl;
				};

				// Face
				size_t nNFace=pSgrp->getNumOfFace();
				for(size_t iface=0; iface < nNFace; iface++){
				
					ofs << format(" %s %5lu %5lu %5lu %5lu") %pSgrp->getFaceType(iface) %iface %pSgrp->getElemID(iface) %pSgrp->getConvMW3FaceN(iface) %0;

					vector<size_t> vBNode = pSgrp->getBNodeN_Face(iface);
					for(size_t ibnode=0; ibnode < vBNode.size(); ibnode++) ofs << format(" %5lu") %vBNode[ibnode];
					ofs << format("   %10.3e") %0.000 << endl;//���E�l
				};
				ofs << "End\n" << endl;
			}else{
				/*
				�^�O�FBoundaryFace
				����( �f�B���N���^ )
				�ʋ��E���b�V��ID�@���E�������  ���b�V���p�[�cID  ���E�ߓ_�� Face��
				���E�ߓ_ID  �ߓ_ID  DOF���@ DOF�ԍ� ���E�l DOF�ԍ� ���E�l DOF�ԍ� ���E�l�c
				�c
				�ʌ`��^�C�v ���E��ID  �v�fID  �ʔԍ�  �@ ���E�ߓ_ID...(�J��Ԃ�)  
				�c
				*/
				ofs << "BoundaryFace" << endl;
				ofs << format(" %lu %s %5lu %5lu %5lu") %pSgrp->getID() %msDirichlet %pMesh->getID() %pSgrp->getNumOfNode() %pSgrp->getNumOfFace() << endl;
				// BNode 
				size_t nNNode=pSgrp->getNumOfNode();
				for(size_t inode=0; inode < nNNode; inode++){
					CNode* pNode = pSgrp->getNode(inode);
					ofs << format(" %5lu %5lu %lu %lu %lf") %inode %pNode->getID() %1 %0 %0.000 << endl;
				};
				// Face
				size_t nNFace=pSgrp->getNumOfFace();
				for(size_t iface=0; iface < nNFace; iface++){
				
					ofs << format(" %s %5lu %5lu %5lu") %pSgrp->getFaceType(iface) %iface %pSgrp->getElemID(iface) %pSgrp->getConvMW3FaceN(iface);

					vector<size_t> vBNode = pSgrp->getBNodeN_Face(iface);
					for(size_t ibnode=0; ibnode < vBNode.size(); ibnode++) ofs << format(" %5lu") %vBNode[ibnode];
					ofs << endl;
				};
				ofs << "End\n" << endl;
			}//if(BndType)
		};
	};


	// ���b�Z�[�W�\��
	string sSumGrpN= lexical_cast<string>(nSumGrpN);
	pMsg->info("wrote :"+ sSumGrpN +" block \"BoundaryFace\"");

	return true;
}
//--
// BoundaryEdgeMesh : �f�t�H���g��Neumann�^�ŏo��(���R:FrontISTR�͋��E�l���g�p���Ȃ�)
//--
bool CFileWriter::WriteBndEdgeMesh(CAssyModel *pAssyModel, ofstream& ofs)
{
	if(!pAssyModel->existMesh()) return false;//Mesh������

	CMessage *pMsg=CMessage::Instance();

	size_t nNMesh=pAssyModel->getNumOfMesh();
	size_t nSumGrpN=0;
	string sBndType;
	/*
	�^�O�FBoundaryEdgeMesh
	����
	���b�V���p�[�cID  ���E������
	�Ӌ��E���b�V��ID  ���E������ށ@����  ���R�x�� ���R�x�ԍ� ���R�x�ԍ� ...(�J��Ԃ�) ������(DOF��) DOF ���� DOF ���� ...
	�c
	*/
	for(size_t i=0; i < nNMesh; i++){
		CMesh *pMesh=pAssyModel->getMesh(i);

		size_t nNLgrp=pMesh->getNumOfLgrp();
		nSumGrpN += nNLgrp;

		if(nNLgrp > 0){
			ofs << "BoundaryEdgeMesh" << endl;
			ofs << format(" %lu %lu") %pMesh->getID() %nNLgrp << endl;
			for(size_t ii=0; ii < nNLgrp; ii++){
				CLgroup* pLgrp=pMesh->getLgrp(ii);

				switch(pLgrp->getBndType()){ //���E��ނ���H
				case( BndType::Dirichlet ): sBndType = msDirichlet; break;
				case( BndType::Neumann ): sBndType = msNeumann; break;
				case( BndType::NotUse ): sBndType = msNeumann; break;
				default: 
					sBndType = msNeumann;
					break;
				}

				ofs << format(" %lu %s %s %lu %lu %lu") %pLgrp->getID() %sBndType %pLgrp->getGroupName() %1 %0 %0 << endl;//���R�x��:1,���R�x:0,������:0
			};
			ofs << "End\n" << endl;
		}
	};


	for(size_t i=0; i < nNMesh; i++){
		CMesh *pMesh=pAssyModel->getMesh(i);

		size_t nNLgrp=pMesh->getNumOfLgrp();
		
		for(size_t ii=0; ii < nNLgrp; ii++){
			CLgroup *pLgrp= pMesh->getLgrp(ii);
			if(pLgrp->getBndType()!=BndType::Dirichlet){
				/*
				�^�O�FBoundaryEdge
				���� ( �m�C�}�� )
				�Ӌ��E���b�V��ID�@���E�������  ���b�V���p�[�cID  ���E�ߓ_���@Edge��
				���E�ߓ_ID  �ߓ_ID 
				�c
				�ӌ`��^�C�v ���E��ID  �v�fID  �Ӕԍ�  ���R�x�ԍ� ���E�ߓ_ID ���E�ߓ_ID ���E�l
				�c
				*/
				ofs << "BoundaryEdge" << endl;
				ofs << format(" %lu %s %5lu %5lu %5lu") %pLgrp->getID() %msNeumann %pMesh->getID() %pLgrp->getNumOfNode() %pLgrp->getNumOfEdge() << endl;
				// BNode 
				size_t nNNode=pLgrp->getNumOfNode();
				for(size_t inode=0; inode < nNNode; inode++){
					CNode* pNode = pLgrp->getNode(inode);
					ofs << format(" %5lu %5lu") %inode %pNode->getID() << endl;
				};
				// Edge
				size_t nNEdge=pLgrp->getNumOfEdge();
				for(size_t iedge=0; iedge < nNEdge; iedge++){
					ofs << format(" %s %5lu %5lu %5lu %5lu") %pLgrp->getEdgeType(iedge) %iedge %pLgrp->getElemID(iedge) %pLgrp->getConvMW3EdgeN(iedge) %0;

					vector<size_t> vBNode = pLgrp->getBNode_Edge(iedge);
					for(size_t ibnode=0; ibnode < vBNode.size(); ibnode++) ofs << format(" %5lu") %vBNode[ibnode];
					ofs << format("   %10.3e") %0.000 << endl;//���E�l
				};
				ofs << "End\n" << endl;
			}else{
				/*
				�^�O�FBoundaryEdge
				���� ( �f�B���N�� )
				�Ӌ��E���b�V��ID�@���E�������  ���b�V���p�[�cID  ���E�ߓ_���@Edge��
				���E�ߓ_ID  �ߓ_ID  DOF���@ DOF�ԍ� ���E�l DOF�ԍ� ���E�l DOF�ԍ� ���E�l�c
				�c
				�ӌ`��^�C�v ���E��ID  �v�fID  �Ӕԍ�  ���E�ߓ_ID ���E�ߓ_ID 
				�c
				*/
				ofs << "BoundaryEdge" << endl;
				ofs << format(" %lu %s %5lu %5lu %5lu") %pLgrp->getID() %msDirichlet %pMesh->getID() %pLgrp->getNumOfNode() %pLgrp->getNumOfEdge() << endl;
				// BNode 
				size_t nNNode=pLgrp->getNumOfNode();
				for(size_t inode=0; inode < nNNode; inode++){
					CNode* pNode = pLgrp->getNode(inode);
					ofs << format(" %5lu %5lu %lu %lu %lf") %inode %pNode->getID() %1 %0 %0.000 << endl;
				};
				// Edge
				size_t nNEdge=pLgrp->getNumOfEdge();
				for(size_t iedge=0; iedge < nNEdge; iedge++){
					ofs << format(" %s %5lu %5lu %5lu") %pLgrp->getEdgeType(iedge) %iedge %pLgrp->getElemID(iedge) %pLgrp->getConvMW3EdgeN(iedge);

					vector<size_t> vBNode = pLgrp->getBNode_Edge(iedge);
					for(size_t ibnode=0; ibnode < vBNode.size(); ibnode++) ofs << format(" %5lu") %vBNode[ibnode];
					ofs << endl;
				};
				ofs << "End\n" << endl;
			}//if(BndType)
		};
	};

	// ���b�Z�[�W�\��
	string sSumGrpN= lexical_cast<string>(nSumGrpN);
	pMsg->info("wrote :"+ sSumGrpN +" block \"BoundaryEdge\"");

	return true;
}
//--
// BoundaryVolumeMesh : �f�t�H���g��Neumann�^�ŏo��(���R:FrontISTR�͋��E�l���g�p���Ȃ�)
//--
bool CFileWriter::WriteBndVolMesh(CAssyModel *pAssyModel, ofstream& ofs)
{
	if(!pAssyModel->existMesh()) return false;//Mesh������

	CMessage* pMsg=CMessage::Instance();

	size_t nNMesh=pAssyModel->getNumOfMesh();
	size_t nSumGrpN=0;
	string sBndType;
	/*
	�^�O�FBoundaryVolumeMesh
	����
	���b�V���p�[�cID  ���E������
	�̐ϋ��E���b�V��ID  ���E������ށ@����  ���R�x�� ���R�x�ԍ� ���R�x�ԍ� ...(�J��Ԃ�) ������(DOF��) DOF ���� DOF ���� ...
	�c
	*/
	for(size_t i=0; i < nNMesh; i++){
		CMesh *pMesh=pAssyModel->getMesh(i);
		nSumGrpN += pMesh->getNumOfEgrp();

		if(pMesh->getNumOfEgrp() > 0){
			ofs << "BoundaryVolumeMesh" << endl;
			ofs << format(" %lu %lu") %pMesh->getID() %pMesh->getNumOfEgrp() << endl;
			for(size_t ii=0; ii < pMesh->getNumOfEgrp(); ii++){
				CEgroup *pEgrp=pMesh->getEgrp(ii);

				switch(pEgrp->getBndType()){ //���E��ނ���H
				case( BndType::Dirichlet ): sBndType = msDirichlet; break;
				case( BndType::Neumann ): sBndType = msNeumann; break;
				case( BndType::NotUse ): sBndType = msNeumann; break;
				default: 
					sBndType = msNeumann;
					break;
				}

				ofs << format(" %lu %s %s %lu %lu %lu") %pEgrp->getID() %sBndType %pEgrp->getGroupName() %1 %0 %0 << endl;//���R�x��:1,���R�x:0,������:0
			};
			ofs << "End\n" << endl;
		}
	};

	// regex
	string strRegex= FistrElemTypeS::regStandard();// MW3�ɑ��݂���v�f�^�C�v
	sregex regBasis = sregex::compile(strRegex);
	string strRegex2= FistrElemTypeS::regShellType();// �V�F���v�f
	sregex regShell= sregex::compile(strRegex2);
	smatch res;

	for(size_t i=0; i < nNMesh; i++){
		CMesh *pMesh=pAssyModel->getMesh(i);
		size_t nNEgrp= pMesh->getNumOfEgrp();
		for(size_t ii=0; ii < nNEgrp; ii++){
			CEgroup *pEgrp=pMesh->getEgrp(ii);

			if(pEgrp->getBndType()!=BndType::Dirichlet){
				/*
				�^�O�FBoundaryVolume
				���� ( �m�C�}�� )
				�̐ϋ��E���b�V��ID�@���E�������  ���b�V���p�[�cID  ���E�ߓ_�� �v�f��
				���E�ߓ_ID  �ߓ_ID  
				�c
				�`��^�C�v   ���E�̐�ID  �v�fID  0  ���R�x�ԍ� ���E�ߓ_ID...(�J��Ԃ�)   ���E�l
				�c
				*/
				ofs << "BoundaryVolume" << endl;
				ofs << format(" %lu %s %5lu %5lu %5lu") %pEgrp->getID() %msNeumann %pMesh->getID() %pEgrp->getNumOfNode() %pEgrp->getNumOfElement() << endl;
				// Node
				for(size_t inode=0; inode < pEgrp->getNumOfNode(); inode++){
					ofs << format(" %5lu %5lu") %inode %pEgrp->getNodeID(inode) << endl;
				};
				// Volume(Element)
				for(size_t ielem=0; ielem < pEgrp->getNumOfElement(); ielem++){

					CElement *pElem= pEgrp->getElement(ielem);
					string sType= pElem->getCType();
				
					if( regex_search(sType,res,regBasis) ){
						// �v�f�`��MW�R�ɑ���.
						ofs << format(" %s %5lu %5lu %5lu %5lu") %sType %ielem %pElem->getID() %0 %0 ;
					}else if( regex_search(sType,res,regShell) ){
						// �V�F���v�f�˒u������
						if(sType==FistrElemTypeS::TriShell()) sType=FistrElemTypeS::Triangle();
						if(sType==FistrElemTypeS::TriShell2())sType=FistrElemTypeS::Triangle2();
						if(sType==FistrElemTypeS::QuadShell())sType=FistrElemTypeS::Quad();
						if(sType==FistrElemTypeS::QuadShell2())sType=FistrElemTypeS::Quad2();
						ofs << format(" %s %5lu %5lu %5lu %5lu") %sType %ielem %pElem->getID() %0 %0 ;
					}else{
						// �K�i�O
						pMsg->error( res.str()+" not supported");//Error --- IFaceQuad���܂�.
					}
					// �v�f�\��BNode
					for(size_t enode=0; enode < pElem->getNumOfNode(); enode++){
						size_t nID= pElem->getNodeID_Fistr2MW3(enode);
						size_t nIX= pEgrp->getBNodeIndex(nID);
						ofs << format(" %5lu") %nIX;
					};
					ofs << format("   %10.3e") %0.000 << endl;//���E�l
				};
				ofs << "End\n" << endl;
			}else{
				/*
				�^�O�FBoundaryVolume
				���� ( �f�B���N�� )
				�̐ϋ��E���b�V��ID�@���E�������  ���b�V���p�[�cID  ���E�ߓ_�� �v�f��
				���E�ߓ_ID  �ߓ_ID  DOF���@ DOF�ԍ� ���E�l DOF�ԍ� ���E�l DOF�ԍ� ���E�l�c
				�c
				�`��^�C�v   ���E�̐�ID  �v�fID  0  �@���E�ߓ_ID...(�J��Ԃ�)   
				�c
				*/
				ofs << "BoundaryVolume" << endl;
				ofs << format(" %lu %s %5lu %5lu %5lu") %pEgrp->getID() %msDirichlet %pMesh->getID() %pEgrp->getNumOfNode() %pEgrp->getNumOfElement() << endl;
				// Node
				for(size_t inode=0; inode < pEgrp->getNumOfNode(); inode++){
					ofs << format(" %5lu %5lu %lu %lu %lf") %inode %pEgrp->getNodeID(inode) %1 %0 %0.000 << endl;
				};
				// Volume(Element)
				for(size_t ielem=0; ielem < pEgrp->getNumOfElement(); ielem++){

					CElement *pElem= pEgrp->getElement(ielem);
					string sType= pElem->getCType();
				
					if( regex_search(sType,res,regBasis) ){
						// �v�f�`��MW�R�ɑ���.
						ofs << format(" %s %5lu %5lu %5lu") %sType %ielem %pElem->getID() %0 ;
					}else if( regex_search(sType,res,regShell) ){
						// �V�F���v�f�˒u������
						if(sType==FistrElemTypeS::TriShell()) sType=FistrElemTypeS::Triangle();
						if(sType==FistrElemTypeS::TriShell2())sType=FistrElemTypeS::Triangle2();
						if(sType==FistrElemTypeS::QuadShell())sType=FistrElemTypeS::Quad();
						if(sType==FistrElemTypeS::QuadShell2())sType=FistrElemTypeS::Quad2();
						ofs << format(" %s %5lu %5lu %5lu") %sType %ielem %pElem->getID() %0;
					}else{
						// �K�i�O
						pMsg->error( res.str()+" not supported");//Error --- IFaceQuad���܂�.
					}
					// �v�f�\��BNode
					for(size_t enode=0; enode < pElem->getNumOfNode(); enode++){
						size_t nID= pElem->getNodeID_Fistr2MW3(enode);
						size_t nIX= pEgrp->getBNodeIndex(nID);
						ofs << format(" %5lu") %nIX;
					};
					ofs << endl;
				};
				ofs << "End\n" << endl;
			}//if(BndType)
		};
	};


	// ���b�Z�[�W�\��
	string sSumGrpN= lexical_cast<string>(nSumGrpN);
	pMsg->info("wrote :"+ sSumGrpN +" block \"BoundaryVolume\"");

	return true;
}
//--
// ElementGroup
//   �� BoundaryVolumeMesh�f�[�^��ElementGroup�Ƃ��ēǂݑւ��ďo��
//--
bool CFileWriter::WriteElemGroup(CAssyModel *pAssyModel, ofstream& ofs)
{
	if(!pAssyModel->existMesh()) return false;//Mesh������

	CMessage* pMsg=CMessage::Instance();

	size_t nNMesh=pAssyModel->getNumOfMesh();
	size_t nSumGrpN=0;

	/*
	ElementGroup
		�O���[�v�ԍ�  �O���[�v��  ���b�V���p�[�c�ԍ�
		�c
	End
	*/
	for(size_t i=0; i < nNMesh; i++){
		CMesh *pMesh=pAssyModel->getMesh(i);
		nSumGrpN += pMesh->getNumOfEgrp();
	};
	if(nSumGrpN > 0){
		ofs << "ElementGroup" << endl;
		for(size_t i=0; i < nNMesh; i++){
			CMesh *pMesh=pAssyModel->getMesh(i);
			for(size_t ii=0; ii < pMesh->getNumOfEgrp(); ii++){
				CEgroup *pEgrp=pMesh->getEgrp(ii);
				ofs << format(" %lu %s %lu") %pEgrp->getID() %pEgrp->getGroupName() %pMesh->getID() << endl;
			};//---for(�O���[�v��)
		};//---for(���b�V����)end
		ofs << "End\n" << endl;
	}

	/*
	ElementGroupEntity
		�O���[�v�ԍ�  ���b�V���p�[�c�ԍ�
		�v�fID  �v�fID  �v�fID  �v�fID  �v�fID
		�v�fID  �v�fID  �v�fID  �v�fID  �v�fID
		�c
	End
	*/
	if(nSumGrpN > 0){
		for(size_t i=0; i < nNMesh; i++){
			CMesh *pMesh=pAssyModel->getMesh(i);
			for(size_t ii=0; ii < pMesh->getNumOfEgrp(); ii++){
				CEgroup *pEgrp=pMesh->getEgrp(ii);
				ofs << "ElementGroupEntity" << endl;
				ofs << format(" %lu %lu")  %pEgrp->getID() %pMesh->getID() << endl;

				for(size_t ielem=0; ielem < pEgrp->getNumOfElement(); ielem++){
					CElement *pElem= pEgrp->getElement(ielem);
					
					ofs << format(" %lu")  %pElem->getID();
					if(ielem!=0 && ielem%5==0) ofs << endl;
				};
				if((pEgrp->getNumOfElement()-1)%5!=0) ofs << endl;
				ofs << "End\n" << endl;
			};
		};
	}


	// ���b�Z�[�W�\��
	string sSumGrpN= lexical_cast<string>(nSumGrpN);
	pMsg->info("wrote :"+ sSumGrpN +" block \"ElementGroup\"");

	return true;
}

//--
// ContactMesh
//--
bool CFileWriter::WriteContactMesh(CAssyModel *pAssyModel, ofstream& ofs)
{
	if(!pAssyModel->existConPair() && !pAssyModel->existMPCPair()) return false;//MPC��Contact�̗����Ƃ�����

	CMessage *pMsg=CMessage::Instance();
	
	/*
	�^�O�FContactMesh
	����
	�ڍ����b�V���� �ő�ID  �ŏ�ID
	�ڍ����b�V��ID  ���gRank �@�����ԍ�(0:MPC, 1:Contact, ...)
	�ڍ��ߓ_�� �ő�ID  �ŏ�ID
	�ڍ��ߓ_ID  X  Y  Z  ���b�V���p�[�cID  �ߓ_ID  rank  maslave  v 3 0 ("v 3 0"�̓_�~�[�萔)
	�c
	�}�X�^�[�ڍ��ʐ�  �ő�ID  �ŏ�ID
	�}�X�^�[��ID ���b�V���p�[�cID  �v�fID  (�v�f)�ʔԍ�  �`��  �ڍ��ߓ_ID �c  ���gRank
	�c
	�X���[�u�ڍ��ʐ�  �ő�ID  �ŏ�ID
	�X���[�u��ID ���b�V���p�[�cID  �v�fID  (�v�f)�ʔԍ�  �`��  �ڍ��ߓ_ID �c  ���gRank
	�c
	
	�� maslave�F0:�}�X�^�[�A1:�X���[�u
	*/

	size_t nNumMPC= pAssyModel->getNumOfMPCPair();
	
	//MPC�S�� 
	ofs << "ContactMesh" << endl;
	ofs << format(" %lu %lu %lu") %nNumMPC %pAssyModel->getMaxIDinMPCPair() %pAssyModel->getMinIDinMPCPair() << endl;
	
	for(size_t i=0; i < nNumMPC; i++){
		// MPC�ڍ���(�P��-�ڍ���)
		ofs << format(" %lu %lu %lu") %i %0 %0 << endl;
		
		CGroupPair *pMPCPair= pAssyModel->getMPCPair(i);
		
		size_t nAllNNum= pMPCPair->getNumOfAllNode();   //���ߓ_��
		size_t nMNNum  = pMPCPair->getNumOfMasterNode();//�}�X�^�[�ߓ_��
		size_t nMMeshID= pMPCPair->getMasterMeshID();   //�}�X�^�[�p�[�cID
		size_t nSMeshID= pMPCPair->getSlaveMeshID();    //�X���[�u�p�[�cID
		// �ڍ��ߓ_
		ofs << format(" %9lu %9lu %9lu") %nAllNNum %pMPCPair->getMaxNodeIX() %pMPCPair->getMinNodeIX() << endl;
		for(size_t inode=0; inode < nAllNNum; inode++){

			CNode* pNode= pMPCPair->getNode(inode);
			double X=pNode->getX(), Y=pNode->getY(), Z=pNode->getZ();
			size_t nNID=pNode->getID();
			if(inode < nMNNum){
				//�}�X�^�[�_
				ofs << format(" %9lu %23.15e %23.15e %23.15e %lu %9lu %lu %lu %s") %inode %X %Y %Z %nMMeshID %nNID %0 %0 %"v 3 0" << endl;
			}else{
				//�X���[�u�_
				ofs << format(" %9lu %23.15e %23.15e %23.15e %lu %9lu %lu %lu %s") %inode %X %Y %Z %nSMeshID %nNID %0 %1 %"v 3 0" << endl;
			}
		};

		//�}�X�^�[��
		size_t nMFaceNum= pMPCPair->getNumOfMasterFace();
		size_t nMaxMFaceIX = pMPCPair->getMaxMFaceIX();
		size_t nMinMFaceIX = pMPCPair->getMinMFaceIX();
		ofs << format(" %9lu %9lu %9lu") %nMFaceNum %nMaxMFaceIX %nMinMFaceIX << endl;
		// �}�X�^�[��ID ���b�V���p�[�cID  �v�fID  (�v�f)�ʔԍ�  �`��  �ڍ��ߓ_ID �c  ���gRank
		for(size_t iface=0; iface < nMFaceNum; iface++){
			size_t nElemID= pMPCPair->getMasterElemID(iface);
			size_t nFaceN = pMPCPair->getMasterFaceN_MW3(iface);
			string sFaceTye=pMPCPair->getMasterFaceType(iface);
			ofs << format(" %9lu %9lu %9lu %9lu %s") %iface %nMMeshID %nElemID %nFaceN %sFaceTye;

			vector<size_t> vConIX= pMPCPair->getMFaceNodeIX(iface);
			for(size_t inode=0; inode < vConIX.size(); inode++){
				ofs << format(" %9lu") %vConIX[inode];
			};
			ofs << format(" %lu") %0 << endl;
		};

		//�X���[�u��
		size_t nSFaceNum= pMPCPair->getNumOfSlaveFace();
		size_t nMaxSFaceIX= pMPCPair->getMaxSFaceIX();
		size_t nMinSFaceIX= pMPCPair->getMinSFaceIX();
		ofs << format(" %9lu %9lu %9lu") %nSFaceNum %nMaxSFaceIX %nMinSFaceIX << endl;
		// �X���[�u��ID ���b�V���p�[�cID  �v�fID  (�v�f)�ʔԍ�  �`��  �ڍ��ߓ_ID �c  ���gRank
		for(size_t iface=0; iface < nSFaceNum; iface++){
			size_t nElemID= pMPCPair->getSlaveElemID(iface);
			size_t nFaceN = pMPCPair->getSlaveFaceN_MW3(iface);
			string sFaceTye=pMPCPair->getSlaveFaceType(iface);
			ofs << format(" %9lu %9lu %9lu %9lu %s") %iface %nSMeshID %nElemID %nFaceN %sFaceTye;

			vector<size_t> vConIX= pMPCPair->getSFaceNodeIX(iface);
			for(size_t inode=0; inode < vConIX.size(); inode++){
				ofs << format(" %9lu") %vConIX[inode];
			};
			ofs << format(" %lu") %0 << endl;
		};

	};
	ofs << "End\n" << endl;

	string sNumMPC= lexical_cast<string>(nNumMPC);
	pMsg->info("wrote :"+sNumMPC+" block \"ContactMesh(MPC)\"" );
	
	return true;
}


//--
// MW3 ���b�V���o��
//--
void CFileWriter::WriteMesh_MW3(CAssyModel *pAssyModel, string filename)
{
	CMessage *pMsg=CMessage::Instance();

	string strMW3std  = FileName::regMW3Mesh();
	string strMW3fistr= FileName::regMW3FistrMesh();
	sregex regMW3std  = sregex::compile(strMW3std);
	sregex regMW3fistr= sregex::compile(strMW3fistr);
	smatch res1, res2;
	//--
	// �t�@�C�����`�F�b�N : rank�ԍ�"0"�̕t����
	//--
	// 1.MW3�W��          ���b�V���t�@�C����:".0.msh"
	// 2.MW3FrontISTR�Ή� ���b�V���t�@�C����:".msh.0"
	//
	if(!regex_search(filename, res1, regMW3std) && !regex_search(filename, res2, regMW3fistr) ){
		// Warn
		pMsg->warn("invalid output file extension.");

		string user_str;
		pMsg->user_in("select file extension kind: enter STD or FISTR => ", user_str);//----------------------- ���[�U�[�I��
		transform(user_str.begin(), user_str.end(), user_str.begin(), ::tolower);//-- �S�ď������ɕϊ� 

		string strExt = FileName::regExt();
		sregex regExt = sregex::compile(strExt);
		smatch resExt;
		
		if( regex_search(filename, resExt, regExt) ){
			//--
			// �g���q".msh"���t���Ă��邪rank�ԍ��͖����ꍇ
			//--
			if(user_str=="std"){//MW3�W��
				filename = regex_replace(filename, regExt, string(".0.msh") );//".msh"��".0.msh"�ɒu��
			}else if(user_str=="fistr"){//MW3FrontISTR�Ή�
				filename = regex_replace(filename, regExt, string(".msh.0") );//".msh"��".msh.0"�ɒu��
			}else{
				pMsg->error("input error, change to std type.");
				filename = regex_replace(filename, regExt, string(".0.msh") );//".msh"��".0.msh"�ɒu��
			}
		}else{
			//--
			// �g���q��.msh����Ȃ��ꍇ
			//--
			if(user_str=="std"){   filename += ".0.msh";}else
			if(user_str=="fistr"){ filename += ".msh.0";}
			else{ pMsg->error("input error, change to std type."); filename += ".0.msh";}
		}
	}//if(�g���q��rank�ԍ��̗L��) 


	ofstream ofs;
	ofs.open(filename.c_str());// Open
	//--
	// �t�@�C���o��
	//--
	if(ofs){
		pMsg->info("file open:\""+filename+"\"");

		WriteHeader(ofs);
		WriteAssyModel(pAssyModel, ofs);//---AssyModel
		WriteNode(pAssyModel, ofs);     //---Node
		WriteElement(pAssyModel, ofs);  //---Element
		WriteBndNodeMesh(pAssyModel, ofs);//-BoundaryNodeMesh
		WriteBndFaceMesh(pAssyModel, ofs);//-BoundaryFaceMesh
		WriteBndEdgeMesh(pAssyModel, ofs);//-BoundaryEdgeMesh
		////WriteBndVolMesh(pAssyModel, ofs); //-BoundaryVolumeMesh �� fistr v4 �͕s�g�p
		WriteElemGroup(pAssyModel, ofs);//---ElementGroup �� BoundaryVolumeMesh�̓ǂݑւ�
		//
		// # �ʐM�E�ʂ̓V���O���Ȃ̂Ŗ��� : CommMesh2
		//
		WriteContactMesh(pAssyModel, ofs);//-ContactMesh

		pMsg->info("file close:\""+filename+"\"\n");
	}else{
		pMsg->error("\""+filename+"\" could not open.");
		exit(0);//------------------------------------------ exit(0)
	}

	ofs.close();// Close
}



