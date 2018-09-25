/*
 ----------------------------------------------------------
|
| Software Name :conv2mw3 Ver 0.1 beta
|
|   Sgroup.cpp
|
|                     Written by T.Takeda,    2012/06/01
|                                Y.Sato,      2012/06/01
|
|   Contact address : IIS, The University of Tokyo CISS
|
 ----------------------------------------------------------
*/
#include "Sgroup.h"
class CMesh;
CSgroup::CSgroup()
{
}
CSgroup::~CSgroup()
{
}
//--
//
//--
void CSgroup::addElemFaceID(size_t nElemID, size_t nFaceN)
{
	pair<size_t,size_t> nPair;

	nPair.first = nElemID;
	nPair.second= nFaceN;

	mvElemFace.push_back(nPair);
}


//--
// DATA�o�͏��� 
//--
bool CSgroup::setup(map<size_t,CNode*> mNode, map<size_t,CElement*> mElem)
{
	vector<size_t> vNID;//�ߓ_ID

	for(size_t i=0; i < mvElemFace.size(); i++){
		size_t nEID = mvElemFace[i].first; //FrontISTR �v�fID
		size_t nFace= mvElemFace[i].second;//FrontISTR �ʔԍ�
		
		CElement *pElem= mElem[nEID];
		vector<CNode*> vNode= pElem->getFistrFaceNode(nFace);//FrontISTR �ʔԍ��̐ߓ_�Q

		for(size_t ii=0; ii < vNode.size(); ii++){
			vNID.push_back( vNode[ii]->getID() );
		};
	};
	sort(vNID.begin(), vNID.end());
  vector<size_t>::iterator new_end = unique(vNID.begin(), vNID.end());
  vNID.erase(new_end, vNID.end());

	//�o�͗p Node* �z��
	for(size_t i=0; i < vNID.size(); i++){
		size_t nNID= vNID[i];
		mvNode.push_back( mNode[nNID] );
	};
	//�o�͗p NodeID��index
	for(size_t i=0; i < mvNode.size(); i++){
		CNode *pNode=mvNode[i];
		mmNID2NIX[pNode->getID()]=i;
	};
	//�ʂ̐ߓ_��
	for(size_t i=0; i < mvElemFace.size(); i++){
		size_t nEID = mvElemFace[i].first; //FrontISTR �v�fID
		size_t nFace= mvElemFace[i].second;//FrontISTR �ʔԍ�

		CElement *pElem= mElem[nEID];
		mvFaceNodeNum.push_back( pElem->getFaceNodeNum(nFace) );
	};
	//�ʂ̃^�C�v
	for(size_t i=0; i < mvElemFace.size(); i++){
		size_t nEID = mvElemFace[i].first; //FrontISTR �v�fID
		size_t nFace= mvElemFace[i].second;//FrontISTR �ʔԍ�

		CElement *pElem= mElem[nEID];
		mvFaceType.push_back( pElem->getFaceType(nFace) );
	};

	//�ʔԍ�(MW3)
	for(size_t i=0; i < mvElemFace.size(); i++){
		size_t nEID = mvElemFace[i].first;
		size_t nFace= mvElemFace[i].second;//FrontISTR �ʔԍ�

		CElement *pElem=mElem[nEID];

		size_t nMW3FaceN= pElem->getMW3FaceNum(nFace);
		mvMW3FaceNum.push_back(nMW3FaceN);
	};
	//�ʍ\����BNode�ԍ�(index�ԍ�)
	for(size_t i=0; i < mvElemFace.size(); i++){
		size_t nEID = mvElemFace[i].first;
		size_t nFace= mvElemFace[i].second;//FrontISTR �ʔԍ�

		CElement *pElem=mElem[nEID];

		vector<size_t> vIndex;//�ʍ\�� Node_Index
		vector<CNode*> vNode = pElem->getFistrFaceNode(nFace);//�ʍ\��Node*�z��
		for(size_t ii=0; ii < vNode.size(); ii++){
			CNode* pNode=vNode[ii];
			vIndex.push_back( mmNID2NIX[pNode->getID()] );
		};
		mvvBNodeNum.push_back(vIndex);
	};

	//�ʍ\����NodeID
	for(size_t i=0; i < mvElemFace.size(); i++){
		size_t nEID = mvElemFace[i].first;
		size_t nFace= mvElemFace[i].second;//FrontISTR �ʔԍ�

		CElement *pElem=mElem[nEID];

		vector<size_t> vNID;
		vector<CNode*> vNode = pElem->getFistrFaceNode(nFace);
		for(size_t ii=0; ii < vNode.size(); ii++){
			CNode* pNode=vNode[ii];
			vNID.push_back(pNode->getID());
		};
		mvvFaceNodeID.push_back(vNID);
	};

	return true;
}
//--
// �o��
//--
size_t CSgroup::getNumOfNode()
{
	return mvNode.size();
}
//--
CNode* CSgroup::getNode(size_t index)
{
	return mvNode[index];
}
//--
size_t CSgroup::getNumOfFace()
{
	return mvElemFace.size();
}
//--
size_t CSgroup::getElemID(size_t index)
{
	return mvElemFace[index].first;
}
//--
size_t CSgroup::getFaceN(size_t index)//�ʔԍ� ���͒l�̂܂�.
{
	return mvElemFace[index].second;
}
//--
size_t CSgroup::getFaceNodeNum(size_t index)//�ʂ̐ߓ_��
{
	return mvFaceNodeNum[index];
}
//--
string CSgroup::getFaceType(size_t index)//�ʂ̃^�C�v
{
	return mvFaceType[index];
}
//--
size_t CSgroup::getConvMW3FaceN(size_t index)//FrontISTR��MW3 �ʔԍ�
{
	return mvMW3FaceNum[index];
}
//--
vector<size_t> CSgroup::getBNodeN_Face(size_t index)//�ʂ��\������BoundaryNode�ԍ�(mvNode��index�ԍ�)
{
	return mvvBNodeNum[index];
}
//--
vector<size_t> CSgroup::getFaceNodeID(size_t index)
{
	return mvvFaceNodeID[index];
}




