/*
 ----------------------------------------------------------
|
| Software Name :conv2mw3 Ver 0.1 beta
|
|   FrontISTR_Type.h
|
|                     Written by T.Takeda,    2012/06/01
|                                Y.Sato,      2012/06/01
|
|   Contact address : IIS, The University of Tokyo CISS
|
 ----------------------------------------------------------
*/
#ifndef E8F2AD0_FFFD_4ce7_B818_51DA3511C5DF
#define E8F2AD0_FFFD_4ce7_B818_51DA3511C5DF

//--
// �v�f�^�C�v
//--
struct FistrElementType{
  enum{
		Beam=111,
    Beam2=112,
		Triangle=231,
    Triangle2=232,
    Quad=241,
    Quad2=242,
    Tetra=341,
    Tetra2=342,
    Prism=351,
    Prism2=352,
    Hexa=361,
    Hexa2=362,
		IFaceQuad=541,
		IFaceQuad2=542,
		TriShell=731,
    TriShell2=732,
    QuadShell=741,
		QuadShell2=742
  };
};

#include <string>
//--
// �v�f�^�C�v(string)
//--
struct FistrElemTypeS{

	// MW3 �ɑ��݂���v�f�^�C�v
	static string Beam(){ return "Beam";}
	static string Beam2(){ return "Beam2";}
	static string Triangle(){ return "Triangle";}
	static string Triangle2(){ return "Triangle2";}
	static string Quad(){ return "Quad";}
	static string Quad2(){ return "Quad2";}
	static string Tetra(){ return "Tetra";}
	static string Tetra2(){ return "Tetra2";}
	static string Prism(){ return "Prism";}
	static string Prism2(){ return "Prism2";}
	static string Hexa(){ return "Hexa";}
	static string Hexa2(){ return "Hexa2";}
	// MW3 �ɑ��݂���v�f�^�C�v regex
	static string regStandard(){ return "Beam|Beam2|Triangle|Triangle2|Quad|Quad2|Tetra|Tetra2|Prism|Prism2|Hexa|Hexa2";}

	// MW3 �ɑ��݂��Ȃ��v�f�^�C�v
	static string IFaceQuad(){ return "IFaceQuad";}
	static string IFaceQuad2(){ return "IFaceQuad2";}
	static string TriShell(){ return "TriShell";}
	static string TriShell2(){ return "TriShell2";}
	static string QuadShell(){ return "QuadShell";}
	static string QuadShell2(){ return "QuadShell2";}
	// MW3 �ɑ��݂��Ȃ��v�f�^�C�v regex
	static string regNonStandard(){ return "IFaceQuad|IFaceQuad2|TriShell|TriShell2|QuadShell|QuadShell2";}

	// �V�F���v�f�^�C�v
	static string regShellType(){ return "TriShell|TriShell2|QuadShell|QuadShell2";}
	// �S���Ή��s�ȃ^�C�v
	static string regNonSupported(){ return "IFaceQuad|IFaceQuad2";}

	// ���ʁE�V�F��
	static string regPlate(){ return "TriShell|TriShell2|QuadShell|QuadShell2|Triangle|Triangle2|Quad|Quad2";}
	// ��
	static string regLine(){ return "Beam|Beam2";}

	static string Unknown(){ return "Unknown";}
};

//--
// �o�̓t�@�C�����`�F�b�N
//--
struct FileName{
	// MW3 �W�����b�V���E�t�@�C���̌㔼 ".0.msh"�ł��邱�Ƃ��m�F.
	static string regMW3Mesh(){ return "\\.0\\.msh$";}
	// MW3 FrontISTR�Ή����b�V���E�t�@�C���̌㔼 ".msh.0"�ł��邱�Ƃ��m�F.
	static string regMW3FistrMesh(){ return "\\.msh\\.0$";}

	// �g���q ".msh" �̊m�F.
	static string regExt(){ return "\\.msh$";}
};
//--
// MW3 ���E�������
//--
struct BndType{
	enum{
		Dirichlet,
		Neumann,
		Other,
		NotUse
	};
};

//--
// �t�@�C�������̃^�O : FrontISTR ver4.1
//--
struct FistrTag{
	
	// �w�b�_�[ 
	static string Header(){ return "!HEADER";}
	static string Node(){ return "!NODE";}
	static string Element(){ return "!ELEMENT";}
	static string Egroup(){ return "!EGROUP";}
	static string Sgroup(){ return "!SGROUP";}
	static string Ngroup(){ return "!NGROUP";}
	static string AssemblyPair(){ return "!ASSEMBLY_PAIR";}
	static string ContactPair(){ return "!CONTACT_PAIR";}
	static string End(){ return "!END";}
	static string Ex(){ return "!";} 

	// �p�����[�^ 
	static string Ver(){ return "VER";}
	static string PartsName(){ return "PARTNAME";}
	static string Num(){ return "NUM";}
	static string Type(){ return "TYPE";}
	static string Egrp(){ return "EGRP";}
	static string Sgrp(){ return "SGRP";}
	static string Ngrp(){ return "NGRP";}
	static string Name(){ return "NAME";}
	static string Equal(){ return "=";}
	static string Generate(){ return "GENERATE";}



	// �w�b�_�[ regex
	static string regHeader(){ return "^!HEADER";}
	static string regNode(){ return "^!NODE";}
	static string regElement(){ return "^!ELEMENT";}
	static string regEgroup(){ return "^!EGROUP";}
	static string regSgroup(){ return "^!SGROUP";}
	static string regNgroup(){ return "^!NGROUP";}
	static string regAssemblyPair(){ return "^!ASSEMBLY_PAIR";}
	static string regContactPair(){ return "^!CONTACT_PAIR";}
	static string regEnd(){ return "^!END";}

	static string regEx(){ return "^!.*";}
	static string regAllHeaders(){ return "!HEADER|!NODE|!ELEMENT|!EGROUP|!SGROUP|!NGROUP|!ASSEMBLY_PAIR|!CONTACT_PAIR|!END";}


	// �p�����[�^ regex
	static string regVer(){ return "VER.*";}
	static string regPartsName(){ return "PARTNAME.*";}
	static string regNum(){ return "NUM.*";}
	static string regType(){ return "TYPE.*";}
	static string regEgrp(){ return "EGRP.*";}
	static string regSgrp(){ return "SGRP.*";}
	static string regNgrp(){ return "NGRP.*";}
	static string regName(){ return "NAME.*";}
	static string regEqual(){ return "=";}

	// �o�[�W�����ԍ�(FrontISTR���b�V�������o�[�W����)
	static string VerNumber(){ return "4";}

};

struct MW3_Tag{
	// �o�[�W�����ԍ�(MW3 �t�@�C������)
	static string VerNumber(){ return "0.4";}
};

#endif //include_guard





