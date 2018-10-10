/*
 ----------------------------------------------------------
|
| Software Name :HEC-MW Ver 4.3 beta
|
|   ./src/ShellBase.cpp
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
#include "ShellBase.h"
#include "ElementType.h"
using namespace pmw;
uiint CShellBase::mnBaseType = BaseElementType::Shell;
CShellBase::CShellBase():CElement()
{
    ;
}
CShellBase::~CShellBase()
{
}