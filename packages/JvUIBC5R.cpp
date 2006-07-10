//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvUIBC5R.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\source\JvUIB.pas");
USEUNIT("..\source\JvUIBase.pas");
USEUNIT("..\source\JvUIBConst.pas");
USEUNIT("..\source\JvUIBDataSet.pas");
USEUNIT("..\source\JvUIBError.pas");
USEUNIT("..\source\JvUIBLib.pas");
USEUNIT("..\source\JvUIBMetaData.pas");
USEUNIT("..\source\JvUIBSQLParser.pas");
USEPACKAGE("Vcldb50.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
