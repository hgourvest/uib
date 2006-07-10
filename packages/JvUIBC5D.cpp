//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvUIBC5D.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcldb50.bpi");
USEUNIT("..\source\JvUIBReg.pas");
USEPACKAGE("SynEdit_BCB5.bpi");
USEPACKAGE("JvUIBC5R.bpi");
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
