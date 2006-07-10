//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\source\JvUIBTransactionEdit.pas", Jvuibtransactionedit, UIBTransactionEditForm);
USEFORMNS("..\source\JvUIBDatabaseEdit.pas", Jvuibdatabaseedit, UIBDatabaseEditForm);
USEFORMNS("..\source\JvUIBSQLEdit.pas", Jvuibsqledit, UIBSQLEditForm);
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



 