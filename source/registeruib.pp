{******************************************************************************}
{                        UNIFIED INTERBASE (UIB)                               }
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JvUIBReg.pas.                                           }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ Register Components for Lazarus                                              }
{                                                                              }
{ Unit owner:    Olivier Guilbaud                                              }
{ Last modified: Jun 30, 2003                                                  }
{                                                                              }
{ History                                                                      }
{   dec 10 2003 : Add  TJvUIBDataSet and TJvUIBScript                          }                                            {                                                                              }
{******************************************************************************}
unit registeruib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, jvuib, jvuibdataset, LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitUIB;
begin
  RegisterComponents('Jv UIB', [TJvUIBDatabase, TJvUIBTransaction, TJvUIBQuery,
    TJvUIBBackup, TJvUIBRestore, TJvUIBScript, TJvUIBEvents, TJvUIBRepair,
    TJvUIBSecurity, TJvUIBConfig, TJvUIBServerInfo]);
end;

procedure RegisterUnitDataSet;
begin
  RegisterComponents('Jv UIB', [TJvUIBDataSet]);
end;

procedure Register;
begin
  RegisterUnit('JvUIB',@RegisterUnitUIB);
  RegisterUnit('JvUIBDataSet',@RegisterUnitDataSet);
end;

initialization
  {$i registeruib.lrs}

end.
