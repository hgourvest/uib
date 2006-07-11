{******************************************}
{                                          }
{             FastReport v3.0              }
{          Language resource file          }
{                                          }
{         Copyright (c) 1998-2005          }
{         by Alexander Tzyganenko,         }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit frxrcUIB;

interface

implementation

uses frxRes;

const resStr =
'obUIBComps=UIB Components' + #13#10 +
'obUIBDB=UIB Database' + #13#10 +
'obUIBT=UIB Transaction' + #13#10 +
'obUIBQ=UIB Query' + #13#10 +
'';

initialization
  frxResources.AddStrings(resStr);

end.
