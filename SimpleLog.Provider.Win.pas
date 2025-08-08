unit SimpleLog.Provider.Win;

{*******************************************************}
{                                                       }
{                    Simple Log                         }
{                                                       }
{          Logging Library from Delphi Worlds           }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  SimpleLog.Log;

type
  TPlatformLogProvider = class(TInterfacedObject, ILogProvider)
  public
    { ILogProvider }
    procedure Log(const ALevel: TLogLevel; const AMessage: string);
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

{ TPlatformLogProvider }

procedure TPlatformLogProvider.Log(const ALevel: TLogLevel; const AMessage: string);
begin
  OutputDebugString(PChar(Format('[%s]: %s', [ALevel.AsString, AMessage])));
end;

end.
