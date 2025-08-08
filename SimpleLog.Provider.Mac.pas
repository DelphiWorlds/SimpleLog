unit SimpleLog.Provider.Mac;

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
  Macapi.Helpers, Macapi.ObjCRuntime;

const
  libFoundation = '/System/Library/Frameworks/Foundation.framework/Foundation';

type
  PNSString = Pointer;

procedure NSLog(format: PNSString); cdecl; varargs; external libFoundation name _PU + 'NSLog';

{ TPlatformLogProvider }

procedure TPlatformLogProvider.Log(const ALevel: TLogLevel; const AMessage: string);
begin
  NSLog(StringToID(ALevel.AsString + ': ' + AMessage));
end;

end.
