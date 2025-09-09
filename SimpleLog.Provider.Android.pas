unit SimpleLog.Provider.Android;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.Helpers;

type
  JLog = interface;

  JLogClass = interface(JObjectClass)
    ['{62108FE8-1DBB-4C4F-A0C7-35D12BD116DC}']
    {class} function _GetASSERT: Integer; cdecl;
    {class} function _GetDEBUG: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetINFO: Integer; cdecl;
    {class} function _GetVERBOSE: Integer; cdecl;
    {class} function _GetWARN: Integer; cdecl;
    {class} function d(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function d(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function getStackTraceString(tr: JThrowable): JString; cdecl;
    {class} function i(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function i(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function isLoggable(tag: JString; level: Integer): Boolean; cdecl;
    {class} function println(priority: Integer; tag: JString; msg: JString): Integer; cdecl;
    {class} function v(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function v(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function w(tag: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function wtf(tag: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} property ASSERT: Integer read _GetASSERT;
    {class} property DEBUG: Integer read _GetDEBUG;
    {class} property ERROR: Integer read _GetERROR;
    {class} property INFO: Integer read _GetINFO;
    {class} property VERBOSE: Integer read _GetVERBOSE;
    {class} property WARN: Integer read _GetWARN;
  end;

  [JavaSignature('android/util/Log')]
  JLog = interface(JObject)
    ['{6A5EC34E-CB76-4AB0-A11D-7CCB3B40C571}']
  end;
  TJLog = class(TJavaGenericImport<JLogClass, JLog>) end;

{ TPlatformLogProvider }

procedure TPlatformLogProvider.Log(const ALevel: TLogLevel; const AMessage: string);
var
  LMessage, LTag: JString;
begin
  LTag := StringToJString(SimpleLog.Log.Log.Tag);
  LMessage := StringToJString(AMessage);
  case ALevel of
    TLogLevel.Debug:
      TJLog.JavaClass.d(LTag, LMessage);
    TLogLevel.Error:
      TJLog.JavaClass.e(LTag, LMessage);
    TLogLevel.Fatal:
      TJLog.JavaClass.wtf(LTag, LMessage);
    TLogLevel.Info, TLogLevel.Trace:
      TJLog.JavaClass.i(LTag, LMessage);
    TLogLevel.Warning:
      TJLog.JavaClass.w(LTag, LMessage);
  end;
end;

end.
