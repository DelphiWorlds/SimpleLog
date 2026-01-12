unit SimpleLog.Provider.SysLog;

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

{$SCOPEDENUMS ON}

uses
  System.SysUtils,
  IdSysLog, IdSysLogMessage, IdGlobal,
  SimpleLog.Log, SimpleLog.Indy;

const
  cPortSysLogDefault = 514;

type
  TIdSysLogEx = class(TIdSysLog)
  private
    FCipher: ICipher;
  protected
    procedure SetCipher(const ACipher: ICipher);
    property Cipher: ICipher read FCipher;
  public
    procedure SendBuffer(const AHost: string; const APort: TIdPort; const AIPVersion: TIdIPVersion; const ABuffer : TIdBytes); override;
  end;

  ISysLogProvider = interface(ILogProvider)
    ['{483C82CC-A143-4720-80D8-BB55131463DC}']
    function GetCipher: ICipher;
    function GetDeviceID: string;
    procedure SetCipher(const ACipher: ICipher);
    procedure SetDeviceID(const ADeviceID: string);
    procedure SetServer(const AHost: string; const APort: Word = cPortSysLogDefault);
    property Cipher: ICipher read GetCipher write SetCipher;
    property DeviceID: string read GetDeviceID write SetDeviceID;
  end;

  TSysLogProvider = class(TInterfacedObject, ILogProvider, ISysLogProvider)
  private
    FAppName: string;
    FDeviceID: string;
    FRFC: TSysLogRFC;
    FSysLog : TIdSysLogEx;
    function GetAppName: string;
    function GetDeviceName: string;
  public
    { ILogProvider }
    procedure Log(const ALevel: TLogLevel; const AMessage: string);
    { ISysLogProvider }
    function GetCipher: ICipher;
    function GetDeviceID: string;
    procedure SetCipher(const ACipher: ICipher);
    procedure SetDeviceID(const ADeviceID: string);
    procedure SetServer(const AHost: string; const APort: Word = cPortSysLogDefault);
  public
    constructor Create(const AHost: string; const APort: Word = cPortSysLogDefault);
    destructor Destroy; override;
  end;

implementation

uses
  System.IOUtils, System.DateUtils,
  {$IF Defined(POSIX)}
  Posix.Unistd,
  {$ENDIF}
  {$IF Defined(MACOS)}
  Macapi.Helpers,
  {$ENDIF}
  {$IF Defined(MSWINDOWS)}
  Winapi.Windows;
  {$ELSEIF Defined(OSX)}
  Macapi.Foundation;
  {$ELSEIF Defined(IOS)}
  iOSapi.Foundation, iOSapi.Helpers;
  {$ELSEIF Defined(ANDROID)}
  Androidapi.JNI.Os, Androidapi.Helpers;
  {$ELSEIF Defined(LINUX)}
  Posix.UniStd;
  {$ENDIF}

function GetProcessId: UInt32;
begin
  {$IFDEF MSWINDOWS}
  Result := GetCurrentProcessId;
  {$ELSE}
  Result := getpid;
  {$ENDIF}
end;

{ TIdSysLogEx }

procedure TIdSysLogEx.SendBuffer(const AHost: string; const APort: TIdPort; const AIPVersion: TIdIPVersion; const ABuffer: TIdBytes);
begin
  if FCipher <> nil then
    inherited SendBuffer(AHost, APort, AIPVersion, TIdBytes(FCipher.Encrypt(TBytes(ABuffer))))
  else
    inherited;
end;

procedure TIdSysLogEx.SetCipher(const ACipher: ICipher);
begin
  if FCipher <> ACipher then
    FCipher := nil;
  FCipher := ACipher;
end;

{ TSysLogProvider }

constructor TSysLogProvider.Create(const AHost: string; const APort: Word = cPortSysLogDefault);
begin
  inherited Create;
  FRFC := TSysLogRFC.RFC5424;
  FAppName := GetAppName;
  FSysLog := TIdSysLogEx.Create(nil);
  SetServer(AHost, APort);
end;

destructor TSysLogProvider.Destroy;
begin
  FSysLog.Free;
  inherited;
end;

function TSysLogProvider.GetAppName: string;
begin
  Result := TPath.GetFileNameWithoutExtension(TPath.GetFileName(ParamStr(0)));
end;

function TSysLogProvider.GetCipher: ICipher;
begin
  Result := FSysLog.Cipher;
end;

function TSysLogProvider.GetDeviceID: string;
begin
  Result := FDeviceID;
end;

{$IF Defined(MSWINDOWS)}
function TSysLogProvider.GetDeviceName: string;
var
  LComputerName: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  LSize: DWORD;
begin
  LSize := Length(LComputerName);
  if GetComputerName(LComputerName, LSize) then
    SetString(Result, LComputerName, LSize)
  else
    Result := '';
end;

{$ELSEIF Defined(OSX)}
function TSysLogProvider.GetDeviceName: string;
begin
  Result := NSStrToStr(TNSHost.Wrap(TNSHost.OCClass.currentHost).localizedName);
end;

{$ELSEIF Defined(IOS)}
function TSysLogProvider.GetDeviceName: string;
begin
  Result := NSStrToStr(TiOSHelper.CurrentDevice.name);
end;

{$ELSEIF Defined(ANDROID)}
function TSysLogProvider.GetDeviceName: string;
begin
  Result := JStringToString(TJBuild.JavaClass.MODEL);
  if Result.IsEmpty then
    Result := Format('%s %s', [JStringToString(TJBuild.JavaClass.MANUFACTURER), JStringToString(TJBuild.JavaClass.PRODUCT)]);
end;

{$ELSEIF Defined(LINUX)}
function TSysLogProvider.GetDeviceName: string;
const
  cMaxHostName = 255;
var
  LHost: array[0..cMaxHostName] of Byte;
  LHostPtr: TPtrWrapper;
begin
  LHostPtr := TPtrWrapper.Create(@LHost[0]);
  gethostname(LHostPtr.ToPointer, cMaxHostName);
  Result := TMarshal.ReadStringAsAnsi(LHostPtr);
end;
{$ENDIF}

procedure TSysLogProvider.SetCipher(const ACipher: ICipher);
begin
  FSysLog.SetCipher(ACipher);
end;

procedure TSysLogProvider.SetDeviceID(const ADeviceID: string);
begin
  FDeviceID := ADeviceID;
end;

procedure TSysLogProvider.SetServer(const AHost: string; const APort: Word = cPortSysLogDefault);
begin
  FSysLog.Host := AHost;
  FSysLog.Port := APort;
end;

procedure TSysLogProvider.Log(const ALevel: TLogLevel; const AMessage: string);
var
  LSysLogMessage: TIdSysLogMessageEx;
begin
  LSysLogMessage := TIdSysLogMessageEx.Create(nil);
  try
    LSysLogMessage.RFC := FRFC;
    LSysLogMessage.TimeStamp := Now;
    LSysLogMessage.Hostname := FDeviceID;
    LSysLogMessage.Msg.Content := '<' + ALevel.AsString + '>: ' + AMessage;
    LSysLogMessage.Msg.Process := FAppName;
    LSysLogMessage.Msg.PID := GetProcessId;
    case ALevel of
      TLogLevel.Debug:
        LSysLogMessage.Severity := TIdSyslogSeverity.slDebug;
      TLogLevel.Error:
        LSysLogMessage.Severity := TIdSyslogSeverity.slError;
      TLogLevel.Info, TLogLevel.Trace:
        LSysLogMessage.Severity := TIdSyslogSeverity.slInformational;
      TLogLevel.Fatal:
        LSysLogMessage.Severity := TIdSyslogSeverity.slCritical;
      TLogLevel.Warning:
        LSysLogMessage.Severity := TIdSyslogSeverity.slWarning;
    end;
    FSysLog.SendLogMessage(LSysLogMessage, False);
  finally
    LSysLogMessage.Free;
  end;
end;

end.
