unit SimpleLog.Log;

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
  System.SysUtils;

type
  TLogLevel = (Debug, Error, Fatal, Info, Trace, Warning);

  TLogLevelHelper = record helper for TLogLevel
    function AsString: string;
  end;

  ICipher = interface(IInterface)
    ['{D996BDEF-94AB-47A1-90ED-FD5A2A88F268}']
    function Decrypt(const AValue: TBytes): TBytes;
    function Encrypt(const AValue: TBytes): TBytes;
  end;

  ILogProvider = interface(IInterface)
    ['{F9E573BC-EAD3-44AB-A59F-25FAD7DBD2B2}']
    procedure Log(const ALevel: TLogLevel; const AMessage: string);
  end;

  IMethodLog = interface(IInterface)
    ['{B35BB974-BBEE-44B4-8298-66DC9858C1C4}']
  end;

  ILog = interface(IInterface)
    ['{BCEC9767-0A1B-4247-8ACC-BB2887A71E89}']
    procedure AddProvider(const AProvider: ILogProvider);
    procedure d(const AFmt: string); overload;
    procedure d(const AFmt: string; const AParams: array of const); overload;
    procedure e(const AFmt: string); overload;
    procedure e(const AFmt: string; const AParams: array of const); overload;
    procedure f(const AFmt: string); overload;
    procedure f(const AFmt: string; const AParams: array of const); overload;
    function GetTag: string;
    procedure i(const AFmt: string); overload;
    procedure i(const AFmt: string; const AParams: array of const); overload;
    function m(const AFmt: string): IMethodLog; overload;
    function m(const AFmt: string; const AParams: array of const): IMethodLog; overload;
    procedure SetTag(const Value: string);
    procedure w(const AFmt: string); overload;
    procedure w(const AFmt: string; const AParams: array of const); overload;
    property Tag: string read GetTag write SetTag;
  end;

var
  Log: ILog;

implementation

uses
  System.Diagnostics,
  {$IF Defined(MSWINDOWS)}
  // This uses clause is here only to prevent an error on shutdown on Windows when using TSysLogProvider
  // If you know why it helps, please let me know
  IdSysLog,
  SimpleLog.Provider.Win;
  {$ELSEIF Defined(MACOS)}
  SimpleLog.Provider.Mac;
  {$ELSEIF Defined(ANDROID)}
  SimpleLog.Provider.Android;
  {$ENDIF}


const
  cLogLevelCaptions: array[TLogLevel] of string = ('DEBUG','ERROR', 'FATAL', 'INFO', 'TRACE', 'WARN');

type
  TMethodLog = class(TInterfacedObject, IMethodLog)
  private
    FLogLevel: TLogLevel;
    FMessage: string;
    FStopWatch: TStopWatch;
  public
    constructor Create(const ALevel: TLogLevel; const AMessage: string);
    destructor Destroy; override;
  end;

  IMethodLogCallback = interface(IInterface)
    ['{F74DE9FF-A2C4-4DC6-BBAC-7E9B8BD88252}']
    procedure Log(const ALevel: TLogLevel; const AMessage: string);
  end;

  TLogProviders = TArray<ILogProvider>;

  TLog = class(TInterfacedObject, ILog, IMethodLogCallback)
  private
    FPlatformLogProvider: ILogProvider;
    FProviders: TLogProviders;
    FTag: string;
    function DoLog(const ALevel: TLogLevel; const AFmt: string; const AParams: array of const): IMethodLog;
  public
    { IMethodCallback }
    procedure Log(const ALevel: TLogLevel; const AMessage: string);
    { ILog }
    procedure AddProvider(const AProvider: ILogProvider);
    procedure d(const AFmt: string); overload;
    procedure d(const AFmt: string; const AParams: array of const); overload;
    procedure e(const AFmt: string); overload;
    procedure e(const AFmt: string; const AParams: array of const); overload;
    procedure f(const AFmt: string); overload;
    procedure f(const AFmt: string; const AParams: array of const); overload;
    function GetTag: string;
    procedure i(const AFmt: string); overload;
    procedure i(const AFmt: string; const AParams: array of const); overload;
    function m(const AFmt: string): IMethodLog; overload;
    function m(const AFmt: string; const AParams: array of const): IMethodLog; overload;
    procedure SetTag(const Value: string);
    procedure w(const AFmt: string); overload;
    procedure w(const AFmt: string; const AParams: array of const); overload;
  public
    constructor Create;
  end;

{ TLogLevelHelper }

function TLogLevelHelper.AsString: string;
begin
  Result := cLogLevelCaptions[Self];
end;

{ TLog }

constructor TLog.Create;
begin
  inherited;
  FPlatformLogProvider := TPlatformLogProvider.Create;
  AddProvider(FPlatformLogProvider);
end;

procedure TLog.AddProvider(const AProvider: ILogProvider);
begin
  FProviders := FProviders + [AProvider];
end;

function TLog.GetTag: string;
begin
  Result := FTag;
end;

procedure TLog.SetTag(const Value: string);
begin
  FTag := Value;
end;

function TLog.DoLog(const ALevel: TLogLevel; const AFmt: string; const AParams: array of const): IMethodLog;
var
  LMessage: string;
begin
  Result := nil;
  LMessage := Format(AFmt, AParams);
  if ALevel = TLogLevel.Trace then
  begin
    Log(ALevel, '+' + LMessage);
    Result := TMethodLog.Create(ALevel, '-' + LMessage);
  end
  else
    Log(ALevel, LMessage);
end;

procedure TLog.Log(const ALevel: TLogLevel; const AMessage: string);
var
  LProvider: ILogProvider;
begin
  for LProvider in FProviders do
  try
    LProvider.Log(ALevel, AMessage);
  except
    on E: Exception do
    begin
      if LProvider <> FPlatformLogProvider then
        FPlatformLogProvider.Log(TLogLevel.Error, Format('%s: %s', [E.ClassName, E.Message]));
    end;
  end;
end;

procedure TLog.d(const AFmt: string);
begin
  d(AFmt, []);
end;

procedure TLog.d(const AFmt: string; const AParams: array of const);
begin
  DoLog(TLogLevel.Debug, AFmt, AParams);
end;

procedure TLog.e(const AFmt: string);
begin
  e(AFmt, []);
end;

procedure TLog.e(const AFmt: string; const AParams: array of const);
begin
  DoLog(TLogLevel.Error, AFmt, AParams);
end;

procedure TLog.f(const AFmt: string);
begin
  f(AFmt, []);
end;

procedure TLog.f(const AFmt: string; const AParams: array of const);
begin
  DoLog(TLogLevel.Fatal, AFmt, AParams);
end;

procedure TLog.i(const AFmt: string);
begin
  i(AFmt, []);
end;

procedure TLog.i(const AFmt: string; const AParams: array of const);
begin
  DoLog(TLogLevel.Info, AFmt, AParams);
end;

function TLog.m(const AFmt: string): IMethodLog;
begin
  Result := m(AFmt, []);
end;

function TLog.m(const AFmt: string; const AParams: array of const): IMethodLog;
begin
  Result := DoLog(TLogLevel.Trace, AFmt, AParams);
end;

procedure TLog.w(const AFmt: string);
begin
  w(AFmt, []);
end;

procedure TLog.w(const AFmt: string; const AParams: array of const);
begin
  DoLog(TLogLevel.Warning, AFmt, AParams);
end;

{ TMethodLog }

constructor TMethodLog.Create(const ALevel: TLogLevel; const AMessage: string);
begin
  inherited Create;
  FLogLevel := ALevel;
  FMessage := AMessage;
  FStopWatch := TStopWatch.StartNew;
end;

destructor TMethodLog.Destroy;
begin
  (Log as IMethodLogCallback).Log(FLogLevel, FMessage + Format(' [%s ms]', [FStopWatch.Elapsed.TotalMilliseconds.ToString]));
  inherited;
end;

initialization
  Log := TLog.Create;

end.
