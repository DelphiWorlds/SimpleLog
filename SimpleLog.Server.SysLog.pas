unit SimpleLog.Server.SysLog;

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
  IdSysLogMessage,
  SimpleLog.Log,
  SimpleLog.Indy;

type
  ISysLogServerListener = interface(IInterface)
    ['{36615B48-134F-4CCE-8C9C-0A64DC0AB3FD}']
    procedure ReceivedMessage(const AMessage: TIdSysLogMessageEx);
  end;

  ISysLogServer = interface(IInterface)
    ['{DE0029E0-30E7-4761-91BE-3130EAE3F80B}']
    procedure AddListener(const AListener: ISysLogServerListener);
    function GetCipher: ICipher;
    function GetDefaultPort: Integer;
    function GetIsActive: Boolean;
    procedure SetCipher(const Value: ICipher);
    procedure SetDefaultPort(const Value: Integer);
    procedure SetIsActive(const Value: Boolean);
    property IsActive: Boolean read GetIsActive write SetIsActive;
  end;

var
  SysLogServer: ISysLogServer;

implementation

uses
  IdSocketHandle;

type
  TSysLogServerListeners = TArray<ISysLogServerListener>;

  TSysLogServer = class(TInterfacedObject, ISysLogServer)
  private
    FListeners: TSysLogServerListeners;
    FServer: TIdSysLogServerEx;
    procedure ServerSyslogHandler(Sender: TObject; ASysLogMessage: TIdSysLogMessage; ABinding: TIdSocketHandle);
  public
    { ISysLogServer }
    procedure AddListener(const AListener: ISysLogServerListener);
    function GetCipher: ICipher;
    function GetDefaultPort: Integer;
    function GetIsActive: Boolean;
    procedure SetCipher(const Value: ICipher);
    procedure SetDefaultPort(const Value: Integer);
    procedure SetIsActive(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TSysLogServer }

constructor TSysLogServer.Create;
begin
  inherited;
  FServer := TIdSysLogServerEx.Create(nil);
  FServer.OnSyslog := ServerSyslogHandler;
end;

destructor TSysLogServer.Destroy;
begin
  FServer.Free;
  inherited;
end;

function TSysLogServer.GetCipher: ICipher;
begin
  Result := FServer.GetCipher;
end;

function TSysLogServer.GetDefaultPort: Integer;
begin
  Result := FServer.DefaultPort;
end;

function TSysLogServer.GetIsActive: Boolean;
begin
  Result := FServer.Active;
end;

procedure TSysLogServer.SetIsActive(const Value: Boolean);
begin
  FServer.Active := Value;
end;

procedure TSysLogServer.AddListener(const AListener: ISysLogServerListener);
begin
  FListeners := FListeners + [AListener];
end;

procedure TSysLogServer.ServerSyslogHandler(Sender: TObject; ASysLogMessage: TIdSysLogMessage; ABinding: TIdSocketHandle);
var
  LListener: ISysLogServerListener;
begin
  for LListener in FListeners do
  try
    LListener.ReceivedMessage(TIdSysLogMessageEx(ASysLogMessage));
  except
    // Eat any exceptions a listener might cause
  end;
end;

procedure TSysLogServer.SetCipher(const Value: ICipher);
begin
  FServer.SetCipher(Value);
end;

procedure TSysLogServer.SetDefaultPort(const Value: Integer);
var
  LWasActive: Boolean;
begin
  LWasActive := FServer.Active;
  FServer.Active := False;
  FServer.DefaultPort := Value;
  FServer.Active := LWasActive;
end;

initialization
  SysLogServer := TSysLogServer.Create;

end.
