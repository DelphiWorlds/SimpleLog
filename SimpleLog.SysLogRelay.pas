unit SimpleLog.SysLogRelay;

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
  IdSysLog, IdSysLogServer, IdGlobal, IdUDPServer, IdSocketHandle, IdSysLogMessage,
  SimpleLog.Log, SimpleLog.Indy;

type
  TSysLogRelayOption = (UsePeerIPForHostName);

  TSysLogRelayOptions = set of TSysLogRelayOption;

  ISysLogRelay = interface(IInterface)
    ['{FD7EA664-8266-4505-8F00-0517D1B62752}']
    function GetIsActive: Boolean;
    function GetOptions: TSysLogRelayOptions;
    procedure SetCipher(const Value: ICipher);
    procedure SetDefaultPort(const Value: Integer);
    procedure SetDestination(const Value: string);
    procedure SetIsActive(const Value: Boolean);
    procedure SetOptions(const Value: TSysLogRelayOptions);
    property IsActive: Boolean read GetIsActive write SetIsActive;
    property Options: TSysLogRelayOptions read GetOptions write SetOptions;
  end;

  TSysLogRelay = class(TInterfacedObject, ISysLogRelay)
  private
    FClient: TIdSysLog;
    FDestinationHost: string;
    FDestinationPort: Integer;
    FOptions: TSysLogRelayOptions;
    FServer: TIdSysLogServerEx;
    procedure ServerSyslogHandler(Sender: TObject; ASysLogMessage: TIdSysLogMessage; ABinding: TIdSocketHandle);
  public
    { ISysLogRelay }
    function GetIsActive: Boolean;
    function GetOptions: TSysLogRelayOptions;
    procedure SetCipher(const Value: ICipher);
    procedure SetDefaultPort(const Value: Integer);
    procedure SetDestination(const Value: string);
    procedure SetIsActive(const Value: Boolean);
    procedure SetOptions(const Value: TSysLogRelayOptions);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils;

{ TSysLogRelay }

constructor TSysLogRelay.Create;
begin
  inherited;
  FClient := TIdSysLog.Create(nil);
  FServer := TIdSyslogServerEx.Create(nil);
  FServer.OnSyslog := ServerSyslogHandler;
  Include(FOptions, TSysLogRelayOption.UsePeerIPForHostName);
end;

destructor TSysLogRelay.Destroy;
begin
  FClient.Free;
  FServer.Free;
  inherited;
end;

function TSysLogRelay.GetIsActive: Boolean;
begin
  Result := FServer.Active;
end;

function TSysLogRelay.GetOptions: TSysLogRelayOptions;
begin
  Result := FOptions;
end;

procedure TSysLogRelay.ServerSyslogHandler(Sender: TObject; ASysLogMessage: TIdSysLogMessage; ABinding: TIdSocketHandle);
begin
  if TSysLogRelayOption.UsePeerIPForHostName in FOptions then
    ASysLogMessage.HostName := ABinding.PeerIP;
  FClient.Host := FDestinationHost;
  if FDestinationPort > 0 then
    FClient.Port := FDestinationPort
  else
    FClient.Port := FServer.DefaultPort;
  FClient.SendLogMessage(ASysLogMessage);
end;

procedure TSysLogRelay.SetCipher(const Value: ICipher);
begin
  FServer.SetCipher(Value);
end;

procedure TSysLogRelay.SetDefaultPort(const Value: Integer);
begin
  FServer.DefaultPort := Value;
end;

procedure TSysLogRelay.SetDestination(const Value: string);
var
  LHostParts: TArray<string>;
begin
  LHostParts := Value.Split([':']);
  if Length(LHostParts) > 0 then
  begin
    FDestinationPort := FServer.DefaultPort;
    FDestinationHost := LHostParts[0];
    if Length(LHostParts) > 1 then
      FDestinationPort := StrToIntDef(LHostParts[1], FDestinationPort);
  end;
end;

procedure TSysLogRelay.SetIsActive(const Value: Boolean);
begin
  FServer.Active := Value;
end;

procedure TSysLogRelay.SetOptions(const Value: TSysLogRelayOptions);
begin
  FOptions := Value;
end;

end.
