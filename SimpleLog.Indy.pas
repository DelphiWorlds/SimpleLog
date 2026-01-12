unit SimpleLog.Indy;

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
  IdSysLogServer, IdGlobal, IdUDPServer, IdSocketHandle, IdSysLogMessage,
  SimpleLog.Log;

type
  TSysLogRFC = (RFC3164, RFC5424);

  TIdSysLogMessageEx = class(TIdSysLogMessage)
  private
    FAppName: string;
    FMsgID: string;
    FRFC: TSysLogRFC;
    function GetSectionEnd(const AStartPos: Integer): Integer;
    procedure ReadAppName(var AStartPos: Integer);
    procedure ReadHostName(var AStartPos: Integer);
    procedure ReadISO8601Timestamp(var AStartPos: Integer);
    procedure ReadMessage(var AStartPos: Integer);
    procedure ReadMsgID(var AStartPos: Integer);
    procedure ReadProcID(var AStartPos: Integer);
  protected
    procedure Parse; override;
  public
    function EncodeMessage: string; override;
    property AppName: string read FAppName;
    property MsgID: string read FMsgID;
    property RFC: TSysLogRFC read FRFC write FRFC;
  end;

  TIdSysLogServerEx = class(TIdSysLogServer)
  private
    FCipher: ICipher;
    procedure ReadData(const AData: TIdBytes; const ABinding: TIdSocketHandle);
  protected
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle); override;
  public
    function GetCipher: ICipher;
    procedure SetCipher(const Value: ICipher);
  end;

implementation

uses
  System.SysUtils, System.DateUtils;

function CleanValue(const AValue: string): string;
begin
  if AValue.IsEmpty then
    Result := '-'
  else
    Result := AValue;
end;

{ TIdSysLogMessageEx }

function TIdSysLogMessageEx.EncodeMessage: string;
begin
  if FRFC = TSysLogRFC.RFC5424 then
  begin
    // For now, msg id not provided
    Result := Format('<%d>1 %s %s %s %d %s - %s',
      [PRI, DateToISO8601(Timestamp, False), CleanValue(Hostname), CleanValue(Msg.Process), Msg.PID, '-', Msg.Content]);
  end
  else
    Result := inherited;
end;

function TIdSysLogMessageEx.GetSectionEnd(const AStartPos: Integer): Integer;
begin
  Result := AStartPos;
  while (Result < Length(FRawMessage)) and (FRawMessage[Result] <> ' ') do
    Inc(Result);
end;

procedure TIdSysLogMessageEx.Parse;
var
  APos: Integer;
  LVersion: string;
begin
  APos := 1;
  ReadPRI(APos);
  LVersion := Copy(FRawMessage, APos, 2);
  if LVersion = '1 ' then
  begin
    FRFC := TSysLogRFC.RFC5424;
    Inc(APos, 2);
    ReadISO8601Timestamp(APos);
    ReadHostName(APos);
    ReadAppName(APos);
    ReadProcID(APos);
    ReadMsgID(APos);
    Inc(APos, 2);
    ReadMessage(APos);
  end
  else
  begin
    FRFC := TSysLogRFC.RFC3164;
    ReadHeader(APos);
    ReadMSG(APos);
  end;
end;

procedure TIdSysLogMessageEx.ReadAppName(var AStartPos: Integer);
var
  LEnd: Integer;
begin
  LEnd := GetSectionEnd(AStartPos);
  // AppName in RFC 5414 (at least) can include characters like '-' and '.'
  FAppName := Copy(FRawMessage, AStartPos, LEnd - AStartPos);
  if FAppName = '-' then
    FAppName := '';
  AStartPos := LEnd + 1;
end;

procedure TIdSysLogMessageEx.ReadHostName(var AStartPos: Integer);
var
  LEnd: Integer;
begin
  LEnd := GetSectionEnd(AStartPos);
  FHostname := Copy(FRawMessage, AStartPos, LEnd - AStartPos);
  if FHostname = '-' then
    FHostname := '';
  AStartPos := LEnd + 1;
end;

procedure TIdSysLogMessageEx.ReadISO8601Timestamp(var AStartPos: Integer);
var
  LEnd: Integer;
begin
  LEnd := GetSectionEnd(AStartPos);
  FTimestamp := ISO8601ToDate(Copy(FRawMessage, AStartPos, LEnd - AStartPos));
  AStartPos := LEnd + 1;
end;

procedure TIdSysLogMessageEx.ReadMessage(var AStartPos: Integer);
begin
  Msg.Content := Copy(FRawMessage, AStartPos, Length(FRawMessage) - AStartPos + 1);
end;

procedure TIdSysLogMessageEx.ReadMsgID(var AStartPos: Integer);
var
  LEnd: Integer;
begin
  LEnd := GetSectionEnd(AStartPos);
  FMsgID := Copy(FRawMessage, AStartPos, LEnd - AStartPos);
  if FMsgID = '-' then
    FMsgID := '';
  AStartPos := LEnd + 1;
end;

procedure TIdSysLogMessageEx.ReadProcID(var AStartPos: Integer);
var
  LEnd: Integer;
begin
  LEnd := GetSectionEnd(AStartPos);
  Msg.PID := StrToIntDef(Copy(FRawMessage, AStartPos, LEnd - AStartPos), -1);
  AStartPos := LEnd + 1;
end;

{ TIdSysLogServerEx }

procedure TIdSysLogServerEx.DoUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
begin
  if Length(AData) > 0 then
  begin
    if FCipher <> nil then
      ReadData(TIdBytes(FCipher.Decrypt(TBytes(AData))), ABinding)
    else
      ReadData(AData, ABinding);
  end;
end;

function TIdSysLogServerEx.GetCipher: ICipher;
begin
  Result := FCipher;
end;

procedure TIdSysLogServerEx.ReadData(const AData: TIdBytes; const ABinding: TIdSocketHandle);
var
  LMsg: TIdSysLogMessageEx;
begin
  LMsg := TIdSysLogMessageEx.Create(Self);
  try
    LMsg.ReadFromBytes(AData, ABinding.PeerIP);
  //  ReadFromStream(AData, (AData as TMemoryStream).Size, ABinding.PeerIP);
    DoSyslogEvent(LMsg, ABinding);
  finally
    FreeAndNil(LMsg)
  end;
end;

procedure TIdSysLogServerEx.SetCipher(const Value: ICipher);
begin
  FCipher := Value;
end;

end.
