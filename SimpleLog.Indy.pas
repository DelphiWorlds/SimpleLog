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
  IdSysLogServer, IdGlobal, IdUDPServer, IdSocketHandle,
  SimpleLog.Log;

type
  TIdSysLogServerEx = class(TIdSysLogServer)
  private
    FCipher: ICipher;
  protected
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle); override;
  public
    function GetCipher: ICipher;
    procedure SetCipher(const Value: ICipher);
  end;

implementation

uses
  System.SysUtils;

{ TIdSysLogServerEx }

procedure TIdSysLogServerEx.DoUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
begin
  if Length(AData) > 0 then
  begin
    if FCipher <> nil then
      inherited DoUDPRead(AThread, TIdBytes(FCipher.Decrypt(TBytes(AData))), ABinding)
    else
      inherited DoUDPRead(AThread, AData, ABinding);
  end;
end;

function TIdSysLogServerEx.GetCipher: ICipher;
begin
  Result := FCipher;
end;

procedure TIdSysLogServerEx.SetCipher(const Value: ICipher);
begin
  FCipher := Value;
end;

end.
