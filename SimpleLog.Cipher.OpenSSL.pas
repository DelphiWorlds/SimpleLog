unit SimpleLog.Cipher.OpenSSL;

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
  System.SysUtils,
  SimpleLog.Log;

type
  TOpenSSLCipher = class(TInterfacedObject, ICipher)
  private
    FKey: TBytes;
  public
    { ICipher }
    function Decrypt(const AValue: TBytes): TBytes;
    function Encrypt(const AValue: TBytes): TBytes;
  public
    constructor Create(const AKey: TBytes); overload;
    constructor Create(const AKey: string); overload;
  end;

implementation

uses
  DW.OpenSSL;

{ TOpenSSLCipher }

constructor TOpenSSLCipher.Create(const AKey: TBytes);
begin
  inherited Create;
  FKey := AKey;
end;

constructor TOpenSSLCipher.Create(const AKey: string);
begin
  Create(TEncoding.Default.GetBytes(AKey));
end;

function TOpenSSLCipher.Decrypt(const AValue: TBytes): TBytes;
begin
  Result := DecryptData(AValue, FKey);
end;

function TOpenSSLCipher.Encrypt(const AValue: TBytes): TBytes;
begin
  Result := EncryptData(AValue, FKey);
end;

end.
