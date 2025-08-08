unit SimpleLog.Cipher.Simple;

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
  TSimpleCipher = class(TInterfacedObject, ICipher)
  public
    { ICipher }
    function Decrypt(const AValue: TBytes): TBytes;
    function Encrypt(const AValue: TBytes): TBytes;
  end;

implementation

// Yes, sorry - ridiculous, I know :-)

{ TSimpleCipher }

function TSimpleCipher.Decrypt(const AValue: TBytes): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(AValue));
  for I := 0 to Length(Result) - 1 do
  begin
    if AValue[I] = 0 then
      Result[I] := 255
    else
      Result[I] := AValue[I] - 1;
  end;
end;

function TSimpleCipher.Encrypt(const AValue: TBytes): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(AValue));
  for I := 0 to Length(Result) - 1 do
  begin
    if AValue[I] = 255 then
      Result[I] := 0
    else
      Result[I] := AValue[I] + 1;
  end;
end;

end.
