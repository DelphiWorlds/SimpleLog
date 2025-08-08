unit SimpleLog.Provider.LogFile;

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
  TFileLogCreationOption = (UseSame, NewAtStart, NewOnDayChange);

  TFileLogSettings = record
  public
    class function Defaults: TFileLogSettings; static;
  public
    CreationOption: TFileLogCreationOption;
    FileNameExt: string;
    FileNameDateTimeFormat: string; // e.g. 'yyyy-mm-dd'
    Path: string;
  end;

  TFileLogProvider = class(TInterfacedObject, ILogProvider)
  private
    FFileName: string;
    FSettings: TFileLogSettings;
    FUseSettings: Boolean;
    function FindMostRecentLogFile: string;
    function GetDayChangeFileName: string;
    function GetFileName: string;
    function GetNewFileName: string;
  public
    { ILogProvider }
    procedure Log(const ALevel: TLogLevel; const AMessage: string);
  public
    constructor Create(const AFileName: string); overload;
    constructor Create(const APath: string; const ASettings: TFileLogSettings); overload;
    constructor Create(const ASettings: TFileLogSettings); overload;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils, System.StrUtils;

{ TFileLogSettings }

class function TFileLogSettings.Defaults: TFileLogSettings;
begin
  Result.FileNameExt := '.log';
  Result.FileNameDateTimeFormat := 'yyyy-dd-mm';
  Result.CreationOption := TFileLogCreationOption.NewOnDayChange;
end;

{ TFileLogProvider }

constructor TFileLogProvider.Create(const AFileName: string);
begin
  inherited Create;
  FUseSettings := False;
  FFileName := AFileName;
end;

constructor TFileLogProvider.Create(const APath: string; const ASettings: TFileLogSettings);
begin
  Create(ASettings);
  FSettings.Path := APath;
end;

constructor TFileLogProvider.Create(const ASettings: TFileLogSettings);
begin
  inherited Create;
  FUseSettings := True;
  FSettings := ASettings;
end;

function TFileLogProvider.FindMostRecentLogFile: string;
var
  LFileName: string;
  LLastWriteTime, LMostRecentTime: TDateTime;
begin
  Result := '';
  LMostRecentTime := 0;
  if TDirectory.Exists(FSettings.Path) then
  begin
    for LFileName in TDirectory.GetFiles(FSettings.Path, '*' + FSettings.FileNameExt, TSearchOption.soTopDirectoryOnly) do
    begin
      LLastWriteTime := TFile.GetLastWriteTime(LFileName);
      if LLastWriteTime > LMostRecentTime then
      begin
        LMostRecentTime := LLastWriteTime;
        Result := LFileName;
      end;
    end;
  end;
end;

function TFileLogProvider.GetDayChangeFileName: string;
var
  LTargetFileName: string;
  LLastWriteTime: TDateTime;
begin
  Result := '';
  if not FFileName.IsEmpty then
    LTargetFileName := FFileName
  else
    LTargetFileName := FindMostRecentLogFile;
  if not LTargetFileName.IsEmpty and TFile.Exists(LTargetFileName) then
  begin
    LLastWriteTime := TFile.GetLastWriteTime(LTargetFileName);
    if Trunc(Now) = Trunc(LLastWriteTime) then
      Result := LTargetFileName;
  end;
end;

function TFileLogProvider.GetNewFileName: string;
var
  LSuffixCount: Integer;
  LSuffix, LBaseFileName: string;
begin
  LBaseFileName := TPath.Combine(FSettings.Path, FormatDateTime(FSettings.FileNameDateTimeFormat, Now));
  LSuffixCount := 0;
  repeat
    Inc(LSuffixCount);
    LSuffix := IfThen(LSuffixCount > 1, '.' + LSuffixCount.ToString, '');
    Result := LBaseFileName + LSuffix + FSettings.FileNameExt;
  until not TFile.Exists(Result) or (FSettings.CreationOption = TFileLogCreationOption.UseSame);
end;

function TFileLogProvider.GetFileName: string;
begin
  Result := '';
  if not FSettings.FileNameExt.StartsWith('.') then
    FSettings.FileNameExt := '.' + FSettings.FileNameExt;
  case FSettings.CreationOption of
    TFileLogCreationOption.NewOnDayChange:
      Result := GetDayChangeFileName;
    TFileLogCreationOption.UseSame:
      if not FFileName.IsEmpty then
        Result := FFileName;
  end;
  if Result.IsEmpty then
    Result := GetNewFileName;
end;

procedure TFileLogProvider.Log(const ALevel: TLogLevel; const AMessage: string);
var
  LWriter: TStreamWriter;
  LFileName, LTimestamp: string;
begin
  LTimestamp := FormatDateTime('yyyy/dd/mm hh:nn:ss.zzz', Now);
  if FUseSettings then
  begin
    LFileName := GetFileName;
    if FSettings.CreationOption <> TFileLogCreationOption.UseSame then
      LTimestamp := FormatDateTime('hh:nn:ss.zzz', Now);
  end
  else
    LFileName := FFileName;
  if not LFileName.IsEmpty and ForceDirectories(TPath.GetDirectoryName(LFileName)) then
  begin
    FFileName := LFileName;
    LWriter := TStreamWriter.Create(LFileName, TFile.Exists(LFileName));
    try
      LWriter.WriteLine(LTimestamp + ' - ' + ALevel.AsString + ': ' + AMessage);
    finally
      LWriter.Free;
    end;
  end;
end;

end.
