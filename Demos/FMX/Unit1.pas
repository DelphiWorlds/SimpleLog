unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Layouts,
  SimpleLog.Log,
  SimpleLog.Provider.SysLog;

type
  TForm1 = class(TForm)
    LogMethodButton: TButton;
    SysLogLayout: TLayout;
    SysLogServerLayout: TLayout;
    SysLogServerLabel: TLabel;
    SysLogServerEdit: TEdit;
    EncryptedLayout: TLayout;
    EncryptedCheckBox: TCheckBox;
    SysLogInfoLabel: TLabel;
    UpdateSysLogButton: TButton;
    procedure LogMethodButtonClick(Sender: TObject);
    procedure UpdateSysLogButtonClick(Sender: TObject);
    procedure EncryptedCheckBoxChange(Sender: TObject);
  private
    FCipher: ICipher;
    FSysLogProvider: ISysLogProvider;
    function GetLogsPath: string;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

// Enable this define to use OpenSSL.
// Note: You will need to have DW.OpenSSL.pas from Kastri in the path, and the relevant OpenSSL binaries available
{.$DEFINE OPENSSL}

uses
  System.IOUtils,
  {$IF Defined(OPENSSL)}
  SimpleLog.Cipher.OpenSSL,
  {$ELSE}
  SimpleLog.Cipher.Simple,
  {$ENDIF}
  SimpleLog.Provider.LogFile;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  SysLogServerEdit.Text := 'localhost';
  {$IF Defined(OPENSSL)}
  // The key being used here is just an example. Ensure that the key matches the corresponding key on the server
  FCipher := TOpenSSLCipher.Create('s1mPL3l0gs1mPL3l0gs1mPL3l0gABCDE');
  {$ELSE}
  FCipher := TSimpleCipher.Create;
  {$ENDIF}
  Log.AddProvider(TFileLogProvider.Create(GetLogsPath, TFileLogSettings.Defaults));
  FSysLogProvider := TSysLogProvider.Create(SysLogServerEdit.Text);
  Log.AddProvider(FSysLogProvider);
end;

procedure TForm1.EncryptedCheckBoxChange(Sender: TObject);
begin
  if EncryptedCheckBox.IsChecked then
    FSysLogProvider.SetCipher(FCipher)
  else
    FSysLogProvider.SetCipher(nil);
end;

function TForm1.GetLogsPath: string;
var
  LAppName: string;
begin
  Result := 'Logs';
  LAppName := TPath.GetFileNameWithoutExtension(TPath.GetFileName(ParamStr(0)));
  {$IF Defined(IOS) or Defined(ANDROID)}
  Result := TPath.Combine(TPath.GetDocumentsPath, Result);
  {$ELSEIF Defined(MACOS) or Defined(MSWINDOWS)}
  Result := TPath.Combine(TPath.GetSharedDocumentsPath, LAppName + PathDelim + Result);
  {$ELSEIF Defined(LINUX)}
  Result := TPath.Combine(TPath.GetDocumentsPath, LAppName + PathDelim + Result);
  {$ENDIF}
end;

procedure TForm1.LogMethodButtonClick(Sender: TObject);
begin
  Log.m('TForm1.LogMethodButtonClick', []);
  Log.i('The caption of the form is: %s', [Caption]);
  Log.e('This is just an error example');
end;

procedure TForm1.UpdateSysLogButtonClick(Sender: TObject);
var
  LServerParts: TArray<string>;
  LHost: string;
  LPort: Integer;
begin
  LServerParts := SysLogServerEdit.Text.Split([':']);
  if Length(LServerParts) > 0 then
  begin
    LPort := 514; // SysLog default
    LHost := LServerParts[0];
    if Length(LServerParts) > 1 then
      LPort := StrToIntDef(LServerParts[1], LPort);
    FSysLogProvider.SetServer(LHost, LPort);
    ShowMessage('SysLog server setting updated');
  end;
end;

end.
