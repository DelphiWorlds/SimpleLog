unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ActnList, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  SimpleLog.SysLogRelay, SimpleLog.Log;

type
  TForm1 = class(TForm)
    btnStartStop: TButton;
    aclMain: TActionList;
    actStartStop: TAction;
    SysLogLayout: TLayout;
    SysLogServerLayout: TLayout;
    SysLogServerLabel: TLabel;
    SysLogServerEdit: TEdit;
    EncryptedLayout: TLayout;
    EncryptedCheckBox: TCheckBox;
    procedure actStartStopExecute(Sender: TObject);
    procedure actStartStopUpdate(Sender: TObject);
    procedure cbEncryptedChange(Sender: TObject);
  private
    FCipher: ICipher;
    FRelay: ISysLogRelay;
    procedure UpdateRelayDestination;
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
  {$IF Defined(OPENSSL)}
  SimpleLog.Cipher.OpenSSL;
  {$ELSE}
  SimpleLog.Cipher.Simple;
  {$ENDIF}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  // The key being used here is just an example. Ensure that the key matches the corresponding key on the client
  {$IF Defined(OPENSSL)}
  FCipher := TOpenSSLCipher.Create('s1mPL3l0gs1mPL3l0gs1mPL3l0gABCDE');
  {$ELSE}
  FCipher := TSimpleCipher.Create;
  {$ENDIF}
  FRelay := TSysLogRelay.Create;
  // i.e. Relay will listen on 51423, actual SysLog server listens on 514
  FRelay.SetDefaultPort(51423);
  SysLogServerEdit.Text := 'localhost:514';
end;

procedure TForm1.actStartStopExecute(Sender: TObject);
var
  LWasActive: Boolean;
begin
  LWasActive := FRelay.IsActive;
  FRelay.IsActive := not FRelay.IsActive;
  if not LWasActive and FRelay.IsActive then
    UpdateRelayDestination;
end;

procedure TForm1.actStartStopUpdate(Sender: TObject);
const
  cStartStopCaptions: array[Boolean] of string = ('Start', 'Stop');
begin
  actStartStop.Enabled := not SysLogServerEdit.Text.IsEmpty;
  actStartStop.Text := cStartStopCaptions[FRelay.IsActive];
end;

procedure TForm1.cbEncryptedChange(Sender: TObject);
begin
  if EncryptedCheckBox.IsChecked then
    FRelay.SetCipher(FCipher)
  else
    FRelay.SetCipher(nil);
end;

procedure TForm1.UpdateRelayDestination;
begin
  FRelay.SetDestination(SysLogServerEdit.Text);
end;

end.
