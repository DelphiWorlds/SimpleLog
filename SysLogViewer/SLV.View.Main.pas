unit SLV.View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, System.Rtti, FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Grid, FMX.StdCtrls,
  IdSysLogMessage,
  SimpleLog.Log,
  SimpleLog.Server.SysLog;

type
  TMainView = class(TForm, ISysLogServerListener)
    BottomLayout: TLayout;
    LogGrid: TStringGrid;
    TimeColumn: TStringColumn;
    IPColumn: TStringColumn;
    HostColumn: TStringColumn;
    FacilityColumn: TStringColumn;
    SeverityColumn: TStringColumn;
    TagColumn: TStringColumn;
    MessageColumn: TStringColumn;
    ServerStatusLabel: TLabel;
    TopLayout: TLayout;
    EncryptedCheckBox: TCheckBox;
    ClearGridButton: TButton;
    procedure LogGridResized(Sender: TObject);
    procedure EncryptedCheckBoxChange(Sender: TObject);
    procedure ClearGridButtonClick(Sender: TObject);
  private
    FCipher: ICipher;
    procedure UpdateServerStatus;
  public
    { ISysLogServerListener }
    procedure ReceivedMessage(const AMessage: TIdSysLogMessage);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainView: TMainView;

implementation

{$R *.fmx}

// Enable this define to use OpenSSL.
// Note: You will need to have DW.OpenSSL.pas from Kastri in the path, and the relevant OpenSSL binaries available
{$DEFINE OPENSSL}

uses
  {$IF Defined(OPENSSL)}
  SimpleLog.Cipher.OpenSSL;
  {$ELSE}
  SimpleLog.Cipher.Simple;
  {$ENDIF}

type
  TIdSyslogSeverityHelper = record helper for TIdSyslogSeverity
    function AsString: string;
  end;

  TCustomGridHelper = class helper for TCustomGrid
  private
    function GetTotalColumnWidth: Single;
    function GetVerticalScrollWidth: Single;
  public
    procedure FitColumns;
  end;

{ TIdSyslogSeverityHelper }

function TIdSyslogSeverityHelper.AsString: string;
const
  cValues: array[TIdSyslogSeverity] of string = ('Emergency', 'Alert', 'Critical', 'Error', 'Warning', 'Notice', 'Info', 'Debug');
begin
  Result := cValues[Self];
end;

{ TCustomGridHelper }

procedure TCustomGridHelper.FitColumns;
var
  LTotalColWidth, LScale: Single;
  I: Integer;
begin
  if Width > 50 then
  begin
    LTotalColWidth := GetTotalColumnWidth;
    if LTotalColWidth > 0 then
    begin
      LScale := ((Width - 4) - GetVerticalScrollWidth) / LTotalColWidth;
      for I := 0 to ColumnCount - 1 do
      begin
        if Columns[I].Visible then
          Columns[I].Width := (Columns[I].Width * LScale) - 1;
      end;
    end;
  end;
end;

function TCustomGridHelper.GetTotalColumnWidth: Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ColumnCount - 1 do
  begin
    if Columns[I].Visible then
    begin
      Result := Result + Columns[I].Width;
      if TGridOption.ColLines in Options then
        Result := Result + 1;
    end;
  end;
end;

function TCustomGridHelper.GetVerticalScrollWidth: Single;
begin
  if VScrollBar <> nil then
    Result := VScrollBar.Width
  else
    Result := 0;
end;

{ TMainView }

constructor TMainView.Create(AOwner: TComponent);
begin
  inherited;
  // The key being used here is just an example. Ensure that the key matches the corresponding key on the client
  {$IF Defined(OPENSSL)}
  FCipher := TOpenSSLCipher.Create('s1mPL3l0gs1mPL3l0gs1mPL3l0gABCDE');
  {$ELSE}
  FCipher := TSimpleCipher.Create;
  {$ENDIF}
  LogGrid.RowCount := 0;
  SysLogServer.AddListener(Self);
  SysLogServer.IsActive := True;
  UpdateServerStatus;
end;

procedure TMainView.ClearGridButtonClick(Sender: TObject);
begin
  LogGrid.RowCount := 0;
end;

procedure TMainView.EncryptedCheckBoxChange(Sender: TObject);
begin
  if EncryptedCheckBox.IsChecked then
    SysLogServer.SetCipher(FCipher)
  else
    SysLogServer.SetCipher(nil);
  UpdateServerStatus;
end;

procedure TMainView.LogGridResized(Sender: TObject);
begin
  LogGrid.FitColumns;
end;

procedure TMainView.ReceivedMessage(const AMessage: TIdSysLogMessage);
var
  LRow: Integer;
  LMessage: string;
begin
  LRow := LogGrid.RowCount;
  LogGrid.RowCount := LogGrid.RowCount + 1;
  LogGrid.Cells[TimeColumn.Index, LRow] := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
  LogGrid.Cells[IPColumn.Index, LRow] := AMessage.Peer;
  LogGrid.Cells[HostColumn.Index, LRow] := AMessage.Hostname;
  LogGrid.Cells[SeverityColumn.Index, LRow] := AMessage.Severity.AsString;
  if AMessage.Tag > 0 then
    LogGrid.Cells[TagColumn.Index, LRow] := AMessage.Tag.ToString;
  LMessage := AMessage.Msg.Text;
  if LMessage.IndexOf(':') > -1 then
    LMessage := LMessage.Substring(LMessage.IndexOf(':') + 1).Trim;
  LogGrid.Cells[MessageColumn.Index, LRow] := LMessage;
end;

procedure TMainView.UpdateServerStatus;
const
  cActiveCaptions: array[Boolean] of string = ('Inactive', 'Active');
  cEncryptedCaptions: array[Boolean] of string = ('Unencrypted', 'Encrypted');
begin
  ServerStatusLabel.Text := Format('Server is: %s on Port: %d (%s)',
    [cActiveCaptions[SysLogServer.IsActive], SysLogServer.GetDefaultPort, cEncryptedCaptions[SysLogServer.GetCipher <> nil]]);
end;

end.
