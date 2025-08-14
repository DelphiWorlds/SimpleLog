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
  TGridItem = record
    Severity: TIdSyslogSeverity;
  end;

  TGridItems = TArray<TGridItem>;

  TStringGrid = class(FMX.Grid.TStringGrid)
  private
    FWidths: TArray<Single>;
    function GetTotalColumnWidth: Single;
    function GetVerticalScrollWidth: Single;
    procedure StoreWidths;
  public
    procedure FitColumns;
    procedure ScrollToBottom;
  end;

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
    AutoScrollCheckBox: TCheckBox;
    procedure LogGridResized(Sender: TObject);
    procedure EncryptedCheckBoxChange(Sender: TObject);
    procedure ClearGridButtonClick(Sender: TObject);
    procedure LogGridDrawColumnBackground(Sender: TObject; const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure LogGridDrawColumnHeader(Sender: TObject; const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF);
  private
    FCipher: ICipher;
    FGridItems: TGridItems;
    procedure AddMessageToGrid(const AMessage: TIdSysLogMessage);
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
  System.Math,
  {$IF Defined(OPENSSL)}
  SimpleLog.Cipher.OpenSSL;
  {$ELSE}
  SimpleLog.Cipher.Simple;
  {$ENDIF}

type
  TIdSyslogSeverityHelper = record helper for TIdSyslogSeverity
    function AsString: string;
    function AsColor: TAlphaColor;
  end;

{ TIdSyslogSeverityHelper }

function TIdSyslogSeverityHelper.AsColor: TAlphaColor;
const
  cValues: array[TIdSyslogSeverity] of TAlphaColor = (
    TAlphaColors.Null, // Emergency
    TAlphaColors.Null, // Alert
    TAlphaColors.Orchid, // Critical
    TAlphaColors.Red, // Error
    TAlphaColors.Yellow, // Warning
    TAlphaColors.Null, // Notice
    TAlphaColors.Null, // Info
    TAlphaColors.Null  // Debug
  );
begin
  Result := cValues[Self];
end;

function TIdSyslogSeverityHelper.AsString: string;
const
  cValues: array[TIdSyslogSeverity] of string = ('Emergency', 'Alert', 'Critical', 'Error', 'Warning', 'Notice', 'Info', 'Debug');
begin
  Result := cValues[Self];
end;

{ TStringGrid }

procedure TStringGrid.StoreWidths;
var
  I: Integer;
begin
  SetLength(FWidths, ColumnCount);
  for I := 0 to ColumnCount - 1 do
  begin
    if Columns[I].Visible then
      FWidths[I] := Columns[I].Width
    else
      FWidths[I] := 0;
  end;
end;

procedure TStringGrid.FitColumns;
var
  LTotalColWidth, LScale, LAvailableWidth: Single;
  I: Integer;
begin
  if Length(FWidths) <> ColumnCount then
    StoreWidths;
  LTotalColWidth := 0;
  for I := 0 to ColumnCount - 1 do
  begin
    if Columns[I].Visible then
    begin
      LTotalColWidth := LTotalColWidth + FWidths[I];
      if TGridOption.ColLines in Options then
        LTotalColWidth := LTotalColWidth + 1;
    end;
  end;
  if LTotalColWidth > 0 then
  begin
    LAvailableWidth := Width - 4 - GetVerticalScrollWidth;
    LScale := LAvailableWidth / LTotalColWidth;
    for I := 0 to ColumnCount - 1 do
    begin
      if Columns[I].Visible then
        Columns[I].Width := Max(10, FWidths[I] * LScale - 1);
    end;
  end;
end;

function TStringGrid.GetTotalColumnWidth: Single;
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

function TStringGrid.GetVerticalScrollWidth: Single;
begin
  if VScrollBar <> nil then
    Result := VScrollBar.Width
  else
    Result := 0;
end;

procedure TStringGrid.ScrollToBottom;
var
  LDestHeight: Single;
begin
  LDestHeight := ContentBounds.Height - ViewportSize.Height;
  ScrollBy(0, LDestHeight - ViewportPosition.Y, True);
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
  SysLogServer.SetCipher(FCipher);
  SysLogServer.IsActive := True;
  EncryptedCheckBox.IsChecked := True;
  UpdateServerStatus;
end;

procedure TMainView.AddMessageToGrid(const AMessage: TIdSysLogMessage);
var
  LRow: Integer;
  LMessage: string;
begin
  LRow := LogGrid.RowCount;
  LogGrid.RowCount := LogGrid.RowCount + 1;
  SetLength(FGridItems, LogGrid.RowCount);
  FGridItems[LRow].Severity := AMessage.Severity;
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
  if AutoScrollCheckBox.IsChecked then
    LogGrid.ScrollToBottom;
end;

procedure TMainView.ClearGridButtonClick(Sender: TObject);
begin
  LogGrid.RowCount := 0;
  SetLength(FGridItems, LogGrid.RowCount);
end;

procedure TMainView.EncryptedCheckBoxChange(Sender: TObject);
begin
  if EncryptedCheckBox.IsChecked then
    SysLogServer.SetCipher(FCipher)
  else
    SysLogServer.SetCipher(nil);
  UpdateServerStatus;
end;

procedure TMainView.LogGridDrawColumnBackground(Sender: TObject; const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  LColor: TAlphaColor;
begin
  LColor := FGridItems[Row].Severity.AsColor;
  if LColor <> TAlphaColors.Null then
  begin
    Canvas.Fill.Color := LColor;
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.FillRect(Bounds, 0, 0, AllCorners, 0.5);
  end;
end;

procedure TMainView.LogGridDrawColumnHeader(Sender: TObject; const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF);
begin
  Canvas.Fill.Color := TAlphaColors.MoneyGreen;
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.FillRect(Bounds, 0, 0, AllCorners, 1.0);

  Canvas.Stroke.Color := TAlphaColorRec.Gray;
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Thickness := 1;
  Canvas.DrawLine(PointF(Bounds.Right - 0.5, Bounds.Top), PointF(Bounds.Right - 0.5, Bounds.Bottom), 1.0);
  Canvas.DrawLine(PointF(Bounds.Left, Bounds.Bottom - 0.5), PointF(Bounds.Right, Bounds.Bottom - 0.5), 1.0);

  Canvas.Fill.Color := TAlphaColorRec.Black;
  Canvas.FillText(RectF(Bounds.Left + 2, Bounds.Top, Bounds.Right - 2, Bounds.Bottom),
    Column.Header, False, 1.0, [], TTextAlign.Leading, TTextAlign.Center);
end;

procedure TMainView.LogGridResized(Sender: TObject);
begin
  LogGrid.FitColumns;
end;

procedure TMainView.ReceivedMessage(const AMessage: TIdSysLogMessage);
begin
  AddMessageToGrid(AMessage);
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
