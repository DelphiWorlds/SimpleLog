program SysLogViewer;

uses
  System.StartUpCopy,
  FMX.Forms,
  SLV.View.Main in 'SLV.View.Main.pas' {MainView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainView, MainView);
  Application.Run;
end.
