program FirebirdInstaller;





















{$R *.dres}

uses
  Vcl.Forms,
  ViewMain in 'Main\ViewMain.pas' {WindowMain},
  Vcl.Themes,
  Vcl.Styles,
  MySets in 'Code\MySets.pas',
  MyUtils in 'Code\MyUtils.pas',
  MyArrays in 'Code\MyArrays.pas',
  InstallConfigs in 'Bean\InstallConfigs.pas',
  MyDialogs in 'Code\MyDialogs.pas',
  Installation in 'Main\Installation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TWindowMain, WindowMain);
  Application.Run;
end.
