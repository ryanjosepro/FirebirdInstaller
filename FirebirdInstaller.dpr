program FirebirdInstaller;





















{$R *.dres}

uses
  Vcl.Forms,
  ViewMain in 'View\ViewMain.pas' {WindowMain},
  Vcl.Themes,
  Vcl.Styles,
  MyUtils in 'Code\MyUtils.pas',
  MyArrays in 'Code\MyArrays.pas',
  MyDialogs in 'Code\MyDialogs.pas',
  Installation in 'Controller\Installation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TWindowMain, WindowMain);
  Application.Run;
end.
