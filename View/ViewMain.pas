unit ViewMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, System.ImageList, Vcl.ImgList,
  System.Actions, Vcl.ActnList, Vcl.CheckLst, Vcl.Grids,
  IOUtils, StrUtils,
  MyUtils, Install, Vcl.ComCtrls, WinSvc, Vcl.ExtCtrls;

type
  TWindowMain = class(TForm)
    Images: TImageList;
    OpenFilePath: TFileOpenDialog;
    OpenDllPath: TFileOpenDialog;
    Page: TPageControl;
    TabSetup: TTabSheet;
    BtnPath: TSpeedButton;
    LblPort: TLabel;
    LblVersion: TLabel;
    LblPath: TLabel;
    LblServiceName: TLabel;
    LblDll: TLabel;
    CheckAll: TCheckBox;
    TxtPort: TEdit;
    BoxVersion: TComboBox;
    TxtPath: TEdit;
    BtnCopyDll: TSpeedButton;
    BtnDeleteDll: TSpeedButton;
    BtnAdd: TSpeedButton;
    BtnRemove: TSpeedButton;
    BtnExecute: TSpeedButton;
    BtnLoadFolders: TSpeedButton;
    Actions: TActionList;
    ActPath: TAction;
    ActAdd: TAction;
    ActRemove: TAction;
    ActLoadFolders: TAction;
    ActCopyDll: TAction;
    DeleteDll: TAction;
    ActInstall: TAction;
    ActUninstall: TAction;
    ActEsc: TAction;
    BtnAddFirewall: TSpeedButton;
    ActAddFirewall: TAction;
    ActRemoveFirewall: TAction;
    BtnRemoveFirewall: TSpeedButton;
    BtnServices: TSpeedButton;
    ActServices: TAction;
    MemoLog: TMemo;
    BtnOpenDir: TSpeedButton;
    ActOpenDir: TAction;
    LblDefaultInstance: TLabel;
    ListDll: TCheckListBox;
    TxtServiceName: TEdit;
    RadioGroupMethod: TRadioGroup;
    ActStart: TAction;
    ShapeStatusService: TShape;
    TimerStatus: TTimer;
    ShapeStatusPath: TShape;
    BtnRemoveService: TSpeedButton;
    BtnStopService: TSpeedButton;
    BtnStartService: TSpeedButton;
    ActRemoveService: TAction;
    ActStopService: TAction;
    ActStartService: TAction;
    procedure ActPathExecute(Sender: TObject);
    procedure ActAddExecute(Sender: TObject);
    procedure ActRemoveExecute(Sender: TObject);
    procedure ActUninstallExecute(Sender: TObject);
    procedure ActInstallExecute(Sender: TObject);
    procedure ActEscExecute(Sender: TObject);
    procedure ActLoadFoldersExecute(Sender: TObject);
    procedure ActCopyDllExecute(Sender: TObject);
    procedure DeleteDllExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BoxVersionChange(Sender: TObject);
    procedure CheckAllClick(Sender: TObject);
    procedure ListDllClickCheck(Sender: TObject);
    procedure ActAddFirewallExecute(Sender: TObject);
    procedure ActRemoveFirewallExecute(Sender: TObject);
    procedure ActServicesExecute(Sender: TObject);
    procedure ActOpenDirExecute(Sender: TObject);
    procedure ActStartExecute(Sender: TObject);
    procedure RadioGroupMethodClick(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);
    procedure ActRemoveServiceExecute(Sender: TObject);
    procedure ActStopServiceExecute(Sender: TObject);
    procedure ActStartServiceExecute(Sender: TObject);
  private
    function GetInstallation: TInstall;
    procedure UpdateButtons;
  end;

var
  WindowMain: TWindowMain;

implementation

{$R *.dfm}

procedure TWindowMain.FormActivate(Sender: TObject);
begin
  BoxVersionChange(BoxVersion);
  ActLoadFolders.Execute;
  UpdateButtons;
  //CheckAll.Checked := true;
end;

procedure TWindowMain.TimerStatusTimer(Sender: TObject);
var
  ServiceStatus: Cardinal;
begin
  if TDirectory.Exists(TxtPath.Text) then
  begin
    ShapeStatusPath.Brush.Color := clGreen;
    ShapeStatusPath.Hint := 'Diretório Existente';
  end
  else
  begin
    ShapeStatusPath.Brush.Color := clRed;
    ShapeStatusPath.Hint := 'Diretório Não Existente';
  end;

  ServiceStatus := TUtils.GetServiceStatus('', 'FirebirdServer' + TUtils.IfEmpty(TxtServiceName.Text, 'DefaultInstance'));

  case TStatusService(ServiceStatus) of
  csNotInstalled:
  begin
    ShapeStatusService.Brush.Color := clRed;
    ShapeStatusService.Hint := 'Serviço Não Instalado';
  end;
  csPaused:
  begin
    ShapeStatusService.Brush.Color := clYellow;
    ShapeStatusService.Hint := 'Serviço Pausado';
  end;
  csStopped:
  begin
    ShapeStatusService.Brush.Color := clYellow;
    ShapeStatusService.Hint := 'Serviço Parado';
  end;
  csRunning:
  begin
    ShapeStatusService.Brush.Color := clGreen;
    ShapeStatusService.Hint := 'Serviço Rodando';
  end;
  end;

end;

procedure TWindowMain.ActAddExecute(Sender: TObject);
begin
  if OpenDllPath.Execute then
  begin
    ListDll.Items.Add(OpenDllPath.FileName);
  end;
end;

procedure TWindowMain.ActRemoveExecute(Sender: TObject);
var
  Index: integer;
begin
  Index := ListDll.ItemIndex;
  ListDll.DeleteSelected;
  if Index = 0 then
  begin
    ListDll.ItemIndex := 0;
  end
  else
  begin
    ListDll.ItemIndex := Index - 1;
  end;
end;

procedure TWindowMain.ActLoadFoldersExecute(Sender: TObject);
var
  Path: string;
begin
  for Path in TDirectory.GetDirectories('C:\') do
  begin
    if ContainsText(Path, 'NETSide') then
    begin
      ListDll.Items.Add(Path);
    end;
  end;
end;

procedure TWindowMain.ActCopyDllExecute(Sender: TObject);
var
  Installation: TInstall;
begin
  try
    Installation := GetInstallation;

    Installation.CopyDll;
    
    ShowMessage('Dlls Copiadas!');
  finally
    FreeAndNil(Installation);
  end;
end;

procedure TWindowMain.DeleteDllExecute(Sender: TObject);
var
  Installation: TInstall;
begin
  try
    Installation := GetInstallation;

    Installation.DeleteDll;

    ShowMessage('Dlls Deletadas!');
  finally
    FreeAndNil(Installation);
  end;
end;

procedure TWindowMain.ActAddFirewallExecute(Sender: TObject);
begin
  TUtils.AddFirewallPort('Firebird' + TUtils.IfEmpty(TxtServiceName.Text, 'DefaultInstance'), TxtPort.Text);
  ShowMessage('Porta e serviço adicionados no firewall!');
end;

procedure TWindowMain.ActRemoveFirewallExecute(Sender: TObject);
begin
  TUtils.DeleteFirewallPort('Firebird' + TUtils.IfEmpty(TxtServiceName.Text, 'DefaultInstance'), TxtPort.Text);
  ShowMessage('Porta e serviço removidos do firewall!');
end;

procedure TWindowMain.ActServicesExecute(Sender: TObject);
begin
  TUtils.ExecCmd('/C services.msc', 0);
end;

procedure TWindowMain.ActRemoveServiceExecute(Sender: TObject);
begin
  TUtils.RemoveService('FirebirdServer' + TxtServiceName.Text);
end;

procedure TWindowMain.ActStopServiceExecute(Sender: TObject);
begin
  TUtils.StopService('FirebirdServer' + TxtServiceName.Text);
end;

procedure TWindowMain.ActStartServiceExecute(Sender: TObject);
begin
  TUtils.StartService('FirebirdServer' + TxtServiceName.Text);
end;

procedure TWindowMain.ActOpenDirExecute(Sender: TObject);
begin
  TUtils.OpenOnExplorer(TxtPath.Text);
end;

procedure TWindowMain.RadioGroupMethodClick(Sender: TObject);
begin
  TxtPath.Enabled := RadioGroupMethod.ItemIndex = 1;
  BtnPath.Enabled := RadioGroupMethod.ItemIndex = 1;
end;

procedure TWindowMain.ActStartExecute(Sender: TObject);
begin
  case RadioGroupMethod.ItemIndex of
  0:
    ActUninstall.Execute;
  1:
    ActInstall.Execute;
  end;
end;

procedure TWindowMain.ActInstallExecute(Sender: TObject);
var
  Installation: TInstall;
begin
  try
    Screen.Cursor := crHourGlass;

    Application.ProcessMessages;

    Installation := GetInstallation;

    Installation.Install;
  finally
    Screen.Cursor := crDefault;
    FreeAndNil(Installation);
  end;
end;

procedure TWindowMain.ActUninstallExecute(Sender: TObject);
var
  Installation: TInstall;
begin
  try
    screen.Cursor := crHourGlass;

    Application.ProcessMessages;

    Installation := GetInstallation;

    Installation.Uninstall;
  finally
    Screen.Cursor := crDefault;
    FreeAndNil(Installation);
  end;
end;

procedure TWindowMain.ActPathExecute(Sender: TObject);
begin
  if OpenFilePath.Execute then
  begin
    TxtPath.Text := OpenFilePath.FileName;
  end;
end;

procedure TWindowMain.ActEscExecute(Sender: TObject);
begin
  Close;
end;

function TWindowMain.GetInstallation: TInstall;
var
  Configs: TInstallConfig;
  I: integer;
begin
  Configs := TInstallConfig.Create;

  with Configs do
  begin
    Version := TVersion(BoxVersion.ItemIndex);
    Path := TUtils.IfEmpty(TxtPath.Text, 'C:\Program Files (x86)\Firebird');
    ServiceName := TxtServiceName.Text;
    Port := TUtils.IfEmpty(TxtPort.Text, '3050');
    DllPaths := TStringList.Create;
    Log := MemoLog;

    for I := 0 to ListDll.Count - 1 do
    begin
      if ListDll.Checked[I] then
      begin
        DllPaths.Add(ListDll.Items[I]);
      end;
    end;

  end;

  Result := TInstall.Create(Configs);
end;

procedure TWindowMain.BoxVersionChange(Sender: TObject);
begin
  case TVersion(BoxVersion.ItemIndex) of
  vrFb21:
  begin
    TxtPath.Text := 'C:\Program Files (x86)\Firebird\Firebird_2_1';
    TxtServiceName.Text := 'NETSide2.1';
    TxtPort.Text := '3050';
  end;

  vrFb25:
  begin
    TxtPath.Text := 'C:\Program Files (x86)\Firebird\Firebird_2_5';
    TxtServiceName.Text := 'NETSide2.5';
    TxtPort.Text := '3060';
  end;

  vrFb30:
  begin
    TxtPath.Text := 'C:\Program Files (x86)\Firebird\Firebird_3_0';
    TxtServiceName.Text := 'NETSide3.0';
    TxtPort.Text := '3070';
  end;

  vrFb40:
  begin
    TxtPath.Text := 'C:\Program Files (x86)\Firebird\Firebird_4_0';
    TxtServiceName.Text := 'NETSide4.0';
    TxtPort.Text := '3080';
  end;
  end;
end;

procedure TWindowMain.CheckAllClick(Sender: TObject);
begin
  if CheckAll.Checked then
  begin
    ListDll.CheckAll(cbChecked);
  end
  else
  begin
    ListDll.CheckAll(cbUnchecked);
  end;
  UpdateButtons;
end;

procedure TWindowMain.ListDllClickCheck(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TWindowMain.UpdateButtons;
var
  I: integer;
  Checked: boolean;
begin
  Checked := false;
  for I := 0 to ListDll.Count - 1 do
  begin
    Checked := Checked or ListDll.Checked[I];
  end;

  BtnCopyDll.Enabled := Checked;
  BtnDeleteDll.Enabled := Checked;
end;

end.
