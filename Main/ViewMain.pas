unit ViewMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, System.ImageList, Vcl.ImgList,
  System.Actions, Vcl.ActnList, Vcl.CheckLst, Vcl.Grids,
  IOUtils, StrUtils,
  MySets, MyUtils, InstallConfigs, Installation;

type
  TWindowMain = class(TForm)
    BoxVersion: TComboBox;
    LblVersion: TLabel;
    LblPath: TLabel;
    TxtPath: TEdit;
    Actions: TActionList;
    ActPath: TAction;
    ActEsc: TAction;
    Images: TImageList;
    BtnPath: TSpeedButton;
    OpenFilePath: TFileOpenDialog;
    LblServiceName: TLabel;
    TxtServiceName: TEdit;
    LblPort: TLabel;
    TxtPort: TEdit;
    ListDll: TListBox;
    LblDll: TLabel;
    BtnAdd: TSpeedButton;
    ActAdd: TAction;
    ActRemove: TAction;
    BtnRemove: TSpeedButton;
    ActInstall: TAction;
    ActUninstall: TAction;
    BtnInstall: TSpeedButton;
    BtnUninstall: TSpeedButton;
    OpenDllPath: TFileOpenDialog;
    ActLoadFolders: TAction;
    BtnLoadFolders: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ActCopyDll: TAction;
    SpeedButton1: TSpeedButton;
    DeleteDll: TAction;
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
  end;

var
  WindowMain: TWindowMain;

implementation

{$R *.dfm}

procedure TWindowMain.FormActivate(Sender: TObject);
begin
  ActLoadFolders.Execute;
end;

procedure TWindowMain.ActAddExecute(Sender: TObject);
begin
  if OpenDllPath.Execute then
  begin
    ListDll.Items.Add(OpenDllPath.FileName);
  end;
end;

procedure TWindowMain.ActRemoveExecute(Sender: TObject);
begin
  ListDll.DeleteSelected;
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
  Configs: TInstallConfigs;
  Installation: TInstallation;
begin
  try
    Configs := TInstallConfigs.Create;

    with Configs do
    begin
      Version := TVersion(BoxVersion.ItemIndex);

      DllPaths := ListDll.Items.ToStringArray;
    end;

    Installation := TInstallation.Create(Configs);

    Installation.CopyDll;
  finally
    FreeAndNil(Configs);
    FreeAndNil(Installation);
  end;
end;

procedure TWindowMain.DeleteDllExecute(Sender: TObject);
var
  Configs: TInstallConfigs;
  Installation: TInstallation;
begin
  try
    Configs := TInstallConfigs.Create;

    with Configs do
    begin
      Version := TVersion(BoxVersion.ItemIndex);

      DllPaths := ListDll.Items.ToStringArray;
    end;

    Installation := TInstallation.Create(Configs);

    Installation.DeleteDll;
  finally
    FreeAndNil(Configs);
    FreeAndNil(Installation);
  end;
end;

procedure TWindowMain.ActInstallExecute(Sender: TObject);
var
  Configs: TInstallConfigs;
  Installation: TInstallation;
begin
  try
    Configs := TInstallConfigs.Create;

    screen.Cursor := crHourGlass;

    with Configs do
    begin
      Version := TVersion(BoxVersion.ItemIndex);
      Path := TUtils.IfEmpty(TxtPath.Text, 'C:\Program Files (x86)\Firebird');
      ServiceName := TxtServiceName.Text;
      Port := TUtils.IfEmpty(TxtPort.Text, '3050');
      DllPaths := ListDll.Items.ToStringArray;
    end;

    Installation := TInstallation.Create(Configs);

    Configs.Source;

    Installation.Install;
  finally
    Screen.Cursor := crDefault;
    FreeAndNil(Configs);
    FreeAndNil(Installation);
  end;
end;

procedure TWindowMain.ActUninstallExecute(Sender: TObject);
var
  Configs: TInstallConfigs;
  Installation: TInstallation;
begin
  try
    Configs := TInstallConfigs.Create;

    screen.Cursor := crHourGlass;

    with Configs do
    begin
      Version := TVersion(BoxVersion.ItemIndex);
      Path := TUtils.IfEmpty(TxtPath.Text, 'C:\Program Files (x86)\Firebird');
      ServiceName := TxtServiceName.Text;
      Port := TUtils.IfEmpty(TxtPort.Text, '3050');
      DllPaths := ListDll.Items.ToStringArray;
    end;

    Installation := TInstallation.Create(Configs);

    Installation.Uninstall;
  finally
    Screen.Cursor := crDefault;
    FreeAndNil(Configs);
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

end.
