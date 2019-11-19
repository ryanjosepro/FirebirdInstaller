unit Controller;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils, Vcl.Dialogs, Vcl.Controls, IOUtils, ShellApi,
  Sets, Bean, MyUtils, MyDialogs;

type
  TController = class
  private
    Configs: TInstallationConfigs;
    function Destiny: string;
    function Source: string;
    function VersionFolder: string;
  public
    constructor Create(Configs: TInstallationConfigs);
    procedure Install;
    procedure Uninstall;
  end;

implementation

{ TController }

constructor TController.Create(Configs: TInstallationConfigs);
begin
  self.Configs := Configs;
end;

procedure TController.Install;
var
  Cancel: boolean;
begin
  if not DirectoryExists(Configs.Path) then
  begin
    CreateDir(Configs.Path);
  end;

  if DirectoryExists(Destiny) then
  begin
    if TDialogs.OkCancel('A pasta de instala��o "' + Destiny + '" j� existe e ser� sobreescrita!') = mrOk  then
    begin
      TDirectory.Delete(Destiny, true);
    end
    else
    begin
      Cancel := true;
    end;
  end;

  if not Cancel then
  begin
    TDirectory.Copy(Source, Destiny);

    TUtils.ExexDos('');
  end;
end;

procedure TController.Uninstall;
begin

end;

function TController.Destiny: string;
begin
  Result := Configs.Path + VersionFolder;
end;

function TController.Source: string;
begin
  Result := TUtils.AppPath + 'Data' + VersionFolder;
end;

function TController.VersionFolder: string;
begin
  case Configs.Version of
  vrFb21:
    Result := '\Firebird_2_1';
  vrFb25:
    Result := '\Firebird_2_5';
  vrFb30:
    Result := '\Firebird_3_0';
  end;
end;

end.
