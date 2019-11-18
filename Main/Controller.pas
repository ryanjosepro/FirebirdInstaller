unit Controller;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils,
  Sets, Bean, MyUtils;

type
  TController = class
  private
    Configs: TInstallationConfigs;
    function GetSource(Version: TVersion): string;
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
begin

end;

procedure TController.Uninstall;
begin

end;

function TController.GetSource(Version: TVersion): string;
begin
  case Configs.Version of
  vrFb21:
    Result := TUtils.AppPath + 'Data\Firebird_2_1';
  vrFb25:
    Result := TUtils.AppPath + 'Data\Firebird_2_5';
  vrFb30:
    Result := TUtils.AppPath + 'Data\Firebird_3_0';
  end;
end;

end.
