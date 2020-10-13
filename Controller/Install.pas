unit Install;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils, Vcl.Dialogs, Vcl.Controls, IniFiles,
  IOUtils, ShellApi, Windows, Zip, Vcl.Forms, Vcl.StdCtrls,
  MyUtils, MyDialogs;

type
  TVersion = (vrFb21 = 0, vrFb25 = 1, vrFb30 = 2, vrFb40 = 3);

  TInstallConfig = class
  strict private
    FVersion: TVersion;
    FPath: string;
    FServiceName: string;
    FPort: string;
    FDllPaths: TStringList;
    FLog: TMemo;
    procedure SetVersion(const Value: TVersion);
    procedure SetPath(const Value: string);
    procedure SetServiceName(const Value: string);
    procedure SetPort(const Value: string);
    procedure SetDllPaths(const Value: TStringList);
    procedure SetLog(const Value: TMemo);

    function FolderName: string;
    function DllName: string;
    function ResourceName: string;
  public
    property Version: TVersion read FVersion write SetVersion;
    property Path: string read FPath write SetPath;
    property ServiceName: string read FServiceName write SetServiceName;
    property Port: string read FPort write SetPort;
    property DllPaths: TStringList read FDllPaths write SetDllPaths;
    property Log: TMemo read FLog write SetLog;

    function PathBin: string;
    function PathConf: string;
    function Source(Extract: boolean = true): string;
    function SourceBin(Extract: boolean = false): string;
    function SourceDll: string;
  end;

  TInstall = class
  private
    Configs: TInstallConfig;
    procedure Log(Text: string);
  public
    constructor Create(Configs: TInstallConfig);
    procedure CopyDll;
    procedure DeleteDll;
    procedure Install;
    procedure Uninstall;
  end;

implementation

{ TInstallConfig }

procedure TInstallConfig.SetVersion(const Value: TVersion);
begin
  FVersion := Value;
end;

procedure TInstallConfig.SetPath(const Value: string);
begin
  FPath := Value;
end;

procedure TInstallConfig.SetServiceName(const Value: string);
begin
  FServiceName := TUtils.IfEmpty(Value, 'DefaultInstance');
end;

procedure TInstallConfig.SetPort(const Value: string);
begin
  FPort := Value;
end;

procedure TInstallConfig.SetDllPaths(const Value: TStringList);
begin
  FDllPaths := Value;
end;

procedure TInstallConfig.SetLog(const Value: TMemo);
begin
  FLog := Value;
end;

//Destinos
function TInstallConfig.PathBin: string;
begin
  case Version of
  vrFb21, vrFb25:
    Result := Path + '\Bin';
  vrFb30, vrFb40:
    Result := Path;
  end;
end;

function TInstallConfig.PathConf: string;
begin
  Result := Path + '\firebird.conf';
end;

//Fontes
function TInstallConfig.Source(Extract: boolean = true): string;
begin
  if Extract then
  begin
    TUtils.DeleteIfExistsDir(TUtils.Temp + FolderName);

    //TZipFile.ExtractZipFile(TUtils.AppPath + 'Data' + FolderName + '.zip', TUtils.Temp);

    TUtils.ExtractResourceZip(ResourceName, TUtils.Temp);
  end;

  Result := TUtils.Temp + FolderName;
end;

function TInstallConfig.SourceBin(Extract: boolean = false): string;
begin
  case Version of
  vrFb21, vrFb25:
    Result := Source(Extract) + '\Bin';
  vrFb30, vrFb40:
    Result := Source(Extract);
  end;
end;

function TInstallConfig.SourceDll: string;
begin
  TUtils.DeleteIfExistsDir(TUtils.Temp + '\Dlls' + DllName);

  TUtils.ExtractResourceZip('DataDlls', TUtils.Temp);

  Result := TUtils.Temp + '\Dlls' + DllName;
end;

//Nome da pasta pela versão
function TInstallConfig.FolderName: string;
begin
  case Version of
  vrFb21:
    Result := '\Firebird_2_1';
  vrFb25:
    Result := '\Firebird_2_5';
  vrFb30:
    Result := '\Firebird_3_0';
  vrFb40:
    Result := '\Firebird_4_0';
  end;
end;

function TInstallConfig.DllName: string;
begin
  case Version of
  vrFb21:
    Result := '\fbclient21.dll';
  vrFb25:
    Result := '\fbclient25.dll';
  vrFb30:
    Result := '\fbclient30.dll';
  vrFb40:
    Result := '\fbclient40.dll';
  end;
end;

//Nome do resource pela versão
function TInstallConfig.ResourceName: string;
begin
  case Version of
  vrFb21:
    Result := 'DataFB21';
  vrFb25:
    Result := 'DataFB25';
  vrFb30:
    Result := 'DataFB30';
  vrFb40:
    Result := 'DataFB40';
  end;
end;



{ TInstallation }

constructor TInstall.Create(Configs: TInstallConfig);
begin
  self.Configs := Configs;
end;

procedure TInstall.Log(Text: string);
begin
  if Configs.Log <> nil then
  begin
    Configs.Log.Lines.Add(Text);

    Application.ProcessMessages;
  end;
end;

procedure TInstall.CopyDll;
var
  DllFolder: string;
begin
  for DllFolder in Configs.DllPaths do
  begin
    TUtils.DeleteIfExistsFile(DllFolder + '\fbclient.dll');
    TFile.Copy(Configs.SourceDll, DllFolder + '\fbclient.dll');
  end;
end;

procedure TInstall.DeleteDll;
var
  DllFolder: string;
begin
  for DllFolder in Configs.DllPaths do
  begin
    TUtils.DeleteIfExistsFile(DllFolder + '\fbclient.dll');
  end;
end;

procedure TInstall.Install;
var
  Cancel: boolean;
  ConfFile: TStringList;
  PathFiles: TArray<string>;
  PathFileName, FilesInUse, CdBin: string;
begin
  try
    if Configs.Log <> nil then
      Configs.Log.Clear;

    Application.ProcessMessages;

    Log('*******************************');
    Log('Path: ' + Configs.Path);
    Log('Service: ' + Configs.ServiceName);
    Log('Port: ' + Configs.Port);
    Log('*******************************');
    Log('');

    Log('Iniciando instalação');

    Log('Verificando se o serviço já existe');

    if TUtils.GetServiceStatus('', 'FirebirdServer' + Configs.ServiceName) = 0 then
    begin
      Log('Verificando se o diretório já existe');

      if (TDirectory.Exists(Configs.Path)) and (not TDirectory.IsEmpty(Configs.Path)) then
      begin
        if TDialogs.YesNo('A PASTA DE INSTALAÇÃO "' + Configs.Path + '" já existe, '+
        'deseja SOBREESCREVÊ-LA?' + TUtils.BreakLine +
        'Tenha certeza de que a pasta não está em uso!') = mrYes then
        begin
          Log('Verificando se os arquivos estão em uso');

          //Verificação aquivos em uso
          PathFiles := TDirectory.GetFiles(Configs.Path, '*', TSearchOption.soAllDirectories);

          for PathFileName in PathFiles do
          begin
            if TUtils.IsFileInUse(PathFileName) then
            begin
              FilesInUse := FilesInUse + TUtils.BreakLine + PathFileName;
            end;
          end;

          if FilesInUse <> '' then
          begin
            Cancel := true;

            ShowMessage('Erro ao sobreescrever a PASTA DE INSTALAÇÃO, os seguintes arquivos estão em USO '+
            TUtils.BreakLine + FilesInUse);
          end
          else
          begin
            Log('Deletando diretório');

            TDirectory.Delete(Configs.Path, true);

            Sleep(4000);

            Application.ProcessMessages;
          end;
        end
        else
        begin
          Cancel := true;
        end;
      end;

      if not Cancel then
      begin
        if not TDirectory.Exists(Configs.Path) then
        begin
          Log('Copiando arquivos de instalação para ' + Configs.Path);

          TDirectory.Copy(Configs.Source, Configs.Path);

          Log('Configurando porta');

          ConfFile := TStringList.Create;

          ConfFile.LoadFromFile(Configs.PathConf);

          ConfFile.Insert(0, 'RemoteServicePort = ' + Configs.Port);

          ConfFile.SaveToFile(Configs.PathConf);

          CdBin := 'cd ' + Configs.PathBin;

          Log('Instalando registro');

          TUtils.ExecDos(CdBin + ' && instreg r');

          Log('Instalando serviço');

          TUtils.ExecDos(CdBin + ' && instsvc i -a -g -n ' + Configs.ServiceName);

          Log('Iniciando serviço');

          TUtils.ExecDos(CdBin + ' && instsvc start -n ' + Configs.ServiceName);

          Log('Copiando Dlls');

          CopyDll;

          Log('Adicionando ao firewall');

          TUtils.AddFirewallPort('Firebird ' + Configs.ServiceName, Configs.Port);

          Log('');
          Log('*******************************');
          Log('Instalação concluída');
          Log('*******************************');

          ShowMessage('Instalação Concluída!');
        end
        else
        begin
          Log('');
          Log('*******************************');
          Log('Desinstalação cancelada');
          Log('*******************************');

          ShowMessage('Erro na PASTA DE INSTALAÇÃO, verifique se a '+
          'pasta de instalação está em USO!');
        end
      end
      else
      begin
        Log('');
        Log('*******************************');
        Log('Instalação cancelada');
        Log('*******************************');
      end;
    end
    else
    begin
      Log('');
      Log('*******************************');
      Log('Instalação Cancelada');
      Log('*******************************');

      ShowMessage('O SERVIÇO ' + '''FirebirdServer' + Configs.ServiceName + ''' JÁ EXISTE, '+
      'desinstale-o antes de continuar a instalação!');
    end;
  Except on E: Exception do
    Log('Erro: ' + E.Message);
  end;
end;

procedure TInstall.Uninstall;
var
  ConfFile: TStringList;
  PathFiles: TArray<string>;
  PathFileName, FilesInUse, CdBin: string;
begin
  try
    if Configs.Log <> nil then
      Configs.Log.Clear;

    Application.ProcessMessages;

    Log('*******************************');
    Log('Path: ' + Configs.Path);
    Log('Service: ' + Configs.ServiceName);
    Log('Port: ' + Configs.Port);
    Log('*******************************');
    Log('');

    Log('Iniciando desinstalação');

    if TDialogs.YesNo('Tem certeza que deseja DESINSTALAR o firebird "' + Configs.ServiceName + '"?') = mrYes then
    begin
      Log('Verificando se o serviço existe');

      if TUtils.GetServiceStatus('', 'FirebirdServer' + Configs.ServiceName) = 0 then
      begin
        ShowMessage('Erro ao tentar desinstalar, o serviço '+
        '''FirebirdServer' + Configs.ServiceName + ''' NÃO EXISTE!');
      end
      else
      begin
        //////////////////////

        //Pegando path a partir do serviço

        Log('Pegando path a partir do serviço');

        Configs.Path := TUtils.GetServiceExecutablePath('', 'FirebirdServer' + Configs.ServiceName);

        Configs.Path := ExtractFilePath(Configs.Path.Split(['"'])[1]);

        if Configs.Path.Substring(Configs.Path.Length - 5, 5).ToLower = '\bin\' then
        begin
          Configs.Path := Configs.Path.Substring(0, Configs.Path.Length - 5);
        end;

        //////////////////////

        Log('Verificando se o diretório existe');

        if DirectoryExists(Configs.Path) then
        begin
          Log('Verificando se a pasta bin existe');

          if DirectoryExists(Configs.PathBin) then
          begin
            Log('Repondo arquivos de desinstalação caso não existam');

            if not FileExists(Configs.PathBin + '\instsvc.exe') then
              TFile.Copy(Configs.SourceBin(true) + '\instsvc.exe', Configs.PathBin + '\instsvc.exe');
            if not FileExists(Configs.PathBin + '\instreg.exe') then
              TFile.Copy(Configs.SourceBin + '\instreg.exe', Configs.PathBin + '\instreg.exe');

            CdBin := 'cd ' + Configs.PathBin;

            Log('Parando serviço');

            TUtils.ExecDos(CdBin + ' && instsvc stop -n ' + Configs.ServiceName);
          end;

          Application.ProcessMessages;

          Log('Verificando se há arquivos em uso');

          PathFiles := TDirectory.GetFiles(Configs.Path, '*', TSearchOption.soAllDirectories);

          for PathFileName in PathFiles do
          begin
            if TUtils.IsFileInUse(PathFileName) then
            begin
              FilesInUse := FilesInUse + #13#10 + PathFileName;
            end;
          end;

          if FilesInUse = '' then
          begin
            if DirectoryExists(Configs.PathBin) then
            begin
              Log('Desinstalando serviço');

              TUtils.ExecDos(CdBin + ' && instsvc r -n ' + Configs.ServiceName);

              Log('Desinstalando registro');

              TUtils.ExecDos(CdBin + ' && instreg r');
            end;

            Log('Deletando pasta de instalação');

            TDirectory.Delete(Configs.Path, true);

            Sleep(4000);

            Application.ProcessMessages;

            if DirectoryExists(Configs.Path) then
            begin
              Log('');
              Log('*******************************');
              Log('Desinstalação cancelada');
              Log('*******************************');

              ShowMessage('Erro ao desinstalar, verifique se o '+
              'NOME DO SERVIÇO é o referente ao da PASTA DE '+
              'INSTALAÇÃO, ou se a pasta de instalação está em USO!');
            end
            else
            begin
              Log('Deletando Dlls');

              DeleteDll;

              Log('Removendo do firewall');

              TUtils.DeleteFirewallPort('Firebird ' + Configs.ServiceName, Configs.Port);

              Log('');
              Log('*******************************');
              Log('Desinstalação concluída');
              Log('*******************************');

              ShowMessage('Desinstalação Concluída');
            end;
          end
          else
          begin
            Log('Reiniciando serviço');

            TUtils.ExecDos(CdBin + ' && instsvc start -n ' + Configs.ServiceName);

            Log('');
            Log('*******************************');
            Log('Desinstalação cancelada');
            Log('*******************************');

            ShowMessage('Erro ao desinstalar, os seguintes arquivos estão em USO '+
            TUtils.BreakLine + FilesInUse);
          end
        end
        else
        begin
          Log('');
          Log('*******************************');
          Log('Desinstalação cancelada');
          Log('*******************************');

          ShowMessage('Erro ao desinstalar, diretório INEXISTENTE!');
        end;
      end;
    end
    else
    begin
      Log('');
      Log('*******************************');
      Log('Desinstalação cancelada');
      Log('*******************************');
    end;
  except on E: Exception do
    Log('Erro: ' + E.Message);
  end;
end;

end.
