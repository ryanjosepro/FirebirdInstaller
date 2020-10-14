unit MyUtils;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Variants, System.StrUtils, System.Zip,
  ShellAPI, Vcl.Forms, Windows, IOUtils, WinSvc,
  MyArrays;

type
  TStatusService =  (csNotInstalled, csStopped, csStartPending, csStopPending, csRunning,
   csContinuePending, csPausePending, csPaused);

  TUtils = class
  public
    class function Iif(Cond: boolean; V1, V2: variant): variant;
    class function IfLess(Value, Value2: integer): integer;
    class function IfEmpty(Value, Replace: string): string;
    class function IfZero(Value, Replace: integer): integer;

    class function IifLess(Cond: boolean; V1, V2: integer): integer;
    class function IifEmpty(Cond: boolean; V1, V2: string): string;
    class function IifZero(Cond: boolean; V1, V2: integer): integer;

    class function Cut(Text, Separator: string): TStringArray;

    class function ArrayToStr(StrArray: TStringArray; Separator: string; StrFinal: string; Starts: integer = 0; EndsBefore: integer = 0): string; overload;
    class function ArrayToStr(StrArray: System.TArray<System.string>; Separator: string; StrFinal: string; Starts: integer = 0; EndsBefore: integer = 0): string; overload;

    class function Extract(StrList: TStringList; Starts, Ends: integer): TStringList; overload;
    class function Extract(StrList: TStringList; Starts, Ends: string; IncStarts: boolean = true; IncEnds: boolean = true): TStringList; overload;
    class function Extract(StrList: TStringList; Starts: integer; Ends: string; IncEnds: boolean = false): TStringList; overload;
    class function Extract(StrList: TStringList; Starts: string; Ends: integer; IncStarts: boolean = false): TStringList; overload;

    class procedure ExecCmd(Comand: string; ShowCmd: integer = 1);
    class function ExecDos(CommandLine: string; Work: string = 'C:\'): string;

    class procedure DeleteIfExistsDir(Dir: string);
    class procedure DeleteIfExistsFile(FileName: string);
    class function GetLastFolder(Dir: String): String;
    class function AppPath: string;

    class function IsFileInUse(FileName: TFileName): Boolean; static;

    class procedure OpenOnExplorer(Path: string); static;

    class function BreakLine: string;

    class function Temp: string;

    class procedure AddFirewallPort(RuleName, Port: string);

    class procedure DeleteFirewallPort(RuleName: string);

    class procedure ExtractResourceZip(ResourceName, Path: string);

    class function GetServiceStatus(sMachine, sService: string): DWORD; static;
    class function GetServiceExecutablePath(strMachine, strServiceName: string): String; static;
    class procedure RemoveService(Service: string);
    class procedure StopService(Service: string);
    class procedure StartService(Service: string);
  end;

implementation

//Método para usar operador ternário
class function TUtils.Iif(Cond: boolean; V1, V2: variant): variant;
begin
  if Cond then
  begin
    Result := V1;
  end
  else
  begin
    Result := V2;
  end;
end;

//Retorna o menor valor
class function TUtils.IfLess(Value, Value2: integer): integer;
begin
  Result := Iif(Value < Value2, Value, Value2);
end;

//Retorna um substituto se o valor for vazio
class function TUtils.IfEmpty(Value, Replace: string): string;
begin
  Result := Iif(Value.Trim = '', Replace, Value);
end;

//Retorna um substituto se o valor for zero
class function TUtils.IfZero(Value, Replace: integer): integer;
begin
  Result := Iif(Value = 0, Replace, Value);
end;

//Iif e IfLess juntos num método só
class function TUtils.IifLess(Cond: boolean; V1, V2: integer): integer;
begin
  Result := Iif(Cond, V1, IfLess(V2, V1));
end;

//Iif e IfEmpty juntos num método só
class function TUtils.IifEmpty(Cond: boolean; V1, V2: string): string;
begin
  Result := Iif(Cond, V1, IfEmpty(V2, V1));
end;

//Iif e IfZero juntos num método só
class function TUtils.IifZero(Cond: boolean; V1, V2: integer): integer;
begin
  Result := Iif(Cond, V1, IfZero(V2, V1));
end;

//Divide uma string em array baseando-se no separador
class function TUtils.Cut(Text, Separator: string): TStringArray;
var
  StrArray: TStringDynArray;
  Cont: integer;
begin
  SetLength(StrArray, Length(SplitString(Text, Separator)));
  StrArray := SplitString(Text, Separator);
  SetLength(Result, Length(StrArray));
  for Cont := 0 to Length(StrArray) - 1 do
  begin
    Result[Cont] := StrArray[Cont];
  end;
end;

//Transforma um array em uma string
class function TUtils.ArrayToStr(StrArray: TStringArray; Separator, StrFinal: string; Starts: integer; EndsBefore: integer): string;
var
  Cont: integer;
begin
  Result := '';
  for Cont := TUtils.Iif(Starts >= Length(StrArray), 0, Starts) to Length(StrArray) - 1 - EndsBefore do
  begin
    if Cont = Length(StrArray) - 1 - EndsBefore then
    begin
      Result := Result + StrArray[Cont] + StrFinal;
    end
    else
    begin
      Result := Result + StrArray[Cont] + Separator;
    end;
  end;
end;

class function TUtils.ArrayToStr(StrArray: System.TArray<System.string>; Separator, StrFinal: string; Starts: integer; EndsBefore: integer): string;
var
  Cont: integer;
begin
  Result := '';
  for Cont := TUtils.Iif(Starts >= Length(StrArray), 0, Starts) to Length(StrArray) - 1 - EndsBefore do
  begin
    if Cont = Length(StrArray) - 1 - EndsBefore then
    begin
      Result := Result + StrArray[Cont] + StrFinal;
    end
    else
    begin
      Result := Result + StrArray[Cont] + Separator;
    end;
  end;
end;

//Extrai uma parte de uma StringList
class function TUtils.Extract(StrList: TStringList; Starts, Ends: integer): TStringList;
var
  Cont: integer;
begin
  Result := TStringList.Create;
  Ends := IfLess(Ends + 1, StrList.Count);
  for Cont := Starts to Ends do
  begin
    Result.Add(StrList[Cont]);
  end;
end;

class function TUtils.Extract(StrList: TStringList; Starts, Ends: string; IncStarts: boolean; IncEnds: boolean): TStringList;
var
  Cont: integer;
begin
  Result := TStringList.Create;
  Cont := 0;
  while StrList[Cont] <> Starts do
  begin
    Inc(Cont);
  end;

  for Cont := Iif(IncStarts, Cont, Cont + 1) to StrList.Count - 1 do
  begin
    if StrList[Cont] <> Ends then
    begin
      Result.Add(StrList[Cont]);
    end
    else
    begin
      if IncEnds then
      begin
        Result.Add(StrList[Cont]);
      end;
      Break;
    end;
  end;
end;

class function TUtils.Extract(StrList: TStringList; Starts: integer; Ends: string; IncEnds: boolean): TStringList;
var
  Cont: integer;
begin
  Result := TStringList.Create;
  for Cont := 0 to StrList.Count - 1 do
  begin
    if StrList[Cont] <> Ends then
    begin
      Result.Add(StrList[Cont]);
    end
    else
    begin
      if IncEnds then
      begin
        Result.Add(StrList[Cont]);
      end;
      Break;
    end;
  end;
end;

class function TUtils.Extract(StrList: TStringList; Starts: string; Ends: integer; IncStarts: boolean): TStringList;
var
  Cont: integer;
begin
  Result := TStringList.Create;
  Cont := 0;
  while StrList[Cont] <> Starts do
  begin
    Inc(Cont);
  end;

  for Cont := Iif(IncStarts, Cont, Cont + 1) to Ends do
  begin
    Result.Add(StrList[Cont]);
  end;
end;

//Executa um comando cmd - async
class procedure TUtils.ExecCmd(Comand: string; ShowCmd: integer = 1);
begin
  ShellExecute(0, nil, 'cmd.exe', PWideChar(Comand), nil, ShowCmd);
end;

//Executa um comando cmd - sync
class function TUtils.ExecDos(CommandLine: string; Work: string = 'C:\'): string;
var
  SecAtrrs: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  pCommandLine: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  WorkDir: string;
  Handle: Boolean;
begin
  Result := '';
  with SecAtrrs do begin
    nLength := SizeOf(SecAtrrs);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SecAtrrs, 0);
  try
    with StartupInfo do
    begin
      FillChar(StartupInfo, SizeOf(StartupInfo), 0);
      cb := SizeOf(StartupInfo);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;
    WorkDir := Work;
    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CommandLine),
                            nil, nil, True, 0, nil,
                            PChar(WorkDir), StartupInfo, ProcessInfo);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := windows.ReadFile(StdOutPipeRead, pCommandLine, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            pCommandLine[BytesRead] := #0;
            Result := Result + pCommandLine;
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      finally
        CloseHandle(ProcessInfo.hThread);
        CloseHandle(ProcessInfo.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;

//Métodos para gerenciar arquivos e diretórios
class procedure TUtils.DeleteIfExistsDir(Dir: string);
begin
  if TDirectory.Exists(Dir) then
    TDirectory.Delete(Dir, true);
end;

class procedure TUtils.DeleteIfExistsFile(FileName: string);
begin
  if FileExists(FileName) then
    TFile.Delete(FileName);
end;

class function TUtils.GetLastFolder(Dir: String): String;
var
  sa: TStringDynArray;
begin
  sa := SplitString(Dir, PathDelim);
  Result := sa[High(sa)];
end;

class function TUtils.AppPath: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

class function TUtils.IsFileInUse(FileName: TFileName): Boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  HFileRes := CreateFile(PChar(FileName),
                         GENERIC_READ or GENERIC_WRITE,
                         0,
                         nil,
                         OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL,
                         0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(HFileRes);
end;

class procedure TUtils.OpenOnExplorer(Path: string);
begin
  ShellExecute(0, PWideChar('explore'), PWideChar(Path), nil, nil, SW_SHOWNORMAL);
end;

//Retorna uma quebra de linha
class function TUtils.BreakLine: string;
begin
  Result := #13#10;
end;

//Retorna o diretório temp
class function TUtils.Temp: string;
begin
  Result := GetEnvironmentVariable('TEMP') + '\';
end;

class procedure TUtils.AddFirewallPort(RuleName, Port: string);
begin
  ExecDos('netsh advfirewall firewall add rule name="' + RuleName + '" dir=in action=allow protocol=TCP localport=' + Port);
  ExecDos('netsh advfirewall firewall add rule name="' + RuleName + '" dir=out action=allow protocol=TCP localport=' + Port);
end;

class procedure TUtils.DeleteFirewallPort(RuleName: string);
begin
  ExecDos('netsh advfirewall firewall delete rule name="' + RuleName + '"');
end;

class procedure TUtils.ExtractResourceZip(ResourceName, Path: string);
var
  Stream: TResourceStream;
  ZipFile: TZipFile;
begin
  Stream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  ZipFile := TZipFile.Create;

  try
    ZipFile.Open(Stream, zmRead);

    ZipFile.ExtractAll(Path);
  finally
    FreeAndNil(Stream);
    FreeAndNil(ZipFile);
  end;
end;

class function TUtils.GetServiceStatus(sMachine, sService: string): DWORD;
var
  schm, schs: SC_Handle;
  ss: TServiceStatus;
  dwStat: DWORD;
begin
  dwStat := 0;
  schm := OpenSCManager(PChar(sMachine), Nil, SC_MANAGER_CONNECT);
  if (schm > 0) then
  begin
    schs := OpenService(schm, PChar(sService), SERVICE_QUERY_STATUS);
    if (schs > 0) then
    begin
      if (QueryServiceStatus(schs, ss)) then
      begin
        dwStat := ss.dwCurrentState;
      end;
      CloseServiceHandle(schs);
    end;
    CloseServiceHandle(schm);
  end;
  Result := dwStat;
end;

class function TUtils.GetServiceExecutablePath(strMachine: string; strServiceName: string): String;
var
  hSCManager,hSCService: SC_Handle;
  lpServiceConfig: PQueryServiceConfigW;
  nSize, nBytesNeeded: DWord;
begin
  Result := '';
  hSCManager := OpenSCManager(PChar(strMachine), nil, SC_MANAGER_CONNECT);
  if (hSCManager > 0) then
  begin
    hSCService := OpenService(hSCManager, PChar(strServiceName), SERVICE_QUERY_CONFIG);
    if (hSCService > 0) then
    begin
      QueryServiceConfig(hSCService, nil, 0, nSize);
      lpServiceConfig := AllocMem(nSize);
      try
        if not QueryServiceConfig(hSCService, lpServiceConfig, nSize, nBytesNeeded) Then
        begin
          Exit;
        end;

        Result := lpServiceConfig^.lpBinaryPathName;
      finally
        Dispose(lpServiceConfig);
      end;
      CloseServiceHandle(hSCService);
    end;
  end;
end;

class procedure TUtils.RemoveService(Service: string);
begin
  TUtils.ExecDos('sc delete ' + Service);
end;

class procedure TUtils.StopService(Service: string);
begin
  TUtils.ExecDos('sc stop ' + Service);
end;

class procedure TUtils.StartService(Service: string);
begin
  TUtils.ExecDos('sc start ' + Service);
end;

end.
