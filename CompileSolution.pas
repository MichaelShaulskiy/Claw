program CompileSolution;

{$mode objfpc}{$H+}

uses
  SysUtils, process, Classes, StrUtils;

var
  CompilerProcess: TProcess;
  ExitCode: Integer;
  LogFile: TextFile;

procedure LogMessage(const Msg: string);
begin
  WriteLn(Msg);
  WriteLn(LogFile, Msg);
  Flush(LogFile);
end;

function ExtractProjectPath(Line: string): string;
var
  Parts: TStringList;
begin
  Result := '';
  Delete(Line, 1, Pos('= ', Line) + 1);
  Line := Trim(Line);
  Parts := TStringList.Create;
  try
    ExtractStrings([','], [' '], PChar(Line), Parts);
    if Parts.Count >= 2 then
    begin
      Result := Trim(Parts[1]);
      Result := StringReplace(Result, '"', '', [rfReplaceAll]);
    end;
  finally
    Parts.Free;
  end;
end;

function ParseSolution(const SolutionPath: string; ProjectPaths: TStringList): Boolean;
var
  SolutionFile: TStringList;
  Line, ProjectPath: string;
  i: Integer;
begin
  Result := False;
  if not FileExists(SolutionPath) then
  begin
    LogMessage('Fehler: Solution-Datei nicht gefunden: ' + SolutionPath);
    Exit;
  end;

  SolutionFile := TStringList.Create;
  try
    SolutionFile.LoadFromFile(SolutionPath);
    for i := 0 to SolutionFile.Count - 1 do
    begin
      Line := Trim(SolutionFile[i]);
      if AnsiStartsText('Project(', Line) then
      begin
        ProjectPath := ExtractProjectPath(Line);
        if AnsiEndsText('.fsproj', ProjectPath) then
        begin
          ProjectPath := ExpandFileName(ExtractFilePath(SolutionPath) + ProjectPath);
          ProjectPaths.Add(ProjectPath);
          Result := True;
        end;
      end;
    end;
  finally
    SolutionFile.Free;
  end;

  if ProjectPaths.Count = 0 then
  begin
    LogMessage('Fehler: Keine Projekte in der Solution gefunden');
    Result := False;
  end;
end;

procedure ParseFsproj(const ProjectPath: string; SourceFiles, References: TStringList);
var
  ProjectFile: TStringList;
  Line: string;
  i, StartPos, EndPos: Integer;
begin
  if not FileExists(ProjectPath) then
  begin
    LogMessage('Fehler: Projektdatei nicht gefunden: ' + ProjectPath);
    Exit;
  end;

  ProjectFile := TStringList.Create;
  try
    ProjectFile.LoadFromFile(ProjectPath);
    for i := 0 to ProjectFile.Count - 1 do
    begin
      Line := Trim(ProjectFile[i]);
      if AnsiStartsText('<Compile Include="', Line) then
      begin
        StartPos := Pos('"', Line) + 1;
        EndPos := PosEx('"', Line, StartPos);
        if (StartPos > 0) and (EndPos > StartPos) then
        begin
          SourceFiles.Add(ExpandFileName(ExtractFilePath(ProjectPath) + Copy(Line, StartPos, EndPos - StartPos)));
        end;
      end
      else if AnsiStartsText('<HintPath>', Line) then
      begin
        StartPos := Pos('>', Line) + 1;
        EndPos := PosEx('</HintPath>', Line, StartPos);
        if (StartPos > 0) and (EndPos > StartPos) then
        begin
          References.Add(ExpandFileName(ExtractFilePath(ProjectPath) + Copy(Line, StartPos, EndPos - StartPos)));
        end;
      end;
    end;
  finally
    ProjectFile.Free;
  end;

  if SourceFiles.Count = 0 then
    LogMessage('Warnung: Keine .fs-Dateien in ' + ProjectPath + ' gefunden');

  if References.Count = 0 then
    LogMessage('Warnung: Keine Abhängigkeits-DLLs in ' + ProjectPath + ' gefunden');
end;

procedure CompileProject(const ProjectPath: string);
var
  SourceFiles, References: TStringList;
begin
  LogMessage('Kompiliere: ' + ProjectPath);

  CompilerProcess := TProcess.Create(nil);
  SourceFiles := TStringList.Create;
  References := TStringList.Create;
  try
    try
      // Abhängigkeiten wiederherstellen
      CompilerProcess.Executable := 'dotnet';
      CompilerProcess.Parameters.Add('restore');
      CompilerProcess.Parameters.Add(ProjectPath);
      CompilerProcess.Options := [poWaitOnExit, poUsePipes];
      CompilerProcess.Execute;
      if CompilerProcess.ExitStatus <> 0 then
      begin
        LogMessage('Fehler beim Wiederherstellen der Abhängigkeiten für ' + ProjectPath + ' Exit-Code: ' + IntToStr(CompilerProcess.ExitStatus));
        Exit;
      end;

      ParseFsproj(ProjectPath, SourceFiles, References);

      if SourceFiles.Count = 0 then
      begin
        LogMessage('Fehler: Keine kompilierbaren Dateien gefunden für ' + ProjectPath);
        Exit;
      end;

      // Projekt kompilieren
      CompilerProcess.Parameters.Clear;
      CompilerProcess.Parameters.Add('build');
      CompilerProcess.Parameters.Add(ProjectPath);
      CompilerProcess.Execute;
      ExitCode := CompilerProcess.ExitStatus;
      if ExitCode = 0 then
        LogMessage('Erfolgreich kompiliert: ' + ProjectPath)
      else
        LogMessage('Fehler beim Kompilieren von ' + ProjectPath + ' Exit-Code: ' + IntToStr(ExitCode));
    except
      on E: Exception do
        LogMessage('Ausführungsfehler: ' + E.Message);
    end;
  finally
    SourceFiles.Free;
    References.Free;
    CompilerProcess.Free;
  end;
end;

var
  ProjectPaths: TStringList;
  i: Integer;
begin
  AssignFile(LogFile, 'compile_log.txt');
  Rewrite(LogFile);
  try
    ProjectPaths := TStringList.Create;
    try
      if not ParseSolution('Claw.sln', ProjectPaths) then
        Exit;
      for i := 0 to ProjectPaths.Count - 1 do
        CompileProject(ProjectPaths[i]);
      LogMessage('Solution-Kompilierung abgeschlossen');
    finally
      ProjectPaths.Free;
    end;
  except
    on E: Exception do
    begin
      LogMessage('Programmfehler: ' + E.Message);
      ExitCode := 1;
    end;
  end;
  CloseFile(LogFile);
  Sleep(1000);
end.