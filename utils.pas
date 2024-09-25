unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ProgramVersionStr: String;
function CreateUniqueFileName(const AFileName, ADir: String): String;
function DumpExceptionCallStack(E: Exception): String;

implementation

uses
  FileInfo, {FileUtil,} LazFileUtils;

function ProgramVersionStr: String;
var
  FileVerInfo: TFileVersionInfo;
begin
  Result := 'unknown';

  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    Result := FileVerInfo.VersionStrings.Values['ProductVersion'];
    if Result = '' then
      Result := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

function CreateUniqueFileName(const AFileName, ADir: String): String;
var
  Num: Integer = 0;
  NewFileName: String;
begin
  NewFileName := AFileName;

  repeat
    Inc(Num);
    if Num > 1 then
      NewFileName := Format('%s (%d)%s', [ExtractFileNameOnly(AFileName),
                                          Num, ExtractFileExt(AFileName)]);
  until not FileExists(ConcatPaths([ADir, NewFileName]));

  Result := NewFileName;
end;

function DumpExceptionCallStack(E: Exception): String;
// https://wiki.freepascal.org/Logging_exceptions#Dump_exception_call_stack
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := {'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding} '';
  if E <> nil then begin
    Report := {Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding} E.ToString + LineEnding + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  //ShowMessage(Report);
  Result := Report;
  //Halt; // End of program execution
end;

end.

