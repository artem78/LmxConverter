unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ProgramVersionStr: String;
function CreateUniqueFileName(const AFileName, ADir: String): String;

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

end.

