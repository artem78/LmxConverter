unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ProgramVersionStr: String;

implementation

uses
  FileInfo;

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

end.

