unit ulmxconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, IniPropStorage, EditBtn{, LCLProc}, about;

type

  TOutputFormat = (ofmtKML=0, ofmtGPX, ofmtLMX, ofmtGeoJSON);

  { TMainForm }

  TMainForm = class(TForm)
    AboutButton: TButton;
    DeleteInputFileCheckBox: TCheckBox;
    SameDirUsedCheckBox: TCheckBox;
    OutputDirEdit: TDirectoryEdit;
    OutputDirLabel: TLabel;
    OutputFormatLabel: TLabel;
    OutputFormatComboBox: TComboBox;
    InputFileNameEdit: TFileNameEdit;
    InputFileNameLabel: TLabel;
    OpenOutputDirButton: TButton;
    ConvertButton: TButton;
    IniPropStorage: TIniPropStorage;
    procedure AboutButtonClick(Sender: TObject);
    procedure ConvertButtonClick(Sender: TObject);
    procedure DeleteInputFileCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InputFileNameEditButtonClick(Sender: TObject);
    procedure InputFileNameEditChange(Sender: TObject);
    procedure OpenOutputDirButtonClick(Sender: TObject);
    procedure OutputDirEditChange(Sender: TObject);
    procedure OutputFormatComboBoxChange(Sender: TObject);
    procedure SameDirUsedCheckBoxChange(Sender: TObject);
  private
    function GetInputFileName: String;
    procedure SetInputFileName(const AFileName: string);
    function GetOutputFormat: TOutputFormat;
    procedure SetOutputFormat(AFormat: TOutputFormat);
    function GetOutputDir: String;
    procedure SetOutputDir(const ADir: String);
    function GetSameDirUsed: Boolean;
    procedure SetSameDirUsed(AVal: Boolean);
    function GetDeleteInputFile: Boolean;
    procedure SetDeleteInputFile(AVal: Boolean);

    procedure UseOutputDirFromInputFileName;
  public
    property InputFileName: String read GetInputFileName write SetInputFileName;
    property OutputFormat: TOutputFormat read GetOutputFormat write SetOutputFormat;
    property OutputDir: String read GetOutputDir write SetOutputDir;
    property SameDirUsed: Boolean read GetSameDirUsed write SetSameDirUsed;
    property DeleteInputFile: Boolean read GetDeleteInputFile write SetDeleteInputFile;
  end;

var
  MainForm: TMainForm;

implementation

uses
  LandmarksConverter, Utils, LCLIntf, LazFileUtils
  {$If defined(LINUX) and (FPC_FULlVERSION <= 30200)}
  , process
  {$EndIf}
  ;

{$R *.lfm}

function MyGetApplicationName: String;
begin
  Result := 'LMXConverter';
end;

{ TMainForm }

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.ConvertButtonClick(Sender: TObject);
var
  OutFileName, OutFileExt, NewOutFileNameOnly: String;
  Converter: TLandmarksConverter;
  FailMessage, SuccessMsg: String;
begin
  // Some checks before
  if InputFileName.IsEmpty or OutputDir.IsEmpty then
     Exit; // Nothing to do

  if not FileExists(InputFileName) then
  begin
    MessageDlg(Format('"%s" is not a valid input file!',
                      [InputFileName]), mtError, [mbOK], 0);
    Exit;
  end;

  if not DirectoryIsWritable(OutputDir) then
  begin
    MessageDlg(Format('"%s" is not a valid directory!',
                      [OutputDir]), mtError, [mbOK], 0);
    Exit;
  end;

  case OutputFormat of
    ofmtKML:
      OutFileExt := 'kml';

    ofmtGPX:
      OutFileExt := 'gpx';

    ofmtLMX:
      OutFileExt := 'lmx';

    ofmtGeoJSON:
      OutFileExt := 'geojson';

    else
    begin
      MessageDlg('Unknown format!', mtError, [mbOK], 0);
      Exit;
    end;
  end;

  OutFileName := ConcatPaths([OutputDir,
                 ExtractFileNameOnly(InputFileName)
                 + '.' + OutFileExt]);
  if FileExists(OutFileName) then
  begin
    if MessageDlg('File "' + OutFileName + '" already exist. Overwrite?',
         mtConfirmation, mbYesNo, 0) <> mrYes then
    begin
      NewOutFileNameOnly := CreateUniqueFileName(ExtractFileName(OutFileName), OutputDir);
      if InputQuery('New file name', '', NewOutFileNameOnly) then
        OutFileName := ConcatPaths([OutputDir, NewOutFileNameOnly])
      else
        Exit;
    end;
  end;

  // Create converter object
  Converter := TLandmarksConverter.Create(InputFileName);
  Converter.Creator := 'LMX Converter ' + ProgramVersionStr;

  try
    try
      Converter.Convert(OutFileName);
      if Converter.ProcessedLandmarks > 0 then
      begin
        SuccessMsg := Format('%d landmarks have been processed.',
                   [Converter.ProcessedLandmarks]);
        MessageDlg('Done!', SuccessMsg, mtInformation, [mbOK], 0);

        if DeleteInputFile then
          DeleteFile(InputFileName);
      end
      else
      begin
        MessageDlg('No landmarks found!', mtWarning, [mbOK], 0);
      end;

    except
      on E: Exception do
      begin
        FailMessage := 'Failed!';
        MessageDlg(FailMessage, mtError, [mbOK], 0);
      end;
    end;
  finally
    Converter.Free;
  end;
end;

procedure TMainForm.DeleteInputFileCheckBoxChange(Sender: TObject);
begin
  DeleteInputFile := DeleteInputFile; // Just for trigger save
end;

procedure TMainForm.FormCreate(Sender: TObject);
  function StrToFormat(AStr: String): TOutputFormat;
  var
    Code: Word;
  begin
    Val('ofmt' + AStr, Result, Code);
    if Code <> 0 then
      raise Exception.CreateFmt('Can''t convert string "%s" to TOutputFormat', [AStr]);
  end;

begin
  OnGetApplicationName := @MyGetApplicationName;

  {$IfDef Linux}
  if ProgramDirectory.StartsWith('/usr/bin') then
    IniPropStorage.IniFileName := ConcatPaths([GetAppConfigDir(False),
                                               IniPropStorage.IniFileName]);
  {$EndIf}

  // Read settings from INI file
  try
    OutputFormat := StrToFormat(IniPropStorage.ReadString('format', 'kml'));
  except // KML is default
    OutputFormat := ofmtKML;
  end;
  SameDirUsed := IniPropStorage.ReadBoolean('dirIgnored', True);
  if not SameDirUsed then
    OutputDir := IniPropStorage.ReadString('dir', '')
  else
    OutputDir := '';
  //InputFileName := IniPropStorage.ReadString('lastInputFile', '');
  InputFileNameEdit.InitialDir := ExtractFileDir(IniPropStorage.ReadString('lastInputFile', ''));
  DeleteInputFile := IniPropStorage.ReadBoolean('deleteInputFile', False);
end;

procedure TMainForm.InputFileNameEditButtonClick(Sender: TObject);
begin
  if not InputFileName.IsEmpty then
    InputFileNameEdit.InitialDir := ExtractFileDir(InputFileName);
end;

procedure TMainForm.InputFileNameEditChange(Sender: TObject);
begin
  InputFileName := InputFileName;

  UseOutputDirFromInputFileName;
end;

procedure TMainForm.OpenOutputDirButtonClick(Sender: TObject);
  {$If defined(LINUX) and (FPC_FULLVERSION <= 30200)}
  // Fix for older Lazarus versions (probably <= 2.0.12)
  procedure OpenDocument(APath: String);
  var
    Proc: TProcess;
  begin
    if not DirectoryExistsUTF8(APath) then
      Exit;

    Proc := TProcess.Create(Nil);
    try
      Proc.Executable := 'xdg-open';
      Proc.Parameters.Add(APath);
      Proc.Execute;
    finally
      Proc.Free;
    end;
  end;
  {$EndIf}
begin
  if not OutputDir.IsEmpty then
    OpenDocument(OutputDir);
end;

procedure TMainForm.OutputDirEditChange(Sender: TObject);
begin
  OutputDir := OutputDir; // Just for trigger save
end;

procedure TMainForm.OutputFormatComboBoxChange(Sender: TObject);
begin
  OutputFormat := OutputFormat; // Just for trigger save
end;

procedure TMainForm.SameDirUsedCheckBoxChange(Sender: TObject);
begin
  SameDirUsed := SameDirUsed; // Just for trigger save
end;

function TMainForm.GetInputFileName: String;
begin
  Result := InputFileNameEdit.Text;
end;

procedure TMainForm.SetInputFileName(const AFileName: string);
begin
  InputFileNameEdit.Text := AFileName;

  IniPropStorage.WriteString('lastInputFile', AFileName);
end;

function TMainForm.GetOutputFormat: TOutputFormat;
begin
  Result := TOutputFormat(OutputFormatComboBox.ItemIndex);
end;

procedure TMainForm.SetOutputFormat(AFormat: TOutputFormat);
  function FormatToStr(AFormat: TOutputFormat): String;
  begin
    Str(AFormat, Result);
    Result := LowerCase(Result);
    Result := StringReplace(Result, 'ofmt', '', []);
  end;

begin
  OutputFormatComboBox.ItemIndex := Ord(AFormat);

  // Save output format setting
  IniPropStorage.WriteString('format', FormatToStr(AFormat));
end;

function TMainForm.GetOutputDir: String;
begin
  Result := OutputDirEdit.Text;
end;

procedure TMainForm.SetOutputDir(const ADir: String);
begin
  OutputDirEdit.Text := ADir;

  // Save output directory setting
  IniPropStorage.WriteString('dir', ADir);
end;

function TMainForm.GetSameDirUsed: Boolean;
begin
  Result := SameDirUsedCheckBox.Checked;
end;

procedure TMainForm.SetSameDirUsed(AVal: Boolean);
begin
  SameDirUsedCheckBox.Checked := AVal;

  IniPropStorage.WriteBoolean('dirIgnored', AVal);

  OutputDirEdit.Enabled := not AVal;
  UseOutputDirFromInputFileName;
end;

function TMainForm.GetDeleteInputFile: Boolean;
begin
  Result := DeleteInputFileCheckBox.Checked;
end;

procedure TMainForm.SetDeleteInputFile(AVal: Boolean);
begin
  DeleteInputFileCheckBox.Checked := AVal;

  IniPropStorage.WriteBoolean('deleteInputFile', AVal);
end;

procedure TMainForm.UseOutputDirFromInputFileName;
begin
  if SameDirUsed then
    OutputDir := ExtractFileDir(InputFileName);
end;

end.

