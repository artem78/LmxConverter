unit ulmxconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, IniPropStorage, EditBtn{, LCLProc}, about;

type

  TOutputFormat = (ofmtKML=0, ofmtGPX, ofmtLMX);

  { TMainForm }

  TMainForm = class(TForm)
    AboutButton: TButton;
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
    procedure FormCreate(Sender: TObject);
    procedure InputFileNameEditButtonClick(Sender: TObject);
    procedure InputFileNameEditChange(Sender: TObject);
    procedure OpenOutputDirButtonClick(Sender: TObject);
    procedure OutputDirEditChange(Sender: TObject);
    procedure OutputFormatRadioGroupClick(Sender: TObject);
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

    procedure UseOutputDirFromInputFileName;
  public
    property InputFileName: String read GetInputFileName write SetInputFileName;
    property OutputFormat: TOutputFormat read GetOutputFormat write SetOutputFormat;
    property OutputDir: String read GetOutputDir write SetOutputDir;
    property SameDirUsed: Boolean read GetSameDirUsed write SetSameDirUsed;
  end;

var
  MainForm: TMainForm;

implementation

uses
  LandmarksConverter, Utils, LCLIntf, LazFileUtils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.ConvertButtonClick(Sender: TObject);
var
  OutFileName, OutFileExt: String;
  Converter: TLandmarksConverter;
  FailMessage, SuccessMsg: String;
begin
  // Some checks before
  if InputFileName.IsEmpty then
     Exit; // Nothing to do

  if not FileExists(InputFileName) then
  begin
    MessageDlg(Format('"%s" is not a valid input file!',
                      [InputFileName]), mtError, [mbOK], 0);
    Exit;
  end;

  case OutputFormat of
    ofmtKML:
      OutFileExt := 'kml';

    ofmtGPX:
      OutFileExt := 'gpx';

    ofmtLMX:
      OutFileExt := 'lmx';

    else
    begin
      MessageDlg('Unknown format!', mtError, [mbOK], 0);
      Exit;
    end;
  end;

  OutFileName := ConcatPaths([OutputDir,
                 ExtractFileNameWithoutExt(ExtractFileNameOnly(InputFileName))
                 + '.' + OutFileExt]);
  if FileExists(OutFileName) then
  begin
    if MessageDlg('File "' + OutFileName + '" already exist. Overwrite?',
         mtConfirmation, mbYesNo, 0) <> mrYes then
    begin
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
end;

procedure TMainForm.InputFileNameEditButtonClick(Sender: TObject);
begin
  InputFileNameEdit.InitialDir := ExtractFileDir(InputFileName);
end;

procedure TMainForm.InputFileNameEditChange(Sender: TObject);
begin
  UseOutputDirFromInputFileName;
end;

procedure TMainForm.OpenOutputDirButtonClick(Sender: TObject);
begin
  if not OutputDir.IsEmpty then
    OpenDocument(OutputDir);
end;

procedure TMainForm.OutputDirEditChange(Sender: TObject);
begin
  OutputDir := OutputDir; // Just for trigger save
end;

procedure TMainForm.OutputFormatRadioGroupClick(Sender: TObject);
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

procedure TMainForm.UseOutputDirFromInputFileName;
begin
  if SameDirUsed then
    OutputDir := ExtractFileDir(InputFileName);
end;

end.

