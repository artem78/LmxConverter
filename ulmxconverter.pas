unit ulmxconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, IniPropStorage{, LCLProc}, about;

type

  TOutputFormat = (ofmtKML=0, ofmtGPX, ofmtLMX);

  { TMainForm }

  TMainForm = class(TForm)
    AboutButton: TButton;
    ChooseInputFileButton: TButton;
    ConvertButton: TButton;
    IniPropStorage: TIniPropStorage;
    InputFileNameEdit: TEdit;
    InputFileDialog: TOpenDialog;
    OutputFormatRadioGroup: TRadioGroup;
    procedure AboutButtonClick(Sender: TObject);
    procedure ChooseInputFileButtonClick(Sender: TObject);
    procedure ConvertButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OutputFormatRadioGroupClick(Sender: TObject);
  private
    function GetInputFileName: String;
    procedure SetInputFileName(const AFileName: string);
    function GetOutputFormat: TOutputFormat;
    procedure SetOutputFormat(AFormat: TOutputFormat);
  public
    property InputFileName: String read GetInputFileName write SetInputFileName;
    property OutputFormat: TOutputFormat read GetOutputFormat write SetOutputFormat;
  end;

var
  MainForm: TMainForm;

implementation

uses
  LandmarksConverter, Utils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ChooseInputFileButtonClick(Sender: TObject);
begin
  if InputFileDialog.Execute then;
     InputFileName := InputFileDialog.FileName;
end;

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

  OutFileName := ChangeFileExt(InputFileName, '.' + OutFileExt);
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
end;

procedure TMainForm.OutputFormatRadioGroupClick(Sender: TObject);
begin
  OutputFormat := OutputFormat; // Just for trigger save
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
  Result := TOutputFormat(OutputFormatRadioGroup.ItemIndex);
end;

procedure TMainForm.SetOutputFormat(AFormat: TOutputFormat);
  function FormatToStr(AFormat: TOutputFormat): String;
  begin
    Str(AFormat, Result);
    Result := LowerCase(Result);
    Result := StringReplace(Result, 'ofmt', '', []);
  end;

begin
  OutputFormatRadioGroup.ItemIndex := Ord(AFormat);

  // Save output format setting
  IniPropStorage.WriteString('format', FormatToStr(AFormat));
end;

end.

