unit ulmxconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, IniPropStorage{, LCLProc}, about;

type

  { TMainForm }

  TMainForm = class(TForm)
    AboutButton: TButton;
    ChooseInputFileButton: TButton;
    ConvertButton: TButton;
    IniPropStorage: TIniPropStorage;
    InputFilePathEdit: TEdit;
    OpenDialog: TOpenDialog;
    OutputFormatRadioGroup: TRadioGroup;
    procedure AboutButtonClick(Sender: TObject);
    procedure ChooseInputFileButtonClick(Sender: TObject);
    procedure ConvertButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OutputFormatRadioGroupClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TOutputFormat = (ofmtKML=0, ofmtGPX, ofmtLMX);

var
  MainForm: TMainForm;

implementation

uses
  LandmarksConverter, Utils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ChooseInputFileButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then;
     InputFilePathEdit.Text := OpenDialog.FileName;
end;

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.ConvertButtonClick(Sender: TObject);
var
  InFileName, OutFileName, OutFileExt: String;
  Converter: TLandmarksConverter;
  FailMessage, SuccessMsg: String;
begin
  InFileName := InputFilePathEdit.Text;

  // Some checks before
  if InFileName = '' then
     Exit; // Nothing to do

  if not FileExists(InFileName) then
  begin
    MessageDlg(Format('"%s" is not a valid input file!', [InFileName]), mtError, [mbOK], 0);
    Exit;
  end;

  case TOutputFormat(OutputFormatRadioGroup.ItemIndex) of
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

  OutFileName := ChangeFileExt(InFileName, '.' + OutFileExt);
  if FileExists(OutFileName) then
  begin
    if MessageDlg('File "' + OutFileName + '" already exist. Overwrite?',
         mtConfirmation, mbYesNo, 0) <> mrYes then
    begin
      Exit;
    end;
  end;

  // Create converter object
  Converter := TLandmarksConverter.Create(InFileName);
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
var
  OutFormat: String;
begin
  // Read settings from INI file
  OutFormat := IniPropStorage.ReadString('format', 'kml');
  if OutFormat = 'gpx' then
     OutputFormatRadioGroup.ItemIndex := Ord(ofmtGPX)
  else if OutFormat = 'lmx' then
     OutputFormatRadioGroup.ItemIndex := Ord(ofmtLMX)
  else // KML is default
     OutputFormatRadioGroup.ItemIndex := Ord(ofmtKML);
end;

procedure TMainForm.OutputFormatRadioGroupClick(Sender: TObject);
var
  OutFormat: String;
begin
  // Save settings for output format
  case TOutputFormat(OutputFormatRadioGroup.ItemIndex) of
    ofmtKML: OutFormat := 'kml';
    ofmtGPX: OutFormat := 'gpx';
    ofmtLMX: OutFormat := 'lmx';
    else     raise Exception.Create('Unknown format');
  end;
  IniPropStorage.WriteString('format', OutFormat);
end;

end.

