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

  TOutputFormat = (ofmtKML=0, ofmtGPX);

var
  MainForm: TMainForm;

implementation

uses
  LandmarksConverter;

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
  InFileName, OutFileName: String;
  Converter: TBaseLandmarksConverter;
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


  // Create converter object depending of output format
  case TOutputFormat(OutputFormatRadioGroup.ItemIndex) of
    ofmtKML:
      Converter := TKMLLandmarksConverter.Create(InFileName);

    ofmtGPX:
      Converter := TGPXLandmarksConverter.Create(InFileName);

    else
    begin
      MessageDlg('Unknown format!', mtError, [mbOK], 0);
      Exit;
    end;
  end;

  OutFileName := ChangeFileExt(InFileName, '.' + Converter.FileExtension);
  if FileExists(OutFileName) then
  begin
    if MessageDlg('File "' + OutFileName + '" already exist. Overwrite?',
         mtConfirmation, mbYesNo, 0) <> mrYes then
    begin
      Converter.Free;
      Exit;
    end;
  end;

  try
    try
      Converter.Convert(OutFileName);
      SuccessMsg := Format('%d landmarks have been processed.',
                 [Converter.ProcessedLandmarks]);
      MessageDlg('Done!', SuccessMsg, mtInformation, [mbOK], 0);
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
    else     raise Exception.Create('Unknown format');
  end;
  IniPropStorage.WriteString('format', OutFormat);
end;

end.

