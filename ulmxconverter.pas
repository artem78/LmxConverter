unit ulmxconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls{,}  {laz2_DOM,  laz2_XMLRead}{, LCLProc}{, laz2_XMLWrite}  ;

type

  { TMainForm }

  TMainForm = class(TForm)
    ChooseLMXButton: TButton;
    ConvertButton: TButton;
    LMXFilePath: TEdit;
    OpenDialog: TOpenDialog;
    OutpuFormatRadioGroup: TRadioGroup;
    procedure ChooseLMXButtonClick(Sender: TObject);
    procedure ConvertButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TOutputFormat = (ofmtKML=0);

var
  MainForm: TMainForm;

implementation

uses
  LandmarksConverter;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ChooseLMXButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then;
     LMXFilePath.Text:=OpenDialog.FileName;
end;

procedure TMainForm.ConvertButtonClick(Sender: TObject);
var
  {lmx, kml: TXMLDocument;
  landmarks_list : TDOMNodeList;
  landmark: TDOMNode;
  i: integer;
  //lon, lat: Double;
  name_, lat, lon, alt, descr: String;
  kml_file_name: String;
  kml_root, kml_doc, kml_folder, kml_placemark, kml_placemark_point,
    kml_placemark_name, kml_placemark_descr, kml_placemark_coords, alt_node: TDOMNode;}

  OutFileName: String;
  Converter: TBaseLandmarksConverter;
begin
  // Some checks before
  if LMXFilePath.Text = '' then
     Exit; // Nothing to do

  if not FileExists(LMXFilePath.Text) then
  begin
    MessageDlg(Format('"%s" is not a valid input file!', [LMXFilePath.Text]), mtError, [mbOK], 0);
    Exit;
  end;


  // Create converter object depending of output format
  case TOutputFormat(OutpuFormatRadioGroup.ItemIndex) of
    ofmtKML:
      Converter := TKMLLandmarksConverter.Create(LMXFilePath.Text);

    else
    begin
      MessageDlg('Unknown format!', mtError, [mbOK], 0);
      Exit;
    end;
  end;

  //new_file_name := UTF8Copy( LMXFilePath.Text, 0, Length(LMXFilePath.Text)-3) + 'kml';
  OutFileName := ExtractFileNameWithoutExt(LMXFilePath.Text) + Converter.FileExtension;


  try
    try
      Converter.Convert(OutFileName);

      ShowMessage('Done!');
    except
      //on e: Exception do ShowMessage('Error!' + #13 + #10 + #13 + #10 + String(e)) ;
      on e: Exception do ShowMessage('Error!') ;
    end;
  finally
    Converter.Free;
  end;
end;

end.

