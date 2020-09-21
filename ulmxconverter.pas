unit ulmxconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,  laz2_DOM,  laz2_XMLRead{, LCLProc}, laz2_XMLWrite  ;

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

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ChooseLMXButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then;
     LMXFilePath.Text:=OpenDialog.FileName;
end;

procedure TMainForm.ConvertButtonClick(Sender: TObject);
var
  lmx, kml: TXMLDocument;
  landmarks_list : TDOMNodeList;
  landmark: TDOMNode;
  i: integer;
  //lon, lat: Double;
  name_, lat, lon, alt: String;
  kml_file_name: String;
  kml_root, kml_doc, kml_folder, kml_placemark, kml_placemark_point,
    kml_placemark_name, kml_placemark_coords, alt_node: TDOMNode;
begin
  // Some checks before
  if LMXFilePath.Text = '' then
     Exit; // Nothing to do

  if not FileExists(LMXFilePath.Text) then
  begin
    MessageDlg(Format('"%s" is not a valid input file!', [LMXFilePath.Text]), mtError, [mbOK], 0);
    Exit;
  end;


  case TOutputFormat(OutpuFormatRadioGroup.ItemIndex) of
    ofmtKML:
      begin
        //new_file_name := UTF8Copy( LMXFilePath.Text, 0, Length(LMXFilePath.Text)-3) + 'kml';
        kml_file_name := ExtractFileNameWithoutExt(LMXFilePath.Text) + '.kml';
        kml := TXMLDocument.Create;
        kml_root := kml.CreateElement('kml');
        TDOMElement(kml_root).SetAttribute('xmlns', 'http://earth.google.com/kml/2.0');
        kml.AppendChild(kml_root);
        kml_doc := kml.CreateElement('Document');
        kml_root.AppendChild(kml_doc);
        kml_folder := kml.CreateElement('Folder');
        kml_doc.AppendChild(kml_folder);

        ReadXMLFile(lmx, LMXFilePath.Text);
        try
          try
            landmarks_list := lmx.DocumentElement.GetElementsByTagName('lm:landmark');
            for i := 0 to landmarks_list.Count-1 do
            begin
              landmark := landmarks_list[i];
              name_ := landmark.FindNode('lm:name').TextContent;
              lat := landmark.FindNode('lm:coordinates').FindNode('lm:latitude').TextContent;
              lon := landmark.FindNode('lm:coordinates').FindNode('lm:longitude').TextContent;
              alt := '0';
              alt_node := landmark.FindNode('lm:coordinates').FindNode('lm:altitude');
              if alt_node <> nil then
                alt := alt_node.TextContent;

              kml_placemark := kml.CreateElement('Placemark');
              kml_placemark_name := kml.CreateElement('name');
              kml_placemark_name.AppendChild(kml.CreateTextNode(name_));
              kml_placemark.AppendChild(kml_placemark_name);
              kml_placemark_point := kml.CreateElement('Point');
              kml_placemark_coords := kml.CreateElement('coordinates');
              kml_placemark_coords.AppendChild(kml.CreateTextNode(Format('%s,%s,%s', [lon, lat, alt])));
              kml_placemark_point.AppendChild(kml_placemark_coords);
              kml_placemark.AppendChild(kml_placemark_point);
              kml_folder.AppendChild(kml_placemark);
            end;
            landmarks_list.Free;

            WriteXMLFile(kml, kml_file_name);

            ShowMessage('Done!');
          except
            //on e: Exception do ShowMessage('Error!' + #13 + #10 + #13 + #10 + String(e)) ;
            on e: Exception do ShowMessage('Error!') ;
          {else
            ShowMessage('Done!');}
          end;

        finally
          lmx.Free;
          kml.Free;
        end;
      end;
    {else
      Exit;}
  end;
end;

end.

