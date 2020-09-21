unit ulmxconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls,  laz2_DOM,  laz2_XMLRead{, LCLProc}, laz2_XMLWrite  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then;
     Edit1.Text:=OpenDialog1.FileName;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  lmx, kml: TXMLDocument;
  landmarks_list : TDOMNodeList;
  landmark: TDOMNode;
  i: integer;
  //lon, lat: Double;
  name_, lat, lon: String;
  kml_file_name: String;
  kml_root, kml_doc, kml_folder, kml_placemark, kml_placemark_point,
    kml_placemark_name, kml_placemark_coords: TDOMNode;
begin
  //new_file_name := UTF8Copy( Edit1.Text, 0, Length(Edit1.Text)-3) + 'kml';
  kml_file_name := ExtractFileNameWithoutExt(Edit1.Text) + '.kml';
  kml := TXMLDocument.Create;
  kml_root := kml.CreateElement('kml');
  TDOMElement(kml_root).SetAttribute('xmlns', 'http://earth.google.com/kml/2.0');
  kml.AppendChild(kml_root);
  kml_doc := kml.CreateElement('Document');
  kml_root.AppendChild(kml_doc);
  kml_folder := kml.CreateElement('Folder');
  kml_doc.AppendChild(kml_folder);

  ReadXMLFile(lmx, Edit1.Text);
  try
    try
      landmarks_list := lmx.DocumentElement.GetElementsByTagName('lm:landmark');
      for i := 0 to landmarks_list.Count-1 do
      begin
        landmark := landmarks_list[i];
        name_ := landmark.FindNode('lm:name').TextContent;
        lat := landmark.FindNode('lm:coordinates').FindNode('lm:latitude').TextContent;
        lon := landmark.FindNode('lm:coordinates').FindNode('lm:longitude').TextContent;


        kml_placemark := kml.CreateElement('Placemark');
        kml_placemark_name := kml.CreateElement('name');
        kml_placemark_name.AppendChild(kml.CreateTextNode(name_));
        kml_placemark.AppendChild(kml_placemark_name);
        kml_placemark_point := kml.CreateElement('Point');
        kml_placemark_coords := kml.CreateElement('coordinates');
        kml_placemark_coords.AppendChild(kml.CreateTextNode(Format('%s,%s,%s', [lon, lat, '0'])));
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

end.

