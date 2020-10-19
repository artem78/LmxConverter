unit LandmarksConverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite;

type
  TLandmark = record
    Name, Description: String;
    Lat, Lon, Alt: Double;
  end;

  { Abstract class for each landmarks converter }

  { TBaseLandmarksConverter }

  TBaseLandmarksConverter = class
  private
    InFileName, OutFileName: String;
    InXML: TXMLDocument;

    // Getters/setters
    function GetFileExtension: String; virtual; abstract;

    // Custom methods
    procedure ProcessLandmark(Landmark: TLandmark); virtual; abstract;

  public
    // Constructor/destructor
    constructor Create(AnInFileName, AnOutFileName: String); {virtual;}
    destructor Destroy; {virtual;} override;

    // Custom methods
    property FileExtension: String read GetFileExtension;

    procedure Convert;
  end;

  { Base abstract class for XML converter }

  { TXMLLandmarksConverter }

  TXMLLandmarksConverter = class(TBaseLandmarksConverter)
  private
    OutXML: TXMLDocument;

    {procedure BeforeProcessing; virtual; abstract;
    procedure AfterProcessing; virtual; abstract;}
  public
    constructor Create(AnInFileName, AnOutFileName: String); {virtual;}
    destructor Destroy; {virtual;} override;
  end;

  { KML converter }

  { TKMLLandmarksConverter }

  TKMLLandmarksConverter = class(TXMLLandmarksConverter)
  private
    //RootElement: TDOMNode;

    function GetFileExtension: String; override;

    procedure ProcessLandmark(Landmark: TLandmark); override;
    //procedure BeforeProcessing; override;
    //procedure AfterProcessing; override;
  public
    constructor Create(AnInFileName, AnOutFileName: String); {virtual;}
    destructor Destroy; {virtual;} override;
  end;

implementation

{ TBaseLandmarksConverter }

constructor TBaseLandmarksConverter.Create(AnInFileName, AnOutFileName: String);
begin
  Self.InFileName := AnInFileName;
  Self.OutFileName := AnOutFileName;

  ReadXMLFile(InXML, Self.InFileName);
end;

destructor TBaseLandmarksConverter.Destroy;
begin
  InXML.Free;
end;

procedure TBaseLandmarksConverter.Convert;
var
  FixedFormat: TFormatSettings;
  LandmarkNodes: TDOMNodeList;
  Idx: Integer;
  Landmark: TLandmark;
  AltNode: TDOMNode;
begin
  FixedFormat.DecimalSeparator := '.';

  LandmarkNodes := InXML.DocumentElement.GetElementsByTagName('lm:landmark');
  for Idx := 0 to LandmarkNodes.Count-1 do
  begin
    with LandmarkNodes[Idx] do
    begin
      Landmark.Name        := FindNode('lm:name').TextContent;
      Landmark.Description := FindNode('lm:description').TextContent;
      with FindNode('lm:coordinates') do
      begin
        Landmark.Lat := StrToFloat(FindNode('lm:latitude').TextContent, FixedFormat);
        Landmark.Lon := StrToFloat(FindNode('lm:longitude').TextContent, FixedFormat);
        AltNode := FindNode('lm:altitude');
        if AltNode <> Nil then
          Landmark.Alt := StrToFloat(AltNode.TextContent, FixedFormat)
        else
          Landmark.Alt := 0.0;
      end;

      ProcessLandmark(Landmark);
    end;
  end;

  LandmarkNodes.Free;
end;

{ TXMLLandmarksConverter }

constructor TXMLLandmarksConverter.Create(AnInFileName, AnOutFileName: String);
begin
  inherited;

  OutXML := TXMLDocument.Create;
end;

destructor TXMLLandmarksConverter.Destroy;
begin
  WriteXMLFile(OutXML, OutFileName);

  OutXML.Free;

  inherited;
end;

{ TKMLLandmarksConverter }

function TKMLLandmarksConverter.GetFileExtension: String;
begin
  Result := 'kml';
end;

procedure TKMLLandmarksConverter.ProcessLandmark(Landmark: TLandmark);
var
  FixedFormat: TFormatSettings;
  PlacemarkElement, Element, PlacemarksRootElement: TDOMNode;
begin
  FixedFormat.DecimalSeparator := '.';

  PlacemarkElement := OutXML.CreateElement('Placemark');

  { Name }
  Element := OutXML.CreateElement('name');
  TDOMDocument(Element).AppendChild(OutXML.CreateTextNode(Landmark.Name));
  PlacemarkElement.AppendChild(Element);

  { Description }
  if Landmark.Description <> '' then
  begin
    Element := OutXML.CreateElement('description');
    TDOMDocument(Element).AppendChild(OutXML.CreateCDATASection(Landmark.Description));
    PlacemarkElement.AppendChild(Element);
  end;

  { Coordinates }
  Element := PlacemarkElement.AppendChild(OutXML.CreateElement('Point'));
  Element := Element.AppendChild(OutXML.CreateElement('coordinates'));
  Element := Element.AppendChild(OutXML.CreateTextNode(
          FloatToStr(Landmark.Lon, FixedFormat) + ',' +
          FloatToStr(Landmark.Lat, FixedFormat) + ',' +
          FloatToStr(Landmark.Alt, FixedFormat)
  ));



  //RootElement.AppendChild(PlacemarkElement);

  PlacemarksRootElement := OutXML.GetElementsByTagName('Folder').Item[0];
  PlacemarksRootElement.AppendChild(PlacemarkElement);

end;

{procedure TKMLLandmarksConverter.BeforeProcessing;
begin

end;  }

{procedure TKMLLandmarksConverter.AfterProcessing;
begin

end;}

constructor TKMLLandmarksConverter.Create(AnInFileName, AnOutFileName: String);
var
  {Element} KMLNode, DocumentNode, FolderNode: TDOMNode;
begin
  inherited;

  {Element := OutXML.CreateElement('kml');
  TDOMElement(Element).SetAttribute('xmlns', 'http://earth.google.com/kml/2.0');
  OutXML.AppendChild(Element);
  Element := Element.AppendChild(OutXML.CreateElement('Document'));
  RootElement := Element.AppendChild(OutXML.CreateElement('Folder')); }

  KMLNode := OutXML.CreateElement('kml');
  TDOMElement(KMLNode).SetAttribute('xmlns', 'http://earth.google.com/kml/2.0');
  OutXML.AppendChild(KMLNode);

  DocumentNode := OutXML.CreateElement('Document');
  KMLNode.AppendChild(DocumentNode);

  FolderNode := OutXML.CreateElement('Folder');
  DocumentNode.AppendChild(FolderNode);

//  RootElement := FolderNode;
end;

destructor TKMLLandmarksConverter.Destroy;
begin
  //RootElement.Free;

  inherited;
end;

end.

