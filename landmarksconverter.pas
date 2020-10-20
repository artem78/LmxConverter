unit LandmarksConverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, math;

type
  TLandmark = record
    Name, Description: String;
    Lat, Lon, Alt: Double;  // Note: Alt may be NaN
    // ToDo: Add more available fields
  end;

  { Abstract class for each landmarks converter }
  TBaseLandmarksConverter = class
  private
    InFileName, OutFileName: String;
    InXML: TXMLDocument;

    function GetFileExtension: String; virtual; abstract;
    procedure ProcessLandmark(Landmark: TLandmark); virtual; abstract;
  public
    constructor Create(AnInFileName: String);
    destructor Destroy; override;
    property FileExtension: String read GetFileExtension;
    procedure Convert(AnOutFileName: String);
  end;

  { Base abstract class for XML converter }
  TXMLLandmarksConverter = class(TBaseLandmarksConverter)
  private
    OutXML: TXMLDocument;
  public
    constructor Create(AnInFileName: String);
    destructor Destroy; override;
  end;

  { KML converter }
  TKMLLandmarksConverter = class(TXMLLandmarksConverter)
  private
    //RootElement: TDOMNode;

    function GetFileExtension: String; override;
    procedure ProcessLandmark(Landmark: TLandmark); override;
  public
    constructor Create(AnInFileName: String);
    destructor Destroy; override;
  end;

  { GPX converter }
  TGPXLandmarksConverter = class(TXMLLandmarksConverter)
  private
    RootElement: TDOMNode;

    function GetFileExtension: String; override;
    procedure ProcessLandmark(Landmark: TLandmark); override;
  public
    constructor Create(AnInFileName: String);
    destructor Destroy; override;
  end;

implementation

{ TGPXLandmarksConverter }

function TGPXLandmarksConverter.GetFileExtension: String;
begin
  Result := 'gpx';
end;

procedure TGPXLandmarksConverter.ProcessLandmark(Landmark: TLandmark);
var
  FixedFormat: TFormatSettings;
  WPTNode, EleNode, NameNode, DescNode: TDOMNode;
begin
  FixedFormat.DecimalSeparator := '.';

  WPTNode := OutXML.CreateElement('wpt');
  TDOMElement(WPTNode).SetAttribute('lat', FloatToStr(Landmark.Lat, FixedFormat));
  TDOMElement(WPTNode).SetAttribute('lon', FloatToStr(Landmark.Lon, FixedFormat));
  RootElement.AppendChild(WPTNode);

  if not IsNan(Landmark.Alt) then
  begin
    EleNode := OutXML.CreateElement('ele');
    EleNode.TextContent := FloatToStr(Landmark.Alt, FixedFormat);
    WPTNode.AppendChild(EleNode);
  end;

  if Landmark.Name <> '' then
  begin
    NameNode := OutXML.CreateElement('name');
    NameNode.TextContent := Landmark.Name;
    WPTNode.AppendChild(NameNode);
  end;

  if Landmark.Description <> '' then
  begin
    DescNode := OutXML.CreateElement('desc');
    DescNode.TextContent := Landmark.Description;
    WPTNode.AppendChild(DescNode);
  end;

end;

constructor TGPXLandmarksConverter.Create(AnInFileName: String);
var
  GPXNode: TDOMNode;
begin
  inherited;

  GPXNode := OutXML.CreateElement('gpx');
  with (TDOMElement(GPXNode)) do
  begin
    SetAttribute('xmlns', 'http://www.topografix.com/GPX/1/1');
    SetAttribute('version', '1.1');
    SetAttribute('creator', ''); // ToDo: add this
    SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
    SetAttribute('xsi:schemaLocation', 'http://www.topografix.com/GPX/1/1 ' +
                 'http://www.topografix.com/GPX/1/1/gpx.xsd')
  end;
  OutXML.AppendChild(GPXNode);
  RootElement := GPXNode;
end;

destructor TGPXLandmarksConverter.Destroy;
begin
  inherited Destroy;
end;

{ TBaseLandmarksConverter }

constructor TBaseLandmarksConverter.Create(AnInFileName: String);
begin
  Self.InFileName := AnInFileName;

  ReadXMLFile(InXML, Self.InFileName);
end;

destructor TBaseLandmarksConverter.Destroy;
begin
  InXML.Free;
end;

procedure TBaseLandmarksConverter.Convert(AnOutFileName: String);
var
  FixedFormat: TFormatSettings;
  LandmarkNodes: TDOMNodeList;
  Idx: Integer;
  Landmark: TLandmark;
  AltNode: TDOMNode;
begin
  OutFileName := AnOutFileName;

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
        if Assigned(AltNode) then
          Landmark.Alt := StrToFloat(AltNode.TextContent, FixedFormat)
        else
          Landmark.Alt := NaN;
      end;

      ProcessLandmark(Landmark);
    end;
  end;

  LandmarkNodes.Free;
end;

{ TXMLLandmarksConverter }

constructor TXMLLandmarksConverter.Create(AnInFileName: String);
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
  CoordsStr: String;
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
    //TDOMDocument(Element).AppendChild(OutXML.CreateTextNode(Landmark.Description));
    TDOMDocument(Element).AppendChild(OutXML.CreateCDATASection(Landmark.Description));
    PlacemarkElement.AppendChild(Element);
  end;

  { Coordinates }
  Element := PlacemarkElement.AppendChild(OutXML.CreateElement('Point'));
  Element := Element.AppendChild(OutXML.CreateElement('coordinates'));
  CoordsStr := FloatToStr(Landmark.Lon, FixedFormat) + ',' +
               FloatToStr(Landmark.Lat, FixedFormat);
  if not IsNan(Landmark.Alt) then      // Altitude is optional
    CoordsStr := CoordsStr + ',' + FloatToStr(Landmark.Alt, FixedFormat);
  Element := Element.AppendChild(OutXML.CreateTextNode(CoordsStr));


  //RootElement.AppendChild(PlacemarkElement);

  PlacemarksRootElement := OutXML.GetElementsByTagName('Folder').Item[0];
  PlacemarksRootElement.AppendChild(PlacemarkElement);

end;

constructor TKMLLandmarksConverter.Create(AnInFileName: String);
var
  KMLNode, DocumentNode, FolderNode: TDOMNode;
begin
  inherited;

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

