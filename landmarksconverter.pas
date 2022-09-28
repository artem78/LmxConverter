unit LandmarksConverter;

{
Formats documentation/schemas:
  LMX 1.0 - https://web.archive.org/web/20080611213659if_/http://www.forum.nokia.com/main/resources/technologies/location_based_services/lmx.xsd
  KML 2.2 - https://developers.google.com/kml/documentation/kmlreference
  GPX 1.1 - https://www.topografix.com/gpx/1/1/
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, math;

type
  TLandmarkAddress = record
    Street, PostalCode, City, State, Country, PhoneNumber: String;
  end;

  TLandmark = record
    Name, Description: String;
    Lat, Lon, Alt, HorAccuracy, VertAccuracy: Double;
    Address: TLandmarkAddress;
    Categories: TStringList;
  end;

  TLandmarks = array of TLandmark;

  TBaseLandmarksReader = class;
  TBaseLandmarksWriter = class;

  { Main converter class }

  TLandmarksConverter = class
  private
    Reader: TBaseLandmarksReader;
    Writer: TBaseLandmarksWriter;

    FProcessedLandmarks: Integer;
  public
    constructor Create(const AInFileName: String{; const AOutFileName: String});
    destructor Destroy; override;

    procedure Convert(const AnOutFileName: String);

    property ProcessedLandmarks: Integer read FProcessedLandmarks;
  end;

  { Abstract class for each landmarks writer }
  TBaseLandmarksWriter = class
  private
    FileName: String;

    function GetFileExtension: String; virtual; abstract;
  public
    constructor Create(AFileName: String);

    property FileExtension: String read GetFileExtension;
    procedure WriteLandmark(Landmark: TLandmark); virtual; abstract;
  end;

  { Base abstract class for XML writer }
  TXMLLandmarksWriter = class(TBaseLandmarksWriter)
  private
    XML: TXMLDocument;
  public
    constructor Create(AnInFileName: String);
    destructor Destroy; override;
  end;

  { KML writer }
  TKMLWriter = class(TXMLLandmarksWriter)
  private
    //RootElement: TDOMNode;

    function GetFileExtension: String; override;

    function FindFolderNode(AFolderName: String): TDOMNode;
    function Addr2Str(AnAddr: TLandmarkAddress): string;
  public
    constructor Create(AnInFileName: String);
    destructor Destroy; override;

    procedure WriteLandmark(Landmark: TLandmark); override;
  end;

  { GPX writer }
  TGPXWriter = class(TXMLLandmarksWriter)
  private
    RootElement: TDOMNode;

    function GetFileExtension: String; override;
  public
    constructor Create(AnInFileName: String);
    destructor Destroy; override;

    procedure WriteLandmark(Landmark: TLandmark); override;
  end;


  { Base class for any reader }

  TBaseLandmarksReader = class
  private
    FFileName: String;
  public
    constructor Create(const AFileName: String); virtual;

    function ReadLandmarks: TLandmarks; virtual; abstract;

    property FileName: String read FFileName;
  end;

  { Base XML reader }

  TXMLLandmarksReader = class(TBaseLandmarksReader)
  protected
    XML: TXMLDocument;
  public
    constructor Create(const AFileName: String); override;
    destructor Destroy; override;
  end;

  { LMX reader }

  TLMXReader = class(TXMLLandmarksReader)
  public
    function ReadLandmarks: TLandmarks; override;
  end;

implementation

{ TLandmarksConverter }

constructor TLandmarksConverter.Create(const AInFileName: String{;
  const AOutFileName: String});
begin
  FProcessedLandmarks := 0;

  Reader := TLMXReader.Create(AInFileName);
  //Writer := TXXXWriter.Create(AOutFileName);
end;

destructor TLandmarksConverter.Destroy;
begin
  inherited Destroy;

  //Writer.Free;
  Reader.Free;
end;

procedure TLandmarksConverter.Convert(const AnOutFileName: String);
var
  Landmark: TLandmark;
  Landmarks: TLandmarks;
begin
  FProcessedLandmarks := 0;

  if AnOutFileName.EndsWith('kml', True) then
    Writer := TKMLWriter.Create(AnOutFileName)
  else if AnOutFileName.EndsWith('gpx', True) then
    Writer := TGPXWriter.Create(AnOutFileName)
  else
    raise Exception.Create('Unsupported format!');

  try
    Landmarks := Reader.ReadLandmarks;
    for Landmark in Landmarks do
    begin
      try
        Writer.WriteLandmark(Landmark);
        Inc(FProcessedLandmarks);
      except
      end;
    end;
  finally
    Writer.Free;
  end;
end;

{ TXMLLandmarksReader }

constructor TXMLLandmarksReader.Create(const AFileName: String);
begin
  inherited Create(AFileName);

  ReadXMLFile(XML, FileName);
end;

destructor TXMLLandmarksReader.Destroy;
begin
  XML.Free;

  inherited Destroy;
end;

{ TLMXReader }

function TLMXReader.ReadLandmarks: TLandmarks;
var
  FixedFormat: TFormatSettings;
  LandmarkNodes{, CategoryNodes}: TDOMNodeList;
  Idx, Idx2: Integer;
  Landmark: TLandmark;
begin
  FixedFormat.DecimalSeparator := '.';

  LandmarkNodes := XML.DocumentElement.GetElementsByTagName('lm:landmark');
  SetLength(Result, LandmarkNodes.Count);
  for Idx := 0 to LandmarkNodes.Count-1 do
  begin
    // Set default (empty) values
    Landmark.Name := '';
    Landmark.Description := '';
    Landmark.Lat := NaN;
    Landmark.Lon := NaN;
    Landmark.Alt := NaN;
    Landmark.HorAccuracy := NaN;
    Landmark.VertAccuracy := NaN;
    Landmark.Address.Street := '';
    Landmark.Address.PostalCode := '';
    Landmark.Address.City := '';
    Landmark.Address.State := '';
    Landmark.Address.Country := '';
    Landmark.Address.PhoneNumber := '';
    Landmark.Categories := TStringList.Create;

    // Parse data from input xml
    with LandmarkNodes[Idx] do
    begin
      // Name
      try
        Landmark.Name := FindNode('lm:name').TextContent;
      except
      end;

      // Description
      try
        Landmark.Description := FindNode('lm:description').TextContent;
      except
      end;

      // Coordinates
      if Assigned(FindNode('lm:coordinates')) then
      begin
        with FindNode('lm:coordinates') do
        begin
          Landmark.Lat := StrToFloat(FindNode('lm:latitude').TextContent, FixedFormat);
          Landmark.Lon := StrToFloat(FindNode('lm:longitude').TextContent, FixedFormat);

          try
            Landmark.Alt := StrToFloat(FindNode('lm:altitude').TextContent, FixedFormat)
          except
          end;

          try
            Landmark.HorAccuracy := StrToFloat(FindNode('lm:horizontalAccuracy').TextContent, FixedFormat);
          except
          end;

          try
            Landmark.VertAccuracy := StrToFloat(FindNode('lm:verticalAccuracy').TextContent, FixedFormat);
          except
          end;
        end;
      end;

      // Address
      if Assigned(FindNode('lm:addressInfo')) then
      begin
        with FindNode('lm:addressInfo') do
        begin
          try
            Landmark.Address.Street := FindNode('lm:street').TextContent;
          except
          end;

          try
            Landmark.Address.PostalCode := FindNode('lm:postalCode').TextContent;
          except
          end;

          try
            Landmark.Address.City := FindNode('lm:city').TextContent;
          except
          end;

          try
            Landmark.Address.State := FindNode('lm:state').TextContent;
          except
          end;

          try
            Landmark.Address.Country := FindNode('lm:country').TextContent;
          except
          end;

          try
            Landmark.Address.PhoneNumber := FindNode('lm:phoneNumber').TextContent;
          except
          end;
        end;
      end;

      // Categories
      {CategoryNodes := TDOMDocument(LandmarkNodes[Idx]).GetElementsByTagName('lm:category'); // SIGPE error
      for Idx2 := 0 to CategoryNodes.Count - 1 do
        Landmark.Categories.Append(CategoryNodes[Idx2].FindNode('lm:name').TextContent);}
      for Idx2 := 0 to ChildNodes.Count - 1 do
      begin
        if (ChildNodes[Idx2].NodeType = ELEMENT_NODE)
            and (ChildNodes[Idx2].NodeName = 'lm:category') then
          Landmark.Categories.Append(ChildNodes[Idx2].FindNode('lm:name').TextContent);
      end;


      Result[Idx] := Landmark;
    end;
  end;

  LandmarkNodes.Free;
end;

{ TBaseLandmarksReader }

constructor TBaseLandmarksReader.Create(const AFileName: String);
begin
  FFileName := AFileName;
end;

{ TGPXWriter }

function TGPXWriter.GetFileExtension: String;
begin
  Result := 'gpx';
end;

procedure TGPXWriter.WriteLandmark(Landmark: TLandmark);
var
  FixedFormat: TFormatSettings;
  WPTNode, EleNode, NameNode, DescNode: TDOMNode;
begin
  FixedFormat.DecimalSeparator := '.';

  WPTNode := XML.CreateElement('wpt');
  TDOMElement(WPTNode).SetAttribute('lat', FloatToStr(Landmark.Lat, FixedFormat));
  TDOMElement(WPTNode).SetAttribute('lon', FloatToStr(Landmark.Lon, FixedFormat));
  RootElement.AppendChild(WPTNode);

  if not IsNan(Landmark.Alt) then
  begin
    EleNode := XML.CreateElement('ele');
    EleNode.TextContent := FloatToStr(Landmark.Alt, FixedFormat);
    WPTNode.AppendChild(EleNode);
  end;

  if Landmark.Name <> '' then
  begin
    NameNode := XML.CreateElement('name');
    NameNode.TextContent := Landmark.Name;
    WPTNode.AppendChild(NameNode);
  end;

  if Landmark.Description <> '' then
  begin
    DescNode := XML.CreateElement('desc');
    DescNode.TextContent := Landmark.Description;
    WPTNode.AppendChild(DescNode);
  end;

end;

constructor TGPXWriter.Create(AnInFileName: String);
var
  GPXNode: TDOMNode;
begin
  inherited;

  GPXNode := XML.CreateElement('gpx');
  with (TDOMElement(GPXNode)) do
  begin
    SetAttribute('xmlns', 'http://www.topografix.com/GPX/1/1');
    SetAttribute('version', '1.1');
    SetAttribute('creator', ''); // ToDo: add this
    SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
    SetAttribute('xsi:schemaLocation', 'http://www.topografix.com/GPX/1/1 ' +
                 'http://www.topografix.com/GPX/1/1/gpx.xsd')
  end;
  XML.AppendChild(GPXNode);
  RootElement := GPXNode;
end;

destructor TGPXWriter.Destroy;
begin
  inherited Destroy;
end;

{ TBaseLandmarksWriter }

constructor TBaseLandmarksWriter.Create(AFileName: String);
begin
  Self.FileName := AFileName;
end;

{ TXMLLandmarksWriter }

constructor TXMLLandmarksWriter.Create(AnInFileName: String);
begin
  inherited;

  XML := TXMLDocument.Create;
end;

destructor TXMLLandmarksWriter.Destroy;
begin
  if FileName <> '' then
    WriteXMLFile(XML, FileName);

  XML.Free;

  inherited;
end;

{ TKMLWriter }

function TKMLWriter.GetFileExtension: String;
begin
  Result := 'kml';
end;

procedure TKMLWriter.{ProcessLandmark}WriteLandmark(Landmark: TLandmark);
var
  FixedFormat: TFormatSettings;
  PlacemarkElement, Element{, PlacemarksRootElement}: TDOMNode;
  DocumentNode, FolderNode, FolderNameNode: TDOMNode;
  CoordsStr: String;
  CatIdx: Integer;
  Category, Addr: String;
begin
  FixedFormat.DecimalSeparator := '.';

  PlacemarkElement := XML.CreateElement('Placemark');

  { Name }
  Element := XML.CreateElement('name');
  TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Name));
  PlacemarkElement.AppendChild(Element);

  { Address }
  // ToDo: Also write xal:AddressDetails element
  Addr := Addr2Str(Landmark.Address);
  if Addr <> '' then
  begin
    Element := XML.CreateElement('address');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(Addr));
    PlacemarkElement.AppendChild(Element);
  end;

  { Phone number }
  if Landmark.Address.PhoneNumber <> '' then
  begin
    Element := XML.CreateElement('phoneNumber');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Address.PhoneNumber));
    PlacemarkElement.AppendChild(Element);
  end;

  { Description }
  if Landmark.Description <> '' then
  begin
    Element := XML.CreateElement('description');
    //TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Description));
    TDOMDocument(Element).AppendChild(XML.CreateCDATASection(Landmark.Description));
    PlacemarkElement.AppendChild(Element);
  end;

  { Coordinates }
  Element := PlacemarkElement.AppendChild(XML.CreateElement('Point'));
  Element := Element.AppendChild(XML.CreateElement('coordinates'));
  CoordsStr := FloatToStr(Landmark.Lon, FixedFormat) + ',' +
               FloatToStr(Landmark.Lat, FixedFormat);
  if not IsNan(Landmark.Alt) then      // Altitude is optional
    CoordsStr := CoordsStr + ',' + FloatToStr(Landmark.Alt, FixedFormat);
  Element := Element.AppendChild(XML.CreateTextNode(CoordsStr));

  { Category(ies) }
  // NOTE: LMX allow landmark to be included in more then one category.
  //    Therefore in KLM landmark may be duplicated in several folders.
  DocumentNode := XML.GetElementsByTagName('Document').Item[0];
  if Landmark.Categories.Count <> 0 then
  begin
    for CatIdx := 0 to Landmark.Categories.Count - 1 do
    begin
      Category := Landmark.Categories.Strings[CatIdx];
      FolderNode := FindFolderNode(Category);
      if not Assigned(FolderNode) then
      begin
        // Create non-exist folder
        FolderNode := XML.CreateElement('Folder');
        FolderNameNode := XML.CreateElement('name');
        FolderNameNode.AppendChild(XML.CreateTextNode(Category));
        FolderNode.AppendChild(FolderNameNode);
        DocumentNode.AppendChild(FolderNode);
      end;

      FolderNode.AppendChild(PlacemarkElement.CloneNode(True));
    end;
  end
  else
  begin
    //RootElement.AppendChild(PlacemarkElement);

    {PlacemarksRootElement := ;
    PlacemarksRootElement.AppendChild(PlacemarkElement);}

    DocumentNode.AppendChild(PlacemarkElement);
  end;
end;

function TKMLWriter.FindFolderNode(AFolderName: String): TDOMNode;
var
  FolderNodes: TDOMNodeList;
  Idx: integer;
begin
  FolderNodes := XML.GetElementsByTagName('Folder');
  for Idx := 0 to FolderNodes.Count - 1 do
  begin
    if FolderNodes[Idx].FindNode('name').TextContent = AFolderName then
    begin
      Result := FolderNodes[Idx];
      Exit;
    end;
  end;

  Result := nil; // Nothing found
end;

function TKMLWriter.Addr2Str(AnAddr: TLandmarkAddress): string;
var
  AddrList: TStringList;
  I: Integer;
  S: String;
begin
  Result := '';

  AddrList := TStringList.Create;

  try
    AddrList.Add(AnAddr.Street);
    AddrList.Add(AnAddr.City);
    AddrList.Add(AnAddr.State);
    AddrList.Add(AnAddr.Country);
    AddrList.Add(AnAddr.PostalCode);

    for I := 0 to AddrList.Count - 1 do
    begin
      S := Trim(AddrList.Strings[I]);
      if S <> '' then
        Result := Result + S + ', ';
    end;

    if Length(Result) >= 2 then
      Result := LeftStr(Result, Length(Result) - 2);

  finally
    AddrList.Free;
  end;
end;

constructor TKMLWriter.Create(AnInFileName: String);
var
  KMLNode, DocumentNode: TDOMNode;
begin
  inherited;

  KMLNode := XML.CreateElement('kml');
  TDOMElement(KMLNode).SetAttribute('xmlns', 'http://www.opengis.net/kml/2.2');
  XML.AppendChild(KMLNode);

  DocumentNode := XML.CreateElement('Document');
  KMLNode.AppendChild(DocumentNode);

//  RootElement := DocumentNode;
end;

destructor TKMLWriter.Destroy;
begin
  //RootElement.Free;

  inherited;
end;

end.

