unit LandmarksConverter;

{
Formats documentation/schemas:
  LMX 1.0 - https://web.archive.org/web/20080611213659if_/http://www.forum.nokia.com/main/resources/technologies/location_based_services/lmx.xsd
  KML 2.2 - https://developers.google.com/kml/documentation/kmlreference
  GPX 1.1 - https://www.topografix.com/gpx/1/1/
}

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, math, fgl;

type
  TLandmarkAddress = record
    Street, PostalCode, City, State, Country, PhoneNumber: String;
    function ToString: String;
  end;

  TLandmark = class
  public
    Name, Description: String;
    Lat, Lon, Alt, HorAccuracy, VertAccuracy: Double;
    Address: TLandmarkAddress;
    Categories: TStringList;

    constructor Create;
    destructor Destroy; override;
  end;

  TLandmarks = specialize TFPGObjectList<TLandmark>;

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

    class function FileExtension: String; virtual; abstract; {static;}
  public
    constructor Create(AFileName: String);

    procedure WriteLandmark(Landmark: TLandmark); virtual; abstract;
  end;

  { Base abstract class for XML writer }
  TXMLLandmarksWriter = class(TBaseLandmarksWriter)
  private
    XML: TXMLDocument;

    class function FloatToStr(AFloat: Double): String; static;
  public
    constructor Create(AnInFileName: String);
    destructor Destroy; override;
  end;

  { KML writer }
  TKMLWriter = class(TXMLLandmarksWriter)
  private
    //RootElement: TDOMNode;

    class function FileExtension: String; {override;} static;

    function FindFolderNode(AFolderName: String): TDOMNode;
  public
    constructor Create(AnInFileName: String);
    destructor Destroy; override;

    procedure WriteLandmark(Landmark: TLandmark); override;
  end;

  { GPX writer }
  TGPXWriter = class(TXMLLandmarksWriter)
  private
    RootElement: TDOMNode;

    class function FileExtension: String; {override;} static;
  public
    constructor Create(AnInFileName: String);
    destructor Destroy; override;

    procedure WriteLandmark(Landmark: TLandmark); override;
  end;

  { LMX Writer }

  TLMXWriter = class(TXMLLandmarksWriter)
  private
    LmCollNode: TDOMNode;

    class function FileExtension: String; {override;} static;
  public
    constructor Create(AnInFileName: String);

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

    class function StrToFloat(const AStr: String): Double; static;
  public
    constructor Create(const AFileName: String); override;
    destructor Destroy; override;
  end;

  { KML reader }

  TKMLReader = class(TXMLLandmarksReader)
  public
    function ReadLandmarks: TLandmarks; override;
  end;

  { GPX reader}

  TGPXReader = class(TXMLLandmarksReader)
  public
    function ReadLandmarks: TLandmarks; override;
  end;

  { LMX reader }

  TLMXReader = class(TXMLLandmarksReader)
  public
    function ReadLandmarks: TLandmarks; override;
  end;

implementation

uses StrUtils;

const
  XMLDecimalSeparator: Char = '.';

{ TLandmarkAddress }

function TLandmarkAddress.ToString: String;
var
  AddrList: TStringList;
  I: Integer;
  S: String;
begin
  Result := '';

  AddrList := TStringList.Create;

  try
    AddrList.Add(Street);
    AddrList.Add(City);
    AddrList.Add(State);
    AddrList.Add(Country);
    AddrList.Add(PostalCode);

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

{ TLMXWriter }

class function TLMXWriter.FileExtension: String;
begin
  Result := 'lmx';
end;

constructor TLMXWriter.Create(AnInFileName: String);
var
  LMXNode: TDOMNode;
begin
  inherited;

  LMXNode := XML.CreateElement('lm:lmx');
  with (TDOMElement(LMXNode)) do
  begin
    SetAttribute('xmlns:lm', 'http://www.nokia.com/schemas/location/landmarks/1/0');
    SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
    SetAttribute('xsi:schemaLocation', 'http://www.nokia.com/schemas/location/landmarks/1/0/ lmx.xsd')
  end;
  XML.AppendChild(LMXNode);

  LmCollNode := XML.CreateElement('lm:landmarkCollection');
  //TDOMElement(LmCollNode).SetAttribute('name', 'Landmarks');
  LMXNode.AppendChild(LmCollNode);
end;

procedure TLMXWriter.WriteLandmark(Landmark: TLandmark);
var
  LandmarkElement, Element, CoordsElem, AddrInfoElem, CatElem: TDOMNode;
  CategoryName: String;
begin
  LandmarkElement := XML.CreateElement('lm:landmark');

  { Name }
  if not Landmark.Name.IsEmpty then
  begin
    Element := XML.CreateElement('lm:name');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Name));
    LandmarkElement.AppendChild(Element);
  end;

  { Description }
  if not Landmark.Description.IsEmpty then
  begin
    Element := XML.CreateElement('lm:description');
    //TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Description));
    TDOMDocument(Element).AppendChild(XML.CreateCDATASection(Landmark.Description));
    LandmarkElement.AppendChild(Element);
  end;

  { Coordinates }
  if (not IsNan(Landmark.Lon)) and (not IsNan(Landmark.Lat)) then
  begin
    CoordsElem := XML.CreateElement('lm:coordinates');

    Element := XML.CreateElement('lm:latitude');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(FloatToStr(Landmark.Lat)));
    CoordsElem.AppendChild(Element);

    Element := XML.CreateElement('lm:longitude');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(FloatToStr(Landmark.Lon)));
    CoordsElem.AppendChild(Element);

    if not IsNan(Landmark.Alt) then
    begin
      Element := XML.CreateElement('lm:altitude');
      TDOMDocument(Element).AppendChild(XML.CreateTextNode(FloatToStr(Landmark.Alt)));
      CoordsElem.AppendChild(Element);
    end;

    if not IsNan(Landmark.HorAccuracy) then
    begin
      Element := XML.CreateElement('lm:horizontalAccuracy');
      TDOMDocument(Element).AppendChild(XML.CreateTextNode(FloatToStr(Landmark.HorAccuracy)));
      CoordsElem.AppendChild(Element);
    end;

    if not IsNan(Landmark.VertAccuracy) then
    begin
      Element := XML.CreateElement('lm:verticalAccuracy');
      TDOMDocument(Element).AppendChild(XML.CreateTextNode(FloatToStr(Landmark.VertAccuracy)));
      CoordsElem.AppendChild(Element);
    end;

    LandmarkElement.AppendChild(CoordsElem);
  end;


  { Address }
  AddrInfoElem := XML.CreateElement('lm:addressInfo');

  if not Landmark.Address.Country.IsEmpty then
  begin
    Element := XML.CreateElement('lm:country');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Address.Country));
    AddrInfoElem.AppendChild(Element);
  end;

  if not Landmark.Address.State.IsEmpty then
  begin
    Element := XML.CreateElement('lm:state');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Address.State));
    AddrInfoElem.AppendChild(Element);
  end;

  if not Landmark.Address.City.IsEmpty then
  begin
    Element := XML.CreateElement('lm:city');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Address.City));
    AddrInfoElem.AppendChild(Element);
  end;

  if not Landmark.Address.PostalCode.IsEmpty then
  begin
    Element := XML.CreateElement('lm:postalCode');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Address.PostalCode));
    AddrInfoElem.AppendChild(Element);
  end;

  if not Landmark.Address.Street.IsEmpty then
  begin
    Element := XML.CreateElement('lm:street');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Address.Street));
    AddrInfoElem.AppendChild(Element);
  end;

  if not Landmark.Address.PhoneNumber.IsEmpty then
  begin
    Element := XML.CreateElement('lm:phoneNumber');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Address.PhoneNumber));
    AddrInfoElem.AppendChild(Element);
  end;

  if AddrInfoElem.ChildNodes.Count > 0 then
    LandmarkElement.AppendChild(AddrInfoElem)
  else
    AddrInfoElem.Free;


  { Categories }
  for CategoryName in Landmark.Categories do
  begin
    CatElem := XML.CreateElement('lm:category');
    Element := XML.CreateElement('lm:name');
    TDOMDocument(Element).AppendChild(XML.CreateTextNode(CategoryName));
    CatElem.AppendChild(Element);
    LandmarkElement.AppendChild(CatElem);
  end;


  LmCollNode.AppendChild(LandmarkElement);
end;

{ TKMLReader }

function TKMLReader.ReadLandmarks: TLandmarks;
var
  PlacemarkNodes: TDOMNodeList;
  PlacemarkNode: TDOMNode;
  Landmark: TLandmark;
  Idx: Integer;
  CoordsStr: String;
begin
  PlacemarkNodes := XML.DocumentElement.GetElementsByTagName('Placemark');
  Result := TLandmarks.Create();
  Result.Capacity := PlacemarkNodes.Count;
  for Idx := 0 to PlacemarkNodes.Count - 1 do
  begin
    PlacemarkNode := PlacemarkNodes[Idx];
    Landmark := TLandmark.Create;

    { Name }
    try
      Landmark.Name := PlacemarkNode.FindNode('name').TextContent;
    except
    end;

    { Description }
    try
      Landmark.Description := PlacemarkNode.FindNode('description').TextContent;
    except
    end;

    // ToDo: parse AddressDetails

    { Phone number }
    try
      Landmark.Address.PhoneNumber := PlacemarkNode.FindNode('phoneNumber').TextContent;
    except
    end;

    { Coordinates }
    try
      CoordsStr := PlacemarkNode.FindNode('Point').FindNode('coordinates').TextContent;
      Landmark.Lon := StrToFloat(ExtractDelimited(1, CoordsStr, [',']));
      Landmark.Lat := StrToFloat(ExtractDelimited(2, CoordsStr, [',']));
      try
        Landmark.Alt := StrToFloat(ExtractDelimited(3, CoordsStr, [',']));
      except
        Landmark.Alt := NaN;
      end;
    except
    end;

    { Category }
    try
      if PlacemarkNode.ParentNode.NodeName = 'Folder' then
      begin
        Landmark.Categories.Append(PlacemarkNode.ParentNode.FindNode('name').TextContent);
      end;
    except
    end;


    Result.Add(Landmark);
  end;

  PlacemarkNodes.Free;
end;

{ TGPXReader }

function TGPXReader.ReadLandmarks: TLandmarks;
var
  WptNodes: TDOMNodeList;
  WptNode: TDOMNode;
  Idx: Integer;
  Landmark: TLandmark;
begin
  WptNodes := XML.DocumentElement.GetElementsByTagName('wpt');
  Result := TLandmarks.Create();
  Result.Capacity := WptNodes.Count;
  for Idx := 0 to WptNodes.Count - 1 do
  begin
    WptNode := WptNodes[Idx];
    Landmark := TLandmark.Create;

    { Coordinates }

    try
      Landmark.Lat := StrToFloat(TDOMElement(WptNode).GetAttribute('lat'));
    except
    end;

    try
      Landmark.Lon := StrToFloat(TDOMElement(WptNode).GetAttribute('lon'));
    except
    end;

    try
      Landmark.Alt := StrToFloat(WptNode.FindNode('ele').TextContent);
    except
    end;


    { Name }
    try
      Landmark.Name := WptNode.FindNode('name').TextContent;
    except
    end;

    { Description }

    // ToDo: What is the difference between 'desc' (description) and 'cmt' (comment) elements?

    try
      Landmark.Description := WptNode.FindNode('desc').TextContent;
    except
    end;

    // Try to read 'cmt' tag as fallback
    if Landmark.Description.IsEmpty then
    begin
      try
        Landmark.Description := WptNode.FindNode('cmt').TextContent;
      except
      end;
    end;


    Result.Add(Landmark);

  end;

  WptNodes.Free;
end;

{ TLandmark }

constructor TLandmark.Create;
begin
  Name := '';
  Description := '';
  Lat := NaN;
  Lon := NaN;
  Alt := NaN;
  HorAccuracy := NaN;
  VertAccuracy := NaN;
  Address.Street := '';
  Address.PostalCode := '';
  Address.City := '';
  Address.State := '';
  Address.Country := '';
  Address.PhoneNumber := '';
  Categories := TStringList.Create;
end;

destructor TLandmark.Destroy;
begin
  Categories.Free;
  Categories := Nil;

  inherited Destroy;
end;

{ TLandmarksConverter }

constructor TLandmarksConverter.Create(const AInFileName: String{;
  const AOutFileName: String});
begin
  FProcessedLandmarks := 0;

  if AInFileName.EndsWith('.' + TKMLWriter.FileExtension, True) then
    Reader := TKMLReader.Create(AInFileName)
  else if AInFileName.EndsWith('.' + TGPXWriter.FileExtension, True) then
    Reader := TGPXReader.Create(AInFileName)
  else if AInFileName.EndsWith('.' + TLMXWriter.FileExtension, True) then
    Reader := TLMXReader.Create(AInFileName)
  else
    raise Exception.Create('Unsupported format!');

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

  if AnOutFileName.EndsWith('.' + TKMLWriter.FileExtension, True) then
    Writer := TKMLWriter.Create(AnOutFileName)
  else if AnOutFileName.EndsWith('.' + TGPXWriter.FileExtension, True) then
    Writer := TGPXWriter.Create(AnOutFileName)
  else if AnOutFileName.EndsWith('.' + TLMXWriter.FileExtension, True) then
    Writer := TLMXWriter.Create(AnOutFileName)
  else
    raise Exception.Create('Unsupported format!');

  try
    Landmarks := Reader.ReadLandmarks;
    try
      for Landmark in Landmarks do
      begin
        try
          Writer.WriteLandmark(Landmark);
          Inc(FProcessedLandmarks);
        except
        end;
      end;
    finally
      Landmarks.Free;
    end;
  finally
    Writer.Free;
  end;
end;

{ TXMLLandmarksReader }

class function TXMLLandmarksReader.StrToFloat(const AStr: String): Double;
var
  Fmt: TFormatSettings;
begin
  Fmt.DecimalSeparator := XMLDecimalSeparator;
  Result := SysUtils.StrToFloat(AStr, Fmt);
end;

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
  LandmarkNodes{, CategoryNodes}: TDOMNodeList;
  Idx, Idx2: Integer;
  Landmark: TLandmark;
begin
  LandmarkNodes := XML.DocumentElement.GetElementsByTagName('lm:landmark');
  Result := TLandmarks.Create();
  Result.Capacity := LandmarkNodes.Count;
  for Idx := 0 to LandmarkNodes.Count-1 do
  begin
    Landmark := TLandmark.Create;

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
          Landmark.Lat := StrToFloat(FindNode('lm:latitude').TextContent);
          Landmark.Lon := StrToFloat(FindNode('lm:longitude').TextContent);

          try
            Landmark.Alt := StrToFloat(FindNode('lm:altitude').TextContent)
          except
          end;

          try
            Landmark.HorAccuracy := StrToFloat(FindNode('lm:horizontalAccuracy').TextContent);
          except
          end;

          try
            Landmark.VertAccuracy := StrToFloat(FindNode('lm:verticalAccuracy').TextContent);
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


      Result.Add(Landmark);
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

class function TGPXWriter.FileExtension: String;
begin
  Result := 'gpx';
end;

procedure TGPXWriter.WriteLandmark(Landmark: TLandmark);
var
  WPTNode, EleNode, NameNode, DescNode: TDOMNode;
begin
  WPTNode := XML.CreateElement('wpt');
  TDOMElement(WPTNode).SetAttribute('lat', FloatToStr(Landmark.Lat));
  TDOMElement(WPTNode).SetAttribute('lon', FloatToStr(Landmark.Lon));
  RootElement.AppendChild(WPTNode);

  if not IsNan(Landmark.Alt) then
  begin
    EleNode := XML.CreateElement('ele');
    EleNode.TextContent := FloatToStr(Landmark.Alt);
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

class function TXMLLandmarksWriter.FloatToStr(AFloat: Double): String;
  var
  Fmt: TFormatSettings;
begin
  Fmt.DecimalSeparator := XMLDecimalSeparator;
  Result := SysUtils.FloatToStr(AFloat, Fmt);
end;

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

class function TKMLWriter.FileExtension: String;
begin
  Result := 'kml';
end;

procedure TKMLWriter.{ProcessLandmark}WriteLandmark(Landmark: TLandmark);
var
  PlacemarkElement, Element{, PlacemarksRootElement}: TDOMNode;
  DocumentNode, FolderNode, FolderNameNode: TDOMNode;
  CoordsStr: String;
  CatIdx: Integer;
  Category, Addr: String;
begin
  PlacemarkElement := XML.CreateElement('Placemark');

  { Name }
  Element := XML.CreateElement('name');
  TDOMDocument(Element).AppendChild(XML.CreateTextNode(Landmark.Name));
  PlacemarkElement.AppendChild(Element);

  { Address }
  // ToDo: Also write xal:AddressDetails element
  Addr := Landmark.Address.ToString;
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
  CoordsStr := FloatToStr(Landmark.Lon) + ',' +
               FloatToStr(Landmark.Lat);
  if not IsNan(Landmark.Alt) then      // Altitude is optional
    CoordsStr := CoordsStr + ',' + FloatToStr(Landmark.Alt);
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

