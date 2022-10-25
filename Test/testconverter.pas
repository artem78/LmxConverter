unit testconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, LandmarksConverter;

type

  TConverterTest= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLmxToGpx;
  private
    Converter: TLandmarksConverter;
  end;

implementation

uses Laz_XMLRead, Laz2_DOM, FileUtil;

const
  SrcDir = 'data';
  DstDir = 'out';

function StrToFloat(AStr: String): Extended;
var
  Fmt: TFormatSettings;
begin
  Fmt.DecimalSeparator := '.';
  Result := SysUtils.StrToFloat(AStr, Fmt);
end;

procedure TConverterTest.TestLmxToGpx;
var
  Doc: TXMLDocument;
  Wpt: TDOMNode;
Const
  LongDescr = 'This is the description of very interesting place. '
       + 'It can cantains special characters like @/;+&%<>£€$¥¤[]{}~№#|§. '
       + 'Также можно писать по-русски и даже по-китайски - 漢語, 汉语, 中文.';
begin
  Converter.Convert(ConcatPaths([DstDir, 'MyLandmarks.gpx']));

  ReadXMLFile(Doc, ConcatPaths([DstDir, 'MyLandmarks.gpx']));
  try
    CheckEquals(4, Doc.GetElementsByTagName('wpt').Count);

    // Check first landmark
    Wpt := Doc.DocumentElement.FindNode('wpt');
    CheckEquals('Mount Everest', Wpt.FindNode('name').TextContent);
    CheckEquals('The highest point of the Earth', Wpt.FindNode('desc').TextContent);
    CheckEquals(27.98806, StrToFloat(Wpt.Attributes.GetNamedItem('lat').TextContent));
    CheckEquals(86.92528, StrToFloat(Wpt.Attributes.GetNamedItem('lon').TextContent));
    CheckEquals(8848.86, StrToFloat(Wpt.FindNode('ele').TextContent));

    Wpt := Doc.DocumentElement.GetElementsByTagName('wpt').Item[1];
    CheckEquals(-10984, StrToFloat(Wpt.FindNode('ele').TextContent));

    // Check description with Unicode characters
    Wpt := Doc.DocumentElement.GetElementsByTagName('wpt').Item[2];
    CheckEquals(LongDescr, Wpt.FindNode('desc').TextContent);
  finally
    Doc.Free;
  end;
end;

procedure TConverterTest.SetUp;
begin
  if DirectoryExists(DstDir) then
    DeleteDirectory(DstDir, False);
  MkDir(DstDir);

  Converter := TLandmarksConverter.Create(ConcatPaths([SrcDir, 'MyLandmarks.lmx']));
end;

procedure TConverterTest.TearDown;
begin
  FreeAndNil(Converter);
end;

initialization

  RegisterTest(TConverterTest);
end.
