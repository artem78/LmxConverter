# Lmx Converter

![](img/screenshot.png)

Converts Nokia landmarks files (LMX) to another formats: [KML](https://en.wikipedia.org/wiki/Keyhole_Markup_Language) and [GPX](https://en.wikipedia.org/wiki/GPS_Exchange_Format). This may be useful if you want to export landmarks from your phone to other programs (like JOSM or Google Earth).

## How to get lmx-file from my phone?
1. Go to `Configurations` --> `Landmarks` or `Applications` --> `Location` --> `Landmarks` depending of your OS version
1. Select landmarks
1. In menu press `Send`, write any file name and choose suitable transferring method

![Landmarks export](img/landmarks-export.gif)

## Conversion example
input file `MyLandmarks.lmx`:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<lm:lmx xmlns:lm="http://www.nokia.com/schemas/location/landmarks/1/0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.nokia.com/schemas/location/landmarks/1/0/ lmx.xsd">
	<lm:landmarkCollection>
		<lm:name>Ориентиры Тест</lm:name>
		<lm:landmark>
			<lm:name>Малина оч круп</lm:name>
			<lm:description></lm:description>
			<lm:coordinates>
				<lm:latitude>55.958761251501</lm:latitude>
				<lm:longitude>37.164745545607</lm:longitude>
				<lm:altitude>203.5</lm:altitude>
				<lm:horizontalAccuracy>16.7166042327881</lm:horizontalAccuracy>
				<lm:verticalAccuracy>22</lm:verticalAccuracy>
			</lm:coordinates>
			<lm:addressInfo>
				<lm:street></lm:street>
				<lm:postalCode></lm:postalCode>
				<lm:city></lm:city>
				<lm:state></lm:state>
				<lm:country></lm:country>
				<lm:phoneNumber></lm:phoneNumber>
			</lm:addressInfo>
			<lm:category>
				<lm:name>Ягоды</lm:name>
			</lm:category>
		</lm:landmark>
		<lm:landmark>
			<lm:name>Оч. Интересное место</lm:name>
			<lm:description>Это описание гео-закладки. Оно может быть длинное и не очень, а также содержать различные символы вроде этих: @/;+&amp;%&lt;&gt;£€$¥¤[]{}~№#|§</lm:description>
			<lm:coordinates>
				<lm:latitude>56.1234</lm:latitude>
				<lm:longitude>44.9876</lm:longitude>
				<lm:altitude>123.669998168945</lm:altitude>
				<lm:horizontalAccuracy>101.620002746582</lm:horizontalAccuracy>
				<lm:verticalAccuracy>5.32000017166138</lm:verticalAccuracy>
			</lm:coordinates>
			<lm:addressInfo>
				<lm:street>Ленина</lm:street>
				<lm:postalCode>123456</lm:postalCode>
				<lm:city>Ленинск</lm:city>
				<lm:state>Ленинградская обл.</lm:state>
				<lm:country>Русь</lm:country>
				<lm:phoneNumber>+70001112233</lm:phoneNumber>
			</lm:addressInfo>
			<lm:category>
				<lm:name>Добавить на OSM</lm:name>
			</lm:category>
			<lm:category>
				<lm:name>Разное</lm:name>
			</lm:category>
			<lm:category>
				<lm:name>OSM необработанные</lm:name>
			</lm:category>
		</lm:landmark>
	</lm:landmarkCollection>
</lm:lmx>
```

output file `MyLandmarks.kml`:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://earth.google.com/kml/2.0">
  <Document>
    <Folder>
      <Placemark>
        <name>Малина оч круп</name>
        <Point>
          <coordinates>37.164745545607,55.958761251501,203.5</coordinates>
        </Point>
      </Placemark>
      <Placemark>
        <name>Оч. Интересное место</name>
        <description><![CDATA[Это описание гео-закладки. Оно может быть длинное и не очень, а также содержать различные символы вроде этих: @/;+&%<>£€$¥¤[]{}~№#|§]]></description>
        <Point>
          <coordinates>44.9876,56.1234,123.669998168945</coordinates>
        </Point>
      </Placemark>
    </Folder>
  </Document>
</kml>

```

output file `MyLandmarks.gpx`:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<gpx xmlns="http://www.topografix.com/GPX/1/1" version="1.1" creator="" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">
  <wpt lat="55.958761251501" lon="37.164745545607">
    <ele>203.5</ele>
    <name>Малина оч круп</name>
  </wpt>
  <wpt lat="56.1234" lon="44.9876">
    <ele>123.669998168945</ele>
    <name>Оч. Интересное место</name>
    <desc>Это описание гео-закладки. Оно может быть длинное и не очень, а также содержать различные символы вроде этих: @/;+&amp;%&lt;&gt;£€$¥¤[]{}~№#|§</desc>
  </wpt>
</gpx>
```
