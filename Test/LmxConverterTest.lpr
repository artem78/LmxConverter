program LmxConverterTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testconverter;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

