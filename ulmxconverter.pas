unit ulmxconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls{, LCLProc};

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

  OutFileName := ExtractFileNameWithoutExt(LMXFilePath.Text) + '.' +
                 Converter.FileExtension;

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

