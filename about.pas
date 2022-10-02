unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLIntf, winpeimagereader;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    OpenGitHubButton: TButton;
    CloseButton: TButton;
    CheckUpdatesButton: TButton;
    Label1: TLabel;
    procedure CheckUpdatesButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenGitHubButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses Utils;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.OpenGitHubButtonClick(Sender: TObject);
begin
  OpenURL('https://github.com/artem78/LmxConverter#readme');
end;

procedure TAboutForm.CheckUpdatesButtonClick(Sender: TObject);
begin
  OpenURL('https://github.com/artem78/LmxConverter/releases/latest');
end;

procedure TAboutForm.CloseButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Label1.Caption := Format(Label1.Caption, [ProgramVersionStr, {$I %DATE%} + ' ' + {$I %TIME%}]);
end;

end.

