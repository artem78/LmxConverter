unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLIntf, ExtCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    LogoImage: TImage;
    OpenGitHubButton: TButton;
    CheckForUpdatesButton: TButton;
    InfoLabel: TLabel;
    procedure CheckForUpdatesButtonClick(Sender: TObject);
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

uses Utils, Windows;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.OpenGitHubButtonClick(Sender: TObject);
begin
  OpenURL('https://github.com/artem78/LmxConverter#readme');
end;

procedure TAboutForm.CheckForUpdatesButtonClick(Sender: TObject);
begin
  OpenURL('https://github.com/artem78/LmxConverter/releases/latest');
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  InfoLabel.Caption := Format(InfoLabel.Caption,
          [ProgramVersionStr, {$I %DATE%} + ' ' + {$I %TIME%}]);
  //LogoImage.Picture.LoadFromResourceName(HINSTANCE, 'MAINICON');
  LogoImage.Picture.Icon.Handle := LoadImage(HInstance, 'MAINICON', IMAGE_ICON,
      64, 64, LR_DEFAULTCOLOR);
end;

end.

