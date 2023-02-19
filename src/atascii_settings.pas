unit atascii_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, lcltype;

type

  { TfrmAntic2Settings }

  TfrmAntic2Settings = class(TForm)
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ConfirmProc(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

var
  frmAntic2Settings: TfrmAntic2Settings;

implementation

{$R *.lfm}

uses
  antic2;

{ TfrmAntic2Settings }

procedure TfrmAntic2Settings.ConfirmProc(Sender: TObject);
begin
  Close;
end;

procedure TfrmAntic2Settings.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmAntic2Settings.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

