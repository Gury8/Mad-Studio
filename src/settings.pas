{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
}
unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, lcltype;

type
  { TfrmGrSettings }
  TfrmGrSettings = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    chkDefaultColors: TCheckBox;
    chkGrMicPalette: TCheckBox;
    cmbGrMode: TComboBox;
//    DividerBevel2: TDividerBevel;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnOkProc(Sender: TObject);
    procedure btnCancelProc(Sender: TObject);
  end;

var
  frmGrSettings: TfrmGrSettings;

implementation

{$R *.lfm}

uses
  common, main, colors;

{ TfrmGrSettings }

procedure TfrmGrSettings.FormShow(Sender: TObject);
begin
  case grMode of
    grMode40x24x4  : cmbGrMode.ItemIndex := 0;
    grMode80x48x2  : cmbGrMode.ItemIndex := 1;
    grMode80x48x4  : cmbGrMode.ItemIndex := 2;
    grMode160x96x2 : cmbGrMode.ItemIndex := 3;
    grMode160x96x4 : cmbGrMode.ItemIndex := 4;
    grMode160x192x2: cmbGrMode.ItemIndex := 5;
    grMode160x192x4: cmbGrMode.ItemIndex := 6;
    grMode320x192x2: cmbGrMode.ItemIndex := 7;
  end;

  chkGrMicPalette.Checked := isGrMicPalette;
end;

procedure TfrmGrSettings.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmGrSettings.btnOkProc(Sender: TObject);
begin
  case cmbGrMode.ItemIndex of
    0: grMode := grMode40x24x4;
    1: grMode := grMode80x48x2;
    2: grMode := grMode80x48x4;
    3: grMode := grMode160x96x2;
    4: grMode := grMode160x96x4;
    5: grMode := grMode160x192x2;
    6: grMode := grMode160x192x4;
    7: grMode := grMode320x192x2;
  end;

  isGrMicPalette := chkGrMicPalette.Checked;

  if chkDefaultColors.Checked then
    frmColors.SetDefaultPalette;

  Close;
end;

procedure TfrmGrSettings.btnCancelProc(Sender: TObject);
begin
  Close;
end;

end.

