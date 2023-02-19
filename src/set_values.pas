{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Set values
}
unit set_values;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SpinEx, lcltype, ExtCtrls, BCMaterialDesignButton;

type
  { TfrmSetValues }
  TfrmSetValues = class(TForm)
    Bevel1 : TBevel;
    btnClose : TBCMaterialDesignButton;
    btnConfirm : TBCMaterialDesignButton;
    editText : TLabeledEdit;
    editX : TSpinEditEx;
    editY : TSpinEditEx;
    lblEditX : TLabel;
    lblEditY : TLabel;
    procedure FormShow(Sender : TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure CloseProc(Sender : TObject);
    procedure ConfirmProc(Sender : TObject);
  end;

var
  frmSetValues : TfrmSetValues;

implementation

{$R *.lfm}

uses
  common;

{ TfrmSetValues }

procedure TfrmSetValues.FormShow(Sender : TObject);
begin
  Caption := setValues.caption;
  setValues.valuesSet := false;
  editX.Enabled := setValues.editX > 0;
  editX.Visible := editX.Enabled;
  editY.Enabled := setValues.editY > 0;
  editY.Visible := editY.Enabled;
  editText.Enabled := setValues.editText = '';
  editText.Visible := editText.Enabled;

  editX.MinValue := setValues.minEditX;
  editX.MaxValue := setValues.maxEditX;
  editY.MinValue := setValues.minEditY;
  editY.MaxValue := setValues.maxEditY;
end;

procedure TfrmSetValues.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmSetValues.ConfirmProc(Sender : TObject);
begin
  if (setValues.warningText = '') or
     (MessageDlg('Warning', setValues.warningText,
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    setValues.valuesSet := true;
    setValues.editX := editX.Value;
    setValues.editY := editY.Value;
    setValues.editText := editText.Text;
  end;

  Close;
end;

procedure TfrmSetValues.CloseProc(Sender : TObject);
begin
  Close;
end;

end.

