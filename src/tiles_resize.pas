{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Antic mode 4 tiles resize dialog
}
unit tiles_resize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SpinEx, lcltype, BCMaterialDesignButton;

type

  { TfrmTilesResize }

  TfrmTilesResize = class(TForm)
    btnClose : TBCMaterialDesignButton;
    btnConfirm : TBCMaterialDesignButton;
    editX : TSpinEditEx;
    editY : TSpinEditEx;
    Label21 : TLabel;
    Label22 : TLabel;
    procedure btnCloseClick(Sender : TObject);
    procedure btnConfirmClick(Sender : TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
  private

  public

  end;

var
  frmTilesResize : TfrmTilesResize;

implementation

{$R *.lfm}

uses
  antic4_tiles;

{ TfrmTilesResize }

procedure TfrmTilesResize.btnConfirmClick(Sender : TObject);
begin
  if MessageDlg('Question',
                'All tiles will be resized! Are you sure to proceed?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    frmAntic4Tiles.isTilesResizeProc := true;
    frmAntic4Tiles.antic4Tile.dimX := editX.Value;
    frmAntic4Tiles.antic4Tile.dimY := editY.Value;
  end
  else begin
    frmAntic4Tiles.isTilesResizeProc := false;
  end;

  Close;
end;

procedure TfrmTilesResize.btnCloseClick(Sender : TObject);
begin
  Close;
end;

procedure TfrmTilesResize.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

end.

