{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Antic mode 4 tile editor character selector
}
unit char_set;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, lcltype, ExtCtrls, ComCtrls, BCMaterialDesignButton,
  common;

type
  { TfrmCharSet }
  TfrmCharSet = class(TForm)
    boxSelChar : TGroupBox;
    btnClose : TBCMaterialDesignButton;
    btnConfirm : TBCMaterialDesignButton;
    imgChar : TImage;
    imgFontSet : TImage;
    imgFontSetInv : TImage;
    Label1 : TLabel;
    Label2 : TLabel;
    memoInfo : TMemo;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure CloseProc(Sender : TObject);
    procedure CharConfirmProc(Sender : TObject);
    procedure imgFontSetDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure ButtonHoverLeave(Sender : TObject);
    procedure ButtonHoverEnter(Sender : TObject);
  private
    isFontSetNormal : boolean;
    factX, factY : byte;      // Character size factor
    factX02, factY02 : byte;  // Character set size factor
    fldFontSet : fldFontSetType;
    procedure ShowFontSet;
    procedure ShowChar(offset : integer);
  end;

var
  frmCharSet : TfrmCharSet;

implementation

{$R *.lfm}

uses
  antic4_tiles, lib;

{ TfrmCharSet }

procedure TfrmCharSet.FormCreate(Sender : TObject);
begin

end;

procedure TfrmCharSet.FormShow(Sender : TObject);
begin
  factX := 3; factY := factX;
  factX02 := 3; factY02 := factX02;
//  frmAntic4Tiles.charOffset := 1;
  isFontSetNormal := true;
  DefaultFontSet(fldFontSet);
  ShowFontSet;
  ShowChar(frmAntic4Tiles.charOffset);
end;

procedure TfrmCharSet.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmCharSet.ShowFontSet;
var
  n : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
begin
  FillRectEx(imgFontSet, coltabFont[1], 0, 0, imgFontSet.Width, imgFontSet.Height);
  FillRectEx(imgFontSetInv, coltabFont[1], 0, 0, imgFontSetInv.Width, imgFontSetInv.Height);
  offset := 0; yoffset := 0;
  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, 24);
    end;

    xoffset := offset*24;
    for yf := 0 to 7 do
      for xf := 0 to 7 do begin
        col := fldFontSet[xf, n shl 3 + yf];
//        col := SetColorIndex(col, true);
        FillRectEx(imgFontSet, coltabFont[col],
                   xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
//        col := SetColorIndex(col, false);

        // Inverse characters
        col := 1 - col;
        FillRectEx(imgFontSetInv, coltabFont[col],
                   xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
      end;

    Inc(offset);
  end;
end;

procedure TfrmCharSet.imgFontSetDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
var
  n, m : byte;
begin
//  showmessage('imgFontSetDown');
  isFontSetNormal := TImage(Sender).Tag = 0;

  for m := 0 to 7 do
    for n := 0 to 15 do begin
      if (x > 24) and (x <= 42) and (y > 24*m) and (y < 28 + 24*m) then begin
        frmAntic4Tiles.charOffset := m shl 4 + 1;
        ShowChar(frmAntic4Tiles.charOffset);
        break;
      end;
      if (x > 24*n) and (x <= 42 + 24*n) and (y > 24*m) and (y < 28 + 24*m) then begin
        frmAntic4Tiles.charOffset := n + m shl 4 + 1;
        if n = 0 then Dec(frmAntic4Tiles.charOffset);
        ShowChar(frmAntic4Tiles.charOffset);
        break;
      end;
    end;

  with memoInfo do begin
    Clear;

    // Calculate code number
    //n := frmAntic4Tiles.charOffset;
    //if (frmAntic4Tiles.charOffset >= 0) and (frmAntic4Tiles.charOffset <= 63) then
    //  n += 32
    //else if (frmAntic4Tiles.charOffset >= 64) and (frmAntic4Tiles.charOffset <= 95) then
    //  n -= 64;
    m := frmAntic4Tiles.charOffset;
    n := StrToInt(AtasciiCode(m));

    Lines.Add('Internal code Dec: ' + IntToStr(m) + ' Hex: ' + Dec2hex(m));
    if isFontSetNormal then
      Lines.Add('Atascii code Dec: ' + IntToStr(n) + ' Hex: ' + Dec2hex(n))
    else
      Lines.Add('Atascii code Dec: ' + IntToStr(n + 128) + ' Hex: ' + Dec2hex(n + 128));

//    Lines.Add('Inverse character value + 128');
  end;
end;

procedure TfrmCharSet.ShowChar(offset : integer);
var
  col, xf, yf : integer;
begin
  FillRectEx(imgChar, clBlack, 0, 0, imgChar.Width, imgChar.Height);
  offset := offset shl 3;

  for yf := 0 to _CHAR_DIM do
    for xf := 0 to _CHAR_DIM do begin
      col := fldFontSet[xf, yf + offset];
      if not isFontSetNormal then
        col := 1 - col;

      FillRectEx(imgChar, coltabFont[col], xf*factX, yf*factY, factX, factY);
    end;

  imgChar.Refresh;
end;

procedure TfrmCharSet.CharConfirmProc(Sender : TObject);
begin
//  isFontSetNormal := TImage(Sender).Tag = 0;
  frmAntic4Tiles.isCharConfirm := true;

  if not isFontSetNormal then
    Inc(frmAntic4Tiles.charOffset, 128);

  Close;
end;

procedure TfrmCharSet.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmCharSet.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

procedure TfrmCharSet.CloseProc(Sender : TObject);
begin
  Close;
end;

end.

