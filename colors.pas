{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Color palette
}
unit colors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  { TfrmColors }
  TfrmColors = class(TForm)
    lblColor4: TLabel;
    lblColor0: TLabel;
    lblColor1: TLabel;
    lblColor2: TLabel;
    lblPcolr0: TLabel;
    lblPcolr1: TLabel;
    lblPcolr2: TLabel;
    lblPcolr3: TLabel;
    lblColor3: TLabel;
    shapeColor0: TShape;
    shapeColor1: TShape;
    shapeColor2: TShape;
    shapeColor3: TShape;
    shapeColor4: TShape;
    shapeColor704: TShape;
    shapeColor705: TShape;
    shapeColor706: TShape;
    shapeColor707: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure shapeColor1MouseLeave(Sender: TObject);
    procedure shapeColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure shapeColorMouseEnter(Sender: TObject);
  private
    { private declarations }
    keyScan : byte;
    procedure colorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
  public
    { public declarations }
    SelColor : byte;
    ColorValue : byte;
  end;

var
  frmColors: TfrmColors;

implementation

{$R *.lfm}

uses
  common, pmg, animator;

{ TfrmColors }

procedure TfrmColors.FormCreate(Sender: TObject);
var
  colorx : TShape;
  i, j, y : integer;
begin
  Left := 10;
  Top := 10;

  j := 0;
  y := -14;

  //if Screen.ActiveFrom = frmPmg then begin
  //  keyScan := 5;
  //  SelColor := 5;
  //end
  //else
  keyScan := 1;
  SelColor := 1;
//  end;

  //colTab[0] := colorMem[0];
  //colTab[1] := colorMem[74];
  //colTab[2] := colorMem[20];
  //colTab[3] := colorMem[101];
  //colTab[8] := RGBToColor(255, 255, 255);

  for i := 0 to 127 do begin
    colorx := TShape.Create(Self);
    colorx.Parent := self;
    colorx.Name := 'color' + inttostr(i);
    colorx.Width := 21;
    colorx.Height := 21;
    inc(j);

    if i mod 8 = 0 then begin
      j := 0;
      inc(y, 23);
    end;

    colorx.Left := j*colorx.Width + j + j + 12;
    colorx.Top := y;
    colorx.OnMouseDown := @colorMouseDown;
    colorx.Tag := i;
    colorx.Hint := 'Dec: ' + IntToStr(i + i) + ' Hex: ' + IntToHex(i + i, 2);
    colorx.ParentShowHint := false;
    colorx.ShowHint := true;
  end;
end;

procedure TfrmColors.FormShow(Sender: TObject);
begin
  propFlagModules[9] := 1;
  isChange := true;

  if formId = formPmg then
    keyScan := 4
  else
    keyScan := 1;

  SelColor := keyScan;

  shapeColor0.Brush.Color := coltab[0];
  shapeColor1.Brush.Color := coltab[1];
  shapeColor2.Brush.Color := coltab[2];
  shapeColor3.Brush.Color := coltab[3];
  shapeColor4.Brush.Color := coltab[10];

  shapeColor704.Brush.Color := coltab[4];
  shapeColor705.Brush.Color := coltab[5];
  shapeColor706.Brush.Color := coltab[6];
  shapeColor707.Brush.Color := coltab[7];
end;

procedure TfrmColors.FormActivate(Sender: TObject);
begin
//  FormShow(Sender);

  //if formId = formPmg then
  //  keyScan := 5
  //else
  //  keyScan := 1;

  if formId = formPmg then begin
    keyScan := frmPmg.player + 4;

    lblColor4.Font.Bold := false;
    lblColor0.Font.Bold := false;
    lblColor1.Font.Bold := false;
    lblColor2.Font.Bold := false;
    lblColor3.Font.Bold := false;
    lblPcolr0.Font.Bold := false;
    lblPcolr1.Font.Bold := false;
    lblPcolr2.Font.Bold := false;
    lblPcolr3.Font.Bold := false;

    case keyScan of
      0: lblColor4.Font.Bold := true;
      1: lblColor0.Font.Bold := true;
      2: lblColor1.Font.Bold := true;
      3: lblColor2.Font.Bold := true;
      4: lblPcolr0.Font.Bold := true;
      5: lblPcolr1.Font.Bold := true;
      6: lblPcolr2.Font.Bold := true;
      7: lblPcolr3.Font.Bold := true;
      10: lblColor3.Font.Bold := true;
    end;
  end;

  SelColor := keyScan;
end;

procedure TfrmColors.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    48: keyScan := 0;
    49: keyScan := 1;
    50: keyScan := 2;
    51: keyScan := 3;
    52: keyScan := 4;
    53: keyScan := 5;
    54: keyScan := 6;
    55: keyScan := 7;
    56: keyScan := 10;
  end;
end;

procedure TfrmColors.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[9] := 0;
end;

procedure TfrmColors.shapeColor1MouseLeave(Sender: TObject);
begin
  //TShape(Sender).Canvas.Pen.Color := clDefault;
//  TShape(Sender).Canvas.Rectangle(bounds(0, 0, TShape(Sender).width, TShape(Sender).Height));
  //TShape(Sender).Canvas.Rectangle(0, 0, TShape(Sender).width, TShape(Sender).Height);
end;

procedure TfrmColors.shapeColorMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SelColor := TShape(Sender).Tag;
  KeyScan := SelColor;
(*shapeColor0.Canvas.Pen.Color := colorMem[shapeColor0.Tag];
  shapeColor0.Canvas.Rectangle(1, 1, TShape(Sender).width - 1, TShape(Sender).Height - 1);
  shapeColor1.Canvas.Pen.Color := colorMem[shapeColor1.Tag];
  shapeColor1.Canvas.Rectangle(1, 1, TShape(Sender).width - 1, TShape(Sender).Height - 1);
  shapeColor2.Canvas.Pen.Color := colorMem[shapeColor2.Tag];
  shapeColor2.Canvas.Rectangle(1, 1, TShape(Sender).width - 1, TShape(Sender).Height - 1);
  shapeColor3.Canvas.Pen.Color := colorMem[shapeColor3.Tag];
  shapeColor3.Canvas.Rectangle(1, 1, TShape(Sender).width - 1, TShape(Sender).Height - 1);
  shapeColor4.Canvas.Pen.Color := colorMem[shapeColor4.Tag];
  shapeColor4.Canvas.Rectangle(1, 1, TShape(Sender).width - 1, TShape(Sender).Height - 1);
  shapeColor704.Canvas.Pen.Color := colorMem[shapeColor704.Tag];
  shapeColor704.Canvas.Rectangle(1, 1, TShape(Sender).width - 1, TShape(Sender).Height - 1);
  shapeColor705.Canvas.Pen.Color := colorMem[shapeColor705.Tag];
  shapeColor705.Canvas.Rectangle(1, 1, TShape(Sender).width - 1, TShape(Sender).Height - 1);
  shapeColor706.Canvas.Pen.Color := colorMem[shapeColor706.Tag];
  shapeColor706.Canvas.Rectangle(1, 1, TShape(Sender).width - 1, TShape(Sender).Height - 1);
  shapeColor707.Canvas.Pen.Color := colorMem[shapeColor707.Tag];
  shapeColor707.Canvas.Rectangle(1, 1, TShape(Sender).width - 1, TShape(Sender).Height - 1);

  TShape(Sender).Canvas.Pen.Color := clBlack;
//  TShape(Sender).Canvas.Rectangle(bounds(0, 0, TShape(Sender).width, TShape(Sender).Height));
  TShape(Sender).Canvas.Rectangle(1, 1, TShape(Sender).width - 1, TShape(Sender).Height - 1);*)

  lblColor4.Font.Bold := false;
  lblColor0.Font.Bold := false;
  lblColor1.Font.Bold := false;
  lblColor2.Font.Bold := false;
  lblColor3.Font.Bold := false;
  lblPcolr0.Font.Bold := false;
  lblPcolr1.Font.Bold := false;
  lblPcolr2.Font.Bold := false;
  lblPcolr3.Font.Bold := false;

  case keyScan of
    0: lblColor4.Font.Bold := true;
    1: lblColor0.Font.Bold := true;
    2: lblColor1.Font.Bold := true;
    3: lblColor2.Font.Bold := true;
    4: lblPcolr0.Font.Bold := true;
    5: lblPcolr1.Font.Bold := true;
    6: lblPcolr2.Font.Bold := true;
    7: lblPcolr3.Font.Bold := true;
    10: lblColor3.Font.Bold := true;
  end;
end;

procedure TfrmColors.colorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Sender is TShape) then begin
    colTab[keyScan] := colorMem[TShape(Sender).Tag];
    colorValues[keyScan] := TShape(Sender).Tag*2;
    case keyScan of
      0: shapeColor0.Brush.Color := colorMem[TShape(Sender).Tag];
      1: shapeColor1.Brush.Color := colorMem[TShape(Sender).Tag];
      2: shapeColor2.Brush.Color := colorMem[TShape(Sender).Tag];
      3: shapeColor3.Brush.Color := colorMem[TShape(Sender).Tag];
      4: shapeColor704.Brush.Color := colorMem[TShape(Sender).Tag];
      5: shapeColor705.Brush.Color := colorMem[TShape(Sender).Tag];
      6: shapeColor706.Brush.Color := colorMem[TShape(Sender).Tag];
      7: shapeColor707.Brush.Color := colorMem[TShape(Sender).Tag];
      10: shapeColor4.Brush.Color := colorMem[TShape(Sender).Tag];
    end;

    if propFlagModules[2] = 1 then begin
      if frmPmg.tabs.PageIndex < 2 then
        frmPmg.RefreshPm(true)
      else
        frmPmg.RefreshMissile;
    end;

    if propFlagModules[8] = 1 then
      frmAnimator.RefreshFrame(0);
  end;
end;

procedure TfrmColors.shapeColorMouseEnter(Sender: TObject);
var
  regNum : string = 'Register ';
begin
//  TShape(Sender).Canvas.Brush.Style := bsClear;
  //TShape(Sender).Canvas.Pen.Color := clBlack;
//  TShape(Sender).Canvas.Rectangle(bounds(0, 0, TShape(Sender).width, TShape(Sender).Height));
//  TShape(Sender).Canvas.Rectangle(0, 0, TShape(Sender).width + 1, TShape(Sender).Height + 1);

  case TShape(Sender).Tag of
    0: regNum += 'COLOR4 712 ($2C8)';
    1: regNum += 'COLOR0 708 ($2C4)';
    2: regNum += 'COLOR1 709 ($2C5)';
    3: regNum += 'COLOR2 710 ($2C6)';
    10: regNum += 'COLOR3 711 ($2C7)';
    4: regNum += 'PCOLR0 704 ($2C0) ';
    5: regNum += 'PCOLR1 705 ($2C1)';
    6: regNum += 'PCOLR2 706 ($2C2)';
    7: regNum += 'PCOLR3 707 ($2C3)';
  end;
  TShape(Sender).Hint := regNum +
                         ', color value (dec: ' + IntToStr(colorValues[TShape(Sender).Tag]) +
                         ', hex: ' + IntToHex(colorValues[TShape(Sender).Tag], 2) + ')';
end;

end.

