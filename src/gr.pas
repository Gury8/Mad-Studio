{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Graphics editor
}
unit gr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, ComCtrls, Menus, types, LCLIntf,
  common;

type
  { TfrmGr }
  TfrmGr = class(TForm)
    btnEllipse: TToolButton;
    btnLine: TToolButton;
    btnNormal: TToolButton;
    btnRect: TToolButton;
    btnFillRect: TToolButton;
    btnTriangle: TToolButton;
    cmbGrMode: TComboBox;
    color0: TImage;
    color1: TImage;
    color2: TImage;
    color4: TImage;
    images: TImageList;
    imgEditor: TImage;
    lblCoord: TLabel;
    menuGr: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    menuExportDataAsm: TMenuItem;
    menuExportDataFastBasic: TMenuItem;
    menuExportDataMadPascal: TMenuItem;
    menuExportDataAction: TMenuItem;
    menuExportDataCC65: TMenuItem;
    MenuItem20: TMenuItem;
    mnuExportData: TMenuItem;
    MenuItem21: TMenuItem;
    menuExportDataAtariBASIC: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    sbGr: TStatusBar;
    timer: TTimer;
    toolbar: TToolBar;
    toolbarDraw: TToolBar;
    ToolButton1: TToolButton;
    ToolButton16: TToolButton;
    btnOpenFile: TToolButton;
    btnSaveFile: TToolButton;
    btnSettings: TToolButton;
    btnClearScreen: TToolButton;
    btnShiftLeft: TToolButton;
    btnShiftRight: TToolButton;
    btnShiftUp: TToolButton;
    btnShiftDown: TToolButton;
    btnMoveLeft: TToolButton;
    btnMoveRight: TToolButton;
    btnMoveUp: TToolButton;
    btnMoveDown: TToolButton;
    btnCodeGen: TToolButton;
    btnSep: TToolButton;
    ToolButton7: TToolButton;
    procedure btnTempClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CloseProc(Sender: TObject);
    procedure cmbGrModeChange(Sender: TObject);
    procedure Color0Proc(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Color1Proc(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Color2Proc(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure color4Proc(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(
      Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure FormMouseWheelDown(
      Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(
      Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure imgEditorMouseDown(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgEditorMouseMove(
      Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgEditorMouseUp(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ExportData(Sender: TObject);
    procedure refreshp;
    procedure Color;
    procedure SaveProc(Sender: TObject);
    procedure SaveAsProc(Sender: TObject);
    procedure NewFileProc(Sender: TObject);
    procedure OptionsProc(Sender: TObject);
    procedure PaletteProc(Sender: TObject);
    procedure GrCodeProc(Sender: TObject);
    procedure MoveRightProc(Sender: TObject);
    procedure MoveLeftProc(Sender: TObject);
    procedure MoveUpProc(Sender: TObject);
    procedure MoveDownProc(Sender: TObject);
    procedure timerTimer(Sender: TObject);
    procedure ShiftLeftProc(Sender: TObject);
    procedure ShiftRightProc(Sender: TObject);
    procedure ShiftUpProc(Sender: TObject);
    procedure ShiftDownProc(Sender: TObject);
    procedure OpenProc(Sender: TObject);
    procedure SaveData(Sender: TObject);
    procedure ClearProc(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
    btn, varBtn : TMousebutton;
    is01bit : boolean;
    grBytes : word;
//    paintbmp: TBitmap;
    MouseIsDown: Boolean;
    PrevX, PrevY: Integer;
    PrevXf, PrevYf: Integer;
    procedure Plot(xf, yf : integer);
    procedure PlotEx(x, y : integer);
    procedure Line(xStart, yStart, xEnd, yEnd : integer);
    procedure Rectangle(x1, y1, x2, y2 : integer);
    procedure FillRectangle(x1, y1, x2, y2 : integer);
    procedure Ellipse(x, y, r : integer);
    procedure Triangle(x1, y1, x2, y2 : integer);
    procedure GrModeSettings;
    procedure SetGrMode;
    procedure ColorRegisters;
  public
    { public declarations }
    filename : string;
    fld : array[0..319, 0..191] of byte;
  end;

var
  frmGr: TfrmGr;

implementation

{$R *.lfm}

uses
  main, settings, colors, gr_gen, code_lib;

{ TfrmGr }

procedure TfrmGr.FormCreate(Sender: TObject);
begin
  frmGr.DoubleBuffered := true;
  btn := mbMiddle;
end;

procedure TfrmGr.btnTempClick(Sender: TObject);
begin
  //for y := 0 to grY do begin
  //  charsPerLine := grX div 4;
  //  for x := 0 to charsPerLine do begin
  //    dta := fld[x*4, y] * 64 +
  //           fld[x*4 + 1, y] * 16 +
  //           fld[x*4 + 2, y] * 4 +
  //           fld[x*4 + 3, y];
  //  end;
  //end;
end;

procedure TfrmGr.FormShow(Sender: TObject);
begin
  propFlagModules[1] := 1;
  isChange := true;

  frmMain.Top := 0;
  formId := formGr;
  sbGr.Panels[0].Text := 'Cursor coordinates: x: 0, y: 0';
//  sbGr.Panels[1].Text := 'Color palette: default';

  grMode := grMode160x96x4;
  GrModeSettings;
  SetGrMode;

  imgEditor.Canvas.Brush.Color := coltab[0];
  imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));

  NewFileProc(Sender);
end;

procedure TfrmGr.FormActivate(Sender: TObject);
begin
  formId := formGr;
end;

// Color registers
procedure TfrmGr.ColorRegisters;
begin
  color1.Visible := not is01bit;
  color2.Visible := not is01bit;
  color4.Canvas.Brush.Color := coltab[0];
  color4.Canvas.FillRect(bounds(0, 0, color4.width, color4.Height));
  color1.Canvas.Brush.Color := coltab[2];
  color1.Canvas.FillRect(bounds(0, 0, color1.width, color1.Height));
  color2.Canvas.Brush.Color := coltab[3];
  color2.Canvas.FillRect(bounds(0, 0, color2.width, color2.Height));

  if grMode = grMode320x192x2 then begin
    color0.Canvas.Brush.Color := coltab[8];
    color0.Canvas.FillRect(bounds(0, 0, color0.width, color0.Height));
    color0.Canvas.Rectangle(0, 0, color0.width, color0.Height);
  end
  else begin
    color0.Canvas.Brush.Color := coltab[1];
    color0.Canvas.FillRect(bounds(0, 0, color0.width, color0.Height));
  end;
end;

procedure TfrmGr.cmbGrModeChange(Sender: TObject);
var
  xf, yf : integer;
  isData : boolean = false;
//  isConfirm : boolean = false;
begin
  for xf := 0 to grX do
    for yf := 0 to grY do
      if fld[xf, yf] > 0 then begin
        isData := true;
        break;
      end;

  if isData then begin
    if MessageDlg('Warning', 'You are about to change graphics mode.' +
                  ' Data will be lost! Do you wish to continue?',
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
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
      Exit;
    end;
  end;

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

  GrModeSettings;
  SetGrMode;

  imgEditor.Canvas.Brush.Color := coltab[0];
  imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));

  NewFileProc(Sender);
end;

procedure TfrmGr.Color0Proc(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  frmColors.SelColor := 1;
  ColorRegisters;
  color0.Canvas.Rectangle(0, 0, color0.width, color0.Height);
  //color0.Canvas.Brush.Color := coltab[1];
  //color0.Canvas.FillRect(bounds(0, 0, color0.width, color0.Height));
  //color0.Canvas.Rectangle(0, 0, color0.width, color0.Height);
  //color1.Canvas.Brush.Color := coltab[2];
  //color1.Canvas.FillRect(bounds(0, 0, color1.width, color1.Height));
  //color2.Canvas.Brush.Color := coltab[3];
  //color2.Canvas.FillRect(bounds(0, 0, color2.width, color2.Height));
end;

procedure TfrmGr.Color1Proc(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  frmColors.SelColor := 2;
  ColorRegisters;
  color1.Canvas.Rectangle(0, 0, color1.width, color1.Height);
  //color0.Canvas.Brush.Color := coltab[1];
  //color0.Canvas.FillRect(bounds(0, 0, color0.width, color0.Height));
  //color1.Canvas.Brush.Color := coltab[2];
  //color1.Canvas.FillRect(bounds(0, 0, color1.width, color1.Height));
  //color2.Canvas.Brush.Color := coltab[3];
  //color2.Canvas.FillRect(bounds(0, 0, color2.width, color2.Height));
end;

procedure TfrmGr.Color2Proc(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  frmColors.SelColor := 3;
  ColorRegisters;
  color2.Canvas.Rectangle(0, 0, color2.width, color2.Height);
  //color0.Canvas.Brush.Color := coltab[1];
  //color0.Canvas.FillRect(bounds(0, 0, color0.width, color0.Height));
  //color1.Canvas.Brush.Color := coltab[2];
  //color1.Canvas.FillRect(bounds(0, 0, color1.width, color1.Height));
  //color2.Canvas.Brush.Color := coltab[3];
  //color2.Canvas.FillRect(bounds(0, 0, color2.width, color2.Height));
end;

procedure TfrmGr.color4Proc(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  frmColors.SelColor := 0;
  ColorRegisters;
  //color0.Canvas.Brush.Color := coltab[1];
  //color0.Canvas.FillRect(bounds(0, 0, color0.width, color0.Height));
  //color1.Canvas.Brush.Color := coltab[2];
  //color1.Canvas.FillRect(bounds(0, 0, color1.width, color1.Height));
  //color2.Canvas.Brush.Color := coltab[3];
  //color2.Canvas.FillRect(bounds(0, 0, color2.width, color2.Height));
end;

procedure TfrmGr.GrModeSettings;
begin
  case grMode of
    grMode40x24x4: begin
      is01bit := false;
      grX := 39; grY := 23; factX := 24; factY := 24;
      grBytes := 240;
    end;
    grMode80x48x2: begin
      is01bit := true;
      grX := 79; grY := 47; factX := 12; factY := 12;
      grBytes := 480;
    end;
    grMode80x48x4: begin
      is01bit := false;
      grX := 79; grY := 47; factX := 12; factY := 12;
      grBytes := 960;
    end;
    grMode160x96x2: begin
      is01bit := true;
      grX := 159; grY := 95; factX := 6; factY := 6;
      grBytes := 3840 div 2;
    end;
    grMode160x96x4: begin
      is01bit := false;
      grX := 159; grY := 95; factX := 6; factY := 6;
      grBytes := 3840;
    end;
    grMode160x192x2: begin
      is01bit := true;
      grX := 159; grY := 191; factX := 6; factY := 3;
      grBytes := 3840;
    end;
    grMode160x192x4: begin
      is01bit := false;
      grX := 159; grY := 191; factX := 6; factY := 3;
      grBytes := 7680;
    end;
    grMode320x192x2: begin
      is01bit := true;
      grX := 319; grY := 191; factX := 3; factY := 3;
      grBytes := 7680;
    end;
  end;

  case grMode of
    grMode40x24x4  : sbGr.Panels[2].Text := 'Graphics mode 40 x 24 in 4 colors';
    grMode80x48x2  : sbGr.Panels[2].Text := 'Graphics mode 80 x 48 in 2 colors';
    grMode80x48x4  : sbGr.Panels[2].Text := 'Graphics mode 80 x 48 in 4 colors';
    grMode160x96x2 : sbGr.Panels[2].Text := 'Graphics mode 160 x 96 in 2 colors';
    grMode160x96x4 : sbGr.Panels[2].Text := 'Graphics mode 160 x 96 in 4 colors';
    grMode160x192x2: sbGr.Panels[2].Text := 'Graphics mode 160 x 192 in 2 colors';
    grMode160x192x4: sbGr.Panels[2].Text := 'Graphics mode 160 x 192 in 4 colors';
    grMode320x192x2: sbGr.Panels[2].Text := 'Graphics mode 320 x 192 in 2 colors';
  end;
end;

procedure TfrmGr.SetGrMode;
var
  a, b : integer;
begin
  for a := 0 to grX do
    for b := 0 to grY do begin
      imgEditor.Canvas.Pixels[a*factX, b*factY] := clWhite;
      fld[a, b] := 0;
    end;

//  btnNormalOld.Down:=false;
//  func := [fNormal];
  btnNormal.Down:=true;

  if grX = 319 then
    frmColors.SelColor := 9
  else
    frmColors.SelColor := 1;

  ColorRegisters;
//  Color;
end;

procedure TfrmGr.PlotEx(x, y : integer);
begin
  btn := varBtn;
  Plot(x, y);
  btn := mbMiddle;
end;

// DrawLine
// http://www.efg2.com/Lab/Library/Delphi/Graphics/Bresenham.txt
procedure TfrmGr.Line (xStart, yStart, xEnd, yEnd : integer);
 {Bresenham's Line Algorithm.  Byte, March 1988, pp. 249-253.}
   VAR
     a, b       :  INTEGER;  {displacements in x and y}
     d          :  INTEGER;  {decision variable}
     diag_inc   :  INTEGER;  {d's increment for diagonal steps}
     dx_diag    :  INTEGER;  {diagonal x step for next pixel}
     dx_nondiag :  INTEGER;  {nondiagonal x step for next pixel}
     dy_diag    :  INTEGER;  {diagonal y step for next pixel}
     dy_nondiag :  INTEGER;  {nondiagonal y step for next pixel}
     i          :  INTEGER;  {loop index}
     nondiag_inc:  INTEGER;  {d's increment for nondiagonal steps}
     swap       :  INTEGER;  {temporary variable for swap}
     x, y       :  INTEGER;  {current x and y coordinates}
 BEGIN
   x := xStart;              {line starting point}
   y := yStart;
   {Determine drawing direction and step to the next pixel.}
   a := xEnd - xStart;       {difference in x dimension}
   b := yEnd - yStart;       {difference in y dimension}
   {Determine whether end point lies to right or left of start point.}
   IF   a < 0                {drawing towards smaller x values?}
   THEN BEGIN
     a := -a;                {make 'a' positive}
     dx_diag := -1
   END
   ELSE
     dx_diag := 1;
   {Determine whether end point lies above or below start point.}
   IF   b < 0                {drawing towards smaller x values?}
   THEN BEGIN
     b := -b;                {make 'a' positive}
     dy_diag := -1
   END
   ELSE
     dy_diag := 1;
   {Identify octant containing end point.}
   IF   a < b
   THEN BEGIN
     swap := a;
     a := b;
     b := swap;
     dx_nondiag := 0;
     dy_nondiag := dy_diag
   END
   ELSE BEGIN
     dx_nondiag := dx_diag;
     dy_nondiag := 0
   END;
   d := b + b - a;            {initial value for d is 2*b - a}
   nondiag_inc := b + b;      {set initial d increment values}
   diag_inc    := b + b - a - a;
   FOR i := 0 TO a DO BEGIN   {draw the a+1 pixels}
     PlotEx(x,y);
     IF   d < 0               {is midpoint above the line?}
     THEN BEGIN               {step nondiagonally}
       x += dx_nondiag;
       y += dy_nondiag;
       d += nondiag_inc   {update decision variable}
     END
     ELSE BEGIN               {midpoint is above the line; step diagonally}
       x += dx_diag;
       y += dy_diag;
       d += diag_inc
     END
   END;
 END;

procedure TfrmGr.Rectangle(x1, y1, x2, y2 : integer);
begin
  //Line(x1, y1, x2 - 1, y1);
  //Line(x2 - 1, y1, x2 - 1, y2 - 1);
  //Line(x1, y2 - 1, x2 - 1, y2 - 1);
  //Line(x1, y1, x1, y2 - 1);

  if x1 > x2 then begin
    Line(x1 - 1, y1, x2 , y1);
    Line(x2, y1, x2, y2 - 1);
    Line(x1 - 1, y2 - 1, x2, y2 - 1);
    Line(x1 - 1, y1, x1 - 1, y2 - 1);
  end
  else begin
    Line(x1, y1, x2 - 1 , y1);
    Line(x2 - 1, y1, x2 - 1, y2 - 1);
    Line(x1, y2 - 1, x2 - 1, y2 - 1);
    Line(x1, y1, x1, y2 - 1);
  end;
//  refreshp;
end;

procedure TfrmGr.FillRectangle(x1, y1, x2, y2 : integer);
var
  y : integer;
begin
  //Line(x1, y1, x2, y1);
  //Line(x2, y1, x2, y2);
  //Line(x1, y2, x2, y2);
  //Line(x1, y1, x1, y2);

  if x1 > x2 then begin
    Line(x1 - 1, y1, x2 , y1);
    Line(x2, y1, x2, y2 - 1);
    Line(x1 - 1, y2 - 1, x2, y2 - 1);
    Line(x1 - 1, y1, x1 - 1, y2 - 1);
    for y := y1 + 1 to y2 - 1 do
      Line(x1 - 1, y, x2, y);
  end
  else begin
    Line(x1, y1, x2 - 1 , y1);
    Line(x2 - 1, y1, x2 - 1, y2 - 1);
    Line(x1, y2 - 1, x2 - 1, y2 - 1);
    Line(x1, y1, x1, y2 - 1);
    for y := y1 + 1 to y2 - 1 do
      Line(x1, y, x2 - 1, y);
  end;
//  refreshp;
end;

(*
procedure TfrmGr.Circle(x, y, r, shapeType : integer);
var
  a, c : Integer;
begin
  // Check for radius of zero
  if r = 0 then begin
    PlotEx(x, y);
    refreshp;
    Exit;
  end;

  // Circle algorithm
  c := 0; a := r - 1;
  while r >= c do begin
    PlotEx(x + c, y + r);
    PlotEx(x + c, y - r);
    PlotEx(x - c, y - r);
    PlotEx(x - c, y + r);
    PlotEx(x + r, y + c);
    PlotEx(x + r, y - c);
    PlotEx(x - r, y - c);
    PlotEx(x - r, y + c);
    Inc(c);
    a += 1 - c - c;

    if shapeType = 0 then
      if a >= 0 then Continue;

    Dec(r);
    a += r + r;
  end;

  refreshp;
end;
*)
(*
void bres(int xc,int yc,int r)
{
int p,k,x,y;
p=1-r;
x=0;
y=r;
for(k=0;k<=y;k++)
{
putpixel(xc+x,yc+y,5);
putpixel(xc-y,yc-x,5);
putpixel(xc+y,yc-x,5);
putpixel(xc-y,yc+x,5);
putpixel(xc+y,yc+x,5);
putpixel(xc-x,yc-y,5);
putpixel(xc+x,yc-y,5);
putpixel(xc-x,yc+y,5);
if(p>0)
 {
 p=p+2*(x+1)+1-2*(y+1);
 x++;
 y--;
 }
else
 {
 p=p+2*(x+1)+1;
 x++;
 }
}
}
*)

//procedure TfrmGr.Circle(xc, yc, r : integer);
//var
//  p,k,x,y : integer;
//begin
//p:=1-r;
//x:=0;
//y:=r;
//for k:=0 to y do begin
//  PlotEx(xc+x,yc+y);
//  PlotEx(xc-y,yc-x);
//  PlotEx(xc+y,yc-x);
//  PlotEx(xc-y,yc+x);
//  PlotEx(xc+y,yc+x);
//  PlotEx(xc-x,yc-y);
//  PlotEx(xc+x,yc-y);
//  PlotEx(xc-x,yc+y);
//if(p>0)then begin
// p:=p+2*(x+1)+1-2*(y+1);
// inc(x);
// dec(y);
//end
//else begin
// p:=p+2*(x+1)+1;
// inc(x);
//end
//end;
//refreshp;
//end;

procedure TfrmGr.Ellipse(x, y, r : integer);
var
  a, c : Integer;
//  swapX, swapY : integer;
begin
  // Check for radius of zero
  if r = 0 then begin
    PlotEx(x, y);
    refreshp;
    Exit;
  end;

  //if x > oldx then begin
  //  swapX := x;
  //  x := oldX;
  //  oldX := swapX;
  //end;
  //
  //if y > oldY then begin
  //  swapY := y;
  //  y := oldY;
  //  oldY := swapY;
  //end;

  // Circle algorithm
  c := 0; a := r - 1;
  while r >= c do begin
    PlotEx(x + c, y + r);
    PlotEx(x + c, y - r);
    PlotEx(x - c, y - r);
    PlotEx(x - c, y + r);
    PlotEx(x + r, y + c);
    PlotEx(x + r, y - c);
    PlotEx(x - r, y - c);
    PlotEx(x - r, y + c);
    Inc(c);
    a += 1 - c - c;
    if a >= 0 then Continue;
    Dec(r);
    a += r + r;
  end;
//  refreshp;
end;

procedure TfrmGr.Triangle(x1, y1, x2, y2 : integer);
begin
  Line(x1, y1, x1 + ((x2 - x1) div 2), y2);
  Line(x1 + ((x2 - x1) div 2), y2, x2, y1);
  Line(x1, y1, x2, y1);
//  refreshp;
end;

//procedure TfrmGr.Button2Click(Sender: TObject);
//begin
//  if size = 40 then
//    size := 32
//  else
//    size := 40;
//
//  refreshp;
//  frmGr.SetFocus;
//end;

procedure TfrmGr.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : integer;
begin
  btn := Button;
  xf := X div factX;
  yf := Y div factY;
  Plot(xf, yf);
end;

procedure TfrmGr.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Plot(x div factX, y div factY);
end;

procedure TfrmGr.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
end;

procedure TfrmGr.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
begin
  frmGr.SetFocus;
end;

procedure TfrmGr.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  if not is01bit and (frmColors.SelColor < 3) then
    inc(frmColors.SelColor);

  Color;
end;

procedure TfrmGr.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  if not is01bit and (frmColors.SelColor > 1) then
    dec(frmColors.SelColor);

  Color;
end;

procedure TfrmGr.Color;
begin
  (*
  //if frmColors.rbDefault.Checked then
  if grX = 319 then frmColors.SelColor := 9;
  color4.Canvas.Brush.Color := coltab[frmColors.SelColor];

  //else begin
  //  color4.Canvas.Brush.Color := coltab2[frmColors.SelColor];
  //end;
  color4.Canvas.FillRect(bounds(0, 0, color4.width, color4.Height));
  *)
end;

procedure TfrmGr.timerTimer(Sender: TObject);
begin
  Color;
end;

// Left
procedure TfrmGr.ShiftLeftProc(Sender: TObject);
var
  x, y, n : integer;
begin
  for y := 0 to grY do begin
    n := fld[0, y];
    for x := 1 to grX do
      fld[x - 1, y] := fld[x, y];

    fld[159, y] := n;
  end;
  refreshp;
end;

// Right
procedure TfrmGr.ShiftRightProc(Sender: TObject);
var
  x, y, n : integer;
begin
  for y := 0 to grY do begin
    n := fld[159, y];
    for x := grX - 1 downto 0 do
      fld[x + 1, y] := fld[x, y];

    fld[0, y] := n;
  end;
  refreshp;
end;

procedure TfrmGr.ShiftUpProc(Sender: TObject);
var
  x, y : integer;
  fld02 : array[0..159] of byte;
begin
  for x := 0 to 159 do
    fld02[x] := fld[x, 0];

  for x := 0 to grX do
    for y := 1 to grY do begin
      fld[x, y - 1] := fld[x, y];
      fld[x, y] := 0;
    end;

  for x := 0 to 159 do
    fld[x, grY] := fld02[x];

  refreshp;
end;

procedure TfrmGr.ShiftDownProc(Sender: TObject);
var
  x, y : integer;
  fld02 : array[0..159] of byte;
begin
  for x := 0 to 159 do
    fld02[x] := fld[x, grY];

  for x := 0 to grX do
    for y := grY downto 0 do begin
      fld[x, y + 1] := fld[x, y];
      fld[x, y] := 0;
    end;

  for x := 0 to 159 do
    fld[x, 0] := fld02[x];

  refreshp;
end;

{
  Load screen from file
}
procedure TfrmGr.OpenProc(Sender: TObject);
var
  x, y : integer;
  dta : byte;
  fs : TFileStream;
  tempCalcX : word;
begin
//  if frmMain.dlgOpen.Execute then
////  if (frmMain.dlgOpen.Files.Count > 0) then begin
////    if (FileExistsUTF8(frmMain.dlgOpen.FileName)) then begin
//      imgEditor.Picture.LoadFromFile(frmMain.dlgOpen.FileName);
////      MyCanvasPaint(Sender);
////    end else begin
////      ShowMessage('File is not found. You will have to open an existing file.');
////    end;
////  end;
//
//  exit;

  frmMain.dlgOpen.Title := 'Open existing picture file';

  case grMode of
    grMode40x24x4:
      frmMain.dlgOpen.Filter := 'Micro Painter files (*.gr3)|*.gr3|All files (*.*)|*.*';
    grMode80x48x2:
      frmMain.dlgOpen.Filter := 'Micro Painter files (*.gr4)|*.gr4|All files (*.*)|*.*';
    grMode80x48x4:
      frmMain.dlgOpen.Filter := 'Micro Painter files (*.gr5)|*.gr5|All files (*.*)|*.*';
    grMode160x96x2:
      frmMain.dlgOpen.Filter := 'Micro Painter files (*.gr6)|*.gr6|All files (*.*)|*.*';
    grMode160x96x4:
      frmMain.dlgOpen.Filter := 'Micro Painter files (*.gr7, *.mic)|*.gr7;*.mic|All files (*.*)|*.*';
    grMode160x192x2:
      frmMain.dlgOpen.Filter := 'Micro Painter files (*.mic)|*.mic|All files (*.*)|*.*';
    grMode160x192x4:
      frmMain.dlgOpen.Filter := 'Micro Painter files (*.mic)|*.mic|All files (*.*)|*.*';
    grMode320x192x2:
      frmMain.dlgOpen.Filter := 'Graphics 8 files (*.gr8)|*.gr8|All files (*.*)|*.*';
  end;
  if frmMain.dlgOpen.Execute then begin
    filename := frmMain.dlgOpen.Filename;
    fs := TFileStream.Create(Filename, fmOpenReadWrite);
    try
      for y := 0 to grY do begin
        // 4-color graphics modes
        //// Resolution less than 320x192
        if not is01bit then begin
           //(grMode = grMode40x24x4) or (grMode = grMode80x48x4) or (grMode = grMode160x96x4)
           //or (grMode = grMode160x192x4) then begin
//        if grX < 319 then begin
          tempCalcX := grX div 4;
          for x := 0 to tempCalcX do begin
            dta := fs.ReadByte;
            fld[x shl 2, y] := dta div 64;
            fld[x shl 2 + 1, y] := (dta mod 64) shr 4;  //div 16;
            fld[x shl 2 + 2, y] := (dta mod 16) shr 2;  //div 4;
            fld[x shl 2 + 3, y] := dta mod 4;
          end;
        end
        // 2-color graphics modes
        else begin
//          if grX = 319 then begin
          tempCalcX := grX shr 3;  // div 8;
          for x := 0 to tempCalcX do begin
            dta := fs.ReadByte;
            fld[x shl 3, y] := dta div 128;
            fld[x shl 3 + 1, y] := (dta mod 128) div 64;
            fld[x shl 3 + 2, y] := (dta mod 64) div 32;
            fld[x shl 3 + 3, y] := (dta mod 32) div 16;
            fld[x shl 3 + 4, y] := (dta mod 16) div 8;
            fld[x shl 3 + 5, y] := (dta mod 8) div 4;
            fld[x shl 3 + 6, y] := (dta mod 4) div 2;
            fld[x shl 3 + 7, y] := (dta mod 2);
          end;
        end;
      end;
      if ((grMode = grMode40x24x4) and (fs.Size = 244))
         or ((grMode = grMode80x48x4) and (fs.Size = 964))
         or ((grMode = grMode160x96x4) and (fs.Size = 3844))
         or ((grMode = grMode160x192x4) and (fs.Size = 7684))
      then begin
        for x := 0 to 3 do begin
          dta := fs.ReadByte;
          coltab[x] := colorMem[dta div 2];
          colorValues[x] := dta;
        end;
      end
      else begin
        if ((grMode = grMode80x48x2) and (fs.Size = 482))
           or ((grMode = grMode160x96x2) and (fs.Size = 1922))
           or ((grMode = grMode160x192x2) and (fs.Size = 3842))
        then begin
          for x := 0 to 1 do begin
            dta := fs.ReadByte;
            coltab[x] := colorMem[dta div 2];
            colorValues[x] := dta;
          end;
        end;
      end;
      ColorRegisters;
      frmColors.Show;
      beep;
      caption := programName + ' ' + programVersion +
                 ' - Graphics editor (' + filename + ')';
    finally
      fs.Free;
      refreshp;
    end;
    //except
    //  on E: EInOutError do
    //   writeln('File handling error occurred. Details: ', E.Message);
    //end;

    //showmessage(
    //  inttostr(colorValues[0]) +
    //  ', ' + inttostr(colorValues[1]) +
    //  ', ' + inttostr(colorValues[2]) +
    //  ', ' + inttostr(colorValues[3]));
  end;
end;

procedure TfrmGr.SaveData(Sender: TObject);
var
  fs : TFileStream;
  x, y : integer;
  dta : byte;
  charsPerLine : byte;
begin
  fs := TFileStream.Create(filename, fmCreate);
  try
    for y := 0 to grY do begin
      if not is01bit then begin
      //if (grMode = grMode40x24x4) or (grMode = grMode80x48x4) or (grMode = grMode160x96x4)
      //   or (grMode = grMode160x192x4) then begin
        charsPerLine := grX div 4;
//        showmessage(inttostr(tempCalcX));
        for x := 0 to charsPerLine do begin
          dta := fld[x*4, y] * 64 +
                 fld[x*4 + 1, y] * 16 +
                 fld[x*4 + 2, y] * 4 +
                 fld[x*4 + 3, y];
          fs.WriteByte(dta);
        end;
      end
      else begin
        charsPerLine := grX div 8;
        for x := 0 to charsPerLine do begin
          dta := fld[x*8, y] * 128 +
                 fld[x*8 + 1, y] * 64 +
                 fld[x*8 + 2, y] * 32 +
                 fld[x*8 + 3, y] * 16 +
                 fld[x*8 + 4, y] * 8 +
                 fld[x*8 + 5, y] * 4 +
                 fld[x*8 + 6, y] * 2 +
                 fld[x*8 + 7, y];
          fs.WriteByte(dta);
        end;
      end;
    end;
    if isGrMicPalette then begin
      if is01bit then
        dta := 1
      else
        dta := 3;

      for x := 0 to dta do
        fs.WriteByte(colorValues[x]);

      //showmessage(
      //  inttostr(colorValues[0]) +
      //  ', ' + inttostr(colorValues[1]) +
      //  ', ' + inttostr(colorValues[2]) +
      //  ', ' + inttostr(colorValues[3]));
    end;
    caption := programName + ' ' + programVersion + ' - Graphics editor (' + filename + ')';
  finally
    fs.Free;
  end;
end;

{
  Clear screen
}
procedure TfrmGr.ClearProc(Sender: TObject);
begin
  FillByte(fld, SizeOf(fld), 0);
  refreshp;
end;

procedure TfrmGr.imgEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : integer;
begin
  btn := Button;
  xf := X div factX;
  yf := Y div factY;

  // Check for mouse button clicked
  if btn = mbRight then
    varBtn := btn
  else
    varBtn := mbLeft;

  if xf > grX then xf := grX;
  if xf < 0 then xf := 0;
  if yf > grY then yf := grY;
  if yf < 0 then yf := 0;

  MouseIsDown := True;
  PrevX := x;
  PrevY := y;
  PrevXf := xf;
  PrevYf := yf;

  if btnNormal.Down then
    Plot(xf, yf);
end;

procedure TfrmGr.imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
  col : byte;
begin
  xf := X div factX;
  yf := Y div factY;
  if xf > grX then xf := grX;
  if xf < 0 then xf := 0;
  if yf > grY then yf := grY;
  if yf < 0 then yf := 0;

  if MouseIsDown then begin
    if btnNormal.Down then
      Plot(xf, yf)
    else if btnLine.Down then begin
      if grX = 319 then
        frmColors.SelColor := 9
      else if is01bit then
        frmColors.SelColor := 1;

      col := frmColors.SelColor;
      if (grX < 319) and (col > 3) then col := 3;

      imgEditor.Canvas.Brush.Color := coltab[col];
      imgEditor.Canvas.Line(PrevXf*factX, PrevYf*factY, xf*factX, yf*factY);
    end
    else if btnEllipse.Down then begin
  //    MyCanvasPaint(Sender);
  //    MyCanvas.Canvas.Ellipse(PrevX, PrevY, X, Y);
  //    Circle(xf, yf, 20, 1);
//      Ellipse(xf, yf, PrevX, PrevY, 20);

      if grX = 319 then
        frmColors.SelColor := 9
      else if is01bit then
        frmColors.SelColor := 1;

      //case btn of
      //  mbLeft : col := frmColors.SelColor;
      //  mbRight: col := 0;
      //else
      //  exit;
      //end;

      col := frmColors.SelColor;
      if (grX < 319) and (col > 3) then col := 3;

//      fld[xf, yf] := frmColors.SelColor;

      imgEditor.Canvas.Brush.Color := coltab[col];
      imgEditor.Canvas.Ellipse(PrevXf*factX, PrevYf*factY, xf*factX, yf*factY);
//      imgEditor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
//      imgEditor.Canvas.Pixels[xf*factX, yf*factY] := coltab[col];
    end
    else if btnRect.Down then begin
      if grX = 319 then
        frmColors.SelColor := 9
      else if is01bit then
        frmColors.SelColor := 1;

      col := frmColors.SelColor;
      if (grX < 319) and (col > 3) then col := 3;

      imgEditor.Canvas.Brush.Color := coltab[col];
      imgEditor.Canvas.Rectangle(PrevXf*factX, PrevYf*factY, xf*factX, yf*factY);
//      imgEditor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
//      imgEditor.Canvas.Pixels[xf*factX, yf*factY] := coltab[col];
    end
    else if btnFillRect.Down then begin
      if grX = 319 then
        frmColors.SelColor := 9
      else if is01bit then
        frmColors.SelColor := 1;

      col := frmColors.SelColor;
      if (grX < 319) and (col > 3) then col := 3;

      imgEditor.Canvas.Brush.Color := coltab[col];
      imgEditor.Canvas.FillRect(PrevXf*factX, PrevYf*factY, xf*factX, yf*factY);
//      imgEditor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
//      imgEditor.Canvas.Pixels[xf*factX, yf*factY] := coltab[col];
    end
    else if btnTriangle.Down then begin
      if grX = 319 then
        frmColors.SelColor := 9
      else if is01bit then
        frmColors.SelColor := 1;

      col := frmColors.SelColor;
      if (grX < 319) and (col > 3) then col := 3;

      imgEditor.Canvas.Brush.Color := coltab[col];
      imgEditor.Canvas.Line(PrevXf*factX, yf*factY, PrevX + ((xf*factX - PrevX) div 2), PrevYf*factY);
      imgEditor.Canvas.Line(PrevXf*factX + ((xf*factX - PrevX) div 2), PrevYf*factY, xf*factX, yf*factY);
      imgEditor.Canvas.Line(PrevXf*factX, yf*factY, xf*factX, Y);

//      imgEditor.Canvas.Rectangle(PrevXf*factX, PrevYf*factY, xf*factX, yf*factY);
//      imgEditor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
//      imgEditor.Canvas.Pixels[xf*factX, yf*factY] := coltab[col];
    end;
  end;
  sbGr.Panels[0].Text := 'Cursor coordinates: ' + 'x: ' + inttostr(xf) + ', y: ' + inttostr(yf);
end;

procedure TfrmGr.imgEditorMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
  swapX : integer;
begin
  btn := mbMiddle;
  if MouseIsDown then begin
    xf := X div factX;
    yf := Y div factY;
    if xf > grX then xf := grX;
    if xf < 0 then xf := 0;
    if yf > grY then yf := grY;
    if yf < 0 then yf := 0;
//    sbGr.Panels[0].Text := 'Cursor coordinates: ' + 'x: ' + inttostr(xf) + ', y: ' + inttostr(yf);

    if btnLine.Down then
      Line(PrevXf, PrevYf, xf, yf)
    else if btnEllipse.Down then begin
  //    MyCanvasPaint(Sender);
  //    MyCanvas.Canvas.Ellipse(PrevX, PrevY, X, Y);

      if PrevXf > xf then begin
        swapX := xf;
        xf := PrevXf;
        PrevXf := swapX;
      end;

      if PrevYf > yf then
        Ellipse(
          PrevXf + (xf - PrevXf) div 2,
          PrevYf - (xf - PrevXf) div 2,
//          PrevXf, PrevYf,
          (xf - PrevXf) div 2)
      else begin
        Ellipse(
          PrevXf + (xf - PrevXf) div 2,
          PrevYf + (xf - PrevXf) div 2,
//          PrevXf, PrevYf,
          (xf - PrevXf) div 2)
      end;
    end
    else if btnRect.Down then
      Rectangle(PrevXf, PrevYf, xf, yf)
    else if btnFillRect.Down then
      FillRectangle(PrevXf, PrevYf, xf, yf)
    else if btnTriangle.Down then
      Triangle(xf, yf, PrevXf, PrevYf);
  end;
  MouseIsDown := false;
  refreshp;
end;

{-----------------------------------------------------------------------------
 Export graphics data in selected programming language as statements
 -----------------------------------------------------------------------------}
procedure TfrmGr.ExportData(Sender: TObject);
var
  f: TextFile;
  code : string;
  lineNum : word = 10;
  langIndex : byte;
  isCRLF : boolean = true;
  numberFormat : byte = 1;
begin
  case TMenuItem(Sender).Tag of
    0, 1: begin
      langIndex := _ATARI_BASIC;
      numberFormat := 0;
      isCRLF := false;
      frmMain.dlgSave.Title := 'Export Atari BASIC / Turbo BASIC XL data as';
      frmMain.dlgSave.Filter :=
        'Atari BASIC / Turbo BASIC XL listed files (*.lst)|*.lst|All files (*.*)|*.*';
    end;
    2: begin
      langIndex := _ACTION;
      isCRLF := false;
      frmMain.dlgSave.Title := 'Export Action! data as';
      frmMain.dlgSave.Filter := 'Action! files (*.act)|*.act|All files (*.*)|*.*';
    end;
    3: begin
      langIndex := _MAD_PASCAL;
      frmMain.dlgSave.Title := 'Export Mad Pascal data as';
      frmMain.dlgSave.Filter := 'Mad Pascal files (*.pas)|*.pas|All files (*.*)|*.*';
    end;
    4: begin
      langIndex := _FAST_BASIC;
      frmMain.dlgSave.Title := 'Export FastBasic data as';
      frmMain.dlgSave.Filter := 'FastBasic files (*.bas)|*.bas|All files (*.*)|*.*';
    end;
    5: begin
      langIndex := _MADS;
      frmMain.dlgSave.Title := 'Export Mad Assembler data as';
      frmMain.dlgSave.Filter := 'Mad Assembler files (*.asm)|*.asm|All files (*.*)|*.*';
    end;
    6: begin
      langIndex := _MAC65;
      isCRLF := false;
      frmMain.dlgSave.Title := 'Export MAC/65 data as';
      frmMain.dlgSave.Filter := 'MAC/65 files (*.asm)|*.asm|All files (*.*)|*.*';
    end;
    7: begin
      langIndex := _CC65;
      frmMain.dlgSave.Title := 'Export CC65 data as';
      frmMain.dlgSave.Filter := 'CC65 files (*.c)|*.c|All files (*.*)|*.*';
    end;
  end;

  if frmMain.dlgSave.Execute then begin
    code := GrDataValues(langIndex, lineNum, grBytes, numberFormat, is01bit);

    if not isCRLF then
      code := StringReplace(code, #13#10, #$9b,[rfReplaceAll]);

    AssignFile(f, frmMain.dlgSave.Filename);
    try
      Rewrite(f);
      Write(f, code);
    finally
      CloseFile(f);
    end;
  end;
end;

procedure TfrmGr.CloseProc(Sender: TObject);
begin
  Close;
end;

procedure TfrmGr.SaveProc(Sender: TObject);
begin
  SaveData(Sender);
end;

procedure TfrmGr.SaveAsProc(Sender: TObject);
begin
  case grMode of
    grMode40x24x4 :
      frmMain.dlgSave.Filter := 'Micro Painter files (*.gr3)|*.gr3|All files (*.*)|*.*';
    grMode80x48x2 :
      frmMain.dlgSave.Filter := 'Micro Painter files (*.gr4)|*.gr4|All files (*.*)|*.*';
    grMode80x48x4 :
      frmMain.dlgSave.Filter := 'Micro Painter files (*.gr5)|*.gr5|All files (*.*)|*.*';
    grMode160x96x2:
      frmMain.dlgSave.Filter := 'Micro Painter files (*.gr6)|*.gr6|All files (*.*)|*.*';
    grMode160x96x4:
      frmMain.dlgSave.Filter := 'Micro Painter files (*.gr7, *.mic)|*.gr7;*.mic|All files (*.*)|*.*';
    grMode160x192x2:
      frmMain.dlgSave.Filter := 'Micro Painter files (*.mic)|*.mic|All files (*.*)|*.*';
    grMode160x192x4:
      frmMain.dlgSave.Filter := 'Micro Painter files (*.mic)|*.mic|All files (*.*)|*.*';
    grMode320x192x2:
      frmMain.dlgSave.Filter := 'Graphics 8 files (*.gr8)|*.gr8|All files (*.*)|*.*';
  end;
  frmMain.dlgSave.Title := 'Save image as';
  frmMain.dlgSave.Filename := filename;
  if frmMain.dlgSave.Execute then begin
    filename := frmMain.dlgSave.Filename;
    SaveData(Sender);
  end;
end;

procedure TfrmGr.NewFileProc(Sender: TObject);
begin
  ClearProc(Sender);
  filename := getDir + 'examples\';
  case grMode of
    grMode40x24x4  : filename := filename + 'screen01.gr3';
    grMode80x48x2  : filename := filename + 'screen01.gr4';
    grMode80x48x4  : filename := filename + 'screen01.gr5';
    grMode160x96x2 : filename := filename + 'screen01.gr6';
    grMode160x96x4 : filename := filename + 'screen01.gr7';
    grMode160x192x2: filename := filename + 'screen01.mic';
    grMode160x192x4: filename := filename + 'screen01.mic';
    grMode320x192x2: filename := filename + 'screen01.gr8';
  end;
  caption := programName + ' ' + programVersion + ' - Graphics editor (' + filename + ')';
end;

procedure TfrmGr.OptionsProc(Sender: TObject);
begin
  frmGrSettings := TfrmGrSettings.Create(Self);
  with frmGrSettings do
    try
      ShowModal;
    finally
      Free;
    end;

  GrModeSettings;
  SetGrMode;
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
  imgEditor.Canvas.Brush.Color := coltab[0];
  imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));
end;

procedure TfrmGr.PaletteProc(Sender: TObject);
begin
  frmColors.Show;
end;

procedure TfrmGr.GrCodeProc(Sender: TObject);
begin
  frmGrGen := TfrmGrGen.Create(Self);
  with frmGrGen do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmGr.Plot(xf, yf : integer);
var
  col : byte;
begin
  if grX = 319 then
    frmColors.SelColor := 9
  else if is01bit then
    frmColors.SelColor := 1;

  case btn of
    mbLeft : col := frmColors.SelColor;
    mbRight: col := 0;
  else
    exit;
  end;

  if (grX < 319) and (col > 3) then
    col := 3;

  if (grX = 319) and (col = 9) then
    fld[xf, yf] := 1
  else
    fld[xf, yf] := col;

  imgEditor.Canvas.Brush.Color := coltab[col];
  imgEditor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
//  imgEditor.Canvas.Pixels[xf*factX, yf*factY] := coltab[2];
end;

procedure TfrmGr.refreshp;
var
  col, xf, yf : integer;
begin
  //imgEditor.Canvas.Brush.Color := clWhite;
  //imgEditor.Canvas.Brush.Style := bsSolid;
  //imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));

  for xf := 0 to grX do
    for yf := 0 to grY do begin
      col := fld[xf, yf];
      if (col = 1) and (grX = 319) then col := 9;
      imgEditor.Canvas.Brush.Color := coltab[col];
      imgEditor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
//      imgEditor.Canvas.Pixels[xf*factX, yf*factY] := coltab[3];
    end;

//  imgEditor.Canvas.Brush.Style := bsClear;
//  imgEditor.Canvas.Pen.Color := clRed;
//  imgEditor.canvas.rectangle(0, 0, (size*fact-1), grY*fact - 1);

  imgEditor.Refresh;
end;

{-----------------------------------------------------------------------------
 Move screen right
 -----------------------------------------------------------------------------}
procedure TfrmGr.MoveRightProc(Sender: TObject);
var
  x, y : integer;
begin
  for x := grX - 1 downto 0 do
    for y := grY downto 0 do begin
      fld[x + 1, y] := fld[x, y];
      fld[x, y] := 0;
    end;

  refreshp;
end;

{-----------------------------------------------------------------------------
 Move screen left
 -----------------------------------------------------------------------------}
procedure TfrmGr.MoveLeftProc(Sender: TObject);
var
  x, y : integer;
begin
  for x := 1 to grX do
    for y := grY downto 0 do begin
      fld[x - 1, y] := fld[x, y];
      fld[x, y] := 0;
    end;

  refreshp;
end;

{-----------------------------------------------------------------------------
 Move screen up
 -----------------------------------------------------------------------------}
procedure TfrmGr.MoveUpProc(Sender: TObject);
var
  x, y : integer;
begin
  for x := 0 to grX - 1 do
    for y := 1 to grY do begin
      fld[x, y - 1] := fld[x, y];
      fld[x, y] := 0;
    end;

  refreshp;
end;

{-----------------------------------------------------------------------------
 Move screen down
 -----------------------------------------------------------------------------}
procedure TfrmGr.MoveDownProc(Sender: TObject);
var
  x, y : integer;
begin
  for x := 0 to grX - 1 do
    for y := grY - 1 downto 0 do begin
      fld[x, y + 1] := fld[x, y];
      fld[x, y] := 0;
    end;

  refreshp;
end;

//procedure TfrmGr.Flodfill(x, y : integer; f, o : integer);
//var
//  x, y : integer;
//begin
//  void flodfill(int x,int y,int f,int o)
//  {
//  int c;
//   c=getpixel(x,y);
//    if(c==o)
//    {
//    setcolor(f);
//    putpixel (x,y,f);
//     delay(10);
//     flodfill(x+1,y,f,o);
//     flodfill(x,y+1,f,o);
//     flodfill(x+1,y+1,f,o);
//     flodfill(x-1,y-1,f,o);
//     flodfill(x-1,y,f,o);
//     flodfill(x,y-1,f,o);
//     flodfill(x-1,y+1,f,o);
//     flodfill(x+1,y-1,f,o);
//    }
//  }
//end;

procedure TfrmGr.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key of
    ord('1'): frmColors.SelColor := 1;
    ord('2'): frmColors.SelColor := 2;
    ord('3'): frmColors.SelColor := 3;
  end;

  Color;
end;

procedure TfrmGr.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[1] := 0;
end;

end.

