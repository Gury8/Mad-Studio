{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Graphics editor
}
unit graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ComCtrls, Menus, types, LCLIntf, LCLType, BCTrackbarUpdown,
  common;

type
  TTextOper = record
    isInit : boolean;
    text: string;
    x : word;
    y : byte
  end;

  { TfrmGraph }
  TfrmGraph = class(TForm)
    editShiftMove : TBCTrackbarUpdown;
    btnEllipse: TToolButton;
    btnFilledEllipse : TToolButton;
    btnLine: TToolButton;
    btnNormal: TToolButton;
    btnRect: TToolButton;
    btnFillRect: TToolButton;
    btnTriangle: TToolButton;
    cmbGraphMode : TComboBox;
    color0: TImage;
    color1: TImage;
    color2: TImage;
    color4: TImage;
    editText : TEdit;
    images: TImageList;
    imgEditor: TImage;
    lblCoord: TLabel;
    menuGr: TMainMenu;
    MenuItem1: TMenuItem;
    itemViewer : TMenuItem;
    miLoadPalette : TMenuItem;
    mnuExit: TMenuItem;
    MenuItem12 : TMenuItem;
    miDefaultPalette : TMenuItem;
    miColorPalette : TMenuItem;
    miView : TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    miSettings: TMenuItem;
    MenuItem2: TMenuItem;
    menuExportDataAsm: TMenuItem;
    menuExportDataFastBasic: TMenuItem;
    menuExportDataMadPascal: TMenuItem;
    menuExportDataAction: TMenuItem;
    menuExportDataCC65: TMenuItem;
    MenuItem20: TMenuItem;
    miRedo : TMenuItem;
    miUndo : TMenuItem;
    MenuItem23 : TMenuItem;
    mnuExportData: TMenuItem;
    miCodeGen: TMenuItem;
    menuExportDataAtariBASIC: TMenuItem;
    MenuItem3: TMenuItem;
    miHelp: TMenuItem;
    miNewImage: TMenuItem;
    miClearImage: TMenuItem;
    miOpenImage: TMenuItem;
    miSaveImage: TMenuItem;
    miSaveImageAs: TMenuItem;
    sbGr: TStatusBar;
    toolbar: TToolBar;
    toolbarDraw: TToolBar;
    btnSep02: TToolButton;
    btnSep01: TToolButton;
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
    tbSep: TToolButton;
    btnText : TToolButton;
    btnUndo : TToolButton;
    btnRedo : TToolButton;
    btnSep03 : TToolButton;
    btnViewer : TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure CloseProc(Sender: TObject);
    procedure cmbGraphModeChange(Sender: TObject);
    procedure btnShiftLeftMouseEnter(Sender : TObject);
    procedure LoadPaletteProc(Sender : TObject);
    procedure LoadDefaultPaletteProc(Sender : TObject);
    procedure TextProc(Sender : TObject);
    procedure SetColorProc(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure imgEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure btnTextDisableProc(Sender : TObject);
    procedure ExportData(Sender: TObject);
    procedure RedoProc(Sender : TObject);
    procedure ViewerProc(Sender : TObject);
    procedure UndoProc(Sender : TObject);
    procedure SaveProc(Sender: TObject);
    procedure SaveAsProc(Sender: TObject);
    procedure NewFileProc(Sender: TObject);
    procedure OptionsProc(Sender: TObject);
    procedure PaletteProc(Sender: TObject);
    procedure CodeGenProc(Sender: TObject);
    procedure MoveScreenProc(Sender: TObject);
    procedure ShiftScreenProc(Sender: TObject);
    procedure OpenProc(Sender: TObject);
    procedure SaveData(Sender: TObject);
    procedure ClearProc(Sender: TObject);
    procedure refreshp;
  private
    { private declarations }
    btn, varBtn : TMousebutton;
    is01bit : boolean;
    grBytes : word;
    MouseIsDown: Boolean;
    PrevX, PrevY: Integer;
    PrevXf, PrevYf: Integer;
    undoList : TStringList;
    cntUndo : integer;
    isNormalPlot : boolean;
    isUndo, isRedo : boolean;
    isLine : boolean;
    factX, factY : byte;
    fldFontSet : fldFontSetType;
    textOper : TTextOper;
    procedure Plot(xf, yf : integer);
    procedure PlotEx(x, y : integer);
    procedure Line(xStart, yStart, xEnd, yEnd : integer);
    procedure Rectangle(x1, y1, x2, y2 : integer);
    procedure FillRectangle(x1, y1, x2, y2 : integer);
    procedure Ellipse(x, y, r : integer);
    procedure FillCircle(xc, yc, r: longint);
    procedure Triangle(x1, y1, x2, y2 : integer);
    procedure GrModeSettings;
    procedure SetGrMode;
    procedure ColorRegisters;
    procedure PutChar(scrPos, y, offset : integer);
    procedure ApplyText(Sender: TObject);
    procedure OpenFile(filename : string);
  public
    { public declarations }
    filename : string;
    fld : array[0..319, 0..191] of byte;
    procedure RefreshColors;
  end;

var
  frmGraph: TfrmGraph;

implementation

{$R *.lfm}

uses
  main, settings, colors, graph_gen, code_lib, lib, viewer;

{ TfrmGraph }

procedure TfrmGraph.FormCreate(Sender: TObject);
begin
  frmGraph.DoubleBuffered := true;
  btn := mbMiddle;
  cntUndo := 0;
  undoList := TStringList.Create;
  isUndo := false;
  isRedo := false;
  isLine := true;
  textOper.isInit := false;
//  textOper.isStart := false;

  SetTrackBarUpDown(editShiftMove, $00DDDDDD, clWhite);
end;

procedure TfrmGraph.FormShow(Sender: TObject);
begin
  propFlagModules[1] := 1;
  isChange := true;

  frmMain.Top := 0;
  formId := formGraph;
  sbGr.Panels[0].Text := 'Cursor coordinates: x: 0, y: 0';
//  sbGr.Panels[1].Text := 'Color palette: default';

  grMode := grMode160x96x4;
  cmbGraphMode.ItemIndex := 4;
  GrModeSettings;
  SetGrMode;

  FillRectEx(imgEditor, coltab[0], 0, 0, imgEditor.Width, imgEditor.Height);
  NewFileProc(Sender);
  DefaultFontSet(fldFontSet);

//  debug('grMode', grMode);
end;

procedure TfrmGraph.FormActivate(Sender: TObject);
begin
  formId := formGraph;
end;

procedure TfrmGraph.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = ord('1') then
    frmColors.SelColor := 1
  else if key = ord('2') then
    frmColors.SelColor := 2
  else if key = ord('3') then
    frmColors.SelColor := 3
  else if (ssCtrl in Shift) and (Key = VK_Z) then
    UndoProc(Sender)
  else if (ssShift in Shift) and (ssCtrl in Shift) and (Key = VK_Z) then
    RedoProc(Sender)
  else if key = VK_F12 then
    frmMain.Show;
end;

procedure TfrmGraph.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[1] := 0;
  formId := formMain;
  undoList.Free;
end;

{-----------------------------------------------------------------------------
 Color registers
 -----------------------------------------------------------------------------}
procedure TfrmGraph.ColorRegisters;
begin
  color1.Visible := not is01bit;
  color2.Visible := not is01bit;
  FillRectEx(color4, coltab[0], 0, 0, color4.width, color4.Height);
  FillRectEx(color1, coltab[2], 0, 0, color1.width, color1.Height);
  FillRectEx(color2, coltab[3], 0, 0, color2.width, color2.Height);

  if grMode = grMode320x192x2 then begin
    FillRectEx(color0, coltab[8], 0, 0, color0.width, color0.Height);
    color0.Canvas.Rectangle(0, 0, color0.width, color0.Height);
  end
  else
    FillRectEx(color0, coltab[1], 0, 0, color0.width, color0.Height);
end;

procedure TfrmGraph.cmbGraphModeChange(Sender: TObject);
var
  xf, yf : integer;
  isData : boolean = false;
begin
//  debug('in 1', grMode);
  for xf := 0 to grX do
    for yf := 0 to grY do
      if fld[xf, yf] > 0 then begin
        isData := true;
        break;
      end;

  if isData then begin
    if MessageDlg('Warning', 'You are about to change graphics mode.' +
                  ' Data will be lost! Do you wish to continue?',
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then begin
      case grMode of
        grMode40x24x4  : cmbGraphMode.ItemIndex := 0;
        grMode80x48x2  : cmbGraphMode.ItemIndex := 1;
        grMode80x48x4  : cmbGraphMode.ItemIndex := 2;
        grMode160x96x2 : cmbGraphMode.ItemIndex := 3;
        grMode160x96x4 : cmbGraphMode.ItemIndex := 4;
        grMode160x192x2: cmbGraphMode.ItemIndex := 5;
        grMode160x192x4: cmbGraphMode.ItemIndex := 6;
        grMode320x192x2: cmbGraphMode.ItemIndex := 7;
      end;
      Exit;
    end;
  end;

  case cmbGraphMode.ItemIndex of
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
  FillRectEx(imgEditor, coltab[0], 0, 0, imgEditor.Width, imgEditor.Height);
  NewFileProc(Sender);
end;

procedure TfrmGraph.btnTextDisableProc(Sender : TObject);
begin
  //if Sender = btnNormal then
  //  undoType := 1
  //else if Sender = btnLine then
  //  undoType := 2;

  editText.Enabled := false;
  editText.Visible := false;
end;

procedure TfrmGraph.SetColorProc(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  case TShape(Sender).Tag of
    0: begin
      frmColors.SelColor := 0;
      ColorRegisters;
      color4.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
    1: begin
      frmColors.SelColor := 1;
      ColorRegisters;
      color0.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
    2: begin
      frmColors.SelColor := 2;
      ColorRegisters;
      color1.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
    3: begin
      frmColors.SelColor := 3;
      ColorRegisters;
      color2.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
  end;
  frmColors.Show;
end;

procedure TfrmGraph.RefreshColors;
begin
  ColorRegisters;
  refreshp;
end;

procedure TfrmGraph.GrModeSettings;
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

procedure TfrmGraph.SetGrMode;
var
  a, b : integer;
begin
  for a := 0 to grX do
    for b := 0 to grY do
//      imgEditor.Canvas.Pixels[a*factX, b*factY] := clWhite;
      fld[a, b] := 0;

  btnNormal.Down := true;

  if grX = 319 then
    frmColors.SelColor := 9
  else
    frmColors.SelColor := 1;

  ColorRegisters;
end;

{-----------------------------------------------------------------------------
 Special plot method
 -----------------------------------------------------------------------------}
procedure TfrmGraph.PlotEx(x, y : integer);
begin
  btn := varBtn;
  Plot(x, y);
  btn := mbMiddle;
end;

{-----------------------------------------------------------------------------
 Draw a line
 -----------------------------------------------------------------------------}
// http://www.efg2.com/Lab/Library/Delphi/Graphics/Bresenham.txt
procedure TfrmGraph.Line (xStart, yStart, xEnd, yEnd : integer);
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
   if isLine then
     undoList.Add('0;0;0;0;21');
 END;

{-----------------------------------------------------------------------------
 Draw a rectangle
 -----------------------------------------------------------------------------}
procedure TfrmGraph.Rectangle(x1, y1, x2, y2 : integer);
begin
  //Line(x1, y1, x2 - 1, y1);
  //Line(x2 - 1, y1, x2 - 1, y2 - 1);
  //Line(x1, y2 - 1, x2 - 1, y2 - 1);
  //Line(x1, y1, x1, y2 - 1);
  isLine := false;
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

  undoList.Add('0;0;0;0;21');
end;

{-----------------------------------------------------------------------------
 Draw filled rectangle
 -----------------------------------------------------------------------------}
procedure TfrmGraph.FillRectangle(x1, y1, x2, y2 : integer);
var
  y : integer;
begin
  isLine := false;
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

  undoList.Add('0;0;0;0;21');
end;

//procedure TfrmGraph.Circle(xc, yc, r : integer);
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

{-----------------------------------------------------------------------------
 Draw a circle
 -----------------------------------------------------------------------------}
procedure TfrmGraph.Ellipse(x, y, r : integer);
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

  isLine := false;

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

  //j := 1;
  //while j < r do begin
  //  i := j;
  //  c := 0; a := i - 1;
  //  while i >= c do begin
  //    PlotEx(x + c, y + i);
  //    PlotEx(x + c, y - i);
  //    PlotEx(x - c, y - i);
  //    PlotEx(x - c, y + i);
  //    PlotEx(x + i, y + c);
  //    PlotEx(x + i, y - c);
  //    PlotEx(x - i, y - c);
  //    PlotEx(x - i, y + c);
  //    Inc(c);
  //    a += 1 - c - c;
  //    if a >= 0 then Continue;
  //    Dec(i);
  //    a += i + i;
  //  end;
  //  Inc(j);
  //end;

//  refreshp;

  undoList.Add('0;0;0;0;21');
end;

{-----------------------------------------------------------------------------
 Draw filled circle
 -----------------------------------------------------------------------------}
// https://www.pascalgamedevelopment.com/showthread.php?30526-Draw-a-filled-circle
procedure TfrmGraph.FillCircle(xc, yc, r: longint);
var
  x, y, d: longint;
begin
  isLine := false;
  x := 0; y := r;
  d := 1 - r;
  while x < y do begin
    if d < 0 then
      d := d + x shl 1 + 3
    else begin
      d := d + x shl 1 - y shl 1 + 5;
      dec(y);
    end;
    Line(xc - x, yc - y, xc + x, yc - y);
    Line(xc - y, yc - x, xc + y, yc - x);
    Line(xc - y, yc + x, xc + y, yc + x);
    Line(xc - x, yc + y, xc + x, yc + y);
    inc(x);
  end;
  undoList.Add('0;0;0;0;21');
end;

{-----------------------------------------------------------------------------
 Draw a triangle
 -----------------------------------------------------------------------------}
procedure TfrmGraph.Triangle(x1, y1, x2, y2 : integer);
begin
  isLine := false;
  Line(x1, y1, x1 + ((x2 - x1) div 2), y2);
  Line(x1 + ((x2 - x1) div 2), y2, x2, y1);
  Line(x1, y1, x2, y1);
  undoList.Add('0;0;0;0;21');
end;

procedure TfrmGraph.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : integer;
begin
  btn := Button;
  xf := X div factX;
  yf := Y div factY;
  Plot(xf, yf);
end;

procedure TfrmGraph.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Plot(x div factX, y div factY);
end;

procedure TfrmGraph.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
end;

procedure TfrmGraph.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
begin
  frmGraph.SetFocus;
end;

procedure TfrmGraph.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  if not is01bit and (frmColors.SelColor < 3) then
    inc(frmColors.SelColor);
end;

procedure TfrmGraph.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  if not is01bit and (frmColors.SelColor > 1) then
    dec(frmColors.SelColor);
end;

{-----------------------------------------------------------------------------
 Shift image the whole screen
 -----------------------------------------------------------------------------}
procedure TfrmGraph.ShiftScreenProc(Sender: TObject);
var
  x, y, n : integer;
  fld02 : array[0..319] of byte;
  i : byte;
begin
  case TToolButton(Sender).Tag of
    // Shift left
    0: begin
        for i := 1 to editShiftMove.Value do
          for y := 0 to grY do begin
            n := fld[0, y];
            for x := 1 to grX do
              fld[x - 1, y] := fld[x, y];

            fld[grX, y] := n;
          end;
    end;
    // Shift right
    1: begin
      for i := 1 to editShiftMove.Value do
        for y := 0 to grY do begin
          n := fld[grX, y];
          for x := grX - 1 downto 0 do
            fld[x + 1, y] := fld[x, y];

          fld[0, y] := n;
        end;
    end;
    // Shift up
    2: begin
      for i := 1 to editShiftMove.Value do begin
        for x := 0 to grX do
          fld02[x] := fld[x, 0];

        for x := 0 to grX do
          for y := 1 to grY do begin
            fld[x, y - 1] := fld[x, y];
            fld[x, y] := 0;
          end;

        for x := 0 to grX do
          fld[x, grY] := fld02[x];
      end;
    end;
    // Shift down
    3: begin
      for i := 1 to editShiftMove.Value do begin
        for x := 0 to grX do
          fld02[x] := fld[x, grY];

        for x := 0 to grX do
          for y := grY downto 0 do begin
            fld[x, y + 1] := fld[x, y];
            fld[x, y] := 0;
          end;

        for x := 0 to grX do
          fld[x, 0] := fld02[x];
      end;
    end;
  end;
  refreshp;
end;

{-----------------------------------------------------------------------------
 Move image the whole screen
 -----------------------------------------------------------------------------}
procedure TfrmGraph.MoveScreenProc(Sender: TObject);
var
  x, y : integer;
  i : byte;
begin
  case TToolButton(Sender).Tag of
    // Move left
    0: begin
      for i := 1 to editShiftMove.Value do
        for x := 1 to grX do
          for y := grY downto 0 do begin
            fld[x - 1, y] := fld[x, y];
            fld[x, y] := 0;
          end;
    end;
    // Move right
    1: begin
      for i := 1 to editShiftMove.Value do
        for x := grX - 1 downto 0 do
          for y := grY downto 0 do begin
            fld[x + 1, y] := fld[x, y];
            fld[x, y] := 0;
          end;
    end;
    // Move up
    2: begin
      for i := 1 to editShiftMove.Value do
        for x := 0 to grX - 1 do
          for y := 1 to grY do begin
            fld[x, y - 1] := fld[x, y];
            fld[x, y] := 0;
          end;
    end;
    // Move down
    3: begin
      for i := 1 to editShiftMove.Value do
        for x := 0 to grX - 1 do
          for y := grY - 1 downto 0 do begin
            fld[x, y + 1] := fld[x, y];
            fld[x, y] := 0;
          end;
    end;
  end;
  refreshp;
end;

{-----------------------------------------------------------------------------
 Load screen from file
 -----------------------------------------------------------------------------}
procedure TfrmGraph.OpenFile(filename : string);
var
  x, y : integer;
  dta : byte;
  fs : TFileStream;
  tempCalcX : word;
begin
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
          if fs.Position < fs.Size then begin
            dta := fs.ReadByte;
            fld[x shl 2, y] := dta div 64;
            fld[x shl 2 + 1, y] := (dta mod 64) shr 4;  //div 16;
            fld[x shl 2 + 2, y] := (dta mod 16) shr 2;  //div 4;
            fld[x shl 2 + 3, y] := dta mod 4;
          end;
        end;
      end
      // 2-color graphics modes
      else begin
  //          if grX = 319 then begin
        tempCalcX := grX shr 3;  // div 8;
        for x := 0 to tempCalcX do begin
          if fs.Position < fs.Size then begin
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
    end;
    if ((grMode = grMode40x24x4) and (fs.Size = 244))
       or ((grMode = grMode80x48x4) and (fs.Size = 964))
       or ((grMode = grMode160x96x4) and (fs.Size = 3844))
       or ((grMode = grMode160x192x4) and (fs.Size = 7684))
    then begin
      for x := 0 to 3 do
        if fs.Position < fs.Size then begin
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
        for x := 0 to 1 do
          if fs.Position < fs.Size then begin
            dta := fs.ReadByte;
            coltab[x] := colorMem[dta div 2];
            colorValues[x] := dta;
          end;
      end;
    end;
    ColorRegisters;
    frmColors.Show;
//    beep;
//    FillRectEx(color0, clRed, 0, 0, color0.width, color0.Height);
    caption := programName + ' ' + programVersion +
               ' - Graphics editor (' + filename + ')';
  finally
    fs.Free;
    refreshp;
  end;
end;

{-----------------------------------------------------------------------------
 Dialog load: Load screen from file
 -----------------------------------------------------------------------------}
procedure TfrmGraph.OpenProc(Sender: TObject);
begin
  frmMain.dlgOpen.Title := 'Open existing picture file';

//  debug('open grMode', grMode);

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
    OpenFile(filename);
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

{-----------------------------------------------------------------------------
 Save screen to file
 -----------------------------------------------------------------------------}
procedure TfrmGraph.SaveData(Sender: TObject);
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

{-----------------------------------------------------------------------------
 Clear screen
 -----------------------------------------------------------------------------}
procedure TfrmGraph.ClearProc(Sender: TObject);
begin
  FillByte(fld, SizeOf(fld), 0);
  refreshp;
end;

procedure TfrmGraph.imgEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
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

  isNormalPlot := btnNormal.Down;

  if not btnText.Down then begin
    textOper.isInit := false;
    if btnNormal.Down then begin
//      undoType := 1;
      Plot(xf, yf);
    end;
  end;
end;

procedure TfrmGraph.imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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
      if grX = 319 then
        frmColors.SelColor := 9
      else if is01bit then
        frmColors.SelColor := 1;

      col := frmColors.SelColor;
      if (grX < 319) and (col > 3) then col := 3;

//      fld[xf, yf] := frmColors.SelColor;

      imgEditor.Canvas.Brush.Color := coltab[col];
      imgEditor.Canvas.Ellipse(PrevXf*factX, PrevYf*factY, xf*factX, yf*factY);
    end
    else if btnFilledEllipse.Down then begin
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

      imgEditor.Canvas.Brush.Color := coltab[col];
      imgEditor.Canvas.Ellipse(PrevXf*factX, PrevYf*factY, xf*factX, yf*factY);
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
    end;
  end;
  sbGr.Panels[0].Text := 'Cursor coordinates: ' + 'x: ' + inttostr(xf) + ', y: ' + inttostr(yf);
end;

procedure TfrmGraph.imgEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
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
    sbGr.Panels[0].Text := 'Cursor coordinates: ' + 'x: ' + inttostr(xf) + ', y: ' + inttostr(yf);

//    editText.Enabled := false;
//    editText.Visible := false;

    if btnLine.Down then begin
//      undoType := 1;
      isLine := true;
      Line(PrevXf, PrevYf, xf, yf);
    end
    else if btnEllipse.Down then begin
      if PrevXf > xf then begin
        swapX := xf;
        xf := PrevXf;
        PrevXf := swapX;
      end;

//      undoType := 1;

//      Ellipse
      if PrevYf > yf then
        Ellipse(PrevXf + (xf - PrevXf) div 2, PrevYf - (xf - PrevXf) div 2, (xf - PrevXf) div 2)
      else
        Ellipse(PrevXf + (xf - PrevXf) div 2, PrevYf + (xf - PrevXf) div 2, (xf - PrevXf) div 2);
    end
    else if btnFilledEllipse.Down then begin
      if PrevXf > xf then begin
        swapX := xf;
        xf := PrevXf;
        PrevXf := swapX;
      end;

//      undoType := 1;

//      Ellipse
      if PrevYf > yf then
        FillCircle(PrevXf + (xf - PrevXf) div 2,
                   PrevYf - (xf - PrevXf) div 2,
                   (xf - PrevXf) div 2)
      else
        FillCircle(PrevXf + (xf - PrevXf) div 2,
                   PrevYf + (xf - PrevXf) div 2,
                   (xf - PrevXf) div 2);
    end
    else if btnRect.Down then begin
//      undoType := 1;
      Rectangle(PrevXf, PrevYf, xf, yf)
    end
    else if btnFillRect.Down then begin
//      undoType := 1;
      FillRectangle(PrevXf, PrevYf, xf, yf)
    end
    else if btnTriangle.Down then begin
//      undoType := 1;
      Triangle(xf, yf, PrevXf, PrevYf)
    end
    else if btnText.Down and textOper.isInit then begin
//      textOper.isInit := false;
//      textOper.isStart := true;
      sbGr.Panels[1].Text := 'Set text at specific coordinates 2';
      textOper.x := xf;
      textOper.y := yf;
      textOper.text := editText.Text;  //'test ' + inttostr(xf) + ' ' + inttostr(yf);
//      editText.Enabled := true;
//      editText.Visible := true;
//      ShowMessage(inttostr(xf) + ' ' + inttostr(yf));
//      undoType := 1;
      ApplyText(Sender);
    end;
  end;
  MouseIsDown := false;
  refreshp;
//  showmessage(inttostr(cntUndo) + ' *** undolist ' + undolist[cntUndo - 1]);
end;

{------------------------------------------------------------------------------
 Export graphics data in selected programming language as data value statements
 ------------------------------------------------------------------------------}
procedure TfrmGraph.ExportData(Sender: TObject);
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
      frmMain.dlgSave.Title := 'Export Action!/Effectus data as';
      frmMain.dlgSave.Filter := 'Action!/Effectus files (*.act, *.eff)|*.act;*.eff|All files (*.*)|*.*';
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
    8: begin
      langIndex := _KICKC;
      frmMain.dlgSave.Title := 'Export KickC data as';
      frmMain.dlgSave.Filter := 'KickC files (*.c)|*.c|All files (*.*)|*.*';
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

{-----------------------------------------------------------------------------
 Undo previous draw operation
 -----------------------------------------------------------------------------}
procedure TfrmGraph.UndoProc(Sender : TObject);
var
  col, xf, yf : integer;
  plotData : TStringArray;
  i, x, y : integer;
begin
  isUndo := true;
//  showmessage('undo ' + inttostr(cntUndo));
  if cntUndo <= 0 then begin
    cntUndo := 0;
    exit;
  end;

(*  if isNormalPlot then begin
//    showmessage('isNormalPlot');
    plotData := undoList[cntUndo - 1].Split(';');
    x := StrToInt(plotData[2]);
    y := StrToInt(plotData[3]);

    for xf := 0 to grX do
      for yf := 0 to grY do
        if (xf = x) and (yf = y) then begin
          col := StrToInt(plotData[0]);
          fld[xf, yf] := col;
          if (col = 1) and (grX = 319) then col := 9;
          imgEditor.Canvas.Brush.Color := coltab[col];
          imgEditor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
        end;

    Dec(cntUndo);
  end
  else begin*)
//    if btnText.Down or btnLine.Down then begin
//      for i := cntUndo - 1 downto 0 do begin
      i := cntUndo - 1;
//      showmessage('pre cntUndo = ' + inttostr(cntUndo));
      repeat
        plotData := undoList[i].Split(';');
//        showmessage('i = ' + inttostr(i) + ' plotData = ' + plotData[4]);
        if (plotData[4] = '1') or (plotData[4] = '2') then begin  // IntToStr(undoType)
//          cntUndo := i - 1;
          x := StrToInt(plotData[2]);
          y := StrToInt(plotData[3]);
          for xf := 0 to grX do
            for yf := 0 to grY do
              if (xf = x) and (yf = y) then begin
                col := StrToInt(plotData[0]);
                fld[xf, yf] := col;
                if (col = 1) and (grX = 319) then col := 9;
                FillRectEx(imgEditor, coltab[col], xf*factX, yf*factY, factX, factY);
              end;
        end;
        Dec(i);
//        if i <= 0 then i := 0;
      until (plotData[4] = '21') or (plotData[4] = '2') or (i = -1);
      cntUndo := i + 1;
//      showmessage('cntUndo = ' + inttostr(cntUndo) + ' i = ' + inttostr(i) + ' pd = ' + plotData[4]);
      //end;
//    end
//    else begin
//      showmessage('3');
  //    for i := cntUndo - 1 downto 0 do begin
  //      plotData := undoList[i].Split(';');
  ////      showmessage('i = ' + inttostr(i) + ', test 3 = ' + test[3]);
  //      if plotData[4] = '1' then begin
  //        cntUndo := i - 1;
  ////        showmessage('cntUndo = ' + inttostr(cntUndo));
  //        break;
  //      end;
  //
  //      x := StrToInt(plotData[2]);
  //      y := StrToInt(plotData[3]);
  //      for xf := 0 to grX do
  //        for yf := 0 to grY do
  //          if (xf = x) and (yf = y) then begin
  //            col := StrToInt(plotData[0]);
  //            fld[xf, yf] := col;
  //            if (col = 1) and (grX = 319) then col := 9;
  //            imgEditor.Canvas.Brush.Color := coltab[col];
  //            imgEditor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
  //          end;
  //    end;
//    end;
(*  end;*)

  imgEditor.Refresh;
end;

{-----------------------------------------------------------------------------
 Redo draw operation
 -----------------------------------------------------------------------------}
procedure TfrmGraph.RedoProc(Sender : TObject);
var
  col, xf, yf : integer;
  plotData : TStringArray;
  x, y : integer;
begin
  if not isUndo then exit;

//  showmessage('redo ' + inttostr(cntUndo));

  isRedo := true;
  Inc(cntUndo);
  if cntUndo > undoList.Count then begin
//    Showmessage('cntUndo > undoList.Count');
    cntUndo := undoList.Count;
    Exit;
  end;

  if isNormalPlot then begin
    plotData := undoList[cntUndo - 1].Split(';');
    x := StrToInt(plotData[2]);
    y := StrToInt(plotData[3]);
//    showmessage(test[0] + ' * ' + test[2] + ' * ' + test[3]);

    for xf := 0 to grX do
      for yf := 0 to grY do
        if (xf = x) and (yf = y) then begin
          col := StrToInt(plotData[1]);
          fld[xf, yf] := col;
          if (col = 1) and (grX = 319) then col := 9;
          FillRectEx(imgEditor, coltab[col], xf*factX, yf*factY, factX, factY);
        end;
  end;

  imgEditor.Refresh;
end;

procedure TfrmGraph.ViewerProc(Sender : TObject);
begin
//  SaveColors;

  frmViewer.isModal := true;
  frmViewer.ShowModal;
//  showmessage(frmViewer.filename);
  if frmViewer.filename <> '' then
    OpenFile(frmViewer.filename);

//  RetrieveColors;
end;

procedure TfrmGraph.CloseProc(Sender: TObject);
begin
  Close;
end;

procedure TfrmGraph.SaveProc(Sender: TObject);
begin
  SaveData(Sender);
end;

procedure TfrmGraph.SaveAsProc(Sender: TObject);
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

procedure TfrmGraph.NewFileProc(Sender: TObject);
begin
  ClearProc(Sender);
  filename := getDir + 'examples\';
  case grMode of
    grMode40x24x4  : filename += 'screen01.gr3';
    grMode80x48x2  : filename += 'screen01.gr4';
    grMode80x48x4  : filename += 'screen01.gr5';
    grMode160x96x2 : filename += 'screen01.gr6';
    grMode160x96x4 : filename += 'screen01.gr7';
    grMode160x192x2: filename += 'screen01.mic';
    grMode160x192x4: filename += 'screen01.mic';
    grMode320x192x2: filename += 'screen01.gr8';
  end;
  caption := programName + ' ' + programVersion + ' - Graphics editor (' + filename + ')';
end;

procedure TfrmGraph.OptionsProc(Sender: TObject);
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
    grMode40x24x4  : cmbGraphMode.ItemIndex := 0;
    grMode80x48x2  : cmbGraphMode.ItemIndex := 1;
    grMode80x48x4  : cmbGraphMode.ItemIndex := 2;
    grMode160x96x2 : cmbGraphMode.ItemIndex := 3;
    grMode160x96x4 : cmbGraphMode.ItemIndex := 4;
    grMode160x192x2: cmbGraphMode.ItemIndex := 5;
    grMode160x192x4: cmbGraphMode.ItemIndex := 6;
    grMode320x192x2: cmbGraphMode.ItemIndex := 7;
  end;
  FillRectEx(imgEditor, coltab[0], 0, 0, imgEditor.Width, imgEditor.Height);
end;

{-----------------------------------------------------------------------------
 Show color palette
 -----------------------------------------------------------------------------}
procedure TfrmGraph.PaletteProc(Sender: TObject);
begin
  frmColors.Show;
end;

{-----------------------------------------------------------------------------
 Code generator
 -----------------------------------------------------------------------------}
procedure TfrmGraph.CodeGenProc(Sender: TObject);
begin
  frmGraphGen := TfrmGraphGen.Create(Self);
  with frmGraphGen do
    try
      ShowModal;
    finally
      Free;
    end;
end;

{-----------------------------------------------------------------------------
 Plot routine
 -----------------------------------------------------------------------------}
procedure TfrmGraph.Plot(xf, yf : integer);
var
  col, oldCol : byte;
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

  // Clear Undo list and start over
  if isUndo then begin
    undoList.Clear;
    isUndo := false;
    cntUndo := 0;
  end;
//  else if isRedo then begin
//    isRedo := false;
////    cntUndo := 0;
//  end;

//  showmessage('undo ' + IntToStr(xf) + ';' + IntToStr(yf) + ' *** ' + inttostr(fld[xf, yf]));
  // 1, 1 ==> 0, 1, 1
  // 1, 1 ==> 1, 1, 1

  oldCol := fld[xf, yf];
  if (grX = 319) and (col = 9) then
    fld[xf, yf] := 1
  else
    fld[xf, yf] := col;

  if isNormalPlot then begin
    undoList.Add(IntToStr(oldCol) + ';' + IntToStr(fld[xf, yf]) + ';' +
                 IntToStr(xf) + ';' + IntToStr(yf) + ';2');
  end
  else begin
    undoList.Add(IntToStr(oldCol) + ';' + IntToStr(fld[xf, yf]) + ';' +
                 IntToStr(xf) + ';' + IntToStr(yf) + ';1');
  end;
////    if btnText.Down then
//      undoList.Add(IntToStr(oldCol) + ';' + IntToStr(fld[xf, yf]) + ';' +
//                   IntToStr(xf) + ';' + IntToStr(yf) + ';1');  // + IntToStr(undoType));
//    //else if btnFilledEllipse.Down then
//    //  undoList.Add(IntToStr(oldCol) + ';' + IntToStr(fld[xf, yf]) + ';' +
//    //               IntToStr(xf) + ';' + IntToStr(yf) + ';1')
//  end;
  Inc(cntUndo);

  FillRectEx(imgEditor, coltab[col], xf*factX, yf*factY, factX, factY);
end;

{-----------------------------------------------------------------------------
 Refresh screen data
 -----------------------------------------------------------------------------}
procedure TfrmGraph.refreshp;
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
      FillRectEx(imgEditor, coltab[col], xf*factX, yf*factY, factX, factY);
    end;

  imgEditor.Refresh;
end;

{-----------------------------------------------------------------------------
 Draw text on screen
 -----------------------------------------------------------------------------}
procedure TfrmGraph.ApplyText(Sender: TObject);
var
  i, n : byte;
  ch : char;
  isAtascii : boolean;
begin
//  showmessage(chr(97) + ' ' + inttostr(ord('b')));  // A 65
  for i := 1 to Length(textOper.text) do begin
    ch := textOper.text[i];

    if (ord(ch) >= 32) and (ord(ch) <= 95) then begin
      n := ord(ch) - 32;
      isAtascii := true;
    end
    else if (ord(ch) >= 97) and (ord(ch) <= 122) then begin
      n := ord(ch);
      isAtascii := true;
    end
    else
      isAtascii := false;

    if isAtascii then
      PutChar(textOper.x + (i - 1) shl 3, textOper.y, n)
  end;
  undoList.Add('Text;0;0;0;21');
  Inc(cntUndo);
end;

{-----------------------------------------------------------------------------
 Draw character on screen
 -----------------------------------------------------------------------------}
procedure TfrmGraph.PutChar(scrPos, y, offset : integer);
var
  col : byte;
  dx, dy, xf, yf : integer;
  isInverse : boolean = false;
begin
  xf := scrPos;
  yf := y;
//  showmessage(inttostr(scrPos) + ' ' + inttostr(y) + ' ' + inttostr(offset));
//  if xf > grX then xf := grX;
//  if xf mod 8 = 0 then Inc(xf, 8);
  //for dy := 0 to 7 do
  //  for dx := 0 to 7 do
  //    if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
  //      xf := dx shl 3;
  //      break;
  //    end;

  if offset > 128 then begin
    Dec(offset, 128);
    isInverse := true;
  end;

  offset := offset shl 3;
  for dy := 0 to 7 do
    for dx := 0 to 7 do begin
      col := fldFontSet[dx, dy + offset];
      if isInverse then begin
        if col = 1 then
          col := 0
        else if col = 0 then
          col := 1;
      end;

      if col = 1 then
        PlotEx(xf + dx, yf + dy);
    end;
end;

procedure TfrmGraph.TextProc(Sender : TObject);
begin
//  undoType := 8;
  textOper.isInit := true;
  sbGr.Panels[1].Text := 'Set text at specific coordinates';

  editText.Enabled := true;
  editText.Visible := true;
end;

procedure TfrmGraph.btnShiftLeftMouseEnter(Sender : TObject);
begin
  editShiftMove.Enabled := true;
  editShiftMove.Visible := true;
end;

procedure TfrmGraph.LoadPaletteProc(Sender : TObject);
begin
  frmMain.LoadColorPaletteProc(Sender);
  RefreshColors;
end;

procedure TfrmGraph.LoadDefaultPaletteProc(Sender : TObject);
begin
  frmMain.LoadDefaultPalette;
  RefreshColors;

  //frmColors.SelColor := 0;
  //ColorRegisters;
  //color4.Canvas.Rectangle(0, 0, color1.width, color1.Height);

//  ColorRegisters;
end;

//  with imgEditor.Canvas do begin
////    Brush.Color:=RGBToColor(0,166,81);
////    Font.Color:=RGBToColor(255,255,255);
////    Brush.Style:=bsClear;
////    GetTextSize('Hello World', 11, 11);
////    TextOut(80,60,'Hello');
////
//    Font.Name := 'Verdana';
//    Font.Height := 64;
//    TextOut(10, 10, 'This is an example.');
//  end;

//procedure TfrmGraph.Flodfill(x, y : integer; f, o : integer);
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

end.

