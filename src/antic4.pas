{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Antic mode 4 and 5 editor
}
unit antic4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, ComCtrls, Buttons, BCTrackbarUpdown, strutils, lcltype,
  common;

type
  TExtension = record
    filterSave : string;
    filter : string;
  end;

  { TfrmAntic4 }
  TfrmAntic4 = class(TForm)
    btnCharDown : TSpeedButton;
    btnCharLeft : TSpeedButton;
    btnCharRight : TSpeedButton;
    btnCharUp : TSpeedButton;
    btnFlipX: TToolButton;
    btnFlipY: TToolButton;
    btnInvert : TToolButton;
    btnRotate: TToolButton;
    btnFillScreen: TToolButton;
    btnViewer : TToolButton;
    chkMaxSize : TCheckBox;
    cmbAnticMode : TComboBox;
    boxResize : TGroupBox;
    color0 : TImage;
    color1 : TImage;
    color2 : TImage;
    color3 : TImage;
    color4 : TImage;
    editX : TBCTrackbarUpdown;
    editY : TBCTrackbarUpdown;
    GroupBox3 : TGroupBox;
    grpCharOper : TGroupBox;
    imgBaseFontSetInv : TImage;
    imgCharAntic45 : TImage;
    imgCharOrig : TImage;
    imgChar: TImage;
    imgEditor: TImage;
    imgFontSet: TImage;
    imgBaseFontSet: TImage;
    imgFontSetInv : TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    lblCharInfo01 : TLabel;
    lblCharInfo02 : TLabel;
    lblAnticMode45: TLabel;
    Label20: TLabel;
    Label21 : TLabel;
    Label22 : TLabel;
    Label23 : TLabel;
    Label24 : TLabel;
    Label25 : TLabel;
    Label26 : TLabel;
    Label27 : TLabel;
    Label28 : TLabel;
    Label29 : TLabel;
    Label3 : TLabel;
    Label30 : TLabel;
    Label31 : TLabel;
    Label32 : TLabel;
    Label33 : TLabel;
    Label34 : TLabel;
    Label35 : TLabel;
    Label36 : TLabel;
    Label37 : TLabel;
    Label38 : TLabel;
    lblAnticMode45Inv : TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblCharInfo03 : TLabel;
    lblNum0: TLabel;
    lblNum1: TLabel;
    lblNum10 : TLabel;
    lblNum11 : TLabel;
    lblNum12 : TLabel;
    lblNum13 : TLabel;
    lblNum14 : TLabel;
    lblNum15 : TLabel;
    lblNum2: TLabel;
    lblNum3: TLabel;
    lblNum4: TLabel;
    lblNum5: TLabel;
    lblNum6: TLabel;
    lblNum7: TLabel;
    lblNum8 : TLabel;
    lblNum9 : TLabel;
    menuAntic4: TMainMenu;
    menuClearChar : TMenuItem;
    menuFile: TMenuItem;
    menuInvert : TMenuItem;
    MenuItem1 : TMenuItem;
    itemViewer : TMenuItem;
    itemOpenAntic2 : TMenuItem;
    MenuItem10 : TMenuItem;
    MenuItem11 : TMenuItem;
    menuCopyChar : TMenuItem;
    MenuItem12 : TMenuItem;
    MenuItem14 : TMenuItem;
    MenuItem17 : TMenuItem;
    MenuItem18 : TMenuItem;
    itemShowGrid : TMenuItem;
    itemHideGrid : TMenuItem;
    MenuItem19 : TMenuItem;
    popHideGrid : TMenuItem;
    popShowGrid : TMenuItem;
    MenuItem3 : TMenuItem;
    MenuItem5 : TMenuItem;
    MenuItem6 : TMenuItem;
    itemDefaultColorPalette : TMenuItem;
    itemClose: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem4 : TMenuItem;
    itemColorPalette : TMenuItem;
    menuRestoreChar : TMenuItem;
    menuView : TMenuItem;
    itemSaveFont: TMenuItem;
    menuEdit: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    itemSaveFontAs: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    menuTools: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    itemLoadFont: TMenuItem;
    popCancelCopyChar : TMenuItem;
    popCopyChar : TMenuItem;
    popMenu : TPopupMenu;
    radMoveChar : TRadioButton;
    radShiftChar : TRadioButton;
    statusBar: TStatusBar;
    ToolBar2: TToolBar;
    btnClearCh: TToolButton;
    btnCopyChar : TToolButton;
    btnSettings : TToolButton;
    ToolButton17: TToolButton;
    btnLoadScreen: TToolButton;
    btnSaveScreen: TToolButton;
    btnClearScreen: TToolButton;
    btnCode: TToolButton;
    ToolButton2 : TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure GenCodeProc(Sender: TObject);
    procedure DefaultColorPaletteProc(Sender : TObject);
    procedure ColorProc(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure imgBaseFontSetDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure imgCharAntic45Down(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure imgCharAntic45Move(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure imgCharAntic45Up(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure InvertProc(Sender : TObject);
    procedure ShowGridProc(Sender : TObject);
    procedure RestoreCharProc(Sender : TObject);
    procedure NumAnticMode45Proc(Sender : TObject);
    procedure LoadAntic2Screen(Sender : TObject);
    procedure CopyCharProc(Sender : TObject);
    procedure CancelCopyCharProc(Sender : TObject);
    procedure ViewerProc(Sender : TObject);
    procedure EditDimProc(Sender : TObject);
    procedure CharOper(Sender : TObject);
    procedure cmbAnticModeChange(Sender : TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgCharDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgCharMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgCharUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgEditorDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgEditorMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgEditorUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgFontSetDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure editYMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure CloseWinProc(Sender: TObject);
    procedure lblNumProc(Sender: TObject);
    procedure ClearEditorProc(Sender: TObject);
    procedure ColorsProc(Sender: TObject);
    procedure DefaultSetProc(Sender: TObject);
    procedure SaveFontAsProc(Sender: TObject);
    procedure LoadScreenProc(Sender: TObject);
    procedure FillScreenWithCharProc(Sender: TObject);
    procedure SaveScreenAsProc(Sender: TObject);
    procedure SaveScreenProc(Sender: TObject);
    procedure LoadFontProc(Sender: TObject);
    procedure SaveFontProc(Sender: TObject);
    procedure ClearScreenProc(Sender: TObject);
    procedure ClearCharProc(Sender: TObject);
    procedure FlipXProc(Sender: TObject);
    procedure FlipYProc(Sender: TObject);
    procedure RotateProc(Sender: TObject);
    procedure ShiftRightProc(Sender: TObject);
    procedure ShiftUpProc(Sender: TObject);
    procedure MoveDownProc(Sender: TObject);
    procedure MoveLeftProc(Sender: TObject);
    procedure MoveRightProc(Sender: TObject);
    procedure MoveUpProc(Sender: TObject);
    procedure ShiftDownProc(Sender: TObject);
    procedure ShiftLeft(Sender: TObject);
    procedure MaxSizeProc(Sender : TObject);
  private
    { private declarations }
    btn : TMousebutton;
    offsY : byte;
    isSaveAs, isSaveFontAs : boolean;
    isFontSetNormal : boolean;
    isEdit : boolean;
    isCreate : boolean;
    ext : TExtension;
    isAntic2 : boolean;
    isCopyChar : boolean;
    isShowGrid : boolean;
    gridColor : TColor;
    procedure ShowFontSet;
    procedure ShowBaseFontSet;
    procedure ShowFontSet02(offset : byte);
    procedure SetXY(_maxX, _maxY : byte);
    procedure Plot(xf, yf : integer);
    procedure PutChar(scrPos, y, offset : integer);
    procedure SaveScreen;
    procedure SaveFont;
    procedure RefreshChar;
    procedure RefreshCharX(offset : integer);
    procedure PlotChar(xf, yf : byte);
    procedure PlotCharAntic45(xf, yf : byte);
    procedure FillScreen(character : byte; isBackground : boolean);
    procedure SetAnticMode(mode : byte);
    procedure OpenFile(filename : string);
    procedure ColorRegisters;
    procedure MaskColor(x, y, offset : integer; isInverse : boolean);
    procedure CopyChar(isCopy : boolean);
    procedure CharInfo(offset : integer);
    procedure SetGrid(gColor : TColor);
  public
    { public declarations }
    filename : string;
    fontName : string;
    fldFontSet : fldFontSetType;
    fldFontSetOrig : fldFontSetType;
    fldChar : charType;
    fldCharAntic45 : charType;
    fldAtascii : array[0.._ANTIC_MODE_4_SIZE - 1] of byte;
    grX, grY : integer;
    chrX, chrY : byte;
    factX, factY,             // Character screen editor offset
    chrFactX, chrFactY,       // Character editor pixel offsets
    factX02, factY02 : byte;  // Font set character offsets
    maxX, maxY : byte;        // Maximum X and Y coordinates
    maxSize : integer;
    anticMode : byte;
    modeHeight : byte;
    isTextModeChanged : boolean;
    charEditIndex : array[0..127] of byte;
    offs : byte;
    charXOffset, charYOffset, charYOffset02 : word;
    procedure RefreshData;
    procedure RefreshColors;
  end;

var
  frmAntic4: TfrmAntic4;

implementation

{$R *.lfm}

uses
  main, lib, colors, antic4_gen, viewer;

{ TfrmAntic4 }

procedure TfrmAntic4.FormCreate(Sender: TObject);
begin
  DoubleBuffered := true;
  btn := mbMiddle;
  isCreate := true;
  isCopyChar := false;
  isShowGrid := false;
  FillByte(charEditIndex, SizeOf(charEditIndex), 0);
  SetTrackBarUpDown(editX, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editY, $00DDDDDD, clWhite);
end;

procedure TfrmAntic4.FormShow(Sender: TObject);
var
  i : byte;
begin
  propFlagModules[6] := 1;
  isChange := true;
  frmMain.Top := 10;
  formId := formAntic4;
  filename := getDir + 'examples\screen01.an4';
  caption := programName + ' ' + programVersion +
             ' - Antic mode 4 and 5 editor (' + filename + ')';

  isEdit := false;

  modeHeight := 24;
  SetXY(39, 23);
  editX.Value := maxX;
  editY.Value := maxY;
  SetAnticMode(4);
  factX := 3;
  factX02 := 2;
//  editorX := 24;

  // Character editor parameters
  chrX := 7; chrY := 7;
  chrFactX := 18; chrFactY := 18;

  isFontSetNormal := true;
  DefaultFontSet(fldFontSet);
  DefaultFontSet(fldFontSetOrig);
  ShowBaseFontSet;
//  FillRectEx(imgEditor, coltab[0], 0, 0, imgEditor.Width, imgEditor.Height);
  FillRectEx(imgChar, coltabFont[0], 0, 0, imgChar.Width, imgChar.Height);
  FillRectEx(imgCharAntic45, colTab[0], 0, 0, imgChar.Width, imgChar.Height);

  for i := 0 to 15 do
    (FindComponent('lblNum' + IntToStr(i)) as TLabel).Caption := '0';

  gridColor := clred;
  gridColor := colTab[0];
  FillScreen(0, false);
  isCreate := false;

  offs := 1;
  isFontSetNormal := true;
  RefreshCharX(offs);
  CharInfo(offs);
  PlotChar(255, 255);
  ColorRegisters;

  statusBar.Panels[2].Text := 'Font/Character set: default';
end;

procedure TfrmAntic4.FormActivate(Sender: TObject);
begin
  ShowFontSet;
  formId := formAntic4;
//  FormStyle := fsSystemStayOnTop;
end;

procedure TfrmAntic4.FormDeactivate(Sender: TObject);
begin
  FormStyle := fsNormal;
end;

procedure TfrmAntic4.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  offs := 0;
  RestoreCharProc(Sender);
  propFlagModules[6] := 0;
  formId := formMain;
end;

procedure TfrmAntic4.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_ESCAPE: CopyChar(false);
    VK_F12: frmMain.Show;
    VK_0: SetGrid(colTab[0]);
    VK_1: SetGrid(clWhite);
    VK_2: SetGrid(clNavy);
    VK_3: SetGrid(clMaroon);
    VK_4: SetGrid(clGreen);
    VK_5: SetGrid(clMedGray);
    VK_6: SetGrid(clMoneyGreen);
    VK_7: SetGrid(clSkyBlue);
    VK_8: SetGrid(clTeal);
    VK_9: SetGrid(clBlack);
  end;
end;

procedure TfrmAntic4.CharOper(Sender : TObject);
begin
  if radShiftChar.Checked then begin
    case (Sender as TSpeedButton).Tag of
      0: ShiftLeft(Sender);
      1: ShiftRightProc(Sender);
      2: ShiftUpProc(Sender);
      3: ShiftDownProc(Sender);
    end;
  end
  else begin
    case (Sender as TSpeedButton).Tag of
      0: MoveLeftProc(Sender);
      1: MoveRightProc(Sender);
      2: MoveUpProc(Sender);
      3: MoveDownProc(Sender);
    end;
  end;
end;

procedure TfrmAntic4.ViewerProc(Sender : TObject);
begin
  frmViewer.isModal := true;
  frmViewer.ShowModal;
  if frmViewer.filename <> '' then
    OpenFile(frmViewer.filename);
end;

procedure TfrmAntic4.ColorProc(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  case TShape(Sender).Tag of
    0: color4.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    1: color0.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    2: color1.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    3: color2.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    10: color3.Canvas.Rectangle(0, 0, color1.width, color1.Height);
  end;
  frmColors.SelColor := TShape(Sender).Tag;
  ColorRegisters;
//  frmColors.Show;
end;

procedure TfrmAntic4.imgBaseFontSetDown(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
var
  n, m, xf, yf : byte;
  offset : word;
begin
  isFontSetNormal := TImage(Sender).Tag = 0;

  if Button = mbRight then begin
    popMenu.Popup(x + TImage(Sender).Left + 40, y + TImage(Sender).Top + 40);
    exit;
  end;

  for m := 0 to 7 do
    for n := 0 to 15 do begin
      if (x > 18) and (x <= charXOffset) and
         (y > 17*m) and (y < charYOffset + 17*m) then
      begin
        offsY := m;
        offs := m shl 4 + 1;
        if not isCopyChar then
          RefreshCharX(offs)
        else begin
          offset := offs shl 3;
          for yf := 0 to chrY do
            for xf := 0 to chrX do
              fldFontSet[xf, yf + offset] := fldChar[xf, yf];
        end;
        break;
      end;
      if (x > 18*n) and (x <= charXOffset + 18*n) and
         (y > 17*m) and (y < charYOffset + 17*m) then
      begin
        offsY := m;
        offs := n + m shl 4 + 1;
        if n = 0 then Dec(offs);
        if not isCopyChar then
          RefreshCharX(offs)
        else begin
          offset := offs shl 3;
          for yf := 0 to chrY do
            for xf := 0 to chrX do
              fldFontSet[xf, yf + offset] := fldChar[xf, yf];
        end;
        break;
      end;
    end;

  CharInfo(offs);
  PlotChar(255, 255);
  ColorRegisters;
end;

procedure TfrmAntic4.imgCharAntic45Down(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
var
  xf, yf : byte;
begin
  btn := Button;
  xf := X div chrFactX;
  yf := Y div chrFactY;
  PlotCharAntic45(xf, yf);
  charEditIndex[offs] := 1;
end;

procedure TfrmAntic4.imgCharAntic45Move(Sender : TObject; Shift : TShiftState; X, Y : Integer);
var
  xf, yf : byte;
begin
  xf := X div chrFactX;
  yf := Y div chrFactY;
  PlotCharAntic45(xf, yf);
end;

procedure TfrmAntic4.imgCharAntic45Up(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  btn := mbMiddle;
//  RefreshChar;
end;

procedure TfrmAntic4.InvertProc(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to chrY do
    for x := 0 to chrX do
      fldChar[x, y] := 1 - fldChar[x, y];

  RefreshChar;
end;

procedure TfrmAntic4.ShowGridProc(Sender : TObject);
begin
//  isGridShow := (Sender as TMenuItem).Tag = 0;
  if (Sender as TMenuItem).Tag = 0 then
    SetGrid(clMedGray)
  else
    SetGrid(colTab[0]);
end;

procedure TfrmAntic4.NumAnticMode45Proc(Sender : TObject);
//var
//  dataValue, i, j : byte;
//  bin : string;
begin
//  if InputQuery('Enter data value manually',
//                'Enter decimal value for character data line',
//                bin) then
//  begin
//    dataValue := StrToInt(bin);
//    bin := IntToBin(dataValue, 8);
//    j := (Sender as TLabel).Tag;
//    for i := 0 to 7 do begin
//      fldChar[i, j] := StrToInt(bin[i + 1]);
//      fldCharAntic45[i, j] := fldChar[i, j];
//    end;
//
//    PlotChar(255, 255);
//    charEditIndex[offs] := 1;
//    RefreshCharX(offs);
////    RefreshChar;
//  end;
end;

procedure TfrmAntic4.EditDimProc(Sender : TObject);
begin
  btn := mbMiddle;
  if not isCreate then
    SetXY(editX.Value, editY.Value);
end;

procedure TfrmAntic4.cmbAnticModeChange(Sender : TObject);
begin
  if MessageDlg('Warning', 'You are about to change text mode.' +
                ' Data will be lost! Do you wish to continue?',
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
  begin
    case anticMode of
      4: cmbAnticMode.ItemIndex := 0;
      5: cmbAnticMode.ItemIndex := 1;
    end;
    Exit;
  end;

  case cmbAnticMode.ItemIndex of
    0: begin
      SetAnticMode(4);
      SetXY(39, 23);
    end;
    1: begin
      SetAnticMode(5);
      SetXY(39, 11);
    end;
  end;

  editX.Value := maxX;
  editY.Value := maxY;

  filename := 'examples\screen01.an' + IntToStr(anticMode);
  caption := programName + ' ' + programVersion +
             ' - Antic mode 4 and 5 editor (' + filename + ')';
  FillScreen(0, false);
end;

procedure TfrmAntic4.SetAnticMode(mode : byte);
begin
  anticMode := mode;
  isAntic4 := mode = 4;

  charXOffset := 32;
  if anticMode = 4 then begin
    modeHeight := 24;
    factY := 3;
    factY02 := 2;
    charYOffset := 28;
    charYOffset02 := 17;
    editY.MaxValue := 23;
    ext.filterSave := 'Save Antic mode 4 screen as';
    ext.filter := 'Antic mode 4 screen files (*.an4)|*.an4';
    lblAnticMode45.Caption := 'Antic mode 4 characters';
    lblAnticMode45Inv.Caption := 'Antic mode 4 inverse characters';
  end
  else begin
    modeHeight := 48;
    factY := 6;
    factY02 := 4;
    charYOffset := 40;
    charYOffset02 := 32;
    editY.MaxValue := 11;
    ext.filterSave := 'Save Antic mode 5 screen as';
    ext.filter := 'Antic mode 5 screen files (*.an5)|*.an5';
    lblAnticMode45.Caption := 'Antic mode 5 characters';
    lblAnticMode45Inv.Caption := 'Antic mode 5 inverse characters';
  end;

  ShowFontSet;
end;

procedure TfrmAntic4.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : integer;
begin
  btn := Button;
  xf := X div factX;
  yf := Y div factY;
  Plot(xf, yf);
end;

procedure TfrmAntic4.imgCharDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : byte;
begin
  btn := Button;
  xf := X div chrFactX;
  yf := Y div chrFactY;
  PlotChar(xf, yf);
  charEditIndex[offs] := 1;
end;

procedure TfrmAntic4.imgCharMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : byte;
begin
  xf := X div chrFactX;
  yf := Y div chrFactY;
  PlotChar(xf, yf);
end;

procedure TfrmAntic4.imgCharUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
  RefreshChar;
end;

procedure TfrmAntic4.imgEditorDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : integer;
begin
  btn := Button;
  xf := X div factX;
  yf := Y div factY;

  // Data is changed
  if not isEdit then begin
    isEdit := true;
    if Pos(' *', caption) = 0 then
      caption := caption + ' *';
  end;

  // Plot character
  Plot(xf, yf);
end;

procedure TfrmAntic4.imgEditorMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
begin
  xf := X div factX;
  yf := Y div factY;
  if (xf <= 318) and (yf <= 190) then
    Plot(xf, yf);
end;

procedure TfrmAntic4.imgEditorUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
end;

procedure TfrmAntic4.imgFontSetDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  n, m, xf, yf : byte;
  offset : integer;
begin
  isFontSetNormal := TImage(Sender).Tag = 0;

  if Button = mbRight then begin
    popMenu.Popup(x + TImage(Sender).Left + 40, y + TImage(Sender).Top + 40);
    exit;
  end;

  for m := 0 to 7 do
    for n := 0 to 15 do begin
      if (x > 18) and (x <= charXOffset) and
         (y > charYOffset02*m) and (y < charYOffset + charYOffset02*m) then
      begin
        offsY := m;
        offs := m shl 4 + 1;
        if not isCopyChar then
          RefreshCharX(offs)
        else begin
          offset := offs shl 3;
          for yf := 0 to chrY do
            for xf := 0 to chrX do
              fldFontSet[xf, yf + offset] := fldChar[xf, yf];
        end;
        break;
      end;
      if (x > 18*n) and (x <= charXOffset + 18*n) and
         (y > charYOffset02*m) and (y < charYOffset + charYOffset02*m) then
      begin
        offsY := m;
        offs := n + m shl 4 + 1;
        if n = 0 then Dec(offs);
        if not isCopyChar then
          RefreshCharX(offs)
        else begin
          offset := offs shl 3;
          for yf := 0 to chrY do
            for xf := 0 to chrX do
              fldFontSet[xf, yf + offset] := fldChar[xf, yf];
        end;
        break;
      end;
    end;

  CharInfo(offs);
  PlotChar(255, 255);
  ColorRegisters;
end;

procedure TfrmAntic4.lblNumProc(Sender: TObject);
var
  dataValue, i, j : byte;
  bin : string;
begin
  if InputQuery('Enter data value manually',
                'Enter decimal value for character data line',
                bin) then
  begin
    dataValue := StrToInt(bin);
    bin := IntToBin(dataValue, 8);
    j := (Sender as TLabel).Tag;
    for i := 0 to 7 do begin
      fldChar[i, j] := StrToInt(bin[i + 1]);
      fldCharAntic45[i, j] := fldChar[i, j];
    end;

    charEditIndex[offs] := 1;
    RefreshChar;
  end;
end;

{-----------------------------------------------------------------------------
 Clear screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.ClearEditorProc(Sender: TObject);
begin
  FillScreen(0, false);
end;

{-----------------------------------------------------------------------------
 Show color palette
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.ColorsProc(Sender: TObject);
begin
  frmColors.Show;
end;

procedure TfrmAntic4.DefaultSetProc(Sender: TObject);
begin
  DefaultFontSet(fldFontSet);
  fontName := '';
  statusBar.Panels[2].Text := 'Font/Character set: default';
  ShowFontSet;
  RefreshData;
end;

{-----------------------------------------------------------------------------
 Load Antic 4 or Antic 5 screen from file - actual reading data
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.OpenFile(filename : string);
var
  j : integer;
  fs : TFileStream;
  dta : byte;
begin
  ShowCursor(frmViewer, frmViewer, crHourGlass);

  filename := LowerCase(filename);
  if isAntic2 then begin
    cmbAnticMode.ItemIndex := 0;
    SetAnticMode(4);
  end
  else begin
    if pos('.an4', filename) > 0 then begin
      cmbAnticMode.ItemIndex := 0;
      SetAnticMode(4);
    end
    else if pos('.an5', filename) > 0 then begin
      cmbAnticMode.ItemIndex := 1;
      SetAnticMode(5);
    end;
  end;

  fs := TFileStream.Create(filename, fmOpenReadWrite);
  try
    if isAntic2 then begin
      if pos('.an2', filename) > 0 then begin
        // Dimension x, y
        maxX := fs.ReadByte;
        maxY := fs.ReadByte;

        SetXY(maxx, maxy);
        editX.Value := maxX;
        editY.Value := maxY;
      end
      else begin
        SetXY(39, 23);
        editX.Value := 39;
        editY.Value := 23;
      end;
    end
    else begin
      // Dimension x, y
      maxX := fs.ReadByte;
      maxY := fs.ReadByte;

      SetXY(maxx, maxy);
      editX.Value := maxX;
      editY.Value := maxY;

      // Read color values
      for j := 0 to 3 do begin
        dta := fs.ReadByte;
        coltab[j] := colorMem[dta div 2];
        colorValues[j] := dta;
      end;
      // 4th inverse color register
      dta := fs.ReadByte;
      coltab[10] := colorMem[dta div 2];
      colorValues[10] := dta;
    end;

    // Read data
    for j := 0 to maxSize do
      if fs.Position < fs.Size then
        fldAtascii[j] := fs.ReadByte;
  finally
    fs.Free;
    RefreshColors;
    caption := programName + ' ' + programVersion +
               ' - Antic mode 4 and 5 editor (' + filename + ')';
    ShowCursor(frmViewer, frmViewer, crDefault);
  end;
end;

{-----------------------------------------------------------------------------
 Load Antic 4 or Antic 5 screen from file - dialog box
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.LoadScreenProc(Sender: TObject);
begin
  frmMain.dlgOpen.Title := 'Open existing Antic mode 4 and 5 screen file';
  frmMain.dlgOpen.Filter := 'Antic mode 4/5 screen files' +
                            ' (*.an4, *.an5)|*.an4;*.an5|All files (*.*)|*.*';
  if filename <> '' then
    frmMain.dlgOpen.Filename := filename;

  if frmMain.dlgOpen.Execute then begin
    filename := frmMain.dlgOpen.Filename;
    isAntic2 := false;
    OpenFile(filename);
  end;
end;

procedure TfrmAntic4.FillScreenWithCharProc(Sender: TObject);
begin
  FillScreen(offs, false);
end;

procedure TfrmAntic4.SaveScreenAsProc(Sender: TObject);
begin
  isSaveAs := true;
  SaveScreen;
end;

procedure TfrmAntic4.SaveScreenProc(Sender: TObject);
begin
  SaveScreen;
end;

{-----------------------------------------------------------------------------
 Save Antic mode 4/5 screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.SaveScreen;
var
  fs : TFileStream;
  isOk : boolean = false;

procedure SaveData;
var
  j : integer;
begin
  fs := TFileStream.Create(filename, fmCreate);
  try
    // Dimension x, y
    fs.WriteByte(maxX);
    fs.WriteByte(maxY);

    // Save colors
    for j := 0 to 3 do
      fs.WriteByte(colorValues[j]);

    // 4th inverse color register
    fs.WriteByte(colorValues[10]);

    // Save data
    for j := 0 to maxSize do
      fs.WriteByte(fldAtascii[j]);

    isOk := true;
  finally
    fs.Free;
  end;
end;

begin
  try
    if isSaveAs then begin
      frmMain.dlgSave.Title := ext.filterSave;
  //    frmMain.dlgSave.Filter := 'Antic mode 4/5 screen files (*.an4, *.an5, *.gr0)' +
  //                              '|*.an4;*.an5;*.gr0|All files (*.*)|*.*';
      frmMain.dlgSave.Filter := ext.filter;
      frmMain.dlgSave.Filename := filename;
      if frmMain.dlgSave.Execute then begin
        filename := frmMain.dlgSave.Filename;
        SaveData;
      end;
    end
    else
      SaveData;
  finally
    if isOk then begin
      isEdit := false;
//      statusBar.Panels[1].Text := 'File ' + filename + ' saved';
      caption := programName + ' ' + programVersion +
                 ' - Antic mode 4 and 5 editor (' + filename + ')';
    end;
  end;
end;

{-----------------------------------------------------------------------------
 Load custom character set
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.LoadFontProc(Sender: TObject);
var
  bin : string[9];
  j : integer;
  r, i : byte;
  fs : TFileStream;
begin
  frmMain.dlgOpen.Title := 'Open existing character set file';
  frmMain.dlgOpen.Filter := 'Character set files (*.fnt, *.fon, *.set)' +
                            '|*.fnt;*.fon;*.set|All files (*.*)|*.*';
  //if fontName <> '' then
  fontName := '';
  frmMain.dlgOpen.Filename := fontName;
  if frmMain.dlgOpen.Execute then begin
    fontName := frmMain.dlgOpen.Filename;
    fs := TFileStream.Create(fontName, fmOpenReadWrite);
    try
      for j := 0 to 1023 do
        if fs.Position < fs.Size then begin
          r := fs.ReadByte;
          bin := IntToBin(r, 8);
          for i := 0 to 7 do
            fldFontSet[i, j] := StrToInt(bin[i + 1]);
        end;
    finally
      fs.Free;
      RefreshColors;
      statusBar.Panels[2].Text := 'Font/Character set file: ' + fontName;
    end;
  end;
end;

procedure TfrmAntic4.SaveFontProc(Sender: TObject);
begin
  SaveFont;
end;

procedure TfrmAntic4.SaveFontAsProc(Sender: TObject);
begin
  isSaveFontAs := true;
  SaveFont;
end;

procedure TfrmAntic4.SaveFont;
var
  i : byte;
  j : integer;
  bin : string[9];
  fs : TFileStream;
begin
  frmMain.dlgSave.Title := 'Save character set';
  frmMain.dlgSave.Filter := 'Character set files (*.fnt, *.fon, *.set)|*.fnt;*.fon;*.set|' +
                            'All files (*.*)|*.*';
  // Save as
  if isSaveFontAs then begin
    //if fontName <> '' then
    frmMain.dlgSave.Filename := fontName;

    if frmMain.dlgSave.Execute then begin
      fontName := frmMain.dlgSave.Filename;
      fs := TFileStream.Create(fontName, fmCreate);
      try
        for j := 0 to 1023 do begin
          bin := '';
          for i := 0 to 7 do
            bin += IntToStr(fldFontSet[i, j]);

          fs.WriteByte(bin2dec(bin));
        end;
        ShowFontSet;
        statusBar.Panels[2].Text := 'Font/Character set file: ' + fontName;
      finally
        fs.Free;
      end;
    end;
  end
  // Save
  else begin
    fs := TFileStream.Create(fontName, fmCreate);
    try
      for j := 0 to 1023 do begin
        bin := '';
        for i := 0 to 7 do
          bin += IntToStr(fldFontSet[i, j]);

        fs.WriteByte(bin2dec(bin));
      end;
      ShowFontSet;
      statusBar.Panels[2].Text := 'Font/Character set file: ' + fontName;
    finally
      fs.Free;
    end;
  end;
end;

procedure TfrmAntic4.ClearScreenProc(Sender: TObject);
begin
  FillScreen(0, false);
end;

procedure TfrmAntic4.GenCodeProc(Sender: TObject);
begin
  frmAntic4Gen := TfrmAntic4Gen.Create(Self);
  with frmAntic4Gen do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmAntic4.SetXY(_maxX, _maxY : byte);
begin
  maxX := _maxX;
  maxY := _maxY;
  maxSize := (maxX + 1)*(maxY + 1) - 1;
  grX := (maxX + 1) shl 3;
  grY := (maxY + 1) shl 3;
  imgEditor.Width := (maxX + 1)*24;
  imgEditor.Height := (maxY + 1)*modeHeight;
  imgEditor.Invalidate;
  statusBar.Panels[0].Text := 'Max X coord.: ' + IntToStr(maxX) +
                              ', max Y coord.: ' + IntToStr(maxY);
end;

{-----------------------------------------------------------------------------
 Fill screen with character
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.FillScreen(character : byte; isBackground : boolean);
var
  j : integer;
  m, r : byte;
begin
  if not isBackground then
    FillByte(fldAtascii, SizeOf(fldAtascii), character);

  r := 0; m := 0;
  for j := 0 to maxSize do begin
    if (j > maxX) and (j mod (maxX + 1) = 0) then begin
      r := 0;
      Inc(m, 2);
    end;

    if isBackground then begin
      if fldAtascii[j] = 0 then begin
        PutChar(r, m, fldAtascii[j]);
        fldAtascii[j] := character;
      end;
    end
    else
      PutChar(r, m, fldAtascii[j]);

    Inc(r);
  end;

  SetGrid(gridColor);

  //imgEditor.Invalidate;
  //imgEditor.Update;
  //imgEditor.Refresh;
end;

procedure TfrmAntic4.SetGrid(gColor : TColor);
begin
  isShowGrid := gColor <> colTab[0];
//  if isshowgrid then debug('is') else debug('not is');
  gridColor := gColor;
//  RefreshChar;
  RefreshData;
  RefreshChar;
//  debug('after SetGrid');
end;

procedure TfrmAntic4.ShowFontSet;
var
  n, cnt : byte;
  col, xf, yf : byte;
  offset, xoffset, yoffset : integer;
  mask : array[0..1] of byte;
begin
  FillRectEx(imgFontSet, coltab[0], 0, 0, imgFontSet.Width, imgFontSet.Height);
  FillRectEx(imgFontSetInv, coltab[0], 0, 0, imgFontSet.Width, imgFontSet.Height);

  offset := 0; yoffset := 0;
  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, charYOffset02);
    end;
    cnt := 0;
    xoffset := offset*18;
    for yf := 0 to chrY do
      for xf := 0 to chrX do begin
        Inc(cnt);
        col := fldFontSet[xf, n shl 3 + yf];
        mask[cnt - 1] := col;
        if cnt = 2 then begin
          // Antic mode 4 and 5 display
          if (mask[0] = 0) and (mask[1] = 1) then
            col := 1
          else if (mask[0] = 1) and (mask[1] = 0) then
            col := 2
          else if (mask[0] = 1) and (mask[1] = 1) then
            col := 3;

          FillRectEx(imgFontSet, coltab[col],
                     (xf - 1)*factX02 + xoffset, yf*factY02 + yoffset, factX02 shl 1, factY02);

          // Antic mode 4 and 5 inverse display
          if (mask[0] = 1) and (mask[1] = 1) then
            col := 10;

          FillRectEx(imgFontSetInv, coltab[col],
                     (xf - 1)*factX02 + xoffset, yf*factY02 + yoffset, factX02 shl 1, factY02);
          cnt := 0;
        end;
      end;

    Inc(offset);
  end;
end;

procedure TfrmAntic4.DefaultColorPaletteProc(Sender : TObject);
begin
  frmMain.LoadDefaultPalette;
  RefreshColors;
end;

procedure TfrmAntic4.LoadAntic2Screen(Sender : TObject);
begin
  frmMain.dlgOpen.Title := 'Open existing Antic mode 2 (text mode 0) screen file';
  frmMain.dlgOpen.Filter := 'Antic mode 2 (text mode 0) screen files' +
                            ' (*.gr0, *.an2, *.asc)|*.gr0;*.an2;*.asc|All files (*.*)|*.*';
  if frmMain.dlgOpen.Execute then begin
    filename := frmMain.dlgOpen.Filename;
    isAntic2 := true;
    OpenFile(filename);
  end;
end;

procedure TfrmAntic4.CopyCharProc(Sender : TObject);
begin
  CopyChar(true);
end;

procedure TfrmAntic4.CancelCopyCharProc(Sender : TObject);
begin
  CopyChar(false);
end;

procedure TfrmAntic4.MaskColor(x, y, offset : integer; isInverse : boolean);
var
  cnt : byte = 0;
  i, j, col : byte;
  mask : array[0..1] of byte;
begin
  for i := 0 to 7 do
    for j := 0 to 7 do begin
      Inc(cnt);
      col := fldFontSet[j, i + offset];
      mask[cnt - 1] := col;
      if cnt = 2 then begin
        if (mask[0] = 0) and (mask[1] = 1) then
          col := 1
        else if (mask[0] = 1) and (mask[1] = 0) then
          col := 2
        else if (mask[0] = 1) and (mask[1] = 1) then begin
          if isInverse then
            col := 10
          else
            col := 3;
        end;
        FillRectEx(imgEditor, colTab[col], (x + j - 1)*factX, (y + i)*factY, factX shl 1, factY);
        cnt := 0;
      end;
    end;
end;

{-----------------------------------------------------------------------------
 Draw character
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.Plot(xf, yf : integer);
var
  xx, yy : byte;
  offset, offset2, offs2 : integer;
  i, j : byte;
begin
  if xf > grX then xf := grX;
//  if xf mod 8 = 0 then Inc(xf, 8);

  for j := 0 to maxY do begin
    for i := 0 to maxX do
      if (xf >= i shl 3) and (xf < (i + 1) shl 3) then begin
        xf := i shl 3;
        xx := i;
        break;
      end;

    if (yf > j shl 3) and (yf <= (j + 1) shl 3) then begin
      yf := j shl 3;
      yy := j;
      break;
    end;
  end;

  if xx + yy*(maxX + 1) > maxSize then Exit;

  statusBar.Panels[1].Text := 'Cursor coordinates (x: ' +
                              inttostr(xx) + ', y: ' + inttostr(yy) + ')';
  case btn of
    mbLeft : offs2 := offs;
    mbRight: offs2 := 0;
  else
    exit;
  end;

  offset2 := offs2;
  offset := offset2 shl 3;
  if not isFontSetNormal then
    Inc(offset2, 128);

  fldAtascii[xx + yy*(maxX + 1)] := offset2;
  MaskColor(xf, yf, offset, not isFontSetNormal);

  // Refresh original set
  ShowFontSet02(offs);
end;

{-----------------------------------------------------------------------------
 Draw character on screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.PutChar(scrPos, y, offset : integer);
var
  dx, dy, xf, yf : integer;
  isInverse : boolean = false;
begin
//  exit;
  xf := scrPos shl 3;
  yf := y shl 2;

  for dy := 0 to maxY do
    for dx := 0 to maxX do
      if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
        xf := dx shl 3;
        break;
      end;

  if offset > 127 then begin
    isInverse := true;
    Dec(offset, 128);
  end;

  offset := offset shl 3;
  ///////// ddd,kjdfklsjdfskljdfsdfsdfs
  MaskColor(xf, yf, offset, isInverse);
end;

procedure TfrmAntic4.RefreshChar;
var
  xf, yf : integer;
  j : integer;
  m, r : byte;
begin
  FillRectEx(imgChar, coltabFont[0], 0, 0, imgChar.Width, imgChar.Height);

  for yf := 0 to chrY do
    for xf := 0 to chrX do begin
      fldFontSet[xf, yf + offs shl 3] := fldChar[xf, yf];
      FillRectEx(imgChar, colTabFont[fldChar[xf, yf]],
                 xf*chrFactX, yf*chrFactY, chrFactX, chrFactY);
    end;

  imgChar.Refresh;
  PlotChar(255, 255);

//  debug('step 3');

  r := 0; m := 0;
  for j := 0 to maxSize do begin
    if (j > maxX) and (j mod (maxX + 1) = 0) then begin
      r := 0;
      Inc(m, 2);
    end;

    if (fldAtascii[j] = offs) or (fldAtascii[j] = offs + 128) then
      PutChar(r, m, fldAtascii[j]);

    Inc(r);
  end;

//  debug('before RefreshCharX(offs);');

  RefreshCharX(offs);
//  debug('after RefreshCharX(offs);');
end;

procedure TfrmAntic4.RefreshCharX(offset : integer);
var
  col : byte;
  xf, yf : integer;
begin
  FillRectEx(imgChar, coltabFont[0], 0, 0, imgChar.Width, imgChar.Height);
  FillRectEx(imgCharAntic45, colTab[0], 0, 0, imgChar.Width, imgChar.Height);
  offset := offset shl 3;
  for yf := 0 to chrY do
    for xf := 0 to chrX do begin
      // Show pixel of selected character
      col := fldFontSetOrig[xf, yf + offset];
      if not isFontSetNormal then
        col := 1 - col;

      FillRectEx(imgCharOrig, coltabFont[col], xf shl 2, yf shl 2, 4, 4);

      // Show pixel of edited character
      col := fldFontSet[xf, yf + offset];
      fldChar[xf, yf] := col;
      FillRectEx(imgChar, coltabFont[col], xf*chrFactX, yf*chrFactY, chrFactX, chrFactX);

      if isShowGrid then begin
//        debug('gridColor <> colTab[0]');
        imgChar.Canvas.Pen.Color := gridColor;
        imgChar.Canvas.Rectangle(xf*chrFactX, yf*chrFactY,
                                 xf*chrFactX + (xf + 1)*chrFactX + 1,
                                 yf*chrFactY + (yf + 1)*chrFactY + 1);
      end;

      if (xf = 1) or (xf = 3) or (xf = 5) or (xf = 7) then begin
        if (fldChar[xf - 1, yf] = 0) and (fldChar[xf, yf] = 0) then
          col := 0
        else if (fldChar[xf - 1, yf] = 1) and (fldChar[xf, yf] = 0) then
          col := 2
        else if (fldChar[xf - 1, yf] = 0) and (fldChar[xf, yf] = 1) then
          col := 1
        else if (fldChar[xf - 1, yf] = 1) and (fldChar[xf, yf] = 1) then begin
          if isFontSetNormal then
            col := 3
          else
            col := 10;
        end;
        fldCharAntic45[xf - 1, yf] := colTab[col];
        fldCharAntic45[xf, yf] := colTab[col];
        FillRectEx(imgCharAntic45, colTab[col], (xf - 1)*chrFactX, yf*chrFactY,
                   (xf - 1)*chrFactX*2, chrFactY);
        if isShowGrid then begin
//          debug('isShowGrid');
          imgCharAntic45.Canvas.Pen.Color := gridColor
        end
        else begin
//          debug('not isShowGrid');
          imgCharAntic45.Canvas.Pen.Color := colTab[col];
        end;

        if xf = 1 then
          imgCharAntic45.Canvas.Rectangle(0, yf*chrFactY,
                                          chrFactX*2 + 1,
                                          yf*chrFactY + (yf + 1)*chrFactY + 1)
        else
          imgCharAntic45.Canvas.Rectangle((xf - 1)*chrFactX, yf*chrFactY,
                                          (xf - 1)*chrFactX + (xf - 1)*chrFactX + 1,
                                          yf*chrFactY + (yf + 1)*chrFactY + 1);
      end;
    end;

  imgChar.Refresh;
  imgCharOrig.Refresh;
end;

{-----------------------------------------------------------------------------
 Draw pixel inside character editor
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.PlotChar(xf, yf : byte);
var
  col, i, n : byte;
  bin : string[8];
  j : integer;
  binConv : string[20];
begin
  if xf + yf < 510 then begin
    case btn of
      mbLeft : col := 1;
      mbRight: col := 0;
    else
      exit;
    end;

    if xf > chrX then xf := chrX;
    fldChar[xf, yf] := col;
    fldFontset[xf, yf + offs shl 3] := col;
    FillRectEx(imgChar, colTabFont[col], xf*chrFactX, yf*chrFactY, chrFactX, chrFactY);

    // Antic mode 4 and 5 standard and inverse display
    if (xf = 1) or (xf = 3) or (xf = 5) or (xf = 7) then begin
      if (fldChar[xf - 1, yf] = 0) and (fldChar[xf, yf] = 0) then
        col := 0
      else if (fldChar[xf - 1, yf] = 1) and (fldChar[xf, yf] = 0) then
        col := 2
      else if (fldChar[xf - 1, yf] = 0) and (fldChar[xf, yf] = 1) then
        col := 1
      else if (fldChar[xf - 1, yf] = 1) and (fldChar[xf, yf] = 1) then begin
        if isFontSetNormal then
          col := 3
        else
          col := 10;
      end;
      fldCharAntic45[xf - 1, yf] := colTab[col];
      fldCharAntic45[xf, yf] := colTab[col];
      FillRectEx(imgCharAntic45, colTab[col], (xf - 1)*chrFactX, yf*chrFactY,
                 chrFactX*2, chrFactY);
    end
    else begin
      if (fldChar[xf, yf] = 0) and (fldChar[xf + 1, yf] = 0) then
        col := 0
      else if (fldChar[xf, yf] = 1) and (fldChar[xf + 1, yf] = 0) then
        col := 2
      else if (fldChar[xf, yf] = 0) and (fldChar[xf + 1, yf] = 1) then
        col := 1
      else if (fldChar[xf, yf] = 1) and (fldChar[xf + 1, yf] = 1) then begin
        if isFontSetNormal then
          col := 3
        else
          col := 10;
      end;
      fldCharAntic45[xf, yf] := colTab[col];
      fldCharAntic45[xf + 1, yf] := colTab[col];
      FillRectEx(imgCharAntic45, colTab[col], xf*chrFactX, yf*chrFactY, chrFactX*2, chrFactY);
    end;
  end;

  // Show character data information
  for j := 0 to 7 do begin
    bin := '';
    for i := 0 to 7 do
      bin += IntToStr(fldChar[i, j]);

    binConv := IntToStr(bin2dec(bin)) + ' $' + Dec2Hex(bin2dec(bin));
    for n := 0 to 7 do
      case j of
        0 : begin
          lblNum0.Caption := binConv;
          lblNum8.Caption := lblNum0.Caption;
        end;
        1 : begin
          lblNum1.Caption := binConv;
          lblNum9.Caption := lblNum1.Caption;
        end;
        2 : begin
          lblNum2.Caption := binConv;
          lblNum10.Caption := lblNum2.Caption;
        end;
        3 : begin
          lblNum3.Caption := binConv;
          lblNum11.Caption := lblNum3.Caption;
        end;
        4 : begin
          lblNum4.Caption := binConv;
          lblNum12.Caption := lblNum4.Caption;
        end;
        5 : begin
          lblNum5.Caption := binConv;
          lblNum13.Caption := lblNum5.Caption;
        end;
        6 : begin
          lblNum6.Caption := binConv;
          lblNum14.Caption := lblNum6.Caption;
        end;
        7 : begin
          lblNum7.Caption := binConv;
          lblNum15.Caption := lblNum7.Caption;
        end;
      end;
  end;

  // Refresh original set
  ShowFontSet02(offs);

  // Reset mouse button click
  btn := mbMiddle;
end;

{-----------------------------------------------------------------------------
 Draw Antic 4/5 pixel inside character editor
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.PlotCharAntic45(xf, yf : byte);
var
  col : byte;
begin
  case btn of
    mbLeft : col := 1;
    mbRight: col := 0;
  else
    exit;
  end;

  if xf > chrX then xf := chrX;
  fldCharAntic45[xf, yf] := frmColors.SelColor;

  case xf of
    0, 2, 4, 6: begin
      if col = 0 then begin
        fldCharAntic45[xf + 1, yf] := 0;
        FillRectEx(imgCharAntic45, colTab[0], xf*chrFactX, yf*chrFactY, chrFactX*2, chrFactY);
        fldChar[xf, yf] := 0;
        fldChar[xf + 1, yf] := 0;
      end
      else begin
        fldCharAntic45[xf + 1, yf] := frmColors.SelColor;
        FillRectEx(imgCharAntic45, colTab[frmColors.SelColor],
                   xf*chrFactX, yf*chrFactY, chrFactX*2, chrFactY);
        case frmColors.SelColor of
          0: begin
            fldChar[xf, yf] := 0; fldChar[xf + 1, yf] := 0;
          end;
          1: begin
            fldChar[xf, yf] := 0; fldChar[xf + 1, yf] := 1;
          end;
          2: begin
            fldChar[xf, yf] := 1; fldChar[xf + 1, yf] := 0;
          end;
          3: begin
            fldChar[xf, yf] := 1; fldChar[xf + 1, yf] := 1;
          end;
        end;
      end;
    end;
    1, 3, 5, 7: begin
      if col = 0 then begin
        fldCharAntic45[xf - 1, yf] := 0;
        FillRectEx(imgCharAntic45, colTab[0],
                   (xf - 1)*chrFactX, yf*chrFactY, chrFactX*2, chrFactY);
        fldChar[xf - 1, yf] := 0;
        fldChar[xf, yf] := 0;
      end
      else begin
        fldCharAntic45[xf - 1, yf] := frmColors.SelColor;
        FillRectEx(imgCharAntic45, colTab[frmColors.SelColor],
                   (xf - 1)*chrFactX, yf*chrFactY, chrFactX*2, chrFactY);
        case frmColors.SelColor of
          0: begin
            fldChar[xf - 1, yf] := 0; fldChar[xf, yf] := 0;
          end;
          1: begin
            fldChar[xf - 1, yf] := 0; fldChar[xf, yf] := 1;
          end;
          2: begin
            fldChar[xf - 1, yf] := 1; fldChar[xf, yf] := 0;
          end;
          3: begin
            fldChar[xf - 1, yf] := 1; fldChar[xf, yf] := 1;
          end;
        end;
      end;
    end;
  end;

  RefreshChar;
end;

procedure TfrmAntic4.ShowFontSet02(offset : byte);
var
  cnt : byte;
  col, xf, yf: byte;
  xoffset, yoffset : integer;
  mask : array[0..1] of byte;
begin
  yoffset := offsY*charYOffset02;

  if offset < 16 then
    xoffset := offset*18;

  for cnt := 1 to 7 do
    if (offset >= cnt shl 4) and (offset < charXOffset + (cnt - 1) shl 4) then begin
      xoffset := (offset - cnt shl 4)*18;
      break;
    end;

  cnt := 0;
  for yf := 0 to chrY do
    for xf := 0 to chrX do begin
      Inc(cnt);
      col := fldFontSet[xf, offset shl 3 + yf];
      mask[cnt - 1] := col;
      if cnt = 2 then begin
        // Antic mode 4 and 5 display
        if (mask[0] = 0) and (mask[1] = 1) then
          col := 1
        else if (mask[0] = 1) and (mask[1] = 0) then
          col := 2
        else if (mask[0] = 1) and (mask[1] = 1) then
          col := 3;

        FillRectEx(imgFontSet, coltab[col],
                   (xf - 1)*factX02 + xoffset, yf*factY02 + yoffset, factX02 shl 1, factY02);

        // Antic mode 4 and 5 inverse display
        if (mask[0] = 1) and (mask[1] = 1) then
          col := 10;

        FillRectEx(imgFontSetInv, coltab[col],
                   (xf - 1)*factX02 + xoffset, yf*factY02 + yoffset, factX02 shl 1, factY02);
        cnt := 0;
      end;
    end;
end;

procedure TfrmAntic4.ShowBaseFontSet;
var
  n : byte;
  col, xf, yf : byte;
  offset, xoffset, yoffset : integer;
begin
  FillRectEx(imgBaseFontSet, coltabFont[1], 0, 0, imgBaseFontSet.Width, imgBaseFontSet.Height);
  FillRectEx(imgBaseFontSetInv, coltabFont[1], 0, 0, imgBaseFontSet.Width, imgBaseFontSet.Height);
  offset := 0; yoffset := 0;
  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, 17);
    end;
    xoffset := offset*18;
    for yf := 0 to chrY do
      for xf := 0 to chrX do begin
        // Standard characters
        col := fldFontSet[xf, n shl 3 + yf];
        FillRectEx(imgBaseFontSet, coltabFont[col],
                   xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
        // Inverse characters
        col := 1 - col;
        FillRectEx(imgBaseFontSetInv, coltabFont[col],
                   xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
      end;

    Inc(offset);
  end;
end;

{-----------------------------------------------------------------------------
 Mirror character horizontally
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.FlipXProc(Sender: TObject);
var
  x, y, n : byte;
begin
  for y := 0 to chrY do
    for x := chrX downto 4 do begin
      n := fldChar[x, y];
      fldChar[x, y] := fldChar[7 - x, y];
      fldChar[7 - x, y] := n;
    end;

  RefreshChar;
//  RefreshCharX(offs);
end;

{-----------------------------------------------------------------------------
 Mirror character vertically
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.FlipYProc(Sender: TObject);
var
  x, y, n : byte;
begin
  for x := 0 to chrX do
    for y := chrY downto 4 do begin
      n := fldChar[x, y];
      fldChar[x, y] := fldChar[x, 7 - y];
      fldChar[x, 7 - y] := n;
    end;

  RefreshChar;
//  RefreshCharX(offs);
end;

{-----------------------------------------------------------------------------
 Rotate character
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.RotateProc(Sender: TObject);
var
  x, y, n : byte;
  arr : charType;
begin
  for y := 0 to chrY do
    for x := 0 to chrX do
      arr[y, x] := fldChar[x, y];

  for x := 0 to chrX do
    for n := 0 to chrY do
      fldChar[7 - n, x] := arr[n, x];

  RefreshChar;
//  RefreshCharX(offs);
end;

{-----------------------------------------------------------------------------
 Shift character
 -----------------------------------------------------------------------------}

// Shift character left
procedure TfrmAntic4.ShiftLeft(Sender: TObject);
var
  x, y, n : byte;
begin
  for y := 0 to chrY do begin
    n := fldChar[0, y];
    for x := 1 to chrX do
      fldChar[x - 1, y] := fldChar[x, y];

    fldChar[7, y] := n;
  end;

  RefreshChar;
end;

// Shift character right
procedure TfrmAntic4.ShiftRightProc(Sender: TObject);
var
  x, y, n : byte;
begin
  for y := 0 to chrY do begin
    n := fldChar[7, y];
    for x := chrX - 1 downto 0 do
      fldChar[x + 1, y] := fldChar[x, y];

    fldChar[0, y] := n;
  end;

  RefreshChar;
end;

// Shift character up
procedure TfrmAntic4.ShiftUpProc(Sender: TObject);
var
  x, y : byte;
  fld02 : array[0..7] of byte;
begin
  for x := 0 to chrY do
    fld02[x] := fldChar[x, 0];

  for x := 0 to chrX do
    for y := 1 to chrY do begin
      fldChar[x, y - 1] := fldChar[x, y];
      fldChar[x, y] := 0;
    end;

  for x := 0 to 7 do
    fldChar[x, chrY] := fld02[x];

  RefreshChar;
end;

// Shift character down
procedure TfrmAntic4.ShiftDownProc(Sender: TObject);
var
  x, y : byte;
  fld02 : array[0..7] of byte;
begin
  for x := 0 to 7 do
    fld02[x] := fldChar[x, chrY];

  for x := 0 to chrX do
    for y := chrY - 1 downto 0 do begin
      fldChar[x, y + 1] := fldChar[x, y];
      fldChar[x, y] := 0;
    end;

  for x := 0 to chrX do
    fldChar[x, 0] := fld02[x];

  RefreshChar;
end;

{-----------------------------------------------------------------------------
 Move character
 -----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------
 Move character left
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.MoveLeftProc(Sender: TObject);
begin
  MoveLeft(chrX, chrY, fldChar);
  RefreshChar;
end;

{-----------------------------------------------------------------------------
 Move character right
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.MoveRightProc(Sender: TObject);
begin
  MoveRight(chrX, chrY, fldChar);
  RefreshChar;
end;

{-----------------------------------------------------------------------------
 Move character up
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.MoveUpProc(Sender: TObject);
begin
  MoveUp(chrX, chrY, fldChar);
  RefreshChar;
end;

{-----------------------------------------------------------------------------
 Move character down
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.MoveDownProc(Sender: TObject);
begin
  MoveDown(chrX, chrY, fldChar);
  RefreshChar;
end;

{-----------------------------------------------------------------------------
 Clear character
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.ClearCharProc(Sender: TObject);
begin
  FillByte(fldChar, SizeOf(fldChar), 0);
  RefreshChar;
//  RefreshCharX(offs);
end;

{-----------------------------------------------------------------------------
 Refresh screen data
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.RefreshData;
var
  i, x, y : word;
  xx, yy : word;
begin
  FillRectEx(imgChar, coltabFont[0], 0, 0, imgChar.Width, imgChar.Height);
  FillRectEx(imgCharAntic45, colTab[0], 0, 0, imgChar.Width, imgChar.Height);
//  imgEditor.Canvas.Clear;
  x := 0; y := 0;
  for i := 0 to maxSize do begin
    if (i > maxX) and (i mod (maxX + 1) = 0) then begin
      x := 0;
      Inc(y, 2);
    end;
    PutChar(x, y, fldAtascii[i]);

    // Draw a grid
    imgEditor.Canvas.Pen.Color := gridColor;
    xx := x*24;
    yy := y*modeHeight shr 1;
//    if gridColor <> colTab[0] then begin
    if isShowGrid then begin
//      debug('RefreshData isShowGrid');
      imgEditor.Canvas.Line(xx, yy, xx + 24, yy);
      imgEditor.Canvas.Line(xx, yy + modeHeight, xx + 24, yy + modeHeight);
      imgEditor.Canvas.Line(xx, yy, xx, yy + modeHeight);
      imgEditor.Canvas.Line(xx + 24, yy, xx + 24, yy + modeHeight);
    end;
    Inc(x);
  end;

  //imgEditor.Invalidate;
  //imgEditor.Update;
  //imgEditor.Refresh;
  ShowFontSet;
  //if isShowGrid then begin
  //    debug('RefreshData isShowGrid');
  //end;
end;

procedure TfrmAntic4.RefreshColors;
begin
  RefreshData;
  ColorRegisters;
end;

{-----------------------------------------------------------------------------
 Color registers
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.ColorRegisters;
begin
  FillRectEx(color4, coltab[0], 0, 0, color0.width, color0.Height);
  FillRectEx(color0, coltab[1], 0, 0, color0.width, color0.Height);
  FillRectEx(color1, coltab[2], 0, 0, color0.width, color0.Height);

  if isFontSetNormal then
    FillRectEx(color2, coltab[3], 0, 0, color0.width, color0.Height)
  else
    FillRectEx(color2, coltab[10], 0, 0, color0.width, color0.Height);
end;

procedure TfrmAntic4.editYMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  btn := mbMiddle;
  if not isCreate then
    SetXY(editX.Value, editY.Value);
end;

procedure TfrmAntic4.MaxSizeProc(Sender : TObject);
begin
  if chkMaxSize.Checked then begin
    editX.Value := 39;
    editY.Value := 23;
    EditDimProc(Sender);
  end;
end;

{-----------------------------------------------------------------------------
 Copy character data to selected character set cell
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.CopyChar(isCopy : boolean);
begin
  isCopyChar := isCopy;
  if iscopy then begin
    statusBar.Panels[3].Text := 'Copy drawing character to selected character set cell.' +
                                ' Press ''Esc'' key to end the operation!';
    ShowCursor(frmAntic4, frmAntic4, crDrag);
  end
  else begin
    statusBar.Panels[3].Text := '';
    ShowCursor(frmAntic4, frmAntic4, crDefault);
  end;
end;

{-----------------------------------------------------------------------------
 Restore default character data
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.RestoreCharProc(Sender : TObject);
var
  xf, yf, col : byte;
begin
  FillRectEx(imgChar, colTab[0], 0, 0, imgChar.Width, imgChar.Height);
  for yf := 0 to chrY do
    for xf := 0 to chrX do begin
      col := fldFontSetOrig[xf, yf + offs shl 3];
      fldFontSet[xf, yf + offs shl 3] := col;
      FillRectEx(imgChar, coltabFont[col], xf*chrFactX, yf*chrFactY, chrFactX, chrFactX);
    end;

  PlotChar(255, 255);
  RefreshCharX(offs);
  RefreshChar;
end;

{-----------------------------------------------------------------------------
 Show character information
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.CharInfo(offset : integer);
var
  n : byte;
begin
  if offset < 0 then begin
    lblCharInfo01.Caption := '';
    lblCharInfo02.Caption := '';
    lblCharInfo03.Caption := '';
  end
  else begin
    n := offset;
    if (offset >= 0) and (offset <= 63) then
      n += 32
    else if (offset >= 64) and (offset <= 95) then
      n -= 64;

    lblCharInfo01.Caption := 'Internal code Dec: ' + IntToStr(offset) + ' Hex: ' + Dec2hex(offset);
    lblCharInfo02.Caption := 'Atascii code Dec: ' + IntToStr(n) + ' Hex: ' + Dec2hex(n);
    lblCharInfo03.Caption := 'Atascii inverse Dec: ' + IntToStr(n + 128) +
                             ' Hex: ' + Dec2hex(n + 128);
  end;
end;

procedure TfrmAntic4.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

