{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Antic mode 4 tile editor
}
unit antic4_tiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StrUtils,
  ExtCtrls, Menus, ComCtrls, StdCtrls, Buttons, BCTrackbarUpdown,
  common;

type
  { TfrmAntic4Tiles }
  TfrmAntic4Tiles = class(TForm)
    boxResize : TGroupBox;
    btnCharDown : TSpeedButton;
    btnCharLeft : TSpeedButton;
    btnCharRight : TSpeedButton;
    btnCharUp : TSpeedButton;
    btnClearTile : TToolButton;
    btnClearTiles : TToolButton;
    btnGenCode : TToolButton;
    btnFillTile : TToolButton;
    btnFlipX : TToolButton;
    btnFlipY : TToolButton;
    btnLoadScreen : TToolButton;
    btnRotate : TToolButton;
    btnSaveScreen : TToolButton;
    btnDraw : TToolButton;
    btnSettings : TToolButton;
    btnViewer : TToolButton;
    cmbAnticMode : TComboBox;
    color0 : TImage;
    color1 : TImage;
    color2 : TImage;
    color3 : TImage;
    color4 : TImage;
    editX : TBCTrackbarUpdown;
    editY : TBCTrackbarUpdown;
    boxCharOper : TGroupBox;
    imgChar0000 : TImage;
    imgChar0100 : TImage;
    imgChar0010 : TImage;
    imgChar0001 : TImage;
    imgChar1000 : TImage;
    imgChar1100 : TImage;
    imgChar1010 : TImage;
    imgChar1001 : TImage;
    imgChar2000 : TImage;
    imgChar2100 : TImage;
    imgChar2010 : TImage;
    imgChar2001 : TImage;
    imgChar3000 : TImage;
    imgChar3100 : TImage;
    imgChar3010 : TImage;
    imgChar3001 : TImage;
    imgChar4000 : TImage;
    imgChar4100 : TImage;
    imgChar4010 : TImage;
    imgChar4001 : TImage;
    imgTile01Char0000 : TImage;
    imgTile02Char0000 : TImage;
    imgTile02Char1 : TImage;
    imgTile02Char1000 : TImage;
    imgTile02Char1001 : TImage;
    imgTile02Char0100 : TImage;
    imgTile03Char1000 : TImage;
    imgTile03Char1001 : TImage;
    imgTile03Char0100 : TImage;
    imgTile02Char1010 : TImage;
    imgTile02Char0010 : TImage;
    imgTile03Char0000 : TImage;
    imgTile03Char1010 : TImage;
    imgTile03Char0010 : TImage;
    imgTile02Char1100 : TImage;
    imgTile02Char0001 : TImage;
    imgTile03Char1100 : TImage;
    imgTile03Char0001 : TImage;
    imgTile02Char2000 : TImage;
    imgTile02Char2001 : TImage;
    imgTile03Char2000 : TImage;
    imgTile03Char2001 : TImage;
    imgTile02Char2010 : TImage;
    imgTile01Char2100 : TImage;
    imgTile01Char2010 : TImage;
    imgTile01Char2001 : TImage;
    imgTile03Char2010 : TImage;
    imgTile02Char2100 : TImage;
    imgTile01Char3000 : TImage;
    imgTile01Char3002 : TImage;
    imgTile01Char3003 : TImage;
    imgTile01Char3011 : TImage;
    imgTile01Char3100 : TImage;
    imgTile01Char3010 : TImage;
    imgTile01Char3001 : TImage;
    imgTile01Char3101 : TImage;
    imgTile01Char4000 : TImage;
    imgTile03Char2100 : TImage;
    imgTile02Char3000 : TImage;
    imgTile02Char3001 : TImage;
    imgTile03Char3000 : TImage;
    imgTile03Char3001 : TImage;
    imgTile03Char3010 : TImage;
    imgTile03Char3100 : TImage;
    imgTile02Char4000 : TImage;
    imgTile02Char4001 : TImage;
    imgTile02Char3010 : TImage;
    imgTile03Char4000 : TImage;
    imgTile03Char4001 : TImage;
    imgTile02Char4010 : TImage;
    imgTile01Char4100 : TImage;
    imgTile01Char4010 : TImage;
    imgTile01Char0100 : TImage;
    imgTile01Char4001 : TImage;
    imgTile01Char0010 : TImage;
    imgTile01Char0001 : TImage;
    imgTile01Char1000 : TImage;
    imgTile01Char1100 : TImage;
    imgTile01Char1010 : TImage;
    imgTile01Char1001 : TImage;
    imgTile01Char2000 : TImage;
    imgTile01 : TImage;
    imgTile02Char3100 : TImage;
    imgTile03Char4010 : TImage;
    imgTile02Char4100 : TImage;
    imgTile02 : TImage;
    imgTile03Char4100 : TImage;
    imgTile03 : TImage;
    imgTile04 : TImage;
    imgTile05 : TImage;
    imgTile06 : TImage;
    imgTile07 : TImage;
    imgTile08 : TImage;
    itemClose : TMenuItem;
    itemColorPalette : TMenuItem;
    itemDefaultColorPalette : TMenuItem;
    itemHideGrid : TMenuItem;
    itemShowGrid : TMenuItem;
    itemViewer : TMenuItem;
    lblTiles01Dim : TLabel;
    lblTiles02Dim : TLabel;
    lblTiles03Dim : TLabel;
    lblTiles08Dim : TLabel;
    lblTiles07Dim : TLabel;
    lblTiles06Dim : TLabel;
    lblTiles05Dim : TLabel;
    lblTiles04Dim : TLabel;
    Label21 : TLabel;
    Label22 : TLabel;
    menuAntic4Tiles : TMainMenu;
    itemUndo : TMenuItem;
    MenuItem2 : TMenuItem;
    itemRedo : TMenuItem;
    popClearTile : TMenuItem;
    menuEdit : TMenuItem;
    menuFile : TMenuItem;
    popInvertTile : TMenuItem;
    itemInvertTile : TMenuItem;
    itemFillTile : TMenuItem;
    popDraw : TMenuItem;
    popFillTile : TMenuItem;
    popRotateTile : TMenuItem;
    popDelim01 : TMenuItem;
    itemClearTile : TMenuItem;
    MenuItem16 : TMenuItem;
    MenuItem17 : TMenuItem;
    MenuItem18 : TMenuItem;
    popDelim02 : TMenuItem;
    itemCodeGen : TMenuItem;
    itemClearTiles : TMenuItem;
    itemFlipTileX : TMenuItem;
    itemFlipTileY : TMenuItem;
    itemRotateTile : TMenuItem;
    popFlipTileX : TMenuItem;
    MenuItem4 : TMenuItem;
    popFlipTileY : TMenuItem;
    itemOpenTile : TMenuItem;
    itemSaveTile : TMenuItem;
    itemSaveTileAs : TMenuItem;
    menuTools : TMenuItem;
    menuView : TMenuItem;
    paintBox : TPaintBox;
    popCancelCopyChar : TMenuItem;
    popCopyChar : TMenuItem;
    popHideGrid : TMenuItem;
    popMenu : TPopupMenu;
    popShowGrid : TMenuItem;
    radMoveChar : TRadioButton;
    radShiftChar : TRadioButton;
    scrollBox : TScrollBox;
    shape : TShape;
    shapeTileGrid : TShape;
    shapeEditor : TShape;
    statusBar : TStatusBar;
    toolBar : TToolBar;
    btnSelectTile : TToolButton;
    btnUndo : TToolButton;
    btnRedo : TToolButton;
    ToolButton17 : TToolButton;
    ToolButton18 : TToolButton;
    ToolButton2 : TToolButton;
    btnInvert : TToolButton;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure btnFlipXClick(Sender : TObject);
    procedure ClearTileProc(Sender : TObject);
    procedure CharOper(Sender : TObject);
    procedure ColorProc(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure FlipXProc(Sender : TObject);
    procedure FlipYProc(Sender : TObject);
    procedure ColorPaletteProc(Sender : TObject);
    procedure CharSetProc(Sender : TObject);
    procedure SelectTileProc(Sender : TObject);
    procedure UndoProc(Sender : TObject);
    procedure RedoProc(Sender : TObject);
    procedure ViewerProc(Sender : TObject);
    procedure ShowGridProc(Sender : TObject);
    procedure LoadDefaultColorsProc(Sender : TObject);
    procedure paintBoxDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure paintBoxPaint(Sender : TObject);
    procedure RotateProc(Sender : TObject);
    procedure InvertProc(Sender : TObject);
    procedure GenCodeProc(Sender : TObject);
    procedure ClearTilesProc(Sender : TObject);
    procedure FillTileProc(Sender : TObject);
    procedure editXLeave(Sender : TObject);
    procedure editXUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure CloseWinProc(Sender : TObject);
    procedure LoadTileProc(Sender : TObject);
    procedure SaveTileAsProc(Sender : TObject);
    procedure SaveTileProc(Sender : TObject);
    procedure SaveTile;
    procedure imgCharDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure imgCharUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure ShiftDownProc(Sender: TObject);
    procedure ShiftLeftProc(Sender: TObject);
    procedure ShiftRightProc(Sender: TObject);
    procedure ShiftUpProc(Sender: TObject);
    procedure MoveDownProc(Sender: TObject);
    procedure MoveLeftProc(Sender: TObject);
    procedure MoveRightProc(Sender: TObject);
    procedure MoveUpProc(Sender: TObject);
  private
    btn : TMousebutton;
    isSaveAs : boolean;
    isFontSetNormal : boolean;
    isCreate : boolean;
    isShow : boolean;
    isCopyChar : boolean;
    isShowGrid : boolean;
    isSelectTile : boolean;
    mouseIsDown: boolean;
    coordX, coordY : integer;
    imageObject : TObject;
    fld02 : array[0..7] of byte;
    undoValues : array[0..2] of TAntic4CharValueType;
//    undoValuesEx : TAntic4CharValueExType;
    undoValuesEx : array[0..19, 0..7, 0..7] of byte;
    redoValuesEx : array[0..19, 0..7, 0..7] of byte;
    procedure SetXY(_maxX, _maxY : byte);
    procedure SetAnticMode(mode : byte);
    procedure PlotCharAntic45(Sender : TObject; xf, yf : byte);
    procedure ColorRegisters;
    procedure OpenFile(filename : string);
    procedure MaskColor(image : TImage; index : byte);
    procedure RefreshChar(image : TImage; index, selColor : byte);
    function CheckInverse(index : byte) : boolean;
    procedure SetCell(image : TImage; isActive : boolean);
    procedure SetCells;
    procedure RefreshTiles(index : byte);
    procedure PutChar(scrPos, y, offset : byte);
    procedure ShowTilesDim;
    procedure RefreshCharX(imgChar : TImage; offset : integer);
    procedure ShowTileChars;
  public
    filename : string;
    fontName : string;
    fldFontSet : fldFontSetType;
    fldChar : array[0..19, 0..7, 0..7] of byte;
//    fldCharMem : array[0..2, 0..19, 0..7, 0..7] of byte;
    fldCharAntic45 : array[0..19, 0..7, 0..7] of byte;
    fldCharAntic45Ex : array[1.._MAX_TILES, 0..19, 0..7, 0..7] of byte;
    inverseIndex : array[0..19] of boolean;
    cellIndex : array[0..19] of boolean;
    charColor : array[0..19] of byte;
    factX, factY : byte;        // Character screen editor offset
    chrFactX, chrFactY : byte;  // Character editor pixel offsets
//    factX02, factY02 : byte;    // Font set character offsets
    maxXX, maxYY : byte;        // Maximum X and Y coordinates
    maxSize : integer;
    anticMode : byte;
    modeHeight : byte;
    isTextModeChanged : boolean;
    selectTile : byte;   // Selected tile part
//    selectTiles : byte;  // Selected tiles group
//    charXOffset, charYOffset, charYOffset02 : word;
    antic4Tile : TAntic4CharType;
    antic4TileArray : array[1.._MAX_TILES] of TAntic4CharType;
    procedure RefreshData;
    procedure RefreshGrid;
    procedure UpdateColors;
  end;

var
  frmAntic4Tiles : TfrmAntic4Tiles;

implementation

{$R *.lfm}

uses
  main, lib, colors, char_set;

{ TfrmAntic4Tiles }

procedure TfrmAntic4Tiles.FormCreate(Sender : TObject);
begin
  DoubleBuffered := true;
  btn := mbMiddle;
  isCreate := true;
  isShow := true;
  isCopyChar := false;
  isShowGrid := true;
  isSelectTile := false;
//  FillByte(charEditIndex, SizeOf(charEditIndex), 0);
  FillByte(fldChar, SizeOf(fldChar), 0);
  FillByte(fldCharAntic45, SizeOf(fldCharAntic45), 0);
  FillByte(fldCharAntic45Ex, SizeOf(fldCharAntic45Ex), 0);
//  debug('SizeOf', SizeOf(charEditIndex), SizeOf(fldChar), SizeOf(fldCharAntic45));
  SetTrackBarUpDown(editX, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editY, $00DDDDDD, clWhite);
end;

procedure TfrmAntic4Tiles.FormShow(Sender : TObject);
var
  i : byte;
begin
  propFlagModules[12] := 1;
  isChange := true;
  frmMain.Top := 10;
  formId := formAntic4TileEditor;
  filename := getDir + 'examples\tile01.tl4';
  caption := programName + ' ' + programVersion +
             ' - Antic mode 4 tile editor (' + filename + ')';

  modeHeight := 24;
  SetXY(4, 5);
  editX.Value := antic4Tile.dimX;
  editY.Value := antic4Tile.dimY;
  SetAnticMode(4);
  factX := 2;
//  factX02 := 2;

  maxXX := 4;
  maxYY := 5;

  // Character editor parameters
  chrFactX := 14; chrFactY := 14;

//  PaintBox.Canvas.Pen.Width:=2; // or any width, the settings will respect
//  PaintBox.Canvas.Pen.Color:=clBlack;
////  PaintBox.Canvas.Pen.JoinStyle:=pjsMiter;// remove rounded borders
////  PaintBox.Canvas.Pen.Style:=psInsideframe; // draw border inside rectangle
//  PaintBox.Canvas.Rectangle(0, 0, PaintBox.Width, PaintBox.Height);

//PaintBox.Canvas.Brush.Style := bsClear;
//PaintBox.Canvas.Pen.Color := clRed;
//PaintBox.Canvas.Rectangle(PaintBox.ClientRect);
//PaintBox.Invalidate;

  isFontSetNormal := true;
  DefaultFontSet(fldFontSet);
//  ShowBaseFontSet;

  btnDraw.Down := true;

  FillRectEx(imgTile01, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
  FillRectEx(imgTile02, coltab[0], 0, 0, imgTile02.Width, imgTile02.Height);
  FillRectEx(imgTile03, coltab[0], 0, 0, imgTile03.Width, imgTile03.Height);
  FillRectEx(imgTile04, coltab[0], 0, 0, imgTile04.Width, imgTile04.Height);
  FillRectEx(imgTile05, coltab[0], 0, 0, imgTile05.Width, imgTile05.Height);
  FillRectEx(imgTile06, coltab[0], 0, 0, imgTile06.Width, imgTile06.Height);
  FillRectEx(imgTile07, coltab[0], 0, 0, imgTile07.Width, imgTile07.Height);
  FillRectEx(imgTile08, coltab[0], 0, 0, imgTile08.Width, imgTile08.Height);

//  SetCell(imgChar0000, true);
//  SetCell(imgChar0100, true);

  //cellIndex[0] := true;
  //cellIndex[1] := true;
//  for i := 0 to SizeOf(cellIndex) - 1 do
//    cellIndex[i] := true;

  shapeEditor.Brush.Color := colTab[0];

  selectTile := 0;

  //  selectTiles := 1;
  antic4Tile.selected := 1;
  for i := 1 to _MAX_TILES do begin
    antic4TileArray[i].dimX := 4;
    antic4TileArray[i].dimY := 5;
  end;

  isCreate := false;
  editXLeave(Sender);

  //for i := 0 to 15 do
  //  (FindComponent('lblNum' + IntToStr(i)) as TLabel).Caption := '0';

  //gridColor := colTab[0];
//  FillScreen(0, false);
//  FillByte(fldAtascii, SizeOf(fldAtascii), 97);

//  offs := 1;
  isFontSetNormal := true;
//  RefreshCharX(offs);
//  PlotChar(255, 255);
  ColorRegisters;

//  ShowTilesDim;
  SelectTileProc(imgTile01);

  //fldAtascii[4 + 6*(maxX + 1)] := 97;
  //fldAtascii[5 + 6*(maxX + 1)] := 97;

  ShowTileChars;

  imgTile01Char4001.Canvas.Pen.Color := clBlack;
  imgTile01Char4001.Canvas.Pen.Width := 1;
end;

procedure TfrmAntic4Tiles.SetCell(image : TImage; isActive : boolean);
begin
  image.Enabled := isActive;
//  image.Visible := isActive;
  if isActive then
    FillRectEx(image, colTab[0], 0, 0, image.Width, image.Height)
  else
    FillRectEx(image, clGray, 0, 0, image.Width, image.Height);
end;

{-----------------------------------------------------------------------------
 Load Antic 4/5 tile from file - actual reading data
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.OpenFile(filename : string);
var
  fs : TFileStream;
  x, y, i, j, r, index : byte;
  bin : string;
begin
  ShowCursor(frmAntic4Tiles, frmAntic4Tiles, crHourGlass);

  filename := LowerCase(filename);
//  cmbAnticMode.ItemIndex := 0;
  SetAnticMode(4);

  fs := TFileStream.Create(filename, fmOpenReadWrite);
  try
    // Dimension x, y
//    maxX := fs.ReadByte;
//    maxY := fs.ReadByte;
//    debug('maxx, maxy', maxx, maxy);
    antic4Tile.dimX := fs.ReadByte;
    antic4Tile.dimY := fs.ReadByte;
    antic4TileArray[antic4Tile.selected].dimX := antic4Tile.dimX;
    antic4TileArray[antic4Tile.selected].dimY := antic4Tile.dimY;

    SetXY(antic4Tile.dimX, antic4Tile.dimY);
    editX.Value := antic4Tile.dimX;
    editY.Value := antic4Tile.dimY;
    editX.Invalidate;
    editY.Invalidate;
    FillByte(fldCharAntic45, SizeOf(fldCharAntic45), 0);

    // Read data
//    for j := 0 to maxSize do
//      if fs.Position < fs.Size then
//        fldAtascii[j] := fs.ReadByte;

    //for y := 0 to maxY - 1 do
    //  for x := 0 to maxX - 1 do begin
    //    for j := 0 to 7 do
    //      for i := 0 to 7 do
    //        fldCharAntic45[cnt, i, j] := fs.ReadByte;
    //  end;

    for y := 0 to antic4Tile.dimY - 1 do
      for x := 0 to antic4Tile.dimX - 1 do begin
        index := x + y shl 2;
//        debug('index', index);
        for i := 0 to _CHAR_DIM do
          if fs.Position < fs.Size then begin
            r := fs.ReadByte;
            bin := IntToBin(r, 8);
//            debug(bin + ', index: ', index);
            for j := 0 to _CHAR_DIM do
              fldChar[index, j, i] := StrToInt(bin[j + 1]);
          end;

        r := fs.ReadByte;
        if r = 0 then
          inverseIndex[index] := false
        else
          inverseIndex[index] := true;
      end;
  finally
    fs.Free;
    RefreshGrid;
    UpdateColors;
    editXLeave(Application);
    caption := programName + ' ' + programVersion +
               ' - Antic mode 4/5 tile editor (' + filename + ')';
    ShowCursor(frmAntic4Tiles, frmAntic4Tiles, crDefault);
  end;
end;

procedure TfrmAntic4Tiles.LoadTileProc(Sender : TObject);
begin
  frmMain.dlgOpen.Title := 'Open existing Antic mode 4/5 tile file';
  frmMain.dlgOpen.Filter := 'Antic mode 4/5 tile files' +
                            ' (*.tl4)|*.tl4|All files (*.*)|*.*';
  if filename <> '' then
    frmMain.dlgOpen.Filename := filename;

  if frmMain.dlgOpen.Execute then begin
    filename := frmMain.dlgOpen.Filename;
    OpenFile(filename);
  end;
end;

procedure TfrmAntic4Tiles.SaveTileProc(Sender : TObject);
begin
  SaveTile;
end;

procedure TfrmAntic4Tiles.SaveTileAsProc(Sender : TObject);
begin
  isSaveAs := true;
  SaveTile;
end;

{-----------------------------------------------------------------------------
 Save Antic mode 4/5 tile
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.SaveTile;
var
  fs : TFileStream;
  isOk : boolean = false;

procedure SaveData;
var
  i, j, x, y, index : byte;
  bin : string;
begin
//  debug('maxx, maxy', maxx, maxy);
  fs := TFileStream.Create(filename, fmCreate);
  try
    // Dimension x, y
    fs.WriteByte(antic4Tile.dimX);
    fs.WriteByte(antic4Tile.dimY);

    // Save data
////    for i := 0 to maxX*maxY - 1 do
//    for y := 0 to maxY - 1 do
//      for x := 0 to maxX - 1 do begin
//        for j := 0 to 7 do
//          for i := 0 to 7 do
//            fs.WriteByte(fldCharAntic45[cnt, i, j]);
//
//        Inc(cnt);
//      end;

//for j := 0 to 1023 do begin
//  bin := '';
//  for i := 0 to 7 do
//    bin += IntToStr(fldFontSet[i, j]);
//
//  fs.WriteByte(bin2dec(bin));
//end;
//    index := 0;
    for y := 0 to antic4Tile.dimY - 1 do
//      index := x;
      for x := 0 to antic4Tile.dimX - 1 do begin
        index := x + y shl 2;
        for i := 0 to _CHAR_DIM do begin
          bin := '';
          for j := 0 to _CHAR_DIM do
            bin += IntToStr(fldChar[index, j, i]);

//          debug(bin + ', index: ', index);
          fs.WriteByte(bin2dec(bin));
        end;

        if CheckInverse(index) then
          fs.WriteByte(1)
        else
          fs.WriteByte(0);
      end;

    isOk := true;
  finally
    fs.Free;
  end;
end;

begin
  try
    if isSaveAs then begin
      frmMain.dlgSave.Filter := 'Antic mode 4/5 tile files (*.tl4)' +
                                '|*.tl4|All files (*.*)|*.*';
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
//      isEdit := false;
      caption := programName + ' ' + programVersion +
                 ' - Antic mode 4&5 tile editor (' + filename + ')';
    end;
  end;
end;

procedure TfrmAntic4Tiles.imgCharDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
var
  xf, yf : byte;
begin
  btn := Button;
  xf := X div chrFactX;
  yf := Y div chrFactY;

  if btnSelectTile.Down then
    with shapeTileGrid do begin
      Visible := true;
//      BringToFront;
      Top := TImage(Sender).Top - 2;
      Width := TImage(Sender).Width + 4;
      Left := TImage(Sender).Left - 2;
      Height := TImage(Sender).Height + 4;
    end;

  mouseIsDown := True;
  selectTile := TImage(Sender).Tag;

  if ((antic4Tile.dimY = 1) and (selectTile > 3)) or
     ((antic4Tile.dimY = 2) and (selectTile > 7)) or
     ((antic4Tile.dimY = 3) and (selectTile > 11)) or
     ((antic4Tile.dimY = 4) and (selectTile > 15)) then begin
    exit;
  end;

  if btnDraw.Down then
    PlotCharAntic45(Sender, xf, yf);

//  charEditIndex[offs] := 1;
end;

procedure TfrmAntic4Tiles.imgCharUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  btn := mbMiddle;
  if MouseIsDown then begin
    for y := 0 to _CHAR_DIM do
      for x := 0 to _CHAR_DIM do
        undoValuesEx[selectTile, x, y] := fldChar[selectTile, x, y];

    if btnRotate.Down then
      RotateProc(Sender)
    else if btnFlipX.Down then
      FlipXProc(Sender)
    else if btnFlipY.Down then
      FlipYProc(Sender)
    else if btnInvert.Down then
      InvertProc(Sender)
    else if btnClearTile.Down then
      ClearTileProc(Sender)
    else if btnFillTile.Down then
      FillTileProc(Sender)
  end;
  MouseIsDown := false;
end;

{-----------------------------------------------------------------------------
 Draw Antic 4/5 pixel inside character editor
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.PlotCharAntic45(Sender : TObject; xf, yf : byte);
var
  col : byte;
begin
  case btn of
    mbLeft : col := 1;
    mbRight: col := 0;
  else
    exit;
  end;

  if xf > _CHAR_DIM then xf := _CHAR_DIM;
//  selectTile := TImage(Sender).Tag;
//  statusBar.Panels[2].Text := 'Index: ' + inttostr(index);

  fldCharAntic45[selectTile, xf, yf] := frmColors.SelColor;
  fldCharAntic45Ex[antic4Tile.selected, selectTile, xf, yf] := frmColors.SelColor;

  case xf of
    0, 2, 4, 6: begin
      undoValues[0].tileSelected := selectTile;
      undoValues[0].x := xf;
      undoValues[0].y := yf;
      undoValues[0].value := fldChar[selectTile, xf, yf];
      undoValues[1].tileSelected := selectTile;
      undoValues[1].x := xf + 1;
      undoValues[1].y := yf;
      undoValues[1].value := fldChar[selectTile, xf + 1, yf];
      if col = 0 then begin
//        fldCharMem[0, selectTile, xf, yf] := fldChar[selectTile, xf, yf];
//        fldCharMem[0, selectTile, xf + 1, yf] := fldChar[selectTile, xf + 1, yf];
//        fldCharMemEx[0, selectTile, 0] := fldChar[selectTile, xf, yf];
//        fldCharMemEx[0, selectTile, 1] := fldChar[selectTile, xf + 1, yf];

        //fldCharMemEx[0, selectTile, 0] := fldChar[selectTile, xf, yf];
        //fldCharMemEx[0, selectTile, 1] := xf;
        //fldCharMemEx[0, selectTile, 2] := yf;
        //fldCharMemEx[0, selectTile, 3] := fldChar[selectTile, xf + 1, yf];
        //fldCharMemEx[0, selectTile, 4] := xf + 1;
        //fldCharMemEx[0, selectTile, 5] := yf;

        fldCharAntic45[selectTile, xf + 1, yf] := 0;
        fldCharAntic45Ex[antic4Tile.selected, selectTile, xf + 1, yf] := 0;
        FillRectEx(TImage(Sender), colTab[0], xf*chrFactX + 1, yf*chrFactY + 1,
                   chrFactX*2 - 1, chrFactY - 1);
        fldChar[selectTile, xf, yf] := 0;
        fldChar[selectTile, xf + 1, yf] := 0;
      end
      else begin
        fldCharAntic45[selectTile, xf + 1, yf] := frmColors.SelColor;
        fldCharAntic45Ex[antic4Tile.selected, selectTile, xf + 1, yf] := frmColors.SelColor;
        FillRectEx(TImage(Sender), colTab[frmColors.SelColor],
                   xf*chrFactX + 1, yf*chrFactY + 1, chrFactX*2 - 1, chrFactY - 1);
        case frmColors.SelColor of
          0: begin
            fldChar[selectTile, xf, yf] := 0;
            fldChar[selectTile, xf + 1, yf] := 0;
          end;
          1: begin
            fldChar[selectTile, xf, yf] := 0;
            fldChar[selectTile, xf + 1, yf] := 1;
          end;
          2: begin
            fldChar[selectTile, xf, yf] := 1;
            fldChar[selectTile, xf + 1, yf] := 0;
          end;
          3, 10: begin
            fldChar[selectTile, xf, yf] := 1;
            fldChar[selectTile, xf + 1, yf] := 1;
          end;
        end;
      end;
    end;
    1, 3, 5, 7: begin
      undoValues[0].tileSelected := selectTile;
      undoValues[0].x := xf - 1;
      undoValues[0].y := yf;
      undoValues[0].value := fldChar[selectTile, xf - 1, yf];
      undoValues[1].tileSelected := selectTile;
      undoValues[1].x := xf;
      undoValues[1].y := yf;
      undoValues[1].value := fldChar[selectTile, xf, yf];
      if col = 0 then begin
//        fldCharMem[0, selectTile, xf - 1, yf] := fldChar[selectTile, xf - 1, yf];
//        fldCharMem[0, selectTile, xf, yf] := fldChar[selectTile, xf, yf];
//        fldCharMemEx[0, selectTile, 0] := fldChar[selectTile, xf - 1, yf];
//        fldCharMemEx[0, selectTile, 1] := fldChar[selectTile, xf, yf];
        fldCharAntic45[selectTile, xf - 1, yf] := 0;
        fldCharAntic45Ex[antic4Tile.selected, selectTile, xf - 1, yf] := 0;
        FillRectEx(TImage(Sender), colTab[0],
                   (xf - 1)*chrFactX + 1, yf*chrFactY + 1, chrFactX*2 - 1, chrFactY - 1);
        fldChar[selectTile, xf - 1, yf] := 0;
        fldChar[selectTile, xf, yf] := 0;
      end
      else begin
//        fldCharMem[0, selectTile, xf - 1, yf] := fldChar[selectTile, xf - 1, yf];
//        fldCharMem[0, selectTile, xf, yf] := fldChar[selectTile, xf, yf];
//        fldCharMemEx[0, selectTile, 0] := fldChar[selectTile, xf - 1, yf];
//        fldCharMemEx[0, selectTile, 1] := fldChar[selectTile, xf, yf];
        fldCharAntic45[selectTile, xf - 1, yf] := frmColors.SelColor;
        fldCharAntic45Ex[antic4Tile.selected, selectTile, xf - 1, yf] := frmColors.SelColor;
        FillRectEx(TImage(Sender), colTab[frmColors.SelColor],
                   (xf - 1)*chrFactX + 1, yf*chrFactY + 1, chrFactX*2 - 1, chrFactY - 1);
        case frmColors.SelColor of
          0: begin
            fldChar[selectTile, xf - 1, yf] := 0;
            fldChar[selectTile, xf, yf] := 0;
          end;
          1: begin
            fldChar[selectTile, xf - 1, yf] := 0;
            fldChar[selectTile, xf, yf] := 1;
          end;
          2: begin
            fldChar[selectTile, xf - 1, yf] := 1;
            fldChar[selectTile, xf, yf] := 0;
          end;
          3, 10: begin
            fldChar[selectTile, xf - 1, yf] := 1;
            fldChar[selectTile, xf, yf] := 1;
          end;
        end;
      end;
    end;
  end;

//  statusBar.Panels[1].Text := 'Indexxx: ' + inttostr(index);
  RefreshChar(TImage(Sender), selectTile, frmColors.SelColor);

  //for i := 0 to _CHAR_DIM do
  //  for j := 0 to _CHAR_DIM do begin
  //    fldCharAntic45[0, j, i] := fldCharAntic45Ex[selectTiles, 0, j, i];
  //    fldCharAntic45[1, j, i] := fldCharAntic45Ex[selectTiles, 1, j, i];
  //    fldCharAntic45[2, j, i] := fldCharAntic45Ex[selectTiles, 2, j, i];
  //  end;

  RefreshTiles(selectTile);
end;

// Refresh screen
procedure TfrmAntic4Tiles.RefreshTiles(index : byte);
var
  i, j : integer;
  m, r : byte;
begin
//  debug(maxx);
  r := 0; m := 0;
  for j := 0 to maxSize do begin
//    if (j > maxX - 1) and (j mod (maxX - 1) = 0) then begin
//    if (j > maxX) and (j mod (maxX - 1) = 0) then begin
//    end
    for i := 1 to maxXX do
      if j = maxXX*i then begin
        r := 0;
        Inc(m, 2);
        break;
      end;

//    if (fldAtascii[j] = index) or (fldAtascii[j] = index + 128) then begin
    PutChar(r, m, j);  //fldAtascii[j]);
    Inc(r);
  end;
end;

{-----------------------------------------------------------------------------
 Draw character on screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.PutChar(scrPos, y, offset : byte);
var
  xf, yf : integer;
  //isInverse : boolean = false;
  cnt : byte = 0;
  i, j, col : byte;
  mask : array[0..1] of byte;
begin
  xf := scrPos shl 3;
  yf := y shl 2;

  //for dy := 0 to maxY do
  //  for dx := 0 to maxX do
  //    if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
  //      xf := dx shl 3;
  //      break;
  //    end;

  //if offset > 127 then begin
  //  isInverse := true;
  //  Dec(offset, 128);
  //end;

//  offset := offset shl 3;
//  statusBar.Panels[1].Text := 'xf, yf, offset: ' + inttostr(xf) + ',' + inttostr(yf) + ',' + inttostr(offset);

  for i := 0 to _CHAR_DIM do
    for j := 0 to _CHAR_DIM do begin
      Inc(cnt);
      col := fldChar[offset, j, i];
      mask[cnt - 1] := col;
      if cnt = 2 then begin
        if (mask[0] = 0) and (mask[1] = 1) then
          col := 1
        else if (mask[0] = 1) and (mask[1] = 0) then
          col := 2
        else if (mask[0] = 1) and (mask[1] = 1) then begin
//          if isInverse then
//            col := frmColors.SelColor;
//          else
          if inverseIndex[offset] then
            col := 10
          else
            col := 3;
        end;
//        col := frmColors.SelColor;

//        case antic4Tile.selected of
        FillRectEx((FindComponent('imgTile0' + IntToStr(antic4Tile.selected)) as TImage),
                   colTab[col], (xf + j - 1)*factX, (yf + i)*factY, factX shl 1, factY);
(*          2: FillRectEx(imgTile02, colTab[col], (xf + j - 1)*factX, (yf + i)*factY,
                        factX shl 1, factY);
          3: FillRectEx(imgTile03, colTab[col], (xf + j - 1)*factX, (yf + i)*factY,
                        factX shl 1, factY);
        end;*)

        cnt := 0;
      end;
    end;
end;

procedure TfrmAntic4Tiles.RefreshChar(image : TImage; index, selColor : byte);
var
  oldColor : byte;
  i, j : byte;
begin
  // Check color3 or inverse color
  if (selColor = 3) or (selColor = 10) then begin
    if selColor = 3 then begin
      oldColor := 10;
      inverseIndex[index] := false;
    end
    else begin
      oldColor := 3;
      inverseIndex[index] := true;
    end;

    for i := 0 to _CHAR_DIM do
      for j := 0 to _CHAR_DIM do
        if fldCharAntic45[index, j, i] = oldColor then
          fldCharAntic45[index, j, i] := selColor;
  end;

  MaskColor(image, index);
end;

{-----------------------------------------------------------------------------
 Color registers
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ColorRegisters;
begin
  FillRectEx(color4, coltab[0], 0, 0, color0.width, color0.Height);
  FillRectEx(color0, coltab[1], 0, 0, color0.width, color0.Height);
  FillRectEx(color1, coltab[2], 0, 0, color0.width, color0.Height);
  FillRectEx(color2, coltab[3], 0, 0, color0.width, color0.Height);
  FillRectEx(color3, coltab[10], 0, 0, color0.width, color0.Height);
end;

procedure TfrmAntic4Tiles.SetXY(_maxX, _maxY : byte);
begin
  antic4Tile.dimX := _maxX;
  antic4Tile.dimY := _maxY;
  maxSize := (antic4Tile.dimX + 1)*(antic4Tile.dimY + 1) - 1;
//  maxSize := maxX*maxY - 1;
//  imgTile01.Width := (maxX + 1)*24;
//  imgTile01.Height := (maxY + 1)*modeHeight;
//  imgTile01.Invalidate;
//  statusBar.Panels[0].Text := 'Max X coord.: ' + IntToStr(maxX) +
//                              ', max Y coord.: ' + IntToStr(maxY);
//  debug(maxsize);
end;

procedure TfrmAntic4Tiles.SetAnticMode(mode : byte);
begin
  anticMode := mode;
//  isAntic4 := mode = 4;

//  charXOffset := 32;
  if anticMode = 4 then begin
    modeHeight := 24;
    factY := 2;
//    factY02 := 2;
//    charYOffset := 28;
//    charYOffset02 := 17;
  end
  else begin
    modeHeight := 48;
    factY := 4;
//    factY02 := 4;
//    charYOffset := 40;
//    charYOffset02 := 32;
  end;

//  ShowFontSet;
end;

{-----------------------------------------------------------------------------
 Refresh screen data
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.RefreshData;
begin
  FillRectEx(imgChar0000, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar0100, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar0010, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar0001, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar1000, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar1100, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar1010, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar1001, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar2000, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar2100, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar2010, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar2001, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar3000, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar3100, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar3010, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar3001, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar4000, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar4100, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar4010, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  FillRectEx(imgChar4001, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  //FillRectEx(imgChar5000, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  //FillRectEx(imgChar5100, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  //FillRectEx(imgChar5010, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
  //FillRectEx(imgChar5001, colTab[0], 0, 0, imgChar0000.Width, imgChar0000.Height);
(*
//  imgTile01.Canvas.Clear;
  x := 0; y := 0;
  for i := 0 to maxSize do begin
    if (i > maxX) and (i mod (maxX + 1) = 0) then begin
      x := 0;
      Inc(y, 2);
    end;
    PutChar(x, y, fldAtascii[i]);

    // Draw a grid
    imgTile01.Canvas.Pen.Color := gridColor;
    xx := x*24;
    yy := y*modeHeight shr 1;
//    if gridColor <> colTab[0] then begin
    if isShowGrid then begin
//      debug('RefreshData isShowGrid');
      imgTile01.Canvas.Line(xx, yy, xx + 24, yy);
      imgTile01.Canvas.Line(xx, yy + modeHeight, xx + 24, yy + modeHeight);
      imgTile01.Canvas.Line(xx, yy, xx, yy + modeHeight);
      imgTile01.Canvas.Line(xx + 24, yy, xx + 24, yy + modeHeight);
    end;
    Inc(x);
  end;

  ShowFontSet;*)

  MaskColor(imgChar0000, 0); MaskColor(imgChar0100, 1);
  MaskColor(imgChar0010, 2); MaskColor(imgChar0001, 3);

  MaskColor(imgChar1000, 4); MaskColor(imgChar1100, 5);
  MaskColor(imgChar1010, 6); MaskColor(imgChar1001, 7);

  MaskColor(imgChar2000, 8); MaskColor(imgChar2100, 9);
  MaskColor(imgChar2010, 10); MaskColor(imgChar2001, 11);

  MaskColor(imgChar3000, 12); MaskColor(imgChar3100, 13);
  MaskColor(imgChar3010, 14); MaskColor(imgChar3001, 15);

  MaskColor(imgChar4000, 16); MaskColor(imgChar4100, 17);
  MaskColor(imgChar4010, 18); MaskColor(imgChar4001, 19);

  //with imgChar0000.Canvas do begin
  //  Pen.Color := clSkyBlue;
  //  Pen.Style := psSolid;
  //  FillRect(bounds(0, 0, imgChar0000.Width, imgChar0000.Height));
  //end;
end;

procedure TfrmAntic4Tiles.MaskColor(image : TImage; index : byte);
var
  cnt : byte = 0;
  i, j, col : byte;
  mask : array[0..1] of byte;
begin
  if not cellIndex[index] then Exit;

  for i := 0 to _CHAR_DIM do begin
    for j := 0 to _CHAR_DIM do begin
      Inc(cnt);
      col := fldChar[index, j, i];
      mask[cnt - 1] := col;
      if cnt = 2 then begin
        if (mask[0] = 0) and (mask[1] = 1) then
          col := 1
        else if (mask[0] = 1) and (mask[1] = 0) then
          col := 2
        else if (mask[0] = 1) and (mask[1] = 1) then begin
          if inverseIndex[index] then
            col := 10
          else
            col := 3;
        end;

//        if col > 0 then showmessage(inttostr(col));
        FillRectEx(image, colTab[col], (j - 1)*chrFactX + 1, i*chrFactY + 1,
                   chrFactX shl 1 - 1, chrFactY - 1);
        if isShowGrid then begin
          image.Canvas.Pen.Color := clGray;
          image.Canvas.Rectangle((j - 1)*chrFactX, i*chrFactY, (j - 1)*chrFactX + 1 + chrFactX*2,
                                 i*chrFactY + chrFactY + 1);
        end;
        cnt := 0;
      end;
    end;
  end;

//  image.Canvas.Pen.Color := clSkyBlue;
//  image.Canvas.Rectangle(0, 0, image.Width, image.Height);
  image.Invalidate;
end;

procedure TfrmAntic4Tiles.RefreshGrid;
begin
  RefreshData;
//  ColorRegisters;
end;

function TfrmAntic4Tiles.CheckInverse(index : byte) : boolean;
var
  i, j : byte;
begin
  result := false;
  for i := 0 to _CHAR_DIM do
    for j := 0 to _CHAR_DIM do
      if fldCharAntic45[index, j, i] = 10 then
        result := true;
end;

procedure TfrmAntic4Tiles.ColorProc(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  frmColors.SelColor := TImage(Sender).Tag;
  ColorRegisters;
(*  case frmColors.SelColor of
    0: begin
      color4.Canvas.Pen.Color := clGreen;
//      color4.Canvas.RoundRect(0, 0, color1.width, color1.Height, 4, 4);
      color4.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
    1: begin
      color0.Canvas.Pen.Color := clGreen;
      color0.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
    2: begin
      color1.Canvas.Pen.Color := clGreen;
      color1.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
    3: begin
//      inverseIndex[selectTile] := false;
      color2.Canvas.Pen.Color := clGreen;
      color2.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
    10: begin
//      inverseIndex[selectTile] := clGreen;
      color3.Canvas.Pen.Color := clRed;
      color3.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
  end;*)
  TImage(Sender).Canvas.Pen.Color := clRed;
  TImage(Sender).Canvas.Rectangle(0, 0, color1.width, color1.Height);
end;

procedure TfrmAntic4Tiles.editXLeave(Sender : TObject);
var
  i, j : byte;
begin
  btn := mbMiddle;
  if not isCreate then
    SetXY(editX.Value, editY.Value);

//  antic4Tile.dimX := editX.Value;
//  antic4Tile.dimY := editY.Value;
  antic4TileArray[antic4Tile.selected].dimX := editX.Value;
  antic4TileArray[antic4Tile.selected].dimY := editY.Value;
  ShowTilesDim;

  for i := 0 to SizeOf(cellIndex) - 1 do
    cellIndex[i] := false;

  for i := 0 to editY.Value - 1 do
    for j := 0 to editX.Value - 1 do
      cellIndex[j + i shl 2] := true;

  SetCells;
end;

procedure TfrmAntic4Tiles.editXUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
var
  i, j : byte;
begin
  //if editX.Value = 1 then
  //  SetCell(imgChar0000, true)
  //SetCell(imgChar0000, true);
  //if editY.Value <= 2 then begin
  //  SetCell(imgChar1000, true);
  //end;

  btn := mbMiddle;
  if not isCreate then
    SetXY(editX.Value, editY.Value);

//  antic4Tile.dimX := editX.Value;
//  antic4Tile.dimY := editY.Value;
  antic4TileArray[antic4Tile.selected].dimX := editX.Value;
  antic4TileArray[antic4Tile.selected].dimY := editY.Value;
  ShowTilesDim;

  for i := 0 to SizeOf(cellIndex) - 1 do
    cellIndex[i] := false;

  for i := 0 to editY.Value - 1 do
    for j := 0 to editX.Value - 1 do
      cellIndex[j + i shl 2] := true;

  SetCells;
end;

procedure TfrmAntic4Tiles.SetCells;
var
  y, x, index : byte;
  image : TImage;
begin
  for y := 0 to 3 do
    for x := 0 to 3 do begin
      index := x + y shl 2;
      case index of
        0 : image := imgChar0000;
        1 : image := imgChar0100;
        2 : image := imgChar0010;
        3 : image := imgChar0001;
        4 : image := imgChar1000;
        5 : image := imgChar1100;
        6 : image := imgChar1010;
        7 : image := imgChar1001;
        8 : image := imgChar2000;
        9 : image := imgChar2100;
        10 : image := imgChar2010;
        11 : image := imgChar2001;
        12 : image := imgChar3000;
        13 : image := imgChar3100;
        14 : image := imgChar3010;
        15 : image := imgChar3001;
        16 : image := imgChar4000;
        17 : image := imgChar4100;
        18 : image := imgChar4010;
        19 : image := imgChar4001;
      end;
      image.Enabled := cellIndex[index];
//      image.Visible := cellIndex[index];
      if cellIndex[index] then
        FillRectEx(image, colTab[0], 0, 0, image.Width, image.Height)
      else
        FillRectEx(image, clGray, 0, 0, image.Width, image.Height);
    end;

  if (imageObject <> nil) then begin
    (imageObject as TImage).Width := factX*editX.Value*8;
    (imageObject as TImage).Height := factY*editY.Value*8;
    (imageObject as TImage).Picture.Bitmap.Width := (imageObject as TImage).Width;
    (imageObject as TImage).Picture.Bitmap.Height := (imageObject as TImage).Height;
    (imageObject as TImage).Refresh;

    //case antic4Tile.selected of
    //  1: begin
    //    imgTile01.Width := (imageObject as TImage).Width;
    //    imgTile01.Height := (imageObject as TImage).Height;
    //    imgTile01.Picture.Bitmap.Width := (imageObject as TImage).Width;
    //    imgTile01.Picture.Bitmap.Height := (imageObject as TImage).Height;
    //    imgTile01.Refresh;
    //  end;
    //end;
  end;

  RefreshData;
  RefreshTiles(selectTile);
end;

procedure TfrmAntic4Tiles.GenCodeProc(Sender : TObject);
begin
  ShowMessage('Not implemented yet!');

  //frmAntic4Gen := TfrmAntic4Gen.Create(Self);
  //with frmAntic4Gen do
  //  try
  //    ShowModal;
  //  finally
  //    Free;
  //  end;
end;

{-----------------------------------------------------------------------------
 Invert tile data
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.InvertProc(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      fldChar[selectTile, x, y] := 1 - fldChar[selectTile, x, y];

  UpdateColors;
  RefreshGrid;
  RefreshTiles(selectTile);
end;

{-----------------------------------------------------------------------------
 Rotate character
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.RotateProc(Sender : TObject);
var
  x, y, n : byte;
  arr : charType;
begin
  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      arr[y, x] := fldChar[selectTile, x, y];

  for x := 0 to _CHAR_DIM do
    for n := 0 to _CHAR_DIM do
      fldChar[selectTile, 7 - n, x] := arr[n, x];

  UpdateColors;
  RefreshGrid;
  RefreshTiles(selectTile);
end;

{-----------------------------------------------------------------------------
 Mirror character horizontally
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.FlipXProc(Sender : TObject);
var
  x, y, n : byte;
begin
  for y := 0 to _CHAR_DIM do
    for x := _CHAR_DIM downto 4 do begin
      n := fldChar[selectTile, x, y];
      fldChar[selectTile, x, y] := fldChar[selectTile, 7 - x, y];
      fldChar[selectTile, 7 - x, y] := n;
    end;

  UpdateColors;
  RefreshGrid;
  RefreshTiles(selectTile);
end;

{-----------------------------------------------------------------------------
 Mirror character vertically
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.FlipYProc(Sender : TObject);
var
  x, y, n : byte;
begin
  for x := 0 to _CHAR_DIM do
    for y := _CHAR_DIM downto 4 do begin
      n := fldChar[selectTile, x, y];
      fldChar[selectTile, x, y] := fldChar[selectTile, x, 7 - y];
      fldChar[selectTile, x, 7 - y] := n;
    end;

  UpdateColors;
  RefreshGrid;
  RefreshTiles(selectTile);
end;

procedure TfrmAntic4Tiles.ClearTilesProc(Sender : TObject);
begin
  case antic4Tile.selected of
    1: FillRectEx(imgTile01, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
    2: FillRectEx(imgTile02, coltab[0], 0, 0, imgTile02.Width, imgTile02.Height);
    3: FillRectEx(imgTile03, coltab[0], 0, 0, imgTile03.Width, imgTile03.Height);
    4: FillRectEx(imgTile04, coltab[0], 0, 0, imgTile04.Width, imgTile04.Height);
    5: FillRectEx(imgTile05, coltab[0], 0, 0, imgTile05.Width, imgTile05.Height);
    6: FillRectEx(imgTile06, coltab[0], 0, 0, imgTile06.Width, imgTile06.Height);
    7: FillRectEx(imgTile07, coltab[0], 0, 0, imgTile07.Width, imgTile07.Height);
    8: FillRectEx(imgTile08, coltab[0], 0, 0, imgTile08.Width, imgTile08.Height);
  end;

//  FillByte(fldAtascii, SizeOf(fldAtascii), 0);
  FillByte(fldCharAntic45, SizeOf(fldCharAntic45), 0);
  FillByte(fldChar, SizeOf(fldChar), 0);
  FillByte(inverseIndex, SizeOf(inverseIndex), 0);
  SetCells;
//  RefreshData;

  UpdateColors;
end;

{-----------------------------------------------------------------------------
 Shift tile left
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftLeftProc(Sender: TObject);
var
  x, y : byte;
  n1, n2 : byte;
begin
  //for y := 0 to _CHAR_DIM do begin
  //  n := fldChar[selectTile, 0, y];
  //  for x := 1 to _CHAR_DIM do
  //    fldChar[selectTile, x - 1, y] := fldChar[selectTile, x, y];
  //
  //  fldChar[selectTile, 7, y] := n;
  //end;
  for y := 0 to _CHAR_DIM do begin
    n1 := fldChar[selectTile, 0, y];
    n2 := fldChar[selectTile, 1, y];
    for x := 1 to _CHAR_DIM do
      if (x = 1) or (x = 3) or (x = 5) then begin
        fldChar[selectTile, x - 1, y] := fldChar[selectTile, x + 1, y];
        fldChar[selectTile, x, y] := fldChar[selectTile, x + 2, y];
      end;

    fldChar[selectTile, 6, y] := n1;
    fldChar[selectTile, 7, y] := n2;
  end;
end;

{-----------------------------------------------------------------------------
 Shift tile right
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftRightProc(Sender: TObject);
var
  x, y : byte;
  n1, n2 : byte;
begin
  //for y := 0 to _CHAR_DIM do begin
  //  n := fldChar[selectTile, 7, y];
  //  for x := _CHAR_DIM - 1 downto 0 do
  //    fldChar[selectTile, x + 1, y] := fldChar[selectTile, x, y];
  //
  //  fldChar[selectTile, 0, y] := n;
  //end;
  for y := 0 to _CHAR_DIM do begin
    n1 := fldChar[selectTile, 7, y];
    n2 := fldChar[selectTile, 6, y];
    for x := _CHAR_DIM downto 1 do
      if (x = 7) or (x = 5) or (x = 3) then begin
        fldChar[selectTile, x, y] := fldChar[selectTile, x - 2, y];
        fldChar[selectTile, x - 1, y] := fldChar[selectTile, x - 3, y];
      end;

    fldChar[selectTile, 1, y] := n1;
    fldChar[selectTile, 0, y] := n2;
  end;
end;

{-----------------------------------------------------------------------------
 Shift tile up
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftUpProc(Sender: TObject);
var
  x, y : byte;
begin
  for x := 0 to _CHAR_DIM do
    fld02[x] := fldChar[selectTile, x, 0];

  for x := 0 to _CHAR_DIM do
    for y := 1 to _CHAR_DIM do begin
      fldChar[selectTile, x, y - 1] := fldChar[selectTile, x, y];
      fldChar[selectTile, x, y] := 0;
    end;

  for x := 0 to _CHAR_DIM do
    fldChar[selectTile, x, _CHAR_DIM] := fld02[x];

//  RefreshGrid;
end;

{-----------------------------------------------------------------------------
 Shift tile down
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftDownProc(Sender: TObject);
var
  x, y : byte;
begin
  for x := 0 to _CHAR_DIM do
    fld02[x] := fldChar[selectTile, x, _CHAR_DIM];

  for x := 0 to _CHAR_DIM do
    for y := _CHAR_DIM - 1 downto 0 do begin
      fldChar[selectTile, x, y + 1] := fldChar[selectTile, x, y];
      fldChar[selectTile, x, y] := 0;
    end;

  for x := 0 to _CHAR_DIM do
    fldChar[selectTile, x, 0] := fld02[x];
end;

{-----------------------------------------------------------------------------
 Move character left
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.MoveLeftProc(Sender: TObject);
var
  x, y : byte;
begin
  //for y := 0 to _CHAR_DIM do
  //  for x := 1 to _CHAR_DIM do begin
  //    fldChar[selectTile, x - 1, y] := fldChar[selectTile, x, y];
  //    fldChar[selectTile, x, y] := 0;
  //  end;

  for y := 0 to _CHAR_DIM do begin
    for x := 1 to _CHAR_DIM do
      if (x = 1) or (x = 3) or (x = 5) then begin
        fldChar[selectTile, x - 1, y] := fldChar[selectTile, x + 1, y];
        fldChar[selectTile, x, y] := fldChar[selectTile, x + 2, y];
        fldChar[selectTile, x + 1, y] := 0;
        fldChar[selectTile, x + 2, y] := 0;
      end;
  end;
end;

//{-----------------------------------------------------------------------------
// Move character right
// -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.MoveRightProc(Sender: TObject);
var
  x, y : byte;
begin
  //for y := 0 to _CHAR_DIM do
  //  for x := _CHAR_DIM - 1 downto 0 do begin
  //    fldChar[selectTile, x + 1, y] := fldChar[selectTile, x, y];
  //    fldChar[selectTile, x, y] := 0;
  //  end;

//begin
//  MoveRight(chrX, chrY, fldChar);

  for y := 0 to _CHAR_DIM do begin
//    n1 := fldChar[selectTile, 7, y];
//    n2 := fldChar[selectTile, 6, y];
    for x := _CHAR_DIM downto 1 do
      if (x = 7) or (x = 5) or (x = 3) then begin
        fldChar[selectTile, x, y] := fldChar[selectTile, x - 2, y];
        fldChar[selectTile, x - 1, y] := fldChar[selectTile, x - 3, y];
        fldChar[selectTile, x - 2, y] := 0;
        fldChar[selectTile, x - 3, y] := 0;
      end;

//    fldChar[selectTile, 1, y] := n1;
//    fldChar[selectTile, 0, y] := n2;
  end;
end;

{-----------------------------------------------------------------------------
 Move character up
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.MoveUpProc(Sender: TObject);
var
  x, y : byte;
begin
  for x := 0 to _CHAR_DIM do
    for y := 1 to _CHAR_DIM do begin
      fldChar[selectTile, x, y - 1] := fldChar[selectTile, x, y];
      fldChar[selectTile, x, y] := 0;
    end;
end;

{-----------------------------------------------------------------------------
 Move character down
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.MoveDownProc(Sender: TObject);
var
  x, y : byte;
begin
  for x := 0 to _CHAR_DIM do
    for y := _CHAR_DIM - 1 downto 0 do begin
      fldChar[selectTile, x, y + 1] := fldChar[selectTile, x, y];
      fldChar[selectTile, x, y] := 0;
    end;
end;

procedure TfrmAntic4Tiles.CharOper(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      undoValuesEx[selectTile, x, y] := fldChar[selectTile, x, y];

  if radShiftChar.Checked then begin
    case (Sender as TSpeedButton).Tag of
      0: ShiftLeftProc(Sender);
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
  UpdateColors;
  RefreshGrid;
  RefreshTiles(selectTile);
end;

procedure TfrmAntic4Tiles.ClearTileProc(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      fldChar[selectTile, x, y] := 0;

  UpdateColors;
  RefreshGrid;
  RefreshTiles(selectTile);
//  editXLeave(Sender);
end;

procedure TfrmAntic4Tiles.FillTileProc(Sender : TObject);
var
  x, y : byte;
begin
  //for y := 0 to _CHAR_DIM do
  //  for x := 0 to _CHAR_DIM do
  //    undoValuesEx[selectTile, x, y] := fldChar[selectTile, x, y];

  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      if (x = 1) or (x = 3) or (x = 5) or (x = 7) then begin
        case frmColors.SelColor of
          0: begin
            fldChar[selectTile, x - 1, y] := 0;
            fldChar[selectTile, x, y] := 0;
          end;
          1: begin
            fldChar[selectTile, x - 1, y] := 0;
            fldChar[selectTile, x, y] := 1;
          end;
          2: begin
            fldChar[selectTile, x - 1, y] := 1;
            fldChar[selectTile, x, y] := 0;
          end;
          3, 10: begin
            fldChar[selectTile, x - 1, y] := 1;
            fldChar[selectTile, x, y] := 1;
          end;
        end;
      end;

  UpdateColors;
  inverseIndex[selectTile] := frmColors.SelColor = 10;
  RefreshGrid;
//  RefreshChar(imgChar0000, selectTile, frmColors.SelColor);
  RefreshTiles(selectTile);
end;

// Update data colors on tile grid
procedure TfrmAntic4Tiles.UpdateColors;
var
  x, y, i : byte;
begin
  //for y := 0 to _CHAR_DIM do
  //  for x := 0 to _CHAR_DIM do
  //    redoValuesEx[selectTile, x, y] := undoValuesEx[selectTile, x, y];

  for i := 0 to 19 do
    for y := 0 to _CHAR_DIM do
      for x := 0 to _CHAR_DIM do
        if (x = 1) or (x = 3) or (x = 5) or (x = 7) then begin
          if (fldChar[i, x - 1, y] = 0) and (fldChar[i, x, y] = 0) then begin
            fldCharAntic45[i, x - 1, y] := 0;
            fldCharAntic45[i, x, y] := 0;
            fldCharAntic45Ex[antic4Tile.selected, i, x - 1, y] := 0;
            fldCharAntic45Ex[antic4Tile.selected, i, x, y] := 0;
          end
          else if (fldChar[i, x - 1, y] = 0) and (fldChar[i, x, y] = 1) then begin
            fldCharAntic45[i, x - 1, y] := 1;
            fldCharAntic45[i, x, y] := 1;
            fldCharAntic45Ex[antic4Tile.selected, i, x - 1, y] := 1;
            fldCharAntic45Ex[antic4Tile.selected, i, x, y] := 1;
          end
          else if (fldChar[i, x - 1, y] = 1) and (fldChar[i, x, y] = 0) then begin
            fldCharAntic45[i, x - 1, y] := 2;
            fldCharAntic45[i, x, y] := 2;
            fldCharAntic45Ex[antic4Tile.selected, i, x - 1, y] := 2;
            fldCharAntic45Ex[antic4Tile.selected, i, x, y] := 2;
          end
          else if (fldChar[i, x - 1, y] = 1) and (fldChar[i, x, y] = 1) then begin
            fldCharAntic45[i, x - 1, y] := 3;
            fldCharAntic45[i, x, y] := 3;
            fldCharAntic45Ex[antic4Tile.selected, i, x - 1, y] := 3;
            fldCharAntic45Ex[antic4Tile.selected, i, x, y] := 3;
          end;
        end;
end;

procedure TfrmAntic4Tiles.ColorPaletteProc(Sender : TObject);
begin
  frmColors.Show;
end;

procedure TfrmAntic4Tiles.CharSetProc(Sender : TObject);
begin
//  ShowMessage('Not implemented yet!');
  frmCharSet := TfrmCharSet.Create(Self);
  with frmCharSet do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmAntic4Tiles.SelectTileProc(Sender : TObject);
var
  i : byte;
  xf, yf : byte;
  col : byte;
begin
//  ShowTilesDim;
  imageObject := Sender;

  with shape do begin
    Visible := true;
    Top := TImage(Sender).Top - 4;
    Left := TImage(Sender).Left - 4;
    Width := TImage(Sender).Width + 8;
    Height := TImage(Sender).Height + 8;
  end;

  antic4Tile.selected := TImage(Sender).Tag;
  antic4Tile.dimX := antic4TileArray[antic4Tile.selected].dimX;
  antic4Tile.dimY := antic4TileArray[antic4Tile.selected].dimY;
  editX.Value := antic4Tile.dimX;
  editY.Value := antic4Tile.dimY;

//  fldCharAntic45Ex[selectTiles, selectTile, xf, yf] := frmColors.SelColor;
(*
  for i := 0 to _CHAR_DIM do
    for j := 0 to _CHAR_DIM do begin
      //fldChar[0, j, i] := fldCharAntic45Ex[selectTiles, 0, j, i];
      //fldChar[1, j, i] := fldCharAntic45Ex[selectTiles, 1, j, i];

      fldCharAntic45[0, j, i] := fldCharAntic45Ex[selectTiles, 0, j, i];
      fldCharAntic45[1, j, i] := fldCharAntic45Ex[selectTiles, 1, j, i];*)
(*
      FillRectEx(imgChar0000, colTab[fldCharAntic45Ex[selectTiles, 0, j, i]],
                   (j - 1)*chrFactX + 1, i*chrFactY + 1, chrFactX shl 1 - 1, chrFactY - 1);
      FillRectEx(imgChar0100, colTab[fldCharAntic45Ex[selectTiles, 1, j, i]],
                   (j - 1)*chrFactX + 1, i*chrFactY + 1, chrFactX shl 1 - 1, chrFactY - 1);
//      for k := 0 to 15 do
//        fldCharAntic45[k, j, i] := fldCharAntic45Ex[selectTiles, k, j, i];
    end;
*)
  for i := 0 to 19 do
    for yf := 0 to _CHAR_DIM do
      for xf := 0 to _CHAR_DIM do
        case xf of
          0, 2, 4, 6: begin
            col := fldCharAntic45Ex[antic4Tile.selected, i, xf + 1, yf];
            fldCharAntic45[i, xf, yf] := col;
            fldCharAntic45[i, xf + 1, yf] := col;
            case col of
              0: begin
                fldChar[i, xf, yf] := 0;
                fldChar[i, xf + 1, yf] := 0;
              end;
              1: begin
                fldChar[i, xf, yf] := 0;
                fldChar[i, xf + 1, yf] := 1;
              end;
              2: begin
                fldChar[i, xf, yf] := 1;
                fldChar[i, xf + 1, yf] := 0;
              end;
              3, 10: begin
                fldChar[i, xf, yf] := 1;
                fldChar[i, xf + 1, yf] := 1;
              end;
            end;
          end;
        end;

//  RefreshData;

  editXLeave(Sender);
end;

procedure TfrmAntic4Tiles.UndoProc(Sender : TObject);
var
  x, y : byte;
begin
  //for i := 0 to 19 do
  //  for y := 0 to _CHAR_DIM do
  //    for x := 0 to _CHAR_DIM do
  //      fldChar[i, x, y] := fldCharMem[0, i, x, y];

//  fldCharMemEx[0, selectTile, 1] := fldChar[selectTile, xf + 1, yf];

  //fldCharMemEx[0, selectTile, 0] := fldChar[selectTile, xf - 1, yf];
  //fldCharMemEx[0, selectTile, 1] := xf - 1;
  //fldCharMemEx[0, selectTile, 2] := yf;
  //fldCharMemEx[0, selectTile, 3] := fldChar[selectTile, xf, yf];
  //fldCharMemEx[0, selectTile, 4] := xf;
  //fldCharMemEx[0, selectTile, 5] := yf;

//  for i := 0 to 19 do
//    fldChar[i, x, y] := fldCharMemEx[0, i, y];

  //undoValues[0].tileSelected := selectTile;
  //undoValues[0].x := xf - 1;
  //undoValues[0].y := yf;
  //undoValues[0].value := fldChar[selectTile, xf - 1, yf];
  //undoValues[1].tileSelected := selectTile;
  //undoValues[1].x := xf;
  //undoValues[1].y := yf;
  //undoValues[1].value := fldChar[selectTile, xf, yf];

  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      redoValuesEx[selectTile, x, y] := fldChar[selectTile, x, y];

  if btnDraw.Down then begin
    fldChar[undoValues[0].tileSelected, undoValues[0].x, undoValues[0].y] := undoValues[0].value;
    fldChar[undoValues[1].tileSelected, undoValues[1].x, undoValues[1].y] := undoValues[1].value;
  end
  else begin
    for y := 0 to _CHAR_DIM do
      for x := 0 to _CHAR_DIM do
        fldChar[selectTile, x, y] := undoValuesEx[selectTile, x, y];
  end;

  UpdateColors;
  RefreshGrid;
  RefreshTiles(selectTile);
end;

procedure TfrmAntic4Tiles.RedoProc(Sender : TObject);
var
  x, y : byte;
begin
  //for y := 0 to _CHAR_DIM do
  //  for x := 0 to _CHAR_DIM do
  //    undoValuesEx[selectTile, x, y] := redoValuesEx[selectTile, x, y];

  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      fldChar[selectTile, x, y] := redoValuesEx[selectTile, x, y];

  UpdateColors;
  RefreshGrid;
  RefreshTiles(selectTile);
end;

procedure TfrmAntic4Tiles.LoadDefaultColorsProc(Sender : TObject);
begin
  frmMain.LoadDefaultPalette;
  RefreshGrid;
end;

procedure TfrmAntic4Tiles.ShowGridProc(Sender : TObject);
begin
  case TMenuItem(Sender).Tag of
    0: isShowGrid := true;
    1: isShowGrid := false;
  end;
  RefreshData;
end;

procedure TfrmAntic4Tiles.paintBoxDown(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
var
  i, j : word;
  xx, yy : byte;
begin
  btn := Button;
  coordX := X div factX;
  coordY := Y div factY;

//  statusBar.Panels[1].Text := 'xy : ' + inttostr(coordX) + ', ' + inttostr(coordY);

  //// Data is changed
  //if not isEdit then begin
  //  isEdit := true;
  //  if Pos(' *', caption) = 0 then
  //    caption := caption + ' *';
  //end;

  // Calculate Atari matrix coordinates for Antic mode 4
  for j := 0 to 23 do begin
    for i := 0 to 39 do
      if (coordX >= (i shl 3)*editX.Value) and (coordX < ((i + 1) shl 3)*editX.Value) then begin
        if j = 0 then begin
          if i > 0 then
            coordX := (i shl 4)*editX.Value
          else
            coordX := 0;
        end;
        xx := i;
        break;
      end;

    if (coordY >= (j shl 3)*editY.Value) and (coordY < ((j + 1) shl 3)*editY.Value) then begin
      coordY := (j shl 4)*editY.Value;
      yy := j;
      break;
    end;
  end;

//  if xx + yy*(maxX + 1) > maxSize then Exit;
  statusBar.Panels[1].Text := 'Cursor coordinates (x: ' +
                              inttostr(xx) + ', y: ' + inttostr(yy) + ')';

  //offset2 := offs2;
  //offset := offset2 shl 3;
  //if not isFontSetNormal then
  //  Inc(offset2, 128);

//  fldAtascii[xx + yy*(maxX + 1)] := offset2;

  paintBoxPaint(imageObject as TImage);

  //imgTile01.Picture := nil; //<-- add this.
  //imgTile01.SetBounds(imgTile01.Left, imgTile01.Top, 14, 14);
  //imgTile01.Invalidate;
//  paintBoxPaint(imgTile01);
end;

//constructor Icon.Create(X, Y: Integer);
//var Bitmap:TBitmap;
//begin
//  Bitmap:=TBitmap.Create;
//  try
//    Bitmap.Height := IconSize;
//    Bitmap.Width := IconSize;
//    Bitmap.Canvas.Pen.Color := clBlack;
//    Bitmap.Canvas.Rectangle(Round(X-(IconSize/2)), Round(Y-(IconSize/2)),
//      Round(X+(IconSize/2)), Round(Y+(IconSize/2)));
//    PaintBox.Canvas.Draw(0, 0, Bitmap);
//  finally
//    Bitmap.Free;
//  end;
//end;

procedure TfrmAntic4Tiles.paintBoxPaint(Sender : TObject);
begin
  if not (sender is TImage) then exit;

//  selectTile := TImage(Sender).Tag;

//  case selectTiles of
//    1: begin
////    paintBox.Canvas.Brush.Color := clRed;
////    paintBox.Canvas.Rectangle(10, 10, 30, 30);
//      paintBox.Canvas.Draw(0, 0, imgtile01.Picture.Bitmap);
//    end;
//  end;

  //antic4Tile.selected := TImage(Sender).Tag;
  //antic4Tile.dimX := antic4TileArray[antic4Tile.selected].dimX;
  //antic4Tile.dimY := antic4TileArray[antic4Tile.selected].dimY;

//  Bitmap:=TBitmap.Create;
//  try
//    Bitmap.Assign(TImage(Sender).Picture.Bitmap);
//    Bitmap.Height := 30;
//    Bitmap.Width := 30;
////    Bitmap.Canvas.Pen.Color := clBlack;
////    Bitmap.Canvas.Rectangle(Round(X-(IconSize/2)), Round(Y-(IconSize/2)),
////      Round(X+(IconSize/2)), Round(Y+(IconSize/2)));
//    paintBox.Canvas.Draw(coordX, coordY, Bitmap);
//  finally
//    Bitmap.Free;
//  end;

//paintBox.Canvas.CopyRect(
  //  Rect(coordX,coordY,TImage(Sender).Picture.Width,TImage(Sender).Picture.Height),
  //  TImage(Sender).Canvas,
  //  Rect(
  //    0, 0, TImage(Sender).Picture.Width-40, TImage(Sender).Picture.Height-40));

  paintBox.Canvas.Draw(coordX, coordY, TImage(Sender).Picture.Bitmap);
end;

procedure TfrmAntic4Tiles.btnFlipXClick(Sender : TObject);
begin
  shapeTileGrid.Visible := false;
end;

procedure TfrmAntic4Tiles.ViewerProc(Sender : TObject);
begin
  ShowMessage('Not implemented yet!');
end;

procedure TfrmAntic4Tiles.ShowTilesDim;
var
  i : byte;

function CalcBytes(dim : byte) : string;
begin
  result := IntToStr(dim shl 3);
end;

begin
  for i := 1 to _MAX_TILES do
    (FindComponent('lblTiles0' + IntToStr(i) + 'Dim') as TLabel).Caption :=
      IntToStr(antic4TileArray[i].dimX) + ' x' + IntToStr(antic4TileArray[i].dimY) + #13#10 +
      '(' + CalcBytes(antic4TileArray[i].dimX) + ' x' + CalcBytes(antic4TileArray[i].dimY) + ')';
end;

procedure TfrmAntic4Tiles.RefreshCharX(imgChar : TImage; offset : integer);
var
  col : byte;
  xf, yf : integer;
begin
  FillRectEx(imgChar, coltabFont[0], 0, 0, imgChar.Width, imgChar.Height);
//  FillRectEx(imgCharAntic45, colTab[0], 0, 0, imgChar.Width, imgChar.Height);
  offset := offset shl 3;
  for yf := 0 to _CHAR_DIM do
    for xf := 0 to _CHAR_DIM do begin
      // Show pixel of selected character
      col := fldFontSet[xf, yf + offset];
//      if not isFontSetNormal then
//        col := 1 - col;

      FillRectEx(imgChar, coltabFont[col], xf shl 1, yf shl 1, 2, 2);

      // Show pixel of edited character
//      col := fldFontSet[xf, yf + offset];
//      fldChar[xf, yf] := col;
//      FillRectEx(imgChar, coltabFont[col], xf*chrFactX, yf*chrFactY, chrFactX, chrFactX);
(*
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
      end;*)
    end;

  imgChar.Refresh;
//  imgCharOrig.Refresh;
end;

procedure TfrmAntic4Tiles.ShowTileChars;
begin
  RefreshCharX(imgTile01Char0000, 64);
  RefreshCharX(imgTile01Char0100, 65);
  RefreshCharX(imgTile01Char0010, 66);
  RefreshCharX(imgTile01Char0001, 67);
  RefreshCharX(imgTile01Char1000, 68);
  RefreshCharX(imgTile01Char1100, 69);
  RefreshCharX(imgTile01Char1010, 70);
  RefreshCharX(imgTile01Char1001, 71);
  RefreshCharX(imgTile01Char2000, 72);
  RefreshCharX(imgTile01Char2100, 73);
  RefreshCharX(imgTile01Char2010, 74);
  RefreshCharX(imgTile01Char2001, 75);
  RefreshCharX(imgTile01Char3000, 76);
  RefreshCharX(imgTile01Char3100, 77);
  RefreshCharX(imgTile01Char3010, 78);
  RefreshCharX(imgTile01Char3001, 79);
  RefreshCharX(imgTile01Char4000, 80);
  RefreshCharX(imgTile01Char4100, 81);
  RefreshCharX(imgTile01Char4010, 82);
  RefreshCharX(imgTile01Char4001, 83);

  RefreshCharX(imgTile02Char0000, 84);
  RefreshCharX(imgTile02Char0100, 85);
  RefreshCharX(imgTile02Char0010, 86);
  RefreshCharX(imgTile02Char0001, 87);
  RefreshCharX(imgTile02Char1000, 88);
  RefreshCharX(imgTile02Char1100, 89);
  RefreshCharX(imgTile02Char1010, 90);
  RefreshCharX(imgTile02Char1001, 91);
  RefreshCharX(imgTile02Char2000, 92);
  RefreshCharX(imgTile02Char2100, 93);
  RefreshCharX(imgTile02Char2010, 94);
  RefreshCharX(imgTile02Char2001, 95);
  RefreshCharX(imgTile02Char3000, 96);
  RefreshCharX(imgTile02Char3100, 97);
  RefreshCharX(imgTile02Char3010, 98);
  RefreshCharX(imgTile02Char3001, 99);
  RefreshCharX(imgTile02Char4000, 100);
  RefreshCharX(imgTile02Char4100, 101);
  RefreshCharX(imgTile02Char4010, 102);
  RefreshCharX(imgTile02Char4001, 103);

  RefreshCharX(imgTile03Char0000, 104);
  RefreshCharX(imgTile03Char0100, 105);
  RefreshCharX(imgTile03Char0010, 106);
  RefreshCharX(imgTile03Char0001, 107);
  RefreshCharX(imgTile03Char1000, 108);
  RefreshCharX(imgTile03Char1100, 109);
  RefreshCharX(imgTile03Char1010, 110);
  RefreshCharX(imgTile03Char1001, 111);
  RefreshCharX(imgTile03Char2000, 112);
  RefreshCharX(imgTile03Char2100, 113);
  RefreshCharX(imgTile03Char2010, 114);
  RefreshCharX(imgTile03Char2001, 115);
  RefreshCharX(imgTile03Char3000, 116);
  RefreshCharX(imgTile03Char3100, 117);
  RefreshCharX(imgTile03Char3010, 118);
  RefreshCharX(imgTile03Char3001, 119);
  RefreshCharX(imgTile03Char4000, 120);
  RefreshCharX(imgTile03Char4100, 121);
  RefreshCharX(imgTile03Char4010, 122);
  RefreshCharX(imgTile03Char4001, 123);
end;

procedure TfrmAntic4Tiles.CloseWinProc(Sender : TObject);
begin
  Close;
end;

end.
(*
if inttostr(fldChar[index, 0, i]) +
            inttostr(fldChar[index, 1, i]) +
            inttostr(fldChar[index, 2, i]) +
            inttostr(fldChar[index, 3, i]) +
            inttostr(fldChar[index, 4, i]) +
            inttostr(fldChar[index, 5, i]) +
            inttostr(fldChar[index, 6, i]) +
            inttostr(fldChar[index, 7, i]) <> '00000000' then
showmessage(inttostr(fldChar[index, 0, i]) +
            inttostr(fldChar[index, 1, i]) +
            inttostr(fldChar[index, 2, i]) +
            inttostr(fldChar[index, 3, i]) +
            inttostr(fldChar[index, 4, i]) +
            inttostr(fldChar[index, 5, i]) +
            inttostr(fldChar[index, 6, i]) +
            inttostr(fldChar[index, 7, i]));
            *)
