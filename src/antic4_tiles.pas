{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Antic mode 4 and 5 tile editor
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
    btnSelectTile : TToolButton;
    btnGenCode : TToolButton;
    btnFillTile : TToolButton;
    btnFlipX : TToolButton;
    btnFlipY : TToolButton;
    btnLoadScreen : TToolButton;
    btnRotate : TToolButton;
    btnSaveScreen : TToolButton;
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
    imgTile01 : TImage;
    imgTile02 : TImage;
    imgTile03 : TImage;
    imgTile04 : TImage;
    imgTile05 : TImage;
    imgTile06 : TImage;
    imgTile07 : TImage;
    imgTile08 : TImage;
    imgTile09 : TImage;
    itemClose : TMenuItem;
    itemColorPalette : TMenuItem;
    itemDefaultColorPalette : TMenuItem;
    itemHideGrid : TMenuItem;
    itemShowGrid : TMenuItem;
    itemViewer : TMenuItem;
    Label21 : TLabel;
    Label22 : TLabel;
    menuAntic4Tiles : TMainMenu;
    menuClearChar : TMenuItem;
    menuEdit : TMenuItem;
    menuFile : TMenuItem;
    menuInvert : TMenuItem;
    itemInvertTile : TMenuItem;
    MenuItem10 : TMenuItem;
    MenuItem11 : TMenuItem;
    itemClearTile : TMenuItem;
    MenuItem16 : TMenuItem;
    MenuItem17 : TMenuItem;
    MenuItem18 : TMenuItem;
    MenuItem19 : TMenuItem;
    itemCodeGen : TMenuItem;
    itemClearTiles : TMenuItem;
    itemFlipTileX : TMenuItem;
    itemFlipTileY : TMenuItem;
    itemRotateTile : TMenuItem;
    MenuItem3 : TMenuItem;
    MenuItem4 : TMenuItem;
    MenuItem5 : TMenuItem;
    itemOpenTile : TMenuItem;
    itemSaveTile : TMenuItem;
    itemSaveTileAs : TMenuItem;
    menuRestoreChar : TMenuItem;
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
    scrlBox : TScrollBox;
    statusBar : TStatusBar;
    toolBar : TToolBar;
    ToolButton17 : TToolButton;
    ToolButton2 : TToolButton;
    btnInvert : TToolButton;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure ClearTileProc(Sender : TObject);
    procedure CharOper(Sender : TObject);
    procedure ColorProc(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure FlipXProc(Sender : TObject);
    procedure FlipYProc(Sender : TObject);
    procedure ColorPaletteProc(Sender : TObject);
    procedure SelectTileProc(Sender : TObject);
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
//    procedure ColorProc(Sender : TObject; Shift : TShiftState; X, Y : Integer);
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
//    isEdit : boolean;
    isCreate : boolean;
    isShow : boolean;
    isCopyChar : boolean;
    isShowGrid : boolean;
    isSelectTile : boolean;
//    gridColor : TColor;
    mouseIsDown: boolean;
//    isNormalPlot : boolean;
    coorX, coorY : integer;
    imageObject : TObject;
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
    procedure RefreshScreen(index : byte);
    procedure PutChar(scrPos, y, offset : byte);
  public
    filename : string;
    fontName : string;
    fldFontSet : fldFontSetType;
    fldChar : array[0..19, 0..7, 0..7] of byte;
    fldCharEx : array[0..8, 0..19, 0..7, 0..7] of byte;
    fldCharAntic45 : array[0..19, 0..7, 0..7] of byte;
    fldCharAntic45Ex : array[0..8, 0..19, 0..7, 0..7] of byte;
//    fldAtascii : array[0.._ANTIC_MODE_4_SIZE - 1] of byte;
    inverseIndex : array[0..19] of boolean;
    cellIndex : array[0..19] of boolean;
    charColor : array[0..19] of byte;
    factX, factY,             // Character screen editor offset
    chrFactX, chrFactY,       // Character editor pixel offsets
    factX02, factY02 : byte;  // Font set character offsets
    maxX, maxY : byte;        // Maximum X and Y coordinates
    maxXX, maxYY : byte;      // Maximum X and Y coordinates
    maxSize : integer;
    anticMode : byte;
    modeHeight : byte;
    isTextModeChanged : boolean;
    selectTile : byte;
    charXOffset, charYOffset, charYOffset02 : word;
    selectTiles : byte;
    procedure RefreshData;
    procedure RefreshColors;
  end;

var
  frmAntic4Tiles : TfrmAntic4Tiles;

implementation

{$R *.lfm}

uses
  main, lib, colors;

{ TfrmAntic4Tiles }

procedure TfrmAntic4Tiles.FormCreate(Sender : TObject);
begin
  DoubleBuffered := true;
  btn := mbMiddle;
  isCreate := true;
  isShow := true;
  isCopyChar := false;
  isShowGrid := false;
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
begin
  propFlagModules[12] := 1;
  isChange := true;
  frmMain.Top := 10;
  formId := formAntic4TileEditor;
  filename := getDir + 'examples\screen01.tl4';
  caption := programName + ' ' + programVersion +
             ' - Antic mode 4 and 5 tile editor (' + filename + ')';

  modeHeight := 24;
  SetXY(3, 3);
  editX.Value := maxX;
  editY.Value := maxY;
  SetAnticMode(4);
  factX := 2;
  factX02 := 2;

  maxXX := 4;
  maxYY := 5;

  // Character editor parameters
  chrFactX := 14; chrFactY := 14;

  isFontSetNormal := true;
  DefaultFontSet(fldFontSet);
//  ShowBaseFontSet;

  btnSelectTile.Down := true;

  FillRectEx(imgTile01, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
  FillRectEx(imgTile02, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
  FillRectEx(imgTile03, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
  FillRectEx(imgTile04, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);

//  SetCell(imgChar0000, true);
//  SetCell(imgChar0100, true);

  //cellIndex[0] := true;
  //cellIndex[1] := true;
  //cellIndex[4] := true;
  //cellIndex[5] := true;
//  for i := 0 to SizeOf(cellIndex) - 1 do
//    cellIndex[i] := true;

  selectTile := 0;
  selectTiles := 1;
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

  //fldAtascii[4 + 6*(maxX + 1)] := 97;
  //fldAtascii[5 + 6*(maxX + 1)] := 97;
end;

procedure TfrmAntic4Tiles.SetCell(image : TImage; isActive : boolean);
begin
  image.Enabled := isActive;
//  image.Visible := isActive;
  if isActive then begin
    FillRectEx(image, colTab[0], 0, 0, image.Width, image.Height);
  end
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
    maxX := fs.ReadByte;
    maxY := fs.ReadByte;
//    debug('maxx, maxy', maxx, maxy);

    SetXY(maxx, maxy);
    editX.Value := maxX;
    editY.Value := maxY;
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
//    index := 0;
    for y := 0 to maxY - 1 do
      for x := 0 to maxX - 1 do begin
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
    RefreshColors;
//    RefreshScreen(selectTile);
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
    fs.WriteByte(maxX);
    fs.WriteByte(maxY);

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
    for y := 0 to maxY - 1 do begin
//      index := x;
      for x := 0 to maxX - 1 do begin
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

procedure TfrmAntic4Tiles.imgCharDown(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
var
  xf, yf : byte;
begin
  btn := Button;
  xf := X div chrFactX;
  yf := Y div chrFactY;

  mouseIsDown := True;
  selectTile := TImage(Sender).Tag;

  if ((maxY = 1) and (selectTile > 3)) or
     ((maxY = 2) and (selectTile > 7)) or
     ((maxY = 3) and (selectTile > 11)) or
     ((maxY = 4) and (selectTile > 15)) then begin
    exit;
  end;

  if btnSelectTile.Down then
    PlotCharAntic45(Sender, xf, yf);

//  charEditIndex[offs] := 1;
end;

procedure TfrmAntic4Tiles.imgCharUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  btn := mbMiddle;
  if MouseIsDown then begin
    if btnRotate.Down then begin
      RotateProc(Sender);
    end
    else if btnFlipX.Down then begin
      FlipXProc(Sender);
    end
    else if btnFlipY.Down then begin
      FlipYProc(Sender);
    end
    else if btnInvert.Down then begin
      InvertProc(Sender);
    end
    else if btnClearTile.Down then begin
      ClearTileProc(Sender);
    end
    else if btnFillTile.Down then begin
      FillTileProc(Sender);
    end
  end;
  MouseIsDown := false;

//  fldCharAntic45Ex
//  fldCharAntic45[selectTile, xf - 1, yf]
  //case selectTiles of
  //  1: begin
  //    for i := 0 to 19 do begin
  //    end;
  //  end;
  //  2: begin
  //  end;
  //end;
end;

{-----------------------------------------------------------------------------
 Draw Antic 4/5 pixel inside character editor
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.PlotCharAntic45(Sender : TObject; xf, yf : byte);
var
  col : byte;
//  index : byte;
begin
  //if isSelectTile then begin
  //  isSelectTile := false;
  //  ShowCursor(frmAntic4Tiles, frmAntic4Tiles, crDefault);
  //  Exit;
  //end;

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
  fldCharAntic45Ex[selectTiles, selectTile, xf, yf] := frmColors.SelColor;

  case xf of
    0, 2, 4, 6: begin
      if col = 0 then begin
        fldCharAntic45[TImage(Sender).Tag, xf + 1, yf] := 0;
        fldCharAntic45Ex[selectTiles, TImage(Sender).Tag, xf + 1, yf] := 0;
        FillRectEx(TImage(Sender), colTab[0], xf*chrFactX + 1, yf*chrFactY + 1,
                   chrFactX*2 - 1, chrFactY - 1);
        fldChar[selectTile, xf, yf] := 0;
        fldChar[selectTile, xf + 1, yf] := 0;
      end
      else begin
        fldCharAntic45[selectTile, xf + 1, yf] := frmColors.SelColor;
        fldCharAntic45Ex[selectTiles, selectTile, xf + 1, yf] := frmColors.SelColor;
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
      if col = 0 then begin
        fldCharAntic45[selectTile, xf - 1, yf] := 0;
        fldCharAntic45[TImage(Sender).Tag, xf - 1, yf] := 0;
        FillRectEx(TImage(Sender), colTab[0],
                   (xf - 1)*chrFactX + 1, yf*chrFactY + 1, chrFactX*2 - 1, chrFactY - 1);
        fldChar[selectTile, xf - 1, yf] := 0;
        fldChar[selectTile, xf, yf] := 0;
      end
      else begin
        fldCharAntic45[selectTile, xf - 1, yf] := frmColors.SelColor;
        fldCharAntic45Ex[selectTiles, selectTile, xf - 1, yf] := frmColors.SelColor;
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
  RefreshScreen(selectTile);
end;

// Refresh screen
procedure TfrmAntic4Tiles.RefreshScreen(index : byte);
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
    for i := 1 to 4 do
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

        case selectTiles of
          1: FillRectEx(imgTile01, colTab[col], (xf + j - 1)*factX, (yf + i)*factY,
                        factX shl 1, factY);
          2: FillRectEx(imgTile02, colTab[col], (xf + j - 1)*factX, (yf + i)*factY,
                        factX shl 1, factY);
          3: FillRectEx(imgTile03, colTab[col], (xf + j - 1)*factX, (yf + i)*factY,
                        factX shl 1, factY);
          4: FillRectEx(imgTile04, colTab[col], (xf + j - 1)*factX, (yf + i)*factY,
                        factX shl 1, factY);
        end;
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
  maxX := _maxX;
  maxY := _maxY;
  maxSize := (maxX + 1)*(maxY + 1) - 1;
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

  charXOffset := 32;
  if anticMode = 4 then begin
    modeHeight := 24;
    factY := 2;
    factY02 := 2;
    charYOffset := 28;
    charYOffset02 := 17;
  end
  else begin
    modeHeight := 48;
    factY := 4;
    factY02 := 4;
    charYOffset := 40;
    charYOffset02 := 32;
  end;

//  ShowFontSet;
end;

{-----------------------------------------------------------------------------
 Refresh screen data
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.RefreshData;
begin
  //FillRectEx(imgTile01, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
  //FillRectEx(imgTile02, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
  //FillRectEx(imgTile03, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
  //FillRectEx(imgTile04, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);

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

  MaskColor(imgChar0000, 0);
  MaskColor(imgChar0100, 1);
  MaskColor(imgChar0010, 2);
  MaskColor(imgChar0001, 3);

  MaskColor(imgChar1000, 4);
  MaskColor(imgChar1100, 5);
  MaskColor(imgChar1010, 6);
  MaskColor(imgChar1001, 7);

  MaskColor(imgChar2000, 8);
  MaskColor(imgChar2100, 9);
  MaskColor(imgChar2010, 10);
  MaskColor(imgChar2001, 11);

  MaskColor(imgChar3000, 12);
  MaskColor(imgChar3100, 13);
  MaskColor(imgChar3010, 14);
  MaskColor(imgChar3001, 15);

  MaskColor(imgChar4000, 16);
  MaskColor(imgChar4100, 17);
  MaskColor(imgChar4010, 18);
  MaskColor(imgChar4001, 19);

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

  for i := 0 to _CHAR_DIM do
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

        FillRectEx(image, colTab[col], (j - 1)*chrFactX + 1, i*chrFactY + 1,
                   chrFactX shl 1 - 1, chrFactY - 1);
        image.Canvas.Pen.Color := clGray;
        image.Canvas.Rectangle((j - 1)*chrFactX, i*chrFactY,
                               (j - 1)*chrFactX + 1 + chrFactX*2,
                               i*chrFactY + chrFactY + 1);
        cnt := 0;
      end;
    end;

//  image.Canvas.Pen.Color := clSkyBlue;
//  image.Canvas.Rectangle(0, 0, image.Width, image.Height);
  image.Invalidate;
end;

procedure TfrmAntic4Tiles.RefreshColors;
begin
  RefreshData;
  ColorRegisters;
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
  frmColors.SelColor := TShape(Sender).Tag;
  ColorRegisters;
  case frmColors.SelColor of
    0: begin
      color4.Canvas.Pen.Color := clRed;
      color4.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
    1: begin
      color0.Canvas.Pen.Color := clRed;
      color0.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
    2: begin
      color1.Canvas.Pen.Color := clRed;
      color1.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
    3: begin
//      inverseIndex[selectTile] := false;
      color2.Canvas.Pen.Color := clRed;
      color2.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
    10: begin
//      inverseIndex[selectTile] := true;
      color3.Canvas.Pen.Color := clRed;
      color3.Canvas.Rectangle(0, 0, color1.width, color1.Height);
    end;
  end;
end;

procedure TfrmAntic4Tiles.editXLeave(Sender : TObject);
var
  i, j : byte;
begin
  btn := mbMiddle;
  if not isCreate then
    SetXY(editX.Value, editY.Value);

  maxX := editX.Value;
  maxY := editY.Value;

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
  //SetCell(imgChar0000, false);

  //if editX.Value = 1 then
  //  SetCell(imgChar0000, true)
  //SetCell(imgChar0000, true);
  //if editY.Value <= 2 then begin
  //  SetCell(imgChar1000, true);
  //end;

  btn := mbMiddle;
  if not isCreate then
    SetXY(editX.Value, editY.Value);

  maxX := editX.Value;
  maxY := editY.Value;

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

  RefreshData;
  RefreshScreen(selectTile);
end;

procedure TfrmAntic4Tiles.GenCodeProc(Sender : TObject);
begin
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

  RefreshColors;
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

  RefreshColors;
  RefreshScreen(selectTile);
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

  RefreshColors;
  RefreshScreen(selectTile);
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

  RefreshColors;
  RefreshScreen(selectTile);
end;

procedure TfrmAntic4Tiles.ClearTilesProc(Sender : TObject);
begin
  case selectTiles of
    1: FillRectEx(imgTile01, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
    2: FillRectEx(imgTile02, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
    3: FillRectEx(imgTile03, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
    4: FillRectEx(imgTile04, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
  end;

//  FillByte(fldAtascii, SizeOf(fldAtascii), 0);
  FillByte(fldCharAntic45, SizeOf(fldCharAntic45), 0);
  FillByte(fldChar, SizeOf(fldChar), 0);
  FillByte(inverseIndex, SizeOf(inverseIndex), 0);
  SetCells;
//  RefreshData;
end;

{-----------------------------------------------------------------------------
 Shift tile left
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftLeftProc(Sender: TObject);
var
  x, y, n : byte;
begin
  for y := 0 to _CHAR_DIM do begin
    n := fldChar[selectTile, 0, y];
    for x := 1 to _CHAR_DIM do
      fldChar[selectTile, x - 1, y] := fldChar[selectTile, x, y];

    fldChar[selectTile, 7, y] := n;
  end;
end;

{-----------------------------------------------------------------------------
 Shift tile right
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftRightProc(Sender: TObject);
var
  x, y, n : byte;
begin
  for y := 0 to _CHAR_DIM do begin
    n := fldChar[selectTile, 7, y];
    for x := _CHAR_DIM - 1 downto 0 do
      fldChar[selectTile, x + 1, y] := fldChar[selectTile, x, y];

    fldChar[selectTile, 0, y] := n;
  end;
end;

{-----------------------------------------------------------------------------
 Shift tile up
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftUpProc(Sender: TObject);
var
  x, y : byte;
  fld02 : array[0..7] of byte;
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

//  RefreshColors;
end;

{-----------------------------------------------------------------------------
 Shift tile down
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftDownProc(Sender: TObject);
var
  x, y : byte;
  fld02 : array[0..7] of byte;
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
  for y := 0 to _CHAR_DIM do begin
//    n := fldChar[selectTile, 0, y];
    for x := 1 to _CHAR_DIM do begin
      fldChar[selectTile, x - 1, y] := fldChar[selectTile, x, y];
      fldChar[selectTile, x, y] := 0;
    end;
//    fldChar[selectTile, 7, y] := n;
  end;
//begin
//  MoveLeft(chrX, chrY, fldChar);
end;

//{-----------------------------------------------------------------------------
// Move character right
// -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.MoveRightProc(Sender: TObject);
var
  x, y : byte;
begin
  for y := 0 to _CHAR_DIM do begin
//    n := fldChar[selectTile, 7, y];
    for x := _CHAR_DIM - 1 downto 0 do begin
      fldChar[selectTile, x + 1, y] := fldChar[selectTile, x, y];
      fldChar[selectTile, x, y] := 0;
    end;
//    fldChar[selectTile, 0, y] := n;
  end;
//begin
//  MoveRight(chrX, chrY, fldChar);
end;

{-----------------------------------------------------------------------------
 Move character up
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.MoveUpProc(Sender: TObject);
var
  x, y : byte;
//  fld02 : array[0..7] of byte;
begin
  //for x := 0 to _CHAR_DIM do
  //  fld02[x] := fldChar[selectTile, x, 0];

  for x := 0 to _CHAR_DIM do
    for y := 1 to _CHAR_DIM do begin
      fldChar[selectTile, x, y - 1] := fldChar[selectTile, x, y];
      fldChar[selectTile, x, y] := 0;
    end;

  //for x := 0 to _CHAR_DIM do
  //  fldChar[selectTile, x, _CHAR_DIM] := fld02[x];

//  RefreshColors;
//begin
//  MoveUp(chrX, chrY, fldChar);
end;

{-----------------------------------------------------------------------------
 Move character down
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.MoveDownProc(Sender: TObject);
var
  x, y : byte;
//  fld02 : array[0..7] of byte;
begin
  //for x := 0 to _CHAR_DIM do
  //  fld02[x] := fldChar[selectTile, x, _CHAR_DIM];

  for x := 0 to _CHAR_DIM do
    for y := _CHAR_DIM - 1 downto 0 do begin
      fldChar[selectTile, x, y + 1] := fldChar[selectTile, x, y];
      fldChar[selectTile, x, y] := 0;
    end;

  //for x := 0 to _CHAR_DIM do
  //  fldChar[selectTile, x, 0] := fld02[x];
//begin
//  MoveDown(chrX, chrY, fldChar);
end;

procedure TfrmAntic4Tiles.CharOper(Sender : TObject);
begin
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
  RefreshColors;
  RefreshScreen(selectTile);
end;

procedure TfrmAntic4Tiles.ClearTileProc(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      fldChar[selectTile, x, y] := 0;

  RefreshColors;
  RefreshScreen(selectTile);
//  editXLeave(Sender);
end;

procedure TfrmAntic4Tiles.FillTileProc(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do begin
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
    end;

  inverseIndex[selectTile] := frmColors.SelColor = 10;
  RefreshColors;
  RefreshScreen(selectTile);
end;

procedure TfrmAntic4Tiles.ColorPaletteProc(Sender : TObject);
begin
  frmColors.Show;
end;

procedure TfrmAntic4Tiles.SelectTileProc(Sender : TObject);
begin
  selectTiles := TImage(Sender).Tag;
  imageObject := Sender;
//  paintBoxPaint(Sender);
end;

procedure TfrmAntic4Tiles.LoadDefaultColorsProc(Sender : TObject);
begin
  frmMain.LoadDefaultPalette;
  RefreshColors;
end;

procedure TfrmAntic4Tiles.ShowGridProc(Sender : TObject);
begin

end;

procedure TfrmAntic4Tiles.paintBoxDown(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
var
//  xf, yf : integer;
//  xx, yy : byte;
  i, j : byte;
begin
  btn := Button;
  coorX := X div factX;
  coorY := Y div factY;
//  coorX := x;
//  coorY := y;

  //// Data is changed
  //if not isEdit then begin
  //  isEdit := true;
  //  if Pos(' *', caption) = 0 then
  //    caption := caption + ' *';
  //end;

  //// Plot character
  //Plot(xf, yf);

  for j := 0 to 10 do begin
    for i := 0 to 10 do
      if (coorX >= i shl 3) and (coorX < (i + 1) shl 3) then begin
        coorX := i shl 3;
//        xx := i;
        break;
      end;

    if (coorY > j shl 3) and (coorY <= (j + 1) shl 3) then begin
      coorY := j shl 3;
//      yy := j;
      break;
    end;
  end;

  debug('coorx, coory', coorx, coory);
//  if xx + yy*(maxX + 1) > maxSize then Exit;

  paintBoxPaint(imageObject);
end;

procedure TfrmAntic4Tiles.paintBoxPaint(Sender : TObject);
begin
//  debug('paintBoxPaint');
//  if isCreate or isShow then exit;
//  if TImage(Sender)
  if not (sender is TImage) then exit;

//  selectTile := TImage(Sender).Tag;

//  case selectTiles of
//    1: begin
////    paintBox.Canvas.Brush.Color := clRed;
////    paintBox.Canvas.Rectangle(10, 10, 30, 30);
//      paintBox.Canvas.Draw(0, 0, imgtile01.Picture.Bitmap);
//    end;
//    2: begin
//  //  paintBox.Canvas.Brush.Color := clBlue;
//  //  paintBox.Canvas.Rectangle(30, 30, 70, 70);
////    paintBox.Canvas.Draw(30, 30, imgtile2.Picture.Bitmap);
//      paintBox.Canvas.Draw(0, 0, imgtile0.Picture.Bitmap);
//    end;
//  end;

  paintBox.Canvas.Draw(coorX, coorY, TImage(Sender).Picture.Bitmap);
end;
(*
{-----------------------------------------------------------------------------
 Draw character
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.Plot(xf, yf : integer);
var
  xx, yy : byte;
  offset, offset2, offs2 : integer;
  i, j : byte;
begin
  if xf > 7 then xf := 7;
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

procedure TfrmAntic4Tiles.MaskColor(x, y, offset : integer; isInverse : boolean);
var
  cnt : byte = 0;
  i, j, col : byte;
  mask : array[0..1] of byte;
begin
  for i := 0 to _CHAR_DIM do
    for j := 0 to _CHAR_DIM do begin
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
*)
procedure TfrmAntic4Tiles.CloseWinProc(Sender : TObject);
begin
  Close;
end;

end.

