{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2023
  Unit: Antic mode 4 tile editor
}
unit antic4_tiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StrUtils,
  ExtCtrls, Menus, ComCtrls, StdCtrls, Buttons, SpinEx, lcltype,
  common;

type
  { TfrmAntic4Tiles }
  TfrmAntic4Tiles = class(TForm)
    boxResize : TGroupBox;
    btnCharDown : TSpeedButton;
    btnCharLeft : TSpeedButton;
    btnCharRight : TSpeedButton;
    btnCharUp : TSpeedButton;
    btnClearTileChar : TToolButton;
    btnClearTile : TToolButton;
    btnGenCode : TToolButton;
    btnFillTileChar : TToolButton;
    btnFlipX : TToolButton;
    btnFlipY : TToolButton;
    btnLoadScreen : TToolButton;
    btnRotateTileChar : TToolButton;
    btnSaveScreen : TToolButton;
    btnDraw : TToolButton;
    btnViewer : TToolButton;
    chkTileDimAuto : TCheckBox;
    cmbAnticMode : TComboBox;
    color0 : TImage;
    color1 : TImage;
    color2 : TImage;
    color3 : TImage;
    color4 : TImage;
    editY : TSpinEditEx;
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
    itemClose : TMenuItem;
    itemColorPalette : TMenuItem;
    itemDefaultColorPalette : TMenuItem;
    itemHideGrid : TMenuItem;
    itemShowGrid : TMenuItem;
    itemViewer : TMenuItem;
    Label1 : TLabel;
    lblFunc : TLabel;
    lblTile01Dim : TLabel;
    lblTile02Dim : TLabel;
    lblTile03Dim : TLabel;
    lblTile04Dim : TLabel;
    lblTileRepo : TLabel;
    lblTileMatrix : TLabel;
    lblTileMatrix1 : TLabel;
    lblTiles01Dim : TLabel;
    lblTiles02Dim : TLabel;
    lblTiles03Dim : TLabel;
    lblTile08Dim : TLabel;
    lblTile07Dim : TLabel;
    lblTile06Dim : TLabel;
    lblTile05Dim : TLabel;
    lblTiles04Dim : TLabel;
    Label21 : TLabel;
    Label22 : TLabel;
    menuAntic4Tiles : TMainMenu;
    itemUndo : TMenuItem;
    itemTileDraw : TMenuItem;
    itemFillTiles : TMenuItem;
    itemClearScreen : TMenuItem;
    MenuItem1 : TMenuItem;
    itemResizeTiles : TMenuItem;
    MenuItem4 : TMenuItem;
    MenuItem5 : TMenuItem;
    MenuItem6 : TMenuItem;
    MenuItem7 : TMenuItem;
    popFillTile : TMenuItem;
    popClearScreen : TMenuItem;
    MenuItem3 : TMenuItem;
    popClearTilesAll : TMenuItem;
    popCopyTileToAll : TMenuItem;
    popClearTiles : TMenuItem;
    MenuItem2 : TMenuItem;
    itemRedo : TMenuItem;
    popClearTile : TMenuItem;
    menuEdit : TMenuItem;
    menuFile : TMenuItem;
    popInvertTileChar : TMenuItem;
    itemInvertTile : TMenuItem;
    itemFillTile : TMenuItem;
    popDrawTileChar : TMenuItem;
    popFillTileChar : TMenuItem;
    popRotateTileChar : TMenuItem;
    itemClearTileChar : TMenuItem;
    MenuItem16 : TMenuItem;
    MenuItem17 : TMenuItem;
    MenuItem18 : TMenuItem;
    popDelim02 : TMenuItem;
    itemCodeGen : TMenuItem;
    itemClearTile : TMenuItem;
    itemFlipTileX : TMenuItem;
    itemFlipTileY : TMenuItem;
    itemRotateTile : TMenuItem;
    popFlipTileCharX : TMenuItem;
    popFlipTileCharY : TMenuItem;
    itemOpenTile : TMenuItem;
    itemSaveTile : TMenuItem;
    itemSaveTileAs : TMenuItem;
    menuTools : TMenuItem;
    menuView : TMenuItem;
    paintBox : TPaintBox;
    popHideGrid : TMenuItem;
    popMenu : TPopupMenu;
    popShowGrid : TMenuItem;
    radMoveChar : TRadioButton;
    radShiftChar : TRadioButton;
    radShiftTile : TRadioButton;
    scrollBox : TScrollBox;
    shape : TShape;
    shapeTileGrid : TShape;
    shapeEditor : TShape;
    editX : TSpinEditEx;
    statusBar : TStatusBar;
    toolBar : TToolBar;
    btnSelectTileChar : TToolButton;
    btnUndo : TToolButton;
    btnRedo : TToolButton;
    ToolButton17 : TToolButton;
    ToolButton18 : TToolButton;
    ToolButton19 : TToolButton;
    ToolButton2 : TToolButton;
    btnInvertTileChar : TToolButton;
    procedure FormCreate(Sender : TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender : TObject);
    procedure FormActivate(Sender : TObject);
    procedure FormDeactivate(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure CopyTileToAllProc(Sender : TObject);
    procedure itemTileDrawProc(Sender : TObject);
    procedure MenuItem5Click(Sender : TObject);
    procedure CopySelectedTileCharProc(Sender : TObject);
    procedure PasteTileCharProc(Sender : TObject);
    procedure MiscOper(Sender : TObject);
    procedure ClearTileCharProc(Sender : TObject);
    procedure CharOper(Sender : TObject);
    procedure ColorDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure FlipTileCharXProc(Sender : TObject);
    procedure FlipTileCharYProc(Sender : TObject);
    procedure FillTileProc(Sender : TObject);
    procedure ColorPaletteProc(Sender : TObject);
    procedure ClearScreenProc(Sender : TObject);
    procedure paintBoxMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure paintBoxUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure ClearTilesProc(Sender : TObject);
    procedure popFunc(Sender : TObject);
    procedure SelectTileProc(Sender : TObject);
    procedure UndoProc(Sender : TObject);
    procedure RedoProc(Sender : TObject);
    procedure ViewerProc(Sender : TObject);
    procedure ShowGridProc(Sender : TObject);
    procedure LoadDefaultColorsProc(Sender : TObject);
    procedure paintBoxDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure paintBoxPaint(Sender : TObject);
    procedure RotateTileCharProc(Sender : TObject);
    procedure InvertTileCharProc(Sender : TObject);
    procedure GenCodeProc(Sender : TObject);
    procedure ClearTileProc(Sender : TObject);
    procedure FillTileCharProc(Sender : TObject);
    procedure editXLeave(Sender : TObject);
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

    procedure ShiftTileDownProc(Sender: TObject);
    procedure ShiftTileLeftProc(Sender: TObject);
    procedure ShiftTileRightProc(Sender: TObject);
    procedure ShiftTileUpProc(Sender: TObject);

    procedure MoveDownProc(Sender: TObject);
    procedure MoveLeftProc(Sender: TObject);
    procedure MoveRightProc(Sender: TObject);
    procedure MoveUpProc(Sender: TObject);
    procedure MenuOper(Sender : TObject);
    procedure OnMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure ResizeTilesProc(Sender : TObject);
  private
    btn : TMousebutton;
    isSaveAs : boolean;
    isCreate : boolean;
    isShow : boolean;
    isCopyChar : boolean;
    isShowGrid : boolean;
    isSelectTile : boolean;
    isSizeEdit : boolean;
    mouseIsDown: boolean;
    coordX, coordY : integer;
    imageObject : TObject;
    fld02 : array[0..7] of byte;
    isPaintBoxMove : boolean;
    isLoadTile : boolean;
    undoValues : array[0..2] of TAntic4CharValueType;  // Undo values for normal draw
    undoValuesEx : array[0.._MAX_TILE_CHARS - 1, 0..7, 0..7] of byte;   // Undo values for misc draw
    redoValuesEx : array[0.._MAX_TILE_CHARS - 1, 0..7, 0..7] of byte;   // Redo values
    procedure SetXY(_maxX, _maxY : byte);
    procedure SetAnticMode(mode : byte);
    procedure PlotCharAntic45(Sender : TObject; xf, yf : byte);
    procedure ColorRegisters;
    procedure OpenFile(filename : string);
    procedure MaskColor(image : TImage; index : byte);
    procedure RefreshChar(image : TImage; index, selColor : byte);
    procedure SetCell(image : TImage; isActive : boolean);
    procedure SetCells;
    procedure RefreshTiles;
    procedure DrawTile(scrPos, y, offset : byte);
    procedure ShowTilesDim;
    procedure RefreshCharX(imgChar : TImage; offset : integer);
    procedure ShowTileChars;
    procedure ColorProc(image : TImage);
    procedure CreateObjects(index : byte);
    procedure RefreshScreen;
  public
    fldFontSet : fldFontSetType;
    fldChar : array[0.._MAX_TILE_CHARS - 1, 0..7, 0..7] of byte;
    fldCharAntic45 : array[0.._MAX_TILE_CHARS - 1, 0..7, 0..7] of byte;
    fldCharCopy : array[0..7, 0..7] of byte;
    fldCharAntic45Copy : array[0.._MAX_TILE_CHARS - 1, 0..7, 0..7] of byte;
    fldCharAntic45Ex : array[1.._MAX_TILES, 0.._MAX_TILE_CHARS - 1, 0..7, 0..7] of byte;
    cellIndex : array[0.._MAX_TILE_CHARS - 1] of boolean;
    charColor : array[0.._MAX_TILE_CHARS - 1] of byte;
    factX, factY : byte;        // Character screen editor offset
    chrFactX, chrFactY : byte;  // Character editor pixel offsets
    maxXX, maxYY : byte;        // Maximum X and Y coordinates
    maxSize : integer;
    anticMode : byte;
    modeHeight : byte;
    //isTextModeChanged : boolean;
    selectTileChar : byte;         // Selected tile character
    antic4Tile : TAntic4TileType;  // Selected tile
    antic4TileArray : array[1.._MAX_TILES] of TAntic4TileType;
    tiles : array[0.._ANTIC_MODE_4_SIZE - 1] of TTileType;  // Tiles pointer
    charOffset : byte;
    isCharConfirm : boolean;
//    isTilesResizeProc : boolean;
    procedure RefreshGridData;
    procedure UpdateCharColors;
    procedure RefreshColors;
  end;

var
  frmAntic4Tiles : TfrmAntic4Tiles;

implementation

{$R *.lfm}

uses
  main, lib, colors, char_set, antic4_tiles_gen, viewer, set_values;

{ TfrmAntic4Tiles }

procedure TfrmAntic4Tiles.FormCreate(Sender : TObject);
var
  i : word;
begin
  DoubleBuffered := true;
  btn := mbMiddle;
  isCreate := true;
  isSizeEdit := false;
  isShow := true;
  isCopyChar := false;
  isShowGrid := true;
  isLoadTile := false;

  isPaintBoxMove := false;

  isSelectTile := false;
  FillByte(fldChar, SizeOf(fldChar), 0);
  FillByte(fldCharAntic45, SizeOf(fldCharAntic45), 0);
  FillByte(fldCharAntic45Ex, SizeOf(fldCharAntic45Ex), 0);

  // Clear tile pointers
  for i := 0 to _ANTIC_MODE_4_SIZE - 1 do begin
    tiles[i].tileIndex := 0;
    tiles[i].coordX := 0;
    tiles[i].coordY := 0;
    tiles[i].x := 0;
    tiles[i].y := 0;
  end;

  imgCharList.Clear;
  for i := 1 to _MAX_TILES do CreateObjects(i);
end;

procedure TfrmAntic4Tiles.FormShow(Sender : TObject);
var
  i, j : byte;
begin
  propFlagModules[12] := 1;
  frmMain.Top := 10;
  formId := formAntic4TileEditor;

  modeHeight := 24;
  SetXY(4, 5);
  editX.Value := antic4Tile.dimX;
  editY.Value := antic4Tile.dimY;
  SetAnticMode(4);
  factX := 2;

  // Set maximum tile dimensions
  maxXX := 4; maxYY := 5;

  // Tile character editor parameters
  chrFactX := 14; chrFactY := chrFactX;

  DefaultFontSet(fldFontSet);

  btnDraw.Down := true;

  FillRectEx(imgTile01, coltab[0], 0, 0, imgTile01.Width, imgTile01.Height);
  FillRectEx(imgTile02, coltab[0], 0, 0, imgTile02.Width, imgTile02.Height);
  FillRectEx(imgTile03, coltab[0], 0, 0, imgTile03.Width, imgTile03.Height);
  FillRectEx(imgTile04, coltab[0], 0, 0, imgTile04.Width, imgTile04.Height);
  FillRectEx(imgTile05, coltab[0], 0, 0, imgTile05.Width, imgTile05.Height);
  FillRectEx(imgTile06, coltab[0], 0, 0, imgTile06.Width, imgTile06.Height);
  FillRectEx(imgTile07, coltab[0], 0, 0, imgTile07.Width, imgTile07.Height);
  FillRectEx(imgTile08, coltab[0], 0, 0, imgTile08.Width, imgTile08.Height);

  shapeEditor.Brush.Color := colTab[0];

  selectTileChar := 0;
  antic4Tile.selected := 1;
  for i := 1 to _MAX_TILES do begin
    if i < 3 then begin
      antic4TileArray[i].dimX := 4;
      antic4TileArray[i].dimY := 5;
    end
    else if i < 5 then begin
      antic4TileArray[i].dimX := 3;
      antic4TileArray[i].dimY := 3;
    end
    else if i < 7 then begin
      antic4TileArray[i].dimX := 2;
      antic4TileArray[i].dimY := 2;
    end
    else begin
      antic4TileArray[i].dimX := 1;
      antic4TileArray[i].dimY := 1;
    end;

    for j := 0 to 19 do
      antic4TileArray[i].charInverse[j] := false;

    antic4TileArray[i].filename := getDir + 'examples\tile0' + IntToStr(i) + '.tl4';
  end;

  caption := programName + ' ' + programVersion +
             ' - Antic mode 4 tile editor (' + antic4TileArray[1].filename + ')';

  ColorRegisters;

  ShowTilesDim;
  SelectTileProc(imgTile02);
  SelectTileProc(imgTile03);
  SelectTileProc(imgTile04);
  SelectTileProc(imgTile05);
  SelectTileProc(imgTile06);
  SelectTileProc(imgTile07);
  SelectTileProc(imgTile08);
  SelectTileProc(imgTile01);
  ShowTileChars;

  ColorProc(color0);

  isCreate := false;

  lblFunc.Caption := 'Function: ' + _TILE_FUNCTION[1];
end;

procedure TfrmAntic4Tiles.FormActivate(Sender : TObject);
begin
  formId := formAntic4TileEditor;
end;

procedure TfrmAntic4Tiles.FormDeactivate(Sender : TObject);
begin
  FormStyle := fsNormal;
end;

procedure TfrmAntic4Tiles.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  propFlagModules[12] := 0;
  formId := formMain;
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

procedure TfrmAntic4Tiles.FillTileProc(Sender : TObject);
var
  i, x, y : byte;
begin
  for i := 0 to _MAX_TILE_CHARS - 1 do begin
    for y := 0 to _CHAR_DIM do
      for x := 0 to _CHAR_DIM do
        if (x = 1) or (x = 3) or (x = 5) or (x = 7) then begin
          fldChar[i, x - 1, y] := 0;
          fldChar[i, x, y] := 0;
          case frmColors.SelColor of
            1: begin
              fldChar[i, x, y] := 1;
            end;
            2: begin
              fldChar[i, x - 1, y] := 1;
            end;
            3, 10: begin
              fldChar[i, x - 1, y] := 1;
              fldChar[i, x, y] := 1;
            end;
          end;
        end;
  end;
  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
end;

{-----------------------------------------------------------------------------
 Load Antic 4 tile from file
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.OpenFile(filename : string);
var
  fs : TFileStream;
  x, y, i, j, r, index : byte;
  bin : string;
begin
//  ShowCursor(frmAntic4Tiles, frmAntic4Tiles, crHourGlass);
  Screen.BeginWaitCursor;

  filename := LowerCase(filename);
//  cmbAnticMode.ItemIndex := 0;
  SetAnticMode(4);

  fs := TFileStream.Create(filename, fmOpenReadWrite);
  try
    // Read tile character dimension boundaries (x, y)
    antic4Tile.dimX := fs.ReadByte;
    antic4Tile.dimY := fs.ReadByte;
    antic4TileArray[antic4Tile.selected].dimX := antic4Tile.dimX;
    antic4TileArray[antic4Tile.selected].dimY := antic4Tile.dimY;

//    debug('1 antic4Tile.dimX, antic4Tile.dimY', antic4Tile.dimX, antic4Tile.dimY);
//    debug('1 editX, editY', editX.Value, editY.Value);
    maxSize := (antic4Tile.dimX + 1)*(antic4Tile.dimY + 1) - 1;

//    SetXY(antic4Tile.dimX, antic4Tile.dimY);
//    debug('2 editX, editY', editX.Value, editY.Value);
    editX.Value := antic4Tile.dimX;
    editY.Value := antic4Tile.dimY;
    editX.Invalidate;
    editY.Invalidate;
    editY.Value := antic4Tile.dimY;
    FillByte(fldCharAntic45, SizeOf(fldCharAntic45), 0);

    // Read data
    for y := 0 to antic4Tile.dimY - 1 do
      for x := 0 to antic4Tile.dimX - 1 do begin
        index := x + y shl 2;
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
          antic4TileArray[antic4Tile.selected].charInverse[index] := false
        else
          antic4TileArray[antic4Tile.selected].charInverse[index] := true;
      end;
  finally
    fs.Free;

    UpdateCharColors;

    case antic4Tile.selected of
      1: SelectTileProc(imgTile01);
      2: SelectTileProc(imgTile02);
      3: SelectTileProc(imgTile03);
      4: SelectTileProc(imgTile04);
      5: SelectTileProc(imgTile05);
      6: SelectTileProc(imgTile06);
      7: SelectTileProc(imgTile07);
      8: SelectTileProc(imgTile08);
    end;

    ShowTilesDim;
    ShowTileChars;

    caption := programName + ' ' + programVersion +
               ' - Antic mode 4 tile editor (' + filename + ')';
//    ShowCursor(frmAntic4Tiles, frmAntic4Tiles, crDefault);
    Screen.EndWaitCursor;
    isLoadTile := false;
  end;
end;

procedure TfrmAntic4Tiles.LoadTileProc(Sender : TObject);
begin
  frmMain.dlgOpen.Title := 'Open existing Antic mode 4 tile file';
  frmMain.dlgOpen.Filter := 'Antic mode 4 tile files' +
                            ' (*.tl4)|*.tl4|All files (*.*)|*.*';
  if antic4TileArray[antic4Tile.selected].filename <> '' then
    frmMain.dlgOpen.Filename := antic4TileArray[antic4Tile.selected].filename;

  if frmMain.dlgOpen.Execute then begin
    isLoadTile := true;
    antic4TileArray[antic4Tile.selected].filename := frmMain.dlgOpen.Filename;
    OpenFile(antic4TileArray[antic4Tile.selected].filename);
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
 Save Antic mode 4 tile
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
  fs := TFileStream.Create(antic4TileArray[antic4Tile.selected].filename, fmCreate);
  try
    // Save tile character dimension boundaries (x, y)
    fs.WriteByte(antic4Tile.dimX);
    fs.WriteByte(antic4Tile.dimY);

    // Save data
    for y := 0 to antic4Tile.dimY - 1 do begin
      for x := 0 to antic4Tile.dimX - 1 do begin
        index := x + y shl 2;
        for i := 0 to _CHAR_DIM do begin
          bin := '';
          for j := 0 to _CHAR_DIM do
            bin += IntToStr(fldChar[index, j, i]);

          fs.WriteByte(bin2dec(bin));
        end;
        if antic4TileArray[antic4Tile.selected].charInverse[index] then
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
      frmMain.dlgSave.Filter := 'Antic mode 4 tile files (*.tl4)' +
                                '|*.tl4|All files (*.*)|*.*';
      frmMain.dlgSave.Filename := antic4TileArray[antic4Tile.selected].filename;
      if frmMain.dlgSave.Execute then begin
        antic4TileArray[antic4Tile.selected].filename := frmMain.dlgSave.Filename;
        SaveData;
      end;
    end
    else
      SaveData;
  finally
    if isOk then begin
//      isEdit := false;
      caption := programName + ' ' + programVersion + ' - Antic mode 4 tile editor (' +
                 antic4TileArray[antic4Tile.selected].filename + ')';
    end;
  end;
end;

procedure TfrmAntic4Tiles.imgCharDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
var
  xf, yf : byte;
  i : byte;
begin
  //debug('imgCharDown');
  btn := Button;
  //if btn = mbRight then begin
  //  popMenu.Popup(x + TControl(Sender).Left + 30, y + TControl(Sender).Top + 30);
  //  exit;
  //end;

  xf := X div chrFactX;
  yf := Y div chrFactY;

  if btnSelectTileChar.Down then
    with shapeTileGrid do begin
      Visible := true;
//      BringToFront;
      Top := TImage(Sender).Top - 2;
      Width := TImage(Sender).Width + 4;
      Left := TImage(Sender).Left - 2;
      Height := TImage(Sender).Height + 4;
    end;

  mouseIsDown := True;
  selectTileChar := TImage(Sender).Tag;

  if ((antic4Tile.dimY = 1) and (selectTileChar > 3)) or
     ((antic4Tile.dimY = 2) and (selectTileChar > 7)) or
     ((antic4Tile.dimY = 3) and (selectTileChar > 11)) or
     ((antic4Tile.dimY = 4) and (selectTileChar > 15)) then
  begin
    exit;
  end;

  if (btn = mbRight) and (Sender is TImage) then begin
//    debug('imgCharDown popMenu pre');
    popMenu.Popup(x + TControl(Sender).Left + 30, y + TControl(Sender).Top + 30);

    // Some delay for proper functioning
    for i := 1 to 30 do
      Application.ProcessMessages;

    btn := mbMiddle;

    for y := 0 to _CHAR_DIM do begin
      for x := 0 to _CHAR_DIM do
        undoValuesEx[selectTileChar, x, y] := fldChar[selectTileChar, x, y];
    end;

    if btnDraw.Down then begin
      ColorProc(color4);
//      PlotCharAntic45(Sender, xf, yf);
    end
    else if btnRotateTileChar.Down then
      RotateTileCharProc(Sender)
    else if btnFlipX.Down then
      FlipTileCharXProc(Sender)
    else if btnFlipY.Down then
      FlipTileCharYProc(Sender)
    else if btnInvertTileChar.Down then
      InvertTileCharProc(Sender)
    else if btnClearTileChar.Down then
      ClearTileCharProc(Sender)
    else if btnFillTileChar.Down then
      FillTileCharProc(Sender);

    MouseIsDown := false;
  end
  else begin
    if btnDraw.Down then
      PlotCharAntic45(Sender, xf, yf);
  end;
end;

procedure TfrmAntic4Tiles.imgCharUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
//  debug('imgCharUp');
  btn := mbMiddle;
  if MouseIsDown then begin
    for y := 0 to _CHAR_DIM do
      for x := 0 to _CHAR_DIM do
        undoValuesEx[selectTileChar, x, y] := fldChar[selectTileChar, x, y];

    if btnRotateTileChar.Down then begin
//      lblFunc.Caption := 'Function: ' + _TILE_FUNCTION[5];
      RotateTileCharProc(Sender);
    end
    else if btnFlipX.Down then
      FlipTileCharXProc(Sender)
    else if btnFlipY.Down then
      FlipTileCharYProc(Sender)
    else if btnInvertTileChar.Down then
      InvertTileCharProc(Sender)
    else if btnClearTileChar.Down then
      ClearTileCharProc(Sender)
    else if btnFillTileChar.Down then
      FillTileCharProc(Sender)
  end;
  MouseIsDown := false;
end;

{-----------------------------------------------------------------------------
 Draw Antic 4 pixel inside character editor
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

  fldCharAntic45[selectTileChar, xf, yf] := frmColors.SelColor;
  fldCharAntic45Ex[antic4Tile.selected, selectTileChar, xf, yf] := frmColors.SelColor;

  //tileChar.tileSelected := selectTileChar;
  //tileChar.x := xf;
  //tileChar.y := yf;
  //tileChar.value := fldCharAntic45[selectTileChar, xf, yf];

//  if (i > _MAX_TILE_CHARS - 1) and (i mod _MAX_TILE_CHARS = 0) then begin

//  debug(selectTileChar + (antic4Tile.selected - 1)*_MAX_TILE_CHARS);
(*
  // Show standard/inverse characters only for 4x5 tiles
//  if antic4Tile.dimX*antic4Tile.dimY = _MAX_TILE_CHARS then begin
    // Inverse character
    if frmColors.SelColor = 10 then begin
      if antic4TileArray[antic4Tile.selected].charValues[selectTileChar] < 128 then
        antic4TileArray[antic4Tile.selected].charValues[selectTileChar] :=
          antic4TileArray[antic4Tile.selected].charValues[selectTileChar] + 128;
      (*
      if sel = 4 then begin
  //      debug(selectTileChar + (antic4Tile.selected - 1)*_MAX_TILE_CHARS);
        RefreshCharX(
          TImage(imgCharList[sel + (antic4Tile.selected - 1)*_MAX_TILE_CHARS + (antic4Tile.dimX - 4)]),
          antic4TileArray[antic4Tile.selected].charValues[sel])
      end
      else if sel = 5 then
        RefreshCharX(
          TImage(imgCharList[44]),
          antic4TileArray[antic4Tile.selected].charValues[sel])
      else*)
        //RefreshCharX(
        //  TImage(imgCharList[selectTileChar + (antic4Tile.selected - 1)*_MAX_TILE_CHARS]),
        //  antic4TileArray[antic4Tile.selected].charValues[sel]);

      //if sel < 4 then
      //  RefreshCharX(
      //    TImage(imgCharList[sel + (antic4Tile.selected - 1)*_MAX_TILE_CHARS]),
      //    antic4TileArray[antic4Tile.selected].charValues[sel])
      //else if sel < 8 then
      //  RefreshCharX(
      //    TImage(imgCharList[sel + (antic4Tile.selected - 1)*_MAX_TILE_CHARS + (antic4Tile.dimX - 4)]),
      //    antic4TileArray[antic4Tile.selected].charValues[sel])

      RefreshCharX(
        TImage(imgCharList[selectTileChar + (antic4Tile.selected - 1)*_MAX_TILE_CHARS]),
        antic4TileArray[antic4Tile.selected].charValues[selectTileChar]);
    end
    // Standard character
    else if frmColors.SelColor = 3 then begin
      if antic4TileArray[antic4Tile.selected].charValues[selectTileChar] > 127 then
        antic4TileArray[antic4Tile.selected].charValues[selectTileChar] :=
          antic4TileArray[antic4Tile.selected].charValues[selectTileChar] - 128;

      RefreshCharX(
        TImage(imgCharList[selectTileChar + (antic4Tile.selected - 1)*_MAX_TILE_CHARS]),
        antic4TileArray[antic4Tile.selected].charValues[selectTileChar])
    end;
//  end;
*)

  if frmColors.SelColor = 10 then begin
  //  if antic4TileArray[antic4Tile.selected].charValues[selectTileChar] < 128 then
  //    antic4TileArray[antic4Tile.selected].charValues[selectTileChar] :=
  //      antic4TileArray[antic4Tile.selected].charValues[selectTileChar] + 128;
    antic4TileArray[antic4Tile.selected].charInverse[selectTileChar] := true;
  end
  // Standard character
  else if frmColors.SelColor = 3 then begin
  //  if antic4TileArray[antic4Tile.selected].charValues[selectTileChar] > 127 then
  //    antic4TileArray[antic4Tile.selected].charValues[selectTileChar] :=
  //      antic4TileArray[antic4Tile.selected].charValues[selectTileChar] - 128;
    antic4TileArray[antic4Tile.selected].charInverse[selectTileChar] := false;
  end;

  case xf of
    0, 2, 4, 6: begin
      undoValues[0].tileSelected := selectTileChar;
      undoValues[0].x := xf;
      undoValues[0].y := yf;
      undoValues[0].value := fldChar[selectTileChar, xf, yf];
      undoValues[1].tileSelected := selectTileChar;
      undoValues[1].x := xf + 1;
      undoValues[1].y := yf;
      undoValues[1].value := fldChar[selectTileChar, xf + 1, yf];
      if col = 0 then begin
        //fldCharMemEx[0, selectTile, 0] := fldChar[selectTile, xf, yf];
        //fldCharMemEx[0, selectTile, 1] := xf;
        //fldCharMemEx[0, selectTile, 2] := yf;
        //fldCharMemEx[0, selectTile, 3] := fldChar[selectTile, xf + 1, yf];
        //fldCharMemEx[0, selectTile, 4] := xf + 1;
        //fldCharMemEx[0, selectTile, 5] := yf;

        fldCharAntic45[selectTileChar, xf + 1, yf] := 0;
        fldCharAntic45Ex[antic4Tile.selected, selectTileChar, xf + 1, yf] := 0;
        FillRectEx(TImage(Sender), colTab[0], xf*chrFactX + 1, yf*chrFactY + 1,
                   chrFactX*2 - 1, chrFactY - 1);
        fldChar[selectTileChar, xf, yf] := 0;
        fldChar[selectTileChar, xf + 1, yf] := 0;
      end
      else begin
        fldCharAntic45[selectTileChar, xf + 1, yf] := frmColors.SelColor;
        fldCharAntic45Ex[antic4Tile.selected, selectTileChar, xf + 1, yf] := frmColors.SelColor;
        FillRectEx(TImage(Sender), colTab[frmColors.SelColor],
                   xf*chrFactX + 1, yf*chrFactY + 1, chrFactX*2 - 1, chrFactY - 1);
        case frmColors.SelColor of
          0: begin
            fldChar[selectTileChar, xf, yf] := 0;
            fldChar[selectTileChar, xf + 1, yf] := 0;
          end;
          1: begin
            fldChar[selectTileChar, xf, yf] := 0;
            fldChar[selectTileChar, xf + 1, yf] := 1;
          end;
          2: begin
            fldChar[selectTileChar, xf, yf] := 1;
            fldChar[selectTileChar, xf + 1, yf] := 0;
          end;
          3, 10: begin
            fldChar[selectTileChar, xf, yf] := 1;
            fldChar[selectTileChar, xf + 1, yf] := 1;
          end;
        end;
      end;
    end;
    1, 3, 5, 7: begin
      undoValues[0].tileSelected := selectTileChar;
      undoValues[0].x := xf - 1;
      undoValues[0].y := yf;
      undoValues[0].value := fldChar[selectTileChar, xf - 1, yf];
      undoValues[1].tileSelected := selectTileChar;
      undoValues[1].x := xf;
      undoValues[1].y := yf;
      undoValues[1].value := fldChar[selectTileChar, xf, yf];
      if col = 0 then begin
        fldCharAntic45[selectTileChar, xf - 1, yf] := 0;
        fldCharAntic45Ex[antic4Tile.selected, selectTileChar, xf - 1, yf] := 0;
        FillRectEx(TImage(Sender), colTab[0],
                   (xf - 1)*chrFactX + 1, yf*chrFactY + 1, chrFactX*2 - 1, chrFactY - 1);
        fldChar[selectTileChar, xf - 1, yf] := 0;
        fldChar[selectTileChar, xf, yf] := 0;
      end
      else begin
//        fldCharMem[0, selectTile, xf - 1, yf] := fldChar[selectTile, xf - 1, yf];
//        fldCharMem[0, selectTile, xf, yf] := fldChar[selectTile, xf, yf];
//        fldCharMemEx[0, selectTile, 0] := fldChar[selectTile, xf - 1, yf];
//        fldCharMemEx[0, selectTile, 1] := fldChar[selectTile, xf, yf];
        fldCharAntic45[selectTileChar, xf - 1, yf] := frmColors.SelColor;
        fldCharAntic45Ex[antic4Tile.selected, selectTileChar, xf - 1, yf] := frmColors.SelColor;
        FillRectEx(TImage(Sender), colTab[frmColors.SelColor],
                   (xf - 1)*chrFactX + 1, yf*chrFactY + 1, chrFactX*2 - 1, chrFactY - 1);
        case frmColors.SelColor of
          0: begin
            fldChar[selectTileChar, xf - 1, yf] := 0;
            fldChar[selectTileChar, xf, yf] := 0;
          end;
          1: begin
            fldChar[selectTileChar, xf - 1, yf] := 0;
            fldChar[selectTileChar, xf, yf] := 1;
          end;
          2: begin
            fldChar[selectTileChar, xf - 1, yf] := 1;
            fldChar[selectTileChar, xf, yf] := 0;
          end;
          3, 10: begin
            fldChar[selectTileChar, xf - 1, yf] := 1;
            fldChar[selectTileChar, xf, yf] := 1;
          end;
        end;
      end;
    end;
  end;

  RefreshChar(TImage(Sender), selectTileChar, frmColors.SelColor);

  //for i := 0 to _CHAR_DIM do
  //  for j := 0 to _CHAR_DIM do begin
  //    fldCharAntic45[0, j, i] := fldCharAntic45Ex[selectTiles, 0, j, i];
  //    fldCharAntic45[1, j, i] := fldCharAntic45Ex[selectTiles, 1, j, i];
  //    fldCharAntic45[2, j, i] := fldCharAntic45Ex[selectTiles, 2, j, i];
  //  end;

  RefreshTiles;
end;

// Refresh screen
procedure TfrmAntic4Tiles.RefreshTiles;
var
  i, j : integer;
  m, r : byte;
begin
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
    DrawTile(r, m, j);  //fldAtascii[j]);
    Inc(r);
  end;

  RefreshScreen;
end;

{-----------------------------------------------------------------------------
 Draw tile character on screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.DrawTile(scrPos, y, offset : byte);
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
          if antic4TileArray[antic4Tile.selected].charInverse[offset] then
            col := 10
          else
            col := 3;
        end;
//        col := frmColors.SelColor;

        FillRectEx((FindComponent('imgTile0' + IntToStr(antic4Tile.selected)) as TImage),
                   colTab[col], (xf + j - 1)*factX, (yf + i)*factY, factX shl 1, factY);
        cnt := 0;
      end;
    end;
end;

procedure TfrmAntic4Tiles.RefreshChar(image : TImage; index, selColor : byte);
var
  oldColor : byte;
  i, j : byte;
begin
  // Check inverse flag color
  if (selColor = 3) or (selColor = 10) then begin
    if selColor = 3 then begin
      oldColor := 10;
      antic4TileArray[antic4Tile.selected].charInverse[index] := false;
    end
    else begin
      oldColor := 3;
      antic4TileArray[antic4Tile.selected].charInverse[index] := true;
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
end;

procedure TfrmAntic4Tiles.SetAnticMode(mode : byte);
begin
  anticMode := mode;
//  isAntic4 := mode = 4;

//  charXOffset := 32;
  if anticMode = 4 then begin
    modeHeight := 24;
    factY := 2;
  end
  else begin
    modeHeight := 48;
    factY := 4;
  end;
end;

{-----------------------------------------------------------------------------
 Refresh screen data
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.RefreshGridData;
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
//          if inverseIndex[index] then
          if antic4TileArray[antic4Tile.selected].charInverse[index] then
            col := 10
          else
            col := 3;
        end;

//        if col > 0 then showmessage(inttostr(col));
        FillRectEx(image, colTab[col], (j - 1)*chrFactX + 1, i*chrFactY + 1,
                   chrFactX shl 1 - 1, chrFactY - 1);
        if isShowGrid then begin
          image.Canvas.Pen.Color := clGray;
          image.Canvas.Rectangle((j - 1)*chrFactX, i*chrFactY,
                                 (j - 1)*chrFactX + 1 + chrFactX shl 1, i*chrFactY + chrFactY + 1);
        end;
        cnt := 0;
      end;
    end;
  end;

//  image.Canvas.Pen.Color := clSkyBlue;
//  image.Canvas.Rectangle(0, 0, image.Width, image.Height);
  image.Invalidate;
end;

procedure TfrmAntic4Tiles.ColorDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  //frmColors.SelColor := TImage(Sender).Tag;
  //ColorRegisters;
  //TImage(Sender).Canvas.Pen.Color := clRed;
  //TImage(Sender).Canvas.Rectangle(0, 0, color1.width, color1.Height);
  ColorProc(TImage(Sender));
end;

procedure TfrmAntic4Tiles.ColorProc(image : TImage);  //; selColor : byte);
begin
  frmColors.SelColor := image.Tag;
  ColorRegisters;
  image.Canvas.Pen.Color := clRed;
  image.Canvas.Rectangle(0, 0, color1.width, color1.Height);
end;

procedure TfrmAntic4Tiles.editXLeave(Sender : TObject);
begin
//   debug('pre editXLeave');
  if isLoadTile then exit;

  if isCreate or isSelectTile then begin
    isSelectTile := false;
    exit;
  end;

  btn := mbMiddle;
  SetXY(editX.Value, editY.Value);
  antic4TileArray[antic4Tile.selected].dimX := antic4Tile.dimX;
  antic4TileArray[antic4Tile.selected].dimY := antic4Tile.dimY;

  ShowTilesDim;
  SetCells;

  if not (Sender is TImage) then begin
    //and chkTileDimAuto.Checked then begin
    ShowTileChars;
  end;
end;

procedure TfrmAntic4Tiles.SetCells;
var
  y, x, index : byte;
  image : TImage;
begin
  for x := 0 to SizeOf(cellIndex) - 1 do
    cellIndex[x] := false;

  for x := 0 to editY.Value - 1 do
    for y := 0 to editX.Value - 1 do
      cellIndex[y + x shl 2] := true;

  for y := 0 to 3 do begin
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

  RefreshGridData;
  RefreshTiles;
end;

procedure TfrmAntic4Tiles.GenCodeProc(Sender : TObject);
begin
  frmAntic4TilesGen := TfrmAntic4TilesGen.Create(Self);
  with frmAntic4TilesGen do
    try
      ShowModal;
    finally
      Free;
    end;
end;

{-----------------------------------------------------------------------------
 Invert tile data
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.InvertTileCharProc(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to _CHAR_DIM do begin
    for x := 0 to _CHAR_DIM do
      fldChar[selectTileChar, x, y] := 1 - fldChar[selectTileChar, x, y];
  end;
  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
end;

{-----------------------------------------------------------------------------
 Rotate character
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.RotateTileCharProc(Sender : TObject);
var
  x, y, n : byte;
  arr : charType;
begin
  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      arr[y, x] := fldChar[selectTileChar, x, y];

  for x := 0 to _CHAR_DIM do
    for n := 0 to _CHAR_DIM do
      fldChar[selectTileChar, 7 - n, x] := arr[n, x];

  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
end;

{-----------------------------------------------------------------------------
 Mirror character horizontally
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.FlipTileCharXProc(Sender : TObject);
var
  y : byte;
  n1, n2 : byte;
begin
(*
  for y := 0 to _CHAR_DIM do
    for x := _CHAR_DIM downto 4 do begin
      n := fldChar[selectTileChar, x, y];
      fldChar[selectTileChar, x, y] := fldChar[selectTileChar, 7 - x, y];
      fldChar[selectTileChar, 7 - x, y] := n;
    end;
*)
  for y := 0 to _CHAR_DIM do begin
    n1 := fldChar[selectTileChar, 0, y];
    n2 := fldChar[selectTileChar, 1, y];
    fldChar[selectTileChar, 0, y] := fldChar[selectTileChar, 6, y];
    fldChar[selectTileChar, 1, y] := fldChar[selectTileChar, 7, y];
    fldChar[selectTileChar, 6, y] := n1;
    fldChar[selectTileChar, 7, y] := n2;

    n1 := fldChar[selectTileChar, 2, y];
    n2 := fldChar[selectTileChar, 3, y];
    fldChar[selectTileChar, 2, y] := fldChar[selectTileChar, 4, y];
    fldChar[selectTileChar, 3, y] := fldChar[selectTileChar, 5, y];
    fldChar[selectTileChar, 4, y] := n1;
    fldChar[selectTileChar, 5, y] := n2;
  end;

  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
(*
  for i := 0 to _MAX_TILE_CHARS - 1 do
    for y := 0 to _CHAR_DIM do begin
      n1 := fldChar[i, 0, y];
      n2 := fldChar[i, 1, y];
      for x := 1 to _CHAR_DIM do
        if (x = 1) or (x = 3) or (x = 5) then begin
          fldChar[i, x - 1, y] := fldChar[i, x + 1, y];
          fldChar[i, x, y] := fldChar[i, x + 2, y];
        end;

      fldChar[i, 6, y] := n1;
      fldChar[i, 7, y] := n2;
    end;
    *)
end;

{-----------------------------------------------------------------------------
 Mirror character vertically
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.FlipTileCharYProc(Sender : TObject);
var
  x, y, n : byte;
begin
  for x := 0 to _CHAR_DIM do
    for y := _CHAR_DIM downto 4 do begin
      n := fldChar[selectTileChar, x, y];
      fldChar[selectTileChar, x, y] := fldChar[selectTileChar, x, 7 - y];
      fldChar[selectTileChar, x, 7 - y] := n;
    end;

  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
end;

procedure TfrmAntic4Tiles.ClearTileProc(Sender : TObject);
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

  FillByte(fldCharAntic45, SizeOf(fldCharAntic45), 0);
  FillByte(fldChar, SizeOf(fldChar), 0);
  SetCells;

  UpdateCharColors;
end;

{-----------------------------------------------------------------------------
 Shift tile character left
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
    n1 := fldChar[selectTileChar, 0, y];
    n2 := fldChar[selectTileChar, 1, y];
    for x := 1 to _CHAR_DIM do
      if (x = 1) or (x = 3) or (x = 5) then begin
        fldChar[selectTileChar, x - 1, y] := fldChar[selectTileChar, x + 1, y];
        fldChar[selectTileChar, x, y] := fldChar[selectTileChar, x + 2, y];
      end;

    fldChar[selectTileChar, 6, y] := n1;
    fldChar[selectTileChar, 7, y] := n2;
  end;
end;

{-----------------------------------------------------------------------------
 Shift tile left
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftTileLeftProc(Sender: TObject);
var
  x, y, i : byte;
  n1, n2 : byte;
begin
  for i := 0 to _MAX_TILE_CHARS - 1 do
    for y := 0 to _CHAR_DIM do begin
      n1 := fldChar[i, 0, y];
      n2 := fldChar[i, 1, y];
      for x := 1 to _CHAR_DIM do
        if (x = 1) or (x = 3) or (x = 5) then begin
          fldChar[i, x - 1, y] := fldChar[i, x + 1, y];
          fldChar[i, x, y] := fldChar[i, x + 2, y];
        end;

      fldChar[i, 6, y] := n1;
      fldChar[i, 7, y] := n2;
    end;
end;

{-----------------------------------------------------------------------------
 Shift tile character right
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftRightProc(Sender: TObject);
var
  x, y : byte;
  n1, n2 : byte;
begin
  for y := 0 to _CHAR_DIM do begin
    n1 := fldChar[selectTileChar, 7, y];
    n2 := fldChar[selectTileChar, 6, y];
    for x := _CHAR_DIM downto 1 do
      if (x = 7) or (x = 5) or (x = 3) then begin
        fldChar[selectTileChar, x, y] := fldChar[selectTileChar, x - 2, y];
        fldChar[selectTileChar, x - 1, y] := fldChar[selectTileChar, x - 3, y];
      end;

    fldChar[selectTileChar, 1, y] := n1;
    fldChar[selectTileChar, 0, y] := n2;
  end;
end;

{-----------------------------------------------------------------------------
 Shift tile right
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftTileRightProc(Sender: TObject);
var
  x, y, i : byte;
  n1, n2 : byte;
begin
  for i := 0 to _MAX_TILE_CHARS - 1 do
    for y := 0 to _CHAR_DIM do begin
      n1 := fldChar[i, 7, y];
      n2 := fldChar[i, 6, y];
      for x := _CHAR_DIM downto 1 do
        if (x = 7) or (x = 5) or (x = 3) then begin
          fldChar[i, x, y] := fldChar[i, x - 2, y];
          fldChar[i, x - 1, y] := fldChar[i, x - 3, y];
        end;

      fldChar[i, 1, y] := n1;
      fldChar[i, 0, y] := n2;
    end;
end;

{-----------------------------------------------------------------------------
 Shift tile character up
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftUpProc(Sender: TObject);
var
  x, y : byte;
begin
  for x := 0 to _CHAR_DIM do
    fld02[x] := fldChar[selectTileChar, x, 0];

  for x := 0 to _CHAR_DIM do
    for y := 1 to _CHAR_DIM do begin
      fldChar[selectTileChar, x, y - 1] := fldChar[selectTileChar, x, y];
      fldChar[selectTileChar, x, y] := 0;
    end;

  for x := 0 to _CHAR_DIM do
    fldChar[selectTileChar, x, _CHAR_DIM] := fld02[x];
end;

{-----------------------------------------------------------------------------
 Shift tile up
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftTileUpProc(Sender: TObject);
var
  x, y, i : byte;
begin
  for i := 0 to _MAX_TILE_CHARS - 1 do begin
    for x := 0 to _CHAR_DIM do
      fld02[x] := fldChar[i, x, 0];

    for x := 0 to _CHAR_DIM do
      for y := 1 to _CHAR_DIM do begin
        fldChar[i, x, y - 1] := fldChar[i, x, y];
        fldChar[i, x, y] := 0;
      end;

    for x := 0 to _CHAR_DIM do
      fldChar[i, x, _CHAR_DIM] := fld02[x];
  end;
end;

{-----------------------------------------------------------------------------
 Shift tile character down
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftDownProc(Sender: TObject);
var
  x, y : byte;
begin
  for x := 0 to _CHAR_DIM do
    fld02[x] := fldChar[selectTileChar, x, _CHAR_DIM];

  for x := 0 to _CHAR_DIM do
    for y := _CHAR_DIM - 1 downto 0 do begin
      fldChar[selectTileChar, x, y + 1] := fldChar[selectTileChar, x, y];
      fldChar[selectTileChar, x, y] := 0;
    end;

  for x := 0 to _CHAR_DIM do
    fldChar[selectTileChar, x, 0] := fld02[x];
end;

{-----------------------------------------------------------------------------
 Shift tile down
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.ShiftTileDownProc(Sender: TObject);
var
  x, y, i : byte;
begin
  for i := 0 to _MAX_TILE_CHARS - 1 do begin
    for x := 0 to _CHAR_DIM do
      fld02[x] := fldChar[i, x, _CHAR_DIM];

    for x := 0 to _CHAR_DIM do
      for y := _CHAR_DIM - 1 downto 0 do begin
        fldChar[i, x, y + 1] := fldChar[i, x, y];
        fldChar[i, x, y] := 0;
      end;

    for x := 0 to _CHAR_DIM do
      fldChar[i, x, 0] := fld02[x];
  end;
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
        fldChar[selectTileChar, x - 1, y] := fldChar[selectTileChar, x + 1, y];
        fldChar[selectTileChar, x, y] := fldChar[selectTileChar, x + 2, y];
        fldChar[selectTileChar, x + 1, y] := 0;
        fldChar[selectTileChar, x + 2, y] := 0;
      end;
  end;
end;

{-----------------------------------------------------------------------------
 Move character right
 -----------------------------------------------------------------------------}
procedure TfrmAntic4Tiles.MoveRightProc(Sender: TObject);
var
  x, y : byte;
begin
  for y := 0 to _CHAR_DIM do
    for x := _CHAR_DIM downto 1 do
      if (x = 7) or (x = 5) or (x = 3) then begin
        fldChar[selectTileChar, x, y] := fldChar[selectTileChar, x - 2, y];
        fldChar[selectTileChar, x - 1, y] := fldChar[selectTileChar, x - 3, y];
        fldChar[selectTileChar, x - 2, y] := 0;
        fldChar[selectTileChar, x - 3, y] := 0;
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
      fldChar[selectTileChar, x, y - 1] := fldChar[selectTileChar, x, y];
      fldChar[selectTileChar, x, y] := 0;
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
      fldChar[selectTileChar, x, y + 1] := fldChar[selectTileChar, x, y];
      fldChar[selectTileChar, x, y] := 0;
    end;
end;

procedure TfrmAntic4Tiles.CharOper(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      undoValuesEx[selectTileChar, x, y] := fldChar[selectTileChar, x, y];

  if radShiftChar.Checked then begin
    case (Sender as TSpeedButton).Tag of
      0: ShiftLeftProc(Sender);
      1: ShiftRightProc(Sender);
      2: ShiftUpProc(Sender);
      3: ShiftDownProc(Sender);
    end;
  end
  else if radMoveChar.Checked then begin
    case (Sender as TSpeedButton).Tag of
      0: MoveLeftProc(Sender);
      1: MoveRightProc(Sender);
      2: MoveUpProc(Sender);
      3: MoveDownProc(Sender);
    end;
  end
  else if radShiftTile.Checked then begin
    case (Sender as TSpeedButton).Tag of
      0: ShiftTileLeftProc(Sender);
      1: ShiftTileRightProc(Sender);
      2: ShiftTileUpProc(Sender);
      3: ShiftTileDownProc(Sender);
    end;
  end;

  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
end;

procedure TfrmAntic4Tiles.ClearTileCharProc(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      fldChar[selectTileChar, x, y] := 0;

  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
end;

procedure TfrmAntic4Tiles.FillTileCharProc(Sender : TObject);
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
            fldChar[selectTileChar, x - 1, y] := 0;
            fldChar[selectTileChar, x, y] := 0;
          end;
          1: begin
            fldChar[selectTileChar, x - 1, y] := 0;
            fldChar[selectTileChar, x, y] := 1;
          end;
          2: begin
            fldChar[selectTileChar, x - 1, y] := 1;
            fldChar[selectTileChar, x, y] := 0;
          end;
          3, 10: begin
            fldChar[selectTileChar, x - 1, y] := 1;
            fldChar[selectTileChar, x, y] := 1;
          end;
        end;
      end;

  antic4TileArray[antic4Tile.selected].charInverse[selectTileChar] := frmColors.SelColor = 10;
  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
end;

// Update data colors on tile grid
procedure TfrmAntic4Tiles.UpdateCharColors;
var
  x, y, i : byte;
begin
  //for y := 0 to _CHAR_DIM do
  //  for x := 0 to _CHAR_DIM do
  //    redoValuesEx[selectTile, x, y] := undoValuesEx[selectTile, x, y];

  for i := 0 to _MAX_TILE_CHARS - 1 do
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
            if antic4TileArray[antic4Tile.selected].charInverse[i] then begin
              fldCharAntic45[i, x - 1, y] := 3;
              fldCharAntic45[i, x, y] := 3;
              fldCharAntic45Ex[antic4Tile.selected, i, x - 1, y] := 3;
              fldCharAntic45Ex[antic4Tile.selected, i, x, y] := 3;
            end
            else begin
              fldCharAntic45[i, x - 1, y] := 10;
              fldCharAntic45[i, x, y] := 10;
              fldCharAntic45Ex[antic4Tile.selected, i, x - 1, y] := 10;
              fldCharAntic45Ex[antic4Tile.selected, i, x, y] := 10;
            end;
          end;
        end;
end;

procedure TfrmAntic4Tiles.ColorPaletteProc(Sender : TObject);
begin
  frmColors.Show;
end;

procedure TfrmAntic4Tiles.paintBoxMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
begin
  isPaintBoxMove := true;
  paintBoxDown(Sender, btn, Shift, x, y);
end;

procedure TfrmAntic4Tiles.paintBoxUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  btn := mbMiddle;
end;

procedure TfrmAntic4Tiles.ClearTilesProc(Sender : TObject);
var
  i : byte;
begin
  FillByte(fldChar, SizeOf(fldChar), 0);
  FillByte(fldCharAntic45, SizeOf(fldCharAntic45), 0);
  FillByte(fldCharAntic45Ex, SizeOf(fldCharAntic45Ex), 0);

  antic4Tile.dimX := 4;
  antic4Tile.dimY := 5;
  editX.Value := antic4Tile.dimX;
  editY.Value := antic4Tile.dimY;

  for i := 1 to 8 do begin
    antic4Tile.selected := i;
    RefreshTiles;
  end;

  antic4Tile.selected := 1;
  SetCells;

  with shape do begin
    Visible := true;
    Top := (imageObject as TImage).Top - 4;
    Left := (imageObject as TImage).Left - 4;
    Width := (imageObject as TImage).Width + 8;
    Height := (imageObject as TImage).Height + 8;
  end;
end;

procedure TfrmAntic4Tiles.popFunc(Sender : TObject);
begin
  //showmessage('popFunc * ' + inttostr(TMenuItem(Sender).Tag));
  case TMenuItem(Sender).Tag of
    0, 12: btnDraw.Down := true;
    1: btnFlipX.Down := true;
    2: btnFlipY.Down := true;
    3: btnRotateTileChar.Down := true;
    4: btnInvertTileChar.Down := true;
    5: btnFillTileChar.Down := true;
    6: btnClearTileChar.Down := true;
    7: btnClearTile.Down := true;
  end;

  MiscOper(Sender);
end;

procedure TfrmAntic4Tiles.SelectTileProc(Sender : TObject);
var
  i : byte;
  xf, yf : byte;
  col : byte;
begin
  imageObject := Sender;
  isSizeEdit := false;

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

  isSelectTile := true;
  editX.Value := antic4Tile.dimX;
  isSelectTile := true;
  editY.Value := antic4Tile.dimY;
  editX.Invalidate;
  editY.Invalidate;

  caption := programName + ' ' + programVersion +
             ' - Antic mode 4 tile editor (' + antic4TileArray[antic4Tile.selected].filename + ')';
  for i := 0 to _MAX_TILE_CHARS - 1 do begin
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
  end;

  //for i := 0 to SizeOf(cellIndex) - 1 do
  //  cellIndex[i] := false;
  //
  //for i := 0 to editY.Value - 1 do
  //  for j := 0 to editX.Value - 1 do
  //    cellIndex[j + i shl 2] := true;

  SetCells;
end;

procedure TfrmAntic4Tiles.UndoProc(Sender : TObject);
var
  x, y : byte;
begin
  //for i := 0 to _MAX_TILE_CHARS - 1 do
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

//  for i := 0 to _MAX_TILE_CHARS - 1 do
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
      redoValuesEx[selectTileChar, x, y] := fldChar[selectTileChar, x, y];

  if btnDraw.Down then begin
    fldChar[undoValues[0].tileSelected, undoValues[0].x, undoValues[0].y] := undoValues[0].value;
    fldChar[undoValues[1].tileSelected, undoValues[1].x, undoValues[1].y] := undoValues[1].value;
  end
  else begin
    for y := 0 to _CHAR_DIM do
      for x := 0 to _CHAR_DIM do
        fldChar[selectTileChar, x, y] := undoValuesEx[selectTileChar, x, y];
  end;

  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
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
      fldChar[selectTileChar, x, y] := redoValuesEx[selectTileChar, x, y];

  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
end;

procedure TfrmAntic4Tiles.LoadDefaultColorsProc(Sender : TObject);
begin
  frmMain.LoadDefaultPalette;
  RefreshGridData;
end;

procedure TfrmAntic4Tiles.ShowGridProc(Sender : TObject);
begin
  isShowGrid := TMenuItem(Sender).Tag = 0;
  RefreshGridData;
end;

procedure TfrmAntic4Tiles.paintBoxDown(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
var
  i, j : byte;
  xx, yy : byte;
  x02, y02 : byte;
  coordX02, coordY02 : integer;
begin
  btn := Button;
  coordX := X div factX;
  coordY := Y div factY;
  coordX02 := coordX;
  coordY02 := coordY;

//  statusBar.Panels[2].Text := 'xy : ' + inttostr(coordX) + ', ' + inttostr(coordY);

  //// Data is changed
  //if not isEdit then begin
  //  isEdit := true;
  //  if Pos(' *', caption) = 0 then
  //    caption := caption + ' *';
  //end;

  // Calculate Atari tile matrix coordinates for Antic mode 4
  for j := 0 to 23 do begin
    for i := 0 to 39 do begin
      if (coordX >= (i shl 3)*editX.Value) and (coordX < ((i + 1) shl 3)*editX.Value) then begin
        if j = 0 then begin
          if i > 0 then begin
            coordX := (i shl 4)*editX.Value;
//            x01 := ((i shl 3)*editX.Value) shr 2;
            x02 := ((i shl 3)*editX.Value) shr 3;
          end
          else begin
            coordX := 0;
            x02 := 0;
          end;
        end;
        break;
      end;
    end;
    if (coordY >= (j shl 3)*editY.Value) and (coordY < ((j + 1) shl 3)*editY.Value) then begin
      coordY := (j shl 4)*editY.Value;
//      yy := j;
//      y01 := j shl 4;
      y02 := ((j shl 3)*editY.Value) shr 3;
      break;
    end;
  end;

  // Calculate screen coordinates for Antic mode 4
  for j := 0 to 23 do begin
    for i := 0 to 39 do begin
      if (coordX02 >= (i shl 3)) and (coordX02 < ((i + 1) shl 3)) then begin
        if j = 0 then begin
          if i > 0 then begin
//            coordX := (i shl 4);
//            x01 := ((i shl 3)*editX.Value) shr 2;
            xx := ((i shl 3)) shr 3;
          end
          else begin
//            coordX := 0;
            xx := 0;
          end;
        end;
        break;
      end;
    end;

    if (coordY02 >= (j shl 3)) and (coordY02 < ((j + 1) shl 3)) then begin
//      coordY02 := (j shl 4);
//      yy := j;
//      y01 := j shl 4;
      yy := ((j shl 3)) shr 3;
      break;
    end;
  end;

  if x02 + y02*(39 + 1) > _ANTIC_MODE_4_SIZE - 1 then Exit;

//  if xx + yy*(maxX + 1) > maxSize then Exit;
  statusBar.Panels[1].Text := 'Cursor coordinates (x: ' +
                              inttostr(xx) + ', y: ' + inttostr(yy) + ')';
  statusBar.Panels[2].Text := 'Tile coordinates (x: ' +
                              inttostr(x02) + ', y: ' + inttostr(y02) + ')';

  coordX02 := x02 + y02*(39 + 1);
  if not isPaintBoxMove then begin
    tiles[coordX02].tileIndex := antic4Tile.selected;
    tiles[coordX02].coordX := coordX;
    tiles[coordX02].coordY := coordY;
    tiles[coordX02].x := x02;
    tiles[coordX02].y := y02;

    case btn of
      mbLeft : begin end;
      mbRight: begin end;
    else
      exit;
    end;

    paintBoxPaint(imageObject as TImage);
  end;

  isPaintBoxMove := false;
end;

procedure TfrmAntic4Tiles.paintBoxPaint(Sender : TObject);
begin
  if not (sender is TImage) then exit;

//  case selectTiles of
//    1: begin
////    paintBox.Canvas.Brush.Color := clRed;
////    paintBox.Canvas.Rectangle(10, 10, 30, 30);
//      paintBox.Canvas.Draw(0, 0, imgtile01.Picture.Bitmap);
//    end;
//  end;

//paintBox.Canvas.CopyRect(
  //  Rect(coordX,coordY,TImage(Sender).Picture.Width,TImage(Sender).Picture.Height),
  //  TImage(Sender).Canvas,
  //  Rect(
  //    0, 0, TImage(Sender).Picture.Width-40, TImage(Sender).Picture.Height-40));

  paintBox.Canvas.Draw(coordX, coordY, TImage(Sender).Picture.Bitmap);
//  paintBox.Canvas.Draw(coordX, coordY, (imageObject as TImage).Picture.Bitmap);
end;

procedure TfrmAntic4Tiles.MiscOper(Sender : TObject);
begin
  shapeTileGrid.Visible := false;

  if btnSelectTileChar.Down then
    lblFunc.Caption := 'Function: ' + _TILE_FUNCTION[0]
  else if btnDraw.Down then
    lblFunc.Caption := 'Function: ' + _TILE_FUNCTION[1]
  else if btnRotateTileChar.Down then
    lblFunc.Caption := 'Function: ' + _TILE_FUNCTION[5]
  else if btnFlipX.Down then
    lblFunc.Caption := 'Function: ' + _TILE_FUNCTION[2]
  else if btnFlipY.Down then
    lblFunc.Caption := 'Function: ' + _TILE_FUNCTION[3]
  else if btnInvertTileChar.Down then
    lblFunc.Caption := 'Function: ' + _TILE_FUNCTION[4]
  else if btnFillTileChar.Down then
    lblFunc.Caption := 'Function: ' + _TILE_FUNCTION[6]
  else if btnClearTileChar.Down then
    lblFunc.Caption := 'Function: ' + _TILE_FUNCTION[7];
end;

procedure TfrmAntic4Tiles.ViewerProc(Sender : TObject);
begin
  frmViewer.isModal := true;
  frmViewer.ShowModal;
  if frmViewer.filename <> '' then
    OpenFile(frmViewer.filename);
end;

procedure TfrmAntic4Tiles.ShowTilesDim;
var
  i : byte;
begin
  for i := 1 to _MAX_TILES do
    (FindComponent('lblTile0' + IntToStr(i) + 'Dim') as TLabel).Caption :=
      IntToStr(antic4TileArray[i].dimX) + ' x ' + IntToStr(antic4TileArray[i].dimY);
//      #13#10'(' + IntToStr(antic4TileArray[i].dimX shl 3) + ' x' + IntToStr(antic4TileArray[i].dimY shl 3) + ')';
end;

procedure TfrmAntic4Tiles.RefreshCharX(imgChar : TImage; offset : integer);
var
  col : byte;
  xf, yf : integer;
  isInverse : boolean;
begin
  isInverse := offset > 127;
  FillRectEx(imgChar, coltabFont[0], 0, 0, imgChar.Width, imgChar.Height);
  offset := offset shl 3;
  for yf := 0 to _CHAR_DIM do
    for xf := 0 to _CHAR_DIM do begin
      // Show pixel of selected character
      col := fldFontSet[xf, yf + offset];
//      if not isFontSetNormal then
//        col := 1 - col;

      if isInverse then
        col := 1 - col;

      FillRectEx(imgChar, coltabFont[col], xf shl 1, yf shl 1, 2, 2);
    end;

  imgChar.Refresh;
end;

procedure TfrmAntic4Tiles.ShowTileChars;
var
  i : byte;
  cntTiles : byte = 1;
  cntValues : byte = 0;
  charValuePos : byte = 1;
  tileMaxSize : byte;
  xPos : byte = 0;
  yPos : byte = 96;
begin
//  isFontSetNormal := true;
  tileMaxSize := antic4TileArray[cntTiles].dimX*antic4TileArray[cntTiles].dimY - 1;

//  debug('imgCharList', imgCharList.count);
  for i := 0 to imgCharList.Count - 1 do begin
    TImage(imgCharList[i]).Enabled := false;
    TImage(imgCharList[i]).Visible := false;

    // Check the maximum possible number of characters for the tile
    if (i > _MAX_TILE_CHARS - 1) and (i mod _MAX_TILE_CHARS = 0) then begin
      // Next tile
      Inc(cntTiles);
      // Start with first tile character
      cntValues := 0;
      // Y coordinate of tile character
      yPos := 96;
      // Calculate new maximum count of tile characters
      tileMaxSize := antic4TileArray[cntTiles].dimX*antic4TileArray[cntTiles].dimY - 1;
//      debug('ShowTileChars', tileMaxSize, i, charValuePos);
    end;

    // Draw tile character until maximum dimension boundaries are reached
    if cntValues <= tileMaxSize then begin
      // Check if maximum count of x positioned tiles exceed set values
      if cntValues mod antic4TileArray[cntTiles].dimX = 0 then begin
        xPos := 0;      // Reset tile character position horizontally
        inc(yPos, 18);  // Position tile character vertically
      end;

      // Enable and show character for the tile at specific position
      TImage(imgCharList[i]).Enabled := true;
      TImage(imgCharList[i]).Visible := true;
      TImage(imgCharList[i]).Left := (cntTiles - 1)*80 + xPos*(TImage(imgCharList[i]).Width - 1) +
                                     xPos + 10;
      TImage(imgCharList[i]).Top := yPos;

//      if charValuePos = 0 then debug('0', i);

      // Set character value of the tile and show it on the screen
      antic4TileArray[cntTiles].charValues[cntValues] := charValuePos;
      RefreshCharX(TImage(imgCharList[i]), charValuePos);
      TImage(imgCharList[i]).Hint := 'Dec: ' + IntToStr(charValuePos) +
                                     ' Hex: ' + IntToHex(charValuePos, 2);
      Inc(xPos);          // Position for next tile character
      Inc(cntValues);     // Next tile character

      if charValuePos = 127 then
        charValuePos := 0;

      Inc(charValuePos);  // Next character value
    end;
  end;
end;

procedure TfrmAntic4Tiles.CreateObjects(index : byte);
var
  image : TImage;
  i : byte;
  j, y : word;
begin
  j := 0;
  y := 96;
  for i := 0 to _MAX_TILE_CHARS - 1 do begin
    image := TImage.Create(self);
    image.Parent := scrollBox;
    image.Name := 'imgTile0' + IntToStr(index) + 'Char' + inttostr(i);
    image.Enabled := false;
    image.Visible := false;
    image.Width := 18;
    image.Height := 18;
    inc(j);
    if i mod 4 = 0 then begin
      j := 0;
      inc(y, 18);
    end;

//    image.Left := (index - 1)*80 + j*(image.Width - 1) + j + 10;
//    image.Top := y;
//    image.OnClick := @CharSetProc;
    image.OnMouseDown := @OnMouseDown;
    image.Tag := i;
    image.ParentShowHint := false;
    image.ShowHint := true;

    // Alternative to Tag property for detecting selected tile
    image.Constraints.MinWidth := index;

    imgCharList.Add(image);
  end;
end;

procedure TfrmAntic4Tiles.CopyTileToAllProc(Sender : TObject);
var
  x, y, i, j : byte;
begin
  if MessageDlg('Question',
                'Selected tile will be copied to all other tiles! Are you sure to proceed?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    for i := 1 to 8 do begin
      for j := 0 to _MAX_TILE_CHARS - 1 do
        for y := 0 to _CHAR_DIM do
          for x := 0 to _CHAR_DIM do
            if i <> antic4Tile.selected then
              fldCharAntic45Ex[i, j, x, y] := fldCharAntic45Ex[antic4Tile.selected, j, x, y];
    end;

    j := antic4Tile.selected;
    for i := 1 to 8 do begin
      if i <> j then begin
        antic4Tile.selected := i;
        RefreshTiles;
      end;
    end;
    antic4Tile.selected := j;
    RefreshTiles;
  end;
end;

procedure TfrmAntic4Tiles.OnMouseDown(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  case TImage(Sender).Constraints.MinWidth of
    1: imageObject := imgTile01;
    2: imageObject := imgTile02;
    3: imageObject := imgTile03;
    4: imageObject := imgTile04;
    5: imageObject := imgTile05;
    6: imageObject := imgTile06;
    7: imageObject := imgTile07;
    8: imageObject := imgTile08;
  end;

  SelectTileProc(imageObject);

  isCharConfirm := false;
  charOffset := antic4TileArray[antic4Tile.selected].charValues[TImage(Sender).Tag];
//  debug('char', antic4Tile.selected, charOffset, TImage(Sender).Tag);
//  charOffset := 33;

  if Button = mbRight then begin
    frmCharSet := TfrmCharSet.Create(Self);
    with frmCharSet do
      try
        ShowModal;
      finally
        Free;
      end;

    if isCharConfirm then begin
      antic4TileArray[antic4Tile.selected].charValues[TImage(Sender).Tag] := charOffset;
      RefreshCharX(TImage(Sender), charOffset);
    end;
  end;
end;

procedure TfrmAntic4Tiles.itemTileDrawProc(Sender : TObject);
begin
  btnDraw.down := true;
  MiscOper(Sender);
end;

procedure TfrmAntic4Tiles.MenuItem5Click(Sender : TObject);
begin
  ColorProc(color4);
  btnDraw.Down := true;
  MiscOper(Sender);
end;

procedure TfrmAntic4Tiles.CopySelectedTileCharProc(Sender : TObject);
var
  x, y : byte;
begin
//  debug('1', selectTileChar);
  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      fldCharCopy[x, y] := fldChar[selectTileChar, x, y];
end;

procedure TfrmAntic4Tiles.PasteTileCharProc(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to _CHAR_DIM do
    for x := 0 to _CHAR_DIM do
      fldChar[selectTileChar, x, y] := fldCharCopy[x, y];

  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
end;

procedure TfrmAntic4Tiles.RefreshScreen;
var
  i : word;
begin
  for i := 0 to _ANTIC_MODE_4_SIZE - 1 do begin
    if tiles[i].tileIndex = antic4Tile.selected then begin
      coordX := tiles[i].coordX;
      coordY := tiles[i].coordY;
      case tiles[i].tileIndex of
        1: imageObject := imgTile01;
        2: imageObject := imgTile02;
        3: imageObject := imgTile03;
        4: imageObject := imgTile04;
        5: imageObject := imgTile05;
        6: imageObject := imgTile06;
        7: imageObject := imgTile07;
        8: imageObject := imgTile08;
      end;
      paintBoxPaint(imageObject as TImage);
    end;
  end;
end;

procedure TfrmAntic4Tiles.MenuOper(Sender : TObject);
begin
  case TMenuItem(Sender).Tag of
    0: btnDraw.Down := true;
    1: btnFlipX.Down := true;
    2: btnFlipY.Down := true;
    3: btnRotateTileChar.Down := true;
    4: btnInvertTileChar.Down := true;
    5: btnFillTileChar.Down := true;
    6: btnClearTileChar.Down := true;
    7: btnClearTile.Down := true;
  end;

  MiscOper(Sender);
end;

procedure TfrmAntic4Tiles.RefreshColors;
begin
  ColorRegisters;
  UpdateCharColors;
  RefreshGridData;
  RefreshTiles;
end;

procedure TfrmAntic4Tiles.ResizeTilesProc(Sender : TObject);
var
  i : byte;
begin
  setValues.caption := 'Tiles resize dialog';
  setValues.warningText := 'All tiles will be resized! Are you sure to proceed?';
  setValues.editX := 4;
  setValues.editY := 5;
  setValues.minEditX := 1;
  setValues.minEditY := 1;
  setValues.maxEditX := 4;
  setValues.maxEditY := 5;
  setValues.editText := 'dummy';

  frmSetValues := TfrmSetValues.Create(Self);
  with frmSetValues do
    try
      ShowModal;
    finally
      Free;
    end;

//  if not frmAntic4Tiles.isTilesResizeProc then exit;
  if setValues.valuesSet then begin
    frmAntic4Tiles.antic4Tile.dimX := setValues.editX;
    frmAntic4Tiles.antic4Tile.dimY := setValues.editY;

    selectTileChar := 0;
    antic4Tile.selected := 1;
    for i := 1 to _MAX_TILES do begin
      antic4TileArray[i].dimX := antic4Tile.dimX;
      antic4TileArray[i].dimY := antic4Tile.dimY;
    end;

    ShowTilesDim;
    SelectTileProc(imgTile02);
    SelectTileProc(imgTile03);
    SelectTileProc(imgTile04);
    SelectTileProc(imgTile05);
    SelectTileProc(imgTile06);
    SelectTileProc(imgTile07);
    SelectTileProc(imgTile08);
    SelectTileProc(imgTile01);
    ShowTileChars;

    RefreshScreen
  end;
end;

procedure TfrmAntic4Tiles.ClearScreenProc(Sender : TObject);
var
  i : word;
begin
  // Clear tile pointers
  for i := 0 to _ANTIC_MODE_4_SIZE - 1 do begin
    tiles[i].tileIndex := 0;
    tiles[i].coordX := 0;
    tiles[i].coordY := 0;
    tiles[i].x := 0;
    tiles[i].y := 0;
  end;

  // Clear screen
  with shapeEditor.Canvas do begin
    Brush.Color := coltab[0];
    Brush.Style := bsSolid;
    FillRect(bounds(0, 0, shapeEditor.Width, shapeEditor.Height));
  end;
end;

procedure TfrmAntic4Tiles.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F12: frmMain.Show;
  end;
end;

procedure TfrmAntic4Tiles.CloseWinProc(Sender : TObject);
begin
  Close;
end;

end.
