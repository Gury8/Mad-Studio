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
    btnLoadFont: TBitBtn;
    btnRotate: TToolButton;
    btnSaveFont: TBitBtn;
    btnFillScreen: TToolButton;
    btnViewer : TToolButton;
    chkMaxSize : TCheckBox;
    cmbAnticMode : TComboBox;
    color0 : TImage;
    color1 : TImage;
    color2 : TImage;
    color4 : TImage;
    boxResize : TGroupBox;
    editX : TBCTrackbarUpdown;
    editY : TBCTrackbarUpdown;
    GroupBox3 : TGroupBox;
    grpCharOper : TGroupBox;
    imgCharOrig : TImage;
    imgChar: TImage;
    imgEditor: TImage;
    imgFontSet: TImage;
    imgBaseFontSet: TImage;
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
    Label2: TLabel;
    Label20: TLabel;
    Label21 : TLabel;
    Label22 : TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblNum0: TLabel;
    lblNum1: TLabel;
    lblNum2: TLabel;
    lblNum3: TLabel;
    lblNum4: TLabel;
    lblNum5: TLabel;
    lblNum6: TLabel;
    lblNum7: TLabel;
    memoInfo : TMemo;
    menuAntic4: TMainMenu;
    menuFile: TMenuItem;
    MenuItem1 : TMenuItem;
    itemViewer : TMenuItem;
    itemOpenAntic2Screen : TMenuItem;
    MenuItem6 : TMenuItem;
    itemDefaultColorPalette : TMenuItem;
    itemClose: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem4 : TMenuItem;
    itemColorPalette : TMenuItem;
    menuView : TMenuItem;
    itemSaveFont: TMenuItem;
    menuEdit: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    itemSaveFontAs: TMenuItem;
    MenuItem22: TMenuItem;
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
    radMoveChar : TRadioButton;
    radShiftChar : TRadioButton;
    statusBar: TStatusBar;
    ToolBar2: TToolBar;
    btnClearCh: TToolButton;
    ToolButton17: TToolButton;
    btnLoadScreen: TToolButton;
    btnSaveScreen: TToolButton;
    btnClearScreen: TToolButton;
    btnCode: TToolButton;
    ToolButton2: TToolButton;
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
    procedure InvertProc(Sender : TObject);
    procedure itemViewerClick(Sender : TObject);
    procedure LoadAntic2Screen(Sender : TObject);
    procedure ViewerProc(Sender : TObject);
    procedure EditDimProc(Sender : TObject);
    procedure CharOper(Sender : TObject);
    procedure cmbAnticModeChange(Sender : TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgCharMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgCharMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgCharMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgFontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure editYMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure CloseWinProc(Sender: TObject);
    procedure lblNum0Click(Sender: TObject);
    procedure ClearEditorProc(Sender: TObject);
    procedure ColorsProc(Sender: TObject);
    procedure DefaultSetProc(Sender: TObject);
    procedure SaveFontAsProc(Sender: TObject);
    procedure LoadScreenProc(Sender: TObject);
    procedure FillBackWithCharClick(Sender: TObject);
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
    procedure chkMaxSizeClick(Sender : TObject);
  private
    { private declarations }
    varBtn : tMousebutton;
    btn : tMousebutton;
    offsX, offsY : byte;
    isSaveAs, isSaveFontAs : boolean;
    isFontSetNormal : boolean;
    isEdit : boolean;
    isCreate : boolean;
    ext : TExtension;
    isAntic2 : boolean;
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
    procedure FillScreen(character : byte; isBackground : boolean);
    procedure SetAnticMode(mode : byte);
    procedure OpenFile(filename : string);
    procedure ColorRegisters;
  public
    { public declarations }
    filename : string;
    fontName : string;
    fldFontSet : fldFontSetType;
    fldChar : charType;
    fldAtascii : array[0.._ANTIC_MODE_4_SIZE - 1] of byte;
    grX, grY : integer;
    chrX, chrY : byte;
    factX, factY,
    chrFactX, chrFactY,
    factX02, factY02 : byte;
    maxX, maxY : byte;  // Maximum X and Y coordinates
    maxSize : integer;
    anticMode : byte;
    modeHeight : byte;
    isTextModeChanged : boolean;
    charEditIndex : array[0..127] of byte;
    offs : byte;
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
  caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';

  isEdit := false;
  statusBar.Panels[1].Text := 'Cursor coordinates (x: 0, y: 0)';
//  imgEditor.Canvas.Brush.Color := coltab[0];
//  imgEditor.Canvas.Brush.Style := bsSolid;
//  imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));

  modeHeight := 24;
  SetXY(39, 23);
  editX.Value := maxX;
  editY.Value := maxY;
  SetAnticMode(4);
  factX := 3;
  factX02 := 3;

  // Character editor parameters
  chrX := 7; chrY := 7;
  chrFactX := 22; chrFactY := 22;

  offs := 0;
  isFontSetNormal := true;
  DefaultFontSet(fldFontSet);
  ShowFontSet;
  ShowBaseFontSet;

  FillRectEx(imgChar, colTab[0], 0, 0, imgChar.Width, imgChar.Height);

  for i := 0 to 7 do
    (FindComponent('lblNum' + IntToStr(i)) as TLabel).Caption := '0';

  FillScreen(0, false);
  isCreate := false;

  ColorRegisters;
end;

procedure TfrmAntic4.FormActivate(Sender: TObject);
begin
  ShowFontSet;
  formId := formAntic4;
//  FormStyle := fsSystemStayOnTop;
end;

procedure TfrmAntic4.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[6] := 0;
  formId := formMain;
end;

procedure TfrmAntic4.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_F12: frmMain.Show;
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

procedure TfrmAntic4.InvertProc(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to chrY do
    for x := 0 to chrX do
      fldChar[x, y] := 1 - fldChar[x, y];

  RefreshChar;
end;

procedure TfrmAntic4.itemViewerClick(Sender : TObject);
begin

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
  caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
  FillScreen(0, false);
end;

procedure TfrmAntic4.SetAnticMode(mode : byte);
begin
  anticMode := mode;
  isAntic4 := mode = 4;

  if anticMode = 4 then begin
    modeHeight := 24;
    factY := 3;
    factY02 := 3;
    editY.MaxValue := 23;
    ext.filterSave := 'Save Antic mode 4 screen as';
    ext.filter := 'Antic mode 4 screen files (*.an4)|*.an4';
  end
  else begin
    modeHeight := 48;
    factY := 6;
    factY02 := 6;
    editY.MaxValue := 11;
    ext.filterSave := 'Save Antic mode 5 screen as';
    ext.filter := 'Antic mode 5 screen files (*.an5)|*.an5';
  end;
end;

procedure TfrmAntic4.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
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

  Plot(xf, yf);
end;

procedure TfrmAntic4.imgCharMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : byte;
begin
  btn := Button;
  xf := X div chrFactX;
  yf := Y div chrFactY;

  // Check for mouse button clicked
  if btn = mbRight then
    varBtn := btn
  else
    varBtn := mbLeft;

  PlotChar(xf, yf);
  charEditIndex[offs] := 1;
end;

procedure TfrmAntic4.imgCharMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : byte;
begin
  xf := X div chrFactX;
  yf := Y div chrFactY;
  PlotChar(xf, yf);
end;

procedure TfrmAntic4.imgCharMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
  RefreshChar;
end;

procedure TfrmAntic4.imgEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
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

  // Data is changed
  if not isEdit then begin
    isEdit := true;
    if Pos(' *', caption) = 0 then
      caption := caption + ' *';
  end;

  // Plot character
  Plot(xf, yf);
end;

procedure TfrmAntic4.imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
begin
  xf := X div factX;
  yf := Y div factY;
  if (xf <= 318) and (yf <= 190) then
    Plot(xf, yf);
end;

procedure TfrmAntic4.imgEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
end;

procedure TfrmAntic4.imgFontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  n, m : byte;
begin
  if TShape(Sender).Tag = 0 then
    isFontSetNormal := true
  else
    isFontSetNormal := false;

  for m := 0 to 7 do
    for n := 0 to 15 do begin
      if (x > 24) and (x <= 42) and (y > 24*m) and (y < 28 + 24*m) then begin
        offsY := m; offsX := 1;
        offs := m shl 4 + 1;
        RefreshCharX(offs);
        break;
      end;
      if (x > 24*n) and (x <= 42 + 24*n) and (y > 24*m) and (y < 28 + 24*m) then begin
        offsY := m; offsX := n;
        offs := n + m shl 4 + 1;
        if n = 0 then Dec(offs);
        RefreshCharX(offs);
        break;
      end;
    end;
//
//  for m := 0 to chrY do begin
//    for n := 0 to chrX do begin
////      col := fldChar[xf, yf];
////      fldFontSet[xf, yf + offs shl 3] := fldChar[xf, yf];
//      imgCharOrig.Canvas.Brush.Color := coltabFont[0];
//      imgCharOrig.Canvas.FillRect(bounds(n*chrFactX, m*chrFactY, chrFactX, chrFactY));
//    end;
//  end;
//
//  imgCharOrig.Refresh;

  with memoInfo do begin
    Clear;

    // Calculate code number
    n := offs;
    if (offs >= 0) and (offs <= 63) then
      n += 32
    else if (offs >= 64) and (offs <= 95) then
      n -= 64;

    Lines.Add('Internal code Dec: ' + IntToStr(offs) + ' Hex: ' + Dec2hex(offs));
    Lines.Add('Atascii code Dec: ' + IntToStr(n) + ' Hex: ' + Dec2hex(n));
  //      if not isFontSetNormal then
  //        Lines.Add('Inverse character value + 128');
  end;

  PlotChar(255, 255);
end;

procedure TfrmAntic4.lblNum0Click(Sender: TObject);
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
    for i := 0 to 7 do
      fldChar[i, j] := StrToInt(bin[i + 1]);

    PlotChar(255, 255);
    charEditIndex[offs] := 1;
//    DrawFontSetChar(offs);
    RefreshChar;
  end;
end;

//procedure TfrmAntic4.MenuItem13Click(Sender: TObject);
//begin
////  ClearClick(Sender);
//  filename := 'examples\screen01.an4';
//  caption := programName + ' ' + programVersion +
//             ' - Antic mode 4 and 5 editor (' + filename + ')';
//end;

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
  ShowFontSet;
  RefreshData;
end;

procedure TfrmAntic4.SaveFontAsProc(Sender: TObject);
begin
  isSaveFontAs := true;
  SaveFont;
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

    //// Read color values
    //if (fs.Size = 485) or (fs.Size = 965) then begin
    //  for j := 0 to 4 do begin
    //    dta := fs.ReadByte;
    //    coltab[j] := colorMem[dta div 2];
    //    colorValues[j] := dta;
    //  end;
    //  //dta := fs.ReadByte;
    //  //coltab[10] := colorMem[dta div 2];
    //  //colorValues[10] := dta;
    //end;
  finally
    fs.Free;
//    RefreshData;
    RefreshColors;
    caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
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
  if frmMain.dlgOpen.Execute then begin
    filename := frmMain.dlgOpen.Filename;
    isAntic2 := false;
    OpenFile(filename);
  end;
end;

procedure TfrmAntic4.FillBackWithCharClick(Sender: TObject);
begin
  FillScreen(offs, true);
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
  frmMain.dlgOpen.Filter := 'Character set files (*.fnt, *.fon, *.set)|*.fnt;*.fon;*.set|All files (*.*)|*.*';
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
//      Caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
//      ClearCharClick(Sender);
//      FillByte(fldChar, SizeOf(fldChar), 0);
    finally
      fs.Free;
      RefreshColors;
//      RefreshData;
    end;
  end;
end;

procedure TfrmAntic4.SaveFontProc(Sender: TObject);
begin
  SaveFont;
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

    //// Save colors
    //for j := 0 to 4 do
    //  fs.WriteByte(colorValues[j]);

    isOk := true;
//        caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
  finally
    fs.Free;
  end;
end;

begin
  try
  // Save as
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
  // Save
  else
    SaveData;

  finally
    if isOk then begin
      isEdit := false;
      statusBar.Panels[1].Text := 'File ' + filename + ' saved';
      caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
    end;
  end;
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
  //      Caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
        ShowFontSet;
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
//      Caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
      ShowFontSet;
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

procedure TfrmAntic4.FormDeactivate(Sender: TObject);
begin
  FormStyle := fsNormal;
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

  //r := 0; m := 0;
  //for j := maxX to maxSize do begin
  //  PutChar(r, m, fldAtascii[j]);
  //  Inc(r);
  //  if j mod maxX = 0 then begin
  //    r := 0;
  //    Inc(m, 2);
  //  end;
  //end;
end;

procedure TfrmAntic4.ShowFontSet;
var
  n, cnt : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
  mask : array[0..1] of byte;
begin
  FillRectEx(imgFontSet, coltab[0], 0, 0, imgFontSet.Width, imgFontSet.Height);

  offset := 0; yoffset := 0;
  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, 24);
    end;
    cnt := 0;
    xoffset := offset*24;
    for yf := 0 to chrY do
      for xf := 0 to chrX do begin
        Inc(cnt);
        col := fldFontSet[xf, n shl 3 + yf];
        mask[cnt - 1] := col;
        if cnt = 2 then begin
//          if (mask[0] = 0) and (mask[1] = 0) then
//            col := 0
          if (mask[0] = 0) and (mask[1] = 1) then
            col := 1
          else if (mask[0] = 1) and (mask[1] = 0) then
            col := 2
          else if (mask[0] = 1) and (mask[1] = 1) then
            col := 3;

          FillRectEx(imgFontSet, coltab[col],
                     (xf - 1)*factX02 + xoffset, yf*3 + yoffset, factX02 shl 1, 3);
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

{-----------------------------------------------------------------------------
 Draw character
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.Plot(xf, yf : integer);
var
  cnt, col, i, j, xx, yy : byte;
  offset, offset2, offs2 : integer;
  mask : array[0..1] of byte;
begin
  case btn of
    mbLeft : offs2 := offs;
    mbRight: offs2 := 0;
  else
    exit;
  end;

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
  cnt := 0;
  offset2 := offs2;
  offset := offset2 shl 3;
  fldAtascii[xx + yy*(maxX + 1)] := offset2;
  for i := 0 to 7 do begin
    for j := 0 to 7 do begin
      Inc(cnt);
      col := fldFontSet[j, i + offset];
      mask[cnt - 1] := col;
      if cnt = 2 then begin
//        if (mask[0] = 0) and (mask[1] = 0) then
//          col := 0
        if (mask[0] = 0) and (mask[1] = 1) then
          col := 1
        else if (mask[0] = 1) and (mask[1] = 0) then
          col := 2
        else if (mask[0] = 1) and (mask[1] = 1) then
          col := 3;

        FillRectEx(imgEditor, coltab[col], (xf + j - 1)*factX, (yf + i)*factY, factX shl 1, factY);
        cnt := 0;
      end;
//      fldScreen[xf + dx, yf + i] := col;
    end;
  end;

  // Refresh original set
  ShowFontSet02(offset2);
end;

{-----------------------------------------------------------------------------
 Draw character on screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.PutChar(scrPos, y, offset : integer);
var
  cnt, col : byte;
  dx, dy, xf, yf : integer;
  mask : array[0..1] of byte;
begin
  xf := scrPos shl 3;
  yf := y shl 2;
  for dy := 0 to maxY do
    for dx := 0 to maxX do
      if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
        xf := dx shl 3;
        break;
      end;

  cnt := 0;
  offset := offset shl 3;
  for dy := 0 to 7 do
    for dx := 0 to 7 do begin
      Inc(cnt);
      col := fldFontSet[dx, dy + offset];
      mask[cnt - 1] := col;
      if cnt = 2 then begin
//        if (mask[0] = 0) and (mask[1] = 0) then
//          col := 0
        if (mask[0] = 0) and (mask[1] = 1) then
          col := 1
        else if (mask[0] = 1) and (mask[1] = 0) then
          col := 2
        else if (mask[0] = 1) and (mask[1] = 1) then
          col := 3;

        FillRectEx(imgEditor, coltab[col],
                   (xf + dx - 1)*factX, (yf + dy)*factY, factX shl 1, factY);
        cnt := 0;
      end;
//      fldScreen[xf + dx, yf + dy] := col;
    end;
end;

procedure TfrmAntic4.RefreshChar;
var
  xf, yf : integer;
  j : integer;
  m, r : byte;
begin
  FillRectEx(imgChar, colTab[0], 0, 0, imgChar.Width, imgChar.Height);

  for yf := 0 to chrY do
    for xf := 0 to chrX do begin
//      col := fldChar[xf, yf];
      fldFontSet[xf, yf + offs shl 3] := fldChar[xf, yf];
//      imgChar.Canvas.Brush.Color := coltabFont[fldChar[xf, yf]];
//      imgChar.Canvas.FillRect(bounds(xf*chrFactX, yf*chrFactY, chrFactX, chrFactY));
      FillRectEx(imgChar, coltabFont[fldChar[xf, yf]],
                 xf*chrFactX, yf*chrFactY, chrFactX, chrFactY);
    end;

  imgChar.Refresh;
  PlotChar(255, 255);

  r := 0; m := 0;
  for j := 0 to maxSize do begin
    if (j > maxX) and (j mod (maxX + 1) = 0) then begin
      r := 0;
      Inc(m, 2);
    end;
    if fldAtascii[j] = offs then
      PutChar(r, m, fldAtascii[j]);

    Inc(r);
  end;
end;

procedure TfrmAntic4.RefreshCharX(offset : integer);
var
  col, xf, yf : integer;
begin
  FillRectEx(imgChar, colTab[0], 0, 0, imgChar.Width, imgChar.Height);
  offset := offset shl 3;
  for yf := 0 to chrY do
    for xf := 0 to chrX do begin
      col := fldFontSet[xf, yf + offset];
      fldChar[xf, yf] := col;
      FillRectEx(imgChar, coltabFont[col], xf*chrFactX, yf*chrFactY, chrFactX, chrFactX);
      FillRectEx(imgCharOrig, coltabFont[col], xf*3, yf*3, 3, 3);
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
    FillRectEx(imgChar, coltabFont[col], xf*chrFactX, yf*chrFactY, chrFactX, chrFactY);
  end;
//  else begin
//    with memoInfo do begin
//      Clear;
//
//      // Calculate code number
//      if (offs >= 0) and (offs <= 63) then
//        i := offs + 32
//      else if (offs >= 64) and (offs <= 95) then
//        i := offs - 64
//      else
//        i := offs;
//
//      Lines.Add('Internal code Dec: ' + IntToStr(offs) + ' Hex: ' + Dec2hex(offs));
//      Lines.Add('Atascii code Dec: ' + IntToStr(i) + ' Hex: ' + Dec2hex(i));
////      if not isFontSetNormal then
////        Lines.Add('Inverse character value + 128');
//    end;
//  end;

  for j := 0 to 7 do begin
    bin := '';
    for i := 0 to 7 do
      bin += IntToStr(fldChar[i, j]);

    for n := 0 to 7 do
      case j of
        0 : lblNum0.Caption := IntToStr(bin2dec(bin)) + ' $' + Dec2Hex(bin2dec(bin));
        1 : lblNum1.Caption := IntToStr(bin2dec(bin)) + ' $' + Dec2Hex(bin2dec(bin));
        2 : lblNum2.Caption := IntToStr(bin2dec(bin)) + ' $' + Dec2Hex(bin2dec(bin));
        3 : lblNum3.Caption := IntToStr(bin2dec(bin)) + ' $' + Dec2Hex(bin2dec(bin));
        4 : lblNum4.Caption := IntToStr(bin2dec(bin)) + ' $' + Dec2Hex(bin2dec(bin));
        5 : lblNum5.Caption := IntToStr(bin2dec(bin)) + ' $' + Dec2Hex(bin2dec(bin));
        6 : lblNum6.Caption := IntToStr(bin2dec(bin)) + ' $' + Dec2Hex(bin2dec(bin));
        7 : lblNum7.Caption := IntToStr(bin2dec(bin)) + ' $' + Dec2Hex(bin2dec(bin));
      end;
  end;

  // Refresh original set
  ShowFontSet02(offs);
end;

procedure TfrmAntic4.ShowFontSet02(offset : byte);
var
  cnt : byte;
  col, xf, yf, xoffset, yoffset : integer;
  mask : array[0..1] of byte;
  fy : byte = 3;
begin
  yoffset := offsY*24;

  if offset < 16 then
    xoffset := offset*24;

  for cnt := 1 to 7 do
    if (offset >= cnt shl 4) and (offset < 32 + (cnt - 1) shl 4) then begin
      xoffset := (offset - cnt shl 4)*24;
      break;
    end;

  //if (offset >= 16) and (offset < 32) then
  //  xoffset := (offset - 16) * 24;
  //
  //if (offset >= 32) and (offset < 48) then
  //  xoffset := (offset - 16) * 24;

  cnt := 0;

  for yf := 0 to chrY do
    for xf := 0 to chrX do begin
      Inc(cnt);
      col := fldFontSet[xf, offset shl 3 + yf];
      mask[cnt - 1] := col;
      if cnt = 2 then begin
//        if (mask[0] = 0) and (mask[1] = 0) then
//          col := 0
        if (mask[0] = 0) and (mask[1] = 1) then
          col := 1
        else if (mask[0] = 1) and (mask[1] = 0) then
          col := 2
        else if (mask[0] = 1) and (mask[1] = 1) then
          col := 3;

        FillRectEx(imgFontSet, coltab[col],
                   (xf - 1)*factX02 + xoffset, yf*fy + yoffset, factX02 shl 1, fy);
        cnt := 0;
      end;
    end;
end;

procedure TfrmAntic4.ShowBaseFontSet;
var
  n : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
begin
  FillRectEx(imgBaseFontSet, coltabFont[1], 0, 0, imgBaseFontSet.Width, imgBaseFontSet.Height);
  offset := 0; yoffset := 0;
  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, 24);
    end;
    xoffset := offset*24;
    for yf := 0 to chrY do
      for xf := 0 to chrX do begin
        col := fldFontSet[xf, n shl 3 + yf];
        FillRectEx(imgBaseFontSet, coltabFont[col],
                   xf*factX02 + xoffset, yf*3 + yoffset, factX02, 3);
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
end;

{-----------------------------------------------------------------------------
 Refresh screen data
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.RefreshData;
var
  i, r, m : word;
begin
  r := 0; m := 0;
  for i := 0 to maxSize do begin
    if (i > maxX) and (i mod (maxX + 1) = 0) then begin
      r := 0;
      Inc(m, 2);
    end;
    PutChar(r, m, fldAtascii[i]);
    Inc(r);
  end;

//  imgEditor.Refresh;
  ShowFontSet;
end;

procedure TfrmAntic4.RefreshColors;
begin
  //oldColor := frmColors.SelColor;
  //
  //frmColors.SelColor := 1;
  //color0.Canvas.Rectangle(0, 0, color0.width, color0.Height);
  //frmColors.SelColor := 2;
  //color1.Canvas.Rectangle(0, 0, color1.width, color1.Height);

//  frmColors.SelColor := oldColor;
  //case oldColor of
  //  1: color0.Canvas.Rectangle(0, 0, color0.width, color0.Height);
  //  2: color1.Canvas.Rectangle(0, 0, color1.width, color1.Height);
  //  3: color2.Canvas.Rectangle(0, 0, color2.width, color2.Height);
  //  0: color4.Canvas.Rectangle(0, 0, color4.width, color4.Height);
  //end;

  RefreshData;
  ColorRegisters;
end;

{-----------------------------------------------------------------------------
 Color registers
 -----------------------------------------------------------------------------}
procedure TfrmAntic4.ColorRegisters;
begin
  //frmColors.SelColor := 0;
  //color4.Canvas.Rectangle(0, 0, color1.width, color1.Height);
  //frmColors.SelColor := 1;
  //color0.Canvas.Rectangle(0, 0, color1.width, color1.Height);

  FillRectEx(color4, coltab[0], 0, 0, color4.width, color4.Height);
  FillRectEx(color1, coltab[2], 0, 0, color1.width, color1.Height);
  FillRectEx(color2, coltab[3], 0, 0, color2.width, color2.Height);
  FillRectEx(color0, coltab[1], 0, 0, color0.width, color0.Height);
end;

procedure TfrmAntic4.editYMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  btn := mbMiddle;
  if not isCreate then
    SetXY(editX.Value, editY.Value);
end;

procedure TfrmAntic4.chkMaxSizeClick(Sender : TObject);
begin
  if chkMaxSize.Checked then begin
    editX.Value := 39;
    editY.Value := 23;
    EditDimProc(Sender);
  end;
end;

procedure TfrmAntic4.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

