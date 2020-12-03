{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Text mode 1 and 2 editor
}
unit antic6;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, strutils,
  ExtCtrls, ComCtrls, StdCtrls, Menus, BCTrackbarUpdown, lcltype,
  common;

type
  { TfrmAntic6 }
  TfrmAntic6 = class(TForm)
    btnApplyText: TButton;
    btnClearText: TButton;
    btnFillScreen: TToolButton;
    btnNewScreen: TToolButton;
    btnViewer : TToolButton;
    cmbTextMode : TComboBox;
    editTextX : TBCTrackbarUpdown;
    editText: TLabeledEdit;
    boxTextPos: TGroupBox;
    editTextY : TBCTrackbarUpdown;
    imgChar: TImage;
    imgEditor: TImage;
    imgFontSet: TImage;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    menuAntic6: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2 : TMenuItem;
    MenuItem4 : TMenuItem;
    itemViewer : TMenuItem;
    itemLoadFont : TMenuItem;
    MenuItem5 : TMenuItem;
    itemDefaultFont : TMenuItem;
    miSetDefaults : TMenuItem;
    miCloseWin: TMenuItem;
    miNew: TMenuItem;
    miFillChar: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem19: TMenuItem;
    miCodeGen: TMenuItem;
    miClear: TMenuItem;
    MenuItem3: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    statusBar: TStatusBar;
    ToolBar2: TToolBar;
    ToolButton16: TToolButton;
    btnOpen: TToolButton;
    btnSave: TToolButton;
    btnClear: TToolButton;
    btnCodeGen: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure DefaultFontProc(Sender : TObject);
    procedure LoadFontProc(Sender : TObject);
    procedure ViewerProc(Sender : TObject);
    procedure ColorPaletteProc(Sender: TObject);
    procedure FillCharProc(Sender: TObject);
    procedure DefaultsProc(Sender : TObject);
    procedure cmbTextModeChange(Sender : TObject);
    procedure WriteTextProc(Sender: TObject);
    procedure ClearTextProc(Sender: TObject);
    procedure imgEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgFontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure CloseWinProc(Sender: TObject);
    procedure ClearScreenProc(Sender: TObject);
    procedure SaveAsProc(Sender: TObject);
    procedure OpenProc(Sender: TObject);
    procedure SaveProc(Sender: TObject);
    procedure NewScreenProc(Sender: TObject);
    procedure GenCodeProc(Sender: TObject);
  private
    { private declarations }
    varBtn : tMousebutton;
    btn : tMousebutton;
    offs : byte;   // Internal character offset
    offsX : byte;
    offsY : byte;
    offsAbs : byte;  // Absolute offset
    isSaveAs : boolean;
    fldChar : charType;
    filter : string;
    getDir : string;
    isStart : boolean;
    isExtended : boolean;
    procedure Plot(xf, yf : integer);
    procedure PutChar(scrPos, y, offset : integer);
    procedure SaveScreen;
    procedure FillScreen(character : byte);
    procedure RefreshCharX(offset : integer);
    procedure OpenFile(filename : string);
  public
    { public declarations }
    filename : string;
    fldFontSet : fldFontSetType;
    fldAtascii : array[0.._TEXT_MODE_1_SIZE - 1] of byte;
    factX, factY,
    grX02, grY02,
    factX02, factY02 : byte;
    modeSize : integer;
    modeHeight : byte;
    textMode : byte;
    isTextModeChanged : boolean;
    charStatus : byte;  // Character status in character set
    procedure ShowFontSet;
    procedure RefreshData;
  end;

var
  frmAntic6: TfrmAntic6;

implementation

{$R *.lfm}

uses
  main, lib, colors, antic6_gen, viewer;

{ TfrmAntic6 }

procedure TfrmAntic6.FormCreate(Sender: TObject);
begin
  DoubleBuffered := true;
  btn := mbMiddle;

  factX := 6;
  grX02 := 7; grY02 := 7;
  factX02 := 6; factY02 := 3;
  isStart := true;
  isExtended := false;

  getDir := ExtractFilePath(Application.ExeName);

  SetTrackBarUpDown(editTextX, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editTextY, $00DDDDDD, clWhite);
end;

procedure TfrmAntic6.FormShow(Sender: TObject);
begin
  propFlagModules[5] := 1;
  isChange := true;
  frmMain.Top := 10;
  formId := formAntic6;

  cmbTextModeChange(Sender);
  filename := getDir + 'examples\screen01.gr' + IntToStr(textMode);
  caption := programName + ' ' + programVersion + ' - Text mode 1 and 2 editor (' + filename + ')';
//  statusBar.Panels[0].Text := 'Cursor coordinates: x: 0, y: 0';

  FillScreen(0);

  offs := 1;
  DefaultFontSet(fldFontSet);
  ShowFontSet;
end;

procedure TfrmAntic6.FormActivate(Sender: TObject);
begin
  formId := formAntic6;
  ShowFontSet;
end;

procedure TfrmAntic6.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[5] := 0;
  formId := formMain;
end;

procedure TfrmAntic6.ViewerProc(Sender : TObject);
begin
  frmViewer.isModal := true;
  frmViewer.ShowModal;
  if frmViewer.filename <> '' then
    OpenFile(frmViewer.filename);
end;

procedure TfrmAntic6.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_F12: frmMain.Show;
  end;
end;

procedure TfrmAntic6.DefaultFontProc(Sender : TObject);
begin
  DefaultFontSet(fldFontSet);
  RefreshData;
end;

procedure TfrmAntic6.LoadFontProc(Sender : TObject);
var
  bin : string[9];
  j : integer;
  r, i : byte;
  fs : TFileStream;
  fontname : string;
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
    finally
      fs.Free;

      // Refresh screen
      FillByte(fldChar, SizeOf(fldChar), 0);
      RefreshData;
    end;
  end;
end;

procedure TfrmAntic6.cmbTextModeChange(Sender : TObject);
begin
  if not isStart then begin
    if MessageDlg('Warning', 'You are about to change text mode.' +
                  ' Data will be lost! Do you wish to continue?',
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      case textMode of
        1: cmbTextMode.ItemIndex := 0;
        2: cmbTextMode.ItemIndex := 1;
      end;
      Exit;
    end;
  end;

  FillScreen(0);
  isStart := false;

  case cmbTextMode.ItemIndex of
    0: grMode := grMode40x24x4;
    1: grMode := grMode80x48x2;
  end;

  textMode := cmbTextMode.ItemIndex + 1;
  if textMode = 1 then begin
    modeHeight := 24;
    factY := 3;
  end
  else begin
    modeHeight := 12;
    factY := 6;
  end;
  modeSize := 20*modeHeight;
//  grY := (modeHeight - 1) shl 3;
  isAntic6 := modeHeight = 24;

  if textMode = 1 then
    filter := 'Text mode 1 screen files (*.gr1, *.an6)|*.gr1;*.an6|All files (*.*)|*.*'
  else
    filter := 'Text mode 2 screen files (*.gr2, *.an7)|*.gr2;*.an7|All files (*.*)|*.*';

  RefreshCharX(0);

  //GrModeSettings;
  //SetGrMode;
  //imgEditor.Canvas.Brush.Color := coltab[0];
  //imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));
end;

procedure TfrmAntic6.WriteTextProc(Sender: TObject);
var
  i, n : byte;
  ch : char;
  isAtascii : boolean;
begin
  for i := 1 to Length(editText.Text) do begin
    ch := editText.Text[i];
    isAtascii := false;
    if (ord(ch) >= 32) and (ord(ch) <= 95) then begin
      n := ord(ch) - 32;
      isAtascii := true;
    end
    else if (ord(ch) >= 97) and (ord(ch) <= 122) then begin
      n := ord(ch);
      isAtascii := true;
    end;
    if isAtascii then begin
      //if editTextY.Value = 0 then
      //  PutChar(editTextX.Value + i - 1, editTextY.Value, n)
      //else
      PutChar(editTextX.Value + i - 1, editTextY.Value + editTextY.Value, n);
      fldAtascii[editTextX.Value + i - 1 + editTextY.Value*20] := n;
    end;
  end;
end;

procedure TfrmAntic6.ColorPaletteProc(Sender: TObject);
begin
  frmColors.Show;
end;

procedure TfrmAntic6.FillCharProc(Sender: TObject);
begin
  FillScreen(offs);
end;

procedure TfrmAntic6.DefaultsProc(Sender : TObject);
begin
  frmColors.SetDefaultPalette;
  RefreshData;
end;

procedure TfrmAntic6.ClearTextProc(Sender: TObject);
begin
  editText.Text := '';
end;

procedure TfrmAntic6.imgEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
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

procedure TfrmAntic6.imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
begin
  xf := X div factX;
  yf := Y div factY;

  //  if yf > 192 then showmessage(inttostr(yf));
  if (xf <= 318) and (yf <= 190) then
    Plot(xf, yf);
end;

procedure TfrmAntic6.imgEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
end;

procedure TfrmAntic6.imgFontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  n, m : byte;
begin
  for m := 0 to 8 do
    for n := 0 to 19 do begin
      if (x > 48) and (x <= 100) and (y > 24*m) and (y < 28 + 24*m) then begin
        offsY := m; offsX := 1;
        offs := 20*m + 1;
        RefreshCharX(offs);
        break;
      end;
      if (x > 48*n) and (x <= 100 + 48*n) and (y > 24*m) and (y < 28 + 24*m) then begin
        offsY := m; offsX := n;
        offs := n + 20*m + 1;
        if n = 0 then Dec(offs);
        RefreshCharX(offs);
        break;
      end;
    end;

  // Uppercase alphabet, numbers, punctuation
  if offs <= 63 then begin
    offsAbs := offs;
    charStatus := _CHARSET_UPPER;
    frmColors.SelColor := 1;
  end
  // Lowercase alphabet
  else if (offs > 63) and (offs <= 63 + 26) then begin
    Dec(offs, 31);
    offsAbs := offs + 64;
    charStatus := _CHARSET_LOWER;
    frmColors.SelColor := 2;
  end
  // Inverse uppercase alphabet, numbers, punctuation
  else if (offs > 63 + 26) and (offs < 63 + 26 + 64) then begin
    Dec(offs, 89);
    offsAbs := offs + 128;
    charStatus := _CHARSET_UPPER_INV;
    frmColors.SelColor := 3;
  end
  // Inverse lowercase alphabet
  else if (offs >= 63 + 26 + 64) and (offs < 63 + 26 + 64 + 26) then begin
    Dec(offs, 89 + 31);
    offsAbs := offs + 128 + 64;
    charStatus := _CHARSET_LOWER_INV;
    frmColors.SelColor := 10;
  end;

//  statusBar.Panels[1].Text := inttostr(offs) + '/' + inttostr(offsabs);
end;

procedure TfrmAntic6.CloseWinProc(Sender: TObject);
begin
  Close;
end;

// Clear screen
procedure TfrmAntic6.ClearScreenProc(Sender: TObject);
begin
  FillScreen(0);
end;

procedure TfrmAntic6.SaveAsProc(Sender: TObject);
begin
  isSaveAs := true;
  SaveScreen;
end;

{-----------------------------------------------------------------------------
 Load text mode 1/2 (Antic mode 6/7) screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic6.OpenFile(filename : string);
var
  i : word;
  fs : TFileStream;
  dta : byte;
begin
  fs := TFileStream.Create(Filename, fmOpenReadWrite);
  try
    // Read image data
    for i := 0 to modeSize - 1 do
      if fs.Position < fs.Size then
        fldAtascii[i] := fs.ReadByte;

    // Read color values
    if fs.Position < fs.Size then
      for i := 0 to 3 do
        if fs.Position < fs.Size then begin
          dta := fs.ReadByte;
          coltab[i] := colorMem[dta div 2];
          colorValues[i] := dta;
        end;

    if fs.Position < fs.Size then begin
      dta := fs.ReadByte;
      coltab[10] := colorMem[dta div 2];
      colorValues[10] := dta;
    end;

    // Show image data
    FillByte(fldChar, SizeOf(fldChar), 0);
    RefreshData;

    caption := programName + ' ' + programVersion + ' - Text mode 1 and 2 editor (' + filename + ')';
  finally
    fs.Free;
  end;
end;

procedure TfrmAntic6.OpenProc(Sender: TObject);
begin
  frmMain.dlgOpen.Title := 'Open existing text mode ' + IntToStr(textMode) + ' screen file';
  frmMain.dlgOpen.Filter := filter;
  if frmMain.dlgOpen.Execute then begin
    filename := frmMain.dlgOpen.Filename;
    OpenFile(filename);
  end;
end;

{-----------------------------------------------------------------------------
 Save text mode 1/2 (Antic mode 6/7) screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic6.SaveProc(Sender: TObject);
begin
  SaveScreen;
//  for x := 0 to dta do
//    fs.WriteByte(colorValues[x]);
end;

procedure TfrmAntic6.NewScreenProc(Sender: TObject);
begin
//  ClearScreenProc(Sender);
//  filename := 'examples\screen01.gr' + IntToStr(textMode);
//  caption := programName + ' ' + programVersion + ' - Text mode 1 and 2 editor (' + filename + ')';

  FillScreen(0);
  filename := getDir + 'examples\screen01.gr' + IntToStr(textMode);
  caption := programName + ' ' + programVersion + ' - Text mode 1 and 2 editor (' + filename + ')';
  statusBar.Panels[0].Text := 'Cursor coordinates: x: 0, y: 0';
end;

procedure TfrmAntic6.GenCodeProc(Sender: TObject);
begin
  frmAntic6Gen := TfrmAntic6Gen.Create(Self);
  with frmAntic6Gen do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmAntic6.SaveScreen;
var
  j : integer;
  fs : TFileStream;
begin
  frmMain.dlgSave.Title := 'Save text mode ' + IntToStr(textMode) + ' screen as';
  frmMain.dlgSave.Filter := filter;

  // Save as
  if isSaveAs then begin
    if frmMain.dlgSave.Execute then begin
      filename := frmMain.dlgSave.Filename;
      fs := TFileStream.Create(Filename, fmCreate);
      try
        // Save data
        for j := 0 to modeSize - 1 do
          fs.WriteByte(fldAtascii[j]);

        // Save colors
        for j := 0 to 3 do
          fs.WriteByte(colorValues[j]);

        fs.WriteByte(colorValues[10]);

      //except
      //  on E: EFOpenError do
      //    writeln('Error writing font file occurred! Details: ', E.Message);
      //end;
        caption := programName + ' ' + programVersion + ' - Text mode 1 and 2 editor (' + filename + ')';
      finally
        fs.Free;
      end;
    end;
  end
  // Save
  else begin
    fs := TFileStream.Create(Filename, fmCreate);
    try
      // Save data
      for j := 0 to modeSize - 1 do
        fs.WriteByte(fldAtascii[j]);

      // Save colors
      for j := 0 to 3 do
        fs.WriteByte(colorValues[j]);

      fs.WriteByte(colorValues[10]);
    finally
      fs.Free;
    end;
    //except
    //  on E: EFOpenError do
    //    writeln('Error writing font file occurred! Details: ', E.Message);
    //end;
  end;
end;

// Fill screen with character
procedure TfrmAntic6.FillScreen(character : byte);
var
  j : integer;
  m, r : byte;
begin
  FillByte(fldAtascii, SizeOf(fldAtascii), character);

  r := 0; m := 0;
  for j := 0 to modeSize - 1 do begin
    if (j > 19) and (j mod 20 = 0) then begin
      r := 0;
      Inc(m, 2);
    end;
    PutChar(r, m, fldAtascii[j]);
    Inc(r);
  end;
  //r := 0; m := 0;
  //for j := 20 to modeSize do begin
  //  PutChar(r, m, fldAtascii[j]);
  //  Inc(r);
  //    if (*(j > 39) and*) (j mod 20 = 0) then begin
  //      r := 0;
  //      Inc(m, 2);
  //    end;
  //end;
end;

//procedure TfrmAntic6.ShowFontSet;
//var
//  n : byte;
//  col, offset, xoffset, yoffset : word;
//
//procedure DrawChar(colType : byte);
//var
//  xf, yf : word;
//begin
//  xoffset := offset*48;
//  for yf := 0 to grY02 do
//    for xf := 0 to grX02 do begin
//      col := fldFontSet[xf, n shl 3 + yf];
////        if col = 1 then col := 2;
//      if colType > 0 then
//        if col = 1 then col := colType;
//
//      FillRectEx(imgFontSet, coltab[col],
//                 xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
//    end;
//end;
//
//begin
//  FillRectEx(imgFontSet, coltab[1], 0, 0, imgFontSet.Width, imgFontSet.Height);
//  offset := 0; yoffset := 0;
//
//  // Uppercase alphabet, numbers, punctuation
//  for n := 0 to 63 do begin
//    if (n mod 20 = 0) and (n > 0) then begin
//      offset := 0;
//      Inc(yoffset, 24);
//    end;
//    DrawChar(2);
//  end;
//
//  // Lowercase alphabet
//  for n := 33 to 58 do begin
//    if n = 49 then begin
//      offset := 0;
//      Inc(yoffset, 24);
//    end;
//    DrawChar(2);
//    Inc(offset);
//  end;
//
//  // Inverse uppercase alphabet, numbers, punctuation
//  for n := 1 to 63 do begin
//    if (n = 11) or (n = 31) or (n = 51) then begin
//      offset := 0;
//      Inc(yoffset, 24);
//    end;
//    DrawChar(3);
//    Inc(offset);
//  end;
//
//  // Inverse lowercase alphabet
//  for n := 33 to 58 do begin
//    if n = 40 then begin
//      offset := 0;
//      Inc(yoffset, 24);
//    end;
//    DrawChar(10);
//    Inc(offset);
//  end;
//end;

procedure TfrmAntic6.ShowFontSet;
var
  n : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
begin
  FillRectEx(imgFontSet, coltab[1], 0, 0, imgFontSet.Width, imgFontSet.Height);
  offset := 0; yoffset := 0;

  // Uppercase alphabet, numbers, punctuation
  for n := 0 to 63 do begin
    if (n mod 20 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, 24);
    end;
    xoffset := offset*48;
    for yf := 0 to grY02 do
      for xf := 0 to grX02 do begin
        col := fldFontSet[xf, n shl 3 + yf];
//        if col = 1 then col := 2;

        FillRectEx(imgFontSet, coltab[col],
                   xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
      end;

    Inc(offset);
  end;

  // Lowercase alphabet
  for n := 33 to 58 do begin
    if n = 49 then begin
      offset := 0;
      Inc(yoffset, 24);
    end;
    xoffset := offset*48;
    for yf := 0 to grY02 do
      for xf := 0 to grX02 do begin
        col := fldFontSet[xf, n shl 3 + yf];
        if col = 1 then col := 2;
//        col := col shl 1;
        FillRectEx(imgFontSet, coltab[col],
                   xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
      end;

    Inc(offset);
  end;

  // Inverse uppercase alphabet, numbers, punctuation
  for n := 1 to 63 do begin
    if (n = 11) or (n = 31) or (n = 51) then begin
      offset := 0;
      Inc(yoffset, 24);
    end;
    xoffset := offset*48;
    for yf := 0 to grY02 do
      for xf := 0 to grX02 do begin
        col := fldFontSet[xf, n shl 3 + yf];
        if col = 1 then col := 3;
        FillRectEx(imgFontSet, coltab[col], xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
      end;

    Inc(offset);
  end;

  // Inverse lowercase alphabet
  for n := 33 to 58 do begin
    if n = 40 then begin
      offset := 0;
      Inc(yoffset, 24);
    end;
    xoffset := offset*48;
    for yf := 0 to grY02 do
      for xf := 0 to grX02 do begin
        col := fldFontSet[xf, n shl 3 + yf];
        if col = 1 then col := 10;
        FillRectEx(imgFontSet, coltab[col], xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
      end;

    Inc(offset);
  end;
end;

// Draw character
procedure TfrmAntic6.Plot(xf, yf : integer);
var
  col, i, j, xx, yy : byte;
  dx, dy, offset, offs2 : integer;
begin
  case btn of
    mbLeft : begin
      offs2 := offs;
    end;
    mbRight: begin
      offs2 := 0;
      offsAbs := 0;
    end
    else
      Exit;
  end;

  if xf > 19 shl 3 then xf := 19 shl 3;

  for j := 0 to modeHeight - 1 do begin
    for i := 0 to 19 do begin
      if (xf >= i shl 3) and (xf < (i + 1) shl 3) then begin
        xf := i shl 3;
        xx := i;
        break;
      end;
    end;
    if (yf > j shl 3) and (yf <= (j + 1) shl 3) then begin
      yf := j shl 3;
      yy := j;
      break;
    end;
  end;

  if xx + yy*20 > modeSize - 1 then Exit;

  statusBar.Panels[0].Text := 'Cursor coordinates: ' + inttostr(xx) + ', ' + inttostr(yy);
  offset := offs2 shl 3;
  fldAtascii[xx + yy*20] := offsAbs;
  for dy := 0 to 7 do
    for dx := 0 to 7 do begin
      col := fldFontSet[dx, dy + offset];
//      fldScreen[xf + dx, yf + dy] := col;
      fldChar[dx, dy] := col;
      if col > 0 then col := charStatus;
      FillRectEx(imgEditor, coltab[col], (xf + dx)*factX, (yf + dy)*factY, factX, factY);
    end;
end;

// Draw character
procedure TfrmAntic6.PutChar(scrPos, y, offset : integer);
var
  col, col02 : byte;
  dx, dy, xf, yf : integer;
//  isInverse : boolean = false;
begin
  xf := scrPos shl 3;
  yf := y shl 2;

  for dy := 0 to modeHeight - 1 do
    for dx := 0 to 19 do
      if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
        xf := dx shl 3;
        break;
      end;

  col02 := _CHARSET_UPPER;

  if (offset >= 97) and (offset <= 122) then begin
    col02 := _CHARSET_LOWER;
    Dec(offset, 64);
  end
  else if (offset >= 128) and (offset <= 128 + 63) then begin
    col02 := _CHARSET_UPPER_INV;
    Dec(offset, 128);
  end
  else if (offset >= 128 + 97) and (offset <= 128 + 97 + 26) then begin
    col02 := _CHARSET_LOWER_INV;
    Dec(offset, 64);
  end;

  offset := offset shl 3;

  for dy := 0 to 7 do
    for dx := 0 to 7 do begin
      col := fldFontSet[dx, dy + offset];

      if col = 1 then col := col02;
//      fldScreen[xf + dx, yf + dy] := col;

//      imgEditor.Canvas.Brush.Color := coltab[col];
//      if col02 = _CHARSET_LOWER_INV then
//        imgEditor.Canvas.FillRect(bounds((xf + dx)*factX+6, (yf + dy)*factY, factX, factY))
//      else
      FillRectEx(imgEditor, coltab[col], (xf + dx)*factX, (yf + dy)*factY, factX, factY);
    end;
end;

procedure TfrmAntic6.RefreshCharX(offset : integer);
var
  n : byte;
  col, xf, yf, offsetx : integer;
begin
  FillRectEx(imgChar, coltab[0], 0, 0, imgChar.Width, imgChar.Height);
  offsetx := 0;

  // Uppercase alphabet, numbers, punctuation
  if offset <= 63 then begin
    for n := 0 to 63 do begin
      if offset = offsetx then begin
        for yf := 0 to grY02 do
          for xf := 0 to grX02 do begin
            col := fldFontSet[xf, n shl 3 + yf];
            FillRectEx(imgChar, coltab[col], xf*factX02, yf*factY, factX02, factY);
          end;

        break;
      end;
      Inc(offsetx);
    end;
  end
  // Lowercase alphabet
  else if (offset > 63) and (offset <= 63 + 26) then begin
    offsetx := 64;
    for n := 33 to 58 do begin
      if offset = offsetx then begin
        for yf := 0 to grY02 do
          for xf := 0 to grX02 do begin
            col := fldFontSet[xf, n shl 3 + yf];
            if col = 1 then col := 2;
//            col := col shl 1;
            FillRectEx(imgChar, coltab[col], xf*factX02, yf*factY, factX02, factY);
          end;

        break;
      end;
      Inc(offsetx);
    end;
  end
  // Inverse uppercase alphabet, numbers, punctuation
  else if (offset >= 90) and (offset <= 152) then begin
    offsetx := 89;
    for n := 0 to 63 do begin
      if offset = offsetx then begin
        for yf := 0 to grY02 do
          for xf := 0 to grX02 do begin
            col := fldFontSet[xf, n shl 3 + yf];
            if col = 1 then col := 3;
            FillRectEx(imgChar, coltab[col], xf*factX02, yf*factY, factX02, factY);
          end;

        break;
      end;
      Inc(offsetx);
    end;
  end
  // Inverse lowercase alphabet
  else if (offset >= 153) and (offset <= 179) then begin
    offsetx := 153;
    for n := 33 to 58 do begin
      if offset = offsetx then begin
        for yf := 0 to grY02 do
          for xf := 0 to grX02 do begin
            col := fldFontSet[xf, n shl 3 + yf];
            if col = 1 then col := 10;
            FillRectEx(imgChar, coltab[col], xf*factX02, yf*factY, factX02, factY);
          end;

        break;
      end;
      Inc(offsetx);
    end;
  end;
  (*
  imgChar.Canvas.Brush.Color := coltab[0];
  imgChar.Canvas.Brush.Style := bsSolid;
  imgChar.Canvas.FillRect(bounds(0, 0, imgChar.Width, imgChar.Height));

  offset := offset shl 3;

  for yf := 0 to 7 do begin
    for xf := 0 to 7 do begin
      col := fldFontSet[xf, yf + offset];

      //if not isFontSetNormal then begin
      //  if col = 1 then col := 0
      //  else if col = 0 then col := 1;
      //end;

//      fldChar[xf, yf] := col;
      imgChar.Canvas.Brush.Color := coltabFont[col];
      imgChar.Canvas.FillRect(bounds(xf*3, yf*3, 3, 3));
//      imgChar.Canvas.Pixels[xf*factX, yf*factY] := coltab[col];
    end;
  end;

  imgChar.Refresh;
  *)
end;

{-----------------------------------------------------------------------------
 Refresh screen data
 -----------------------------------------------------------------------------}
procedure TfrmAntic6.RefreshData;
var
  i, r, m : word;
begin
  r := 0; m := 0;
  for i := 0 to modeSize - 1 do begin
    if (i > 19) and (i mod 20 = 0) then begin
      r := 0;
      Inc(m, 2);
    end;
    PutChar(r, m, fldAtascii[i]);
    Inc(r);
  end;

//  imgEditor.Refresh;
  ShowFontSet;
  offs := 1;
  RefreshCharX(offs);
  frmColors.RefreshColors;
end;

end.

