{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2023
  Unit: Text mode 0 (Antic mode 2) editor
}
unit antic2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ComCtrls, Menus, StdCtrls, strutils, lcltype,
  common;

type
  { TfrmAntic2 }
  TfrmAntic2 = class(TForm)
    boxCharInfo : TGroupBox;
    btnViewer : TToolButton;
    chkMaxSize : TCheckBox;
    editX : TSpinEditEx;
    boxResize : TGroupBox;
    editY : TSpinEditEx;
    imgChar : TImage;
    imgEditor: TImage;
    imgFontSet: TImage;
    imgFontSetInv: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label5 : TLabel;
    Label6 : TLabel;
    lblCharInfo01 : TLabel;
    lblCharInfo02 : TLabel;
    lblCharInfo03 : TLabel;
    menuAntic2: TMainMenu;
    MenuItem1: TMenuItem;
    itemViewer : TMenuItem;
    menuView : TMenuItem;
    itemDefaultFont : TMenuItem;
    itemLoadCharSet : TMenuItem;
    miClose: TMenuItem;
    miNewScreen: TMenuItem;
    miFillScreen: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem19: TMenuItem;
    miGenCode: TMenuItem;
    miClearScreen: TMenuItem;
    MenuItem3: TMenuItem;
    miLoadScreen: TMenuItem;
    miSaveScreen: TMenuItem;
    miSaveScreenAs: TMenuItem;
    statusBar: TStatusBar;
    ToolBar2: TToolBar;
    btnNewScreen: TToolButton;
    btnFillScreen: TToolButton;
    btnApplyText : TToolButton;
    ToolButton16: TToolButton;
    btnLoadScreen: TToolButton;
    btnSaveScreen: TToolButton;
    ToolButton25: TToolButton;
    btnClearScreen: TToolButton;
    btnGenCode: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ApplyTextProc(Sender: TObject);
    procedure editTextXChange(Sender : TObject);
    procedure ScreenSizeProc(Sender : TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure LoadFontProc(Sender : TObject);
    procedure DefaultFontProc(Sender : TObject);
    procedure ViewerProc(Sender : TObject);
    procedure imgEditorDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgEditorMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgEditorUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgFontSetDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure CloseWinProc(Sender: TObject);
    procedure GenCodeProc(Sender: TObject);
    procedure FillScreenProc(Sender: TObject);
    procedure NewScreenProc(Sender: TObject);
    procedure ColorsProc(Sender: TObject);
    procedure SaveScreenAsProc(Sender: TObject);
    procedure SaveScreenProc(Sender: TObject);
    procedure SaveProc(fs : TFileStream);
    procedure LoadScreenProc(Sender: TObject);
    procedure ClearScreenProc(Sender: TObject);
    procedure MaxSizeProc(Sender : TObject);
  private
    { private declarations }
    btn : TMousebutton;
    offs : byte;
    // offsX, offsY : byte;
    isSaveAs : boolean;
    isFontSetNormal : boolean;
    fldChar : charType;
    isCreate : boolean;
    isEdit : boolean;
    procedure ShowFontSet;
    procedure Plot(xf, yf : integer);
    procedure PutChar(scrPos, y, offset : integer);
    procedure FillScreen(character : byte);
    procedure SetXY(_maxX, _maxY : byte);
    procedure RefreshCharX(offset : integer);
    procedure OpenFile(filename : string);
    procedure CharInfo(offset : integer);
  public
    { public declarations }
    filename : string;
    fontName : string;
    fldFontSet : fldFontSetType;
//    fldScreen : array[0..40*8, 0..24*8] of byte;
    fldAtascii : array[0..960] of byte;
    grX, grY : integer;
    factX, factY,
    factX02, factY02 : byte;
    maxX, maxY : byte;  // Maximum X and Y coordinates
    maxSize : integer;
    procedure RefreshData;
  end;

var
  frmAntic2: TfrmAntic2;

implementation

{$R *.lfm}

uses
  main, lib, antic2_gen, colors, viewer, set_values;

{ TfrmAntic2 }

procedure TfrmAntic2.FormCreate(Sender: TObject);
begin
  DoubleBuffered := true;
  btn := mbMiddle;
  isCreate := true;
end;

procedure TfrmAntic2.FormShow(Sender: TObject);
begin
  propFlagModules[4] := 1;

  frmMain.Top := 10;
  formId := formAntic2;

  filename := getDir + 'examples\screen01.gr0';
  caption := programName + ' ' + programVersion + ' - Text mode 0 editor (' + filename + ')';

  statusBar.Panels[1].Text := 'Editor cursor coordinates (x: 0, y: 0)';
  FillRectEx(imgEditor, coltabFont[0], 0, 0, imgEditor.Width, imgEditor.Height);

  isEdit := false;
  SetXY(39, 23);
  editX.Value := maxX;
  editY.Value := maxY;
  factX := 3; factY := 3;
  factX02 := 3; factY02 := 3;
  isFontSetNormal := true;
  DefaultFontSet(fldFontSet);
  ShowFontSet;

  offs := 1;
  RefreshCharX(offs);

  FillScreen(0);
  isCreate := false;
end;

procedure TfrmAntic2.FormActivate(Sender: TObject);
begin
  formId := formAntic2;
//  FormStyle := fsSystemStayOnTop;
end;

procedure TfrmAntic2.FormDeactivate(Sender: TObject);
begin
  FormStyle := fsNormal;
end;

procedure TfrmAntic2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[4] := 0;
  formId := formMain;
end;

procedure TfrmAntic2.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_F12: frmMain.Show;
  end;
end;

procedure TfrmAntic2.ViewerProc(Sender : TObject);
begin
  frmViewer.isModal := true;
  frmViewer.ShowModal;
  if frmViewer.filename <> '' then
    OpenFile(frmViewer.filename);
end;

{-----------------------------------------------------------------------------
 Load custom character set
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.LoadFontProc(Sender : TObject);
var
  bin : string[9];
  j : integer;
  r, i : byte;
  fs : TFileStream;
begin
  frmMain.dlgOpen.Title := 'Open existing character set file';
  frmMain.dlgOpen.Filter := 'Character set files (*.fnt, *.fon, *.set)' +
                            '|*.fnt;*.fon;*.set|All files (*.*)|*.*';
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
      ShowFontSet;
    end;
  end;
end;

procedure TfrmAntic2.ScreenSizeProc(Sender : TObject);
begin
  btn := mbMiddle;
  if not isCreate then
    SetXY(editX.Value, editY.Value);

  //if (editX.Value < 39) or (editY.Value < 23) then
  //  chkMaxSize.Checked := false;

  chkMaxSize.Checked := (editX.Value = 39) and (editY.Value = 23);

  if maxSize = 959 then
    filename := getDir + 'examples\screen01.gr0'
  else
    filename := getDir + 'examples\screen01.an2';

  caption := programName + ' ' + programVersion + ' - Text mode 0 editor (' + filename + ')';
end;

procedure TfrmAntic2.DefaultFontProc(Sender : TObject);
begin
  DefaultFontSet(fldFontSet);
  ShowFontSet;
  RefreshData;
end;

procedure TfrmAntic2.editTextXChange(Sender : TObject);
begin
  //if not isCreate then
  //  SetXY(editX.Value, editY.Value);
end;

procedure TfrmAntic2.SetXY(_maxX, _maxY : byte);
begin
  //maxX := _maxX + 1;
  //maxY := _maxY + 1;
  //maxSize := maxX*maxY - 1;
  //grX := maxX shl 3;
  //grY := maxY shl 3;
  //imgEditor.Width := maxX*24;
  //imgEditor.Height := maxY*24;
  //statusBar.Panels[0].Text := 'Max X coord.: ' + IntToStr(_maxX) +
  //                            ', max Y coord.: ' + IntToStr(_maxY);
  maxX := _maxX;
  maxY := _maxY;
  maxSize := (maxX + 1)*(maxY + 1) - 1;
  grX := (maxX + 1) shl 3;
  grY := (maxY + 1) shl 3;
  imgEditor.Width := (maxX + 1)*24;
  imgEditor.Height := (maxY + 1)*24;
  imgEditor.Invalidate;
  statusBar.Panels[0].Text := 'Max X coord.: ' + IntToStr(maxX) +
                              ', max Y coord.: ' + IntToStr(maxY);
end;

procedure TfrmAntic2.ApplyTextProc(Sender: TObject);
var
  i, n : byte;
  ch : char;
  isAtascii : boolean;
begin
  setValues.caption := 'Set text position dialog';
  setValues.warningText := '';
  setValues.editX := 3;
  setValues.editY := 3;
  setValues.minEditX := 0;
  setValues.minEditY := 0;
  setValues.maxEditX := 39;
  setValues.maxEditY := 19;
  setValues.editText := '';

  frmSetValues := TfrmSetValues.Create(Self);
  with frmSetValues do
    try
      ShowModal;
    finally
      Free;
    end;

  if setValues.valuesSet then begin
  //  showmessage(chr(97) + ' ' + inttostr(ord('b')));  // A 65

    for i := 1 to Length(setValues.editText) do begin
      ch := setValues.editText[i];
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
        if setValues.editY = 0 then
          PutChar(setValues.editX + i - 1, setValues.editY, n)
        else
          PutChar(setValues.editX + i - 1, setValues.editY + setValues.editY, n);

        fldAtascii[setValues.editX + i - 1 + setValues.editY*(maxX + 1)] := n;
      end;
    end;
  end;
end;

procedure TfrmAntic2.imgEditorDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : integer;
begin
  btn := Button;
  xf := X div factX;
  yf := Y div factY;
  Plot(xf, yf);

  // Data is changed
  if not isEdit then begin
    isEdit := true;
    if Pos(' *', caption) = 0 then
      caption := caption + ' *';
  end;
end;

procedure TfrmAntic2.imgEditorMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
begin
  xf := X div factX;
  yf := Y div factY;

  //  if yf > 192 then showmessage(inttostr(yf));
  if (xf <= 318) and (yf <= 190) then
    Plot(xf, yf);

//  statusBar.Panels[1].Text := 'Editor cursor coordinates (x: ' + inttostr(xf) + ', y: ' + inttostr(yf) + ')';
end;

procedure TfrmAntic2.imgEditorUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
end;

procedure TfrmAntic2.imgFontSetDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  n, m : byte;
begin
  if TImage(Sender).Tag = 0 then
    isFontSetNormal := true
  else
    isFontSetNormal := false;

  for m := 0 to 7 do begin
    for n := 0 to 15 do begin
      if (x > 24) and (x <= 42) and (y > 24*m) and (y < 28 + 24*m) then begin
//        offsY := m; offsX := 1;
        offs := m shl 4 + 1;
        RefreshCharX(offs);
        break;
      end;
      if (x > 24*n) and (x <= 42 + 24*n) and (y > 24*m) and (y < 28 + 24*m) then begin
//        offsY := m; offsX := n;
        offs := n + m shl 4 + 1;
        if n = 0 then Dec(offs);
        RefreshCharX(offs);
        break;
      end;
    end;
  end;

//  CharInfo(offs);
(*
    //if isFontSetNormal then
    //  m := offs
    //else begin
    //  m := offs + 128;
    //  n += 128;
    //end;

    Lines.Add('Internal code Dec: ' + IntToStr(offs) + ' Hex: ' + Dec2hex(offs));
    if isFontSetNormal then
      Lines.Add('Atascii code Dec: ' + IntToStr(n) + ' Hex: ' + Dec2hex(n))
    else
      Lines.Add('Atascii code Dec: ' + IntToStr(n + 128) + ' Hex: ' + Dec2hex(n + 128));

//  if chkFillChar.Checked then begin
////    if not isFontSetNormal then Inc(offs, 128);
//    FillScreen(offs);
//  end;
*)
end;

procedure TfrmAntic2.CloseWinProc(Sender: TObject);
begin
  Close;
end;

{-----------------------------------------------------------------------------
 Text mode 0 screen code generator
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.GenCodeProc(Sender: TObject);
begin
  frmAntic2Gen := TfrmAntic2Gen.Create(Self);
  with frmAntic2Gen do
    try
      ShowModal;
    finally
      Free;
    end;
end;

{-----------------------------------------------------------------------------
 Fill screen with selected character
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.FillScreenProc(Sender: TObject);
begin
  FillScreen(offs);
end;

{-----------------------------------------------------------------------------
 Clear screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.NewScreenProc(Sender: TObject);
begin
  isFontSetNormal := true;
  ClearScreenProc(Sender);
  filename := 'examples\screen01.gr0';
  caption := programName + ' ' + programVersion + ' - Text mode 0 editor (' + filename + ')';
end;

{-----------------------------------------------------------------------------
 Show color palette
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.ColorsProc(Sender: TObject);
begin
  frmColors.Show;
end;

procedure TfrmAntic2.SaveProc(fs : TFileStream);
var
  j : integer;
begin
//  debug('save values', maxX, maxY, maxSize);
  isEdit := false;

  // Text mode 0 standard format
  if (editX.Value = 39) and (editY.Value = 23) then begin
    for j := 0 to maxSize do
      fs.WriteByte(fldAtascii[j]);
  end
  // Text mode 0 packed/extended format
  else begin
    // Dimension x, y
    fs.WriteByte(maxX);
    fs.WriteByte(maxY);

    // Save data
    for j := 0 to maxSize do
      fs.WriteByte(fldAtascii[j]);
  end;
end;

{-----------------------------------------------------------------------------
 Save text mode 0 (Antic mode 2) screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.SaveScreenProc(Sender: TObject);
var
  fs : TFileStream;
begin
  // Save as
  if isSaveAs then begin
    frmMain.dlgSave.Title := 'Save text mode 0 screen as';

    if (editX.Value = 39) and (editY.Value = 23) then
      frmMain.dlgSave.Filter := 'Text mode 0 screen files (*.gr0, *.asc)' +
                                '|*.gr0;*.asc|All files (*.*)|*.*'
    else
      frmMain.dlgSave.Filter := 'Text mode 0 screen files (*.an2)|*.an2|All files (*.*)|*.*';

    frmMain.dlgSave.Filename := filename;
    if frmMain.dlgSave.Execute then begin
      filename := frmMain.dlgSave.Filename;
      fs := TFileStream.Create(filename, fmCreate);
      try
        SaveProc(fs);
        caption := programName + ' ' + programVersion + ' - Text mode 0 editor (' + filename + ')';
      //except
      //  on E: EFOpenError do
      //    writeln('Error writing font file occurred! Details: ', E.Message);
      //end;
      finally
        fs.Free;
      end;
    end;
  end
  // Save
  else begin
    fs := TFileStream.Create(Filename, fmCreate);
    try
      SaveProc(fs);
      caption := programName + ' ' + programVersion + ' - Text mode 0 editor (' + filename + ')';
    finally
      fs.Free;
    end;
  end;
end;

{-----------------------------------------------------------------------------
 Save text mode 0 (Antic mode 2) screen as
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.SaveScreenAsProc(Sender: TObject);
begin
  isSaveAs := true;
  SaveScreenProc(Sender);
  isSaveAs := false;
end;

{-----------------------------------------------------------------------------
 Load text mode 0 (Antic mode 2) screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.OpenFile(filename : string);
var
  j, r, m : integer;
  fs : TFileStream;
begin
  isEdit := false;

//  filename := LowerCase(filename);
  fs := TFileStream.Create(filename, fmOpenReadWrite);
  try
    //if (editX.Value = 39) and (editY.Value = 23) then begin
    // Read image dimension
    if fs.Size < 960 then begin
      maxX := fs.ReadByte;
      maxY := fs.ReadByte;
    end
    else begin
      maxX := 39;
      maxY := 23;
    end;

    SetXY(maxx, maxy);
    editX.Value := maxX;
    editY.Value := maxY;
//    debug('load values', maxX, maxY, maxSize);

    r := 0; m := 0;
    for j := 0 to maxSize do begin
      if fs.Position < fs.Size then begin
        fldAtascii[j] := fs.ReadByte;
        if (j > maxX) and (j mod (maxX + 1) = 0) then begin
          r := 0;
          Inc(m, 2);
        end;
        PutChar(r, m, fldAtascii[j]);
        Inc(r);
      end;
    end;

    FillByte(fldChar, SizeOf(fldChar), 0);
    caption := programName + ' ' + programVersion + ' - Text mode 0 editor (' + filename + ')';
  finally
    fs.Free;
  end;
  //except
  //  on E: EFOpenError do
  //    writeln('Error reading font file occurred! Details: ', E.Message);
  //end;
end;

{-----------------------------------------------------------------------------
 Load Antic 2 screen from file
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.LoadScreenProc(Sender: TObject);
begin
  frmMain.dlgOpen.Title := 'Open existing Text mode 0 screen file';
  frmMain.dlgOpen.Filter := 'Text mode 0 screen files' +
                            ' (*.gr0, *.asc, *.an2)|*.gr0;*.asc;*.an2|All files (*.*)|*.*';

  if frmMain.dlgOpen.Execute then begin
    filename := frmMain.dlgOpen.Filename;
    OpenFile(filename);
  end;
end;

{-----------------------------------------------------------------------------
 Fill screen with character
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.FillScreen(character : byte);
var
  j : integer;
  m, r : byte;
begin
  if not isFontSetNormal then Inc(character, 128);
  FillByte(fldAtascii, SizeOf(fldAtascii), character);
  r := 0; m := 0;
  for j := 0 to maxSize do begin
    if (j > maxX) and (j mod (maxX + 1) = 0) then begin
      r := 0;
      Inc(m, 2);
    end;
    PutChar(r, m, fldAtascii[j]);
    Inc(r);
  end;
end;

{-----------------------------------------------------------------------------
 Clear screen
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.ClearScreenProc(Sender: TObject);
begin
  isFontSetNormal := true;
  FillScreen(0);
end;

procedure TfrmAntic2.ShowFontSet;
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
    for yf := 0 to _CHAR_DIM do begin
      for xf := 0 to _CHAR_DIM do begin
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
    end;
    Inc(offset);
  end;
end;

{-----------------------------------------------------------------------------
 Draw character
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.Plot(xf, yf : integer);
var
  col, i, j, xx, yy : byte;
  dx, dy, offset, offset2, offs2 : integer;
begin
  case btn of
    mbLeft : offs2 := offs;
    mbRight: offs2 := 0;
  else
    exit;
  end;

//  if xf > grX then xf := grX;
//  if xf mod 8 = 0 then Inc(xf, 8);

  for j := 0 to maxY do begin
    for i := 0 to maxX do begin
      if (xf >= i shl 3) and (xf < (i + 1) shl 3) then begin
        // 8*i = i shl 3
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

  if xx + yy*(maxX + 1) > maxSize then Exit;

  statusBar.Panels[1].Text := 'Editor cursor coordinates (x: ' +
                              inttostr(xx) + ', y: ' + inttostr(yy) + ')';

  //if (xf > 0) and (xf < 7) then xf := 0;
  //if (xf > 7) and (xf < 14) then xf := 7;
  //if (xf > 14) and (xf < 21) then xf := 14;

  offset2 := offs2;
  offset := offset2 shl 3;

  if not isFontSetNormal then Inc(offset2, 128);
  fldAtascii[xx + yy*(maxX + 1)] := offset2;

  for dy := 0 to 7 do begin
    for dx := 0 to 7 do begin
      col := fldFontSet[dx, dy + offset];
      if not isFontSetNormal then
        col := 1 - col;

      fldChar[dx, dy] := col;
//      fldScreen[xf + dx, yf + dy] := col;
//      col := SetColorIndex(col, isFontSetNormal);
      FillRectEx(imgEditor, coltabFont[col], (xf + dx)*factX, (yf + dy)*factY, factX, factY);
    end;
  end;
end;

{-----------------------------------------------------------------------------
 Draw character
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.PutChar(scrPos, y, offset : integer);
var
  col : byte;
  dx, dy, xf, yf : integer;
  isInverse : boolean = false;
begin
  xf := scrPos shl 3;
  yf := y shl 2;
//  showmessage(inttostr(scrPos) + ' ' + inttostr(y) + ' ' + inttostr(offset));
//  if xf > grX then xf := grX;
//  if xf mod 8 = 0 then Inc(xf, 8);
  for dy := 0 to maxY do begin
    for dx := 0 to maxX do
      if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
        xf := dx shl 3;
        break;
      end;
  end;
  if offset >= 128 then begin
    Dec(offset, 128);
    isInverse := true;
  end;

  offset := offset shl 3;

  for dy := 0 to 7 do begin
    for dx := 0 to 7 do begin
      col := fldFontSet[dx, dy + offset];
//      fldScreen[xf + dx, yf + dy] := col;

      if isInverse then
        col := 1 - col;

      FillRectEx(imgEditor, coltabFont[col], (xf + dx)*factX, (yf + dy)*factY, factX, factY);
    end;
  end;
end;

procedure TfrmAntic2.RefreshCharX(offset : integer);
var
  col, xf, yf : integer;
begin
  FillRectEx(imgChar, clBlack, 0, 0, imgChar.Width, imgChar.Height);
  offset := offset shl 3;
  for yf := 0 to 7 do begin
    for xf := 0 to 7 do begin
      col := fldFontSet[xf, yf + offset];
      if not isFontSetNormal then
        col := 1 - col;

      fldChar[xf, yf] := col;
      FillRectEx(imgChar, coltabFont[col], xf*(factX + 1), yf*(factY + 1), factX + 1, factY + 1);
    end;
  end;

  imgChar.Refresh;
  CharInfo(offset);
end;

{-----------------------------------------------------------------------------
 Refresh screen data
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.RefreshData;
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
//  frmColors.RefreshColors;
end;

procedure TfrmAntic2.MaxSizeProc(Sender : TObject);
begin
  if chkMaxSize.Checked then begin
    editX.Value := 39;
    editY.Value := 23;
    ScreenSizeProc(Sender);
  end;
end;

{-----------------------------------------------------------------------------
 Show character information
 -----------------------------------------------------------------------------}
procedure TfrmAntic2.CharInfo(offset : integer);
var
  n : byte;
begin
  if offset < 0 then begin
    lblCharInfo01.Caption := '';
    lblCharInfo02.Caption := '';
    lblCharInfo03.Caption := '';
  end
  else begin
    n := StrToInt(AtasciiCode(offset));
    lblCharInfo01.Caption := 'Internal code Dec: ' + IntToStr(offset) +
                             ' Hex: ' + Dec2hex(offset);
    lblCharInfo02.Caption := 'Atascii code Dec: ' + IntToStr(n) +
                             ' Hex: ' + Dec2hex(n);
    lblCharInfo03.Caption := 'Atascii inverse Dec: ' + IntToStr(n + 128) +
                             ' Hex: ' + Dec2hex(n + 128);
  end;
end;

end.

