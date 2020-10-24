{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2019
  Unit: Text mode 0 (Antic mode 2) editor
}
unit antic2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ComCtrls, Menus, StdCtrls, Spin, common;

type
  { TfrmAntic2 }
  TfrmAntic2 = class(TForm)
    btnApplyText: TButton;
    btnClearText: TButton;
    editText: TLabeledEdit;
    editTextX: TSpinEdit;
    editTextY: TSpinEdit;
    editX: TSpinEdit;
    editY: TSpinEdit;
    boxTextPos: TGroupBox;
    GroupBox2: TGroupBox;
    imgChar: TImage;
    imgEditor: TImage;
    imgFontSet: TImage;
    imgFontSetInv: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    memoInfo: TMemo;
    menuAntic2: TMainMenu;
    MenuItem1: TMenuItem;
    miClose: TMenuItem;
    MenuItem12: TMenuItem;
    miNewScreen: TMenuItem;
    miFillScreen: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem19: TMenuItem;
    miGenCode: TMenuItem;
    miClearScreen: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    miLoadScreen: TMenuItem;
    miSaveScreen: TMenuItem;
    miSaveScreenAs: TMenuItem;
    statusBar: TStatusBar;
    ToolBar2: TToolBar;
    btnNewScreen: TToolButton;
    btnFillScreen: TToolButton;
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
    procedure ApplyText(Sender: TObject);
    procedure ClearText(Sender: TObject);
    procedure editTextXChange(Sender : TObject);
    procedure imgEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgFontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure CloseWinProc(Sender: TObject);
    procedure GenCodeProc(Sender: TObject);
    procedure FillScreenProc(Sender: TObject);
    procedure NewScreenProc(Sender: TObject);
    procedure ColorsProc(Sender: TObject);
    procedure SaveScreenAsProc(Sender: TObject);
    procedure SaveScreenProc(Sender: TObject);
    procedure LoadScreenProc(Sender: TObject);
    procedure ClearScreenProc(Sender: TObject);
  private
    { private declarations }
    varBtn : tMousebutton;
    btn : tMousebutton;
    offs, offsX, offsY : byte;
    isSaveAs : boolean;
    isFontSetNormal : boolean;
    fldChar : charType;
    isCreate : boolean;
    procedure ShowFontSet;
    procedure Plot(xf, yf : integer);
    procedure PutChar(scrPos, y, offset : integer);
    procedure FillScreen(character : byte);
    procedure SetXY(_maxX, _maxY : byte);
    procedure RefreshCharX(offset : integer);
  public
    { public declarations }
    filename : string;
    fldFontSet : fldFontSetType;
//    fldScreen : array[0..40*8, 0..24*8] of byte;
    fldAtascii : array[0..960] of byte;
    grX, grY : integer;
    factX, factY,
    grX02, grY02,
    factX02, factY02 : byte;
    maxX, maxY : byte;  // Maximum X and Y coordinates
    maxSize : integer;
  end;

var
  frmAntic2: TfrmAntic2;

implementation

{$R *.lfm}

uses
  main, lib, antic2_gen, colors;

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
  isChange := true;

  frmMain.Top := 10;
  formId := formAtascii;

  filename := getDir + 'examples\screen01.gr0';
  caption := programName + ' ' + programVersion + ' - Text mode 0 editor (' + filename + ')';

  statusBar.Panels[1].Text := 'Editor cursor coordinates (x: 0, y: 0)';

  imgEditor.Canvas.Brush.Color := coltabFont[0];
  imgEditor.Canvas.Brush.Style := bsSolid;
  imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));

  SetXY(39, 23);
  editX.Value := maxX - 1;
  editY.Value := maxY - 1;
  factX := 3; factY := 3;
  grX02 := 7; grY02 := 7;
  factX02 := 3; factY02 := 3;
  offs := 1;
  isFontSetNormal := true;
  DefaultFontSet(fldFontSet);
  ShowFontSet;

  FillScreen(0);
  isCreate := false;
end;

procedure TfrmAntic2.FormActivate(Sender: TObject);
begin
  formId := formAtascii;
//  FormStyle := fsSystemStayOnTop;
end;

procedure TfrmAntic2.FormDeactivate(Sender: TObject);
begin
  FormStyle := fsNormal;
end;

procedure TfrmAntic2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[4] := 0;
end;

procedure TfrmAntic2.editTextXChange(Sender : TObject);
begin
  if not isCreate then
    SetXY(editX.Value, editY.Value);
end;

procedure TfrmAntic2.SetXY(_maxX, _maxY : byte);
begin
  maxX := _maxX + 1; maxY := _maxY + 1;
  maxSize := maxX*maxY - 1;
  grX := maxX shl 3;
  grY := maxY shl 3;
  imgEditor.Width := maxX*24;
  imgEditor.Height := maxY*24;
  statusBar.Panels[0].Text := 'Max X coord.: ' + IntToStr(_maxX) +
                              ', max Y coord.: ' + IntToStr(_maxY);
end;

procedure TfrmAntic2.ApplyText(Sender: TObject);
var
  i, n : byte;
  ch : char;
  isAtascii : boolean;
begin
//  showmessage(chr(97) + ' ' + inttostr(ord('b')));  // A 65
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

    //if (j > 39) and (j mod 40 = 0) then begin
    //  r := 0;
    //  Inc(m, 2);
    //end;

    if isAtascii then begin
      if editTextY.Value = 0 then
        PutChar(editTextX.Value + i - 1, editTextY.Value, n)
      else
        PutChar(editTextX.Value + i - 1, editTextY.Value + editTextY.Value, n);

      fldAtascii[editTextX.Value + i - 1 + editTextY.Value*maxX] := n;
    end;
  end;
end;

procedure TfrmAntic2.ClearText(Sender: TObject);
begin
  editText.Text := '';
end;

procedure TfrmAntic2.imgEditorMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TfrmAntic2.imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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

procedure TfrmAntic2.imgEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
end;

procedure TfrmAntic2.imgFontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
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
        offsY := m; offsX := 1;
        offs := 16*m + 1;
        RefreshCharX(offs);
        break;
      end;
      if (x > 24*n) and (x <= 42 + 24*n) and (y > 24*m) and (y < 28 + 24*m) then begin
        offsY := m; offsX := n;
        offs := n + 16*m + 1;
        if n = 0 then Dec(offs);
        RefreshCharX(offs);
        break;
      end;
    end;
  end;

  with memoInfo do begin
    Clear;

    // Calculate code number
    n := offs;
    if (offs >= 0) and (offs <= 63) then
      n += 32
    else if (offs >= 64) and (offs <= 95) then
      n -= 64;

    m := offs;

    //if isFontSetNormal then
    //  m := offs
    //else begin
    //  m := offs + 128;
    //  n += 128;
    //end;

    Lines.Add('Internal code Dec: ' + IntToStr(m) + ' Hex: ' + Dec2hex(m));
    Lines.Add('Atascii code Dec: ' + IntToStr(n) + ' Hex: ' + Dec2hex(n));
    if not isFontSetNormal then
      Lines.Add('Inverse character value + 128');
  end;

//  if chkFillChar.Checked then begin
////    if not isFontSetNormal then Inc(offs, 128);
//    FillScreen(offs);
//  end;
end;

procedure TfrmAntic2.CloseWinProc(Sender: TObject);
begin
  Close;
end;

// Text mode 0 screen code generator
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

procedure TfrmAntic2.FillScreenProc(Sender: TObject);
begin
  FillScreen(offs);
end;

procedure TfrmAntic2.NewScreenProc(Sender: TObject);
begin
  ClearScreenProc(Sender);
  filename := 'examples\screen01.gr0';
  caption := programName + ' ' + programVersion + ' - Text mode 0 editor (' + filename + ')';
end;

procedure TfrmAntic2.ColorsProc(Sender: TObject);
begin
  frmColors.Show;
end;

procedure TfrmAntic2.SaveScreenProc(Sender: TObject);
var
  j : integer;
  fs : TFileStream;
begin
  // Save as
  if isSaveAs then begin
    frmMain.dlgSave.Title := 'Save text mode 0 screen as';
    frmMain.dlgSave.Filter := 'Text mode 0 screen files (*.scr, *.asc, *.gr0)|*.scr;*.asc;*.gr0|All files (*.*)|*.*';
    frmMain.dlgSave.Filename := filename;
    if frmMain.dlgSave.Execute then begin
      filename := frmMain.dlgSave.Filename;
      fs := TFileStream.Create(filename, fmCreate);
      try
        for j := 0 to maxSize do
          fs.WriteByte(fldAtascii[j]);

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
      for j := 0 to maxSize do
        fs.WriteByte(fldAtascii[j]);
    finally
      fs.Free;
    end;
    //except
    //  on E: EFOpenError do
    //    writeln('Error writing font file occurred! Details: ', E.Message);
    //end;
  end;
end;

//// Save Atascii screen
//procedure TfrmAntic2.SaveScreen(Sender: TObject);
//begin
//  //if filename = 'screen01.gr0' then
//  //  isSaveAs := true
//  //else
//  //  isSaveAs := false;
//
//  SaveScreen;
//end;

// Save Atascii screen as
procedure TfrmAntic2.SaveScreenAsProc(Sender: TObject);
begin
  isSaveAs := true;
  SaveScreenProc(Sender);
end;

// Load Atascii screen from file
procedure TfrmAntic2.LoadScreenProc(Sender: TObject);
var
  j, r, m : integer;
  fs : TFileStream;
begin
  frmMain.dlgOpen.Title := 'Open existing Text mode 0 screen file';
  frmMain.dlgOpen.Filter :=
    'Text mode 0 screen files (*.scr, *.asc, *.gr0)|*.scr;*.asc;*.gr0|All files (*.*)|*.*';
  if frmMain.dlgOpen.Execute then begin
    filename := frmMain.dlgOpen.Filename;
    fs := TFileStream.Create(Filename, fmOpenReadWrite);
    try
      r := 0; m := 0;
      for j := 0 to maxSize do begin
        fldAtascii[j] := fs.ReadByte;
        if (j > (maxX - 1)) and (j mod maxX = 0) then begin
          r := 0;
          Inc(m, 2);
        end;
        PutChar(r, m, fldAtascii[j]);
        Inc(r);
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
end;

// Fill screen with character
procedure TfrmAntic2.FillScreen(character : byte);
var
  j : integer;
  m, r : byte;
begin
  if not isFontSetNormal then Inc(character, 128);
  FillByte(fldAtascii, SizeOf(fldAtascii), character);
  r := 0; m := 0;
  for j := 0 to maxSize do begin
    if (j > (maxX - 1)) and (j mod maxX = 0) then begin
      r := 0;
      Inc(m, 2);
    end;
    PutChar(r, m, fldAtascii[j]);
    Inc(r);
  end;
end;

// Clear screen
procedure TfrmAntic2.ClearScreenProc(Sender: TObject);
begin
  FillScreen(0);
end;

// Options
//procedure TfrmAntic2.OptionsClick(Sender: TObject);
//begin
  //frmAtasciiSettings := TfrmAtasciiSettings.Create(Self);
  //with frmAtasciiSettings do begin
  //  try
  //    ShowModal;
  //  finally
  //    Free;
  //  end;
  //end;
//end;

procedure TfrmAntic2.ShowFontSet;
var
  n : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
begin
  imgFontSet.Canvas.Brush.Color := coltabFont[1];
  imgFontSet.Canvas.Brush.Style := bsSolid;
  imgFontSet.Canvas.FillRect(bounds(0, 0, imgFontSet.Width, imgFontSet.Height));

  imgFontSetInv.Canvas.Brush.Color := coltabFont[1];
  imgFontSetInv.Canvas.Brush.Style := bsSolid;
  imgFontSetInv.Canvas.FillRect(bounds(0, 0, imgFontSetInv.Width, imgFontSetInv.Height));

  offset := 0; yoffset := 0;

  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, 24);
    end;

    xoffset := offset*24;

    for yf := 0 to grY02 do begin
      for xf := 0 to grX02 do begin
        col := fldFontSet[xf, n shl 3 + yf];
//        col := SetColorIndex(col, true);
        imgFontSet.Canvas.Brush.Color := coltabFont[col];
        imgFontSet.Canvas.FillRect(
          bounds(xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02));
//        imgFontSet.Canvas.Pixels[xf*factX02, yf*factY02 + yoffset] := coltab[col];
//        col := SetColorIndex(col, false);
        if col = 1 then
          col := 0
        else if col = 0 then
          col := 1;

        imgFontSetInv.Canvas.Brush.Color := coltabFont[col];
        imgFontSetInv.Canvas.FillRect(bounds(xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02));
//        imgFontSetInv.Canvas.Pixels[xf*factX02, yf*factY02 + yoffset] := coltab[col];
      end;
    end;
    Inc(offset);
  end;
end;

// Draw character
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

//  showmessage(inttostr(xf));
//  if xf > grX then xf := grX;
//  if xf mod 8 = 0 then Inc(xf, 8);

  for j := 0 to maxY - 1 do begin
    for i := 0 to maxX - 1 do begin
      if (xf >= i shl 3) and (xf < (i + 1) shl 3) then begin  // 8*i = i shl 3
        xf := i shl 3;
        xx := i;
        break;
      end;
    end;
    if (yf > 8*j) and (yf <= 8*(j + 1)) then begin
      yf := j shl 3;  // *8
      yy := j;
      break;
    end;
  end;

  if xx + yy*maxX > maxSize then Exit;

  statusBar.Panels[1].Text :=
    'Editor cursor coordinates (x: ' + inttostr(xx) + ', y: ' + inttostr(yy) + ')';

  //if (xf > 0) and (xf < 7) then xf := 0;
  //if (xf > 7) and (xf < 14) then xf := 7;
  //if (xf > 14) and (xf < 21) then xf := 14;

  offset2 := offs2;
  offset := offset2 shl 3;

  if not isFontSetNormal then Inc(offset2, 128);

  fldAtascii[xx + yy*maxX] := offset2;

  for dy := 0 to 7 do begin
    for dx := 0 to 7 do begin
      col := fldFontSet[dx, dy + offset];
      if not isFontSetNormal then begin
        if col = 1 then
          col := 0
        else if col = 0 then
          col := 1;
      end;
      fldChar[dx, dy] := col;
//      fldScreen[xf + dx, yf + dy] := col;
//      col := SetColorIndex(col, isFontSetNormal);
      imgEditor.Canvas.Brush.Color := coltabFont[col];
      imgEditor.Canvas.FillRect(bounds((xf + dx)*factX, (yf + dy)*factY, factX, factY));
//      imgEditor.Canvas.Pixels[(xf + dx)*factX, (yf + dy)*factY] := coltab[col];
    end;
  end;

  //fldScreen[xf, yf] := col;
  //imgEditor.Canvas.Brush.Color := coltab[col];
  //imgEditor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
  //imgEditor.Canvas.Pixels[xf*factX, yf*factY] := coltab[col];
end;

// Draw character
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
  for dy := 0 to maxY - 1 do
    for dx := 0 to maxX - 1 do
      if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
        xf := dx shl 3;
        break;
      end;

  if offset > 128 then begin
    Dec(offset, 128);
    isInverse := true;
  end;

  offset := offset shl 3;

  for dy := 0 to 7 do
    for dx := 0 to 7 do begin
      col := fldFontSet[dx, dy + offset];
//      fldScreen[xf + dx, yf + dy] := col;

      if isInverse then begin
        if col = 1 then
          col := 0
        else if col = 0 then
          col := 1;
      end;

      imgEditor.Canvas.Brush.Color := coltabFont[col];
      imgEditor.Canvas.FillRect(bounds((xf + dx)*factX, (yf + dy)*factY, factX, factY));
//      imgEditor.Canvas.Pixels[(xf + dx)*factX, (yf + dy)*factY] := coltab[col];
    end;
end;

procedure TfrmAntic2.RefreshCharX(offset : integer);
var
  col, xf, yf : integer;
begin
  imgChar.Canvas.Brush.Color := clBlack;
  imgChar.Canvas.Brush.Style := bsSolid;
  imgChar.Canvas.FillRect(bounds(0, 0, imgChar.Width, imgChar.Height));

  offset := offset shl 3;

  for yf := 0 to 7 do
    for xf := 0 to 7 do begin
      col := fldFontSet[xf, yf + offset];
      if not isFontSetNormal then begin
        if col = 1 then
          col := 0
        else if col = 0 then
          col := 1;
      end;
      fldChar[xf, yf] := col;
      imgChar.Canvas.Brush.Color := coltabFont[col];
      imgChar.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
//      imgChar.Canvas.Pixels[xf*factX, yf*factY] := coltab[col];
    end;

  imgChar.Refresh;
end;

end.

