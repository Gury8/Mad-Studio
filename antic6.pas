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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Spin, Menus,
  common;

type
  { TfrmAntic6 }
  TfrmAntic6 = class(TForm)
    btnApplyText: TButton;
    btnClearText: TButton;
    btnFillScreen: TToolButton;
    btnNewScreen: TToolButton;
    cmbTextMode : TComboBox;
    editText: TLabeledEdit;
    editTextX: TSpinEdit;
    editTextY: TSpinEdit;
    boxTextPos: TGroupBox;
    imgChar: TImage;
    imgEditor: TImage;
    imgFontSet: TImage;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    menuAntic6: TMainMenu;
    MenuItem1: TMenuItem;
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
    miColors: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    statusBar: TStatusBar;
    ToolBar2: TToolBar;
    btnSettings: TToolButton;
    ToolButton16: TToolButton;
    btnOpen: TToolButton;
    btnSave: TToolButton;
    ToolButton25: TToolButton;
    btnClear: TToolButton;
    btnCodeGen: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure ColorPaletteProc(Sender: TObject);
    procedure FillCharProc(Sender: TObject);
    procedure miSetDefaultsClick(Sender : TObject);
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
    charStatus : byte;  // Character status in character set
    fldChar : charType;
    filter : string;
    getDir : string;
    isStart : boolean;
    procedure Plot(xf, yf : integer);
    procedure PutChar(scrPos, y, offset : integer);
    procedure SaveScreen;
    procedure FillScreen(character : byte);
    procedure RefreshCharX(offset : integer);
  public
    { public declarations }
    filename : string;
    fldFontSet : fldFontSetType;
    fldAtascii : array[0.._TEXT_MODE_1_SIZE - 1] of byte;
    grX, grY : integer;
    factX, factY,
    grX02, grY02, factX02, factY02 : byte;
    modeSize : integer;
    modeHeight : byte;
    textMode : byte;
    isTextModeChanged : boolean;
    procedure ShowFontSet;
  end;

var
  frmAntic6: TfrmAntic6;

implementation

{$R *.lfm}

uses
  main, lib, colors, antic6_gen;

{ TfrmAntic6 }

procedure TfrmAntic6.FormCreate(Sender: TObject);
begin
  DoubleBuffered := true;
  btn := mbMiddle;

  factX := 6;
  grX02 := 7; grY02 := 7;
  factX02 := 6; factY02 := 3;
  grX := 19 shl 3;
  isStart := true;

  getDir := ExtractFilePath(Application.ExeName);

//  FillByte(fldAtascii, SizeOf(fldAtascii), 0);
//  FillScreen(0);
end;

procedure TfrmAntic6.FormShow(Sender: TObject);
begin
  propFlagModules[5] := 1;
  isChange := true;
  frmMain.Top := 10;
  formId := formTextModes12;

  cmbTextModeChange(Sender);
  filename := getDir + 'examples\screen01.gr' + IntToStr(textMode);
  caption := programName + ' ' + programVersion + ' - Text mode 1 and 2 editor (' + filename + ')';
//  statusBar.Panels[0].Text := 'Cursor coordinates: x: 0, y: 0';

  imgEditor.Canvas.Brush.Color := coltab[0];
  imgEditor.Canvas.Brush.Style := bsSolid;
  imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));

//  isFontSetNormal := true;
  offs := 1;
  DefaultFontSet(fldFontSet);
  ShowFontSet;

//  FillScreen(0);
end;

procedure TfrmAntic6.FormActivate(Sender: TObject);
begin
  formId := formTextModes12;
  ShowFontSet;
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
  modeSize := 20 * modeHeight;
  grY := (modeHeight - 1) shl 3;

  if textMode = 1 then
    filter := 'Text mode 1 screen files (*.gr1)|*.gr1|All files (*.*)|*.*'
  else
    filter := 'Text mode 2 screen files (*.gr2)|*.gr2|All files (*.*)|*.*';

  RefreshCharX(0);

  //GrModeSettings;
  //SetGrMode;
  //
  //imgEditor.Canvas.Brush.Color := coltab[0];
  //imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));
  //
  //NewFileProc(Sender);
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
      if editTextY.Value = 0 then
        PutChar(editTextX.Value + i - 1, editTextY.Value, n)
      else
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

procedure TfrmAntic6.miSetDefaultsClick(Sender : TObject);
begin
  frmMain.SetDefaultPalette;
  ShowFontSet;
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
  for m := 0 to 8 do begin
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
  end;

  // Uppercase alphabet, numbers, punctuation
  if offs <= 63 then begin
    offsAbs := offs;
    charStatus := _CHARSET_UPPER;
  end
  // Lowercase alphabet
  else if (offs > 63) and (offs <= 63 + 26) then begin
    Dec(offs, 31);
    offsAbs := offs + 64;
    charStatus := _CHARSET_LOWER;
  end
  // Inverse uppercase alphabet, numbers, punctuation
  else if (offs > 63 + 26) and (offs < 63 + 26 + 64) then begin
    Dec(offs, 89);
    offsAbs := offs + 128;
    charStatus := _CHARSET_UPPER_INV;
  end
  // Inverse lowercase alphabet
  else if (offs >= 63 + 26 + 64) and (offs < 63 + 26 + 64 + 26) then begin
    Dec(offs, 89 + 31);
    offsAbs := offs + 128 + 64;
    charStatus := _CHARSET_LOWER_INV;
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

procedure TfrmAntic6.OpenProc(Sender: TObject);
var
  j, r, m : integer;
  fs : TFileStream;
begin
  frmMain.dlgOpen.Title := 'Open existing text mode ' + IntToStr(textMode) + ' screen file';
  frmMain.dlgOpen.Filter := filter;
  if frmMain.dlgOpen.Execute then begin
    filename := frmMain.dlgOpen.Filename;
    fs := TFileStream.Create(Filename, fmOpenReadWrite);
    try
      r := 0; m := 0;
      for j := 0 to modeSize - 1 do begin
        fldAtascii[j] := fs.ReadByte;
        if (j > 19) and (j mod 20 = 0) then begin
          r := 0;
          Inc(m, 2);
        end;
        PutChar(r, m, fldAtascii[j]);
        Inc(r);
      end;
      FillByte(fldChar, SizeOf(fldChar), 0);
      caption := programName + ' ' + programVersion + ' - Text mode 1 and 2 editor (' + filename + ')';
    finally
      fs.Free;
    end;
    //except
    //  on E: EFOpenError do
    //    writeln('Error reading font file occurred! Details: ', E.Message);
    //end;
  end;
end;

procedure TfrmAntic6.SaveProc(Sender: TObject);
begin
  SaveScreen;
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
        for j := 0 to modeSize - 1 do
          fs.WriteByte(fldAtascii[j]);

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
      for j := 0 to modeSize - 1 do
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
  //for j := 0 to 19 do begin
  //  PutChar(r, m, fldAtascii[j]);
  //  Inc(r);
  //end;
  //
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

procedure TfrmAntic6.ShowFontSet;
var
  n : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
begin
  imgFontSet.Canvas.Brush.Color := coltab[1];
  imgFontSet.Canvas.Brush.Style := bsSolid;
  imgFontSet.Canvas.FillRect(bounds(0, 0, imgFontSet.Width, imgFontSet.Height));
  offset := 0; yoffset := 0;

  // Uppercase alphabet, numbers, punctuation
  for n := 0 to 63 do begin
    if (n mod 20 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, 24);
    end;
    xoffset := offset * 48;
    for yf := 0 to grY02 do
      for xf := 0 to grX02 do begin
        col := fldFontSet[xf, n shl 3 + yf];
//        if col = 1 then col := 2;
        imgFontSet.Canvas.Brush.Color := coltab[col];
        imgFontSet.Canvas.FillRect(bounds(xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02));
//        imgFontSet.Canvas.Pixels[xf*factX02, yf*factY02 + yoffset] := coltab[col];
      end;

    Inc(offset);
  end;

  // Lowercase alphabet
  for n := 33 to 58 do begin
    if n = 49 then begin
      offset := 0;
      Inc(yoffset, 24);
    end;
    xoffset := offset * 48;
    for yf := 0 to grY02 do
      for xf := 0 to grX02 do begin
        col := fldFontSet[xf, n shl 3 + yf];
        if col = 1 then col := 2;
        imgFontSet.Canvas.Brush.Color := coltab[col];
        imgFontSet.Canvas.FillRect(bounds(xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02));
//        imgFontSet.Canvas.Pixels[xf*factX02, yf*factY02 + yoffset] := coltab[col];
      end;

    Inc(offset);
  end;

  // Inverse uppercase alphabet, numbers, punctuation
  for n := 1 to 63 do begin
    if (n = 11) or (n = 31) or (n = 51) then begin
      offset := 0;
      Inc(yoffset, 24);
    end;
    xoffset := offset * 48;
    for yf := 0 to grY02 do
      for xf := 0 to grX02 do begin
        col := fldFontSet[xf, n shl 3 + yf];
        if col = 1 then col := 3;
        imgFontSet.Canvas.Brush.Color := coltab[col];
        imgFontSet.Canvas.FillRect(bounds(xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02));
//        imgFontSet.Canvas.Pixels[xf*factX02, yf*factY02 + yoffset] := coltab[col];
      end;

    Inc(offset);
  end;

  // Inverse lowercase alphabet
  for n := 33 to 58 do begin
    if n = 40 then begin
      offset := 0;
      Inc(yoffset, 24);
    end;
    xoffset := offset * 48;
    for yf := 0 to grY02 do
      for xf := 0 to grX02 do begin
        col := fldFontSet[xf, n shl 3 + yf];
        if col = 1 then col := 10;
        imgFontSet.Canvas.Brush.Color := coltab[col];
        imgFontSet.Canvas.FillRect(bounds(xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02));
//        imgFontSet.Canvas.Pixels[xf*factX02, yf*factY02 + yoffset] := coltab[col];
      end;

    Inc(offset);
  end;
end;

(*
// Load default font set
procedure TfrmAntic6.LoadDefaultFont;
var
  r, i : byte;
  j : integer;
  bin : string[9];
  rs: TResourceStream;
begin
  // Create a resource stream from font resource
  rs := TResourceStream.Create(HInstance, 'CHRSET', RT_RCDATA);
  try
    // Read font data
    for j := 0 to 1023 do begin
      r := rs.ReadByte;
      bin := IntToBin(r, 8);
      for i := 0 to 7 do begin
        fldFontSet[i, j] := StrToInt(bin[i + 1]);
      end;
    end;

    ShowFontSet;
  finally
    rs.Free; // destroy the resource stream
  end;
end;
*)
(*
// Load default font
procedure TfrmAntic6.FontSet;
var
  filenamex : string;
  bin : string[9];
  j : integer;
  r, i : byte;
  fs : TFileStream;
begin
  filenamex := GetCurrentDir + '\bin\default.fnt';
  fs := TFileStream.Create(filenamex, fmOpenRead);
  try
    for j := 0 to 1023 do begin
      r := fs.ReadByte;
      bin := IntToBin(r, 8);
      for i := 0 to 7 do
        fldFontSet[i, j] := StrToInt(bin[i + 1]);
    end;
    ShowFontSet;
  finally
    fs.Free;
  //except
  //  on E: EFOpenError do
  //    writeln('Error reading font file occurred! Details: ', E.Message);
  end;
end;
*)

// Draw character
procedure TfrmAntic6.Plot(xf, yf : integer);
var
  col, i, j, xx, yy : byte;
  dx, dy, offset, offs2 : integer;
begin
  case btn of
    mbLeft :
      offs2 := offs;
    mbRight:
      begin
        offs2 := 0;
        offsAbs := 0;
      end
  else
    exit;
  end;

  if xf > grX then xf := grX;

  for j := 0 to modeHeight - 1 do begin
    for i := 0 to 19 do begin
      if (xf >= i shl 3) and (xf < (i + 1) shl 3) then begin
        xf := i shl 3;
        xx := i;
        break;
      end;
    end;
    if (yf > j shl 3) and (yf <= (j + 1) shl 3) then begin
      yf := j shl 3;  // *8
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
      imgEditor.Canvas.Brush.Color := coltab[col];
      imgEditor.Canvas.FillRect(bounds((xf + dx)*factX, (yf + dy)*factY, factX, factY));
//      imgEditor.Canvas.Pixels[(xf + dx)*factX, (yf + dy)*factY] := coltab[col];
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
      if (xf >= 8*dx) and (xf < 8*(dx + 1)) then begin
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

      imgEditor.Canvas.Brush.Color := coltab[col];
//      if col02 = _CHARSET_LOWER_INV then
//        imgEditor.Canvas.FillRect(bounds((xf + dx)*factX+6, (yf + dy)*factY, factX, factY))
//      else
        imgEditor.Canvas.FillRect(bounds((xf + dx)*factX, (yf + dy)*factY, factX, factY));
//      imgEditor.Canvas.Pixels[(xf + dx)*factX, (yf + dy)*factY] := coltab[col];
    end;
end;

procedure TfrmAntic6.RefreshCharX(offset : integer);
var
  n : byte;
  col, xf, yf, offsetx : integer;
begin
//  showmessage(inttostr(offset));

  with imgChar.Canvas do begin
    Brush.Color := clBlack;
    Brush.Style := bsSolid;
    FillRect(bounds(0, 0, imgChar.Width, imgChar.Height));
  end;
  offsetx := 0;

  // Uppercase alphabet, numbers, punctuation
  if offset <= 63 then begin
    for n := 0 to 63 do begin
      if offset = offsetx then begin
        for yf := 0 to grY02 do begin
          for xf := 0 to grX02 do begin
            col := fldFontSet[xf, n shl 3 + yf];
            imgChar.Canvas.Brush.Color := coltab[col];
            imgChar.Canvas.FillRect(bounds(xf*factX02, yf*factY, factX02, factY));
          end;
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
        for yf := 0 to grY02 do begin
          for xf := 0 to grX02 do begin
            col := fldFontSet[xf, n shl 3 + yf];
            if col = 1 then col := 2;
            imgChar.Canvas.Brush.Color := coltab[col];
            imgChar.Canvas.FillRect(bounds(xf*factX02, yf*factY, factX02, factY));
          end;
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
        for yf := 0 to grY02 do begin
          for xf := 0 to grX02 do begin
            col := fldFontSet[xf, n shl 3 + yf];
            if col = 1 then col := 3;
            imgChar.Canvas.Brush.Color := coltab[col];
            imgChar.Canvas.FillRect(bounds(xf*factX02, yf*factY, factX02, factY));
          end;
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
        for yf := 0 to grY02 do begin
          for xf := 0 to grX02 do begin
            col := fldFontSet[xf, n shl 3 + yf];
            if col = 1 then col := 10;
            imgChar.Canvas.Brush.Color := coltab[col];
            imgChar.Canvas.FillRect(bounds(xf*factX02, yf*factY, factX02, factY));
          end;
        end;
        break;
      end;
      Inc(offsetx);
    end;
  end;
  (*
  imgChar.Canvas.Brush.Color := clBlack;
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

procedure TfrmAntic6.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[5] := 0;
end;

end.

