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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, ComCtrls, Spin, Buttons, strutils,
  common;

type
  { TfrmAntic4 }
  TfrmAntic4 = class(TForm)
    btnFlipX: TToolButton;
    btnFlipY: TToolButton;
    btnLoadFont: TBitBtn;
    btnRotate: TToolButton;
    btnSaveFont: TBitBtn;
    btnFillScreen: TToolButton;
    btnCharDown : TSpeedButton;
    btnCharLeft : TSpeedButton;
    btnCharRight : TSpeedButton;
    btnCharUp : TSpeedButton;
    cmbTextMode : TComboBox;
    editX: TSpinEdit;
    editY: TSpinEdit;
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
    Label21: TLabel;
    Label22: TLabel;
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
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    miSaveFont: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    btnSaveFontAs: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miLoadFont: TMenuItem;
    radMoveChar : TRadioButton;
    radShiftChar : TRadioButton;
    statusBar: TStatusBar;
    ToolBar2: TToolBar;
    btnClearCh: TToolButton;
    ToolButton1: TToolButton;
    ToolButton17: TToolButton;
    btnLoadScreen: TToolButton;
    btnSaveScreen: TToolButton;
    btnClearScreen: TToolButton;
    btnCode: TToolButton;
    ToolButton2: TToolButton;
    procedure CharOper(Sender : TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure GenCodeProc(Sender: TObject);
    procedure cmbTextModeChange(Sender : TObject);
    procedure editXChange(Sender: TObject);
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
    procedure CloseWinProc(Sender: TObject);
    procedure lblNum0Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
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
    procedure MoveLeftClick(Sender: TObject);
    procedure ShiftRightProc(Sender: TObject);
    procedure ShiftUpProc(Sender: TObject);
    procedure MoveDownProc(Sender: TObject);
    procedure MoveLeftProc(Sender: TObject);
    procedure MoveRightProc(Sender: TObject);
    procedure MoveUpProc(Sender: TObject);
    procedure ShiftDownProc(Sender: TObject);
    procedure ShiftLeft(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
    varBtn : tMousebutton;
    btn : tMousebutton;
    offsX, offsY : byte;
    isSaveAs, isSaveFontAs : boolean;
    isFontSetNormal : boolean;
    isEdit : boolean;
    isCreate : boolean;
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
    procedure SetTextMode(textModeX : byte);
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
    textMode : byte;
    modeSize : integer;
    modeHeight : byte;
    isTextModeChanged : boolean;
    charEditIndex : array[0..127] of byte;
    offs : byte;
  end;

var
  frmAntic4: TfrmAntic4;

implementation

{$R *.lfm}

uses
  main, lib, colors, antic4_gen;

{ TfrmAntic4 }

procedure TfrmAntic4.FormCreate(Sender: TObject);
begin
  DoubleBuffered := true;
  btn := mbMiddle;
  isCreate := true;
  FillByte(charEditIndex, SizeOf(charEditIndex), 0);
//  btnMoveCharLeft.GetBitmap(12,frmMain.images);
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
  imgEditor.Canvas.Brush.Color := coltab[0];
  imgEditor.Canvas.Brush.Style := bsSolid;
//  imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));
  //shapeColor0.Brush.Color := coltab[0];
  //shapeColor1.Brush.Color := coltab[1];
  //shapeColor2.Brush.Color := coltab[2];
  //shapeColor3.Brush.Color := coltab[3];
  //shapeColor4.Brush.Color := coltab[10];

  SetXY(39, 23);
  editX.Value := maxX - 1;
  editY.Value := maxY - 1;
  SetTextMode(4);
//  cmbTextModeChange(Sender);
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

  imgChar.Canvas.Brush.Color := clBlack;
  imgChar.Canvas.FillRect(bounds(0, 0, imgChar.Width, imgChar.Height));

  for i := 0 to 7 do
    (FindComponent('lblNum' + IntToStr(i)) as TLabel).Caption := '0';

  FillScreen(0, false);
  isCreate := false;
end;

procedure TfrmAntic4.FormActivate(Sender: TObject);
begin
  ShowFontSet;
  formId := formAtascii;
//  FormStyle := fsSystemStayOnTop;
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

procedure TfrmAntic4.cmbTextModeChange(Sender : TObject);
begin
  if MessageDlg('Warning', 'You are about to change text mode.' +
                ' Data will be lost! Do you wish to continue?',
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
  begin
    case textMode of
      4  : cmbTextMode.ItemIndex := 0;
      5  : cmbTextMode.ItemIndex := 1;
    end;
    Exit;
  end;

  case cmbTextMode.ItemIndex of
    0: SetTexTMode(4);
    1: SetTexTMode(5);
  end;

  filename := 'examples\screen01.an' + IntToStr(textMode);
  caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
  FillScreen(0, false);
end;

procedure TfrmAntic4.SetTextMode(textModeX : byte);
begin
  textMode := textModeX;
  grX := 39*8;

  if textMode = 4 then begin
    modeHeight := 24;
    factY := 3;
    factY02 := 3;
  end
  else begin
    modeHeight := 12;
    factY := 6;
    factY02 := 6;
  end;

  modeSize := 40*modeHeight;
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

//  imgCharOrig.Canvas.Brush.Color := clBlack;
//  imgCharOrig.Canvas.Brush.Style := bsSolid;
//  imgCharOrig.Canvas.FillRect(bounds(0, 0, imgCharOrig.Width, imgCharOrig.Height));
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

procedure TfrmAntic4.CloseWinProc(Sender: TObject);
begin
  Close;
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

procedure TfrmAntic4.MenuItem13Click(Sender: TObject);
begin
//  ClearClick(Sender);
  filename := 'examples\screen01.an4';
  caption := programName + ' ' + programVersion +
             ' - Antic mode 4 and 5 editor (' + filename + ')';
end;

// Clear screen
procedure TfrmAntic4.ClearEditorProc(Sender: TObject);
begin
  FillScreen(0, false);
end;

procedure TfrmAntic4.ColorsProc(Sender: TObject);
begin
  frmColors.Show;
end;

procedure TfrmAntic4.DefaultSetProc(Sender: TObject);
begin
  DefaultFontSet(fldFontSet);
  fontName := '';
  ShowFontSet;
//  ClearCharClick(Sender);
end;

procedure TfrmAntic4.SaveFontAsProc(Sender: TObject);
begin
  isSaveFontAs := true;
  SaveFont;
end;

procedure TfrmAntic4.LoadScreenProc(Sender: TObject);
var
  j, r, m : integer;
  fs : TFileStream;
begin
  frmMain.dlgOpen.Title := 'Open existing Antic mode 4 and 5 screen file';
  frmMain.dlgOpen.Filter :=
    'Antic mode 4/5 screen files (*.an4, *.an5, *.gr0)|*.an4;*.an5;*.gr0|All files (*.*)|*.*';
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
      caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
    finally
      fs.Free;
    end;
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

//procedure TfrmAntic4.AnticMode4Proc(Sender: TObject);
//begin
//  SetTexTMode(4);
//  filename := 'examples\screen01.an' + IntToStr(textMode);
//  caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
//  FillScreen(0, false);
//end;
//
//procedure TfrmAntic4.AnticMode5Proc(Sender: TObject);
//begin
//  SetTexTMode(5);
//  filename := 'examples\screen01.an' + IntToStr(textMode);
//  caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
//  FillScreen(0, false);
//end;

procedure TfrmAntic4.SaveScreenAsProc(Sender: TObject);
begin
  isSaveAs := true;
  SaveScreen;
end;

procedure TfrmAntic4.SaveScreenProc(Sender: TObject);
begin
  SaveScreen;
end;

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
      for j := 0 to 1023 do begin
        r := fs.ReadByte;
        bin := IntToBin(r, 8);
        for i := 0 to 7 do
          fldFontSet[i, j] := StrToInt(bin[i + 1]);
      end;
//      Caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
//      ClearCharClick(Sender);
//      FillByte(fldChar, SizeOf(fldChar), 0);
      ShowFontSet;
    finally
      fs.Free;
    end;
  end;
end;

procedure TfrmAntic4.SaveFontProc(Sender: TObject);
begin
  SaveFont;
end;

procedure TfrmAntic4.SaveScreen;
var
  j : integer;
  fs : TFileStream;
  isOk : boolean = false;
begin
  try
  // Save as
  if isSaveAs then begin
    frmMain.dlgSave.Title := 'Save Antic mode 4/5 screen as';
    frmMain.dlgSave.Filter := 'Antic mode 4/5 screen files (*.an4, *.an5, *.gr0)' +
                              '|*.an4;*.an5;*.gr0|All files (*.*)|*.*';
    frmMain.dlgSave.Filename := filename;
    if frmMain.dlgSave.Execute then begin
      filename := frmMain.dlgSave.Filename;
      fs := TFileStream.Create(filename, fmCreate);
      try
        for j := 0 to maxSize do
          fs.WriteByte(fldAtascii[j]);

        isOk := true;
//        caption := programName + ' ' + programVersion + ' - Antic mode 4 and 5 editor (' + filename + ')';
      finally
        fs.Free;
      end;
    end;
  end
  // Save
  else begin
    fs := TFileStream.Create(filename, fmCreate);
    try
      for j := 0 to maxSize do
        fs.WriteByte(fldAtascii[j]);

      isOk := true;
    finally
      fs.Free;
    end;
  end;
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

procedure TfrmAntic4.editXChange(Sender: TObject);
begin
  if not isCreate then
    SetXY(editX.Value, editY.Value);
end;

procedure TfrmAntic4.FormDeactivate(Sender: TObject);
begin
  FormStyle := fsNormal;
end;

procedure TfrmAntic4.SetXY(_maxX, _maxY : byte);
begin
  maxX := _maxX + 1;
  maxY := _maxY + 1;
  maxSize := maxX*maxY - 1;
  grX := maxX shl 3;
  grY := maxY shl 3;
  imgEditor.Width := maxX * 24;
  imgEditor.Height := maxY * 24;
  statusBar.Panels[0].Text := 'Max X coord.: ' + IntToStr(_maxX) +
                              ', max Y coord.: ' + IntToStr(_maxY);
end;

// Fill screen with character
procedure TfrmAntic4.FillScreen(character : byte; isBackground : boolean);
var
  j : integer;
  m, r : byte;
begin
  if not isBackground then
    FillByte(fldAtascii, SizeOf(fldAtascii), character);

  r := 0; m := 0;
  for j := 0 to maxSize do begin
    if (j > (maxX - 1)) and (j mod maxX = 0) then begin
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
  //for j := 0 to maxX - 1 do begin
  //  PutChar(r, m, fldAtascii[j]);
  //  Inc(r);
  //end;
  //
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
  imgFontSet.Canvas.Brush.Color := coltab[0];
  imgFontSet.Canvas.Brush.Style := bsSolid;
  imgFontSet.Canvas.FillRect(bounds(0, 0, imgFontSet.Width, imgFontSet.Height));

  //imgBaseFontSet.Canvas.Brush.Color := coltab[0];
  //imgBaseFontSet.Canvas.Brush.Style := bsSolid;
  //imgBaseFontSet.Canvas.FillRect(bounds(0, 0, imgBaseFontSet.Width, imgBaseFontSet.Height));

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

          imgFontSet.Canvas.Brush.Color := coltab[col];
          imgFontSet.Canvas.FillRect(bounds(
            (xf - 1)*factX02 + xoffset, yf*3 + yoffset, factX02 shl 1, 3));
          cnt := 0;
        end;
      end;

    Inc(offset);
  end;
end;

// Draw character
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

  for j := 0 to maxY - 1 do begin
    for i := 0 to maxX - 1 do begin
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

  if xx + yy*maxX > maxSize then Exit;

  statusBar.Panels[1].Text := 'Cursor coordinates (x: ' +
                              inttostr(xx) + ', y: ' + inttostr(yy) + ')';
  cnt := 0;
  offset2 := offs2;
  offset := offset2 shl 3;
  fldAtascii[xx + yy*maxX] := offset2;
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

        imgEditor.Canvas.Brush.Color := coltab[col];
        imgEditor.Canvas.FillRect(bounds((xf + j - 1)*factX, (yf + i)*factY, factX shl 1, factY));
        cnt := 0;
      end;
//      fldScreen[xf + dx, yf + i] := col;
    end;
  end;

  // Refresh original set
  ShowFontSet02(offset2);
end;

// Draw character
procedure TfrmAntic4.PutChar(scrPos, y, offset : integer);
var
  cnt, col : byte;
  dx, dy, xf, yf : integer;
  mask : array[0..1] of byte;
begin
  xf := scrPos shl 3;
  yf := y shl 2;
//  showmessage(inttostr(scrPos) + ' ' + inttostr(y) + ' ' + inttostr(offset));
  for dy := 0 to maxY - 1 do
    for dx := 0 to maxX - 1 do
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

        imgEditor.Canvas.Brush.Color := coltab[col];
        imgEditor.Canvas.FillRect(bounds(
          (xf + dx - 1)*factX, (yf + dy)*factY, factX shl 1, factY));
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
  imgChar.Canvas.Brush.Color := clBlack;
  imgChar.Canvas.Brush.Style := bsSolid;
  imgChar.Canvas.FillRect(bounds(0, 0, imgChar.Width, imgChar.Height));

  for yf := 0 to chrY do
    for xf := 0 to chrX do begin
//      col := fldChar[xf, yf];
      fldFontSet[xf, yf + offs shl 3] := fldChar[xf, yf];
      imgChar.Canvas.Brush.Color := coltabFont[fldChar[xf, yf]];
      imgChar.Canvas.FillRect(bounds(xf*chrFactX, yf*chrFactY, chrFactX, chrFactY));
    end;

  imgChar.Refresh;
  PlotChar(255, 255);

  r := 0; m := 0;
  for j := 0 to maxSize do begin
    if (j > (maxX - 1)) and (j mod maxX = 0) then begin
      r := 0;
      Inc(m, 2);
    end;
    if fldAtascii[j] = offs then
      PutChar(r, m, fldAtascii[j]);

    Inc(r);
  end;
end;

// Character editor
procedure TfrmAntic4.RefreshCharX(offset : integer);
var
  col, xf, yf : integer;
begin
  imgChar.Canvas.Brush.Color := clBlack;
  imgChar.Canvas.Brush.Style := bsSolid;
  imgChar.Canvas.FillRect(bounds(0, 0, imgChar.Width, imgChar.Height));
  offset := offset shl 3;
  for yf := 0 to chrY do
    for xf := 0 to chrX do begin
      col := fldFontSet[xf, yf + offset];
      fldChar[xf, yf] := col;
      imgChar.Canvas.Brush.Color := coltabFont[col];
      imgChar.Canvas.FillRect(bounds(xf*chrFactX, yf*chrFactY, chrFactX, chrFactX));

      imgCharOrig.Canvas.Brush.Color := coltabFont[col];
      imgCharOrig.Canvas.FillRect(bounds(xf*3, yf*3, 3, 3));
//      imgChar.Canvas.Pixels[xf*chrFactX, yf*chrFactY] := coltabFont[col];
    end;

  imgChar.Refresh;
  imgCharOrig.Refresh;
end;

// Draw character pixel in editor
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

    imgChar.Canvas.Brush.Color := coltabFont[col];
    imgChar.Canvas.FillRect(bounds(xf*chrFactX, yf*chrFactY, chrFactX, chrFactY));
  //  imgChar.Canvas.Pixels[xf*factX, yf*factY] := coltabFont[col];
  end
  else begin
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
  end;

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

  //r := 0; m := 0;
  //for j := 0 to maxSize do begin
  //  if (j > (maxX - 1)) and (j mod maxX = 0) then begin
  //    r := 0;
  //    Inc(m, 2);
  //  end;
  //  PutChar(r, m, fldAtascii[j]);
  //  Inc(r);
  //end;
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

        imgFontSet.Canvas.Brush.Color := coltab[col];
        imgFontSet.Canvas.FillRect(
          bounds((xf - 1)*factX02 + xoffset, yf*fy + yoffset, factX02 shl 1, fy));
        cnt := 0;
      end;
    end;
end;

procedure TfrmAntic4.ShowBaseFontSet;
var
  n : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
begin
  imgBaseFontSet.Canvas.Brush.Color := coltabFont[1];
  imgBaseFontSet.Canvas.Brush.Style := bsSolid;
  imgBaseFontSet.Canvas.FillRect(bounds(0, 0, imgBaseFontSet.Width, imgBaseFontSet.Height));
  offset := 0; yoffset := 0;
  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, 24);
    end;
    xoffset := offset * 24;
    for yf := 0 to chrY do
      for xf := 0 to chrX do begin
        col := fldFontSet[xf, n shl 3 + yf];
        imgBaseFontSet.Canvas.Brush.Color := coltabFont[col];
        imgBaseFontSet.Canvas.FillRect(bounds(xf*factX02 + xoffset, yf*3 + yoffset, factX02, 3));
      end;

    Inc(offset);
  end;
end;

// Mirror - horizontal
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

// Mirror - vertical
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

// Rotate
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

procedure TfrmAntic4.MoveLeftClick(Sender : TObject);
begin

end;

// Shift left
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

// Shift right
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

// Shift up
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

// Shift down
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

// Move left
procedure TfrmAntic4.MoveLeftProc(Sender: TObject);
begin
  MoveLeft(chrX, chrY, fldChar);
  RefreshChar;
end;

// Move right
procedure TfrmAntic4.MoveRightProc(Sender: TObject);
begin
  MoveRight(chrX, chrY, fldChar);
  RefreshChar;
end;

// Move up
procedure TfrmAntic4.MoveUpProc(Sender: TObject);
begin
  MoveUp(chrX, chrY, fldChar);
  RefreshChar;
end;

// Move down
procedure TfrmAntic4.MoveDownProc(Sender: TObject);
begin
  MoveDown(chrX, chrY, fldChar);
  RefreshChar;
end;

// Clear character
procedure TfrmAntic4.ClearCharProc(Sender: TObject);
begin
  FillByte(fldChar, SizeOf(fldChar), 0);
  RefreshChar;
end;

procedure TfrmAntic4.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[6] := 0;
end;

end.

