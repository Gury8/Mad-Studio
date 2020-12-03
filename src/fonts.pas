{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Character set (font) editor
}
unit fonts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ExtCtrls, ComCtrls, Buttons, strutils, lcltype,
  common;

type
  { TfrmFonts }
  TfrmFonts = class(TForm)
    btnByteEditor : TToolButton;
    btnCharDown : TSpeedButton;
    btnCharLeft : TSpeedButton;
    btnCharRight : TSpeedButton;
    btnCharUp : TSpeedButton;
    btnInvert : TToolButton;
    btnViewer : TToolButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3 : TGroupBox;
    grpCharOper : TGroupBox;
    imgAntic2CharNewInv: TImage;
    imgAntic4Char: TImage;
    imgAntic2CharInv: TImage;
    imgAntic2CharNew: TImage;
    imgAntic4CharNew: TImage;
    imgAntic5CharNew: TImage;
    imgAntic5FontSet: TImage;
    imgAntic7Char: TImage;
    imgAntic5Char: TImage;
    imgAntic7CharNew: TImage;
    imgAntic7FontSet: TImage;
    imgAntic2Char: TImage;
    imgAntic6Char: TImage;
    imgAntic6CharNew: TImage;
    imgCharOrig : TImage;
    imgFontSet: TImage;
    imgChar: TImage;
    imgAntic4FontSet: TImage;
    imgAntic6FontSet: TImage;
    imgFontSetInv: TImage;
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
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblNum1: TLabel;
    lblNum2: TLabel;
    lblNum3: TLabel;
    lblNum0: TLabel;
    lblNum4: TLabel;
    lblNum5: TLabel;
    lblNum6: TLabel;
    lblNum7: TLabel;
    memoInfo : TMemo;
    menuForms: TMainMenu;
    MenuItem1: TMenuItem;
    menuExit: TMenuItem;
    itemFlipX: TMenuItem;
    itemFlipY: TMenuItem;
    menuCopyChar: TMenuItem;
    itemInvert : TMenuItem;
    itemRotate: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem19: TMenuItem;
    menuCode: TMenuItem;
    menuClear: TMenuItem;
    menuDefaultSet: TMenuItem;
    itemFontView : TMenuItem;
    itemByteEditor : TMenuItem;
    menuView : TMenuItem;
    MenuItem3: TMenuItem;
    menuOpen: TMenuItem;
    menuSave: TMenuItem;
    menuSaveAs: TMenuItem;
    radMoveChar : TRadioButton;
    radShiftChar : TRadioButton;
    sbFont: TStatusBar;
    toolbar: TToolBar;
    btnCode: TToolButton;
    btnCopyChar: TToolButton;
    ToolButton16: TToolButton;
    btnLoad: TToolButton;
    btnSave: TToolButton;
    ToolButton2: TToolButton;
    btnClear: TToolButton;
    btnFlipX: TToolButton;
    btnFlipY: TToolButton;
    btnRotate: TToolButton;
    ToolButton3: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure InvertProc(Sender : TObject);
    procedure ByteEditorProc(Sender : TObject);
    procedure ViewerProc(Sender : TObject);
    procedure CharOper(Sender : TObject);
    procedure imgAntic5FontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgAntic6FontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgCharMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgCharMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgCharMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgFontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure CloseProc(Sender: TObject);
    procedure CopyCharProc(Sender: TObject);
    procedure lblNum0Click(Sender: TObject);
    procedure SetDefaultProc(Sender: TObject);
    procedure CodeGenProc(Sender: TObject);
    procedure LoadFontProc(Sender: TObject);
    procedure ClearCharProc(Sender: TObject);
    procedure MoveLeftProc(Sender: TObject);
    procedure MoveRightProc(Sender: TObject);
    procedure MoveUpProc(Sender: TObject);
    procedure MoveDownProc(Sender: TObject);
    procedure ShiftLeftProc(Sender: TObject);
    procedure ShiftRightProc(Sender: TObject);
    procedure ShiftUpProc(Sender: TObject);
    procedure ShiftDownProc(Sender: TObject);
    procedure FlipXProc(Sender: TObject);
    procedure FlipYProc(Sender: TObject);
    procedure RotateProc(Sender: TObject);
    procedure SaveFileProc(Sender: TObject);
    procedure SaveFileAsProc(Sender: TObject);
  private
    { private declarations }
    btn : tMousebutton;
    varBtn : tMousebutton;
    offsX, offsY : byte;
    grX, grY, factX, factY : byte;
    grX02, grY02, factX02, factY02 : byte;
    factX06, factY06 : byte;
    factY07 : byte;
    factOrigX, factOrigY : byte;
    fact6OrigX, fact6OrigY, fact7OrigY : byte;
    isCopyChar : boolean;
  //  isFileLoaded : boolean;
//    isSaveAs : boolean;
    procedure ShowFontSet;
    procedure ShowAntic4FontSet(image : TImage; factor, factorY : byte);
    procedure ShowAntic6FontSet(image : TImage; factor, factorY : byte);
    procedure ShowFontSet02(offset : byte);
    procedure ShowAntic4FontSet02(offset : byte; image : TImage; factYpar, offsetFac : byte);
    procedure ShowAntic6FontSet02(offset, heightFactor : byte);
    procedure Plot(xf, yf : byte);
    procedure RefreshChar;
    procedure RefreshCharX(offset : integer);
    procedure ShowAntic2Char(offset : word; image : TImage; fontType : fldFontSetType;
      isInverse : boolean);
    procedure ShowAntic4Char(offset : word; image : TImage; fontType : fldFontSetType;
      factXOrigY : byte);
    procedure ShowAntic6Char(offset : word; image : TImage; fontType : fldFontSetType;
      factXOrigY : byte);
    procedure OpenFile(filename : string);
    procedure SaveFontProc;
  public
    { public declarations }
    filename : string;
    fld, fldOrig : fldFontSetType;
    offs : byte;
    fldChar : charType;
    charEditIndex : array[0..127] of byte;
  end;

var
  frmFonts : TfrmFonts;

implementation

{$R *.lfm}

uses
  main, lib, font_gen, byte_editor, viewer;

{ TfrmFonts }

procedure TfrmFonts.FormCreate(Sender: TObject);
begin
  DoubleBuffered := true;
  btn := mbMiddle;
  isCopyChar := false;
  FillByte(charEditIndex, SizeOf(charEditIndex), 0);

  grX := 7; grY := 7;
  factX := 24; factY := 24;

  // Text mode 0 variables
  grX02 := 7; grY02 := 7;
  factX02 := 2; factY02 := 2;
  factOrigX := 3; factOrigY := 3;

  // Text mode 1 variables
  factX06 := 4; factY06 := 2;
  fact6OrigX := 6; fact6OrigY := 3;

  // Text mode 2 variables
  factY07 := 4;
  fact7OrigY := 6;

  DefaultFontSet(fld);
  DefaultFontSet(fldOrig);

  filename := getDir + 'examples\chrset01.fnt';
//  isFileLoaded := false;
end;

procedure TfrmFonts.FormShow(Sender: TObject);
var
  i : byte;
begin
  propFlagModules[3] := 1;
  isChange := true;
  formId := formFont;

  frmMain.Top := 0;
  Caption := programName + ' ' + programVersion + ' - Character set editor (' + filename + ')';

  sbFont.Panels[0].Text := 'Cursor coordinates: x: 0, y: 0';

  FillRectEx(imgChar, colTab[0], 0, 0, imgChar.Width, imgChar.Height);
  FillRectEx(imgFontSet, colTab[0], 0, 0, imgFontSet.Width, imgFontSet.Height);
  FillRectEx(imgFontSetInv, colTab[0], 0, 0, imgFontSetInv.Width, imgFontSetInv.Height);

  for i := 0 to 7 do
    (FindComponent('lblNum' + IntToStr(i)) as TLabel).Caption := '0';

  ShowFontSet;
end;

procedure TfrmFonts.FormActivate(Sender: TObject);
begin
  formId := formFont;
end;

procedure TfrmFonts.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[3] := 0;
  formId := formMain;
end;

procedure TfrmFonts.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: begin
      Cursor := crDefault;
      isCopyChar := false;
      sbFont.Panels[1].Text := '';
    end;
    VK_F12: frmMain.Show;
  end;
end;

procedure TfrmFonts.CharOper(Sender : TObject);
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
end;

procedure TfrmFonts.ByteEditorProc(Sender : TObject);
var
  x, y : byte;
  bin : string;
  dataValue : byte;
begin
  beType := formFont;
  maxLines := 8;
  frmByteEditor.ShowModal;

  if isDataExport then begin
    for y := 0 to 7 do begin
      dataValue := exportData[y];
      bin := IntToBin(dataValue, 8);
      for x := 0 to 7 do
        fldChar[x, y] := StrToInt(bin[x + 1]);
    end;
    Plot(255, 255);
    charEditIndex[offs] := 1;
    RefreshChar;
  end;
end;

procedure TfrmFonts.InvertProc(Sender : TObject);
var
  x, y : byte;
begin
  for y := 0 to grY do
    for x := 0 to grX do
      fldChar[x, y] := 1 - fldChar[x, y];

  RefreshChar;
end;

procedure TfrmFonts.ViewerProc(Sender : TObject);
begin
  frmViewer.isModal := true;
  frmViewer.ShowModal;
  if frmViewer.filename <> '' then
    OpenFile(frmViewer.filename);
end;

procedure TfrmFonts.imgCharMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : byte;
begin
  btn := Button;
  xf := X div factX;
  yf := Y div factY;

  // Check for mouse button clicked
  if btn = mbRight then
    varBtn := btn
  else
    varBtn := mbLeft;

  //if not isFontSetNormal then begin
  //  setNormalMode := true;
  //end;

  Plot(xf, yf);
  charEditIndex[offs] := 1;
end;

procedure TfrmFonts.imgCharMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : byte;
begin
  xf := X div factX;
  yf := Y div factY;
  Plot(xf, yf);
  sbFont.Panels[0].Text := 'Cursor coordinates: ' + 'x: ' + inttostr(xf) + ', y: ' + inttostr(yf);
end;

procedure TfrmFonts.imgCharMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
  RefreshChar;
end;

procedure TfrmFonts.imgFontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  n, m : byte;
  xf, yf : byte;
  offset : word;
begin
//  isFontSetNormal := TImage(Sender).Tag = 0;
  for m := 0 to 7 do begin
    if (x > 16) and (x <= 32) and (y > m shl 4) and (y < 20 + m shl 4) then begin
      offsY := m; offsX := 1;
      offs := m shl 4 + 1;
      if not isCopyChar then
        RefreshCharX(offs)
      else begin
        offset := offs shl 3;
        for yf := 0 to grY do
          for xf := 0 to grX do
            fld[xf, yf + offset] := fldChar[xf, yf];

//        ShowFontSet02(offs);
      end;
      break;
    end;
    for n := 0 to 14 do begin
      if (x > n shl 4) and (x <= 32 + n shl 4) and (y > m shl 4) and (y < 20 + m shl 4) then begin
        offsY := m; offsX := n;
        offs := n + m shl 4 + 1;
        if n = 0 then Dec(offs);
        if not isCopyChar then
          RefreshCharX(offs)
        else begin
          offset := offs shl 3;
          for yf := 0 to grY do
            for xf := 0 to grX do
              fld[xf, yf + offset] := fldChar[xf, yf];

//          ShowFontSet02(offs);
        end;
        break;
      end;
    end;
  end;

  // Show character data values
  Plot(255, 255);
  ShowAntic2Char(offs, imgAntic2Char, fldOrig, false);
  ShowAntic2Char(offs, imgAntic2CharInv, fldOrig, true);
  ShowAntic2Char(offs, imgCharOrig, fldOrig, false);
  ShowAntic4Char(offs, imgAntic4Char, fldOrig, fact6OrigY);
  ShowAntic4Char(offs, imgAntic5Char, fldOrig, fact7OrigY);
  ShowAntic6Char(offs, imgAntic6Char, fldOrig, fact6OrigY);
  ShowAntic6Char(offs, imgAntic7Char, fldOrig, fact7OrigY);
end;

procedure TfrmFonts.imgAntic5FontSetMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  n, m : byte;
  xf, yf : byte;
  offset : word;
begin
  for m := 0 to 7 do begin
    if (x > 16) and (x <= 32) and (y > 32*m) and (y < 32*m + 32) then begin
      offsY := m; offsX := 1;
      offs := m shl 4 + 1;  // 16*m
      if not isCopyChar then
        RefreshCharX(offs)
      else begin
        offset := offs shl 3;
        for yf := 0 to grY do begin
          for xf := 0 to grX do
            fld[xf, yf + offset] := fldChar[xf, yf];
        end;
      end;
      break;
    end;
    for n := 0 to 15 do begin
      if (x > n shl 4) and (x <= 32 + n shl 4) and (y > m shl 5) and (y < m shl 5 + 32) then begin
        offsY := m; offsX := n;
        offs := n + m shl 4 + 1;
        if n = 0 then Dec(offs);

        if not isCopyChar then
          RefreshCharX(offs)
        else begin
          offset := offs shl 3;
          for yf := 0 to grY do
            for xf := 0 to grX do
              fld[xf, yf + offset] := fldChar[xf, yf];
        end;
        break;
      end;
    end;
  end;

  // Show character data values
  Plot(255, 255);
  ShowAntic2Char(offs, imgAntic2Char, fldOrig, false);
  ShowAntic2Char(offs, imgAntic2CharInv, fldOrig, true);
  ShowAntic2Char(offs, imgCharOrig, fldOrig, false);
  ShowAntic4Char(offs, imgAntic4Char, fldOrig, fact6OrigY);
  ShowAntic4Char(offs, imgAntic5Char, fldOrig, fact7OrigY);
  ShowAntic6Char(offs, imgAntic6Char, fldOrig, fact6OrigY);
  ShowAntic6Char(offs, imgAntic7Char, fldOrig, fact7OrigY);
end;

procedure TfrmFonts.imgAntic6FontSetMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  n, m : byte;
  xf, yf : byte;
  offset : word;
  factor : byte;
begin
  factor := TImage(Sender).Tag;
  for m := 0 to 3 do begin
    if (x > 32) and (x <= 64) and
       (y > (16 + factor shl 4)*m) and
       (y < (16 + factor shl 4)*(m + 1)) then
    begin
      offsY := m; offsX := 1;
      offs := m shl 4 + 1;  // 16*m
      if not isCopyChar then
        RefreshCharX(offs)
      else begin
        offset := offs shl 3;
        for yf := 0 to grY do
          for xf := 0 to grX do
            fld[xf, yf + offset] := fldChar[xf, yf];
      end;
      break;
    end;
    for n := 0 to 14 do begin
      if (x > 32*n) and (x <= 64 + 32*n) and
         (y > (16 + factor shl 4)*m) and
         (y < (16 + factor shl 4)*(m + 1)) then
      begin
        offsY := m; offsX := n;
        offs := n + m shl 4 + 1;  // 16*m
        if n = 0 then Dec(offs);

        if not isCopyChar then
          RefreshCharX(offs)
        else begin
          offset := offs shl 3;
          for yf := 0 to grY do
            for xf := 0 to grX do
              fld[xf, yf + offset] := fldChar[xf, yf];
        end;
        break;
      end;
    end;
  end;

//  sbFont.Panels[1].Text := 'offsY = ' + inttostr(offsY) + ' offs = ' + inttostr(offs);

  // Show character data values
  Plot(255, 255);
  ShowAntic2Char(offs, imgAntic2Char, fldOrig, false);
  ShowAntic2Char(offs, imgAntic2CharInv, fldOrig, true);
  ShowAntic2Char(offs, imgCharOrig, fldOrig, false);
  ShowAntic4Char(offs, imgAntic4Char, fldOrig, fact6OrigY);
  ShowAntic4Char(offs, imgAntic5Char, fldOrig, fact7OrigY);
  ShowAntic6Char(offs, imgAntic6Char, fldOrig, fact6OrigY);
  ShowAntic6Char(offs, imgAntic7Char, fldOrig, fact7OrigY);
end;

procedure TfrmFonts.CopyCharProc(Sender: TObject);
begin
  isCopyChar := true;
  sbFont.Panels[1].Text :=
    'Copy drawing character to selected character set cell. Press ''Esc'' key to end the operation!';
  Cursor := crDrag;
end;

procedure TfrmFonts.lblNum0Click(Sender: TObject);
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

    Plot(255, 255);
    charEditIndex[offs] := 1;
    RefreshChar;
  end;
end;

procedure TfrmFonts.SetDefaultProc(Sender: TObject);
begin
  DefaultFontSet(fld);
  ShowFontSet;
//  ClearCharProc(Sender);
end;

{-----------------------------------------------------------------------------
 Source code generator
 -----------------------------------------------------------------------------}
procedure TfrmFonts.CodeGenProc(Sender: TObject);
begin
  frmFontSetGen := TfrmFontSetGen.Create(Self);
  with frmFontSetGen do
    try
      ShowModal;
    finally
      Free;
    end;
end;

{-----------------------------------------------------------------------------
 Load character set from file - actual reading data
 -----------------------------------------------------------------------------}
procedure TfrmFonts.OpenFile(filename : string);
var
  bin : string[9];
  j : integer;
  r, i : byte;
  fs : TFileStream;
begin
  fs := TFileStream.Create(Filename, fmOpenReadWrite);
  try
    for j := 0 to 1023 do
      if fs.Position < fs.Size then begin
        r := fs.ReadByte;
        bin := IntToBin(r, 8);
        for i := 0 to 7 do
          fld[i, j] := StrToInt(bin[i + 1]);
      end;

    Caption := programName + ' ' + programVersion + ' - Character set editor (' + filename + ')';
    ShowFontSet;
  finally
    fs.Free;
  end;
end;

{-----------------------------------------------------------------------------
 Load character set from file - dialog box
 -----------------------------------------------------------------------------}
procedure TfrmFonts.LoadFontProc(Sender: TObject);
begin
  frmMain.dlgOpen.Title := 'Open existing character set file';
  frmMain.dlgOpen.Filter := 'Character set files (*.fnt, *.fon, *.set)' +
                            '|*.fnt;*.fon;*.set|All files (*.*)|*.*';
  if frmMain.dlgOpen.Execute then begin
    ClearCharProc(Sender);
    filename := frmMain.dlgOpen.Filename;
//    isFileLoaded := true;
    OpenFile(filename);
  end;
end;

{-----------------------------------------------------------------------------
 Save character set to file
 -----------------------------------------------------------------------------}
procedure TfrmFonts.SaveFontProc;
var
  bin : string[9];
  j : integer;
  i : byte;
  fs : TFileStream;
begin
//    filename := GetCurrentDir + '\FONT.SET';
    fs := TFileStream.Create(filename, fmCreate);
    try
      for j := 0 to 1023 do begin
        bin := '';
        for i := 0 to 7 do
          bin += IntToStr(fld[i, j]);

        fs.WriteByte(bin2dec(bin));
       //   fs.ReadBuffer(buffer, 8);
//        bin := IntToBin(r, 8);
//        for i := 0 to 7 do begin
//          fld[i, j] := StrToInt(bin[i + 1]);
//        end;
      end;
      Caption := programName + ' ' + programVersion +
                 ' - Character set editor (' + filename + ')';
      ShowFontSet;
    finally
      fs.Free;
//      closefile(fil);
    end;
    //except
    //  on E: EFOpenError do
    //    writeln('Error writing font file occurred! Details: ', E.Message);
    //end;
end;

{-----------------------------------------------------------------------------
 Save font file
 -----------------------------------------------------------------------------}
procedure TfrmFonts.SaveFileProc(Sender: TObject);
begin
  SaveFontProc;
end;

{-----------------------------------------------------------------------------
 Save font file as
 -----------------------------------------------------------------------------}
procedure TfrmFonts.SaveFileAsProc(Sender: TObject);
begin
  frmMain.dlgSave.Title := 'Save character set as';
  frmMain.dlgSave.Filter := 'Character set files (*.fnt, *.fon, *.set)' +
                            '|*.fnt;*.fon;*.set|All files (*.*)|*.*';
  frmMain.dlgSave.Filename := filename;
  if frmMain.dlgSave.Execute then begin
    filename := frmMain.dlgSave.Filename;
    SaveFontProc;
  end;
end;

// Clear character
procedure TfrmFonts.ClearCharProc(Sender: TObject);
begin
  FillByte(fldChar, SizeOf(fldChar), 0);
  RefreshChar;
end;

// Move left
procedure TfrmFonts.ShiftLeftProc(Sender: TObject);
var
  x, y, n : byte;
begin
  for y := 0 to grY do begin
    n := fldChar[0, y];
    for x := 1 to grX do
      fldChar[x - 1, y] := fldChar[x, y];

    fldChar[7, y] := n;
  end;
  RefreshChar;
end;

// Move right
procedure TfrmFonts.ShiftRightProc(Sender: TObject);
var
  x, y, n : byte;
begin
  for y := 0 to grY do begin
    n := fldChar[7, y];
    for x := grX - 1 downto 0 do
      fldChar[x + 1, y] := fldChar[x, y];

    fldChar[0, y] := n;
  end;
  RefreshChar;
end;

// Move left
procedure TfrmFonts.MoveLeftProc(Sender: TObject);
begin
  MoveLeft(grX, grY, fldChar);
  RefreshChar;
end;

procedure TfrmFonts.MoveRightProc(Sender: TObject);
begin
  MoveRight(grX, grY, fldChar);
  RefreshChar;
end;

procedure TfrmFonts.MoveUpProc(Sender: TObject);
begin
  MoveUp(grX, grY, fldChar);
  RefreshChar;
end;

procedure TfrmFonts.MoveDownProc(Sender: TObject);
begin
  MoveDown(grX, grY, fldChar);
  RefreshChar;
end;

procedure TfrmFonts.ShiftUpProc(Sender: TObject);
var
  x, y : byte;
  fld02 : array[0..7] of byte;
begin
  for x := 0 to 7 do
    fld02[x] := fldChar[x, 0];

  for x := 0 to grX do
    for y := 1 to grY do begin
      fldChar[x, y - 1] := fldChar[x, y];
      fldChar[x, y] := 0;
    end;

  for x := 0 to 7 do
    fldChar[x, grY] := fld02[x];

  RefreshChar;
end;

procedure TfrmFonts.ShiftDownProc(Sender: TObject);
var
  x, y : byte;
  fld02 : array[0..7] of byte;
begin
  for x := 0 to 7 do
    fld02[x] := fldChar[x, grY];

  for x := 0 to grX do
    for y := grY - 1 downto 0 do begin
      fldChar[x, y + 1] := fldChar[x, y];
      fldChar[x, y] := 0;
    end;

  for x := 0 to 7 do
    fldChar[x, 0] := fld02[x];

  RefreshChar;
end;

// Flip character horizontally
procedure TfrmFonts.FlipXProc(Sender: TObject);
var
  x, y, n : byte;
begin
  for y := 0 to grY do
    for x := 7 downto 4 do begin
      n := fldChar[x, y];
      fldChar[x, y] := fldChar[7 - x, y];
      fldChar[7 - x, y] := n;
    end;

  RefreshChar;
end;

// Flip character vertically
procedure TfrmFonts.FlipYProc(Sender: TObject);
var
  x, y, n : byte;
begin
  for x := 0 to grX do
    for y := 7 downto 4 do begin
      n := fldChar[x, y];
      fldChar[x, y] := fldChar[x, 7 - y];
      fldChar[x, 7 - y] := n;
//      fld[x, 7 - y + offs*8] := n;
    end;

  RefreshChar;
end;

// Rotate character
procedure TfrmFonts.RotateProc(Sender: TObject);
var
  x, y : byte;
  arr : charType;
begin
  for y := 0 to grY do
    for x := 0 to grX do
      arr[y, x] := fldChar[x, y];

  for x := 0 to grX do
    for y := 0 to 7 do
      fldChar[7 - y, x] := arr[y, x];

  RefreshChar;
end;

procedure TfrmFonts.RefreshChar;
var
  col, xf, yf : integer;
begin
//  imgChar.Canvas.Brush.Color := colTab[0];
  imgChar.Canvas.Brush.Style := bsSolid;
//  imgChar.Canvas.FillRect(bounds(0, 0, imgChar.Width, imgChar.Height));
  FillRectEx(imgChar, colTab[0], 0, 0, imgChar.Width, imgChar.Height);

//  offset := offset shl 4;

  for yf := 0 to grY do
    for xf := 0 to grX do begin
      col := fldChar[xf, yf];
      fld[xf, yf + offs shl 3] := col;
//      imgChar.Canvas.Brush.Color := coltabFont[col];
//      imgChar.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
      FillRectEx(imgChar, coltabFont[col], xf*factX, yf*factY, factX, factY);
//      imgChar.Canvas.Pixels[xf*factX, yf*factY] := coltab[col];
    end;

  //for x := 0 to grX do begin
  //  for y := 0 to grY do begin
  //    n := fldChar[x, y];
  //    fld[x, y + offs*8] := n;
  //  end;
  //end;

  imgChar.Refresh;
  Plot(255, 255);
//  ShowFontSet;
end;

procedure TfrmFonts.RefreshCharX(offset : integer);
var
  col, xf, yf : integer;
begin
//  imgChar.Canvas.Brush.Color := colTab[0];
  imgChar.Canvas.Brush.Style := bsSolid;
//  imgChar.Canvas.FillRect(bounds(0, 0, imgChar.Width, imgChar.Height));
  FillRectEx(imgChar, colTab[0], 0, 0, imgChar.Width, imgChar.Height);

  offset := offset shl 3;

  for yf := 0 to grY do
    for xf := 0 to grX do begin
      col := fld[xf, yf + offset];
      fldChar[xf, yf] := col;
//      imgChar.Canvas.Brush.Color := coltabFont[col];
//      imgChar.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
      FillRectEx(imgChar, coltabFont[col], xf*factX, yf*factY, factX, factY);
    end;

  imgChar.Refresh;
//  sbFont.Panels[1].Text := 'offset = ' + IntToStr(offset) + ' / offs = ' + IntToStr(offs);
end;

procedure TfrmFonts.ShowFontSet;
var
  n : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
begin
//  imgFontSet.Canvas.Brush.Color := colTab[0];
  imgFontSet.Canvas.Brush.Style := bsSolid;
//  imgFontSet.Canvas.FillRect(bounds(0, 0, imgFontSet.Width, imgFontSet.Height));
  FillRectEx(imgFontSet, colTab[0], 0, 0, imgFontSet.Width, imgFontSet.Height);

//  imgFontSetInv.Canvas.Brush.Color := colTab[0];
  imgFontSetInv.Canvas.Brush.Style := bsSolid;
//  imgFontSetInv.Canvas.FillRect(bounds(0, 0, imgFontSetInv.Width, imgFontSetInv.Height));
  FillRectEx(imgFontSetInv, colTab[0], 0, 0, imgFontSetInv.Width, imgFontSetInv.Height);

  offset := 0;
  yoffset := 0;

  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, 16);
    end;
    xoffset := offset shl 4;
    for yf := 0 to grY02 do begin
      for xf := 0 to grX02 do begin
        col := fld[xf, n shl 3 + yf];
//        imgFontSet.Canvas.Brush.Color := coltabFont[col];
//        imgFontSet.Canvas.FillRect(
//          bounds(xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02));
////        imgFontSet.Canvas.Pixels[xf*factX02, yf*factY02 + yoffset] := coltab[col];
        FillRectEx(
          imgFontSet, coltabFont[col], xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);

        if col = 0 then
          col := 1
        else if col = 1 then
          col := 0;

//        imgFontSetInv.Canvas.Brush.Color := coltabFont[col];
//        imgFontSetInv.Canvas.FillRect(
//          bounds(xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02));
////        imgFontSetInv.Canvas.Pixels[xf*factX02, yf*factY02 + yoffset] := coltab[col];
        FillRectEx(imgFontSetInv,
          coltabFont[col], xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
      end;
    end;
    Inc(offset);
  end;

  ShowAntic4FontSet(imgAntic4FontSet, 16, factY02);
  ShowAntic4FontSet(imgAntic5FontSet, 32, factY07);
  ShowAntic6FontSet(imgAntic6FontSet, 16, factY06);
  ShowAntic6FontSet(imgAntic7FontSet, 32, factY07);
end;

procedure TfrmFonts.ShowFontSet02(offset : byte);
var
  n : byte;
  col, xf, yf : integer;
  xoffset, yoffset : integer;
begin
  yoffset := offsY*16;
  if offset < 16 then
    xoffset := offset*16
  else begin
    for n := 1 to 7 do
      if (offset >= n shl 4) and (offset < 32 + (n - 1) shl 4) then begin
        xoffset := (offset - n shl 4) shl 4;
        break;
      end;
  end;
  for yf := 0 to grY02 do begin
    for xf := 0 to grX02 do begin
      col := fld[xf, offset shl 3 + yf];
//      imgFontSet.Canvas.Brush.Color := coltabFont[col];
//      imgFontSet.Canvas.FillRect(
//          bounds(xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02));
      FillRectEx(imgFontSet, coltabFont[col], xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
      if col = 0 then
        col := 1
      else if col = 1 then
        col := 0;

//      imgFontSetInv.Canvas.Brush.Color := coltabFont[col];
//      imgFontSetInv.Canvas.FillRect(
//          bounds(xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02));
      FillRectEx(imgFontSetInv, coltabFont[col], xf*factX02 + xoffset, yf*factY02 + yoffset, factX02, factY02);
    end;
  end;
end;

procedure TfrmFonts.ShowAntic4FontSet(image : TImage; factor, factorY : byte);
var
  n, cnt : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
  mask : array[0..1] of byte;
begin
//  image.Canvas.Brush.Color := coltab[0];
  image.Canvas.Brush.Style := bsSolid;
//  image.Canvas.FillRect(bounds(0, 0, image.Width, image.Height));
  FillRectEx(image, coltab[0], 0, 0, image.Width, image.Height);

  //imgBaseFontSet.Canvas.Brush.Color := coltab[0];
  //imgBaseFontSet.Canvas.Brush.Style := bsSolid;
  //imgBaseFontSet.Canvas.FillRect(bounds(0, 0, imgBaseFontSet.Width, imgBaseFontSet.Height));

  offset := 0;
  yoffset := 0;

  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, factor);  // 32
    end;
    cnt := 0;
    xoffset := offset shl 4;
    for yf := 0 to grY do begin
      for xf := 0 to grX do begin
        Inc(cnt);
        col := fld[xf, n shl 3 + yf];
        mask[cnt - 1] := col;
        if cnt = 2 then begin
          if (mask[0] = 0) and (mask[1] = 1) then
            col := 1
          else if (mask[0] = 1) and (mask[1] = 0) then
            col := 2
          else if (mask[0] = 1) and (mask[1] = 1) then
            col := 3;

//          image.Canvas.Brush.Color := coltab[col];
//          image.Canvas.FillRect(bounds(
//            xf*factX02 + xoffset, yf*factorY + yoffset, factX02*2, factorY));
          FillRectEx(image, coltab[col], xf*factX02 + xoffset, yf*factorY + yoffset, factX02 shl 1, factorY);
          cnt := 0;
        end;
      end;
    end;
    Inc(offset);
  end;
end;

procedure TfrmFonts.ShowAntic6FontSet(image : TImage; factor, factorY : byte);
var
  n : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
begin
//  image.Canvas.Brush.Color := coltab[1];
  image.Canvas.Brush.Style := bsSolid;
//  image.Canvas.FillRect(bounds(0, 0, image.Width, image.Height));
  FillRectEx(image, coltab[1], 0, 0, image.Width, image.Height);
  offset := 0; yoffset := 0;

  // Uppercase alphabet, numbers, punctuation
  for n := 0 to 63 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, factor);
    end;
    xoffset := offset shl 5;  // * 32;
    for yf := 0 to grY02 do
      for xf := 0 to grX02 do begin
        col := fld[xf, n shl 3 + yf];
//        image.Canvas.Brush.Color := coltab[col];
//        image.Canvas.FillRect(bounds(xf*factX06 + xoffset, yf*factorY + yoffset, factX06, factorY));
        FillRectEx(image, coltab[col], xf*factX06 + xoffset, yf*factorY + yoffset, factX06, factorY);
      end;

    Inc(offset);
  end;
end;

{-----------------------------------------------------------------------------
 Draw character pixel in editor
 -----------------------------------------------------------------------------}
procedure TfrmFonts.Plot(xf, yf : byte);
var
  col, i, j, m, n : byte;
  bin : string[8];
begin
  if xf + yf < 510 then begin
    case btn of
      mbLeft : col := 1;
      mbRight: col := 0;
    else
      exit;
    end;
    if xf > grX then xf := grX;
    fldChar[xf, yf] := col;
    fld[xf, yf + offs shl 3] := col;
//    imgChar.Canvas.Brush.Color := coltabFont[col];
//    imgChar.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
  //  imgChar.Canvas.Pixels[xf*factX, yf*factY] := coltab[col];
    FillRectEx(imgChar, coltabFont[col], xf*factX, yf*factY, factX, factY);
  end
  else begin
    with memoInfo do begin
      Clear;

      // Calculate code number
      n := offs;
      if (offs >= 0) and (offs <= 63) then
        n += 32
      else if (offs >= 64) and (offs <= 95) then
        n -= 64;

      m := offs;
      Lines.Add('Internal code Dec: ' + IntToStr(m) + ' Hex: ' + Dec2hex(m));
      Lines.Add('Atascii code Dec: ' + IntToStr(n) + ' Hex: ' + Dec2hex(n));
//      if not isFontSetNormal then
//        Lines.Add('Inverse character value + 128');
    end;
  end;

  // Show character data values
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
  ShowAntic4FontSet02(offs, imgAntic4FontSet, factY02, 16);
  ShowAntic4FontSet02(offs, imgAntic5FontSet, factY07, 32);
  ShowAntic6FontSet02(offs, 16);
  ShowAntic6FontSet02(offs, 32);

  ShowAntic2Char(offs, imgAntic2CharNew, fld, false);
  ShowAntic2Char(offs, imgAntic2CharNewInv, fld, true);
  ShowAntic4Char(offs, imgAntic4CharNew, fld, fact6OrigY);
  ShowAntic4Char(offs, imgAntic5CharNew, fld, fact7OrigY);
  ShowAntic6Char(offs, imgAntic6CharNew, fld, fact6OrigY);
  ShowAntic6Char(offs, imgAntic7CharNew, fld, fact7OrigY);
end;

procedure TfrmFonts.ShowAntic4FontSet02(offset : byte; image : TImage; factYpar, offsetFac : byte);
var
  cnt : byte;
  col, xf, yf : byte;
  xoffset, yoffset : integer;
  mask : array[0..1] of byte;
begin
  yoffset := offsY*offsetFac;
  if offset < 16 then
    xoffset := offset*16
  else begin
    for cnt := 1 to 7 do
      if (offset >= cnt shl 4) and (offset < 32 + (cnt - 1) shl 4) then begin
        xoffset := (offset - cnt shl 4) shl 4;
        break;
      end;
  end;
  cnt := 0;
  for yf := 0 to grY do
    for xf := 0 to grX do begin
      Inc(cnt);
      col := fld[xf, offset shl 3 + yf];
      mask[cnt - 1] := col;
      if cnt = 2 then begin
        if (mask[0] = 0) and (mask[1] = 1) then
          col := 1
        else if (mask[0] = 1) and (mask[1] = 0) then
          col := 2
        else if (mask[0] = 1) and (mask[1] = 1) then
          col := 3;

        image.Canvas.Brush.Color := coltab[col];
        image.Canvas.FillRect(bounds(
          xf*factX02 + xoffset, yf*factYpar + yoffset, factX02*2, factYpar));
        cnt := 0;
      end;
    end;
end;

//procedure TfrmFonts.ShowAntic5FontSet02(offset : byte);
//var
//  cnt : byte;
//  col, xf, yf, xoffset, yoffset : integer;
//  mask : array[0..1] of byte;
//begin
//  yoffset := offsY * 32;
//  if offset < 16 then begin
//    xoffset := offset * 16;
//  end
//  else begin
//    for cnt := 1 to 7 do begin
//      if (offset >= 16*cnt) and (offset < 32 + 16*(cnt - 1)) then begin
//        xoffset := (offset - 16*cnt) * 16;
//        break;
//      end;
//    end;
//  end;
//  cnt := 0;
//  for yf := 0 to grY do begin
//    for xf := 0 to grX do begin
//      Inc(cnt);
//      col := fld[xf, offset*8 + yf];
//      mask[cnt - 1] := col;
//      if cnt = 2 then begin
//        if (mask[0] = 0) and (mask[1] = 1) then
//          col := 1
//        else if (mask[0] = 1) and (mask[1] = 0) then
//          col := 2
//        else if (mask[0] = 1) and (mask[1] = 1) then begin
//          col := 3;
//        end;
//        imgAntic5FontSet.Canvas.Brush.Color := coltab[col];
//        imgAntic5FontSet.Canvas.FillRect(
//          bounds(xf*factX02 + xoffset, yf*factY07 + yoffset, factX02*2, factY07));
//        cnt := 0;
//      end;
//    end;
//  end;
//end;

procedure TfrmFonts.ShowAntic6FontSet02(offset, heightFactor : byte);
var
  n : byte;
  col, xf, yf : byte;
  xoffset, yoffset : integer;
begin
  yoffset := offsY * heightFactor;
  if offset < 16 then
    xoffset := offset shl 5
  else begin
    for n := 1 to 3 do begin
      if (offset >= n shl 4) and (offset < 32 + (n - 1) shl 4) then begin
        xoffset := (offset - n shl 4) shl 5;
        break;
      end;
    end;
  end;
  for yf := 0 to grY02 do
    for xf := 0 to grX02 do begin
      col := fld[xf, offset shl 3 + yf];
      if heightFactor = 16 then begin
        imgAntic6FontSet.Canvas.Brush.Color := colTab[col];
        imgAntic6FontSet.Canvas.FillRect(
          bounds(xf*factX06 + xoffset, yf*factY06 + yoffset, factX06, factY06));
      end
      else begin
        imgAntic7FontSet.Canvas.Brush.Color := colTab[col];
        imgAntic7FontSet.Canvas.FillRect(
          bounds(xf*factX06 + xoffset, yf*factY07 + yoffset, factX06, factY07));
      end;
    end;
//  sbFont.Panels[1].Text := 'offset = ' + inttostr(offset) + ' xoffset = ' + inttostr(xoffset);
end;

//procedure TfrmFonts.ShowAntic6FontSet03(offset : byte);
//var
//  n : byte;
//  col, xf, yf : integer;
//  xoffset, yoffset : integer;
//begin
//  yoffset := offsY * 32;
//  if offset < 16 then begin
//    xoffset := offset * 32;
//  end
//  else begin
//    for n := 1 to 3 do begin
//      if (offset >= 16*n) and (offset < 32 + 16*(n - 1)) then begin
//        xoffset := (offset - 16*n)*32;
//        break;
//      end;
//    end;
//  end;
//  for yf := 0 to grY02 do begin
//    for xf := 0 to grX02 do begin
//      col := fld[xf, offset*8 + yf];
//      imgAntic7FontSet.Canvas.Brush.Color := colTab[col];
//      imgAntic7FontSet.Canvas.FillRect(
//        bounds(xf*factX06 + xoffset, yf*factY07 + yoffset, factX06, factY07));
//    end;
//  end;
////  sbFont.Panels[1].Text := 'offset = ' + inttostr(offset) + ' xoffset = ' + inttostr(xoffset);
//end;

procedure TfrmFonts.ShowAntic2Char(offset : word; image : TImage; fontType : fldFontSetType;
  isInverse : boolean);
var
  col, xf, yf : byte;
begin
  image.Canvas.Brush.Color := colTab[0];
  image.Canvas.Brush.Style := bsSolid;
  image.Canvas.FillRect(bounds(0, 0, image.Width, image.Height));

  offset := offset shl 3;

  for yf := 0 to 7 do
    for xf := 0 to 7 do begin
      col := fontType[xf, yf + offset];
      if isInverse then begin
        if col = 1 then
          col := 0
        else if col = 0 then
          col := 1;
      end;
      image.Canvas.Brush.Color := colTabFont[col];
      image.Canvas.FillRect(bounds(xf*factOrigX, yf*factOrigY, factOrigX, factOrigY));
    end;

  image.Refresh;
end;

procedure TfrmFonts.ShowAntic4Char(offset : word; image : TImage; fontType : fldFontSetType;
  factXOrigY : byte);
var
  cnt : byte;
  col, xf, yf : integer;
  mask : array[0..1] of byte;
begin
  image.Canvas.Brush.Color := coltab[0];
  image.Canvas.Brush.Style := bsSolid;
  image.Canvas.FillRect(bounds(0, 0, image.Width, image.Height));

//  for n := 0 to 127 do begin
//    if offset = xoffset then begin
      cnt := 0;
      for yf := 0 to grY do
        for xf := 0 to grX do begin
          Inc(cnt);
          col := fontType[xf, offset shl 3 + yf];
          mask[cnt - 1] := col;
          if cnt = 2 then begin
            if (mask[0] = 0) and (mask[1] = 1) then
              col := 1
            else if (mask[0] = 1) and (mask[1] = 0) then
              col := 2
            else if (mask[0] = 1) and (mask[1] = 1) then
              col := 3;

            image.Canvas.Brush.Color := coltab[col];
            image.Canvas.FillRect(bounds(xf*factOrigX - factOrigX, yf*factXOrigY, factOrigX*2, factXOrigY));
            cnt := 0;
          end;
        end;
//    end;
//    Inc(xoffset);
//  end;
end;

procedure TfrmFonts.ShowAntic6Char(offset : word; image : TImage; fontType : fldFontSetType;
  factXOrigY : byte);
var
  col, xf, yf : integer;
begin
  image.Canvas.Brush.Color := coltab[0];
  image.Canvas.Brush.Style := bsSolid;
  image.Canvas.FillRect(bounds(0, 0, image.Width, image.Height));

  // Uppercase alphabet, numbers, punctuation
  if offset <= 63 then begin
//    for n := 0 to 63 do begin
//      if offset = offsetx then begin
        for yf := 0 to grY02 do
          for xf := 0 to grX02 do begin
            col := fontType[xf, offset shl 3 + yf];
            image.Canvas.Brush.Color := coltab[col];
            image.Canvas.FillRect(bounds(xf*fact6OrigX, yf*factXOrigY, fact6OrigX, factXOrigY));
          end;
//        break;
//      end;
//      Inc(offsetx);
//    end;
  end;
end;

(*
procedure TfrmFonts.ShowAntic6Char(offset : integer);
var
  n : byte;
  col, xf, yf, offsetx : integer;
begin
  imgAntic6Char.Canvas.Brush.Color := colTab[0];
  imgAntic6Char.Canvas.Brush.Style := bsSolid;
  imgAntic6Char.Canvas.FillRect(bounds(0, 0, imgAntic6Char.Width, imgAntic6Char.Height));
  offsetx := 0;

  // Uppercase alphabet, numbers, punctuation
  if offset <= 63 then begin
    for n := 0 to 63 do begin
      if offset = offsetx then begin
        for yf := 0 to grY02 do begin
          for xf := 0 to grX02 do begin
            col := fldOrig[xf, n*8 + yf];
            imgAntic6Char.Canvas.Brush.Color := coltab[col];
            imgAntic6Char.Canvas.FillRect(bounds(xf*fact6OrigX, yf*fact6OrigY, fact6OrigX, fact6OrigY));
          end;
        end;
        break;
      end;
      Inc(offsetx);
    end;
  end;
end;
*)
procedure TfrmFonts.CloseProc(Sender: TObject);
begin
  Close;
end;

end.

