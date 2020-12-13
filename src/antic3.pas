{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Antic mode 3 editor
}
unit antic3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, lcltype,
  StrUtils, ExtCtrls, ComCtrls, StdCtrls, Menus, Buttons,
  common;

type
  { TfrmAntic3 }
  TfrmAntic3 = class(TForm)
    btnCharDown : TSpeedButton;
    btnCharLeft : TSpeedButton;
    btnCharRight : TSpeedButton;
    btnCharUp : TSpeedButton;
    btnClearScreen: TToolButton;
    btnFillScreen: TToolButton;
    btnGenCode: TToolButton;
    btnLoadScreen: TToolButton;
    btnNewScreen: TToolButton;
    btnSaveScreen: TToolButton;
    boxSelChar: TGroupBox;
    grpCharOper : TGroupBox;
    imgChar: TImage;
    imgCharEdit: TImage;
    imgEditor: TImage;
    imgFontSet: TImage;
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
    lblNum8: TLabel;
    lblNum9: TLabel;
    memoInfo: TMemo;
    menuAntic3: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem19: TMenuItem;
    itemLoadFont: TMenuItem;
    MenuItem3: TMenuItem;
    itemSaveFont: TMenuItem;
    itemSetDefaultFont: TMenuItem;
    MenuItem8: TMenuItem;
    itemClearScreen: TMenuItem;
    itemClose: TMenuItem;
    itemFillScreen: TMenuItem;
    itemGenCode: TMenuItem;
    itemLoadScreen: TMenuItem;
    itemNewScreen: TMenuItem;
    itemSaveScreen: TMenuItem;
    itemSaveScreenAs: TMenuItem;
    radMoveChar : TRadioButton;
    radShiftChar : TRadioButton;
    statusBar: TStatusBar;
    ToolBar2: TToolBar;
    btnClearChar: TToolButton;
    ToolButton16: TToolButton;
    ToolButton25: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure CharOper(Sender : TObject);
    procedure MoveCharDown(Sender: TObject);
    procedure MoveCharLeft(Sender: TObject);
    procedure MoveCharRight(Sender: TObject);
    procedure MoveCharUp(Sender: TObject);
    procedure lblNum0Click(Sender: TObject);
    procedure ShiftCharDownProc(Sender: TObject);
    procedure ShiftCharLeftProc(Sender: TObject);
    procedure ShiftCharRightProc(Sender: TObject);
    procedure ShiftCharUpProc(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    procedure GenCodeProc(Sender: TObject);
    procedure ClearScreenProc(Sender: TObject);
    procedure FillScreenProc(Sender: TObject);
    procedure imgCharEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgCharEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgCharEditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgFontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure LoadScreenProc(Sender: TObject);
    procedure itemLoadFontClick(Sender: TObject);
    procedure itemSaveFontClick(Sender: TObject);
    procedure itemSetDefaultFontClick(Sender: TObject);
    procedure NewScreenProc(Sender: TObject);
    procedure SaveScreenAsProc(Sender: TObject);
    procedure SaveScreenProc(Sender: TObject);
    procedure LoadFontProc(Sender: TObject);
    procedure SaveFontProc(Sender: TObject);
    procedure ClearCharProc(Sender: TObject);
  private
    varBtn : tMousebutton;
    btn : tMousebutton;
    varBtn02 : tMousebutton;
    btn02 : tMousebutton;
    offs, offsX, offsY : byte;
    isSaveAs : boolean;
    isFontSetNormal : boolean;
    isCreate : boolean;
    procedure ShowFontSet;
    procedure Plot(xf, yf : integer);
    procedure PutChar(scrPos, y, offset : integer);
    procedure FillScreen(character : byte);
    procedure SetXY(_maxX, _maxY : byte);
    procedure RefreshCharX(offset : integer);
    procedure DrawFontSetChar(offset : byte);
    procedure PlotChar(xf, yf : byte);
    procedure RefreshChar;
    procedure ShapeClear(image : TImage; fcolor : TColor);
    procedure ShowBinValues;
    procedure RefreshData;
  public
    filename : string;
    fontFilename : string;
    fldFontSet : fldFontSetType;
    fldAtascii : array[0..960] of byte;
    fldChar : charTypeAntic3;
    grX, grY : integer;
    factX, factY,
    grX02, grY02,
    factX03, factY03 : byte;
    factX04, factY04 : byte;
    maxX, maxY : byte;
    maxSize : integer;
    charEditIndex : array[0..127] of byte;
    fld : fldAntic3FontSetType;
  end;

var
  frmAntic3: TfrmAntic3;

implementation

{$R *.lfm}

uses
  main, lib, antic3_gen;

{ TfrmAntic3 }

procedure TfrmAntic3.FormCreate(Sender: TObject);
begin
  DoubleBuffered := true;
  btn := mbMiddle;
  btn02 := mbMiddle;
  isCreate := true;
  FillByte(charEditIndex, SizeOf(charEditIndex), 0);
  FillByte(fldChar, SizeOf(fldChar), 0);

  DefaultFontSet(fldFontSet);
end;

procedure TfrmAntic3.FormShow(Sender: TObject);
var
  i : byte;
begin
  propFlagModules[10] := 1;
  isChange := true;
  frmMain.Top := 10;
  formId := formAntic3;

  filename := getDir + 'examples\screen01.an3';
  fontFilename := getDir + 'examples\an3set01.fn3';
  caption := programName + ' ' + programVersion + ' - Antic mode 3 editor (' + filename + ')';
  statusBar.Panels[0].Text := 'Editor cursor coordinates (x: 0, y: 0)';

  //imgEditor.Canvas.Brush.Color := coltabFont[0];
  //imgEditor.Canvas.Brush.Style := bsSolid;
  //imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));
  ShapeClear(imgEditor, coltabFont[0]);

  factX := 2; factY := 2;
  grX02 := 7; grY02 := 9;
  factX03 := 4; factY03 := 4;
  factX04 := 24; factY04 := 24;
  grX := 7; grY := 7;
  SetXY(39, 19);
  offs := 1;
  isFontSetNormal := true;
  ShowFontSet;

  FillScreen(0);
  isCreate := false;

  imgChar.Visible := true;
  imgFontSetMouseDown(self, mbLeft, [ssMiddle], 3, 3);

  for i := 0 to 9 do
    (FindComponent('lblNum' + IntToStr(i)) as TLabel).Caption := '0';
end;

procedure TfrmAntic3.FormActivate(Sender: TObject);
begin
  formId := formAntic3;
//  FormStyle := fsSystemStayOnTop;
end;

procedure TfrmAntic3.FormDeactivate(Sender: TObject);
begin
  FormStyle := fsNormal;
end;

procedure TfrmAntic3.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[10] := 0;
  formId := formMain;
end;

procedure TfrmAntic3.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_F12: frmMain.Show;
  end;
end;

procedure TfrmAntic3.ShowFontSet;
var
  n : byte;
  col : byte;
  row01, row02, row03, row04 : array[0..7] of byte;
  xf, yf : integer;
  offset, xoffset, yoffset : integer;
begin
  //imgFontSet.Canvas.Brush.Color := coltabFont[1];
  //imgFontSet.Canvas.Brush.Style := bsSolid;
  //imgFontSet.Canvas.FillRect(bounds(0, 0, imgFontSet.Width, imgFontSet.Height));
  ShapeClear(imgFontSet, coltabFont[1]);
  ShapeClear(imgFontSetInv, coltabFont[1]);

  offset := 0; yoffset := 0;
  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, 20);
    end;
    xoffset := offset shl 4;
    for yf := 0 to grY do begin
      for xf := 0 to grX do begin
        col := fldFontSet[xf, n shl 3 + yf];
        FillRectEx(imgFontSet, coltabFont[col],
                   xf*factX + xoffset, yf*factY + yoffset, factX, factY);
        if yf = 0 then
          row01[xf] := col
        else if yf = 1 then
          row02[xf] := col;

        fld[xf, n*10 + yf] := col;

        //if col = 1 then
        //  col := 0
        //else if col = 0 then
        //  col := 1;
        col := 1 - col;

        if yf = 0 then
          row03[xf] := col;

        if yf = 1 then
          row04[xf] := col;

        FillRectEx(imgFontSetInv, coltabFont[col],
                   xf*factX + xoffset, yf*factY + yoffset, factX, factY);

//        if isDefault then begin
        FillRectEx(imgFontSet, coltabFont[0],
                   xf*factX + xoffset, factY shl 3 + yoffset, factX, factY);

        FillRectEx(imgFontSet, coltabFont[0],
                   xf*factX + xoffset, 9*factY + yoffset, factX, factY);
          //imgFontSetInv.Canvas.Brush.Color := coltabFont[1];
          //imgFontSetInv.Canvas.FillRect(bounds(xf*factX02 + xoffset,
          //                                     8*factY02 + yoffset,
          //                                     factX02, factY02));
          //imgFontSetInv.Canvas.FillRect(bounds(xf*factX02 + xoffset,
          //                                     9*factY02 + yoffset,
          //                                     factX02, factY02));
//        end;
      end;
    end;
    if n >= 96 then
      for xf := 0 to grX02 do begin
        FillRectEx(imgFontSet, coltabFont[row01[xf]],
                   xf*factX + xoffset, factY shl 3 + yoffset, factX, factY);
        FillRectEx(imgFontSet, coltabFont[row02[xf]],
                   xf*factX + xoffset, 9*factY + yoffset, factX, factY);

        FillRectEx(imgFontSet, coltabFont[0], xf*factX + xoffset, yoffset, factX, factY);
        FillRectEx(imgFontSet, coltabFont[0], xf*factX + xoffset, factY + yoffset, factX, factY);

        FillRectEx(imgFontSetInv, coltabFont[row03[xf]], xf*factX + xoffset, factY shl 3 + yoffset, factX, factY);
        FillRectEx(imgFontSetInv, coltabFont[row04[xf]],
                   xf*factX + xoffset, 9*factY + yoffset, factX, factY);
        FillRectEx(imgFontSetInv, coltabFont[1], xf*factX + xoffset, yoffset, factX, factY);
        FillRectEx(imgFontSetInv, coltabFont[1], xf*factX + xoffset, factY + yoffset, factX, factY);

        fld[xf, n*10] := 0;
        fld[xf, n*10 + 1] := 0;
        fld[xf, n*10 + 8] := row01[xf];
        fld[xf, n*10 + 9] := row02[xf];
      end;

    Inc(offset);
  end;
end;

// Code generator
procedure TfrmAntic3.GenCodeProc(Sender: TObject);
begin
  frmAntic3Gen := TfrmAntic3Gen.Create(Self);
  with frmAntic3Gen do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmAntic3.SetXY(_maxX, _maxY : byte);
begin
  maxX := _maxX + 1;
  maxY := _maxY + 1;
  maxSize := maxX*maxY - 1;
//  grX := maxX shl 3;
//  grY := maxY shl 3;
  imgEditor.Width := maxX*16;
  imgEditor.Height := maxY*20;
//  statusBar.Panels[0].Text := 'Max X coord.: ' + IntToStr(_maxX) +
//                              ', max Y coord.: ' + IntToStr(_maxY);
end;

procedure TfrmAntic3.imgEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
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

procedure TfrmAntic3.imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
begin
  xf := X div factX;
  yf := Y div factY;

  //  if yf > 192 then showmessage(inttostr(yf));
  if (xf <= 318) and (yf <= 210) then
    Plot(xf, yf);

//  statusBar.Panels[1].Text := 'Editor cursor coordinates' +
//                              ' (x: ' + inttostr(xf) + ', y: ' + inttostr(yf) + ')';
end;

procedure TfrmAntic3.imgEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
end;

procedure TfrmAntic3.imgFontSetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  n, m : byte;
  atascii : string[3];
begin
  if TImage(Sender).Tag = 0 then
    isFontSetNormal := true
  else
    isFontSetNormal := false;

  for m := 0 to 7 do
    for n := 0 to 15 do begin
      if (x > 17) and (x <= 24) and (y > 20*m) and (y < 28 + 20*m) then begin
        offsY := m; offsX := 1;
        offs := m shl 4 + 1;
        RefreshCharX(offs);
        break;
      end;
      if (x > 17*n) and (x <= 24 + 17*n) and (y > 20*m) and (y < 28 + 20*m) then begin
        offsY := m; offsX := n;
        offs := n + m shl 4 + 1;
        if n = 0 then Dec(offs);
        RefreshCharX(offs);
        break;
      end;
    end;

  with memoInfo do begin
    Clear;

    // Calculate code number
    //if (offs >= 0) and (offs <= 63) then begin
    //  n := offs + 32;
    //end
    //else if (offs >= 64) and (offs <= 95) then begin
    //  n := offs - 64;
    //end
    //else begin
    //  n := offs;
    //end;

    atascii := AtasciiCode(offs);
    n := StrToInt(atascii);
    m := offs;
    Lines.Add('Internal code Dec: ' + IntToStr(m) + ' Hex: ' + Dec2hex(m));
    Lines.Add('Atascii code Dec: ' + atascii + ' Hex: ' + Dec2hex(n));
    if not isFontSetNormal then
      Lines.Add('Inverse character value + 128');
  end;

//  PlotChar(255, 255);
end;

procedure TfrmAntic3.CloseWinProc(Sender: TObject);
begin
  Close;
end;

procedure TfrmAntic3.lblNum0Click(Sender: TObject);
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
    DrawFontSetChar(offs);
    RefreshChar;
  end;
end;

procedure TfrmAntic3.ShiftCharUpProc(Sender: TObject);
var
  x, y : byte;
  fld02 : array[0..7] of byte;
begin
  for x := 0 to 7 do
    fld02[x] := fldChar[x, 0];

  for x := 0 to grX do
    for y := 1 to grY02 do begin
      fldChar[x, y - 1] := fldChar[x, y];
      fldChar[x, y] := 0;
    end;

  for x := 0 to 7 do
    fldChar[x, grY02] := fld02[x];

//  RefreshChar;
end;

procedure TfrmAntic3.ShiftCharRightProc(Sender: TObject);
var
  x, y, n : byte;
begin
  for y := 0 to grY02 do begin
    n := fldChar[7, y];
    for x := grX - 1 downto 0 do
      fldChar[x + 1, y] := fldChar[x, y];

    fldChar[0, y] := n;
  end;

//  RefreshChar;
end;

procedure TfrmAntic3.MoveCharLeft(Sender: TObject);
var
  x, y : byte;
begin
  for x := 1 to grX do
    for y := grY02 downto 0 do begin
      fldChar[x - 1, y] := fldChar[x, y];
      fldChar[x, y] := 0;
    end;

//  RefreshChar;
end;

procedure TfrmAntic3.MoveCharUp(Sender: TObject);
var
  x, y : byte;
begin
  for x := 0 to grX do
    for y := 1 to grY02 do begin
      fldChar[x, y - 1] := fldChar[x, y];
      fldChar[x, y] := 0;
    end;

//  RefreshChar;
end;

procedure TfrmAntic3.ShiftCharLeftProc(Sender: TObject);
var
  x, y, n : byte;
begin
  for y := 0 to grY02 do begin
    n := fldChar[0, y];
    for x := 1 to grX do
      fldChar[x - 1, y] := fldChar[x, y];

    fldChar[7, y] := n;
  end;

//  RefreshChar;
end;

procedure TfrmAntic3.ShiftCharDownProc(Sender: TObject);
var
  x, y : byte;
  fld02 : array[0..7] of byte;
begin
  for x := 0 to 7 do
    fld02[x] := fldChar[x, grY02];

  for x := 0 to grX do
    for y := grY02 - 1 downto 0 do begin
      fldChar[x, y + 1] := fldChar[x, y];
      fldChar[x, y] := 0;
    end;

  for x := 0 to 7 do
    fldChar[x, 0] := fld02[x];

//  RefreshChar;
end;

procedure TfrmAntic3.MoveCharDown(Sender: TObject);
var
  x, y : byte;
begin
  for x := 0 to grX do
    for y := grY02 - 1 downto 0 do begin
      fldChar[x, y + 1] := fldChar[x, y];
      fldChar[x, y] := 0;
    end;

//  RefreshChar;
end;

procedure TfrmAntic3.MoveCharRight(Sender: TObject);
var
  x, y : byte;
begin
  for x := grX - 1 downto 0 do
    for y := grY02 downto 0 do begin
      fldChar[x + 1, y] := fldChar[x, y];
      fldChar[x, y] := 0;
    end;

//  RefreshChar;
end;

procedure TfrmAntic3.CharOper(Sender : TObject);
begin
  if radShiftChar.Checked then begin
    case (Sender as TSpeedButton).Tag of
      0: ShiftCharLeftProc(Sender);
      1: ShiftCharRightProc(Sender);
      2: ShiftCharUpProc(Sender);
      3: ShiftCharDownProc(Sender);
    end;
  end
  else begin
    case (Sender as TSpeedButton).Tag of
      0: MoveCharLeft(Sender);
      1: MoveCharRight(Sender);
      2: MoveCharUp(Sender);
      3: MoveCharDown(Sender);
    end;
  end;

  PlotChar(255, 255);
  DrawFontSetChar(offs);
  RefreshChar;
end;

procedure TfrmAntic3.FillScreenProc(Sender: TObject);
begin
  FillScreen(offs);
end;

procedure TfrmAntic3.NewScreenProc(Sender: TObject);
begin
  ClearScreenProc(Sender);
  filename := 'examples\screen01.an3';
  caption := programName + ' ' + programVersion + ' - Antic mode 3 editor (' + filename + ')';
end;

procedure TfrmAntic3.SaveScreenProc(Sender: TObject);
var
  j : integer;
  fs : TFileStream;
begin
  // Save as
  if isSaveAs then begin
    frmMain.dlgSave.Title := 'Save Antic mode 3 screen as';
    frmMain.dlgSave.Filter := 'Antic mode 3 screen files (*.an3)|*.an3|All files (*.*)|*.*';
    frmMain.dlgSave.Filename := filename;
    if frmMain.dlgSave.Execute then begin
      filename := frmMain.dlgSave.Filename;
      fs := TFileStream.Create(filename, fmCreate);
      try
        for j := 0 to maxSize do
          fs.WriteByte(fldAtascii[j]);

        caption := programName + ' ' + programVersion + ' - Antic mode 3 editor (' + filename + ')';
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
//procedure TfrmAntic3.SaveScreen(Sender: TObject);
//begin
//  //if filename = 'screen01.an3' then
//  //  isSaveAs := true
//  //else
//  //  isSaveAs := false;
//
//  SaveScreen;
//end;

// Save Atascii screen as
procedure TfrmAntic3.SaveScreenAsProc(Sender: TObject);
begin
  isSaveAs := true;
  SaveScreenProc(Sender);
end;

// Load Atascii screen from file
procedure TfrmAntic3.LoadScreenProc(Sender: TObject);
var
  j, r, m : integer;
  fs : TFileStream;
begin
  frmMain.dlgOpen.Title := 'Open existing Antic mode 3 screen file';
  frmMain.dlgOpen.Filter := 'Antic mode 3 screen files (*.an3)|*.an3|All files (*.*)|*.*';
  if frmMain.dlgOpen.Execute then begin
    filename := frmMain.dlgOpen.Filename;
    fs := TFileStream.Create(Filename, fmOpenReadWrite);
    try
      r := 0; m := 0;
      for j := 0 to maxSize do
        if fs.Position < fs.Size then begin
          fldAtascii[j] := fs.ReadByte;
          if (j > (maxX - 1)) and (j mod maxX = 0) then begin
            r := 0;
            Inc(m);
          end;
          PutChar(r, m, fldAtascii[j]);
          Inc(r);
        end;

      FillByte(fldChar, SizeOf(fldChar), 0);
      caption := programName + ' ' + programVersion +
                 ' - Antic mode 3 editor (' + filename + ')';
    finally
      fs.Free;
    end;
    //except
    //  on E: EFOpenError do
    //    writeln('Error reading font file occurred! Details: ', E.Message);
    //end;
  end;
end;

procedure TfrmAntic3.itemLoadFontClick(Sender: TObject);
begin
  LoadFontProc(Sender);
  RefreshData;
end;

procedure TfrmAntic3.itemSaveFontClick(Sender: TObject);
begin
  SaveFontProc(Sender);
end;

procedure TfrmAntic3.itemSetDefaultFontClick(Sender: TObject);
begin
  DefaultFontSet(fldFontSet);
  ShowFontSet;
  RefreshData;
end;

// Fill screen with character
procedure TfrmAntic3.FillScreen(character : byte);
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
      Inc(m);
    end;
    PutChar(r, m, fldAtascii[j]);
    Inc(r);
  end;
end;

// Clear screen
procedure TfrmAntic3.ClearScreenProc(Sender: TObject);
begin
  FillScreen(0);
end;

// Draw character
procedure TfrmAntic3.Plot(xf, yf : integer);
var
  col, i, j, xx, yy : byte;
  dx, dy            : integer;
  offset2, offs2    : integer;
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
    for i := 0 to maxX - 1 do begin
      if (xf >= i shl 3) and (xf < (i + 1) shl 3) then begin
        // 8*i = i shl 3
        xf := i shl 3;
        xx := i;
        break;
      end;
    end;
    if (yf > 10*j) and (yf <= 10*(j + 1)) then begin
      yf := j*10;
      yy := j;
      break;
    end;
  end;

  if xx + yy*maxX > maxSize then Exit;

  statusBar.Panels[0].Text := 'Editor cursor coordinates (x: ' +
                              inttostr(xx) + ', y: ' + inttostr(yy) + ')';
  offset2 := offs2;
  //offset := offset2 shl 3;
  if not isFontSetNormal then Inc(offset2, 128);

  fldAtascii[xx + yy*maxX] := offset2;

  for dy := 0 to 9 do begin
    for dx := 0 to 7 do begin
//      col := fldFontSet[dx, dy + offset];
//      col := fld[dx, dy + offset];
      col := fldChar[dx, dy];

      //if not isFontSetNormal then begin
      //  if col = 1 then
      //    col := 0
      //  else if col = 0 then begin
      //    col := 1;
      //  end;
      //end;

//      fldChar[dx, dy] := col;

//      fldScreen[xf + dx, yf + dy] := col;
//      col := SetColorIndex(col, isFontSetNormal);
      imgEditor.Canvas.Brush.Color := coltabFont[col];
      imgEditor.Canvas.FillRect(bounds((xf + dx)*factX, (yf + dy)*factY, factX, factY));
    end;
  end;
end;

// Draw character
procedure TfrmAntic3.PutChar(scrPos, y, offset : integer);
var
  col : byte;
  dx, dy, xf, yf : integer;
  isInverse : boolean = false;
begin
  xf := scrPos shl 3;
  yf := y*10;

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

  for dy := 0 to 9 do
    for dx := 0 to 7 do begin
      //col := fld[dx, dy + offset];
      col := fld[dx, offset*10 + dy];
//      fldScreen[xf + dx, yf + dy] := col;
      if isInverse then begin
        //if col = 1 then
        //  col := 0
        //else if col = 0 then
        //  col := 1;
        col := 1 - col;
      end;

      FillRectEx(imgEditor, coltabFont[col], (xf + dx)*factX, (yf + dy)*factY, factX, factY);
    end;
end;

procedure TfrmAntic3.ShapeClear(image : TImage; fcolor : TColor);
begin
  image.Canvas.Brush.Style := bsSolid;
  FillRectEx(image, fcolor, 0, 0, image.Width, image.Height);
end;

procedure TfrmAntic3.RefreshCharX(offset : integer);
var
  col, xf, yf : integer;
begin
  ShapeClear(imgChar, colTab[0]);
  ShapeClear(imgCharEdit, colTab[0]);

  for yf := 0 to grY02 do
    for xf := 0 to grX02 do begin
      //col := fldFontSet[xf, yf + offset];
      col := fld[xf, offset*10 + yf];

      if not isFontSetNormal then begin
        //if col = 1 then
        //  col := 0
        //else if col = 0 then
        //  col := 1;
        col := 1 - col;
      end;

      fldChar[xf, yf] := col;
//      imgChar.Canvas.Brush.Color := coltabFont[col];
//      imgChar.Canvas.FillRect(bounds(xf*factX03, yf*factY03, factX03, factY03));
      FillRectEx(imgChar, coltabFont[col], xf*factX03, yf*factY03, factX03, factY03);

//      fldChar02[xf, yf] := col;
//      imgCharEdit.Canvas.Brush.Color := coltabFont[col];
//      imgCharEdit.Canvas.FillRect(bounds(xf*factX04, yf*factY04, factX04, factY04));
      FillRectEx(imgCharEdit, coltabFont[col], xf*factX04, yf*factY04, factX04, factY04);
    end;

  imgChar.Refresh;
  imgCharEdit.Refresh;
end;

procedure TfrmAntic3.DrawFontSetChar(offset : byte);
var
  n : byte;
  col, xf, yf : integer;
  xoffset, yoffset : integer;
begin
  yoffset := offsY*20;
  if offset < 16 then
    xoffset := offset shl 4
  else begin
    for n := 1 to 7 do
      if (offset >= n shl 4) and (offset < 32 + (n - 1) shl 4) then begin
        xoffset := (offset - n shl 4) shl 4;
        break;
      end;
  end;
  for yf := 0 to grY02 do
    for xf := 0 to grX02 do begin
      col := fldChar[xf, yf];
      if not isFontSetNormal then begin
        //if col = 0 then
        //  col := 1
        //else if col = 1 then
        //  col := 0;
        col := 1 - col;
      end;

      fld[xf, offset*10 + yf] := col;
//      imgFontSet.Canvas.Brush.Color := coltabFont[col];
//      imgFontSet.Canvas.FillRect(bounds(
//        xf*factX + xoffset, yf*factY + yoffset, factX, factY));
      FillRectEx(imgFontSet, coltabFont[col],
                 xf*factX + xoffset, yf*factY + yoffset, factX, factY);

      //if col = 0 then
      //  col := 1
      //else if col = 1 then
      //  col := 0;
      col := 1 - col;

//      imgFontSetInv.Canvas.Brush.Color := coltabFont[col];
//      imgFontSetInv.Canvas.FillRect(bounds(
//        xf*factX + xoffset, yf*factY + yoffset, factX, factY));
      FillRectEx(imgFontSetInv, coltabFont[col],
                 xf*factX + xoffset, yf*factY + yoffset, factX, factY);
    end;
end;

// Show character data values
procedure TfrmAntic3.ShowBinValues;
var
  i, j : byte;
  bin : string[8];
begin
  for j := 0 to grY02 do begin
    bin := '';
    for i := 0 to grX do
      bin += IntToStr(fldChar[i, j]);

    bin := IntToStr(bin2dec(bin)) + ' $' + Dec2Hex(bin2dec(bin));
    case j of
      0 : lblNum0.Caption := bin;
      1 : lblNum1.Caption := bin;
      2 : lblNum2.Caption := bin;
      3 : lblNum3.Caption := bin;
      4 : lblNum4.Caption := bin;
      5 : lblNum5.Caption := bin;
      6 : lblNum6.Caption := bin;
      7 : lblNum7.Caption := bin;
      8 : lblNum8.Caption := bin;
      9 : lblNum9.Caption := bin;
    end;
  end;
end;

// Draw character pixel in editor
procedure TfrmAntic3.PlotChar(xf, yf : byte);
var
  col : byte;
begin
  if xf + yf < 510 then begin
    case btn02 of
      mbLeft : col := 1;
      mbRight: col := 0;
    else
      exit;
    end;

    // Check Antic 3 character boundaries
    if xf > grX then xf := grX;

    if offs < 96 then
      if yf > grY then yf := grY
    else
      if yf < 2 then yf := 2;

    fldChar[xf, yf] := col;
//    fld[xf, yf + offs shl 3] := col;
//    imgCharEdit.Canvas.Brush.Color := coltabFont[col];
//    imgCharEdit.Canvas.FillRect(bounds(xf*factX04, yf*factY04, factX04, factY04));
    FillRectEx(imgCharEdit, coltabFont[col], xf*factX04, yf*factY04, factX04, factY04);
  end;

  ShowBinValues;

  // Refresh original set
  if xf + yf < 510 then
    DrawFontSetChar(offs);
end;

procedure TfrmAntic3.imgCharEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : byte;
begin
  btn02 := Button;
  xf := X div factX04;
  yf := Y div factY04;

  // Check for mouse button clicked
  if btn02 = mbRight then
    varBtn02 := btn
  else
    varBtn02 := mbLeft;

  //if not isFontSetNormal then begin
  //  setNormalMode := true;
  //end;

  PlotChar(xf, yf);
  charEditIndex[offs] := 1;
end;

procedure TfrmAntic3.imgCharEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : byte;
begin
  xf := X div factX04;
  yf := Y div factY04;
  PlotChar(xf, yf);
  statusBar.Panels[1].Text := 'Character editor cursor coordinates: ' +
                              'x: ' + inttostr(xf) + ', y: ' + inttostr(yf);
end;

procedure TfrmAntic3.imgCharEditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn02 := mbMiddle;
  RefreshChar;
end;

procedure TfrmAntic3.RefreshChar;
var
  col, xf, yf : integer;
  j : integer;
  m, r : byte;
begin
  //for yf := 0 to chrY do begin
  //  for xf := 0 to chrX do begin
  //    fldFontSet[xf, yf + offs shl 3] := fldChar[xf, yf];
  //    imgChar.Canvas.Brush.Color := coltabFont[fldChar[xf, yf]];
  //    imgChar.Canvas.FillRect(bounds(xf*chrFactX, yf*chrFactY, chrFactX, chrFactY));
  //  end;
  //end;

  ShapeClear(imgCharEdit, colTab[0]);

//  offset := offset shl 4;
//  showmessage(inttostr(offs));

  for yf := 0 to grY + 2 do
    for xf := 0 to grX do begin
      col := fldChar[xf, yf];
//      fld[xf, yf + offs shl 3] := col;
      FillRectEx(imgCharEdit, coltabFont[col], xf*factX04, yf*factY04, factX04, factY04);
    end;

  imgCharEdit.Refresh;
  ShowBinValues;
  PlotChar(255, 255);
//  ShowFontSet;

  r := 0; m := 0;
  for j := 0 to maxSize do begin
    if (j > (maxX - 1)) and (j mod maxX = 0) then begin
      r := 0;
      Inc(m);
    end;
    if fldAtascii[j] = offs then
      PutChar(r, m, fldAtascii[j]);

    Inc(r);
  end;
end;

procedure TfrmAntic3.LoadFontProc(Sender: TObject);
var
  bin : string[9];
  j : integer;
  r, i : byte;
  fs : TFileStream;
begin
  frmMain.dlgOpen.Title := 'Open existing Antic 3 character set file';
  frmMain.dlgOpen.Filter := 'Antic 3 character set files (*.fn3, *.fnt, *.fon, *.set)' +
                            '|*.fn3;*.fnt;*.fon;*.set|All files (*.*)|*.*';
  if frmMain.dlgOpen.Execute then begin
    fontFilename := frmMain.dlgOpen.Filename;
    fs := TFileStream.Create(fontFilename, fmOpenReadWrite);
    try
      for j := 0 to 1023 do
        if fs.Position < fs.Size then begin
          r := fs.ReadByte;
          bin := IntToBin(r, 8);
          for i := 0 to 7 do
            fldFontSet[i, j] := StrToInt(bin[i + 1]);
        end;

      Caption := programName + ' ' + programVersion +
                 ' - Antic mode 3 editor (' + fontFilename + ')';
//      ClearCharProc(Sender);
      ShowFontSet;
    finally
      fs.Free;
    end;
    //except
    //  on E: EFOpenError do
    //    writeln('Error reading font file occurred! Details: ', E.Message);
    //end;
  end;
end;

//procedure TfrmAntic3.LoadFontProc02(Sender: TObject);
//var
//  bin : string[9];
//  fs : TFileStream;
////  cnt : word = 0;
//begin
//  frmMain.dlgOpen.Title := 'Open existing character set file';
//  frmMain.dlgOpen.Filter := 'Character set files (*.fnt, *.fon, *.set)' +
//                            '|*.fnt;*.fon;*.set|All files (*.*)|*.*';
//  if frmMain.dlgOpen.Execute then begin
//    filename := frmMain.dlgOpen.Filename;
//    fs := TFileStream.Create(Filename, fmOpenReadWrite);
//    try
//      (*
//      for j := 0 to 1023 do begin
//        r := fs.ReadByte;
//        bin := IntToBin(r, 8);
//        if j < 768 then begin
////          if j mod 7 = 0 then begin
////            cnt02 := j;
//            for i := 0 to 7 do
//              fld[i, j + cnt02] := StrToInt(bin[i + 1]);
////          end;
//        end;
//        if j mod 8 = 0 then begin
//          cnt02 := 2;
////          Inc(cnt02, 2);
//        end
//        else
//          cnt02 := 0;
//      end;*)
//      for j := 0 to 1023 do begin
//        r := fs.ReadByte;
//        bin := IntToBin(r, 8);
//        for n := 0 to 127 do begin
//          if ((j >= n*8) and (j <= n*8 + 8)) then begin
//            for i := 0 to 7 do begin
////              fld[i, cnt] := StrToInt(bin[i + 1]);
//              fldFontSet[i, j] := StrToInt(bin[i + 1]);
//            end;
//            (*
//            if (j < 776) and (j = n*8 + 8) then begin
//              for i := 0 to 7 do begin
//                fld[i, cnt + 1] := 0;
//                fld[i, cnt + 2] := 0;
//              end;
//              Inc(cnt, 2);
//            end
//            else if (j >= 776) and (j = n*8 + 8) then begin
//              //for i := 0 to 7 do begin
//              //  fld[i, cnt - 8] := 0;
//              //  fld[i, cnt - 7] := 0;
//              //  fld[i, cnt - 1] := 0;
//              //  fld[i, cnt] := 0;
//              //end;
//              //Inc(cnt, 2);
//            end
//            else
//              Inc(cnt);
//            *)
//          end;
//        end;
//        //if ((j >= 8) and (j <= 15)) then begin
//        //  for i := 0 to 7 do begin
//        //    fld[i, cnt] := StrToInt(bin[i + 1]);
//        //  end;
//        //  if j = 15 then begin
//        //    fld[i, cnt + 1] := 0;
//        //    fld[i, cnt + 2] := 0;
//        //    Inc(cnt, 2);
//        //  end
//        //  else
//        //    Inc(cnt);
//        //end;
//        (*
//        if (j mod 8 = 0) and (j > 0) then begin
//          showmessage(inttostr(j) + ' * ' + InttoStr((j mod 8)));
//          fld[i, j + cnt + 1] := 0;
//          fld[i, j + cnt + 2] := 0;
////          cnt := 0;
//        end
//        else
//          cnt := j;
//        *)
//      end;
//      Caption := programName + ' ' + programVersion + ' - Antic mode 3 editor (' + filename + ')';
////      ShowFontSet(false);
//      //for i := 0 to 127 do
//      //  DrawFontSetChar(i);
//    finally
//      fs.Free;
//    end;
//  end;
//end;

procedure TfrmAntic3.SaveFontProc(Sender: TObject);
var
  bin : string[9];
  j : integer;
  i, n : byte;
  fs : TFileStream;
begin
  frmMain.dlgSave.Title := 'Save Antic 3 character set as';
  frmMain.dlgSave.Filter := 'Antic 3 character set files (*.fn3, *.fnt, *.fon, *.set)' +
                            '|*.fn3;*.fnt;*.fon;*.set|All files (*.*)|*.*';
  frmMain.dlgSave.Filename := fontFilename;
  if frmMain.dlgSave.Execute then begin
    fontFilename := frmMain.dlgSave.Filename;
//    filename := GetCurrentDir + '\FONT.SET';
    fs := TFileStream.Create(fontFilename, fmCreate);
    try
      (*
      for j := 0 to 1280 do begin
        bin := '';
        for i := 0 to 7 do begin
          bin += IntToStr(fld[i, j]);
        end;
        if j < 960 then begin
//          if j mod 10 = 0 then begin
          if cnt <= 8 then begin
            fs.WriteByte(bin2dec(bin));
          end;
//          end;
        end
        else begin
          if cnt >= 2 then begin
            fs.WriteByte(bin2dec(bin));
          end;
        end;
        if cnt < 10 then
          Inc(cnt)
        else begin
          cnt := 0;
        end;
        //if j mod 8 = 0 then begin
        //  cnt02 := 2;
        //end;
      end;
      *)
(*
      for j := 0 to 1279 do begin
        for n := 0 to 127 do begin
          if (j < 970) and ((j >= n*10) and (j <= n*10 + 7)) then begin
            bin := '';
            for i := 0 to 7 do begin
              bin += IntToStr(fld[i, j]);
            end;
            fs.WriteByte(bin2dec(bin));
          end
          else if (j >= 970) and ((j >= n*10) and (j <= n*10 + 7)) then begin
            bin := '';
            for i := 0 to 7 do begin
              bin += IntToStr(fld[i, j]);
            end;
            fs.WriteByte(bin2dec(bin));
          end;
          end;
      end;
      *)
      for j := 0 to 127 do begin
        if j < 96 then begin
          for n := 0 to 7 do
            for i := 0 to 7 do
              fldFontSet[i, j shl 3 + n] := fld[i, j*10 + n];
        end
        else begin
          for n := 0 to 7 do
            for i := 0 to 7 do
              fldFontSet[i, j shl 3 + n] := fld[i, j*10 + n];

          for i := 0 to 7 do begin
            fldFontSet[i, j shl 3] := fld[i, j*10 + 8];
            fldFontSet[i, j shl 3 + 1] := fld[i, j*10 + 9];
          end;
        end;
      end;

      for j := 0 to 1023 do begin
        bin := '';
        for i := 0 to 7 do
          bin += IntToStr(fldFontSet[i, j]);

        fs.WriteByte(bin2dec(bin));
      end;

      Caption := programName + ' ' + programVersion +
                 ' - Antic mode 3 editor (' + fontFilename + ')';
    finally
      fs.Free;
    end;
  end;
end;

procedure TfrmAntic3.ClearCharProc(Sender: TObject);
begin
  FillByte(fldChar, SizeOf(fldChar), 0);
  PlotChar(255, 255);
  DrawFontSetChar(offs);
  RefreshChar;
end;

procedure TfrmAntic3.RefreshData;
var
  j : word;
  r, m : byte;
begin
  r := 0; m := 0;
  for j := 0 to maxSize do begin
    if (j > (maxX - 1)) and (j mod maxX = 0) then begin
      r := 0;
      Inc(m);
    end;
    PutChar(r, m, fldAtascii[j]);
    Inc(r);
  end;
//  FillByte(fldChar, SizeOf(fldChar), 0);
  RefreshCharX(0);
end;

end.
