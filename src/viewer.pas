{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: The Viewer - Mad Studio files viewer
}
unit viewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, lcltype, ShellCtrls,
  ComCtrls, EditBtn, StdCtrls, ExtCtrls, BCMaterialDesignButton, strutils,
  common;

type
  { TfrmViewer }
  TfrmViewer = class(TForm)
    btnClose : TBCMaterialDesignButton;
    btnOpen : TBCMaterialDesignButton;
    cmbFormats : TComboBox;
    boxPlayer : TGroupBox;
    boxView : TGroupBox;
    imgFontSet : TImage;
    imgP0_double : TImage;
    imgP0_doubleSr : TImage;
    imgP0_normal : TImage;
    imgP0_normalSr : TImage;
    imgP0_quadrable : TImage;
    imgP0_quadrableSr : TImage;
    imgView : TImage;
    Label43 : TLabel;
    Label44 : TLabel;
    Label46 : TLabel;
    Label47 : TLabel;
    Label48 : TLabel;
    Label49 : TLabel;
    Label50 : TLabel;
    Label51 : TLabel;
    lblFilename : TLabel;
    lblPath : TLabel;
    lblView02 : TLabel;
    shellListView : TShellListView;
    shellTreeView : TShellTreeView;
    StatusBar : TStatusBar;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure OpenProc(Sender : TObject);
    procedure CloseProc(Sender : TObject);
    procedure ShellListViewProc(Sender : TObject);
    procedure shellListViewKeyUp(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure cmbFormatsChange(Sender : TObject);
    procedure btnCloseMouseEnter(Sender : TObject);
    procedure btnCloseMouseLeave(Sender : TObject);
    procedure btnOpenMouseEnter(Sender : TObject);
    procedure btnOpenMouseLeave(Sender : TObject);
  private
    fld : array[0..319, 0..191] of byte;
    is01bit : boolean;
    fldFontSet : fldFontSetType;
    fldCharSet : fldFontSetType;
    fldAtascii : array[0.._ANTIC_MODE_4_SIZE - 1] of byte;
    factX02, factY02 : byte;
    maxX, maxY : byte;
    maxSize : integer;
    Antic4maxX, Antic4maxY : byte;
    Antic4maxSize : integer;
    fileExt : string;
    comboType : byte;
    fldPlayer : fldType;
    procedure GrModeSettings;
    procedure DrawImage;
    procedure ShowFontSet;
    procedure ShowAntic4FontSet(image : TImage; factor, factorY : byte);
    procedure PutAntic2Char(scrPos, y, offset : integer);
    procedure PutAntic4Char(scrPos, y, offset : integer);
    procedure PutAntic6Char(scrPos, y, offset : integer);
    procedure ClearData(Sender : TObject);
    procedure RefreshPM;
    procedure RefreshPlayer(pm : TImage; factX, factY : byte);
  public
    filename : string;
    isModal : boolean;
  end;

var
  frmViewer : TfrmViewer;

const
  _COMBO_ANTIC_2 = 106;
  _COMBO_ANTIC_4 = 109;
  _COMBO_ANTIC_5 = 110;
  _COMBO_ANTIC_6 = 107;
  _COMBO_ANTIC_7 = 108;
  _COMBO_CHR_SET = 100;
  _COMBO_PLAYER  = 111;

implementation

{$R *.lfm}

uses
  lib, colors, main;

{ TfrmViewer }

procedure TfrmViewer.FormCreate(Sender : TObject);
begin
  isDebug := true;
  isModal := false;
end;

procedure TfrmViewer.FormShow(Sender : TObject);
var
  path : string;
begin
  SaveColors;
  btnOpen.Visible := isModal;
  btnOpen.Enabled := btnOpen.Visible;

  path := ExtractFileDrive(ParamStr(0));
  shellTreeView.Root := path;
  shellTreeView.Root := '';
  shellListView.Root := path;
  shellListView.Root := '';
  shellTreeView.Path := getDir + 'examples\';;

  ClearData(Sender);
  DefaultFontSet(fldFontSet);
  FillRectEx(imgView, coltab[0], 0, 0, imgView.Width, imgView.Height);
  FillRectEx(imgFontSet, coltab[0], 0, 0, imgFontSet.Width, imgFontSet.Height);
  lblFilename.Caption := '';
  lblPath.Caption := '';

  case formId of
    formGraph: begin
      case grMode of
        grMode40x24x4: cmbFormats.ItemIndex := 3;
        grMode160x96x4: cmbFormats.ItemIndex := 1;
        grMode160x192x4: cmbFormats.ItemIndex := 4;
        grMode320x192x2: cmbFormats.ItemIndex := 2;
      end;
    end;
    formFont: begin
      cmbFormats.ItemIndex := 5;
    end;
    formAntic2: begin
      cmbFormats.ItemIndex := 6;
    end;
    formAntic6: begin
      if isAntic6 then
        cmbFormats.ItemIndex := 7
      else
        cmbFormats.ItemIndex := 8;
    end;
    formAntic4: begin
      if isAntic4 then
        cmbFormats.ItemIndex := 9
      else
        cmbFormats.ItemIndex := 10;
    end;
    formPmg: begin
      cmbFormats.ItemIndex := 11;
    end
    else
      cmbFormats.ItemIndex := 0;
  end;

  cmbFormatsChange(Sender);
end;

procedure TfrmViewer.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  isModal := false;
  RetrieveColors;
end;

procedure TfrmViewer.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_F12: frmMain.Show;
  end;
end;

procedure TfrmViewer.ClearData(Sender : TObject);
begin
  frmColors.SetDefaultPalette;

  FillByte(fldAtascii, SizeOf(fldAtascii), 0);
  FillByte(fld, SizeOf(fld), 0);
  FillByte(fldCharSet, SizeOf(fldCharSet), 0);

  FillRectEx(imgView, coltab[0], 0, 0, imgView.Width, imgView.Height);
  FillRectEx(imgFontSet, coltab[0], 0, 0, imgFontSet.Width, imgFontSet.Height);

  imgFontSet.Visible := false;
  lblView02.Visible := false;
  boxPlayer.Visible := false;
end;

procedure TfrmViewer.cmbFormatsChange(Sender : TObject);
begin
  comboType := 255;
  case cmbFormats.ItemIndex of
    0: begin
      shellListView.Mask := '*.gr0;*.an2;*.asc;*.gr1;*.gr2;*.an6;*.an7;*.gr3;*.gr7;*.mic;*.gr8;' +
                            '*.fnt;*.fon;*.set;*.an4;*.an5;*.spr';
    end;
    1: begin
      comboType := grMode160x96x4;
      shellListView.Mask := '*.gr7;*.mic';
    end;
    2: begin
      comboType := grMode320x192x2;
      shellListView.Mask := '*.gr8';
    end;
    3: begin
      comboType := grMode40x24x4;
      shellListView.Mask := '*.gr3';
    end;
    4: begin
      comboType := grMode160x192x4;
      shellListView.Mask := '*.mic';
    end;
    5: begin
      comboType := _COMBO_CHR_SET;
//      imgFontSet.Visible := true;
      shellListView.Mask := '*.fnt;*.fon;*.set';
    end;
    6: begin
      comboType := _COMBO_ANTIC_2;
      shellListView.Mask := '*.gr0;*.an2;*.asc';
    end;
    7: begin
      comboType := _COMBO_ANTIC_6;
      shellListView.Mask := '*.gr1;*.an6;';
    end;
    8: begin
      comboType := _COMBO_ANTIC_7;
      shellListView.Mask := '*.gr2;*.an7;';
    end;
    9: begin
      comboType := _COMBO_ANTIC_4;
      shellListView.Mask := '*.an4';
    end;
    10: begin
      comboType := _COMBO_ANTIC_5;
      shellListView.Mask := '*.an5';
    end;
    11: begin
      comboType := _COMBO_PLAYER;
      shellListView.Mask := '*.spr';
    end;
  end;
end;

{-----------------------------------------------------------------------------
 Graphics mode settings
 -----------------------------------------------------------------------------}
procedure TfrmViewer.GrModeSettings;
begin
  is01bit := false;
  case comboType of
    grMode40x24x4: begin
      grX := 39; grY := 23; factX := 8; factY := 8;
    end;
    grMode80x48x2: begin
      is01bit := true;
      grX := 79; grY := 47; factX := 12; factY := 12;
    end;
    grMode80x48x4: begin
      grX := 79; grY := 47; factX := 12; factY := 12;
    end;
    grMode160x96x2: begin
      is01bit := true;
      grX := 159; grY := 95; factX := 2; factY := 2;
    end;
    grMode160x96x4: begin
      grX := 159; grY := 95; factX := 2; factY := 2;
    end;
    grMode160x192x2: begin
      is01bit := true;
      grX := 159; grY := 191; factX := 2; factY := 1;
    end;
    grMode160x192x4: begin
      grX := 159; grY := 191; factX := 2; factY := 1;
    end;
    grMode320x192x2: begin
      is01bit := true;
      grX := 319; grY := 191; factX := 1; factY := 1;
    end;
  end;

  //case grMode of
  //  grMode40x24x4  : sbGr.Panels[2].Text := 'Graphics mode 40 x 24 in 4 colors';
  //  grMode80x48x2  : sbGr.Panels[2].Text := 'Graphics mode 80 x 48 in 2 colors';
  //  grMode80x48x4  : sbGr.Panels[2].Text := 'Graphics mode 80 x 48 in 4 colors';
  //  grMode160x96x2 : sbGr.Panels[2].Text := 'Graphics mode 160 x 96 in 2 colors';
  //  grMode160x96x4 : sbGr.Panels[2].Text := 'Graphics mode 160 x 96 in 4 colors';
  //  grMode160x192x2: sbGr.Panels[2].Text := 'Graphics mode 160 x 192 in 2 colors';
  //  grMode160x192x4: sbGr.Panels[2].Text := 'Graphics mode 160 x 192 in 4 colors';
  //  grMode320x192x2: sbGr.Panels[2].Text := 'Graphics mode 320 x 192 in 2 colors';
  //end;
end;

procedure TfrmViewer.ShellListViewProc(Sender : TObject);
var
  x, y : integer;
  dta : byte;
  fs : TFileStream;
  tempCalcX : word;
  bin : string[9];
  j : integer;
  r, i : byte;
  r4, m4 : integer;
  col, h : byte;
begin
  if not Assigned(shellListView.Selected) then Exit;
  ClearData(Sender);
  lblFilename.Caption := shellListView.Selected.Caption;
  lblPath.Caption := shellListView.Root;
  fileExt := ExtractFileExt(shellListView.Selected.Caption);

  fs := TFileStream.Create(shellListView.Root + shellListView.Selected.Caption, fmOpenRead);
  try
    if cmbFormats.ItemIndex = 0 then begin
      comboType := 255;
      case fs.Size of
        3844: comboType := grMode160x96x4;
        7680: comboType := grMode320x192x2;
        244: comboType := grMode40x24x4;
        7684: comboType := grMode160x192x4;
        1024: comboType := _COMBO_CHR_SET;
        960: comboType := _COMBO_ANTIC_2;
        42: comboType := _COMBO_PLAYER;
        _ANTIC_MODE_4_SIZE + 5:
          comboType := _COMBO_ANTIC_4;
        _TEXT_MODE_1_SIZE, _TEXT_MODE_1_SIZE + 5:
          comboType := _COMBO_ANTIC_6;
        _TEXT_MODE_2_SIZE, _TEXT_MODE_2_SIZE + 5:
          comboType := _COMBO_ANTIC_7;
      end;

      fileExt := LowerCase(fileExt);

      if (fileExt = '.fnt') or (fileExt = '.fon') or (fileExt = '.set') then
        comboType := _COMBO_CHR_SET
      else if (fileExt = '.gr0') or (fileExt = '.an2') or (fileExt = '.asc') then
        comboType := _COMBO_ANTIC_2
      else if fileExt = '.gr3' then
        comboType := grMode40x24x4
      else if (fileExt = '.gr7') or (fileExt = '.mic') then
        comboType := grMode160x96x4
      else if fileExt = '.gr8' then
        comboType := grMode320x192x2
      else if (fileExt = '.gr1') or (fileExt = '.an6') then
        comboType := _COMBO_ANTIC_6
      else if (fileExt = '.gr2') or (fileExt = '.an7') then
        comboType := _COMBO_ANTIC_7
      else if fileExt = '.an4' then
        comboType := _COMBO_ANTIC_4
      else if fileExt = '.an5' then
        comboType := _COMBO_ANTIC_5
      else if fileExt = '.spr' then
        comboType := _COMBO_PLAYER
    end;

    if comboType = 255 then Exit;

    boxView.Visible := comboType <> _COMBO_PLAYER;
    boxPlayer.Visible := comboType = _COMBO_PLAYER;

    //Cursor := crHourGlass;
    //for x := 0 to ControlCount - 1 do
    //  Controls[x].Cursor := crHourGlass;
    ShowCursor(frmViewer, frmViewer, crHourGlass);

    { Character set view
     -------------------------------------------------------------------------}
    if comboType = _COMBO_CHR_SET then begin
      imgFontSet.Visible := true;
      factX := 24; factY := 24;
      factX02 := 2; factY02 := 2;

      for j := 0 to 1023 do
        if fs.Position < fs.Size then begin
          r := fs.ReadByte;
          bin := IntToBin(r, 8);
          for i := 0 to 7 do
            fldCharSet[i, j] := StrToInt(bin[i + 1]);
        end;

      boxView.Caption := 'Normal characters';
      lblView02.Caption := 'Antic mode 4 characters';
      ShowFontSet;
    end
    { Antic 2 (text mode 0) screen view
     -------------------------------------------------------------------------}
    else if comboType = _COMBO_ANTIC_2 then begin
      FillRectEx(imgView, coltabFont[0], 0, 0, imgView.Width, imgView.Height);
      boxView.Caption := 'Antic mode 2 screen';
      lblView02.Caption := '';

      // Read image dimension
      if fs.Size < 960 then begin
        maxX := fs.ReadByte;
        maxY := fs.ReadByte;
      end
      else begin
        maxX := 39;
        maxY := 23;
      end;

      maxSize := (maxX + 1)*(maxY + 1) - 1;
      factX := 1; factY := 1;

      col := 0; h := 0;
      for j := 0 to maxSize do
        if fs.Position < fs.Size then begin
          fldAtascii[j] := fs.ReadByte;
          if (j > maxX) and (j mod (maxX + 1) = 0) then begin
            col := 0;
            Inc(h, 2);
          end;
          PutAntic2Char(col, h, fldAtascii[j]);
          Inc(col);
        end;
    end
    { Antic 6/7 (text mode 1/2) screen view
     -------------------------------------------------------------------------}
    else if (comboType = _COMBO_ANTIC_6) or (comboType = _COMBO_ANTIC_7) then begin
      boxView.Caption := 'Antic mode 6 screen';
      lblView02.Caption := '';

      factX := 2;
      maxX := 20;
      if comboType = _COMBO_ANTIC_6 then begin
        factY := 1;
        maxY := 24;
      end
      else begin
        factY := 2;
        maxY := 12;
      end;

      maxSize := maxX*maxY - 1;

      for j := 0 to maxSize do
        if fs.Position < fs.Size then
          fldAtascii[j] := fs.ReadByte;

      // Read color values
      if (fs.Size = _TEXT_MODE_1_SIZE + 5) or (fs.Size = _TEXT_MODE_2_SIZE + 5) then begin
        for i := 0 to 3 do begin
          dta := fs.ReadByte;
          coltab[i] := colorMem[dta div 2];
          colorValues[i] := dta;
        end;
        dta := fs.ReadByte;
        coltab[10] := colorMem[dta div 2];
        colorValues[10] := dta;
      end;

      FillRectEx(imgView, coltab[0], 0, 0, imgView.Width, imgView.Height);
//      imgView.Invalidate;

//      debug('maxsize', maxsize);
      r4 := 0; m4 := 0;
      for j := 0 to maxSize do begin
        if (j > (maxX - 1)) and (j mod maxX = 0) then begin
          r4 := 0;
          Inc(m4, 2);
        end;
        PutAntic6Char(r4, m4, fldAtascii[j]);
        Inc(r4);
      end;
//      debug('end');
    end
    { Antic 4 and Antic 5 screen view
     -------------------------------------------------------------------------}
    else if (comboType = _COMBO_ANTIC_4) or (comboType = _COMBO_ANTIC_5) then begin
//      ShowMessage('Antic 4 and Antic 5');
      boxView.Caption := 'Antic mode 4 screen';
      lblView02.Caption := '';

(*      r4 := 0; m4 := 0;
      for j := 0 to Antic4maxSize do
        fldAtascii[j] := fs.ReadByte;

      // Read color values
      if (fs.Size = _ANTIC_MODE_4_SIZE + 5) or (fs.Size = _ANTIC_MODE_5_SIZE + 5) then
        for i := 0 to 4 do begin
          dta := fs.ReadByte;
          coltab[i] := colorMem[dta div 2];
          colorValues[i] := dta;
        end;
        *)

      // Dimension x, y
      Antic4maxX := fs.ReadByte;
      Antic4maxY := fs.ReadByte;
      factX := 1;
      if comboType = _COMBO_ANTIC_4 then
        factY := 1
      else
        factY := 2;

      Antic4maxSize := (Antic4maxX + 1)*(Antic4maxY + 1) - 1;

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

      // Read data
      for j := 0 to Antic4maxSize do
        if fs.Position < fs.Size then
          fldAtascii[j] := fs.ReadByte;

      FillRectEx(imgView, coltab[0], 0, 0, imgView.Width, imgView.Height);

      r4 := 0; m4 := 0;
      for j := 0 to Antic4maxSize do begin
        if (j > Antic4maxX) and (j mod (Antic4maxX + 1) = 0) then begin
          r4 := 0;
          Inc(m4, 2);
        end;
        PutAntic4Char(r4, m4, fldAtascii[j]);
        Inc(r4);
      end;
    end
    { Player (sprite) view
     -------------------------------------------------------------------------}
    else if comboType = _COMBO_PLAYER then begin
      // Read player height
      h := fs.ReadByte;

      // Read player color
      col := fs.ReadByte;
      coltab[4] := colorMem[col div 2];
      colorValues[4] := col;

      // Read player data
      for y := 0 to h - 1 do begin
        x := fs.ReadByte;
        bin := Dec2Bin(x);
        for x := 0 to 7 do
          fldPlayer[0, x, y] := StrToInt(bin[x + 1]);
      end;

      RefreshPM;
    end
    { Graphics view
     -------------------------------------------------------------------------}
    else begin
      boxView.Caption := 'Graphics mode view';
      lblView02.Caption := '';
      GrModeSettings;
      for y := 0 to grY do begin
        // 4-color graphics modes
        //// Resolution less than 320x192
        if not is01bit then begin
           //(grMode = grMode40x24x4) or (grMode = grMode80x48x4) or (grMode = grMode160x96x4)
           //or (grMode = grMode160x192x4) then begin
  //        if grX < 319 then begin
          tempCalcX := grX div 4;
          for x := 0 to tempCalcX do begin
            dta := fs.ReadByte;
            fld[x shl 2, y] := dta div 64;
            fld[x shl 2 + 1, y] := (dta mod 64) shr 4;
            fld[x shl 2 + 2, y] := (dta mod 16) shr 2;
            fld[x shl 2 + 3, y] := dta mod 4;
          end;
        end
        // 2-color graphics modes
        else begin
  //          if grX = 319 then begin
          tempCalcX := grX shr 3;
          for x := 0 to tempCalcX do begin
            dta := fs.ReadByte;
            fld[x shl 3, y] := dta div 128;
            fld[x shl 3 + 1, y] := (dta mod 128) div 64;
            fld[x shl 3 + 2, y] := (dta mod 64) div 32;
            fld[x shl 3 + 3, y] := (dta mod 32) div 16;
            fld[x shl 3 + 4, y] := (dta mod 16) div 8;
            fld[x shl 3 + 5, y] := (dta mod 8) div 4;
            fld[x shl 3 + 6, y] := (dta mod 4) div 2;
            fld[x shl 3 + 7, y] := (dta mod 2);
          end;
        end;
      end;
      if ((comboType = grMode40x24x4) and (fs.Size = 244)) or
         ((comboType = grMode80x48x4) and (fs.Size = 964)) or
         ((comboType = grMode160x96x4) and (fs.Size = 3844)) or
         ((comboType = grMode160x192x4) and (fs.Size = 7684)) then
      begin
        for x := 0 to 3 do begin
          dta := fs.ReadByte;
          coltab[x] := colorMem[dta div 2];
          colorValues[x] := dta;
        end;
      end
      else begin
        if ((comboType = grMode80x48x2) and (fs.Size = 482)) or
           ((comboType = grMode160x96x2) and (fs.Size = 1922)) or
           ((comboType = grMode160x192x2) and (fs.Size = 3842)) then
        begin
          for x := 0 to 1 do begin
            dta := fs.ReadByte;
            coltab[x] := colorMem[dta div 2];
            colorValues[x] := dta;
          end;
        end;
      end;
      DrawImage;
    end;
  finally
    fs.Free;
    ShowCursor(frmViewer, frmViewer, crDefault);
    //Cursor := crDefault;
    //for x := 0 to ControlCount - 1 do
    //  Controls[x].Cursor := crDefault;
  end;
end;

procedure TfrmViewer.shellListViewKeyUp(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  ShellListViewProc(Sender);
end;

{-----------------------------------------------------------------------------
 Draw image depending on selected module tyoe
 -----------------------------------------------------------------------------}
procedure TfrmViewer.DrawImage;
var
  col, xf, yf : integer;
begin
  FillRectEx(imgView, coltab[0], 0, 0, imgView.Width, imgView.Height);
  for xf := 0 to grX do
    for yf := 0 to grY do begin
      col := fld[xf, yf];
      if col >= 1 then begin
        if grX = 319 then col := 9;
        FillRectEx(imgView, coltab[col], xf*factX, yf*factY, factX, factY);
      end;
    end;

//  imgView.Refresh;
end;

{-----------------------------------------------------------------------------
 Show character set (font) in text mode 0
 -----------------------------------------------------------------------------}
procedure TfrmViewer.ShowFontSet;
var
  n : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
begin
  FillRectEx(imgView, coltabFont[0], 0, 0, imgFontSet.Width, imgFontSet.Height);
  offset := 0;
  yoffset := 0;
  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, 16);
    end;
    xoffset := offset shl 4;
    for yf := 0 to 7 do
      for xf := 0 to 7 do begin
        col := fldCharSet[xf, n shl 3 + yf];

        if col = 1 then
          FillRectEx(imgView, coltabFont[col], xf*factX02 + xoffset, yf*factY02 + yoffset,
                     factX02, factY02);
      end;

    Inc(offset);
  end;

  ShowAntic4FontSet(imgFontSet, 16, factY02);
end;

{-----------------------------------------------------------------------------
 Draw Antic mode 2 character on screen
 -----------------------------------------------------------------------------}
procedure TfrmViewer.PutAntic2Char(scrPos, y, offset : integer);
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
  //for dy := 0 to maxY - 1 do
  //  for dx := 0 to maxX - 1 do
  //    if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
  //      xf := dx shl 3;
  //      break;
  //    end;

  if offset > 128 then begin
    Dec(offset, 128);
    isInverse := true;
  end;

//  offset := offset shl 3;

  for dy := 0 to 7 do
    for dx := 0 to 7 do begin
      col := fldFontSet[dx, dy + offset shl 3];
      if isInverse then
        col := 1 - col;

      if col = 1 then
        FillRectEx(imgView, coltabFont[col], (xf + dx)*factX, (yf + dy)*factY, factX, factY);
    end;
end;

{-----------------------------------------------------------------------------
 Show character set (font) in Antic mode 4
 -----------------------------------------------------------------------------}
procedure TfrmViewer.ShowAntic4FontSet(image : TImage; factor, factorY : byte);
var
  n, cnt : byte;
  col, xf, yf, offset, xoffset, yoffset : integer;
  mask : array[0..1] of byte;
begin
  FillRectEx(image, coltab[0], 0, 0, image.Width, image.Height);
  offset := 0;
  yoffset := 0;
  for n := 0 to 127 do begin
    if (n mod 16 = 0) and (n > 0) then begin
      offset := 0;
      Inc(yoffset, factor);
    end;
    cnt := 0;
    xoffset := offset shl 4;
    for yf := 0 to 7 do begin
      for xf := 0 to 7 do begin
        Inc(cnt);
        col := fldCharSet[xf, n shl 3 + yf];
        mask[cnt - 1] := col;
        if cnt = 2 then begin
          if (mask[0] = 0) and (mask[1] = 1) then
            col := 1
          else if (mask[0] = 1) and (mask[1] = 0) then
            col := 2
          else if (mask[0] = 1) and (mask[1] = 1) then
            col := 3;

          if col >= 1 then
            FillRectEx(image, coltab[col], xf*factX02 + xoffset, yf*factorY + yoffset,
                       factX02 shl 1, factorY);

          cnt := 0;
        end;
      end;
    end;
    Inc(offset);
  end;
end;

{-----------------------------------------------------------------------------
 Draw Antic mode 4 character on screen
 -----------------------------------------------------------------------------}
procedure TfrmViewer.PutAntic4Char(scrPos, y, offset : integer);
var
  cnt, col : byte;
  dx, dy, xf, yf : integer;
  mask : array[0..1] of byte;
begin
  xf := scrPos shl 3;
  yf := y shl 2;
  //for dy := 0 to Antic4maxY - 1 do
  //  for dx := 0 to Antic4maxX - 1 do
  //    if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
  //      xf := dx shl 3;
  //      break;
  //    end;

  cnt := 0;
//  offset := offset shl 3;
  for dy := 0 to 7 do
    for dx := 0 to 7 do begin
      Inc(cnt);
      col := fldFontSet[dx, dy + offset shl 3];
      mask[cnt - 1] := col;
      if cnt = 2 then begin
        if (mask[0] = 0) and (mask[1] = 1) then
          col := 1
        else if (mask[0] = 1) and (mask[1] = 0) then
          col := 2
        else if (mask[0] = 1) and (mask[1] = 1) then
          col := 3;

        if col >= 1 then
          FillRectEx(imgView, coltab[col],
                     (xf + dx - 1)*factX, (yf + dy)*factY, factX shl 1, factY);

        cnt := 0;
      end;
    end;
end;

{-----------------------------------------------------------------------------
 Draw Antic mode 6 and 7 character on screen
 -----------------------------------------------------------------------------}
procedure TfrmViewer.PutAntic6Char(scrPos, y, offset : integer);
var
  col, col02 : byte;
  xf, yf : integer;
  dx, dy : byte;
//  isInverse : boolean = false;
begin
  xf := scrPos shl 3;
  yf := y shl 2;

  //for dy := 0 to maxY - 1 do
  //  for dx := 0 to 19 do
  //    if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
  //      xf := dx shl 3;
  //      break;
  //    end;

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
  end
  else
    col02 := _CHARSET_UPPER;

  for dy := 0 to 7 do
    for dx := 0 to 7 do begin
      col := fldFontSet[dx, dy + offset shl 3];
      if col = 1 then begin
        col := col02;
        FillRectEx(imgView, coltab[col], (xf + dx)*factX, (yf + dy)*factY, factX, factY);
      end;
    end;
end;

procedure TfrmViewer.RefreshPM;
begin
  RefreshPlayer(imgP0_normal, 4, 4);
  RefreshPlayer(imgP0_double, 8, 4);
  RefreshPlayer(imgP0_quadrable, 16, 4);

  RefreshPlayer(imgP0_normalSr, 4, 2);
  RefreshPlayer(imgP0_doubleSr, 8, 2);
  RefreshPlayer(imgP0_quadrableSr, 16, 2);
end;

procedure TfrmViewer.RefreshPlayer(pm : TImage; factX, factY : byte);
var
  col : byte;
  xf, yf : integer;
begin
  FillRectEx(pm, coltab[0], 0, 0, pm.Width, pm.Height);
  for yf := 0 to _PM_MAX_LINES - 1 do
    for xf := 0 to _PM_WIDTH do begin
      col := fldPlayer[0, xf, yf];
      if col = 1 then
        pm.Canvas.Brush.Color := coltab[4]
      else if col <> 1 then
        pm.Canvas.Brush.Color := coltab[0];

      pm.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY))
    end;
end;

{-----------------------------------------------------------------------------
 Set filename of selected object to be opened in program module
 -----------------------------------------------------------------------------}
procedure TfrmViewer.OpenProc(Sender : TObject);
begin
  filename := lblPath.Caption + lblFilename.Caption;
  Close;
end;

procedure TfrmViewer.btnOpenMouseEnter(Sender : TObject);
begin
  btnOpen.NormalColor := $00CECECE;
  btnOpen.NormalColorEffect := clWhite;
end;

procedure TfrmViewer.btnOpenMouseLeave(Sender : TObject);
begin
  btnOpen.NormalColor := clWhite;
  btnOpen.NormalColorEffect := clSilver;
end;

procedure TfrmViewer.btnCloseMouseEnter(Sender : TObject);
begin
  btnClose.NormalColor := $00CECECE;
  btnClose.NormalColorEffect := clWhite;
end;

procedure TfrmViewer.btnCloseMouseLeave(Sender : TObject);
begin
  btnClose.NormalColor := clWhite;
  btnClose.NormalColorEffect := clSilver;
end;

procedure TfrmViewer.CloseProc(Sender : TObject);
begin
  frmViewer.filename := '';
  Close;
end;

end.

