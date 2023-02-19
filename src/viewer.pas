{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2023
  Unit: The Viewer - Mad Studio files viewer
}
unit viewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, lcltype, ShellCtrls,
  ComCtrls, StdCtrls, ExtCtrls, BCMaterialDesignButton, strutils,
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
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
  private
    fld : array[0..319, 0..191] of byte;
    is01bit : boolean;
    fldFontSet : fldFontSetType;
    fldCharSet : fldFontSetType;
    fldAtascii : array[0.._ANTIC_MODE_4_SIZE - 1] of byte;
    factX02, factY02 : byte;
    // Multi-color player resolution value factor
    factX03 : byte;
    maxX, maxY : byte;
    maxSize : integer;
    Antic4maxX, Antic4maxY : byte;
    Antic4maxSize : integer;
    maxTileSize : byte;
    fileExt : string;
    comboType : byte;
    fldPlayer : fldType;
    missile : array[0..1, 0..220] of byte;
    fldChar : array[0.._MAX_TILE_CHARS - 1, 0..7, 0..7] of byte;
    antic4TileArray : TAntic4TileType;
    // Player matrix and position
    playerPos : array[0..3, 0..80, 0..220] of byte;
    // Player position
    playerIndex : array[0..3] of byte;
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
    procedure RefresPM_MultiAll(player, factX, factY : byte);
//    procedure SyncPlayer;
    procedure DrawTile;
    procedure DrawTileChar(scrPos, y, offset : byte);
    procedure RefreshMissile;
    procedure DrawMissile(pm : TImage; factX, factY : byte);
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
  _COMBO_MISSILE = 112;
  _COMBO_MULTI_COLOR_PLAYER = 113;
  _COMBO_ANTIC_4_TILE = 114;

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
    end;
    formAntic4TileEditor: begin
      cmbFormats.ItemIndex := 14;
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
  FillByte(fldPlayer, SizeOf(fldPlayer), 0);
  FillByte(playerPos, SizeOf(playerPos), 0);
  FillByte(missile, SizeOf(missile), 0);

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
                            '*.fnt;*.fon;*.set;*.an4;*.an5;*.spr;*.msl;*.mpl;*.tl4';
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
    12: begin
      comboType := _COMBO_MISSILE;
      shellListView.Mask := '*.msl';
    end;
    13: begin
      comboType := _COMBO_MULTI_COLOR_PLAYER;
      shellListView.Mask := '*.mpl';
    end;
    14: begin
      comboType := _COMBO_ANTIC_4_TILE;
      shellListView.Mask := '*.tl4';
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
//  mplTypeIndex : byte = 11;
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
        36: comboType := _COMBO_MISSILE;
        174: comboType := _COMBO_MULTI_COLOR_PLAYER;
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
      else if fileExt = '.msl' then
        comboType := _COMBO_MISSILE
      else if fileExt = '.mpl' then
        comboType := _COMBO_MULTI_COLOR_PLAYER
      else if fileExt = '.tl4' then
        comboType := _COMBO_ANTIC_4_TILE
    end;

    if comboType = 255 then Exit;

    boxView.Visible := (comboType <> _COMBO_PLAYER) and (comboType <> _COMBO_MISSILE);
    boxPlayer.Visible := not boxView.Visible;  ////comboType = _COMBO_PLAYER;

    //Cursor := crHourGlass;
    //for x := 0 to ControlCount - 1 do
    //  Controls[x].Cursor := crHourGlass;
//    ShowCursor(frmViewer, frmViewer, crHourGlass);
    Screen.BeginWaitCursor;

    { Character set view
     -------------------------------------------------------------------------}
    if comboType = _COMBO_CHR_SET then begin
      imgFontSet.Visible := true;
      factX := 24; factY := 24;
      factX02 := 2; factY02 := 2;
      for j := 0 to 1023 do begin
        if fs.Position < fs.Size then begin
          r := fs.ReadByte;
          bin := IntToBin(r, 8);
          for i := 0 to 7 do
            fldCharSet[i, j] := StrToInt(bin[i + 1]);
        end;
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
      for j := 0 to maxSize do begin
        if fs.Position < fs.Size then begin
          fldAtascii[j] := fs.ReadByte;
          if (j > maxX) and (j mod (maxX + 1) = 0) then begin
            col := 0;
            Inc(h, 2);
          end;
          PutAntic2Char(col, h, fldAtascii[j]);
          Inc(col);
        end;
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

      for j := 0 to maxSize do begin
        if fs.Position < fs.Size then
          fldAtascii[j] := fs.ReadByte;
      end;

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

      r4 := 0; m4 := 0;
      for j := 0 to maxSize do begin
        if (j > (maxX - 1)) and (j mod maxX = 0) then begin
          r4 := 0;
          Inc(m4, 2);
        end;
        PutAntic6Char(r4, m4, fldAtascii[j]);
        Inc(r4);
      end;
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
      for j := 0 to Antic4maxSize do begin
        if fs.Position < fs.Size then
          fldAtascii[j] := fs.ReadByte;
      end;

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
    { Missile view
     -------------------------------------------------------------------------}
    else if comboType = _COMBO_MISSILE then begin
      // Read missile height
      h := fs.ReadByte;

      // Read missile color
      col := fs.ReadByte;
      coltab[4] := colorMem[col div 2];
      colorValues[4] := col;

      // Read missile data
      for y := 0 to h - 1 do begin
        if fs.Position < fs.Size then begin
          x := fs.ReadByte;
          bin := Dec2Bin(x);
          for x := 0 to 1 do
            missile[x, y] := StrToInt(bin[x + 7]);
        end;
      end;
      RefreshMissile;
    end
    { Multi-color player (sprite) view
     -------------------------------------------------------------------------}
    else if comboType = _COMBO_MULTI_COLOR_PLAYER then begin
      factX02 := 4; factY02 := 4;

      // Read player height
      h := fs.ReadByte;

      for i := 0 to 11 do begin
        // Player X-position
        if i < 4 then
          playerIndex[i] := fs.ReadByte
        // Player color
        else if (i >= 4) and (i <= 7) then begin
          j := fs.ReadByte;
          coltab[i] := colorMem[j div 2];
          colorValues[i] := j;
        end
        // Player size
        else begin
          j := fs.ReadByte;
          if i >= 8 then
            playerSize[i - 8] := j
        end;
      end;

//      debug('colorValues', colorValues[0], colorValues[1], colorValues[2]);
//      debug('playerSize', playerSize[0], playerSize[1], playerSize[2]);

      // 3rd color enable flag
      j := fs.ReadByte;
      if j = 0 then begin
        isPmMixedColor := false;
//          sbPmg.Panels[2].Text := 'Player mixed (3rd) color disabled';
      end
      else begin
        isPmMixedColor := true;
//          sbPmg.Panels[2].Text := 'Player mixed (3rd) color enabled';
      end;

      // Player data
      for i := 0 to 3 do begin
        for y := 0 to h - 1 do begin
          if fs.Position < fs.Size then begin
            x := fs.ReadByte;
            bin := Dec2Bin(x);
            for x := 0 to 7 do
              fldPlayer[i, x, y] := StrToInt(bin[x + 1]);
          end;
        end;

        case playerSize[i] of
          0: factX03 := 4;
          1: factX03 := 8;
          3: factX03 := 16;
        end;

        RefresPM_MultiAll(i, factX02, factY02);
      end;
    end
    { Antic 4 tile view
     -------------------------------------------------------------------------}
    else if comboType = _COMBO_ANTIC_4_TILE then begin
      FillRectEx(imgView, colTab[0], 0, 0, imgView.Width, imgView.Height);
      FillByte(fldChar, SizeOf(fldChar), 0);
      factX := 4; factY := 4;
      Antic4maxX := fs.ReadByte;
      Antic4maxY := fs.ReadByte;
//      debug('Antic4maxX', Antic4maxX, Antic4maxY);
      maxTileSize := (Antic4maxX + 1)*(Antic4maxY + 1) - 1;
      for y := 0 to Antic4maxY - 1 do begin
        for x := 0 to Antic4maxX - 1 do begin
          h := x + y shl 2;
          for i := 0 to _CHAR_DIM do begin
            if fs.Position < fs.Size then begin
              r := fs.ReadByte;
              bin := IntToBin(r, 8);
              for j := 0 to _CHAR_DIM do
                fldChar[h, j, i] := StrToInt(bin[j + 1]);
            end;
          end;
          col := fs.ReadByte;
          if col = 0 then
            antic4TileArray.charInverse[h] := false
          else
            antic4TileArray.charInverse[h] := true;
        end;
      end;

      DrawTile;
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
//    ShowCursor(frmViewer, frmViewer, crDefault);
    Screen.EndWaitCursor;
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
  for xf := 0 to grX do begin
    for yf := 0 to grY do begin
      col := fld[xf, yf];
      if col >= 1 then begin
        if grX = 319 then col := 9;
        FillRectEx(imgView, coltab[col], xf*factX, yf*factY, factX, factY);
      end;
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
    for yf := 0 to 7 do begin
      for xf := 0 to 7 do begin
        col := fldCharSet[xf, n shl 3 + yf];

        if col = 1 then
          FillRectEx(imgView, coltabFont[col], xf*factX02 + xoffset, yf*factY02 + yoffset,
                     factX02, factY02);
      end;
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

  if offset >= 128 then begin
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
//  mask : array[0..1] of byte;
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
//        col := fldCharSet[xf, n shl 3 + yf];
        col := Antic45Mask(fldCharSet[xf, n shl 3 + yf], cnt);
        //mask[cnt - 1] := col;
        //if cnt = 2 then begin
        //  if (mask[0] = 0) and (mask[1] = 1) then
        //    col := 1
        //  else if (mask[0] = 1) and (mask[1] = 0) then
        //    col := 2
        //  else if (mask[0] = 1) and (mask[1] = 1) then
        //    col := 3;

        if cnt = 2 then begin
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
//  mask : array[0..1] of byte;
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
//      col := fldFontSet[dx, dy + offset shl 3];
      col := Antic45Mask(fldFontSet[dx, dy + offset shl 3], cnt);
      //mask[cnt - 1] := col;
      //if cnt = 2 then begin
      //  if (mask[0] = 0) and (mask[1] = 1) then
      //    col := 1
      //  else if (mask[0] = 1) and (mask[1] = 0) then
      //    col := 2
      //  else if (mask[0] = 1) and (mask[1] = 1) then
      //    col := 3;

      if cnt = 2 then begin
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

{-----------------------------------------------------------------------------
 Draw player
 -----------------------------------------------------------------------------}
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
  for yf := 0 to _PM_MAX_LINES - 1 do begin
    for xf := 0 to _PM_WIDTH do begin
      col := fldPlayer[0, xf, yf];
      if col = 1 then
        pm.Canvas.Brush.Color := coltab[4]
      else if col <> 1 then
        pm.Canvas.Brush.Color := coltab[0];

      pm.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY))
    end;
  end;
end;

{-----------------------------------------------------------------------------
 Draw missile
 -----------------------------------------------------------------------------}
procedure TfrmViewer.RefreshMissile;
begin
   DrawMissile(imgP0_normal, 4, 4);
   DrawMissile(imgP0_double, 8, 4);
   DrawMissile(imgP0_quadrable, 16, 4);
   DrawMissile(imgP0_normalSr, 4, 2);
   DrawMissile(imgP0_doubleSr, 8, 2);
   DrawMissile(imgP0_quadrableSr, 16, 2);
end;

procedure TfrmViewer.DrawMissile(pm : TImage; factX, factY : byte);
var
  col : byte;
  xf, yf : integer;
begin
  FillRectEx(pm, coltab[0], 0, 0, pm.Width, pm.Height);
  for yf := 0 to _PM_MAX_LINES - 1 do begin
    for xf := 0 to 1 do begin
      col := missile[xf, yf];
      if col = 1 then begin
        col := 0;
        pm.Canvas.Brush.Color := coltab[col + 4]
      end
      else
        pm.Canvas.Brush.Color := coltab[0];

      pm.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
//      pm.Canvas.Pixels[xf*factX, yf*factY] := coltab[col + 4];
    end;
//    Inc(xf);
//    if xf > 1 then xf := 0;
  end;
  pm.Refresh;
end;

{-----------------------------------------------------------------------------
 Draw multi-color player
 -----------------------------------------------------------------------------}
(*
procedure TfrmViewer.SyncPlayer;
var
  col : byte;
  xf, yf : integer;
  i, n : byte;
begin
//  index := player;
  for i := 0 to 3 do begin
    maxX := _MAX_PLAYER_POS - 1;
    for yf := 0 to _PM_MAX_LINES - 1 do begin
      n := 0;
      for xf := playerIndex[i] to maxX do begin
        col := playerPos[i, xf, yf];

        if playerSize[i] = _PLAYER_SIZE_NORMAL then begin
          if n < 8 then
            fldPlayer[i, n, yf] := col;

          Inc(n);
        end
        else if playerSize[i] = _PLAYER_SIZE_DOUBLE then begin
          if xf mod 2 = 0 then begin
            if n < 16 then
              fldPlayer[i, n, yf] := col;

            Inc(n);
          end;
        end
        else if playerSize[i] = _PLAYER_SIZE_QUADRUPLE then begin
          if xf mod 4 = 0 then begin
            if n < 32 then
              fldPlayer[i, n, yf] := col;

            Inc(n);
          end;
        end;
      end;
    end;
  end;
end;
*)
procedure TfrmViewer.RefresPM_MultiAll(player, factX, factY : byte);
var
  xf, yf : integer;
  index : byte;
  col, col01, col02, col03 : byte;
  c, d : integer;
begin
  for c := 0 to _MAX_PLAYER_POS - 1 do
    for d := 0 to _PM_MAX_LINES - 1 do
      playerPos[player, c, d] := 0;

  if player < 2 then
    index := 0
  else
    index := 2;

  // Player data and size
  for yf := 0 to _PM_MAX_LINES - 1 do begin
    d := 0;
    for xf := 0 to 7 do begin
      if playerSize[player] = _PLAYER_SIZE_NORMAL then
        playerPos[player, playerIndex[player] + xf, yf] := fldPlayer[player, xf, yf]
      else if playerSize[player] = _PLAYER_SIZE_DOUBLE then begin
        playerPos[player, playerIndex[player] + xf + d, yf] := fldPlayer[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 1, yf] := fldPlayer[player, xf, yf];
        Inc(d);
      end
      else if playerSize[player] = _PLAYER_SIZE_QUADRUPLE then begin
        playerPos[player, playerIndex[player] + xf + d, yf] := fldPlayer[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 1, yf] := fldPlayer[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 2, yf] := fldPlayer[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 3, yf] := fldPlayer[player, xf, yf];
        Inc(d, 3);
      end
    end;
  end;

  for yf := 0 to _PM_MAX_LINES - 1 do begin
    for xf := 0 to _PM_WIDTH + _MAX_PLAYER_POS do begin
      col := playerPos[0, xf, yf];      // Player 0 value
      col01 := playerPos[1, xf, yf];    // Player 1 value
      col02 := playerPos[2, xf, yf];    // Player 2 value
      col03 := playerPos[3, xf, yf];    // Player 3 value

      imgView.Canvas.Brush.Color := coltab[index + 4];

      // Color register priority
      if col = 1 then begin
        if (col01 = 1) and isPmMixedColor then
          imgView.Canvas.Brush.Color := coltab[4] or coltab[5]
        else
          imgView.Canvas.Brush.Color := coltab[4]
      end
      else if col01 = 1 then
        imgView.Canvas.Brush.Color := coltab[5]
      else if col02 = 1 then begin
        if col = 1 then begin
          if (col01 = 1) and isPmMixedColor then
            imgView.Canvas.Brush.Color := coltab[4] or coltab[5]
          else
            imgView.Canvas.Brush.Color := coltab[4]
        end
        else if col01 = 1 then
          imgView.Canvas.Brush.Color := coltab[5]
        else if (col03 = 1) and isPmMixedColor then
          imgView.Canvas.Brush.Color := coltab[6] or coltab[7]
        else
          imgView.Canvas.Brush.Color := coltab[6];
      end
      else begin
        imgView.Canvas.Brush.Color := coltab[0];
        if col01 = 1 then
          imgView.Canvas.Brush.Color := coltab[5];

        if col03 = 1 then
          imgView.Canvas.Brush.Color := coltab[7];
      end;

      imgView.Canvas.FillRect(bounds(xf*factX02 + 1, yf*factY02 + 1, factX03, factY));
    end;
  end;
  imgView.Refresh;
//  SyncPlayer;
end;

{-----------------------------------------------------------------------------
 Draw tile
 -----------------------------------------------------------------------------}
procedure TfrmViewer.DrawTile;
var
  i, j : integer;
  m, r : byte;
begin
  r := 0; m := 0;
  for j := 0 to maxTileSize do begin
    for i := 1 to 4 do begin
      if j = i shl 2 then begin  // *4
        r := 0;
        Inc(m, 2);
        break;
      end;
    end;
    DrawTileChar(r, m, j);
    Inc(r);
  end;
end;

{-----------------------------------------------------------------------------
 Draw tile character
 -----------------------------------------------------------------------------}
procedure TfrmViewer.DrawTileChar(scrPos, y, offset : byte);
var
  xf, yf : integer;
  cnt : byte = 0;
  i, j, col : byte;
  mask : array[0..1] of byte;
begin
  xf := scrPos shl 3;
  yf := y shl 2;

  for i := 0 to _CHAR_DIM do begin
    for j := 0 to _CHAR_DIM do begin
      Inc(cnt);
      col := fldChar[offset, j, i];
//      col := Antic45Mask(fldChar[offset, j, i], cnt);
      mask[cnt - 1] := col;
      if cnt = 2 then begin
        if (mask[0] = 0) and (mask[1] = 1) then
          col := 1
        else if (mask[0] = 1) and (mask[1] = 0) then
          col := 2
        else if (mask[0] = 1) and (mask[1] = 1) then begin
          if antic4TileArray.charInverse[offset] then
            col := 10
          else
            col := 3;
        end;

        FillRectEx(imgView, colTab[col], (xf + j - 1)*factX, (yf + i)*factY, factX shl 1, factY);
        cnt := 0;
      end;
    end;
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

procedure TfrmViewer.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmViewer.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

procedure TfrmViewer.CloseProc(Sender : TObject);
begin
  frmViewer.filename := '';
  Close;
end;

end.

