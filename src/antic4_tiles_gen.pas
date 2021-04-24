{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Antic mode 4 tile editor - source code generator
}
unit antic4_tiles_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, lcltype, Windows,
  ComCtrls, Buttons, SpinEx, BCListBox, BCMDButton, BCMaterialDesignButton, StrUtils,
  common;

type
  { TfrmAntic4TilesGen }
  TfrmAntic4TilesGen = class(TForm)
    boxColors : TGroupBox;
    boxStartLine : TGroupBox;
    boxTilePos : TGroupBox;
    btnAtariBASIC : TBCMDButton;
    btnClose : TBCMaterialDesignButton;
    btnCopyToEditor : TBCMaterialDesignButton;
    btnEffectus : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnKickC : TBCMDButton;
    btnMads : TBCMDButton;
    btnCC65 : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    chkTextWindow : TCheckBox;
    chkUseColors : TCheckBox;
    color0 : TShape;
    color1 : TShape;
    color2 : TShape;
    color3 : TShape;
    color4 : TShape;
    editTilePosY : TSpinEditEx;
    editStartLine : TSpinEditEx;
    editLineStep : TSpinEditEx;
    editTilePosX : TSpinEditEx;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label7 : TLabel;
    Label8 : TLabel;
    lblLineStep : TLabel;
    lblLineStep1 : TLabel;
    lblStartLine : TLabel;
    lblStartLine1 : TLabel;
    ListExamples : TBCPaperListBox;
    memo : TMemo;
    panelLang : TBCPaperPanel;
    radDataType : TRadioGroup;
    StaticText1 : TStaticText;
    StaticText2 : TStaticText;
    StaticText3 : TStaticText;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure CloseProc(Sender : TObject);
    procedure CopyToEditorProc(Sender : TObject);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
    procedure ExamplesProc(Sender : TObject);
    procedure LanguageProc(Sender : TObject);
  private
    listings : TListings;
    isCreate : boolean;
    fldChar : array[0.._MAX_TILE_CHARS - 1, 0..7, 0..7] of byte;
    procedure CreateCode;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
    function Example04 : string;
    function Example05 : string;
    function Example06 : string;
    function SetTileDataValues(isOnlyTile : boolean) : string;
    function SetTileCharDataValues(isOnlyTile : boolean) : string;
  public

  end;

var
  frmAntic4TilesGen : TfrmAntic4TilesGen;

implementation

{$R *.lfm}

uses
  antic4_tiles, src_editor, code_lib, lib;

{ TfrmAntic4TilesGen }

procedure TfrmAntic4TilesGen.FormCreate(Sender : TObject);
var
  i : byte;
begin
  isCreate := true;

  // Example 1
  for i := 0 to 7 do
    listings[0, i] := true;

  // Example 2
  for i := 0 to 7 do
    listings[1, i] := true;

  // Example 3
  for i := 0 to 7 do
    listings[2, i] := true;

  // Example 4
  for i := 0 to 7 do
    listings[3, i] := true;

  // Example 5
  listings[4, 0] := true;
  listings[4, 1] := true;
  listings[4, 2] := true;
  listings[4, 3] := false;
  listings[4, 4] := true;
  listings[4, 5] := false;
  listings[4, 6] := false;
  listings[4, 7] := false;

  // Example 6
  listings[5, 0] := true;
  listings[5, 1] := true;
  listings[5, 2] := true;
  listings[5, 3] := false;
  listings[5, 4] := true;
  listings[5, 5] := false;
  listings[5, 6] := false;
  listings[5, 7] := false;

//  anticMode := IntToStr(frmAntic4Tiles.anticMode);
end;

procedure TfrmAntic4TilesGen.FormShow(Sender : TObject);
begin
  FormStyle := fsSystemStayOnTop;

  color0.Brush.Color := colTab[0];   // POKE 712,C
  color1.Brush.Color := colTab[1];   // POKE 708,C
  color2.Brush.Color := colTab[2];   // POKE 709,C
  color3.Brush.Color := colTab[3];   // POKE 710,C
  color4.Brush.Color := colTab[10];  // POKE 711,C

  isCreate := false;
  langIndex := 0;
  ListExamples.ListBox.ItemIndex := 0;
  ExamplesProc(Sender);
end;

procedure TfrmAntic4TilesGen.CreateCode;
var
  code : string;
begin
  boxStartLine.Enabled := langIndex < 2;
  boxStartLine.Visible := boxStartLine.Enabled;

  if langIndex = 0 then begin
    radDataType.ItemIndex := 0;
    TRadioButton(radDataType.Controls[1]).Enabled := false;
    TRadioButton(radDataType.Controls[2]).Enabled := false;
  end
  else begin
    TRadioButton(radDataType.Controls[1]).Enabled := true;
    TRadioButton(radDataType.Controls[2]).Enabled := true;
  end;

  memo.Lines.Clear;

  case ListExamples.ListBox.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
    3: code := Example04;
    4: code := Example05;
    5: code := Example06;
  end;

  memo.Lines.Add(code);

  // Set cursor position at the top of memo object
  memo.SelStart := 0;
  memo.SelLength := 0;
  SendMessage(memo.Handle, EM_SCROLLCARET, 0, 0);
end;

function TfrmAntic4TilesGen.SetTileDataValues(isOnlyTile : boolean) : string;
var
  x, y : byte;
  line : string;
  i, j, index : byte;
  bin : string;
  tiles, minTile, maxTile : byte;
  col : byte;
  maxTiles02 : byte;
  codeLine : string = '';
  sum : word;
begin
  if isOnlyTile then begin
    minTile := frmAntic4Tiles.antic4Tile.selected;
    maxTile := minTile;
  end
  else begin
    minTile := 1;
    maxTile := _MAX_TILES;
  end;

  case langIndex of
    _ACTION:
      codeLine := 'BYTE ARRAY'#13#10;
    _MAD_PASCAL:
      codeLine := 'const'#13#10;
  end;
  for tiles := minTile to maxTile do begin
    sum := 0;

    // Map Antic 4 mode character data to Antic 2 mode character data
    for i := 0 to _MAX_TILE_CHARS - 1 do
      for y := 0 to _CHAR_DIM do
        for x := 0 to _CHAR_DIM do
          case x of
            0, 2, 4, 6: begin
              col := frmAntic4Tiles.fldCharAntic45Ex[tiles, i, x + 1, y];
              Inc(sum, col);
              case col of
                0: begin
                  fldChar[i, x, y] := 0;
                  fldChar[i, x + 1, y] := 0;
                end;
                1: begin
                  fldChar[i, x, y] := 0;
                  fldChar[i, x + 1, y] := 1;
                end;
                2: begin
                  fldChar[i, x, y] := 1;
                  fldChar[i, x + 1, y] := 0;
                end;
                3, 10: begin
                  fldChar[i, x, y] := 1;
                  fldChar[i, x + 1, y] := 1;
                end;
              end;
            end;
          end;

    if not isOnlyTile and (sum = 0) then continue;

    // Comments
    case langIndex of
      0, 1:
        codeLine += IntToStr(code.number) + ' REM';
      _ACTION:
        codeLine += '  ;';
      _FAST_BASIC:
        codeLine += '''';
      _MAD_PASCAL:
        codeLine += '  //';
      _KICKC, _CC65:
        codeLine += '//';
      _MADS:
        codeLine += ';';
    end;

    codeLine += ' TILE ' + IntToStr(tiles) + ' (' +
                 IntToStr(frmAntic4Tiles.antic4TileArray[tiles].dimX) + 'x' +
                 IntToStr(frmAntic4Tiles.antic4TileArray[tiles].dimY) + ')'#13#10;

    // Declaration
    maxTiles02 := frmAntic4Tiles.antic4TileArray[tiles].dimX *
                frmAntic4Tiles.antic4TileArray[tiles].dimY shl 3 - 1;
//    maxTiles := frmAntic4Tiles.antic4TileArray[tiles].dimX *
//                frmAntic4Tiles.antic4TileArray[tiles].dimY - 1;
    case langIndex of
      _ACTION:
        codeLine += '  tile' + IntToStr(tiles) + ' = [';
      _MAD_PASCAL:
        codeLine += '  tile' + IntToStr(tiles) + ' : array[0..' + IntToStr(maxTiles02) +
                     '] of byte = ('#13#10;
      _FAST_BASIC:
         codeLine += 'DATA tile' + IntToStr(tiles) + '() BYTE = ';
      _KICKC:
        codeLine += 'const unsigned char tile' + IntToStr(tiles) +
                     '[] = {'#13#10;
      _CC65:
        codeLine += 'const unsigned char tile' + IntToStr(tiles) +
                     '[' + IntToStr(maxTiles02) + '] = {'#13#10;
      _MADS:
         codeLine += 'TILE' + IntToStr(tiles);
    end;

    // Data values
//    index := 0;
    for y := 0 to frmAntic4Tiles.antic4TileArray[tiles].dimY - 1 do begin
      for x := 0 to frmAntic4Tiles.antic4TileArray[tiles].dimX - 1 do begin
        case langIndex of
          0, 1: begin
            Inc(code.number, code.step);
            codeLine += IntToStr(code.number) + ' DATA ';
          end;
          _FAST_BASIC: begin
            if x + y > 0 then
              codeLine += 'DATA BYTE = ';
          end;
          _MADS: begin
            codeLine += ' .BYTE ';
          end;
        end;

        index := x + y shl 2;
//        index := x + y*(frmAntic4Tiles.antic4TileArray[tiles].dimY - 1);

//        DEBUG('index', index);
        for i := 0 to _CHAR_DIM do begin
          bin := '';
          for j := 0 to _CHAR_DIM do
            bin += IntToStr(fldChar[index, j, i]);

          case radDataType.ItemIndex of
            0: line := IntToStr(Bin2Dec(bin));
            1: line := '$' + Dec2Hex(bin2Dec(bin));
            2: line := '%' + bin;
          end;

          codeLine += line;
          if i < _CHAR_DIM then begin
            if langIndex = _ACTION then
              codeLine += ' '
            else begin
              codeLine += ',';
              if langIndex > 1 then
                codeLine += ' ';
            end;
          end;
        end;

        if (langIndex <> _MADS) and (langIndex > 1) then begin
          if (x = frmAntic4Tiles.antic4TileArray[tiles].dimX - 1) and
             (y = frmAntic4Tiles.antic4TileArray[tiles].dimY - 1) then
          begin
            if langIndex = _ACTION then
              codeLine += ']'#13#10;
          end
          else
//            codeLine += '(' + inttostr(maxTiles) + ') x, (' + inttostr(index) + ')';
            codeLine += ', ';
        end;

        codeLine += #13#10;
//        Inc(index);
      end;
    end;

    case langIndex of
      //_ACTION:
      //   codeLine += ']'#13#10#13#10;
      _MAD_PASCAL:
         codeLine += ');'#13#10#13#10;
      _KICKC, _CC65:
         codeLine += '};'#13#10#13#10;
    end;

    if (tiles < maxTile) and (langIndex < 2) then
      Inc(code.number, code.step);
  end;

  result := codeLine;
end;

function TfrmAntic4TilesGen.SetTileCharDataValues(isOnlyTile : boolean) : string;
var
  x, y : byte;
  line : string;
  index : byte;
  bin : string;
  tiles, minTile, maxTile : byte;
  maxTiles : byte;
  codeLine : string = '';
  sum : word;
  i : byte;
  col : byte;
begin
  if isOnlyTile then begin
    minTile := frmAntic4Tiles.antic4Tile.selected;
    maxTile := minTile;
  end
  else begin
    minTile := 1;
    maxTile := _MAX_TILES;
  end;

  case langIndex of
    _ACTION:
      codeLine := 'BYTE ARRAY'#13#10;
    _MAD_PASCAL:
      codeLine := 'const'#13#10;
  end;

  for tiles := minTile to maxTile do begin
    sum := 0;

    // Map Antic 4 mode character data to Antic 2 mode character data
    for i := 0 to _MAX_TILE_CHARS - 1 do
      for y := 0 to _CHAR_DIM do
        for x := 0 to _CHAR_DIM do
          case x of
            0, 2, 4, 6: begin
//              col := frmAntic4Tiles.fldCharAntic45Ex[tiles, i, x, y];
//              Inc(sum, col);
              col := frmAntic4Tiles.fldCharAntic45Ex[tiles, i, x + 1, y];
              Inc(sum, col);
            end;
          end;

    if not isOnlyTile and (sum = 0) then continue;

    case langIndex of
      0, 1:
        codeLine += IntToStr(code.number) + ' REM';
      _ACTION:
        codeLine += '  ;';
      _FAST_BASIC:
        codeLine += '''';
      _MAD_PASCAL, _KICKC, _CC65:
        codeLine += '  //';
      _MADS:
        codeLine += ';';
    end;

    // Comments
    codeLine += ' TILE ' + IntToStr(tiles) + ' CHARACTER INTERNAL CODE MAP (' +
                 IntToStr(frmAntic4Tiles.antic4TileArray[tiles].dimX) + 'x' +
                 IntToStr(frmAntic4Tiles.antic4TileArray[tiles].dimY) + ')'#13#10;

    // Declaration
    maxTiles := frmAntic4Tiles.antic4TileArray[tiles].dimX *
                frmAntic4Tiles.antic4TileArray[tiles].dimY - 1;
    case langIndex of
      _ACTION:
        codeLine += '  tile' + IntToStr(tiles) + 'CharMap = [';
      _MAD_PASCAL:
        codeLine += '  tile' + IntToStr(tiles) + 'CharMap : array[0..' + IntToStr(maxTiles) +
                     '] of byte = ('#13#10;
      _FAST_BASIC:
         codeLine += 'DATA tile' + IntToStr(tiles) + 'CharMap() BYTE = ';
      _KICKC:
        codeLine += 'const unsigned char tile' + IntToStr(tiles) + 'CharMap' +
                     '[] = {'#13#10;
      _CC65:
        codeLine += 'const unsigned char tile' + IntToStr(tiles) + 'CharMap' +
                     '[' + IntToStr(maxTiles) + '] = {'#13#10;
      _MADS:
         codeLine += 'TILE' + IntToStr(tiles);
    end;

//    for index := 0 to _MAX_TILE_CHARS - 1 do
//      debug('index, value', index, frmAntic4Tiles.antic4TileArray[tiles].charValues[index]);

    // Character internal code values
    index := 0;
    for y := 0 to frmAntic4Tiles.antic4TileArray[tiles].dimY - 1 do begin
      case langIndex of
        0, 1: begin
          Inc(code.number, code.step);
          codeLine += IntToStr(code.number) + ' DATA ';
        end;
        _FAST_BASIC: begin
          if y > 0 then
            codeLine += 'DATA BYTE = ';
        end;
        _MADS: begin
          codeLine += ' .BYTE ';
        end;
      end;
      for x := 0 to frmAntic4Tiles.antic4TileArray[tiles].dimX - 1 do begin
//        index := x + y shl 2;
//        index := x + y*frmAntic4Tiles.antic4TileArray[tiles].dimX;

        //if y < 2 then begin

//        index := x + y shl 2;

        //end
        //else begin
        //  if y = 2 then begin
        //    case x of
        //      0: index := 9;
        //      1: index := 10;
        //      2: index := 11;
        //    end;
        //  end;
        //end;

//        if frmAntic4Tiles.antic4TileArray[tiles].charValues[index] = 0 then begin
//          frmAntic4Tiles.antic4TileArray[tiles].charValues[index] := 33;
        //  debug('index', index);
//        end;

//        debug('index, value', index, frmAntic4Tiles.antic4TileArray[tiles].charValues[index]);

        if radDataType.ItemIndex = 0 then
          line := IntToStr(frmAntic4Tiles.antic4TileArray[tiles].charValues[index])
        else begin
          bin := IntToBin(frmAntic4Tiles.antic4TileArray[tiles].charValues[index], 8);
          case radDataType.ItemIndex of
            1: line := '$' + Dec2Hex(bin2Dec(bin));
            2: line := '%' + bin;
          end;
        end;

        codeLine += line;

        if (langIndex = _MADS) and (x = frmAntic4Tiles.antic4TileArray[tiles].dimX - 1) then begin
        end
        else if (x = frmAntic4Tiles.antic4TileArray[tiles].dimX - 1) and
           (y = frmAntic4Tiles.antic4TileArray[tiles].dimY - 1) then
        begin
          if langIndex = _ACTION then
            codeLine += ']'#13#10;
        end
        else begin
          if langIndex = _ACTION then
            codeLine += ' '
          else begin
            if x < frmAntic4Tiles.antic4TileArray[tiles].dimX - 1 then begin
              codeLine += ',';
              if langIndex > 1 then
                codeLine += ' ';
            end
            else if langIndex > 1 then
              codeLine += ',';
          end;
        end;
        Inc(index);
      end;
      codeLine += #13#10;
    end;

    case langIndex of
      //_ACTION:
      //   codeLine += ']'#13#10#13#10;
      _MAD_PASCAL:
         codeLine += ');'#13#10#13#10;
      _KICKC, _CC65:
         codeLine += '};'#13#10#13#10;
    end;

    if (tiles < maxTiles) and (langIndex < 2) then
      Inc(code.number, code.step);
  end;

  result := codeLine;
end;

function TfrmAntic4TilesGen.Example01 : string;
begin
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
  end;

  code.line := SetTileDataValues(true);
  result := code.line;
end;

function TfrmAntic4TilesGen.Example02 : string;
begin
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
  end;

  code.line := SetTileDataValues(false);
  result := code.line;
end;

function TfrmAntic4TilesGen.Example03 : string;
begin
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
  end;

  code.line := SetTileCharDataValues(true);
  code.line += SetTileDataValues(true);
  result := code.line;
end;

function TfrmAntic4TilesGen.Example04 : string;
begin
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
  end;

  code.line := SetTileCharDataValues(false);

//  if langIndex < 2 then
//    Inc(code.number, code.step);

  code.line += SetTileDataValues(false);
  result := code.line;
end;

function TfrmAntic4TilesGen.Example05 : string;
var
  maxTileChars : byte;
  i, index : byte;
  x, y : byte;
  tileSelected : byte;
  charx : byte;
  inverseIndex : byte;
  strTextWindow : string = '';
begin
  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  code.line := '';
  tileSelected := frmAntic4Tiles.antic4Tile.selected;

  maxTileChars := frmAntic4Tiles.antic4TileArray[tileSelected].dimX *
                  frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1;

  { Atari BASIC / Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line += CodeLine('REM ******************************');
    code.line += CodeLine('REM MODIFIED CHARACTERS');
    code.line += CodeLine('REM ******************************');
    code.line += CodeLine('DIM TILE1CHARMAP(' + IntToStr(maxTileChars) + ')');
    code.line += CodeLine('NMEMTOP=PEEK(106)-4');
    code.line += CodeLine('POKE 106,NMEMTOP');
    code.line += CodeLine('GRAPHICS 12' + strTextWindow);
    code.line += CodeLine('POKE 82,0');

    if chkTextWindow.Checked then
      code.line += CodeLine('? "COPY ATARI CHARACTER SET TO RAM AREA"');

    code.line += CodeLine('CHRAM=NMEMTOP*256');
    code.line += CodeLine('FOR I=0 TO 1023:POKE CHRAM+I,PEEK(57344+I):NEXT I');

    //if chkTextWindow.Checked then
    //  code.line += CodeLine('? "DONE!"');

    if chkTextWindow.Checked then
      code.line += CodeLine('? "STORE TILE CHARACTERS"');

    code.line += CodeLine('FOR I=0 TO ' + IntToStr(maxTileChars) +
                          ':READ CHAR:TILE1CHARMAP(I)=CHAR:NEXT I');

    if chkTextWindow.Checked then
      code.line += CodeLine('? "MODIFY CHARACTER DATA FOR THE TILE"');

    code.line += CodeLine('FOR I=0 TO ' + IntToStr(maxTileChars));
    code.line += CodeLine('FOR J=0 TO 7:READ D:POKE CHRAM+J+TILE1CHARMAP(I)*8,D:NEXT J');
    code.line += CodeLine('NEXT I');

    //if chkTextWindow.Checked then
    //  code.line += CodeLine('? "DONE!"');

    code.line += CodeLine('REM MODIFY CHARACTER SET POINTER');
    code.line += CodeLine('POKE 756,NMEMTOP');

    index := 0;
    for y := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1 do begin
      Inc(code.number, code.step);
      code.line += IntToStr(code.number) + ' POS.' + editTilePosX.Text + ',' +
                   IntToStr(editTilePosY.Value + y) + ':? #6;';
      for x := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimX - 1 do begin
        inverseIndex := x + y shl 2;

        charx := StrToInt(AtasciiCode(
                   frmAntic4Tiles.antic4TileArray[tileSelected].charValues[index]));
        if frmAntic4Tiles.antic4TileArray[tileSelected].charInverse[inverseIndex] then
          Inc(charx, 128);

        code.line += 'CHR$(' + IntToStr(charx) + ')';
        if x < frmAntic4Tiles.antic4TileArray[tileSelected].dimX - 1 then
          code.line += ';';

        Inc(index);
      end;
      code.line += #13#10;
    end;

    if chkUseColors.Checked then begin
      code.line += GenSetColors(_ATARI_BASIC);
    //  Inc(code.number, code.step);
    //  code.line += CodeLine('REM SET COLORS') +
    //               CodeLine('POKE 708,' + IntToStr(colorValues[1]) +
    //               ':POKE 709,' + IntToStr(colorValues[2])) +
    //               CodeLine('POKE 710,' + IntToStr(colorValues[3]) +
    //               ':POKE 711,' + IntToStr(colorValues[10])) +
    //               CodeLine('POKE 712,' + IntToStr(colorValues[0]));
    end;

    if not chkTextWindow.Checked then begin
      Inc(code.number, code.step);
      code.line += WaitKeyCode(langIndex);
    end;

    Inc(code.number, code.step);
    code.line += SetTileCharDataValues(true);
    code.line += SetTileDataValues(true);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := '';
(*
    code.line := '; Modified characters'#13#10#13#10 +
                 'BYTE CH=$2FC'#13#10 +
                 'BYTE RAMTOP=$6A'#13#10 +
                 'BYTE CHBAS=$2F4'#13#10#13#10 +
                 'CARD TOPMEM'#13#10#13#10;

    code.line += SetTileCharDataValues(true);
    code.line += SetTileDataValues(true);
    code.line += 'BYTE _CHAR_OFFSET = [8]'#13#10;

//    for i := 0 to 127 do
//      if frmFonts.charEditIndex[i] = 1 then begin
//        code.line += '; Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
//                     'BYTE ARRAY char' + IntToStr(i) + '=[' +  //#13#10'  ' +
//                     SetDataValues(frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ' ') +
//                     ']'#13#10;
////        SetValues(_ACTION, i)
//      end;

    code.line += #13#10'PROC MAIN()'#13#10#13#10 +
                 'GRAPHICS(12)'#13#10#13#10 +
                 '; RESERVE MEMORY FOR NEW CHARACTER SET'#13#10 +
                 'TOPMEM=RAMTOP-8'#13#10 +
                 'TOPMEM==*256'#13#10#13#10 +
                 '; VECTOR TO PAGE ADDRESS OF NEW SET'#13#10 +
                 'CHBAS=TOPMEM/256'#13#10#13#10 +
                 '; MOVE NEW CHARACTER SET TO RESERVED MEMORY'#13#10 +
                 'MOVEBLOCK(TOPMEM,57344,1024)'#13#10#13#10 +
                 '; CUSTOM CHARACTER SET DATA'#13#10;
//    for i := 0 to 127 do
//      if frmFonts.charEditIndex[i] = 1 then
//        code.line += 'MOVEBLOCK(TOPMEM+' + IntToStr(i) + '*8,char' + IntToStr(i) + ',8)' + #13#10;

for i := 0 to maxTileChars do
  //code.line += '  MOVEBLOCK(tile' + IntToStr(tileSelected) + ' + _CHAR_OFFSET*' + IntToStr(i) + ', ' +
  //             'TOPMEM + tile' + IntToStr(tileSelected) +
  //             'CharMap(' + IntToStr(i) + ')*8), 8)'#13#10;
  code.line += 'MOVEBLOCK(TOPMEM+tile' + IntToStr(tileSelected) + 'CharMap(' + IntToStr(i) +
               ')*8,tile' + IntToStr(tileSelected) + ' + _CHAR_OFFSET*' + IntToStr(i) + ',8)' + #13#10;

//    code.line += 's := ''ABCD''';
//    code.line += 'GotoXY(2, 2); BPut(6, @s[1], 2);';
(*
code.line += #13#10'  s = '''''#13#10;

for y := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1 do
  for x := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimX - 1 do begin
    index := x + y shl 2;
    code.line += '  s = Concat(s, Chr(' + AtasciiCode(frmAntic4Tiles.antic4TileArray[tileSelected].charValues[index]) + '))'#13#10;
  end;

code.line += #13#10;
for y := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1 do begin
  code.line += '  POSITION (2, ' + IntToStr(2 + y) + '); Put(6, @s[' +
               IntToStr(1 + frmAntic4Tiles.antic4TileArray[tileSelected].dimX*y) + '], ' +
               IntToStr(frmAntic4Tiles.antic4TileArray[tileSelected].dimX) + ')'#13#10;
end;
*)
    code.line += 'POSITION(2,2)';
    code.line += 'PRINTD(6,"!#$")';
    code.line += #13#10'PRINTF("%E%E%EPRESS ANY KEY TO EXIT!%E")'#13#10 +
                 WaitKeyCode(_ACTION) + #13#10 +
                 'RETURN';

   if chkUseColors.Checked then
     code.line += #13#10 +
                  'POKE(708,' + IntToStr(colorValues[1]) + ') POKE(709,' +
                  IntToStr(colorValues[2]) + ')'#13#10 +
                  'POKE(710,' + IntToStr(colorValues[3]) + ') POKE(711,' +
                  IntToStr(colorValues[10]) + ')'#13#10 +
                  'POKE(712,' + IntToStr(colorValues[0]) + ')'#13#10;
                 *)
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := 'uses graph, crt, cio;'#13#10#13#10;
    code.line += SetTileCharDataValues(true);
    code.line += SetTileDataValues(true);
    code.line += '  _CHAR_OFFSET = 8;'#13#10#13#10 +
                 'var'#13#10 +
                 '  CHBAS  : byte absolute $2F4;'#13#10 +
                 '  RamSet: word;'#13#10 +
                 '  MEMTOP: word absolute $2E5;'#13#10 +
                 '  Charset : array [0..0] of byte;'#13#10 +
                 '  s: string;'#13#10 +
                 'begin'#13#10 +
                 '  InitGraph(12' + strTextWindow + ');'#13#10 +
                 '  RamSet := (MEMTOP - $400) and $FC00;'#13#10 +
                 '  chbas := Hi(RamSet);'#13#10#13#10 +
                 '  MEMTOP := RamSet;'#13#10#13#10 +
                 '  // Move new character set to reserved memory'#13#10 +
                 '  Move(pointer($e000), pointer(RamSet), 1024);'#13#10 +
                 '  Charset := pointer(RamSet);'#13#10#13#10;

    code.line += '  // Custom character set data'#13#10;
    for i := 0 to maxTileChars do
      code.line += '  Move(tile' + IntToStr(tileSelected) + '[_CHAR_OFFSET*' + IntToStr(i) + '], ' +
                   'pointer(word(@Charset) + tile' + IntToStr(tileSelected) +
                   'CharMap[' + IntToStr(i) + ']*8), 8);'#13#10;

//    code.line += 's := ''ABCD''';
//    code.line += 'GotoXY(2, 2); BPut(6, @s[1], 2);';
//    code.line += 'GotoXY(2, 3); BPut(6, @s[3], 2);';

    code.line += #13#10'  s := '''';'#13#10;
    index := 0;
    for y := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1 do begin
      for x := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimX - 1 do begin
        inverseIndex := x + y shl 2;

        charx := StrToInt(AtasciiCode(
                   frmAntic4Tiles.antic4TileArray[tileSelected].charValues[index]));
        if frmAntic4Tiles.antic4TileArray[tileSelected].charInverse[inverseIndex] then
          Inc(charx, 128);

        code.line += '  s := Concat(s, Chr(' + IntToStr(charx) + '));'#13#10;
        Inc(index);
      end;
    end;

    code.line += #13#10;
    for y := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1 do begin
      code.line += '  GotoXY(' + IntToStr(editTilePosX.Value + 1) + ', ' +
                   IntToStr(editTilePosY.Value + y + 1) + '); BPut(6, @s[' +
                   IntToStr(1 + frmAntic4Tiles.antic4TileArray[tileSelected].dimX*y) + '], ' +
                   IntToStr(frmAntic4Tiles.antic4TileArray[tileSelected].dimX) + ');'#13#10;
    end;

    if chkUseColors.Checked then
      code.line += GenSetColors(_MAD_PASCAL);
      //code.line += #13#10'  // Set colors'#13#10 +
      //             '  POKE(712, ' + IntToStr(colorValues[0]) + ');'#13#10 +
      //             '  POKE(708, ' + IntToStr(colorValues[1]) + ');'#13#10 +
      //             '  POKE(709, ' + IntToStr(colorValues[2]) + ');'#13#10 +
      //             '  POKE(710, ' + IntToStr(colorValues[3]) + ');'#13#10 +
      //             '  POKE(711, ' + IntToStr(colorValues[10]) + ');'#13#10;

    if chkTextWindow.Checked then
      code.line += '  Write(''Press any key to exit!'');';

    code.line += WaitKeyCode(langIndex);
    code.line += 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := SetTileCharDataValues(true);
    code.line += SetTileDataValues(true);
    code.line += #13#10'NMEMTOP = PEEK(106) - 4'#13#10 +
                 'POKE 106, NMEMTOP'#13#10 +
                 'GRAPHICS 12' + strTextWindow + #13#10 +
                 'CHRAM = NMEMTOP*256'#13#10 +
                 'MOVE 57344, CHRAM, 1024'#13#10#13#10 +
                 'CHR_OFFSET = 8'#13#10#13#10;

    code.line += ''' Modify characters for the tile'#13#10;

    for i := 0 to maxTileChars do
      code.line += 'MOVE ADR(tile' + IntToStr(tileSelected) +
                   ') + CHR_OFFSET*' + IntToStr(i) + ', CHRAM + tile' + IntToStr(tileSelected) +
                   'CharMap(' + IntToStr(i) + ')*8, 8'#13#10;
    //MOVE ADR(tile1), CHRAM + tile1char(0)*8, 8
    //MOVE ADR(tile1) + CHR_OFFSET, CHRAM + tile1char(1)*8, 8
    //MOVE ADR(tile1) + CHR_OFFSET*2, CHRAM + tile1char(2)*8, 8

    code.line += #13#10''' Modify character set pointer'#13#10;
    code.line += 'POKE 756, NMEMTOP'#13#10;

//    POS.2,2 : ? #6,"AB"
// POS.2, 2 : ? #6, CHR$(65); CHR$(66)
    index := 0;
    for y := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1 do begin
      code.line += 'POS.' + editTilePosX.Text + ', ' + IntToStr(editTilePosY.Value + y) +
                   ' : ? #6, ';  //""'#13#10;
      for x := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimX - 1 do begin
        inverseIndex := x + y shl 2;

        charx := frmAntic4Tiles.antic4TileArray[tileSelected].charValues[index];
        if frmAntic4Tiles.antic4TileArray[tileSelected].charInverse[index] then
          Inc(charx, 128);

        charx := StrToInt(AtasciiCode(
                   frmAntic4Tiles.antic4TileArray[tileSelected].charValues[index]));
        if frmAntic4Tiles.antic4TileArray[tileSelected].charInverse[inverseIndex] then
          Inc(charx, 128);

        code.line += 'CHR$(' + IntToStr(charx) + ')';
        if x < frmAntic4Tiles.antic4TileArray[tileSelected].dimX - 1 then
          code.line += '; ';

        Inc(index);
      end;

      code.line += #13#10;
    end;

    if chkUseColors.Checked then
      code.line += GenSetColors(_FAST_BASIC);
      //code.line += #13#10'''Set colors'#13#10 +
      //             'POKE 708, ' + IntToStr(colorValues[1]) +
      //             ' : POKE 709, ' + IntToStr(colorValues[2]) + #13#10 +
      //             'POKE 710, ' + IntToStr(colorValues[3]) +
      //             ' : POKE 711, ' + IntToStr(colorValues[10]) + #13#10 +
      //             'POKE 712, ' + IntToStr(colorValues[0]) + #13#10;

    if chkTextWindow.Checked then
      code.line += '? "Press any key to exit!"';

    code.line += WaitKeyCode(langIndex);
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
  end;

  result := code.line;
end;

function TfrmAntic4TilesGen.Example06 : string;
var
  maxTileChars : byte;
  i, j, index : byte;
  x, y : byte;
  tileSelected : byte;
  charx : byte;
  scr : word;
  inverseIndex : byte;
  strTextWindow : string = '';
begin
  code.line := '';

  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC / Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line += CodeLine('REM ******************************');
    code.line += CodeLine('REM SCREEN WITH PREDEFINED TILES');
    code.line += CodeLine('REM ******************************');

    for j := 1 to _MAX_TILES do begin
      maxTileChars := frmAntic4Tiles.antic4TileArray[j].dimX *
                      frmAntic4Tiles.antic4TileArray[j].dimY - 1;
      code.line += CodeLine('DIM TILE' + IntToStr(j) + 'CHARMAP(' + IntToStr(maxTileChars) + ')');
    end;

    code.line += CodeLine('NMEMTOP=PEEK(106)-4');
    code.line += CodeLine('POKE 106,NMEMTOP');
    code.line += CodeLine('GRAPHICS 12' + strTextWindow);
    code.line += CodeLine('POKE 82,0');

    if chkTextWindow.Checked then
      code.line += CodeLine('? "COPY ATARI CHARACTER SET TO RAM AREA"');

    code.line += CodeLine('CHRAM=NMEMTOP*256');
    code.line += CodeLine('FOR I=0 TO 1023:POKE CHRAM+I,PEEK(57344+I):NEXT I');

    //if chkTextWindow.Checked then
    //  code.line += CodeLine('? "DONE!"');

    for j := 1 to _MAX_TILES do begin
      index := 0;
      for i := 0 to _MAX_TILE_CHARS - 1 do begin
        for y := 0 to _CHAR_DIM do
          for x := 0 to _CHAR_DIM do
            case x of
              0, 2, 4, 6:
                Inc(index, frmAntic4Tiles.fldCharAntic45Ex[j, i, x + 1, y]);
            end;
      end;
      if index > 0 then begin
        maxTileChars := frmAntic4Tiles.antic4TileArray[j].dimX *
                        frmAntic4Tiles.antic4TileArray[j].dimY - 1;

        if chkTextWindow.Checked then
          code.line += CodeLine('? "STORE TILE ' + IntToStr(j) + ' CHARACTERS"');

        code.line += CodeLine('FOR I=0 TO ' + IntToStr(maxTileChars) +
                              ':READ CHAR:TILE' + IntToStr(j) + 'CHARMAP(I)=CHAR:NEXT I');
      end;
    end;

    for j := 1 to _MAX_TILES do begin
      index := 0;
      for i := 0 to _MAX_TILE_CHARS - 1 do begin
        for y := 0 to _CHAR_DIM do
          for x := 0 to _CHAR_DIM do
            case x of
              0, 2, 4, 6:
                Inc(index, frmAntic4Tiles.fldCharAntic45Ex[j, i, x + 1, y]);
            end;
      end;
      if index > 0 then begin
        maxTileChars := frmAntic4Tiles.antic4TileArray[j].dimX *
                        frmAntic4Tiles.antic4TileArray[j].dimY - 1;

        if chkTextWindow.Checked then
          code.line += CodeLine('? "MODIFY CHARACTERS FOR TILE' + IntToStr(j) + '"');

        code.line += CodeLine('FOR I=0 TO ' + IntToStr(maxTileChars));
        code.line += CodeLine('FOR J=0 TO 7:READ D:POKE CHRAM+J+TILE' + IntToStr(j) +
                     'CHARMAP(I)*8,D:NEXT J');
        code.line += CodeLine('NEXT I');
        //if chkTextWindow.Checked then
        //  code.line += CodeLine('? "DONE!"');
      end;
    end;

    code.line += CodeLine('REM MODIFY CHARACTER SET POINTER');
    code.line += CodeLine('POKE 756,NMEMTOP');

    for scr := 0 to _ANTIC_MODE_4_SIZE - 1 do
      if frmAntic4Tiles.tiles[scr].tileIndex > 0 then begin
        tileSelected := frmAntic4Tiles.tiles[scr].tileIndex;
        maxTileChars := frmAntic4Tiles.antic4TileArray[tileSelected].dimX *
                        frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1;
        index := 0;
        for y := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1 do begin
          Inc(code.number, code.step);
          code.line += IntToStr(code.number) + ' POS.' + IntToStr(frmAntic4Tiles.tiles[scr].x) + ', ' +
                       IntToStr(frmAntic4Tiles.tiles[scr].y + y) + ':? #6;';
          for x := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimX - 1 do begin
            inverseIndex := x + y shl 2;
            charx := StrToInt(AtasciiCode(
                       frmAntic4Tiles.antic4TileArray[tileSelected].charValues[index]));
            if frmAntic4Tiles.antic4TileArray[tileSelected].charInverse[inverseIndex] then
              Inc(charx, 128);

            code.line += 'CHR$(' + IntToStr(charx) + ')';
            if x < frmAntic4Tiles.antic4TileArray[tileSelected].dimX - 1 then
              code.line += ';';

            Inc(index);
          end;

          code.line += #13#10;
        end;
      end;

    if chkUseColors.Checked then begin
      code.line += GenSetColors(_ATARI_BASIC);
      //Inc(code.number, code.step);
      //code.line += CodeLine('POKE 708,' + IntToStr(colorValues[1]) +
      //             ':POKE 709,' + IntToStr(colorValues[2])) +
      //             CodeLine('POKE 710,' + IntToStr(colorValues[3]) +
      //             ':POKE 711,' + IntToStr(colorValues[10])) +
      //             CodeLine('POKE 712,' + IntToStr(colorValues[0]));
    end;

    if not chkTextWindow.Checked then begin
      Inc(code.number, code.step);
      code.line += WaitKeyCode(langIndex);
    end;

    Inc(code.number, code.step);
    code.line += SetTileCharDataValues(false);
    code.line += SetTileDataValues(false);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := '';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := 'uses graph, crt, cio;'#13#10#13#10;
    code.line += SetTileCharDataValues(false);
    code.line += SetTileDataValues(false);
    code.line += '  _CHAR_OFFSET = 8;'#13#10#13#10 +
                 'var'#13#10 +
                 '  CHBAS  : byte absolute $2F4;'#13#10 +
                 '  RamSet: word;'#13#10 +
                 '  MEMTOP: word absolute $2E5;'#13#10 +
                 '  Charset : array [0..0] of byte;'#13#10 +
                 '  s: string;'#13#10 +
                 'begin'#13#10 +
                 '  InitGraph(12' + strTextWindow + ');'#13#10 +
                 '  RamSet := (MEMTOP - $400) and $FC00;'#13#10 +
                 '  chbas := Hi(RamSet);'#13#10#13#10 +
                 '  MEMTOP := RamSet;'#13#10#13#10 +
                 '  // Move new character set to reserved memory'#13#10 +
                 '  Move(pointer($e000), pointer(RamSet), 1024);'#13#10 +
                 '  Charset := pointer(RamSet);'#13#10#13#10;

    code.line += '  // Modify characters for the tiles'#13#10;
    //for i := 0 to maxTileChars do
    //  code.line += '  Move(tile' + IntToStr(tileSelected) + '[_CHAR_OFFSET*' + IntToStr(i) + '], ' +
    //               'pointer(word(@Charset) + tile' + IntToStr(tileSelected) +
    //               'CharMap[' + IntToStr(i) + ']*8), 8);'#13#10;

    for j := 1 to _MAX_TILES do begin
      index := 0;
      for i := 0 to _MAX_TILE_CHARS - 1 do begin
        for y := 0 to _CHAR_DIM do
          for x := 0 to _CHAR_DIM do
            case x of
              0, 2, 4, 6:
                Inc(index, frmAntic4Tiles.fldCharAntic45Ex[j, i, x + 1, y]);
            end;
      end;

      if index > 0 then begin
        maxTileChars := frmAntic4Tiles.antic4TileArray[j].dimX *
                        frmAntic4Tiles.antic4TileArray[j].dimY - 1;
        for i := 0 to maxTileChars do
          code.line += '  Move(tile' + IntToStr(j) + '[_CHAR_OFFSET*' + IntToStr(i) + '], ' +
                       'pointer(word(@Charset) + tile' + IntToStr(j) +
                       'CharMap[' + IntToStr(i) + ']*8), 8);'#13#10;
      end;
    end;


//    code.line += 's := ''ABCD''';
//    code.line += 'GotoXY(2, 2); BPut(6, @s[1], 2);';
//    code.line += 'GotoXY(2, 3); BPut(6, @s[3], 2);';

    for scr := 0 to _ANTIC_MODE_4_SIZE - 1 do begin
      if frmAntic4Tiles.tiles[scr].tileIndex > 0 then begin
        tileSelected := frmAntic4Tiles.tiles[scr].tileIndex;
        maxTileChars := frmAntic4Tiles.antic4TileArray[tileSelected].dimX *
                        frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1;

        code.line += #13#10'  s := '''';'#13#10;

        index := 0;
        for y := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1 do
          for x := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimX - 1 do begin
            inverseIndex := x + y shl 2;

            charx := StrToInt(AtasciiCode(
                       frmAntic4Tiles.antic4TileArray[tileSelected].charValues[index]));
            if frmAntic4Tiles.antic4TileArray[tileSelected].charInverse[inverseIndex] then
              Inc(charx, 128);

            code.line += '  s := Concat(s, Chr(' + IntToStr(charx) + '));'#13#10;
            Inc(index);
          end;

        index := 0;
        for y := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1 do begin
          code.line += 'GotoXY(' + IntToStr(frmAntic4Tiles.tiles[scr].x + 1) + ', ' +
                       IntToStr(frmAntic4Tiles.tiles[scr].y + y + 1) + '); BPut(6, @s[' +
                       IntToStr(1 + frmAntic4Tiles.antic4TileArray[tileSelected].dimX*y) + '], ' +
                       IntToStr(frmAntic4Tiles.antic4TileArray[tileSelected].dimX) + ');'#13#10;
        end;
      end;
    end;
    if chkUseColors.Checked then
      code.line += GenSetColors(_MAD_PASCAL);
      //code.line += #13#10'  POKE(712, ' + IntToStr(colorValues[0]) + ');'#13#10 +
      //             '  POKE(708, ' + IntToStr(colorValues[1]) + ');'#13#10 +
      //             '  POKE(709, ' + IntToStr(colorValues[2]) + ');'#13#10 +
      //             '  POKE(710, ' + IntToStr(colorValues[3]) + ');'#13#10 +
      //             '  POKE(711, ' + IntToStr(colorValues[10]) + ');'#13#10;

    if chkTextWindow.Checked then
      code.line += 'Write("Press any key to exit!")';

    code.line += WaitKeyCode(langIndex);
    code.line += 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := SetTileCharDataValues(false);
    code.line += SetTileDataValues(false);
    code.line += #13#10'NMEMTOP = PEEK(106) - 4'#13#10 +
                 'POKE 106, NMEMTOP'#13#10 +
                 'GRAPHICS 12' + strTextWindow + #13#10 +
                 'CHRAM = NMEMTOP*256'#13#10 +
                 'MOVE 57344, CHRAM, 1024'#13#10#13#10 +
                 'CHR_OFFSET = 8'#13#10#13#10;

    code.line += ''' Modify characters for the tiles'#13#10;

    for j := 1 to _MAX_TILES do begin
      index := 0;
      for i := 0 to _MAX_TILE_CHARS - 1 do begin
        for y := 0 to _CHAR_DIM do
          for x := 0 to _CHAR_DIM do
            case x of
              0, 2, 4, 6:
                Inc(index, frmAntic4Tiles.fldCharAntic45Ex[j, i, x + 1, y]);
            end;
      end;

      if index > 0 then begin
        maxTileChars := frmAntic4Tiles.antic4TileArray[j].dimX *
                        frmAntic4Tiles.antic4TileArray[j].dimY - 1;
        for i := 0 to maxTileChars do
          code.line += 'MOVE ADR(tile' + IntToStr(j) +
                       ') + CHR_OFFSET*' + IntToStr(i) + ', CHRAM + tile' + IntToStr(j) +
                       'CharMap(' + IntToStr(i) + ')*8, 8'#13#10;
      end;
    end;

    code.line += #13#10''' Modify character set pointer'#13#10;
    code.line += 'POKE 756, NMEMTOP'#13#10;

    for scr := 0 to _ANTIC_MODE_4_SIZE - 1 do begin
      if frmAntic4Tiles.tiles[scr].tileIndex > 0 then begin
        tileSelected := frmAntic4Tiles.tiles[scr].tileIndex;
        maxTileChars := frmAntic4Tiles.antic4TileArray[tileSelected].dimX *
                        frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1;
        index := 0;
        for y := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimY - 1 do begin
          code.line += 'POS.' + IntToStr(frmAntic4Tiles.tiles[scr].x) + ', ' +
                       IntToStr(frmAntic4Tiles.tiles[scr].y + y) + ' : ? #6, ';
          for x := 0 to frmAntic4Tiles.antic4TileArray[tileSelected].dimX - 1 do begin
            inverseIndex := x + y shl 2;
            charx := StrToInt(AtasciiCode(
                       frmAntic4Tiles.antic4TileArray[tileSelected].charValues[index]));
            if frmAntic4Tiles.antic4TileArray[tileSelected].charInverse[inverseIndex] then
              Inc(charx, 128);

            code.line += 'CHR$(' + IntToStr(charx) + ')';
            if x < frmAntic4Tiles.antic4TileArray[tileSelected].dimX - 1 then
              code.line += '; ';

            Inc(index);
          end;

          code.line += #13#10;
        end;
      end;
    end;
    if chkUseColors.Checked then
      code.line += GenSetColors(_FAST_BASIC);
      //code.line += #13#10 +
      //             'POKE 708, ' + IntToStr(colorValues[1]) +
      //             ' : POKE 709, ' + IntToStr(colorValues[2]) + #13#10 +
      //             'POKE 710, ' + IntToStr(colorValues[3]) +
      //             ' : POKE 711, ' + IntToStr(colorValues[10]) + #13#10 +
      //             'POKE 712, ' + IntToStr(colorValues[0]) + #13#10;

    if chkTextWindow.Checked then
      code.line += '? "Press any key to exit!"';

    code.line += WaitKeyCode(langIndex);
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
  end;

  result := code.line;
end;

procedure TfrmAntic4TilesGen.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmAntic4TilesGen.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmAntic4TilesGen.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

procedure TfrmAntic4TilesGen.ExamplesProc(Sender : TObject);
begin
  if listExamples.ListBox.ItemIndex < 0 then exit;

//  debug(listExamples.ListBox.ItemIndex);
  //langIndex := (Sender as TBCMDButton).Tag;
  //(Sender as TBCMDButton).Enabled := listings[0, 2];

  btnAtariBASIC.Enabled := listings[listExamples.ListBox.ItemIndex, 0];
  btnTurboBasicXL.Enabled := listings[listExamples.ListBox.ItemIndex, 1];
  btnMadPascal.Enabled := listings[listExamples.ListBox.ItemIndex, 2];
  btnEffectus.Enabled := listings[listExamples.ListBox.ItemIndex, 3];
  btnFastBasic.Enabled := listings[listExamples.ListBox.ItemIndex, 4];
  btnKickC.Enabled := listings[listExamples.ListBox.ItemIndex, 5];
  btnMads.Enabled := listings[listExamples.ListBox.ItemIndex, 6];
  btnCC65.Enabled := listings[listExamples.ListBox.ItemIndex, 7];

  boxTilePos.Enabled := listExamples.ListBox.ItemIndex = 4;
  boxTilePos.Visible := boxTilePos.Enabled;

  boxColors.Enabled := listExamples.ListBox.ItemIndex >= 4;
  boxColors.Visible := boxColors.Enabled;

  chkTextWindow.Enabled := listExamples.ListBox.ItemIndex >= 4;
  chkTextWindow.Visible := chkTextWindow.Enabled;

  CreateCode;
end;

procedure TfrmAntic4TilesGen.CopyToEditorProc(Sender : TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor(Sender);
  Close;
end;

procedure TfrmAntic4TilesGen.LanguageProc(Sender : TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmAntic4TilesGen.CloseProc(Sender : TObject);
begin
  Close;
end;

end.

