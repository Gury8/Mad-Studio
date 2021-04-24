
function TfrmAntic4TilesGen.SetTileDataValues(isOnlyTile : boolean) : string;
var
  x, y : byte;
  line : string;
  i, j, index : byte;
  bin : string;
  tiles, minTile, maxTile : byte;
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
    0, 1:
      code.line := '';
    _ACTION:
      code.line := 'BYTE ARRAY'#13#10;
    _MAD_PASCAL:
      code.line := 'const'#13#10;
  end;
  for tiles := minTile to maxTile do begin
    case langIndex of
      0, 1:
        code.line += CodeLine('REM ***');
      _ACTION:
        code.line += '  ;';
      _MAD_PASCAL:
        code.line += '  //';
      _FAST_BASIC:
        code.line += '  ''';
      _KICKC:
        code.line += '  //';
    end;

    code.line += ' TILE ' + IntToStr(tiles) + ' (' + IntToStr(frmAntic4Tiles.antic4TileArray[tiles].dimX) + 'x' +
                 IntToStr(frmAntic4Tiles.antic4TileArray[tiles].dimY) + ')'#13#10;

    if minTile < maxTile then
      for i := 0 to 19 do
        for y := 0 to _CHAR_DIM do
          for x := 0 to _CHAR_DIM do
            case x of
              0, 2, 4, 6: begin
                col := frmAntic4Tiles.fldCharAntic45Ex[tiles, i, x + 1, y];
                frmAntic4Tiles.fldCharAntic45[i, x, y] := col;
                frmAntic4Tiles.fldCharAntic45[i, x + 1, y] := col;
                case col of
                  0: begin
                    frmAntic4Tiles.fldChar[i, x, y] := 0;
                    frmAntic4Tiles.fldChar[i, x + 1, y] := 0;
                  end;
                  1: begin
                    frmAntic4Tiles.fldChar[i, x, y] := 0;
                    frmAntic4Tiles.fldChar[i, x + 1, y] := 1;
                  end;
                  2: begin
                    frmAntic4Tiles.fldChar[i, x, y] := 1;
                    frmAntic4Tiles.fldChar[i, x + 1, y] := 0;
                  end;
                  3, 10: begin
                    frmAntic4Tiles.fldChar[i, x, y] := 1;
                    frmAntic4Tiles.fldChar[i, x + 1, y] := 1;
                  end;
                end;
              end;
            end;

    for y := 0 to frmAntic4Tiles.antic4TileArray[tiles].dimY - 1 do begin
      case langIndex of
        0, 1:
          code.line += CodeLine('REM ROW ' + IntToStr(y + 1));
        //_ACTION:
        //  code.line += '  ; TILE ' + IntToStr(tiles) + ' ROW ' + IntToStr(y + 1) + #13#10;
//                       '  tileChar
      end;

      for x := 0 to frmAntic4Tiles.antic4TileArray[tiles].dimX - 1 do begin
        case langIndex of
          0, 1:
            code.line += IntToStr(code.number) + ' DATA ';
          _ACTION:
            code.line += '  tileChar' + IntToStr(tiles) + '_' + IntToStr(x + 1) + '_' + IntToStr(y + 1) + ' = [';
          _MAD_PASCAL:
            code.line += '  tileChar' + IntToStr(tiles) + '_' + IntToStr(x + 1) + '_' + IntToStr(y + 1) +
                         ': array[0..7] of byte = (';
        end;

        index := x + y shl 2;
  //        DEBUG('index', index);
        for i := 0 to _CHAR_DIM do begin
          bin := '';
          for j := 0 to _CHAR_DIM do
            bin += IntToStr(frmAntic4Tiles.fldChar[index, j, i]);

          line := IntToStr(Bin2Dec(bin));
          case radDataType.ItemIndex of
            1: line := '$' + Dec2Hex(bin2Dec(line));
            2: line := '%' + line;
          end;

          code.line += line;
          if i < _CHAR_DIM then begin
            if langIndex = _ACTION then
              code.line += ' '
            else
              code.line += ',';
          end;
        end;

        if (x < frmAntic4Tiles.antic4TileArray[tiles].dimX - 1) and (langIndex <> _ACTION) then
          code.line += #13#10;

        case langIndex of
          0, 1:
            Inc(code.number, code.step);
          _ACTION:
             code.line += ']'#13#10;
        end;
      end;
      code.line += #13#10;
      Inc(code.number, code.step);
    end;
  end;

  result := code.line;
end;

-----

d:\atari\appl\mpnew\mp d:\atari\appl\mpnew\samples\games\pong\pong.pas
d:\Atari\Projects\mad_studio\bin\mp\mp d:\Atari\Projects\mad_studio\bin\mp\code59.pas

----------

//function TfrmAntic4TilesGen.Example04 : string;
//begin
//  code.line := IntToStr(langIndex);
//
//  { Atari BASIC / Turbo BASIC XL
//   ---------------------------------------------------------------------------}
//  if langIndex < 2 then begin
//  end
//  { Action!
//   ---------------------------------------------------------------------------}
//  else if langIndex = _ACTION then begin
//  end
//  { Mad Pascal
//   ---------------------------------------------------------------------------}
//  else if langIndex = _MAD_PASCAL then begin
//  end
//  { FastBasic
//   ---------------------------------------------------------------------------}
//  else if langIndex = _FAST_BASIC then begin
//  end
//  { KickC
//   ---------------------------------------------------------------------------}
//  else if langIndex = _KICKC then begin
//  end;
//
//  result := code.line;
//end;

--------------

(*
if inttostr(fldChar[index, 0, i]) +
            inttostr(fldChar[index, 1, i]) +
            inttostr(fldChar[index, 2, i]) +
            inttostr(fldChar[index, 3, i]) +
            inttostr(fldChar[index, 4, i]) +
            inttostr(fldChar[index, 5, i]) +
            inttostr(fldChar[index, 6, i]) +
            inttostr(fldChar[index, 7, i]) <> '00000000' then
showmessage(inttostr(fldChar[index, 0, i]) +
            inttostr(fldChar[index, 1, i]) +
            inttostr(fldChar[index, 2, i]) +
            inttostr(fldChar[index, 3, i]) +
            inttostr(fldChar[index, 4, i]) +
            inttostr(fldChar[index, 5, i]) +
            inttostr(fldChar[index, 6, i]) +
            inttostr(fldChar[index, 7, i]));
            *)
(*
            //  PaintBox.Canvas.Pen.Width:=2; // or any width, the settings will respect
            //  PaintBox.Canvas.Pen.Color:=clBlack;
            ////  PaintBox.Canvas.Pen.JoinStyle:=pjsMiter;// remove rounded borders
            ////  PaintBox.Canvas.Pen.Style:=psInsideframe; // draw border inside rectangle
            //  PaintBox.Canvas.Rectangle(0, 0, PaintBox.Width, PaintBox.Height);

            //PaintBox.Canvas.Brush.Style := bsClear;
            //PaintBox.Canvas.Pen.Color := clRed;
            //PaintBox.Canvas.Rectangle(PaintBox.ClientRect);
            //PaintBox.Invalidate;
*)

//constructor Icon.Create(X, Y: Integer);
//var Bitmap:TBitmap;
//begin
//  Bitmap:=TBitmap.Create;
//  try
//    Bitmap.Height := IconSize;
//    Bitmap.Width := IconSize;
//    Bitmap.Canvas.Pen.Color := clBlack;
//    Bitmap.Canvas.Rectangle(Round(X-(IconSize/2)), Round(Y-(IconSize/2)),
//      Round(X+(IconSize/2)), Round(Y+(IconSize/2)));
//    PaintBox.Canvas.Draw(0, 0, Bitmap);
//  finally
//    Bitmap.Free;
//  end;
//end;

-----------------

//If not FileExists(YourFileName) then
//  SynEdit1.lines.savetofile (YourFileName) else  //no dialog
//begin
//  SaveDialog1.Filename := YourFilename;
//  SaveDialog1.Options + [ofOverwritePrompt];  // shows the file exists and prompts the dialog
//  if Savedialog1.execute then
//  begin
//    SynEdit1.lines.savetofile (savedialog1.filename);
//    SynEdit1.setfocus;
//  end;
//end;

MOVE ADR(tile2), CHRAM + tile2CharMap(0)*8, 8*20
Move(tile2, pointer(word(@Charset) + tile2CharMap[0]*8), 8*20);

