{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Code listing custom library
}
unit code_lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dialogs, controls, graphics, StdCtrls, ExtCtrls, buttons, Windows,
  common;

type
  TCodeLine = record
    line : string;
    number : word;
    step: word;
  end;

const
  _REM = 'REM ******************************';
  _REM_MAD_STUDIO = ' Mad Studio example';
  _BASIC_SCR_MEM_VAR = 'SCR';
  _SCR_MEM_VAR       = 'screen';
  _SCR_MEM_DATA_VAR  = 'screenData';
  _DATA_SIZE         = 'size';

var
  code : TCodeLine;
  isConstData : boolean;

function GrDataValues(langIndex : byte; var lineNum : word; grBytes : smallint;
  numberFormat : byte; is01bit : boolean) : string;

function DataValues(langIndex : byte; const byteArray : array of byte; rows, charsPerLine : byte;
  numberFormat : byte) : string; overload;

function DataValues(langIndex : byte; const byteArray : array of byte; lineNum : word;
  lineStep : word; rows, charsPerLine : byte; numberFormat : byte) : string; overload;

function WaitKeyCode(langIndex : byte) : string;

function DecHex(dta, numberFormat : byte) : string; overload;

function SetDataValues(fldChar : charType; fld : fldFontSetType; charOffset, dataType : byte;
  separator : string) : string;

function CodeLine(line : string) : string;

function GenSetColors(langIndex : byte) : string;

function SetFastCopyRoutine : string;

function SetCIOFastLoad(varCharRAM : string; isCalcHiLo, isSetCIO : boolean) : string;

function DisplayScreen(langIndex : byte; maxX, maxY : byte; isFile : boolean; isSetCIO : boolean)
  : string; overload;

function DisplayScreen(langIndex : byte; maxX, maxY : byte; isFile : boolean) : string; overload;

function SetCharSet(langIndex : byte; fontName : string; isChBasNew, isSetCIO : boolean) : string; overload;

function SetCharSet(langIndex : byte; fontName : string; isChBasNew : boolean) : string; overload;

procedure SetListings(var listings : TListings);

procedure Set01(groupBox : TGroupBox; langIndex : byte; radDataType : TRadioGroup; isBinary : boolean);
procedure Set02(memo : TMemo; code : string);

implementation

uses
  Graph, lib;

function DecHex(dta, numberFormat : byte) : string;
begin
  if numberFormat = 0 then
    result := IntToStr(dta)
  else
    result := '$' + Dec2Hex(dta);
end;

function GrDataValues(langIndex : byte; var lineNum : word; grBytes : smallint;
  numberFormat : byte; is01bit : boolean) : string;
var
  x, y : integer;
  dta : byte;
  charsPerLine : byte;
  codex : string;
  lineNumStr : string;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
//    Inc(lineNum, 10);
    codex := IntToStr(LineNum) + ' REM *** Picture data ***'#13#10;  //#$9b
    for y := 0 to grY do begin
      if not is01bit then begin
        charsPerLine := grX div 4;
        Inc(lineNum, code.step);
        codex += IntToStr(LineNum) + ' DATA ';
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*4, y] * 64 +
                 frmGraph.fld[x*4 + 1, y] * 16 +
                 frmGraph.fld[x*4 + 2, y] * 4 +
                 frmGraph.fld[x*4 + 3, y];
          codex += DecHex(dta, numberFormat);
          if x < charsPerLine then
            codex += ',';
        end;
      end
      else begin
        charsPerLine := grX div 8;
        Inc(lineNum, code.step);
        codex += IntToStr(LineNum) + ' DATA ';
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x shl 3, y] * 128 +
                 frmGraph.fld[x shl 3 + 1, y] * 64 +
                 frmGraph.fld[x shl 3 + 2, y] * 32 +
                 frmGraph.fld[x shl 3 + 3, y] * 16 +
                 frmGraph.fld[x shl 3 + 4, y] * 8 +
                 frmGraph.fld[x shl 3 + 5, y] * 4 +
                 frmGraph.fld[x shl 3 + 6, y] * 2 +
                 frmGraph.fld[x shl 3 + 7, y];
//          code += IntToStr(dta);
          codex += DecHex(dta, numberFormat);
          if x < charsPerLine then
            codex += ',';
        end;
      end;
      codex += #13#10;  //#$9b
    end;

    if not is01bit then begin
      // Color data
      Inc(lineNum, code.step);
      codex += IntToStr(LineNum) + ' REM *** Color data ***'#13#10;
      Inc(lineNum, code.step);
      codex += IntToStr(LineNum) + ' DATA ';
      codex += IntToStr(colorValues[0]) +
              ',' + IntToStr(colorValues[1]) +
              ',' + IntToStr(colorValues[2]) +
              ',' + IntToStr(colorValues[3]);
    end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    codex := '; Picture data'#13#10 +
            'BYTE ARRAY picData = [';  // + #13#10;
    for y := 0 to grY do begin
      if not is01bit then begin
        charsPerLine := grX div 4;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*4, y] * 64 +
                 frmGraph.fld[x*4 + 1, y] * 16 +
                 frmGraph.fld[x*4 + 2, y] * 4 +
                 frmGraph.fld[x*4 + 3, y];
//          code += IntToStr(dta);
          codex += DecHex(dta, numberFormat);
          if x < charsPerLine then
            codex += ' ';
        end;
      end
      else begin
        charsPerLine := grX div 8;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*8, y] * 128 +
                 frmGraph.fld[x*8 + 1, y] * 64 +
                 frmGraph.fld[x*8 + 2, y] * 32 +
                 frmGraph.fld[x*8 + 3, y] * 16 +
                 frmGraph.fld[x*8 + 4, y] * 8 +
                 frmGraph.fld[x*8 + 5, y] * 4 +
                 frmGraph.fld[x*8 + 6, y] * 2 +
                 frmGraph.fld[x*8 + 7, y];
//          code += IntToStr(dta);
          codex += DecHex(dta, numberFormat);
          if x < charsPerLine then
            codex += ' ';
        end;
      end;
      if y < grY then
        codex += #13#10'  '
      else
        codex +=']'#13#10;
    end;

    if not is01bit then begin
      // Color data
      codex += #13#10'; Color data'#13#10 +
              'BYTE ARRAY colors = [' +
              IntToStr(colorValues[0]) +
              ' ' + IntToStr(colorValues[1]) +
              ' ' + IntToStr(colorValues[2]) +
              ' ' + IntToStr(colorValues[3]) + ']';
    end;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    codex := 'var'#13#10 +
            '  // Picture data'#13#10 +
            '  picData : array[0..' + IntToStr(grBytes - 1) + '] of byte = ('#13#10;
    for y := 0 to grY do begin
      codex += '    ';
      if not is01bit then begin
        charsPerLine := grX div 4;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*4, y] * 64 +
                 frmGraph.fld[x*4 + 1, y] * 16 +
                 frmGraph.fld[x*4 + 2, y] * 4 +
                 frmGraph.fld[x*4 + 3, y];
          if (y = grY) and (x = charsPerLine) then
//            code += IntToStr(dta)
            codex += DecHex(dta, numberFormat)
          else
//            code += IntToStr(dta) + ', ';
            codex += DecHex(dta, numberFormat) + ', ';
        end;
      end
      else begin
        charsPerLine := grX div 8;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*8, y] * 128 +
                 frmGraph.fld[x*8 + 1, y] * 64 +
                 frmGraph.fld[x*8 + 2, y] * 32 +
                 frmGraph.fld[x*8 + 3, y] * 16 +
                 frmGraph.fld[x*8 + 4, y] * 8 +
                 frmGraph.fld[x*8 + 5, y] * 4 +
                 frmGraph.fld[x*8 + 6, y] * 2 +
                 frmGraph.fld[x*8 + 7, y];
          if (y = grY) and (x = charsPerLine) then
//            code += IntToStr(dta)
            codex += DecHex(dta, numberFormat)
          else
//            code += IntToStr(dta) + ', ';
            codex += DecHex(dta, numberFormat) + ', ';
        end;
      end;
      codex += #13#10;
    end;

    codex +='  );'#13#10;

    if not is01bit then begin
      // Color data
      codex += #13#10'  // Color data'#13#10 +
               '  colors : array[0..3] of byte = ('#13#10 +
               '    ' + IntToStr(colorValues[0]) +
               ', ' + IntToStr(colorValues[1]) +
               ', ' + IntToStr(colorValues[2]) +
               ', ' + IntToStr(colorValues[3]) + #13#10;
      codex +='  );';
    end;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    codex += ''' Picture data'#13#10 +
            'DATA picData() BYTE = ';
    for y := 0 to grY do begin
      if not is01bit then begin
        if y > 0 then
          codex += 'DATA BYTE = ';

        charsPerLine := grX div 4;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*4, y] * 64 +
                 frmGraph.fld[x*4 + 1, y] * 16 +
                 frmGraph.fld[x*4 + 2, y] * 4 +
                 frmGraph.fld[x*4 + 3, y];
          if (y = grY) and (x = charsPerLine) then
//            code += IntToStr(dta)
            codex += DecHex(dta, numberFormat)
          else
//            code += IntToStr(dta) + ', ';
            codex += DecHex(dta, numberFormat) + ', ';
        end;
      end
      else begin
        if y > 0 then
          codex += 'DATA BYTE = ';

        charsPerLine := grX div 8;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*8, y] * 128 +
                 frmGraph.fld[x*8 + 1, y] * 64 +
                 frmGraph.fld[x*8 + 2, y] * 32 +
                 frmGraph.fld[x*8 + 3, y] * 16 +
                 frmGraph.fld[x*8 + 4, y] * 8 +
                 frmGraph.fld[x*8 + 5, y] * 4 +
                 frmGraph.fld[x*8 + 6, y] * 2 +
                 frmGraph.fld[x*8 + 7, y];
          if (y = grY) and (x = charsPerLine) then
//            code += IntToStr(dta)
            codex += DecHex(dta, numberFormat)
          else
//            code += IntToStr(dta) + ', ';
            codex += DecHex(dta, numberFormat) + ', ';
        end;
      end;
      codex += #13#10;
    end;
    if not is01bit then
      // Color data
      codex += #13#10''' Color data'#13#10 +
               'DATA colors() BYTE = ' +
               IntToStr(colorValues[0]) +
               ', ' + IntToStr(colorValues[1]) +
               ', ' + IntToStr(colorValues[2]) +
               ', ' + IntToStr(colorValues[3]);
  end
  { Mad Assembler (MADS)
   ---------------------------------------------------------------------------}
  else if langIndex = _MADS then begin
    codex += '; Picture data'#13#10 +
            'picData'#13#10;
    for y := 0 to grY do begin
      codex += ' .BYTE ';
      if not is01bit then begin
        charsPerLine := grX div 4;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*4, y] * 64 +
                 frmGraph.fld[x*4 + 1, y] * 16 +
                 frmGraph.fld[x*4 + 2, y] * 4 +
                 frmGraph.fld[x*4 + 3, y];
//           code += '$' + Dec2Hex(dta);
          codex += DecHex(dta, numberFormat);
          if x < charsPerLine then
            codex += ',';
        end;
      end
      else begin
        charsPerLine := grX div 8;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*8, y] * 128 +
                 frmGraph.fld[x*8 + 1, y] * 64 +
                 frmGraph.fld[x*8 + 2, y] * 32 +
                 frmGraph.fld[x*8 + 3, y] * 16 +
                 frmGraph.fld[x*8 + 4, y] * 8 +
                 frmGraph.fld[x*8 + 5, y] * 4 +
                 frmGraph.fld[x*8 + 6, y] * 2 +
                 frmGraph.fld[x*8 + 7, y];
//           code += '$' + Dec2Hex(dta);
          codex += DecHex(dta, numberFormat);
          if x < charsPerLine then
            codex += ',';
        end;
      end;
      if y < grY then
        codex += #13#10;
    end;
    if not is01bit then begin
      // Color data
      codex += #13#10#13#10'; Color data'#13#10 +
               'colors'#13#10 +
               ' .BYTE ' + IntToStr(colorValues[0]) +
               ',' + IntToStr(colorValues[1]) +
               ',' + IntToStr(colorValues[2]) +
               ',' + IntToStr(colorValues[3]);
    end;
  end
  { MAC/65
   ---------------------------------------------------------------------------}
  else if langIndex = _MAC65 then begin
//    Inc(lineNum, 10);
//    code += IntToStr(LineNum) + ' DATA ';

    if lineNum = 65535 then begin
      codex += ' ; Picture data'#13#10;
      codex += ' PICDATA'#13#10;
    end
    else begin
      lineNumStr := IntToStr(lineNum) + ' ';
      codex += lineNumStr + '; Picture data'#13#10;
      Inc(lineNum, code.step);
      lineNumStr := IntToStr(lineNum) + ' ';
      codex += lineNumStr + 'PICDATA'#13#10;
    end;
     for y := 0 to grY do begin
       if lineNum = 65535 then
         codex += '  .BYTE '
       else begin
         Inc(lineNum, code.step);
         lineNumStr := IntToStr(lineNum) + ' ';
         codex += lineNumStr + '.BYTE ';
       end;
       if not is01bit then begin
         charsPerLine := grX div 4;
         for x := 0 to charsPerLine do begin
           dta := frmGraph.fld[x*4, y] * 64 +
                  frmGraph.fld[x*4 + 1, y] * 16 +
                  frmGraph.fld[x*4 + 2, y] * 4 +
                  frmGraph.fld[x*4 + 3, y];
//           code += '$' + Dec2Hex(dta);
           codex += DecHex(dta, numberFormat);
           if x < charsPerLine then
             codex += ',';
         end;
       end
       else begin
         charsPerLine := grX div 8;
         for x := 0 to charsPerLine do begin
           dta := frmGraph.fld[x*8, y] * 128 +
                  frmGraph.fld[x*8 + 1, y] * 64 +
                  frmGraph.fld[x*8 + 2, y] * 32 +
                  frmGraph.fld[x*8 + 3, y] * 16 +
                  frmGraph.fld[x*8 + 4, y] * 8 +
                  frmGraph.fld[x*8 + 5, y] * 4 +
                  frmGraph.fld[x*8 + 6, y] * 2 +
                  frmGraph.fld[x*8 + 7, y];
//           code += '$' + Dec2Hex(dta);
           codex += DecHex(dta, numberFormat);
           if x < charsPerLine then
             codex += ',';
         end;
       end;
       if y < grY then
         codex += #13#10;
     end;
    if not is01bit then begin
      // Color data
      if lineNum = 65535 then begin
        codex += #13#10' ;'#13#10;
        codex += ' ; Color data'#13#10 +
                 ' COLORS'#13#10 +
                 '  .BYTE ' + IntToStr(colorValues[0]) +
                 ',' + IntToStr(colorValues[1]) +
                 ',' + IntToStr(colorValues[2]) +
                 ',' + IntToStr(colorValues[3]);
      end
      else begin
        Inc(lineNum, code.step);
        lineNumStr := IntToStr(lineNum) + ' ';
        codex += #13#10 + lineNumStr + ';'#13#10;
        Inc(lineNum, code.step);
        lineNumStr := IntToStr(lineNum) + ' ';
        codex += lineNumStr + '; Color data'#13#10;
        Inc(lineNum, code.step);
        lineNumStr := IntToStr(lineNum) + ' ';
        codex += lineNumStr + 'COLORS'#13#10;
        Inc(lineNum, code.step);
        lineNumStr := IntToStr(lineNum) + ' ';
        codex += lineNumStr + '.BYTE ' + IntToStr(colorValues[0]) +
                 ',' + IntToStr(colorValues[1]) +
                 ',' + IntToStr(colorValues[2]) +
                 ',' + IntToStr(colorValues[3]);
      end;
    end;
  end
  { CC65
   ---------------------------------------------------------------------------}
  else if langIndex = _CC65 then begin
    codex := '// Picture data'#13#10 +
            'const unsigned char picData[' + IntToStr(grBytes - 1) + '] = {'#13#10;
    for y := 0 to grY do begin
      codex += '  ';
      if not is01bit then begin
        charsPerLine := grX div 4;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*4, y] * 64 +
                 frmGraph.fld[x*4 + 1, y] * 16 +
                 frmGraph.fld[x*4 + 2, y] * 4 +
                 frmGraph.fld[x*4 + 3, y];
          if (y = grY) and (x = charsPerLine) then
//            code += IntToStr(dta)
            codex += DecHex(dta, numberFormat)
          else
//            code += IntToStr(dta) + ', ';
            codex += DecHex(dta, numberFormat) + ', ';
        end;
      end
      else begin
        charsPerLine := grX div 8;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*8, y] * 128 +
                 frmGraph.fld[x*8 + 1, y] * 64 +
                 frmGraph.fld[x*8 + 2, y] * 32 +
                 frmGraph.fld[x*8 + 3, y] * 16 +
                 frmGraph.fld[x*8 + 4, y] * 8 +
                 frmGraph.fld[x*8 + 5, y] * 4 +
                 frmGraph.fld[x*8 + 6, y] * 2 +
                 frmGraph.fld[x*8 + 7, y];
          if (y = grY) and (x = charsPerLine) then
//            code += IntToStr(dta)
            codex += DecHex(dta, numberFormat)
          else
//            code += IntToStr(dta) + ', ';
            codex += DecHex(dta, numberFormat) + ', ';
        end;
      end;
      codex += #13#10;
    end;

    codex +='};'#13#10;

    if not is01bit then begin
      // Color data
      codex += #13#10'// Color data'#13#10 +
               'const unsigned char colors[3] = {'#13#10 +
               '  ' + IntToStr(colorValues[0]) +
               ', ' + IntToStr(colorValues[1]) +
               ', ' + IntToStr(colorValues[2]) +
               ', ' + IntToStr(colorValues[3]) + #13#10 +
               '};';
    end;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    //    const char pmdata[] = { 0,255,129,129,129,129,129,129,255,0};
    codex := '// Picture data'#13#10 +
            'const char picData[] = {'#13#10;
    for y := 0 to grY do begin
      codex += '  ';
      if not is01bit then begin
        charsPerLine := grX div 4;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*4, y] * 64 +
                 frmGraph.fld[x*4 + 1, y] * 16 +
                 frmGraph.fld[x*4 + 2, y] * 4 +
                 frmGraph.fld[x*4 + 3, y];
          if (y = grY) and (x = charsPerLine) then
//            code += IntToStr(dta)
            codex += DecHex(dta, numberFormat)
          else
//            code += IntToStr(dta) + ', ';
            codex += DecHex(dta, numberFormat) + ', ';
        end;
      end
      else begin
        charsPerLine := grX div 8;
        for x := 0 to charsPerLine do begin
          dta := frmGraph.fld[x*8, y] * 128 +
                 frmGraph.fld[x*8 + 1, y] * 64 +
                 frmGraph.fld[x*8 + 2, y] * 32 +
                 frmGraph.fld[x*8 + 3, y] * 16 +
                 frmGraph.fld[x*8 + 4, y] * 8 +
                 frmGraph.fld[x*8 + 5, y] * 4 +
                 frmGraph.fld[x*8 + 6, y] * 2 +
                 frmGraph.fld[x*8 + 7, y];
          if (y = grY) and (x = charsPerLine) then
//            code += IntToStr(dta)
            codex += DecHex(dta, numberFormat)
          else
//            code += IntToStr(dta) + ', ';
            codex += DecHex(dta, numberFormat) + ', ';
        end;
      end;
      codex += #13#10;
    end;

    codex +='};'#13#10;

    if not is01bit then begin
      // Color data
      codex += #13#10'// Color data'#13#10 +
               'const char colors[] = {'#13#10 +
               '  ' + IntToStr(colorValues[0]) +
               ', ' + IntToStr(colorValues[1]) +
               ', ' + IntToStr(colorValues[2]) +
               ', ' + IntToStr(colorValues[3]) + #13#10 +
               '};';
    end;
  end;

  result := codex;
end;

function DataValues(langIndex : byte; const byteArray : array of byte; rows, charsPerLine : byte;
  numberFormat : byte) : string; overload;
var
  x, y : integer;
  dta : byte;
  code : string = '';
  cnt : byte = 0;
  maxSize : word;
begin
  maxSize := (rows + 1)*(charsPerLine + 1) - 1;

  { Action!
   ---------------------------------------------------------------------------}
  if langIndex = _ACTION then begin
    code := //'; Text mode 0 data'#13#10 +
            'BYTE ARRAY screenData = [';  // + #13#10;
    //for x := 0 to maxSize do begin
    //  if (x = 0) and (x mod charsPerLine = 0) then
    //    code += ' '
    //end;
    y := 0;
    for x := 0 to maxSize do begin
      if (x > charsPerLine) and (x mod (charsPerLine + 1) = 0) then begin
        cnt := 0;
        code += ' '#13#10;
        Inc(y);
      end;
      dta := byteArray[cnt + y*(charsPerLine + 1)];
      Inc(cnt);
      code += DecHex(dta, numberFormat);
      if cnt <= charsPerLine then
        code += ' ';
    end;
    code +=']'#13#10;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code := 'const'#13#10;
    if not isConstData then
      code += //'var'#13#10 +
              //'  // Text mode 0 data'#13#10 +
              '  screenData : array[0..' + IntToStr(maxSize) + '] of byte = ('#13#10
    else
      code += '  screenData : array[0..' + _DATA_SIZE + '] of byte = ('#13#10;

    y := 0;
    for x := 0 to maxSize do begin
      if (x > charsPerLine) and (x mod (charsPerLine + 1) = 0) then begin
        cnt := 0;
        code += ', '#13#10;
        Inc(y);
      end;
      dta := byteArray[cnt + y*(charsPerLine + 1)];
      Inc(cnt);
      code += DecHex(dta, numberFormat);
      if cnt <= charsPerLine then
        code += ', ';
    end;
    code +=');'#13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code := //''' Text mode 0 data'#13#10 +
            'DATA screenData() BYTE = ';
  //    if (x = 0) and (x mod charsPerLine = 0) then begin
  //      code += ' ';
  //    end
    y := 0;
    for x := 0 to maxSize do begin
      if (x > charsPerLine) and (x mod (charsPerLine + 1) = 0) then begin
        cnt := 0;
        code += #13#10'DATA BYTE = ';
        Inc(y);
      end;
      dta := byteArray[cnt + y*(charsPerLine + 1)];
      Inc(cnt);
      code += DecHex(dta, numberFormat);
      if x < maxSize then
        code += ', ';
    end;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    code := //'var'#13#10 +
            //'  // Text mode 0 data'#13#10 +
            'const char screenData[] = {'#13#10;
    //y := 0;
    //for x := 0 to maxSize do begin
    //  if (x = 0) and (x mod charsPerLine = 0) then
    //    code += ' '
    //  else if (x > charsPerLine - 1) and
    //          (x mod charsPerLine = 0) then begin
    //    code += ','#13#10;
    //    cnt := 0;
    //    Inc(y);
    //  end;
    //  dta := byteArray[cnt + y*40];
    //  Inc(cnt);
    //  code += DecHex(dta, numberFormat);
    //  if cnt < charsPerLine then
    //    code += ',';
    //end;
    //code +='};'#13#10;

    y := 0;
    for x := 0 to maxSize do begin
      if (x > charsPerLine) and (x mod (charsPerLine + 1) = 0) then begin
        cnt := 0;
        code += ', '#13#10;
        Inc(y);
      end;
      dta := byteArray[cnt + y*(charsPerLine + 1)];
      Inc(cnt);
      code += DecHex(dta, numberFormat);
      if cnt <= charsPerLine then
        code += ', ';
    end;
    code +='};'#13#10;
  end;

  result := code;
end;

{ Atari BASIC, Turbo BASIC XL
 ---------------------------------------------------------------------------}
function DataValues(langIndex : byte; const byteArray : array of byte; lineNum : word;
  lineStep : word; rows, charsPerLine : byte; numberFormat : byte) : string; overload;
var
  x, y : word;
  dta : byte;
  code : string = '';
  cnt : byte = 0;
  maxSize : word;
begin
  //for y := 0 to rows - 1 do begin
  //  code += IntToStr(LineNum) + ' DATA ';
  //  for x := 0 to charsPerLine - 1 do begin
  //    dta := byteArray[x + y*40];
  //    code += DecHex(dta, numberFormat);
  //    if x < charsPerLine - 1 then
  //      code += ',';
  //  end;
  //  code += #13#10;
  //  Inc(lineNum, lineStep);
  //end;

  maxSize := (rows + 1)*(charsPerLine + 1) - 1;

//  for x := 0 to maxSize do begin
//    if (x = 0) and (x mod charsPerLine = 0) then begin
//      Inc(lineNum, lineStep);
//      code += IntToStr(LineNum) + ' DATA ';
//    end
//    else if (x >= charsPerLine + 1 ) and
//            (x mod (charsPerLine - 1) = 0) then
//    begin
//      cnt := 0;
//      Inc(lineNum, lineStep);
//      code += #13#10;
//      code += IntToStr(LineNum) + ' DATA ';
//      Inc(y);
//    end;
//    dta := byteArray[cnt + y*40];
//    Inc(cnt);
//    code += DecHex(dta, numberFormat);
//    if cnt <= charsPerLine then
//      code += ',';
//  end;

  y := 0;
  code := IntToStr(LineNum) + ' DATA ';
  for x := 0 to maxSize do begin
    if (x > charsPerLine) and (x mod (charsPerLine + 1) = 0) then begin
      cnt := 0;
      Inc(lineNum, lineStep);
      code += #13#10;
      code += IntToStr(LineNum) + ' DATA ';
      Inc(y);
    end;
    dta := byteArray[cnt + y*(charsPerLine + 1)];
    Inc(cnt);
    code += DecHex(dta, numberFormat);
    if cnt <= charsPerLine then
      code += ',';
  end;

  result := code;
end;

function WaitKeyCode(langIndex : byte) : string;
var
  codex : string;
  lineNumOld : word;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
    lineNumOld := code.number;
    codex := CodeLine('REM Press any key to exit');
    codex += CodeLine('POKE 764,255:IF PEEK(764)<>255 THEN POKE 764,255:END');
    codex += CodeLine('GOTO ' + IntToStr(lineNumOld));
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    codex := #13#10'; Press any key to exit'#13#10 +
            'CH=255'#13#10 +
            'DO UNTIL CH#255 OD'#13#10 +
            'CH=255'#13#10;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    codex := #13#10'  // Press any key to exit'#13#10 +
             '  repeat until KeyPressed;'#13#10 +
             '  ReadKey;'#13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    codex := #13#10''' Press any key to exit'#13#10 +
             'REPEAT: UNTIL Key()'#13#10;
  end;

  result := codex;
end;

function SetDataValues(fldChar : charType; fld : fldFontSetType; charOffset, dataType : byte;
  separator : string) : string;
var
  x, y : byte;
  line, values : string;
begin
  for y := 0 to 7 do begin
    line := '';
    for x := 0 to 7 do begin
      if charOffset = 255 then
        line += IntToStr(fldChar[x, y])
      else
        line += IntToStr(fld[x, y + charOffset shl 3]);
    end;
    case dataType of
      0: line := IntToStr(Bin2Dec(line));
      1: line := '$' + Dec2Hex(bin2Dec(line));
      2: line := '%' + line;
    end;
    if y = 0 then values := line;
    if Trim(line) = '$' then line := '0';
    if Trim(values) = '$' then values := '0';
    if y > 0 then
      values += separator + line
  end;

  result := values;
end;

function CodeLine(line : string) : string;
begin
  result := IntToStr(code.number) + ' ' + line + #13#10;
  Inc(code.number, code.step);
end;

function GenSetColors(langIndex : byte) : string;
begin
  if langIndex < 2 then begin
    Inc(code.number, code.step);
    result := CodeLine('REM Set colors') +
              CodeLine('POKE 708,' + IntToStr(colorValues[1]) +
              ':POKE 709,' + IntToStr(colorValues[2])) +
              CodeLine('POKE 710,' + IntToStr(colorValues[3]) +
              ':POKE 711,' + IntToStr(colorValues[10])) +
              CodeLine('POKE 712,' + IntToStr(colorValues[0]));
  end
  else if langIndex = _MAD_PASCAL then
    result := #13#10'  // Set colors'#13#10 +
              '  POKE(712, ' + IntToStr(colorValues[0]) + ');'#13#10 +
              '  POKE(708, ' + IntToStr(colorValues[1]) + ');'#13#10 +
              '  POKE(709, ' + IntToStr(colorValues[2]) + ');'#13#10 +
              '  POKE(710, ' + IntToStr(colorValues[3]) + ');'#13#10 +
              '  POKE(711, ' + IntToStr(colorValues[10]) + ');'#13#10
  else if langIndex = _ACTION then
    result := #13#10'; Set colors'#13#10 +
              'POKE(708,' + IntToStr(colorValues[1]) + ') POKE(709,' +
              IntToStr(colorValues[2]) + ')'#13#10 +
              'POKE(710,' + IntToStr(colorValues[3]) + ') POKE(711,' +
              IntToStr(colorValues[10]) + ')'#13#10 +
              'POKE(712,' + IntToStr(colorValues[0]) + ')'#13#10
  else if langIndex = _FAST_BASIC then
    result := #13#10''' Set colors'#13#10 +
              'POKE 708, ' + IntToStr(colorValues[1]) +
              ' : POKE 709, ' + IntToStr(colorValues[2]) + #13#10 +
              'POKE 710, ' + IntToStr(colorValues[3]) +
              ' : POKE 711, ' + IntToStr(colorValues[10]) + #13#10 +
              'POKE 712, ' + IntToStr(colorValues[0]) + #13#10;
end;

//function SetCopyCharSetML(var charDataCodeNum : byte; chrRAM : string) : string;
//begin
//  result := CodeLine('REM CALL MACHINE LANGUAGE ROUTINE') +
//            CodeLine('X=USR(ADR(MLCODE$),57344,' + chrRAM + ',4)');
//  if code.number < 30000 then
//    charDataCodeNum := 30000
//  else
//    Inc(charDataCodeNum, 1000);
//
//  result += CodeLine('RESTORE ' + IntToStr(charDataCodeNum));
//
//end;

function SetFastCopyRoutine : string;
begin
  result := CodeLine('REM Machine language routine') +
            CodeLine('FOR I=1 TO 33:READ A:MLCODE$(I,I)=CHR$(A):NEXT I') +
            CodeLine('RETURN') +
            CodeLine('DATA 104,104,133,205,104,133,204,104,133,207,104,133,206,104,104,170') +
            CodeLine('DATA 160,0,177,204,145,206,136,208,249,230,205,230,207,202,208,242,96');
end;

function SetCIOFastLoad(varCharRAM : string; isCalcHiLo, isSetCIO : boolean) : string;
begin
  result := CodeLine('REM Set CIO for faster file load');

  if isSetCIO then begin
    result += CodeLine('X=16:DIM ML$(7)') +
              CodeLine('ML$="hhh*LV*":ML$(4,4)=CHR$(170):ML$(7,7)=CHR$(228)') +
              CodeLine('ICCOM=834:ICBADR=836:ICBLEN=840');
  end;

  result += CodeLine('REM High and low address for IOCB #1');
  if isCalcHiLo then
    result += CodeLine('POKE ICBADR+X+1,INT(' + varCharRAM + '/256):POKE ICBADR+X,' +
                       varCharRAM + '-INT(' + varCharRAM + '/256)*256')
  else
    result += CodeLine('POKE ICBADR+X+1,' + varCharRAM + ':POKE ICBADR+X,0');

  result += CodeLine('POKE ICBLEN+X+1,4:POKE ICBLEN+X,0') +
            CodeLine('REM Call CIO (7 = "get" command, 16 = IOCB #1)') +
            CodeLine('POKE ICCOM+X,7:A=USR(ADR(ML$),X)');
end;

function DisplayScreen(langIndex : byte; maxX, maxY : byte; isFile : boolean; isSetCIO : boolean) :
  string; overload;
var
//  maxSize : word;
  setMaxX : string;
begin
//  maxSize := (maxX + 1)*(maxY + 1) - 1;

  if (maxX = antic_mode_max_x) and (maxY = antic_mode_max_y) then begin
    case langIndex of
      _ATARI_BASIC: begin
        if isFile then
          result := SetCIOFastLoad(_BASIC_SCR_MEM_VAR, true, isSetCIO)
        else
          result := CodeLine('FOR I=0 TO ' + UpperCase(_DATA_SIZE)) +
                    CodeLine('READ BYTE:POKE ' + _BASIC_SCR_MEM_VAR + '+I,BYTE') +
                    CodeLine('NEXT I');
      end;
    end
  end
  else begin
    case langIndex of
      _ATARI_BASIC: begin
        result := CodeLine('CNT=0');
        result += CodeLine('FOR I=0 TO ' + UpperCase(_DATA_SIZE));
        if isFile then begin
          result += CodeLine('GET #1,BYTE');
          setMaxX := 'MAXX+1';
        end
        else
          setMaxX := IntToStr(maxX + 1);

        result += CodeLine('IF CNT=' + setMaxX + ' THEN ' +
                           _BASIC_SCR_MEM_VAR + '=' + _BASIC_SCR_MEM_VAR + '+40-CNT:CNT=0') +
                  CodeLine('READ BYTE:POKE ' + _BASIC_SCR_MEM_VAR + '+I,BYTE') +
                  CodeLine('CNT=CNT+1') +
                  CodeLine('NEXT I');
      end;
    end;
  end;
end;

function DisplayScreen(langIndex : byte; maxX, maxY : byte; isFile : boolean) : string; overload;
var
//  maxSize : word;
  setMaxX : string;
begin
//  maxSize := (maxX + 1)*(maxY + 1) - 1;

  if (maxX = antic_mode_max_x) and (maxY = antic_mode_max_y) then begin
    case langIndex of
      _ATARI_BASIC: begin
        if isFile then
          result := SetCIOFastLoad(_BASIC_SCR_MEM_VAR, true, true)
        else
          result := CodeLine('FOR I=0 TO ' + UpperCase(_DATA_SIZE)) +
                    CodeLine('READ BYTE:POKE ' + _BASIC_SCR_MEM_VAR + '+I,BYTE') +
                    CodeLine('NEXT I');
      end;
      _TURBO_BASIC_XL: begin
        if isFile then
          result := CodeLine('BGET #%1,DPEEK(88),SIZE')
        else
          result := CodeLine('FOR I=%0 TO ' + UpperCase(_DATA_SIZE)) +
                    CodeLine('READ BYTE:POKE ' + _BASIC_SCR_MEM_VAR + '+I,BYTE') +
                    CodeLine('NEXT I')
      end;
      _ACTION: begin
        if isFile then
          result := 'FOR i=0 TO ' + _DATA_SIZE + ' DO'#13#10 +
                    '  data=GetD(2)'#13#10 +
                    '  PokeC(' + _SCR_MEM_VAR + '+i,data)'#13#10 +
                    'OD'#13#10#13#10
        else
          result := 'MoveBlock(' + _SCR_MEM_VAR + ', ' + _SCR_MEM_DATA_VAR + ', ' + _DATA_SIZE +
                    '+1)'#13#10;
      end;
      _MAD_PASCAL: begin
        if isFile then
          result := '  BGet(1, pointer(' + _SCR_MEM_VAR + '), ' + _DATA_SIZE + ');'#13#10#13#10
        else
          result := '  Move(' + _SCR_MEM_DATA_VAR + ', pointer(' + _SCR_MEM_VAR + '), ' + _DATA_SIZE + ' + 1);'#13#10;
      end;
      _FAST_BASIC: begin
        if isFile then
          result := 'BGET #1, ' + _SCR_MEM_VAR + ', ' + _DATA_SIZE + #13#10
        else
          result := 'MOVE ADR(' + _SCR_MEM_DATA_VAR + '), ' + _SCR_MEM_VAR + ', ' + _DATA_SIZE +
                    ' + 1' + #13#10;
      end;
    end
  end
  else begin
    case langIndex of
      _ATARI_BASIC: begin
        result := CodeLine('CNT=0');
        result += CodeLine('FOR I=0 TO ' + UpperCase(_DATA_SIZE));  //IntToStr(maxSize));
        if isFile then begin
          result += CodeLine('GET #1,BYTE');
          setMaxX := 'MAXX+1';
        end
        else
          setMaxX := IntToStr(maxX + 1);

        result += CodeLine('IF CNT=' + setMaxX + ' THEN ' +
                           _BASIC_SCR_MEM_VAR + '=' + _BASIC_SCR_MEM_VAR + '+40-CNT:CNT=0') +
                  CodeLine('READ BYTE:POKE ' + _BASIC_SCR_MEM_VAR + '+I,BYTE') +
                  CodeLine('CNT=CNT+1') +
                  CodeLine('NEXT I');
      end;
      _TURBO_BASIC_XL: begin
        result := CodeLine('CNT=%0');
        result += CodeLine('FOR I=%0 TO ' + UpperCase(_DATA_SIZE));
        if isFile then begin
          result += CodeLine('GET #%1,BYTE');
          setMaxX := 'MAXX+1';
        end
        else
          setMaxX := IntToStr(maxX + 1);

        result += CodeLine('IF CNT=' + setMaxX + ' THEN ' +
                           _BASIC_SCR_MEM_VAR + '=' + _BASIC_SCR_MEM_VAR + '+40-CNT:CNT=%0') +
                  CodeLine('READ BYTE:POKE ' + _BASIC_SCR_MEM_VAR + '+I,BYTE') +
                  CodeLine('CNT=CNT+%1') +
                  CodeLine('NEXT I');
      end;
      _ACTION: begin
        result := 'FOR i=0 TO ' + _DATA_SIZE + ' DO'#13#10;
        if isFile then begin
          result += '  data=GetD(2)'#13#10;
          setMaxX := 'MAXX+1';
        end
        else
          setMaxX := IntToStr(maxX + 1);

        result += '  IF cnt = ' + setMaxX + ' THEN'#13#10 +
                  '    ' + _SCR_MEM_VAR + '==+40-cnt'#13#10 +
                  '    cnt = 0'#13#10 +
                  '  FI'#13#10;
        if isFile then
          result += '  PokeC(' + _SCR_MEM_VAR + ' + i, data)'#13#10
        else
          result += '  PokeC(' + _SCR_MEM_VAR + ' + i, ' + _SCR_MEM_DATA_VAR + '(i))'#13#10;

        result += '  cnt==+1'#13#10 +
                  'OD'#13#10;
      end;
      _MAD_PASCAL: begin
        result := '  // Read screen data'#13#10 +
                  '  for i := 0 to ' + _DATA_SIZE + ' do begin'#13#10;
        if isFile then begin
          result += '    data := Get(1);'#13#10;
          setMaxX := 'MaxX + 1';
        end
        else
          setMaxX := IntToStr(maxX + 1);

        result += '    if cnt = ' + setMaxX + ' then begin'#13#10 +
                  '      Inc(' + _SCR_MEM_VAR + ', 40 - cnt);'#13#10 +
                  '      cnt := 0;'#13#10 +
                  '    end;'#13#10;
        if isFile then
          result += '    Poke(' + _SCR_MEM_VAR + ' + i, data);'#13#10
        else
          result += '    Poke(' + _SCR_MEM_VAR + ' + i, ' + _SCR_MEM_DATA_VAR + '[i]);'#13#10;

        result += '    Inc(cnt);'#13#10 +
                  '  end;'#13#10;
      end;
      _FAST_BASIC: begin
        result := ''' Screen data'#13#10 +
                  'cnt = 0'#13#10 +
                  'FOR i = 0 TO ' + _DATA_SIZE + #13#10;
        if isFile then begin
          result += '  GET #1, byte'#13#10;
          setMaxX := 'MaxX + 1';
        end
        else
          setMaxX := IntToStr(maxX + 1);

        result += '  IF cnt = ' + setMaxX + #13#10 +
                  '    ' + _SCR_MEM_VAR + ' = ' + _SCR_MEM_VAR + ' + 40 - cnt'#13#10 +
                  '    cnt = 0'#13#10 +
                  '  ENDIF'#13#10;
        if isFile then
          result += '  POKE ' + _SCR_MEM_VAR + ' + i, byte'#13#10
        else
          result += '  POKE ' + _SCR_MEM_VAR + ' + i, ' + _SCR_MEM_DATA_VAR + '(i)'#13#10;

        result += '  INC cnt'#13#10 +
                  'NEXT'#13#10;
      end;
    end;
  end;
end;

function SetCharSet(langIndex : byte; fontName : string; isChBasNew, isSetCIO : boolean) : string; overload;
begin
  case langIndex of
    _ATARI_BASIC: begin
      result := CodeLine('REM Load font set from file') +
                CodeLine('OPEN #1,4,0,"' + fontName + '"') +
                SetCIOFastLoad('TOPMEM', false, isSetCIO) +
                CodeLine('CLOSE #1');
      if isChBasNew then begin
        result += CodeLine('REM Modify character set pointer');
        result += CodeLine('POKE 756,TOPMEM');
      end;
    end;
  end;
end;

function SetCharSet(langIndex : byte; fontName : string; isChBasNew : boolean) : string; overload;
begin
  case langIndex of
    _ATARI_BASIC: begin
      result := CodeLine('REM Load font set from file') +
                CodeLine('OPEN #1,4,0,"' + fontName + '"') +
                SetCIOFastLoad('TOPMEM', false, true) +
                CodeLine('CLOSE #1');
      if isChBasNew then begin
        result += CodeLine('REM Modify character set pointer');
        result += CodeLine('POKE 756,TOPMEM');
      end;
    end;
    _TURBO_BASIC_XL: begin
      result := CodeLine('REM Load font set from file') +
                CodeLine('OPEN #%1,4,%0,"' + fontName + '"') +
                CodeLine('BGET #%1,CHRAM,1024') +
                CodeLine('CLOSE #%1');
      if isChBasNew then begin
        result += CodeLine('REM Modify character set pointer');
        result += CodeLine('POKE 756,TOPMEM');
      end;
    end;
    _MAD_PASCAL: begin
      result := #13#10'  // Load character set file'#13#10 +
//                '  Cls(1);'#13#10 +
                '  Opn(1, 4, 0, ' + QuotedStr(fontName) + ');'#13#10 +
                '  BGet(1, pointer(chRAM), 1024);'#13#10 +
                '  Cls(1);'#13#10#13#10;
      if isChBasNew then begin
        result += '  // Set address of new set'#13#10 +
                  '  CHBAS := topMem;'#13#10;
      end;
    end;
    _ACTION: begin
      result := '; Read font data'#13#10 +
                'OPEN(2,"' + fontName + '",4,0)'#13#10#13#10 +
                'FOR I=0 TO 1023'#13#10 +
                'DO'#13#10 +
                '  DATA=GETD(2)'#13#10 +
                '  FONT(I)=DATA'#13#10 +
                'OD'#13#10#13#10 +
                'CLOSE(2)'#13#10#13#10 +
                '; Copy font set to new address'#13#10 +
                'MOVEBLOCK(CHRAM,FONT,1024)'#13#10#13#10;
      if isChBasNew then begin
        result += '; Set address of new set'#13#10 +
                  'CHBAS=TOPMEM'#13#10;
      end;
    end;
    _FAST_BASIC: begin
      result := #13#10''' Load character set from file'#13#10 +
//                'CLOSE #1'#13#10 +
                'OPEN #1, 4, 0, "' + fontName + '"'#13#10 +
                'BGET #1, CHRAM, 1024'#13#10 +
                'CLOSE #1'#13#10#13#10;
      if isChBasNew then begin
        result += ''' Modify character set pointer'#13#10 +
                  'POKE 756, TOPMEM'#13#10;
      end;
    end;
  end;
end;

procedure SetListings(var listings : TListings);
var
  i : byte;
begin
  for i := 0 to 7 do begin
    listings[i, 0] := true;
    listings[i, 1] := true;
    listings[i, 2] := true;
    listings[i, 3] := true;
    listings[i, 4] := true;
    listings[i, 5] := true;
    listings[i, 6] := true;
    listings[i, 7] := true;
    listings[i, 8] := true;
  end;
end;

procedure Set01(groupBox : TGroupBox; langIndex : byte; radDataType : TRadioGroup; isBinary : boolean);
begin
  groupBox.Enabled := langIndex < 2;
  groupBox.Visible := groupBox.Enabled;

  if langIndex = 0 then begin
    radDataType.ItemIndex := 0;
    TRadioButton(radDataType.Controls[1]).Enabled := false;
    if isBinary then
      TRadioButton(radDataType.Controls[2]).Enabled := false;
  end
  else begin
    TRadioButton(radDataType.Controls[1]).Enabled := true;
    if isBinary then
      TRadioButton(radDataType.Controls[2]).Enabled := true;
  end;
end;

procedure Set02(memo : TMemo; code : string);
begin
  memo.Lines.Clear;
  memo.Lines.Add(code);

  // Set cursor position at the top of memo object
  memo.SelStart := 0;
  memo.SelLength := 0;
  SendMessage(memo.Handle, EM_SCROLLCARET, 0, 0);
end;

end.

