{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Custom library
}
unit code_lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dialogs,
  common;

type
  TCodeLine = record
    line : string;
    number : word;
    step: word;
  end;

var
  code : TCodeLine;

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
//  cr_lf : string = #13#10;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
//    Inc(lineNum, 10);
    codex := IntToStr(LineNum) + ' REM *** PICTURE DATA ***'#13#10;  //#$9b
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
      codex += IntToStr(LineNum) + ' REM *** COLOR DATA ***'#13#10;
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
    codex := '; Picture data' + #13#10 +
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

//      codex += '  ';
    end;

    if not is01bit then begin
      // Color data
      codex += '; Color data' + #13#10 +
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
    codex := 'var' + #13#10 +
            '  // Picture data' + #13#10 +
            '  picData : array[0..' + IntToStr(grBytes - 1) + '] of byte = (' + #13#10;
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

    codex +='  );' + #13#10;

    if not is01bit then begin
      // Color data
      codex += '  // Color data' + #13#10 +
              '  colors : array[0..3] of byte = (' + #13#10 +
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
    codex += ''' Picture data' + #13#10 +
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
      codex += #13#10 +
               ''' Color data' + #13#10 +
               'DATA colors() BYTE = ' +
               IntToStr(colorValues[0]) +
               ', ' + IntToStr(colorValues[1]) +
               ', ' + IntToStr(colorValues[2]) +
               ', ' + IntToStr(colorValues[3]);
  end
  { Mad Assembler (MADS)
   ---------------------------------------------------------------------------}
  else if langIndex = _MADS then begin
    codex += '; Picture data' + #13#10 +
            'picData' + #13#10;
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
      codex += #13#10 + #13#10 + '; Color data' + #13#10 +
               'colors' + #13#10 +
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
      codex += ' ; Picture data' + #13#10;
      codex += ' PICDATA' + #13#10;
    end
    else begin
      lineNumStr := IntToStr(lineNum) + ' ';
      codex += lineNumStr + '; Picture data' + #13#10;
      Inc(lineNum, code.step);
      lineNumStr := IntToStr(lineNum) + ' ';
      codex += lineNumStr + 'PICDATA' + #13#10;
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
        codex += #13#10 + ' ;' + #13#10;
        codex += ' ; Color data' + #13#10 +
                 ' COLORS' + #13#10 +
                 '  .BYTE ' + IntToStr(colorValues[0]) +
                 ',' + IntToStr(colorValues[1]) +
                 ',' + IntToStr(colorValues[2]) +
                 ',' + IntToStr(colorValues[3]);
      end
      else begin
        Inc(lineNum, code.step);
        lineNumStr := IntToStr(lineNum) + ' ';
        codex += #13#10 + lineNumStr + ';' + #13#10;
        Inc(lineNum, code.step);
        lineNumStr := IntToStr(lineNum) + ' ';
        codex += lineNumStr + '; Color data' + #13#10;
        Inc(lineNum, code.step);
        lineNumStr := IntToStr(lineNum) + ' ';
        codex += lineNumStr + 'COLORS' + #13#10;
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
    codex := '// Picture data' + #13#10 +
            'const unsigned char picData[' + IntToStr(grBytes - 1) + '] = {' + #13#10;
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

    codex +='};' + #13#10;

    if not is01bit then begin
      // Color data
      codex += '// Color data' + #13#10 +
               'const unsigned char colors[3] = {' + #13#10 +
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
    codex := '// Picture data' + #13#10 +
            'const char picData[] = {' + #13#10;
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

    codex +='};' + #13#10;

    if not is01bit then begin
      // Color data
      codex += '// Color data' + #13#10 +
               'const char colors[] = {' + #13#10 +
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
  numberFormat : byte) : string;
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
    code := //'; Text mode 0 data' + #13#10 +
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
    code +=']' + #13#10;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code := //'var' + #13#10 +
            //'  // Text mode 0 data' + #13#10 +
            '  screenData : array[0..' + IntToStr(maxSize) + '] of byte = (' + #13#10;
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
    code +=');' + #13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code += //''' Text mode 0 data' + #13#10 +
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
    code := //'var' + #13#10 +
            //'  // Text mode 0 data' + #13#10 +
            'const char screenData[] = {' + #13#10;
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
    //code +='};' + #13#10;

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
    code +='};' + #13#10;
  end;

  result := code;
end;

{ Atari BASIC, Turbo BASIC XL
 ---------------------------------------------------------------------------}
function DataValues(langIndex : byte; const byteArray : array of byte; lineNum : word;
  lineStep : word; rows, charsPerLine : byte; numberFormat : byte) : string;
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

//  debug('charsPerLine, rows', charsPerLine, rows);
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
////    debug('x, cnt, y', x, cnt, y);
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
  //50 IF CNT=5 THEN SCR=SCR+40-CNT:CNT=0
  //60 POKE SCR+I,BYTE
  //70 CNT=CNT+1
  //80 NEXT I

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
    codex := CodeLine('POKE 764,255:IF PEEK(764)<>255 THEN POKE 764,255:END');
//    Inc(lineNum, 10);
    codex += CodeLine('GOTO ' + IntToStr(lineNumOld));
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    codex := #13#10'; Press any key to exit' + #13#10 +
            'CH=255'#13#10 +
            'DO UNTIL CH#255 OD'#13#10 +
            'CH=255'#13#10;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    codex := #13#10'  repeat until KeyPressed;'#13#10 +
            '  ReadKey;'#13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    codex := #13#10'REPEAT: UNTIL Key()'#13#10;
  end;

  result := codex;
end;

function Gr15Opt: string;
//(grBytes : smallint; numberFormat : byte) : string;
var
  x, y : byte;
  dta, old : byte;
  charsPerLine : byte;
  code, codeHeader : string;
  picMapData : string;
  cnt : integer = 0;
  cnt02 : integer = 0;
  isFirst : boolean;
begin
  { Mad Pascal
   ---------------------------------------------------------------------------}
  code := '';
//    showmessage(inttostr(grx) + ' / ' + inttostr(gry));

  for y := 0 to grY do begin
//      code += '    ';
    charsPerLine := grX div 4;
    for x := 0 to charsPerLine do begin
      dta := frmGraph.fld[x*4, y] * 64 +
             frmGraph.fld[x*4 + 1, y] * 16 +
             frmGraph.fld[x*4 + 2, y] * 4 +
             frmGraph.fld[x*4 + 3, y];
      if (y = 0) and (x = 0) then begin
        old := dta;
        isFirst := true;
      end;
//        showmessage('1.) ' + inttostr(dta) + ' cnt = ' + inttostr(cnt) + ' perline = ' + inttostr(charsPerLine));
//        if (y > 0) and (dta = old) then begin
//        if not isStart and (dta = old) then begin
      if (dta = old) and not isFirst then  //((y = 0) and (x = 0))  then begin  // and (cnt02 > 0)  // and (y < 3)
        Inc(cnt)
      else begin
//          showmessage('2.) ' + inttostr(dta) + ' cnt = ' + inttostr(cnt));
        if not isFirst then begin
          code += IntToStr(old);
//            if cnt > 0 then begin
////              code += 'x' + IntToStr(cnt + 1);
//              picMapData += IntToStr(cnt + 1);
//            end
//            else begin
//              picMapData += '1';
//            end;
          picMapData += IntToStr(cnt + 1);
          code += ', ';
          picMapData += ', ';
        end;
//          if (y = grY) and (x = charsPerLine) then
//          else begin
        //if not isFirst then begin
        //  code += ', ';
        //end;
        if (cnt02 mod 20 = 0) and not isFirst then begin
          code += #13#10;
          picMapData += #13#10;
        end;
        old := dta;
        cnt := 0;
        inc(cnt02);
        isFirst := false;
      end;
    end;
  end;

  code += IntToStr(old);
  picMapData += IntToStr(old);
  //if cnt > 0 then begin
  //  code += 'x' + IntToStr(cnt + 1);
  //end;

  codeHeader := 'var'#13#10 +
                '  // Picture map data'#13#10 +
                '  picMapData : array[0..' + IntToStr(cnt02 - 1) + '] of byte = ('#13#10 +
                picMapData + ');'#13#10 +
                '  // Picture data'#13#10 +
                '  picData : array[0..' + IntToStr(cnt02 - 1) + '] of byte = ('#13#10;
  code := codeHeader + code;
  code += ');'#13#10;

  result := code;
end;

function Gr15Asm(scanlines : byte) : string;
var
  x, y : byte;
  dta, old : byte;
  charsPerLine : byte;
  code, codeHeader : string;
  picMapData : string;
  cnt : integer = 0;
  cnt02 : integer = 0;
  isFirst : boolean;
begin
  { Mads Assembler
   ---------------------------------------------------------------------------}
  code := '';
  for y := 0 to scanlines do begin
//      code += '    ';
    charsPerLine := grX div 4;
    for x := 0 to charsPerLine do begin
      dta := frmGraph.fld[x*4, y] * 64 +
             frmGraph.fld[x*4 + 1, y] * 16 +
             frmGraph.fld[x*4 + 2, y] * 4 +
             frmGraph.fld[x*4 + 3, y];
      if (y = 0) and (x = 0) then begin
        old := dta;
        isFirst := true;
        code += '  dta ';
        picMapData += '  dta ';
      end;
      if (dta = old) and not isFirst then
        Inc(cnt)
      else begin
//          showmessage('2.) ' + inttostr(dta) + ' cnt = ' + inttostr(cnt));
        if not isFirst then begin
          code += IntToStr(old);
//            if cnt > 0 then begin
////              code += 'x' + IntToStr(cnt + 1);
//              picMapData += IntToStr(cnt + 1);
//            end
//            else begin
//              picMapData += '1';
//            end;
          picMapData += IntToStr(cnt + 1);

          if (cnt02 mod 40 <> 0) and not isFirst then begin
            code += ', ';
            picMapData += ', ';
          end;
        end;
//          if (y = grY) and (x = charsPerLine) then
//          else begin
        //if not isFirst then begin
        //  code += ', ';
        //end;
        if (cnt02 mod 40 = 0) and not isFirst then begin
          code += #13#10;
          picMapData += #13#10;
          code += '  dta ';
          picMapData += '  dta ';
        end;
        old := dta;
        cnt := 0;
        inc(cnt02);
        isFirst := false;
      end;
    end;
  end;

  code += IntToStr(old);
  picMapData += IntToStr(old);
  //if cnt > 0 then begin
  //  code += 'x' + IntToStr(cnt + 1);
  //end;

  codeHeader := '  ; Picture map data - ' + IntToStr(cnt02) + #13#10 +
                picMapData + #13#10 +
                '  ; Picture data'#13#10;
  code := codeHeader + code + #13#10;

  result := code;
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
    result := CodeLine('REM SET COLORS') +
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
    result := #13#10'''Set colors'#13#10 +
              'POKE 708, ' + IntToStr(colorValues[1]) +
              ' : POKE 709, ' + IntToStr(colorValues[2]) + #13#10 +
              'POKE 710, ' + IntToStr(colorValues[3]) +
              ' : POKE 711, ' + IntToStr(colorValues[10]) + #13#10 +
              'POKE 712, ' + IntToStr(colorValues[0]) + #13#10;
end;

end.

