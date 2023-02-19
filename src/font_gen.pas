{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Character set editor - source code generator
}
unit font_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, lcltype, BCListBox, BCMDButton, BCMaterialDesignButton,
  common;

type
  { TfrmFontSetGen }
  TfrmFontSetGen = class(TForm)
    btnCopyToEditor : TBCMaterialDesignButton;
    btnClose : TBCMaterialDesignButton;
    boxStartLine : TGroupBox;
    btnAtariBASIC : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnKickC : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnMads : TBCMDButton;
    btnMac65 : TBCMDButton;
    btnCC65 : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    editFontname: TLabeledEdit;
    editLineStep : TSpinEditEx;
    editStartLine : TSpinEditEx;
    lblLineStep : TLabel;
    lblStartLine : TLabel;
    listExamples : TBCPaperListBox;
    memo : TMemo;
    panelLang : TBCPaperPanel;
    radDataType : TRadioGroup;
    StaticText1 : TStaticText;
    StaticText2 : TStaticText;
    StaticText3 : TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditorProc(Sender: TObject);
    procedure LangProc(Sender : TObject);
    procedure CreateCodeProc(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    procedure ExamplesProc(Sender: TObject);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
  private
    { private declarations }
    listings : TListings;
    isCreate : boolean;
    procedure CreateCode;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
    function Example04 : string;
    function Example05 : string;
  end;

var
  frmFontSetGen: TfrmFontSetGen;

implementation

{$R *.lfm}

uses
  fonts, src_editor, lib, code_lib;

{ TfrmFontSetGen }

procedure TfrmFontSetGen.FormCreate(Sender: TObject);
begin
  isCreate := true;

  SetListings(listings);

  // Example 4
  listings[3, 5] := false;
  listings[3, 6] := false;
  listings[3, 7] := false;
  listings[3, 8] := false;

  // Example 5
  listings[4, 5] := false;
  listings[4, 6] := false;
  listings[4, 7] := false;
  listings[4, 8] := false;
end;

procedure TfrmFontSetGen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;

  if frmFonts.filename = '' then
    editFontname.Text:= 'H1:DEFAULT.FNT'
  else
    editFontname.Text:= 'H1:' + ExtractFileName(frmFonts.filename);

  isCreate := false;
  langIndex := 0;
//  ListExamples.Selected := ListExamples.Items.GetFirstNode;
  listExamples.ListBox.ItemIndex := 0;
  CreateCodeProc(Sender);
end;

procedure TfrmFontSetGen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmFontSetGen.CopyToEditorProc(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor(Sender);
  Close;
end;

procedure TfrmFontSetGen.CreateCode;
var
  code : string;
begin
  //boxStartLine.Enabled := langIndex < 2;
  //boxStartLine.Visible := boxStartLine.Enabled;
  //
  //if langIndex = 0 then begin
  //  radDataType.ItemIndex := 0;
  //  TRadioButton(radDataType.Controls[1]).Enabled := false;
  //  TRadioButton(radDataType.Controls[2]).Enabled := false;
  //end
  //else begin
  //  TRadioButton(radDataType.Controls[1]).Enabled := true;
  //  TRadioButton(radDataType.Controls[2]).Enabled := true;
  //end;

  Set01(boxStartLine, langIndex, radDataType, true);

  case ListExamples.ListBox.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
    3: code := Example04;
    4: code := Example05;
  end;

  //memo.Lines.Clear;
  //memo.Lines.Add(code);
  //
  //// Set cursor position at the top of memo object
  //memo.SelStart := 0;
  //memo.SelLength := 0;
  //SendMessage(memo.Handle, EM_SCROLLCARET, 0, 0);

  Set02(memo, code);
end;

{-----------------------------------------------------------------------------
 Selected character data statements
 -----------------------------------------------------------------------------}
function TfrmFontSetGen.Example01 : string;
begin
  { Atari BASIC / Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM CHR$(' + AtasciiCode(frmFonts.offs) + ')');
//    Inc(code.number, code.step);
    code.line += IntToStr(code.number) + ' DATA ' +
                 SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex, ',') +
                 #13#10;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := 'BYTE ARRAY CHARDATA=[' +  // #13#10 + '  ' +
                 SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex, ' ') +
                 ']'#13#10;
//    SetValues(_ACTION, 255)
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := 'const'#13#10 +
            '  charData : array[0..7] of byte ='#13#10 +
            '    (' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex,
                                    ', ') + ');'#13#10;
    // SetValues(_MAD_PASCAL, 255)
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := 'DATA charData() BYTE = '#13#10 +
            '  ' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex,
                                 ', ') + #13#10;
//    SetValues(_FAST_BASIC, 255)
  end
  { Mad Assembler
   ---------------------------------------------------------------------------}
  else if langIndex = _MADS then begin
    code.line := 'chrData .BYTE ' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255,
                                                  radDataType.ItemIndex, ',') + #13#10;
//    SetValues(_MADS, 255)
  end
  { MAC/65
   ---------------------------------------------------------------------------}
  else if langIndex = _MAC65 then begin
    code.line := 'CHRDATA .BYTE ' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255,
                                                  radDataType.ItemIndex, ',') + #13#10;
//    SetValues(_MAC65, 255)
  end
  { CC65
   ---------------------------------------------------------------------------}
  else if langIndex = _CC65 then begin
    code.line := 'const unsigned char chr[7] = {'#13#10 +
            '  ' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex,
                                 ', ') + #13#10 +
            '};'#13#10;
//    SetValues(_CC65, 255)
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    code.line := 'const char chr[] = {'#13#10 +
            '  ' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex,
                                 ', ') + #13#10 +
            '};'#13#10;
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Modified characters
 -----------------------------------------------------------------------------}
function TfrmFontSetGen.Example02 : string;
var
  i : byte;
begin
  code.line := '';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
        code.line += IntToStr(code.number) + ' DATA ' +
                     SetDataValues(frmFonts.fldChar, frmFonts.fld,i, radDataType.ItemIndex, ',') +
                     #13#10;
        Inc(code.number, code.step);
      end;
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
        code.line += IntToStr(code.number) + ' DATA ' +
                     SetDataValues(frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ',') +
                     #13#10;
        Inc(code.number, code.step);
      end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += '; Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                     'BYTE ARRAY char' + IntToStr(i) + '=[' +  //#13#10'  ' +
                     SetDataValues(frmFonts.fldChar, frmFonts.fld,
                     i, radDataType.ItemIndex, ' ') + ']'#13#10;
      end;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += '  // Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                '  char' + IntToStr(i) + ' : array[0..7] of byte = (' +
                SetDataValues(frmFonts.fldChar, frmFonts.fld,
                              i, radDataType.ItemIndex, ', ') + ');'#13#10;
      end;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
//     code := ''' Modified characters'#13#10;
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += ''' Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                'DATA char' + IntToStr(i) + '() BYTE = ' +
                SetDataValues(frmFonts.fldChar, frmFonts.fld,
                              i, radDataType.ItemIndex, ', ') + #13#10;
      end;
  end
  { Mad Assembler
   ---------------------------------------------------------------------------}
  else if langIndex = _MADS then begin
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += '; Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                'char' + IntToStr(i) + ' .BYTE ' +
                SetDataValues(frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ',') +
                #13#10;
      end;
  end
  { MAC/65
   ---------------------------------------------------------------------------}
  else if langIndex = _MAC65 then begin
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += '; Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                'CHRDATA' + IntToStr(i) + ' .BYTE ' +
                SetDataValues(frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ',') +
                #13#10;
      end;
  end
  { CC65
   ---------------------------------------------------------------------------}
  else if langIndex = _CC65 then begin
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += '// Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                'const unsigned char chr' + IntToStr(i) + '[7] = {'#13#10 +
                '  ' + SetDataValues(frmFonts.fldChar, frmFonts.fld,
                                     i, radDataType.ItemIndex, ', ') + '};'#13#10;
      end;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += '// Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                'const char chr' + IntToStr(i) + '[] = {'#13#10 +
                '  ' + SetDataValues(frmFonts.fldChar, frmFonts.fld,
                                     i, radDataType.ItemIndex, ', ') + '};'#13#10;
      end;
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Full character set data statements
 -----------------------------------------------------------------------------}
function TfrmFontSetGen.Example03 : string;
var
  i, j : byte;
  temp : string;
begin
  { Atari BASIC / Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine(_REM) +
//                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Character set data') +
                 CodeLine(_REM);
    for i := 0 to 127 do begin
      //code += IntToStr(lineNum) + ' DATA ' +
      //        SetDataValues(frmFonts.fldChar, frmFonts.fld, i, rgDataType.ItemIndex, ',') + #13#10;
      code.line += CodeLine('DATA ' + SetDataValues(
                              frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ','));
//      SetValues(radLang.ItemIndex, i)
//      Inc(lineNum, 10);
    end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := '; Character set data'#13#10 +
                 'BYTE ARRAY';
    for i := 0 to 127 do begin
      code.line += #13#10'  char' + IntToStr(i) + '=[' +
                   SetDataValues(frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ' ') +
                   ']';
//      SetValues(_ACTION, i)
      if i < 127 then
        code.line += ',';
    end;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '// Character set data'#13#10 +
            'const';
    for i := 0 to 127 do begin
//      code += #13#10;
      code.line += #13#10'  char' + IntToStr(i) + ' : array[0..7] of byte ='#13#10 +
                   '    (' + SetDataValues(
                   frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ', ') + ');';
//      SetValues(_MAD_PASCAL, i)
    end;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
     code.line := ''' Character set data';
     for i := 0 to 127 do begin
//       code += #13#10;
       code.line += #13#10'DATA char' + IntToStr(i) + '() BYTE = ' +
                    SetDataValues(frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ', ');
//       SetValues(_FAST_BASIC, i)
     end;
  end
  { Mad Assembler (MADS)
   ---------------------------------------------------------------------------}
  else if langIndex = _MADS then begin
    code.line := '; Character set data'#13#10 +
                 'fontData';
    for i := 0 to 127 do begin
      code.line += #13#10' .BYTE ' + SetDataValues(
                   frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ',');
//      SetValues(_MADS, i)
    end;
  end
  { MAC/65
   ---------------------------------------------------------------------------}
  else if langIndex = _MAC65 then begin
    code.line := '; Character set data'#13#10 +
                 'FONTDATA';
    for i := 0 to 127 do begin
      code.line += #13#10'  .BYTE ' + SetDataValues(
                   frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ',');
//      SetValues(_MAC65, i)
    end;
  end
  { CC65
   ---------------------------------------------------------------------------}
  else if langIndex = _CC65 then begin
    code.line := '// Character set data'#13#10 +
                 'const unsigned char fontData[127] = {'#13#10;
    j := 0;
    for i := 0 to 127 do begin
      Inc(j);
      code.line += '  ';
//      for j := 0 to 7 do begin
      temp := SetDataValues(frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ', ');
//        SetValues(_CC65, i)
      if (i = 127) and (j = 8) then begin
        code.line += temp;
        j := 0;
      end
      else begin
        code.line += temp;
        if i < 127 then
          code.line += ', ';
      end;

      code.line += #13#10;
    end;
    code.line +='};'#13#10;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    code.line := '// Character set data'#13#10 +
                 'const char fontData[] = {'#13#10;
    j := 0;
    for i := 0 to 127 do begin
      Inc(j);
      code.line += '  ';
      temp := SetDataValues(frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ', ');
      if (i = 127) and (j = 8) then begin
        code.line += temp;
        j := 0;
      end
      else begin
        code.line += temp;
        if i < 127 then
          code.line += ', ';
      end;

      code.line += #13#10;
    end;
    code.line +='};'#13#10;
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Modified characters
 -----------------------------------------------------------------------------}
function TfrmFontSetGen.Example04 : string;
var
  i : byte;
  rtnCodeNum : word = 20000;       // ML starting Atari BASIC line number
  charDataCodeNum : word = 30000;  // Character data starting Atari BASIC line number
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;

    if code.number > 20000 then
      Inc(rtnCodeNum, 1000);

    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Modified characters') +
                 CodeLine(_REM) +
                 CodeLine('DIM MLCODE$(33)') +
                 CodeLine('GOSUB ' + IntToStr(rtnCodeNum)) +
                 CodeLine('TOPMEM=PEEK(106)-4:POKE 106,TOPMEM') +
                 CodeLine('GRAPHICS 0') +
                 CodeLine('CHRAM=TOPMEM*256') +
                 //CodeLine('FOR I=0 TO 1023') +
                 //CodeLine('POKE CHRAM+I,PEEK(57344+I)') +
                 //CodeLine('NEXT I');
                 CodeLine('REM CALL MACHINE LANGUAGE ROUTINE') +
                 CodeLine('X=USR(ADR(MLCODE$),57344,CHRAM,4)');
    if code.number > 30000 then
      Inc(charDataCodeNum, 1000);

    code.line += CodeLine('RESTORE ' + IntToStr(charDataCodeNum));
    code.line += CodeLine('REM Modified characters');
    for i := 0 to 127 do begin
      if frmFonts.charEditIndex[i] = 1 then
        code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE CHRAM+I+' +
                              IntToStr(i) + '*8,CHAR:NEXT I');
    end;
    code.line += CodeLine('POKE 756,TOPMEM');
    code.line += CodeLine('END');
    // Machine language routine
    code.number := rtnCodeNum;
    code.line += SetFastCopyRoutine;
    // Modified character data
    code.number := charDataCodeNum;
    code.line += CodeLine('REM MODIFIED CHARACTER DATA');
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
        code.line += CodeLine('DATA ' + SetDataValues(
                              frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ','));
      end;
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Modified characters') +
                 CodeLine('--') +
                 CodeLine('TOPMEM=PEEK(106)-4:POKE 106,TOPMEM') +
                 CodeLine('GRAPHICS 0%') +
                 CodeLine('CHRAM=TOPMEM*256') +
                 CodeLine('MOVE 57344,CHRAM,1024');
    for i := 0 to 127 do begin
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE CHRAM+I+' + IntToStr(i) +
                              '*8,CHAR:NEXT I');
      end;
    end;
    code.line += CodeLine('REM MODIFY CHARACTER SET POINTER') +
                 CodeLine('POKE 756,TOPMEM') +
                 CodeLine('REM Modified characters');
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
        code.line += CodeLine('DATA ' + SetDataValues(
                                frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ','));
      end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := ';' + _REM_MAD_STUDIO + #13#10 +
                 '; Modified characters'#13#10#13#10 +
                 'BYTE CH=$2FC'#13#10 +
                 'BYTE RAMTOP=$6A'#13#10 +
                 'BYTE CHBAS=$2F4'#13#10#13#10 +
                 'CARD TOPMEM'#13#10#13#10;
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += '; Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                     'BYTE ARRAY char' + IntToStr(i) + '=[' +  //#13#10'  ' +
                     SetDataValues(frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ' ') +
                     ']'#13#10;
//        SetValues(_ACTION, i)
      end;

    code.line += #13#10'PROC MAIN()'#13#10#13#10 +
                 'GRAPHICS(0)'#13#10#13#10 +
                 '; RESERVE MEMORY FOR NEW CHARACTER SET'#13#10 +
                 'TOPMEM=RAMTOP-8'#13#10 +
                 'TOPMEM==*256'#13#10#13#10 +
                 '; VECTOR TO PAGE ADDRESS OF NEW SET'#13#10 +
                 'CHBAS=TOPMEM/256'#13#10#13#10 +
                 '; MOVE NEW CHARACTER SET TO RESERVED MEMORY'#13#10 +
                 'MOVEBLOCK(TOPMEM,57344,1024)'#13#10#13#10 +
                 '; CUSTOM CHARACTER SET DATA'#13#10;
    for i := 0 to 127 do begin
      if frmFonts.charEditIndex[i] = 1 then
        code.line += 'MOVEBLOCK(TOPMEM+' + IntToStr(i) + '*8,char' + IntToStr(i) + ',8)'#13#10;
    end;
    code.line += #13#10'PRINTF("%E%E%EPRESS ANY KEY TO EXIT!%E")'#13#10 +
                 WaitKeyCode(_ACTION) + #13#10 +
                 'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Modified characters'#13#10#13#10 +
                 'uses crt, fastgraph;'#13#10#13#10 +
                 'var'#13#10 +
                 '  topMem, chRAM : word;'#13#10 +
                 '  CHBAS  : byte absolute $2F4;'#13#10 +
                 '  RAMTOP : byte absolute $6A;'#13#10#13#10;
    for i := 0 to 127 do begin
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += '  // Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                     '  char' + IntToStr(i) + ' : array[0..7] of byte = (' +
                     SetDataValues(frmFonts.fldChar, frmFonts.fld,
                                   i, radDataType.ItemIndex, ', ') + ');'#13#10;
//        SetValues(_MAD_PASCAL, i)
      end;

    //code += '  char33 : array[0..7] of byte ='#13#10 +
    //        '    (' + SetValues(0, 33) + ');';
    end;
    code.line += #13#10'begin'#13#10 +
                 '  InitGraph(0);'#13#10#13#10 +
                 '  // Set new character set'#13#10 +
                 '  topMem := RAMTOP - 8;'#13#10 +
                 '  chRAM := topMem shl 8;'#13#10#13#10 +
//                 '  CHBAS := hi(topMem);'#13#10#13#10 +
                 '  // Move new character set to reserved memory'#13#10 +
                 '  Move(pointer(57344), pointer(topMem), 1024);'#13#10#13#10 +
                 '  // Custom character set data'#13#10;
    for i := 0 to 127 do begin
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line +=
         //          '  WriteLn(''Internal value: ' + IntToStr(i) + ''');'#13#10 +
                     '  Move(char' + IntToStr(i) + ', pointer(chRAM + ' + IntToStr(i) + '*8), 8);'#13#10;
      end;
    end;

    code.line += #13#10'  // Set address of new set'#13#10 +
                 '  CHBAS := topMem;'#13#10;

    code.line += #13#10'  Writeln(#$9b#$9b''Press any key to exit!'');' +
                 WaitKeyCode(_MAD_PASCAL) +
                 'end.';
   end
  { FastBasic
   ---------------------------------------------------------------------------}
   else if langIndex = _FAST_BASIC then begin
     code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                  ''' Modified characters'#13#10#13#10;
     for i := 0 to 127 do begin
       if frmFonts.charEditIndex[i] = 1 then begin
         code.line += ''' Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                      'DATA char' + IntToStr(i) + '() BYTE = ' +
                      SetDataValues(frmFonts.fldChar, frmFonts.fld,
                                    i, radDataType.ItemIndex, ', ') + #13#10;
//         SetValues(_FAST_BASIC, i)
       end;
     end;
     code.line += #13#10'TOPMEM = PEEK(106) - 4'#13#10 +
                  'POKE 106, TOPMEM'#13#10 +
                  'GRAPHICS 0'#13#10 +
                  'CHRAM = TOPMEM*256'#13#10 +
                  'MOVE 57344, CHRAM, 1024'#13#10#13#10 +
                  ''' Custom character set data'#13#10;
    for i := 0 to 127 do begin
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line +=
//            '? "Internal value: ' + IntToStr(i) + '"'#13#10 +
//            'MOVE CHRAM + ' + IntToStr(i) + '*8, ADR(char' + IntToStr(i) + '), 8'#13#10;
                   'MOVE ADR(char' + IntToStr(i) + '), CHRAM + ' + IntToStr(i) + '*8, 8'#13#10;
      end;
    end;
    code.line += #13#10''' MODIFY CHARACTER SET POINTER'#13#10 +
                 'POKE 756, TOPMEM'#13#10#13#10 +
                 '? "Press any key to exit!"' +
                 WaitKeyCode(_FAST_BASIC);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Load character set from file
 -----------------------------------------------------------------------------}
function TfrmFontSetGen.Example05 : string;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Load character set from file') +
                 CodeLine(_REM);
    code.line += CodeLine('TOPMEM=PEEK(106)-8');
    code.line += CodeLine('POKE 106,TOPMEM');
//    code.line += CodeLine('CHRAM=TOPMEM*256');
    code.line += CodeLine('GRAPHICS 0');
    code.line += CodeLine('FOR I=0 TO 255') +
                 CodeLine('IF I>32 AND I<125 THEN ? CHR$(I);') +
                 CodeLine('NEXT I');
//    code.line += CodeLine('CLOSE #1');
    code.line += SetCharSet(langIndex, editFontname.Text, true);
    code.line += WaitKeyCode(langIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Load character set from file') +
                 CodeLine('--');
    code.line += CodeLine('TOPMEM=PEEK(106)-8') +
                 CodeLine('POKE 106,TOPMEM') +
                 CodeLine('CHRAM=TOPMEM*256') +
                 CodeLine('GRAPHICS %0') +
                 CodeLine('FOR I=%0 TO 255') +
                 CodeLine('IF I>32 AND I<125 THEN ? CHR$(I);') +
                 CodeLine('NEXT I') +
                 CodeLine('CLOSE #%1') +
                 SetCharSet(langIndex, editFontname.Text, true) +
                 WaitKeyCode(langIndex);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Load character set from file'#13#10#13#10 +
                 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt, Cio;'#13#10#13#10 +
                 'var'#13#10 +
                 '  topMem, chRAM : word;'#13#10 +
                 '  CHBAS  : byte absolute $2F4;'#13#10 +
                 '  RAMTOP : byte absolute $6A;'#13#10 +
                 '  i, data : byte;'#13#10#13#10 +
                 'begin'#13#10 +
                 '  // Set new character set'#13#10 +
                 '  topMem := RAMTOP - 12;'#13#10 +
                 '  chRAM := topMem shl 8;'#13#10 +
                 '  InitGraph(0);'#13#10 +
                 '  // Write to screen'#13#10 +
                 '  for i := 0 to 255 do begin'#13#10 +
                 '    if (i > 32) and (i < 125) then Write(chr(i));'#13#10 +
                 '  end;'#13#10#13#10 +
                 '  Cls(1);'#13#10;
    code.line += SetCharSet(langIndex, editFontname.Text, true) +
                 WaitKeyCode(langIndex) +
                 'end.';
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := ';' + _REM_MAD_STUDIO + #13#10 +
                 '; Load character set from file'#13#10#13#10 +
                 'BYTE ch=$2FC'#13#10 +
                 'CARD i'#13#10;
    code.line += 'BYTE RAMTOP=$6A'#13#10 +
                 'BYTE CHBAS=$2F4'#13#10 +
                 'CARD TOPMEM, CHRAM'#13#10 +
                 'BYTE ARRAY FONT(1023)'#13#10#13#10 +
                 'PROC Main()'#13#10#13#10 +
                 '; RESERVE MEMORY FOR NEW CHARACTER SET'#13#10 +
                 'TOPMEM=RAMTOP-12'#13#10 +
                 'CHRAM=TOPMEM LSH 8'#13#10#13#10 +
                 'Put(125)'#13#10 +
                 '; DEMONSTRATION'#13#10 +
                 'FOR i=0 TO 255'#13#10 +
                 'DO'#13#10 +
                 '  IF (i > 32) AND (i < 125) THEN'#13#10 +
                 '    PUT(i)'#13#10 +
                 '  FI'#13#10 +
                 'OD'#13#10#13#10 +
                 '; Set up channel 2 for screen'#13#10 +
                 'Close(2)'#13#10;
    code.line += SetCharSet(langIndex, editFontname.Text, true) +
                 WaitKeyCode(langIndex) + #13#10 +
                 'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                 ''' Load character set from file'#13#10#13#10;
    code.line += 'TOPMEM = PEEK(106) - 4'#13#10 +
                 'POKE 106, TOPMEM'#13#10 +
                 'CHRAM = TOPMEM*256'#13#10 +
                 'GRAPHICS 0'#13#10#13#10 +
                 'FOR I=0 TO 255'#13#10 +
                 '  IF I>32 AND I<125 THEN ? CHR$(I);'#13#10 +
                 'NEXT'#13#10 +
                 'CLOSE #1'#13#10;
    code.line += SetCharSet(langIndex, editFontname.Text, true);
    code.line += WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

procedure TfrmFontSetGen.CreateCodeProc(Sender: TObject);
begin
  if not isCreate then
    CreateCode;
end;

procedure TfrmFontSetGen.ExamplesProc(Sender: TObject);
begin
  if listExamples.ListBox.ItemIndex < 0 then exit;

  btnAtariBASIC.Enabled := listings[listExamples.ListBox.ItemIndex, 0];
  btnTurboBasicXL.Enabled := listings[listExamples.ListBox.ItemIndex, 1];
  btnMadPascal.Enabled := listings[listExamples.ListBox.ItemIndex, 2];
  btnEffectus.Enabled := listings[listExamples.ListBox.ItemIndex, 3];
  btnFastBasic.Enabled := listings[listExamples.ListBox.ItemIndex, 4];
  btnKickC.Enabled := listings[listExamples.ListBox.ItemIndex, 5];
  btnMads.Enabled := listings[listExamples.ListBox.ItemIndex, 6];
  btnMac65.Enabled := listings[listExamples.ListBox.ItemIndex, 7];
  btnCC65.Enabled := listings[listExamples.ListBox.ItemIndex, 8];

  editFontname.Visible := listExamples.ListBox.ItemIndex = 4;
  editFontname.Enabled := editFontname.Visible;

  CreateCode;
end;

procedure TfrmFontSetGen.LangProc(Sender : TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmFontSetGen.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmFontSetGen.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

procedure TfrmFontSetGen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

