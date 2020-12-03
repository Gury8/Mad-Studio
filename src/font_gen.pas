{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Character set editor - source code generator
}
unit font_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  lcltype, BCTrackbarUpdown, BCListBox, BCMDButton, BCMaterialDesignButton, Windows,
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
    btnMadPascal1 : TBCMDButton;
    btnMadPascal2 : TBCMDButton;
    btnMadPascal3 : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    editFilename: TLabeledEdit;
    editLineStep : TBCTrackbarUpdown;
    editStartLine : TBCTrackbarUpdown;
    lblLineStep1 : TLabel;
    lblStartLine1 : TLabel;
    memo : TMemo;
    panelLang : TBCPaperPanel;
    radDataType : TRadioGroup;
    StaticText1 : TStaticText;
    StaticText2 : TStaticText;
    StaticText3 : TStaticText;
    ListExamples: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditorProc(Sender: TObject);
    procedure LangProc(Sender : TObject);
    procedure CreateCodeProc(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    procedure ExamplesClick(Sender: TObject);
    procedure editStartLineMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure btnCopyToEditorMouseEnter(Sender : TObject);
    procedure btnCopyToEditorMouseLeave(Sender : TObject);
    procedure btnCloseMouseEnter(Sender : TObject);
    procedure btnCloseMouseLeave(Sender : TObject);
  private
    { private declarations }
    listings : TListings;
    procedure CreateCode;
    function Example01 : string;
    function Example02 : string;
    function Example02a : string;
    function Example03 : string;
    function Example04 : string;
  public
    { public declarations }
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
  // Example 1
  listings[0, 0] := true;
  listings[0, 1] := true;
  listings[0, 2] := true;
  listings[0, 3] := true;
  listings[0, 4] := true;
  listings[0, 5] := true;
  listings[0, 6] := true;
  listings[0, 7] := true;

  // Example 2
  listings[1, 0] := true;
  listings[1, 1] := true;
  listings[1, 2] := true;
  listings[1, 3] := true;
  listings[1, 4] := true;
  listings[1, 5] := true;
  listings[1, 6] := true;
  listings[1, 7] := true;

  // Example 3
  listings[2, 0] := true;
  listings[2, 1] := true;
  listings[2, 2] := true;
  listings[2, 3] := true;
  listings[2, 4] := true;
  listings[2, 5] := true;
  listings[2, 6] := true;
  listings[2, 7] := true;

  // Example 4
  listings[3, 0] := true;
  listings[3, 1] := true;
  listings[3, 2] := true;
  listings[3, 3] := true;
  listings[3, 4] := true;

  // Example 5
  listings[4, 0] := true;
  listings[4, 1] := true;
  listings[4, 2] := true;
  listings[4, 3] := true;
  listings[4, 4] := true;

  SetTrackBarUpDown(editStartLine, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editLineStep, $00DDDDDD, clWhite);
end;

procedure TfrmFontSetGen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;

  //if frmFonts.filename = '' then
  //  editFilename.Text:= 'H1:DEFAULT.FNT'
  //else
  //  editFilename.Text:= 'H1:' + ExtractFileName(frmFonts.filename);

  langIndex := 0;
  ListExamples.Selected := ListExamples.Items.GetFirstNode;
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

  case ListExamples.selected.AbsoluteIndex of
    1: code := Example01;
    2: code := Example02a;
    3: code := Example02;
    4: code := Example03;
    5: code := Example04;
  end;

  memo.Lines.Add(code);

  // Set cursor position at the top of memo object
  memo.SelStart := 0;
  memo.SelLength := 0;
  SendMessage(memo.Handle, EM_SCROLLCARET, 0, 0);
end;

// Selected character data statements
function TfrmFontSetGen.Example01 : string;
begin
  { Atari BASIC / Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('DATA REM CHR$(' + AtasciiCode(frmFonts.offs) + ')');
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
                 ']' + #13#10;
//    SetValues(_ACTION, 255)
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := 'const' + #13#10 +
            '  charData : array[0..7] of byte =' + #13#10 +
            '    (' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex, ', ') + ');' + #13#10;
    // SetValues(_MAD_PASCAL, 255)
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := 'DATA charData() BYTE = ' + #13#10 +
            '  ' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex, ', ') + #13#10;
//    SetValues(_FAST_BASIC, 255)
  end
  { Mad Assembler
   ---------------------------------------------------------------------------}
  else if langIndex = _MADS then begin
    code.line := 'chrData .BYTE ' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex, ',') + #13#10;
//    SetValues(_MADS, 255)
  end
  { MAC/65
   ---------------------------------------------------------------------------}
  else if langIndex = _MAC65 then begin
    code.line := 'CHRDATA .BYTE ' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex, ',') + #13#10;
//    SetValues(_MAC65, 255)
  end
  { CC65
   ---------------------------------------------------------------------------}
  else if langIndex = _CC65 then begin
    code.line := 'const unsigned char chr[7] = {' + #13#10 +
            '  ' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex, ', ') + #13#10 +
            '};' + #13#10;
//    SetValues(_CC65, 255)
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    code.line := 'const char chr[] = {' + #13#10 +
            '  ' + SetDataValues(frmFonts.fldChar, frmFonts.fld, 255, radDataType.ItemIndex, ', ') + #13#10 +
            '};' + #13#10;
  end;

  result := code.line;
end;

// Modified characters
function TfrmFontSetGen.Example02a : string;
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
//     code := ''' Modified characters' + #13#10;
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

//// Picture data
//const unsigned char picData[3839] = {
//  0, 0, 0, 48, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//  0, 0, 0, 48, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//};

// Full character set data statements
function TfrmFontSetGen.Example02 : string;
var
  i, j : byte;
//  lineNum : word = 10;
  temp : string;
begin
  { Atari BASIC / Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM *******************************') +
                 CodeLine('REM CHARACTER SET DATA') +
                 CodeLine('REM *******************************');
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
      code.line += #13#10'  char' + IntToStr(i) + ' : array[0..7] of byte =' + #13#10 +
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
      else
        code.line += temp + ', ';

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
      else
        code.line += temp + ', ';

      code.line += #13#10;
    end;
    code.line +='};'#13#10;
  end;

  result := code.line;
end;

// Modified characters
function TfrmFontSetGen.Example03 : string;
var
  i : byte;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM *******************************') +
                 CodeLine('REM MODIFIED CHARACTERS') +
                 CodeLine('REM *******************************') +
                 CodeLine('NMEMTOP=PEEK(106)-4') +
                 CodeLine('POKE 106,NMEMTOP') +
                 CodeLine('GRAPHICS 0') +
                 CodeLine('CHRAM=NMEMTOP*256') +
                 CodeLine('FOR I=0 TO 1023') +
                 CodeLine('POKE CHRAM+I,PEEK(57344+I)') +
                 CodeLine('NEXT I');
//    count := 100;
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE CHRAM+I+' +
                              IntToStr(i) + '*8,CHAR:NEXT I');
//        Inc(count, 10);
      end;

    code.line += CodeLine('REM MODIFY CHARACTER SET POINTER') +
                 CodeLine('POKE 756,NMEMTOP') +
                 CodeLine('REM MODIFIED CHARACTER DATA');
//    Inc(count, 30);
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
//        Inc(count, 10);
        code.line += CodeLine('DATA ' + SetDataValues(
                                frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ','));
//        SetValues(_ATARI_BASIC, i)
//        Inc(count, 10);
      end;
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM MODIFIED CHARACTERS') +
                 CodeLine('--') +
                 CodeLine('NMEMTOP=PEEK(106)-4') +
                 CodeLine('POKE 106,NMEMTOP') +
                 CodeLine('GRAPHICS 0%') +
                 CodeLine('CHRAM=NMEMTOP*256') +
                 CodeLine('MOVE 57344,CHRAM,1024');
//    count := 100;
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE CHRAM+I+' + IntToStr(i) +
                              '*8,CHAR:NEXT I');
//        Inc(count, 10);
      end;

    code.line += CodeLine(' REM MODIFY CHARACTER SET POINTER') +
                 CodeLine(' POKE 756,NMEMTOP') +
                 CodeLine(' REM MODIFIED CHARACTER DATA');
//    Inc(count, 30);
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
//        Inc(count, 10);
        code.line += CodeLine('DATA ' + SetDataValues(
                                frmFonts.fldChar, frmFonts.fld, i, radDataType.ItemIndex, ','));
//        SetValues(_ATARI_BASIC, i)
//        Inc(count, 10);
      end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := '; Modified characters'#13#10#13#10 +
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
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then
        code.line += 'MOVEBLOCK(TOPMEM+' + IntToStr(i) + '*8,char' + IntToStr(i) + ',8)' + #13#10;

   code.line += #13#10 +
                'PRINTF("%E%E%EPRESS ANY KEY TO EXIT!%E")'#13#10 +
                //'CH=255'#13#10 +
                //'DO UNTIL CH<255 OD'#13#10 +
                //'CH=255'#13#10 +
                WaitKeyCode(_ACTION) + #13#10 +
                'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '// Modified characters'#13#10#13#10 +
            'uses crt, graph;'#13#10#13#10 +
            'var'#13#10 +
//            '  font : array[0..1023] of byte;'#13#10 +
            '  topMem : word;'#13#10 +
            '  CHBAS  : byte absolute $2F4;'#13#10 +
            '  RAMTOP : byte absolute $6A;'#13#10#13#10;
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line += '  // Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                '  char' + IntToStr(i) + ' : array[0..7] of byte = (' +
                SetDataValues(frmFonts.fldChar, frmFonts.fld,
                              i, radDataType.ItemIndex, ', ') + ');'#13#10;
//        SetValues(_MAD_PASCAL, i)
      end;

    //code += '  char33 : array[0..7] of byte ='#13#10 +
    //        '    (' + SetValues(0, 33) + ');';

    code.line += #13#10'begin'#13#10 +
                 '  InitGraph(0);'#13#10#13#10 +
                 '  // Set new character set'#13#10 +
                 '  topMem := RAMTOP - 8;'#13#10 +
                 '  topMem := topMem shl 8;'#13#10 +
                 '  CHBAS := hi(topMem);'#13#10#13#10 +
                 '  // Move new character set to reserved memory'#13#10 +
                 '  Move(pointer(57344), pointer(topMem), 1024);'#13#10#13#10 +
                 '  // Custom character set data' + #13#10;
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line +=
  //          '  WriteLn(''Internal value: ' + IntToStr(i) + ''');' + #13#10 +
            '  Move(char' + IntToStr(i) + ', pointer(topMem + ' + IntToStr(i) + '*8), 8);' + #13#10;
      end;

    //code += #13#10 +
    //        '  WriteLn(''Internal value: ' + IntToStr(33) + ''');' + #13#10 +
    //        '  Move(char' + IntToStr(33) + ',pointer(topMem+' + IntToStr(33) + '*8),8);' + #13#10;

    code.line += #13#10 +
            '  Writeln(#$9b#$9b''Press any key to exit!'');' + #13#10 +
            //'  repeat until KeyPressed;' + #13#10 +
            //'  ReadKey;' + #13#10 +
            WaitKeyCode(_MAD_PASCAL) +
            'end.';
   end
  { FastBasic
   ---------------------------------------------------------------------------}
   else if langIndex = _FAST_BASIC then begin
     code.line := ''' Modified characters'#13#10#13#10;
     for i := 0 to 127 do
       if frmFonts.charEditIndex[i] = 1 then begin
         code.line += ''' Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                 'DATA char' + IntToStr(i) + '() BYTE = ' +
                 SetDataValues(frmFonts.fldChar, frmFonts.fld,
                               i, radDataType.ItemIndex, ', ') + #13#10;
//         SetValues(_FAST_BASIC, i)
       end;

     code.line += #13#10'NMEMTOP = PEEK(106) - 4'#13#10 +
             'POKE 106, NMEMTOP'#13#10 +
             'GRAPHICS 0'#13#10 +
             'CHRAM = NMEMTOP*256'#13#10 +
             'MOVE 57344, CHRAM, 1024'#13#10#13#10 +
             ''' Custom character set data'#13#10;
    for i := 0 to 127 do
      if frmFonts.charEditIndex[i] = 1 then begin
        code.line +=
  //          '? "Internal value: ' + IntToStr(i) + '"'#13#10 +
//            'MOVE CHRAM + ' + IntToStr(i) + '*8, ADR(char' + IntToStr(i) + '), 8' + #13#10;
        'MOVE ADR(char' + IntToStr(i) + '), CHRAM + ' + IntToStr(i) + '*8, 8' + #13#10;
      end;

    code.line += #13#10''' MODIFY CHARACTER SET POINTER'#13#10 +
            'POKE 756, NMEMTOP'#13#10#13#10 +
            '? "Press any key to exit!"'#13#10 +
            //'REPEAT'#13#10 +
            //'UNTIL Key()';
            WaitKeyCode(_FAST_BASIC);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

// Load character set from file
function TfrmFontSetGen.Example04 : string;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM *******************************') +
                 CodeLine('REM LOAD CHARACTER SET FROM FILE') +
                 CodeLine('REM *******************************') +
                 CodeLine('NMEMTOP=PEEK(106)-4') +
                 CodeLine('POKE 106,NMEMTOP') +
                 CodeLine('GRAPHICS 0') +
                 CodeLine('CHRAM=NMEMTOP*256') +
                 CodeLine('FOR I=0 TO 255') +
                 CodeLine('IF I>32 AND I<125 THEN ? CHR$(I);') +
                 CodeLine('NEXT I') +
                 CodeLine('REM LOAD FONT SET FROM FILE') +
                 CodeLine('OPEN #1,4,0,"' + editFilename.Text + '"') +
                 CodeLine('FOR I=0 TO 1023') +
                 CodeLine('GET #1,N:POKE CHRAM+I,N') +
                 CodeLine('NEXT I') +
                 CodeLine('CLOSE #1') +
                 CodeLine('REM MODIFY CHARACTER SET POINTER') +
                 CodeLine('POKE 756,NMEMTOP');
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM LOAD CHARACTER SET FROM FILE') +
                 CodeLine('--') +
                 CodeLine('NMEMTOP=PEEK(106)-4') +
                 CodeLine('POKE 106,NMEMTOP') +
                 CodeLine('GRAPHICS %0') +
                 CodeLine('CHRAM=NMEMTOP*256') +
                 CodeLine('FOR I=%0 TO 255') +
                 CodeLine('IF I>32 AND I<125 THEN ? CHR$(I);') +
                 CodeLine('NEXT I') +
                 CodeLine('REM LOAD FONT SET FROM FILE') +
                 CodeLine('CLOSE #%1') +
                 CodeLine('OPEN #%1,4,%0,"' + editFilename.Text + '"') +
                 CodeLine('BGET #%1,CHRAM,1024') +
                 CodeLine('CLOSE #%1') +
                 CodeLine('REM MODIFY CHARACTER SET POINTER') +
                 CodeLine('POKE 756,NMEMTOP');
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := '; LOAD CHARACTER SET FROM FILE'#13#10#13#10 +
                 'BYTE CH=$2FC'#13#10 +
                 'BYTE RAMTOP=$6A'#13#10 +
                 'BYTE CHBAS=$2F4'#13#10 +
                 'BYTE DATA'#13#10 +
                 'CARD TOPMEM'#13#10 +
                 'CARD N'#13#10 +
                 'BYTE ARRAY FONT(1023)'#13#10#13#10 +
                 'PROC MAIN()'#13#10#13#10 +
                 '; TEXT MODE 0'#13#10 +
                 'GRAPHICS(0)'#13#10#13#10 +
                 '; RESERVE MEMORY FOR NEW CHARACTER SET'#13#10 +
                 'TOPMEM=RAMTOP-8'#13#10 +
                 'TOPMEM==*256'#13#10#13#10 +
                 '; DEMONSTRATION'#13#10 +
                 'FOR N=0 TO 255'#13#10 +
                 'DO'#13#10 +
                 '  IF (N > 32) AND (N < 125) THEN'#13#10 +
                 '    PUT(N)'#13#10 +
                 '  FI'#13#10 +
                 'OD'#13#10#13#10 +
                 '; CLOSE CHANNEL 2 BEFORE REUSING IT'#13#10 +
                 'CLOSE(2)'#13#10#13#10 +
                 '; OPEN CHANNEL 2 FOR OUR FONT'#13#10 +
                 'OPEN(2,"' + editFilename.Text + '",4,0)'#13#10#13#10 +
                 '; READ FONT DATA BYTE BY BYTE'#13#10 +
                 'FOR N=0 TO 1023'#13#10 +
                 'DO'#13#10 +
                 '  DATA=GETD(2)'#13#10 +
                 '  FONT(N)=DATA'#13#10 +
                 'OD'#13#10#13#10 +
                 '; CLOSE CHANNEL 2'#13#10 +
                 'CLOSE(2)'#13#10#13#10 +
                 '; VECTOR TO PAGE ADDRESS OF NEW SET'#13#10 +
                 'CHBAS=TOPMEM/256'#13#10#13#10 +
                 '; COPY FONT SET TO NEW ADDRESS'#13#10 +
                 'MOVEBLOCK(TOPMEM,FONT,1024)'#13#10#13#10 +
                 'PRINTF("%E%E%EPRESS ANY KEY TO EXIT!%E")'#13#10 +
                 //'CH=255'#13#10 +
                 //'DO UNTIL CH<255 OD'#13#10 +
                 //'CH=255'#13#10 +
                 WaitKeyCode(_ACTION) +
                 #13#10'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '// Load character set from file'#13#10#13#10 +
                'uses crt, graph;'#13#10#13#10 +
                'var'#13#10 +
                '  f : file;'#13#10 +
                '  filename : TString;'#13#10 +
                '  n : byte;'#13#10 +
                '  font : array[0..1023] of byte;'#13#10 +
                '  topMem : word;'#13#10 +
                '  CHBAS  : byte absolute $2F4;'#13#10 +
                '  RAMTOP : byte absolute $6A;'#13#10 +
                #13#10 +
                'begin'#13#10 +
                '  InitGraph(0);'#13#10 +
                #13#10 +
                '  // Set new character set'#13#10 +
                '  topMem := RAMTOP - 8;'#13#10 +
                '  topMem := topMem shl 8;'#13#10 +
                '  CHBAS := hi(topMem);'#13#10 +
                #13#10 +
                '  // Write to screen'#13#10 +
                '  for n := 0 to 255 do begin'#13#10 +
                '    if (n > 32) and (n < 125) then Write(chr(n));'#13#10 +
                '  end;'#13#10 +
                #13#10 +
                '  // Load file'#13#10 +
                '  filename := ''' + editFilename.Text + ''';'#13#10 +
                '  Assign(f, filename);'#13#10 +
                '  Reset(f, 1);'#13#10 +
                '  BlockRead(f, font, 1024);'#13#10 +
                '  Close(f);'#13#10 +
                #13#10 +
                '  // Move new character set to reserved memory'#13#10 +
                '  Move(font, pointer(topMem), SizeOf(font));'#13#10 +
                #13#10 +
                '  Writeln(#$9b#$9b''Press any key to exit!'');'#13#10 +
                //'  repeat until KeyPressed;'#13#10 +
                //'  ReadKey;'#13#10 +
                WaitKeyCode(_MAD_PASCAL) +
                'end.'
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := ''' Load character set from file'#13#10#13#10 +
                 'NMEMTOP = PEEK(106) - 4'#13#10 +
                 'POKE 106, NMEMTOP'#13#10 +
                 'GRAPHICS 0'#13#10 +
                 'CHRAM = NMEMTOP*256'#13#10 +
                 'FOR I=0 TO 255'#13#10 +
                 '  IF I>32 AND I<125 THEN ? CHR$(I);'#13#10 +
                 'NEXT'#13#10 +
                 ''' Load character set from file'#13#10#13#10 +
                 'CLOSE #1'#13#10 +
                 'OPEN #1, 4, 0, "H1:DEFAULT.FNT"'#13#10 +
                 'BGET #1, CHRAM, 1024'#13#10 +
                 'CLOSE #1'#13#10#13#10 +
                 ''' Modify character set pointer'#13#10 +
                 'POKE 756, NMEMTOP'#13#10 +
                 WaitKeyCode(_FAST_BASIC);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

//Data values for selected character as data statements
//Data values for full character set as data statements
//Modified character set with loader
//Load character set from file

procedure TfrmFontSetGen.CreateCodeProc(Sender: TObject);
begin
  CreateCode;
end;

procedure TfrmFontSetGen.ExamplesClick(Sender: TObject);
//var
//  i : byte;
begin
  (*
//  showmessage(inttostr(ListExamples.selected.AbsoluteIndex));
  for i := 0 to radLang.Items.Count - 1 do begin
//    radLang.Controls[i].Enabled := listings[listExamples.ItemIndex , i];
////    radLang.Controls[i].Visible := listings[listExamples.ItemIndex, i];
    radLang.Controls[i].Enabled := listings[ListExamples.selected.AbsoluteIndex - 1, i];
  end;
  radLang.ItemIndex := langIndex;
  *)
  editFilename.Visible := ListExamples.selected.AbsoluteIndex = 5;
  editFilename.Enabled := editFilename.Visible;
  CreateCode;
end;

procedure TfrmFontSetGen.editStartLineMouseUp(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  CreateCode;
end;

procedure TfrmFontSetGen.LangProc(Sender : TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmFontSetGen.btnCopyToEditorMouseEnter(Sender : TObject);
begin
  btnCopyToEditor.NormalColor := $00CECECE;
  btnCopyToEditor.NormalColorEffect := clWhite;
end;

procedure TfrmFontSetGen.btnCopyToEditorMouseLeave(Sender : TObject);
begin
  btnCopyToEditor.NormalColor := clWhite;
  btnCopyToEditor.NormalColorEffect := clSilver;
end;

procedure TfrmFontSetGen.btnCloseMouseEnter(Sender : TObject);
begin
  btnClose.NormalColor := $00CECECE;
  btnClose.NormalColorEffect := clWhite;
end;

procedure TfrmFontSetGen.btnCloseMouseLeave(Sender : TObject);
begin
  btnClose.NormalColor := clWhite;
  btnClose.NormalColorEffect := clSilver;
end;

procedure TfrmFontSetGen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

