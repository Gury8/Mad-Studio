{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Antic mode 4 & 5 editor - source code generator
}
unit antic4_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, lcltype, Spin, Windows,
  common;

type
  { TfrmAntic4Gen }
  TfrmAntic4Gen = class(TForm)
    Button2: TButton;
    btnCopyToEditor: TButton;
    chkTextWindow: TCheckBox;
    editFilename: TLabeledEdit;
    editFontName: TLabeledEdit;
    editLineStep : TSpinEdit;
    editStartLine : TSpinEdit;
    GroupBox1: TGroupBox;
    boxStartLine : TGroupBox;
    Label7: TLabel;
    lblLineStep : TLabel;
    lblStartLine : TLabel;
    memo: TMemo;
    radLang: TRadioGroup;
    rgDataType: TRadioGroup;
    tvExamples: TTreeView;
    procedure editStartLineChange(Sender : TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditorProc(Sender: TObject);
    procedure tvExamplesProc(Sender: TObject);
//    procedure radLangProc(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
  private
    { private declarations }
    listings : TListings;
    anticMode : string;
    procedure CreateCode;
//    function SetValues(index, offset : byte) : string;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
    function Example04 : string;
    function Example05 : string;
    function Example06 : string;
  public
    { public declarations }
  end;

var
  frmAntic4Gen: TfrmAntic4Gen;

implementation

{$R *.lfm}

uses
  antic4, src_editor, code_lib, lib;

{ TfrmAntic4Gen }

procedure TfrmAntic4Gen.FormCreate(Sender: TObject);
begin
  // Example 1
  listings[0, 0] := true;
  listings[0, 1] := true;
  listings[0, 2] := true;
  listings[0, 3] := true;
  listings[0, 4] := true;

  // Example 2
  listings[1, 0] := true;
  listings[1, 1] := true;
  listings[1, 2] := true;
  listings[1, 3] := true;
  listings[1, 4] := true;

  // Example 3
  listings[2, 0] := true;
  listings[2, 1] := true;
  listings[2, 2] := true;
  listings[2, 3] := true;
  listings[2, 4] := true;

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

  // Example 6
  listings[5, 0] := true;
  listings[5, 1] := true;
  listings[5, 2] := true;
  listings[5, 3] := true;
  listings[5, 4] := true;

  tvExamples.Selected := tvExamples.Items.GetFirstNode;
  anticMode := IntToStr(frmAntic4.textMode);
  langIndex := 0;
end;

procedure TfrmAntic4Gen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;

  editFilename.Text:= 'H1:' + ExtractFileName(frmAntic4.filename);

  if frmAntic4.fontName = '' then
    editFontName.Text:= 'H1:DEFAULT.FNT'
  else
    editFontName.Text:= 'H1:' + ExtractFileName(frmAntic4.fontName);
end;

procedure TfrmAntic4Gen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmAntic4Gen.editStartLineChange(Sender : TObject);
begin
  CreateCode;
end;

procedure TfrmAntic4Gen.tvExamplesProc(Sender: TObject);
var
  i : byte;
begin
  for i := 0 to radLang.Items.Count - 1 do begin
    //radLang.Controls[i].Enabled := listings[listExamples.ItemIndex, i];
    radLang.Controls[i].Enabled := listings[tvExamples.selected.AbsoluteIndex - 1, i];
  end;
  radLang.ItemIndex := langIndex;
  editFilename.Visible := tvExamples.selected.AbsoluteIndex >= 5;
  editFilename.Enabled := editFilename.Visible;
  editFontName.Visible := tvExamples.selected.AbsoluteIndex = 6;
  editFontName.Enabled := editFontName.Visible;

  CreateCode;
end;

procedure TfrmAntic4Gen.CopyToEditorProc(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor03(Sender);
  Close;
end;

procedure TfrmAntic4Gen.CreateCode;
var
  code : string;
begin
  langIndex := radLang.ItemIndex;

  boxStartLine.Enabled := langIndex < 2;
  boxStartLine.Visible := boxStartLine.Enabled;

  if langIndex = 0 then begin
    rgDataType.ItemIndex := 0;
    TRadioButton(rgDataType.Controls[1]).Enabled := false;
  end
  else
    TRadioButton(rgDataType.Controls[1]).Enabled := true;

  memo.Lines.Clear;

  case tvExamples.selected.AbsoluteIndex of
    1: code := Example01;
    2: code := Example02;
    3: code := Example03;
    4: code := Example04;
    5: code := Example05;
    6: code := Example06;
  end;

  memo.Lines.Add(code);

  // Set cursor position at the top of memo object
  memo.SelStart := 0;
  memo.SelLength := 0;
  SendMessage(memo.Handle, EM_SCROLLCARET, 0, 0);
end;

// Data values for selected character
function TfrmAntic4Gen.Example01 : string;
begin
  { Atari BASIC / Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if radLang.ItemIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('DATA REM CHR$(' + AtasciiCode(frmAntic4.offs) + ')');
    code.line += CodeLine('DATA ' + SetDataValues(
                          frmAntic4.fldChar, frmAntic4.fldFontSet, 255, rgDataType.ItemIndex, ','));
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    code.line := 'BYTE ARRAY CHARDATA=[' +  //#13#10 +
                 SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                               255, rgDataType.ItemIndex, ' ') + ']'#13#10;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    code.line := 'const'#13#10 +
                 '  charData : array[0..7] of byte ='#13#10 +
                 '    (' + SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                    255, rgDataType.ItemIndex, ', ') + ');'#13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    code.line := 'DATA charData() BYTE = '#13#10 +
                 '  ' + SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                 255, rgDataType.ItemIndex, ', ') + #13#10;
  end;

  result := code.line;
end;

// Data values for modified characters
function TfrmAntic4Gen.Example02 : string;
var
  i : byte;
begin
  code.line := '';

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (radLang.ItemIndex = _ATARI_BASIC) or (radLang.ItemIndex = _TURBO_BASIC_XL) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then begin
        code.line += CodeLine(' REM CHR$(' + AtasciiCode(i) + ')');
        code.line += CodeLine(' DATA ' + SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                              i, rgDataType.ItemIndex, ','));
      end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then begin
       code.line += '; Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
               'BYTE ARRAY char' + IntToStr(i) + '=[' +  //#13#10'  ' +
               SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                             i, rgDataType.ItemIndex, ' ') + ']'#13#10;
     end;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then begin
       code.line += '  // Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
               '  char' + IntToStr(i) + ' : array[0..7] of byte = (' +
               SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                             i, rgDataType.ItemIndex, ', ') + ');'#13#10;
     end;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then begin
       code.line += ''' Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
               'DATA char' + IntToStr(i) + '() BYTE = ' +
               SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                             i, rgDataType.ItemIndex, ', ') + #13#10;
//         SetValues(_FAST_BASIC, i)
     end;
  end;

  result := code.line;
end;

// Screen data values
function TfrmAntic4Gen.Example03 : string;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (radLang.ItemIndex = _ATARI_BASIC) or (radLang.ItemIndex = _TURBO_BASIC_XL) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := DataValues(_ATARI_BASIC, frmAntic4.fldAtascii, code.number, code.step,
                       frmAntic4.modeHeight, 20, rgDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then
    code.line := DataValues(_ACTION, frmAntic4.fldAtascii,
              frmAntic4.modeHeight, 20, frmAntic4.modeSize, rgDataType.ItemIndex)
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then
    code.line := DataValues(_MAD_PASCAL, frmAntic4.fldAtascii,
              frmAntic4.modeHeight, 20, frmAntic4.modeSize, rgDataType.ItemIndex)
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then
    code.line := DataValues(_FAST_BASIC, frmAntic4.fldAtascii,
              frmAntic4.modeHeight, 20, frmAntic4.modeSize, rgDataType.ItemIndex);

  result := code.line;
end;

// Modified characters with screen loader
function TfrmAntic4Gen.Example04 : string;
var
  i : byte;
  strTextWindow : string = '';
  grMode : string;
begin
  grMode := IntToStr(frmAntic4.textMode + 8);
  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if radLang.ItemIndex = _ATARI_BASIC then begin
//    lineNum := editStartLine.Value;
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM *******************************') +
            CodeLine('REM MODIFIED CHARACTERS') +
            CodeLine('REM IN ANTIC MODE ' + anticMode) +
            CodeLine('REM *******************************') +
            CodeLine('NMEMTOP=PEEK(106)-16') +
            CodeLine('POKE 106,NMEMTOP') +
            CodeLine('GRAPHICS ' + grMode + strTextWindow) +
            CodeLine('CHRAM=NMEMTOP*256') +
            CodeLine('OLDVAL=PEEK(559):POKE 559,0') +
            CodeLine('FOR I=0 TO 1023') +
            CodeLine('POKE CHRAM+I,PEEK(57344+I)') +
            CodeLine('NEXT I');
    // Modify characters
//    lineNum := 90;
    code.line += CodeLine('REM MODIFY SOME CHARACTERS');
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then
        code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE CHRAM+I+' + IntToStr(i) + '*8,CHAR:NEXT I');

    code.line += CodeLine('REM MODIFY CHARACTER SET POINTER') +
            CodeLine('POKE 756,NMEMTOP') +
            CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
            CodeLine('POKE 559,OLDVAL') +
            CodeLine('FOR I=0 TO ' + IntToStr(frmAntic4.modeSize - 1)) +
            CodeLine('READ BYTE:POKE SCR+I,BYTE') +
            CodeLine('NEXT I') +
            CodeLine('REM WAIT FOR KEY PRESS');
    code.line += WaitKeyCode(radLang.ItemIndex);
    // Modified characters
    code.line += CodeLine('REM MODIFIED CHARACTER DATA');
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
        code.line += CodeLine('DATA ' + SetDataValues(
                              frmAntic4.fldChar, frmAntic4.fldFontSet, i, rgDataType.ItemIndex, ','));
      end;

    // Screen data
//    Inc(lineNum, 10);
//    code += IntToStr(lineNum) + ' REM SCREEN DATA'#13#10;
    code.line += CodeLine('REM SCREEN DATA') +
                          DataValues(_ATARI_BASIC, frmAntic4.fldAtascii, code.number, code.step,
                          frmAntic4.modeHeight, 40, rgDataType.ItemIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM MODIFIED CHARACTERS') +
                 CodeLine('REM IN ANTIC MODE ' + anticMode) +
                 CodeLine('--') +
                 CodeLine('NMEMTOP=PEEK(106)-16') +
                 CodeLine('POKE 106,NMEMTOP') +
                 CodeLine('GRAPHICS ' + grMode + strTextWindow) +
                 CodeLine('CHRAM=NMEMTOP*256') +
                 CodeLine('MOVE 57344,CHRAM,1024');
   // Modify characters
//   lineNum := 90;
   code.line += CodeLine('REM MODIFY SOME CHARACTERS');
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then
       code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE CHRAM+I+' + IntToStr(i) +
                             '*8,CHAR:NEXT I');

   code.line += CodeLine('REM MODIFY CHARACTER SET POINTER') +
                CodeLine('POKE 756,NMEMTOP') +
                CodeLine('SCR=DPEEK(88)') +
                CodeLine('FOR I=0 TO ' + IntToStr(frmAntic4.modeSize - 1)) +
                CodeLine('READ BYTE:POKE SCR+I,BYTE') +
                CodeLine('NEXT I') +
                CodeLine('REM WAIT FOR KEY PRESS') +
                WaitKeyCode(radLang.ItemIndex);
   // Modified characters
   code.line += CodeLine('REM MODIFIED CHARACTER DATA');
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then
       code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')') +
                    CodeLine('DATA ' + SetDataValues(
                             frmAntic4.fldChar, frmAntic4.fldFontSet, i, rgDataType.ItemIndex, ','));

   // Screen data
   code.line += CodeLine('REM SCREEN DATA');
//   Inc(lineNum, 10);
   code.line += DataValues(_ATARI_BASIC, frmAntic4.fldAtascii, code.number, code.step,
                      frmAntic4.modeHeight, 40, rgDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    code.line := '; Modified characters in Antic mode ' + anticMode + #13#10#13#10;
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then
        code.line += '; Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                'BYTE ARRAY char' + IntToStr(i) + '=[' +  //#13#10'  ' +
                SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                              i, rgDataType.ItemIndex, ' ') + ']'#13#10;

    code.line += #13#10 + DataValues(_ACTION, frmAntic4.fldAtascii,
                 frmAntic4.modeHeight, 40, frmAntic4.modeSize, rgDataType.ItemIndex);
    code.line += '; Screen memory address'#13#10 +
            'CARD screen=88'#13#10 + #13#10 +
            '; Keyboard code of last key pressed'#13#10 +
            'BYTE ch = $2FC'#13#10 + #13#10 +
            'BYTE ramTop=$6A'#13#10 +
            'BYTE chBas=$2F4'#13#10 +
            'CARD topMem'#13#10#13#10 +
            'PROC Main()'#13#10#13#10 +
            'Graphics(' + grMode + strTextWindow + ')'#13#10#13#10 +
            '; Reserve memory for new character set'#13#10 +
            'topMem=ramTop-12'#13#10 +
            'topMem==*256'#13#10#13#10 +
            '; New character set page address'#13#10 +
            'chBas=TOPMEM/256'#13#10#13#10 +
            '; Move new character set to reserved memory'#13#10 +
            'MoveBlock(topMem,57344,1024)'#13#10#13#10 +
            '; Modified character data'#13#10;
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then
        code.line += 'MoveBlock(topMem+' + IntToStr(i) + '*8,char' + IntToStr(i) + ',8)'#13#10;

    code.line += 'MoveBlock(screen, screenData, ' + IntToStr(frmAntic4.modeSize) + ')'#13#10 +
            WaitKeyCode(_ACTION) + #13#10 +
           'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    code.line := '// Modified characters in Antic mode ' + anticMode + #13#10#13#10 +
                 'uses'#13#10 +
                 '  FastGraph, Crt;'#13#10 + #13#10 +
                 'var'#13#10 +
                 '  topMem : word;'#13#10 +
                 '  CHBAS  : byte absolute $2F4;'#13#10 +
                 '  RAMTOP : byte absolute $6A;'#13#10 +
                 '  screen : word absolute 88;'#13#10 + #13#10;
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then
        code.line += '  // Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                     '  char' + IntToStr(i) + ' : array[0..7] of byte = (' +
                     SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                   i, rgDataType.ItemIndex, ', ') + ');'#13#10;

    code.line += #13#10 + DataValues(_MAD_PASCAL, frmAntic4.fldAtascii,
                 frmAntic4.modeHeight, 40, frmAntic4.modeSize, rgDataType.ItemIndex) + #13#10 +
                 'begin'#13#10 +
                 '  InitGraph(' + grMode + strTextWindow + ');'#13#10#13#10 +
                 '  // Set new character set'#13#10 +
                 '  topMem := RAMTOP - 12;'#13#10 +
                 '  topMem := topMem shl 8;'#13#10 +
                 '  CHBAS := hi(topMem);'#13#10#13#10 +
                 '  // Move new character set to reserved memory'#13#10 +
                 '  Move(pointer(57344), pointer(topMem), 1024);'#13#10#13#10 +
                 '  // Custom character set data'#13#10;
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then
        code.line += '  Move(char' + IntToStr(i) + ', pointer(topMem + ' + IntToStr(i) + '*8), 8);'#13#10;

    code.line += '  Move(screenData, pointer(screen), ' + IntToStr(frmAntic4.modeSize) + ');'#13#10 +
                 WaitKeyCode(_MAD_PASCAL) +
                 'end.';
   end
  { FastBasic
   ---------------------------------------------------------------------------}
   else if radLang.ItemIndex = _FAST_BASIC then begin
     code.line := ''' Modified characters in Antic mode ' + anticMode + #13#10#13#10;
     for i := 0 to 127 do
       if frmAntic4.charEditIndex[i] = 1 then
         code.line += ''' Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                      'DATA char' + IntToStr(i) + '() BYTE = ' +
                      SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                      i, rgDataType.ItemIndex, ', ') + #13#10;

     code.line += #13#10 + DataValues(_FAST_BASIC, frmAntic4.fldAtascii,
                  frmAntic4.modeHeight, 40, frmAntic4.modeSize, rgDataType.ItemIndex);
     code.line += #13#10 +
                  'NMEMTOP = PEEK(106) - 8'#13#10 +
                  'POKE 106, NMEMTOP'#13#10 +
                  'GRAPHICS ' + grMode + strTextWindow + #13#10 +
                  'scr = DPEEK(88)'#13#10 +
                  'CHRAM = NMEMTOP*256'#13#10 +
                  'MOVE 57344, CHRAM, 1024'#13#10 +
                  #13#10 +
                  ''' Custom character set data'#13#10;
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then
//        code += 'MOVE CHRAM + ' + IntToStr(i) + '*8, ADR(char' + IntToStr(i) + '), 8'#13#10;
        code.line += 'MOVE ADR(char' + IntToStr(i) + '), CHRAM + ' + IntToStr(i) + '*8, 8' + #13#10;

    code.line += #13#10''' MODIFY CHARACTER SET POINTER'#13#10 +
            'POKE 756, NMEMTOP'#13#10 +
            'MOVE ADR(screenData), scr, ' + IntToStr(frmAntic4.modeSize) + #13#10 +
            WaitKeyCode(_FAST_BASIC);
  end;

  result := code.line;
end;

// Load Antic mode 4/5 screen
function TfrmAntic4Gen.Example05 : string;
var
  screenCode : array[0..2] of string;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if radLang.ItemIndex = _ATARI_BASIC then begin
    if (frmAntic4.maxX < 40) or (frmAntic4.maxY < 24) then begin
      screenCode[0] := ':CNT=0';
      screenCode[1] := ':IF CNT=' + IntToStr(frmAntic4.maxX) + ' THEN SCR=SCR+40-CNT:CNT=0';
      screenCode[2] := ':CNT=CNT+1';
    end;
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM *******************************') +
            CodeLine('REM LOAD ANTIC MODE ' + anticMode + ' SCREEN') +
            CodeLine('REM *******************************') +
            CodeLine('SIZE=' + IntToStr(frmAntic4.modeSize) + screenCode[0]) +
            CodeLine('GRAPHICS ' + IntToStr(frmAntic4.textMode + 8) + '+16') +
            CodeLine('CLOSE #1') +
            CodeLine('OPEN #1,4,0,"' + editFilename.Text + '"') +
            CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
            CodeLine('FOR I=0 TO SIZE-1') +
            CodeLine('GET #1,BYTE' + screenCode[1]) +
            CodeLine('POKE SCR+I,BYTE' + screenCode[2]) +
            CodeLine('NEXT I') +
            CodeLine('CLOSE #1');
//    lineNum := 80;
    code.line += WaitKeyCode(radLang.ItemIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
            CodeLine('REM LOAD ANTIC MODE ' + anticMode + ' SCREEN') +
            CodeLine('--') +
            CodeLine('SIZE=' + IntToStr(frmAntic4.modeSize) + screenCode[0]) +
            CodeLine('GRAPHICS ' + IntToStr(frmAntic4.textMode + 8) + '+16') +
            CodeLine('CLOSE #%1') +
            CodeLine('OPEN #%1,4,%0,"' + editFilename.Text + '"');

    if (frmAntic4.maxX < 40) or (frmAntic4.maxY < 24) then
      code.line += CodeLine('SCR=DPEEK(88)') +
              CodeLine('FOR I=%0 TO SIZE-%1') +
              CodeLine('GET #%1,BYTE' + screenCode[1]) +
              CodeLine('POKE SCR+I,BYTE' + screenCode[2]) +
              CodeLine('NEXT I') +
              CodeLine('CLOSE #%1')
    else
      code.line += CodeLine('BGET #%1,DPEEK(88),SIZE') +
              CodeLine('CLOSE #%1');
//    lineNum := 80;
    code.line += WaitKeyCode(radLang.ItemIndex);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    if (frmAntic4.maxX < 40) or (frmAntic4.maxY < 24) then begin
      screenCode[0] := '  cnt : byte = 0;'#13#10;
  //    screenCode[1] := '  if cnt=' + IntToStr(frmAsciiEditor.maxX) +
  //    ' then begin scr := scr + 40 - cnt; cnt := 0; end;';
  //    screenCode[2] := '  cnt==+1';
    end;
    code.line := '// Load ANTIC mode ' + anticMode + ' screen'#13#10 +
            #13#10 +
            'uses'#13#10 +
            '  SysUtils, FastGraph, Crt;'#13#10 +
            #13#10 +
            'const'#13#10 +
            '  picSize = ' + IntToStr(frmAntic4.modeSize) + ';'#13#10 +
            #13#10 +
            'var'#13#10 +
            '  f : file;'#13#10 +
            screenCode[0] +
            '  filename : TString;'#13#10 +
            '  buf : pointer;'#13#10 +
            #13#10 +
            'begin'#13#10 +
            '  InitGraph(' + IntToStr(frmAntic4.textMode + 8) + ' + 16);'#13#10 +
            '  buf := pointer(DPeek(88));'#13#10 +
            #13#10 +
            '  // Open file'#13#10 +
            '  filename := ' + QuotedStr(editFilename.Text) + ';'#13#10 +
            '  Assign(f, filename);'#13#10 +
            #13#10 +
            '  // Read data'#13#10 +
            '  Reset(f, 1);'#13#10 +
            '  BlockRead(f, buf, picSize);'#13#10 +
            #13#10 +
            '  // Close file'#13#10 +
            '  Close(f);'#13#10 +
            WaitKeyCode(_MAD_PASCAL) +
            'end.';
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    if (frmAntic4.maxX < 40) or (frmAntic4.maxY < 24) then begin
      screenCode[0] := ', cnt=[0]';
      screenCode[1] := '  IF cnt=' + IntToStr(frmAntic4.maxX) + ' THEN scr==+40-cnt cnt=0 FI';
      screenCode[2] := '  cnt==+1';
    end;
    code.line := '; Load ANTIC mode ' + anticMode + ' screen'#13#10 +
            #13#10'BYTE ch=$2FC'#13#10 +
//            'BYTE screen=88'#13#10 +
            'BYTE data' + screenCode[0] + #13#10 +
            'CARD size=[' + IntToStr(frmAntic4.modeSize) + ']'#13#10 +
            'CARD scr'#13#10 +
            'CARD n'#13#10#13#10 +
            'PROC Main()'#13#10#13#10 +
            'Graphics(' + IntToStr(frmAntic4.textMode + 8) + '+16)'#13#10 +
            'scr=PeekC(88)'#13#10#13#10 +
            '; Set up channel 2 for screen'#13#10 +
            'Close(2)'#13#10 +
            'Open(2,"' + editFilename.Text + '",4,0)'#13#10#13#10 +
            '; Read font data'#13#10 +
            'FOR n=0 TO size-1'#13#10 +
            'DO'#13#10 +
            '  data=GetD(2)'#13#10 +
            screenCode[1] + #13#10 +
            '  PokeC(scr+n,data)'#13#10 +
            screenCode[2] + #13#10 +
            'OD'#13#10#13#10 +
            '; Close channel 2'#13#10 +
            'Close(2)'#13#10#13#10 +
            '; Press any key to exit'#13#10 +
            WaitKeyCode(_ACTION) + #13#10 +
            'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
   else if radLang.ItemIndex = _FAST_BASIC then begin
     if (frmAntic4.maxX < 40) or (frmAntic4.maxY < 24) then begin
       screenCode[0] := ' : cnt = 0';
       screenCode[2] := ' : cnt = cnt + 1';
     end;
     code.line := ''' Load ANTIC mode ' + anticMode + ' screen'#13#10 +
                  #13#10 +
                  'size = ' + IntToStr(frmAntic4.modeSize) + screenCode[0] + #13#10 +
                  'GRAPHICS ' + IntToStr(frmAntic4.textMode + 8) + ' + 16'#13#10 +
                  'CLOSE #1'#13#10 +
                  'OPEN #1, 4, 0, "' + editFilename.Text + '"'#13#10;

     if (frmAntic4.maxX < 40) or (frmAntic4.maxY < 24) then
       code.line += 'scr = DPEEK(88)'#13#10 +
                    #13#10 +
                    'FOR i = 0 TO size - 1'#13#10 +
                    '  GET #1, byte'#13#10 +
                    '  IF cnt = ' + IntToStr(frmAntic4.maxX) + #13#10 +
                    '    scr = scr + 40 - cnt : cnt = 0'#13#10 +
                    '  ENDIF'#13#10 +
                    '  POKE scr + i, byte' + screenCode[2] + #13#10 +
                    'NEXT'#13#10
     else
       code.line += 'BGET #1, DPEEK(88), size'#13#10;

     code.line += #13#10'CLOSE #1'#13#10 +
                  WaitKeyCode(_FAST_BASIC);
  end;

  result := code.line;
end;

// Load Antic mode 4/5 screen with custom character set
function TfrmAntic4Gen.Example06 : string;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if radLang.ItemIndex = _ATARI_BASIC then begin
   code.number := editStartLine.Value;
   code.step := editLineStep.Value;
   code.line := CodeLine('REM *******************************') +
                CodeLine('REM LOAD ANTIC MODE ' + anticMode + ' SCREEN') +
                CodeLine('REM WITH CUSTOM CHARACTER SET ') +
                CodeLine('REM *******************************') +
                CodeLine('SIZE=' + IntToStr(frmAntic4.modeSize)) +
                CodeLine('GRAPHICS ' + IntToStr(frmAntic4.textMode + 8) + '+16') +
                CodeLine('NMEMTOP=PEEK(106)-12') +
                CodeLine('POKE 106,NMEMTOP') +
                CodeLine('CHRAM=NMEMTOP*256') +
                CodeLine('REM LOAD CHARACTER SET') +
                CodeLine('OPEN #1,4,0,"' + editFontName.Text + '"') +
                CodeLine('FOR I=0 TO 1023') +
                CodeLine('GET #1,N:POKE CHRAM+I,N') +
                CodeLine('NEXT I') +
                CodeLine('CLOSE #1') +
                CodeLine('REM MODIFY CHARACTER SET POINTER') +
                CodeLine('POKE 756,NMEMTOP') +
                CodeLine('REM LOAD ANTIC MODE 4 SCREEN') +
                CodeLine('OPEN #1,4,0,"' + editFilename.Text + '"') +
                CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                CodeLine('FOR I=0 TO SIZE-1') +
                CodeLine('GET #1,BYTE') +
                CodeLine('POKE SCR+I,BYTE') +
                CodeLine('NEXT I') +
                CodeLine('CLOSE #1');
//    lineNum := 200;
    code.line += WaitKeyCode(radLang.ItemIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM LOAD ANTIC MODE ' + anticMode + ' SCREEN') +
                 CodeLine('REM WITH CUSTOM CHARACTER SET') +
                 CodeLine('--') +
                 CodeLine('SIZE=' + IntToStr(frmAntic4.modeSize)) +
                 CodeLine('GRAPHICS ' + IntToStr(frmAntic4.textMode + 8) + '+16') +
                 CodeLine('NMEMTOP=PEEK(106)-12') +
                 CodeLine('POKE 106,NMEMTOP') +
                 CodeLine('CHRAM=NMEMTOP*256') +
                 CodeLine('REM LOAD CHARACTER SET') +
                 CodeLine('OPEN #1,4,0,"' + editFontName.Text + '"') +
                 CodeLine('BGET #%1,CHRAM,1024') +
     //            '70 FOR I=0 TO 1023'#13#10 +
     //            '80 GET #1,N:POKE CHRAM+I,N'#13#10 +
     //            '90 NEXT I'#13#10 +
                 CodeLine('CLOSE #%1') +
                 CodeLine('REM MODIFY CHARACTER SET POINTER') +
                 CodeLine('POKE 756,NMEMTOP') +
                 CodeLine('REM LOAD ANTIC MODE 4 SCREEN') +
                 CodeLine('OPEN #%1,4,0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('FOR I=%0 TO SIZE-%1') +
                 CodeLine('GET #%1,BYTE') +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine('NEXT I') +
                 CodeLine('CLOSE #%1') +
//    lineNum := 180;
                 WaitKeyCode(radLang.ItemIndex);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    code.line := '// Load ANTIC mode ' + anticMode + ' screen'#13#10 +
                 '// and custom character set'#13#10 +
                 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt;'#13#10 +
                 #13#10 +
                 'const'#13#10 +
                 '  picSize = ' + IntToStr(frmAntic4.modeSize) + ';'#13#10 +
                 #13#10 +
                 'var'#13#10 +
                 '  f : file;'#13#10 +
                 '  filename: TString;'#13#10 +
                 '  buf: pointer;'#13#10 +
                 '  n : byte;'#13#10 +
                 '  font : array[0..1023] of byte;'#13#10 +
                 '  topMem : word;'#13#10 +
                 '  CHBAS  : byte absolute $2F4;'#13#10 +
                 '  RAMTOP : byte absolute $6A;'#13#10 +
                 'begin'#13#10 +
                 '  InitGraph(' + IntToStr(frmAntic4.textMode + 8) + '+16);'#13#10 +
                 #13#10 +
                 '  // Set new character set'#13#10 +
                 '  topMem := RAMTOP - 12;'#13#10 +
                 '  topMem := topMem shl 8;'#13#10 +
                 '  CHBAS := hi(topMem);'#13#10 +
                 #13#10 +
                 '  // Load custom character set'#13#10 +
                 '  filename := + ' + QuotedStr(editFontName.Text) + ';'#13#10 +
                 '  Assign(f, filename);'#13#10 +
                 '  Reset(f, 1);'#13#10 +
                 '  BlockRead(f, font, 1024);'#13#10 +
                 '  Close(f);'#13#10 +
                 #13#10 +
                 '  // Move new character set to reserved memory'#13#10 +
                 '  Move(font, pointer(topMem), SizeOf(font));'#13#10 +
                 #13#10 +
                 '  // Find screen area pointer'#13#10 +
                 '  buf := pointer(DPeek(88));'#13#10 +
                 #13#10 +
                 '  // Open file'#13#10 +
                 '  filename := ' + QuotedStr(editFilename.Text) + ';'#13#10 +
                 '  Assign(f, filename);'#13#10 +
                 #13#10 +
                 '  // Read data'#13#10 +
                 '  Reset(f, 1);'#13#10 +
                 '  BlockRead(f, buf, picSize);'#13#10 +
                 #13#10 +
                 '  // Close file'#13#10 +
                 '  Close(f);'#13#10 +
                 WaitKeyCode(_MAD_PASCAL) +
                 'end.';
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    code.line := '; Load ANTIC mode ' + anticMode + ' screen'#13#10 +
                '; and custom character set'#13#10 +
                #13#10 +
                'BYTE CH=$2FC'#13#10 +
    //            'BYTE SCREEN=$58'#13#10 +
                'BYTE RAMTOP=$6A'#13#10 +
                'BYTE CHBAS=$2F4'#13#10 +
                #13#10 +
                'BYTE DATA'#13#10 +
                'CARD TOPMEM'#13#10 +
                'CARD N'#13#10 +
                'CARD SCR'#13#10 +
                #13#10 +
                'BYTE ARRAY FONT(1023)'#13#10 +
                'CARD SIZE=[' + IntToStr(frmAntic4.modeSize) + ']'#13#10 +
                #13#10 +
                'PROC MAIN()'#13#10 + #13#10 +
                'GRAPHICS(' + IntToStr(frmAntic4.textMode + 8) + '+16)'#13#10 +
                #13#10 +
                '; RESERVE MEMORY FOR NEW CHARACTER SET'#13#10 +
                'TOPMEM=RAMTOP-12'#13#10 +
                'TOPMEM==*256'#13#10 + #13#10 +
                '; LOAD CUSTOM CHARACTER SET'#13#10 +
                #13#10 +
                'CLOSE(2)'#13#10 +
                'OPEN(2,"' + editFontName.Text + '",4,0)'#13#10 +
                #13#10 +
                'FOR N=0 TO 1023'#13#10 +
                'DO'#13#10 +
                '  DATA=GETD(2)'#13#10 +
                '  FONT(N)=DATA'#13#10 +
                'OD'#13#10 +
                #13#10 +
                'CLOSE(2)'#13#10 +
                #13#10 +
                '; COPY FONT SET TO NEW ADDRESS'#13#10 +
                'CHBAS=TOPMEM/256'#13#10 +
                'MOVEBLOCK(TOPMEM,FONT,1024)'#13#10 +
                #13#10 +
                '; LOAD ANTIC MODE 4 SCREEN'#13#10 +
                #13#10 +
                'SCR=PEEKC(88)'#13#10 +
                #13#10 +
                '; SET UP CHANNEL 2 FOR SCREEN'#13#10 +
                'CLOSE(2)'#13#10 +
                'OPEN(2,"' + editFilename.Text + '",4,0)'#13#10 +
                #13#10 +
                '; READ FONT DATA BYTE BY BYTE'#13#10 +
                'FOR N=0 TO SIZE-1'#13#10 +
                'DO'#13#10 +
                '  DATA=GETD(2)'#13#10 +
                '  POKEC(SCR+N,DATA)'#13#10 +
                'OD'#13#10 + #13#10 +
                '; CLOSE CHANNEL 2'#13#10 +
                'CLOSE(2)'#13#10 +
                #13#10 +
                '; PRESS ANY KEY TO EXIT'#13#10 +
                WaitKeyCode(_ACTION) +
                #13#10 +
                'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    code.line := ''' Load ANTIC mode ' + anticMode + ' screen'#13#10 +
                ''' and custom character set'#13#10#13#10 +
                'size = ' + IntToStr(frmAntic4.modeSize) + #13#10#13#10 +
                'GRAPHICS ' + IntToStr(frmAntic4.textMode + 8) + ' + 16'#13#10#13#10 +
                'NMEMTOP = PEEK(106) - 12'#13#10 +
                'POKE 106, NMEMTOP'#13#10 +
                'CHRAM = NMEMTOP*256'#13#10#13#10 +
                ''' LOAD CHARACTER SET'#13#10 +
                'OPEN #1, 4, 0, "' + editFontName.Text + '"'#13#10 +
                'BGET #1, CHRAM, 1024'#13#10 +
    //            'FOR I = 0 TO 1023'#13#10 +
    //            'GET #1, N:POKE CHRAM + I, N'#13#10 +
    //            'NEXT'#13#10 +
                'CLOSE #1'#13#10#13#10 +
                ''' MODIFY CHARACTER SET POINTER'#13#10 +
                'POKE 756, NMEMTOP'#13#10#13#10 +
                ''' LOAD ANTIC MODE 4 SCREEN'#13#10 +
                'OPEN #1, 4, 0, "' + editFilename.Text + '"'#13#10 +
                'scr = DPEEK(88)'#13#10 +
                'BGET #1, scr, size'#13#10 +
    //            'FOR i = 0 TO size - 1'#13#10 +
    //            '  GET #1, BYTE'#13#10 +
    //            '  POKE scr + i, BYTE'#13#10 +
    //            'NEXT'#13#10 +
                'CLOSE #1'#13#10 +
                WaitKeyCode(_FAST_BASIC);
  end;

  result := code.line;
end;

//procedure TfrmAntic4Gen.radLangProc(Sender: TObject);
//begin
//  CreateCode;
//
//  //lblStartLine.Enabled := langIndex < 2;
//  //lblStartLine.Visible := langIndex < 2;
//  //editStartLine.Enabled := langIndex < 2;
//  //editStartLine.Visible := langIndex < 2;
//  //lblLineStep.Enabled := langIndex < 2;
//  //lblLineStep.Visible := langIndex < 2;
//  //editLineStep.Enabled := langIndex < 2;
//  //editLineStep.Visible := langIndex < 2;
//end;

procedure TfrmAntic4Gen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

