{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Antic mode 2 editor - source code generator
}
unit antic2_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  ComCtrls, StdCtrls, ExtCtrls, lcltype, Spin, Windows,
  common;

type
  { TfrmAntic2Gen }
  TfrmAntic2Gen = class(TForm)
    btnClose: TButton;
    btnCopyToEditor: TButton;
    editFilename: TLabeledEdit;
    editLineStep : TSpinEdit;
    editStartLine : TSpinEdit;
    boxStartLine : TGroupBox;
    Label7: TLabel;
    lblLineStep : TLabel;
    lblStartLine : TLabel;
    listExamples: TListBox;
    memo: TMemo;
    radLang: TRadioGroup;
    rgDataType: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditorProc(Sender: TObject);
    procedure listExamplesProc(Sender: TObject);
    procedure radLangProc(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    //    procedure editStartLineChange(Sender : TObject);
  private
    { private declarations }
    listings : TListings;
    procedure CreateCode;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
  public
    { public declarations }
  end;

var
  frmAntic2Gen: TfrmAntic2Gen;

implementation

{$R *.lfm}

uses
  antic2, src_editor, code_lib, lib;

{ TfrmAntic2Gen }
procedure TfrmAntic2Gen.FormCreate(Sender: TObject);
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

  langIndex := 0;
end;

procedure TfrmAntic2Gen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;
  editFilename.Text:= 'H1:' + ExtractFileName(frmAntic2.filename);
//  listExamplesProc(Sender);
end;

procedure TfrmAntic2Gen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmAntic2Gen.CreateCode;
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

  // List of listing examples
  case listExamples.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
  end;

  memo.Lines.Add(code);

  // Set cursor position at the top of memo object
  memo.SelStart := 0;
  memo.SelLength := 0;
  SendMessage(memo.Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TfrmAntic2Gen.CopyToEditorProc(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor03(Sender);
  Close;

  //frmSrcEdit.Show;
  //langIndex := tabs.TabIndex;
  //case langIndex of
  //  0: frmSrcEdit.editor.Lines := memoAtariBASIC.Lines;
  //  1: frmSrcEdit.editor.Lines := memoTurboBASICXL.Lines;
  //  2: frmSrcEdit.editor.Lines := memoAction.Lines;
  //  3: frmSrcEdit.editor.Lines := memoMadPascal.Lines;
  //  4: frmSrcEdit.editor.Lines := memoFastBasic.Lines;
  //end;

  //if langIndex = 2 then
  //  Inc(langIndex)
  //else if langIndex = 3 then begin
  //  Dec(langIndex);
  //end;

  //frmSrcEdit.MemoToEditor02(Sender);
  //Close;
end;

//procedure TfrmAntic2Gen.editStartLineChange(Sender : TObject);
//begin
//  CreateCode;
//end;

function TfrmAntic2Gen.Example01 : string;
var
  maxSize : word;
  screenCode : array[0..2] of string;
begin
  maxSize := frmAntic2.maxSize + 1;

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if radLang.ItemIndex = _ATARI_BASIC then begin
    if (frmAntic2.maxX < 40) or (frmAntic2.maxY < 24) then begin
      screenCode[0] := ':CNT=0';
      screenCode[1] := ':IF CNT=' + IntToStr(frmAntic2.maxX) + ' THEN SCR=SCR+40-CNT:CNT=0';
      screenCode[2] := ':CNT=CNT+1';
    end;
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM *******************************') +
                 CodeLine('REM LOAD TEXT MODE 0 SCREEN') +
                 CodeLine('REM *******************************') +
                 CodeLine('SIZE=' + IntToStr(maxSize) + screenCode[0]) +
                 CodeLine('GRAPHICS 0') +
                 CodeLine('CLOSE #1') +
                 CodeLine('OPEN #1,4,0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                 CodeLine('FOR I=0 TO SIZE-1') +
                 CodeLine('GET #1,BYTE' + screenCode[1]) +
                 CodeLine('POKE SCR+I,BYTE' + screenCode[2]) +
                 CodeLine('NEXT I') +
                 CodeLine('CLOSE #1') +
//    lineNum := 80;
                 WaitKeyCode(radLang.ItemIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM LOAD TEXT MODE 0 SCREEN') +
                 CodeLine('--') +
                 CodeLine('SIZE=' + IntToStr(maxSize) + screenCode[0]) +
                 CodeLine('GRAPHICS %0') +
                 CodeLine('CLOSE #%1') +
                 CodeLine('OPEN #%1,4,%0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('FOR I=%0 TO SIZE-%1') +
                 CodeLine('GET #%1,BYTE' + screenCode[1]) +
                 CodeLine('POKE SCR+I,BYTE' + screenCode[2]) +
                 CodeLine('NEXT I') +
                 CodeLine('CLOSE #%1') +
                 //  '30 BGET #%1,DPEEK(88),SIZE'#13#10 +
    //    lineNum := 80;
                 WaitKeyCode(radLang.ItemIndex);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    if (frmAntic2.maxX < 40) or (frmAntic2.maxY < 24) then begin
      screenCode[0] := '  cnt : byte = 0;';
  //    screenCode[1] := '  if cnt=' + IntToStr(frmAsciiEditor.maxX) + ' then begin scr := scr + 40 - cnt; cnt := 0; end;';
  //    screenCode[2] := '  cnt==+1';
    end;
    code.line := '// Load text mode 0 screen'#13#10 +
                 #13#10 +
                 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt;'#13#10 +
                 #13#10 +
                 'const'#13#10 +
                 '  picSize = ' + IntToStr(maxSize) + ';'#13#10 +
                 #13#10 +
                 'var'#13#10 +
                 '  f : file;'#13#10 +
                 screenCode[0] + #13#10 +
                 '  filename : TString;'#13#10 +
                 '  buf : pointer;'#13#10 +
                 #13#10 +
                 'begin'#13#10 +
                 '  InitGraph(0);'#13#10 +
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
                 //'  repeat until KeyPressed;'#13#10 +
                 //'  ReadKey;'#13#10 +
                 'end.';
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    if (frmAntic2.maxX < 40) or (frmAntic2.maxY < 24) then begin
      screenCode[0] := ', cnt=[0]';
      screenCode[1] := '  IF cnt=' + IntToStr(frmAntic2.maxX) + ' THEN scr==+40-cnt cnt=0 FI';
      screenCode[2] := '  cnt==+1';
    end;
    code.line := '; Load text mode 0 screen'#13#10 +
                 #13#10 +
                 'BYTE ch=$2FC'#13#10 +
     //            'BYTE screen=88'#13#10 +
                 'BYTE data' + screenCode[0] + #13#10 +
                 'CARD size=[' + IntToStr(maxSize) + ']'#13#10 +
                 'CARD scr'#13#10 +
                 'CARD n'#13#10 +
                 #13#10 +
                 'PROC Main()'#13#10 +
                 #13#10 +
                 'Graphics(0)'#13#10 +
                 'Scr=PeekC(88)'#13#10 +
                 #13#10 +
                 '; Set up channel 2 for screen'#13#10 +
                 'Close(2)'#13#10 +
                 'Open(2,"' + editFilename.Text + '",4,0)'#13#10 +
                 #13#10 +
                 '; Read font data'#13#10 +
                 'FOR n=0 TO size-1'#13#10 +
                 'DO'#13#10 +
                 '  data=GetD(2)'#13#10 +
                 screenCode[1] + #13#10 +
                 '  PokeC(Scr+n,data)'#13#10 +
                 screenCode[2] + #13#10 +
                 'OD'#13#10 +
                 #13#10 +
                 '; Close channel 2'#13#10 +
                 'Close(2)'#13#10 +
                 WaitKeyCode(_ACTION) +
       //          #13#10 +
       //          '; Press any key to exit'#13#10 +
       ////            'ch=255'#13#10 +
       //          'DO UNTIL ch<255 OD'#13#10 +
       //          'ch=255'#13#10 +
                 #13#10 +
                 'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    if (frmAntic2.maxX < 40) or (frmAntic2.maxY < 24) then begin
      screenCode[0] := 'cnt = 0';
      screenCode[2] := ' : cnt = cnt + 1';
    end;
    code.line := ''' Load text mode 0 screen'#13#10 +
                 #13#10 +
                 'size = ' + IntToStr(maxSize) + #13#10 +
                 screenCode[0] + #13#10 +
                 'GRAPHICS 0'#13#10 +
                 #13#10 +
                 'CLOSE #1'#13#10 +
                 'OPEN #1, 4, 0, "' + editFilename.Text + '"'#13#10 +
                 #13#10 +
       //          '''BGET #1, DPEEK(88), 3839'#13#10 +
                 'scr = DPEEK(88)'#13#10 +
                 #13#10 +
                 'FOR i = 0 TO size - 1'#13#10 +
                 '  GET #1, BYTE'#13#10;
                 if (frmAntic2.maxX < 40) or (frmAntic2.maxY < 24) then begin
                   code.line += 'IF cnt = ' + IntToStr(frmAntic2.maxX) + #13#10 +
                           '  scr = scr + 40 - cnt : cnt = 0'#13#10 +
                           'ENDIF'#13#10;
                 end;
     code.line += '  POKE scr + i, BYTE' + screenCode[2] + #13#10 +
                  'NEXT'#13#10 +
                  #13#10 +
                  'CLOSE #1'#13#10 +
                  WaitKeyCode(_FAST_BASIC);
                  //#13#10 +
                  //'REPEAT'#13#10 +
                  //'UNTIL Key()';
  end;

  result := code.line;
end;

// Data values
function TfrmAntic2Gen.Example02 : string;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (radLang.ItemIndex = _ATARI_BASIC) or (radLang.ItemIndex = _TURBO_BASIC_XL) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := DataValues(_ATARI_BASIC, frmAntic2.fldAtascii, code.number, code.step,
                            24, 40, rgDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    code.line := DataValues(_ACTION, frmAntic2.fldAtascii, 24, 40, 960, rgDataType.ItemIndex);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    code.line := DataValues(_MAD_PASCAL, frmAntic2.fldAtascii, 24, 40, 960, rgDataType.ItemIndex);
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    code.line := DataValues(_FAST_BASIC, frmAntic2.fldAtascii, 24, 40, 960, rgDataType.ItemIndex);
  end;
  //{
  // Mad Assembler (MADS)
  // ---------------------------------------------------------------------------}
  //else if radLang.ItemIndex = _MADS then begin
  //  code := GrDataValues(_MADS, lineNum, 240, rgDataType.ItemIndex, false);
  //end
  //{
  // MAC/65 / Atari Assembler/Editor
  // ---------------------------------------------------------------------------}
  ////else if radLang.ItemIndex = _MAC65 then begin
  ////  if chkMAC65Lines.Checked then
  ////    lineNum := 10
  ////  else begin
  ////    lineNum := 65535;
  ////  end;
  ////  code := GrDataValues(_MAC65, lineNum, grBytes, rgDataType.ItemIndex, false);
  ////end
  //{
  // CC65
  // ---------------------------------------------------------------------------}
  //else if radLang.ItemIndex = _CC65 then begin
  //  code := GrDataValues(_CC65, lineNum, 240, rgDataType.ItemIndex, false);
  //end;

  result := code.line;
end;

// Data values with screen loader
function TfrmAntic2Gen.Example03 : string;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if radLang.ItemIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS 0') +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                 CodeLine('FOR I=0 TO ' + IntToStr(960 - 1)) +
                 CodeLine('READ BYTE') +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine('NEXT I') +
                 WaitKeyCode(radLang.ItemIndex);

    // Screen data
//    Inc(lineNum, 10);
    code.line += DataValues(_ATARI_BASIC, frmAntic2.fldAtascii, code.number, code.step,
                            24, 40, rgDataType.ItemIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS %0') +
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('FOR I=%0 TO ' + IntToStr(960 - 1)) +
                 CodeLine('READ BYTE') +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine('NEXT I') +
                 WaitKeyCode(radLang.ItemIndex);

    // Screen data
//    Inc(lineNum, 10);
    code.line += DataValues(_TURBO_BASIC_XL, frmAntic2.fldAtascii, code.number, code.step,
                            24, 40, rgDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    code.line := DataValues(_ACTION, frmAntic2.fldAtascii, 24, 40, 960, rgDataType.ItemIndex);
    code.line += #13#10 +
                 '; Screen memory address'#13#10 +
                 'CARD SCREEN = 88'#13#10 +
                 #13#10 +
                 '; Keyboard code of last key pressed'#13#10 +
                 'BYTE CH = $2FC'#13#10 +
                 #13#10 +
                 'PROC Main()'#13#10 +
                 #13#10 +
                 'Graphics(0)'#13#10 +
                 #13#10 +
                 'MoveBlock(SCREEN, screenData, ' + IntToStr(960) + ')'#13#10 +
                 WaitKeyCode(radLang.ItemIndex) +
                 #13#10 +
                 'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    code.line := 'uses'#13#10 +
                 '  FastGraph, Crt;'#13#10#13#10 +
                 'var'#13#10 +
                 ' screen : word absolute 88;'#13#10 +
                 #13#10;
    code.line += DataValues(_MAD_PASCAL, frmAntic2.fldAtascii, 24, 40, 960, rgDataType.ItemIndex);
    code.line += #13#10 +
                 'begin'#13#10 +
                 '  InitGraph(0);'#13#10 +
                 #13#10 +
                 '  Move(screenData, pointer(screen), ' + IntToStr(960) + ');'#13#10 +
                 WaitKeyCode(radLang.ItemIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    code.line := DataValues(_FAST_BASIC, frmAntic2.fldAtascii, 24, 40, 960, rgDataType.ItemIndex);
    code.line += #13#10 +
                 'GRAPHICS 0'#13#10 +
                 'scr = DPEEK(88)'#13#10 +
                 'MOVE ADR(screenData), scr, ' + IntToStr(960) + #13#10 +
                 WaitKeyCode(radLang.ItemIndex);
  end;

  result := code.line;
end;

procedure TfrmAntic2Gen.listExamplesProc(Sender: TObject);
var
  i : byte;
begin
  for i := 0 to radLang.Items.Count - 1 do begin
    radLang.Controls[i].Enabled := listings[listExamples.ItemIndex, i];
//    radLang.Controls[i].Visible := listings[listExamples.ItemIndex, i];
  end;
  if langIndex < 5 then
    radLang.ItemIndex := langIndex;

  editFilename.Visible := listExamples.ItemIndex = 0;
  editFilename.Enabled := listExamples.ItemIndex = 0;
  CreateCode;
end;

procedure TfrmAntic2Gen.radLangProc(Sender: TObject);
begin
//  langIndex := radLang.ItemIndex;
  CreateCode;
end;

procedure TfrmAntic2Gen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

