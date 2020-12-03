{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Graphics editor - source code generator
}
unit gr_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, lcltype, ComCtrls, Spin, Windows,
  common;

type
  { TfrmGrGen }
  TfrmGrGen = class(TForm)
    boxStartLine : TGroupBox;
    btnClose: TButton;
    btnCopyToEditor: TButton;
    chkAddColors: TCheckBox;
    chkTextWindow: TCheckBox;
    chkMAC65Lines: TCheckBox;
    editFilename: TEdit;
    editLineStep : TSpinEdit;
    editStartLine : TSpinEdit;
    grpColors: TGroupBox;
    grpMAC65Lines: TGroupBox;
    grpFilename: TGroupBox;
    Label1: TLabel;
    Label6: TLabel;
    imgColor0: TShape;
    editColor0: TSpinEdit;
    Label7: TLabel;
    listExamples: TListBox;
    memo: TMemo;
    radLang: TRadioGroup;
    rgDataType: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditorProc(Sender: TObject);
    procedure ListExamplesProc(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    procedure radLangProc(Sender: TObject);
  private
    { private declarations }
    listings : TListings;
    grBytes : integer;
    is01bit : boolean;
    procedure CreateCode;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
  public
    { public declarations }
  end;

var
  frmGrGen: TfrmGrGen;

implementation

{$R *.lfm}

uses
  src_editor, gr, code_lib, lib;

{ TfrmGrGen }

procedure TfrmGrGen.FormCreate(Sender: TObject);
begin
  is01bit := false;

  case grMode of
    grMode40x24x4: begin
      grBytes := 240;
    end;
    grMode80x48x2: begin
      is01bit := true;
      grBytes := 480;
    end;
    grMode80x48x4: begin
      grBytes := 960;
    end;
    grMode160x96x2: begin
      is01bit := true;
      grBytes := 3840 div 2;
    end;
    grMode160x96x4: begin
      grBytes := 3840;
    end;
    grMode160x192x2: begin
      is01bit := true;
      grBytes := 3840;
    end;
    grMode160x192x4: begin
      grBytes := 7680;
    end;
    grMode320x192x2: begin
      is01bit := true;
      grBytes := 7680;
      chkAddColors.Checked := false;
    end;
  end;

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
  listings[1, 5] := true;
  listings[1, 6] := true;
  listings[1, 7] := true;

  // Example 3
  listings[2, 0] := true;
  listings[2, 1] := true;
  listings[2, 2] := true;
  listings[2, 3] := true;
  listings[2, 4] := true;

  // Example 4
  //listings[3, 0] := true;
  //listings[3, 1] := true;
  //listings[3, 2] := true;
  //listings[3, 3] := true;
  //listings[3, 4] := true;

  langIndex := 0;
end;

procedure TfrmGrGen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;
//  tabs.TabIndex := langIndex;

//  imgColor1.Canvas.Brush.Color := colorMem[20];
  editFilename.Text := 'H1:' + ExtractFileName(frmGr.filename);

  editColor0.Value := colorValues[0];
  imgColor0.Brush.Color := colTab[0];

  listExamplesProc(Sender);
end;

procedure TfrmGrGen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmGrGen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

procedure TfrmGrGen.CopyToEditorProc(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
//  memo.Lines.TextLineBreakStyle:=#$9b;
//  memo.Lines.LineBreak:=#$9b;
//  frmSrcEdit.editor.Lines.LineBreak:=#$9b;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor03(Sender);
  Close;
end;

//procedure TfrmGrGen.editStartLineChange(Sender : TObject);
//begin
//  CreateCode;
//end;

procedure TfrmGrGen.ListExamplesProc(Sender: TObject);
var
  i : byte;
begin
  for i := 0 to radLang.Items.Count - 1 do begin
    radLang.Controls[i].Enabled := listings[listExamples.ItemIndex, i];
//    radLang.Controls[i].Visible := listings[listExamples.ItemIndex, i];
  end;
  radLang.ItemIndex := langIndex;
  CreateCode;
end;

procedure TfrmGrGen.CreateCode;
var
  code : string;
begin
  //if langIndex < 2 then
  //  rgDataType.ItemIndex := 0;

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

  case listExamples.ItemIndex of
    0: begin
      code := Example01;
      grpFilename.Enabled := true;
      grpColors.Enabled := true;
    end;
    1: begin
      code := Example02;
      grpFilename.Enabled := false;
      grpColors.Enabled := false;
    end;
    2: begin
      code := Example03;
      grpFilename.Enabled := false;
      grpColors.Enabled := true;
    end;
    //3: begin
    //  code := Example04;
    //  grpFilename.Enabled := false;
    //  grpColors.Enabled := true;
    //end;
  end;

  memo.Lines.Add(code);

  // Set cursor position at the top of memo object
  memo.SelStart := 0;
  memo.SelLength := 0;
  SendMessage(memo.Handle, EM_SCROLLCARET, 0, 0);

  //memoAtariBASIC.Clear;
  //memoTurboBASICXL.Clear;
  //memoAction.Clear;
  //
  //case cmbExamples.ItemIndex of
  //  0: Example01;
  //end;
end;

// Picture loader
function TfrmGrGen.Example01 : string;
var
  strTextWindow : string = '';
begin
  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if radLang.ItemIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS ' + IntToStr(grMode) + strTextWindow) +
                 CodeLine('CLOSE #1') +
                 CodeLine('OPEN #1,4,0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                 CodeLine('FOR I=0 TO ' + IntToStr(grBytes - 1)) +
                 CodeLine('GET #1,BYTE') +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine('NEXT I');
    if chkAddColors.Checked then begin
      code.line += CodeLine('GET #1,BYTE:POKE 712,BYTE');
      code.line += CodeLine('GET #1,BYTE:POKE 708,BYTE');

      if not is01bit then begin
        code.line += CodeLine('GET #1,BYTE:POKE 709,BYTE');
        code.line += CodeLine('GET #1,BYTE:POKE 710,BYTE');
      end;
    end;
//    lineNum := 120;
    code.line += CodeLine('CLOSE #1') +
                 WaitKeyCode(radLang.ItemIndex);
//            '120 IF PEEK(764)<>255 THEN POKE 764,255:END'#13#10 +
//            '130 GOTO 120';
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS ' + IntToStr(grMode) + strTextWindow) +
                 CodeLine('CLOSE #%1') +
                 CodeLine('OPEN #%1,4,%0,"' + editFilename.Text + '"') +
                 CodeLine('BGET #%1,DPEEK(88),' + IntToStr(grBytes - 1));

    //      Lines.Add('20 DIM FN$(30)');
    //      Lines.Add('30 FN$(%1)="' + editFilename.Text + '"');
    if chkAddColors.Checked then begin
      code.line += CodeLine('BGET #%1,PEEK(712),%1');
      code.line += CodeLine('BGET #%1,PEEK(708),%1');

      if not is01bit then begin
        code.line += CodeLine('BGET #%1,PEEK(709),%1');
        code.line += CodeLine('BGET #%1,PEEK(710),%1');
      end;
    end;
//    lineNum := 80;
    code.line += CodeLine('CLOSE #%1') +
                 WaitKeyCode(radLang.ItemIndex);
//            '80 IF PEEK(764)<>255 THEN POKE 764,255:END'#13#10 +
//            '90 GOTO 80';
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    if not chkTextWindow.Checked then
      strTextWindow := ' + 16';

    code.line := 'PROC MAIN()'#13#10#13#10 +
                 'CARD I       ; COUNTER'#13#10 +
                 'BYTE DATA    ; BYTE OF DATA'#13#10 +
                 'BYTE CH=764  ; KEY SCAN REGISTER'#13#10 +
                 'CARD SCR=88  ; SCREEN DISPLAY'#13#10#13#10 +
                 'GRAPHICS(' + IntToStr(grMode) + strTextWindow + ')'#13#10 +
                 'SCR=PEEKC(88)'#13#10#13#10 +
                 'CLOSE(1)'#13#10 +
                 'OPEN(1,"' + editFilename.Text + '",4,0)'#13#10 +
                 'FOR I=0 TO ' + IntToStr(grBytes - 1) + ' DO'#13#10 +
                 '  DATA=GETD(1)'#13#10 +
                 '  POKEC(SCR+I,DATA)'#13#10 +
                 'OD'#13#10;
    if chkAddColors.Checked then begin
      code.line += #13#10'DATA=GETD(1) POKE(712,DATA)'#13#10 +
                   'DATA=GETD(1) POKE(708,DATA)'#13#10;

      if not is01bit then
        code.line += 'DATA=GETD(1) POKE(709,DATA)'#13#10 +
                     'DATA=GETD(1) POKE(710,DATA)'#13#10;
    end;
    code.line += #13#10'CLOSE(1)'#13#10 +
                 //'CH=255'#13#10 +
                 //'DO UNTIL CH#255 OD'#13#10 +
                 //'CH=255'#13#10 +
                 WaitKeyCode(radLang.ItemIndex) +
                 #13#10'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    if not chkTextWindow.Checked then
      strTextWindow := ' + 16';

    code.line := 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt;'#13#10 +
                 #13#10 +
                 'const'#13#10 +
                 '  picSize = ' + IntToStr(grBytes - 1) + ';'#13#10 +
                 #13#10 +
                 'var'#13#10 +
                 '  f : file;'#13#10 +
                 '  filename : TString;'#13#10 +
                 '  buf : pointer;'#13#10 +
                 'begin'#13#10 +
                 '  InitGraph(' + IntToStr(grMode) + strTextWindow + ');'#13#10 +
                 '  buf := pointer(DPeek(88));'#13#10 +
                 #13#10 +
                 '  // Open file'#13#10 +
                 '  filename := ''' + editFilename.Text + ''';'#13#10 +
                 '  Assign(f, filename);'#13#10 +
                 #13#10 +
                 '  // Read data'#13#10 +
                 '  Reset(f, 1);'#13#10 +
                 '  BlockRead(f, buf, picSize);'#13#10;
    if chkAddColors.Checked then begin
      code.line += #13#10 +
                   '  // Read colors'#13#10 +
                   '  buf := pointer(712); '#13#10 +
                   '  BlockRead(f, buf, 1);'#13#10 +
                   '  buf := pointer(708);'#13#10;
      if is01bit then
        code.line += '  BlockRead(f, buf, 1);'#13#10
      else
        code.line += '  BlockRead(f, buf, 3);'#13#10;
    end;
    code.line += #13#10 +
                 '  // Close file'#13#10 +
                 '  Close(f);'#13#10 +
                 //#13#10 +
                 //'  repeat until KeyPressed;'#13#10 +
                 //'  ReadKey;'#13#10 +
                 WaitKeyCode(radLang.ItemIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    if not chkTextWindow.Checked then
      strTextWindow := ' + 16';

    code.line := 'GRAPHICS ' + IntToStr(grMode) + strTextWindow + #13#10 +
                 #13#10 +
                 'CLOSE #1'#13#10 +
                 'OPEN #1, 4, 0, "' + editFilename.Text + '"'#13#10 +
                 'BGET #1, DPEEK(88), ' + IntToStr(grBytes - 1) + #13#10;
    if chkAddColors.Checked then begin
      code.line += 'BGET #1, PEEK(712), 1'#13#10 +
                   'BGET #1, PEEK(708), 1'#13#10;
      if not is01bit then
        code.line += 'BGET #1, PEEK(709), 1'#13#10 +
                     'BGET #1, PEEK(710), 1'#13#10;
    end;
    code.line += 'CLOSE #1'#13#10 +
                 //#13#10 +
                 //'REPEAT'#13#10 +
                 //'UNTIL Key()';
                 WaitKeyCode(radLang.ItemIndex);
  end;

  result := code.line;
end;

// Data values
function TfrmGrGen.Example02 : string;
begin
  code.number := editStartLine.Value;
  code.step := editLineStep.Value;

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (radLang.ItemIndex = _ATARI_BASIC) or (radLang.ItemIndex = _TURBO_BASIC_XL) then
    code.line := GrDataValues(_ATARI_BASIC, code.number, grBytes, rgDataType.ItemIndex, is01bit)
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then
    code.line := GrDataValues(_ACTION, code.number, grBytes, rgDataType.ItemIndex, is01bit)
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then
    code.line := GrDataValues(_MAD_PASCAL, code.number, grBytes, rgDataType.ItemIndex, is01bit)
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then
    code.line := GrDataValues(_FAST_BASIC, code.number, grBytes, rgDataType.ItemIndex, is01bit)
  { Mad Assembler (MADS)
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MADS then
    code.line := GrDataValues(_MADS, code.number, grBytes, rgDataType.ItemIndex, is01bit)
  { MAC/65 / Atari Assembler/Editor
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAC65 then begin
    if not chkMAC65Lines.Checked then
      code.number := 65535;

    code.line := GrDataValues(_MAC65, code.number, grBytes, rgDataType.ItemIndex, is01bit);
  end
  { CC65
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _CC65 then
    code.line := GrDataValues(_CC65, code.number, grBytes, rgDataType.ItemIndex, is01bit);

  result := code.line;
end;

// Data values with picture loader
function TfrmGrGen.Example03 : string;
var
  strTextWindow : string;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if radLang.ItemIndex = _ATARI_BASIC then begin
    if not chkTextWindow.Checked then
      strTextWindow := '+16';

    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS ' + IntToStr(grMode) + strTextWindow);
    code.line += CodeLine('SCR=PEEK(88)+PEEK(89)*256');
    code.line += CodeLine('FOR I=0 TO ' + IntToStr(grBytes - 1));
    code.line += CodeLine('READ BYTE');
    code.line += CodeLine('POKE SCR+I,BYTE');
    code.line += CodeLine('NEXT I');
    if chkAddColors.Checked then begin
      code.line += CodeLine('READ BYTE:POKE 712,BYTE');
      code.line += CodeLine('READ BYTE:POKE 708,BYTE');
      if not is01bit then begin
        code.line += CodeLine('READ BYTE:POKE 709,BYTE');
        code.line += CodeLine('READ BYTE:POKE 710,BYTE');
      end;
    end;

    //Inc(lineNum, 10);
    //code += IntToStr(LineNum) + ' IF PEEK(764)<>255 THEN POKE 764,255:END'#13#10;
    //Inc(lineNum, 10);
    //code += IntToStr(LineNum) + ' GOTO ' + IntToStr(LineNum - 10) + #13#10;
    code.line += WaitKeyCode(radLang.ItemIndex);

    // Picture data
//    Inc(lineNum, 10);
    code.line += GrDataValues(_ATARI_BASIC, code.number, grBytes, rgDataType.ItemIndex, is01bit);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _TURBO_BASIC_XL then begin
    if not chkTextWindow.Checked then
      strTextWindow := '+16';

    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS ' + IntToStr(grMode) + strTextWindow);
    code.line += CodeLine('SCR=DPEEK(88)');
    code.line += CodeLine('FOR I=%0 TO ' + IntToStr(grBytes - 1));
    code.line += CodeLine('READ BYTE');
    code.line += CodeLine('POKE SCR+I,BYTE');
    code.line += CodeLine('NEXT I');
    if chkAddColors.Checked then begin
      code.line += CodeLine('READ BYTE:POKE 712,BYTE');
      code.line += CodeLine('READ BYTE:POKE 708,BYTE');
      if not is01bit then begin
        code.line += CodeLine('READ BYTE:POKE 709,BYTE');
        code.line += CodeLine('READ BYTE:POKE 710,BYTE');
      end;
    end;

    //Inc(lineNum, 10);
    //code += IntToStr(LineNum) + ' IF PEEK(764)<>255 THEN POKE 764,255:END'#13#10;
    //Inc(lineNum, 10);
    //code += IntToStr(LineNum) + ' GOTO ' + IntToStr(LineNum - 10) + #13#10;
    code.line += WaitKeyCode(radLang.ItemIndex);

    // Picture data
//    Inc(lineNum, 10);
    code.line += GrDataValues(_TURBO_BASIC_XL, code.number, grBytes, rgDataType.ItemIndex, is01bit);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    if not chkTextWindow.Checked then
      strTextWindow := '+ 16';

    code.line := GrDataValues(_ACTION, code.number, grBytes, rgDataType.ItemIndex, is01bit);
    code.line += #13#10#13#10 +
                 '; Screen memory address'#13#10 +
                 'CARD SCREEN = 88'#13#10#13#10 +
                 '; Keyboard code of last key pressed'#13#10 +
                 'BYTE CH = $2FC'#13#10#13#10 +
                 'PROC Main()'#13#10#13#10 +
                 'Graphics(' + IntToStr(grMode) + strTextWindow + ')'#13#10#13#10 +
                 'MoveBlock(SCREEN, picData, ' + IntToStr(grBytes) + ')'#13#10;
    if chkAddColors.Checked then begin
      code.line += 'Poke(712, colors(0))'#13#10 +
                   'Poke(708, colors(1))'#13#10;
      if not is01bit then
        code.line += 'Poke(709, colors(2))'#13#10 +
                     'Poke(710, colors(3))'#13#10;
    end;

    //code += #13#10 +
    //        'DO UNTIL CH#255 OD'#13#10 +
    //        'CH=255'#13#10 +
    //        #13#10 +
    //        'RETURN';

    code.line += WaitKeyCode(radLang.ItemIndex) +
                 #13#10'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    if not chkTextWindow.Checked then
      strTextWindow := ' + 16';

    code.line := 'uses'#13#10 +
                 '  FastGraph, Crt;'#13#10#13#10;
    code.line += 'var'#13#10 +
                 ' screen : word absolute 88;'#13#10#13#10;
    code.line += GrDataValues(_MAD_PASCAL, code.number, grBytes, rgDataType.ItemIndex, is01bit);
    code.line += #13#10#13#10 +
                 'begin'#13#10 +
                 '  InitGraph(' + IntToStr(grMode) + strTextWindow + ');'#13#10#13#10 +
                 '  Move(picData, pointer(screen), ' + IntToStr(grBytes) + ');'#13#10;
    if chkAddColors.Checked then begin
      code.line += '  Poke(712, colors[0]);'#13#10 +
                   '  Poke(708, colors[1]);'#13#10;
      if not is01bit then
        code.line += '  Poke(709, colors[2]);'#13#10 +
                     '  Poke(710, colors[3]);'#13#10;
    end;
    code.line += WaitKeyCode(radLang.ItemIndex) +
                 'end.';
                 //'  repeat until KeyPressed;'#13#10 +
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    if not chkTextWindow.Checked then
      strTextWindow := ' + 16';

    code.line := GrDataValues(_FAST_BASIC, code.number, grBytes, rgDataType.ItemIndex, is01bit);
    code.line += #13#10#13#10 +
                 'GRAPHICS ' + IntToStr(grMode) + strTextWindow + #13#10 +
                 'scr = DPEEK(88)'#13#10 +
                 'MOVE ADR(picData), scr, ' + IntToStr(grBytes) + #13#10;
    if chkAddColors.Checked then begin
      code.line += 'POKE 712, colors(0)'#13#10 +
                   'POKE 708, colors(1)'#13#10;
      if not is01bit then
        code.line += 'POKE 709, colors(2)'#13#10 +
                     'POKE 710, colors(3)'#13#10;
    end;
    code.line += WaitKeyCode(radLang.ItemIndex);
  end;
  {----------------------------------------------------------------------------
   MAC/65
   CC65
   ---------------------------------------------------------------------------}

  result := code.line;
end;

// Data values
//function TfrmGrGen.Example04 : string;
//var
//  code : string;
//begin
//  {----------------------------------------------------------------------------
//   Mad Pascal
//   ---------------------------------------------------------------------------}
//  if radLang.ItemIndex = _MAD_PASCAL then begin
////    code := Gr15Opt(grBytes, rgDataType.ItemIndex);
//    code := Gr15Asm(editScanlines.Value - 1);
//  end;
//
//  result := code;
//end;

procedure TfrmGrGen.radLangProc(Sender: TObject);
begin
  //langIndex := radLang.ItemIndex;
  CreateCode;
end;

end.
//CARD SCR=88
//BYTE CH=764
//BYTE POINTER P
//BYTE ARRAY FN(30)
//CARD FILESIZE=[7680]
//
//GRAPHICS(15)
//SCOPY(FN,"D:TITLE.SCR")
//CLOSE(1)
//OPEN(1,FN,4,0)
//FOR P=SCR TO SCR+FILESIZE-1 DO
//  P^=GETD(1)
//OD
//P=712
//P^=GETD(1)
//FOR P=708 TO 710 DO
//  P^=GETD(1)
//OD
//CLOSE(1)
