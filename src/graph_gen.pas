{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Graphics editor - source code generator
}
unit graph_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, lcltype,
  ComCtrls, BCTrackbarUpdown, BCListBox, BCMDButton, BCMaterialDesignButton, Windows,
  common;

type
  { TfrmGraphGen }
  TfrmGraphGen = class(TForm)
    btnCopyToEditor : TBCMaterialDesignButton;
    btnClose : TBCMaterialDesignButton;
    boxStartLine : TGroupBox;
    btnAtariBASIC : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnKickC : TBCMDButton;
    btnKickC1 : TBCMDButton;
    btnKickC2 : TBCMDButton;
    btnKickC3 : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    chkAddColors: TCheckBox;
    chkTextWindow: TCheckBox;
    chkMAC65Lines: TCheckBox;
    editFilename: TEdit;
    editLineStep : TBCTrackbarUpdown;
    editStartLine : TBCTrackbarUpdown;
    grpColors: TGroupBox;
    grpMAC65Lines: TGroupBox;
    grpFilename: TGroupBox;
    lblLineStep1 : TLabel;
    lblStartLine1 : TLabel;
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
    procedure editStartLineMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure ListExamplesProc(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    procedure radLangProc(Sender: TObject);
    procedure btnCloseMouseEnter(Sender : TObject);
    procedure btnCloseMouseLeave(Sender : TObject);
    procedure btnCopyToEditorMouseEnter(Sender : TObject);
    procedure btnCopyToEditorMouseLeave(Sender : TObject);
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
  frmGraphGen: TfrmGraphGen;

implementation

{$R *.lfm}

uses
  src_editor, graph, code_lib, lib;

{ TfrmGraphGen }

procedure TfrmGraphGen.FormCreate(Sender: TObject);
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

  SetTrackBarUpDown(editStartLine, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editLineStep, $00DDDDDD, clWhite);
end;

procedure TfrmGraphGen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;

  langIndex := 0;
//  editFilename.Text := 'H1:' + ExtractFileName(frmGraph.filename);
  listExamples.ListBox.ItemIndex := 0;
  ListExamplesProc(Sender);
end;

procedure TfrmGraphGen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmGraphGen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

procedure TfrmGraphGen.CopyToEditorProc(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
//  memo.Lines.TextLineBreakStyle:=#$9b;
//  memo.Lines.LineBreak:=#$9b;
//  frmSrcEdit.editor.Lines.LineBreak:=#$9b;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor(Sender);
  Close;
end;

procedure TfrmGraphGen.ListExamplesProc(Sender: TObject);
begin
//  for i := 0 to radLang.Items.Count - 1 do begin
//    radLang.Controls[i].Enabled := listings[listExamples.ItemIndex, i];
////    radLang.Controls[i].Visible := listings[listExamples.ItemIndex, i];
//  end;
//  radLang.ItemIndex := langIndex;
  CreateCode;
end;

procedure TfrmGraphGen.CreateCode;
var
  code : string;
begin
  boxStartLine.Enabled := langIndex < 2;
  boxStartLine.Visible := boxStartLine.Enabled;

  if langIndex = 0 then begin
    radDataType.ItemIndex := 0;
    TRadioButton(radDataType.Controls[1]).Enabled := false;
//    TRadioButton(radDataType.Controls[2]).Enabled := false;
  end
  else begin
    TRadioButton(radDataType.Controls[1]).Enabled := true;
//    TRadioButton(radDataType.Controls[2]).Enabled := true;
  end;

  memo.Lines.Clear;

  case listExamples.ListBox.ItemIndex of
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
  end;

  memo.Lines.Add(code);

  // Set cursor position at the top of memo object
  memo.SelStart := 0;
  memo.SelLength := 0;
  SendMessage(memo.Handle, EM_SCROLLCARET, 0, 0);
end;

// Picture loader
function TfrmGraphGen.Example01 : string;
var
  strTextWindow : string = '';
begin
  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
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
    code.line += CodeLine('CLOSE #1');
    code.line += WaitKeyCode(langIndex);
//            '120 IF PEEK(764)<>255 THEN POKE 764,255:END'#13#10 +
//            '130 GOTO 120';
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
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
    code.line += CodeLine('CLOSE #%1');
    code.line += WaitKeyCode(langIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
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
                 WaitKeyCode(langIndex) +
                 #13#10'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
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
      code.line += #13#10'  // Read colors'#13#10 +
                   '  buf := pointer(712); '#13#10 +
                   '  BlockRead(f, buf, 1);'#13#10 +
                   '  buf := pointer(708);'#13#10;
      if is01bit then
        code.line += '  BlockRead(f, buf, 1);'#13#10
      else
        code.line += '  BlockRead(f, buf, 3);'#13#10;
    end;
    code.line += #13#10'  // Close file'#13#10 +
                 '  Close(f);'#13#10 +
                 //#13#10 +
                 //'  repeat until KeyPressed;'#13#10 +
                 //'  ReadKey;'#13#10 +
                 WaitKeyCode(langIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
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
                 WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

// Data values
function TfrmGraphGen.Example02 : string;
begin
  code.number := editStartLine.Value;
  code.step := editLineStep.Value;

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex = _ATARI_BASIC) or (langIndex = _TURBO_BASIC_XL) then
    code.line := GrDataValues(_ATARI_BASIC, code.number, grBytes, radDataType.ItemIndex, is01bit)
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then
    code.line := GrDataValues(_ACTION, code.number, grBytes, radDataType.ItemIndex, is01bit)
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then
    code.line := GrDataValues(_MAD_PASCAL, code.number, grBytes, radDataType.ItemIndex, is01bit)
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then
    code.line := GrDataValues(_FAST_BASIC, code.number, grBytes, radDataType.ItemIndex, is01bit)
  { Mad Assembler (MADS)
   ---------------------------------------------------------------------------}
  else if langIndex = _MADS then
    code.line := GrDataValues(_MADS, code.number, grBytes, radDataType.ItemIndex, is01bit)
  { MAC/65 / Atari Assembler/Editor
   ---------------------------------------------------------------------------}
  else if langIndex = _MAC65 then begin
    if not chkMAC65Lines.Checked then
      code.number := 65535;

    code.line := GrDataValues(_MAC65, code.number, grBytes, radDataType.ItemIndex, is01bit);
  end
  { CC65
   ---------------------------------------------------------------------------}
  else if langIndex = _CC65 then
    code.line := GrDataValues(_CC65, code.number, grBytes, radDataType.ItemIndex, is01bit)
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KickC then
    code.line := GrDataValues(_KickC, code.number, grBytes, radDataType.ItemIndex, is01bit);

  result := code.line;
end;

// Data values with picture loader
function TfrmGraphGen.Example03 : string;
var
  strTextWindow : string;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
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
    code.line += WaitKeyCode(langIndex);

    // Picture data
//    Inc(lineNum, 10);
    code.line += GrDataValues(_ATARI_BASIC, code.number, grBytes, radDataType.ItemIndex, is01bit);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    if not chkTextWindow.Checked then
      strTextWindow := '+16';

    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS ' + IntToStr(grMode) + strTextWindow) +
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('FOR I=%0 TO ' + IntToStr(grBytes - 1)) +
                 CodeLine('READ BYTE') +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine('NEXT I');
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
    code.line += WaitKeyCode(langIndex);

    // Picture data
//    Inc(lineNum, 10);
    code.line += GrDataValues(_TURBO_BASIC_XL, code.number, grBytes, radDataType.ItemIndex, is01bit);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    if not chkTextWindow.Checked then
      strTextWindow := '+ 16';

    code.line := GrDataValues(_ACTION, code.number, grBytes, radDataType.ItemIndex, is01bit);
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

    code.line += WaitKeyCode(langIndex) +
                 #13#10'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    if not chkTextWindow.Checked then
      strTextWindow := ' + 16';

    code.line := 'uses'#13#10 +
                 '  FastGraph, Crt;'#13#10#13#10;
    code.line += 'var'#13#10 +
                 ' screen : word absolute 88;'#13#10#13#10;
    code.line += GrDataValues(_MAD_PASCAL, code.number, grBytes, radDataType.ItemIndex, is01bit);
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
    code.line += WaitKeyCode(langIndex) +
                 'end.';
                 //'  repeat until KeyPressed;'#13#10 +
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    if not chkTextWindow.Checked then
      strTextWindow := ' + 16';

    code.line := GrDataValues(_FAST_BASIC, code.number, grBytes, radDataType.ItemIndex, is01bit);
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
    code.line += WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

// Data values for compressed gr. mode 15
//function TfrmGraphGen.Example04 : string;
//var
//  code : string;
//begin
//  if radLang.ItemIndex = _MAD_PASCAL then begin
////    code := Gr15Opt(grBytes, radDataType.ItemIndex);
//    code := Gr15Asm(editScanlines.Value - 1);
//  end;
//
//  result := code;
//end;

procedure TfrmGraphGen.radLangProc(Sender: TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmGraphGen.editStartLineMouseUp(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  CreateCode;
end;

procedure TfrmGraphGen.btnCopyToEditorMouseEnter(Sender : TObject);
begin
  btnCopyToEditor.NormalColor := $00CECECE;
  btnCopyToEditor.NormalColorEffect := clWhite;
end;

procedure TfrmGraphGen.btnCloseMouseEnter(Sender : TObject);
begin
  btnClose.NormalColor := $00CECECE;
  btnClose.NormalColorEffect := clWhite;
end;

procedure TfrmGraphGen.btnCopyToEditorMouseLeave(Sender : TObject);
begin
  btnCopyToEditor.NormalColor := clWhite;
  btnCopyToEditor.NormalColorEffect := clSilver;
end;

procedure TfrmGraphGen.btnCloseMouseLeave(Sender : TObject);
begin
  btnClose.NormalColor := clWhite;
  btnClose.NormalColorEffect := clSilver;
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

