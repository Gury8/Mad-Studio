{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Graphics editor - source code generator
}
unit graph_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, lcltype, BCListBox, BCMDButton, BCMaterialDesignButton,
  common;

type
  { TfrmGraphGen }
  TfrmGraphGen = class(TForm)
    boxColors : TGroupBox;
    btnCopyToEditor : TBCMaterialDesignButton;
    btnClose : TBCMaterialDesignButton;
    boxStartLine : TGroupBox;
    btnAtariBASIC : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnKickC : TBCMDButton;
    btnMads : TBCMDButton;
    btnMac65 : TBCMDButton;
    btnCC65 : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    chkMAC65Lines: TCheckBox;
    chkTextWindow : TCheckBox;
    chkUseColors : TCheckBox;
    color0 : TShape;
    color1 : TShape;
    color2 : TShape;
    color3 : TShape;
    color4 : TShape;
    editFilename: TEdit;
    editLineStep : TSpinEditEx;
    editStartLine : TSpinEditEx;
    boxMAC65Lines: TGroupBox;
    boxFilename: TGroupBox;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label7 : TLabel;
    Label8 : TLabel;
    lblLineStep1 : TLabel;
    lblStartLine1 : TLabel;
    listExamples : TBCPaperListBox;
    memo : TMemo;
    panelLang : TBCPaperPanel;
    radDataType : TRadioGroup;
    StaticText1 : TStaticText;
    StaticText2 : TStaticText;
    StaticText3 : TStaticText;
    procedure editFilenameChange(Sender : TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditorProc(Sender: TObject);
    procedure ListExamplesProc(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    procedure radLangProc(Sender: TObject);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
  private
    { private declarations }
    listings : TListings;
    grBytes : integer;
    isCreate : boolean;
    is01bit : boolean;
    procedure CreateCode;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
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
  isCreate := true;
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
      chkUseColors.Checked := false;
    end;
  end;

  SetListings(listings);

  // Example 2
  listings[1, 5] := false;
  listings[1, 6] := false;
  listings[1, 7] := false;
  listings[1, 8] := false;

  // Example 3
  listings[2, 5] := false;
  listings[2, 6] := false;
  listings[2, 7] := false;
  listings[2, 8] := false;
end;

procedure TfrmGraphGen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;

  if frmGraph.filename <> '' then
    editFilename.Text := 'H1:' + ExtractFileName(frmGraph.filename);

  color0.Brush.Color := colTab[0];   // POKE 712,C
  color1.Brush.Color := colTab[1];   // POKE 708,C
  color2.Brush.Color := colTab[2];   // POKE 709,C
  color3.Brush.Color := colTab[3];   // POKE 710,C
  color4.Brush.Color := colTab[10];  // POKE 711,C

  isCreate := false;
  langIndex := 0;
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
  if listExamples.ListBox.ItemIndex < 0 then exit;

  btnAtariBASIC.Enabled := listings[listExamples.ListBox.ItemIndex, 0];
  btnTurboBasicXL.Enabled := listings[listExamples.ListBox.ItemIndex, 1];
  btnMadPascal.Enabled := listings[listExamples.ListBox.ItemIndex, 2];
  btnEffectus.Enabled := listings[listExamples.ListBox.ItemIndex, 3];
  btnFastBasic.Enabled := listings[listExamples.ListBox.ItemIndex, 4];
  btnKickC.Enabled := listings[listExamples.ListBox.ItemIndex, 8];
  btnMads.Enabled := listings[listExamples.ListBox.ItemIndex, 5];
  btnCC65.Enabled := listings[listExamples.ListBox.ItemIndex, 7];
  btnMac65.Enabled := listings[listExamples.ListBox.ItemIndex, 6];

  boxColors.Enabled := listExamples.ListBox.ItemIndex > 0;
  boxColors.Visible := boxColors.Enabled;

  boxMAC65Lines.Enabled := listExamples.ListBox.ItemIndex = 0;
  boxMAC65Lines.Visible := boxMAC65Lines.Enabled;

  boxFilename.Enabled := listExamples.ListBox.ItemIndex = 2;
  boxFilename.Visible := boxFilename.Enabled;

  chkTextWindow.Enabled := listExamples.ListBox.ItemIndex > 0;
  chkTextWindow.Visible := chkTextWindow.Enabled;

  CreateCode;
end;

procedure TfrmGraphGen.CreateCode;
var
  code : string;
begin
  Set01(boxStartLine, langIndex, radDataType, false);

  case listExamples.ListBox.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
  end;

  Set02(memo, code);
end;

{-----------------------------------------------------------------------------
 Screen data values
 -----------------------------------------------------------------------------}
function TfrmGraphGen.Example01 : string;
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

{-----------------------------------------------------------------------------
 Display picture screen
 -----------------------------------------------------------------------------}
function TfrmGraphGen.Example02 : string;
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
    if chkUseColors.Checked then begin
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
    if chkUseColors.Checked then begin
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
    if chkUseColors.Checked then begin
      code.line += 'Poke(712, colors(0))'#13#10 +
                   'Poke(708, colors(1))'#13#10;
      if not is01bit then
        code.line += 'Poke(709, colors(2))'#13#10 +
                     'Poke(710, colors(3))'#13#10;
    end;

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
    if chkUseColors.Checked then begin
      code.line += '  Poke(712, colors[0]);'#13#10 +
                   '  Poke(708, colors[1]);'#13#10;
      if not is01bit then
        code.line += '  Poke(709, colors[2]);'#13#10 +
                     '  Poke(710, colors[3]);'#13#10;
    end;
    code.line += WaitKeyCode(langIndex) +
                 'end.';
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
    if chkUseColors.Checked then begin
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

{-----------------------------------------------------------------------------
 Load picture from disk
 -----------------------------------------------------------------------------}
function TfrmGraphGen.Example03 : string;
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
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256');
//                 CodeLine('FOR I=0 TO ' + IntToStr(grBytes - 1)) +
//                 CodeLine('GET #1,BYTE') +
//                 CodeLine('POKE SCR+I,BYTE') +
//                 CodeLine('NEXT I');

//    code.line += SetCIOFastLoad(_BASIC_SCR_MEM_VAR, true, true);
    //DisplayScreen(langIndex, antic_mode_max_x, antic_mode_max_y, true);
    code.line += CodeLine('REM SET CIO FOR FASTER FILE LOAD');
    code.line += CodeLine('X=16:DIM ML$(7)') +
                 CodeLine('ML$="hhh*LV*":ML$(4,4)=CHR$(170):ML$(7,7)=CHR$(228)') +
                 CodeLine('ICCOM=834:ICBADR=836:ICBLEN=840');
    code.line += CodeLine('REM High and low address for IOCB #1') +
                 CodeLine('POKE ICBADR+X+1,INT(SCR/256):POKE ICBADR+X,SCR-INT(SCR/256)*256') +
                 CodeLine('POKE ICBLEN+X+1,15:POKE ICBLEN+X,0') +
                 CodeLine('REM CALL CIO (7 = "get" command, 16 = IOCB #1)') +
                 CodeLine('POKE ICCOM+X,7:A=USR(ADR(ML$),X)');

    if chkUseColors.Checked then begin
      code.line += CodeLine('GET #1,BYTE:POKE 712,BYTE');
      code.line += CodeLine('GET #1,BYTE:POKE 708,BYTE');
      if not is01bit then begin
        code.line += CodeLine('GET #1,BYTE:POKE 709,BYTE');
        code.line += CodeLine('GET #1,BYTE:POKE 710,BYTE');
      end;
    end;
    code.line += CodeLine('CLOSE #1');
    code.line += WaitKeyCode(langIndex);
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
    if chkUseColors.Checked then begin
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
    if chkUseColors.Checked then begin
      code.line += #13#10'DATA=GETD(1) POKE(712,DATA)'#13#10 +
                   'DATA=GETD(1) POKE(708,DATA)'#13#10;
      if not is01bit then
        code.line += 'DATA=GETD(1) POKE(709,DATA)'#13#10 +
                     'DATA=GETD(1) POKE(710,DATA)'#13#10;
    end;
    code.line += #13#10'CLOSE(1)'#13#10 +
                 WaitKeyCode(langIndex) +
                 #13#10'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    if not chkTextWindow.Checked then
      strTextWindow := ' + 16';

    //             'const'#13#10 +
    //             '  picSize = ' + IntToStr(grBytes - 1) + ';'#13#10 +
    //             'var'#13#10 +
    //             '  f : file;'#13#10 +
    //             '  filename : TString;'#13#10 +
    //             '  buf : pointer;'#13#10 +
    //             'begin'#13#10 +
    //             '  InitGraph(' + IntToStr(grMode) + strTextWindow + ');'#13#10 +
    //             '  buf := pointer(DPeek(88));'#13#10 +
    //             '  // Open file'#13#10 +
    //             '  filename := ''' + editFilename.Text + ''';'#13#10 +
    //             '  Assign(f, filename);'#13#10 +
    //             '  // Read data'#13#10 +
    //             '  Reset(f, 1);'#13#10 +
    //             '  BlockRead(f, buf, picSize);'#13#10;
    //if chkUseColors.Checked then begin
    //  code.line += #13#10'  // Read colors'#13#10 +
    //               '  buf := pointer(712); '#13#10 +
    //               '  BlockRead(f, buf, 1);'#13#10 +
    //               '  buf := pointer(708);'#13#10;
    //  if is01bit then
    //    code.line += '  BlockRead(f, buf, 1);'#13#10
    //  else
    //    code.line += '  BlockRead(f, buf, 3);'#13#10;
    //end;
    //code.line += #13#10'  // Close file'#13#10 +
    //             '  Close(f);'#13#10 +
    //             WaitKeyCode(langIndex) +
    //             'end.';

    code.line := '// Load picture'#13#10#13#10 +
                 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt, Cio;'#13#10#13#10 +
                 'const'#13#10 +
                 '  picSize = ' + IntToStr(grBytes - 1) + ';'#13#10 +
                 'var'#13#10 +
                 '  scr : word;'#13#10 +
                 '  cnt : byte = 0;'#13#10 +
                 '  maxX, maxY : byte;'#13#10 +
                 '  colReg : array[0..4] of byte absolute 708;'#13#10;
    code.line += '  i, data : byte;'#13#10#13#10 +
                 'begin'#13#10 +
                 '  InitGraph(' + IntToStr(grMode) + strTextWindow + ');'#13#10 +
                 '  scr := DPeek(88);'#13#10 +
                 '  // Open file'#13#10 +
                 '  Cls(1);'#13#10;

    code.line += '  Opn(1, 4, 0, ' + QuotedStr(editFilename.Text) + ');'#13#10#13#10;
    //code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, true);
    code.line += '  BGet(1, pointer(scr), picSize);'#13#10#13#10;

    if chkUseColors.Checked then begin
      code.line += '  // Read color values'#13#10 +
                   '  colReg[4] := Get(1);'#13#10 +
                   '  colReg[0] := Get(1);'#13#10 +
                   '  colReg[1] := Get(1);'#13#10 +
                   '  colReg[2] := Get(1);'#13#10 +
                   '  colReg[3] := Get(1);'#13#10 +
                   '  Poke(712, colReg[4]);'#13#10 +
                   '  Poke(708, colReg[0]);'#13#10 +
                   '  Poke(709, colReg[1]);'#13#10 +
                   '  Poke(710, colReg[2]);'#13#10 +
                   '  Poke(711, colReg[3]);'#13#10#13#10;
    end;

    code.line += '  // Close file'#13#10 +
                 '  Cls(1);'#13#10 +
                 WaitKeyCode(langIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    if not chkTextWindow.Checked then
      strTextWindow := ' + 16';

    code.line := 'GRAPHICS ' + IntToStr(grMode) + strTextWindow + #13#10#13#10 +
                 'CLOSE #1'#13#10 +
                 'OPEN #1, 4, 0, "' + editFilename.Text + '"'#13#10 +
                 'BGET #1, DPEEK(88), ' + IntToStr(grBytes - 1) + #13#10;
    if chkUseColors.Checked then begin
      code.line += 'BGET #1, PEEK(712), 1'#13#10 +
                   'BGET #1, PEEK(708), 1'#13#10;
      if not is01bit then
        code.line += 'BGET #1, PEEK(709), 1'#13#10 +
                     'BGET #1, PEEK(710), 1'#13#10;
    end;
    code.line += 'CLOSE #1'#13#10 +
                 WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

procedure TfrmGraphGen.radLangProc(Sender: TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmGraphGen.editFilenameChange(Sender : TObject);
begin
  if not isCreate then
    CreateCode;
end;

procedure TfrmGraphGen.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmGraphGen.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
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

