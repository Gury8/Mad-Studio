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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, ComCtrls, StdCtrls, ExtCtrls,
  lcltype, BCTrackbarUpdown, BCListBox, BCMDButton, BCMaterialDesignButton, Windows,
  common;

type
  { TfrmAntic2Gen }
  TfrmAntic2Gen = class(TForm)
    btnCopyToEditor : TBCMaterialDesignButton;
    btnClose : TBCMaterialDesignButton;
    chkMaxSize : TCheckBox;
    panelLang : TBCPaperPanel;
    boxStartLine : TGroupBox;
    btnAtariBASIC : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnKickC : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    editFilename: TLabeledEdit;
    editLineStep : TBCTrackbarUpdown;
    editMaxX : TBCTrackbarUpdown;
    editMaxY : TBCTrackbarUpdown;
    editStartLine : TBCTrackbarUpdown;
    GroupBox1 : TGroupBox;
    Label5 : TLabel;
    Label6 : TLabel;
    lblLineStep1 : TLabel;
    lblStartLine1 : TLabel;
    ListExamples : TBCPaperListBox;
    memo : TMemo;
    radDataType : TRadioGroup;
    StaticText1 : TStaticText;
    StaticText2 : TStaticText;
    StaticText3 : TStaticText;
    procedure CheckMaxSizeProc(Sender : TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CreateCodeProc(Sender : TObject);
    procedure editLineStepMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure CopyToEditorProc(Sender: TObject);
    procedure ExamplesProc(Sender: TObject);
    procedure radLangProc(Sender: TObject);
    procedure btnCloseMouseEnter(Sender : TObject);
    procedure btnCloseMouseLeave(Sender : TObject);
    procedure btnCopyToEditorMouseEnter(Sender : TObject);
    procedure btnCopyToEditorMouseLeave(Sender : TObject);
    procedure CloseWinProc(Sender: TObject);
  private
    { private declarations }
    listings : TListings;
    isMaxSize : boolean;
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

  SetTrackBarUpDown(editStartLine, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editLineStep, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editMaxX, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editMaxY, $00DDDDDD, clWhite);
end;

procedure TfrmAntic2Gen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;

  editFilename.Text:= 'H1:' + ExtractFileName(frmAntic2.filename);
  editMaxX.Value := frmAntic2.editX.Value;
  editMaxY.Value := frmAntic2.editY.Value;

  langIndex := 0;
  ListExamples.ListBox.ItemIndex := 0;
  ExamplesProc(Sender);
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

  isMaxSize := (editMaxX.Value = 39) and (editMaxY.Value = 23);
  chkMaxSize.Checked := isMaxSize;

  // List of listing examples
  case ListExamples.ListBox.ItemIndex of
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
  frmSrcEdit.MemoToEditor(Sender);
  Close;
end;

function TfrmAntic2Gen.Example01 : string;
var
  maxSize : word;
begin
  maxSize := (editMaxX.Value + 1)*(editMaxY.Value + 1) - 1;

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM ******************************') +
                 CodeLine('REM LOAD TEXT MODE 0 SCREEN') +
                 CodeLine('REM ******************************') +
                 CodeLine('GRAPHICS 0') +
                 CodeLine('CLOSE #1') +
                 CodeLine('OPEN #1,4,0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256');
    if isMaxSize then
      code.line += CodeLine('SIZE=' + IntToStr(maxSize)) +
                   CodeLine('FOR I=0 TO SIZE') +
                   CodeLine('GET #1,BYTE') +
                   CodeLine('POKE SCR+I,BYTE')
    else
      code.line += CodeLine('GET #1,MAXX') +
                   CodeLine('GET #1,MAXY') +
                   CodeLine('SIZE=(MAXX+1)*(MAXY+1)-1:CNT=0') +
                   CodeLine('FOR I=0 TO SIZE') +
                   CodeLine('GET #1,BYTE:IF CNT=MAXX + 1' +  // IntToStr(editMaxX.Value + 1) +
                            ' THEN SCR=SCR+40-CNT:CNT=0') +
                   CodeLine('POKE SCR+I,BYTE:CNT=CNT+1');

    code.line += CodeLine('NEXT I') +
                 CodeLine('CLOSE #1') +
                 WaitKeyCode(langIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM LOAD TEXT MODE 0 SCREEN') +
                 CodeLine('--') +
                 CodeLine('GRAPHICS %0') +
                 CodeLine('CLOSE #%1') +
                 CodeLine('OPEN #%1,4,%0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=DPEEK(88)');

    if isMaxSize then
      code.line += CodeLine('SIZE=' + IntToStr(maxSize)) +
                   CodeLine('FOR I=%0 TO SIZE') +
                   CodeLine('GET #%1,BYTE') +
                   CodeLine('POKE SCR+I,BYTE')
    else
      code.line += CodeLine('GET #%1,MAXX') +
                   CodeLine('GET #%1,MAXY') +
                   CodeLine('SIZE=(MAXX+%1)*(MAXY+%1)-%1:CNT=%0') +
                   CodeLine('FOR I=%0 TO SIZE') +
                   CodeLine('GET #%1,BYTE:IF CNT=MAXX + 1' +  //IntToStr(editMaxX.Value + 1) +
                            ' THEN SCR=SCR+40-CNT:CNT=%0') +
                   CodeLine('POKE SCR+I,BYTE:CNT=CNT+1');

    code.line += CodeLine('NEXT I') +
                 CodeLine('CLOSE #%1') +
                 WaitKeyCode(langIndex);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    if isMaxSize then begin
      code.line := '// Load text mode 0 screen'#13#10#13#10 +
                   'uses'#13#10 +
                   '  SysUtils, FastGraph, Crt;'#13#10#13#10 +
                   'const'#13#10 +
                   '  picSize = ' + IntToStr(maxSize) + ';'#13#10#13#10 +
                   'var'#13#10 +
                   '  f : file;'#13#10 +
                   '  filename : TString;'#13#10 +
                   '  buf : pointer;'#13#10 +
                   'begin'#13#10 +
                   '  InitGraph(0);'#13#10 +
                   '  buf := pointer(DPeek(88));'#13#10#13#10 +
                   '  // Open file'#13#10 +
                   '  filename := ' + QuotedStr(editFilename.Text) + ';'#13#10 +
                   '  Assign(f, filename);'#13#10#13#10 +
                   '  // Read data'#13#10 +
                   '  Reset(f, 1);'#13#10 +
                   '  BlockRead(f, buf, picSize);'#13#10#13#10 +
                   '  // Close file'#13#10 +
                   '  Close(f);'#13#10 +
                   WaitKeyCode(langIndex) +
                   'end.';
    end
    else begin
      code.line := '// Load text mode 0 screen'#13#10#13#10 +
                   'uses'#13#10 +
                   '  Crt, SySutils, CIO;'#13#10#13#10 +
                   'var'#13#10 +
                   '  data, maxX, maxY : byte;'#13#10 +
                   '  cnt : byte = 0;'#13#10#13#10 +
                   '  size : word;'#13#10 +
                   '  screen : word;'#13#10 +
                   '  i : word;'#13#10 +
                   'begin'#13#10 +
                   '  screen := DPeek(88);'#13#10 +
                   '  // Set up channel 2 for screen'#13#10 +
                   '  Cls(2);'#13#10 +
                   '  Opn(2, 4, 0, ' + QuotedStr(editFilename.Text) + ');'#13#10#13#10 +
                   '  // Read image dimension values'#13#10 +
                   '  maxX := Get(2);'#13#10 +
                   '  maxY := Get(2);'#13#10 +
                   '  size := (maxX + 1)*(maxY + 1) - 1;'#13#10#13#10 +
                   '  // Read image data'#13#10 +
                   '  for i := 0 TO size do begin'#13#10 +
                   '    data := Get(2);'#13#10 +
                   '    if cnt = maxX + 1 then begin'#13#10 +
                   '      Inc(screen, 40 - cnt);'#13#10 +
                   '      cnt := 0;'#13#10 +
                   '    end;'#13#10 +
                   '    DPoke(screen + i, data);'#13#10 +
                   '    Inc(cnt);'#13#10 +
                   '  end;'#13#10#13#10 +
                   '  // Close channel 2'#13#10 +
                   '  Cls(2);'#13#10 +
                   WaitKeyCode(langIndex) +
                   'end.';
    end;

    //if isMaxSize then
    //  code.line += '  Move(screenData, pointer(screen), ' + IntToStr(maxSize + 1) + ');'#13#10
    //else begin
    //  code.line += 'for i := 0 to ' + IntToStr(maxSize) + ' do begin'#13#10 +
    //               '  if cnt = ' + IntToStr(editMaxX.Value + 1) + ' then begin'#13#10 +
    //               '    Inc(screen, 40 - cnt);'#13#10 +
    //               '    cnt := 0;'#13#10 +
    //               '  end;'#13#10 +
    //               '  Poke(screen + i, screenData[i]);'#13#10 +
    //               '  Inc(cnt);'#13#10 +
    //               'end;'#13#10;
    //end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := '; Load text mode 0 screen'#13#10#13#10 +
                 'BYTE ch=$2FC'#13#10 +
                 'BYTE data'#13#10 +
                 'CARD screen'#13#10 +
                 'CARD n'#13#10;

    if not isMaxSize then
      code.line += 'BYTE maxX, maxY, cnt=[0]'#13#10 +
                   'CARD size'#13#10
    else
      code.line += 'CARD size=[' + IntToStr(maxSize) + ']'#13#10;

    code.line += 'PROC Main()'#13#10#13#10 +
                 'Put(125)'#13#10 +
                 'screen = PeekC(88)'#13#10#13#10 +
                 '; Set up channel 2 for screen'#13#10 +
                 'Close(2)'#13#10 +
                 'Open(2,"' + editFilename.Text + '",4,0)'#13#10#13#10;

    if not isMaxSize then
      code.line += '; Read image dimension values'#13#10 +
                   'maxX = GetD(2)'#13#10 +
                   'maxY = GetD(2)'#13#10 +
                   'size = (maxX + 1)*(maxY + 1) - 3'#13#10#13#10;

    code.line += '; Read image data'#13#10 +
                 'FOR n = 0 TO size DO'#13#10 +
                 '  data = GetD(2)'#13#10;
    if not isMaxSize then
      code.line += '  IF cnt = maxX + 1 THEN'#13#10 +
                   '    screen = screen + 40 - cnt'#13#10 +
                   '    cnt = 0;'#13#10 +
                   '  FI'#13#10;

    code.line += '  PokeC(screen + n, data)'#13#10 +
                 '  cnt==+1' + #13#10 +
                 'OD'#13#10#13#10 +
                 '; Close channel 2'#13#10 +
                 'Close(2)'#13#10 +
                 WaitKeyCode(langIndex) +
                 #13#10 +
                 'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := ''' Load text mode 0 screen'#13#10#13#10 +
                 'GRAPHICS 0'#13#10#13#10 +
                 'CLOSE #1'#13#10 +
                 'OPEN #1, 4, 0, "' + editFilename.Text + '"'#13#10#13#10 +
       //          '''BGET #1, DPEEK(88), 3839'#13#10 +
                 'scr = DPEEK(88)'#13#10;

    if not isMaxSize then
      code.line += 'cnt = 0'#13#10#13#10 +
                   ''' Read image dimension values'#13#10 +
                   'GET #1, maxX'#13#10 +
                   'GET #1, maxY'#13#10 +
                   'size = (maxX + 1)*(maxY + 1) - 1'#13#10#13#10
    else
      code.line += 'size = ' + IntToStr(maxSize) + #13#10#13#10;

    code.line += ''' Read image data'#13#10 +
                 'FOR i = 0 TO size'#13#10 +
                 '  GET #1, BYTE'#13#10;
    if not isMaxSize then
      code.line += '  IF cnt = maxX + 1'#13#10 +  // + IntToStr(editMaxX.Value + 1) + #13#10 +
                   '    scr = scr + 40 - cnt'#13#10 +
                   '    cnt = 0'#13#10 +
                   '  ENDIF'#13#10;

    code.line += '  POKE scr + i, BYTE'#13#10 +
                 '  cnt = cnt + 1'#13#10 +
                 'NEXT'#13#10#13#10 +
                 'CLOSE #1'#13#10 +
                 WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Data values
 -----------------------------------------------------------------------------}
function TfrmAntic2Gen.Example02 : string;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex = _ATARI_BASIC) or (langIndex = _TURBO_BASIC_XL) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := DataValues(_ATARI_BASIC, frmAntic2.fldAtascii, code.number, code.step,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then
//    frmAntic2.maxSize + 1
    code.line := DataValues(_ACTION, frmAntic2.fldAtascii,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex)
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then
    code.line := DataValues(_MAD_PASCAL, frmAntic2.fldAtascii,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex)
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then
    code.line := DataValues(_FAST_BASIC, frmAntic2.fldAtascii,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex)
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then
    code.line := DataValues(_KICKC, frmAntic2.fldAtascii,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);

  //{
  // Mad Assembler (MADS)
  // ---------------------------------------------------------------------------}
  //else if langIndex = _MADS then begin
  //end
  //{
  // MAC/65 / Atari Assembler/Editor
  // ---------------------------------------------------------------------------}
  ////else if langIndex = _MAC65 then begin
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
  //else if langIndex = _CC65 then begin
  //end;

  result := code.line;
end;

// Data values with screen loader
function TfrmAntic2Gen.Example03 : string;
var
  maxSize : word;
begin
  maxSize := (editMaxX.Value + 1)*(editMaxY.Value + 1) - 1;

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS 0');
    code.line += CodeLine('SCR=PEEK(88)+PEEK(89)*256');

    if not isMaxSize then
      code.line += CodeLine('CNT=0');

    code.line += CodeLine('FOR I=0 TO ' + IntToStr(maxSize));
    code.line += CodeLine('READ BYTE');
    if not isMaxSize then
      code.line += CodeLine('IF CNT=' + IntToStr(editMaxX.Value + 1) +
                   ' THEN SCR=SCR+40-CNT:CNT=0');

    code.line += CodeLine('POKE SCR+I,BYTE');
    if not isMaxSize then
      code.line += CodeLine('CNT=CNT+1');

    code.line += CodeLine('NEXT I');
    code.line += WaitKeyCode(langIndex);

    // Screen data
    code.line += DataValues(_ATARI_BASIC, frmAntic2.fldAtascii, code.number, code.step,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS %0');
    code.line += CodeLine('SCR=DPEEK(88)');
    if not isMaxSize then
      code.line += CodeLine('CNT=%0');

    code.line += CodeLine('FOR I=%0 TO ' + IntToStr(maxSize));
    code.line += CodeLine('READ BYTE');
    if not isMaxSize then
      code.line += CodeLine('IF CNT=' + IntToStr(editMaxX.Value + 1) + ' THEN SCR=SCR+40-CNT:CNT=%0');

    code.line += CodeLine('POKE SCR+I,BYTE');
    if not isMaxSize then
      code.line += CodeLine('CNT=CNT+%1');

    code.line += CodeLine('NEXT I');
    code.line += WaitKeyCode(langIndex);

    // Screen data
    code.line += DataValues(_TURBO_BASIC_XL, frmAntic2.fldAtascii, code.number, code.step,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
//    frmAntic2.maxSize + 1
    code.line := DataValues(_ACTION, frmAntic2.fldAtascii, editMaxY.Value, editMaxX.Value,
                            radDataType.ItemIndex);
    code.line += #13#10'; Screen memory address'#13#10 +
                 'CARD SCREEN = 88'#13#10#13#10 +
                 'CARD i'#13#10 +
                 'CARD cnt = [0]'#13#10#13#10 +
                 '; Keyboard code of last key pressed'#13#10 +
                 'BYTE CH = $2FC'#13#10#13#10 +
                 'PROC Main()'#13#10#13#10 +
                 'Graphics(0)'#13#10#13#10;
    if isMaxSize then
      code.line += 'MoveBlock(SCREEN, screenData, ' + IntToStr(maxSize + 1) + ')'#13#10
    else begin
      code.line += 'SCREEN=PEEKC(88)'#13#10;
      code.line += 'FOR i = 0 TO ' + IntToStr(maxSize) + ' DO'#13#10 +
                   '  IF cnt = ' + IntToStr(editMaxX.Value + 1) + ' THEN'#13#10 +
                   '    screen ==+ 40 - cnt'#13#10 +
                   '    cnt = 0'#13#10 +
                   '  FI'#13#10 +
                   '  POKEC(screen + i, screenData(i))'#13#10 +
//                   '  cnt==+1'#13#10 +
                   '  cnt=cnt+1'#13#10 +
                   'OD'#13#10;
    end;
    code.line += WaitKeyCode(langIndex) +
                 #13#10'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
//    frmAntic2.maxSize + 1
    code.line := 'uses'#13#10 +
                 '  FastGraph, Crt;'#13#10#13#10 +
                 'var'#13#10 +
                 '  screen : word absolute 88;'#13#10 +
                 '  i : word;'#13#10 +
                 '  cnt : word = 0;'#13#10;
    code.line += DataValues(_MAD_PASCAL, frmAntic2.fldAtascii, editMaxY.Value, editMaxX.Value,
                            radDataType.ItemIndex);
    code.line += #13#10'begin'#13#10 +
                 '  InitGraph(0);'#13#10#13#10;

    if isMaxSize then
      code.line += '  Move(screenData, pointer(screen), ' + IntToStr(maxSize + 1) + ');'#13#10
    else begin
      code.line += 'for i := 0 to ' + IntToStr(maxSize) + ' do begin'#13#10 +
                   '  if cnt = ' + IntToStr(editMaxX.Value + 1) + ' then begin'#13#10 +
                   '    Inc(screen, 40 - cnt);'#13#10 +
                   '    cnt := 0;'#13#10 +
                   '  end;'#13#10 +
                   '  Poke(screen + i, screenData[i]);'#13#10 +
                   '  Inc(cnt);'#13#10 +
                   'end;'#13#10;
    end;

    code.line += WaitKeyCode(langIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := DataValues(_FAST_BASIC, frmAntic2.fldAtascii,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
    code.line += #13#10#13#10'GRAPHICS 0'#13#10 +
                 'scr = DPEEK(88)'#13#10;
    if isMaxSize then
      code.line += 'MOVE ADR(screenData), scr, ' + IntToStr(maxSize + 1) + #13#10
    else begin
      code.line += 'cnt = 0'#13#10 +
                   'FOR i = 0 TO ' + IntToStr(maxSize) + #13#10 +
                   '  IF cnt = ' + IntToStr(editMaxX.Value + 1) + #13#10 +
                   '    scr = scr + 40 - cnt'#13#10 +
                   '    cnt = 0'#13#10 +
                   '  ENDIF'#13#10 +
                   '  POKE scr + i, screenData(i)'#13#10 +
                   '  cnt = cnt + 1'#13#10 +
                   'NEXT'#13#10;
    end;
    code.line += WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

procedure TfrmAntic2Gen.ExamplesProc(Sender: TObject);
begin
//  for i := 0 to radLang.Items.Count - 1 do begin
//    radLang.Controls[i].Enabled := listings[ListExamples.ItemIndex, i];
////    radLang.Controls[i].Visible := listings[ListExamples.ItemIndex, i];
//  end;
//  if langIndex < 5 then
//    radLang.ItemIndex := langIndex;

  editFilename.Visible := ListExamples.ListBox.ItemIndex = 0;
  editFilename.Enabled := ListExamples.ListBox.ItemIndex = 0;
  CreateCode;
end;

procedure TfrmAntic2Gen.radLangProc(Sender: TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmAntic2Gen.editLineStepMouseUp(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  CreateCode;
end;

procedure TfrmAntic2Gen.CreateCodeProc(Sender : TObject);
begin
  CreateCode;
end;

procedure TfrmAntic2Gen.btnCopyToEditorMouseEnter(Sender : TObject);
begin
  btnCopyToEditor.NormalColor := $00CECECE;
  btnCopyToEditor.NormalColorEffect := clWhite;
end;

procedure TfrmAntic2Gen.btnCopyToEditorMouseLeave(Sender : TObject);
begin
  btnCopyToEditor.NormalColor := clWhite;
  btnCopyToEditor.NormalColorEffect := clSilver;
end;

procedure TfrmAntic2Gen.btnCloseMouseEnter(Sender : TObject);
begin
  btnClose.NormalColor := $00CECECE;
  btnClose.NormalColorEffect := clWhite;
end;

procedure TfrmAntic2Gen.btnCloseMouseLeave(Sender : TObject);
begin
  btnClose.NormalColor := clWhite;
  btnClose.NormalColorEffect := clSilver;
end;

procedure TfrmAntic2Gen.CheckMaxSizeProc(Sender : TObject);
begin
  if chkMaxSize.Checked then begin
    editMaxX.Value := 39;
    editMaxY.Value := 23;
  end;
//  CreateCode;
end;

procedure TfrmAntic2Gen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

