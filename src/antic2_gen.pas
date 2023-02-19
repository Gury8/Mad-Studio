{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Antic mode 2 editor - source code generator
}
unit antic2_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, StdCtrls,
  ExtCtrls, lcltype, BCListBox, BCMDButton, BCMaterialDesignButton,
  common;

type
  { TfrmAntic2Gen }
  TfrmAntic2Gen = class(TForm)
    btnCopyToEditor : TBCMaterialDesignButton;
    btnClose : TBCMaterialDesignButton;
    chkCharSet : TCheckBox;
    chkMaxSize : TCheckBox;
    editFontName : TLabeledEdit;
    editLineStep : TSpinEditEx;
    editMaxY : TSpinEditEx;
    editStartLine : TSpinEditEx;
    editMaxX : TSpinEditEx;
    boxCharSet : TGroupBox;
    panelLang : TBCPaperPanel;
    boxStartLine : TGroupBox;
    btnAtariBASIC : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnKickC : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    editFilename: TLabeledEdit;
    boxResizeScreen : TGroupBox;
    Label5 : TLabel;
    Label6 : TLabel;
    lblLineStep : TLabel;
    lblStartLine : TLabel;
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
    procedure CopyToEditorProc(Sender: TObject);
    procedure ExamplesProc(Sender: TObject);
    procedure radLangProc(Sender: TObject);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
    procedure CloseWinProc(Sender: TObject);
  private
    { private declarations }
    listings : TListings;
    isMaxSize : boolean;
    procedure CreateCode;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
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
  antic_mode_max_x := _ANTIC_MODE_2_MAX_X;
  antic_mode_max_y := _ANTIC_MODE_2_MAX_Y;

  SetListings(listings);

  // Example 2
  listings[1, 8] := false;

  // Example 3
  listings[2, 8] := false;
end;

procedure TfrmAntic2Gen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;

  editFilename.Text:= 'H1:' + ExtractFileName(frmAntic2.filename);
  editFontname.Text := 'H1:' + ExtractFileName(frmAntic2.fontName);
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
//  boxStartLine.Enabled := langIndex < 2;
//  boxStartLine.Visible := boxStartLine.Enabled;
//
//  if langIndex = 0 then begin
//    radDataType.ItemIndex := 0;
//    TRadioButton(radDataType.Controls[1]).Enabled := false;
////    TRadioButton(radDataType.Controls[2]).Enabled := false;
//  end
//  else begin
//    TRadioButton(radDataType.Controls[1]).Enabled := true;
////    TRadioButton(radDataType.Controls[2]).Enabled := true;
//  end;

  Set01(boxStartLine, langIndex, radDataType, false);

  editFontname.Enabled := chkCharSet.Checked;

  isMaxSize := (editMaxX.Value = 39) and (editMaxY.Value = 23);
  chkMaxSize.Checked := isMaxSize;

  // List of listing examples
  case ListExamples.ListBox.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
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

procedure TfrmAntic2Gen.CopyToEditorProc(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor(Sender);
  Close;
end;

{-----------------------------------------------------------------------------
 Data values
 -----------------------------------------------------------------------------}
function TfrmAntic2Gen.Example01 : string;
begin
  isConstData := false;

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

{-----------------------------------------------------------------------------
 Display text mode 0 screen
 -----------------------------------------------------------------------------}
function TfrmAntic2Gen.Example02 : string;
var
  maxSize : word;
begin
  maxSize := (editMaxX.Value + 1)*(editMaxY.Value + 1) - 1;
  isConstData := false;
//  code.line := '';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;

    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Display text mode 0 screen') +
                 CodeLine(_REM);
    if chkCharSet.Checked then begin
      code.line += CodeLine('TOPMEM=PEEK(106)-8');
      code.line += CodeLine('POKE 106,TOPMEM');
      code.line += CodeLine('CHRAM=TOPMEM*256');
    end;

    code.line += CodeLine('GRAPHICS 0');
    code.line += CodeLine(_BASIC_SCR_MEM_VAR + '=PEEK(88)+PEEK(89)*256');
    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += CodeLine('SIZE=' + IntToStr(maxSize));
    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, false);
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

    code.line := CodeLine('--') +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Display text mode 0 screen') +
                 CodeLine('--');
    if chkCharSet.Checked then begin
      code.line += CodeLine('TOPMEM=PEEK(106)-8');
      code.line += CodeLine('POKE 106,TOPMEM');
      code.line += CodeLine('CHRAM=TOPMEM*256');
    end;

    code.line += CodeLine('GRAPHICS %0');
    code.line += CodeLine(_BASIC_SCR_MEM_VAR + '=DPEEK(88)');

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += CodeLine('SIZE=' + IntToStr(maxSize));
    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, false);
    code.line += WaitKeyCode(langIndex);

    // Screen data
    code.line += DataValues(_TURBO_BASIC_XL, frmAntic2.fldAtascii, code.number, code.step,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := ';' + _REM_MAD_STUDIO + #13#10 +
                 '; Display text mode 0 screen'#13#10#13#10;
    code.line += DataValues(_ACTION, frmAntic2.fldAtascii, editMaxY.Value, editMaxX.Value,
                            radDataType.ItemIndex);
    code.line += #13#10'; Screen memory address'#13#10 +
                 'CARD SCREEN=88'#13#10 +
                 '; The size of the screen'#13#10 +
                 'CARD size=[' + IntToStr(maxSize) + ']'#13#10;
    if not isMaxSize then
      code.line += 'CARD cnt=[0]'#13#10;

    if chkCharSet.Checked then
      code.line += '; Character set supporting variables'#13#10 +
                   'BYTE data'#13#10;

    if not isMaxSize or chkCharSet.Checked then
      code.line += 'CARD i'#13#10;

    if chkCharSet.Checked then
      code.line += 'BYTE RAMTOP=$6A'#13#10 +
                   'BYTE CHBAS=$2F4'#13#10 +
                   'CARD TOPMEM, CHRAM'#13#10 +
                   'BYTE ARRAY FONT(1023)'#13#10#13#10;

    code.line += '; Keyboard code of last key pressed'#13#10 +
                 'BYTE CH=$2FC'#13#10#13#10;
    code.line += 'PROC Main()'#13#10#13#10;

    if chkCharSet.Checked then
      code.line += '; RESERVE MEMORY FOR NEW CHARACTER SET'#13#10 +
                   'TOPMEM=RAMTOP-12'#13#10 +
                   'CHRAM=TOPMEM LSH 8'#13#10#13#10;

    code.line += 'Graphics(0)'#13#10#13#10;

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, false);
    code.line += WaitKeyCode(langIndex) +
                 #13#10'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    isConstData := true;

    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Display text mode 0 screen'#13#10#13#10;
    code.line += 'uses'#13#10 +
                 '  FastGraph, Crt';
    if chkCharSet.Checked then
      code.line += ', Cio';

    code.line += ';'#13#10#13#10;

    code.line += 'const'#13#10 +
                 '  size : word = ' + IntToStr(maxSize) + ';'#13#10#13#10;

    code.line += DataValues(_MAD_PASCAL, frmAntic2.fldAtascii, editMaxY.Value, editMaxX.Value,
                            radDataType.ItemIndex);

    code.line += #13#10'var'#13#10 +
                 '  screen : word absolute 88;'#13#10;
    if not isMaxSize then
      code.line += '  i : word;'#13#10 +
                   '  cnt : word = 0;'#13#10;

    if chkCharSet.Checked then
      code.line += '  topMem, chRAM : word;'#13#10 +
                   '  CHBAS  : byte absolute $2F4;'#13#10 +
                   '  RAMTOP : byte absolute $6A;'#13#10;

    code.line += #13#10'begin'#13#10;

    if chkCharSet.Checked then
      code.line += '  // Set new character set'#13#10 +
                   '  topMem := RAMTOP - 12;'#13#10 +
                   '  chRAM := topMem shl 8;'#13#10;

    code.line += '  InitGraph(0);'#13#10#13#10;

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, false);

    code.line += WaitKeyCode(langIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                 ''' Display text mode 0 screen'#13#10#13#10;
    code.line += DataValues(_FAST_BASIC, frmAntic2.fldAtascii,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
    if chkCharSet.Checked then begin
      code.line += #13#10#13#10'TOPMEM = PEEK(106) - 4'#13#10 +
                   'POKE 106, TOPMEM'#13#10 +
                   'CHRAM = TOPMEM*256';
    end;
    code.line += #13#10#13#10'GRAPHICS 0'#13#10 +
                 'screen = DPEEK(88)'#13#10;

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += 'size = ' + IntToStr(maxSize) + #13#10;
    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, false);
    code.line += WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Load text mode 0 screen
 -----------------------------------------------------------------------------}
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

    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Load text mode 0 screen') +
                 CodeLine(_REM);
    if chkCharSet.Checked then
      code.line += CodeLine('TOPMEM=PEEK(106)-8') +
                   CodeLine('POKE 106,TOPMEM') +
                   CodeLine('CHRAM=TOPMEM*256');

    code.line += CodeLine('GRAPHICS 0');
    code.line += CodeLine('CLOSE #1');
    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += CodeLine('OPEN #1,4,0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256');
    if isMaxSize then
      code.line += CodeLine('SIZE=' + IntToStr(maxSize))
    else
      code.line += CodeLine('REM SCREEN DIM. VALUES') +
                   CodeLine('GET #1,MAXX') +
                   CodeLine('GET #1,MAXY') +
                   CodeLine('SIZE=(MAXX+1)*(MAXY+1)-1');

    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, true,
                               not chkCharSet.Checked);
    code.line += CodeLine('CLOSE #1');
    code.line += WaitKeyCode(langIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Load text mode 0 screen') +
                 CodeLine('--');
    if chkCharSet.Checked then
      code.line += CodeLine('TOPMEM=PEEK(106)-8') +
                   CodeLine('POKE 106,TOPMEM') +
                   CodeLine('CHRAM=TOPMEM*256');

    code.line += CodeLine('GRAPHICS %0');
    code.line += CodeLine('CLOSE #%1');
    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += CodeLine('OPEN #%1,4,%0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=DPEEK(88)');
    if isMaxSize then
      code.line += CodeLine('SIZE=' + IntToStr(maxSize))
    else
      code.line += CodeLine('REM SCREEN DIM. VALUES') +
                   CodeLine('GET #%1,MAXX') +
                   CodeLine('GET #%1,MAXY') +
                   CodeLine('SIZE=(MAXX+%1)*(MAXY+%1)-%1');

    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, true);
    code.line += CodeLine('CLOSE #%1') +
                 WaitKeyCode(langIndex);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Load text mode 0 screen'#13#10#13#10 +
                 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt, Cio;'#13#10#13#10 +
                 'var'#13#10 +
                 '  screen : word;'#13#10;
    if not isMaxSize then
      code.line += '  cnt : byte = 0;'#13#10 +
                   '  maxX, maxY : byte;'#13#10 +
                   '  size, i : word;'#13#10 +
                   '  data : byte;'#13#10
    else
      code.line += '  size : word = ' + IntToStr(maxSize) + ';'#13#10;

    if chkCharSet.Checked then begin
      code.line += '  topMem, chRAM : word;'#13#10 +
                   '  CHBAS  : byte absolute $2F4;'#13#10 +
                   '  RAMTOP : byte absolute $6A;'#13#10;
    end;

    code.line += #13#10'begin'#13#10;

    if chkCharSet.Checked then begin
      code.line += '  // Set new character set'#13#10 +
                   '  topMem := RAMTOP - 12;'#13#10 +
                   '  chRAM := topMem shl 8;'#13#10;
    end;

    code.line += '  InitGraph(0);'#13#10 +
                 '  screen := DPeek(88);'#13#10#13#10 +
                 '  // Open file'#13#10 +
                 '  Cls(1);'#13#10;
    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += '  Opn(1, 4, 0, ' + QuotedStr(editFilename.Text) + ');'#13#10#13#10;

    if not isMaxSize then
      code.line += '  // Read screen dimension'#13#10 +
                   '  maxX := Get(1);'#13#10 +
                   '  maxY := Get(1);'#13#10 +
                   '  size := (maxX + 1)*(maxY + 1) - 1;'#13#10#13#10;

    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, true);
    code.line += '  // Close file'#13#10 +
                 '  Cls(1);'#13#10 +
                 WaitKeyCode(langIndex) +
                 'end.';
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := ';' + _REM_MAD_STUDIO + #13#10 +
                 '; Load text mode 0 screen'#13#10#13#10 +
                 'BYTE ch=$2FC'#13#10 +
                 'BYTE data'#13#10 +
                 'CARD screen'#13#10 +
                 'CARD i'#13#10;
    if not isMaxSize then
      code.line += 'BYTE maxX, maxY, cnt=[0]'#13#10 +
                   'CARD size'#13#10
    else
      code.line += 'CARD size=[' + IntToStr(maxSize) + ']'#13#10;

    if chkCharSet.Checked then
      code.line += 'BYTE RAMTOP=$6A'#13#10 +
                   'BYTE CHBAS=$2F4'#13#10 +
                   'CARD TOPMEM, CHRAM'#13#10 +
                   'BYTE ARRAY FONT(1023)'#13#10;

    code.line += #13#10'PROC Main()'#13#10#13#10;

    if chkCharSet.Checked then
      code.line += '; RESERVE MEMORY FOR NEW CHARACTER SET'#13#10 +
                   'TOPMEM=RAMTOP-12'#13#10 +
                   'CHRAM=TOPMEM LSH 8'#13#10#13#10;

    code.line += 'Put(125)'#13#10 +
                 'screen = PeekC(88)'#13#10#13#10 +
                 '; Set up channel 2 for screen'#13#10 +
                 'Close(2)'#13#10;
    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += 'Open(2,"' + editFilename.Text + '",4,0)'#13#10#13#10;

    if not isMaxSize then
      code.line += '; Read image dimension values'#13#10 +
                   'maxX = GetD(2)'#13#10 +
                   'maxY = GetD(2)'#13#10 +
                   'size = (maxX + 1)*(maxY + 1) - 3'#13#10#13#10;

    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, true);
    code.line += '; Close channel 2'#13#10 +
                 'Close(2)'#13#10 +
                 WaitKeyCode(langIndex) + #13#10 +
                 'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                 ''' Load text mode 0 screen'#13#10#13#10;
    if chkCharSet.Checked then begin
      code.line += 'TOPMEM = PEEK(106) - 4'#13#10 +
                   'POKE 106, TOPMEM'#13#10 +
                   'CHRAM = TOPMEM*256'#13#10;
    end;

    code.line += 'GRAPHICS 0'#13#10#13#10 +
                 'CLOSE #1'#13#10;
    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += 'OPEN #1, 4, 0, "' + editFilename.Text + '"'#13#10#13#10 +
                 'screen = DPEEK(88)'#13#10;
    if not isMaxSize then
      code.line += 'cnt = 0'#13#10#13#10 +
                   ''' Read image dimension values'#13#10 +
                   'GET #1, maxX'#13#10 +
                   'GET #1, maxY'#13#10 +
                   'size = (maxX + 1)*(maxY + 1) - 1'#13#10#13#10
    else
      code.line += 'size = ' + IntToStr(maxSize) + #13#10#13#10;

    //code.line += ''' Read image data'#13#10 +
    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, true);
    code.line += 'CLOSE #1'#13#10 +
                 WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

procedure TfrmAntic2Gen.ExamplesProc(Sender: TObject);
begin
  if listExamples.ListBox.ItemIndex < 0 then exit;

  btnAtariBASIC.Enabled := listings[listExamples.ListBox.ItemIndex, 0];
  btnTurboBasicXL.Enabled := listings[listExamples.ListBox.ItemIndex, 1];
  btnMadPascal.Enabled := listings[listExamples.ListBox.ItemIndex, 2];
  btnEffectus.Enabled := listings[listExamples.ListBox.ItemIndex, 3];
  btnFastBasic.Enabled := listings[listExamples.ListBox.ItemIndex, 4];
  btnKickC.Enabled := listings[listExamples.ListBox.ItemIndex, 8];

  editFilename.Visible := ListExamples.ListBox.ItemIndex = 2;
  editFilename.Enabled := ListExamples.ListBox.ItemIndex = 2;

//  boxResizeScreen.Enabled := listExamples.ListBox.ItemIndex < 2;
//  boxResizeScreen.Visible := boxResizeScreen.Enabled;

  boxCharSet.Enabled := listExamples.ListBox.ItemIndex >= 1;
  boxCharSet.Visible := boxCharSet.Enabled;

  CreateCode;
end;

procedure TfrmAntic2Gen.radLangProc(Sender: TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmAntic2Gen.CreateCodeProc(Sender : TObject);
begin
  CreateCode;
end;

procedure TfrmAntic2Gen.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmAntic2Gen.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

procedure TfrmAntic2Gen.CheckMaxSizeProc(Sender : TObject);
begin
  if chkMaxSize.Checked then begin
    editMaxX.Value := 39;
    editMaxY.Value := 23;
  end;
end;

procedure TfrmAntic2Gen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

