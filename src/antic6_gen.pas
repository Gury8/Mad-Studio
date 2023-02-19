{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Text mode 1 and 2 editor - source code generator
}
unit antic6_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, lcltype, BCListBox, BCMDButton, BCMaterialDesignButton,
  common;

type
  { TfrmAntic6Gen }
  TfrmAntic6Gen = class(TForm)
    boxCharSet : TGroupBox;
    boxColors : TGroupBox;
    btnCopyToEditor : TBCMaterialDesignButton;
    btnClose : TBCMaterialDesignButton;
    boxStartLine : TGroupBox;
    btnAtariBASIC : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnKickC : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    chkCharSet : TCheckBox;
    chkTextWindow: TCheckBox;
    chkUseColors : TCheckBox;
    color0 : TShape;
    color1 : TShape;
    color2 : TShape;
    color3 : TShape;
    color4 : TShape;
    editFilename: TLabeledEdit;
    editFontName : TLabeledEdit;
    editLineStep : TSpinEditEx;
    editStartLine : TSpinEditEx;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label7 : TLabel;
    Label8 : TLabel;
    lblLineStep : TLabel;
    lblStartLine : TLabel;
    ListExamples : TBCPaperListBox;
    memo : TMemo;
    panelLang : TBCPaperPanel;
    radDataType : TRadioGroup;
    StaticText1 : TStaticText;
    StaticText2 : TStaticText;
    StaticText3 : TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditorProc(Sender: TObject);
    procedure CreateCodeProc(Sender : TObject);
    procedure listExamplesProc(Sender: TObject);
    procedure radLangProc(Sender: TObject);
    procedure chkCharSetChange(Sender : TObject);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
  private
    { private declarations }
    listings : TListings;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
    function Example04 : string;
    function GenChrDataBASIC(grMode : string) : string;
    function GenChrDataAction(grMode : string) : string;
    function GenChrDataMadPascal(grMode : string) : string;
    function GenChrDataFastBASIC(grMode : string) : string;
    procedure CreateCode;
  end;

var
  frmAntic6Gen: TfrmAntic6Gen;

implementation

{$R *.lfm}

uses
  antic6, src_editor, code_lib, lib;

{ TfrmAntic6Gen }

procedure TfrmAntic6Gen.FormCreate(Sender: TObject);
begin
  if frmAntic6.textMode = 1 then begin
    antic_mode_max_x := _ANTIC_MODE_6_MAX_X;
    antic_mode_max_y := _ANTIC_MODE_6_MAX_Y;
  end
  else begin
    antic_mode_max_x := _ANTIC_MODE_7_MAX_X;
    antic_mode_max_y := _ANTIC_MODE_7_MAX_Y;
  end;

  SetListings(listings);

  // Example 2
  listings[1, 8] := false;

  // Example 3
  listings[2, 8] := false;

  // Example 4
  listings[3, 8] := false;
end;

procedure TfrmAntic6Gen.chkCharSetChange(Sender : TObject);
begin
  editFontName.Enabled := chkCharSet.Checked;
end;

procedure TfrmAntic6Gen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;

  color0.Brush.Color := colTab[0];   // POKE 712,C
  color1.Brush.Color := colTab[1];   // POKE 708,C
  color2.Brush.Color := colTab[2];   // POKE 709,C
  color3.Brush.Color := colTab[3];   // POKE 710,C
  color4.Brush.Color := colTab[10];  // POKE 711,C

  editFilename.Text:= 'H1:' + ExtractFileName(frmAntic6.filename);
  editFontname.Text := 'H1:' + ExtractFileName(frmAntic6.fontName);

  chkCharSet.Checked := frmAntic6.fontName <> '';

  langIndex := 0;
  ListExamples.ListBox.ItemIndex := 0;
  listExamplesProc(Sender);
end;

procedure TfrmAntic6Gen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmAntic6Gen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

procedure TfrmAntic6Gen.CreateCode;
var
  code : string;
begin
  Set01(boxStartLine, langIndex, radDataType, false);

  case ListExamples.ListBox.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
    3: code := Example04;
  end;

  Set02(memo, code);
end;

procedure TfrmAntic6Gen.CopyToEditorProc(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor(Sender);
  Close;
end;

// Generate code for Atari BASIC and Turbo BASIC XL
function TfrmAntic6Gen.GenChrDataBASIC(grMode : string) : string;
var
  j : word;
  cnt : byte = 0;
  str : string;  //array[0..23] of string;
  isNormal, isFirst : boolean;

procedure Rep(isLast : boolean);
var
  stry : string;
begin
  if isNormal then
    stry := CodeLine('? #6;' + str + '";');

  if isLast then
    SetLength(stry, Length(stry) - 1);

  code.line += stry + #13#10;
  isNormal := false;
  isFirst := true;
  str := '';
end;

begin
  isNormal := false;
  isFirst := true;
  code.number := editStartLine.Value;
  code.step := editLineStep.Value;
  code.line := CodeLine(_REM) +
               CodeLine('REM' + _REM_MAD_STUDIO) +
               CodeLine('REM Print character codes to the screen') +
               CodeLine(_REM) +
               CodeLine('GRAPHICS ' + grMode);
  for j := 0 to frmAntic6.modeSize do begin
    //if j = frmAntic6.modeSize then break;
    if (j > 19) and (j mod 20 = 0) then begin
      Rep(true);
      Inc(cnt);
      if cnt <= 23 then
        code.line += CodeLine('POS.0,' + IntToStr(cnt));
    end;
    if (frmAntic6.fldAtascii[j] >= 0) and (frmAntic6.fldAtascii[j] <= 63) then begin
      isNormal := true;
      if isFirst then begin
        str += '"';
        isFirst := false;
      end;

      if Length(str) < 112 then
        str += chr(frmAntic6.fldAtascii[j] + 32)
      else
        Rep(false);
    end
    else if (frmAntic6.fldAtascii[j] >= 97) and (frmAntic6.fldAtascii[j] <= 122) then begin
      isNormal := true;
      if isFirst then begin
        str += '"';
        isFirst := false;
      end;

      if Length(str) < 112 then
        str += Chr(frmAntic6.fldAtascii[j])
      else
        Rep(false);
    end
    else if (frmAntic6.fldAtascii[j] >= 128) and (frmAntic6.fldAtascii[j] <= 128 + 63) then begin
      if Length(str) < 112 then begin
        if isNormal then
          str += '";';

        str += 'CHR$(' + inttostr(frmAntic6.fldAtascii[j] + 32) + ');';
      end
      else
        Rep(false);

      isNormal := false;
      isFirst := true;
    end
    else if (frmAntic6.fldAtascii[j] >= 128 + 97) and
            (frmAntic6.fldAtascii[j] <= 128 + 97 + 26) then begin
      if Length(str) < 112 then begin
        if isNormal then
          str += '";';

        str += 'CHR$(' + inttostr(frmAntic6.fldAtascii[j]) + ');';
      end
      else
        Rep(false);

      isNormal := false;
      isFirst := true;
    end;
  end;

  code.line += WaitKeyCode(langIndex);

  result := code.line;
end;

// Generate code for Action!
function TfrmAntic6Gen.GenChrDataAction(grMode : string) : string;
var
  j : short;
  str : string = '';

//GRAPHICS(1)
//PUTD(6,65) PUTD(6,65)
//PRINTD(6,"TEST AbA")
// P 65)
begin
  code.line := ';' + _REM_MAD_STUDIO + #13#10 +
               '; Print character codes to the screen'#13#10#13#10 +
               'DEFINE PUT="PUTD(6,"'#13#10#13#10 +
               'BYTE CH=764'#13#10#13#10 +
               'GRAPHICS(' + grMode + ')'#13#10#13#10;
  for j := 0 to frmAntic6.modeSize do begin
    if (j > 7) and (j mod 6 = 0) then begin
      code.line += str + #13#10;
      str := '';
    end;

    if (frmAntic6.fldAtascii[j] >= 0) and (frmAntic6.fldAtascii[j] <= 63) then
      str += 'PUT ' + IntToStr(frmAntic6.fldAtascii[j] + 32) + ') '
    else if (frmAntic6.fldAtascii[j] >= 97) and (frmAntic6.fldAtascii[j] <= 122) then
      str += 'PUT ' + IntToStr(frmAntic6.fldAtascii[j]) + ') '
    else if (frmAntic6.fldAtascii[j] >= 128) and (frmAntic6.fldAtascii[j] <= 128 + 63) then
      str += 'PUT ' + IntToStr(frmAntic6.fldAtascii[j] + 32) + ') '
    else if (frmAntic6.fldAtascii[j] >= 128 + 97) and (frmAntic6.fldAtascii[j] <= 128 + 97 + 26) then
      str += 'PUT ' + IntToStr(frmAntic6.fldAtascii[j]) + ') ';
  end;
  code.line += WaitKeyCode(_ACTION) +
               #13#10'RETURN';
  result := code.line;
end;

// Generate code for Mad Pascal
function TfrmAntic6Gen.GenChrDataMadPascal(grMode : string) : string;
var
  j : short;
  str : string = '';
begin
  code.line := '//' + _REM_MAD_STUDIO + #13#10 +
               '// Print character codes to the screen'#13#10#13#10 +
               'uses crt, fastgraph, cio;'#13#10#13#10 +
               'var'#13#10 +
               '  textBuf : string;'#13#10 +
               #13#10'begin'#13#10 +
               '  InitGraph(' + grMode + ');'#13#10#13#10;
  str := '  textBuf := ';
  for j := 0 to frmAntic6.modeSize do begin
    if (j > 19) and (j mod 20 = 0) then begin
      //str += ';'#13#10'  BlockWrite(f, textBuf[1], Length(textBuf));';
      str += ';'#13#10'  BPut(6, @textBuf[1], Length(textBuf));';
      code.line += str + #13#10;
      str := '  textBuf := ';
    end;

    if (frmAntic6.fldAtascii[j] >= 0) and (frmAntic6.fldAtascii[j] <= 63) then
      str += '#' + IntToStr(frmAntic6.fldAtascii[j] + 32)
    else if (frmAntic6.fldAtascii[j] >= 97) and (frmAntic6.fldAtascii[j] <= 122) then
      str += '#' + IntToStr(frmAntic6.fldAtascii[j])
    else if (frmAntic6.fldAtascii[j] >= 128) and (frmAntic6.fldAtascii[j] <= 128 + 63) then
      str += '#' + IntToStr(frmAntic6.fldAtascii[j] + 32)
    else if (frmAntic6.fldAtascii[j] >= 128 + 97) and (frmAntic6.fldAtascii[j] <= 128 + 97 + 26) then
      str += '#' + IntToStr(frmAntic6.fldAtascii[j]);
  end;
  code.line += WaitKeyCode(_MAD_PASCAL) +
               'end.';

  result := code.line;
end;

// Generate code for FastBasic
function TfrmAntic6Gen.GenChrDataFastBASIC(grMode : string) : string;
var
  j : short;
  cnt : byte = 0;
  str : string;
  isNormal, isFirst : boolean;

procedure Rep(isLast : boolean);
begin
  str := '? #6,' + str;
  if isNormal then
    str += '";';

  if isLast then
    SetLength(str, Length(str) - 1);

  code.line += str + #13#10;
  isNormal := false;
  isFirst := true;
  str := '';
end;

begin
  isNormal := false;
  isFirst := true;
  code.line := '''' + _REM_MAD_STUDIO + #13#10 +
               ''' Print character codes to the screen'#13#10#13#10 +
               'GRAPHICS ' + grMode +
               #13#10'POS. 0, 0'#13#10;
  for j := 0 to frmAntic6.modeSize do begin
    if (j > 19) and (j mod 20 = 0) then begin
      Rep(true);
      Inc(cnt);
      if cnt <= 23 then
        code.line += 'POS. 0, ' + IntToStr(cnt) + #13#10;
    end;
    if (frmAntic6.fldAtascii[j] >= 0) and (frmAntic6.fldAtascii[j] <= 63) then begin
      isNormal := true;
      if isFirst then begin
        str += '"';
        isFirst := false;
      end;
      if Length(str) < 112 then
        str += chr(frmAntic6.fldAtascii[j] + 32)
      else
        Rep(false);
    end
    else if (frmAntic6.fldAtascii[j] >= 97) and (frmAntic6.fldAtascii[j] <= 122) then begin
      isNormal := true;
      if isFirst then begin
        str += '"';
        isFirst := false;
      end;
      if Length(str) < 112 then
        str += chr(frmAntic6.fldAtascii[j])
      else
        Rep(false);
    end
    else if (frmAntic6.fldAtascii[j] >= 128) and (frmAntic6.fldAtascii[j] <= 128 + 63) then begin
      if Length(str) < 112 then begin
        if isNormal then
          str += '";';

        str += 'CHR$(' + inttostr(frmAntic6.fldAtascii[j] + 32) + ');';
      end
      else
        Rep(false);

      isNormal := false;
      isFirst := true;
    end
    else if (frmAntic6.fldAtascii[j] >= 128 + 97) and
            (frmAntic6.fldAtascii[j] <= 128 + 97 + 26) then begin
      if Length(str) < 112 then begin
        if isNormal then
          str += '";';

        str += 'CHR$(' + inttostr(frmAntic6.fldAtascii[j]) + ');';
      end
      else
        Rep(false);

      isNormal := false;
      isFirst := true;
    end;
  end;

  result := code.line + WaitKeyCode(_FAST_BASIC);
end;

{-----------------------------------------------------------------------------
 Data values
 -----------------------------------------------------------------------------}
function TfrmAntic6Gen.Example01 : string;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex = _ATARI_BASIC) or (langIndex = _TURBO_BASIC_XL) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := DataValues(_ATARI_BASIC, frmAntic6.fldAtascii, code.number, code.step,
                            frmAntic6.modeHeight, 20, radDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then
    code.line := DataValues(_ACTION, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, radDataType.ItemIndex)
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then
//    frmAntic6.modeSize
    code.line := DataValues(_MAD_PASCAL, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, radDataType.ItemIndex)
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then
    code.line := DataValues(_FAST_BASIC, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, radDataType.ItemIndex)
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then
    code.line := DataValues(_KICKC, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, radDataType.ItemIndex);

  //{----------------------------------------------------------------------------
  // Mad Assembler (MADS)
  // ---------------------------------------------------------------------------}
  //else if radLang.ItemIndex = _MADS then
  //  code := GrDataValues(_MADS, lineNum, 240, rgDataType.ItemIndex, false);
  //{----------------------------------------------------------------------------
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
  //{----------------------------------------------------------------------------
  // CC65
  // ---------------------------------------------------------------------------}
  //else if radLang.ItemIndex = _CC65 then
  //  code := GrDataValues(_CC65, lineNum, 240, rgDataType.ItemIndex, false);

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Display text mode 1/2 screen
 -----------------------------------------------------------------------------}
function TfrmAntic6Gen.Example02 : string;
var
  maxSize : word;
  strTextWindow, grMode : string;
begin
  grMode := IntToStr(frmAntic6.textMode);
  maxSize := frmAntic6.modeSize - 1;

  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;

    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Display text mode ' + grMode + ' screen') +
                 CodeLine(_REM);

    if chkCharSet.Checked then begin
      code.line += CodeLine('TOPMEM=PEEK(106)-8');
      code.line += CodeLine('POKE 106,TOPMEM');
    end;

    code.line += CodeLine('GRAPHICS ' + grMode + strTextWindow) +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256');

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += CodeLine('SIZE=' + IntToStr(maxSize));
    code.line += DisplayScreen(langIndex, antic_mode_max_x, antic_mode_max_y, false);

    if chkUseColors.Checked then
      code.line += GenSetColors(_ATARI_BASIC);
      //code.line += CodeLine('POKE 712,' + IntToStr(colorValues[0]) + ':' +
      //                      'POKE 708,' + IntToStr(colorValues[1]) + ':' +
      //                      'POKE 709,' + IntToStr(colorValues[2]) + ':' +
      //                      'POKE 710,' + IntToStr(colorValues[3]) + ':' +
      //                      'POKE 711,' + IntToStr(colorValues[10]));

    code.line += WaitKeyCode(langIndex);

    // Screen data
    code.line += DataValues(_ATARI_BASIC, frmAntic6.fldAtascii, code.number, code.step,
                            frmAntic6.modeHeight, 20, radDataType.ItemIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;

    code.line := CodeLine('--') +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Display text mode ' + grMode + ' screen') +
                 CodeLine('--');

    if chkCharSet.Checked then begin
      code.line += CodeLine('TOPMEM=PEEK(106)-8');
      code.line += CodeLine('POKE 106,TOPMEM');
      code.line += CodeLine('CHRAM=TOPMEM*256');
    end;

    code.line += CodeLine('GRAPHICS ' + grMode + strTextWindow) +
                 CodeLine('SCR=DPEEK(88)');

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += CodeLine('SIZE=' + IntToStr(maxSize));
    code.line += DisplayScreen(langIndex, antic_mode_max_x, antic_mode_max_y, false);

    if chkUseColors.Checked then
      code.line += GenSetColors(_TURBO_BASIC_XL);

    code.line += WaitKeyCode(langIndex);

    // Screen data
    code.line += DataValues(_TURBO_BASIC_XL, frmAntic6.fldAtascii, code.number, code.step,
                            frmAntic6.modeHeight, 20, radDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := ';' + _REM_MAD_STUDIO + #13#10 +
                 '; Display text mode ' + grMode + ' screen'#13#10#13#10;
    code.line += DataValues(_ACTION, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, radDataType.ItemIndex);
    code.line += #13#10'; Screen memory address'#13#10 +
                 'CARD SCREEN = 88'#13#10#13#10 +
                 '; Keyboard code of last key pressed'#13#10 +
                 'BYTE CH = $2FC'#13#10 +
                 '; The size of the screen'#13#10 +
                 'CARD size=[' + IntToStr(maxSize) + ']'#13#10#13#10;

    if chkCharSet.Checked then
      code.line += 'BYTE RAMTOP=$6A'#13#10 +
                   'BYTE CHBAS=$2F4'#13#10 +
                   'CARD TOPMEM, CHRAM'#13#10 +
                   'BYTE ARRAY FONT(1023)'#13#10 +
                   'BYTE DATA'#13#10 +
                   'CARD i'#13#10#13#10;

    code.line += 'PROC Main()'#13#10#13#10;

    if chkCharSet.Checked then
      code.line += '; RESERVE MEMORY FOR NEW CHARACTER SET'#13#10 +
                   'TOPMEM=RAMTOP-12'#13#10 +
                   'CHRAM=TOPMEM LSH 8'#13#10#13#10;

    code.line += 'Graphics(' + grMode + strTextWindow + ')'#13#10#13#10;
//                 'MoveBlock(SCREEN, screenData, ' + IntToStr(frmAntic6.modeSize) + ')'#13#10;

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += DisplayScreen(langIndex, antic_mode_max_x, antic_mode_max_y, false);

    if chkUseColors.Checked then
      code.line += GenSetColors(_ACTION);

    code.line += WaitKeyCode(langIndex) +
                 #13#10'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Display text mode ' + grMode + ' screen'#13#10#13#10;
    code.line += 'uses'#13#10 +
                 '  FastGraph, Crt';
    if chkCharSet.Checked then
      code.line += ', Cio';

    code.line += ';'#13#10#13#10;

    code.line += 'var'#13#10 +
                 '  screen : word absolute 88;'#13#10 +
                 '  // The size of the screen'#13#10 +
                 '  size : word = ' + IntToStr(maxSize) + ';'#13#10#13#10;

    if chkCharSet.Checked then
      code.line += '  topMem, chRAM : word;'#13#10 +
                   '  CHBAS  : byte absolute $2F4;'#13#10 +
                   '  RAMTOP : byte absolute $6A;'#13#10;

    code.line += DataValues(_MAD_PASCAL, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, radDataType.ItemIndex) + #13#10 +
                 'begin'#13#10;

    if chkCharSet.Checked then
      code.line += '  // Set new character set'#13#10 +
                   '  topMem := RAMTOP - 12;'#13#10 +
                   '  chRAM := topMem shl 8;'#13#10;

    code.line += '  InitGraph(' + grMode + strTextWindow +');'#13#10;
//                 '  Move(screenData, pointer(screen), ' + IntToStr(frmAntic6.modeSize) + ');'#13#10;

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += DisplayScreen(langIndex, antic_mode_max_x, antic_mode_max_y, false);

    if chkUseColors.Checked then
      code.line += GenSetColors(_MAD_PASCAL);

    code.line += WaitKeyCode(langIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                 ''' Display text mode ' + grMode + ' screen'#13#10#13#10;

    code.line += DataValues(_FAST_BASIC, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, radDataType.ItemIndex);

    code.line += #13#10#13#10;

    if chkCharSet.Checked then
      code.line += 'TOPMEM = PEEK(106) - 4'#13#10 +
                   'POKE 106, TOPMEM'#13#10 +
                   'CHRAM = TOPMEM*256'#13#10;

    code.line += 'GRAPHICS ' + grMode + strTextWindow + #13#10 +
                 'screen = DPEEK(88)'#13#10;
//                 'MOVE ADR(screenData), scr, ' + IntToStr(frmAntic6.modeSize) + #13#10;

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += ''' The size of the screen'#13#10 +
                 'size = ' + IntToStr(maxSize) + #13#10#13#10 +
                 DisplayScreen(langIndex, antic_mode_max_x, antic_mode_max_y, false);

    if chkUseColors.Checked then
      code.line += GenSetColors(_FAST_BASIC);

    code.line += WaitKeyCode(langIndex);
  end
  { KickC
   ---------------------------------------------------------------------------}
  else  // if langIndex = _KICKC then
    code.line := '';

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Load text mode 1/2 screen
 -----------------------------------------------------------------------------}
function TfrmAntic6Gen.Example03 : string;
var
  strTextWindow, grMode : string;
begin
  grMode := IntToStr(frmAntic6.textMode);

  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Load text mode ' + grMode + ' screen') +
                 CodeLine(_REM);

    if chkCharSet.Checked then begin
      code.line += CodeLine('TOPMEM=PEEK(106)-8');
      code.line += CodeLine('POKE 106,TOPMEM');
    end;

    code.line += CodeLine('GRAPHICS ' + grMode + strTextWindow);

    code.line += CodeLine('CLOSE #1');
    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += CodeLine('OPEN #1,4,0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                 CodeLine('SIZE=' + IntToStr(frmAntic6.modeSize - 1));
    code.line += DisplayScreen(langIndex, antic_mode_max_x, antic_mode_max_y, true,
                               not chkCharSet.Checked);
    code.line += CodeLine('CLOSE #1');

    if chkUseColors.Checked then
      code.line += GenSetColors(langIndex);

    code.line += WaitKeyCode(langIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Load text mode ' + grMode + ' screen') +
                 CodeLine('--');

    if chkCharSet.Checked then begin
      code.line += CodeLine('TOPMEM=PEEK(106)-8');
      code.line += CodeLine('POKE 106,TOPMEM');
      code.line += CodeLine('CHRAM=TOPMEM*256');
    end;

    code.line += CodeLine('GRAPHICS ' + grMode + strTextWindow);
    code.line += CodeLine('CLOSE #%1');

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += CodeLine('OPEN #%1,4,%0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('SIZE=' + IntToStr(frmAntic6.modeSize - 1));
    code.line += DisplayScreen(langIndex, antic_mode_max_x, antic_mode_max_y, true);
    code.line += CodeLine('CLOSE #%1');

    if chkUseColors.Checked then
      code.line += GenSetColors(langIndex);

    code.line += WaitKeyCode(langIndex);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Load text mode ' + grMode + ' screen'#13#10#13#10 +
                 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt, Cio;'#13#10#13#10 +
                 'var'#13#10 +
                 '  screen : word;'#13#10 +
//                 '  cnt : byte = 0;'#13#10 +
//                 '  maxX, maxY : byte;'#13#10 +
                 '  size : word = ' + IntToStr(frmAntic6.modeSize) + ';'#13#10 +
                 '  colReg : array[0..4] of byte absolute 708;'#13#10;
    if chkCharSet.Checked then
      code.line += '  topMem, chRAM : word;'#13#10 +
                   '  CHBAS  : byte absolute $2F4;'#13#10 +
                   '  RAMTOP : byte absolute $6A;'#13#10;

//    code.line += '  i, data : byte;'#13#10#13#10 +
    code.line += #13#10'begin'#13#10;

    if chkCharSet.Checked then
      code.line += '  // Set new character set'#13#10 +
                   '  topMem := RAMTOP - 12;'#13#10 +
                   '  chRAM := topMem shl 8;'#13#10;

    code.line += '  InitGraph(' + grMode + strTextWindow + ');'#13#10 +
                 '  screen := DPeek(88);'#13#10 +
//                 '  size := ' + IntToStr(frmAntic6.modeSize) + ';'#13#10#13#10 +
                 '  // Open file'#13#10 +
                 '  Cls(1);'#13#10;

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += '  Opn(1, 4, 0, ' + QuotedStr(editFilename.Text) + ');'#13#10#13#10;
    code.line += DisplayScreen(langIndex, antic_mode_max_x, antic_mode_max_y, true);
    code.line += '  // Close file'#13#10 +
                 '  Cls(1);'#13#10;

    if chkUseColors.Checked then
      code.line += GenSetColors(langIndex);

    code.line += WaitKeyCode(langIndex) +
                 'end.';
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := ';' + _REM_MAD_STUDIO + #13#10 +
                 '; Load text mode ' + grMode + ' screen'#13#10 +
                 #13#10'BYTE ch=$2FC'#13#10 +
                 'BYTE data, cnt=[0]'#13#10 +
                 'CARD size=[' + IntToStr(frmAntic6.modeSize - 1) + ']'#13#10 +
                 'CARD screen'#13#10 +
                 'BYTE maxX, maxY'#13#10 +
                 'BYTE ARRAY colReg(708)'#13#10 +
                 'CARD i'#13#10#13#10;
    if chkCharSet.Checked then
      code.line += 'BYTE RAMTOP=$6A'#13#10 +
                   'BYTE CHBAS=$2F4'#13#10 +
                   'CARD TOPMEM, CHRAM'#13#10 +
                   'BYTE ARRAY FONT(1023)'#13#10#13#10;

    code.line += 'PROC Main()'#13#10#13#10;

    if chkCharSet.Checked then
      code.line += '; RESERVE MEMORY FOR NEW CHARACTER SET'#13#10 +
                   'TOPMEM=RAMTOP-12'#13#10 +
                   'CHRAM=TOPMEM LSH 8'#13#10#13#10;

    code.line += 'Graphics(' + grMode + strTextWindow + ')'#13#10 +
                 'screen=PeekC(88)'#13#10#13#10 +
                 '; Set up channel 2 for screen'#13#10 +
                 'Close(2)'#13#10;

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += 'Open(2,"' + editFilename.Text + '",4,0)'#13#10#13#10 +
//                 'size=' + IntToStr(frmAntic6.modeSize - 1) + #13#10#13#10 +
                 '; Screen data'#13#10;
    code.line += DisplayScreen(langIndex, antic_mode_max_x, antic_mode_max_y, true);
    code.line += '; Close channel 2'#13#10 +
                 'Close(2)'#13#10;

    if chkUseColors.Checked then
      code.line += GenSetColors(langIndex);

    code.line += WaitKeyCode(langIndex) + #13#10 +
                 'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
   else if langIndex = _FAST_BASIC then begin
     code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                  ''' Load text mode ' + grMode + ' screen'#13#10#13#10;

     if chkCharSet.Checked then
       code.line += 'TOPMEM = PEEK(106) - 4'#13#10 +
                    'POKE 106, TOPMEM'#13#10 +
                    'CHRAM = TOPMEM*256'#13#10#13#10;

     code.line += 'GRAPHICS ' + grMode + strTextWindow + #13#10#13#10 +
                  'CLOSE #1'#13#10;

     if chkCharSet.Checked then
       code.line += SetCharSet(langIndex, editFontname.Text, true);

     code.line += 'OPEN #1, 4, 0, "' + editFilename.Text + '"'#13#10 +
                  'screen = DPEEK(88)'#13#10 +
                  'size = ' + IntToStr(frmAntic6.modeSize) + #13#10#13#10;
     code.line += DisplayScreen(langIndex, antic_mode_max_x, antic_mode_max_y, true);
     code.line += 'CLOSE #1'#13#10;

     if chkUseColors.Checked then
       code.line += GenSetColors(langIndex);

     code.line += WaitKeyCode(langIndex);
  end
  else
    code.line := '';

  result := code.line;
end;

function TfrmAntic6Gen.Example04 : string;
var
  strTextWindow, grMode : string;
begin
  grMode := IntToStr(frmAntic6.textMode);

  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then
    code.line := GenChrDataBASIC(grMode + strTextWindow)
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then
    code.line := GenChrDataBASIC(grMode + strTextWindow)
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then
    code.line := GenChrDataMadPascal(grMode + strTextWindow)
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then
    code.line := GenChrDataAction(grMode + strTextWindow)
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then
    code.line := GenChrDataFastBASIC(grMode + strTextWindow)
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
//    code.line := GenChrDataKickC(grMode + strTextWindow);
    code.line := '';
  end;

  result := code.line;
end;

procedure TfrmAntic6Gen.listExamplesProc(Sender: TObject);
begin
  if listExamples.ListBox.ItemIndex < 0 then exit;

  btnAtariBASIC.Enabled := listings[listExamples.ListBox.ItemIndex, 0];
  btnTurboBasicXL.Enabled := listings[listExamples.ListBox.ItemIndex, 1];
  btnMadPascal.Enabled := listings[listExamples.ListBox.ItemIndex, 2];
  btnEffectus.Enabled := listings[listExamples.ListBox.ItemIndex, 3];
  btnFastBasic.Enabled := listings[listExamples.ListBox.ItemIndex, 4];
  btnKickC.Enabled := listings[listExamples.ListBox.ItemIndex, 8];

  boxColors.Enabled := listExamples.ListBox.ItemIndex > 0;
  boxColors.Visible := boxColors.Enabled;

  editFilename.Visible := ListExamples.ListBox.ItemIndex = 2;
  editFilename.Enabled := editFilename.Visible;

  boxCharSet.Enabled := boxColors.Enabled;
  boxCharSet.Visible := boxColors.Enabled;

  CreateCode;
end;

procedure TfrmAntic6Gen.radLangProc(Sender: TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmAntic6Gen.CreateCodeProc(Sender : TObject);
begin
  CreateCode;
end;

procedure TfrmAntic6Gen.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmAntic6Gen.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

end.

