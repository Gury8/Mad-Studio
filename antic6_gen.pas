{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Text mode 1 and 2 editor - source code generator
}
unit antic6_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, lcltype, Spin, Windows,
  common;

type
  { TfrmAntic6Gen }
  TfrmAntic6Gen = class(TForm)
    btnExit: TButton;
    btnCopyToEditor: TButton;
    chkDefaultColors: TCheckBox;
    chkTextWindow: TCheckBox;
    editFilename: TLabeledEdit;
    editLineStep : TSpinEdit;
    editStartLine : TSpinEdit;
    boxStartLine : TGroupBox;
    boxColors: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    color0: TShape;
    color1: TShape;
    color2: TShape;
    color3: TShape;
    color4: TShape;
    Label8: TLabel;
    lblLineStep : TLabel;
    lblStartLine : TLabel;
    listExamples: TListBox;
    memo: TMemo;
    radLang: TRadioGroup;
    rgDataType: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditorProc(Sender: TObject);
    procedure listExamplesProc(Sender: TObject);
    procedure radLangProc(Sender: TObject);
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
  public
    { public declarations }
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

  langIndex := 0;
end;

procedure TfrmAntic6Gen.FormShow(Sender: TObject);
begin
  //FormStyle := fsSystemStayOnTop;
  //tabs.TabIndex := langIndex;

  //color0.Brush.Color := colTab[0];   // POKE 712,C
  //color1.Brush.Color := colTab[2];   // POKE 708,C
  //color2.Brush.Color := colTab[3];   // POKE 709,C
  //color3.Brush.Color := colTab[1];   // POKE 710,C
  //color4.Brush.Color := colTab[10];  // POKE 711,C
  //
  //editFilename.Text:= 'H1:' + ExtractFileName(frmMode12.filename);
  //
  //CreateCode;

  FormStyle := fsSystemStayOnTop;
  editFilename.Text:= 'H1:' + ExtractFileName(frmAntic6.filename);
//  listExamplesProc(Sender);

  color0.Brush.Color := colTab[0];   // POKE 712,C
  color1.Brush.Color := colTab[2];   // POKE 708,C
  color2.Brush.Color := colTab[3];   // POKE 709,C
  color3.Brush.Color := colTab[1];   // POKE 710,C
  color4.Brush.Color := colTab[10];  // POKE 711,C
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

procedure TfrmAntic6Gen.CopyToEditorProc(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor03(Sender);
  Close;

//  frmSrcEdit.MemoToEditor(
//    Sender, tabs, memoAtariBASIC, memoTurboBASICXL, memoAction, memoMadPascal);
  //frmSrcEdit.Show;
  //langIndex := tabs.TabIndex;
  //case langIndex of
  //  0: frmSrcEdit.editor.Lines := memoAtariBASIC.Lines;
  //  1: frmSrcEdit.editor.Lines := memoTurboBASICXL.Lines;
  //  2: frmSrcEdit.editor.Lines := memoMadPascal.Lines;
  //  3: frmSrcEdit.editor.Lines := memoAction.Lines;
  //  4: frmSrcEdit.editor.Lines := memoFastBasic.Lines;
  //end;
  //frmSrcEdit.MemoToEditor02(Sender);
  //Close;
end;

//procedure TfrmAntic6Gen.editStartLineChange(Sender : TObject);
//begin
//  CreateCode;
//end;

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
//  Inc(lineNum, 5);
  stry := CodeLine('? #6;' + str);
  if isNormal then
    stry += '";';

  if isLast then
    SetLength(stry, Length(stry) - 1);

  code.line += stry + #13#10;
  isNormal := false;
  isFirst := true;
  str := '';
end;

begin
//  editLineNum.Value := 10;
 //  lineNum := editStartLine.Value;
  code.number := editStartLine.Value;
  code.step := editLineStep.Value;
  code.line := CodeLine('REM Print character codes to the screen');
  code.line += CodeLine('GRAPHICS ' + grMode);
//  Inc(lineNum, 10);
  isNormal := false;
  isFirst := true;
  for j := 0 to frmAntic6.modeSize do begin
    //if j = frmAntic6.modeSize then break;
    if (j > 19) and (j mod 20 = 0) then begin
      Rep(true);
      Inc(cnt);
//      Inc(lineNum, 5);
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
          str += '";CHR$(' + inttostr(frmAntic6.fldAtascii[j] + 32) + ');'
        else
          str += 'CHR$(' + inttostr(frmAntic6.fldAtascii[j] + 32) + ');';
      end
      else
        Rep(false);

      isNormal := false;
      isFirst := true;
    end
    else if (frmAntic6.fldAtascii[j] >= 128 + 97) and (frmAntic6.fldAtascii[j] <= 128 + 97 + 26) then begin
      if Length(str) < 112 then begin
        if isNormal then
          str += '";CHR$(' + inttostr(frmAntic6.fldAtascii[j]) + ');'
        else
          str += 'CHR$(' + inttostr(frmAntic6.fldAtascii[j]) + ');';
      end
      else
        Rep(false);

      isNormal := false;
      isFirst := true;
    end;
  end;

//  Inc(lineNum, 10);
//  code += IntToStr(lineNum) + ' GOTO ' + IntToStr(lineNum);
  code.line += WaitKeyCode(radLang.ItemIndex);

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
  code.line := '; Print character codes to the screen'#13#10#13#10 +
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
  code.line := '// Print character codes to the screen'#13#10 +
               'uses crt, graph;'#13#10 +
               #13#10'var'#13#10 +
               '  f : file;'#13#10 +
               '  textBuf : string;'#13#10 +
               #13#10'begin'#13#10 +
               '  Assign(f, ''S:''); Rewrite(f, 1);'#13#10 +
               '  InitGraph(' + grMode + ');'#13#10 +
               #13#10;
  str := '  textBuf := ';
  for j := 0 to frmAntic6.modeSize do begin
    if (j > 19) and (j mod 20 = 0) then begin
      str += ';'#13#10 +
             '  BlockWrite(f, textBuf[1], Length(textBuf));';
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
  code.line := ''' Print character codes to the screen'#13#10 +
               #13#10 +
               'GRAPHICS ' + grMode +
               #13#10 +
               'POS. 0, 0'#13#10;
  for j := 0 to frmAntic6.modeSize do begin
    if (j > 19) and (j mod 20 = 0) then begin
      Rep(true);
      Inc(cnt);
      if cnt <= 23 then
        code.line += 'POS. 0, ' + IntToStr(cnt) + #13#10;
//      memoFastBASIC.Lines.Add('POS. 0, ' + IntToStr(cnt));
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
          str += '";CHR$(' + inttostr(frmAntic6.fldAtascii[j] + 32) + ');'
        else
          str += 'CHR$(' + inttostr(frmAntic6.fldAtascii[j] + 32) + ');';
      end
      else
        Rep(false);

      isNormal := false;
      isFirst := true;
    end
    else if (frmAntic6.fldAtascii[j] >= 128 + 97) and (frmAntic6.fldAtascii[j] <= 128 + 97 + 26) then begin
      if Length(str) < 112 then begin
        if isNormal then
          str += '";CHR$(' + inttostr(frmAntic6.fldAtascii[j]) + ');'
        else
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

procedure TfrmAntic6Gen.CreateCode;
var
  code : string;
begin
  langIndex := radLang.ItemIndex;

  if langIndex < 2 then begin
    rgDataType.ItemIndex := 0;
    TRadioButton(rgDataType.Controls[1]).Enabled := false;
  end
  else
    TRadioButton(rgDataType.Controls[1]).Enabled := true;

  case listExamples.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
    3: code := Example04;
  end;

  memo.Lines.Clear;
  memo.Lines.Add(code);

  // Set cursor position at the top of memo object
  memo.SelStart := 0;
  memo.SelLength := 0;
  SendMessage(memo.Handle, EM_SCROLLCARET, 0, 0);
end;

function TfrmAntic6Gen.Example01 : string;
var
  strTextWindow, grMode : string;
begin
  grMode := IntToStr(frmAntic6.textMode);

  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if radLang.ItemIndex = _ATARI_BASIC then
    code.line := GenChrDataBASIC(grMode + strTextWindow)
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _TURBO_BASIC_XL then
    code.line := GenChrDataBASIC(grMode + strTextWindow)
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then
    code.line := GenChrDataMadPascal(grMode + strTextWindow)
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then
    code.line := GenChrDataAction(grMode + strTextWindow)
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then
    code.line := GenChrDataFastBASIC(grMode + strTextWindow);

  result := code.line;
end;

function TfrmAntic6Gen.Example02 : string;
var
  strTextWindow, grMode : string;
begin
  grMode := IntToStr(frmAntic6.textMode);

  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if radLang.ItemIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM *******************************') +
                 CodeLine('REM LOAD TEXT MODE ' + grMode + ' SCREEN') +
                 CodeLine('REM *******************************') +
                 CodeLine('SIZE=' + IntToStr(frmAntic6.modeSize)) +
                 CodeLine('GRAPHICS ' + grMode + strTextWindow) +
                 CodeLine('CLOSE #1') +
                 CodeLine('OPEN #1,4,0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                 CodeLine('FOR I=0 TO SIZE-1') +
                 CodeLine('GET #1,BYTE') +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine('NEXT I') +
                 CodeLine('CLOSE #1');

    if not chkDefaultColors.Checked then
      code.line += CodeLine('POKE 708,' + IntToStr(colorValues[1]) + ':POKE 709,' + IntToStr(colorValues[2])) +
                   CodeLine('POKE 710,' + IntToStr(colorValues[3]) + ':POKE 711,' + IntToStr(colorValues[10])) +
                   CodeLine('POKE 712,' + IntToStr(colorValues[0]));

//    lineNum := 110;
    code.line += WaitKeyCode(_ATARI_BASIC);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM LOAD TEXT MODE ' + grMode + ' SCREEN') +
                 CodeLine('--') +
                 CodeLine('SIZE=' + IntToStr(frmAntic6.modeSize)) +
                 CodeLine('GRAPHICS %' + grMode + strTextWindow) +
                 CodeLine('CLOSE #%1') +
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('OPEN #%1,4,%0,"' + editFilename.Text + '"') +
                 CodeLine('BGET #%1,DPEEK(SCR),SIZE') +
                 //'20 OPEN #%1,4,%0,"' + editFilename.Text + '"'#13#10 +
                 //'40 FOR I=%0 TO SIZE-%1'#13#10 +
                 //'50 GET #%1,BYTE'#13#10 +
                 //'60 POKE SCR+I,BYTE'#13#10 +
                 //'70 NEXT I'#13#10 +
                 CodeLine('60 CLOSE #%1');
    if not chkDefaultColors.Checked then begin
      code.line += CodeLine('POKE 708,' + IntToStr(colorValues[1]) + ':POKE 709,' + IntToStr(colorValues[2])) +
                   CodeLine('POKE 710,' + IntToStr(colorValues[3]) + ':POKE 711,' + IntToStr(colorValues[10])) +
                   CodeLine('POKE 712,' + IntToStr(colorValues[0]));
    end;
//    lineNum := 90;
    code.line += WaitKeyCode(_TURBO_BASIC_XL);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    code.line := '// Load text mode ' + grMode + ' screen'#13#10#13#10 +
                'uses'#13#10 +
                '  SysUtils, FastGraph, Crt;'#13#10#13#10 +
                'const'#13#10 +
                '  fileSize = ' + IntToStr(frmAntic6.modeSize) + ';'#13#10#13#10 +
                'var'#13#10 +
                '  f : file;'#13#10#13#10 +
                '  filename : TString;'#13#10 +
                '  buf : pointer;'#13#10#13#10 +
                'begin'#13#10 +
                '  InitGraph(' + grMode + strTextWindow + ');'#13#10 +
                '  buf := pointer(DPeek(88));'#13#10#13#10 +
                '  // Open file'#13#10 +
                '  filename := ' + QuotedStr(editFilename.Text) + ';'#13#10 +
                '  Assign(f, filename);'#13#10#13#10 +
                '  // Read data'#13#10 +
                '  Reset(f, 1);'#13#10 +
                '  BlockRead(f, buf, fileSize);'#13#10#13#10 +
                '  // Close file'#13#10 +
                '  Close(f);'#13#10;

    if not chkDefaultColors.Checked then begin
      code.line += #13#10 +
                  'Poke(708,' + IntToStr(colorValues[1]) + '); Poke(709,' + IntToStr(colorValues[2]) + ');'#13#10 +
                  'Poke(710,' + IntToStr(colorValues[3]) + '); Poke(711,' + IntToStr(colorValues[10]) + ');'#13#10 +
                  'Poke(712,' + IntToStr(colorValues[0]) + ');'#13#10;
    end;
    code.line += WaitKeyCode(_MAD_PASCAL) +
                 'end.';
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    code.line := '; Load text mode ' + grMode + ' screen'#13#10#13#10 +
                'BYTE ch=$2FC'#13#10 +
                'BYTE data'#13#10 +
                'CARD size=[' + IntToStr(frmAntic6.modeSize) + ']'#13#10 +
                'CARD scr'#13#10 +
                'CARD n'#13#10#13#10 +
                'PROC Main()'#13#10#13#10 +
                'Graphics(' + grMode + strTextWindow + ')'#13#10 +
                'scr=PeekC(88)'#13#10#13#10 +
                '; Set up channel 2 for screen'#13#10 +
                'Close(2)'#13#10 +
                'Open(2,"' + editFilename.Text + '",4,0)'#13#10#13#10 +
                '; Read font data'#13#10 +
                'FOR n=0 TO size-1'#13#10 +
                'DO'#13#10 +
                '  data=GetD(2)'#13#10 +
                '  PokeC(scr+n,data)'#13#10 +
                'OD'#13#10 + #13#10 +
                '; Close channel 2'#13#10 +
                'Close(2)';

     if not chkDefaultColors.Checked then
       code.line += #13#10 +
                   'POKE(708,' + IntToStr(colorValues[1]) + ') POKE(709,' +
                   IntToStr(colorValues[2]) + ')'#13#10 +
                   'POKE(710,' + IntToStr(colorValues[3]) + ') POKE(711,' +
                   IntToStr(colorValues[10]) + ')'#13#10 +
                   'POKE(712,' + IntToStr(colorValues[0]) + ')'#13#10;
     //code += #13#10 +
     //       '; Press any key to exit'#13#10 +
     //       'ch=255'#13#10 +
     //       'DO UNTIL ch<255 OD'#13#10 +
     //       'ch=255'#13#10 +
     //       #13#10 +
     //       'RETURN';

     code.line += #13#10 +
                  WaitKeyCode(_ACTION) +
                  #13#10 +
                  'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    code.line := ''' Load text mode ' + grMode + ' screen'#13#10#13#10 +
                'size = ' + IntToStr(frmAntic6.modeSize) + #13#10#13#10 +
      //          #13#10 +
                'Graphics ' + grMode + strTextWindow + #13#10#13#10 +
                'CLOSE #1'#13#10 +
                'OPEN #1, 4, 0, "' + editFilename.Text + '"'#13#10#13#10 +
      //          '''BGET #1, DPEEK(88), 3839'#13#10 +
                'scr = DPeek(88)'#13#10#13#10 +
                'FOR i = 0 TO size - 1'#13#10 +
                '  GET #1, BYTE'#13#10;

                //if (frmMode12.maxX < 40) or (frmMode12.maxY < 24) then begin
                //  code += 'IF cnt = ' + IntToStr(frmMode12.maxX) + #13#10 +
                //          '  scr = scr + 40 - cnt : cnt = 0'#13#10 +
                //          'ENDIF'#13#10;
                //end;

     code.line += '  POKE scr + i, BYTE'#13#10 +
                  'NEXT'#13#10 +
                  #13#10 +
                  'CLOSE #1'#13#10;
     if not chkDefaultColors.Checked then
       code.line += #13#10 +
                    'POKE 708, ' + IntToStr(colorValues[1]) +
                    ' : POKE 709, ' + IntToStr(colorValues[2]) + #13#10 +
                    'POKE 710, ' + IntToStr(colorValues[3]) +
                    ' : POKE 711, ' + IntToStr(colorValues[10]) + #13#10 +
                    'POKE 712, ' + IntToStr(colorValues[0]) + #13#10;

     code.line += WaitKeyCode(_FAST_BASIC);
  end;

  result := code.line;
end;

// Data values
function TfrmAntic6Gen.Example03 : string;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (radLang.ItemIndex = _ATARI_BASIC) or (radLang.ItemIndex = _TURBO_BASIC_XL) then
    code.line := DataValues(_ATARI_BASIC, frmAntic6.fldAtascii, code.number, code.step,
                            frmAntic6.modeHeight, 20, rgDataType.ItemIndex)
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then
    code.line := DataValues(_ACTION, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, frmAntic6.modeSize, rgDataType.ItemIndex)
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then
    code.line := DataValues(_MAD_PASCAL, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, frmAntic6.modeSize, rgDataType.ItemIndex)
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then
    code.line := DataValues(_FAST_BASIC, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, frmAntic6.modeSize, rgDataType.ItemIndex);
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

// Data values with screen loader
function TfrmAntic6Gen.Example04 : string;
var
  strTextWindow, grMode : string;
begin
  grMode := IntToStr(frmAntic6.textMode);

  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if radLang.ItemIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS ' + grMode + strTextWindow) +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                 CodeLine('FOR I=0 TO ' + IntToStr(frmAntic6.modeSize - 1)) +
                 CodeLine('READ BYTE') +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine('NEXT I') +
                 WaitKeyCode(radLang.ItemIndex);

    // Screen data
//    Inc(lineNum, 10);
    code.line += DataValues(_ATARI_BASIC, frmAntic6.fldAtascii, code.number, code.step,
                       frmAntic6.modeHeight, 20, rgDataType.ItemIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS ' + grMode + strTextWindow) +
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('FOR I=%0 TO ' + IntToStr(frmAntic6.modeSize - 1)) +
                 CodeLine('READ BYTE') +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine('NEXT I') +
                 WaitKeyCode(radLang.ItemIndex);
    // Screen data
//    Inc(lineNum, 10);
    code.line += DataValues(_TURBO_BASIC_XL, frmAntic6.fldAtascii, code.number, code.step,
                       frmAntic6.modeHeight, 20, rgDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    code.line := DataValues(_ACTION, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, frmAntic6.modeSize, rgDataType.ItemIndex);
    code.line += #13#10 +
                 '; Screen memory address'#13#10 +
                 'CARD SCREEN = 88'#13#10#13#10 +
                 '; Keyboard code of last key pressed'#13#10 +
                 'BYTE CH = $2FC'#13#10#13#10 +
                 'PROC Main()'#13#10#13#10 +
                 'Graphics(' + grMode + strTextWindow + ')'#13#10#13#10 +
                 'MoveBlock(SCREEN, screenData, ' + IntToStr(frmAntic6.modeSize) + ')'#13#10;
    code.line += WaitKeyCode(radLang.ItemIndex) +
                 #13#10'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    code.line := 'uses'#13#10 +
                 '  FastGraph, Crt;'#13#10#13#10 +
                 'var'#13#10 +
                 ' screen : word absolute 88;'#13#10#13#10 +
                 DataValues(_MAD_PASCAL, frmAntic6.fldAtascii,
                            frmAntic6.modeHeight, 20, frmAntic6.modeSize, rgDataType.ItemIndex) +
                 #13#10 +
                 'begin'#13#10 +
                 '  InitGraph(' + grMode + strTextWindow +');'#13#10#13#10 +
                 '  Move(screenData, pointer(screen), ' + IntToStr(frmAntic6.modeSize) + ');'#13#10 +
                 WaitKeyCode(radLang.ItemIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    code.line := DataValues(_FAST_BASIC, frmAntic6.fldAtascii,
                       frmAntic6.modeHeight, 20, frmAntic6.modeSize, rgDataType.ItemIndex);
    code.line += #13#10 +
                 'GRAPHICS ' + grMode + strTextWindow + #13#10 +
                 'scr = DPEEK(88)'#13#10 +
                 'MOVE ADR(screenData), scr, ' + IntToStr(frmAntic6.modeSize) + #13#10 +
                 WaitKeyCode(radLang.ItemIndex);
  end;

  result := code.line;
end;

procedure TfrmAntic6Gen.listExamplesProc(Sender: TObject);
var
  i : byte;
begin
  for i := 0 to radLang.Items.Count - 1 do
    radLang.Controls[i].Enabled := listings[listExamples.ItemIndex, i];
//    radLang.Controls[i].Visible := listings[listExamples.ItemIndex, i];

  radLang.ItemIndex := langIndex;
  editFilename.Visible := listExamples.ItemIndex = 1;
  editFilename.Enabled := listExamples.ItemIndex = 1;
  CreateCode;
end;

procedure TfrmAntic6Gen.radLangProc(Sender: TObject);
begin
  //langIndex := radLang.ItemIndex;
  CreateCode;
end;

end.

