{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Antic mode 4 & 5 editor - source code generator
}
unit antic4_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  lcltype, BCTrackbarUpdown, BCListBox, BCMDButton, BCMaterialDesignButton, Windows,
  common;

type
  { TfrmAntic4Gen }
  TfrmAntic4Gen = class(TForm)
    boxColors : TGroupBox;
    btnCopyToEditor : TBCMaterialDesignButton;
    btnClose : TBCMaterialDesignButton;
    btnAtariBASIC : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnKickC : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    chkMaxSize : TCheckBox;
    chkTextWindow: TCheckBox;
    chkUseColors : TCheckBox;
    color0 : TShape;
    color1 : TShape;
    color2 : TShape;
    color3 : TShape;
    color4 : TShape;
    editFilename: TLabeledEdit;
    editFontName: TLabeledEdit;
    editLineStep : TBCTrackbarUpdown;
    editMaxX : TBCTrackbarUpdown;
    editMaxY : TBCTrackbarUpdown;
    editStartLine : TBCTrackbarUpdown;
    boxFilenames: TGroupBox;
    boxStartLine : TGroupBox;
    boxResizeScreen : TGroupBox;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    Label6 : TLabel;
    Label7 : TLabel;
    Label8 : TLabel;
    lblLineStep : TLabel;
    lblStartLine : TLabel;
    memo: TMemo;
    panelLang : TBCPaperPanel;
    radDataType: TRadioGroup;
    StaticText1 : TStaticText;
    StaticText2 : TStaticText;
    StaticText3 : TStaticText;
    listExamples: TTreeView;
    procedure chkUseColorsChange(Sender : TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditorProc(Sender: TObject);
    procedure ExamplesProc(Sender: TObject);
    procedure editStartLineChange(Sender : TObject);
    procedure editStartLineMouseLeave(Sender : TObject);
    procedure editStartLineMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure LanguageProc(Sender : TObject);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
    procedure CheckMaxSizeProc(Sender : TObject);
    procedure CloseWinProc(Sender: TObject);
  private
    { private declarations }
    listings : TListings;
    anticMode : string;
    isMaxSize : boolean;
    isCreate : boolean;
    procedure CreateCode;
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
  isCreate := true;

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

  anticMode := IntToStr(frmAntic4.anticMode);

  SetTrackBarUpDown(editStartLine, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editLineStep, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editMaxX, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editMaxY, $00DDDDDD, clWhite);
end;

procedure TfrmAntic4Gen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;

  //if frmAntic4.fontName = '' then
  //  editFontName.Text:= 'H1:DEFAULT.FNT'
  //else
  //  editFontName.Text:= 'H1:' + ExtractFileName(frmAntic4.fontName);

  editMaxX.Value := frmAntic4.editX.Value;
  editMaxY.Value := frmAntic4.editY.Value;
  editFilename.Text := 'H1:' + ExtractFileName(frmAntic4.filename);
  isCreate := false;

  color0.Brush.Color := colTab[0];   // POKE 712,C
  color1.Brush.Color := colTab[1];   // POKE 708,C
  color2.Brush.Color := colTab[2];   // POKE 709,C
  color3.Brush.Color := colTab[3];   // POKE 710,C
  color4.Brush.Color := colTab[10];  // POKE 711,C

  langIndex := 0;
  listExamples.Selected := listExamples.Items.GetFirstNode;
  ExamplesProc(Sender);
end;

procedure TfrmAntic4Gen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmAntic4Gen.ExamplesProc(Sender: TObject);
begin
  //for i := 0 to radLang.Items.Count - 1 do begin
  //  //radLang.Controls[i].Enabled := listings[listExamples.ItemIndex, i];
  //  radLang.Controls[i].Enabled := listings[listExamples.selected.AbsoluteIndex - 1, i];
  //end;
  //radLang.ItemIndex := langIndex;
  editFilename.Visible := listExamples.selected.AbsoluteIndex >= 5;
  editFilename.Enabled := editFilename.Visible;
  editFontName.Visible := listExamples.selected.AbsoluteIndex = 6;
  editFontName.Enabled := editFontName.Visible;

  CreateCode;
end;

procedure TfrmAntic4Gen.CopyToEditorProc(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor(Sender);
  Close;
end;

procedure TfrmAntic4Gen.CreateCode;
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

  isMaxSize := (editMaxX.Value = 39) and (editMaxY.Value = 23);
  chkMaxSize.Checked := isMaxSize;

  case listExamples.selected.AbsoluteIndex of
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

{-----------------------------------------------------------------------------
 Data values for selected character
 -----------------------------------------------------------------------------}
function TfrmAntic4Gen.Example01 : string;
begin
  { Atari BASIC / Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM CHR$(' + AtasciiCode(frmAntic4.offs) + ')');
    code.line += CodeLine('DATA ' + SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet, 255,
                          radDataType.ItemIndex, ','));
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := 'BYTE ARRAY CHARDATA=[' +  //#13#10 +
                 SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                               255, radDataType.ItemIndex, ' ') + ']'#13#10;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := 'const'#13#10 +
                 '  charData : array[0..7] of byte ='#13#10 +
                 '    (' + SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                    255, radDataType.ItemIndex, ', ') + ');'#13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := 'DATA charData() BYTE = ' +
                 SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet, 255,
                               radDataType.ItemIndex, ', ') + #13#10;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    //    const char pmdata[] = { 0,255,129,129,129,129,129,129,255,0};
    code.line := 'const char charData[] = {'#13#10 +
                 '  ' + SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                 255, radDataType.ItemIndex, ', ') + '};'#13#10;
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Data values for modified characters
 -----------------------------------------------------------------------------}
function TfrmAntic4Gen.Example02 : string;
var
  i : byte;
begin
  code.line := '';

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex = _ATARI_BASIC) or (langIndex = _TURBO_BASIC_XL) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then begin
        code.line += CodeLine(' REM CHR$(' + AtasciiCode(i) + ')');
        code.line += CodeLine(' DATA ' + SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                              i, radDataType.ItemIndex, ','));
      end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then begin
       code.line += '; Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                    'BYTE ARRAY char' + IntToStr(i) + '=[' +  //#13#10'  ' +
                    SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                  i, radDataType.ItemIndex, ' ') + ']'#13#10;
     end;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then begin
       code.line += '  // Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                    '  char' + IntToStr(i) + ' : array[0..7] of byte = (' +
                    SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                  i, radDataType.ItemIndex, ', ') + ');'#13#10;
     end;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then begin
       code.line += ''' Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                    'DATA char' + IntToStr(i) + '() BYTE = ' +
                    SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                  i, radDataType.ItemIndex, ', ') + #13#10;
     //         SetValues(_FAST_BASIC, i)
     end;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then begin
       code.line += '// Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                    'const char char' + IntToStr(i) + '[] = {' +
                    SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                  i, radDataType.ItemIndex, ', ') + '};'#13#10;
     end;
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Screen data values
 -----------------------------------------------------------------------------}
function TfrmAntic4Gen.Example03 : string;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex = _ATARI_BASIC) or (langIndex = _TURBO_BASIC_XL) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := DataValues(_ATARI_BASIC, frmAntic4.fldAtascii, code.number, code.step,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then
//    frmAntic4.maxSize
    code.line := DataValues(_ACTION, frmAntic4.fldAtascii,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex)
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then
    code.line := DataValues(_MAD_PASCAL, frmAntic4.fldAtascii,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex)
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then
    code.line := DataValues(_FAST_BASIC, frmAntic4.fldAtascii,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex)
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then
    code.line := DataValues(_KICKC, frmAntic4.fldAtascii,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Data values with screen loader
 -----------------------------------------------------------------------------}
function TfrmAntic4Gen.Example04 : string;
var
  screenCode : array[0..2] of string;
  maxSize : word;
  strTextWindow : string = '';
  grMode : string;
begin
  grMode := IntToStr(frmAntic4.anticMode + 8);
  maxSize := (editMaxX.Value + 1)*(editMaxY.Value + 1) - 1;
  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    if (editMaxX.Value < 40) or (editMaxY.Value < 24) then begin
      screenCode[0] := ':CNT=0';
      screenCode[1] := 'IF CNT=' + IntToStr(editMaxX.Value + 1) + ' THEN SCR=SCR+40-CNT:CNT=0';
      screenCode[2] := 'CNT=CNT+1';
    end;
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS ' + grMode + strTextWindow + screenCode[0]) +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                 CodeLine('FOR I=0 TO ' + IntToStr(maxSize)) +
                 CodeLine('READ BYTE') +
                 CodeLine(screenCode[1]) +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine(screenCode[2]) +
                 CodeLine('NEXT I');
    if chkUseColors.Checked then
      code.line += GenSetColors(_ATARI_BASIC);
      //code.line += CodeLine('POKE 708,' + IntToStr(colorValues[1]) + ':POKE 709,' + IntToStr(colorValues[2])) +
      //             CodeLine('POKE 710,' + IntToStr(colorValues[3]) + ':POKE 711,' + IntToStr(colorValues[10])) +
      //             CodeLine('POKE 712,' + IntToStr(colorValues[0]));

    code.line += WaitKeyCode(langIndex);

    // Screen data
    code.line += DataValues(_ATARI_BASIC, frmAntic4.fldAtascii, code.number, code.step,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS ' + grMode + strTextWindow + screenCode[0]) +
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('FOR I=%0 TO ' + IntToStr(maxSize)) +
                 CodeLine('READ BYTE') +
                 CodeLine(screenCode[1]) +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine(screenCode[2]) +
                 CodeLine('NEXT I');
    if chkUseColors.Checked then
      code.line += GenSetColors(_TURBO_BASIC_XL);
      //code.line += CodeLine('POKE 708,' + IntToStr(colorValues[1]) + ':POKE 709,' + IntToStr(colorValues[2])) +
      //             CodeLine('POKE 710,' + IntToStr(colorValues[3]) + ':POKE 711,' + IntToStr(colorValues[10])) +
      //             CodeLine('POKE 712,' + IntToStr(colorValues[0]));

    code.line += WaitKeyCode(langIndex);

    // Screen data
    code.line += DataValues(_TURBO_BASIC_XL, frmAntic4.fldAtascii, code.number, code.step,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
//    frmAntic2.maxSize + 1
    code.line := DataValues(_ACTION, frmAntic4.fldAtascii, editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
    code.line += #13#10'; Screen memory address'#13#10 +
                 'CARD SCREEN = 88'#13#10#13#10 +
                 'CARD i'#13#10 +
                 'CARD cnt = [0]'#13#10#13#10 +
                 '; Keyboard code of last key pressed'#13#10 +
                 'BYTE CH = $2FC'#13#10#13#10 +
                 'PROC Main()'#13#10#13#10 +
                 'Graphics(' + grMode + strTextWindow + ')'#13#10#13#10;
    if (editMaxX.Value = 39) and (editMaxY.Value = 23) then
      code.line += 'MoveBlock(SCREEN, screenData, ' + IntToStr(maxSize + 1) + ')'#13#10
    else begin
      code.line += 'SCREEN=PEEKC(88)'#13#10;
      code.line += 'FOR i=0 TO ' + IntToStr(maxSize) + ' DO'#13#10 +
                   '  IF cnt = ' + IntToStr(editMaxX.Value + 1) + ' THEN'#13#10 +
                   '    screen==+40-cnt'#13#10 +
                   '    cnt = 0'#13#10 +
                   '  FI'#13#10 +
                   '  POKEC(screen + i, screenData(i))'#13#10 +
                   '  cnt==+1'#13#10 +
//                   '  cnt=cnt+1'#13#10 +
                   'OD'#13#10;
    end;

    if chkUseColors.Checked then
      code.line += GenSetColors(_ACTION);
      //code.line += #13#10 +
      //             'POKE(708,' + IntToStr(colorValues[1]) + ') POKE(709,' +
      //             IntToStr(colorValues[2]) + ')'#13#10 +
      //             'POKE(710,' + IntToStr(colorValues[3]) + ') POKE(711,' +
      //             IntToStr(colorValues[10]) + ')'#13#10 +
      //             'POKE(712,' + IntToStr(colorValues[0]) + ')'#13#10;

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
    code.line += DataValues(_MAD_PASCAL, frmAntic4.fldAtascii, editMaxY.Value, editMaxX.Value,
                            radDataType.ItemIndex);
    code.line += #13#10'begin'#13#10 +
                 '  InitGraph(' + grMode + strTextWindow + ');'#13#10#13#10;

    if (editMaxX.Value = 39) and (editMaxY.Value = 23) then
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

    if chkUseColors.Checked then
      code.line += GenSetColors(_MAD_PASCAL);
      //code.line += #13#10'  POKE(712, ' + IntToStr(colorValues[0]) + ');'#13#10 +
      //             '  POKE(708, ' + IntToStr(colorValues[1]) + ');'#13#10 +
      //             '  POKE(709, ' + IntToStr(colorValues[2]) + ');'#13#10 +
      //             '  POKE(710, ' + IntToStr(colorValues[3]) + ');'#13#10 +
      //             '  POKE(711, ' + IntToStr(colorValues[10]) + ');'#13#10;

    code.line += WaitKeyCode(langIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := DataValues(_FAST_BASIC, frmAntic4.fldAtascii,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
    code.line += #13#10#13#10'GRAPHICS ' + grMode + strTextWindow + #13#10 +
                 'scr = DPEEK(88)'#13#10;
    if (editMaxX.Value = 39) and (editMaxY.Value = 23) then
      code.line += 'MOVE ADR(screenData), scr, ' + IntToStr(frmAntic4.maxSize + 1) + #13#10
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

    if chkUseColors.Checked then
      code.line += GenSetColors(_FAST_BASIC);
      //code.line += #13#10 +
      //             'POKE 708, ' + IntToStr(colorValues[1]) +
      //             ' : POKE 709, ' + IntToStr(colorValues[2]) + #13#10 +
      //             'POKE 710, ' + IntToStr(colorValues[3]) +
      //             ' : POKE 711, ' + IntToStr(colorValues[10]) + #13#10 +
      //             'POKE 712, ' + IntToStr(colorValues[0]) + #13#10;

    code.line += WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Modified characters with screen loader
 -----------------------------------------------------------------------------}
function TfrmAntic4Gen.Example05 : string;
var
  i : byte;
  strTextWindow : string = '';
  grMode : string;
begin
  grMode := IntToStr(frmAntic4.anticMode + 8);
  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM ******************************') +
                 CodeLine('REM MODIFIED CHARACTERS') +
                 CodeLine('REM IN ANTIC MODE ' + anticMode) +
                 CodeLine('REM ******************************') +
                 CodeLine('NMEMTOP=PEEK(106)-16') +
                 CodeLine('POKE 106,NMEMTOP') +
                 CodeLine('GRAPHICS ' + grMode + strTextWindow) +
                 CodeLine('CHRAM=NMEMTOP*256') +
                 CodeLine('OLDVAL=PEEK(559):POKE 559,0') +
                 CodeLine('FOR I=0 TO 1023') +
                 CodeLine('POKE CHRAM+I,PEEK(57344+I)') +
                 CodeLine('NEXT I');
    // Modify characters
    code.line += CodeLine('REM MODIFIED CHARACTERS');
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then
        code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE CHRAM+I+' + IntToStr(i) + '*8,CHAR:NEXT I');

    code.line += CodeLine('REM MODIFY CHARACTER SET POINTER') +
                 CodeLine('POKE 756,NMEMTOP') +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                 CodeLine('POKE 559,OLDVAL') +
                 CodeLine('FOR I=0 TO ' + IntToStr(frmAntic4.maxSize - 1)) +
                 CodeLine('READ BYTE:POKE SCR+I,BYTE') +
                 CodeLine('NEXT I');
    if chkUseColors.Checked then
      code.line += GenSetColors(_ATARI_BASIC);
      //code.line += CodeLine('POKE 712,' + IntToStr(colorValues[0]) + ':' +
      //                      'POKE 708,' + IntToStr(colorValues[1]) + ':' +
      //                      'POKE 709,' + IntToStr(colorValues[2]) + ':' +
      //                      'POKE 710,' + IntToStr(colorValues[3]) + ':' +
      //                      'POKE 711,' + IntToStr(colorValues[10]));

    CodeLine('REM WAIT FOR KEY PRESS');
    code.line += WaitKeyCode(langIndex);
    // Modified characters
    code.line += CodeLine('REM MODIFIED CHARACTER DATA');
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
        code.line += CodeLine('DATA ' + SetDataValues(
                              frmAntic4.fldChar, frmAntic4.fldFontSet, i, radDataType.ItemIndex, ','));
      end;

    // Screen data
//    Inc(lineNum, 10);
//    code += IntToStr(lineNum) + ' REM SCREEN DATA'#13#10;
//    Inc(code.number, code.step);
    code.line += CodeLine('REM SCREEN DATA');
    code.line += DataValues(_ATARI_BASIC, frmAntic4.fldAtascii, code.number, code.step,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
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
   code.line += CodeLine('REM MODIFIED CHARACTERS');
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then
       code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE CHRAM+I+' + IntToStr(i) +
                             '*8,CHAR:NEXT I');

   code.line += CodeLine('REM MODIFY CHARACTER SET POINTER') +
                CodeLine('POKE 756,NMEMTOP') +
                CodeLine('SCR=DPEEK(88)') +
                CodeLine('FOR I=0 TO ' + IntToStr(frmAntic4.maxSize - 1)) +
                CodeLine('READ BYTE:POKE SCR+I,BYTE') +
                CodeLine('NEXT I');
    if chkUseColors.Checked then
      code.line += GenSetColors(_TURBO_BASIC_XL);
      //code.line += CodeLine('POKE 712,' + IntToStr(colorValues[0]) + ':' +
      //                      'POKE 708,' + IntToStr(colorValues[1]) + ':' +
      //                      'POKE 709,' + IntToStr(colorValues[2]) + ':' +
      //                      'POKE 710,' + IntToStr(colorValues[3]) + ':' +
      //                      'POKE 711,' + IntToStr(colorValues[10]));

    CodeLine('REM WAIT FOR KEY PRESS');
    code.line += WaitKeyCode(langIndex);

   // Modified characters
   code.line += CodeLine('REM MODIFIED CHARACTER DATA');
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then
       code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
       code.line += CodeLine('DATA ' + SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                       i, radDataType.ItemIndex, ','));

   // Screen data
   code.line += CodeLine('REM SCREEN DATA');
   code.line += DataValues(_ATARI_BASIC, frmAntic4.fldAtascii, code.number, code.step,
                           editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := '; Modified characters in Antic mode ' + anticMode + #13#10#13#10;
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then
        code.line += '; Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                     'BYTE ARRAY char' + IntToStr(i) + '=[' +  //#13#10'  ' +
                     SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                   i, radDataType.ItemIndex, ' ') + ']'#13#10;

    code.line += #13#10 + DataValues(
                 _ACTION, frmAntic4.fldAtascii, editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
    code.line += '; Screen memory address'#13#10 +
                 'CARD screen=88'#13#10#13#10 +
                 '; Keyboard code of last key pressed'#13#10 +
                 'BYTE ch = $2FC'#13#10#13#10 +
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

    code.line += 'MoveBlock(screen, screenData, ' + IntToStr(frmAntic4.maxSize) + ')'#13#10;

    if chkUseColors.Checked then
      code.line += GenSetColors(_ACTION);
      //code.line += 'POKE(712, ' + IntToStr(colorValues[0]) + ')'#13#10 +
      //             'POKE(708, ' + IntToStr(colorValues[1]) + ')'#13#10 +
      //             'POKE(709, ' + IntToStr(colorValues[2]) + ')'#13#10 +
      //             'POKE(710, ' + IntToStr(colorValues[3]) + ')'#13#10 +
      //             'POKE(711, ' + IntToStr(colorValues[10]) + ')'#13#10;

    code.line += WaitKeyCode(langIndex) + #13#10 +
                 'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '// Modified characters in Antic mode ' + anticMode + #13#10#13#10 +
                 'uses'#13#10 +
                 '  FastGraph, Crt;'#13#10#13#10 +
                 'var'#13#10 +
                 '  topMem : word;'#13#10 +
                 '  CHBAS  : byte absolute $2F4;'#13#10 +
                 '  RAMTOP : byte absolute $6A;'#13#10 +
                 '  screen : word absolute 88;'#13#10#13#10;
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then
        code.line += '  // Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) + #13#10 +
                     '  char' + IntToStr(i) + ' : array[0..7] of byte = (' +
                     SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                   i, radDataType.ItemIndex, ', ') + ');'#13#10;

    code.line += #13#10 + DataValues(_MAD_PASCAL, frmAntic4.fldAtascii,
                 editMaxY.Value, editMaxX.Value, radDataType.ItemIndex) + #13#10 +
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

    code.line += '  Move(screenData, pointer(screen), ' + IntToStr(frmAntic4.maxSize) + ');'#13#10;

    if chkUseColors.Checked then
      code.line += GenSetColors(_MAD_PASCAL);
      //code.line += '  POKE(712, ' + IntToStr(colorValues[0]) + ');'#13#10 +
      //             '  POKE(708, ' + IntToStr(colorValues[1]) + ');'#13#10 +
      //             '  POKE(709, ' + IntToStr(colorValues[2]) + ');'#13#10 +
      //             '  POKE(710, ' + IntToStr(colorValues[3]) + ');'#13#10 +
      //             '  POKE(711, ' + IntToStr(colorValues[10]) + ');'#13#10;

    code.line += WaitKeyCode(langIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
   else if langIndex = _FAST_BASIC then begin
     code.line := ''' Modified characters in Antic mode ' + anticMode + #13#10#13#10;
     for i := 0 to 127 do
       if frmAntic4.charEditIndex[i] = 1 then
         code.line += ''' Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) +
                      #13#10'DATA char' + IntToStr(i) + '() BYTE = ' +
                      SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                    i, radDataType.ItemIndex, ', ') + #13#10;

     code.line += #13#10 + DataValues(_FAST_BASIC, frmAntic4.fldAtascii,
                  editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
     code.line += #13#10 +
                  'NMEMTOP = PEEK(106) - 8'#13#10 +
                  'POKE 106, NMEMTOP'#13#10 +
                  'GRAPHICS ' + grMode + strTextWindow + #13#10 +
                  'scr = DPEEK(88)'#13#10 +
                  'CHRAM = NMEMTOP*256'#13#10 +
                  'MOVE 57344, CHRAM, 1024'#13#10#13#10 +
                  ''' Custom character set data'#13#10;
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then
//        code += 'MOVE CHRAM + ' + IntToStr(i) + '*8, ADR(char' + IntToStr(i) + '), 8'#13#10;
        code.line += 'MOVE ADR(char' + IntToStr(i) + '), CHRAM + ' + IntToStr(i) + '*8, 8'#13#10;

    code.line += #13#10''' MODIFY CHARACTER SET POINTER'#13#10 +
                 'POKE 756, NMEMTOP'#13#10 +
                 'MOVE ADR(screenData), scr, ' + IntToStr(frmAntic4.maxSize) + #13#10;

    if chkUseColors.Checked then
      code.line += GenSetColors(_FAST_BASIC);
      //code.line += 'POKE 712, ' + IntToStr(colorValues[0]) + #13#10 +
      //             'POKE 708, ' + IntToStr(colorValues[1]) + #13#10 +
      //             'POKE 709, ' + IntToStr(colorValues[2]) + #13#10 +
      //             'POKE 710, ' + IntToStr(colorValues[3]) + #13#10 +
      //             'POKE 711, ' + IntToStr(colorValues[10]) + #13#10;

    code.line += WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Load Antic mode 4/5 screen
 -----------------------------------------------------------------------------}
function TfrmAntic4Gen.Example06 : string;
//var
//  screenCode : array[0..2] of string;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM ******************************') +
                 CodeLine('REM LOAD ANTIC MODE ' + anticMode + ' SCREEN') +
                 CodeLine('REM ******************************') +
                 CodeLine('GRAPHICS ' + IntToStr(frmAntic4.anticMode + 8) + '+16') +
                 CodeLine('CLOSE #1') +
                 CodeLine('OPEN #1,4,0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                 CodeLine('REM SCREEN DIM. VALUES') +
                 CodeLine('GET #1,MAXX') +
                 CodeLine('GET #1,MAXY') +
                 CodeLine('SIZE=(MAXX+1)*(MAXY+1)-1') +
                 CodeLine('REM COLOR VALUES') +
                 CodeLine('GET #1,COLOR4') +
                 CodeLine('GET #1,COLOR0') +
                 CodeLine('GET #1,COLOR1') +
                 CodeLine('GET #1,COLOR2') +
                 CodeLine('GET #1,COLOR3') +
                 CodeLine('POKE 712,COLOR4') +
                 CodeLine('POKE 708,COLOR0') +
                 CodeLine('POKE 709,COLOR1') +
                 CodeLine('POKE 710,COLOR2') +
                 CodeLine('POKE 711,COLOR3') +
                 CodeLine('REM SCREEN VALUES') +
                 CodeLine('CNT=0') +
                 CodeLine('FOR I=0 TO SIZE') +
                 CodeLine('GET #1,BYTE:IF CNT=MAXX+1 THEN SCR=SCR+40-CNT:CNT=0') +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine('CNT=CNT+1') +
                 CodeLine('NEXT I') +
                 CodeLine('CLOSE #1');
    code.line += WaitKeyCode(langIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM LOAD ANTIC MODE ' + anticMode + ' SCREEN') +
                 CodeLine('--') +
                 CodeLine('GRAPHICS ' + IntToStr(frmAntic4.anticMode + 8) + '+16') +
                 CodeLine('CLOSE #%1') +
                 CodeLine('OPEN #%1,4,%0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('REM SCREEN DIM. VALUES') +
                 CodeLine('GET #%1,MAXX') +
                 CodeLine('GET #%1,MAXY') +
                 CodeLine('SIZE=(MAXX+%1)*(MAXY+%1)-%1') +
                 CodeLine('REM COLOR VALUES') +
                 CodeLine('GET #%1,COLOR4') +
                 CodeLine('GET #%1,COLOR0') +
                 CodeLine('GET #%1,COLOR1') +
                 CodeLine('GET #%1,COLOR2') +
                 CodeLine('GET #%1,COLOR3') +
                 CodeLine('POKE 712,COLOR4') +
                 CodeLine('POKE 708,COLOR0') +
                 CodeLine('POKE 709,COLOR1') +
                 CodeLine('POKE 710,COLOR2') +
                 CodeLine('POKE 711,COLOR3') +
                 CodeLine('REM SCREEN VALUES') +
                 CodeLine('CNT=%0') +
                 CodeLine('FOR I=%0 TO SIZE') +
                 // CodeLine('BGET #%1,DPEEK(88),SIZE') +
                 CodeLine('GET #%1,BYTE:IF CNT=MAXX+%1 THEN SCR=SCR+40-CNT:CNT=%0') +
                 CodeLine('POKE SCR+I,BYTE') +
                 CodeLine('CNT=CNT+%1') +
                 CodeLine('NEXT I') +
                 CodeLine('CLOSE #%1');
    code.line += WaitKeyCode(langIndex);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
  //    if cnt=' + IntToStr(frmAsciiEditor.maxX) +
  //    then begin scr := scr + 40 - cnt; cnt := 0; end;';
  //    cnt==+1';
    code.line := '// Load ANTIC mode ' + anticMode + ' screen'#13#10#13#10 +
                 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt;'#13#10#13#10 +
                 'const'#13#10 +
                 '  picSize = ' + IntToStr(frmAntic4.maxSize) + ';'#13#10#13#10 +
                 'var'#13#10 +
                 '  f : file;'#13#10 +
                 '  filename : TString;'#13#10 +
                 '  buf : pointer;'#13#10#13#10 +
                 '  cnt : byte = 0;'#13#10#13#10 +
                 'begin'#13#10 +
                 '  InitGraph(' + IntToStr(frmAntic4.anticMode + 8) + ' + 16);'#13#10 +
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
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := '; Load ANTIC mode ' + anticMode + ' screen'#13#10 +
                 #13#10'BYTE ch=$2FC'#13#10 +
     //            'BYTE screen=88'#13#10 +
                 'BYTE data, cnt=[0]'#13#10 +
                 'CARD size'#13#10 +
                 'CARD scr'#13#10 +
                 'BYTE maxX, maxY'#13#10 +
                 'BYTE color4'#13#10 +
                 'BYTE color0, color1, color2, color3'#13#10 +
                 'CARD n'#13#10#13#10 +
                 'PROC Main()'#13#10#13#10 +
                 'Graphics(' + IntToStr(frmAntic4.anticMode + 8) + '+16)'#13#10 +
                 'scr=PeekC(88)'#13#10#13#10 +
                 '; Set up channel 2 for screen'#13#10 +
                 'Close(2)'#13#10 +
                 'Open(2,"' + editFilename.Text + '",4,0)'#13#10#13#10 +
                 '; Screen dimension values'#13#10 +
                 'maxX=GetD(2)'#13#10 +
                 'maxY=GetD(2)'#13#10 +
                 'size=(maxX+1)*(maxY+1)-1'#13#10#13#10 +
                 '; Color values'#13#10 +
                 'color4=GetD(2)'#13#10 +
                 'color0=GetD(2)'#13#10 +
                 'color1=GetD(2)'#13#10 +
                 'color2=GetD(2)'#13#10 +
                 'color3=GetD(2)'#13#10 +
                 'Poke(712, color4)'#13#10 +
                 'Poke(708, color0)'#13#10 +
                 'Poke(709, color1)'#13#10 +
                 'Poke(710, color2)'#13#10 +
                 'Poke(711, color3)'#13#10#13#10 +
                 '; Screen data'#13#10 +
                 'FOR n=0 TO size DO'#13#10 +
                 '  data=GetD(2)'#13#10 +
                 '  IF cnt=maxX+1 THEN scr==+40-cnt cnt=0 FI'#13#10 +
                 '  PokeC(scr+n,data)'#13#10 +
                 '  cnt==+1'#13#10 +
                 'OD'#13#10#13#10 +
                 '; Close channel 2'#13#10 +
                 'Close(2)'#13#10#13#10 +
                 WaitKeyCode(langIndex) + #13#10 +
                 'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
   else if langIndex = _FAST_BASIC then begin
     code.line := ''' Load ANTIC mode ' + anticMode + ' screen'#13#10#13#10 +
                  'GRAPHICS ' + IntToStr(frmAntic4.anticMode + 8) + ' + 16'#13#10 +
                  'CLOSE #1'#13#10 +
                  'OPEN #1, 4, 0, "' + editFilename.Text + '"'#13#10;

       code.line += 'scr = DPEEK(88)'#13#10 +
                    #13#10 +
                    ''' Screen dimension values'#13#10 +
                    'GET #1, maxX'#13#10 +
                    'GET #1, maxY'#13#10 +
                    'size = (maxX + 1)*(maxY + 1) - 1'#13#10#13#10 +
                    ''' Color values'#13#10 +
                    'GET #1, color4'#13#10 +
                    'GET #1, color0'#13#10 +
                    'GET #1, color1'#13#10 +
                    'GET #1, color2'#13#10 +
                    'GET #1, color3'#13#10 +
                    'POKE 712, color4'#13#10 +
                    'POKE 708, color0'#13#10 +
                    'POKE 709, color1'#13#10 +
                    'POKE 710, color2'#13#10 +
                    'POKE 711, color3'#13#10#13#10 +
                    ''' Screen data'#13#10 +
                    'cnt = 0'#13#10 +
                    'FOR i = 0 TO size'#13#10 +
                    '  GET #1, byte'#13#10 +
                    '  IF cnt = maxX + 1'#13#10 +
                    '    scr = scr + 40 - cnt : cnt = 0'#13#10 +
                    '  ENDIF'#13#10 +
                    '  POKE scr + i, byte : cnt = cnt + 1'#13#10 +
                    'NEXT'#13#10;

     code.line += #13#10'CLOSE #1'#13#10 +
                  WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;
(*
{-----------------------------------------------------------------------------
 Load Antic mode 4/5 screen with custom character set
 -----------------------------------------------------------------------------}
function TfrmAntic4Gen.Example07 : string;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
   code.number := editStartLine.Value;
   code.step := editLineStep.Value;
   code.line := CodeLine('REM ******************************') +
                CodeLine('REM LOAD ANTIC MODE ' + anticMode + ' SCREEN') +
                CodeLine('REM WITH CUSTOM CHARACTER SET ') +
                CodeLine('REM ******************************') +
                CodeLine('SIZE=' + IntToStr(frmAntic4.maxSize)) +
                CodeLine('GRAPHICS ' + IntToStr(frmAntic4.anticMode + 8) + '+16') +
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
    code.line += WaitKeyCode(langIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM LOAD ANTIC MODE ' + anticMode + ' SCREEN') +
                 CodeLine('REM WITH CUSTOM CHARACTER SET') +
                 CodeLine('--') +
                 CodeLine('SIZE=' + IntToStr(frmAntic4.maxSize)) +
                 CodeLine('GRAPHICS ' + IntToStr(frmAntic4.anticMode + 8) + '+16') +
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
                 WaitKeyCode(langIndex);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '// Load ANTIC mode ' + anticMode + ' screen'#13#10 +
                 '// and custom character set'#13#10 +
                 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt;'#13#10#13#10 +
                 'const'#13#10 +
                 '  picSize = ' + IntToStr(frmAntic4.maxSize) + ';'#13#10#13#10 +
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
                 '  InitGraph(' + IntToStr(frmAntic4.anticMode + 8) + '+16);'#13#10#13#10 +
                 '  // Set new character set'#13#10 +
                 '  topMem := RAMTOP - 12;'#13#10 +
                 '  topMem := topMem shl 8;'#13#10 +
                 '  CHBAS := hi(topMem);'#13#10#13#10 +
                 '  // Load custom character set'#13#10 +
                 '  filename := + ' + QuotedStr(editFontName.Text) + ';'#13#10 +
                 '  Assign(f, filename);'#13#10 +
                 '  Reset(f, 1);'#13#10 +
                 '  BlockRead(f, font, 1024);'#13#10 +
                 '  Close(f);'#13#10#13#10 +
                 '  // Move new character set to reserved memory'#13#10 +
                 '  Move(font, pointer(topMem), SizeOf(font));'#13#10#13#10 +
                 '  // Find screen area pointer'#13#10 +
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
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := '; Load ANTIC mode ' + anticMode + ' screen'#13#10 +
                '; and custom character set'#13#10#13#10 +
                'BYTE CH=$2FC'#13#10 +
    //            'BYTE SCREEN=$58'#13#10 +
                'BYTE RAMTOP=$6A'#13#10 +
                'BYTE CHBAS=$2F4'#13#10#13#10 +
                'BYTE DATA'#13#10 +
                'CARD TOPMEM'#13#10 +
                'CARD N'#13#10 +
                'CARD SCR'#13#10#13#10 +
                'BYTE ARRAY FONT(1023)'#13#10 +
                'CARD SIZE=[' + IntToStr(frmAntic4.maxSize) + ']'#13#10#13#10 +
                'PROC MAIN()'#13#10#13#10 +
                'GRAPHICS(' + IntToStr(frmAntic4.anticMode + 8) + '+16)'#13#10#13#10 +
                '; RESERVE MEMORY FOR NEW CHARACTER SET'#13#10 +
                'TOPMEM=RAMTOP-12'#13#10 +
                'TOPMEM==*256'#13#10#13#10 +
                '; LOAD CUSTOM CHARACTER SET'#13#10#13#10 +
                'CLOSE(2)'#13#10 +
                'OPEN(2,"' + editFontName.Text + '",4,0)'#13#10#13#10 +
                'FOR N=0 TO 1023'#13#10 +
                'DO'#13#10 +
                '  DATA=GETD(2)'#13#10 +
                '  FONT(N)=DATA'#13#10 +
                'OD'#13#10#13#10 +
                'CLOSE(2)'#13#10#13#10 +
                '; COPY FONT SET TO NEW ADDRESS'#13#10 +
                'CHBAS=TOPMEM/256'#13#10 +
                'MOVEBLOCK(TOPMEM,FONT,1024)'#13#10#13#10 +
                '; LOAD ANTIC MODE 4 SCREEN'#13#10#13#10 +
                'SCR=PEEKC(88)'#13#10#13#10 +
                '; SET UP CHANNEL 2 FOR SCREEN'#13#10 +
                'CLOSE(2)'#13#10 +
                'OPEN(2,"' + editFilename.Text + '",4,0)'#13#10#13#10 +
                '; READ FONT DATA BYTE BY BYTE'#13#10 +
                'FOR N=0 TO SIZE-1'#13#10 +
                'DO'#13#10 +
                '  DATA=GETD(2)'#13#10 +
                '  POKEC(SCR+N,DATA)'#13#10 +
                'OD'#13#10#13#10 +
                '; CLOSE CHANNEL 2'#13#10 +
                'CLOSE(2)'#13#10#13#10 +
                '; PRESS ANY KEY TO EXIT'#13#10 +
                WaitKeyCode(langIndex) +
                #13#10'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := ''' Load ANTIC mode ' + anticMode + ' screen'#13#10 +
                ''' and custom character set'#13#10#13#10 +
                'size = ' + IntToStr(frmAntic4.maxSize) + #13#10#13#10 +
                'GRAPHICS ' + IntToStr(frmAntic4.anticMode + 8) + ' + 16'#13#10#13#10 +
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
                WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;
*)
procedure TfrmAntic4Gen.LanguageProc(Sender : TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmAntic4Gen.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmAntic4Gen.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

procedure TfrmAntic4Gen.CheckMaxSizeProc(Sender : TObject);
begin
  if chkMaxSize.Checked then begin
    editMaxX.Value := 39;
    editMaxY.Value := 23;
  end;
end;

procedure TfrmAntic4Gen.chkUseColorsChange(Sender : TObject);
begin
  CreateCode;
end;

procedure TfrmAntic4Gen.editStartLineChange(Sender : TObject);
begin
  if not isCreate then
    CreateCode;
end;

procedure TfrmAntic4Gen.editStartLineMouseLeave(Sender : TObject);
begin
  CreateCode;
end;

procedure TfrmAntic4Gen.editStartLineMouseUp(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  CreateCode;
end;

procedure TfrmAntic4Gen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

