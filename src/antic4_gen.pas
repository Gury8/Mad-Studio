{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2023
  Unit: Antic mode 4 & 5 editor - source code generator
}
unit antic4_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, lcltype, BCListBox, BCMDButton, BCMaterialDesignButton, Windows,
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
    chkCharSet : TCheckBox;
    chkMaxSize : TCheckBox;
    chkTextWindow : TCheckBox;
    chkUseColors : TCheckBox;
    color0 : TShape;
    color1 : TShape;
    color2 : TShape;
    color3 : TShape;
    color4 : TShape;
    editFilename: TLabeledEdit;
    editFontName: TLabeledEdit;
    editLineStep : TSpinEditEx;
    boxFiles: TGroupBox;
    boxStartLine : TGroupBox;
    boxScreen : TGroupBox;
    editMaxX : TSpinEditEx;
    editMaxY : TSpinEditEx;
    editStartLine : TSpinEditEx;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    Label6 : TLabel;
    Label7 : TLabel;
    Label8 : TLabel;
    lblLineStep : TLabel;
    lblStartLine : TLabel;
    listExamples : TBCPaperListBox;
    memo: TMemo;
    panelLang : TBCPaperPanel;
    radDataType: TRadioGroup;
    StaticText1 : TStaticText;
    StaticText2 : TStaticText;
    StaticText3 : TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditorProc(Sender: TObject);
    procedure ExamplesProc(Sender: TObject);
    procedure editCode(Sender : TObject);
    procedure LanguageProc(Sender : TObject);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
    procedure CheckMaxSizeProc(Sender : TObject);
    procedure chkCharSetChange(Sender : TObject);
    procedure CloseWinProc(Sender: TObject);
  private
    { private declarations }
    listings : TListings;
    anticMode : string;
    isMaxSize : boolean;
    isCreate : boolean;
    isModChar : boolean;
    procedure CreateCode;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
    function Example04 : string;
    function Example05 : string;
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

  if frmAntic4.anticMode = 4 then begin
    antic_mode_max_x := _ANTIC_MODE_4_MAX_X;
    antic_mode_max_y := _ANTIC_MODE_4_MAX_Y;
  end
  else begin
    antic_mode_max_x := _ANTIC_MODE_5_MAX_X;
    antic_mode_max_y := _ANTIC_MODE_5_MAX_Y;
  end;

  SetListings(listings);

  // Example 4
  listings[3, 8] := false;

  // Example 5
  listings[4, 8] := false;

  anticMode := IntToStr(frmAntic4.anticMode);
  isModChar := frmAntic4.CheckModChars;
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
  editFontname.Text := 'H1:' + ExtractFileName(frmAntic4.fontName);
  isCreate := false;

  color0.Brush.Color := colTab[0];   // POKE 712,C
  color1.Brush.Color := colTab[1];   // POKE 708,C
  color2.Brush.Color := colTab[2];   // POKE 709,C
  color3.Brush.Color := colTab[3];   // POKE 710,C
  color4.Brush.Color := colTab[10];  // POKE 711,C

  chkCharSet.Checked := frmAntic4.fontName <> '';

  langIndex := 0;
  listExamples.ListBox.ItemIndex := 0;
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
  if listExamples.ListBox.ItemIndex < 0 then exit;

  btnAtariBASIC.Enabled := listings[listExamples.ListBox.ItemIndex, 0];
  btnTurboBasicXL.Enabled := listings[listExamples.ListBox.ItemIndex, 1];
  btnMadPascal.Enabled := listings[listExamples.ListBox.ItemIndex, 2];
  btnEffectus.Enabled := listings[listExamples.ListBox.ItemIndex, 3];
  btnFastBasic.Enabled := listings[listExamples.ListBox.ItemIndex, 4];
  btnKickC.Enabled := listings[listExamples.ListBox.ItemIndex, 8];
//  btnMads.Enabled := listings[listExamples.ListBox.ItemIndex, 6];
//  btnCC65.Enabled := listings[listExamples.ListBox.ItemIndex, 7];

  boxScreen.Enabled := listExamples.ListBox.ItemIndex >= 3;
  boxScreen.Visible := boxScreen.Enabled;

  boxColors.Enabled := listExamples.ListBox.ItemIndex = 3;
  boxColors.Visible := boxColors.Enabled;

  boxFiles.Enabled := listExamples.ListBox.ItemIndex >= 3;
  boxFiles.Visible := boxFiles.Enabled;

  editFilename.Enabled := listExamples.ListBox.ItemIndex = 4;
  editFilename.Visible := editFilename.Enabled;

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

procedure TfrmAntic4Gen.chkCharSetChange(Sender : TObject);
begin
  editFontName.Enabled := chkCharSet.Checked;
end;

procedure TfrmAntic4Gen.CreateCode;
var
  code : string;
begin
  //boxStartLine.Enabled := langIndex < 2;
  //boxStartLine.Visible := boxStartLine.Enabled;
  //
  //if langIndex = 0 then begin
  //  radDataType.ItemIndex := 0;
  //  TRadioButton(radDataType.Controls[1]).Enabled := false;
  //  TRadioButton(radDataType.Controls[2]).Enabled := false;
  //end
  //else begin
  //  TRadioButton(radDataType.Controls[1]).Enabled := true;
  //  TRadioButton(radDataType.Controls[2]).Enabled := true;
  //end;

  Set01(boxStartLine, langIndex, radDataType, true);

  isMaxSize := (editMaxX.Value = 39) and (editMaxY.Value = 23);
  chkMaxSize.Checked := isMaxSize;

//  case listExamples.selected.AbsoluteIndex of
  case ListExamples.ListBox.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
    3: code := Example04;
    4: code := Example05;
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

{-----------------------------------------------------------------------------
 Data values for selected character
 -----------------------------------------------------------------------------}
function TfrmAntic4Gen.Example01 : string;
begin
  isConstData := false;

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
  isConstData := false;

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex = _ATARI_BASIC) or (langIndex = _TURBO_BASIC_XL) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    for i := 0 to 127 do
      if frmAntic4.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
        code.line += CodeLine('DATA ' + SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                              i, radDataType.ItemIndex, ','));
      end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then begin
       code.line += '; Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) +
                    #13#10'BYTE ARRAY char' + IntToStr(i) + '=[' +
                    SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                  i, radDataType.ItemIndex, ' ') + ']'#13#10;
     end;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
   code.line := 'const'#13#10;
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then begin
       code.line += '  // Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) +
                    #13#10'  char' + IntToStr(i) + ' : array[0..7] of byte = (' +
                    SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                  i, radDataType.ItemIndex, ', ') + ');'#13#10;
     end;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
   for i := 0 to 127 do
     if frmAntic4.charEditIndex[i] = 1 then begin
       code.line += ''' Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) +
                    #13#10'DATA char' + IntToStr(i) + '() BYTE = ' +
                    SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                  i, radDataType.ItemIndex, ', ') + #13#10;
     //         SetValues(_FAST_BASIC, i)
     end;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    for i := 0 to 127 do begin
      if frmAntic4.charEditIndex[i] = 1 then
        code.line += '// Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) +
                     #13#10'const char char' + IntToStr(i) + '[] = {' +
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
  isConstData := false;

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
 Display Antic 4/5 screen (with modified characters)
 -----------------------------------------------------------------------------}
function TfrmAntic4Gen.Example04 : string;
var
  i : byte;
  strTextWindow : string = '';
  grMode : string;
  maxSize : word;
  rtnCodeNum : word = 20000;       // ML starting Atari BASIC line number
  charDataCodeNum : word = 30000;  // Character data starting Atari BASIC line number
begin
  grMode := IntToStr(frmAntic4.anticMode + 8);
  maxSize := (editMaxX.Value + 1)*(editMaxY.Value + 1) - 1;
  if not chkTextWindow.Checked then
    strTextWindow := '+16';

  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;

    if code.number > 20000 then
      Inc(rtnCodeNum, 1000);

    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Display ANTIC mode ' + IntToStr(frmAntic4.anticMode) + ' screen');

    if isModChar then
      code.line += CodeLine('REM with modified characters');

    code.line += CodeLine(_REM);

    if isModChar and not chkCharSet.Checked then begin
      code.line += CodeLine('DIM MLCODE$(33)');
      code.line += CodeLine('GOSUB ' + IntToStr(rtnCodeNum));
    end;

    if isModChar or chkCharSet.Checked then
      code.line += CodeLine('TOPMEM=PEEK(106)-8') +
                   CodeLine('POKE 106,TOPMEM') +
                   CodeLine('CHRAM=TOPMEM*256');

    code.line += CodeLine('GRAPHICS ' + grMode + strTextWindow);

    if isModChar and not chkCharSet.Checked then begin
      code.line += CodeLine('REM Call machine language routine');
      code.line += CodeLine('X=USR(ADR(MLCODE$),57344,CHRAM,4)');
    end
    else if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, false);

    if isModChar then begin
      if code.number > 30000 then
        Inc(charDataCodeNum, 1000);

      // Modify characters
      code.line += CodeLine('RESTORE ' + IntToStr(charDataCodeNum));
      code.line += CodeLine('REM Modify characters');
      for i := 0 to 127 do begin
        if frmAntic4.charEditIndex[i] = 1 then
          code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE CHRAM+I+' + IntToStr(i) + '*8,CHAR:NEXT I');
      end;
    end;

    if isModChar or chkCharSet.Checked then begin
      code.line += CodeLine('REM Modify character set pointer');
      code.line += CodeLine('POKE 756,TOPMEM');
    end;

    code.line += CodeLine(_BASIC_SCR_MEM_VAR + '=PEEK(88)+PEEK(89)*256');
    if chkUseColors.Checked then
      code.line += GenSetColors(_ATARI_BASIC);

    code.line += CodeLine('SIZE=' + IntToStr(maxSize));
    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, false);

    code.line += WaitKeyCode(langIndex);

    if isModChar then begin
      // Machine language routine
      if not chkCharSet.Checked then begin
        code.number := rtnCodeNum;
        code.line += SetFastCopyRoutine;
      end;

      // Modified characters
      code.number := charDataCodeNum;
      code.line += CodeLine('REM Modified character data');
      for i := 0 to 127 do
        if frmAntic4.charEditIndex[i] = 1 then begin
          code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
          code.line += CodeLine('DATA ' + SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                                        i, radDataType.ItemIndex, ','));
        end;
    end;

    // Screen data
    code.line += CodeLine('REM Screen data');
    code.line += DataValues(_ATARI_BASIC, frmAntic4.fldAtascii, code.number, code.step,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;

    code.line := CodeLine('--') +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM DISPLAY ANTIC MODE ' + IntToStr(frmAntic4.anticMode) + ' SCREEN');
    if isModChar then
      code.line += CodeLine('REM WITH MODIFIED CHARACTERS');

    code.line += CodeLine('--');

    if isModChar and not chkCharSet.Checked then
      code.line += CodeLine('DIM MLCODE$(33)') +
                   CodeLine('GOSUB ' + IntToStr(rtnCodeNum));

    if isModChar or chkCharSet.Checked then
      code.line += CodeLine('TOPMEM=PEEK(106)-8') +
                   CodeLine('POKE 106,TOPMEM') +
                   CodeLine('CHRAM=TOPMEM*256');

    code.line += CodeLine('GRAPHICS ' + grMode + strTextWindow);

    if isModChar and not chkCharSet.Checked then
      code.line += CodeLine('MOVE 57344,CHRAM,1024')
    else if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, false);

    if isModChar then begin
      if code.number > 30000 then
        Inc(charDataCodeNum, 1000);

      // Modify characters
      code.line += CodeLine('RESTORE ' + IntToStr(charDataCodeNum));
      code.line += CodeLine('REM MODIFY CHARACTERS');
      for i := 0 to 127 do begin
        if frmAntic4.charEditIndex[i] = 1 then
          code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE CHRAM+I+' + IntToStr(i) + '*8,CHAR:NEXT I');
      end;
    end;

    if isModChar or chkCharSet.Checked then
      code.line += CodeLine('REM MODIFY CHARACTER SET POINTER') +
                   CodeLine('POKE 756,TOPMEM');

    code.line += CodeLine(_BASIC_SCR_MEM_VAR + '=DPEEK(88)');
    if chkUseColors.Checked then
      code.line += GenSetColors(_TURBO_BASIC_XL);

    code.line += CodeLine('SIZE=' + IntToStr(maxSize));
    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, false);

    code.line += WaitKeyCode(langIndex);

    if isModChar then begin
      // Modified characters
      code.number := charDataCodeNum;
      code.line += CodeLine('REM MODIFIED CHARACTER DATA');
      for i := 0 to 127 do
        if frmAntic4.charEditIndex[i] = 1 then begin
          code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
          code.line += CodeLine('DATA ' + SetDataValues(
                                frmAntic4.fldChar, frmAntic4.fldFontSet, i, radDataType.ItemIndex, ','));
        end;
    end;

    // Screen data
    code.line += CodeLine('REM SCREEN DATA');
    code.line += DataValues(_TURBO_BASIC_XL, frmAntic4.fldAtascii, code.number, code.step,
                            editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := ';' + _REM_MAD_STUDIO + #13#10 +
                 '; Modified characters in Antic mode ' + anticMode + #13#10#13#10;
    if isModChar then begin
      for i := 0 to 127 do begin
        if frmAntic4.charEditIndex[i] = 1 then
          code.line += '; Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) +
                       #13#10'BYTE ARRAY char' + IntToStr(i) + '=[' +
                       SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                     i, radDataType.ItemIndex, ' ') + ']'#13#10;
      end;
      code.line += #13#10;
    end;
    code.line += DataValues(_ACTION, frmAntic4.fldAtascii, editMaxY.Value, editMaxX.Value,
                                     radDataType.ItemIndex);
    code.line += #13#10'; Screen memory address'#13#10 +
                 'CARD SCREEN=88'#13#10#13#10 +
                 '; The size of the screen'#13#10 +
                 'CARD size=[' + IntToStr(maxSize) + ']'#13#10;
    if not isMaxSize or chkCharSet.Checked then
      code.line += 'CARD i'#13#10;

    if not isMaxSize then
      code.line += 'CARD cnt=[0]'#13#10;

    if chkCharSet.Checked then
      code.line += '; Character set supporting variables'#13#10 +
                   'BYTE data'#13#10 +
                   'BYTE ARRAY FONT(1023)'#13#10;

    code.line += '; Keyboard code of last key pressed'#13#10 +
                 'BYTE CH=$2FC'#13#10#13#10;
    if isModChar or chkCharSet.Checked then
      code.line += 'BYTE RAMTOP=$6A'#13#10 +
                   'BYTE CHBAS=$2F4'#13#10 +
                   'CARD TOPMEM, CHRAM'#13#10#13#10;

    code.line += 'PROC Main()'#13#10#13#10;

    if isModChar or chkCharSet.Checked then
      code.line += '; Reserve memory for new character set'#13#10 +
                   'TOPMEM=RAMTOP-12'#13#10 +
                   'CHRAM=TOPMEM LSH 8'#13#10#13#10;

    code.line += 'Graphics(' + grMode + strTextWindow + ')'#13#10;
    code.line += 'screen=PeekC(88)'#13#10;

    if isModChar and not chkCharSet.Checked then
      code.line += '; Move new character set to reserved memory'#13#10 +
                   'MoveBlock(CHRAM,57344,1024)'#13#10#13#10
    else if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, false);

    if isModChar or chkCharSet.Checked then
      code.line += '; New character set page address'#13#10 +
                   'chBas=TOPMEM'#13#10#13#10;

    if isModChar then begin
      for i := 0 to 127 do
        if frmAntic4.charEditIndex[i] = 1 then
          code.line += 'MoveBlock(CHRAM+' + IntToStr(i) + '*8,char' + IntToStr(i) + ',8)'#13#10;
    end;

    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, false);

    if chkUseColors.Checked then
      code.line += GenSetColors(_ACTION);

    code.line += WaitKeyCode(langIndex) + #13#10 +
                 'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Modified characters in Antic mode ' + anticMode + #13#10#13#10 +
                 'uses'#13#10 +
                 '  FastGraph, Crt';
    if chkCharSet.Checked then
      code.line += ', Cio';

    code.line += ';'#13#10#13#10;

    code.line += 'const'#13#10 +
                 '  size : word = ' + IntToStr(maxSize) + ';'#13#10#13#10;

    code.line += 'var'#13#10 +
                 '  screen : word absolute 88;'#13#10;
    if isModChar or chkCharSet.Checked then
      code.line += '  topMem, chRAM : word;'#13#10 +
                   '  CHBAS  : byte absolute $2F4;'#13#10 +
                   '  RAMTOP : byte absolute $6A;'#13#10;

    if not isMaxSize then
      code.line += '  i : word;'#13#10 +
                   '  cnt : word = 0;'#13#10;

    if isModChar then begin
      for i := 0 to 127 do begin
        if frmAntic4.charEditIndex[i] = 1 then
          code.line += #13#10'  // Internal code: ' + IntToStr(i) + ' Atascii code: ' +
                       AtasciiCode(i) + #13#10 +
                       '  char' + IntToStr(i) + ' : array[0..7] of byte = (' +
                       SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                     i, radDataType.ItemIndex, ', ') + ');'#13#10;
      end;
      code.line += #13#10;
    end;
    code.line += DataValues(_MAD_PASCAL, frmAntic4.fldAtascii,
                 editMaxY.Value, editMaxX.Value, radDataType.ItemIndex) + #13#10 +
                 'begin'#13#10;

    if isModChar or chkCharSet.Checked then
      code.line += '  // Set new character set'#13#10 +
                   '  topMem := RAMTOP - 12;'#13#10 +
                   '  chRAM := topMem shl 8;'#13#10;

    code.line += '  InitGraph(' + grMode + strTextWindow + ');'#13#10#13#10;

    if isModChar and not chkCharSet.Checked then
      //'  CHBAS := hi(topMem);'#13#10#13#10 +
      code.line += '  // Move new character set to reserved memory'#13#10 +
                   '  Move(pointer(57344), pointer(chRAM), 1024);'#13#10#13#10
    else if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, false);

    if isModChar or chkCharSet.Checked then
      code.line += '  CHBAS := topMem;'#13#10;

    if isModChar then begin
      for i := 0 to 127 do begin
        if frmAntic4.charEditIndex[i] = 1 then
          code.line += '  Move(char' + IntToStr(i) + ', pointer(chRam + ' + IntToStr(i) + '*8), 8);'#13#10;
      end;
      code.line += #13#10;
    end;

    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, false);

    if chkUseColors.Checked then
      code.line += GenSetColors(_MAD_PASCAL);

    code.line += WaitKeyCode(langIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
   else if langIndex = _FAST_BASIC then begin
     code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                  ''' Modified characters in Antic mode ' + anticMode + #13#10#13#10;
     if isModChar then begin
       for i := 0 to 127 do begin
         if frmAntic4.charEditIndex[i] = 1 then
           code.line += ''' Internal code: ' + IntToStr(i) + ' Atascii code: ' + AtasciiCode(i) +
                        #13#10'DATA char' + IntToStr(i) + '() BYTE = ' +
                        SetDataValues(frmAntic4.fldChar, frmAntic4.fldFontSet,
                                      i, radDataType.ItemIndex, ', ') + #13#10;
       end;
       code.line += #13#10;
     end;

     code.line += DataValues(_FAST_BASIC, frmAntic4.fldAtascii,
                  editMaxY.Value, editMaxX.Value, radDataType.ItemIndex);
     code.line += #13#10;

     if isModChar or chkCharSet.Checked then
       code.line += 'TOPMEM = PEEK(106) - 8'#13#10 +
                    'POKE 106, TOPMEM'#13#10 +
                    'CHRAM = TOPMEM*256'#13#10;

     code.line += 'GRAPHICS ' + grMode + strTextWindow + #13#10 +
                  'screen = DPEEK(88)'#13#10;

     if isModChar and not chkCharSet.Checked then
       code.line += 'MOVE 57344, CHRAM, 1024'#13#10#13#10
     else if chkCharSet.Checked then
       code.line += SetCharSet(langIndex, editFontname.Text, false);

     if isModChar or chkCharSet.Checked then
       code.line += ''' Modify character set pointer'#13#10 +
                    'POKE 756, TOPMEM'#13#10;

     if isModChar then begin
        code.line += ''' Custom character set data'#13#10;
        for i := 0 to 127 do begin
          if frmAntic4.charEditIndex[i] = 1 then
    //        code += 'MOVE CHRAM + ' + IntToStr(i) + '*8, ADR(char' + IntToStr(i) + '), 8'#13#10;
            code.line += 'MOVE ADR(char' + IntToStr(i) + '), CHRAM + ' + IntToStr(i) + '*8, 8'#13#10;
        end;
    end;

    code.line += 'SIZE = ' + IntToStr(maxSize) + #13#10;
    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, false);

    if chkUseColors.Checked then
      code.line += GenSetColors(_FAST_BASIC);

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
function TfrmAntic4Gen.Example05 : string;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM LOAD ANTIC MODE ' + anticMode + ' SCREEN') +
                 CodeLine(_REM);

    if chkCharSet.Checked then begin
      code.line += CodeLine('TOPMEM=PEEK(106)-8');
      code.line += CodeLine('POKE 106,TOPMEM');
    end;

    code.line += CodeLine('GRAPHICS ' + IntToStr(frmAntic4.anticMode + 8) + '+16');

    code.line += CodeLine('CLOSE #1');
    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += CodeLine('OPEN #1,4,0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                 CodeLine('REM SCREEN DIM. VALUES') +
                 CodeLine('GET #1,MAXX') +
                 CodeLine('GET #1,MAXY') +
                 CodeLine('SIZE=(MAXX+1)*(MAXY+1)-1') +
                 CodeLine('REM COLOR VALUES') +
                 CodeLine('GET #1,COLOR4') +
                 CodeLine('GET #1,COLOR0:GET #1,COLOR1') +
                 CodeLine('GET #1,COLOR2:GET #1,COLOR3') +
                 CodeLine('POKE 712,COLOR4') +
                 CodeLine('POKE 708,COLOR0:POKE 709,COLOR1') +
                 CodeLine('POKE 710,COLOR2:POKE 711,COLOR3');

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
                 CodeLine('REM LOAD ANTIC MODE ' + anticMode + ' SCREEN') +
                 CodeLine('--');

    if chkCharSet.Checked then
      code.line += CodeLine('TOPMEM=PEEK(106)-8') +
                   CodeLine('POKE 106,TOPMEM') +
                   CodeLine('CHRAM=TOPMEM*256');

    code.line += CodeLine('GRAPHICS ' + IntToStr(frmAntic4.anticMode + 8) + '+16');
    code.line += CodeLine('CLOSE #%1');

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += CodeLine('OPEN #%1,4,%0,"' + editFilename.Text + '"') +
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('REM SCREEN DIM. VALUES') +
                 CodeLine('GET #%1,MAXX') +
                 CodeLine('GET #%1,MAXY') +
                 CodeLine('SIZE=(MAXX+%1)*(MAXY+%1)-%1') +
                 CodeLine('REM COLOR VALUES') +
                 CodeLine('GET #%1,COLOR4') +
                 CodeLine('GET #%1,COLOR0:GET #%1,COLOR1') +
                 CodeLine('GET #%1,COLOR2:GET #%1,COLOR3') +
                 CodeLine('POKE 712,COLOR4') +
                 CodeLine('POKE 708,COLOR0:POKE 709,COLOR1') +
                 CodeLine('POKE 710,COLOR2:POKE 711,COLOR3');
    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, true);
    code.line += CodeLine('CLOSE #%1');
    code.line += WaitKeyCode(langIndex);
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Load ANTIC mode ' + anticMode + ' screen'#13#10#13#10 +
                 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt, Cio;'#13#10#13#10 +
                 'var'#13#10 +
                 '  screen : word;'#13#10 +
                 '  maxX, maxY : byte;'#13#10 +
                 '  size : word;'#13#10 +
                 '  colReg : array[0..4] of byte absolute 708;'#13#10;
    if chkCharSet.Checked then
      code.line += '  topMem, chRAM : word;'#13#10 +
                   '  CHBAS  : byte absolute $2F4;'#13#10 +
                   '  RAMTOP : byte absolute $6A;'#13#10;

    if not isMaxSize then
      code.line += '  cnt : byte = 0;'#13#10 +
                   '  i : word;'#13#10 +
                   '  data : byte;'#13#10;

//    code.line += '  i, data : byte;'#13#10#13#10 +
    code.line += 'begin'#13#10;

    if chkCharSet.Checked then
      code.line += '  // Set new character set'#13#10 +
                   '  topMem := RAMTOP - 12;'#13#10 +
                   '  chRAM := topMem shl 8;'#13#10;

    code.line += '  InitGraph(' + IntToStr(frmAntic4.anticMode + 8) + ' + 16);'#13#10 +
                 '  screen := DPeek(88);'#13#10 +
                 '  // Open file'#13#10 +
                 '  Cls(1);'#13#10;

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += '  Opn(1, 4, 0, ' + QuotedStr(editFilename.Text) + ');'#13#10#13#10 +
                 '  // Read screen dimension'#13#10 +
                 '  maxX := Get(1);'#13#10 +
                 '  maxY := Get(1);'#13#10;

    if isMaxSize then
      code.line += '  size := (maxX + 1)*(maxY + 1);'#13#10
    else
      code.line += '  size := (maxX + 1)*(maxY + 1) - 1;'#13#10;

    code.line += #13#10'  // Read color values'#13#10 +
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
                 '; Load ANTIC mode ' + anticMode + ' screen'#13#10 +
                 #13#10'BYTE ch=$2FC'#13#10 +
                 'BYTE data, cnt=[0]'#13#10 +
                 'CARD size'#13#10 +
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

    code.line += 'Graphics(' + IntToStr(frmAntic4.anticMode + 8) + '+16)'#13#10 +
                 'screen=PeekC(88)'#13#10#13#10 +
                 '; Set up channel 2 for screen'#13#10 +
                 'Close(2)'#13#10;

    if chkCharSet.Checked then
      code.line += SetCharSet(langIndex, editFontname.Text, true);

    code.line += 'Open(2,"' + editFilename.Text + '",4,0)'#13#10#13#10 +
                 '; Screen dimension values'#13#10 +
                 'maxX=GetD(2)'#13#10 +
                 'maxY=GetD(2)'#13#10 +
                 'size=(maxX+1)*(maxY+1)-1'#13#10#13#10 +
                 '; Color values'#13#10 +
                 'colReg(4)=GetD(2)'#13#10 +
                 'colReg(0)=GetD(2)'#13#10 +
                 'colReg(1)=GetD(2)'#13#10 +
                 'colReg(2)=GetD(2)'#13#10 +
                 'colReg(3)=GetD(2)'#13#10 +
                 'Poke(712, colReg(4))'#13#10 +
                 'Poke(708, colReg(0))'#13#10 +
                 'Poke(709, colReg(1))'#13#10 +
                 'Poke(710, colReg(2))'#13#10 +
                 'Poke(711, colReg(3))'#13#10#13#10 +
                 '; Screen data'#13#10;
    code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, true);
    code.line += '; Close channel 2'#13#10 +
                 'Close(2)'#13#10#13#10;
    code.line += WaitKeyCode(langIndex) + #13#10 +
                 'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
   else if langIndex = _FAST_BASIC then begin
     code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                  ''' Load ANTIC mode ' + anticMode + ' screen'#13#10#13#10;

     if chkCharSet.Checked then
       code.line += 'TOPMEM = PEEK(106) - 4'#13#10 +
                    'POKE 106, TOPMEM'#13#10 +
                    'CHRAM = TOPMEM*256'#13#10;

     code.line += 'GRAPHICS ' + IntToStr(frmAntic4.anticMode + 8) + ' + 16'#13#10 +
                  'CLOSE #1'#13#10;

     if chkCharSet.Checked then
       code.line += SetCharSet(langIndex, editFontname.Text, true);

     code.line += 'OPEN #1, 4, 0, "' + editFilename.Text + '"'#13#10 +
                  'screen = DPEEK(88)'#13#10#13#10 +
                  ''' Screen dimension values'#13#10 +
                  'GET #1, maxX'#13#10 +
                  'GET #1, maxY'#13#10;

     if isMaxSize then
       code.line += 'size = (maxX + 1)*(maxY + 1)'#13#10
     else
       code.line += 'size = (maxX + 1)*(maxY + 1) - 1'#13#10;

     code.line += #13#10''' Color values'#13#10 +
                  'GET #1, color4'#13#10 +
                  'GET #1, color0: GET #1, color1'#13#10 +
                  'GET #1, color2: GET #1, color3'#13#10 +
                  'POKE 712, color4'#13#10 +
                  'POKE 708, color0: POKE 709, color1'#13#10 +
                  'POKE 710, color2: POKE 711, color3'#13#10#13#10;
     code.line += DisplayScreen(langIndex, editMaxX.Value, editMaxY.Value, true);
     code.line += #13#10'CLOSE #1'#13#10 +
                  WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

procedure TfrmAntic4Gen.LanguageProc(Sender : TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmAntic4Gen.CheckMaxSizeProc(Sender : TObject);
begin
  if chkMaxSize.Checked then begin
    editMaxX.Value := 39;
    editMaxY.Value := 23;
  end;
end;

procedure TfrmAntic4Gen.editCode(Sender : TObject);
begin
  if not isCreate then
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

procedure TfrmAntic4Gen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

