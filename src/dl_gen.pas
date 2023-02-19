{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Display list editor - source code generator
}
unit dl_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lcltype, ExtCtrls, BCListBox, BCMDButton, BCMaterialDesignButton, Windows,
  common;

type
  { TfrmDisplayListGen }
  TfrmDisplayListGen = class(TForm)
    btnCopyToEditor : TBCMaterialDesignButton;
    btnClose : TBCMaterialDesignButton;
    boxStartLine : TGroupBox;
    btnAtariBASIC : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnKickC : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    editAddrHex: TLabel;
    editAddrDec : TSpinEditEx;
    editLineStep : TSpinEditEx;
    box: TGroupBox;
    editHighByte : TSpinEditEx;
    editStartLine : TSpinEditEx;
    editLowByte : TSpinEditEx;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblLineStep : TLabel;
    lblStartLine : TLabel;
    ListExamples : TBCPaperListBox;
    memo : TMemo;
    panelLang : TBCPaperPanel;
    radDataType : TRadioGroup;
    lblLang : TStaticText;
    lblCode : TStaticText;
    lblExamples : TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CloseWin(Sender: TObject);
    procedure CopyToEditor(Sender: TObject);
    procedure editAddrDecChange(Sender: TObject);
    procedure listExamplesProc(Sender: TObject);
    procedure radLangProc(Sender: TObject);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
  private
    { private declarations }
    listings : TListings;
    procedure CreateCode;
    function Example01 : string;
  end;

var
  frmDisplayListGen: TfrmDisplayListGen;

implementation

{$R *.lfm}

uses
  dl_editor, src_editor, lib, code_lib;

{ TfrmDisplayListGen }

procedure TfrmDisplayListGen.FormCreate(Sender: TObject);
begin
  SetListings(listings);

  // Example 1
  listings[0, 8] := false;
end;

procedure TfrmDisplayListGen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;

  langIndex := 0;
  ListExamples.ListBox.ItemIndex := 0;
  editAddrDec.Value := 14000;
  editAddrDecChange(Sender);
end;

procedure TfrmDisplayListGen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmDisplayListGen.CloseWin(Sender: TObject);
begin
  Close;
end;

procedure TfrmDisplayListGen.CopyToEditor(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor(Sender);
  Close;
end;

procedure TfrmDisplayListGen.editAddrDecChange(Sender: TObject);
begin
  //S=57344-SF:HS = INT(S/256):LS=S-HS*256
  editHighByte.Value := editAddrDec.Value div 256;
  editLowByte.Value := editAddrDec.Value - editHighByte.Value*256;
  editAddrHex.Caption := Dec2Hex(editAddrDec.Value);
  CreateCode;
end;

procedure TfrmDisplayListGen.listExamplesProc(Sender: TObject);
begin
  if listExamples.ListBox.ItemIndex < 0 then exit;

  btnAtariBASIC.Enabled := listings[listExamples.ListBox.ItemIndex, 0];
  btnTurboBasicXL.Enabled := listings[listExamples.ListBox.ItemIndex, 1];
  btnMadPascal.Enabled := listings[listExamples.ListBox.ItemIndex, 2];
  btnEffectus.Enabled := listings[listExamples.ListBox.ItemIndex, 3];
  btnFastBasic.Enabled := listings[listExamples.ListBox.ItemIndex, 4];
  btnKickC.Enabled := listings[listExamples.ListBox.ItemIndex, 8];

  CreateCode;
end;

procedure TfrmDisplayListGen.radLangProc(Sender: TObject);
begin
  //langIndex := radLang.ItemIndex;
  //if langIndex = 0 then
  //  rgDataType.ItemIndex := 0;

  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmDisplayListGen.CreateCode;
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

  case ListExamples.ListBox.ItemIndex of
    0: code := Example01;
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
 Display list example
 -----------------------------------------------------------------------------}
function TfrmDisplayListGen.Example01 : string;
var
  i : Integer = 0;
  str : string = '';
  strAction : string = '';
  strBASIC : string = '';
  savmsc, savmsc02 : string;
  dlist : string;
  ch : string;
  LMARGN : string;
begin
  while frmDisplayList.dl[i] <> 65 do begin
    if i = frmDisplayList.LMS_offset then
      frmDisplayList.dl[i] := editLowByte.Value
    else if i = frmDisplayList.LMS_offset + 1 then
      frmDisplayList.dl[i] := editHighByte.Value;

    if radDataType.ItemIndex = 0 then begin
      str += IntToStr(frmDisplayList.dl[i]) + ', ';
      strBASIC := strBASIC + IntToStr(frmDisplayList.dl[i]) + ',';
      strAction := strAction + IntToStr(frmDisplayList.dl[i]) + ' ';
    end
    else begin
      str += '$' + Dec2Hex(frmDisplayList.dl[i]) + ', ';
      strBASIC := strBASIC + '$' + Dec2Hex(frmDisplayList.dl[i]) + ',';
      strAction := strAction + '$' + Dec2Hex(frmDisplayList.dl[i]) + ' ';
    end;
    Inc(i);
  end;
  strBASIC := copy(strBASIC, 1, Length(strBASIC) - 1);
  strAction := copy(strAction, 1, Length(strAction) - 1);

  //S=57344-SF:HS = INT(S/256):LS=S-HS*256
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Display list example') +
                 CodeLine(_REM) +
                 CodeLine('GRAPHICS 0') +
                 CodeLine('DL=PEEK(560)+PEEK(561)*256') +
                 CodeLine('LO=PEEK(DL+4)') +
                 CodeLine('HI=PEEK(DL+5)') +
                 CodeLine('HIDL=INT(DL/256)') +
                 CodeLine('LODL=DL-HIDL*256') +
                 CodeLine('POKE 559,0') +
                 CodeLine('FOR I=0 TO ' + IntToStr(i - 1) + ':READ B:POKE DL+I,B:NEXT I') +
                 CodeLine('POKE DL+' + IntToStr(i) + ',65') +
                 CodeLine('POKE DL+' + IntToStr(i + 1) + ',LODL') +
                 CodeLine('POKE DL+' + IntToStr(i + 2) + ',HIDL') +
                 CodeLine('POKE DL+' + IntToStr(frmDisplayList.LMS_offset) + ',LO') +
                 CodeLine('POKE DL+' + IntToStr(frmDisplayList.LMS_offset + 1) + ',HI') +
                 CodeLine('POKE 559,34') +
                 CodeLine('FOR I=1 TO 25:POKE 82,0:PRINT "DISPLAY LIST EXAMPLE":NEXT I') +
                 CodeLine('DATA ' + strBASIC) +
                 CodeLine('END');
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Display list example') +
                 CodeLine('--') +
                 CodeLine('GRAPHICS %0') +
                 CodeLine('DL=DPEEK(560)') +
                 CodeLine('LO=PEEK(DL+4)') +
                 CodeLine('HI=PEEK(DL+5)') +
                 CodeLine('HIDL=INT(DL/256)') +
                 CodeLine('LODL=DL-HIDL*256') +
                 CodeLine('POKE 559,%0') +
                 CodeLine('FOR I=%0 TO ' + IntToStr(i - 1) + ':READ B:POKE DL+I,B:NEXT I') +
                 CodeLine('POKE DL+' + IntToStr(i) + ',65') +
                 CodeLine('POKE DL+' + IntToStr(i + 1) + ',LODL') +
                 CodeLine('POKE DL+' + IntToStr(i + 2) + ',HIDL') +
                 CodeLine('POKE DL+' + IntToStr(frmDisplayList.LMS_offset) + ',LO') +
                 CodeLine('POKE DL+' + IntToStr(frmDisplayList.LMS_offset + 1) + ',HI') +
                 CodeLine('POKE 559,34') +
                 CodeLine('FOR I=%1 TO 25:POKE 82,%0:PRINT "DISPLAY LIST EXAMPLE":NEXT I') +
                 CodeLine('DATA ' + strBASIC) +
                 CodeLine('END');
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := ';' + _REM_MAD_STUDIO + #13#10 +
                 '; Display list example'#13#10#13#10 +
                 'BYTE ARRAY DL=[' +  //#13#10 +
                 strAction;
    if radDataType.ItemIndex = 0 then
      code.line += ' 65]'
    else
      code.line += ' $41]';

    if radDataType.ItemIndex = 0 then begin
      savmsc := editAddrDec.Text;
      savmsc02 := '88';
//      nmien := '54286';
      dlist := '560';
      ch := '764';
      LMARGN := '82';
    end
    else begin
      savmsc := '$' + editAddrHex.Caption;
      savmsc02 := '$58';
//      nmien := '$D40E';
      dlist := '$230';
      ch := '$2FC';
      LMARGN := '$52';
    end;
    code.line += #13#10'CARD DLIST=' + dlist + #13#10 +
                 'BYTE CH=' + ch + #13#10 +
                 'BYTE I,MLO,MHI'#13#10#13#10 +
                 'PROC MAIN()'#13#10#13#10 +
                 'GRAPHICS(0)'#13#10 +
                 'MLO=PEEK(DLIST+4)'#13#10 +
                 'MHI=PEEK(DLIST+5)'#13#10 +
                 'DLIST=DL'#13#10 +
             //    'nmien=$c0'#13#10 +
                 'POKE(DLIST+' + IntToStr(frmDisplayList.LMS_offset) + ',MLO)'#13#10 +
                 'POKE(DLIST+' + IntToStr(frmDisplayList.LMS_offset + 1) + ',MHI)'#13#10 +
             //    'SAVMSC=' + savmsc + #13#10 +
                 #13#10 +
                 'FOR I=1 TO 25 DO'#13#10 +
                 '  POKE(' + LMARGN + ',0)'#13#10 +
                 '  PRINTE("Display list example")'#13#10 +
                 'OD'#13#10 +
                 WaitKeyCode(langIndex) +
                 #13#10'RETURN';
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Display list example'#13#10#13#10 +
                 'uses crt, graph;'#13#10#13#10 +
                 'const'#13#10 +
                 '  dl : array [0..' + IntToStr(i + 2) + '] of byte = ('#13#10 +
                 '    ' + str;
    if radDataType.ItemIndex = 0 then
      code.line += '    65, lo(word(@dl)), hi(word(@dl)));'
    else
      code.line += '    $41, lo(word(@dl)), hi(word(@dl)));';

    if radDataType.ItemIndex = 0 then begin
      savmsc := editAddrDec.Text;
      savmsc02 := '88';
  //    nmien := '54286';
      dlist := '560';
      LMARGN := '82';
    end
    else begin
      savmsc := '$' + editAddrHex.Caption;
      savmsc02 := '$58';
  //    nmien := '$D40E';
      dlist := '$230';
      LMARGN := '$52';
    end;
    code.line += #13#10'var'#13#10 +
                 '  savmsc : word absolute ' + savmsc02 + ';'#13#10 +
                 //    '  nmien : byte absolute ' + nmien + ';'#13#10 +
                 '  dlist : word absolute ' + dlist + ';'#13#10 +
                 '  i : byte;'#13#10#13#10 +
                 'begin'#13#10 +
                 '  InitGraph(0);'#13#10 +
                 '  dlist := word(@dl);'#13#10 +
                 //    '  nmien := $c0;'#13#10 +
                 '  savmsc := ' + savmsc + ';'#13#10#13#10 +
                 '  for i := 1 to 25 do begin'#13#10 +
                 '    Poke(' + LMARGN + ', 0);'#13#10 +
                 '    writeln(''Display list example'');'#13#10 +
                 '  end;'#13#10 +
                 WaitKeyCode(langIndex) +
                 'end.';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                 ''' Display list example'#13#10#13#10 +
                 'DATA dlData() BYTE = ' + strBASIC + #13#10#13#10 +
                 'GRAPHICS 0'#13#10 +
                 'dl = DPEEK(560)'#13#10 +
                 'lo = PEEK(dl + 4)'#13#10 +
                 'hi = PEEK(dl + 5)'#13#10 +
                 'hidl = INT(dl/256)'#13#10 +
                 'lodl = dl - hidl*256'#13#10#13#10 +
                 'POKE 559, 0'#13#10#13#10 +
                 'FOR i = 0 TO ' + IntToStr(i - 1) + #13#10 +
                 '  POKE dl + i, dlData(i)'#13#10 +
                 'NEXT'#13#10#13#10 +
                 'POKE dl + ' + IntToStr(i) + ', 65'#13#10 +
                 'POKE dl + ' + IntToStr(i + 1) + ', lodl'#13#10 +
                 'POKE dl + ' + IntToStr(i + 2) + ', hidl'#13#10 +
                 'POKE dl + ' + IntToStr(frmDisplayList.LMS_offset) + ', lo'#13#10 +
                 'POKE dl + ' + IntToStr(frmDisplayList.LMS_offset + 1) + ', hi'#13#10#13#10 +
                 'POKE 559, 34'#13#10#13#10 +
                 'FOR i = 1 TO 25'#13#10 +
                 '  POKE 82, 0'#13#10 +
                 '  PRINT "Display list example"'#13#10 +
                 'NEXT'#13#10 +
                 WaitKeyCode(langIndex);
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

procedure TfrmDisplayListGen.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmDisplayListGen.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

(*
1 GRAPHICS 0
10 DL=PEEK(560)+PEEK(561)*256
20 FOR I=0 TO 32:? PEEK(DL+I);"*";
30 NEXT I
*)
end.

