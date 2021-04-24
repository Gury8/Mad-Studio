{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Antic mode 3 editor - source code generator
}
unit antic3_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  lcltype, BCTrackbarUpdown, BCListBox, BCMDButton, BCMaterialDesignButton, Windows,
  common;

type
  { TfrmAntic3Gen }
  TfrmAntic3Gen = class(TForm)
    btnCopyToEditor : TBCMaterialDesignButton;
    btnClose : TBCMaterialDesignButton;
    boxStartLine : TGroupBox;
    btnAtariBASIC : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnKickC : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    editFilename: TLabeledEdit;
    editLineStep : TBCTrackbarUpdown;
    editStartLine : TBCTrackbarUpdown;
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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditorProc(Sender: TObject);
    procedure listExamplesProc(Sender: TObject);
    procedure radLangProc(Sender: TObject);
    procedure btnCloseMouseEnter(Sender : TObject);
    procedure btnCloseMouseLeave(Sender : TObject);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
    procedure editStartLineMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure CloseWinProc(Sender: TObject);
  private
    { private declarations }
    listings : TListings;
    procedure CreateCode;
    function SetValues(offset : byte; separator : string) : string;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
    function Example04 : string;
  public
    { public declarations }
  end;

var
  frmAntic3Gen: TfrmAntic3Gen;

implementation

{$R *.lfm}

uses
  antic3, src_editor, code_lib, lib;

{ TfrmAntic2Code }

procedure TfrmAntic3Gen.FormCreate(Sender: TObject);
begin
  // Example 1
  listings[0, 0] := true;
  listings[0, 1] := true;
  listings[0, 2] := false;
  listings[0, 3] := false;
  listings[0, 4] := false;

  // Example 2
  listings[1, 0] := true;
  listings[1, 1] := true;
  listings[1, 2] := false;
  listings[1, 3] := false;
  listings[1, 4] := false;

  // Example 3
  listings[2, 0] := true;
  listings[2, 1] := true;
  listings[2, 2] := false;
  listings[2, 3] := false;
  listings[2, 4] := false;

  // Example 4
  listings[3, 0] := true;
  listings[3, 1] := true;
  listings[3, 2] := false;
  listings[3, 3] := false;
  listings[3, 4] := false;

  SetTrackBarUpDown(editStartLine, $00DDDDDD, clWhite);
  SetTrackBarUpDown(editLineStep, $00DDDDDD, clWhite);
end;

procedure TfrmAntic3Gen.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;
//  editFilename.Text:= 'H1:' + ExtractFileName(frmAntic3.filename);

  langIndex := 0;
  ListExamples.ListBox.ItemIndex := 0;
  listExamplesProc(Sender);
end;

procedure TfrmAntic3Gen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmAntic3Gen.CopyToEditorProc(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor(Sender);
  Close;
end;

procedure TfrmAntic3Gen.CreateCode;
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

  // List of listing examples
  case ListExamples.ListBox.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
    3: code := Example04;
  end;

  memo.Lines.Add(code);

  // Set cursor position at the top of memo object
  memo.SelStart := 0;
  memo.SelLength := 0;
  SendMessage(memo.Handle, EM_SCROLLCARET, 0, 0);
end;

function TfrmAntic3Gen.SetValues(offset : byte; separator : string) : string;
var
  x, y : byte;
  line, values : string;
begin
  for y := 0 to 7 do begin
    line := '';
    for x := 0 to 7 do begin
      if offset = 255 then
        line += IntToStr(frmAntic3.fldChar[x, y])
      else begin
        if offset < 96 then
          line += IntToStr(frmAntic3.fld[x, y + offset*10])
        else begin
          if y = 0 then
            line += IntToStr(frmAntic3.fld[x, offset*10 + 8])
          else if y = 1 then
            line += IntToStr(frmAntic3.fld[x, offset*10 + 9])
          else
            line += IntToStr(frmAntic3.fld[x, y + offset*10]);
        end;
      end;
    end;
//    if offset = 98 then
//      showmessage(inttostr(y) + ' * ' + line);
    case radDataType.ItemIndex of
      0: line := IntToStr(Bin2Dec(line));
      1: line := '$' + Dec2Hex(bin2Dec(line));
      2: line := '%' + line;
    end;
    if y = 0 then values := line;
    if Trim(line) = '$' then line := '0';
    if Trim(values) = '$' then values := '0';
    if y > 0 then begin
//      if index = _ACTION then
//        values += ' ' + line
//      else
        values += separator + line;
    end;
  end;

  result := values;
end;

function TfrmAntic3Gen.Example01 : string;
begin
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS 0') +
                 CodeLine('REM Turn off TV display') +
                 CodeLine('POKE 559,0') +
                 CodeLine('REM Find start of display list') +
                 CodeLine('DL=PEEK(560)+256*PEEK(561)') +
                 CodeLine('REM Modify display list to ANTIC mode 3') +
                 CodeLine('POKE DL+3,67') +
                 CodeLine('FOR I=6 TO 24:POKE DL+I,3:NEXT I') +
                 CodeLine('POKE DL+25,65') +
                 CodeLine('POKE DL+26,PEEK(DL+30)') +
                 CodeLine('POKE DL+27,PEEK(DL+31)') +
                 CodeLine('REM Turn on TV display') +
                 CodeLine('POKE 559,34');
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('GRAPHICS %0') +
                 CodeLine('REM Turn off TV display') +
                 CodeLine('POKE 559,%0') +
                 CodeLine('REM Find start of display list') +
                 CodeLine('DL=DPEEK(560)') +
                 CodeLine('REM Modify display list to ANTIC mode 3') +
                 CodeLine('POKE DL+%3,67') +
                 CodeLine('FOR I=6 TO 24:POKE DL+I,%3:NEXT I') +
                 CodeLine('POKE DL+25,65') +
                 CodeLine('POKE DL+26,PEEK(DL+30)') +
                 CodeLine('POKE DL+27,PEEK(DL+31)') +
                 CodeLine('REM Turn on TV display') +
                 CodeLine('POKE 559,34');
  end;

  result := code.line;
end;

function TfrmAntic3Gen.Example02 : string;
var
  i : byte;
  modeSize : word;
  modeHeight : byte;
begin
  modeSize := 40*20;
  modeHeight := 20;
  { Atari BASIC
   ---------------------------------------------------------------------------}
  if langIndex = _ATARI_BASIC then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM Reserve 4 pages of RAM for character set') +
                 CodeLine('MEM=PEEK(106)-4:POKE 106,MEM-1:RAMSTART=256*MEM') +
                 CodeLine('GRAPHICS 0') +
                 CodeLine('REM Turn off TV display') +
                 CodeLine('POKE 559,0') +
                 CodeLine('REM Find start of display list') +
                 CodeLine('DL=PEEK(560)+256*PEEK(561)') +
                 CodeLine('REM Modify display list to ANTIC mode 3') +
                 CodeLine('POKE DL+3,67') +
                 CodeLine('FOR I=6 TO 24:POKE DL+I,3:NEXT I') +
                 CodeLine('POKE DL+25,65') +
                 CodeLine('POKE DL+26,PEEK(DL+30)') +
                 CodeLine('POKE DL+27,PEEK(DL+31)') +
                 CodeLine('REM Load ML routine') +
                 CodeLine('FOR I=1 TO 35:READ A:POKE 1535+I,A:NEXT I') +
                 CodeLine('DATA 104,160,255,162,7,177,203,72,136,177,203,200,145,205') +
                 CodeLine('DATA 136,202,208,246,104,145,205,136,192') +
                 CodeLine('DATA 255,208,233,198,206,198,204,198,207,208,223,96') +
                 CodeLine('REM Initialize work variables for character set Transfer to RAM') +
                 CodeLine('POKE 203,0:POKE 204,227') +
                 CodeLine('POKE 205,0:POKE 206,MEM+3:POKE 207,4') +
                 CodeLine('REM Call ML routine to move character set') +
                 CodeLine('A=USR(1536)');
//    lineNum := 290;
//    code.number := 290;
    for i := 0 to 127 do
      if frmAntic3.charEditIndex[i] = 1 then
        code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+' +
                     IntToStr(i) + '*8,CHAR:NEXT I');

//    Inc(code.number, 10);
    code.line += CodeLine('REM MODIFY CHARACTER SET POINTER') +
                 CodeLine('POKE 756,MEM') +
                 CodeLine('REM MODIFIED CHARACTER DATA') +
                 CodeLine('REM Turn on TV display') +
                 CodeLine('POKE 559,34') +
//    Inc(code.number, 40);
                 CodeLine('SCR=PEEK(88)+PEEK(89)*256') +
                 CodeLine('FOR I=0 TO ' + IntToStr(modeSize - 1)) +
                 CodeLine('READ BYTE:POKE SCR+I,BYTE') +
                 CodeLine('NEXT I');

    // Modified characters
    code.line += CodeLine('REM MODIFIED CHARACTERS');
    for i := 0 to 127 do
      if frmAntic3.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
        code.line += CodeLine('DATA ' + SetValues(i, ','));
//        Inc(lineNum, 10);
      end;

    // Screen data
    code.line += CodeLine('REM SCREEN DATA');
//    Inc(code.number, 10);
    code.line += DataValues(_ATARI_BASIC, frmAntic3.fldAtascii, code.number, code.step,
                            modeHeight, 40, radDataType.ItemIndex);

//    lineNum := 80;
//    code += WaitKeyCode(radLang.ItemIndex, lineNum);
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM Reserve 4 pages of RAM for character set') +
                 CodeLine('MEM=PEEK(106)-4:POKE 106,MEM-1:RAMSTART=256*MEM') +
                 CodeLine('GRAPHICS %0') +
                 CodeLine('REM Turn off TV display') +
                 CodeLine('POKE 559,%0') +
                 CodeLine('REM Find start of display list') +
                 CodeLine('DL=DPEEK(560)') +
                 CodeLine('REM Modify display list to ANTIC mode 3') +
                 CodeLine('POKE DL+%3,67') +
                 CodeLine('FOR I=6 TO 24:POKE DL+I,%3:NEXT I') +
                 CodeLine('POKE DL+25,65') +
                 CodeLine('POKE DL+26,PEEK(DL+30)') +
                 CodeLine('POKE DL+27,PEEK(DL+31)') +
                 CodeLine('REM Load ML routine') +
                 CodeLine('FOR I=%1 TO 35:READ A:POKE 1535+I,A:NEXT I') +
                 CodeLine('DATA 104,160,255,162,7,177,203,72,136,177,203,200,145,205') +
                 CodeLine('DATA 136,202,208,246,104,145,205,136,192') +
                 CodeLine('DATA 255,208,233,198,206,198,204,198,207,208,223,96') +
                 CodeLine('REM Initialize work variables for character set Transfer to RAM') +
                 CodeLine('POKE 203,%0:POKE 204,227') +
                 CodeLine('POKE 205,%0:POKE 206,MEM+%3:POKE 207,4') +
                 CodeLine('REM Call ML routine to move character set') +
                 CodeLine('A=USR(1536)');
//    code.number := 290;
    for i := 0 to 127 do
      if frmAntic3.charEditIndex[i] = 1 then begin
        code.line += CodeLine('FOR I=%0 TO 7:READ CHAR:POKE RAMSTART+I+' +
                              IntToStr(i) + '*8,CHAR:NEXT I');
        //code += IntToStr(lineNum) + ' FOR I=%0 TO 7'#13#10 +
        //        IntToStr(lineNum + 10) + ' READ CHAR:POKE RAMSTART+I+' + IntToStr(i) + '*8,CHAR'#13#10 +
        //        IntToStr(lineNum + 20) + ' NEXT I'#13#10;
//        Inc(lineNum, 30);
      end;

//    Inc(code.number, 10);
    code.line += CodeLine('REM MODIFY CHARACTER SET POINTER') +
                 CodeLine('POKE 756,MEM') +
                 CodeLine('REM MODIFIED CHARACTER DATA') +
                 CodeLine('REM Turn on TV display') +
                 CodeLine('POKE 559,34') +
    //Inc(code.number, 40);
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('FOR I=%0 TO ' + IntToStr(modeSize - 1)) +
                 CodeLine('READ BYTE:POKE SCR+I,BYTE') +
                 CodeLine('NEXT I');
//    Inc(lineNum, 10);

    // Modified characters
    code.line += CodeLine('REM MODIFIED CHARACTERS');
//    Inc(lineNum, 10);
    for i := 0 to 127 do
      if frmAntic3.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
        code.line += CodeLine('DATA ' + SetValues(i, ','));
      end;

    // Screen data
    code.line += CodeLine('REM SCREEN DATA');
//    Inc(code.number, 10);
    code.line += DataValues(_ATARI_BASIC, frmAntic3.fldAtascii, code.number, code.step,
                            modeHeight, 40, radDataType.ItemIndex);
  end;
  { Mad Pascal
   ---------------------------------------------------------------------------}
//  else if langIndex = _MAD_PASCAL then begin
//  end
  { Action!
   ---------------------------------------------------------------------------}
//  else if langIndex = _ACTION then begin
//  end
  { FastBasic
   ---------------------------------------------------------------------------}
  //else if langIndex = _FAST_BASIC then begin
  //end;

  result := code.line;
end;

function TfrmAntic3Gen.Example03 : string;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex = _ATARI_BASIC) or (langIndex = _TURBO_BASIC_XL) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM Reserve 4 pages of RAM for character set') +
                 CodeLine('MEM=PEEK(106)-4:POKE 106,MEM-1:RAMSTART=256*MEM') +
                 CodeLine('GRAPHICS 0') +
                 CodeLine('REM Turn off TV display') +
                 CodeLine('POKE 559,0') +
                 CodeLine('REM Find start of display list') +
                 CodeLine('DL=PEEK(560)+256*PEEK(561)') +
                 CodeLine('REM Modify display list to ANTIC mode 3') +
                 CodeLine('POKE DL+3,67') +
                 CodeLine('FOR I=6 TO 24:POKE DL+I,3:NEXT I') +
                 CodeLine('POKE DL+25,65') +
                 CodeLine('POKE DL+26,PEEK(DL+30)') +
                 CodeLine('POKE DL+27,PEEK(DL+31)') +
                 CodeLine('REM Load ML routine') +
                 CodeLine('FOR I=1 TO 35:READ A:POKE 1535+I,A:NEXT I') +
                 CodeLine('DATA 104,160,255,162,7,177,203,72,136,177,203,200,145,205') +
                 CodeLine('DATA 136,202,208,246,104,145,205,136,192') +
                 CodeLine('DATA 255,208,233,198,206,198,204,198,207,208,223,96') +
                 CodeLine('REM Initialize work variables for character set Transfer to RAM') +
                 CodeLine('POKE 203,0:POKE 204,227') +
                 CodeLine('POKE 205,0:POKE 206,MEM+3:POKE 207,4') +
                 CodeLine('REM Call ML routine to move character set') +
                 CodeLine('A=USR(1536)') +
                 CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+12*8,CHAR:NEXT I') +
                 CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+27*8,CHAR:NEXT I') +
                 CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+103*8,CHAR:NEXT I') +
                 CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+106*8,CHAR:NEXT I') +
                 CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+112*8,CHAR:NEXT I') +
                 CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+113*8,CHAR:NEXT I') +
                 CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+121*8,CHAR:NEXT I') +
                 CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+96*8,CHAR:NEXT I') +
                 CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+123*8,CHAR:NEXT I') +
                 CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+124*8,CHAR:NEXT I') +
                 CodeLine('REM MODIFY CHARACTER SET POINTER') +
                 CodeLine('POKE 756,MEM') +
                 CodeLine('REM MODIFIED CHARACTER DATA') +
                 CodeLine('REM Turn on TV display') +
                 CodeLine('POKE 559,34') +
                 CodeLine('DATA 0,0,0,0,0,24,24,48') +
                 CodeLine('DATA 0,0,24,24,0,24,24,48') +
                 CodeLine('DATA 102,60,0,62,102,102,62,6') +
                 CodeLine('DATA 6,60,6,0,31,6,6,6') +
                 CodeLine('DATA 96,240,0,124,102,102,124,96') +
                 CodeLine('DATA 6,15,0,62,102,102,62,6') +
                 CodeLine('DATA 24,48,0,102,102,102,62,12') +
                 CodeLine('DATA 48,126,0,0,60,102,12,24') +
                 CodeLine('DATA 102,60,0,0,126,12,24,12') +
                 CodeLine('DATA 126,12,0,0,12,28,60,108') +
                 CodeLine('REM ANTIC 3 text demonstration') +
                 CodeLine('?:? "Characters g,j,p,q,y"') +
                 CodeLine('?:? "Color is yellow."') +
                 CodeLine('? "Is this panther pink?"') +
                 CodeLine('? "There are lots of quotes!"') +
                 CodeLine('? "I found some grapes."') +
                 CodeLine('? "These sentences are jokes."') +
                 CodeLine('? "AG";CHR$(96);"CO";CHR$(123)') +
                 CodeLine('? "NaC";CHR$(96);"H";CHR$(123);"O";CHR$(96)') +
                 CodeLine('? "A12(SO";CHR$(124);")";CHR$(123)');
  end;
  //{ Mad Pascal
  // ---------------------------------------------------------------------------}
  //else if langIndex = _MAD_PASCAL then begin
  //end
  //{ Action!
  // ---------------------------------------------------------------------------}
  //else if langIndex = _ACTION then begin
  //end
  //{ FastBasic
  // ---------------------------------------------------------------------------}
  //else if langIndex = _FAST_BASIC then begin
  //end;

  result := code.line;
end;

// Data values for modified characters
function TfrmAntic3Gen.Example04 : string;
var
  lineNum : word = 10;
  code : string = '';
  i : byte;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex = _ATARI_BASIC) or (langIndex = _TURBO_BASIC_XL) then begin
    for i := 0 to 127 do begin
      if frmAntic3.charEditIndex[i] = 1 then begin
        code += IntToStr(lineNum) + ' REM CHR$(' + AtasciiCode(i) + ')' + #13#10;
        Inc(lineNum, 10);
        code += IntToStr(lineNum) + ' DATA ' + SetValues(i, ',') + #13#10;
        Inc(lineNum, 10);
      end;
    end;
  end;
  //{ Action!
  // ---------------------------------------------------------------------------}
  //else if langIndex = _ACTION then begin
  //end
  //{ Mad Pascal
  // ---------------------------------------------------------------------------}
  //else if langIndex = _MAD_PASCAL then begin
  //end
  //{ FastBasic
  // ---------------------------------------------------------------------------}
  //else if langIndex = _FAST_BASIC then begin
  //end;

  result := code;
end;

procedure TfrmAntic3Gen.listExamplesProc(Sender: TObject);
//var
//  i : byte;
begin
//  for i := 0 to radLang.Items.Count - 1 do
//    radLang.Controls[i].Enabled := listings[ListExamples.ItemIndex, i];
////    radLang.Controls[i].Visible := listings[ListExamples.ItemIndex, i];
//
//  if langIndex < 5 then
//    radLang.ItemIndex := langIndex;

//  editFilename.Visible := ListExamples.ItemIndex = 0;
//  editFilename.Enabled := ListExamples.ItemIndex = 0;
  CreateCode;
end;

procedure TfrmAntic3Gen.radLangProc(Sender: TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmAntic3Gen.editStartLineMouseUp(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  CreateCode;
end;

procedure TfrmAntic3Gen.ButtonHoverEnter(Sender : TObject);
begin
//  btnCopyToEditor.NormalColor := $00CECECE;
//  btnCopyToEditor.NormalColorEffect := clWhite;
  SetButton(btnCopyToEditor, true);
end;

procedure TfrmAntic3Gen.ButtonHoverLeave(Sender : TObject);
begin
//  btnCopyToEditor.NormalColor := clWhite;
//  btnCopyToEditor.NormalColorEffect := clSilver;
  SetButton(btnCopyToEditor, false);
end;

procedure TfrmAntic3Gen.btnCloseMouseEnter(Sender : TObject);
begin
//  btnClose.NormalColor := $00CECECE;
//  btnClose.NormalColorEffect := clWhite;
  SetButton(btnClose, true);
end;

procedure TfrmAntic3Gen.btnCloseMouseLeave(Sender : TObject);
begin
//  btnClose.NormalColor := clWhite;
//  btnClose.NormalColorEffect := clSilver;
  SetButton(btnClose, false);
end;

procedure TfrmAntic3Gen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

