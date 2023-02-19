{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Antic mode 3 editor - source code generator
}
unit antic3_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, lcltype, BCListBox, BCMDButton, BCMaterialDesignButton,
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
    editLineStep : TSpinEditEx;
    editStartLine : TSpinEditEx;
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
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
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
  SetListings(listings);

  // Example 1
  listings[0, 2] := false;
  listings[0, 3] := false;
  listings[0, 4] := false;

  // Example 2
  listings[1, 2] := false;
  listings[1, 3] := false;
  listings[1, 4] := false;

  // Example 3
  listings[2, 2] := false;
  listings[2, 3] := false;
  listings[2, 4] := false;

  // Example 4
  listings[3, 2] := false;
  listings[3, 3] := false;
  listings[3, 4] := false;
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
  Set01(boxStartLine, langIndex, radDataType, true);

  // List of listing examples
  case ListExamples.ListBox.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
    3: code := Example04;
  end;

  Set02(memo, code);
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

    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Default Antic mode 3 screen') +
                 CodeLine(_REM) +
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
                 CodeLine('REM Turn on TV display') +
                 CodeLine('POKE 559,34');
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;

    code.line := CodeLine('--') +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Default Antic mode 3 screen') +
                 CodeLine('--') +
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
    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Antic mode 3 screen with modified characters') +
                 CodeLine(_REM) +
                 CodeLine('REM Reserve 4 pages of RAM for character set') +
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
    for i := 0 to 127 do
      if frmAntic3.charEditIndex[i] = 1 then
        code.line += CodeLine('FOR I=0 TO 7:READ CHAR:POKE RAMSTART+I+' +
                     IntToStr(i) + '*8,CHAR:NEXT I');

    code.line += CodeLine('REM MODIFY CHARACTER SET POINTER') +
                 CodeLine('POKE 756,MEM') +
                 CodeLine('REM MODIFIED CHARACTER DATA') +
                 CodeLine('REM Turn on TV display') +
                 CodeLine('POKE 559,34') +
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
  end
  { Turbo BASIC XL
   ---------------------------------------------------------------------------}
  else if langIndex = _TURBO_BASIC_XL then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('--') +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Antic mode 3 screen with modified characters') +
                 CodeLine('--') +
                 CodeLine('REM Reserve 4 pages of RAM for character set') +
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
    for i := 0 to 127 do begin
      if frmAntic3.charEditIndex[i] = 1 then
        code.line += CodeLine('FOR I=%0 TO 7:READ CHAR:POKE RAMSTART+I+' +
                              IntToStr(i) + '*8,CHAR:NEXT I');
    end;
    code.line += CodeLine('REM MODIFY CHARACTER SET POINTER') +
                 CodeLine('POKE 756,MEM') +
                 CodeLine('REM MODIFIED CHARACTER DATA') +
                 CodeLine('REM Turn on TV display') +
                 CodeLine('POKE 559,34') +
                 CodeLine('SCR=DPEEK(88)') +
                 CodeLine('FOR I=%0 TO ' + IntToStr(modeSize - 1)) +
                 CodeLine('READ BYTE:POKE SCR+I,BYTE') +
                 CodeLine('NEXT I');

    // Modified characters
    code.line += CodeLine('REM MODIFIED CHARACTERS');
    for i := 0 to 127 do begin
      if frmAntic3.charEditIndex[i] = 1 then begin
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')');
        code.line += CodeLine('DATA ' + SetValues(i, ','));
      end;
    end;

    // Screen data
    code.line += CodeLine('REM SCREEN DATA');
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
    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Antic mode 3 screen with predefined characters') +
                 CodeLine(_REM) +
                 CodeLine('REM Reserve 4 pages of RAM for character set') +
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
  i : byte;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex = _ATARI_BASIC) or (langIndex = _TURBO_BASIC_XL) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    for i := 0 to 127 do begin
      if frmAntic3.charEditIndex[i] = 1 then
        code.line += CodeLine('REM CHR$(' + AtasciiCode(i) + ')') +
                     CodeLine('DATA ' + SetValues(i, ','));
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

  result := code.line;
end;

procedure TfrmAntic3Gen.listExamplesProc(Sender: TObject);
begin
  CreateCode;
end;

procedure TfrmAntic3Gen.radLangProc(Sender: TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmAntic3Gen.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmAntic3Gen.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

procedure TfrmAntic3Gen.CloseWinProc(Sender: TObject);
begin
  Close;
end;

end.

