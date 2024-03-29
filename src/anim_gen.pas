{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Player Animator editor - source code generator
}
unit anim_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  StdCtrls, lcltype, ExtCtrls, BCListBox, BCMDButton, BCMaterialDesignButton,
  common;

type
  { TfrmAnimGen }
  TfrmAnimGen = class(TForm)
    boxFlip1 : TGroupBox;
    btnCopyToEditor : TBCMaterialDesignButton;
    btnClose : TBCMaterialDesignButton;
    btnSingleRes : TBCMDButton;
    btnDoubleRes : TBCMDButton;
    editLineStep : TSpinEditEx;
    editStartLine : TSpinEditEx;
    panelLang : TBCPaperPanel;
    boxStartLine : TGroupBox;
    btnAtariBASIC : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnKickC : TBCMDButton;
    btnMads : TBCMDButton;
    btnCC65 : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnTurboBasicXL : TBCMDButton;
    lblLineStep : TLabel;
    lblStartLine : TLabel;
    ListExamples : TBCPaperListBox;
    memo : TMemo;
    radDataType : TRadioGroup;
    StaticText1 : TStaticText;
    StaticText2 : TStaticText;
    StaticText3 : TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CopyToEditor(Sender: TObject);
    procedure ListExamplesProc(Sender: TObject);
    procedure CreateCode;
    procedure radLangProc(Sender: TObject);
    procedure CloseWin(Sender: TObject);
    procedure CreateCodeProc(Sender : TObject);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
  private
    listings : TListings;
    playerPageSize : string;
    PlayerMemSize : string;
    PMBASEStart : string;
    SDMCTL : string;
    PMBASE : string;
    pmRes : string;
    function GenData : string;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
  end;

var
  frmAnimGen: TfrmAnimGen;

implementation

{$R *.lfm}

uses
  animator, src_editor, lib, code_lib;

{ TfrmAnimGen }

procedure TfrmAnimGen.FormCreate(Sender: TObject);
begin
  SetListings(listings);

  // Example 1
  listings[0, 5] := false;
  listings[0, 6] := false;
  listings[0, 7] := false;

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

procedure TfrmAnimGen.FormShow(Sender: TObject);
begin
  langIndex := 0;
  ListExamples.ListBox.ItemIndex := 0;
  listExamplesProc(Sender);
end;

procedure TfrmAnimGen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmAnimGen.CloseWin(Sender: TObject);
begin
  Close;
end;

procedure TfrmAnimGen.CreateCodeProc(Sender : TObject);
begin
  CreateCode;
end;

procedure TfrmAnimGen.CopyToEditor(Sender: TObject);
begin
  if not CheckEditor then Exit;
  frmSrcEdit.Show;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor(Sender);
  Close;
end;

procedure TfrmAnimGen.CreateCode;
var
  code : string;
begin
  Set01(boxStartLine, langIndex, radDataType, false);

  case ListExamples.ListBox.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
  end;

  Set02(memo, code);
end;

function TfrmAnimGen.GenData : string;
var
  codex : string = '';
  bin : string = '';
  pl, i, j, n : byte;
  count : integer;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
//    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
//    code.line := '';
    for pl := 0 to 1 do begin
      if pl = 1 then begin
        codex += #13#10;
        Inc(code.number, code.step);
      end;

      codex += IntToStr(code.number) + ' REM *** COLORS FOR PLAYER ' + IntToStr(pl) + ' ***'#13#10;
      Inc(code.number, code.step);
      codex += IntToStr(code.number) + ' DATA ';

      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        codex += IntToStr(colorValues[pl + 4]);

        if i < frmAnimator.numFrames.Value - 1 then
          codex += ',';
      end;
      Inc(code.number, code.step);
    end;

    for pl := 0 to 1 do begin
      codex += #13#10;
      codex += IntToStr(code.number) + ' REM *** PLAYER ' + IntToStr(pl) + ' DATA ***'#13#10;
      Inc(code.number, code.step);
      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        if i > 0 then
          codex += #13#10;

        Inc(code.number, code.step);
        codex += IntToStr(code.number) + ' REM *** FRAME ' + IntToStr(i + 1) + ' ***'#13#10;
        Inc(code.number, code.step);
        count := 0;

        for j := 0 to animFrameHeight - 1 do begin
          if count = 0 then begin
            Inc(code.number, code.step);
            codex += IntToStr(code.number) + ' DATA ';

            if j > 0 then
              codex += intToStr(Bin2Dec(bin)) + ',';
          end;

          bin := '';
          for n := 0 to 7 do
            bin += IntToStr(frmAnimator.fld[i][pl, n, j]);

          if count < 8 then begin
            codex += intToStr(Bin2Dec(bin));

            if (count < 7) and (j < animFrameHeight - 1) then
              codex += ',';
          end
          else begin
            codex += #13#10;
            count := -1;
          end;

          Inc(count);
        end;
      end;
    end;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    codex := 'const'#13#10 +
            '  _HEIGHT = ' + IntToStr(animFrameHeight) + ';'#13#10 +
            #13#10 +
            'var'#13#10 +
            '  // Colors for players'#13#10;
    for pl := 0 to 1 do begin
      codex += '  p' + IntToStr(pl) + 'Color : array[0..' +
              IntToStr(frmAnimator.numFrames.Value - 1) + '] of byte = (';
      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        codex += IntToStr(colorValues[pl + 4]);

        if i < frmAnimator.numFrames.Value - 1 then
          codex += ', ';
      end;
      codex += ');'#13#10;
    end;

    codex += #13#10;

    for pl := 0 to 1 do begin
      codex += '// Player ' + IntToStr(pl) + ' data'#13#10;

      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        count := 0;
        for j := 0 to animFrameHeight - 1 do begin
          if count = 0 then begin
            if j = 0 then
              codex += 'p' + IntToStr(pl) + 'Frame' + IntToStr(i + 1) +
                       ' : array[0.._HEIGHT - 1] of byte = ('#13#10;

            if j > 0 then
              codex += intToStr(Bin2Dec(bin)) + ', '
            else
              codex += '    ';
          end;

          bin := '';
          for n := 0 to 7 do
            bin += IntToStr(frmAnimator.fld[i][pl, n, j]);

          if count < 8 then begin
            codex += intToStr(Bin2Dec(bin));

            if (count < 8) and (j < animFrameHeight - 1) then
              codex += ', ';
          end
          else begin
            codex += #13#10'    ';
            count := -1;
          end;
          Inc(count);
        end;
        codex += ');'#13#10#13#10;
      end;
    end
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    codex := '; Colors for players'#13#10 +
            'BYTE ARRAY'#13#10;
    for pl := 0 to 1 do begin
      codex += '  p'+ IntToStr(pl) + 'Color = [';

      for i := 0 to frmAnimator.numFrames.Value - 1 do
        codex += IntToStr(colorValues[pl + 4]) + ' ';

      codex += ']';

      if pl = 0 then
        codex += ','#13#10
      else
        codex += #13#10;
    end;
    for pl := 0 to 1 do begin
      codex += #13#10'; Player ' + IntToStr(pl) + ' data'#13#10;
      codex += 'BYTE ARRAY'#13#10;
      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        count := 0;
        for j := 0 to animFrameHeight - 1 do begin
          if count = 0 then begin
            if j = 0 then begin
              codex += '  p' + IntToStr(pl) + 'Frame' + IntToStr(i + 1) +
                      ' = [';  //#13#10;
//              codex += '    ';
            end
            else
              codex += intToStr(Bin2Dec(bin)) + ' '
          end;

          bin := '';
          for n := 0 to 7 do
            bin += IntToStr(frmAnimator.fld[i][pl, n, j]);

          if count < 8 then begin
            codex += intToStr(Bin2Dec(bin));

            if (count < 8) and (j < animFrameHeight - 1) then
              codex += ' ';
          end
          else begin
            codex += #13#10'    ';
            count := -1;
          end;

          Inc(count);
        end;

        codex += ']';
        if i < frmAnimator.numFrames.Value - 1 then
          codex += ',';

        codex += #13#10;
      end;
    end;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    codex := ''' Colors for players'#13#10;
    for pl := 0 to 1 do begin
      codex += 'DATA p' + IntToStr(pl) + 'Color() BYTE = ';
      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        codex += IntToStr(colorValues[pl + 4]);
        if i < frmAnimator.numFrames.Value - 1 then
          codex += ', ';
      end;
      codex += #13#10;
    end;

    codex += #13#10;
    for pl := 0 to 1 do begin
      codex += ''' Player ' + IntToStr(pl) + ' data'#13#10;
      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        count := 0;
        for j := 0 to animFrameHeight - 1 do begin
          if count = 0 then begin
            if j = 0 then
              codex += 'DATA p' + IntToStr(pl) + 'Frame' + IntToStr(i + 1) + '() BYTE = '
            else
              codex += 'DATA            BYTE = ';

            if j > 0 then
              codex += intToStr(Bin2Dec(bin)) + ',';
          end;

          bin := '';
          for n := 0 to 7 do
            bin += IntToStr(frmAnimator.fld[i][pl, n, j]);

          if count < 8 then begin
            codex += intToStr(Bin2Dec(bin));
            if (count < 8) and (j < animFrameHeight - 1) then
              codex += ',';
          end
          else begin
            codex += #13#10;
            count := -1;
          end;
          Inc(count);
        end;
        codex += #13#10;
      end;
    end;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
//    const char pmdata[] = { 0,255,129,129,129,129,129,129,255,0};
    codex := '// Colors for players'#13#10;
    for pl := 0 to 1 do begin
      codex += 'const char p'+ IntToStr(pl) + 'Color[] = {';
      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        codex += IntToStr(colorValues[pl + 4]);
        if i < frmAnimator.numFrames.Value - 1 then
          codex += ', ';
      end;
      codex += '};'#13#10;
    end;
    for pl := 0 to 1 do begin
      codex += #13#10'// Player ' + IntToStr(pl) + ' data'#13#10;
//      codex += 'const char'#13#10;
      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        count := 0;
        for j := 0 to animFrameHeight - 1 do begin
          if count = 0 then begin
            if j = 0 then begin
              codex += '  const char p' + IntToStr(pl) + 'Frame' + IntToStr(i + 1) +
                       '[] = {';
            end
            else
              codex += intToStr(Bin2Dec(bin)) + ' '
          end;

          bin := '';
          for n := 0 to 7 do
            bin += IntToStr(frmAnimator.fld[i][pl, n, j]);

          if count < 8 then begin
            codex += intToStr(Bin2Dec(bin));

            if (count < 8) and (j < animFrameHeight - 1) then
              codex += ', ';
          end
          else begin
            codex += #13#10'    ';
            count := -1;
          end;

          Inc(count);
        end;

        codex += '};'#13#10;
      end;
    end;
  end;

  result := codex;
end;

{-----------------------------------------------------------------------------
 Data values for animation frames
 -----------------------------------------------------------------------------}
function TfrmAnimGen.Example01 : string;
var
  bin : string;
  i, j, n, pl : byte;
  count : integer;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM *** FRAMES ***') +
                 CodeLine('DATA ' + IntToStr(frmAnimator.numFrames.Value)) +
                 CodeLine('REM *** HEIGHT ***') +
                 CodeLine('DATA ' + IntToStr(animFrameHeight)) +
                 CodeLine('REM *** GAP ***') +
                 CodeLine('DATA 0');
    code.line += GenData;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := 'const'#13#10 +
                 '  _FRAMES = ' + IntToStr(frmAnimator.numFrames.Value) + ';'#13#10 +
     //            '  height = ' + IntToStr(animFrameHeight) + ';'#13#10 +
                 '  _GAP = 0;'#13#10#13#10;

    code.line += GenData;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := 'frames = ' + IntToStr(frmAnimator.numFrames.Value) + #13#10 +
                 'height = ' + IntToStr(animFrameHeight) + #13#10 +
                 'gap = 0'#13#10;

    code.line += #13#10;
    code.line += GenData;
  end
  { Mad Assembler (MADS)
   ---------------------------------------------------------------------------}
  else if langIndex = _MADS then begin
    code.line := 'FRAMES'#13#10 +
                 ' .BYTE ' + IntToStr(frmAnimator.numFrames.Value) + #13#10 +
                 'HEIGHT'#13#10 +
                 ' .BYTE ' + IntToStr(animFrameHeight) + #13#10 +
                 'GAP'#13#10 +
                 ' .BYTE 0'#13#10;

    for pl := 0 to 1 do begin
      code.line += 'P'+ IntToStr(pl) + 'COLORS'#13#10;
      code.line += ' .BYTE ';
      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        code.line += '$' + Dec2Hex(colorValues[pl + 4]);
        if i < frmAnimator.numFrames.Value - 1 then
          code.line += ', ';
      end;
      code.line += #13#10;
    end;

    for pl := 0 to 1 do begin
      code.line += 'P' + IntToStr(pl) + 'DATA'#13#10;

      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        code.line += '; FRAME ' + IntToStr(i + 1) + #13#10;
        count := 0;
        for j := 0 to animFrameHeight - 1 do begin
          if count = 0 then begin
            code.line += ' .BYTE ';

            if j > 0 then
              code.line += '$' + Dec2Hex(Bin2Dec(bin)) + ', ';
          end;

          bin := '';
          for n := 0 to 7 do
            bin += IntToStr(frmAnimator.fld[i][pl, n, j]);

          if count < 8 then begin
            code.line += '$' + Dec2Hex(Bin2Dec(bin));

            if (count < 7) and (j < animFrameHeight - 1) then
              code.line += ', ';
          end
          else begin
            code.line += #13#10;
            count := -1;
          end;
          Inc(count);
        end;
        code.line += #13#10;
      end;
    end;
  end
  { CC65
   ---------------------------------------------------------------------------}
  else if langIndex = _CC65 then begin
    code.line := '#define FRAMES ' + IntToStr(frmAnimator.numFrames.Value) + #13#10 +
            '#define HEIGHT ' + IntToStr(animFrameHeight) + #13#10 +
            '#define GAP    0'#13#10 +
            #13#10;

    for pl := 0 to 1 do begin
      code.line += 'const unsigned char P' + IntToStr(pl) + 'COLORS[FRAMES] ='#13#10 +
                   '{'#13#10;
      code.line += '  ';

      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        code.line += IntToStr(colorValues[pl + 4]);

        if i < frmAnimator.numFrames.Value - 1 then
          code.line += ', ';
      end;
      code.line += #13#10'};'#13#10;
    end;
    code.line += #13#10;

    for pl := 0 to 1 do begin
      if pl = 1 then
        code.line += #13#10;

      code.line += 'const unsigned char P' + IntToStr(pl) + 'DATA[FRAMES][HEIGHT] ='#13#10 +
                   '{'#13#10;
      for i := 0 to frmAnimator.numFrames.Value - 1 do begin
        count := 0;
        code.line += '  {'#13#10;
        for j := 0 to animFrameHeight - 1 do begin
          if count = 0 then begin
            code.line += '    ';
            if j > 0 then
              code.line += intToStr(Bin2Dec(bin)) + ', ';
          end;
          bin := '';
          for n := 0 to 7 do
            bin += IntToStr(frmAnimator.fld[i][pl, n, j]);

          if count < 8 then begin
            code.line += intToStr(Bin2Dec(bin));
            if (count < 7) and (j < animFrameHeight - 1) then
              code.line += ', ';
          end
          else begin
            code.line += #13#10;
            count := -1;
          end;
          Inc(count);
        end;
        code.line += #13#10;
        if i = frmAnimator.numFrames.Value - 1 then
          code.line += '  }'#13#10
        else
          code.line += '  },'#13#10;
      end;
      code.line += '};'#13#10;
    end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := 'BYTE frames = [' + IntToStr(frmAnimator.numFrames.Value) + ']'#13#10 +
                 'BYTE height = [' + IntToStr(animFrameHeight) + ']'#13#10 +
                 'BYTE gap = [0]'#13#10#13#10;
    code.line += GenData;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    //    const char pmdata[] = { 0,255,129,129,129,129,129,129,255,0};
    code.line := 'const char frames = ' + IntToStr(frmAnimator.numFrames.Value) + ';'#13#10 +
                 'const char height = ' + IntToStr(animFrameHeight) + ';'#13#10 +
                 'const char gap = 0;'#13#10#13#10;
    code.line += GenData;
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Animation with non-moving object
 -----------------------------------------------------------------------------}
function TfrmAnimGen.Example02 : string;
var
  i : byte;
  dimValue : short;
  rtnCodeNum : word = 20000;       // ML starting Atari BASIC line number
  charDataCodeNum : word = 30000;  // Character data starting Atari BASIC line number
begin
  dimValue := frmAnimator.numFrames.Value * animFrameHeight;

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;

    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Animation with non-moving object') +
                 CodeLine(_REM);
    code.line += CodeLine('DIM MLCODE$(33)');
    code.line += CodeLine('GOSUB ' + IntToStr(rtnCodeNum));
    code.line += CodeLine('DIM PL0(' + IntToStr(dimValue) + '),' +
                          'PL1(' + IntToStr(dimValue) + ')');
    code.line += CodeLine('DIM PL0COL(' + IntToStr(frmAnimator.numFrames.Value - 1) +
                          '),PL1COL(' + IntToStr(frmAnimator.numFrames.Value - 1) + ')');
    code.line += CodeLine('REM Number of frames');
    code.line += CodeLine('FRAMES=' + IntToStr(frmAnimator.numFrames.Value));
    code.line += CodeLine('REM Player data height');
    code.line += CodeLine('HEIGHT=' + IntToStr(animFrameHeight));
    code.line += CodeLine('REM Player position');
    code.line += CodeLine('PX0=90:PY0=45');
    code.line += CodeLine('PX1=90:PY1=45');
    code.line += CodeLine('REM Initialize P/M graphics');
    code.line += CodeLine('POKE 53277,0');
    code.line += CodeLine('GRAPHICS 0');
    code.line += CodeLine('POKE 710,0:POKE 712,0');
    code.line += CodeLine('? "Set P/M graphics"');
    code.line += CodeLine('PMGMEM=PEEK(106)-' + playerPageSize);
    code.line += CodeLine('POKE 54279,PMGMEM');
    code.line += CodeLine('PMGMEM=PMGMEM*256');
    code.line += CodeLine('REM ' + pmRes);
    code.line += CodeLine('POKE 559,' + SDMCTL);
//    code.line += CodeLine('? "Clear player memory"');
//    code.line += CodeLine('FOR I=0 TO ' + PMBASE + '+' + PlayerMemSize + '-1:POKE PMGMEM+' + PMBASEStart + '+I,0:NEXT I');

    code.line += CodeLine('REM CALL MACHINE LANGUAGE ROUTINE');
    code.line += CodeLine('X=USR(ADR(MLCODE$),1536,PEEK(PMGMEM),' +
                          IntToStr(StrToInt(playerPageSize) div 2) + ')');
    if code.number > 30000 then
      Inc(charDataCodeNum, 1000);

    code.line += CodeLine('REM Enable third color');
    code.line += CodeLine('POKE 623,33');
    code.line += CodeLine('REM Player normal size');
    code.line += CodeLine('POKE 53256,0');
    code.line += CodeLine('POKE 53257,0');
    code.line += CodeLine('REM Set player colors');
    code.line += CodeLine('FOR I=0 TO FRAMES-1:READ A:PL0COL(I)=A:NEXT I');
    code.line += CodeLine('FOR I=0 TO FRAMES-1:READ A:PL1COL(I)=A:NEXT I');
    code.line += CodeLine('REM Player horizontal position');
    code.line += CodeLine('POKE 53248,PX0');
    code.line += CodeLine('POKE 53249,PX1');
    code.line += CodeLine('? "SET PLAYER 0 DATA"');
    code.line += CodeLine('FOR FRAME=0 TO FRAMES-1');
    code.line += CodeLine('FOR I=0 TO HEIGHT-1');
    code.line += CodeLine('READ A:PL0(I+FRAME*HEIGHT)=A');
    code.line += CodeLine('NEXT I');
    code.line += CodeLine('NEXT FRAME');
    code.line += CodeLine('? "SET PLAYER 1 DATA"');
    code.line += CodeLine('FOR FRAME=0 TO FRAMES-1');
    code.line += CodeLine('FOR I=0 TO HEIGHT-1');
    code.line += CodeLine('READ A:PL1(I+FRAME*HEIGHT)=A');
    code.line += CodeLine('NEXT I');
    code.line += CodeLine('NEXT FRAME');
    code.line += CodeLine('? "Let''s move..."');
    code.line += CodeLine('REM "Turn on P/M graphics"');
    code.line += CodeLine('POKE 53277,3');
    code.line += CodeLine('FOR ITER=1 TO 4');
//    code += IntToStr(LineNum) + ' FOR I=0 TO 255:POKE PMGMEM+512+I,0:NEXT I'#13#10;
    code.line += CodeLine('FOR FRAME=0 TO FRAMES-1');
    code.line += CodeLine('POKE 704,PL0COL(FRAME)');
    code.line += CodeLine('POKE 705,PL1COL(FRAME)');
    code.line += CodeLine('FOR I=0 TO HEIGHT-1');
    code.line += CodeLine('POKE PMGMEM+' + PMBASE + '+PY0+I,PL0(I+FRAME*HEIGHT)');
    code.line += CodeLine('POKE PMGMEM+' + PMBASE + '+' + PlayerMemSize + '+PY1+I,PL1(I+FRAME*HEIGHT)');
    code.line += CodeLine('NEXT I');
    code.line += CodeLine('NEXT FRAME');
    code.line += CodeLine('NEXT ITER');
    code.line += CodeLine('END');

    // Machine language routine
    code.number := rtnCodeNum;
    code.line += SetFastCopyRoutine;
    // Modified character data
    code.number := charDataCodeNum;

    code.line += GenData;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Animation with non-moving object'#13#10#13#10;
    code.line += 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt;'#13#10#13#10;
    code.line += GenData;

    //code += 'frames = ' + IntToStr(frmAnimator.numFrames.Value) + #13#10 +
    //        'height = ' + IntToStr(animFrameHeight) + #13#10 +
    //        'gap = 0'#13#10;

    code.line += '  PMGMEM : word;'#13#10 +
                 '  px0, py0 : byte;'#13#10 +
                 '  px1, py1 : byte;'#13#10 +
                 '  frame : byte;'#13#10 +
                 '  i : byte;'#13#10#13#10 +
                 'procedure NextFrame;'#13#10 +
                 'begin'#13#10;

    for i := 1 to frmAnimator.numFrames.Value do begin
      if i = 1 then
        code.line += '  if frame = 1 then begin'#13#10 +
                     '    Move(p0Frame' + IntToStr(i) + ', Pointer(PMGMEM + ' + PMBASE + ' + py0), _HEIGHT);' +
                     #13#10 +
                     '    Move(p1Frame' + IntToStr(i) + ', Pointer(PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + py1), _HEIGHT);' +
                     #13#10 +
                     '  end'
      else
        code.line += '  else if frame = ' + IntToStr(i) + ' then begin'#13#10 +
                     '    Move(p0Frame' + IntToStr(i) + ', Pointer(PMGMEM + ' + PMBASE + ' + PY0), _HEIGHT);' +
                     #13#10 +
                     '    Move(p1Frame' + IntToStr(i) + ', Pointer(PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + PY1), _HEIGHT);' +
                     #13#10 +
                     '  end';

      if i = frmAnimator.numFrames.Value then
        code.line += ';';

      code.line += #13#10;
    end;
    code.line += 'end;'#13#10;

    code.line += #13#10'begin'#13#10 +
                '  // Player position'#13#10 +
                '  px0 := 90; py0 := 40;'#13#10 +
                '  px1 := 90; py1 := 40;'#13#10#13#10 +
                '  // Set environment'#13#10 +
                '  InitGraph(0);'#13#10 +
                '  Poke(710, 0); Poke(712, 0);'#13#10#13#10 +
                '  // Set P/M graphics'#13#10 +
                '  Poke(53277, 0);'#13#10 +
                '  PMGMEM := Peek(106) - ' + playerPageSize + ';'#13#10 +
                '  Poke(54279, PMGMEM);'#13#10 +
                '  PMGMEM := PMGMEM * 256;'#13#10#13#10 +
                '  // ' + pmRes + #13#10 +
                '  Poke(559, ' + SDMCTL + ');'#13#10#13#10 +
                '  // Clear player memory'#13#10 +
                '  FillByte(pointer(PMGMEM + ' + PMBASEStart + '), ' + PMBASE + ' - 1 + ' + PlayerMemSize + ', 0);'#13#10#13#10 +
                '  // Enable third color'#13#10 +
                '  Poke(623, 33);'#13#10#13#10 +
                '  // Player normal size'#13#10 +
                '  Poke(53256, 0); Poke(53257, 0);'#13#10#13#10 +
                '  // Turn on P/M graphics'#13#10 +
                '  Poke(53277, 3);'#13#10#13#10 +
                '  frame := 1;'#13#10#13#10 +
                '  Writeln(''Player animation'');'#13#10 +
                '  Poke(53248, px0); Poke(53249, px1);'#13#10#13#10 +
                '  for i := 1 to 50 do begin'#13#10 +
                '    NextFrame;'#13#10 +
                '    Poke(704, p0Color[0]);'#13#10 +
                '    Poke(705, p1Color[0]);'#13#10 +
                '    Pause(5);'#13#10 +
                '    Inc(frame);'#13#10 +
                '    if frame > ' + IntToStr(frmAnimator.numFrames.Value) + ' then frame := 1;' +
                #13#10'  end;'#13#10 +
                WaitKeyCode(langIndex) +
                'end.';
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := ';' + _REM_MAD_STUDIO + #13#10 +
                 '; Animation with non-moving object'#13#10#13#10;
    code.line += GenData;
    code.line += #13#10;
    code.line += 'BYTE PMBASE = $D407  ; PLAYER/MISSILE BASE ADDRESS'#13#10 +
                'BYTE SDMCTL = $22F   ; PLAYER RESOLUTION'#13#10 +
                'BYTE GRACTL = $D01D  ; 1 = MISSILE DMA, 2 = PLAYER DMA'#13#10 +
                'BYTE RAMTOP = $6A    ; TOP OF RAM IN 256-BYTE PAGES'#13#10 +
                'BYTE PRIOR  = 623    ; Priority Register'#13#10 +
                'BYTE CH     = $2FC   ; KEYBOARD CODE OF LAST KEY PRESSED'#13#10 +
                'CARD PMGMEM'#13#10#13#10 +
                'BYTE ARRAY'#13#10 +
                '  PCOLR(1) = $2C0,   ; PLAYER COLOR'#13#10 +
                '  HPOSP(1) = $D000,  ; PLAYER HORIZONTAL POSITION'#13#10 +
                '  SIZEP(1) = $D008   ; PLAYER SIZE'#13#10#13#10 +
    //            'BYTE px0, py0'#13#10 +
    //            'BYTE px1, py1'#13#10 +
                'BYTE height = [' + IntToStr(animFrameHeight) + ']'#13#10 +
                'BYTE frame = [1]'#13#10 +
                'BYTE i'#13#10 +
                'INT delay'#13#10#13#10 +
                'PROC NextFrame()'#13#10;
    code.line += #13#10;
    for i := 1 to frmAnimator.numFrames.Value do begin
      if i = 1 then
        code.line += 'IF frame = 1 THEN'#13#10 +
                      '  MoveBlock(PMGMEM + ' + PMBASE + ' + 70, p0Frame' + IntToStr(i) + ', height)' +
                      #13#10 +
                      '  MoveBlock(PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + 70, p1Frame' + IntToStr(i) + ', height)' +
                      #13#10
      else
        code.line += 'ELSEIF frame = ' + IntToStr(i) + ' THEN'#13#10 +
                     '  MoveBlock(PMGMEM + ' + PMBASE + ' + 70, p0Frame' + IntToStr(i) + ', height)' +
                     #13#10 +
                     '  MoveBlock(PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + 70, p1Frame' + IntToStr(i) + ', height)' +
                     #13#10;
    end;
    code.line += 'FI'#13#10#13#10;
    code.line += 'RETURN'#13#10#13#10 +
                'PROC MAIN()'#13#10#13#10 +
                //'; Player position'#13#10 +
                //'PX0 = 90 PY0 = 40'#13#10 +
                //'PX1 = 90 PY1 = 40'#13#10 +
                '; Set environment'#13#10 +
                'Graphics(0)'#13#10 +
                'Poke(710, 0) Poke(712, 0)'#13#10#13#10 +
                '; Set P/M graphics'#13#10 +
                'GRACTL = 0'#13#10 +
                'PMGMEM = RAMTOP - ' + playerPageSize + #13#10 +
                'PMBASE = PMGMEM'#13#10 +
                'PMGMEM ==* 256'#13#10#13#10 +
                '; ' + pmRes + #13#10 +
                'SDMCTL = 46'#13#10#13#10 +
                '; Clear player memory'#13#10 +
                'Zero(PMGMEM + ' + PMBASEStart + ', ' + PMBASE + ' - 1 + ' + PlayerMemSize + ')'#13#10#13#10 +
                '; Enable third color'#13#10 +
                'PRIOR = 33'#13#10#13#10 +
                '; Player normal size'#13#10 +
                'Zero(SIZEP, 2)'#13#10#13#10 +
                '; Turn on P/M graphics'#13#10 +
                'GRACTL = 3'#13#10#13#10 +
                'PrintE("Player animation")'#13#10 +
                'HPOSP(0) = 90 HPOSP(1) = 90'#13#10#13#10 +
                'FOR i = 1 TO 50 DO'#13#10 +
                '  NextFrame()'#13#10 +
                '  PCOLR(0) = p0Color(0)'#13#10 +
                '  PCOLR(1) = p1Color(0)'#13#10 +
                '  FOR delay = 0 TO 4500 DO OD'#13#10 +
                '  frame ==+ 1'#13#10 +
                '  IF frame > ' + IntToStr(frmAnimator.numFrames.Value) + ' THEN frame = 1 FI' +
                #13#10 +
                'OD'#13#10#13#10 +
                WaitKeyCode(langIndex) +
                '; TURN OFF P/M GRAPHICS'#13#10 +
                'GRACTL=0'#13#10#13#10 +
                'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                 ''' Animation with non-moving object'#13#10#13#10;
    code.line += GenData;

    //code += 'frames = ' + IntToStr(frmAnimator.numFrames.Value) + #13#10 +
    //        'height = ' + IntToStr(animFrameHeight) + #13#10 +
    //        'gap = 0'#13#10;

    code.line += #13#10''' Player position'#13#10 +
                'PX0 = 90 : PY0 = 40'#13#10 +
                'PX1 = 90 : PY1 = 40'#13#10#13#10 +
                ''' Set environment'#13#10 +
                'GRAPHICS 0'#13#10 +
                'POKE 710, 0 : POKE 712, 0'#13#10#13#10 +
                ''' Set P/M graphics'#13#10 +
                'POKE 53277, 0'#13#10 +
                'PMGMEM = PEEK(106) - ' + playerPageSize + #13#10 +
                'POKE 54279, PMGMEM'#13#10 +
                'PMGMEM = PMGMEM * 256'#13#10#13#10 +
                '''' + pmRes + #13#10 +
                'POKE 559, ' + SDMCTL + #13#10#13#10 +
                ''' Clear player memory'#13#10 +
                'MSET PMGMEM + ' + PMBASEStart + ', ' + PMBASE + ' - 1 + ' + PlayerMemSize + ', 0'#13#10 +
                //'POKE PMGMEM + 384, 0
                //'MOVE PMGMEM + 384, PMGMEM + 384 + 1, ' + PMBASE + ' - 1 + 128
                #13#10 +
                ''' Enable third color'#13#10 +
                'POKE 623, 33'#13#10#13#10 +
                ''' Player normal size'#13#10 +
                'POKE 53256, 0 : POKE 53257, 0'#13#10#13#10 +
                ''' Turn on P/M graphics'#13#10 +
                'POKE 53277, 3'#13#10#13#10 +
                'FRAME=1'#13#10#13#10 +
                '? "Player animation"'#13#10 +
                'POKE 53248, PX0 : POKE 53249, PX1'#13#10#13#10 +
                'FOR I = 1 TO 50'#13#10 +
                '  EXEC NextFrame'#13#10 +
                '  POKE 704, p0Color(0)'#13#10 +
                '  POKE 705, p1Color(0)'#13#10 +
                '  PAUSE(4)'#13#10 +
                '  INC FRAME'#13#10 +
                '  IF FRAME > ' + IntToStr(frmAnimator.numFrames.Value) + ' THEN FRAME = 1'#13#10 +
                'NEXT'#13#10 +
                WaitKeyCode(langIndex) + #13#10 +
                'END'#13#10#13#10 +
                'PROC NextFrame'#13#10#13#10;

      for i := 1 to frmAnimator.numFrames.Value do begin
        if i = 1 then begin
          code.line += 'IF FRAME = 1'#13#10 +
                       '  MOVE ADR(p0Frame' + IntToStr(i) + '), PMGMEM + ' + PMBASE + ' + 70, ' +
                       IntToStr(animFrameHeight) + #13#10 +
                       '  MOVE ADR(p1Frame' + IntToStr(i) + '), PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + 70, ' +
                       IntToStr(animFrameHeight) + #13#10;
        end
        else begin
          code.line += 'ELIF FRAME = ' + IntToStr(i) + #13#10 +
                       '  MOVE ADR(p0Frame' + IntToStr(i) + '), PMGMEM + ' + PMBASE + ' + 70, ' +
                       IntToStr(animFrameHeight) + #13#10 +
                       '  MOVE ADR(p1Frame' + IntToStr(i) + '), PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + 70, ' +
                       IntToStr(animFrameHeight) + #13#10;
        end;
      end;
      code.line += 'ENDIF'#13#10#13#10 +
                   'ENDPROC';
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

{-----------------------------------------------------------------------------
 Animation with moving object in horizontal direction
 -----------------------------------------------------------------------------}
function TfrmAnimGen.Example03 : string;
var
  i : byte;
  dimValue : short;
  rtnCodeNum : word = 20000;       // ML starting Atari BASIC line number
  charDataCodeNum : word = 30000;  // Character data starting Atari BASIC line number
begin
  dimValue := frmAnimator.numFrames.Value*animFrameHeight;

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if langIndex < 2 then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;

    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM Animation with moving object') +
                 CodeLine(_REM);
    code.line += CodeLine('DIM MLCODE$(33)');
    code.line += CodeLine('GOSUB ' + IntToStr(rtnCodeNum));
    code.line += CodeLine('DIM PL0(' + IntToStr(dimValue) + '),' +
                          'PL1(' + IntToStr(dimValue) + ')');
    code.line += CodeLine('DIM PL0COL(' + IntToStr(frmAnimator.numFrames.Value - 1) +
                          '),PL1COL(' + IntToStr(frmAnimator.numFrames.Value - 1) + ')');
    code.line += CodeLine('REM Number of frames');
    code.line += CodeLine('FRAMES=' + IntToStr(frmAnimator.numFrames.Value));
    code.line += CodeLine('REM Player data height');
    code.line += CodeLine('HEIGHT=' + IntToStr(animFrameHeight));
    code.line += CodeLine('REM Player position');
    code.line += CodeLine('PX0=90:PY0=45');
    code.line += CodeLine('PX1=90:PY1=45');
    code.line += CodeLine('REM Initialize P/M graphics');
    code.line += CodeLine('POKE 53277,0');
    code.line += CodeLine('GRAPHICS 0');
    code.line += CodeLine('POKE 710,0:POKE 712,0');
    code.line += CodeLine('? "Set P/M graphics"');
    code.line += CodeLine('PMGMEM=PEEK(106)-' + playerPageSize);
    code.line += CodeLine('POKE 54279,PMGMEM');
    code.line += CodeLine('PMGMEM=PMGMEM*256');
    code.line += CodeLine('REM ' + pmRes);
    code.line += CodeLine('POKE 559,' + SDMCTL);
//    code.line += CodeLine('? "Clear player memory"');
//    code.line += CodeLine('FOR I=0 TO ' + PMBASE + '+' + PlayerMemSize + '-1:POKE PMGMEM+' + PMBASEStart + '+I,0:NEXT I');

    code.line += CodeLine('REM CALL MACHINE LANGUAGE ROUTINE');
    code.line += CodeLine('X=USR(ADR(MLCODE$),1536,PEEK(PMGMEM),' +
                          IntToStr(StrToInt(playerPageSize) div 2) + ')');
    if code.number > 30000 then
      Inc(charDataCodeNum, 1000);

    code.line += CodeLine('REM Enable third color');
    code.line += CodeLine('POKE 623,33');
    code.line += CodeLine('REM Player normal size');
    code.line += CodeLine('POKE 53256,0');
    code.line += CodeLine('POKE 53257,0');
    code.line += CodeLine('REM Set player colors');
    code.line += CodeLine('FOR I=0 TO FRAMES-1:READ A:PL0COL(I)=A:NEXT I');
    code.line += CodeLine('FOR I=0 TO FRAMES-1:READ A:PL1COL(I)=A:NEXT I');
    code.line += CodeLine('REM Player horizontal position');
    code.line += CodeLine('POKE 53248,PX0');
    code.line += CodeLine('POKE 53249,PX1');
    code.line += CodeLine('? "SET PLAYER 0 DATA"');
    code.line += CodeLine('FOR FRAME=0 TO FRAMES-1');
    code.line += CodeLine('FOR I=0 TO HEIGHT-1');
    code.line += CodeLine('READ A:PL0(I+FRAME*HEIGHT)=A');
    code.line += CodeLine('NEXT I');
    code.line += CodeLine('NEXT FRAME');
    code.line += CodeLine('? "SET PLAYER 1 DATA"');
    code.line += CodeLine('FOR FRAME=0 TO FRAMES-1');
    code.line += CodeLine('FOR I=0 TO HEIGHT-1');
    code.line += CodeLine('READ A:PL1(I+FRAME*HEIGHT)=A');
    code.line += CodeLine('NEXT I');
    code.line += CodeLine('NEXT FRAME');
    code.line += CodeLine('? "Let''s move..."');
    code.line += CodeLine('REM "Turn on P/M graphics"');
    code.line += CodeLine('POKE 53277,3');
    code.line += CodeLine('FOR ITER=1 TO 4');
//    Inc(lineNum, 10);
//    code += IntToStr(LineNum) + ' FOR I=0 TO 255:POKE PMGMEM+512+I,0:NEXT I'#13#10;
    code.line += CodeLine('FOR FRAME=0 TO FRAMES-1');
    code.line += CodeLine('POKE 704,PL0COL(FRAME)');
    code.line += CodeLine('POKE 705,PL1COL(FRAME)');
    code.line += CodeLine('FOR I=0 TO HEIGHT-1');
    code.line += CodeLine('POKE PMGMEM+' + PMBASE + '+PY0+I,PL0(I+FRAME*HEIGHT)');
    code.line += CodeLine('POKE PMGMEM+' + PMBASE + '+' + PlayerMemSize + '+PY1+I,PL1(I+FRAME*HEIGHT)');
    code.line += CodeLine('NEXT I');
    code.line += CodeLine('REM Move player horizontally');
    code.line += CodeLine('POKE 53248,PX0:POKE 53249,PX1');
    code.line += CodeLine('PX0=PX0+1:PX1=PX1+1');
    code.line += CodeLine('NEXT FRAME');
    code.line += CodeLine('NEXT ITER');
    code.line += CodeLine('END');

    // Machine language routine
    code.number := rtnCodeNum;
    code.line += SetFastCopyRoutine;
    // Modified character data
    code.number := charDataCodeNum;

    code.line += GenData;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Animation with moving object'#13#10#13#10;
    code.line += 'uses'#13#10 +
                 '  SysUtils, FastGraph, Crt;'#13#10 +
                 #13#10;
    code.line += GenData;

    //code += 'frames = ' + IntToStr(frmAnimator.numFrames.Value) + #13#10 +
    //        'height = ' + IntToStr(animFrameHeight) + #13#10 +
    //        'gap = 0'#13#10;

    code.line += '  PMGMEM : word;'#13#10 +
                 '  px0, py0 : byte;'#13#10 +
                 '  px1, py1 : byte;'#13#10 +
                 '  frame : byte;'#13#10 +
                 '  i : byte;'#13#10 +
                 #13#10 +
                 'procedure NextFrame;'#13#10 +
                 'begin'#13#10;
    for i := 1 to frmAnimator.numFrames.Value do begin
      if i = 1 then
        code.line += '  if frame = 1 then begin'#13#10 +
                     '    Move(p0Frame' + IntToStr(i) + ', Pointer(PMGMEM + ' + PMBASE + ' + py0), _HEIGHT);' +
                     #13#10 +
                     '    Move(p1Frame' + IntToStr(i) + ', Pointer(PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + py1), _HEIGHT);' +
                     #13#10 +
                     '  end'
      else
        code.line += '  else if frame = ' + IntToStr(i) + ' then begin'#13#10 +
                     '    Move(p0Frame' + IntToStr(i) + ', Pointer(PMGMEM + ' + PMBASE + ' + py0), _HEIGHT);' +
                     #13#10 +
                     '    Move(p1Frame' + IntToStr(i) + ', Pointer(PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + py1), _HEIGHT);' +
                     #13#10 +
                     '  end';

      if i = frmAnimator.numFrames.Value then
        code.line += ';'#13#10
      else
        code.line += #13#10;
    end;
    code.line += 'end;'#13#10;

    code.line += #13#10'begin'#13#10 +
                '  // Player position'#13#10 +
                '  px0 := 90; py0 := 40;'#13#10 +
                '  px1 := 90; py1 := 40;'#13#10#13#10 +
                '  // Set environment'#13#10 +
                '  InitGraph(0);'#13#10 +
                '  Poke(710, 0); Poke(712, 0);'#13#10 +
                #13#10'  // Set P/M graphics'#13#10 +
                '  Poke(53277, 0);'#13#10 +
                '  PMGMEM := Peek(106) - ' + playerPageSize + ';'#13#10 +
                '  Poke(54279, PMGMEM);'#13#10 +
                '  PMGMEM := PMGMEM * 256;'#13#10#13#10 +
                '  // ' + pmRes + #13#10 +
                '  Poke(559, ' + SDMCTL + ');'#13#10#13#10 +
                '  // Clear player memory'#13#10 +
                '  FillByte(pointer(PMGMEM + ' + PMBASEStart + '), ' + PMBASE + ' - 1 + ' +
                  PlayerMemSize + ', 0);'#13#10#13#10 +
                '  // Enable third color'#13#10 +
                '  Poke(623, 33);'#13#10#13#10 +
                '  // Player normal size'#13#10 +
                '  Poke(53256, 0); Poke(53257, 0);'#13#10#13#10 +
                '  // Turn on P/M graphics'#13#10 +
                '  Poke(53277, 3);'#13#10#13#10 +
                '  frame := 1;'#13#10#13#10 +
                '  Writeln(''Player animation and movement'');'#13#10 +
                '  Poke(53248, px0); Poke(53249, px1);'#13#10#13#10 +
                '  for i := 1 to 50 do begin'#13#10 +
                '    NextFrame;'#13#10 +
                '    Poke(704, p0Color[0]);'#13#10 +
                '    Poke(705, p1Color[0]);'#13#10 +
                '    Poke(53248, px0); Poke(53249, px1);'#13#10 +
                '    Inc(px0); Inc(px1);'#13#10 +
                '    Pause(4);'#13#10 +
                '    Inc(frame);'#13#10 +
                '    if frame > ' + IntToStr(frmAnimator.numFrames.Value) + ' then frame := 1;' +
                #13#10 +
                '  end;'#13#10 +
                WaitKeyCode(langIndex) +
                'end.';
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := ';' + _REM_MAD_STUDIO + #13#10 +
                 '; Animation with moving object'#13#10#13#10;
    code.line += GenData;

    code.line += #13#10;
    code.line += 'BYTE PMBASE = $D407  ; PLAYER/MISSILE BASE ADDRESS'#13#10 +
                 'BYTE SDMCTL = $22F   ; PLAYER RESOLUTION'#13#10 +
                 'BYTE GRACTL = $D01D  ; 1 = MISSILE DMA, 2 = PLAYER DMA'#13#10 +
                 'BYTE RAMTOP = $6A    ; TOP OF RAM IN 256-BYTE PAGES'#13#10 +
                 'BYTE PRIOR  = 623    ; Priority Register'#13#10 +
                 'BYTE CH     = $2FC   ; KEYBOARD CODE OF LAST KEY PRESSED'#13#10 +
                 'CARD PMGMEM'#13#10#13#10 +
                 'BYTE ARRAY'#13#10 +
                 '  PCOLR(1) = $2C0,   ; PLAYER COLOR'#13#10 +
                 '  HPOSP(1) = $D000,  ; PLAYER HORIZONTAL POSITION'#13#10 +
                 '  SIZEP(1) = $D008   ; PLAYER SIZE'#13#10#13#10 +
                 'BYTE px0 = [90]'#13#10 +
                 'BYTE px1 = [90]'#13#10 +
                 'BYTE height = [' + IntToStr(animFrameHeight) + ']'#13#10 +
                 'BYTE frame = [1]'#13#10 +
                 'BYTE i'#13#10 +
                 'INT delay'#13#10#13#10 +
                 'PROC NextFrame()'#13#10;
    code.line += #13#10;
    for i := 1 to frmAnimator.numFrames.Value do begin
      if i = 1 then
        code.line += 'IF frame = 1 THEN'#13#10 +
                     '  MoveBlock(PMGMEM + ' + PMBASE + ' + 70, p0Frame' + IntToStr(i) + ', height)' +
                     #13#10 +
                     '  MoveBlock(PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + 70, p1Frame' + IntToStr(i) + ', height)' +
                     #13#10
      else
        code.line += 'ELSEIF frame = ' + IntToStr(i) + ' THEN'#13#10 +
                     '  MoveBlock(PMGMEM + ' + PMBASE + ' + 70, p0Frame' + IntToStr(i) + ', height)' +
                     #13#10 +
                     '  MoveBlock(PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + 70, p1Frame' + IntToStr(i) + ', height)' +
                     #13#10;
    end;
    code.line += 'FI'#13#10#13#10 +
                'RETURN'#13#10#13#10 +
                'PROC MAIN()'#13#10#13#10 +
                '; Set environment'#13#10 +
                'Graphics(0)'#13#10 +
                'Poke(710, 0) Poke(712, 0)'#13#10#13#10 +
                '; Set P/M graphics'#13#10 +
                'GRACTL = 0'#13#10 +
                'PMGMEM = RAMTOP - ' + playerPageSize + #13#10 +
                'PMBASE = PMGMEM'#13#10 +
                'PMGMEM ==* 256'#13#10#13#10 +
                '; ' + pmRes + #13#10 +
                'SDMCTL = 46'#13#10#13#10 +
                '; Clear player memory'#13#10 +
                'Zero(PMGMEM + ' + PMBASEStart + ', ' + PMBASE + ' - 1 + ' + PlayerMemSize + ')'#13#10#13#10 +
                '; Enable third color'#13#10 +
                'PRIOR = 33'#13#10#13#10 +
                '; Player normal size'#13#10 +
                'Zero(SIZEP, 2)'#13#10#13#10 +
                '; Turn on P/M graphics'#13#10 +
                'GRACTL = 3'#13#10#13#10 +
                'PrintE("Player animation")'#13#10 +
                'HPOSP(0) = px0 HPOSP(1) = px1'#13#10#13#10 +
                'FOR i = 1 TO 50 DO'#13#10 +
                '  NextFrame()'#13#10 +
                '  PCOLR(0) = p0Color(0)'#13#10 +
                '  PCOLR(1) = p1Color(0)'#13#10 +
                '    HPOSP(0) = PX0 HPOSP(1) = PX1'#13#10 +
                '    px0 ==+ 1 px1 ==+ 1'#13#10 +
                '  FOR delay = 0 TO 4500 DO OD'#13#10 +
                '  frame ==+ 1'#13#10 +
                '  IF frame > ' + IntToStr(frmAnimator.numFrames.Value) + ' THEN frame = 1 FI'#13#10 +
                'OD'#13#10 +
                WaitKeyCode(langIndex) + #13#10 +
                '; TURN OFF P/M GRAPHICS'#13#10 +
                'GRACTL=0'#13#10#13#10 +
                'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                 ''' Animation with moving object'#13#10#13#10;
    code.line += GenData;

    //code += 'frames = ' + IntToStr(frmAnimator.numFrames.Value) + #13#10 +
    //        'height = ' + IntToStr(animFrameHeight) + #13#10 +
    //        'gap = 0'#13#10;
    code.line += #13#10''' Player position'#13#10 +
                 'PX0 = 70 : PY0 = 40'#13#10 +
                 'PX1 = 70 : PY1 = 40'#13#10 +
                 #13#10''' Set environment'#13#10 +
                 'GRAPHICS 0'#13#10 +
                 'POKE 710, 0 : POKE 712, 0'#13#10#13#10 +
                 ''' Set P/M graphics'#13#10 +
                 'POKE 53277, 0'#13#10 +
                 'PMGMEM = PEEK(106) - ' + playerPageSize + #13#10 +
                 'POKE 54279, PMGMEM'#13#10 +
                 'PMGMEM = PMGMEM * 256'#13#10#13#10 +
                 '''' + pmRes + #13#10 +
                 'POKE 559, ' + SDMCTL + #13#10#13#10 +
                 ''' Clear player memory'#13#10 +
                 'MSET PMGMEM + ' + PMBASEStart + ', ' + PMBASE + ' - 1 + ' + PlayerMemSize + ', 0'#13#10 +
                 //'POKE PMGMEM + 384, 0
                 //'MOVE PMGMEM + 384, PMGMEM + 384 + 1, ' + PMBASE + ' - 1 + ' + PlayerMemSize + '
                 #13#10''' Enable third color'#13#10 +
                 'POKE 623, 33'#13#10#13#10 +
                 ''' Player normal size'#13#10 +
                 'POKE 53256, 0 : POKE 53257, 0'#13#10#13#10 +
                 ''' Turn on P/M graphics'#13#10 +
                 'POKE 53277, 3'#13#10#13#10 +
                 'FRAME=1'#13#10#13#10 +
                 '? "Player animation and movement"'#13#10 +
                 'POKE 53248, PX0 : POKE 53249, PX1'#13#10 +
                 'FOR I = 1 TO 60'#13#10 +
                 '  EXEC NextFrame'#13#10 +
                 '  POKE 704, p0Color(0)'#13#10 +
                 '  POKE 705, p1Color(0)'#13#10 +
                 '  POKE 53248, PX0 : POKE 53249, PX1'#13#10 +
                 '  INC PX0 : INC PX1'#13#10 +
                 '  PAUSE(2)'#13#10 +
                 '  INC FRAME'#13#10 +
                 '  IF FRAME > ' + IntToStr(frmAnimator.numFrames.Value) + ' THEN FRAME = 1'#13#10 +
                 'NEXT'#13#10;
       code.line += WaitKeyCode(langIndex);
       code.line += 'END'#13#10#13#10 +
                    'PROC NextFrame'#13#10#13#10;

       for i := 1 to frmAnimator.numFrames.Value do begin
         if i = 1 then
           code.line += 'IF FRAME = 1'#13#10 +
                        '  MOVE ADR(p0Frame' + IntToStr(i) + '), PMGMEM + ' + PMBASE + ' + 70, ' +
                        IntToStr(animFrameHeight) + #13#10 +
                        '  MOVE ADR(p1Frame' + IntToStr(i) + '), PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + 70, ' +
                        IntToStr(animFrameHeight) + #13#10
         else
           code.line += 'ELIF FRAME = ' + IntToStr(i) + #13#10 +
                        '  MOVE ADR(p0Frame' + IntToStr(i) + '), PMGMEM + ' + PMBASE + ' + 70, ' +
                        IntToStr(animFrameHeight) + #13#10 +
                        '  MOVE ADR(p1Frame' + IntToStr(i) + '), PMGMEM + ' + PMBASE + ' + ' + PlayerMemSize + ' + 70, ' +
                        IntToStr(animFrameHeight) + #13#10;
       end;
       code.line += 'ENDIF'#13#10#13#10 +
                    'PAUSE(0)'#13#10#13#10 +
                    'ENDPROC';
  end
  else begin
    code.line := '';
  end;

  result := code.line;
end;

procedure TfrmAnimGen.radLangProc(Sender: TObject);
begin
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmAnimGen.ListExamplesProc(Sender: TObject);
begin
  btnAtariBASIC.Enabled := listings[listExamples.ListBox.ItemIndex, 0];
  btnTurboBasicXL.Enabled := listings[listExamples.ListBox.ItemIndex, 1];
  btnMadPascal.Enabled := listings[listExamples.ListBox.ItemIndex, 2];
  btnEffectus.Enabled := listings[listExamples.ListBox.ItemIndex, 3];
  btnFastBasic.Enabled := listings[listExamples.ListBox.ItemIndex, 4];
  btnKickC.Enabled := listings[listExamples.ListBox.ItemIndex, 8];
  btnMads.Enabled := listings[listExamples.ListBox.ItemIndex, 6];
  btnCC65.Enabled := listings[listExamples.ListBox.ItemIndex, 7];

  if btnDoubleRes.Checked then begin
    pmResolution := doubleResolution;
    pmRes := 'P/M graphics double resolution';
    playerPageSize := '8';
    PMBASEStart := '384';
    SDMCTL := '46';
    PMBASE := '512';
    PlayerMemSize := '128';
  end
  else begin
    pmResolution := singleResolution;
    pmRes := 'P/M graphics single resolution';
    playerPageSize := '16';
    PMBASEStart := '768';
    SDMCTL := '62';
    PMBASE := '1024';
    PlayerMemSize := '256';
  end;

  CreateCode;
end;

procedure TfrmAnimGen.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmAnimGen.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

end.

