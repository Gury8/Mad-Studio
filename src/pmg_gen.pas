{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Player/missile graphics editor - source code generator
}
unit pmg_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, lcltype, BCMDButton, BCListBox, BCMaterialDesignButton, strutils,
  common;

type
  { TfrmPmgGen }
  TfrmPmgGen = class(TForm)
    boxPmResolution : TGroupBox;
    btnCopyToEditor: TBCMaterialDesignButton;
    btnClose: TBCMaterialDesignButton;
    Bevel1: TBevel;
    btnAtariBASIC: TBCMDButton;
    btnDoubleRes : TBCMDButton;
    btnSingleRes : TBCMDButton;
    btnTurboBasicXL: TBCMDButton;
    btnMadPascal: TBCMDButton;
    btnFastBasic: TBCMDButton;
    btnKickC: TBCMDButton;
    btnEffectus: TBCMDButton;
    BCPaperPanel1: TBCPaperPanel;
    chkMissilePosDefault: TCheckBox;
    chkPlayerPosDefault: TCheckBox;
    editLineStep: TSpinEditEx;
    editMissilePosX: TSpinEditEx;
    pmColorDec: TSpinEditEx;
    editMissilePosY: TSpinEditEx;
    editStartLine: TSpinEditEx;
    editPosX: TSpinEditEx;
    editPosY: TSpinEditEx;
    listExamples: TBCPaperListBox;
    chkPlayer4Data: TCheckBox;
    chkPriority: TCheckGroup;
    chkPlayers: TCheckGroup;
    chkMissiles: TCheckGroup;
    cmbMissileSize: TComboBox;
    cmbSize: TComboBox;
    editColorHex: TLabel;
    boxPm: TGroupBox;
    boxStartLine: TGroupBox;
    lblLineStep: TLabel;
    lblPmColorReg: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    lblPlayerSize: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    lblMissileSize: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    lblStartLine: TLabel;
    memo: TMemo;
    pmColor: TShape;
    radPm: TRadioGroup;
    radDataType: TRadioGroup;
    lblLanguage: TStaticText;
    lblCode: TStaticText;
    lblExamples: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure chkMissilePosDefaultChange(Sender: TObject);
    procedure chkPlayerPosDefaultChange(Sender: TObject);
    procedure editMissilePosXChange(Sender: TObject);
    procedure editMissilePosYChange(Sender: TObject);
    procedure editPosYChange(Sender: TObject);
    procedure CreateCodeProc(Sender: TObject);
    procedure chkPriorityItem(Sender: TObject; Index: integer);
    procedure CloseWin(Sender: TObject);
    procedure CopyToEditor(Sender: TObject);
    procedure chkPlayersItem(Sender: TObject; Index: integer);
    procedure cmbMisileSizeChange(Sender: TObject);
    procedure cmbSizeChange(Sender: TObject);
    procedure editPosXChange(Sender: TObject);
    procedure listExamplesProc(Sender: TObject);
    procedure btnSingleResClick(Sender : TObject);
    procedure btnDoubleResClick(Sender : TObject);
    procedure pmColorDecChange(Sender: TObject);
    procedure pmColorDecClick(Sender : TObject);
    procedure pmColorDecEnter(Sender : TObject);
    procedure pmColorDecMouseEnter(Sender : TObject);
    procedure radLangProc(Sender: TObject);
    procedure radPmProc(Sender: TObject);
    procedure ButtonHoverEnter(Sender: TObject);
    procedure ButtonHoverLeave(Sender: TObject);
  private
    { private declarations }
    listings: TListings;
    playerPageSize: string;
    PlayerMemSize: string;
    PMBASEStart: string;
    SDMCTL: string;
    PMBASE: string;
    pmRes : string;
    isCreate : boolean;
    isColorDecChange : boolean;
    isEditPosXChange : boolean;
    procedure CreateCode;
    function SetValues(index: byte): string;
    function SetMissileValues(index: byte): string;
    procedure SetPriority;
    function CheckPriority: byte;
    procedure SetPlayersPos;
    function Example01: string;
    function Example02: string;
    function Example03: string;
    function Example04: string;
  end;

var
  frmPmgGen: TfrmPmgGen;

implementation

{$R *.lfm}

uses
  pmg, src_editor, lib, code_lib;

{ TfrmPmgGen }

procedure TfrmPmgGen.FormCreate(Sender: TObject);
begin
  isCreate := true;
  isColorDecChange := false;
  isEditPosXChange := false;

  SetListings(listings);

  // Example 4
  listings[3, 8] := False;

  // Example 5
  listings[4, 8] := False;

  // Example 6
  listings[5, 8] := False;

  if pmResolution = singleResolution then
//    cmbResolution.ItemIndex := 0
    btnSingleRes.Checked := true
  else
    btnDoubleRes.Checked := true

  //  editPosX.Value := frmPmg.arrPosX[0];  //frmPmg.playerIndex[0] + 80;
  //  editPosY.Value := frmPmg.arrPosY[0];  //frmPmg.playerIndex[0] + 80;
  //  if frmPmg.tabs.TabIndex = 1 then begin
  //    SetPlayersPos(true);
  //frmPmg.arrPosX[0] := frmPmg.playerIndex[0] + 80;
  //frmPmg.arrPosX[1] := frmPmg.playerIndex[1] + 80;
  //frmPmg.arrPosX[2] := frmPmg.playerIndex[2] + 80;
  //frmPmg.arrPosX[3] := frmPmg.playerIndex[3] + 80;
  //  end;
  //  FillByte(arrPosY, SizeOf(arrPosY), 40);
  //  FillByte(arrMissilePosX, SizeOf(arrMissilePosX), 110);
  //  FillByte(arrMissilePosY, SizeOf(arrMissilePosY), 100);

  //  for i := 0 to 3 do begin
  //    frmPmg.arrPosY[i] := 40;
  //    frmPmg.arrMissilePosX[i] := 110 + 2*i;
  //    frmPmg.arrMissilePosY[i] := 60 + 10*i;
  ////    playerSize[i] := _PLAYER_SIZE_NORMAL;
  ////    missileSizes[i] := _PLAYER_SIZE_NORMAL;
  //  end;
end;

procedure TfrmPmgGen.FormShow(Sender: TObject);
var
  i, j, player: byte;
begin
  FormStyle := fsSystemStayOnTop;
  //  tabs.TabIndex := langIndex;
  //  playerTabs.TabIndex := 0;
  //  isAutoEditPosX0 := true;

//  chkPlayerPosDefaultChange(Sender);
//  chkMissilePosDefaultChange(Sender);

  isCreate := false;

  radPmProc(Sender);

  for player := 0 to 3 do begin
    //    playerSize[player] := [normalSize];
    for i := 0 to frmPmg.missileMaxY - 1 do begin
      if frmPmg.missileData[player, i] > 0 then begin
        chkMissiles.Checked[player] := True;
        break;
      end;
    end;
    for i := 0 to _PM_MAX_LINES - 1 do begin
      for j := 0 to 7 do
        if frmPmg.fld[player, j, i] > 0 then begin
          chkPlayers.Checked[player] := True;
          break;
        end;
    end;
  end;

  chkPriority.Checked[3] := isPmMixedColor;
  //  FillByte(missileSizes, SizeOf(missileSizes), 0);

  langIndex := 0;

  case frmPmg.tabs.TabIndex of
    0: listExamples.ListBox.ItemIndex := 0;
    1: listExamples.ListBox.ItemIndex := 5;
    2: listExamples.ListBox.ItemIndex := 1;
  end;

  listExamplesProc(Sender);
end;

procedure TfrmPmgGen.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmPmgGen.editMissilePosXChange(Sender: TObject);
begin
  if not isEditPosXChange then exit;
  frmPmg.arrMissilePosX[radPM.ItemIndex] := editMissilePosX.Value;
  CreateCode;
end;

procedure TfrmPmgGen.editMissilePosYChange(Sender: TObject);
begin
  if not isEditPosXChange then exit;
  frmPmg.arrMissilePosY[radPM.ItemIndex] := editMissilePosY.Value;
  CreateCode;
end;

procedure TfrmPmgGen.editPosYChange(Sender: TObject);
begin
  if not isEditPosXChange then exit;
  frmPmg.arrPosY[radPM.ItemIndex] := editPosY.Value;
  CreateCode;
end;

procedure TfrmPmgGen.chkPlayerPosDefaultChange(Sender: TObject);
var
  i: byte;
begin
//  showmessage('chkPlayerPosDefaultChange');
  if chkPlayerPosDefault.Checked then
    for i := 0 to 3 do begin
      frmPmg.arrPosX[i] := 80 + 10 * i;
      frmPmg.arrPosY[i] := 40;
    end;

  editPosX.Value := frmPmg.arrPosX[radPM.ItemIndex];
  editPosY.Value := frmPmg.arrPosY[radPM.ItemIndex];

//  if not isCreate then
    CreateCode;
end;

procedure TfrmPmgGen.chkMissilePosDefaultChange(Sender: TObject);
var
  i: byte;
begin
  if chkMissilePosDefault.Checked then
    for i := 0 to 3 do begin
      frmPmg.arrMissilePosX[i] := 110 + 2 * i;
      frmPmg.arrMissilePosY[i] := 60 + 10 * i;
    end;

  editMissilePosX.Value := frmPmg.arrMissilePosX[radPM.ItemIndex];
  editMissilePosY.Value := frmPmg.arrMissilePosY[radPM.ItemIndex];

//  if not isCreate then
    CreateCode;
end;

procedure TfrmPmgGen.SetPlayersPos;
var
  i: byte;
begin
  for i := 0 to 3 do begin
    //  if oldValues then begin
    //      arrPosX_old[i] := frmPmg.arrPosX[i];
    frmPmg.arrPosX[i] := frmPmg.playerIndex[i] + 80;
    //    end
    //    else begin
    ////      showmessage('old * ' + inttostr(arrPosX_old[i]));
    //      frmPmg.arrPosX[i] := arrPosX_old[i];
    //    end;
  end;

  //  chkPlayersPosDefault.checked := oldValues;
end;

//procedure TfrmPmgGen.SetMissilesPosOld;
//var
//  i : byte;
//begin
//  for i := 0 to 3 do begin
//    arrMissilePosX_old[i] := frmPmg.arrMissilePosX[i];
//    arrMissilePosY_old[i] := frmPmg.arrMissilePosY[i];
//  end;
//end;

procedure TfrmPmgGen.chkPriorityItem(Sender: TObject; Index: integer);
begin
  if index = 0 then
    chkPriority.Checked[1] := False
  else if index = 1 then
    chkPriority.Checked[0] := False;

  //if index = 2 then
  //  chkPriority.Checked[3] := false;
  //else if index = 3 then begin
  //  chkPriority.Checked[2] := false;
  //  if chkPriority.Checked[3] then begin
  //    //editMissilePosX.Value := frmPmg.playerIndex[1] + 80;
  //    //editPosX2.Value := frmPmg.playerIndex[2] + 80;
  //    //editPosX3.Value := frmPmg.playerIndex[3] + 80;
  //    //radPM.ItemIndex + 4
  //    //
  //    //editPosX.Value := frmPmg.playerIndex[0] + 80;
  //    //arrPosX[0] := editPosX.Value;
  //    //arrPosX[1] := frmPmg.playerIndex[1] + 80;
  //    //arrPosX[2] := frmPmg.playerIndex[2] + 80;
  //    //arrPosX[3] := frmPmg.playerIndex[3] + 80;
  //  end;
  //end;

  CreateCode;
end;

procedure TfrmPmgGen.CopyToEditor(Sender: TObject);
begin
  if not CheckEditor then Exit;

  frmSrcEdit.Show;

  //  memo.Lines.TextLineBreakStyle:=#$9b;
  //  memo.Lines.LineBreak:=#$9b;
  //  frmSrcEdit.editor.Lines.LineBreak:=#$9b;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor(Sender);
  Close;
end;

procedure TfrmPmgGen.chkPlayersItem(Sender: TObject; Index: integer);
begin
  CreateCode;
end;

procedure TfrmPmgGen.cmbSizeChange(Sender: TObject);
begin
  //  (Sender as TComboBox).Tag
  //showmessage(inttostr(cmbSize.ItemIndex) + ' * ' + inttostr(playerSize[radPM.ItemIndex]));
  case cmbSize.ItemIndex of
    0: playerSize[radPM.ItemIndex] := _PLAYER_SIZE_NORMAL;
    1: playerSize[radPM.ItemIndex] := _PLAYER_SIZE_DOUBLE;
    2: playerSize[radPM.ItemIndex] := _PLAYER_SIZE_QUADRUPLE;
  end;
  CreateCode;
end;

procedure TfrmPmgGen.cmbMisileSizeChange(Sender: TObject);
begin
  case cmbMissileSize.ItemIndex of
    0: missileSizes[radPM.ItemIndex] := 0;
    1: missileSizes[radPM.ItemIndex] := 1;
    2: missileSizes[radPM.ItemIndex] := 2;
  end;
  CreateCode;
end;

procedure TfrmPmgGen.editPosXChange(Sender: TObject);
begin
  //if isAutoEditPosX0 then
  if not isEditPosXChange then exit;
//  if editPosX.Focused then begin
//    showmessage('editPosXChange');
    frmPmg.arrPosX[radPM.ItemIndex] := editPosX.Value;
    CreateCode;
  //end;
end;

procedure TfrmPmgGen.listExamplesProc(Sender: TObject);
begin
  if listExamples.ListBox.ItemIndex < 0 then exit;

  boxPm.Enabled := listExamples.ListBox.ItemIndex > 2;
  boxPm.Visible := boxPm.Enabled;

  boxPmResolution.Enabled := boxPm.Enabled;
  boxPmResolution.Visible := boxPm.Enabled;

  radPm.Enabled := boxPm.Enabled;
  radPm.Visible := boxPm.Enabled;

  chkPriority.Enabled := boxPm.Enabled;
  chkPriority.Visible := boxPm.Enabled;

  btnAtariBASIC.Enabled := listings[listExamples.ListBox.ItemIndex, 0];
  btnTurboBasicXL.Enabled := listings[listExamples.ListBox.ItemIndex, 1];
  btnMadPascal.Enabled := listings[listExamples.ListBox.ItemIndex, 2];
  btnEffectus.Enabled := listings[listExamples.ListBox.ItemIndex, 3];
  btnFastBasic.Enabled := listings[listExamples.ListBox.ItemIndex, 4];
  btnKickC.Enabled := listings[listExamples.ListBox.ItemIndex, 8];

  CreateCode;
end;

//procedure TfrmPmgGen.pmColorDecUp(Sender : TObject;
//  Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
//begin
//  colorValues[radPM.ItemIndex + 4] := pmColorDec.Value;
//  pmColor.Brush.Color := colorMem[pmColorDec.Value div 2];
//  editColorHex.Caption := '$' + Dec2Hex(pmColorDec.Value);
//  CreateCode;
//end;

procedure TfrmPmgGen.pmColorDecChange(Sender: TObject);
begin
  if not isColorDecChange then exit;
  colorValues[radPM.ItemIndex + 4] := pmColorDec.Value;
  pmColor.Brush.Color := colorMem[pmColorDec.Value div 2];
  editColorHex.Caption := '$' + Dec2Hex(pmColorDec.Value);
  CreateCode;
end;

procedure TfrmPmgGen.pmColorDecClick(Sender : TObject);
begin
end;

procedure TfrmPmgGen.pmColorDecEnter(Sender : TObject);
begin
end;

procedure TfrmPmgGen.pmColorDecMouseEnter(Sender : TObject);
begin
end;

procedure TfrmPmgGen.radLangProc(Sender: TObject);
begin
  //langIndex := radLang.ItemIndex;
  //if langIndex = 0 then
  //  radDataType.ItemIndex := 0;

  //  (Sender as TComboBox).Tag
  langIndex := (Sender as TBCMDButton).Tag;
  CreateCode;
end;

procedure TfrmPmgGen.radPmProc(Sender: TObject);
begin
//  showmessage('radPmProc');
  isColorDecChange := false;
  isEditPosXChange := false;
  //  boxPm.Caption := 'Player/missile ' + IntToStr(radPm.ItemIndex) + ' properties';
  lblPmColorReg.Caption := 'Color register 70' + IntToStr(radPm.ItemIndex + 4) +
                           ' ($2C' + IntToStr(radPm.ItemIndex + 4) + ')';
  pmColor.Brush.Color := colTab[radPM.ItemIndex + 4];
  pmColorDec.Value := colorValues[radPM.ItemIndex + 4];
  editColorHex.Caption := '$' + Dec2Hex(pmColorDec.Value);
  editPosX.Value := frmPmg.arrPosX[radPM.ItemIndex];
  editPosY.Value := frmPmg.arrPosY[radPM.ItemIndex];
  editMissilePosX.Value := frmPmg.arrMissilePosX[radPM.ItemIndex];
  editMissilePosY.Value := frmPmg.arrMissilePosY[radPM.ItemIndex];

  if playerSize[radPM.ItemIndex] = _PLAYER_SIZE_NORMAL then
    cmbSize.ItemIndex := 0
  else if playerSize[radPM.ItemIndex] = _PLAYER_SIZE_DOUBLE then
    cmbSize.ItemIndex := 1
  else
    cmbSize.ItemIndex := 2;

  if missileSizes[radPM.ItemIndex] = 0 then
    cmbMissileSize.ItemIndex := 0
  else if missileSizes[radPM.ItemIndex] = 1 then
    cmbMissileSize.ItemIndex := 1
  else
    cmbMissileSize.ItemIndex := 2;

  isColorDecChange := true;
  isEditPosXChange := true;
end;

function TfrmPmgGen.SetValues(index: byte): string;
var
  x, y: byte;
  line, values: string;
begin
  for y := 0 to _PM_MAX_LINES - 1 do begin
    line := '';
    for x := 0 to 7 do
      line += IntToStr(frmPmg.fld[index, x, y]);

    case radDataType.ItemIndex of
      0: line := IntToStr(bin2dec(line));
      1: line := '$' + dec2hex(bin2dec(line));
      2: line := '%' + line;
    end;

    if y = 0 then
      values := line;

    if Trim(line) = '$' then
      line := '0';

    if Trim(values) = '$' then
      values := '0';

    if y > 0 then begin
      if langIndex = _ACTION then
        values += ' ' + line
      else begin
        if langIndex < 2 then
          values += ',' + line
        else
          values += ', ' + line;
      end;
    end;
  end;

  Result := values;
end;

function TfrmPmgGen.SetMissileValues(index: byte): string;
var
  y: byte;
  Value: byte;
  line, values: string;
begin
  for y := 0 to frmPmg.missileMaxY - 1 do begin
    // ORing all missiles together
    if index = 255 then begin
      Value := frmPmg.missileData[0, y] or
        frmPmg.missileData[1, y] or frmPmg.missileData[2, y] or
        frmPmg.missileData[3, y];
    end
    // Separate missiles
    else begin
      Value := frmPmg.missileData[index, y];
      //case radDataType.ItemIndex of
      //  0: line := IntToStr(frmPmg.missileData[index, y]);  //inttostr(bin2dec(line));
      //  1: line := '$' + dec2hex(frmPmg.missileData[index, y]);
      //  2: line := '%' + IntToBin(frmPmg.missileData[index, y], 8);
      //end;
    end;

    case radDataType.ItemIndex of
      0: line := IntToStr(Value);
      1: line := '$' + dec2hex(Value);
      2: line := '%' + IntToBin(Value, 8);
    end;

    if y = 0 then
      values := line;

    if Trim(line) = '$' then
      line := '0';
    if Trim(values) = '$' then
      values := '0';

    if y > 0 then begin
      if langIndex = _ACTION then
        values += ' ' + line
      else begin
        if langIndex < 2 then
          values += ',' + line
        else
          values += ', ' + line;
      end;
    end;
  end;

  Result := values;
end;

procedure TfrmPmgGen.CreateCode;
var
  code: string;
begin
  Set01(boxStartLine, langIndex, radDataType, True);

  SetPriority;
  if pmResolution = singleResolution then begin
    pmRes := 'P/M graphics single resolution';
    playerPageSize := '16';
    PMBASEStart := '768';
    SDMCTL := '62';
    PMBASE := '1024';
    PlayerMemSize := '256';
  end
  else begin
    pmRes := 'P/M graphics double resolution';
    playerPageSize := '8';
    PMBASEStart := '384';
    SDMCTL := '46';
    PMBASE := '512';
    PlayerMemSize := '128';
  end;

  case listExamples.ListBox.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
    3: code := Example04;
    4: begin
      chkMissilePosDefault.Checked := False;
      code := Example04;
    end;
    5: begin
      //         SetPlayersPos(true);
      chkPlayerPosDefault.Checked := False;
      code := Example04;
    end;
  end;

  Set02(memo, code);
end;

procedure TfrmPmgGen.SetPriority;
var
  i: byte;
begin
  for i := 0 to 3 do
    chkPriority.Checked[i] := False;

  case listExamples.ListBox.ItemIndex of
    3: chkPriority.Checked[0] := True;
    4:
    begin
      chkPriority.Checked[0] := True;
      chkPriority.Checked[2] := True;
    end;
    5:
    begin
      chkPriority.Checked[0] := True;
      chkPriority.Checked[3] := True;
    end;
  end;
end;

function TfrmPmgGen.CheckPriority : byte;
var
  i: byte;
begin
  Result := 0;
  for i := 0 to 3 do
    if chkPriority.Checked[i] then
      Result := Result or GPRIOR[i];
end;

function TfrmPmgGen.Example01 : string;
var
  i: byte;
begin
  code.line := '';

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex < 2) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    for i := 0 to 3 do
      if chkPlayers.Checked[i] then
        //        code += IntToStr((i + 1)*100) + ' DATA ' + SetValues(i) + #13#10;
        code.line += CodeLine('DATA ' + SetValues(i));
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    for i := 0 to 3 do
      if chkPlayers.Checked[i] then
        code.line += 'BYTE ARRAY'#13#10 +
                     '  PLAYER' +
          IntToStr(i) + ' = [' + SetValues(i) + ']'#13#10;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    //    code := 'const'#13#10;
    for i := 0 to 3 do
      if chkPlayers.Checked[i] then
        code.line += '  player' + IntToStr(i) + ' : array[0..' +
                     IntToStr(_PM_MAX_LINES - 1) + '] of byte ='#13#10 +
                     '    (' + SetValues(i) + ');'#13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    for i := 0 to 3 do
      if chkPlayers.Checked[i] then
        code.line += 'DATA player' + IntToStr(i) + '() BYTE = ' + SetValues(i) + #13#10;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    for i := 0 to 3 do
      if chkPlayers.Checked[i] then
        code.line += 'const char player' + IntToStr(i) + '[] = {' +
          SetValues(i) + '};'#13#10;
  end;

  Result := code.line;
end;

function TfrmPmgGen.Example02 : string;
var
  i: byte;
begin
  code.line := '';

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex < 2) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    for i := 0 to 3 do begin
      if chkMissiles.Checked[i] then begin
        //        code += IntToStr(30 + 2*i) + ' REM Missile ' + IntToStr(i) + ' data'#13#10 +
        //                IntToStr(31 + 2*i) + ' DATA ' + SetMissileValues(i) + #13#10;
        code.line += CodeLine('REM Missile ' + IntToStr(i) + ' data');
        code.line += CodeLine('DATA ' + SetMissileValues(i));
      end;
    end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    //    code := '; MISSILE DATA'#13#10;
    for i := 0 to 3 do begin
      if chkMissiles.Checked[i] then
        code.line += 'BYTE ARRAY MISSILE' + IntToStr(i) + '=[' +
          SetMissileValues(i) + ']'#13#10;
    end;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    //    code := 'const'#13#10 +
    //            '  // Missile data'#13#10;
    for i := 0 to 3 do
      if chkMissiles.Checked[i] then
        code.line += '  missile' + IntToStr(i) + ' : array[0..' +
          IntToStr(frmPmg.missileMaxY - 1) + '] of byte ='#13#10 +
          '    (' + SetMissileValues(i) + ');'#13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    for i := 0 to 3 do begin
      if chkMissiles.Checked[i] then
        code.line += 'DATA missile' + IntToStr(i) + '() BYTE = ' +
          SetMissileValues(i) + #13#10;
    end;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    for i := 0 to 3 do begin
      if chkMissiles.Checked[i] then
        code.line += 'const char missile' + IntToStr(i) +
          '[] = {' + SetMissileValues(i) + '};'#13#10;
    end;
  end;

  Result := code.line;
end;

function TfrmPmgGen.Example03 : string;
begin
  code.line := '';

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex < 2) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    //    code := IntToStr(30) + ' REM Player 4 data'#13#10 +
    //            IntToStr(31) + ' DATA ' + SetMissileValues(255) + #13#10;
    code.line += CodeLine('REM Player 4 data');
    code.line += CodeLine('DATA ' + SetMissileValues(255));
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := '; Player 4 data'#13#10 +
                 'BYTE ARRAY player4 = [' + SetMissileValues(255) + ']'#13#10;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    //    code := 'const'#13#10 +
    code.line := '  // Player 4 data'#13#10 +
                 '  player4 : array [0..' + IntToStr(frmPmg.missileMaxY - 1) + '] of byte = ' +
                 '('#13#10 +
                 '    ' + SetMissileValues(255) + ');'#13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := ''' Player 4 data'#13#10 +
                 'DATA player4() BYTE = ' + SetMissileValues(255) + #13#10;
  end
  { KickC
   ---------------------------------------------------------------------------}
  else if langIndex = _KICKC then begin
    code.line := '// Player 4 data'#13#10 +
                 'const char player4[] = {' + SetMissileValues(255) + '};'#13#10;
  end;

  Result := code.line;
end;

function TfrmPmgGen.Example04 : string;
var
  value623 : byte;
  isPlayer : boolean = False;
  isMissile : boolean = False;
  mPosY : string = '0';
  missileDef : string = '';
  SIZEM : byte;
  i : byte;
  rtnCodeNum : word = 20000;       // ML starting Atari BASIC line number
  charDataCodeNum : word = 30000;  // Character data starting Atari BASIC line number
begin
  value623 := CheckPriority;
  editPosX.Enabled := True;

  //if listExamples.ItemIndex = 3 then begin  // in [3, 5]
  //  frmPmg.arrMissilePosY[1] := frmPmg.arrMissilePosY[0] + 10;
  //  frmPmg.arrMissilePosY[2] := frmPmg.arrMissilePosY[0] + 20;
  //  frmPmg.arrMissilePosY[3] := frmPmg.arrMissilePosY[0] + 30;
  //end
  if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then begin
    //if not chkMissilePosDefault.Checked then begin
    //  frmPmg.arrMissilePosX[0] := 110;
    //  frmPmg.arrMissilePosY[0] := 60;
    //end;

    //    isAutoEditPosX0 := false;
    //    frmPmg.arrMissilePosX[0] := editMissilePosX.Value;
    frmPmg.arrMissilePosX[1] := frmPmg.arrMissilePosX[0] + 2;
    frmPmg.arrMissilePosX[2] := frmPmg.arrMissilePosX[0] + 4;
    frmPmg.arrMissilePosX[3] := frmPmg.arrMissilePosX[0] + 6;
    //    frmPmg.arrMissilePosY[0] := editMissilePosY.Value;
    frmPmg.arrMissilePosY[1] := frmPmg.arrMissilePosY[0];
    frmPmg.arrMissilePosY[2] := frmPmg.arrMissilePosY[0];
    frmPmg.arrMissilePosY[3] := frmPmg.arrMissilePosY[0];
  end
  else if listExamples.ListBox.ItemIndex = 5 then begin
    editPosX.Enabled := False;
    for i := 0 to 3 do
      frmPmg.arrPosX[i] := frmPmg.playerIndex[i] + 80;
  end;

  if chkPlayers.Checked[0] or
     chkPlayers.Checked[1] or
     chkPlayers.Checked[2] or
     chkPlayers.Checked[3] then
  begin
    isPlayer := True;
  end;

  if chkMissiles.Checked[0] or chkMissiles.Checked[1] or
    chkMissiles.Checked[2] or chkMissiles.Checked[3] then
  begin
    isMissile := True;
    //_PLAYER_SIZE_NORMAL = 0;
    //_PLAYER_SIZE_DOUBLE = 1;
    //_PLAYER_SIZE_QUADRUPLE = 3;
    for i := 0 to 3 do begin
      case missileSizes[i] of
        0: missileDef += 'N' + IntToStr(i);
        1: missileDef += 'D' + IntToStr(i);
        2: missileDef += 'Q' + IntToStr(i);
      end;
    end;
    for i := 0 to 255 do begin
      if arrMissileSizes[i] = missileDef then begin
        SIZEM := i;
        break;
      end;
    end;
  end;

  { Atari BASIC/Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (langIndex < 2) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine(_REM) +
                 CodeLine('REM' + _REM_MAD_STUDIO) +
                 CodeLine('REM P/M Graphics demonstration') +
                 CodeLine(_REM) +
                 CodeLine('DIM MLCODE$(33)') +
                 CodeLine('GOSUB ' + IntToStr(rtnCodeNum));

    if isPlayer then begin
      code.line += CodeLine('REM Player position');
      for i := 0 to 3 do begin
        if chkPlayers.Checked[i] then
          code.line += CodeLine('PX' + IntToStr(i) + '=' +
                       IntToStr(frmPmg.arrPosX[i]) + ':PY' + IntToStr(i) +
                       '=' + IntToStr(frmPmg.arrPosY[i]));
      end;
    end;

    if isMissile then begin
      code.line += CodeLine('REM Missile position');
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then begin
          code.line += CodeLine('MX' + IntToStr(i) + '=' +
                       IntToStr(frmPmg.arrMissilePosX[i]) + ':MY' +
                       IntToStr(i) + '=' + IntToStr(frmPmg.arrMissilePosY[i]));
          if mPosY = '0' then
            mPosY := 'MY' + IntToStr(i);
        end;
    end;

    code.line += CodeLine('REM Initialize P/M graphics') +
                 CodeLine('POKE 53277,0') + CodeLine('GRAPHICS 0') +
                 CodeLine('POKE 710,0:POKE 712,0') +
                 CodeLine('? "Set P/M graphics"') +
                 CodeLine('PMGMEM=PEEK(106)-' + playerPageSize) +
                 CodeLine('POKE 54279,PMGMEM') +
                 CodeLine('PMGMEM=PMGMEM*256') +
                 CodeLine('REM ' + pmRes) +
                 CodeLine('POKE 559,' + SDMCTL);

    if isPlayer then begin
      //code.line += CodeLine('? "Clear player memory"');
      //code.line += CodeLine('FOR I=0 TO ' + PMBASE + '+' + PlayerMemSize + '-1:? ".";');
      //code.line += CodeLine('POKE PMGMEM+' + PMBASEStart + '+I,0:NEXT I');
      code.line += CodeLine('REM CALL MACHINE LANGUAGE ROUTINE');
      code.line += CodeLine('X=USR(ADR(MLCODE$),1536,PEEK(PMGMEM),' +
                            IntToStr(StrToInt(playerPageSize) div 2) + ')');
      if code.number > 30000 then
        Inc(charDataCodeNum, 1000);
    end;

    //if chkPriority.Checked[0] then
    //  code += '175 REM Priority register - P/M in front of all playfield'#13#10 +
    //          '177 POKE 623,' + IntToStr(value623) + #13#10
    //else if chkPriority.Checked[1] then begin
    //  code += '175 REM Priority register - P/M behind all playfield'#13#10 +
    //          '177 POKE 623,' + IntToStr(value623) + #13#10;
    //end;

    // Priority register settings
    code.line += CodeLine('REM Priority register');
    for i := 0 to 3 do begin
      if chkPriority.Checked[i] then
        code.line += CodeLine('REM - ' + chkPriority.Items[i]);
    end;

    code.line += CodeLine('POKE 623,' + IntToStr(value623));

    for i := 0 to 3 do begin
      if chkPlayers.Checked[i] then begin
        code.line += CodeLine('POKE ' + IntToStr(53256 + i) +
          ',' + IntToStr(playerSize[i]) +
          ':REM Player ' + IntToStr(i) + ' ' + pmSize[playerSize[i]]);
        //if playerSizes[i] = _PLAYER_SIZE_NORMAL then
        //  code += IntToStr(170 + 2*i) + ' POKE ' + IntToStr(53256 + i) +
        //          ',0:REM Player ' + IntToStr(i) + ' Normal size'#13#10
        //else if playerSizes[i] = _PLAYER_SIZE_DOUBLE then
        //  code += IntToStr(170 + 2*i) + ' POKE ' + IntToStr(53256 + i) +
        //          ',1:REM Player ' + IntToStr(i) + ' Double size'#13#10
        //else if playerSizes[i] = _PLAYER_SIZE_QUADRUPLE then
        //  code += IntToStr(170 + 2*i) + ' POKE ' + IntToStr(53256 + i) +
        //          ',3:REM Player ' + IntToStr(i) + ' Quadruple size'#13#10;
      end;
    end;

    for i := 0 to chkMissiles.ControlCount - 1 do begin
      if chkMissiles.Checked[i] then begin
        code.line += CodeLine('REM Missile size');
        code.line += CodeLine('POKE 53260,' + IntToStr(SIZEM));
        code.line += CodeLine('REM Player/missile color');
        break;
      end;
    end;
    for i := 0 to 3 do begin
      if chkPlayers.Checked[i] or chkMissiles.Checked[i] then
        code.line += CodeLine('POKE ' + IntToStr(704 + i) + ',' +
          IntToStr(colorValues[i + 4]));
    end;

    if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += CodeLine('POKE 711,' + IntToStr(colorValues[10]));

    if isPlayer then begin
      code.line += CodeLine('REM Player horizontal position');
      for i := 0 to 3 do begin
        if chkPlayers.Checked[i] then
          code.line += CodeLine('POKE ' + IntToStr(53248 + i) + ',PX' + IntToStr(i));
      end;

      for i := 0 to 3 do begin
        if chkPlayers.Checked[i] then begin
          code.line += CodeLine('? "Draw player ' + IntToStr(i) +
                       ' and set vertical pos."');
          code.line += CodeLine('FOR I=0 TO ' + IntToStr(_PM_MAX_LINES - 1));
          //          code.line += CodeLine('? ".";');
          code.line += CodeLine('READ A:POKE PMGMEM+' + PMBASE +
                       '+' + PlayerMemSize + '*' + IntToStr(i) + '+PY' + IntToStr(i) + '+I,A');
          code.line += CodeLine('NEXT I');
        end;
      end;
    end;

    if (listExamples.ListBox.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then begin
          code.line += CodeLine('REM Draw missile ' + IntToStr(i) +
            ' and set vertical pos.');
          code.line += CodeLine('FOR I=0 TO ' + IntToStr(frmPmg.missileMaxY - 1));
          code.line += CodeLine('READ A:POKE PMGMEM+' + PMBASEStart + '+MY' +
                       IntToStr(i) + '+I,A');
          code.line += CodeLine('NEXT I');
        end;
    end
    else if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += CodeLine('REM Draw player 4 and set vertical pos.') +
        CodeLine('FOR I=0 TO ' + IntToStr(frmPmg.missileMaxY - 1)) +
        //                   CodeLine('? ".";') +
        CodeLine('READ A:POKE PMGMEM+' + PMBASEStart + '+' + mPosY + '+I,A') +
        CodeLine('NEXT I');
    end;

    if (listExamples.ListBox.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      if isMissile then
        code.line += CodeLine('REM MISSILE POSITION');

      for i := 0 to 3 do
        code.line += CodeLine('POKE ' + IntToStr(53252 + i) + ',MX' + IntToStr(i));
    end
    else if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += CodeLine('REM PLAYER 4 POSITION');
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then
          code.line += CodeLine('POKE ' + IntToStr(53252 + i) + ',MX' + IntToStr(i));
    end;

    code.line += CodeLine('REM Turn on P/M graphics') +
                 CodeLine('? CHR$(125):POKE 53277,3') +
                 CodeLine('END');

    // Machine language routine
    code.number := rtnCodeNum;
    code.line += SetFastCopyRoutine;
    // Modified character data
    code.number := charDataCodeNum;

    // Player data
    for i := 0 to 3 do begin
      if chkPlayers.Checked[i] then begin
        code.line += CodeLine('REM Player ' + IntToStr(i) + ' data');
        code.line += CodeLine('DATA ' + SetValues(i));
      end;
    end;

    // Missile data
    if (listExamples.ListBox.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then begin
          code.line += CodeLine('REM Missile ' + IntToStr(i) + ' data');
          code.line += CodeLine('DATA ' + SetMissileValues(i));
        end;
    end
    else if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += CodeLine('REM Player 4 data - ORing missile data');
      code.line += CodeLine('DATA ' + SetMissileValues(255));
    end;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if langIndex = _MAD_PASCAL then begin
    code.line := '//' + _REM_MAD_STUDIO + #13#10 +
                 '// Player/missile graphics demonstration'#13#10#13#10;
    code.line += 'uses crt, graph, atari;'#13#10#13#10 +
                 'var'#13#10 +
                 '  ramtop : byte absolute $6A;    // Top of RAM in 256-byte pages'#13#10#13#10 +
                 '  pcolr : array[0..3] of byte absolute $2C0;   // Player color'#13#10 +
                 '  hposp : array[0..3] of byte absolute $D000;  // Player horizontal position'#13#10 +
                 '  sizep : array[0..3] of byte absolute $D008;  // Player size'#13#10;

    if isMissile then
      code.line += '  hposm : array[0..3] of byte absolute $D004;  // Missile horizontal position'#13#10;

    code.line += '  pmgmem : word;'#13#10;

    if isPlayer then begin
      code.line += #13#10'  // Player data'#13#10;
      for i := 0 to 3 do begin
        if chkPlayers.Checked[i] then
          code.line += '  player' + IntToStr(i) + ' : array [0..' +
                       IntToStr(_PM_MAX_LINES - 1) + '] of byte ='#13#10 +
                       '    (' + SetValues(i) + ');'#13#10;
      end;
    end;

    if (listExamples.ListBox.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      if isMissile then begin
        code.line += #13#10'  // Missile data'#13#10;
        for i := 0 to 3 do begin
          if chkMissiles.Checked[i] then
            code.line += '  missile' + IntToStr(i) +
                         ' : array [0..' + IntToStr(frmPmg.missileMaxY - 1) +
                         '] of byte = ' + '('#13#10 + SetMissileValues(i) + ');'#13#10;
        end;
      end;
    end
    else if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += #13#10'  // Player 4 data'#13#10 +
                   '  player4 : array [0..' + IntToStr(frmPmg.missileMaxY - 1) +
                   '] of byte ='#13#10 +
                   '    (' + SetMissileValues(255) + ');'#13#10;
    end;

    if isPlayer then begin
      code.line += #13#10'  // Player position'#13#10;
      for i := 0 to 3 do begin
        if chkPlayers.Checked[i] then
          code.line += '  px' + IntToStr(i) + ' : byte = ' +
                       IntToStr(frmPmg.arrPosX[i]) + ';' + ' py' +
                       IntToStr(i) + ' : byte = ' + IntToStr(frmPmg.arrPosY[i]) + ';'#13#10;
      end;
    end;

    if isMissile then begin
      code.line += #13#10'  // Missile position'#13#10;
      for i := 0 to 3 do begin
        if chkMissiles.Checked[i] then begin
          code.line += '  mx' + IntToStr(i) + ' : byte = ' +
                       IntToStr(frmPmg.arrMissilePosX[i]) + ';' + ' my' +
                       IntToStr(i) + ' : byte = ' + IntToStr(frmPmg.arrMissilePosY[i]) +
                       ';'#13#10;
          if mPosY = '0' then
            mPosY := 'my' + IntToStr(i);
        end;
      end;
    end;

    code.line += #13#10'begin'#13#10 +
                 '  // Initialize P/M graphics'#13#10 +
                 '  InitGraph(0);'#13#10 +
                 '  gractl := 0;'#13#10 +
                 '  color2 := 0; color4 := 0;'#13#10#13#10 +
                 '  // Set P/M graphics'#13#10 +
                 '  pmgmem := ramtop - ' + playerPageSize + ';'#13#10 +
                 '  pmbase := pmgmem;'#13#10 +
                 '  pmgmem := pmgmem shl 8;'#13#10#13#10 +
                 '  // ' + pmRes + #13#10 +
                 '  sdmctl := ' + SDMCTL + ';'#13#10#13#10 +
                 '  // Clear player memory'#13#10 +
                 //            '  POKE(pmgmem + ' + PMBASEStart + ', 0);'#13#10 +
                 //            '  Move(pmgmem + ' + PMBASEStart + ', pmgmem + ' + PMBASEStart + ' + 1, ' +
                 //            PMBASE + ' - 1 + ' + PlayerMemSize + ');'#13#10;
                 '  FillByte(Pointer(pmgmem + ' + PMBASEStart + '), ' +
                 PMBASE + ' + ' + PlayerMemSize + ', 0);'#13#10;

    //    'Zero(pmgmem + ' + PMBASEStart + ', ' + PMBASE + ' + ' + PlayerMemSize + ')'#13#10;

    // Priority register settings
    code.line += #13#10'  // Priority register'#13#10;
    for i := 0 to 3 do begin
      if chkPriority.Checked[i] then
        code.line += '  // - ' + chkPriority.Items[i] + #13#10;
    end;

    code.line += '  gprior := ' + IntToStr(value623) + ';'#13#10;

    for i := 0 to 3 do begin
      if chkPlayers.Checked[i] then begin
        //code += '  POKE(' + IntToStr(53256 + i) +
        //        ', ' + IntToStr(playerSizes[i]) + ');  // Player ' + IntToStr(i) + ' ' +
        //        pmSize[playerSizes[i]] + #13#10;
        code.line += '  sizep[' + IntToStr(i) + '] := ' + IntToStr(playerSize[i]) +
                     ';  // Player ' + IntToStr(i) + ' ' + pmSize[playerSize[i]] + #13#10;
      end;
    end;

    if isMissile then begin
      code.line += #13#10;
      for i := 0 to chkMissiles.ControlCount - 1 do begin
        if chkMissiles.Checked[i] then begin
          code.line += '  // Missile size'#13#10 +
                       '  sizem := ' + IntToStr(SIZEM) + ';'#13#10;
          break;
        end;
      end;
    end;

    if isPlayer or isMissile then begin
      code.line += #13#10'  // Player/missile color'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] or chkMissiles.Checked[i] then begin
          //code += '  POKE(' + IntToStr(704 + i) + ', ' +
          //        IntToStr(colorValues[i + 4]) + ');'#13#10;
          code.line += '  pcolr[' + IntToStr(i) + ']' + ' := ' +
                       IntToStr(colorValues[i + 4]) + ';'#13#10;
        end;
    end;

    if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += '  color3 := ' + IntToStr(colorValues[10]) + ';'#13#10;

    if isPlayer then begin
      code.line += #13#10'  // Player horizontal position'#13#10;
      for i := 0 to 3 do begin
        if chkPlayers.Checked[i] then begin
          //code += '  POKE(' + IntToStr(53248 + i) + ', px' + IntToStr(i) + ');'#13#10;
          code.line += '  hposp[' + IntToStr(i) + '] := px' + IntToStr(i) + ';'#13#10;
        end;
      end;

      code.line += #13#10;
      for i := 0 to 3 do begin
        if chkPlayers.Checked[i] then begin
          code.line += '  // Draw player ' + IntToStr(i) +
                       ' and set vertical position'#13#10 +
                       '  Move(player' + IntToStr(i) + ', Pointer(pmgmem + ' + PMBASE + ' + ' +
                       PlayerMemSize + '*' + IntToStr(i) + ' + py' + IntToStr(i) + '), ' +
                       IntToStr(_PM_MAX_LINES - 1) + ');'#13#10;
        end;
      end;
    end;

    if (listExamples.ListBox.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      for i := 0 to 3 do begin
        if chkMissiles.Checked[i] then
          code.line += '  // Draw missile ' + IntToStr(i) +
                       ' and set vertical position'#13#10 +
                       '  Move(missile' + IntToStr(i) + ', Pointer(pmgmem + ' + PMBASEStart +
                       ' + my' + IntToStr(i) + '), ' +
                       IntToStr(frmPmg.missileMaxY - 1) + ');'#13#10;
      end;
    end
    else if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += '  // Draw player 4 and set vertical position'#13#10 +
                   '  Move(player4, Pointer(PMGMEM + ' + PMBASEStart +
                   ' + ' + mPosY + '), ' + IntToStr(frmPmg.missileMaxY) + ');'#13#10;
    end;

    if (listExamples.ListBox.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      if isMissile then begin
        code.line += #13#10'  // Missile position'#13#10;
        for i := 0 to 3 do begin
          if chkMissiles.Checked[i] then begin
            //code += '  POKE(' + IntToStr(53252 + i) + ', mx' + IntToStr(i) + ');'#13#10;
            code.line += '  hposm[' + IntToStr(i) + '] := mx' + IntToStr(i) + ';'#13#10;
          end;
        end;
      end;
    end
    else if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += #13#10'  // Player 4 position'#13#10;
      for i := 0 to 3 do begin
        if chkMissiles.Checked[i] then
          //code += '  POKE(' + IntToStr(53252 + i) + ', mx' + IntToStr(i) + ');'#13#10;
          code.line += '  hposm[' + IntToStr(i) + '] := mx' + IntToStr(i) + ';'#13#10;
      end;
    end;
    code.line += #13#10'  gractl := 3;'#13#10 +
                 WaitKeyCode(langIndex) + #13#10 +
                 '  // Reset P/M graphics'#13#10 +
                 '  gractl := 0;'#13#10 +
                 'end.';
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if langIndex = _ACTION then begin
    code.line := ';' + _REM_MAD_STUDIO + #13#10 +
                 '; Player/missile graphics demo'#13#10#13#10;
    code.line += 'PROC Main()'#13#10#13#10 +
                 'BYTE pmbase = $D407  ; Player/Missile base address'#13#10 +
                 'BYTE sdmctl = $22F   ; Player resolution'#13#10 +
                 'BYTE gractl = $D01D  ; 1 = missile DMA, 2 = player DMA'#13#10 +
                 'BYTE ramtop = $6A    ; Top of RAM in 256-byte pages'#13#10 +
                 'BYTE sizem = $D00C   ; Size for all missiles set by bits'#13#10 +
                 'BYTE ch = $2FC       ; Keyboard code of last key pressed'#13#10#13#10 +
                 'BYTE ARRAY pcolr(3) = $2C0   ; Player color'#13#10 +
                 'BYTE ARRAY hposp(3) = $D000  ; Player horizontal position'#13#10 +
                 'BYTE ARRAY sizep(3) = $D008  ; Player size'#13#10#13#10;

    if isMissile then
      code.line += 'BYTE ARRAY hposm(3) = $D004  ; Missile horizontal position'#13#10#13#10;

    code.line += 'CARD pmgmem'#13#10;

    if isPlayer then begin
      code.line += #13#10'; Player position'#13#10;
      for i := 0 to 3 do begin
        if chkPlayers.Checked[i] then
          code.line += 'BYTE px' + IntToStr(i) + ' = [' +
            IntToStr(frmPmg.arrPosX[i]) + '], ' + 'py' +
            IntToStr(i) + ' = [' + IntToStr(frmPmg.arrPosY[i]) + ']'#13#10;
      end;
    end;

    if isMissile then begin
      code.line += #13#10'; Missile position'#13#10;
      for i := 0 to 3 do begin
        if chkMissiles.Checked[i] then begin
          code.line += 'BYTE mx' + IntToStr(i) + ' = [' +
            IntToStr(frmPmg.arrMissilePosX[i]) + '], ' + 'my' +
            IntToStr(i) + ' = [' + IntToStr(frmPmg.arrMissilePosY[i]) + ']'#13#10;
          if mPosY = '0' then
            mPosY := 'my' + IntToStr(i);
        end;
      end;
    end;

    if isPlayer then begin
      code.line += #13#10'; Player data'#13#10;
      for i := 0 to 3 do begin
        if chkPlayers.Checked[i] then
          code.line += 'BYTE ARRAY p' + IntToStr(i) + ' = [' +  //#13#10 +
            SetValues(i) + ']'#13#10;
      end;
    end;

    if (listExamples.ListBox.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      if isMissile then begin
        code.line += #13#10'; Missile data'#13#10;
        for i := 0 to 3 do begin
          if chkMissiles.Checked[i] then
            code.line += 'BYTE ARRAY m' + IntToStr(i) + ' = [' +  //#13#10 +
              SetMissileValues(i) + ']'#13#10;
        end;
      end;
    end
    else if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += 'BYTE ARRAY player4 = [' + SetMissileValues(255) + ']'#13#10;

    code.line += #13#10'; Turn off P/M graphics'#13#10 +
                 'gractl = 0'#13#10#13#10 +
                 'Graphics(0)'#13#10 +
                 'Poke(710, 0) Poke(712, 0)'#13#10#13#10 +
                 '; Set P/M graphics'#13#10;

    code.line += 'pmgmem = ramtop - ' + playerPageSize + #13#10 +
                 'pmbase = pmgmem'#13#10 +
                 'pmgmem = pmgmem * 256'#13#10#13#10 +
                 '; ' + pmRes + #13#10 +
                 'sdmctl = ' + SDMCTL + #13#10#13#10 +
                 '; Clear space for players'#13#10 +
                 'Zero(pmgmem + ' + PMBASEStart + ', ' + PMBASE + ' + ' + PlayerMemSize + ')'#13#10;

    //if chkPriority.Checked[0] then
    //  code += '; Priority register - P/M in front of all playfield'#13#10 +
    //          'Poke(623, ' + IntToStr(value623) + ')'
    //else if chkPriority.Checked[1] then begin
    //  code += '; Priority register - P/M behind all playfield'#13#10 +
    //          'Poke(623, ' + IntToStr(value623) + ')'#13#10;
    //end;

    // Priority register settings
    code.line += #13#10'; Priority register'#13#10;
    for i := 0 to 3 do begin
      if chkPriority.Checked[i] then
        code.line += '; - ' + chkPriority.Items[i] + #13#10;
    end;

    code.line += 'Poke(623, ' + IntToStr(value623) + ')'#13#10#13#10;

    for i := 0 to 3 do begin
      if chkPlayers.Checked[i] then begin
        code.line += 'sizep(' + IntToStr(i) + ') = ' + IntToStr(playerSize[i]) +
                     '   ; Player ' + IntToStr(i) + ' ' + pmSize[playerSize[i]] + #13#10;
        //if playerSizes[0] = _PLAYER_SIZE_NORMAL then
        //  code += 'sizep(' + IntToStr(i) + ') = 0   ; Player ' + IntToStr(i) + ' normal size'#13#10
        //else if playerSizes[0] = _PLAYER_SIZE_DOUBLE then
        //  code += 'sizep(' + IntToStr(i) + ') = 1   ; Player ' + IntToStr(i) + ' double size'#13#10
        //else if playerSizes[0] = _PLAYER_SIZE_QUADRUPLE then
        //  code += 'sizep(' + IntToStr(i) + ') = 3   ; Player ' + IntToStr(i) + ' quadruple size'#13#10;
      end;
    end;

    code.line += 'sizem = ' + IntToStr(SIZEM) + #13#10;

    for i := 0 to 3 do begin
      if chkPlayers.Checked[i] then
        code.line += 'pcolr(' + IntToStr(i) + ') = ' + IntToStr(colorValues[i + 4]) +
                     '  ; Player ' + IntToStr(i) + ' color'#13#10;
    end;

    if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += 'Poke(711,' + IntToStr(colorValues[10]) + ')'#13#10;

    if isPlayer then begin
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += 'hposp(' + IntToStr(i) + ') = px' + IntToStr(i) +
                       '  ; Player ' + IntToStr(i) + ' horizontal position'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += '; Draw player ' + IntToStr(i) +
                       ' and set vertical position'#13#10 +
                       'MoveBlock(pmgmem + ' + PMBASE + ' + ' + PlayerMemSize + '*' + IntToStr(i) +
                       ' + py' + IntToStr(i) + ', p' + IntToStr(i) +
                       ', ' + IntToStr(_PM_MAX_LINES - 1) + ')'#13#10;
    end;

    //code += #13#10'; Copy missile data to P/M memory'#13#10 +
    //        '; and set missile Y position'#13#10;
    if (listExamples.ListBox.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then
          code.line += '; Draw missile ' + IntToStr(i) +
                       ' and set vertical position'#13#10 +
                       'MoveBlock(pmgmem + ' + PMBASEStart + ' + my' + IntToStr(i) + ', m' +
                       IntToStr(i) + ', ' + IntToStr(frmPmg.missileMaxY) + ')'#13#10;
    end
    else if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += 'MoveBlock(pmgmem + ' + PMBASEStart + ' + ' +
                   mPosY + ', player4, ' + IntToStr(frmPmg.missileMaxY) + ')'#13#10;

    if isMissile then begin
      code.line += #13#10'; Missile position'#13#10;
      for i := 0 to 3 do begin
        if chkMissiles.Checked[i] then
          code.line += 'hposm(' + IntToStr(i) + ') = mx' + IntToStr(i) + #13#10;
      end;
    end;
    code.line += #13#10'; Turn on P/M graphics'#13#10 +
                 'gractl = 3'#13#10 +
                 WaitKeyCode(_ACTION) + #13#10 +
                 '; Turn off P/M graphics'#13#10 +
                 'gractl = 0'#13#10#13#10 +
                 'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if langIndex = _FAST_BASIC then begin
    code.line := '''' + _REM_MAD_STUDIO + #13#10 +
                 ''' P/M Graphics demonstration'#13#10#13#10;
    if isPlayer then begin
      code.line += ''' Player data'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += 'DATA player' + IntToStr(i) + '() BYTE = ' +
                       SetValues(i) + #13#10;
    end;

    if (listExamples.ListBox.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      if isMissile then begin
        code.line += #13#10''' Missile data'#13#10;
        for i := 0 to 3 do begin
          if chkMissiles.Checked[i] then
            code.line += 'DATA missile' + IntToStr(i) + '() BYTE = ' +
                         SetMissileValues(i) + #13#10;
        end;
      end;
    end
    else if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += 'DATA player4() BYTE = ' + SetMissileValues(255) + #13#10;

    if isPlayer then begin
      code.line += #13#10''' Player position'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += 'px' + IntToStr(i) + ' = ' + IntToStr(frmPmg.arrPosX[i]) +
                       ' : py' + IntToStr(i) + ' = ' +
                       IntToStr(frmPmg.arrPosY[i]) + #13#10;
    end;

    if isMissile then begin
      code.line += #13#10''' Missile position'#13#10;
      for i := 0 to 3 do begin
        if chkMissiles.Checked[i] then begin
          code.line += 'mx' + IntToStr(i) + ' = ' + IntToStr(frmPmg.arrMissilePosX[i]) +
                       ' : my' + IntToStr(i) + ' = ' +
                       IntToStr(frmPmg.arrMissilePosY[i]) + #13#10;
          if mPosY = '0' then
            mPosY := 'my' + IntToStr(i);
        end;
      end;
    end;

    code.line += #13#10''' Initialize P/M graphics'#13#10 +
                 'POKE 53277, 0'#13#10 +
                 'GRAPHICS 0'#13#10 +
                 'POKE 710, 0 : POKE 712, 0'#13#10#13#10 +
                 '? "Set P/M graphics"'#13#10 +
                 'pmgmem = PEEK(106) - ' + playerPageSize + #13#10 +
                 'POKE 54279, pmgmem'#13#10 +
                 'pmgmem = pmgmem * 256'#13#10#13#10 +
                 '''' + pmRes + #13#10 +
                 'POKE 559, ' + SDMCTL + #13#10#13#10 +
                 ''' Clear player memory'#13#10 +
                 'POKE pmgmem + ' + PMBASEStart + ', 0'#13#10 +
                 'MOVE pmgmem + ' + PMBASEStart + ', pmgmem + ' + PMBASEStart + ' + 1, ' +
                 PMBASE + ' - 1 + ' + PlayerMemSize + #13#10;

    // Priority register settings
    code.line += #13#10''' Priority register'#13#10;
    for i := 0 to 3 do begin
      if chkPriority.Checked[i] then
        code.line += ''' - ' + chkPriority.Items[i] + #13#10;
    end;

    code.line += 'POKE 623, ' + IntToStr(value623) + #13#10;
    for i := 0 to 3 do begin
      if chkPlayers.Checked[i] then
        code.line += 'POKE ' + IntToStr(53256 + i) + ', ' +
                     IntToStr(playerSize[i]) + ' : '' Player ' + IntToStr(i) + ' ' +
                     pmSize[playerSize[i]] + #13#10;
    end;

    if isMissile then begin
      code.line += #13#10;
      for i := 0 to chkMissiles.ControlCount - 1 do begin
        if chkMissiles.Checked[i] then begin
          code.line += ''' Missile size'#13#10 +
                       'POKE 53260, ' + IntToStr(SIZEM) + #13#10;
          break;
        end;
      end;
    end;

    if isPlayer or isMissile then begin
      code.line += #13#10''' Player/missile color'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] or chkMissiles.Checked[i] then
          code.line += 'POKE ' + IntToStr(704 + i) + ', ' +
                       IntToStr(colorValues[i + 4]) + #13#10;
    end;

    if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += 'POKE 711, ' + IntToStr(colorValues[10]) + #13#10;

    if isPlayer then begin
      code.line += #13#10''' Player horizontal position'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += 'POKE ' + IntToStr(53248 + i) + ', px' + IntToStr(i) + #13#10;

      code.line += #13#10;
      for i := 0 to 3 do begin
        if chkPlayers.Checked[i] then
          code.line += ''' Draw player ' + IntToStr(i) +
                       ' and set vertical position'#13#10 +
                       'MOVE ADR(player' + IntToStr(i) + '), pmgmem + ' + PMBASE + ' + ' +
                       PlayerMemSize + '*' + IntToStr(i) + ' + py' + IntToStr(i) +
                       ', ' + IntToStr(_PM_MAX_LINES - 1) + #13#10;
      end;
    end;

    if (listExamples.ListBox.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      for i := 0 to 3 do begin
        if chkMissiles.Checked[i] then
          code.line += ''' Draw missile ' + IntToStr(i) +
                       ' and set vertical position'#13#10 +
                       'MOVE ADR(missile' + IntToStr(i) + '), pmgmem + ' + PMBASEStart + ' + my' +
                       IntToStr(i) + ', ' + IntToStr(frmPmg.missileMaxY - 1) + #13#10;
      end;
    end
    else if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += ''' Draw player 4 and set vertical position'#13#10 +
                   'MOVE ADR(player4), PMGMEM + ' + PMBASEStart +
                   ' + ' + mPosY + ', ' + IntToStr(frmPmg.missileMaxY) + #13#10;
    end;

    if (listExamples.ListBox.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      if isMissile then begin
        code.line += #13#10''' Missile position'#13#10;
        for i := 0 to 3 do begin
          if chkMissiles.Checked[i] then
            code.line += 'POKE ' + IntToStr(53252 + i) + ', mx' + IntToStr(i) + #13#10;
        end;
      end;
    end
    else if (listExamples.ListBox.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += #13#10''' Player 4 position'#13#10;
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then
          code.line += 'POKE ' + IntToStr(53252 + i) + ', mx' + IntToStr(i) + #13#10;
    end;
    code.line += #13#10'? "Turn on P/M graphics"'#13#10 +
                 'POKE 53277, 3'#13#10 +
                 WaitKeyCode(_FAST_BASIC);
    code.line += #13#10'? "Turn off P/M graphics"'#13#10 +
                 'POKE 53277, 0'#13#10;
  end
  else
    code.line := '';

  Result := code.line;
end;

procedure TfrmPmgGen.CreateCodeProc(Sender: TObject);
begin
  CreateCode;
end;

procedure TfrmPmgGen.btnDoubleResClick(Sender : TObject);
begin
  pmResolution := doubleResolution;
  CreateCode;
end;

procedure TfrmPmgGen.btnSingleResClick(Sender : TObject);
begin
  pmResolution := singleResolution;
  CreateCode;
end;

procedure TfrmPmgGen.ButtonHoverEnter(Sender: TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, True);
end;

procedure TfrmPmgGen.ButtonHoverLeave(Sender: TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, False);
end;

procedure TfrmPmgGen.CloseWin(Sender: TObject);
begin
  Close;
end;

end.
