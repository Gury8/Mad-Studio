{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Player/missile graphics editor - source code generator
}
unit pmg_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Spin, lcltype, strutils,
  common;

type
  { TfrmPmgGen }
  TfrmPmgGen = class(TForm)
    btnCloseWin: TButton;
    btnCopyToEditor: TButton;
    chkPlayerPosDefault: TCheckBox;
    chkMissilePosDefault: TCheckBox;
    chkPlayer4Data: TCheckBox;
    chkPriority: TCheckGroup;
    chkPlayers: TCheckGroup;
    chkMissiles: TCheckGroup;
    cmbMissileSize: TComboBox;
    cmbResolution: TComboBox;
    cmbSize: TComboBox;
    editColorHex: TLabel;
    editLineStep : TSpinEdit;
    editMissilePosX: TSpinEdit;
    editMissilePosY: TSpinEdit;
    editPosX: TSpinEdit;
    editPosY: TSpinEdit;
    boxPm: TGroupBox;
    editStartLine : TSpinEdit;
    boxStartLine : TGroupBox;
    Label8: TLabel;
    lblLineStep : TLabel;
    lblPmColorReg: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    lblPlayerSize: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    lblMissileSize: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label7: TLabel;
    lblStartLine : TLabel;
    listExamples: TListBox;
    memo: TMemo;
    pmColor: TShape;
    pmColorDec: TSpinEdit;
    radLang: TRadioGroup;
    radPm: TRadioGroup;
    radDataType: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure chkMissilePosDefaultChange(Sender: TObject);
    procedure chkPlayerPosDefaultChange(Sender: TObject);
    procedure editMissilePosXChange(Sender: TObject);
    procedure editMissilePosYChange(Sender: TObject);
    procedure editPosYChange(Sender: TObject);
    procedure editMissilePosXExit(Sender: TObject);
    procedure editMissilePosYExit(Sender: TObject);
    procedure editPosXExit(Sender: TObject);
    procedure editPosYExit(Sender: TObject);
    procedure chkPriorityItem(Sender: TObject; Index: integer);
    procedure CloseWin(Sender: TObject);
    procedure CopyToEditor(Sender: TObject);
    procedure chkPlayersItem(Sender: TObject; Index: integer);
    procedure cmbMisileSizeChange(Sender: TObject);
    procedure cmbSizeChange(Sender: TObject);
    procedure editPosX0Change(Sender: TObject);
    procedure listExamplesProc(Sender: TObject);
    procedure pmColorDecChange(Sender: TObject);
    procedure radLangProc(Sender: TObject);
    procedure radPmProc(Sender: TObject);
//    procedure tabsChange(Sender: TObject);
  private
    { private declarations }
    listings : TListings;
    playerPageSize : string;
    PlayerMemSize : string;
    PMBASEStart : string;
    SDMCTL : string;
    PMBASE : string;
    procedure CreateCode;
    function SetValues(index : byte) : string;
    function SetMissileValues(index : byte) : string;
    procedure SetPriority;
    function CheckPriority : byte;
    procedure SetPlayersPos;
    function Example01 : string;
    function Example02 : string;
    function Example03 : string;
    function Example04 : string;
  public
    { public declarations }
  end;

var
  frmPmgGen: TfrmPmgGen;

implementation

{$R *.lfm}

uses
  pmg, src_editor, lib, code_lib;

{ TfrmPmgGen }

procedure TfrmPmgGen.FormCreate(Sender: TObject);
var
  i : byte;
begin
  for i := 0 to 5 do begin
    listings[i, 0] := true;
    listings[i, 1] := true;
    listings[i, 2] := true;
    listings[i, 3] := true;
    listings[i, 4] := true;
  end;

  if pmResolution = singleResolution then
    cmbResolution.ItemIndex := 0
  else
    cmbResolution.ItemIndex := 1;

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
  i, j, player : byte;
begin
  FormStyle := fsSystemStayOnTop;
//  tabs.TabIndex := langIndex;
//  playerTabs.TabIndex := 0;
//  isAutoEditPosX0 := true;

  radPmProc(Sender);
//  pmColorDec.Value := colorValues[radPM.ItemIndex + 4];
//  pmColor.Brush.Color := colTab[radPM.ItemIndex + 4];
  //pmColorDec1.Value := colorValues[5];
  //pmColor1.Brush.Color := colTab[5];
  //pmColorDec2.Value := colorValues[6];
  //pmColor2.Brush.Color := colTab[6];
  //pmColorDec.Value := colorValues[7];
  //pmColor.Brush.Color := colTab[7];

  for player := 0 to 3 do begin
//    playerSize[player] := [normalSize];
    for i := 0 to frmPmg.missileMaxY - 1 do
      if frmPmg.missileData[player, i] > 0 then begin
        chkMissiles.Checked[player] := true;
        break;
      end;

    for i := 0 to _PM_MAX_LINES - 1 do
      for j := 0 to 7 do
        if frmPmg.fld[player, j, i] > 0 then begin
          chkPlayers.Checked[player] := true;
          break;
        end;
  end;

  chkPriority.Checked[3] := isPmMixedColor;
//  FillByte(missileSizes, SizeOf(missileSizes), 0);
//  tabsChange(Sender);
//  cmbExamplesChange(Sender);

  case frmPmg.tabs.TabIndex of
    0: listExamples.ItemIndex := 0;
    1: listExamples.ItemIndex := 5;
    2: listExamples.ItemIndex := 1;
  end;

  listExamplesProc(Sender);
end;

procedure TfrmPmgGen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmPmgGen.editMissilePosXChange(Sender: TObject);
begin
  frmPmg.arrMissilePosX[radPM.ItemIndex] := editMissilePosX.Value;
  CreateCode;
end;

procedure TfrmPmgGen.editMissilePosYChange(Sender: TObject);
begin
  frmPmg.arrMissilePosY[radPM.ItemIndex] := editMissilePosY.Value;
  CreateCode;
end;

procedure TfrmPmgGen.editPosYChange(Sender: TObject);
begin
  frmPmg.arrPosY[radPM.ItemIndex] := editPosY.Value;
  CreateCode;
end;

//procedure TfrmPmgGen.editStartLineChange(Sender : TObject);
//begin
//  CreateCode;
//end;

procedure TfrmPmgGen.chkPlayerPosDefaultChange(Sender: TObject);
var
  i : byte;
begin
  if chkPlayerPosDefault.Checked then
    for i := 0 to 3 do begin
      frmPmg.arrPosX[i] := 80 + 10*i;
      frmPmg.arrPosY[i] := 40;
    end;

  //else begin
  //  if (frmPmg.tabs.TabIndex = 1) or (listExamples.ItemIndex = 5) then begin
  //    for i := 0 to 3 do begin
  //      frmPmg.arrPosX[i] := frmPmg.playerIndex[i] + 80;
  //    end;
  //  end;
  //end;

  editPosX.Value := frmPmg.arrPosX[radPM.ItemIndex];
  editPosY.Value := frmPmg.arrPosY[radPM.ItemIndex];
  CreateCode;
end;

procedure TfrmPmgGen.chkMissilePosDefaultChange(Sender: TObject);
var
  i : byte;
begin
  if chkMissilePosDefault.Checked then
    for i := 0 to 3 do begin
      frmPmg.arrMissilePosX[i] := 110 + 2*i;
      frmPmg.arrMissilePosY[i] := 60 + 10*i;
    end;

//  else begin
    //if (frmPmg.tabs.TabIndex = 2) or (listExamples.ItemIndex = 4) then begin
    //  for i := 0 to 3 do begin
    //    frmPmg.arrMissilePosX[i] := frmPmg.missileIndex[i] + 80;
    //  end;
    //end;
//  end;

  editMissilePosX.Value := frmPmg.arrMissilePosX[radPM.ItemIndex];
  editMissilePosY.Value := frmPmg.arrMissilePosY[radPM.ItemIndex];

  CreateCode;
end;

procedure TfrmPmgGen.SetPlayersPos;
var
  i : byte;
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
//
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

procedure TfrmPmgGen.editPosXExit(Sender: TObject);
begin
//  frmPmg.arrPosX[radPM.ItemIndex] := editPosX.Value;
end;

procedure TfrmPmgGen.editPosYExit(Sender: TObject);
begin
//  frmPmg.arrPosY[radPM.ItemIndex] := editPosY.Value;
end;

procedure TfrmPmgGen.editMissilePosXExit(Sender: TObject);
begin
//  frmPmg.arrMissilePosX[radPM.ItemIndex] := editMissilePosX.Value;
end;

procedure TfrmPmgGen.editMissilePosYExit(Sender: TObject);
begin
//  frmPmg.arrMissilePosY[radPM.ItemIndex] := editMissilePosY.Value;
end;

procedure TfrmPmgGen.chkPriorityItem(Sender: TObject; Index: integer);
begin
  if index = 0 then
    chkPriority.Checked[1] := false
  else if index = 1 then
    chkPriority.Checked[0] := false;

  //if index = 2 then
  //  chkPriority.Checked[3] := false;
  //else if index = 3 then begin
  //  chkPriority.Checked[2] := false;
  //  if chkPriority.Checked[3] then begin
  //    //editPosX1.Value := frmPmg.playerIndex[1] + 80;
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

  //if cmbResolution.ItemIndex = 0 then
  //  pmResolution := singleResolution
  //else
  //  pmResolution := doubleResolution;

  frmSrcEdit.Show;

//  memo.Lines.TextLineBreakStyle:=#$9b;
//  memo.Lines.LineBreak:=#$9b;
//  frmSrcEdit.editor.Lines.LineBreak:=#$9b;
  frmSrcEdit.editor.Lines := memo.Lines;
  frmSrcEdit.MemoToEditor03(Sender);
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

procedure TfrmPmgGen.editPosX0Change(Sender: TObject);
begin
  //if isAutoEditPosX0 then
  frmPmg.arrPosX[radPM.ItemIndex] := editPosX.Value;
  CreateCode;
end;

procedure TfrmPmgGen.listExamplesProc(Sender: TObject);
var
  i : byte;
begin
  for i := 0 to radLang.Items.Count - 1 do begin
    radLang.Controls[i].Enabled := listings[listExamples.ItemIndex, i];
//    radLang.Controls[i].Visible := listings[listExamples.ItemIndex, i];
  end;
  radLang.ItemIndex := langIndex;

  //if cmbExamples.ItemIndex > 2 then begin
  //  boxPm.Enabled := true;
  //  boxPm.Visible := true;
  //  radPm.Enabled := true;
  //  radPm.Visible := true;
  //end
  //else begin
  //  boxPm.Enabled := false;
  //  boxPm.Visible := false;
  //  radPm.Enabled := false;
  //  radPm.Visible := false;
  //end;

//  tabsChange(Sender);
  SetPriority;
  case cmbResolution.ItemIndex of
    0: begin
      pmResolution := singleResolution;
      playerPageSize := '16';
      PMBASEStart := '768';
      SDMCTL := '62';
      PMBASE := '1024';
      PlayerMemSize := '256';
    end;
    1: begin
      pmResolution := doubleResolution;
      playerPageSize := '8';
      PMBASEStart := '384';
      SDMCTL := '46';
      PMBASE := '512';
      PlayerMemSize := '128';
    end;
  end;
  CreateCode;
end;

procedure TfrmPmgGen.pmColorDecChange(Sender: TObject);
begin
//  showmessage('pmColorDecChange');
//  pmColorDec.Value := pmColorDec.Value + 1;
//  col := pmColorDec.Value;
  colorValues[radPM.ItemIndex + 4] := pmColorDec.Value;
  pmColor.Brush.Color := colorMem[pmColorDec.Value div 2];
  editColorHex.Caption := '$' + Dec2Hex(pmColorDec.Value);
  CreateCode;
end;

procedure TfrmPmgGen.radLangProc(Sender: TObject);
begin
  //langIndex := radLang.ItemIndex;
  //if langIndex = 0 then
  //  radDataType.ItemIndex := 0;

  CreateCode;
end;

procedure TfrmPmgGen.radPmProc(Sender: TObject);
begin
  boxPm.Caption := 'Player/missile ' + IntToStr(radPm.ItemIndex) + ' properties';
  lblPmColorReg.Caption := 'Color register 70' +
                           IntToStr(radPm.ItemIndex + 4) +
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
end;

function TfrmPmgGen.SetValues(index : byte) : string;
var
  x, y : byte;
  line, values : string;
begin
  for y := 0 to _PM_MAX_LINES - 1 do begin
    line := '';
    for x := 0 to 7 do
      line += IntToStr(frmPmg.fld[index, x, y]);

    case radDataType.ItemIndex of
      0: line := inttostr(bin2dec(line));
      1: line := '$' + dec2hex(bin2dec(line));
      2: line := '%' + line;
    end;
    if y = 0 then values := line;
    if Trim(line) = '$' then line := '0';
    if Trim(values) = '$' then values := '0';
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

  result := values;
end;

function TfrmPmgGen.SetMissileValues(index : byte) : string;
var
  y : byte;
  value : byte;
  line, values : string;
begin
  for y := 0 to frmPmg.missileMaxY - 1 do begin
    // ORing all missiles together
    if index = 255 then begin
      value := frmPmg.missileData[0, y]
               or frmPmg.missileData[1, y]
               or frmPmg.missileData[2, y]
               or frmPmg.missileData[3, y];
    end
    // Separate missiles
    else begin
      value := frmPmg.missileData[index, y];
      //case radDataType.ItemIndex of
      //  0: line := IntToStr(frmPmg.missileData[index, y]);  //inttostr(bin2dec(line));
      //  1: line := '$' + dec2hex(frmPmg.missileData[index, y]);
      //  2: line := '%' + IntToBin(frmPmg.missileData[index, y], 8);
      //end;
    end;

    case radDataType.ItemIndex of
      0: line := IntToStr(value);
      1: line := '$' + dec2hex(value);
      2: line := '%' + IntToBin(value, 8);
    end;

    if y = 0 then values := line;
    if Trim(line) = '$' then line := '0';
    if Trim(values) = '$' then values := '0';

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

  result := values;
end;

procedure TfrmPmgGen.CreateCode;
var
  code : string;
begin
  langIndex := radLang.ItemIndex;

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

  case listExamples.ItemIndex of
    0: code := Example01;
    1: code := Example02;
    2: code := Example03;
    3: code := Example04;
    4: begin
         chkMissilePosDefault.Checked := false;
//         chkMissilePosDefaultChange(frmPmgGen);
         code := Example04;
       end;
    5: begin
//         SetPlayersPos(true);
         chkPlayerPosDefault.Checked := false;
//         chkPlayerPosDefaultChange(frmPmgGen);
         code := Example04;
       end;
  end;

  memo.Lines.Add(code);

  // Set cursor position at the top of memo object
  memo.SelStart := 0;
  memo.SelLength := 0;
  SendMessage(memo.Handle, EM_SCROLLCARET, 0, 0);
end;

//procedure TfrmPmgGen.tabsChange(Sender: TObject);
//begin
//  TRadioButton(radDataType.Controls[0]).Enabled := true;
//  TRadioButton(radDataType.Controls[1]).Enabled := true;
//  TRadioButton(radDataType.Controls[2]).Enabled := true;
//  case langIndex of
//    0: begin
//         radDataType.ItemIndex := 0;
//         TRadioButton(radDataType.Controls[1]).Enabled := false;
//         TRadioButton(radDataType.Controls[2]).Enabled := false;
//       end;
//    1: begin
//         if radDataType.ItemIndex = 2 then
//           radDataType.ItemIndex := 1;
//
//         TRadioButton(radDataType.Controls[2]).Enabled := false;
//       end;
//    2: begin
//         if radDataType.ItemIndex = 2 then
//           radDataType.ItemIndex := 1;
//
//         TRadioButton(radDataType.Controls[2]).Enabled := false;
//       end;
//    4: begin
//         if radDataType.ItemIndex = 2 then
//           radDataType.ItemIndex := 1;
//
//         TRadioButton(radDataType.Controls[2]).Enabled := false;
//       end;
//  end;
//end;

procedure TfrmPmgGen.SetPriority;
var
  i : byte;
begin
  for i := 0 to 3 do
    chkPriority.Checked[i] := false;

  case listExamples.ItemIndex of
    3: chkPriority.Checked[0] := true;
    4: begin
         chkPriority.Checked[0] := true;
         chkPriority.Checked[2] := true;
       end;
    5: begin
         chkPriority.Checked[0] := true;
         chkPriority.Checked[3] := true;
       end;
  end;
end;

function TfrmPmgGen.CheckPriority : byte;
var
  i : byte;
begin
  result := 0;
  for i := 0 to 3 do
    if chkPriority.Checked[i] then
      result := result or GPRIOR[i];
end;

function TfrmPmgGen.Example01: string;
var
  i : byte;
begin
  code.line := '';

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (radLang.ItemIndex < 2) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    for i := 0 to 3 do
      if chkPlayers.Checked[i] then
//        code += IntToStr((i + 1)*100) + ' DATA ' + SetValues(i) + #13#10;
        code.line += CodeLine('DATA ' + SetValues(i));
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    for i := 0 to 3 do
      if chkPlayers.Checked[i] then
        code.line += 'BYTE ARRAY'#13#10 +
                     '  PLAYER' + IntToStr(i) + ' = [' + SetValues(i) + ']'#13#10;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
//    code := 'const'#13#10;
    for i := 0 to 3 do
      if chkPlayers.Checked[i] then
        code.line += '  player' + IntToStr(i) +
                     ' : array[0..' + inttostr(_PM_MAX_LINES - 1) + '] of byte ='#13#10 +
                     '    (' + SetValues(i) + ');'#13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    for i := 0 to 3 do
      if chkPlayers.Checked[i] then
        code.line += 'DATA player' + IntToStr(i) + '() BYTE = ' + SetValues(i) + #13#10;
  end;

  result := code.line;
end;

function TfrmPmgGen.Example02: string;
var
  i : byte;
begin
  code.line := '';

  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (radLang.ItemIndex < 2) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    for i := 0 to 3 do
      if chkMissiles.Checked[i] then begin
//        code += IntToStr(30 + 2*i) + ' REM Missile ' + IntToStr(i) + ' data'#13#10 +
//                IntToStr(31 + 2*i) + ' DATA ' + SetMissileValues(i) + #13#10;
        code.line += CodeLine('REM Missile ' + IntToStr(i) + ' data');
        code.line += CodeLine('DATA ' + SetMissileValues(i));
      end;
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
//    code := '; MISSILE DATA'#13#10;
    for i := 0 to 3 do
      if chkMissiles.Checked[i] then
        code.line += 'BYTE ARRAY MISSILE' + IntToStr(i) + '=[' + SetMissileValues(i) + ']'#13#10;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
//    code := 'const'#13#10 +
//            '  // Missile data'#13#10;
    for i := 0 to 3 do
      if chkMissiles.Checked[i] then
        code.line += '  missile' + IntToStr(i) +
                     ' : array[0..' + inttostr(frmPmg.missileMaxY - 1) + '] of byte ='#13#10 +
                     '    (' + SetMissileValues(i) + ');'#13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    for i := 0 to 3 do
      if chkMissiles.Checked[i] then
        code.line += 'DATA missile' + IntToStr(i) + '() BYTE = ' + SetMissileValues(i) + #13#10;
  end;

  result := code.line;
end;

function TfrmPmgGen.Example03: string;
begin
  { Atari BASIC, Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (radLang.ItemIndex < 2) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
//    code := IntToStr(30) + ' REM Player 4 data'#13#10 +
//            IntToStr(31) + ' DATA ' + SetMissileValues(255) + #13#10;
    code.line += CodeLine(' REM Player 4 data');
    code.line += CodeLine(' DATA ' + SetMissileValues(255));
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    code.line := '; Player 4 data'#13#10 +
                 'BYTE ARRAY player4 = [' + SetMissileValues(255) + ']'#13#10;
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
//    code := 'const'#13#10 +
    code.line := '  // Player 4 data'#13#10 +
                 '  player4 : array [0..' + IntToStr(frmPmg.missileMaxY - 1) + '] of byte = ' +
                 '('#13#10 +
                 '    ' + SetMissileValues(255) + ');'#13#10;
  end
  { FastBasic
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _FAST_BASIC then begin
    code.line := ''' Player 4 data'#13#10 +
                 'DATA player4() BYTE = ' + SetMissileValues(255) + #13#10;
  end;

  result := code.line;
end;

function TfrmPmgGen.Example04: string;
var
  value623 : byte;
  isPlayer : boolean = false;
  isMissile : boolean = false;
  mPosY : string = '0';
  missileDef : string = '';
  SIZEM : byte;
  i : byte;
begin
  value623 := CheckPriority;
  editPosX.Enabled := true;

  //if listExamples.ItemIndex = 3 then begin  // in [3, 5]
  //  frmPmg.arrMissilePosY[1] := frmPmg.arrMissilePosY[0] + 10;
  //  frmPmg.arrMissilePosY[2] := frmPmg.arrMissilePosY[0] + 20;
  //  frmPmg.arrMissilePosY[3] := frmPmg.arrMissilePosY[0] + 30;
  //end
  if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then begin
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

//    editMissilePosX.Value := frmPmg.arrMissilePosX[radPm.ItemIndex];
//    editMissilePosY.Value := frmPmg.arrMissilePosY[radPm.ItemIndex];

//    isAutoEditPosX0 := true;
  end
  else if listExamples.ItemIndex = 5 then begin
    editPosX.Enabled := false;
    for i := 0 to 3 do
      frmPmg.arrPosX[i] := frmPmg.playerIndex[i] + 80;

    //frmPmg.arrPosX[1] := frmPmg.arrPosX[0];
    //frmPmg.arrPosX[2] := frmPmg.arrPosX[0];
    //frmPmg.arrPosX[3] := frmPmg.arrPosX[0];
  end;

  if chkPlayers.Checked[0] or chkPlayers.Checked[1]
     or chkPlayers.Checked[2] or chkPlayers.Checked[3] then
  begin
    isPlayer := true;
  end;

  if chkMissiles.Checked[0] or chkMissiles.Checked[1]
     or chkMissiles.Checked[2] or chkMissiles.Checked[3] then
  begin
    isMissile := true;

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
    for i := 0 to 255 do
      if arrMissileSizes[i] = missileDef then begin
        SIZEM := i;
        break;
      end;
  end;

  { Atari BASIC/Turbo BASIC XL
   ---------------------------------------------------------------------------}
  if (radLang.ItemIndex < 2) then begin
    code.number := editStartLine.Value;
    code.step := editLineStep.Value;
    code.line := CodeLine('REM P/M Graphics demonstration');
    for i := 0 to 3 do
      if chkPlayers.Checked[i] then begin
        code.line += CodeLine('REM Player ' + IntToStr(i) + ' data');
        code.line += CodeLine('DATA ' + SetValues(i));
      end;

    if (listExamples.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      //if chkPlayer4Data.Checked then begin
      //  code += '30 REM ORing missile data'#13#10 +
      //          '32 DATA ' + SetMissileValues(255) + #13#10;
      //end
      //else begin
        for i := 0 to 3 do
          if chkMissiles.Checked[i] then begin
            code.line += CodeLine(' REM Missile ' + IntToStr(i) + ' data');
            code.line += CodeLine(' DATA ' + SetMissileValues(i));
          end;
    end
    else if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += CodeLine('REM Player 4 data - ORing missile data') +
                   CodeLine('DATA ' + SetMissileValues(255));

    if isPlayer then begin
      code.line += CodeLine('REM Player position');
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += CodeLine('PX' + IntToStr(i) +
                       '=' + IntToStr(frmPmg.arrPosX[i]) +
                       ':PY' + IntToStr(i) + '=' + IntToStr(frmPmg.arrPosY[i]));
    end;

    if isMissile then begin
      code.line += CodeLine('REM Missile position');
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then begin
          code.line += CodeLine('MX' + IntToStr(i) +
                       '=' + IntToStr(frmPmg.arrMissilePosX[i]) +
                       ':MY' + IntToStr(i) + '=' + IntToStr(frmPmg.arrMissilePosY[i]));
          if mPosY = '0' then
            mPosY := 'MY' + IntToStr(i);
        end;
    end;

    code.line += CodeLine('REM Initialize P/M graphics') +
                 CodeLine('POKE 53277,0') +
                 CodeLine('GRAPHICS 0') +
                 CodeLine('POKE 710,0:POKE 712,0') +
                 CodeLine('? "Set P/M graphics"') +
                 CodeLine('PMGMEM=PEEK(106)-' + playerPageSize) +
                 CodeLine('POKE 54279,PMGMEM') +
                 CodeLine('PMGMEM=PMGMEM*256') +
                 CodeLine('REM P/M graphics double resolution') +
                 CodeLine('POKE 559,' + SDMCTL);

    if isPlayer then
      code.line += CodeLine('? "Clear player memory"');
      code.line += CodeLine('FOR I=0 TO ' + PMBASE + '+' + PlayerMemSize + '-1:? ".";:' +
                            'POKE PMGMEM+' + PMBASEStart + '+I,0:NEXT I');

    //if chkPriority.Checked[0] then
    //  code += '175 REM Priority register - P/M in front of all playfield'#13#10 +
    //          '177 POKE 623,' + IntToStr(value623) + #13#10
    //else if chkPriority.Checked[1] then begin
    //  code += '175 REM Priority register - P/M behind all playfield'#13#10 +
    //          '177 POKE 623,' + IntToStr(value623) + #13#10;
    //end;

    // Priority register settings
    code.line += CodeLine('REM Priority register');
    for i := 0 to 3 do
      if chkPriority.Checked[i] then
        code.line += CodeLine('REM - ' + chkPriority.Items[i]);

    code.line += CodeLine('POKE 623,' + IntToStr(value623));

    for i := 0 to 3 do
      if chkPlayers.Checked[i] then begin
        code.line += CodeLine('POKE ' + IntToStr(53256 + i) +
                     ',' + IntToStr(playerSize[i]) +
                     ': REM Player ' + IntToStr(i) + ' ' + pmSize[playerSize[i]]);
        //if playerSizes[i] = _PLAYER_SIZE_NORMAL then
        //  code += IntToStr(170 + 2*i) + ' POKE ' + IntToStr(53256 + i) +
        //          ',0: REM Player ' + IntToStr(i) + ' Normal size'#13#10
        //else if playerSizes[i] = _PLAYER_SIZE_DOUBLE then
        //  code += IntToStr(170 + 2*i) + ' POKE ' + IntToStr(53256 + i) +
        //          ',1: REM Player ' + IntToStr(i) + ' Double size'#13#10
        //else if playerSizes[i] = _PLAYER_SIZE_QUADRUPLE then
        //  code += IntToStr(170 + 2*i) + ' POKE ' + IntToStr(53256 + i) +
        //          ',3: REM Player ' + IntToStr(i) + ' Quadruple size'#13#10;
      end;

    for i := 0 to chkMissiles.ControlCount - 1 do
      if chkMissiles.Checked[i] then begin
        code.line += CodeLine('REM Missile size');
        code.line += CodeLine('POKE 53260,' + IntToStr(SIZEM));
        code.line += CodeLine('REM Player/missile color');
        break;
      end;

    for i := 0 to 3 do
      if chkPlayers.Checked[i] or chkMissiles.Checked[i] then
        code.line += CodeLine('POKE ' + IntToStr(704 + i) + ',' + IntToStr(colorValues[i + 4]));

    if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += CodeLine('POKE 711,' + IntToStr(colorValues[10]));

    if isPlayer then begin
      code.line += CodeLine('REM Player horizontal position');
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += CodeLine('POKE ' + IntToStr(53248 + i) + ',PX' + IntToStr(i));

      for i := 0 to 3 do
        if chkPlayers.Checked[i] then begin
          code.line += CodeLine('?:? "Draw player ' + IntToStr(i) + ' and set vertical pos."');
          code.line += CodeLine('FOR I=0 TO ' + IntToStr(_PM_MAX_LINES - 1));
          code.line += CodeLine('? ".";');
          code.line += CodeLine('READ A:POKE PMGMEM+' +
                  PMBASE + '+' + PlayerMemSize + '*' + IntToStr(i) + '+PY' + IntToStr(i) + '+I,A');
          code.line += CodeLine('NEXT I');
        end;
    end;

    if (listExamples.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then begin
          code.line += CodeLine('REM Draw missile ' + IntToStr(i) + ' and set vertical pos.');
          code.line += CodeLine('FOR I=0 TO ' + IntToStr(frmPmg.missileMaxY - 1));
          code.line += CodeLine('READ A:POKE PMGMEM+' + PMBASEStart + '+MY' + IntToStr(i) +
                                '+I,A'#13#10 + IntToStr(246 + 10*i) + ' NEXT I');
        end;
    end
    else if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += CodeLine('REM Draw player 4 and set vertical pos.') +
                   CodeLine('FOR I=0 TO ' + IntToStr(frmPmg.missileMaxY - 1)) +
                   CodeLine('? ".";') +
                   CodeLine('READ A:POKE PMGMEM+' + PMBASEStart + '+' + mPosY + '+I,A') +
                   CodeLine('NEXT I');

    if (listExamples.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      if isMissile then
        code.line += CodeLine('REM MISSILE POSITION');

      for i := 0 to 3 do
        code.line += CodeLine('POKE ' + IntToStr(53252 + i) + ',MX' + IntToStr(i));
    end
    else if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += CodeLine('REM PLAYER 4 POSITION');
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then
          code.line += CodeLine('POKE ' + IntToStr(53252 + i) + ',MX' + IntToStr(i));
    end;

    code.line += CodeLine('REM Turn on P/M graphics') +
                 CodeLine('? CHR$(125):POKE 53277,3') +
                 CodeLine('END');
  end
  { Mad Pascal
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _MAD_PASCAL then begin
    code.line := '{'#13#10 +
                '  Player/missile graphics demonstration'#13#10 +
                '}'#13#10 +
                'uses crt, graph, atari;'#13#10#13#10 +
                'var'#13#10 +
                '  ramtop : byte absolute $6A;    // Top of RAM in 256-byte pages'#13#10#13#10 +
                '  pcolr : array[0..3] of byte absolute $2C0;   // Player color'#13#10 +
                '  hposp : array[0..3] of byte absolute $D000;  // Player horizontal position'#13#10 +
                '  sizep : array[0..3] of byte absolute $D008;  // Player size'#13#10 +
                '  hposm : array[0..3] of byte absolute $D004;  // Missile horizontal position'#13#10 +
                '  pmgmem : word;'#13#10;

    if isPlayer then begin
      code.line += #13#10'  // Player data'#13#10;
      for i := 0 to 3 do begin
        if chkPlayers.Checked[i] then
          code.line += '  player' + IntToStr(i) +
                       ' : array [0..' + IntToStr(_PM_MAX_LINES - 1) + '] of byte ='#13#10 +
                       '    (' + SetValues(i) + ');'#13#10;
      end;
    end;

    if (listExamples.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      if isMissile then
        code.line += #13#10 + '  // Missile data'#13#10;

      for i := 0 to 3 do
        if chkMissiles.Checked[i] then
          code.line += '  missile' + IntToStr(i) +
                       ' : array [0..' + IntToStr(frmPmg.missileMaxY - 1) + '] of byte = ' +
                       '('#13#10 + SetMissileValues(i) + ');'#13#10;
    end
    else if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += #13#10'  // Player 4 data'#13#10 +
                   '  player4 : array [0..' + IntToStr(frmPmg.missileMaxY - 1) + '] of byte ='#13#10 +
                   '    (' + SetMissileValues(255) + ');'#13#10;
    end;

    if isPlayer then begin
      code.line += #13#10'  // Player position'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += '  px' + IntToStr(i) + ' : byte = ' + IntToStr(frmPmg.arrPosX[i]) + ';' +
                       ' py' + IntToStr(i) + ' : byte = ' + IntToStr(frmPmg.arrPosY[i]) + ';'#13#10;
    end;

    if isMissile then begin
      code.line += #13#10 + '  // Missile position'#13#10;
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then begin
          code.line += '  mx' + IntToStr(i) + ' : byte = ' + IntToStr(frmPmg.arrMissilePosX[i]) + ';' +
                       ' my' + IntToStr(i) + ' : byte = ' + IntToStr(frmPmg.arrMissilePosY[i]) + ';' +
                       #13#10;
          if mPosY = '0' then
            mPosY := 'my' + IntToStr(i);
        end;
    end;

    code.line += #13#10 + 'begin'#13#10 +
                 '  // Initialize P/M graphics'#13#10 +
                 '  InitGraph(0);'#13#10 +
                 '  gractl := 0;'#13#10 +
                 '  color2 := 0; color4 := 0;'#13#10 +
                 #13#10 +
                 '  // Set P/M graphics'#13#10 +
                 '  pmgmem := ramtop - ' + playerPageSize + ';'#13#10 +
                 '  pmbase := pmgmem;'#13#10 +
                 '  pmgmem := pmgmem shl 8;'#13#10 +
                 #13#10 +
                 '  // P/M graphics double resolution'#13#10 +
                 '  sdmctl := ' + SDMCTL + ';'#13#10 +
                 #13#10 +
                 '  // Clear player memory'#13#10 +
    //            '  POKE(pmgmem + ' + PMBASEStart + ', 0);'#13#10 +
    //            '  Move(pmgmem + ' + PMBASEStart + ', pmgmem + ' + PMBASEStart + ' + 1, ' +
    //            PMBASE + ' - 1 + ' + PlayerMemSize + ');'#13#10;
                 '  FillByte(Pointer(pmgmem + ' + PMBASEStart + '), ' +
                 PMBASE + ' + ' + PlayerMemSize + ', 0);'#13#10;

//    'Zero(pmgmem + ' + PMBASEStart + ', ' + PMBASE + ' + ' + PlayerMemSize + ')'#13#10;

    // Priority register settings
    code.line += #13#10 + '  // Priority register'#13#10;
    for i := 0 to 3 do
      if chkPriority.Checked[i] then
        code.line += '  // - ' + chkPriority.Items[i] + #13#10;

    code.line += '  gprior := ' + IntToStr(value623) + ';'#13#10;

    for i := 0 to 3 do
      if chkPlayers.Checked[i] then begin
        //code += '  POKE(' + IntToStr(53256 + i) +
        //        ', ' + IntToStr(playerSizes[i]) + ');  // Player ' + IntToStr(i) + ' ' +
        //        pmSize[playerSizes[i]] + #13#10;
        code.line += '  sizep[' + IntToStr(i) + '] := ' + IntToStr(playerSize[i]) +
                ';  // Player ' + IntToStr(i) + ' ' + pmSize[playerSize[i]] + #13#10;

      end;

    if isMissile then begin
      code.line += #13#10;
      for i := 0 to chkMissiles.ControlCount - 1 do
        if chkMissiles.Checked[i] then begin
          code.line += '  // Missile size'#13#10 +
                  '  sizem := ' + IntToStr(SIZEM) + ';'#13#10;
          break;
        end;
    end;

    if isPlayer or isMissile then begin
      code.line += #13#10 + '  // Player/missile color'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] or chkMissiles.Checked[i] then begin
          //code += '  POKE(' + IntToStr(704 + i) + ', ' +
          //        IntToStr(colorValues[i + 4]) + ');'#13#10;
          code.line += '  pcolr[' + IntToStr(i) + ']' + ' := ' +
                  IntToStr(colorValues[i + 4]) + ';'#13#10;
        end;
    end;

    if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += '  color3 := ' + IntToStr(colorValues[10]) + ';'#13#10;

    if isPlayer then begin
      code.line += #13#10 + '  // Player horizontal position'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then begin
          //code += '  POKE(' + IntToStr(53248 + i) + ', px' + IntToStr(i) + ');'#13#10;
          code.line += '  hposp[' + IntToStr(i) + '] := px' + IntToStr(i) + ';'#13#10;
        end;

      code.line += #13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then begin
          code.line += '  // Draw player ' + IntToStr(i) + ' and set vertical position'#13#10 +
                  '  Move(player' + IntToStr(i) + ', Pointer(pmgmem + ' + PMBASE + ' + ' + PlayerMemSize + '*' +
                  IntToStr(i) + ' + py' + IntToStr(i) + '), ' + IntToStr(_PM_MAX_LINES - 1) + ');'#13#10
        end;
    end;

    if (listExamples.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then
          code.line += '  // Draw missile ' + IntToStr(i) + ' and set vertical position'#13#10 +
                  '  Move(missile' + IntToStr(i) + ', Pointer(pmgmem + ' + PMBASEStart +
                  ' + my' + IntToStr(i) + '), ' + IntToStr(frmPmg.missileMaxY - 1) + ');'#13#10;
    end
    else if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += '  // Draw player 4 and set vertical position'#13#10 +
              '  Move(player4, Pointer(PMGMEM + ' + PMBASEStart + ' + ' + mPosY + '), ' +
              IntToStr(frmPmg.missileMaxY) + ');'#13#10;
    end;

    if (listExamples.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      if isMissile then
        code.line += #13#10 + '  // Missile position'#13#10;

      for i := 0 to 3 do
        if chkMissiles.Checked[i] then begin
          //code += '  POKE(' + IntToStr(53252 + i) + ', mx' + IntToStr(i) + ');'#13#10;
          code.line += '  hposm[' + IntToStr(i) + '] := mx' + IntToStr(i) + ';'#13#10;
        end;
    end
    else if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then begin
      code.line += #13#10 + '  // Player 4 position'#13#10;
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then
          //code += '  POKE(' + IntToStr(53252 + i) + ', mx' + IntToStr(i) + ');'#13#10;
          code.line += '  hposm[' + IntToStr(i) + '] := mx' + IntToStr(i) + ';'#13#10;
    end;
    code.line += #13#10 + '  gractl := 3;'#13#10 +
            #13#10 + '  repeat until keypressed;'#13#10#13#10 +
            '  // Reset P/M graphics'#13#10 +
            '  gractl := 0;'#13#10 +
            'end.';

(*  for i := 0 to 3 do
      if chkPlayers.Checked[i] then
        code += '  p_data[' + IntToStr(i) + '] := @p' + IntToStr(i) + 'Data;'#13#10;

    if listExamples.ItemIndex = 3 then begin
      for i := 0 to 3 do begin
        if chkMissiles.Checked[i] then
          code += '  m_data[' + IntToStr(i) + '] := @m' + IntToStr(i) + 'Data;'#13#10;
      end;
    end
    else if listExamples.ItemIndex = 4 then
      for i := 0 to 3 do
        code += '  m_data[' + IntToStr(i) + '] := @m0Data;'#13#10;

    code += #13#10 + '  // Initialize P/M graphics resolution';

    if player0Resolution = [singleResolution] then
      code += '  SetPM(_PM_SINGLE_RES);'#13#10 +
              '  InitPM(_PM_SINGLE_RES);'
    else
      code += '  SetPM(_PM_DOUBLE_RES);'#13#10 +
              '  InitPM(_PM_DOUBLE_RES);'#13#10;

    //if chkPriority.Checked[0] then
    //  code += #13#10 +
    //          '  // Priority register - P/M in front of all playfield'#13#10 +
    //          '  Poke(623, ' + IntToStr(value623) + ');'#13#10
    //else if chkPriority.Checked[1] then
    //  code += #13#10 +
    //          '  // Priority register - P/M behind all playfield'#13#10 +
    //          '  Poke(623, ' + IntToStr(value623) + ');'#13#10;

    // Priority register settings
    code += #13#10 + '  // Priority register'#13#10;
    for i := 0 to 3 do
      if chkPriority.Checked[i] then
        code += '  // - ' + chkPriority.Items[i] + #13#10;

    code += '  Poke(623, ' + IntToStr(value623) + ');'#13#10;
    code += #13#10 +
            '  // Set program colors and hide cursor'#13#10 +
            '  Poke(710, 0); Poke(712, 0);'#13#10 +
            '  Poke(752, 1);'#13#10 +
            #13#10 +
            '  writeln(eol,''Player/missile graphics demonstration'');'#13#10 +
            #13#10 +
            '  // Turn on P/M graphics'#13#10 +
            '  ShowPM(_PM_SHOW_ON);'#13#10 +
            #13#10 +
            '  // Player color'#13#10;

    for i := 0 to 3 do
      if chkPlayers.Checked[i] then begin
        code += '  ColorPM(' + IntToStr(i) + ', ' + IntToStr(colorValues[i + 4]) + ');'#13#10;
      end;

    if listExamples.ItemIndex = 4 then
      code += '  Poke(711, ' + IntToStr(colorValues[10]) + ');'#13#10;

    code += #13#10 + '  // Player size'#13#10;

    for i := 0 to 3 do
      if chkPlayers.Checked[i] then begin
        if playerSizes[i] = _PLAYER_SIZE_NORMAL then
          code += '  SizeP(' + IntToStr(i) + ', _PM_NORMAL_SIZE);'#13#10
        else if playerSizes[i] = _PLAYER_SIZE_DOUBLE then
          code += '  SizeP(' + IntToStr(i) + ', _PM_DOUBLE_SIZE);'#13#10
        else if playerSizes[i] = _PLAYER_SIZE_QUADRUPLE then
          code += '  SizeP(' + IntToStr(i) + ', _PM_QUAD_SIZE);'#13#10;
      end;

    code += #13#10 + '  // Player position'#13#10;

    for i := 0 to 3 do
      if chkPlayers.Checked[i] then
        code += '  MoveP(' + IntToStr(i) + ', px' + IntToStr(i) + ', py' + IntToStr(i) + ');'#13#10;

    if isMissile then
      code += #13#10 + '  // Missile position'#13#10;

    for i := 0 to 3 do begin
//      if chkMissiles.Checked[i] then begin
        code += '  MoveM(' + IntToStr(i) + ', mx' + IntToStr(i) + ', my' + IntToStr(i) + ');'#13#10;
//      end;
    end;
*)
  end
  { Action!
   ---------------------------------------------------------------------------}
  else if radLang.ItemIndex = _ACTION then begin
    code.line := '; Player/missile graphics demo'#13#10#13#10 +
                 'PROC Main()'#13#10#13#10 +
                 'BYTE pmbase = $D407  ; Player/Missile base address'#13#10 +
                 'BYTE sdmctl = $22F   ; Player resolution'#13#10 +
                 'BYTE gractl = $D01D  ; 1 = missile DMA, 2 = player DMA'#13#10 +
                 'BYTE ramtop = $6A    ; Top of RAM in 256-byte pages'#13#10 +
                 'BYTE sizem = $D00C   ; Size for all missiles set by bits'#13#10 +
                 'BYTE ch = $2FC       ; Keyboard code of last key pressed'#13#10#13#10 +
                 'BYTE ARRAY pcolr(3) = $2C0   ; Player color'#13#10 +
                 'BYTE ARRAY hposp(3) = $D000  ; Player horizontal position'#13#10 +
                 'BYTE ARRAY sizep(3) = $D008  ; Player size'#13#10 +
                 'BYTE ARRAY hposm(3) = $D004  ; Missile horizontal position'#13#10#13#10 +
                 'CARD pmgmem'#13#10;

    if isPlayer then begin
      code.line += #13#10 + '; Player position'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += 'BYTE px' + IntToStr(i) + ' = [' + IntToStr(frmPmg.arrPosX[i]) + '], ' +
                       'py' + IntToStr(i) + ' = [' + IntToStr(frmPmg.arrPosY[i]) + ']'#13#10;
    end;

    if isMissile then begin
      code.line += #13#10 + '; Missile position'#13#10;
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then begin
          code.line += 'BYTE mx' + IntToStr(i) + ' = [' + IntToStr(frmPmg.arrMissilePosX[i]) + '], ' +
                       'my' + IntToStr(i) + ' = [' + IntToStr(frmPmg.arrMissilePosY[i]) + ']'#13#10;
          if mPosY = '0' then
            mPosY := 'my' + IntToStr(i);
        end;
    end;

    if isPlayer then begin
      code.line += #13#10 + '; Player data'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += 'BYTE ARRAY p' + IntToStr(i) + ' = [' +  //#13#10 +
                       SetValues(i) + ']'#13#10;
//                       '  ' + SetValues(i) + #13#10 +
//                       ']'#13#10;
    end;

    if (listExamples.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      if isMissile then
        code.line += #13#10 + '; Missile data'#13#10;

      for i := 0 to 3 do
        if chkMissiles.Checked[i] then
          code.line += 'BYTE ARRAY m' + IntToStr(i) + ' = [' +  //#13#10 +
                       SetMissileValues(i) + ']'#13#10;
//                       '  ' + SetMissileValues(i) + #13#10 +
//                       ']'#13#10;
    end
    else if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += 'BYTE ARRAY player4 = [' +  //#13#10 +
                   SetMissileValues(255) + ']'#13#10;
//                   '  ' + SetMissileValues(255) + #13#10 +
//                   ']'#13#10;

    code.line += #13#10'; Turn off P/M graphics'#13#10 +
                 'gractl = 0'#13#10#13#10 +
                 'Graphics(0)'#13#10 +
                 'Poke(710, 0) Poke(712, 0)'#13#10#13#10 +
                 '; Set P/M graphics'#13#10;

    code.line += 'pmgmem = ramtop - ' + playerPageSize + #13#10 +
                 'pmbase = pmgmem'#13#10 +
                 'pmgmem = pmgmem * 256'#13#10#13#10 +
                 '; P/M graphics double resolution'#13#10 +
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
    for i := 0 to 3 do
      if chkPriority.Checked[i] then
        code.line += '; - ' + chkPriority.Items[i] + #13#10;

    code.line += 'Poke(623, ' + IntToStr(value623) + ')'#13#10#13#10;

    for i := 0 to 3 do
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

    code.line += 'sizem = ' + IntToStr(SIZEM) + #13#10;

    for i := 0 to 3 do
      if chkPlayers.Checked[i] then
        code.line += 'pcolr(' + IntToStr(i) + ') = ' + IntToStr(colorValues[i + 4]) +
                '  ; Player ' + IntToStr(i) + ' color'#13#10;

    if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += 'Poke(711,' + IntToStr(colorValues[10]) + ')'#13#10;

    if isPlayer then begin
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += 'hposp(' + IntToStr(i) + ') = px' + IntToStr(i) +
                  '  ; Player ' + IntToStr(i) + ' horizontal position'#13#10;
      for i := 0 to 3 do
        if chkPlayers.Checked[i] then
          code.line += '; Draw player ' + IntToStr(i) + ' and set vertical position'#13#10 +
                  'MoveBlock(pmgmem + ' + PMBASE + ' + ' +
                  PlayerMemSize + '*' + IntToStr(i) + ' + py' + IntToStr(i) +
                  ', p' + IntToStr(i) + ', ' + IntToStr(_PM_MAX_LINES - 1) + ')'#13#10
    end;

    //code += #13#10 + '; Copy missile data to P/M memory'#13#10 +
    //        '; and set missile Y position'#13#10;
    if (listExamples.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then
          code.line += '; Draw missile ' + IntToStr(i) + ' and set vertical position'#13#10 +
                  'MoveBlock(pmgmem + ' + PMBASEStart + ' + my' + IntToStr(i) +
                  ', m' + IntToStr(i) + ', ' + IntToStr(frmPmg.missileMaxY) + ')'#13#10
    end
    else if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then
      code.line += 'MoveBlock(pmgmem + ' + PMBASEStart + ' + ' + mPosY + ', player4, ' +
              IntToStr(frmPmg.missileMaxY) + ')'#13#10;

    if isMissile then begin
      code.line += #13#10 + '; Missile position'#13#10;
      for i := 0 to 3 do
        if chkMissiles.Checked[i] then
          code.line += 'hposm(' + IntToStr(i) + ') = mx' + IntToStr(i) + #13#10;
    end;
    code.line += #13#10 + '; Turn on P/M graphics'#13#10 +
                 'gractl = 3'#13#10#13#10 +
                 'ch = 255'#13#10 +
                 'DO UNTIL CH # 255 OD'#13#10 +
                 'ch = 255'#13#10#13#10 +
                 '; Turn off P/M graphics'#13#10 +
                 'gractl = 0'#13#10#13#10 +
                 'RETURN';
  end
  { FastBasic
   ---------------------------------------------------------------------------}
   else if radLang.ItemIndex = _FAST_BASIC then begin
     code.line := ''' P/M Graphics demonstration'#13#10;

     if isPlayer then begin
       code.line += ''' Player data'#13#10;
       for i := 0 to 3 do
         if chkPlayers.Checked[i] then
           code.line += 'DATA player' + IntToStr(i) + '() BYTE = ' + SetValues(i) + #13#10;
     end;

     if (listExamples.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
       if isMissile then
         code.line += #13#10''' Missile data'#13#10;

       for i := 0 to 3 do
         if chkMissiles.Checked[i] then
           code.line += 'DATA missile' + IntToStr(i) + '() BYTE = ' + SetMissileValues(i) + #13#10;
     end
     else if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then
       code.line += 'DATA player4() BYTE = ' + SetMissileValues(255) + #13#10;

     if isPlayer then begin
       code.line += #13#10''' Player position'#13#10;
       for i := 0 to 3 do
         if chkPlayers.Checked[i] then
           code.line += 'px' + IntToStr(i) + ' = ' + IntToStr(frmPmg.arrPosX[i]) +
                        ' : py' + IntToStr(i) + ' = ' + IntToStr(frmPmg.arrPosY[i]) + #13#10;
     end;

     if isMissile then
       code.line += #13#10''' Missile position'#13#10;

     for i := 0 to 3 do
       if chkMissiles.Checked[i] then begin
         code.line += 'mx' + IntToStr(i) + ' = ' + IntToStr(frmPmg.arrMissilePosX[i]) +
                 ' : my' + IntToStr(i) + ' = ' + IntToStr(frmPmg.arrMissilePosY[i]) + #13#10;
         if mPosY = '0' then
           mPosY := 'my' + IntToStr(i);
       end;

     code.line += #13#10''' Initialize P/M graphics'#13#10 +
                  'POKE 53277, 0'#13#10 +
                  'GRAPHICS 0'#13#10 +
                  'POKE 710, 0 : POKE 712, 0'#13#10#13#10 +
                  '? "Set P/M graphics"'#13#10 +
                  'pmgmem = PEEK(106) - ' + playerPageSize + #13#10 +
                  'POKE 54279, pmgmem'#13#10 +
                  'pmgmem = pmgmem * 256'#13#10#13#10 +
                  ''' P/M graphics double resolution'#13#10 +
                  'POKE 559, ' + SDMCTL + #13#10#13#10 +
                  ''' Clear player memory'#13#10 +
                  'POKE pmgmem + ' + PMBASEStart + ', 0'#13#10 +
                  'MOVE pmgmem + ' + PMBASEStart + ', pmgmem + ' + PMBASEStart + ' + 1, ' +
                  PMBASE+ ' - 1 + ' + PlayerMemSize + #13#10;

     // Priority register settings
     code.line += #13#10 + ''' Priority register'#13#10;
     for i := 0 to 3 do
       if chkPriority.Checked[i] then
         code.line += ''' - ' + chkPriority.Items[i] + #13#10;

     code.line += 'POKE 623, ' + IntToStr(value623) + #13#10;
     for i := 0 to 3 do
       if chkPlayers.Checked[i] then
         code.line += 'POKE ' + IntToStr(53256 + i) +
                      ', ' + IntToStr(playerSize[i]) + ' : '' Player ' + IntToStr(i) + ' ' +
                      pmSize[playerSize[i]] + #13#10;

     if isMissile then
       code.line += #13#10;

     for i := 0 to chkMissiles.ControlCount - 1 do
       if chkMissiles.Checked[i] then begin
         code.line += ''' Missile size'#13#10 +
                 'POKE 53260, ' + IntToStr(SIZEM) + #13#10;
         break;
       end;

     if isPlayer or isMissile then begin
       code.line += #13#10 + ''' Player/missile color'#13#10;
       for i := 0 to 3 do
         if chkPlayers.Checked[i] or chkMissiles.Checked[i] then
           code.line += 'POKE ' + IntToStr(704 + i) + ', ' +
                        IntToStr(colorValues[i + 4]) + #13#10;
     end;

     if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then
       code.line += 'POKE 711, ' + IntToStr(colorValues[10]) + #13#10;

     if isPlayer then begin
       code.line += #13#10 + ''' Player horizontal position'#13#10;
       for i := 0 to 3 do
         if chkPlayers.Checked[i] then
           code.line += 'POKE ' + IntToStr(53248 + i) + ', px' + IntToStr(i) + #13#10;

       code.line += #13#10;
       for i := 0 to 3 do
         if chkPlayers.Checked[i] then
           code.line += ''' Draw player ' + IntToStr(i) + ' and set vertical position'#13#10 +
                        'MOVE ADR(player' + IntToStr(i) + '), pmgmem + '
                        + PMBASE + ' + ' + PlayerMemSize + '*' + IntToStr(i) + ' + py' + IntToStr(i) +
                        ', ' + IntToStr(_PM_MAX_LINES - 1) + #13#10;
     end;

     if (listExamples.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
       for i := 0 to 3 do
         if chkMissiles.Checked[i] then
           code.line += ''' Draw missile ' + IntToStr(i) + ' and set vertical position'#13#10 +
                        'MOVE ADR(missile' + IntToStr(i) + '), pmgmem + ' + PMBASEStart +
                        ' + my' + IntToStr(i) + ', ' + IntToStr(frmPmg.missileMaxY - 1) + #13#10;
     end
     else if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then begin
       code.line += ''' Draw player 4 and set vertical position'#13#10 +
                    'MOVE ADR(player4), PMGMEM + ' + PMBASEStart + ' + ' + mPosY + ', ' +
                    IntToStr(frmPmg.missileMaxY) + #13#10;
     end;

     if (listExamples.ItemIndex in [3, 5]) and not chkPriority.Checked[2] then begin
       if isMissile then
         code.line += #13#10 + ''' Missile position'#13#10;

       for i := 0 to 3 do
         if chkMissiles.Checked[i] then
           code.line += 'POKE ' + IntToStr(53252 + i) + ', mx' + IntToStr(i) + #13#10;
     end
     else if (listExamples.ItemIndex = 4) or chkPriority.Checked[2] then begin
       code.line += #13#10 + ''' Player 4 position'#13#10;
       for i := 0 to 3 do
         if chkMissiles.Checked[i] then
           code.line += 'POKE ' + IntToStr(53252 + i) + ', mx' + IntToStr(i) + #13#10;
     end;
     code.line += #13#10'? "Turn on P/M graphics"'#13#10 +
                  'POKE 53277, 3'#13#10 +
                  WaitKeyCode(_FAST_BASIC);
  end;
  result := code.line;
end;

procedure TfrmPmgGen.CloseWin(Sender: TObject);
begin
  Close;
end;

end.

