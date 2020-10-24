{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Player/missile graphics editor
}
unit pmg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, SpinEx, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, StdCtrls, Spin, lcltype, Buttons, Types, strutils, windows,
  common, byte_editor;

type
  { TfrmPmg }
  TfrmPmg = class(TForm)
    btnApplyMissileHeight : TButton;
    btnLoadAnim : TToolButton;
    btnSep3 : TToolButton;
    btnShiftPlayerDown : TSpeedButton;
    btnShiftPlayerLeft : TSpeedButton;
    btnShiftPlayerRight : TSpeedButton;
    btnShiftPlayerUp : TSpeedButton;
    btnApplyPlayerHeight : TButton;
    chkJoinPlayers : TCheckBox;
    editMissilePosX : TSpinEdit;
    editMissilePosY : TSpinEdit;
    editorMulti: TImage;
    editPlayerHeight : TSpinEditEx;
    editMissileHeight : TSpinEditEx;
    editPosX : TSpinEdit;
    editPosY : TSpinEdit;
    GroupBox1 : TGroupBox;
    grpFlipPlayers : TGroupBox;
    grpPlayerHeight1 : TGroupBox;
    grpShiftMovePlayer : TGroupBox;
    GroupBox2 : TGroupBox;
    GroupBox3 : TGroupBox;
    grpPlayerHeight : TGroupBox;
    imgMissileLocation : TImage;
    imgP0_double: TImage;
    imgM0_double_dr: TImage;
    imgP0_doubleSr: TImage;
    imgM0_double_sr: TImage;
    imgP0_normal: TImage;
    imgM0_normal_dr: TImage;
    imgP0_normalSr: TImage;
    imgM0_normal_sr: TImage;
    imgP0_quadrable: TImage;
    imgM0_quadrable_dr: TImage;
    imgP0_quadrableSr: TImage;
    imgP0: TImage;
    imgM0_quadrable_sr: TImage;
    imgP1: TImage;
    imgP2: TImage;
    imgP3: TImage;
    editor: TImage;
    imgPlayer4: TImage;
    imgMissile0: TImage;
    imgMissile1: TImage;
    imgMissile2: TImage;
    imgMissile3: TImage;
    imgMissile00: TImage;
    imgMissile01: TImage;
    imgMissile02: TImage;
    imgMissile03: TImage;
    imgMissileSingle0: TImage;
    imgMissileSingle1: TImage;
    imgMissileSingle2: TImage;
    imgMissileSingle3: TImage;
    imgP0_normal02: TImage;
    imgP0_single: TImage;
    imgP1_normal02: TImage;
    imgP1_single: TImage;
    imgP2_normal02: TImage;
    imgP2_single: TImage;
    imgP3_normal02: TImage;
    imgP3_single: TImage;
    imgPlayerLocation : TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label41 : TLabel;
    Label42 : TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52 : TLabel;
    Label53 : TLabel;
    Label6 : TLabel;
    lblNum30: TLabel;
    lblNum31: TLabel;
    lblNum32: TLabel;
    lblNum33: TLabel;
    lblNum34: TLabel;
    lblNum35: TLabel;
    lblNum36: TLabel;
    lblNum37: TLabel;
    lblNum38: TLabel;
    lblNum39: TLabel;
    lblPlayer: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label64: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    lblP4: TLabel;
    lblP5: TLabel;
    lblP6: TLabel;
    lblP7: TLabel;
    lblNum0: TLabel;
    lblNum1: TLabel;
    lblNum10: TLabel;
    lblNum11: TLabel;
    lblNum12: TLabel;
    lblNum13: TLabel;
    lblNum14: TLabel;
    lblNum15: TLabel;
    lblNum16: TLabel;
    lblNum17: TLabel;
    lblNum18: TLabel;
    lblNum19: TLabel;
    lblNum2: TLabel;
    lblNum3: TLabel;
    lblNum4: TLabel;
    lblNum5: TLabel;
    lblNum6: TLabel;
    lblNum7: TLabel;
    lblNum8: TLabel;
    lblNum9: TLabel;
    lblNum20: TLabel;
    lblNum21: TLabel;
    lblNum22: TLabel;
    lblNum23: TLabel;
    lblNum24: TLabel;
    lblNum25: TLabel;
    lblNum26: TLabel;
    lblNum27: TLabel;
    lblNum28: TLabel;
    lblNum29: TLabel;
    MenuItem10 : TMenuItem;
    MenuItem11 : TMenuItem;
    MenuItem12 : TMenuItem;
    MenuItem14 : TMenuItem;
    MenuItem15 : TMenuItem;
    MenuItem17 : TMenuItem;
    MenuItem18 : TMenuItem;
    MenuItem5 : TMenuItem;
    MenuItem7 : TMenuItem;
    MenuItem9 : TMenuItem;
    mnuCopyPlayer1 : TMenuItem;
    mnuCopyPlayer0 : TMenuItem;
    miPmDoubleResolution: TMenuItem;
    miPmSingleResolution: TMenuItem;
    mnuMplLoadOld: TMenuItem;
    MenuItem8: TMenuItem;
    mnuMplSaveAs: TMenuItem;
    mnuMplSave: TMenuItem;
    mnuMplLoad: TMenuItem;
    MenuItem13: TMenuItem;
    mnuMissileSave: TMenuItem;
    mnuMissileSaveAs: TMenuItem;
    mnuMissilesSaveAll: TMenuItem;
    mnuMissileLoad: TMenuItem;
    miSaveAPL: TMenuItem;
    mnuLoadApl: TMenuItem;
    miRotate: TMenuItem;
    miFlipX: TMenuItem;
    miFlipY: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    mnuClearPlayer: TMenuItem;
    MenuItem21: TMenuItem;
    mnuClearMissile: TMenuItem;
    mnuClearAllPlayers: TMenuItem;
    mnuClearAllMissiles: TMenuItem;
    MenuItem25: TMenuItem;
    miShowGrid: TMenuItem;
    miHideGrid: TMenuItem;
    MenuItem28: TMenuItem;
    miMultiColorEnable: TMenuItem;
    miMultiColorDisable: TMenuItem;
    miMultiValues: TMenuItem;
    miHideValues: TMenuItem;
    MenuItem33: TMenuItem;
    miShowDec: TMenuItem;
    miShowHex: TMenuItem;
    MenuItem6: TMenuItem;
    menuPmg: TMainMenu;
    MenuItem1: TMenuItem;
    mnuPlayersSaveAll: TMenuItem;
    miExit: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    mnuPlayerLoad: TMenuItem;
    mnuPlayerSave: TMenuItem;
    mnuPlayerSaveAs: TMenuItem;
    panelRight : TPanel;
    panelM0: TPanel;
    panelM1: TPanel;
    panelM2: TPanel;
    panelM3: TPanel;
    popMenu : TPopupMenu;
    radFlipX : TRadioButton;
    radFlipY : TRadioButton;
    radMoveAllPlayers : TRadioButton;
    radMovePlayer : TRadioButton;
    radShiftAllPlayers : TRadioButton;
    radShiftPlayer : TRadioButton;
    shape01: TShape;
    shape02: TShape;
    shape03: TShape;
    Shape04: TShape;
    tabs: TPageControl;
    Panel1: TPanel;
    rbPlayer0: TRadioButton;
    rbPlayer1: TRadioButton;
    rbPlayer2: TRadioButton;
    rbPlayer3: TRadioButton;
    sbPmg: TStatusBar;
    tabPlayers: TTabSheet;
    tabMissiles: TTabSheet;
    tabMultiPM: TTabSheet;
    tbHorizontal : TTrackBar;
    tbMissileHorizontal : TTrackBar;
    tbMissileVertical : TTrackBar;
    tbPlayer0: TTrackBar;
    tbPlayer1: TTrackBar;
    tbPlayer2: TTrackBar;
    tbPlayer3: TTrackBar;
    tbVertical : TTrackBar;
    tglP3x1 : TToggleBox;
    tglP3x2 : TToggleBox;
    tglP3x4 : TToggleBox;
    tglP1x1 : TToggleBox;
    tglP1x2 : TToggleBox;
    tglP1x4 : TToggleBox;
    tglP2x1 : TToggleBox;
    tglP2x2 : TToggleBox;
    tglP2x4 : TToggleBox;
    ToggleBox1 : TToggleBox;
    ToggleBox2 : TToggleBox;
    tglP0x1 : TToggleBox;
    tglP0x2 : TToggleBox;
    tglP0x4 : TToggleBox;
    ToggleBox3 : TToggleBox;
    ToggleBox4 : TToggleBox;
    ToggleBox5 : TToggleBox;
    ToggleBox6 : TToggleBox;
    toolbar: TToolBar;
    btnLoadData: TToolButton;
    btnSaveData: TToolButton;
    btnSep02: TToolButton;
    btnClearData: TToolButton;
    btnFlipX: TToolButton;
    btnFlipY: TToolButton;
    btnRotate: TToolButton;
    btnSaveDataAll: TToolButton;
    btnGenCode: TToolButton;
    btnByteEditor : TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure btnApplyMissileHeightClick(Sender : TObject);
    procedure btnApplyPlayerHeightClick(Sender : TObject);
    procedure chkJoinPlayersChange(Sender : TObject);
    procedure imgP0_normalMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure CopyPlayer2(Sender : TObject);
    procedure CopyPlayer3(Sender : TObject);
    procedure CopyPlayer0(Sender : TObject);
    procedure CopyPlayer1(Sender : TObject);
    procedure CopyToAllPlayers(Sender : TObject);
    procedure PlayerOper(Sender : TObject);
    procedure editMissilePosXChange(Sender: TObject);
    procedure editMissilePosYChange(Sender: TObject);
    procedure SetDoubleResolution(Sender: TObject);
    procedure SetSingleResolution(Sender: TObject);
    procedure SaveDataAll(Sender: TObject);
    procedure panelMissileClick(Sender: TObject);
    procedure SaveMissileDataAll(Sender: TObject);
    procedure tbMissileHorizontalChange(Sender: TObject);
    procedure tbMissileVerticalChange(Sender: TObject);
    procedure SavePlayerDataAll(Sender: TObject);
    procedure chkMultiColor01Change(Sender: TObject);
    procedure editorMultiMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure editorMultiMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure editorMultiMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure editPosXChange(Sender: TObject);
    procedure editPosYChange(Sender: TObject);
    procedure imgMissile00aMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgMissile00aMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgMissileMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgMissileMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgMissileMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure lblNumClick(Sender: TObject);
    procedure ExitForm(Sender: TObject);
    procedure LoadAplData(Sender: TObject);
    procedure SaveAplData(Sender: TObject);
    procedure ShowGrid(Sender: TObject);
    procedure HideGrid(Sender: TObject);
    procedure MultiColorEnable(Sender: TObject);
    procedure MultiColorDisable(Sender: TObject);
    procedure ShowDec(Sender: TObject);
    procedure ShowHex(Sender: TObject);
    procedure HideValues(Sender: TObject);
    procedure rbPlayerChange(Sender: TObject);
    procedure SaveDataAs(Sender: TObject);
    procedure GenCode(Sender: TObject);
    procedure Palette(Sender: TObject);
    procedure editorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure editorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure editorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure tabMultiAllShow(Sender: TObject);
    procedure tabMultiShow(Sender: TObject);
    procedure tabNormalShow(Sender: TObject);
    procedure tabPlayersShow(Sender: TObject);
    procedure tabsChange(Sender: TObject);
    procedure tbHorizontalChange(Sender: TObject);
    procedure tbPlayer0Click(Sender : TObject);
    procedure tbPlayer1Click(Sender : TObject);
    procedure tbPlayer2Click(Sender : TObject);
    procedure tbPlayer3Click(Sender : TObject);
    procedure tbPlayerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure tbVerticalChange(Sender: TObject);
    procedure LoadDataProc(Sender: TObject);
    procedure SaveData(Sender: TObject);
    procedure ClearData(Sender: TObject);
    procedure MoveLeft(Sender: TObject);
    procedure MoveRight(Sender: TObject);
    procedure MoveUp(Sender: TObject);
    procedure MoveDown(Sender: TObject);
    procedure ShiftLeft(Sender: TObject);
    procedure ShiftRight(Sender: TObject);
    procedure ShiftUp(Sender: TObject);
    procedure ShiftDown(Sender: TObject);
    procedure FlipX(Sender: TObject);
    procedure FlipY(Sender: TObject);
    procedure Rotate(Sender: TObject);
    procedure LoadPlayerData;
    procedure LoadMissileData;
    procedure LoadMplData(Sender : TObject);
    procedure tglP0x1Change(Sender : TObject);
    procedure tglP0x2Change(Sender : TObject);
    procedure tglP1x1Change(Sender : TObject);
    procedure tglP0x4Change(Sender : TObject);
    procedure tglP1x2Change(Sender : TObject);
    procedure tglP1x4Change(Sender : TObject);
    procedure tglP2x1Change(Sender : TObject);
    procedure tglP2x2Change(Sender : TObject);
    procedure tglP2x4Change(Sender : TObject);
    procedure tglP3x1Change(Sender : TObject);
    procedure tglP3x2Change(Sender : TObject);
    procedure tglP3x4Change(Sender : TObject);
    procedure ToggleBox1Change(Sender : TObject);
    procedure ToggleBox2Change(Sender : TObject);
    procedure ToggleBox3Change(Sender : TObject);
    procedure ToggleBox4Change(Sender : TObject);
    procedure ToggleBox5Change(Sender : TObject);
    procedure ToggleBox6Change(Sender : TObject);
    procedure ImportData(Sender : TObject);
  private
    { private declarations }
    btn : tMousebutton;
    varBtn : tMousebutton;
    isRotate : boolean;
    tbOldX : array[0..3] of byte;
    isDataChanged : array[0..3] of boolean;
    isMissileDataChanged : array[0..3] of boolean;
    isValueDec : boolean;
    fs : TFileStream;
    procedure PmgSettings;
    procedure SetMissiles;
    procedure Plot(xf, yf : integer);
    procedure PlotMulti(xf, yf : integer);
    procedure RefreshPlayer(pm : TImage; factX, factY : byte);
    procedure SyncPlayer;
    procedure RefresPM_MultiAll(pm : TImage; factX, factY : byte);
    procedure ReadFld(factX, factY : byte);
    procedure SavePlayer(filenamexy : string);
    procedure SaveMplData(filenamexy : string);
    procedure SaveMissile(filenamexy : string);
    procedure SetRotate(isRotatex : boolean);
    procedure DrawMissile(pm : TImage; factX, factY : byte);
    procedure PlotMissile(xf, yf : integer);
    procedure RedrawPlayer4;
    procedure PlayerLocation;
    procedure MissileLocation;
    function CalcPlayerWidth(index, oldx, oldy : byte) : boolean;
    procedure SetObjects;
    procedure RefreshValues;
    procedure InitPmValues;
    procedure SelectMissile;
    procedure ShowMissileData(showData : byte);
    procedure FlipXProc(oper : byte);
    procedure FlipYProc(oper : byte);
  public
    { public declarations }
    filename : string;
    playerFiles : array[0..3] of string;
    missileFiles : array[0..3] of string;
    mplFile : string;
    fld : fldType;
    missiles : array[0..3, 0..1, 0..220] of byte;
    missileMaxY,    // Missile Y dimension
    player : byte;  // Player index
    factX, factY : byte;      // Player editor factor
    factX02, factY02 : byte;  // Multi-color player editor factor
    factX03 : byte;  // Multi-color player editor factor
    missileData : array[0..3, 0..39] of byte;
    playerPos : array[0..3, 0..80, 0..220] of byte;  // Player matrix and position
    playerIndex : array[0..3] of byte;  // Player position
    arrPosX : array[0..3] of byte;
    arrPosY : array[0..3] of byte;
    arrMissilePosX : array[0..3] of byte;
    arrMissilePosY : array[0..3] of byte;
    procedure SetPlayers;
    procedure FillRectEx(image : TImage; col : TColor; x, y : integer; xw, yw : byte);
    procedure RefreshPM(isMultiColor : boolean);
    procedure RefreshAll;
    procedure RefreshMissile;
    procedure ToggleChange(Sender : TObject; tgl01, tgl02, tgl03 : TToggleBox; plSize : byte);
  end;

var
  frmPmg: TfrmPmg;

implementation

{$R *.lfm}

uses
  main, pmg_gen, colors, pm_apl, lib;

{ TfrmPmg }

procedure TfrmPmg.FormCreate(Sender: TObject);
var
  i, j : word;
  labelx : TLabel;
begin
  DoubleBuffered := true;
  btn := mbMiddle;
  FillByte(playerSize, SizeOf(playerSize), 0);
  grY := _PM_MAX_LINES - 1;
  factX := 16;
  factX02 := 12; factY02 := 12;
  factX03 := 12;
//  playerSize[0] := playerSizeNormal;
  missileMaxY := 34;
  for i := 0 to _PM_MAX_LINES - 1 do
    for j := 0 to 3 do begin
      labelx := TLabel.Create(tabMultiPM);
      labelx.Parent := tabMultiPM;
      labelx.Name := 'lblMultiPmValue0' + inttostr(j) + inttostr(i);
      labelx.Left := 770 + 46*j;
      labelx.Font.Name := 'Verdana';
//      labelx.Font.Size := 8;
//      labelx.Caption := 'P' + inttostr(j) + ': 0';
    end;

  for j := 0 to 3 do begin
    // Missile line values
    for i := 0 to missileMaxY - 1 do begin
      labelx := TLabel.Create(tabMissiles);
      labelx.Parent := tabMissiles;
      labelx.Name := 'lblMissile' + inttostr(i + 40*j);
      labelx.Left := 58 + 78*j;
      labelx.Font.Name := 'Verdana';
    end;
    playerSize[j] := _PLAYER_SIZE_NORMAL;
    missileSizes[j] := _PLAYER_SIZE_NORMAL;
    arrPosX[j] := 80 + 10*j;
    arrPosY[j] := 40;
    arrMissilePosX[j] := 110 + j + j;
    arrMissilePosY[j] := 60 + 10*j;
  end;

  // Player editor line index
  for i := 0 to _PM_MAX_LINES - 1 do begin
    labelx := TLabel.Create(tabMissiles);
    labelx.Parent := tabPlayers;
    labelx.Name := 'lblPlayerLine' + inttostr(i);
    labelx.Left := 0;
    labelx.Font.Name := 'Verdana';
  end;

  // Missile editor line index
  for i := 0 to missileMaxY - 1 do begin
    labelx := TLabel.Create(tabMissiles);
    labelx.Parent := tabMissiles;
    labelx.Name := 'lblMissileLine' + inttostr(i);
    labelx.Left := 0;
    labelx.Font.Name := 'Verdana';
  end;

//  pmResolution := [doubleResolution];
//  PmgSettings;
  isValueDec := true;

  // Set colors for palette toolbar
  //pmColor0.Brush.Color := coltab[0];
end;

procedure TfrmPmg.FormShow(Sender: TObject);
var
  i : byte;
begin
  propFlagModules[2] := 1;
  isChange := true;
  frmMain.Top := 0;
  formId := formPmg;

  SetPlayers;
  SetMissiles;
//  filename := getDir + 'examples\player01.spr';
  caption := programName + ' ' + programVersion + ' - Player/missile graphics editor';
  sbPmg.Panels[0].Text := 'Cursor coordinates: x: 0, y: 0';
  InitPmValues;
  tabs.TabIndex := 0;
  for i := 0 to 3 do begin
    (FindComponent('imgMissile' + IntToStr(i)) as TImage).
      Canvas.Brush.Color := clBlack;
    (FindComponent('imgMissileSingle' + IntToStr(i)) as TImage).
      Canvas.Brush.Color := clBlack;
  end;
  SetObjects;
  ClearData(mnuClearAllMissiles);
  player := 0;
  tbHorizontal.Position := arrPosX[player];
  tbVertical.Position := arrPosY[player];
  PlayerLocation;
  MissileLocation;
//  sbPmg.Panels[1].Text := 'Player/missile double resolution';

  SetDoubleResolution(Sender);
end;

procedure TfrmPmg.FormActivate(Sender: TObject);
begin
  formId := formPmg;
  SetObjects;
end;

procedure TfrmPmg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: SetRotate(false);
  end;
(*  if (ssCtrl in Shift) and (Key = VK_D) then begin
    isValueDec := true;
    RefreshValues;
  end
  else if (ssCtrl in Shift) and (Key = VK_H) then begin
    isValueDec := false;
    RefreshValues;
  end;*)
end;

procedure TfrmPmg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[2] := 0;
end;

procedure TfrmPmg.PmgSettings;
begin
  if pmResolution = doubleResolution then begin
    factY := 16;
    factY02 := 12;
  end
  else begin
    factY := 8;
    factY02 := 6;
  end;

  InitPmValues;
  refreshPM(true);
  ShowMissileData(3);
  imgPlayer4.Height := missileMaxY*factY;
  imgPlayer4.Canvas.Brush.Color := clBlack;
  imgPlayer4.Canvas.FillRect(bounds(0, 0, imgPlayer4.Width, imgPlayer4.Height));
  RedrawPlayer4;
  RefresPM_MultiAll(editorMulti, factX02, factY02);
end;

procedure TfrmPmg.PlayerOper(Sender : TObject);
begin
  if radShiftPlayer.Checked or radShiftAllPlayers.Checked then begin
    case (Sender as TSpeedButton).Tag of
      0: ShiftLeft(Sender);
      1: ShiftRight(Sender);
      2: ShiftUp(Sender);
      3: ShiftDown(Sender);
    end;
  end
  else begin
    case (Sender as TSpeedButton).Tag of
      0: MoveLeft(Sender);
      1: MoveRight(Sender);
      2: MoveUp(Sender);
      3: MoveDown(Sender);
    end;
  end;
end;

procedure TfrmPmg.chkJoinPlayersChange(Sender : TObject);
var
  xx, yy : byte;
  oldPos : byte;
  value : byte;
  i : byte;
begin
  for i := 0 to 3 do begin
    oldPos := playerIndex[i];
    playerIndex[i] := playerIndex[player];
    for yy := 0 to _PM_MAX_LINES - 1 do
      for xx := 0 to _PM_WIDTH do begin
        value := playerPos[i, oldPos + xx, yy];
        playerPos[i, oldPos + xx, yy] := 0;
        playerPos[i, playerIndex[i] + xx, yy] := value;
      end;
  end;
  //else begin
  //  for yy := 0 to _PM_MAX_LINES - 1 do begin
  //    for xx := _PM_WIDTH downto 0 do begin
  //      value := playerPos[i, oldPos + xx, yy];
  //      playerPos[i, oldPos + xx, yy] := 0;
  //      playerPos[i, playerIndex[player] + xx, yy] := value;
  //    end;
  //  end;
  //end;
//  RefreshPm;
  RefresPM_MultiAll(editorMulti, factX02, factY02);
end;

procedure TfrmPmg.imgP0_normalMouseDown(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  if Button = mbRight then
    popMenu.Popup(x + 40, y + 40);
end;

procedure TfrmPmg.CopyPlayer0(Sender : TObject);
var
  i, j : byte;
begin
  for j := 0 to _PM_MAX_LINES - 1 do
    for i := 0 to 7 do
      fld[player, i, j] := fld[0, i, j];

  RefreshPm(true);
end;

procedure TfrmPmg.CopyPlayer1(Sender : TObject);
var
  i, j : byte;
begin
  for j := 0 to _PM_MAX_LINES - 1 do
    for i := 0 to 7 do
      fld[player, i, j] := fld[1, i, j];

  RefreshPm(true);
end;

procedure TfrmPmg.CopyPlayer2(Sender : TObject);
var
  i, j : byte;
begin
  for j := 0 to _PM_MAX_LINES - 1 do
    for i := 0 to 7 do
      fld[player, i, j] := fld[2, i, j];

  RefreshPm(true);
end;

procedure TfrmPmg.CopyPlayer3(Sender : TObject);
var
  i, j : byte;
begin
  for j := 0 to _PM_MAX_LINES - 1 do
    for i := 0 to 7 do
      fld[player, i, j] := fld[3, i, j];

  RefreshPm(true);
end;

procedure TfrmPmg.CopyToAllPlayers(Sender : TObject);
var
  i, j, pl : byte;
  selPlayer : byte;
begin
  selPlayer := player;
  for pl := 0 to 3 do
    if selPlayer <> pl then begin
      for j := 0 to _PM_MAX_LINES - 1 do
        for i := 0 to 7 do
          fld[pl, i, j] := fld[selPlayer, i, j];

      player := pl;
      RefreshPm(true);
    end;

  player := selPlayer;
  RefreshPm(true);
end;

procedure TfrmPmg.btnApplyPlayerHeightClick(Sender : TObject);
begin
  _PM_MAX_LINES := editPlayerHeight.Value;
  PmgSettings;
end;

procedure TfrmPmg.btnApplyMissileHeightClick(Sender : TObject);
begin
  missileMaxY := editMissileHeight.Value;
  imgMissile00.Height := missileMaxY*factY;
  imgMissile01.Height := imgMissile00.Height;
  imgMissile02.Height := imgMissile00.Height;
  imgMissile03.Height := imgMissile00.Height;
  imgPlayer4.Height := imgMissile00.Height;
  PmgSettings;
end;

procedure TfrmPmg.SaveDataAll(Sender: TObject);
begin
  if tabs.ActivePage = tabPlayers then
    SavePlayerDataAll(Sender)
  else if tabs.ActivePage = tabMissiles then
    SaveMissileDataAll(Sender);
end;

procedure TfrmPmg.SetSingleResolution(Sender: TObject);
begin
  pmResolution := singleResolution;
  sbPmg.Panels[1].Text := 'Player/missile single resolution';
  PmgSettings;
end;

procedure TfrmPmg.SetDoubleResolution(Sender: TObject);
begin
  pmResolution := doubleResolution;
  sbPmg.Panels[1].Text := 'Player/missile double resolution';
  PmgSettings;
end;

procedure TfrmPmg.editMissilePosXChange(Sender: TObject);
begin
  tbMissileHorizontal.Position := editMissilePosX.Value;
  MissileLocation;
end;

procedure TfrmPmg.editMissilePosYChange(Sender: TObject);
begin
  tbMissileVertical.Position := editMissilePosY.Value;
  MissileLocation;
end;

procedure TfrmPmg.panelMissileClick(Sender: TObject);
begin
  player := TPanel(Sender).Tag;
  (FindComponent('rbPlayer' + IntToStr(player)) as TRadioButton).Checked := true;

  SelectMissile;
  lblPlayer.Caption := ' player ' + IntToStr(player);

  //if playerFiles[player] <> '' then begin
  //  caption := programName + ' ' + programVersion + ' - Player/missile graphics editor';
  //  caption := caption + ' (' + playerFiles[player] + ')';
  //end;
  if missileFiles[player] <> '' then begin
    caption := programName + ' ' + programVersion + ' - Player/missile graphics editor';
    caption := caption + ' (' + missileFiles[player] + ')';
  end;
  RefreshPM(true);
  RefreshMissile;
  tbMissileHorizontal.Position := arrMissilePosX[player];
  tbMissileVertical.Position := arrMissilePosY[player];
end;

procedure TfrmPmg.SaveMissileDataAll(Sender: TObject);
var
  i : byte;
begin
  if isMissileDataChanged[0] or isMissileDataChanged[1] or
     isMissileDataChanged[2] or isMissileDataChanged[3] then
  begin
    for i := 0 to 3 do begin
      if isMissileDataChanged[i] and (missileFiles[i] = '') then begin
        player := i;
        (FindComponent('rbPlayer' + IntToStr(player)) as TRadioButton).Checked := true;
        SaveDataAs(Sender);
      end
      else begin
        if isMissileDataChanged[i] then
          SaveMissile(missileFiles[i]);
//        else
//          ShowMessage('There is no data to be saved for missile ' + IntToStr(i) + '!');
      end;
    end;
  end
  else begin
    ShowMessage('There is no missile data to be saved!');
  end;
end;

procedure TfrmPmg.tbMissileHorizontalChange(Sender: TObject);
begin
  editMissilePosX.Value := tbMissileHorizontal.Position;
  arrMissilePosX[player] := tbMissileHorizontal.Position;
  MissileLocation;
end;

procedure TfrmPmg.tbMissileVerticalChange(Sender: TObject);
begin
  editMissilePosY.Value := tbMissileVertical.Position;
  arrMissilePosY[player] := tbMissileVertical.Position;
  MissileLocation;
end;

procedure TfrmPmg.InitPmValues;
var
  i, j : byte;
  bin : string[8];
begin
  editor.Height := _PM_MAX_LINES*factY;
  editorMulti.Height := _PM_MAX_LINES*factY02;

  for j := 0 to _PM_MAX_LINES - 1 do begin
    bin := '';
    for i := 0 to 7 do
      bin += IntToStr(fld[player, i, j]);

    (FindComponent('lblNum' + IntToStr(j)) as TLabel).Caption :=
      IntToStr(Bin2Dec(bin)) + ' ($' + Dec2Hex(Bin2Dec(bin)) + ')';
    (FindComponent('lblNum' + IntToStr(j)) as TLabel).Left := 150;

    if pmResolution = doubleResolution then begin
      (FindComponent('lblNum' + IntToStr(j)) as TLabel).Top := 18 + j shl 4;
      (FindComponent('lblNum' + IntToStr(j)) as TLabel).Font.Size := 8;
      (FindComponent('lblNum' + IntToStr(j)) as TLabel).Font.Style := [];
    end
    else begin
      (FindComponent('lblNum' + IntToStr(j)) as TLabel).Top := 18 + j shl 3;
      (FindComponent('lblNum' + IntToStr(j)) as TLabel).Font.Size := 6;
      (FindComponent('lblNum' + IntToStr(j)) as TLabel).Font.Style := [];
    end;
  end;
end;

procedure TfrmPmg.imgMissile00aMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  player := TImage(Sender).Tag;
  (FindComponent('rbPlayer' + IntToStr(player)) as TRadioButton).Checked := true;

  btn := Button;

//  xf := X div factX;
//  yf := Y div factY;

  // Check for mouse button clicked
  if btn = mbRight then
    varBtn := btn
  else
    varBtn := mbLeft;

//  PlotMissileMultiAll(xf, yf);
end;

procedure TfrmPmg.imgMissile00aMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
begin
  xf := X div factX;
  yf := Y div factY;
//  PlotMissileMultiAll(xf, yf);
  sbPmg.Panels[0].Text := 'Cursor coordinates: ' +
                          'x: ' + inttostr(xf) + ', y: ' + inttostr(yf);
end;

procedure TfrmPmg.PlayerLocation;
begin
// Double resolution
// A=44, B=114 - min x
// A=204 - max x
  imgPlayerLocation.Canvas.Brush.Color := colTab[0];
  imgPlayerLocation.Canvas.FillRect(bounds(0, 0,
                                           imgPlayerLocation.Width, imgPlayerLocation.Height));
  imgPlayerLocation.Canvas.Brush.Color := colTab[3];
  imgPlayerLocation.Canvas.FillRect(bounds(6, 18, imgPlayerLocation.Width - 12,
                                           imgPlayerLocation.Height - 36));
  imgPlayerLocation.Canvas.Brush.Color := coltab[player + 4];
  imgPlayerLocation.Canvas.FillRect(bounds(tbHorizontal.Position - 44, tbVertical.Position,
                                           8, 10));
end;

procedure TfrmPmg.MissileLocation;
begin
  //imgMissileLocation.Canvas.Brush.Color := colTab[0];
  //imgMissileLocation.Canvas.FillRect(bounds(
  //  0, 0, imgMissileLocation.Width, imgMissileLocation.Height));
  //imgMissileLocation.Canvas.Brush.Color := colTab[0];
  //imgMissileLocation.Canvas.FillRect(bounds(
  //  8, 15, imgMissileLocation.Width - 32, imgMissileLocation.Height - 60));
  //imgMissileLocation.Canvas.Brush.Color := coltab[player + 4];
  //imgMissileLocation.Canvas.FillRect(bounds(
  //  2*tbMissileHorizontal.Position - 76, tbMissileVertical.Position shl 1, 4, 9));
  imgMissileLocation.Canvas.Brush.Color := colTab[0];
  imgMissileLocation.Canvas.FillRect(bounds(
    0, 0, imgMissileLocation.Width, imgMissileLocation.Height));
  imgMissileLocation.Canvas.Brush.Color := colTab[3];
  imgMissileLocation.Canvas.FillRect(bounds(
    6, 18, imgMissileLocation.Width - 12, imgMissileLocation.Height - 36));
  imgMissileLocation.Canvas.Brush.Color := coltab[player + 4];
  imgMissileLocation.Canvas.FillRect(bounds(
    tbMissileHorizontal.Position - 44, tbMissileVertical.Position, 4, 9));
end;

procedure TfrmPmg.editorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : integer;
begin
  if (Sender as TImage).Tag = 555 then
    Exit;

  if (Button = mbRight) and (ssCtrl in Shift) then begin
    popMenu.Popup(x + editor.Left + 40, y + editor.Top + 40);
    exit;
  end;

  btn := Button;
  isDataChanged[player] := true;
  //if (btn = mbLeft) and isRotate then begin
  //  n := shape03.Top;
  //  for i := 0 to _PM_MAX_LINES - 1 do
  //    if (n >= 20 + 16*i) and (n < 30 + 16*i) then begin
  //      offset := i;
  //      break;
  //    end;
  //
  //  for yf := 0 to 7 do
  //    for xf := 0 to 7 do begin
  //      arr[yf, xf] := fld[player, xf, yf + offset];
  //      fld[player, 7 - xf, yf + offset] := arr[xf, yf];
  //    end;
  //
  //  refreshPM;
  //end else begin

  xf := X div factX;
  yf := Y div factY;

  // Check for mouse button clicked
  if btn = mbRight then
    varBtn := btn
  else
    varBtn := mbLeft;

  Plot(xf, yf);
end;

procedure TfrmPmg.editorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
begin
  //if isRotate then begin
  //  shape01.Top := Y;
  //  shape02.Top := Y;
  //  shape03.Top := shape01.Top;
  //  shape04.Top := shape01.Top + shape01.Height;
  //end
  //else begin

  xf := X div factX;
  yf := Y div factY;
  Plot(xf, yf);
  sbPmg.Panels[0].Text := 'Cursor coordinates: ' + 'x: ' +
                           inttostr(xf) + ', y: ' + inttostr(yf);
end;

procedure TfrmPmg.editorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
  refreshPM(true);
end;

procedure TfrmPmg.tabMultiAllShow(Sender: TObject);
begin
  rbPlayerChange(Sender);
end;

procedure TfrmPmg.tabMultiShow(Sender: TObject);
begin
  rbPlayerChange(Sender);
end;

procedure TfrmPmg.tabNormalShow(Sender: TObject);
begin
  rbPlayerChange(Sender);
end;

procedure TfrmPmg.tabPlayersShow(Sender: TObject);
begin
  rbPlayerChange(Sender);
end;

procedure TfrmPmg.tabsChange(Sender: TObject);
begin
  if tabs.ActivePage = tabPlayers then begin
    btnLoadData.Hint := 'Load player data';
    btnSaveData.Hint := 'Save player data';
    btnSaveDataAll.Hint := 'Save data for all players';
    btnClearData.Hint := 'Clear player image data';
    grpShiftMovePlayer.Enabled := true;
    grpShiftMovePlayer.Visible := true;
    grpFlipPlayers.Enabled := true;
    grpFlipPlayers.Visible := true;
    btnByteEditor.Enabled := true;
  end
  else if tabs.ActivePage = tabMultiPM then begin
    btnLoadData.Hint := 'Load multi-color player data';
    btnSaveData.Hint := 'Save multi-color player data';
    btnSaveDataAll.Hint := 'Save data for all players';
    btnClearData.Hint := 'Clear multi-color player image data';

    if isPmMixedColor then
      sbPmg.Panels[2].Text := 'Player mixed (3rd) color enabled'
    else
      sbPmg.Panels[2].Text := 'Player mixed (3rd) color disabled';

    RefreshPM(true);
    InitPmValues;
    RefresPM_MultiAll(editorMulti, factX02, factY02);
//    RefreshValues;
    grpShiftMovePlayer.Enabled := true;
    grpShiftMovePlayer.Visible := true;
    grpFlipPlayers.Enabled := true;
    grpFlipPlayers.Visible := true;
    btnByteEditor.Enabled := true;
  end
  else begin
    btnLoadData.Hint := 'Load missile data';
    btnSaveData.Hint := 'Save missile data';
    btnSaveDataAll.Hint := 'Save data for all missiles';
    btnClearData.Hint := 'Clear missile image data';
    grpShiftMovePlayer.Enabled := false;
    grpShiftMovePlayer.Visible := false;
    grpFlipPlayers.Enabled := false;
    grpFlipPlayers.Visible := false;
    btnByteEditor.Enabled := false;
  end;
end;

procedure TfrmPmg.tbPlayerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xx, yy : byte;
  oldPos : byte;
  value : byte;
  i : byte;
begin
  if ((ssLeft in Shift) = false) or
     (playerSize[player] > _PLAYER_SIZE_NORMAL) then
  begin
    exit;
  end;

  player := (Sender as TTrackBar).Tag;
  oldPos := playerIndex[player];
  playerIndex[player] := (Sender as TTrackBar).Position;
  (FindComponent('rbPlayer' + IntToStr(player)) as TRadioButton).Checked := true;

  if oldPos = playerIndex[player] then
    exit;
(*
  if btn = mbRight then begin
  end
  else begin
    playerPos[player, xf, yf] := col;

    if playerSize[player] = _PLAYER_SIZE_DOUBLE then
      playerPos[player, xf + 1, yf] := col
    else if playerSize[player] = _PLAYER_SIZE_QUADRUPLE then begin
      playerPos[player, xf + 1, yf] := col;
      playerPos[player, xf + 2, yf] := col;
      playerPos[player, xf + 3, yf] := col;
    end;
  end;

  // Player data
  for yf := 0 to _PM_MAX_LINES - 1 do begin
    d := 0;
    for xf := 0 to _PM_WIDTH do begin
      if playerSize[player] = _PLAYER_SIZE_NORMAL then
        playerPos[player, playerIndex[player] + xf, yf] := fld[player, xf, yf]
      else if playerSize[player] = _PLAYER_SIZE_DOUBLE then begin
        playerPos[player, playerIndex[player] + xf + d, yf] := fld[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 1, yf] := fld[player, xf, yf];
        Inc(d);
      end
      else if playerSize[player] = _PLAYER_SIZE_QUADRUPLE then begin
        playerPos[player, playerIndex[player] + xf + d, yf] := fld[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 1, yf] := fld[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 2, yf] := fld[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 3, yf] := fld[player, xf, yf];
        Inc(d, 3);
      end
    end;
  end;
*)
  if (Sender as TTrackBar).Position < oldPos then begin
    for yy := 0 to _PM_MAX_LINES - 1 do
//      d := 0;
      for xx := 0 to _PM_WIDTH do begin
        value := playerPos[player, oldPos + xx, yy];
//        if playerSize[player] = _PLAYER_SIZE_NORMAL then begin
          playerPos[player, oldPos + xx, yy] := 0;
          playerPos[player, playerIndex[player] + xx, yy] := value;
//        end
        //else if playerSize[player] = _PLAYER_SIZE_DOUBLE then begin
        //  playerPos[player, oldPos + xx + d, yy] := 0;
        //  playerPos[player, oldPos + xx + d + 1, yy] := 0;
        //  playerPos[player, playerIndex[player] + xx + d, yy] := value;
        //  playerPos[player, playerIndex[player] + xx + d + 1, yy] := value;
        //  Inc(d);
        //end;
      end;
  end
  else begin
    for yy := 0 to _PM_MAX_LINES - 1 do
//      d := 0;
      for xx := _PM_WIDTH downto 0 do begin
        value := playerPos[player, oldPos + xx, yy];
//        if playerSize[player] = _PLAYER_SIZE_NORMAL then begin
          playerPos[player, oldPos + xx, yy] := 0;
          playerPos[player, playerIndex[player] + xx, yy] := value;
        //end
        //else if playerSize[player] = _PLAYER_SIZE_DOUBLE then begin
        //  playerPos[player, oldPos + xx + d, yy] := 0;
        //  playerPos[player, oldPos + xx + d + 1, yy] := 0;
        //  playerPos[player, playerIndex[player] + xx + d, yy] := value;
        //  playerPos[player, playerIndex[player] + xx + d + 1, yy] := value;
        //  Inc(d);
        //end;
      end;
  end;

  tbOldX[player] := playerIndex[player];
  isDataChanged[player] := true;

  sbPmg.Panels[0].Text := inttostr(playerIndex[player]);

  if chkJoinPlayers.Checked then begin
    for i := 0 to 3 do begin
      //playerIndex[i] := (Sender as TTrackBar).Position;
      //(FindComponent('rbPlayer' + IntToStr(player)) as TRadioButton).Checked := true;
      //
      //if oldPos = playerIndex[player] then
      //  exit;
      //
      //if (Sender as TTrackBar).Position < oldPos then begin
      //  for yy := 0 to _PM_MAX_LINES - 1 do begin
      //    for xx := 0 to _PM_WIDTH do begin
      //      value := playerPos[player, oldPos + xx, yy];
      //      playerPos[i, oldPos + xx, yy] := 0;
      //      playerPos[i, playerIndex[i] + xx, yy] := value;
      //    end;
      //  end;
      //end
      //else begin
      //  for yy := 0 to _PM_MAX_LINES - 1 do begin
      //    for xx := _PM_WIDTH downto 0 do begin
      //      value := playerPos[player, oldPos + xx, yy];
      //      playerPos[i, oldPos + xx, yy] := 0;
      //      playerPos[i, playerIndex[player] + xx, yy] := value;
      //    end;
      //  end;
      //end;
      //
      //tbOldX[i] := playerIndex[player];
      //isDataChanged[i] := true;
      oldPos := playerIndex[i];
      playerIndex[i] := playerIndex[player];

      if (Sender as TTrackBar).Position < oldPos then begin
        for yy := 0 to _PM_MAX_LINES - 1 do
          for xx := 0 to _PM_WIDTH do begin
            value := playerPos[i, oldPos + xx, yy];
            playerPos[i, oldPos + xx, yy] := 0;
            playerPos[i, playerIndex[i] + xx, yy] := value;
          end;
      end
      else begin
        for yy := 0 to _PM_MAX_LINES - 1 do
          for xx := _PM_WIDTH downto 0 do begin
            value := playerPos[i, oldPos + xx, yy];
            playerPos[i, oldPos + xx, yy] := 0;
            playerPos[i, playerIndex[player] + xx, yy] := value;
          end;
      end;
    end;
  end;
//  refreshPM;
  RefresPM_MultiAll(editorMulti, factX02, factY02);
end;

procedure TfrmPmg.tbHorizontalChange(Sender: TObject);
begin
  editPosX.Value := tbHorizontal.Position;
  arrPosX[player] := tbHorizontal.Position;
  PlayerLocation;
end;

procedure TfrmPmg.tbPlayer0Click(Sender : TObject);
begin
  rbPlayer0.Checked := true;
  rbPlayerChange(Sender);
end;

procedure TfrmPmg.tbPlayer1Click(Sender : TObject);
begin
  rbPlayer1.Checked := true;
  rbPlayerChange(Sender);
end;

procedure TfrmPmg.tbPlayer2Click(Sender : TObject);
begin
  rbPlayer2.Checked := true;
  rbPlayerChange(Sender);
end;

procedure TfrmPmg.tbPlayer3Click(Sender : TObject);
begin
  rbPlayer3.Checked := true;
  rbPlayerChange(Sender);
end;

procedure TfrmPmg.tbVerticalChange(Sender: TObject);
begin
  editPosY.Value := tbVertical.Position;
  arrPosY[player] := tbVertical.Position;
  PlayerLocation;
end;

procedure TfrmPmg.SetRotate(isRotatex : boolean);
begin
  isRotate := isRotatex;
  shape01.Visible := isRotatex;
  shape02.Visible := isRotatex;
  shape03.Visible := isRotatex;
  shape04.Visible := isRotatex;
  if isRotate then
    sbPmg.Panels[3].Text := 'Press Esc key to exit Rotate mode'
  else
    sbPmg.Panels[3].Text := '';
end;

procedure TfrmPmg.LoadPlayerData;
var
  x, y, h, col : byte;
  bin : string;
begin
  ClearData(mnuClearPlayer);

  // Read player height
  h := fs.ReadByte;
  //h := _PM_MAX_LINES;

  // Read player color
  col := fs.ReadByte;
  coltab[player + 4] := colorMem[col div 2];
  colorValues[player + 4] := col;

  // Read player data
  for y := 0 to h - 1 do begin
    //for x := 0 to 7 do begin
    //  dta := Char(fs.ReadByte);
    //  fld[player, x, y] := StrToInt(dta);
    //  playerPos[player, playerIndex[player] + x, y] := fld[player, x, y];
    //end;
    x := fs.ReadByte;
    bin := Dec2Bin(x);
    for x := 0 to 7 do begin
      fld[player, x, y] := StrToInt(bin[x + 1]);
      playerPos[player, playerIndex[player] + x, y] := fld[player, x, y];
    end;
  end;
end;

{-----------------------------------------------------------------------------
 Load multi-color players
 -----------------------------------------------------------------------------}
procedure TfrmPmg.LoadMplData(Sender : TObject);
var
  x, y : integer;
  i, j, h : byte;
  mplTypeIndex : byte = 11;
  bin : string;
begin
  SetPlayers;

//  showmessage((Sender as TMenuItem).Name);

  // Read player height
  h := fs.ReadByte;  // _PM_MAX_LINES

  if (Sender as TComponent).Name = 'mnuMplLoadOld' then
//  if menuItem <> mnuMplLoadOld then
    mplTypeIndex := 7;

  for i := 0 to mplTypeIndex do begin
    // Player X-position
    if i < 4 then
      playerIndex[i] := fs.ReadByte
//            showmessage('playerIndex[i] = ' + IntToStr(playerIndex[i]));
    // Player color
    else if (i >= 4) and (i <= 7) then begin
      j := fs.ReadByte;
      coltab[i] := colorMem[j div 2];
      colorValues[i] := j;
    end
    // Player size
    else begin
      if (Sender as TComponent).Name <> 'mnuMplLoadOld' then begin
        j := fs.ReadByte;
        if i = 8 then
          playerSize[0] := j
        else if i = 9 then
          playerSize[1] := j
        else if i = 10 then
          playerSize[2] := j
        else if i = 11 then
          playerSize[3] := j;
      end;
    end;
  end;

  // 3rd color enable flag
  if (Sender as TComponent).Name <> 'mnuMplLoadOld' then begin
    j := fs.ReadByte;
    if j = 0 then begin
      isPmMixedColor := false;
      sbPmg.Panels[2].Text := 'Player mixed (3rd) color disabled';
    end
    else begin
      isPmMixedColor := true;
      sbPmg.Panels[2].Text := 'Player mixed (3rd) color enabled';
    end;
  end;

//  isPmMixedColor := true;
//  sbPmg.Panels[2].Text := 'Player mixed (3rd) color enabled';

  // Player data
  for i := 0 to 3 do begin
    for y := 0 to h - 1 do begin
      x := fs.ReadByte;
      bin := Dec2Bin(x);
      for x := 0 to 7 do begin
        fld[i, x, y] := StrToInt(bin[x + 1]);
//        playerPos[i, playerIndex[i] + x, y] := fld[i, x, y];
      end;
    end;
//    showmessage(inttostr(playerSize[i]));
//    playerSize[i] := 1;
    player := i;

//   if (Sender as TMenuItem).Name = 'mnuMplLoadOld' then
//    if menuItem = mnuMplLoadOld then
//      ReadFld(factX02, factY02)
//    else begin
      case i of
        0: begin
          if playerSize[0] = _PLAYER_SIZE_NORMAL then
            tglP0x1Change(Sender)
          else if playerSize[0] = _PLAYER_SIZE_DOUBLE then
            tglP0x2Change(Sender)
          else
            tglP0x4Change(Sender);
        end;
        1: begin
          if playerSize[1] = _PLAYER_SIZE_NORMAL then
            tglP1x1Change(Sender)
          else if playerSize[1] = _PLAYER_SIZE_DOUBLE then
            tglP1x2Change(Sender)
          else
            tglP1x4Change(Sender);
        end;
        2: begin
          if playerSize[2] = _PLAYER_SIZE_NORMAL then
            tglP2x1Change(Sender)
          else if playerSize[2] = _PLAYER_SIZE_DOUBLE then
            tglP2x2Change(Sender)
          else
            tglP2x4Change(Sender);
        end;
        3: begin
          if playerSize[3] = _PLAYER_SIZE_NORMAL then
            tglP3x1Change(Sender)
          else if playerSize[3] = _PLAYER_SIZE_DOUBLE then
            tglP3x2Change(Sender)
          else
            tglP3x4Change(Sender);
        end;
      end;
  end;
end;

procedure TfrmPmg.LoadMissileData;
var
  x, y, col : byte;
  h : byte;
  bin : string;
begin
  ClearData(mnuClearMissile);

  // Read missile height
  h := fs.ReadByte;
  h := missileMaxY;

  // Read missile color
  col := fs.ReadByte;
  coltab[player + 4] := colorMem[col div 2];
  colorValues[player + 4] := col;

  // Read missile data
  for y := 0 to h - 1 do begin
    //for x := 0 to 7 do begin
    //  dta := Char(fs.ReadByte);
    //  fld[player, x, y] := StrToInt(dta);
    //  playerPos[player, playerIndex[player] + x, y] := fld[player, x, y];
    //end;
    x := fs.ReadByte;
    bin := Dec2Bin(x);
//    showmessage(bin);
    for x := 0 to 1 do
      missiles[player, x, y] := StrToInt(bin[x + 7]);
  end;
  RefreshMissile;
  ShowMissileData(0);

  //// Decimal values from binary data
  //for j := 0 to h - 1 do begin
  //  bin := '';
  //  for i := 0 to 1 do begin
  //    bin += IntToStr(missiles[player, i, j]);
  //  end;
  //  //for n := 0 to missileMaxY - 1 do begin
  //  //  if j = n then
  //  //    (FindComponent('lblNum' + IntToStr(j + labelOffset)) as TLabel).
  //  //      Caption := IntToStr(bin2dec(bin));  // + ' ($' + Dec2Hex(bin2dec(bin)) + ')';
  //  //end;
  //  for i := 0 to tabMissiles.ControlCount - 1 do begin
  //    for col := 0 to h - 1 do begin
  //      if j = col then begin
  //        if (tabMissiles.Controls[i].Name = 'lblMissile' + inttostr(col + player*10)) then begin
  //          tabMissiles.Controls[i].Caption := IntToStr(bin2dec(bin));
  //          break;
  //        end;
  //      end;
  //    end;
  //  end;
  //end;
end;

{-----------------------------------------------------------------------------
 Load data from file
 -----------------------------------------------------------------------------}
procedure TfrmPmg.LoadDataProc(Sender: TObject);
begin
//  showmessage(sender.ClassName);
  // Players
//  if (*(tabs.ActivePage = tabPlayers) or*) (Sender = mnuPlayerLoad) then begin
  if ((tabs.ActivePage = tabPlayers) and (Sender = btnLoadData)) or
     (Sender = mnuPlayerLoad) then
  begin
    frmMain.dlgOpen.Title := 'Open player file';
    frmMain.dlgOpen.Filter := 'Player files (*.spr)|*.spr|All files (*.*)|*.*';
    if frmMain.dlgOpen.Execute then begin
      frmPmg.Cursor := crHourGlass;
      filename := frmMain.dlgOpen.Filename;
      playerFiles[player] := filename;
      fs := TFileStream.Create(filename, fmOpenReadWrite);
      try
        LoadPlayerData;
        tbPlayer0.Position := playerIndex[0];
        tbPlayer1.Position := playerIndex[1];
        tbPlayer2.Position := playerIndex[2];
        tbPlayer3.Position := playerIndex[3];
        SetObjects;
        caption := programName + ' ' + programVersion +
                   ' - Player/missile graphics editor (' + filename + ')';
      finally
        fs.Free;
        RefreshPM(true);
        frmPmg.Cursor := crDefault;
  //      sbPmg.Panels[2].Text := 'Load: ' + inttostr(colorValues[4]) +
  //      ', ' + inttostr(colorValues[5]);
        frmColors.Show;
      end;
    end;
  end
  // Missiles
  else if (tabs.ActivePage = tabMissiles) or (Sender = mnuMissileLoad) then begin
    frmMain.dlgOpen.Title := 'Open missile file';
    frmMain.dlgOpen.Filter := 'Missile files (*.msl)|*.msl|All files (*.*)|*.*';
    if frmMain.dlgOpen.Execute then begin
      frmPmg.Cursor := crHourGlass;
      filename := frmMain.dlgOpen.Filename;
      missileFiles[player] := filename;
      fs := TFileStream.Create(filename, fmOpenReadWrite);
      try
        LoadMissileData;
        SetObjects;
        caption := programName + ' ' + programVersion +
                   ' - Player/missile graphics editor (' + filename + ')';
      finally
        fs.Free;
        RefreshPM(true);
        RedrawPlayer4;
        frmPmg.Cursor := crDefault;
  //      sbPmg.Panels[2].Text := 'Load: ' + inttostr(colorValues[4]) +
  //      ', ' + inttostr(colorValues[5]);
        frmColors.Show;
      end;
    end;
  end
  // Multi-color players
  else if ((tabs.ActivePage = tabMultiPM) and (Sender = btnLoadData)) or
    (Sender = mnuMplLoad) then
  begin
    frmMain.dlgOpen.Title := 'Open multi-color player file';
    frmMain.dlgOpen.Filter := 'Multi-color player files (*.mpl)|*.mpl|All files (*.*)|*.*';
    if frmMain.dlgOpen.Execute then begin
      frmPmg.Cursor := crHourGlass;
      filename := frmMain.dlgOpen.Filename;
      playerFiles[player] := filename;
      fs := TFileStream.Create(filename, fmOpenReadWrite);
      try
        LoadMplData(mnuMplLoad);
        tbPlayer0.Position := playerIndex[0];
        tbPlayer1.Position := playerIndex[1];
        tbPlayer2.Position := playerIndex[2];
        tbPlayer3.Position := playerIndex[3];
        SetObjects;
        caption := programName + ' ' + programVersion +
                   ' - Player/missile graphics editor (' + filename + ')';
        refreshPM(true);
      finally
        fs.Free;
        RefreshAll;
        frmPmg.Cursor := crDefault;
  //      sbPmg.Panels[2].Text := 'Load: ' + inttostr(colorValues[4]) +
  //      ', ' + inttostr(colorValues[5]);
        frmColors.Show;
      end;
    end;
  end
  // Multi-color players
  else if (*(tabs.ActivePage = tabMultiPM) or*) (Sender = mnuMplLoadOld) then begin
    frmMain.dlgOpen.Title := 'Open multi-color player file (old format)';
    frmMain.dlgOpen.Filter := 'Multi-color player files (*.mpl)|*.mpl|All files (*.*)|*.*';
    if frmMain.dlgOpen.Execute then begin
      frmPmg.Cursor := crHourGlass;
      filename := frmMain.dlgOpen.Filename;
      playerFiles[player] := filename;
      fs := TFileStream.Create(filename, fmOpenReadWrite);
      try
        LoadMplData(mnuMplLoadOld);
        tbPlayer0.Position := playerIndex[0];
        tbPlayer1.Position := playerIndex[1];
        tbPlayer2.Position := playerIndex[2];
        tbPlayer3.Position := playerIndex[3];
        SetObjects;
        caption := programName + ' ' + programVersion +
                   ' - Player/missile graphics editor (' + filename + ')';
        refreshPM(true);
      finally
        fs.Free;
        RefreshAll;
        frmPmg.Cursor := crDefault;
  //      sbPmg.Panels[2].Text := 'Load: ' + inttostr(colorValues[4]) +
  //      ', ' + inttostr(colorValues[5]);
        frmColors.Show;
      end;
    end;
  end;
end;

procedure TfrmPmg.SetObjects;
begin
  imgP0.Canvas.Brush.Color := coltab[4];
  imgP0.Canvas.FillRect(bounds(0, 0, imgP0.width, imgP0.Height));
  imgP1.Canvas.Brush.Color := coltab[5];
  imgP1.Canvas.FillRect(bounds(0, 0, imgP0.width, imgP0.Height));
  imgP2.Canvas.Brush.Color := coltab[6];
  imgP2.Canvas.FillRect(bounds(0, 0, imgP0.width, imgP0.Height));
  imgP3.Canvas.Brush.Color := coltab[7];
  imgP3.Canvas.FillRect(bounds(0, 0, imgP0.width, imgP0.Height));
  tbOldX[0] := tbPlayer0.Position;
  tbOldX[1] := tbPlayer1.Position;
  tbOldX[2] := tbPlayer2.Position;
  tbOldX[3] := tbPlayer3.Position;
end;

{-----------------------------------------------------------------------------
 Save player to file
 -----------------------------------------------------------------------------}
procedure TfrmPmg.SavePlayer(filenamexy : string);
var
  x, y : byte;
  bin : string;
begin
  fs := TFileStream.Create(filenamexy, fmCreate);
  try
    // Save player height
    fs.WriteByte(_PM_MAX_LINES);
    // Save player color
    fs.WriteByte(colorValues[player + 4]);

    // Save player data
    for y := 0 to _PM_MAX_LINES - 1 do begin
      bin := '';
      //for x := 0 to 7 do begin
      //  dta := '0';
      //  if fld[player, x, y] > 0 then dta := '1';
      //  fs.WriteByte(Byte(dta));
      //end;
      for x := 0 to 7 do
        bin += IntToStr(fld[player, x, y]);

      fs.WriteByte(bin2dec(bin));
    end;
    isDataChanged[player] := false;
  finally
    fs.Free;
  end;
end;

{-----------------------------------------------------------------------------
 Save missile to file
 -----------------------------------------------------------------------------}
procedure TfrmPmg.SaveMissile(filenamexy : string);
var
  x, y : byte;
  bin : string;
begin
  fs := TFileStream.Create(filenamexy, fmCreate);
  try
    // Save missile height
    fs.WriteByte(missileMaxY);
    // Save missile color
    fs.WriteByte(colorValues[player + 4]);

    // Save missile data
    for y := 0 to missileMaxY - 1 do begin
      bin := '';
      //for x := 0 to 7 do begin
      //  dta := '0';
      //  if fld[player, x, y] > 0 then dta := '1';
      //  fs.WriteByte(Byte(dta));
      //end;
      for x := 0 to 1 do
        bin += IntToStr(missiles[player, x, y]);

      fs.WriteByte(bin2dec(bin));
    end;
    isMissileDataChanged[player] := false;
  finally
    fs.Free;
  end;
end;

{-----------------------------------------------------------------------------
 Save multi-color player (*.mpl) to file
 -----------------------------------------------------------------------------}
procedure TfrmPmg.SaveMplData(filenamexy : string);
var
  i, x : byte;
  y : integer;
  bin : string;
begin
  //frmMain.dlgSave.Filter := 'Multi-color player files (*.mpl)|*.mpl;|All files (*.*)|*.*';
  //frmMain.dlgSave.Title := 'Save multi-color player data as';
  //frmMain.dlgSave.Filename := filename;
  //if frmMain.dlgSave.Execute then begin
  //  filename := frmMain.dlgSave.Filename;
    fs := TFileStream.Create(filenamexy, fmCreate);
    try
      // Save player height
      fs.WriteByte(_PM_MAX_LINES);
      fs.WriteByte(playerIndex[0]);
      fs.WriteByte(playerIndex[1]);
      fs.WriteByte(playerIndex[2]);
      fs.WriteByte(playerIndex[3]);
      fs.WriteByte(colorValues[4]);
      fs.WriteByte(colorValues[5]);
      fs.WriteByte(colorValues[6]);
      fs.WriteByte(colorValues[7]);
      fs.WriteByte(playerSize[0]);
      fs.WriteByte(playerSize[1]);
      fs.WriteByte(playerSize[2]);
      fs.WriteByte(playerSize[3]);

      if isPmMixedColor then
        fs.WriteByte(32)
      else
        fs.WriteByte(0);

      // Save player data
      for i := 0 to 3 do begin
        for y := 0 to _PM_MAX_LINES - 1 do begin
          bin := '';
          for x := 0 to 7 do
            bin += IntToStr(fld[i, x, y]);

          fs.WriteByte(bin2dec(bin));
        end;
        isDataChanged[i] := false;
      end;
    finally
      fs.Free;
    end;
    //caption := programName + ' ' + programVersion +
    //           ' - Player/missile graphics editor (' + filename + ')';
    ////sbPmg.Panels[2].Text := 'Save: ' + inttostr(colorValues[4]) + ', ' + inttostr(colorValues[5]);
end;

procedure TfrmPmg.SaveData(Sender: TObject);
begin
  // Players
  if (tabs.ActivePage = tabPlayers) or (Sender = mnuPlayerSave) then begin
    if isDataChanged[player] and (playerFiles[player] = '') then
      SaveDataAs(Sender)
    else begin
      if isDataChanged[player] then
        SavePlayer(playerFiles[player])
      else
        ShowMessage('There is no player data to be saved!');
    end;
  end
  // Missiles
  else if (tabs.ActivePage = tabMissiles) or (Sender = mnuMissileSave) then begin
    if isMissileDataChanged[player] and (missileFiles[player] = '') then
      SaveDataAs(Sender)
    else begin
      if isMissileDataChanged[player] then
        SaveMissile(missileFiles[player])
      else
        ShowMessage('There is no missile data to be saved!');
    end;
  end
  // Multi-color players
  else if (tabs.ActivePage = tabMultiPM) or (Sender = mnuMplSave) then begin
    if isDataChanged[player] and (mplFile = '') then
      SaveDataAs(Sender)
    else begin
      if isDataChanged[player] then
        SaveMplData(mplFile)
      else
        ShowMessage('There is no multi-color player data to be saved!');
    end;
  end;
end;

procedure TfrmPmg.ShowMissileData(showData : byte);
var
  i, j, k, col, pl : byte;
  bin : string[8];
  currPlayer : byte;
begin
  currPlayer := player;
  pl := currPlayer;

  // Iterate for all missiles
  for k := 0 to showData do begin
    if showData > 0 then
      pl := k;

    // Decimal values from binary data
    for j := 0 to missileMaxY - 1 do begin
      bin := '';
      for i := 0 to 1 do
        bin += IntToStr(missiles[pl, i, j]);

      for i := 0 to tabMissiles.ControlCount - 1 do begin
        for col := 0 to missileMaxY - 1 do begin
          if j = col then begin
            if (tabMissiles.Controls[i].Name = 'lblMissile' + inttostr(col + pl*40)) then begin
              tabMissiles.Controls[i].Caption := IntToStr(bin2dec(bin));
              if pmResolution = doubleResolution then begin
                tabMissiles.Controls[i].Top := 32 + col*16;
                tabMissiles.Controls[i].Font.Size := 8;
              end
              else begin
                tabMissiles.Controls[i].Top := 32 + col*8;
                tabMissiles.Controls[i].Font.Size := 6;
              end;
              break;
            end;
          end;

          if showData = 3 then begin
            if (tabMissiles.Controls[i].Name = 'lblMissileLine' + inttostr(col)) then begin
              tabMissiles.Controls[i].Caption := IntToStr(col);
              if pmResolution = doubleResolution then begin
                tabMissiles.Controls[i].Top := 32 + col shl 4;
                tabMissiles.Controls[i].Font.Size := 8;
              end
              else begin
                tabMissiles.Controls[i].Top := 32 + col shl 3;
                tabMissiles.Controls[i].Font.Size := 6;
              end;
              break;
            end;
          end;
        end;
      end;
      if showData > 0 then begin
        player := k;
        RefreshMissile;
      end;
    end;
  end;
  player := currPlayer;
end;

{-----------------------------------------------------------------------------
 Clear data
 -----------------------------------------------------------------------------}
procedure TfrmPmg.ClearData(Sender: TObject);
var
  i, j : byte;
begin
  // Players
  if (tabs.ActivePage = tabPlayers)
     and ((Sender = mnuClearPlayer) or (Sender = btnClearData)) then
  begin
    for i := 0 to 7 do
      for j := 0 to 220 do
        fld[player, i, j] := 0;

    for i := 0 to _MAX_PLAYER_POS - 1 do
      for j := 0 to _PM_MAX_LINES - 1 do
        playerPos[player, i, j] := 0;

    refreshPM(true);
  end
  // Missiles
  else if (tabs.ActivePage = tabMissiles)
          and ((Sender = mnuClearMissile) or (Sender = btnClearData)) then
  begin
    for i := 0 to 1 do
      for j := 0 to 220 do
        missiles[player, i, j] := 0;

    RefreshMissile;
    ShowMissileData(0);
    RedrawPlayer4;
  end
  // Multi-color players
  else if (tabs.ActivePage = tabMultiPM) or (Sender = mnuClearAllPlayers) then begin
//    SetPlayers;
//    refreshPM;
//  end
  // All players
//  else if Sender = mnuClearAllPlayers then begin
    SetPlayers;
    player := 1; refreshPM(true);
    player := 2; refreshPM(true);
    player := 3; refreshPM(true);
    player := 0; refreshPM(true);
    rbPlayer0.Checked := true;
//    RedrawPlayer4;
  end
  // All missiles
  else if Sender = mnuClearAllMissiles then begin
    // Clear missile data
    SetMissiles;
    // Show information about missile data
    ShowMissileData(2);
    // Clear player 4 area
//    imgPlayer4.Canvas.Brush.Color := clBlack;
//    imgPlayer4.Canvas.FillRect(bounds(0, 0, imgPlayer4.Width, imgPlayer4.Height));
  end;
end;

{-----------------------------------------------------------------------------
 Shift player left
 -----------------------------------------------------------------------------}
procedure TfrmPmg.ShiftLeft(Sender: TObject);
var
  x, y, n, i : byte;
  origPlayer : byte;
begin
  SetRotate(false);

  if radShiftPlayer.Checked then begin
    for y := 0 to _PM_MAX_LINES - 1 do begin
      n := fld[player, 0, y];
      for x := 1 to _PM_WIDTH do
        fld[player, x - 1, y] := fld[player, x, y];

      fld[player, 7, y] := n;
    end;
  end
  else begin
    origPlayer := player;
    for i := 0 to 3 do begin
      for y := 0 to _PM_MAX_LINES - 1 do begin
        n := fld[i, 0, y];
        for x := 1 to _PM_WIDTH do
          fld[i, x - 1, y] := fld[i, x, y];

        fld[i, 7, y] := n;
      end;
      if i <> origPlayer then begin
        player := i;
        ReadFld(factX02, factY02);
        RefreshPM(false);
      end;
    end;

//    for i := 0 to 3 do
      //if i <> origPlayer then begin
      //  player := i;
      //  ReadFld(factX02, factY02);
      //  RefreshPM;
      //end;

    player := origPlayer;
//    (FindComponent('rbPlayer' + IntToStr(player)) as TRadioButton).Checked := true;
  end;
  ReadFld(factX02, factY02);
  refreshPM(true);
end;

{-----------------------------------------------------------------------------
 Shift player right
 -----------------------------------------------------------------------------}
procedure TfrmPmg.ShiftRight(Sender: TObject);
var
  x, y, n, i : byte;
  origPlayer : byte;
begin
//  btnMoveRight(size, _PM_MAX_LINES - 1, fld);
  if radShiftPlayer.Checked then begin
    for y := 0 to _PM_MAX_LINES - 1 do begin
      n := fld[player, 7, y];
      for x := _PM_WIDTH - 1 downto 0 do
        fld[player, x + 1, y] := fld[player, x, y];

      fld[player, 0, y] := n;
    end;
  end
  else begin
    origPlayer := player;
    for i := 0 to 3 do begin
      for y := 0 to _PM_MAX_LINES - 1 do begin
        n := fld[i, 7, y];
        for x := _PM_WIDTH - 1 downto 0 do
          fld[i, x + 1, y] := fld[i, x, y];

        fld[i, 0, y] := n;
      end;
      if i <> origPlayer then begin
        player := i;
        ReadFld(factX02, factY02);
        refreshPM(false);
      end;
    end;
    player := origPlayer;
  end;
  ReadFld(factX02, factY02);
  refreshPM(true);
end;

{-----------------------------------------------------------------------------
 Shift player up
 -----------------------------------------------------------------------------}
procedure TfrmPmg.ShiftUp(Sender: TObject);
var
  x, y, i : byte;
  fld02 : array[0..7] of byte;
//  playerPos02 : array[0..3, 0..31, 0..220] of byte;
  origPlayer : byte;
begin
  if radShiftPlayer.Checked then begin
    for x := 0 to 7 do
      fld02[x] := fld[player, x, 0];

    for x := 0 to _PM_WIDTH do
      for y := 1 to _PM_MAX_LINES - 1 do begin
        fld[player, x, y - 1] := fld[player, x, y];
        fld[player, x, y] := 0;
      end;

    for x := 0 to 7 do
      fld[player, x, _PM_MAX_LINES - 1] := fld02[x];
  end
  else begin
    origPlayer := player;
    for i := 0 to 3 do begin
      for x := 0 to 7 do
        fld02[x] := fld[i, x, 0];

      for x := 0 to _PM_WIDTH do
        for y := 1 to _PM_MAX_LINES - 1 do begin
          fld[i, x, y - 1] := fld[i, x, y];
          fld[i, x, y] := 0;
        end;

      for x := 0 to 7 do
        fld[i, x, _PM_MAX_LINES - 1] := fld02[x];

      if i <> origPlayer then begin
        player := i;
        ReadFld(factX02, factY02);
        refreshPM(false);
      end;
    end;
    player := origPlayer;
  end;
  ReadFld(factX02, factY02);
  refreshPM(true);
end;

{-----------------------------------------------------------------------------
 Shift player down
 -----------------------------------------------------------------------------}
procedure TfrmPmg.ShiftDown(Sender: TObject);
var
  x, y, i : byte;
  fld02 : array[0..7] of byte;
  origPlayer : byte;
begin
  if radShiftPlayer.Checked then begin
    for x := 0 to 7 do
      fld02[x] := fld[player, x, _PM_MAX_LINES - 1];

    for x := 0 to _PM_WIDTH do
      for y := _PM_MAX_LINES - 1 downto 0 do begin
        fld[player, x, y + 1] := fld[player, x, y];
        fld[player, x, y] := 0;
      end;

    for x := 0 to 7 do
      fld[player, x, 0] := fld02[x];
  end
  else begin
    origPlayer := player;
    for i := 0 to 3 do begin
      for x := 0 to 7 do
        fld02[x] := fld[i, x, _PM_MAX_LINES - 1];

      for x := 0 to _PM_WIDTH do
        for y := _PM_MAX_LINES - 1 downto 0 do begin
          fld[i, x, y + 1] := fld[i, x, y];
          fld[i, x, y] := 0;
        end;

      for x := 0 to 7 do
        fld[i, x, 0] := fld02[x];

      if i <> origPlayer then begin
        player := i;
        ReadFld(factX02, factY02);
        refreshPM(false);
      end;
    end;
    player := origPlayer;
  end;
  ReadFld(factX02, factY02);
  refreshPM(true);
end;

{-----------------------------------------------------------------------------
 Move data left
 -----------------------------------------------------------------------------}
procedure TfrmPmg.MoveLeft(Sender: TObject);
var
  i : byte;
  origPlayer : byte;
begin
  if radMovePlayer.Checked then
    MoveLeftPM(_PM_WIDTH, _PM_MAX_LINES - 1, player, fld)
  else begin
    origPlayer := player;
    for i := 0 to 3 do begin
      MoveLeftPM(_PM_WIDTH, _PM_MAX_LINES - 1, i, fld);
      if i <> origPlayer then begin
        player := i;
        ReadFld(factX02, factY02);
        refreshPM(false);
      end;
    end;
    player := origPlayer;
  end;
  ReadFld(factX02, factY02);
  refreshPM(true);
end;

{-----------------------------------------------------------------------------
 Move data right
 -----------------------------------------------------------------------------}
procedure TfrmPmg.MoveRight(Sender: TObject);
var
  i : byte;
  origPlayer : byte;
begin
  if radMovePlayer.Checked then
    MoveRightPM(_PM_WIDTH, _PM_MAX_LINES - 1, player, fld)
  else begin
    origPlayer := player;
    for i := 0 to 3 do begin
      MoveRightPM(_PM_WIDTH, _PM_MAX_LINES - 1, i, fld);
      if i <> origPlayer then begin
        player := i;
        ReadFld(factX02, factY02);
        refreshPM(false);
      end;
    end;
    player := origPlayer;
  end;
  ReadFld(factX02, factY02);
  refreshPM(true);
end;

{-----------------------------------------------------------------------------
 Move data up
 -----------------------------------------------------------------------------}
procedure TfrmPmg.MoveUp(Sender: TObject);
var
  i : byte;
  origPlayer : byte;
begin
  if radMovePlayer.Checked then
    MoveUpPM(_PM_WIDTH, _PM_MAX_LINES - 1, player, fld)
  else begin
    origPlayer := player;
    for i := 0 to 3 do begin
      MoveUpPM(_PM_WIDTH, _PM_MAX_LINES - 1, i, fld);
      if i <> origPlayer then begin
        player := i;
        ReadFld(factX02, factY02);
        refreshPM(false);
      end;
    end;
    player := origPlayer;
  end;
  ReadFld(factX02, factY02);
  refreshPM(true);
end;

{-----------------------------------------------------------------------------
 Move data down
 -----------------------------------------------------------------------------}
procedure TfrmPmg.MoveDown(Sender: TObject);
var
  i : byte;
  origPlayer : byte;
begin
  if radMovePlayer.Checked then
    MoveDownPM(_PM_WIDTH, _PM_MAX_LINES - 1, player, fld)
  else begin
    origPlayer := player;
    for i := 0 to 3 do begin
      MoveDownPM(_PM_WIDTH, _PM_MAX_LINES - 1, i, fld);
      if i <> origPlayer then begin
        player := i;
        ReadFld(factX02, factY02);
        refreshPM(false);
      end;
    end;
    player := origPlayer;
  end;
  ReadFld(factX02, factY02);
  refreshPM(true);
end;

{-----------------------------------------------------------------------------
 Flip player horizontally
 -----------------------------------------------------------------------------}
procedure TfrmPmg.FlipX(Sender: TObject);
//var
//  x, y, m, n : byte;
begin
  FlipXProc(0);

  //SetRotate(false);
  //for y := 0 to _PM_MAX_LINES - 1 do begin
  //  for x := 7 downto 4 do begin
  //    n := fld[player, x, y];
  //    m := playerPos[player, playerIndex[player] + x, y];
  //    fld[player, x, y] := fld[player, 7 - x, y];
  //    fld[player, 7 - x, y] := n;
  //    playerPos[player, playerIndex[player] + x, y] :=
  //      playerPos[player, playerIndex[player] + 7 - x, y];
  //    playerPos[player, playerIndex[player] + 7 - x, y] := m;
  //  end;
  //end;
  //refreshPM;
end;

procedure TfrmPmg.FlipXProc(oper : byte);
var
  x, y, n, fr : byte;
  origFrame : byte;
begin
  origFrame := player;
  SetRotate(false);
  for fr := 0 to oper do begin
    if oper = 3 then player := fr;
    for y := 0 to _PM_MAX_LINES - 1 do
      for x := 7 downto 4 do begin
        n := fld[player, x, y];
//        m := playerPos[player, playerIndex[player] + x, y];
        fld[player, x, y] := fld[player, 7 - x, y];
        fld[player, 7 - x, y] := n;
      end;
  end;
  for fr := 0 to oper do begin
    if oper = 3 then player := fr;
    ReadFld(factX02, factY02);
    refreshPM(false);
  end;
  player := origFrame;
  ReadFld(factX02, factY02);
  refreshPM(true);
end;

{-----------------------------------------------------------------------------
 Flip player vertically
 -----------------------------------------------------------------------------}
procedure TfrmPmg.FlipY(Sender: TObject);
begin
  FlipYProc(0);
end;

procedure TfrmPmg.FlipYProc(oper : byte);
var
  x, y, n, fr : byte;
  origFrame : byte;
begin
  origFrame := player;
  SetRotate(false);
  for fr := 0 to oper do begin
    if oper = 3 then player := fr;
    for x := 0 to _PM_WIDTH do
      for y := _PM_MAX_LINES - 1 downto (_PM_MAX_LINES - 1) div 2 do begin
        n := fld[player, x, y];
//        m := playerPos[player, playerIndex[player] + x, y];
        fld[player, x, y] := fld[player, x, _PM_MAX_LINES - 1 - y];
        fld[player, x, _PM_MAX_LINES - 1 - y] := n;
      end;
  end;
  for fr := 0 to oper do begin
    if oper = 3 then player := fr;
    ReadFld(factX02, factY02);
    refreshPM(false);
  end;
  player := origFrame;
  ReadFld(factX02, factY02);
  refreshPM(true);
end;

{-----------------------------------------------------------------------------
 Rotate player
 -----------------------------------------------------------------------------}
procedure TfrmPmg.Rotate(Sender: TObject);
begin
  SetRotate(true);
end;

procedure TfrmPmg.SetPlayers;
var
  i, j : byte;
begin
  for i := 0 to 3 do begin
    isDataChanged[i] := false;
    playerFiles[i] := '';
  end;

  editor.Canvas.Brush.Color := clBlack;
//  editor.Canvas.FillRect(bounds(0, 0, editor.Width, editor.Height));

  if isShowPmEditorGrid then begin
    editor.Canvas.Pen.Color := clWhite;
    for i := 0 to 7 do
      for j := 0 to _PM_MAX_LINES - 1 do
        editor.Canvas.Rectangle(i*factX - 1, factY*j - 1, factX*(i + 1), factY*(j + 1));
  end;

  editorMulti.Canvas.Brush.Color := clBlack;
//  editorMulti.Canvas.FillRect(bounds(0, 0, editorMulti.Width, editorMulti.Height));

  imgP0_normal02.Canvas.Brush.Color := clBlack;
//  imgP0_normal02.Canvas.FillRect(bounds(0, 0, imgP0_normal02.Width, imgP0_normal02.Height));
  imgP1_normal02.Canvas.Brush.Color := clBlack;
//  imgP1_normal02.Canvas.FillRect(bounds(0, 0, imgP1_normal02.Width, imgP1_normal02.Height));
  imgP2_normal02.Canvas.Brush.Color := clBlack;
//  imgP2_normal02.Canvas.FillRect(bounds(0, 0, imgP2_normal02.Width, imgP2_normal02.Height));
  imgP3_normal02.Canvas.Brush.Color := clBlack;
//  imgP3_normal02.Canvas.FillRect(bounds(0, 0, imgP3_normal02.Width, imgP3_normal02.Height));

  imgP0_single.Canvas.Brush.Color := clBlack;
//  imgP0_single.Canvas.FillRect(bounds(0, 0, imgP0_single.Width, imgP0_single.Height));
  imgP1_single.Canvas.Brush.Color := clBlack;
//  imgP1_single.Canvas.FillRect(bounds(0, 0, imgP1_single.Width, imgP1_single.Height));
  imgP2_single.Canvas.Brush.Color := clBlack;
//  imgP2_single.Canvas.FillRect(bounds(0, 0, imgP2_single.Width, imgP2_single.Height));
  imgP3_single.Canvas.Brush.Color := clBlack;
//  imgP3_single.Canvas.FillRect(bounds(0, 0, imgP3_single.Width, imgP3_single.Height));

  imgP0_normal.Canvas.Brush.Color := clBlack;
//  imgP0_normal.Canvas.FillRect(bounds(0, 0, imgP0_normal.Width, imgP0_normal.Height));
  imgP0_double.Canvas.Brush.Color := clBlack;
//  imgP0_double.Canvas.FillRect(bounds(0, 0, imgP0_double.Width, imgP0_double.Height));
  imgP0_quadrable.Canvas.Brush.Color := clBlack;
//  imgP0_quadrable.Canvas.FillRect(bounds(0, 0, imgP0_quadrable.Width, imgP0_quadrable.Height));

  imgP0_normalSr.Canvas.Brush.Color := clBlack;
//  imgP0_normalSr.Canvas.FillRect(
//    bounds(0, 0, imgP0_normalSr.Width, imgP0_normalSr.Height));

  imgP0_doubleSr.Canvas.Brush.Color := clBlack;
//  imgP0_doubleSr.Canvas.FillRect(
//    bounds(0, 0, imgP0_doubleSr.Width, imgP0_doubleSr.Height));

  imgP0_quadrableSr.Canvas.Brush.Color := clBlack;
//  imgP0_quadrableSr.Canvas.FillRect(
//    bounds(0, 0, imgP0_quadrableSr.Width, imgP0_quadrableSr.Height));

//  FillByte(fld, 3*grX*grY, 0);
//  FillByte(playerPos, 3*31*grY, 0);

  //for n := 0 to 3 do begin
  //  for i := 0 to grX do
  //    for j := 0 to _PM_MAX_LINES - 1 do begin
  //      fld[n, i, j] := 0;
  //    end;
  //end;
  //
  //for n := 0 to 3 do begin
  //  for i := 0 to 31 do
  //    for j := 0 to _PM_MAX_LINES - 1 do begin
  //      playerPos[n, i, j] := 0;
  //    end;
  //end;

  FillChar(fld, SizeOf(fld), 0);
  FillChar(playerPos, SizeOf(playerPos), 0);

//  frmColors.SelColor := 5;
  if rbPlayer0.Checked then
    frmColors.SelColor := 4
  else if rbPlayer1.Checked then
    frmColors.SelColor := 5
  else if rbPlayer2.Checked then
    frmColors.SelColor := 6
  else if rbPlayer3.Checked then
    frmColors.SelColor := 7;

  playerIndex[0] := 0;
  playerIndex[1] := 0;
  playerIndex[2] := 0;
  playerIndex[3] := 0;

  tbPlayer0.Position := 0;
  tbPlayer1.Position := 0;
  tbPlayer2.Position := 0;
  tbPlayer3.Position := 0;

  tbOldX[0] := tbPlayer0.Position;
  tbOldX[1] := tbPlayer1.Position;
  tbOldX[2] := tbPlayer2.Position;
  tbOldX[3] := tbPlayer3.Position;
end;

procedure TfrmPmg.SetMissiles;
begin
  imgMissile00.Canvas.Brush.Color := clBlack;
//  imgMissile00.Canvas.FillRect(bounds(0, 0, imgMissile00.Width, imgMissile00.Height));
  imgMissile01.Canvas.Brush.Color := clBlack;
//  imgMissile01.Canvas.FillRect(bounds(0, 0, imgMissile01.Width, imgMissile01.Height));
  imgMissile02.Canvas.Brush.Color := clBlack;
//  imgMissile02.Canvas.FillRect(bounds(0, 0, imgMissile02.Width, imgMissile02.Height));
  imgMissile03.Canvas.Brush.Color := clBlack;
//  imgMissile03.Canvas.FillRect(bounds(0, 0, imgMissile03.Width, imgMissile03.Height));

  imgMissile0.Canvas.Brush.Color := clBlack;
//  imgMissile0.Canvas.FillRect(bounds(0, 0, imgMissile0.Width, imgMissile0.Height));
  imgMissile1.Canvas.Brush.Color := clBlack;
//  imgMissile1.Canvas.FillRect(bounds(0, 0, imgMissile1.Width, imgMissile1.Height));
  imgMissile2.Canvas.Brush.Color := clBlack;
//  imgMissile2.Canvas.FillRect(bounds(0, 0, imgMissile2.Width, imgMissile2.Height));
  imgMissile3.Canvas.Brush.Color := clBlack;
//  imgMissile3.Canvas.FillRect(bounds(0, 0, imgMissile3.Width, imgMissile3.Height));

  imgMissileSingle0.Canvas.Brush.Color := clBlack;
//  imgMissileSingle0.Canvas.FillRect(
//    bounds(0, 0, imgMissileSingle0.Width, imgMissileSingle0.Height));

  imgMissileSingle1.Canvas.Brush.Color := clBlack;
//  imgMissileSingle1.Canvas.FillRect(
//    bounds(0, 0, imgMissileSingle1.Width, imgMissileSingle1.Height));

  imgMissileSingle2.Canvas.Brush.Color := clBlack;
//  imgMissileSingle2.Canvas.FillRect(
//    bounds(0, 0, imgMissileSingle2.Width, imgMissileSingle2.Height));

  imgMissileSingle3.Canvas.Brush.Color := clBlack;
//  imgMissileSingle3.Canvas.FillRect(
//    bounds(0, 0, imgMissileSingle3.Width, imgMissileSingle3.Height));

  imgPlayer4.Canvas.Brush.Color := clBlack;
//  imgPlayer4.Canvas.FillRect(bounds(0, 0, imgPlayer4.Width, imgPlayer4.Height));

  imgM0_normal_dr.Canvas.Brush.Color := clBlack;
//  imgM0_normal_dr.Canvas.FillRect(bounds(0, 0, imgM0_normal_dr.Width, imgM0_normal_dr.Height));
  imgM0_double_dr.Canvas.Brush.Color := clBlack;
//  imgM0_double_dr.Canvas.FillRect(bounds(0, 0, imgM0_double_dr.Width, imgM0_double_dr.Height));
  imgM0_quadrable_dr.Canvas.Brush.Color := clBlack;
//  imgM0_quadrable_dr.Canvas.FillRect(bounds(0, 0, imgM0_quadrable_dr.Width, imgM0_quadrable_dr.Height));

  imgM0_normal_sr.Canvas.Brush.Color := clBlack;
//  imgM0_normal_sr.Canvas.FillRect(
//    bounds(0, 0, imgM0_normal_sr.Width, imgM0_normal_sr.Height));

  imgM0_double_sr.Canvas.Brush.Color := clBlack;
//  imgM0_double_sr.Canvas.FillRect(
//    bounds(0, 0, imgM0_double_sr.Width, imgM0_double_sr.Height));

  imgM0_quadrable_sr.Canvas.Brush.Color := clBlack;
//  imgM0_quadrable_sr.Canvas.FillRect(
//    bounds(0, 0, imgM0_quadrable_sr.Width, imgM0_quadrable_sr.Height));

  //for m := 0 to 3 do begin
  //  for i := 0 to 1 do
  //    for j := 0 to 220 do
  //      missiles[m, i, j] := 0;
  //end;

  FillChar(missiles,SizeOf(missiles),0);
end;

procedure TfrmPmg.FillRectEx(image : TImage; col : TColor; x, y : integer; xw, yw : byte);
begin
  image.Canvas.Brush.Color := col;
  image.Canvas.FillRect(bounds(x, y, xw, yw));
//  image.Canvas.Pixels[x, y] := col;
end;

procedure TfrmPmg.Plot(xf, yf : integer);
var
  col : byte;
  mem : byte;
begin
  case btn of
    mbLeft : col := 1;
    mbRight: col := 0;
  else
    exit;
  end;
  if xf > _PM_WIDTH then xf := _PM_WIDTH;
  fld[player, xf, yf] := col;
  playerPos[player, playerIndex[player] + xf, yf] := col;
  col := player;
  mem := col + 4;
  if player < 4 then begin
    //editor.Canvas.Brush.Color := coltab[mem];
    //editor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
    FillRectEx(editor, coltab[mem], xf*factX, yf*factY, factX, factY);

    FillRectEx(imgP0_normal, coltab[mem], xf*6, yf*6, 6, 6);
    FillRectEx(imgP0_double, coltab[mem], xf*12, yf*6, 12, 6);
    FillRectEx(imgP0_quadrable, coltab[mem], xf*24, yf*6, 24, 6);

    FillRectEx(imgP0_normalSr, coltab[mem], xf*6, yf*3, 6, 3);
    FillRectEx(imgP0_doubleSr, coltab[mem], xf*12, yf*3, 12, 3);
    FillRectEx(imgP0_quadrableSr, coltab[mem], xf*24, yf*3, 24, 3);

    //  imgP0_normal02.Canvas.Brush.Color := coltab[col + 4];
    case player of
      0: begin
           //imgP0_normal02.Canvas.Brush.Color := coltab[mem];
           //imgP0_normal02.Canvas.FillRect(bounds(xf shl 2, yf shl 2, 4, 4));
           //imgP0_normal02.Canvas.Pixels[xf shl 2, yf shl 2] := coltab[mem];
           FillRectEx(imgP0_normal02, coltab[mem], xf shl 2, yf shl 2, 4, 4);

           //imgP0_single.Canvas.Brush.Color := coltab[mem];
           //imgP0_single.Canvas.FillRect(bounds(xf shl 2, yf shl 1, 4, 2));
           //imgP0_single.Canvas.Pixels[xf shl 2, yf shl 1] := coltab[mem];
           FillRectEx(imgP0_single, coltab[mem], xf shl 2, yf shl 1, 4, 2);
         end;
      1: begin
           //imgP1_normal02.Canvas.Brush.Color := coltab[mem];
           //imgP1_normal02.Canvas.FillRect(bounds(xf shl 2, yf shl 2, 4, 4));
           //imgP1_normal02.Canvas.Pixels[xf shl 2, yf shl 2] := coltab[mem];
           FillRectEx(imgP1_normal02, coltab[mem], xf shl 2, yf shl 2, 4, 4);

           //imgP1_single.Canvas.Brush.Color := coltab[mem];
           //imgP1_single.Canvas.FillRect(bounds(xf shl 2, yf shl 1, 4, 2));
           //imgP1_single.Canvas.Pixels[xf shl 2, yf shl 1] := coltab[mem];
           FillRectEx(imgP1_single, coltab[mem], xf shl 2, yf shl 1, 4, 2);
         end;
      2: begin
           //imgP2_normal02.Canvas.Brush.Color := coltab[mem];
           //imgP2_normal02.Canvas.FillRect(bounds(xf shl 2, yf shl 2, 4, 4));
           //imgP2_normal02.Canvas.Pixels[xf shl 2, yf shl 2] := coltab[mem];
           FillRectEx(imgP2_normal02, coltab[mem], xf shl 2, yf shl 2, 4, 4);

           //imgP2_single.Canvas.Brush.Color := coltab[mem];
           //imgP2_single.Canvas.FillRect(bounds(xf shl 2, yf shl 1, 4, 2));
           //imgP2_single.Canvas.Pixels[xf shl 2, yf shl 1] := coltab[mem];
           FillRectEx(imgP2_single, coltab[mem], xf shl 2, yf shl 1, 4, 2);
         end;
      3: begin
           //imgP3_normal02.Canvas.Brush.Color := coltab[mem];
           //imgP3_normal02.Canvas.FillRect(bounds(xf shl 2, yf shl 2, 4, 4));
           //imgP3_normal02.Canvas.Pixels[xf shl 2, yf shl 2] := coltab[mem];
           FillRectEx(imgP3_normal02, coltab[mem], xf shl 2, yf shl 2, 4, 4);

           //imgP3_single.Canvas.Brush.Color := coltab[mem];
           //imgP3_single.Canvas.FillRect(bounds(xf shl 2, yf shl 1, 4, 2));
           //imgP3_single.Canvas.Pixels[xf shl 2, yf shl 1] := coltab[mem];
           FillRectEx(imgP3_single, coltab[mem], xf shl 2, yf shl 1, 4, 2);
         end;
    end;
//    imgPlayerLocation.Canvas.FillRect(bounds(tbHorizontal.Position, tbVertical.Position, 2, 2));
//    imgPlayerLocation.Canvas.FillRect(bounds(xf*2, yf*2, 2, 2));
  end;
end;

function TfrmPmg.CalcPlayerWidth(index, oldx, oldy : byte) : boolean;
var
  x, y, first : byte;
  n : integer = 0;
  posX : array[0.._MAX_PLAYER_POS - 1] of byte;
  isFlag : boolean = false;
  len : byte = 8;
begin
  FillByte(posX, _MAX_PLAYER_POS, 0);
//  for x := 0 to 31 do posX[x] := 0;

  for y := 0 to _PM_MAX_LINES - 1 do
    for x := 0 to _MAX_PLAYER_POS - 1 do begin
//       _PM_WIDTH shl 2 + 3
       n := playerPos[index, x, y];
       if n = 1 then posX[x] := 1;
//       if x < first then first := x;
    end;

  playerIndex[index] := 0;

  if playerSize[player] = _PLAYER_SIZE_DOUBLE then
    len := 16
  else if playerSize[player] = _PLAYER_SIZE_QUADRUPLE then
    len := 48;

  // Only first 8 continous bits are allowed to be filled
  for x := 0 to _MAX_PLAYER_POS - 1 do begin
    Inc(n, posX[x]);
    if (posX[x] = 1) and not isFlag then begin
      first := x;
      isFlag := true;
      playerIndex[index] := x;
//      sbPmg.Panels[3].Text := inttostr(x);
  //    (FindComponent('tbPlayer' + IntToStr(index)) as TTrackBar).Position := x;
//      tbPlayer0.Position;
    end
    else if (posX[x] = 1) and isFlag then
      if x > first + len - 1 then begin
        //sbPmg.Panels[3].Text := '1.) ' + inttostr(n);
        playerPos[index, oldx, oldy] := 0;

        if playerSize[player] = _PLAYER_SIZE_DOUBLE then
          playerPos[index, oldx + 1, oldy] := 0
        else if playerSize[player] = _PLAYER_SIZE_QUADRUPLE then begin
          playerPos[index, oldx + 1, oldy] := 0;
          playerPos[index, oldx + 2, oldy] := 0;
          playerPos[index, oldx + 3, oldy] := 0;
        end;

        result := false;
        exit;
      end;
  end;

  // Check player width
  if n > len then
    playerPos[index, oldx, oldy] := 0;

//  sbPmg.Panels[3].Text := '2.) ' + inttostr(n);

  result := (n <= len);
end;

procedure TfrmPmg.PlotMulti(xf, yf : integer);
var
  col, mem : byte;
  col01, col02, col03 : byte;
begin
  if not CalcPlayerWidth(player, xf, yf) then Exit;

  case btn of
    mbLeft : col := 1;
    mbRight: col := 0;
  else
    exit;
  end;

  if xf > _PM_WIDTH + _MAX_PLAYER_POS then begin
    xf := _PM_WIDTH + _MAX_PLAYER_POS;
//    showmessage(inttostr(xf));
  end;

  //if not CalcPlayerWidth(player, xf, yf) then
  //  Exit;

//  sbPmg.Panels[1].Text := 'Cursor coordinates: ' + 'x: ' +
//                           inttostr(xf) + ', y: ' + inttostr(yf);

  //if (player < 2) and (btn = mbRight) then begin
  //  playerPos[0, xf, yf] := 0;
  //  playerPos[1, xf, yf] := 0;
  //end
  //else if (player >= 2) and (btn = mbRight) then begin
  //  playerPos[2, xf, yf] := 0;
  //  playerPos[3, xf, yf] := 0;
  //end
  if btn = mbRight then begin
    playerPos[0, xf, yf] := 0;
    playerPos[1, xf, yf] := 0;
    playerPos[2, xf, yf] := 0;
    playerPos[3, xf, yf] := 0;
    if playerSize[player] = _PLAYER_SIZE_DOUBLE then begin
      playerPos[0, xf + 1, yf] := 0;
      playerPos[1, xf + 1, yf] := 0;
      playerPos[2, xf + 1, yf] := 0;
      playerPos[3, xf + 1, yf] := 0;
    end
    else if playerSize[player] = _PLAYER_SIZE_DOUBLE then begin
      playerPos[0, xf + 1, yf] := 0;
      playerPos[1, xf + 1, yf] := 0;
      playerPos[2, xf + 1, yf] := 0;
      playerPos[3, xf + 1, yf] := 0;
      playerPos[0, xf + 2, yf] := 0;
      playerPos[1, xf + 2, yf] := 0;
      playerPos[2, xf + 2, yf] := 0;
      playerPos[3, xf + 2, yf] := 0;
      playerPos[0, xf + 3, yf] := 0;
      playerPos[1, xf + 3, yf] := 0;
      playerPos[2, xf + 3, yf] := 0;
      playerPos[3, xf + 3, yf] := 0;
    end;
  end
  else begin
//    sbPmg.Panels[3].Text := inttostr(xf);
    playerPos[player, xf, yf] := col;

    if playerSize[player] = _PLAYER_SIZE_DOUBLE then
      playerPos[player, xf + 1, yf] := col
    else if playerSize[player] = _PLAYER_SIZE_QUADRUPLE then begin
      playerPos[player, xf + 1, yf] := col;
      playerPos[player, xf + 2, yf] := col;
      playerPos[player, xf + 3, yf] := col;
    end;
//    sbPmg.Panels[3].Text := inttostr(xf);
  end;

  isDataChanged[player] := true;

  if not CalcPlayerWidth(player, xf, yf) then
    Exit;

  col := player;
  mem := col + 4;
//  if player < 4 then begin
    //editor.Canvas.Brush.Color := coltab[col + 4];
    //editor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));

  editorMulti.Canvas.Brush.Color := coltab[mem];

  // Draw players
  col := playerPos[0, xf, yf];      // Player 0 value
  col01 := playerPos[1, xf, yf];    // Player 1 value
  col02 := playerPos[2, xf, yf];    // Player 2 value
  col03 := playerPos[3, xf, yf];    // Player 3 value

//    editorMulti.Canvas.Brush.Color := coltab[ci + 4];

  // Color register priority
  with editorMulti.Canvas do begin
    if col = 1 then begin
      if (col01 = 1) and isPmMixedColor then
        Brush.Color := coltab[4] or coltab[5]
      else
        Brush.Color := coltab[4]
    end
    else if col01 = 1 then
      Brush.Color := coltab[5]
    else if col02 = 1 then begin
      if col = 1 then begin
        if (col01 = 1) and isPmMixedColor then
          Brush.Color := coltab[4] or coltab[5]
        else
          Brush.Color := coltab[4]
      end
      else if col01 = 1 then
        Brush.Color := coltab[5]
      else if (col03 = 1) and isPmMixedColor then
        Brush.Color := coltab[6] or coltab[7]
      else
        Brush.Color := coltab[6];
    end
    else begin
      Brush.Color := clBlack;
      if (col01 = 1) then
        Brush.Color := coltab[5];

      if (col03 = 1) then
        Brush.Color := coltab[7];
    end;

    FillRect(bounds(xf*factX02, yf*factY02, factX03, factY02));
  end;

  imgP0_normal.Canvas.Brush.Color := coltab[mem];
  imgP0_double.Canvas.Brush.Color := coltab[mem];
  imgP0_quadrable.Canvas.Brush.Color := coltab[mem];

  imgP0_normalSr.Canvas.Brush.Color := coltab[mem];
  imgP0_doubleSr.Canvas.Brush.Color := coltab[mem];
  imgP0_quadrableSr.Canvas.Brush.Color := coltab[mem];

  imgP0_normal.Canvas.FillRect(bounds(xf*6, yf*6, 6, 6));
  imgP0_double.Canvas.FillRect(bounds(xf*12, yf*6, 12, 6));
  imgP0_quadrable.Canvas.FillRect(bounds(xf*24, yf*6, 24, 6));

  imgP0_normalSr.Canvas.FillRect(bounds(xf*6, yf*3, 6, 3));
  imgP0_doubleSr.Canvas.FillRect(bounds(xf*12, yf*3, 12, 3));
  imgP0_quadrableSr.Canvas.FillRect(bounds(xf*24, yf*3, 24, 3));

  case player of
    0: begin
      FillRectEx(imgP0_normal02, coltab[mem], xf shl 2, yf shl 2, 4, 4);
      FillRectEx(imgP0_single, coltab[mem], xf shl 2, yf + yf, 4, 2);
    end;
    1: begin
      FillRectEx(imgP1_normal02, coltab[mem], xf shl 2, yf shl 2, 4, 4);
      FillRectEx(imgP1_single, coltab[mem], xf shl 2, yf + yf, 4, 2);
    end;
    2: begin
      FillRectEx(imgP2_normal02, coltab[mem], xf shl 2, yf shl 2, 4, 4);
      FillRectEx(imgP2_single, coltab[mem], xf shl 2, yf + yf, 4, 2);
    end;
    3: begin
      FillRectEx(imgP3_normal02, coltab[mem], xf shl 2, yf shl 2, 4, 4);
      FillRectEx(imgP3_single, coltab[mem], xf shl 2, yf + yf, 4, 2);
    end;
  end;
end;

procedure TfrmPmg.PlotMissile(xf, yf : integer);
var
  col : byte;
  x : integer;
begin
  case btn of
    mbLeft : col := 1;
    mbRight: col := 0;
  else
    exit;
  end;
  if xf > 1 then xf := 1;
  missiles[player, xf, yf] := col;

  // Draw fifth player from missile data
  x := factX*player shl 1;

  if col = 1 then
    imgPlayer4.Canvas.Brush.Color := coltab[10]
  else
    imgPlayer4.Canvas.Brush.Color := clBlack;

  imgPlayer4.Canvas.FillRect(bounds(xf*factX + x, yf*factY, factX, factY));

//  fld[4, xf + player shl 1, yf] := missiles[player, xf, yf];
  col := player;
  isMissileDataChanged[player] := true;
  case player of
    0: begin
       //imgMissile00.Canvas.Brush.Color := coltab[col + 4];
       //imgMissile00.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
       //imgMissile00.Canvas.Pixels[xf*factX, yf*factY] := coltab[col + 4];
       FillRectEx(imgMissile00, coltab[col + 4], xf*factX, yf*factY, factX, factY);

       //imgMissile0.Canvas.Brush.Color := coltab[col + 4];
       //imgMissile0.Canvas.FillRect(bounds(xf shl 2, yf shl 2, 4, 4));
       //imgMissile0.Canvas.Pixels[xf shl 2, yf shl 2] := coltab[col + 4];
       FillRectEx(imgMissile0, coltab[col + 4], xf shl 2, yf shl 2, 4, 4);

       //imgMissileSingle0.Canvas.Brush.Color := coltab[col + 4];
       //imgMissileSingle0.Canvas.FillRect(bounds(xf shl 2, yf + yf, 4, 2));
       //imgMissileSingle0.Canvas.Pixels[xf shl 2, yf + yf] := coltab[col + 4];
       FillRectEx(imgMissileSingle0, coltab[col + 4], xf shl 2, yf + yf, 4, 2);
     end;
    1: begin
       FillRectEx(imgMissile01, coltab[col + 4], xf*factX, yf*factY, factX, factY);
       FillRectEx(imgMissile1, coltab[col + 4], xf shl 2, yf shl 2, 4, 4);
       FillRectEx(imgMissileSingle1, coltab[col + 4], xf shl 2, yf + yf, 4, 2);
     end;
    2: begin
       FillRectEx(imgMissile02, coltab[col + 4], xf*factX, yf*factY, factX, factY);
       FillRectEx(imgMissile2, coltab[col + 4], xf shl 2, yf shl 2, 4, 4);
       FillRectEx(imgMissileSingle2, coltab[col + 4], xf shl 2, yf + yf, 4, 2);
     end;
    3: begin
       FillRectEx(imgMissile03, coltab[col + 4], xf*factX, yf*factY, factX, factY);
       FillRectEx(imgMissile3, coltab[col + 4], xf shl 2, yf shl 2, 4, 4);
       FillRectEx(imgMissileSingle3, coltab[col + 4], xf shl 2, yf + yf, 4, 2);
     end;
  end;

  imgM0_normal_dr.Canvas.Brush.Color := coltab[col + 4];
  imgM0_double_dr.Canvas.Brush.Color := coltab[col + 4];
  imgM0_quadrable_dr.Canvas.Brush.Color := coltab[col + 4];
  imgM0_normal_sr.Canvas.Brush.Color := coltab[col + 4];
  imgM0_double_sr.Canvas.Brush.Color := coltab[col + 4];
  imgM0_quadrable_sr.Canvas.Brush.Color := coltab[col + 4];

  imgM0_normal_dr.Canvas.FillRect(bounds(xf*6, yf*6, 6, 6));
  imgM0_double_dr.Canvas.FillRect(bounds(xf*12, yf*6, 12, 6));
  imgM0_quadrable_dr.Canvas.FillRect(bounds(xf*24, yf*6, 24, 6));
  imgM0_normal_sr.Canvas.FillRect(bounds(xf*6, yf*3, 6, 3));
  imgM0_double_sr.Canvas.FillRect(bounds(xf*12, yf*3, 12, 3));
  imgM0_quadrable_sr.Canvas.FillRect(bounds(xf*24, yf*3, 24, 3));

  ShowMissileData(0);
end;

procedure TfrmPmg.RedrawPlayer4;
var
  i, j, pl : byte;
begin
  for pl := 0 to 3 do
    for j := 0 to missileMaxY - 1 do
      for i := 0 to 1 do begin
  //      fld[4, i, j] := missiles[player, i, j];
        if missiles[pl, i, j] = 1 then
          imgPlayer4.Canvas.Brush.Color := coltab[10]
        else
          imgPlayer4.Canvas.Brush.Color := clBlack;

        imgPlayer4.Canvas.FillRect(bounds(i*factX + factX*pl shl 1, j*factY, factX, factY));
      end;
end;

procedure TfrmPmg.RefreshPlayer(pm : TImage; factX, factY : byte);
var
  col : byte;
  xf, yf : integer;
  c, d : integer;
  index, ci : byte;
  maxX : byte;
begin
  pm.Canvas.Brush.Color := clBlack;
  pm.Canvas.Brush.Style := bsSolid;
  pm.Canvas.FillRect(bounds(0, 0, pm.Width, pm.Height));
  if pm = imgPlayer4 then begin
    ci := 10;
    index := 4
  end
  else begin
    ci := player;
    index := player;
  end;

  maxX := _PM_WIDTH;
  for yf := 0 to _PM_MAX_LINES - 1 do
    for xf := 0 to maxX do begin
      if (pm = editor) and isShowPmEditorGrid then begin
        pm.Canvas.Pen.Color := clWhite;
        c := xf*factX shl 1;
        d := yf*factY shl 1;
        if c = 0 then c := factX;
        if d = 0 then d := factY;
        pm.Canvas.Rectangle(xf*factX, yf*factY, c, d);
//        pm.Canvas.Rectangle(xf*factX, yf*factY, xf*factX*2, yf*factY*2);
      end;
      col := fld[index, xf, yf];
      if col = 1 then
        pm.Canvas.Brush.Color := coltab[ci + 4]
      else if col <> 1 then
        pm.Canvas.Brush.Color := clBlack;

      if (pm = editor) and isShowPmEditorGrid then
        pm.Canvas.FillRect(bounds(xf*factX + 1, yf*factY + 1, factX - 1, factY - 1))
      else
        pm.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY))
    end;
end;

procedure TfrmPmg.SyncPlayer;
var
  col : byte;
  xf, yf : integer;
  index : byte;
  maxX : byte;
  n : byte;
begin
  index := player;
  maxX := _MAX_PLAYER_POS - 1;
  for yf := 0 to _PM_MAX_LINES - 1 do begin
    n := 0;
    for xf := playerIndex[index] to maxX do begin
      col := playerPos[index, xf, yf];

      if playerSize[player] = _PLAYER_SIZE_NORMAL then begin
        if n < 8 then
          fld[index, n, yf] := col;

        Inc(n);
      end
      else if playerSize[player] = _PLAYER_SIZE_DOUBLE then begin
        if xf mod 2 = 0 then begin
          if n < 8*2 then
            fld[index, n, yf] := col;

          Inc(n);
        end;
      end
      else if playerSize[player] = _PLAYER_SIZE_QUADRUPLE then begin
        if xf mod 4 = 0 then begin
          if n < 8*4 then
            fld[index, n, yf] := col;

          Inc(n);
        end;
      end;
    end;
  end;
end;

procedure TfrmPmg.RefresPM_MultiAll(pm : TImage; factX, factY : byte);
var
  xf, yf : integer;
  index, ci : byte;
  col, col01, col02, col03 : byte;
  c, d : integer;
begin
  //pm.Canvas.Brush.Color := clBlack;
  //pm.Canvas.Brush.Style := bsSolid;
  //pm.Canvas.FillRect(bounds(0, 0, pm.Width, pm.Height));

  if player < 2 then
    index := 0
  else
    index := 2;

  // Player data
  //for c := 0 to 3 do
  //  for yf := 0 to _PM_MAX_LINES - 1 do begin
  //    for xf := 0 to 7 do begin
  //      if playerSize[player] = _PLAYER_SIZE_NORMAL then
  //        d := 0
  //      else if playerSize[player] = _PLAYER_SIZE_DOUBLE then
  //        d := 2
  //      else if playerSize[player] = _PLAYER_SIZE_QUADRUPLE then
  //        d := 4;
  //
  //      playerPos[c, playerIndex[c] + xf, yf] := fld[c, xf, yf];
  //      if playerSize[player] = _PLAYER_SIZE_DOUBLE then begin
  //        playerPos[c, playerIndex[c] + xf + 1, yf] := fld[c, xf, yf];
  //      end;
  //    end;
  //  end;

  ci := index;
  for yf := 0 to _PM_MAX_LINES - 1 do begin
    for xf := 0 to _PM_WIDTH + _MAX_PLAYER_POS do begin
      if (pm = editorMulti) and isShowPmEditorGrid then begin
        pm.Canvas.Pen.Color := clWhite;
        c := xf*factX shl 1;
        d := yf*factY shl 1;
        if c = 0 then c := factX;
        if d = 0 then d := factY;
        pm.Canvas.Rectangle(xf*factX, yf*factY, c, d);
      end;

//      if playerSize[player] = _PLAYER_SIZE_NORMAL then begin
        col := playerPos[0, xf, yf];      // Player 0 value
        col01 := playerPos[1, xf, yf];    // Player 1 value
        col02 := playerPos[2, xf, yf];    // Player 2 value
        col03 := playerPos[3, xf, yf];    // Player 3 value
//      end
      //else if playerSize[player] = _PLAYER_SIZE_DOUBLE then begin
      //  if xf mod 2 = 0 then begin
      //    col := playerPos[0, xf, yf];      // Player 0 value
      //    col01 := playerPos[1, xf, yf];    // Player 1 value
      //    col02 := playerPos[2, xf, yf];    // Player 2 value
      //    col03 := playerPos[3, xf, yf];    // Player 3 value
      //  end
      //end;

      pm.Canvas.Brush.Color := coltab[ci + 4];

      // Color register priority
      if col = 1 then begin
        if (col01 = 1) and isPmMixedColor then
          pm.Canvas.Brush.Color := coltab[4] or coltab[5]
        else
          pm.Canvas.Brush.Color := coltab[4]
      end
      else if col01 = 1 then
        pm.Canvas.Brush.Color := coltab[5]
      else if col02 = 1 then begin
        if col = 1 then begin
          if (col01 = 1) and isPmMixedColor then
            pm.Canvas.Brush.Color := coltab[4] or coltab[5]
          else
            pm.Canvas.Brush.Color := coltab[4]
        end
        else if col01 = 1 then
          pm.Canvas.Brush.Color := coltab[5]
        else if (col03 = 1) and isPmMixedColor then
          pm.Canvas.Brush.Color := coltab[6] or coltab[7]
        else
          pm.Canvas.Brush.Color := coltab[6];
      end
      else begin
        pm.Canvas.Brush.Color := clBlack;
        if col01 = 1 then
          pm.Canvas.Brush.Color := coltab[5];

        if col03 = 1 then
          pm.Canvas.Brush.Color := coltab[7];
      end;
      pm.Canvas.FillRect(bounds(xf*factX + 1, yf*factY + 1, factX03, factY - 1));
    end;
  end;
  pm.Refresh;
  SyncPlayer;
end;

procedure TfrmPmg.ReadFld(factX, factY : byte);
var
  xf, yf : integer;
  index, ci : byte;
  col, col01, col02, col03 : byte;
  c, d : integer;
begin
  for c := 0 to _MAX_PLAYER_POS - 1 do
    for d := 0 to _PM_MAX_LINES - 1 do
      playerPos[player, c, d] := 0;

  if player < 2 then
    index := 0
  else
    index := 2;

  // Player data
  for yf := 0 to _PM_MAX_LINES - 1 do begin
    d := 0;
    for xf := 0 to 7 do begin
      if playerSize[player] = _PLAYER_SIZE_NORMAL then
        playerPos[player, playerIndex[player] + xf, yf] := fld[player, xf, yf]
      else if playerSize[player] = _PLAYER_SIZE_DOUBLE then begin
        playerPos[player, playerIndex[player] + xf + d, yf] := fld[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 1, yf] := fld[player, xf, yf];
        Inc(d);
      end
      else if playerSize[player] = _PLAYER_SIZE_QUADRUPLE then begin
        playerPos[player, playerIndex[player] + xf + d, yf] := fld[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 1, yf] := fld[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 2, yf] := fld[player, xf, yf];
        playerPos[player, playerIndex[player] + xf + d + 3, yf] := fld[player, xf, yf];
        Inc(d, 3);
      end
    end;
  end;

  ci := index;
  for yf := 0 to _PM_MAX_LINES - 1 do begin
    for xf := 0 to _PM_WIDTH + _MAX_PLAYER_POS do begin
      if isShowPmEditorGrid then begin
        editorMulti.Canvas.Pen.Color := clWhite;
        c := xf*factX shl 1;
        d := yf*factY shl 1;
        if c = 0 then c := factX;
        if d = 0 then d := factY;
        editorMulti.Canvas.Rectangle(xf*factX, yf*factY, c, d);
      end;

      col := playerPos[0, xf, yf];      // Player 0 value
      col01 := playerPos[1, xf, yf];    // Player 1 value
      col02 := playerPos[2, xf, yf];    // Player 2 value
      col03 := playerPos[3, xf, yf];    // Player 3 value

      editorMulti.Canvas.Brush.Color := coltab[ci + 4];

      // Color register priority
      if col = 1 then begin
        if (col01 = 1) and isPmMixedColor then
          editorMulti.Canvas.Brush.Color := coltab[4] or coltab[5]
        else
          editorMulti.Canvas.Brush.Color := coltab[4]
      end
      else if col01 = 1 then
        editorMulti.Canvas.Brush.Color := coltab[5]
      else if col02 = 1 then begin
        if col = 1 then begin
          if (col01 = 1) and isPmMixedColor then
            editorMulti.Canvas.Brush.Color := coltab[4] or coltab[5]
          else
            editorMulti.Canvas.Brush.Color := coltab[4]
        end
        else if col01 = 1 then
          editorMulti.Canvas.Brush.Color := coltab[5]
        else if (col03 = 1) and isPmMixedColor then
          editorMulti.Canvas.Brush.Color := coltab[6] or coltab[7]
        else
          editorMulti.Canvas.Brush.Color := coltab[6];
      end
      else begin
        editorMulti.Canvas.Brush.Color := clBlack;
        if col01 = 1 then
          editorMulti.Canvas.Brush.Color := coltab[5];

        if col03 = 1 then
          editorMulti.Canvas.Brush.Color := coltab[7];
      end;

      editorMulti.Canvas.FillRect(bounds(xf*factX + 1, yf*factY + 1, factX03, factY - 1));
    end;
  end;
  editorMulti.Refresh;
end;

procedure TfrmPmg.DrawMissile(pm : TImage; factX, factY : byte);
var
  col : byte;
  xf, yf : integer;
begin
  pm.Canvas.Brush.Color := clBlack;
  pm.Canvas.Brush.Style := bsSolid;
  pm.Canvas.FillRect(bounds(0, 0, pm.Width, pm.Height));
//  xf := 0;
  for yf := 0 to missileMaxY - 1 do begin
    for xf := 0 to 1 do begin
      col := missiles[player, xf, yf];
      if col = 1 then begin
        col := player;
        pm.Canvas.Brush.Color := coltab[col + 4]
      end
      else
        pm.Canvas.Brush.Color := clBlack;

      pm.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
//      pm.Canvas.Pixels[xf*factX, yf*factY] := coltab[col + 4];
    end;
//    Inc(xf);
//    if xf > 1 then xf := 0;
  end;
  pm.Refresh;
end;

procedure TfrmPmg.RefreshPM(isMultiColor : boolean);
var
  i, j : byte;
  bin : string[8];
begin
  if tabs.PageIndex = 0 then begin
    RefreshPlayer(editor, factX, factY);

    RefreshPlayer(imgP0_normal, 6, 6);
    RefreshPlayer(imgP0_double, 12, 6);
    RefreshPlayer(imgP0_quadrable, 24, 6);

    RefreshPlayer(imgP0_normalSr, 6, 3);
    RefreshPlayer(imgP0_doubleSr, 12, 3);
    RefreshPlayer(imgP0_quadrableSr, 24, 3);
  end
  else if (tabs.PageIndex = 1) and isMultiColor then
    RefresPM_MultiAll(editorMulti, factX02, factY02);

  case player of
    0: begin
         RefreshPlayer(imgP0_normal02, 4, 4);
         RefreshPlayer(imgP0_single, 4, 2);
       end;
    1: begin
         RefreshPlayer(imgP1_normal02, 4, 4);
         RefreshPlayer(imgP1_single, 4, 2);
       end;
    2: begin
         RefreshPlayer(imgP2_normal02, 4, 4);
         RefreshPlayer(imgP2_single, 4, 2);
       end;
    3: begin
         RefreshPlayer(imgP3_normal02, 4, 4);
         RefreshPlayer(imgP3_single, 4, 2);
       end;
  end;
  for j := 0 to _PM_MAX_LINES - 1 do begin
    if j <= _PM_MAX_LINES - 1 then begin
      bin := '';
      for i := 0 to 7 do
        bin += IntToStr(fld[player, i, j]);

      (FindComponent('lblNum' + IntToStr(j)) as TLabel).
        Caption := IntToStr(Bin2Dec(bin)) + ' ($' + Dec2Hex(Bin2Dec(bin)) + ')'
    end
    else
      (FindComponent('lblNum' + IntToStr(j)) as TLabel).Caption := '';

    //if (tabPlayers.Controls[i].Name = 'lblPlayerLine' + inttostr(col)) then begin
    //  tabPlayers.Controls[i].Caption := IntToStr(col);
    //  if pmResolution = [doubleResolution] then begin
    //    tabPlayers.Controls[i].Top := 32 + col*16;
    //    tabPlayers.Controls[i].Font.Size := 8;
    //  end
    //  else begin
    //    tabPlayers.Controls[i].Top := 32 + col*8;
    //    tabPlayers.Controls[i].Font.Size := 6;
    //  end;
    //end;
  end;

  for i := 0 to tabPlayers.ControlCount - 1 do
    for j := 0 to 39 do begin
      if (tabPlayers.Controls[i].Name = 'lblPlayerLine' + inttostr(j)) then begin
        if j <= _PM_MAX_LINES - 1 then
          tabPlayers.Controls[i].Caption := IntToStr(j)
        else
          tabPlayers.Controls[i].Caption := '';

        if pmResolution = doubleResolution then begin
          tabPlayers.Controls[i].Top := 20 + j shl 4;
          tabPlayers.Controls[i].Font.Size := 8;
        end
        else begin
          tabPlayers.Controls[i].Top := 18 + j shl 3;
          tabPlayers.Controls[i].Font.Size := 6;
        end;
        break;
      end;
    end;
end;

procedure TfrmPmg.RefreshAll;
begin
  if tabs.PageIndex = 0 then
    RefreshPlayer(editor, factX, factY);

  // Normal mode
  RefreshPlayer(imgP0_normal, 6, 6);
  RefreshPlayer(imgP0_double, 12, 6);
  RefreshPlayer(imgP0_quadrable, 24, 6);

  RefreshPlayer(imgP0_normalSr, 6, 3);
  RefreshPlayer(imgP0_doubleSr, 12, 3);
  RefreshPlayer(imgP0_quadrableSr, 24, 3);

//  if tabs.PageIndex = 1 then flag := true;

  player := 0;
  RefreshPlayer(imgP0_normal02, 4, 4);
  RefreshPlayer(imgP0_single, 4, 2);
  player := 1;
  RefreshPlayer(imgP1_normal02, 4, 4);
  RefreshPlayer(imgP1_single, 4, 2);
  player := 2;
  RefreshPlayer(imgP2_normal02, 4, 4);
  RefreshPlayer(imgP2_single, 4, 2);
  player := 3;
  RefreshPlayer(imgP3_normal02, 4, 4);
  RefreshPlayer(imgP3_single, 4, 2);

  player := 0;
  rbPlayer0.Checked := true;
  InitPmValues;
//  RefreshValues;
end;

procedure TfrmPmg.RefreshMissile;
begin
  case player of
    0: begin
         if pmResolution = doubleResolution then
           DrawMissile(imgMissile00, 16, 16)
         else
           DrawMissile(imgMissile00, 16, 8);

         DrawMissile(imgMissile0, 4, 4);
         DrawMissile(imgMissileSingle0, 4, 2);
       end;
    1: begin
         if pmResolution = doubleResolution then
           DrawMissile(imgMissile01, 16, 16)
         else
           DrawMissile(imgMissile01, 16, 8);

         DrawMissile(imgMissile1, 4, 4);
         DrawMissile(imgMissileSingle1, 4, 2);
       end;
    2: begin
         if pmResolution = doubleResolution then
           DrawMissile(imgMissile02, 16, 16)
         else
           DrawMissile(imgMissile02, 16, 8);

         DrawMissile(imgMissile2, 4, 4);
         DrawMissile(imgMissileSingle2, 4, 2);
       end;
    3: begin
         if pmResolution = doubleResolution then
           DrawMissile(imgMissile03, 16, 16)
         else
           DrawMissile(imgMissile03, 16, 8);

         DrawMissile(imgMissile3, 4, 4);
         DrawMissile(imgMissileSingle3, 4, 2);
       end;
  end;
  DrawMissile(imgM0_normal_dr, 6, 6);
  DrawMissile(imgM0_double_dr, 12, 6);
  DrawMissile(imgM0_quadrable_dr, 24, 6);
  DrawMissile(imgM0_normal_sr, 6, 3);
  DrawMissile(imgM0_double_sr, 12, 3);
  DrawMissile(imgM0_quadrable_sr, 24, 3);

//  // Update missile bit information
//  for i := 0 to tabMissiles.ControlCount - 1 do begin
//    for j := 0 to missileMaxY - 1 do begin
////      if j = col then begin
//        if (tabMissiles.Controls[i].Name = 'lblMissile' + inttostr(j + player*10)) then begin
//          tabMissiles.Controls[i].Caption := IntToStr(bin2dec(bin));
//          break;
//        end;
////      end;
//    end;
//  end;
end;

procedure TfrmPmg.imgMissileMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : integer;
begin
  player := TImage(Sender).Tag;
  (FindComponent('rbPlayer' + IntToStr(player)) as TRadioButton).Checked := true;
  //case player of
  //  0: rbPlayer0.Checked := true;
  //  1: rbPlayer1.Checked := true;
  //  2: rbPlayer2.Checked := true;
  //  3: rbPlayer3.Checked := true;
  //end;

  SelectMissile;

  btn := Button;
  xf := X div factX;
  yf := Y div factY;

  // Check for mouse button clicked
  if btn = mbRight then
    varBtn := btn
  else
    varBtn := mbLeft;

  PlotMissile(xf, yf);
end;

procedure TfrmPmg.imgMissileMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
begin
  xf := X div factX;
  yf := Y div factY;
  PlotMissile(xf, yf);
  sbPmg.Panels[0].Text := 'Cursor coordinates: ' + 'x: ' + inttostr(xf) + ', y: ' + inttostr(yf);
end;

procedure TfrmPmg.imgMissileMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
  RefreshMissile;
end;

procedure TfrmPmg.lblNumClick(Sender: TObject);
var
  dataValue, i, j : byte;
  bin : string;
begin
  if InputQuery('Enter player data value manually',
                'Enter decimal value for player data line',
                bin) then
  begin
    dataValue := StrToInt(bin);
    bin := IntToBin(dataValue, 8);
    j := (Sender as TLabel).Tag;
    for i := 0 to 7 do
      fld[player, i, j] := StrToInt(bin[i + 1]);

    RefreshPM(true);
  end
end;

procedure TfrmPmg.editorMultiMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : integer;
begin
//  if (Sender as TImage).Tag = 555 then
//    player := 4;

  btn := Button;
  xf := X div factX02;
  yf := Y div factY02;

  // Check if mouse button was clicked
  if btn = mbRight then
    varBtn := btn
  else
    varBtn := mbLeft;

  if (xf >= playerIndex[player] - 1) then begin  //or (xf >= playerIndex[player]) then begin
//    if not CalcPlayerWidth(player, xf, yf) then Exit;
    PlotMulti(xf, yf);
  end;

  case player of
    0: tbPlayer0.Position := playerIndex[0];
    1: tbPlayer1.Position := playerIndex[1];
    2: tbPlayer2.Position := playerIndex[2];
    3: tbPlayer3.Position := playerIndex[3];
  end;

//  sbPmg.Panels[1].Text := 'index, xf ' + inttostr(playerIndex[player]) + ' ' + inttostr(xf);
end;

procedure TfrmPmg.editorMultiMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
begin
  xf := X div factX02;
  yf := Y div factY02;
  if (xf >= playerIndex[player] - 1) then begin  //or (xf >= playerIndex[player]) then begin
//    if not CalcPlayerWidth(player, xf, yf) then Exit;
    PlotMulti(xf, yf);
  end;
  //sbPmg.Panels[0].Text := '';
end;

procedure TfrmPmg.editorMultiMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
  refreshPM(true);
end;

procedure TfrmPmg.chkMultiColor01Change(Sender: TObject);
begin
  refreshPM(true);
end;

//function TfrmPmg.FindFldData(pl : byte) : boolean;
//var
//  x : byte;
//  y : integer;
//begin
//  result := false;
//  for y := 0 to _PM_MAX_LINES - 1 do begin
//    for x := 0 to 7 do begin
//      if fld[pl, x, y] = 1 then begin
//        result := true;
//        break;
//      end;
//    end;
//  end;
//end;

procedure TfrmPmg.SavePlayerDataAll(Sender: TObject);
var
  i : byte;
begin
  if isDataChanged[0] or isDataChanged[1] or isDataChanged[2] or isDataChanged[3] then
  begin
    for i := 0 to 3 do begin
      if isDataChanged[i] and (playerFiles[i] = '') then begin
        player := i;
        (FindComponent('rbPlayer' + IntToStr(player)) as TRadioButton).Checked := true;
        SaveDataAs(Sender);
      end
      else begin
        if isDataChanged[i] then
          SavePlayer(playerFiles[i])
//        else
//          ShowMessage('There is no data to be saved for player ' + IntToStr(i) + '!');
      end;
    end;
  end
  else
    ShowMessage('There is no player data to be saved!');
end;

procedure TfrmPmg.editPosXChange(Sender: TObject);
begin
  tbHorizontal.Position := editPosX.Value;
  PlayerLocation;
end;

procedure TfrmPmg.editPosYChange(Sender: TObject);
begin
  tbVertical.Position := editPosY.Value;
  PlayerLocation;
end;

procedure TfrmPmg.RefreshValues;
//var
//  i, j, k : byte;
//  line : string;
begin
//  for i := 0 to tabMultiPM.ControlCount - 1 do begin
////    for pl := 0 to 3 do begin
//      for j := 0 to _PM_MAX_LINES - 1 do begin
//        //if tabMultiPM.Controls[i].Name = 'lblMultiPM' + inttostr(j) then begin
//        //  tabMultiPM.Controls[i].Visible := true;
//        //end;
//        if tabMultiPM.Controls[i].Name = 'lblMultiPmValue0' + inttostr(player) + inttostr(j) then begin
//          tabMultiPM.Controls[i].Visible := true;
//          line := '';
//          for k := 0 to 7 do
//            line += IntToStr(playerPos[player, playerIndex[player] + k, j]);
//
//          if isValueDec then
//            line := IntToStr(bin2dec(line))
//          else begin
//            line := '$' + dec2hex(bin2dec(line));
//            if Trim(line) = '$' then line := '$0';
//          end;
//          tabMultiPM.Controls[i].Caption := 'P' + IntToStr(player) + ': ' + line;
//
//          if pmResolution = [doubleResolution] then begin
//            tabMultiPM.Controls[i].Top := 2 + j*12;
//            tabMultiPM.Controls[i].Font.Size := 8;
//          end
//          else begin
//            tabMultiPM.Controls[i].Top := 2 + j*10;
//            tabMultiPM.Controls[i].Font.Size := 6;
//          end;
//        end;
//      end;
//    //end;
//  end;
end;

procedure TfrmPmg.ExitForm(Sender: TObject);
begin
  Close;
end;

procedure TfrmPmg.LoadAplData(Sender: TObject);
begin
  frmPmApl := TfrmPmApl.Create(Self);
  with frmPmApl do
    try
      ShowModal;
    finally
      Free;
    end;

  RefreshAll;
end;

procedure TfrmPmg.SaveAplData(Sender: TObject);
//var
//  filenamexy : string;
//  i : integer;
//  fs : TFileStream;

//procedure FillPlayer(pl : byte);
//var
//  bin : string;
//  x, i, j : byte;
//begin
//  for i := 1 to 17 do begin
//    // Player data, frame i (48 bytes)
//    // 17 - Player data, copy buffer (48 bytes)
//    for j := 0 to 47 do begin
//      if i = 1 then begin
//        if j <= _PM_MAX_LINES - 1 then begin
//          bin := '';
//          for x := 0 to 7 do begin
//            bin += IntToStr(fld[pl, x, j]);
//          end;
//          fs.WriteByte(Bin2Dec(bin));
//        end
//        else begin
//          fs.WriteByte(0);
//        end;
//      end
//      else if i = 2 then begin
//        if j <= _PM_MAX_LINES - 1 then begin
//          bin := '';
//          for x := 0 to 7 do begin
//            bin += IntToStr(fld[pl + 2, x, j]);
//          end;
//          fs.WriteByte(Bin2Dec(bin));
//        end
//        else begin
//          fs.WriteByte(0);
//        end;
//      end
//      else begin
//        fs.WriteByte(0);
//      end;
//    //  if i <= 2 then begin
//    //    if j <= _PM_MAX_LINES - 1 then begin
//    //      bin := '';
//    //      for x := 0 to 7 do begin
//    //        bin += IntToStr(fld[pl, x, j]);
//    //      end;
//    //      fs.WriteByte(Bin2Dec(bin));
//    //    end
//    //    else begin
//    //      fs.WriteByte(0);
//    //    end;
//    //  end
//    //  else begin
//    //    fs.WriteByte(0);
//    //  end;
//    end;
//  end;
//end;

begin
  //frmMain.dlgSave.Filter := 'Atari Player data file (*.apl)|*.apl;|All files (*.*)|*.*';
  //frmMain.dlgSave.Title := 'Save Atari Player data file as';
  //frmMain.dlgSave.Filename := filename;
  //
  //if frmMain.dlgSave.Execute then begin
  //  filenamexy := frmMain.dlgSave.Filename;
  //
  //  fs := TFileStream.Create(filenamexy, fmCreate);
  //  try
  //    // file version identifier
  //    fs.WriteByte($9a);
  //    fs.WriteByte($f8);
  //    fs.WriteByte($39);
  //    fs.WriteByte($21);
  //
  //    // number of frames
  //    fs.WriteByte(1);
  //
  //    // height
  //    fs.WriteByte(48);
  //
  //    // gap
  //    fs.WriteByte(0);
  //
  //    // P0 colour, frames 1..16
  //    for i := 0 to 15 do begin
  //      fs.WriteByte(coltab[4]);
  //    end;
  //
  //    // P0 colour, copy buffer
  //    fs.WriteByte(coltab[4]);
  //
  //    // P1 colour, frames 1..16
  //    for i := 0 to 15 do begin
  //      fs.WriteByte(coltab[5]);
  //    end;
  //
  //    // P1 colour, copy buffer
  //    fs.WriteByte(coltab[5]);
  //
  //    // background colour
  //    fs.WriteByte(0);
  //
  //    FillPlayer(0);
  //    FillPlayer(1);
  //
  //    // selected frame
  //    fs.WriteByte(1);
  //
  //    // pen colour
  //    fs.WriteByte(coltab[4]);
  //
  //    //animation rate
  //    fs.WriteByte(2);
  //
  //    //isDataChanged[player] := false;
  //  finally
  //    fs.Free;
  //  end;
  //end;
end;

procedure TfrmPmg.ShowGrid(Sender: TObject);
begin
  isShowPmEditorGrid := true;
  RefreshPlayer(editor, factX, factY);
  RefresPM_MultiAll(editorMulti, factX02, factY02);
  isChange := true;
end;

procedure TfrmPmg.HideGrid(Sender: TObject);
begin
  isShowPmEditorGrid := false;
  RefreshPlayer(editor, factX, factY);
  RefresPM_MultiAll(editorMulti, factX02, factY02);
  isChange := true;
end;

procedure TfrmPmg.MultiColorEnable(Sender: TObject);
begin
  isPmMixedColor := true;
  sbPmg.Panels[2].Text := 'Player mixed (3rd) color enabled';
  isChange := true;
  refreshPM(true);
end;

procedure TfrmPmg.MultiColorDisable(Sender: TObject);
begin
  isPmMixedColor := false;
  sbPmg.Panels[2].Text := 'Player mixed (3rd) color disabled';
  isChange := true;
  refreshPM(true);
end;

procedure TfrmPmg.ShowDec(Sender: TObject);
begin
  isValueDec := true;
//  RefreshValues;
end;

procedure TfrmPmg.ShowHex(Sender: TObject);
begin
  isValueDec := false;
//  RefreshValues;
end;

procedure TfrmPmg.HideValues(Sender: TObject);
var
  i, j, pl : byte;
begin
  for i := 0 to tabMultiPM.ControlCount - 1 do
    for pl := 0 to 3 do
      for j := 0 to _PM_MAX_LINES - 1 do begin
        //if tabMultiPM.Controls[i].Name = 'lblMultiPM' + inttostr(j) then begin
        //  tabMultiPM.Controls[i].Visible := false;
        //end;
        if tabMultiPM.Controls[i].Name = 'lblMultiPmValue0' + inttostr(pl) + inttostr(j) then
          tabMultiPM.Controls[i].Visible := false;
      end;
end;

procedure TfrmPmg.SelectMissile;
begin
  with panelM0 do begin
    BorderStyle := bsNone;
    BevelOuter := bvNone;
    Font.Style := [];
  end;
  with panelM1 do begin
    BorderStyle := bsNone;
    BevelOuter := bvNone;
    Font.Style := [];
  end;
  with panelM2 do begin
    BorderStyle := bsNone;
    BevelOuter := bvNone;
    Font.Style := [];
  end;
  with panelM3 do begin
    BorderStyle := bsNone;
    BevelOuter := bvNone;
    Font.Style := [];
  end;
  with (FindComponent('panelM' + IntToStr(player)) as TPanel) do begin
    BorderStyle := bsSingle;
    BevelOuter := bvRaised;
    BevelWidth := 1;
    Font.Style := [fsBold];
  end;
end;

procedure TfrmPmg.rbPlayerChange(Sender: TObject);
begin
  if rbPlayer0.Checked then
    player := 0
  else if rbPlayer1.Checked then
    player := 1
  else if rbPlayer2.Checked then
    player := 2
  else if rbPlayer3.Checked then
    player := 3;

  lblPlayer.Caption := ' player ' + IntToStr(player);

  if tabs.ActivePage = tabPlayers then begin
    SelectMissile;
    if playerFiles[player] <> '' then begin
      caption := programName + ' ' + programVersion + ' - Player/missile graphics editor';
      caption := caption + ' (' + playerFiles[player] + ')';
    end;
    RefreshPM(true);
    RefreshMissile;
    tbHorizontal.Position := arrPosX[player];
    tbVertical.Position := arrPosY[player];
    tbMissileHorizontal.Position := arrMissilePosX[player];
    tbMissileVertical.Position := arrMissilePosY[player];
  end
  else if tabs.ActivePage = tabMissiles then begin
    SelectMissile;
    if playerFiles[player] <> '' then begin
      caption := programName + ' ' + programVersion + ' - Player/missile graphics editor';
      caption := caption + ' (' + playerFiles[player] + ')';
    end;
    RefreshMissile;
    tbHorizontal.Position := arrPosX[player];
    tbVertical.Position := arrPosY[player];
    tbMissileHorizontal.Position := arrMissilePosX[player];
    tbMissileVertical.Position := arrMissilePosY[player];
  end;
end;

procedure TfrmPmg.SaveDataAs(Sender: TObject);
begin
  frmMain.dlgSave.InitialDir := getDir + 'examples\';
  frmMain.dlgSave.Filename := '';

  // Player data
  if (tabs.ActivePage = tabPlayers) or (Sender = mnuPlayerSave) then begin
    frmMain.dlgSave.Filter := 'Player files (*.spr)|*.spr|All files (*.*)|*.*';
    frmMain.dlgSave.Title := 'Save player ' + IntToStr(player) + ' data as';
    if frmMain.dlgSave.Execute then begin
      playerFiles[player] := frmMain.dlgSave.Filename;
      SavePlayer(playerFiles[player]);
      caption := programName + ' ' + programVersion +
                 ' - Player/missile graphics editor (' + playerFiles[player] + ')';
    end;
  end
  // Missile data
  else if (tabs.ActivePage = tabMissiles) or (Sender = mnuMissileSave) then begin
    frmMain.dlgSave.Filter := 'Missile files (*.msl)|*.msl|All files (*.*)|*.*';
    frmMain.dlgSave.Title := 'Save missile ' + IntToStr(player) + ' data as';
    if frmMain.dlgSave.Execute then begin
      missileFiles[player] := frmMain.dlgSave.Filename;
      SaveMissile(missileFiles[player]);
      caption := programName + ' ' + programVersion +
                 ' - Player/missile graphics editor (' + missileFiles[player] + ')';
    end;
  end
  // Multi-color players data
  else if (tabs.ActivePage = tabMultiPM) or (Sender = mnuMplSave) then begin
    frmMain.dlgSave.Filter := 'Multi-color player files (*.mpl)|*.mpl|All files (*.*)|*.*';
    frmMain.dlgSave.Title := 'Save multi-color player data as';
    if frmMain.dlgSave.Execute then begin
      mplFile := frmMain.dlgSave.Filename;
      SaveMplData(mplFile);
      caption := programName + ' ' + programVersion +
                 ' - Player/missile graphics editor (' + mplFile + ')';
    end;
  end;
end;

procedure TfrmPmg.GenCode(Sender: TObject);
var
  i, j, pl : byte;
  bin : string[9];
  bit01, bit02 : char;
begin
  for pl := 0 to 3 do
    for j := 0 to missileMaxY - 1 do begin
      bin := '';
      for i := 0 to 1 do
        bin += IntToStr(missiles[pl, i, j]);

      bit01 := bin[2];
      bit02 := bin[1];
      case pl of
        1: bin := '0000' + bit02 + bit01 + '00';
        2: bin := '00' + bit02 + bit01 + '0000';
        3: bin := bit02 + bit01 + '000000';
      end;
      missileData[pl, j] := bin2dec(bin);
      //missileStr := missileStr + IntToStr(bin2dec(bin));
    end;

  frmPmgGen := TfrmPmgGen.Create(Self);
  with frmPmgGen do
    try
      ShowModal;
    finally
      Free;
    end;

  rbPlayerChange(Sender);
end;

procedure TfrmPmg.Palette(Sender: TObject);
begin
  frmColors.Show;
end;

procedure TfrmPmg.ToggleBox1Change(Sender : TObject);
begin
  Togglebox1.State := cbGrayed;
  Togglebox2.State := cbUnchecked;
  SetDoubleResolution(Sender);
end;

procedure TfrmPmg.ToggleBox2Change(Sender : TObject);
begin
  Togglebox1.State := cbUnchecked;
  Togglebox2.State := cbGrayed;
  SetSingleResolution(Sender);
end;

procedure TfrmPmg.ToggleBox3Change(Sender : TObject);
begin
  Togglebox3.State := cbGrayed;
  Togglebox4.State := cbUnchecked;
  FlipXProc(0);
end;

procedure TfrmPmg.ToggleBox4Change(Sender : TObject);
begin
  Togglebox3.State := cbUnchecked;
  Togglebox4.State := cbGrayed;
  FlipXProc(3);
end;

procedure TfrmPmg.ToggleBox5Change(Sender : TObject);
begin
  Togglebox5.State := cbGrayed;
  Togglebox6.State := cbUnchecked;
  FlipYProc(0);
end;

procedure TfrmPmg.ToggleBox6Change(Sender : TObject);
begin
  Togglebox5.State := cbUnchecked;
  Togglebox6.State := cbGrayed;
  FlipYProc(3);
end;

procedure TfrmPmg.ImportData(Sender : TObject);
var
  x, y, i, j : byte;
  bin : string;
begin
  beType := formPmg;
  maxLines := 16;
  frmByteEditor.ShowModal;

  if isDataExport then begin
    for i := 0 to 7 do
      for j := 0 to 220 do
        fld[player, i, j] := 0;

    for i := 0 to _MAX_PLAYER_POS - 1 do
      for j := 0 to _PM_MAX_LINES - 1 do
        playerPos[player, i, j] := 0;

    // Import as player data
//    _PM_MAX_LINES
    for y := 0 to maxLines - 1 do begin
      x := exportData[y];
      bin := Dec2Bin(x);
      for x := 0 to 7 do begin
        fld[player, x, y] := StrToInt(bin[x + 1]);
        playerPos[player, playerIndex[player] + x, y] := fld[player, x, y];
      end;
    end;
    rbPlayerChange(Sender);
  end;
end;

procedure TfrmPmg.ToggleChange(Sender : TObject; tgl01, tgl02, tgl03 : TToggleBox; plSize : byte);
begin
  if Sender is TToggleBox then
    player := (Sender as TToggleBox).Tag;

  case player of
    0: rbPlayer0.Checked := true;
    1: rbPlayer1.Checked := true;
    2: rbPlayer2.Checked := true;
    3: rbPlayer3.Checked := true;
  end;

  case plSize of
    _PLAYER_SIZE_NORMAL : begin
      tgl01.State := cbGrayed;
      tgl02.State := cbUnchecked;
      tgl03.State := cbUnchecked;
      factX03 := 12;
    end;
    _PLAYER_SIZE_DOUBLE : begin
      tgl01.State := cbUnchecked;
      tgl02.State := cbGrayed;
      tgl03.State := cbUnchecked;
      factX03 := 24;
    end;
    _PLAYER_SIZE_QUADRUPLE : begin
      tgl01.State := cbUnchecked;
      tgl02.State := cbUnchecked;
      tgl03.State := cbGrayed;
      factX03 := 48;
    end;
  end;
  playerSize[player] := plSize;
  ReadFld(factX02, factY02);
end;

procedure TfrmPmg.tglP0x1Change(Sender : TObject);
begin
  //player := (Sender as TToggleBox).Tag;
  //tglP0x1.State := cbGrayed;
  //tglP0x2.State := cbUnchecked;
  //tglP0x4.State := cbUnchecked;
  //factX03 := 12;
  //playerSize[player] := _PLAYER_SIZE_NORMAL;
  //ReadFld(editorMulti, factX02, factY02);
  ToggleChange(Sender, tglP0x1, tglP0x2, tglP0x4, _PLAYER_SIZE_NORMAL);
  tbPlayer0.Enabled := true;
end;

procedure TfrmPmg.tglP0x2Change(Sender : TObject);
begin
  //player := (Sender as TToggleBox).Tag;
  //tglP0x1.State := cbUnchecked;
  //tglP0x2.State := cbGrayed;
  //tglP0x4.State := cbUnchecked;
  //factX03 := 24;
  //playerSize[player] := _PLAYER_SIZE_DOUBLE;
  //ReadFld(editorMulti, factX02, factY02);
  ToggleChange(Sender, tglP0x1, tglP0x2, tglP0x4, _PLAYER_SIZE_DOUBLE);
  tbPlayer0.Enabled := false;
end;

procedure TfrmPmg.tglP0x4Change(Sender : TObject);
begin
  //player := (Sender as TToggleBox).Tag;
  //tglP0x1.State := cbUnchecked;
  //tglP0x2.State := cbUnchecked;
  //tglP0x4.State := cbGrayed;
  //factX03 := 48;
  //playerSize[player] := _PLAYER_SIZE_QUADRUPLE;
  //ReadFld(editorMulti, factX02, factY02);
//  RefresPM_MultiAll(editorMulti, factX02, factY02);
  ToggleChange(Sender, tglP0x1, tglP0x2, tglP0x4, _PLAYER_SIZE_QUADRUPLE);
  tbPlayer0.Enabled := false;
end;

procedure TfrmPmg.tglP1x1Change(Sender : TObject);
begin
  ToggleChange(Sender, tglP1x1, tglP1x2, tglP1x4, _PLAYER_SIZE_NORMAL);
  tbPlayer1.Enabled := true;
end;

procedure TfrmPmg.tglP1x2Change(Sender : TObject);
begin
  ToggleChange(Sender, tglP1x1, tglP1x2, tglP1x4, _PLAYER_SIZE_DOUBLE);
  tbPlayer1.Enabled := false;
end;

procedure TfrmPmg.tglP1x4Change(Sender : TObject);
begin
  ToggleChange(Sender, tglP1x1, tglP1x2, tglP1x4, _PLAYER_SIZE_QUADRUPLE);
  tbPlayer1.Enabled := false;
end;

procedure TfrmPmg.tglP2x1Change(Sender : TObject);
begin
  ToggleChange(Sender, tglP2x1, tglP2x2, tglP2x4, _PLAYER_SIZE_NORMAL);
  tbPlayer2.Enabled := true;
end;

procedure TfrmPmg.tglP2x2Change(Sender : TObject);
begin
  ToggleChange(Sender, tglP2x1, tglP2x2, tglP2x4, _PLAYER_SIZE_DOUBLE);
  tbPlayer2.Enabled := false;
end;

procedure TfrmPmg.tglP2x4Change(Sender : TObject);
begin
  ToggleChange(Sender, tglP2x1, tglP2x2, tglP2x4, _PLAYER_SIZE_QUADRUPLE);
  tbPlayer2.Enabled := false;
end;

procedure TfrmPmg.tglP3x1Change(Sender : TObject);
begin
  ToggleChange(Sender, tglP3x1, tglP3x2, tglP3x4, _PLAYER_SIZE_NORMAL);
  tbPlayer3.Enabled := true;
end;

procedure TfrmPmg.tglP3x2Change(Sender : TObject);
begin
  ToggleChange(Sender, tglP3x1, tglP3x2, tglP3x4, _PLAYER_SIZE_DOUBLE);
  tbPlayer3.Enabled := false;
end;

procedure TfrmPmg.tglP3x4Change(Sender : TObject);
begin
  ToggleChange(Sender, tglP3x1, tglP3x2, tglP3x4, _PLAYER_SIZE_QUADRUPLE);
  tbPlayer3.Enabled := false;
end;

end.

