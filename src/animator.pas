{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2023
  Unit: Player Animator editor and player
}
unit animator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, StdCtrls, lcltype, Buttons, Types, LCLIntf, BCMDButton,
  common;

type
  { TfrmAnimator }
  TfrmAnimator = class(TForm)
    boxFlip1 : TGroupBox;
    btnAnimate : TSpeedButton;
    btnByteEditor : TToolButton;
    btnCharDown : TSpeedButton;
    btnCharLeft : TSpeedButton;
    btnCharRight : TSpeedButton;
    btnCharUp : TSpeedButton;
    btnClearPlayer: TToolButton;
    btnCopyToBuffer: TToolButton;
    btnFlipX : TToolButton;
    btnFlipXAll2 : TBCMDButton;
    btnFlipXAll3 : TBCMDButton;
    btnSingleRes : TBCMDButton;
    btnFlipXSelected2 : TBCMDButton;
    btnFlipXSelected3 : TBCMDButton;
    btnDoubleRes : TBCMDButton;
    btnFlipY : TToolButton;
    btnInvert : TToolButton;
    boxFlip : TGroupBox;
    editPlayerHeight : TSpinEditEx;
    numIterations : TSpinEditEx;
    numFrames : TSpinEditEx;
    numAnimRate : TSpinEditEx;
    pcolr1 : TImage;
    pcolr0 : TImage;
    lblFrameMaxHeight : TLabel;
    MenuItem10 : TMenuItem;
    itemByteEditor : TMenuItem;
    itemColorPalette : TMenuItem;
    MenuItem11 : TMenuItem;
    MenuItem12 : TMenuItem;
    MenuItem6 : TMenuItem;
    miLoadAPLEx : TMenuItem;
    MenuItem9 : TMenuItem;
    boxFrameLayer : TGroupBox;
    boxAnimation : TGroupBox;
    boxShiftMove : TGroupBox;
    imgBuffer: TImage;
    imgTemp: TImage;
    imgFrame01: TImage;
    imgFrame02: TImage;
    imgFrame03: TImage;
    imgFrame04: TImage;
    imgFrame10: TImage;
    imgFrame11: TImage;
    imgFrame12: TImage;
    imgFrame13: TImage;
    imgFrame05: TImage;
    imgFrame06: TImage;
    imgFrame07: TImage;
    imgFrame08: TImage;
    imgFrame09: TImage;
    imgFrame14: TImage;
    imgFrame15: TImage;
    imgFrame16: TImage;
    imgP0P1_doubleMultiAll: TImage;
    imgP0P1_doubleSrMultiAll: TImage;
    imgP0P1_normalMultiAll: TImage;
    imgP0P1_normalSrMultiAll: TImage;
    imgP0P1_quadrableMultiAll: TImage;
    imgP0P1_quadrableSrMultiAll: TImage;
    Label18 : TLabel;
    Label19 : TLabel;
    Label20 : TLabel;
    Label45: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    MenuItem13: TMenuItem;
    MenuItem5 : TMenuItem;
    MenuItem8 : TMenuItem;
    popCopyFrameToAll : TMenuItem;
    MenuItem7: TMenuItem;
    popCopyPlayers : TMenuItem;
    miNewAnimation: TMenuItem;
    miCopyFromBuffer: TMenuItem;
    miCopyToBuffer: TMenuItem;
    popMenuClearFrame: TMenuItem;
    popMenuCopyFromBuffer: TMenuItem;
    popMenuCopyToBuffer: TMenuItem;
    miSaveAPL: TMenuItem;
    miLoadAPL: TMenuItem;
    miFlipX: TMenuItem;
    miFlipY: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    miClearPlayer: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem25: TMenuItem;
    miShowGrid: TMenuItem;
    miHideGrid: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    miMultiValues: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    menuAnim: TMainMenu;
    MenuItem1: TMenuItem;
    miExit: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miLoad: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    popMenu: TPopupMenu;
    radMoveChar : TRadioButton;
    radMoveAllChars : TRadioButton;
    radShiftChar : TRadioButton;
    radShiftAllChars : TRadioButton;
    rbPlayer0 : TRadioButton;
    rbPlayer1 : TRadioButton;
    shape: TShape;
    lblFrameBuffer : TStaticText;
    lblFrame09 : TStaticText;
    lblFrame10 : TStaticText;
    lblFrame11 : TStaticText;
    lblFrame12 : TStaticText;
    lblFrame13 : TStaticText;
    lblFrame14 : TStaticText;
    lblFrame15 : TStaticText;
    lblFrame16 : TStaticText;
    lblFrame01 : TStaticText;
    lblFrame02 : TStaticText;
    lblFrame03 : TStaticText;
    lblFrame04 : TStaticText;
    lblFrame05 : TStaticText;
    lblFrame06 : TStaticText;
    lblFrame07 : TStaticText;
    lblFrame08 : TStaticText;
    statusBar: TStatusBar;
    toolbar: TToolBar;
    btnOpen: TToolButton;
    btnSave: TToolButton;
    btnCopyFromBuffer: TToolButton;
    btnCode: TToolButton;
    btnDivider: TToolButton;
    btnNewAnimation: TToolButton;
    ToolButton1 : TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure pcolr0Down(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
    procedure SingleResolutionProc(Sender : TObject);
    procedure DoubleResolutionProc(Sender : TObject);
    procedure editPlayerHeightChange(Sender : TObject);
    procedure FlipXAllProc(Sender : TObject);
    procedure FlipYAllProc(Sender : TObject);
    procedure FlipXSelectedProc(Sender : TObject);
    procedure FlipYSelectedProc(Sender : TObject);
    procedure FrameOper(Sender : TObject);
    procedure ApplyPlayerHeight(Sender : TObject);
    procedure ByteEditorProc(Sender : TObject);
    procedure chkMultiColor01Change(Sender: TObject);
    procedure imgFrameLeave(Sender: TObject);
    procedure imgFrameDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgFrameMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgFrameUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure ClearAllFrames(Sender: TObject);
    procedure CloseModule(Sender: TObject);
    procedure InvertProc(Sender : TObject);
    procedure LoadAPLProc(Sender: TObject);
    procedure CopyFrameToAll(Sender : TObject);
    procedure CopyPlayers(Sender : TObject);
    procedure InvertBitsAll(Sender : TObject);
    procedure NewAnimation(Sender: TObject);
    procedure SaveAPLProc(Sender: TObject);
    procedure ShowGrid(Sender: TObject);
    procedure HideGrid(Sender: TObject);
    procedure Enable3rdColor(Sender: TObject);
    procedure Disable3rdColor(Sender: TObject);
    procedure CopyFromBuffer(Sender: TObject);
    procedure CopyToBuffer(Sender: TObject);
    procedure FrameColorChange(Sender: TObject);
    procedure SavePlayerAs(Sender: TObject);
    procedure GenCode(Sender: TObject);
    procedure ColorPalette(Sender: TObject);
    procedure Animate(Sender: TObject);
    procedure SavePlayer(Sender: TObject);
    procedure ClearFrame(Sender: TObject);
    procedure MoveLeft(Sender: TObject; oper : byte);
    procedure MoveRight(Sender: TObject; oper : byte);
    procedure MoveUp(Sender: TObject; oper : byte);
    procedure MoveDown(Sender: TObject; oper : byte);
    procedure FlipX(Sender: TObject);
    procedure FlipY(Sender: TObject);
    procedure FlipXProc(oper : byte);
    procedure FlipYProc(oper : byte);
  private
    { private declarations }
    btn : tMousebutton;
    isDataChanged : array[0..3] of boolean;
    isEnter : array[0..16] of boolean;
    isEdit : boolean;
    procedure Settings;
    procedure Plot(xf, yf : byte);
    procedure RefreshFrames(pm : TImage; factX, factY : byte; isGridShow : boolean);
    function FindFldData(pl : byte) : boolean;
    procedure SelectedFrame;
    procedure RefreshAnim(isEditorGrid : boolean);
    procedure ShiftLeft(Sender: TObject; oper : byte);
    procedure ShiftRight(Sender: TObject; oper : byte);
    procedure ShiftUp(Sender: TObject; oper : byte);
    procedure ShiftDown(Sender: TObject; oper : byte);
    procedure SetDoubleResolution(Sender: TObject);
    procedure SetSingleResolution(Sender: TObject);
  public
    { public declarations }
    filename : string;
    fld : array[0..16] of fldType;
    fld02 : array[0..1, 0..7] of byte;
    player : byte;  // Player index
    factX02, factY02 : byte;  // Multi-color player editor factor
    playerPos : array[0..16, 0..1, 0.._MAX_ANIM_FRAME_WIDTH, 0.._ANIM_MAX_LINES - 1] of byte;
    playerIndex : array[0..16, 0..1] of byte;
    frame, selected : byte;
    selFrame : byte;          // Selected frame
//    selPen : byte;            // Selected pen color
    selAnimFrameRate : byte;  // Selected animation frame rate
    procedure SetPlayers;
    procedure RefreshFrame(oper : byte);
  end;

var
  frmAnimator: TfrmAnimator;

implementation

{$R *.lfm}

uses
  main, colors, anim_load_save, anim_gen, lib, copy_players, byte_editor;

{ TfrmAnimator }

procedure TfrmAnimator.FormCreate(Sender: TObject);
var
  i : byte;
begin
  DoubleBuffered := true;
  btn := mbMiddle;
  Settings;
  for i := 0 to 16 do
    isEnter[i] := false;
end;

procedure TfrmAnimator.FormShow(Sender: TObject);
begin
  propFlagModules[8] := 1;

  frmMain.Top := 0;
  formId := formAnimator;

  filename := getDir + 'examples\anim01.apl';
  caption := programName + ' ' + programVersion + ' - Player Animator (' + filename + ')';
  statusBar.Panels[0].Text := 'Cursor coordinates: x: 0, y: 0';

  isEdit := false;
  player := 0;
  frame := 0;
  selected := 0;
  selAnimFrameRate := 2;
  animFrameHeight := _ANIM_APL_MAX_LINES;
  editPlayerHeight.Value := _ANIM_APL_MAX_LINES;
  SetPlayers;

  SetDoubleResolution(Sender);
  NewAnimation(Sender);
  RefreshFrame(0);
end;

procedure TfrmAnimator.FormActivate(Sender: TObject);
begin
  formId := formAnimator;
end;

procedure TfrmAnimator.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_1: begin
      rbPlayer0.Checked := true;
//      FrameColorChange(Sender);
    end;
    VK_2: begin
      rbPlayer1.Checked := true;
//      FrameColorChange(Sender);
    end;
    VK_F12: frmMain.Show;
  end;
end;

procedure TfrmAnimator.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[8] := 0;
  formId := formMain;
end;

procedure TfrmAnimator.ApplyPlayerHeight(Sender : TObject);
begin
//  _ANIM_MAX_LINES := editPlayerHeight.Value;
  animFrameHeight := editPlayerHeight.Value;
//  imgFrame01.Height := animFrameHeight*(factY02 + 2);

  //for i := 0 to 3 do
  //  isDataChanged[i] := false;

  //for j := 1 to 16 do begin
  //  if j < 10 then
  //    compIndex := '0' + IntToStr(j)
  //  else
  //    compIndex := IntToStr(j);
  //
  //  (FindComponent('imgFrame' + compIndex) as TImage).Height := imgFrame01.Height;
  //  (FindComponent('imgFrame' + compIndex) as TImage).Canvas.FillRect(
  //    bounds(0, 0, imgFrame01.Width, imgFrame01.Height));
  //end;

//  imgBuffer.Height := imgFrame01.Height;
//  imgBuffer.Canvas.FillRect(bounds(0, 0, imgFrame01.Width, imgFrame01.Height));

//  shape.Height := imgFrame01.Height + 24;
  RefreshAnim(isAnimEditorGrid);

  if animFrameHeight <= _ANIM_APL_MAX_LINES then begin
    aplAnim := normal;
    statusBar.Panels[2].Text := 'Standard Atari Player Editor format';
  end
  else begin
    aplAnim := extended;
    statusBar.Panels[2].Text := 'Extended Atari Player Editor format';
  end;
end;

procedure TfrmAnimator.ByteEditorProc(Sender : TObject);
var
  i, j : byte;
  k : integer;
  bin : string;
begin
  beType := formAnimator;
  maxLines := 16;
  frmByteEditor.ShowModal;

  if isDataExport then begin
    ClearFrame(Sender);

    for i := 0 to 1 do
      for k := 0 to maxLines - 1 do begin
        bin := Dec2Bin(exportData[k]);
        for j := 0 to 7 do begin
          fld[selected][i, j, k] := StrToInt(bin[j + 1]);
          playerPos[selected, i, playerIndex[selected, i] + j, k] := StrToInt(bin[j + 1]);
//            playerPos[16, i, playerIndex[16, i] + j, k];
        end;
      end;

    HideGrid(Sender);
    RefreshFrame(0);
  end;
end;

procedure TfrmAnimator.FrameOper(Sender : TObject);
begin
//  ShowCursor(frmAnimator, frmAnimator, crHourGlass);
//  Screen.Cursor := crHourGlass;
  Screen.BeginWaitCursor;

  if radShiftChar.Checked then begin
    case (Sender as TSpeedButton).Tag of
      0: ShiftLeft(Sender, 0);
      1: ShiftRight(Sender, 0);
      2: ShiftUp(Sender, 0);
      3: ShiftDown(Sender, 0);
    end;
  end
  else if radMoveChar.Checked then begin
    case (Sender as TSpeedButton).Tag of
      0: MoveLeft(Sender, 0);
      1: MoveRight(Sender, 0);
      2: MoveUp(Sender, 0);
      3: MoveDown(Sender, 0);
    end;
  end
  else if radShiftAllChars.Checked then begin
    case (Sender as TSpeedButton).Tag of
      0: ShiftLeft(Sender, 16);
      1: ShiftRight(Sender, 16);
      2: ShiftUp(Sender, 16);
      3: ShiftDown(Sender, 16);
    end;
  end
  else if radMoveAllChars.Checked then begin
    case (Sender as TSpeedButton).Tag of
      0: MoveLeft(Sender, 16);
      1: MoveRight(Sender, 16);
      2: MoveUp(Sender, 16);
      3: MoveDown(Sender, 16);
    end;
  end;

//  ShowCursor(frmAnimator, frmAnimator, crDefault);
//  Screen.Cursor := crDefault;
  Screen.EndWaitCursor;
end;

procedure TfrmAnimator.FlipXSelectedProc(Sender : TObject);
begin
  FlipXProc(0);
end;

procedure TfrmAnimator.FlipYSelectedProc(Sender : TObject);
begin
  FlipYProc(0);
end;

procedure TfrmAnimator.FlipXAllProc(Sender : TObject);
begin
  FlipXProc(16);
end;

procedure TfrmAnimator.editPlayerHeightChange(Sender : TObject);
begin
  if editPlayerHeight.Value <= 48 then begin
    aplAnim := normal;
    filename := getDir + 'examples\anim01.apl';
  end
  else begin
    aplAnim := extended;
    filename := getDir + 'examples\anim01.apm';
//    debug('2');
  end;
  ApplyPlayerHeight(Sender);
end;

procedure TfrmAnimator.pcolr0Down(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  frmColors.SelColor := TShape(Sender).Tag;
  frmColors.Show;

  if frmColors.SelColor = 4 then
    rbPlayer0.Checked := true
  else
    rbPlayer1.Checked := true;
end;

procedure TfrmAnimator.FlipYAllProc(Sender : TObject);
begin
  FlipYProc(16);
end;

{-----------------------------------------------------------------------------
 Animate
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.Animate(Sender: TObject);
var
  i, j : byte;
begin
//  Screen.Cursor := crHourGlass;
  Screen.BeginWaitCursor;

  for j := 1 to numIterations.Value do
    for i := 1 to numFrames.Value do begin
      frame := i - 1;
      RefreshFrames(imgP0P1_normalMultiAll, 3, 3, false);
      RefreshFrames(imgP0P1_doubleMultiAll, 6, 3, false);
      RefreshFrames(imgP0P1_quadrableMultiAll, 12, 3, false);
      RefreshFrames(imgP0P1_normalSrMultiAll, 3, 2, false);
      RefreshFrames(imgP0P1_doubleSrMultiAll, 6, 2, false);
      RefreshFrames(imgP0P1_quadrableSrMultiAll, 12, 2, false);
      Sleep(100);
    end;

//  Screen.Cursor := crDefault;
  Screen.EndWaitCursor;
end;

procedure TfrmAnimator.SelectedFrame;
begin
  frame := selected;
  case frame of
    0: imgTemp := imgFrame01;
    1: imgTemp := imgFrame02;
    2: imgTemp := imgFrame03;
    3: imgTemp := imgFrame04;
    4: imgTemp := imgFrame05;
    5: imgTemp := imgFrame06;
    6: imgTemp := imgFrame07;
    7: imgTemp := imgFrame08;
    8: imgTemp := imgFrame09;
    9: imgTemp := imgFrame10;
    10: imgTemp := imgFrame11;
    11: imgTemp := imgFrame12;
    12: imgTemp := imgFrame13;
    13: imgTemp := imgFrame14;
    14: imgTemp := imgFrame15;
    15: imgTemp := imgFrame16;
    16: imgTemp := imgBuffer;
  end;
end;

{-----------------------------------------------------------------------------
 Clear selected frame
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.ClearFrame(Sender: TObject);
var
  j, m, n : byte;
begin
  for j := 0 to 1 do
    for m := 0 to 7 do
      for n := 0 to 220 do
        fld[selected][j, m, n] := 0;

  for j := 0 to 1 do
    for m := 0 to _MAX_ANIM_FRAME_WIDTH do
      for n := 0 to _ANIM_MAX_LINES - 1 do
        playerPos[selected, j, m, n] := 0;

  RefreshFrame(0);
end;

{-----------------------------------------------------------------------------
 Clear all frames
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.ClearAllFrames(Sender: TObject);
var
  i, j, m, n : byte;
begin
  for i := 0 to 16 do
    for j := 0 to 1 do begin
      for m := 0 to _MAX_ANIM_FRAME_WIDTH do
        for n := 0 to _ANIM_MAX_LINES - 1 do
          playerPos[i, j, m, n] := 0;

      for m := 0 to 7 do
        for n := 0 to 220 do
          fld[i][j, m, n] := 0;
    end;

  for j := 1 to 16 do begin
    if j < 10 then begin
      (FindComponent('imgFrame0' + IntToStr(j)) as TImage).Canvas.Brush.Color := colTab[0];
      (FindComponent('imgFrame0' + IntToStr(j)) as TImage).Canvas.FillRect(
        bounds(0, 0, imgFrame01.Width, imgFrame01.Height));
    end
    else begin
      (FindComponent('imgFrame' + IntToStr(j)) as TImage).Canvas.Brush.Color := colTab[0];
      (FindComponent('imgFrame' + IntToStr(j)) as TImage).Canvas.FillRect(
        bounds(0, 0, imgFrame01.Width, imgFrame01.Height));
    end;
  end;
//  imgBuffer.Canvas.Brush.Color := colTab[0];
//  imgBuffer.Canvas.FillRect(bounds(0, 0, imgBuffer.Width, imgBuffer.Height));
  FillRectEx(imgBuffer, colTab[0], 0, 0, imgBuffer.Width, imgBuffer.Height);

  RefreshFrame(0);
end;

procedure TfrmAnimator.Settings;
begin
  factX02 := 8;
  isAnimEditorGrid := false;

  //grpPlayerHeight.Color := clRed;
  //grpPlayerHeight.BorderWidth := 4;
end;

procedure TfrmAnimator.SetPlayers;
var
  i, j, n : byte;
begin
//  imgFrame01.Height := animFrameHeight*(factY02 + 8);

  for i := 0 to 3 do
    isDataChanged[i] := false;

  //for j := 1 to 16 do begin
  //  if j < 10 then
  //    compIndex := '0' + IntToStr(j)
  //  else
  //    compIndex := IntToStr(j);

    //(FindComponent('imgFrame' + compIndex) as TImage).Canvas.Brush.Color := colTab[0];
    //(FindComponent('imgFrame' + compIndex) as TImage).Height := imgFrame01.Height;
    //(FindComponent('imgFrame' + compIndex) as TImage).Canvas.FillRect(bounds(
    //  0, 0, imgFrame01.Width, imgFrame01.Height));
//  end;

  //imgBuffer.Height := imgFrame01.Height;
  //imgBuffer.Canvas.FillRect(bounds(0, 0, imgFrame01.Width, imgFrame01.Height));

  //imgFrame01.Canvas.Brush.Color := colTab[0];
  //imgFrame01.Canvas.FillRect(bounds(0, 0, imgFrame01.Width, imgFrame01.Height));
  //imgFrame02.Canvas.Brush.Color := colTab[0];
  //imgFrame02.Canvas.FillRect(bounds(0, 0, imgFrame02.Width, imgFrame01.Height));
  //imgFrame03.Canvas.Brush.Color := clBlack;
  //imgFrame03.Canvas.FillRect(bounds(0, 0, imgFrame03.Width, imgFrame01.Height));
//  imgBuffer.Canvas.Brush.Color := colTab[0];
//  imgBuffer.Canvas.FillRect(bounds(0, 0, imgBuffer.Width, imgBuffer.Height));
  FillRectEx(imgBuffer, colTab[0], 0, 0, imgBuffer.Width, imgBuffer.Height);

  for n := 0 to 1 do
    for i := 0 to 7 do
      for j := 0 to _ANIM_MAX_LINES - 1 do
        fld[frame][n, i, j] := 0;

  for n := 0 to 1 do
    for i := 0 to _MAX_ANIM_FRAME_WIDTH - 1 do
      for j := 0 to _ANIM_MAX_LINES - 1 do
        playerPos[frame, n, i, j] := 0;

  if rbPlayer0.Checked then
    frmColors.SelColor := 4
  else if rbPlayer1.Checked then
    frmColors.SelColor := 5;

  playerIndex[frame, 0] := 0;
  playerIndex[frame, 1] := 0;

  with shape do begin
    Top := imgFrame01.Top - 24;
    Left := imgFrame01.Left - 4;
    Width := imgFrame01.Width + 8;
    Height := imgFrame01.Height + 28;
  end;

  numAnimRate.Value := selAnimFrameRate;

  imgFrame01.BringToFront;
end;

{-----------------------------------------------------------------------------
 Plot individual player pixel
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.Plot(xf, yf : byte);
var
  col : byte;
begin
  case btn of
    mbLeft : col := 1;
    mbRight: col := 0;
  else
    exit;
  end;

  if xf > 28 + 3 then
    xf := 28 + 3;

  if btn = mbRight then begin
    playerPos[frame, 0, xf, yf] := 0;
    playerPos[frame, 1, xf, yf] := 0;
    fld[frame][0, xf, yf] := 0;
    fld[frame][1, xf, yf] := 0;
  end
  else begin
    playerPos[frame, player, xf, yf] := col;
    fld[frame][player, xf, yf] := col;
    isDataChanged[player] := true;
  end;

//  if not CalcPlayerWidth(player, xf, yf) then Exit;

  col := player;

  case frame of
    0: begin
//         imgFrame01.Canvas.Brush.Color := coltab[col + 4];
//         imgFrame01.Canvas.FillRect(bounds(xf*factX02, yf*factY02, factX02, factY02));
         FillRectEx(imgFrame01, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    1: begin
         FillRectEx(imgFrame02, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    2: begin
         FillRectEx(imgFrame03, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    3: begin
         FillRectEx(imgFrame04, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    4: begin
         FillRectEx(imgFrame05, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    5: begin
         FillRectEx(imgFrame06, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    6: begin
         FillRectEx(imgFrame07, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    7: begin
         FillRectEx(imgFrame08, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    8: begin
         FillRectEx(imgFrame09, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    9: begin
         FillRectEx(imgFrame10, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    10: begin
         FillRectEx(imgFrame11, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    11: begin
         FillRectEx(imgFrame12, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    12: begin
         FillRectEx(imgFrame13, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    13: begin
         FillRectEx(imgFrame14, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    14: begin
         FillRectEx(imgFrame15, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    15: begin
         FillRectEx(imgFrame16, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
    16: begin
         FillRectEx(imgBuffer, coltab[col + 4], xf*factX02, yf*factY02, factX02, factY02);
       end;
  end;
end;

procedure TfrmAnimator.RefreshFrames(pm : TImage; factX, factY : byte; isGridShow : boolean);
var
  xf, yf : byte;
  index, ci : byte;
  col, col01 : byte;
  c, d : integer;
begin
  FillRectEx(pm, colTab[0], 0, 0, pm.Width, pm.Height);

//  frame := selected;
  index := 0;
  ci := index;

  for yf := 0 to _ANIM_MAX_LINES - 1 do begin
    for xf := 0 to 31 do begin
      if isAnimEditorGrid and isGridShow then begin
        pm.Canvas.Pen.Color := clWhite;
        c := xf*factX shl 1;
        d := yf*factY shl 1;
        if c = 0 then c := factX;
        if d = 0 then d := factY;

        pm.Canvas.Rectangle(xf*factX, yf*factY, c, d);
      end;

      col := playerPos[frame, 0, xf, yf];      // Player 0 value
      col01 := playerPos[frame, 1, xf, yf];    // Player 1 value

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
      else begin
        if col01 = 1 then
          pm.Canvas.Brush.Color := coltab[5]
        else
          pm.Canvas.Brush.Color := colTab[0];
      end;

      if isAnimEditorGrid and isGridShow then
        pm.Canvas.FillRect(bounds(xf*factX + 1, yf*factY + 1, factX - 1, factY - 1))
      else
        pm.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
    end;
  end;

  pm.Refresh;
end;

procedure TfrmAnimator.RefreshFrame(oper : byte);
begin
  SelectedFrame;
  RefreshFrames(imgTemp, factX02, factY02, true);
  if oper = 0 then begin
    RefreshFrames(imgP0P1_normalMultiAll, 3, 3, false);
    RefreshFrames(imgP0P1_doubleMultiAll, 6, 3, false);
    RefreshFrames(imgP0P1_quadrableMultiAll, 12, 3, false);
    RefreshFrames(imgP0P1_normalSrMultiAll, 3, 2, false);
    RefreshFrames(imgP0P1_doubleSrMultiAll, 6, 2, false);
    RefreshFrames(imgP0P1_quadrableSrMultiAll, 12, 2, false);
  end;

  FillRectEx(pcolr0, coltab[4], 0, 0, pcolr1.width, pcolr1.Height);
  FillRectEx(pcolr1, coltab[5], 0, 0, pcolr1.width, pcolr1.Height);

  if selected < 16 then
    statusBar.Panels[1].Text := 'Selected frame: ' + IntToStr(selected + 1)
  else
    statusBar.Panels[1].Text := 'Buffer';
end;

procedure TfrmAnimator.imgFrameDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xf, yf : byte;
begin
  selected := TImage(Sender).Tag;
  frame := selected;

  with shape do begin
    Top := TImage(Sender).Top - 24;
    Left := TImage(Sender).Left - 4;
    Width := TImage(Sender).Width + 8;
    Height := TImage(Sender).Height + 28;
  end;

  SelectedFrame;
  imgTemp.BringToFront;

  if not isEnter[TImage(Sender).Tag] then begin
    isEnter[TImage(Sender).tag] := true;
    Exit;
  end;

  if (Button = mbRight) and (ssCtrl in Shift) then begin
    popMenu.Popup(x + TImage(Sender).Left + 40, y + TImage(Sender).Top + 40);
    exit;
  end;

  btn := Button;
  xf := X div factX02;
  yf := Y div factY02;
  if (xf >= playerIndex[frame, player] - 1) or
     (xf >= playerIndex[frame, player]) then
  begin
    Plot(xf, yf);
  end;

  // Data is changed
  if not isEdit then begin
    isEdit := true;
    if Pos(' *', caption) = 0 then
      caption := caption + ' *';
  end;
end;

procedure TfrmAnimator.imgFrameMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : byte;
begin
  frame := TImage(Sender).Tag;
  //if frame < 16 then begin
  //  if selected = 16 then
  //    statusBar.Panels[1].Text := 'Selected frame: buffer';
  //                                //' / Focused frame: ' + IntToStr(frame + 1)
  //  else
  //    statusBar.Panels[1].Text := 'Selected frame: ' + IntToStr(selected + 1);
  //                                //' / Focused frame: ' + IntToStr(frame + 1)
  //end
  //else
  //  statusBar.Panels[1].Text := 'Buffer';

  xf := X div factX02;
  yf := Y div factY02;

  if (xf >= playerIndex[frame, player] - 1) or (xf >= playerIndex[frame, player]) then
    Plot(xf, yf);

  statusBar.Panels[0].Text := '';
  statusBar.Panels[0].Text := 'Cursor coordinates: ' +
                              'x: ' + IntToStr(xf) + ', y: ' + IntToStr(yf);
end;

procedure TfrmAnimator.imgFrameUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  btn := mbMiddle;
  RefreshFrame(0);
end;

procedure TfrmAnimator.chkMultiColor01Change(Sender: TObject);
begin
  RefreshFrame(0);
end;

procedure TfrmAnimator.imgFrameLeave(Sender: TObject);
begin
  isEnter[TImage(Sender).tag] := false;
end;

function TfrmAnimator.FindFldData(pl : byte) : boolean;
var
  x : byte;
  y : integer;
begin
  result := false;
  for y := 0 to _ANIM_MAX_LINES - 1 do
    for x := 0 to 7 do
      if fld[frame][pl, x, y] = 1 then begin
        result := true;
        break;
      end;
end;

procedure TfrmAnimator.CloseModule(Sender: TObject);
begin
  Close;
end;

procedure TfrmAnimator.LoadAPLProc(Sender: TObject);
begin
  isEdit := false;

  if Sender is TMenuItem then begin
//    debug('Sender is TMenuItem');
    case (Sender as TMenuItem).Tag of
      0: begin
        aplAnim := normal;
        statusBar.Panels[2].Text := 'Standard Atari Player Editor format';
      end;
      1: begin
        aplAnim := extended;
        statusBar.Panels[2].Text := 'Extended Atari Player Editor format';
      end;
      2: begin
        aplAnim := fixed52;
        statusBar.Panels[2].Text := 'Fixed 52-height pixel Atari Player Editor format';
      end;
    end;
  end
  else begin
//    debug('Sender is not TMenuItem');
    aplAnim := normal;
    statusBar.Panels[2].Text := 'Standard Atari Player Editor format';
  end;

  frmAnimLoadSave := TfrmAnimLoadSave.Create(Self);
  with frmAnimLoadSave do
    try
      ShowModal;
    finally
      Free;
    end;

  HideGrid(Sender);
  RefreshFrame(0);
  player := 0;
  rbPlayer0.Checked := true;
  numFrames.Value := animFrames;
  numAnimRate.Value := selAnimFrameRate;
  editPlayerHeight.Value := animFrameHeight;

//  _ANIM_MAX_LINES := animFrameHeight;
//  frmColors.Show;
end;

procedure TfrmAnimator.InvertProc(Sender : TObject);
var
  j, m, n : byte;
begin
  for j := 0 to 1 do
    for m := 0 to 7 do
      for n := 0 to 220 do
        fld[selected][j, m, n] := 1 - fld[selected][j, m, n];

  for j := 0 to 1 do
    for m := 0 to _MAX_ANIM_FRAME_WIDTH do
      for n := 0 to _ANIM_MAX_LINES - 1 do
        playerPos[selected, j, m, n] := 1 - playerPos[selected, j, m, n];

  RefreshFrame(0);
end;

procedure TfrmAnimator.CopyFrameToAll(Sender : TObject);
var
  i, j, k, fr: byte;
begin
  for fr := 0 to 16 do begin
    if fr <> selected then begin
      for i := 0 to 1 do
        for j := 0 to 7 do
          for k := 0 to _ANIM_MAX_LINES - 1 do begin
            fld[fr][i, j, k] := fld[selected][i, j, k];
            playerPos[fr, i, playerIndex[fr, i] + j, k] :=
              playerPos[selected, i, playerIndex[selected, i] + j, k];
          end;

      SelectedFrame;
      RefreshFrames(imgTemp, factX02, factY02, true);
    end;
  end;

  HideGrid(Sender);
end;

procedure TfrmAnimator.CopyPlayers(Sender : TObject);
begin
  frmCopyPlayers := TfrmCopyPlayers.Create(Self);
  with frmCopyPlayers do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmAnimator.InvertBitsAll(Sender : TObject);
var
  i, j, m, n : byte;
  origFrame : byte;
begin
//  ShowCursor(frmAnimator, frmAnimator, crHourGlass);
//  Screen.Cursor := crHourGlass;
  Screen.BeginWaitCursor;

  origFrame := selected;
  for i := 0 to 16 do begin
    selected := i;
    for j := 0 to 1 do begin
      for m := 0 to _MAX_ANIM_FRAME_WIDTH do
        for n := 0 to _ANIM_MAX_LINES - 1 do
          playerPos[i, j, m, n] := 1 - playerPos[i, j, m, n];

      for m := 0 to 7 do
        for n := 0 to 220 do
          fld[i][j, m, n] := 1 - fld[i][j, m, n];
    end;
  end;
  for i := 0 to 16 do begin
    selected := i;
    refreshFrame(i);
  end;
  selected := origFrame;

//  ShowCursor(frmAnimator, frmAnimator, crDefault);
//  Screen.Cursor := crDefault;
  Screen.EndWaitCursor;
end;

procedure TfrmAnimator.NewAnimation(Sender: TObject);
begin
  ClearAllFrames(Sender);
  filename := getDir + 'examples\anim01.apl';
  caption := programName + ' ' + programVersion + ' - Player Animator (' + filename + ')';
//  statusBar.Panels[0].Text := 'Cursor coordinates: x: 0, y: 0';
end;

procedure TfrmAnimator.SaveAPLProc(Sender: TObject);
var
  filenamexy : string;
  i : integer;
  fs : TFileStream;

procedure FillPlayer(pl : byte);
var
  bin : string;
  n, i, j : byte;
begin
  if aplAnim = normal then begin
    for i := 0 to 16 do
      // Player data, frame length
      // 17 - Player data, copy buffer
      for j := 0 to _ANIM_APL_MAX_LINES - 1 do begin
        if j <= animFrameHeight - 1 then begin
          bin := '';
          for n := 0 to 7 do
            bin += IntToStr(fld[i][pl, n, j]);

          fs.WriteByte(Bin2Dec(bin));
        end
        else
          fs.WriteByte(0);
      end;
  end
  else begin
    for i := 0 to 16 do
      // Player data, frame length
      // 17 - Player data, copy buffer
      for j := 0 to animFrameHeight - 1 do begin
        bin := '';
        for n := 0 to 7 do
          bin += IntToStr(fld[i][pl, n, j]);

        fs.WriteByte(Bin2Dec(bin));
      end;
  end;
end;

begin
  isEdit := false;

  if Sender is TMenuItem then begin
    case (Sender as TMenuItem).Tag of
      0: aplAnim := normal;
      1: aplAnim := extended;
      2: aplAnim := fixed52;
    end;
  end
  else
    aplAnim := normal;

  case aplAnim of
    normal: begin
//      animFrameHeight := _ANIM_APL_MAX_LINES;
      frmMain.dlgSave.Filter := 'Atari Player Editor data file (*.apl)|*.apl;|All files (*.*)|*.*';
      statusBar.Panels[2].Text := 'Standard Atari Player Editor format';
    end;
    extended: begin
      frmMain.dlgSave.Filter := 'Atari Player Editor data file (*.apm)|*.apm;|All files (*.*)|*.*';
      statusBar.Panels[2].Text := 'Extended Atari Player Editor format';
    end;
    fixed52: begin
      animFrameHeight := 52;
      frmMain.dlgSave.Filter := 'Atari Player Editor data file' +
                                ' (*.apl, *.apm)|*.apl;*.apm|All files (*.*)|*.*';
      statusBar.Panels[2].Text := 'Fixed 52-height pixel Atari Player Editor format';
    end;
  end;

  //if editPlayerHeight.Value <= 48 then
  //  frmMain.dlgSave.Filter := 'Atari Player Editor data file (*.apl)|*.apl;|All files (*.*)|*.*'
  //else
  //  frmMain.dlgSave.Filter := 'Atari Player Editor extended data file (*.apm)|*.apm;|All files (*.*)|*.*';

  //debug('animFrameHeight', animFrameHeight);
  //if animFrameHeight <= _ANIM_APL_MAX_LINES then
  //  animFrameHeight := _ANIM_APL_MAX_LINES;

  frmMain.dlgSave.Title := 'Save Atari Player data file as';
  frmMain.dlgSave.Filename := filename;
  if frmMain.dlgSave.Execute then begin
//    Screen.Cursor := crHourGlass;
    Screen.BeginWaitCursor;
    filenamexy := frmMain.dlgSave.Filename;
    fs := TFileStream.Create(filenamexy, fmCreate);
    try
      // File version identifier
      fs.WriteByte($9a);
      fs.WriteByte($f8);
      fs.WriteByte($39);
      fs.WriteByte($21);

      // Number of frames
      fs.WriteByte(numFrames.Value);

      // Height
      fs.WriteByte(animFrameHeight);

      // Gap
      fs.WriteByte(0);

      // P0 colour, frames 1..16
      for i := 1 to 16 do
        fs.WriteByte(colorValues[4]);

      // P0 colour, copy buffer
      fs.WriteByte(colorValues[4]);
//      showmessage(inttostr(coltab[4]));

      // P1 colour, frames 1..16
      for i := 1 to 16 do
        fs.WriteByte(colorValues[5]);

      // P1 colour, copy buffer
      fs.WriteByte(colorValues[5]);

      // Background colour
      fs.WriteByte(colorValues[0]);

      FillPlayer(0);
      FillPlayer(1);

      // Selected frame
      fs.WriteByte(1);

      // Pen colour
      fs.WriteByte(colorValues[4]);

      // Animation frame rate
      fs.WriteByte(numAnimRate.Value);

      frmAnimator.caption := programName + ' ' + programVersion +
                             ' - Player Animator (' + filenamexy + ')';

      //isDataChanged[player] := false;
    finally
//      Screen.Cursor := crDefault;
      Screen.EndWaitCursor;
      fs.Free;
    end;
  end;
end;

procedure TfrmAnimator.RefreshAnim(isEditorGrid : boolean);
begin
  isAnimEditorGrid := isEditorGrid;

  frame := 0; RefreshFrames(imgFrame01, factX02, factY02, true);
  frame := 1; RefreshFrames(imgFrame02, factX02, factY02, true);
  frame := 2; RefreshFrames(imgFrame03, factX02, factY02, true);
  frame := 3; RefreshFrames(imgFrame04, factX02, factY02, true);
  frame := 4; RefreshFrames(imgFrame05, factX02, factY02, true);
  frame := 5; RefreshFrames(imgFrame06, factX02, factY02, true);
  frame := 6; RefreshFrames(imgFrame07, factX02, factY02, true);
  frame := 7; RefreshFrames(imgFrame08, factX02, factY02, true);
  frame := 8; RefreshFrames(imgFrame09, factX02, factY02, true);
  frame := 9; RefreshFrames(imgFrame10, factX02, factY02, true);
  frame := 10; RefreshFrames(imgFrame11, factX02, factY02, true);
  frame := 11; RefreshFrames(imgFrame12, factX02, factY02, true);
  frame := 12; RefreshFrames(imgFrame13, factX02, factY02, true);
  frame := 13; RefreshFrames(imgFrame14, factX02, factY02, true);
  frame := 14; RefreshFrames(imgFrame15, factX02, factY02, true);
  frame := 15; RefreshFrames(imgFrame16, factX02, factY02, true);
  frame := 16; RefreshFrames(imgBuffer, factX02, factY02, true);
end;

procedure TfrmAnimator.ShowGrid(Sender: TObject);
begin
  RefreshAnim(true);
end;

procedure TfrmAnimator.HideGrid(Sender: TObject);
begin
  RefreshAnim(false);
end;

procedure TfrmAnimator.Enable3rdColor(Sender: TObject);
begin
  isPmMixedColor := true;
  statusBar.Panels[3].Text := 'Player mixed (3rd) color enabled';
  RefreshFrame(0);
  RefreshAnim(isAnimEditorGrid);
end;

procedure TfrmAnimator.Disable3rdColor(Sender: TObject);
begin
  isPmMixedColor := false;
  statusBar.Panels[3].Text := 'Player mixed (3rd) color disabled';
  RefreshFrame(0);
  RefreshAnim(isAnimEditorGrid);
end;

procedure TfrmAnimator.CopyFromBuffer(Sender: TObject);
var
  i, j, k : byte;
begin
  for i := 0 to 1 do begin
    for j := 0 to 7 do
      for k := 0 to _ANIM_MAX_LINES - 1 do begin
        fld[selected][i, j, k] := fld[16][i, j, k];
        playerPos[selected, i, playerIndex[selected, i] + j, k] :=
          playerPos[16, i, playerIndex[16, i] + j, k];
      end;
  end;

  HideGrid(Sender);
  RefreshFrame(0);
end;

procedure TfrmAnimator.CopyToBuffer(Sender: TObject);
var
  i, j, k : byte;
begin
  for i := 0 to 1 do begin
    for j := 0 to 7 do
      for k := 0 to _ANIM_MAX_LINES - 1 do begin
        fld[16][i, j, k] := fld[selected][i, j, k];
        playerPos[16, i, playerIndex[16, i] + j, k] :=
          playerPos[selected, i, playerIndex[selected, i] + j, k];
      end;
  end;

  HideGrid(Sender);
  RefreshFrame(0);
end;

procedure TfrmAnimator.FrameColorChange(Sender: TObject);
begin
  RefreshFrame(0);

  if rbPlayer0.Checked then
    player := 0
  else if rbPlayer1.Checked then
    player := 1;

//  lblPlayer.Caption := 'Player ' + IntToStr(player);

  //if pmgFilenames[player] <> '' then begin
  //  caption := programName + ' ' + programVersion + ' - Player/missile graphics editor';
  //  caption := caption + ' (' + pmgFilenames[player] + ')';
  //end;
end;

procedure TfrmAnimator.GenCode(Sender: TObject);
begin
  frmAnimGen := TfrmAnimGen.Create(Self);
  with frmAnimGen do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmAnimator.ColorPalette(Sender: TObject);
begin
  frmColors.Show;
end;

{-----------------------------------------------------------------------------
 Shift player left
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.ShiftLeft(Sender: TObject; oper : byte);
var
  i, x, y, m, n, fr : byte;
  origFrame : byte;
begin
  //if oper = 0 then begin
  //  for i := 0 to 1 do
  //    for y := 0 to _ANIM_MAX_LINES - 1 do begin
  //      n := fld[selected][i, 0, y];
  //      m := playerPos[selected, i, playerIndex[selected, i], y];
  //
  //      for x := 1 to grX do begin
  //        fld[selected][i, x - 1, y] := fld[selected][i, x, y];
  //        playerPos[selected, i, playerIndex[selected, i] + x - 1, y] :=
  //          playerPos[selected, i, playerIndex[selected, i] + x, y];
  //      end;
  //
  //      fld[selected][i, 7, y] := n;
  //      playerPos[selected, i, playerIndex[selected, i] + 7, y] := m;
  //    end;
  //end
  //else begin

  origFrame := selected;
//  showmessage(inttostr(origFrame));
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    for i := 0 to 1 do
      for y := 0 to _ANIM_MAX_LINES - 1 do begin
        n := fld[selected][i, 0, y];
        m := playerPos[selected, i, playerIndex[selected, i], y];
        for x := 1 to 7 do begin
          fld[selected][i, x - 1, y] := fld[fr][i, x, y];
          playerPos[selected, i, playerIndex[selected, i] + x - 1, y] :=
            playerPos[selected, i, playerIndex[selected, i] + x, y];
        end;

        fld[selected][i, 7, y] := n;
        playerPos[selected, i, playerIndex[selected, i] + 7, y] := m;
      end;
  end;
  //if oper = 0 then
  //  RefreshFrame(0)
  //else begin
  //  for fr := 0 to 16 do begin
  //    selected := fr;
  //    SelectedFrame;
  //    RefreshFrames(imgTemp, factX02, factY02, true);
  //  end;
  //end;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    refreshFrame(oper);
  end;
  selected := origFrame;
//  showmessage(inttostr(selected));
end;

{-----------------------------------------------------------------------------
 Shift player right
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.ShiftRight(Sender: TObject; oper : byte);
var
  i, x, y, m, n, fr : byte;
  origFrame : byte;
begin
  origFrame := selected;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    for i := 0 to 1 do
      for y := 0 to _ANIM_MAX_LINES - 1 do begin
        n := fld[selected][i, 7, y];
        m := playerPos[selected, i, playerIndex[selected, i] + 7, y];
        for x := 7 - 1 downto 0 do begin
          fld[selected][i, x + 1, y] := fld[selected][i, x, y];
          playerPos[selected, i, playerIndex[selected, i] + x + 1, y] :=
            playerPos[selected, i, playerIndex[selected, i] + x, y];
        end;

        fld[selected][i, 0, y] := n;
        playerPos[selected, i, playerIndex[selected, i], y] := m;
      end;
  end;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    refreshFrame(oper);
  end;
  selected := origFrame;
  //else begin
  //  for fr := 0 to 16 do begin
  //    for i := 0 to 1 do
  //      for y := 0 to _ANIM_MAX_LINES - 1 do begin
  //        n := fld[fr][i, 7, y];
  //        m := playerPos[fr, i, playerIndex[fr, i] + 7, y];
  //
  //        for x := grX - 1 downto 0 do begin
  //          fld[fr][i, x + 1, y] := fld[fr][i, x, y];
  //          playerPos[fr, i, playerIndex[fr, i] + x + 1, y] :=
  //            playerPos[fr, i, playerIndex[fr, i] + x, y];
  //        end;
  //
  //        fld[fr][i, 0, y] := n;
  //        playerPos[fr, i, playerIndex[fr, i], y] := m;
  //      end;
  //
  //    selected := fr;
  //    SelectedFrame;
  //    RefreshFrames(imgTemp, factX02, factY02, true);
  //  end;
  //end;
  //refreshPM;
end;

{-----------------------------------------------------------------------------
 Shift player up
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.ShiftUp(Sender: TObject; oper : byte);
var
  i, x, y, fr : byte;
//  fld02 : array[0..7] of byte;
  origFrame : byte;
begin
  origFrame := selected;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    for i := 0 to 1 do begin
      for x := 0 to 7 do
        fld02[i, x] := fld[selected][i, x, 0];

      for x := 0 to 7 do
        for y := 1 to _ANIM_MAX_LINES - 1 do begin
          fld[selected][i, x, y - 1] := fld[selected][i, x, y];
          fld[selected][i, x, y] := 0;
          playerPos[selected, i, playerIndex[selected, i] + x, y - 1] :=
            playerPos[selected, i, playerIndex[selected, i] + x, y];
          playerPos[selected, i, playerIndex[selected, i] + x, y] := 0;
        end;

      for x := 0 to 7 do begin
        fld[selected][i, x, _ANIM_MAX_LINES - 1] := fld02[i, x];
        playerPos[selected, i, playerIndex[selected, i] + x, _ANIM_MAX_LINES - 1] := fld02[i, x];
      end;
    end;
  end;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    refreshFrame(oper);
  end;
  selected := origFrame;
end;

{-----------------------------------------------------------------------------
 Shift frame down
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.ShiftDown(Sender: TObject; oper : byte);
var
  i, x, y, fr : byte;
  origFrame : byte;
begin
  origFrame := selected;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    for i := 0 to 1 do begin
      for x := 0 to 7 do
        fld02[i, x] := fld[selected][i, x, _ANIM_MAX_LINES - 1];
    end;

    for i := 0 to 1 do begin
      for x := 0 to 7 do
        for y := _ANIM_MAX_LINES - 2 downto 0 do begin
          fld[selected][i, x, y + 1] := fld[selected][i, x, y];
          fld[selected][i, x, y] := 0;
          playerPos[selected, i, playerIndex[selected, i] + x, y + 1] :=
            playerPos[selected, i, playerIndex[selected, i] + x, y];
          playerPos[selected, i, playerIndex[selected, i] + x, y] := 0;
        end;
    end;

    for i := 0 to 1 do begin
      for x := 0 to 7 do begin
        fld[selected][i, x, 0] := fld02[i, x];
        playerPos[selected, i, playerIndex[selected, i] + x, 0] := fld02[i, x];
      end;
    end;
  end;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    refreshFrame(oper);
  end;
  selected := origFrame;
end;

{-----------------------------------------------------------------------------
 Move player left
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.MoveLeft(Sender: TObject; oper : byte);
var
  i, x, y, fr : byte;
  origFrame : byte;
begin
  origFrame := selected;
//  MoveLeftPM(grX, _ANIM_MAX_LINES - 1, player, fld);
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    for i := 0 to 1 do
      for x := 1 to 7 do
        for y := _ANIM_MAX_LINES - 1 downto 0 do begin
          playerPos[selected, i, playerIndex[selected, i] + x - 1, y] :=
            playerPos[selected, i, playerIndex[selected, i] + x, y];
          playerPos[selected, i, playerIndex[selected, i] + x, y] := 0;
        end;
  end;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    refreshFrame(oper);
  end;
  selected := origFrame;
end;

{-----------------------------------------------------------------------------
 Move player right
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.MoveRight(Sender: TObject; oper : byte);
var
  i, x, y, fr : byte;
  origFrame : byte;
begin
  origFrame := selected;
//  MoveRightPM(grX, _ANIM_MAX_LINES - 1, player, fld);
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    for i := 0 to 1 do
      for x := 7 - 1 downto 0 do
        for y := _ANIM_MAX_LINES - 1 downto 0 do begin
          playerPos[selected, i, playerIndex[selected, i] + x + 1, y] :=
            playerPos[selected, i, playerIndex[selected, i] + x, y];
          playerPos[selected, i, playerIndex[selected, i] + x, y] := 0;
        end;
  //end
  //else begin
  //  for fr := 0 to 16 do begin
  //    for i := 0 to 1 do
  //      for x := grX - 1 downto 0 do
  //        for y := _ANIM_MAX_LINES - 1 downto 0 do begin
  //          playerPos[fr, i, playerIndex[fr, i] + x + 1, y] :=
  //            playerPos[fr, i, playerIndex[fr, i] + x, y];
  //          playerPos[fr, i, playerIndex[fr, i] + x, y] := 0;
  //        end;
  //
  //    selected := fr;
  //    SelectedFrame;
  //    RefreshFrames(imgTemp, factX02, factY02, true);
  //  end;
  end;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    refreshFrame(oper);
  end;
  selected := origFrame;
end;

{-----------------------------------------------------------------------------
 Move frame up
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.MoveUp(Sender: TObject; oper : byte);
var
  i, x, y, fr : byte;
  origFrame : byte;
begin
  origFrame := selected;
//  MoveUpPM(grX, _ANIM_MAX_LINES - 1, player, fld);
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    for i := 0 to 1 do
      for x := 0 to 7 do
        for y := 1 to _ANIM_MAX_LINES - 1 do begin
          fld[selected, i, x, y - 1] := fld[selected, i, x, y];
          fld[selected, i, x, y] := 0;
          playerPos[selected, i, playerIndex[selected, i] + x, y - 1] :=
            playerPos[selected, i, playerIndex[selected, i] + x, y];
          playerPos[selected, i, playerIndex[selected, i] + x, y] := 0;
        end;
  end;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    refreshFrame(oper);
  end;
  selected := origFrame;
end;

{-----------------------------------------------------------------------------
 Move frame down
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.MoveDown(Sender: TObject; oper : byte);
var
  i, x, y, fr : byte;
  origFrame : byte;
begin
  origFrame := selected;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    for i := 0 to 1 do
      for x := 0 to 7 do
        for y := _ANIM_MAX_LINES - 2 downto 0 do begin
          fld[selected, i, x, y + 1] := fld[selected, i, x, y];
          fld[selected, i, x, y] := 0;
          playerPos[selected, i, playerIndex[selected, i] + x, y + 1] :=
            playerPos[selected, i, playerIndex[selected, i] + x, y];
          playerPos[selected, i, playerIndex[selected, i] + x, y] := 0;
        end;
  end;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    refreshFrame(oper);
  end;
  selected := origFrame;
end;

{-----------------------------------------------------------------------------
 Flip player horizontally
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.FlipX(Sender: TObject);
begin
  FlipXProc(0);
end;

procedure TfrmAnimator.FlipXProc(oper : byte);
var
  i, x, y, m, n, fr : byte;
  origFrame : byte;
begin
//  ShowCursor(frmAnimator, frmAnimator, crHourGlass);
//  Screen.Cursor := crHourGlass;
  Screen.BeginWaitCursor;

  origFrame := selected;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    for i := 0 to 1 do
      for y := 0 to _ANIM_MAX_LINES - 1 do
        for x := 7 downto 4 do begin
          n := fld[selected][i, x, y];
          m := playerPos[selected, i, playerIndex[selected, i] + x, y];
          fld[selected][i, x, y] := fld[selected][i, 7 - x, y];
          fld[selected][i, 7 - x, y] := n;
          playerPos[selected, i, playerIndex[selected, i] + x, y] :=
            playerPos[selected, i, playerIndex[selected, i] + 7 - x, y];
          playerPos[selected, i, playerIndex[selected, i] + 7 - x, y] := m;
        end;
  end;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    refreshFrame(oper);
  end;
  selected := origFrame;

//  ShowCursor(frmAnimator, frmAnimator, crDefault);
//  Screen.Cursor := crDefault;
  Screen.EndWaitCursor;
end;

{-----------------------------------------------------------------------------
 Flip player vertically
 -----------------------------------------------------------------------------}
procedure TfrmAnimator.FlipY(Sender: TObject);
begin
  FlipYProc(0);
end;

procedure TfrmAnimator.FlipYProc(oper : byte);
var
  i, x, y, m, n, fr : byte;
  origFrame : byte;
  divFactor : byte;
begin
//  ShowCursor(frmAnimator, frmAnimator, crHourGlass);
//  Screen.Cursor := crHourGlass;
  Screen.BeginWaitCursor;

  origFrame := selected;
  divFactor := animFrameHeight div 2;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    for i := 0 to 1 do
      for x := 0 to 7 do
        for y := animFrameHeight - 1 downto divFactor do begin
          n := fld[selected][i, x, y];
          m := playerPos[selected, i, playerIndex[selected, i] + x, y];
          fld[selected][i, x, y] := fld[selected][i, x, animFrameHeight - 1 - y];
          fld[selected][i, x, animFrameHeight - 1 - y] := n;
          playerPos[selected, i, playerIndex[selected, i] + x, y] :=
            playerPos[selected, i, playerIndex[selected, i] + x, animFrameHeight - 1 - y];
          playerPos[selected, i, playerIndex[selected, i] + x, animFrameHeight - 1 - y] := m;
        end;
  end;
  for fr := 0 to oper do begin
    if oper = 16 then selected := fr;
    refreshFrame(oper);
  end;
  selected := origFrame;

//  ShowCursor(frmAnimator, frmAnimator, crDefault);
//  Screen.Cursor := crDefault;
  Screen.EndWaitCursor;
end;

//procedure TfrmAnimator.DoubleResolutionProc(Sender : TObject);
//begin
////  glDoubleRes.State := cbGrayed;
////  tglSingleRes.State := cbUnchecked;
//  SetDoubleResolution(Sender);
//  ApplyPlayerHeight(Sender);
//end;
//
//procedure TfrmAnimator.SingleResolutionProc(Sender : TObject);
//begin
////  tglDoubleRes.State := cbUnchecked;
////  tglSingleRes.State := cbGrayed;
//  SetSingleResolution(Sender);
//  ApplyPlayerHeight(Sender);
//end;

procedure TfrmAnimator.DoubleResolutionProc(Sender : TObject);
begin
  SetDoubleResolution(Sender);
  ApplyPlayerHeight(Sender);
end;

procedure TfrmAnimator.SingleResolutionProc(Sender : TObject);
begin
  SetSingleResolution(Sender);
  ApplyPlayerHeight(Sender);
end;

procedure TfrmAnimator.SetSingleResolution(Sender: TObject);
begin
  pmResolution := singleResolution;
//  statusBar.Panels[1].Text := 'Player/missile single resolution';
  factY02 := 4;
end;

procedure TfrmAnimator.SetDoubleResolution(Sender: TObject);
begin
  pmResolution := doubleResolution;
//  statusBar.Panels[1].Text := 'Player/missile double resolution';
  factY02 := 8;
end;

//procedure TfrmAnimator.SavePlayer(filenamexy : string);
//var
//  x, y : integer;
//  dta : char;
//  fs : TFileStream;
//begin
//  fs := TFileStream.Create(filenamexy, fmCreate);
//  try
//    for y := 0 to _ANIM_MAX_LINES - 1 do begin
//      for x := 0 to 7 do begin
//        dta := '0';
//        if fld[frame][player, x, y] > 0 then dta := '1';
//        fs.WriteByte(Byte(dta));
//      end;
//    end;
//
//    isDataChanged[player] := false;
//  finally
//    fs.Free;
//  end;
//end;

procedure TfrmAnimator.SavePlayer(Sender: TObject);
begin
  //if isDataChanged[player] and (pmgFilenames[player] = '') then
  //  if pmgFilenames[player] = '' then
  //    SaveAsClick(Sender)
  //else begin
  //  if isDataChanged[player] then
  //    SavePlayer(pmgFilenames[player])
  //  else
  //    ShowMessage('There is no data to be saved!');
  //end;
end;

procedure TfrmAnimator.SavePlayerAs(Sender: TObject);
begin
//  frmMain.dlgSave.Filter := 'Player/missile files (*.spr, *.ply, *.pmg)|*.spr;*.ply;*.pmg|All files (*.*)|*.*';
//  frmMain.dlgSave.Title := 'Save player data as';
////  frmMain.dlgSave.Options := frmMain.dlgSave.Options + [ofOverwritePrompt];
//  frmMain.dlgSave.Filename := '';

//  if frmMain.dlgSave.Execute then begin
//    pmgFilenames[player] := frmMain.dlgSave.Filename;
//    SavePlayer(pmgFilenames[player]);
//    caption := programName + ' ' + programVersion + ' - Player/missile graphics editor (' + pmgFilenames[player] + ')';
//  end;
end;

end.

