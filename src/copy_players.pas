{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Copy Player editor data into Animator editor
}
unit copy_players;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, lcltype;

type
  { TfrmCopyPlayers }
  TfrmCopyPlayers = class(TForm)
    Bevel1 : TBevel;
    btnCopyToFrame1 : TButton;
    btnCopyToFrame2 : TButton;
    btnCopyToFrame3 : TButton;
    btnCopyToFrames1 : TButton;
    btnCopyToFrames2 : TButton;
    btnCopyToFrames3 : TButton;
    btnClose : TButton;
    btnCopyToFrame0 : TButton;
    btnCopyToFrames0 : TButton;
    Button1 : TButton;
    Button2 : TButton;
    boxOptions : TGroupBox;
    imgPlayer0dr : TImage;
    imgPlayer1dr : TImage;
    imgPlayer2dr : TImage;
    imgPlayer3dr : TImage;
    imgPlayer0sr : TImage;
    imgPlayer1sr : TImage;
    imgPlayer2sr : TImage;
    imgPlayer3sr : TImage;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    rgFrameLayers : TRadioGroup;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure CloseProc(Sender : TObject);
    procedure CopyToFrameProc(Sender : TObject);
    procedure CopyToFramesProc(Sender : TObject);
    procedure Button1Click(Sender : TObject);
  private
    procedure ReadData(player : byte; pm : TImage; factX, factY : byte);
  end;

var
  frmCopyPlayers : TfrmCopyPlayers;

implementation

{$R *.lfm}

uses
  Common, animator, pmg, lib;

{ TfrmCopyPlayers }

procedure TfrmCopyPlayers.FormCreate(Sender : TObject);
begin
  imgPlayer0dr.Canvas.Brush.Color := colTab[0];
  imgPlayer0sr.Canvas.Brush.Color := colTab[0];
  imgPlayer1dr.Canvas.Brush.Color := colTab[0];
  imgPlayer1sr.Canvas.Brush.Color := colTab[0];
  imgPlayer2dr.Canvas.Brush.Color := colTab[0];
  imgPlayer2sr.Canvas.Brush.Color := colTab[0];
  imgPlayer3dr.Canvas.Brush.Color := colTab[0];
  imgPlayer3sr.Canvas.Brush.Color := colTab[0];
end;

procedure TfrmCopyPlayers.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmCopyPlayers.Button1Click(Sender : TObject);
//var
//  d, h, m, k : byte;
//  f0, f1 : byte;
//  isFound0 : boolean;
//  isFound1 : boolean;
//  isFound2 : boolean;
//  isFound3 : boolean;
begin
  //case rgFrameLayers.ItemIndex of
  //  0: begin f0 := 0; f1 := 0; end;
  //  1: begin f0 := 1; f1 := 1; end;
  //  2: begin f0 := 0; f1 := 1; end;
  //end;
  //
  //isFound0 := false;
  //for h := 0 to 7 do
  //  for m := 0 to _ANIM_MAX_LINES - 1 do
  //    if frmPmg.fld[k][0, h, m] = 1 then begin
  //      isFound0 := true;
  //      break;
  //    end;
  //
  //isFound1 := false;
  //for h := 0 to 7 do
  //  for m := 0 to _ANIM_MAX_LINES - 1 do
  //    if frmPmg.fld[k][1, h, m] = 1 then begin
  //      isFound1 := true;
  //      break;
  //    end;
  //
  //isFound2 := false;
  //for h := 0 to 7 do
  //  for m := 0 to _ANIM_MAX_LINES - 1 do
  //    if frmPmg.fld[k][2, h, m] = 1 then begin
  //      isFound2 := true;
  //      break;
  //    end;
  //
  //isFound3 := false;
  //for h := 0 to 7 do
  //  for m := 0 to _ANIM_MAX_LINES - 1 do
  //    if frmPmg.fld[k][3, h, m] = 1 then begin
  //      isFound3 := true;
  //      break;
  //    end;
  //
  //for k := 0 to 16 do
  //  for d := f0 to f1 do
  //    for h := 0 to 7 do
  //      for m := 0 to _ANIM_MAX_LINES - 1 do
  //        frmAnimator.fld[k][d, h, m] :=
  //          frmPmg.fld[(Sender as TButton).Tag, h, m];
  //
  //for k := 0 to 16 do
  //  for d := f0 to f1 do
  //    for h := 0 to _MAX_ANIM_FRAME_WIDTH - 1 do
  //      for m := 0 to _ANIM_MAX_LINES - 1 do
  //        frmAnimator.playerPos[k, d, h, m] :=
  //          frmPmg.fld[(Sender as TButton).Tag, h, m];
  //
  //for k := 0 to 16 do begin
  //  frmAnimator.selected := k;
  //  frmAnimator.RefreshFrame(0);
  //end;
  //
  //Close;
end;

procedure TfrmCopyPlayers.FormShow(Sender : TObject);
begin
  ReadData(0, imgPlayer0dr, 6, 6);
  ReadData(0, imgPlayer0sr, 6, 3);
  ReadData(1, imgPlayer1dr, 6, 6);
  ReadData(1, imgPlayer1sr, 6, 3);
  ReadData(2, imgPlayer2dr, 6, 6);
  ReadData(2, imgPlayer2sr, 6, 3);
  ReadData(3, imgPlayer3dr, 6, 6);
  ReadData(3, imgPlayer3sr, 6, 3);
end;

procedure TfrmCopyPlayers.CloseProc(Sender : TObject);
begin
  Close;
end;

procedure TfrmCopyPlayers.CopyToFrameProc(Sender : TObject);
var
  d, h, m : byte;
  f0, f1 : byte;
begin
  case rgFrameLayers.ItemIndex of
    0: begin f0 := 0; f1 := 0; end;
    1: begin f0 := 1; f1 := 1; end;
    2: begin f0 := 0; f1 := 1; end;
  end;

  for d := f0 to f1 do
    for h := 0 to 7 do
      for m := 0 to _ANIM_MAX_LINES - 1 do
        frmAnimator.fld[frmAnimator.selected][d, h, m] :=
          frmPmg.fld[(Sender as TButton).Tag, h, m];

  for d := f0 to f1 do
    for h := 0 to _MAX_ANIM_FRAME_WIDTH - 1 do
      for m := 0 to _ANIM_MAX_LINES - 1 do
        frmAnimator.playerPos[frmAnimator.selected, d, h, m] :=
          frmPmg.fld[(Sender as TButton).Tag, h, m];

  frmAnimator.RefreshFrame(0);
  Close;
end;

procedure TfrmCopyPlayers.CopyToFramesProc(Sender : TObject);
var
  d, h, m, k : byte;
  f0, f1 : byte;
begin
  case rgFrameLayers.ItemIndex of
    0: begin f0 := 0; f1 := 0; end;
    1: begin f0 := 1; f1 := 1; end;
    2: begin f0 := 0; f1 := 1; end;
  end;

  for k := 0 to 16 do
    for d := f0 to f1 do
      for h := 0 to 7 do
        for m := 0 to _ANIM_MAX_LINES - 1 do
          frmAnimator.fld[k][d, h, m] :=
            frmPmg.fld[(Sender as TButton).Tag, h, m];

  for k := 0 to 16 do
    for d := f0 to f1 do
      for h := 0 to _MAX_ANIM_FRAME_WIDTH - 1 do
        for m := 0 to _ANIM_MAX_LINES - 1 do
          frmAnimator.playerPos[k, d, h, m] :=
            frmPmg.fld[(Sender as TButton).Tag, h, m];

  for k := 0 to 16 do begin
    frmAnimator.selected := k;
    frmAnimator.RefreshFrame(0);
  end;

  Close;
end;

procedure TfrmCopyPlayers.ReadData(player : byte; pm : TImage; factX, factY : byte);
var
  col : byte;
  xf, yf : integer;
  ci : byte;
  maxX : byte;
  isData : boolean = false;
begin
  FillRectEx(pm, coltab[0], 0, 0, pm.Width, pm.Height);
  ci := 0;
  maxX := _PM_WIDTH;
  for yf := 0 to _PM_MAX_LINES - 1 do
    for xf := 0 to maxX do begin
      col := frmPmg.fld[player, xf, yf];
      if col = 1 then begin
        pm.Canvas.Brush.Color := coltab[ci + 4];
        isData := true;
      end
      else //if col <> 1 then
        pm.Canvas.Brush.Color := colTab[0];

      pm.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY))
    end;

  if not isData then begin
    case player of
      0: begin
        btnCopyToFrame0.Enabled := false;
        btnCopyToFrames0.Enabled := false;
      end;
      1: begin
        btnCopyToFrame1.Enabled := false;
        btnCopyToFrames1.Enabled := false;
      end;
      2: begin
        btnCopyToFrame2.Enabled := false;
        btnCopyToFrames2.Enabled := false;
      end;
      3: begin
        btnCopyToFrame3.Enabled := false;
        btnCopyToFrames3.Enabled := false;
      end;
    end;
  end;
end;

end.

