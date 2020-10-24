{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2019
  Unit:
}
unit pm_apl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, lcltype;

type
  { TfrmPmApl }
  TfrmPmApl = class(TForm)
    btnClose: TButton;
    btnLoad: TButton;
    chkFrames: TCheckGroup;
    radLayer1: TRadioButton;
    rad2Players: TRadioButton;
    radLayer0: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure LoadAplFile(Sender: TObject);
    procedure radLayer0Change(Sender: TObject);
    procedure rad2PlayersChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CloseWin(Sender: TObject);
  private

  public

  end;

var
  frmPmApl: TfrmPmApl;

implementation

{$R *.lfm}

uses
  main, pmg, lib, common;

{ TfrmPmApl }

procedure TfrmPmApl.FormShow(Sender: TObject);
begin
  rad2PlayersChange(Sender);
end;

procedure TfrmPmApl.LoadAplFile(Sender: TObject);
var
  filenamex : string;
  fs : TFileStream;
  d, h, m, n : byte;
  frameIndex : array[0..3] of byte;
  count : byte = 0;

procedure FillPlayers(flag, frame01, frame02, frame03, frame04, pl : byte);
var
  dta : char;
  temp : string;
  x, i, j : byte;
begin
  for i := 0 to 16 do begin
    // Player data, frame i (48 bytes)
    // 17 - Player data, copy buffer (48 bytes)
    for j := 0 to 47 do begin
      d := fs.ReadByte;
      temp := dec2bin(d);

      if flag = 0 then begin
        if (i = frame01) and (j <= _PM_MAX_LINES - 1) then
          for x := 0 to 7 do begin
            dta := temp[x + 1];
            frmPmg.fld[pl, x, j] := StrToInt(dta);
            frmPmg.playerPos[pl, frmPmg.playerIndex[pl] + x, j] := frmPmg.fld[pl, x, j];
          end;

        if (i = frame02) and (j <= _PM_MAX_LINES - 1) then
          for x := 0 to 7 do begin
            dta := temp[x + 1];
            frmPmg.fld[pl + 2, x, j] := StrToInt(dta);
            frmPmg.playerPos[pl + 2, frmPmg.playerIndex[pl + 2] + x, j] := frmPmg.fld[pl + 2, x, j];
          end;

      end
      else begin
        //for k := 0 to 3 do begin
        //  if (i = offset + k) and (j <= _PM_MAX_LINES - 1) then begin
        //    for x := 0 to 7 do begin
        //      dta := temp[x + 1];
        //      frmPmg.fld[offset + k, x, j] := StrToInt(dta);
        //      frmPmg.playerPos[offset + k, frmPmg.playerIndex[offset + k] + x, j] :=
        //        frmPmg.fld[offset + k, x, j];
        //    end;
        //  end;
        //end;

        if (i = frame01) and (j <= _PM_MAX_LINES - 1) then
          for x := 0 to 7 do begin
            dta := temp[x + 1];
            frmPmg.fld[0, x, j] := StrToInt(dta);
            frmPmg.playerPos[0, frmPmg.playerIndex[0] + x, j] := frmPmg.fld[0, x, j];
          end;

        if (i = frame02) and (j <= _PM_MAX_LINES - 1) then
          for x := 0 to 7 do begin
            dta := temp[x + 1];
            frmPmg.fld[1, x, j] := StrToInt(dta);
            frmPmg.playerPos[1, frmPmg.playerIndex[1] + x, j] := frmPmg.fld[1, x, j];
          end;

        if (i = frame03) and (j <= _PM_MAX_LINES - 1) then
          for x := 0 to 7 do begin
            dta := temp[x + 1];
            frmPmg.fld[2, x, j] := StrToInt(dta);
            frmPmg.playerPos[2, frmPmg.playerIndex[2] + x, j] := frmPmg.fld[2, x, j];
          end;

        if (i = frame04) and (j <= _PM_MAX_LINES - 1) then
          for x := 0 to 7 do begin
            dta := temp[x + 1];
            frmPmg.fld[3, x, j] := StrToInt(dta);
            frmPmg.playerPos[3, frmPmg.playerIndex[3] + x, j] := frmPmg.fld[3, x, j];
          end;
      end;
    end;
  end;
end;

begin
  FillByte(frameIndex, SizeOf(frameIndex), 0);

  with frmMain.dlgOpen do begin
    Title := 'Open Atari Player data file';
    Filter := 'Atari Player data files (*.apl)|*.apl|All files (*.*)|*.*';
    if Execute then begin
      filenamex := frmMain.dlgOpen.Filename;
      fs := TFileStream.Create(Filenamex, fmOpenRead);
      try
        frmPmg.SetPlayers;

        // file version identifier
        for h := 0 to 3 do
          d := fs.ReadByte;

        // number of frames, height, gap
        for h := 0 to 2 do
          d := fs.ReadByte;

        // P0 colour, frames 1..16
        d := fs.ReadByte;
        coltab[4] := colorMem[d div 2];

        for h := 1 to 15 do
          d := fs.ReadByte;

        // P0 colour, copy buffer
        d := fs.ReadByte;

        // P1 colour, frames 1..16
        d := fs.ReadByte;
        coltab[5] := colorMem[d div 2];

        for h := 1 to 15 do
          d := fs.ReadByte;

        // P1 colour, copy buffer
        d := fs.ReadByte;

        // background colour
        d := fs.ReadByte;

        if rad2Players.Checked then begin
          for m := 0 to 16 do
            if chkFrames.Checked[m] and (count < 4) then
              for n := 0 to 1 do
                if frameIndex[n] = 0 then begin
                  frameIndex[n] := m;
                  Inc(count);
                  break;
                end;

          frmPmg.playerIndex[0] := 7;
          frmPmg.playerIndex[1] := 7;
          frmPmg.playerIndex[2] := 20;
          frmPmg.playerIndex[3] := 20;
          frmPmg.tbPlayer0.Position := frmPmg.playerIndex[0];
          frmPmg.tbPlayer1.Position := frmPmg.playerIndex[1];
          frmPmg.tbPlayer2.Position := frmPmg.playerIndex[2];
          frmPmg.tbPlayer3.Position := frmPmg.playerIndex[3];

          FillPlayers(0, frameIndex[0], frameIndex[1], 0, 0, 0);
          FillPlayers(0, frameIndex[0], frameIndex[1], 0, 0, 1);
        end
        else begin
          //flag := false;

          for m := 0 to 16 do
            if chkFrames.Checked[m] and (count < 4) then
              for n := 0 to 3 do
                if frameIndex[n] = 0 then begin
                  frameIndex[n] := m;
                  Inc(count);
                  break;
                end;

          frmPmg.playerIndex[0] := 0;
          frmPmg.playerIndex[1] := 8;
          frmPmg.playerIndex[2] := 16;
          frmPmg.playerIndex[3] := 24;
          frmPmg.tbPlayer0.Position := frmPmg.playerIndex[0];
          frmPmg.tbPlayer1.Position := frmPmg.playerIndex[1];
          frmPmg.tbPlayer2.Position := frmPmg.playerIndex[2];
          frmPmg.tbPlayer3.Position := frmPmg.playerIndex[3];

          FillPlayers(1, frameIndex[0], frameIndex[1], frameIndex[2], frameIndex[3], 0);

          if radLayer1.Checked then
            FillPlayers(1, frameIndex[0], frameIndex[1], frameIndex[2], frameIndex[3], 0);
        end;

        // Selected frame, pen colour, animation rate
        for h := 0 to 2 do begin
          d := fs.ReadByte;
        end;
      finally
        fs.Free;
//        frmPmg.RefreshMultiValues;
        frmPmApl.Close;
      //except
      //  on E: EFOpenError do
      //    writeln('File handling error occurred. Details: ', E.Message);
      end;
    end;
  end;
end;

procedure TfrmPmApl.rad2PlayersChange(Sender: TObject);
var
  i : byte;
begin
//  chkFrames.Enabled := false;

  for i := 0 to 16 do begin
    if i < 2 then
      chkFrames.Checked[i] := true
    else
      chkFrames.Checked[i] := false;
  end;
end;

procedure TfrmPmApl.radLayer0Change(Sender: TObject);
var
  i : byte;
begin
//  chkFrames.Enabled := true;

  for i := 0 to 16 do begin
    if i < 4 then
      chkFrames.Checked[i] := true
    else
      chkFrames.Checked[i] := false;
  end;
end;

procedure TfrmPmApl.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmPmApl.CloseWin(Sender: TObject);
begin
  Close;
end;

end.

