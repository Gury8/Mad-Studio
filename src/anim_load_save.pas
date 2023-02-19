{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Player Animator editor and player - Load frame animation data
}
unit anim_load_save;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, lcltype, BCMaterialDesignButton;

type
  { TfrmAnimLoadSave }
  TfrmAnimLoadSave = class(TForm)
    btnClose : TBCMaterialDesignButton;
    btnCopyToEditor : TBCMaterialDesignButton;
    chkFrames: TCheckGroup;
    radLayer2Data: TRadioButton;
    radAllData: TRadioButton;
    radLayer1Data: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CloseProc(Sender: TObject);
    procedure LoadDataProc(Sender: TObject);
    procedure AllDataChange(Sender: TObject);
    procedure FramesItemProc(Sender: TObject; Index: integer);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
  end;

var
  frmAnimLoadSave: TfrmAnimLoadSave;

implementation

{$R *.lfm}

uses
  main, animator, lib, common;

{ TfrmAnimLoadSave }

procedure TfrmAnimLoadSave.FormCreate(Sender: TObject);
var
  i : byte;
begin
  for i := 0 to 16 do
    chkFrames.Checked[i] := true;
end;

procedure TfrmAnimLoadSave.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmAnimLoadSave.LoadDataProc(Sender: TObject);
var
  filenamex : string;
  fs : TFileStream;
  d, h, m, k : byte;

procedure FillPlayers(flag, pl : byte);
var
  temp : string;
  x, i, j : byte;
  height : byte;
begin
  //if aplAnim = fixed52 then
  //  height := 52
  //else begin
  //  if animFrameHeight <= _ANIM_APL_MAX_LINES then
  //    height := _ANIM_APL_MAX_LINES
  //  else begin
  //    if aplAnim = extended then
  //      height := animFrameHeight
  //  end;
  //end;

  case aplAnim of
    normal:
      height := _ANIM_APL_MAX_LINES;
    extended:
      height := animFrameHeight;
    fixed52: begin
      height := 52;
      animFrameHeight := height;
    end;
  end;

  if (flag = 1) and (pl = 1) then begin
    for i := 0 to 16 do
      for j := 0 to height - 1 do
        if fs.Position < fs.Size then
          d := fs.ReadByte;
  end;

  for i := 0 to 16 do begin
    // Player data, frame i
    // 17 - Player data, copy buffer
    for j := 0 to height - 1 do begin
      if fs.Position < fs.Size then begin
        d := fs.ReadByte;
        temp := dec2bin(d);
        //if flag = 0 then begin
        //  if (j <= animFrameHeight - 1) then
            for x := 0 to 7 do begin
              frmAnimator.fld[i, pl, x, j] := StrToInt(temp[x + 1]);
              frmAnimator.playerPos[i, pl, frmAnimator.playerIndex[i, pl] + x, j] :=
                frmAnimator.fld[i, pl, x, j];
            end;
        //end
        //else begin
        //  if (j <= animFrameHeight - 1) then
        //    for x := 0 to 7 do begin
        //      frmAnimator.fld[i, pl, x, j] := StrToInt(temp[x + 1]);
        //      frmAnimator.playerPos[i, pl, frmAnimator.playerIndex[i, pl] + x, j] :=
        //        frmAnimator.fld[i, pl, x, j];
        //    end;
        //end;
      end;
    end;
  end;
end;

begin
  with frmMain.dlgOpen do begin
    Title := 'Open Atari Player Editor data file';
    case aplAnim of
      normal: begin
  //      animFrameHeight := _ANIM_APL_MAX_LINES;
        Filter := 'Atari Player Editor data file (*.apl)|*.apl;|All files (*.*)|*.*';
      end;
      extended: begin
        Filter := 'Atari Player Editor data file (*.apm)|*.apm;|All files (*.*)|*.*';
      end;
      fixed52: begin
//        animFrameHeight := 52;
        Filter := 'Atari Player Editor data file (*.apl, *.apm)|*.apl;*.apm|All files (*.*)|*.*';
      end;
    end;

    if Execute then begin
      Screen.Cursor := crHourGlass;
      filenamex := frmMain.dlgOpen.Filename;
      fs := TFileStream.Create(Filenamex, fmOpenRead);
      try
        for k := 0 to 16 do
          for d := 0 to 1 do
            for h := 0 to 7 do
              for m := 0 to _ANIM_MAX_LINES - 1 do
                frmAnimator.fld[k][d, h, m] := 0;

        for k := 0 to 16 do
          for d := 0 to 1 do
            for h := 0 to _MAX_ANIM_FRAME_WIDTH - 1 do
              for m := 0 to _ANIM_MAX_LINES - 1 do
                frmAnimator.playerPos[k, d, h, m] := 0;

        // File version identifier
        for h := 0 to 3 do
          d := fs.ReadByte;

        // Number of frames
        d := fs.ReadByte;
        animFrames := d;

        // Frame height
        d := fs.ReadByte;
        animFrameHeight := d;

        // Frame gap
        d := fs.ReadByte;

        // P0 colour, frames 1..16
        d := fs.ReadByte;
        coltab[4] := colorMem[d div 2];
        colorValues[4] := d;
//        frmAnimator.color01 := d;

        for h := 2 to 16 do
          if fs.Position < fs.Size then
            d := fs.ReadByte;

        // P0 colour, copy buffer
        d := fs.ReadByte;

        // P1 colour, frames 1..16
        d := fs.ReadByte;
        coltab[5] := colorMem[d div 2];
        colorValues[5] := d;
//        frmAnimator.color02 := d;

        for h := 2 to 16 do
          if fs.Position < fs.Size then
            d := fs.ReadByte;

        // P1 colour, copy buffer
        d := fs.ReadByte;

        // background colour
        d := fs.ReadByte;
        coltab[0] := colorMem[d div 2];

        if radAllData.Checked then begin
          //for m := 0 to 16 do begin
          //  if loadType[m] = 1 then begin
          //    frame01 := m;
          //    break;
          //  end;
          //end;
          //
          //for m := frame01 + 1 to 16 do begin
          //  if loadType[m] = 1 then begin
          //    frame02 := m;
          //    break;
          //  end;
          //end;

          //d := fs.ReadByte;
//          d := fs.ReadByte;

          FillPlayers(0, 0);
          FillPlayers(0, 1);
        end
        else begin
          //flag := false;
          if radLayer1Data.Checked then
            FillPlayers(1, 0)
          else
            FillPlayers(1, 1);
        end;

        // Selected frame
        d := fs.ReadByte;
        frmAnimator.selFrame := d;

        // Selected pen color
        d := fs.ReadByte;
//        frmAnimator.selPen := d;

        // Selected animation frame rate
        d := fs.ReadByte;
        frmAnimator.selAnimFrameRate := d;

        frmAnimator.filename := filenamex;
        frmAnimator.caption := programName + ' ' + programVersion +
                               ' - Player Animator (' + filenamex + ')';
      finally
        Screen.Cursor := crDefault;
        fs.Free;
        frmAnimLoadSave.Close;
      //except
      //  on E: EFOpenError do
      //    writeln('File handling error occurred. Details: ', E.Message);
      end;
    end;
  end;
end;

procedure TfrmAnimLoadSave.AllDataChange(Sender: TObject);
begin
//  chkFrames.Enabled := false;

  //for i := 0 to 16 do begin
  //  chkFrames.Checked[i] := false;
  //end;
  //for i := 0 to 16 do begin
  //  chkFrames.Checked[i] := true;
  //end;
end;

procedure TfrmAnimLoadSave.FramesItemProc(Sender: TObject; Index: integer);
begin
//  for i := 0 to 16 do begin
//    if chkFrames.Checked[i] then
//      loadType[i] := 1
//    else begin
//      loadType[i] := 0
//    end;
////    loadType[i] := chkFrames.Checked[i];
//  end;
end;

procedure TfrmAnimLoadSave.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmAnimLoadSave.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

procedure TfrmAnimLoadSave.CloseProc(Sender: TObject);
begin
  Close;
end;

end.

