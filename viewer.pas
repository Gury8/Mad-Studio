{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: The Viewer - Mad Studio files viewer
}
unit viewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, ComCtrls,
  EditBtn, StdCtrls, ExtCtrls;

type
  { TfrmViewer }
  TfrmViewer = class(TForm)
    cmbFormats : TComboBox;
    imgEditor : TImage;
    label01 : TLabel;
    label02 : TLabel;
    ShellListView : TShellListView;
    ShellTreeView : TShellTreeView;
    StatusBar : TStatusBar;
    procedure cmbFormatsChange(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure ShellListViewClick(Sender : TObject);
    procedure ShellListViewKeyUp(Sender : TObject; var Key : Word; Shift : TShiftState);
  private
    fld : array[0..319, 0..191] of byte;
    is01bit : boolean;
    grBytes : word;
    procedure GrModeSettings;
    procedure SetGrMode;
    procedure DrawImage;
  public
  end;

var
  frmViewer : TfrmViewer;

implementation

{$R *.lfm}

uses
  common;

{ TfrmViewer }

procedure TfrmViewer.FormCreate(Sender : TObject);
begin
  ShellTreeView.Root := getDir + 'examples\';
  ShellListView.Root := getDir + 'examples\';
end;

procedure TfrmViewer.cmbFormatsChange(Sender : TObject);
begin
  case cmbFormats.ItemIndex of
    0: ShellListView.Mask := '*.*';
    1: ShellListView.Mask := '*.gr7';
    2: ShellListView.Mask := '*.gr8';
    3: ShellListView.Mask := '*.gr3';
  end;
end;

procedure TfrmViewer.GrModeSettings;
begin
  is01bit := false;
  case grMode of
    grMode40x24x4: begin
      grX := 39; grY := 23; factX := 8; factY := 8;
      grBytes := 240;
    end;
    grMode80x48x2: begin
      is01bit := true;
      grX := 79; grY := 47; factX := 12; factY := 12;
      grBytes := 480;
    end;
    grMode80x48x4: begin
      grX := 79; grY := 47; factX := 12; factY := 12;
      grBytes := 960;
    end;
    grMode160x96x2: begin
      is01bit := true;
      grX := 159; grY := 95; factX := 2; factY := 2;
      grBytes := 3840 div 2;
    end;
    grMode160x96x4: begin
      grX := 159; grY := 95; factX := 2; factY := 2;
      grBytes := 3840;
    end;
    grMode160x192x2: begin
      is01bit := true;
      grX := 159; grY := 191; factX := 6; factY := 3;
      grBytes := 3840;
    end;
    grMode160x192x4: begin
      grX := 159; grY := 191; factX := 6; factY := 3;
      grBytes := 7680;
    end;
    grMode320x192x2: begin
      is01bit := true;
      grX := 319; grY := 191; factX := 1; factY := 1;
      grBytes := 7680;
    end;
  end;

  //case grMode of
  //  grMode40x24x4  : sbGr.Panels[2].Text := 'Graphics mode 40 x 24 in 4 colors';
  //  grMode80x48x2  : sbGr.Panels[2].Text := 'Graphics mode 80 x 48 in 2 colors';
  //  grMode80x48x4  : sbGr.Panels[2].Text := 'Graphics mode 80 x 48 in 4 colors';
  //  grMode160x96x2 : sbGr.Panels[2].Text := 'Graphics mode 160 x 96 in 2 colors';
  //  grMode160x96x4 : sbGr.Panels[2].Text := 'Graphics mode 160 x 96 in 4 colors';
  //  grMode160x192x2: sbGr.Panels[2].Text := 'Graphics mode 160 x 192 in 2 colors';
  //  grMode160x192x4: sbGr.Panels[2].Text := 'Graphics mode 160 x 192 in 4 colors';
  //  grMode320x192x2: sbGr.Panels[2].Text := 'Graphics mode 320 x 192 in 2 colors';
  //end;
end;

procedure TfrmViewer.SetGrMode;
var
  a, b : integer;
begin
  for a := 0 to grX do
    for b := 0 to grY do begin
      imgEditor.Canvas.Pixels[a*factX, b*factY] := clWhite;
      fld[a, b] := 0;
    end;

//  btnNormalOld.Down:=false;
//  func := [fNormal];

  //btnNormal.Down:=true;

  //if grX = 319 then
  //  frmColors.SelColor := 9
  //else begin
  //  frmColors.SelColor := 1;
  //end;

//  ColorRegisters;
//  Color;
end;

procedure TfrmViewer.ShellListViewClick(Sender : TObject);
var
  x, y : integer;
  dta : byte;
  fs : TFileStream;
  tempCalcX : word;
begin
  label01.Caption := ShellListView.Selected.Caption;
  label02.Caption := ShellListView.Root;

  case cmbFormats.ItemIndex of
    1: grMode := grMode160x96x4;
    2: grMode := grMode320x192x2;
    3: grMode := grMode40x24x4;
  end;

  GrModeSettings;
  SetGrMode;

  fs := TFileStream.Create(ShellListView.Root + ShellListView.Selected.Caption, fmOpenReadWrite);
  try
    for y := 0 to grY do begin
      // 4-color graphics modes
      //// Resolution less than 320x192
      if not is01bit then begin
         //(grMode = grMode40x24x4) or (grMode = grMode80x48x4) or (grMode = grMode160x96x4)
         //or (grMode = grMode160x192x4) then begin
//        if grX < 319 then begin
        tempCalcX := grX div 4;
        for x := 0 to tempCalcX do begin
          dta := fs.ReadByte;
          fld[x shl 2, y] := dta div 64;
          fld[x shl 2 + 1, y] := (dta mod 64) shr 4;  //div 16;
          fld[x shl 2 + 2, y] := (dta mod 16) shr 2;  //div 4;
          fld[x shl 2 + 3, y] := dta mod 4;
        end;
      end
      // 2-color graphics modes
      else begin
//          if grX = 319 then begin
        tempCalcX := grX shr 3;  // div 8;
        for x := 0 to tempCalcX do begin
          dta := fs.ReadByte;
          fld[x shl 3, y] := dta div 128;
          fld[x shl 3 + 1, y] := (dta mod 128) div 64;
          fld[x shl 3 + 2, y] := (dta mod 64) div 32;
          fld[x shl 3 + 3, y] := (dta mod 32) div 16;
          fld[x shl 3 + 4, y] := (dta mod 16) div 8;
          fld[x shl 3 + 5, y] := (dta mod 8) div 4;
          fld[x shl 3 + 6, y] := (dta mod 4) div 2;
          fld[x shl 3 + 7, y] := (dta mod 2);
        end;
      end;
    end;
    if ((grMode = grMode40x24x4) and (fs.Size = 244))
       or ((grMode = grMode80x48x4) and (fs.Size = 964))
       or ((grMode = grMode160x96x4) and (fs.Size = 3844))
       or ((grMode = grMode160x192x4) and (fs.Size = 7684))
    then begin
      for x := 0 to 3 do begin
        dta := fs.ReadByte;
        coltab[x] := colorMem[dta div 2];
        colorValues[x] := dta;
      end;
    end
    else begin
      if ((grMode = grMode80x48x2) and (fs.Size = 482))
         or ((grMode = grMode160x96x2) and (fs.Size = 1922))
         or ((grMode = grMode160x192x2) and (fs.Size = 3842))
      then begin
        for x := 0 to 1 do begin
          dta := fs.ReadByte;
          coltab[x] := colorMem[dta div 2];
          colorValues[x] := dta;
        end;
      end;
    end;
//    ColorRegisters;
//    frmColors.Show;
//    beep;
//    caption := programName + ' ' + programVersion +
//               ' - Graphics editor (' + filename + ')';
  finally
    fs.Free;
    DrawImage;
  end;
end;

procedure TfrmViewer.ShellListViewKeyUp(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  ShellListViewClick(Sender);
end;

//procedure TfrmViewer.FileListDblClick(Sender : TObject);
//begin
//  //if FileList.Selected.ImageIndex = 1 then
//  //  ShowDirectory(AddBackSlash(AddBackSlash(CurrentDirectory)+FileList.Selected.Caption))
//  //else
//  //  ShellExecute(Form1.Handle, PChar('open'),
//  //       PChar(AddBackSlash(CurrentDirectory)+FileList.Selected.Caption),
//  //       PChar(''), PChar(''), 1);
//end;
//
//procedure TfrmViewer.edtPathChange(Sender : TObject);
//begin
//  ShowDirectory(edtPath.Directory+'\');
//end;
//
//function TfrmViewer.AddBackSlash(path:string):string;
//begin
//  //if RightStr(path, 1) <> sep then
//  //  Result:=path+sep
//  //else
//  //  Result:=path;
//end;
//
//procedure TfrmViewer.ShowDirectory(ListDir:string);
//var
//  Info: TSearchRec;
//  Count: Longint;
//  li: TListItem;
//  FileTime: TDateTime;
//
//begin
//
//  ListDir:=AddBackSlash(ListDir);
//  edtPath.Text:=ListDir;
////  CurrentDirectory:=ListDir;
//  Count:=0;
//  FileList.Items.Clear;
//
//  FileList.Items.BeginUpdate;
//
//  // This is a try block which finally runs EndUpdate
//  try
//
//    // if we have found a file...
//    If FindFirst (ListDir+'*',faAnyFile and faDirectory,Info)=0 then
//    begin
//
//      repeat
//
//        // we increase the count so that we can show it later
//        Inc(Count);
//
//        // we do stuff with the file entry we found
//        with Info do
//        begin
//
//          if (Name <> '.') and (Name <> '..') then begin
//
//            li := FileList.Items.Add;
//            li.Caption:=Info.Name;
//
//            // If we find a folder
//            if (Attr and faDirectory) <> 0 then begin
//                li.ImageIndex:=1;
//                li.SubItems.Add('--'); // folders can't show size
//            end
//            // if we find a file
//            else begin
//                li.ImageIndex:=2;
//                li.SubItems.Add(FileSizeFormat(Info.Size));
//            end;
//
//            FileTime := FileDateToDateTime(Info.Time);
//            li.SubItems.Add(FormatDateTime('dd mmm yyyy hh:nn am/pm',FileTime));
//            // for help with the format:
//            // http://lazarus-ccr.sourceforge.net/docs/rtl/sysutils/formatchars.html
//
//          end;
//
//        end;
//
//      until FindNext(info)<>0;
//
//    end;
//
//  finally
//    FileList.Items.EndUpdate;
//  end;
//
//  // we are done with file list
//  FindClose(Info);
//  StatusBar.SimpleText:=inttostr(Count)+' items';
//end;
//
//function TfrmViewer.FileSizeFormat(bytes:Double):string;
//begin
//
//  // below 1024 bytes
//
//  if bytes < 1024 then
//
//    Result:=floattostr(bytes) + 'b'
//
//  // kilobytes; 1kb = 1024bytes
//
//  else if (bytes >= 1024) and (bytes < 1024*1024) then
//
//    Result:=FormatFloat('0.00', bytes/1024)+'kb'
//
//  // megabytes; 1mb = 1024*1024bytes
//
//  else if (bytes >= 1024*1024) and (bytes < 1024*1024*1024) then
//
//    Result:=FormatFloat('0.00', bytes/1024/1024)+'mb'
//
//  // gigabytes; 1gb = 1024*1024*1024bytes
//
//  else if (bytes >= 1024*1024*1024) and (bytes < 1024*1024*1024*1024) then
//
//    Result:=FormatFloat('0.00', bytes/1024/1024/1024)+'gb'
//
//  // for everything above, we show it in terrabytes
//
//  else
//
//    Result:=FormatFloat('0.00', bytes/1024/1024/1024/1024)+'tb';
//
//end;

procedure TfrmViewer.DrawImage;
var
  col, xf, yf : integer;
begin
  //imgEditor.Canvas.Brush.Color := clWhite;
  //imgEditor.Canvas.Brush.Style := bsSolid;
  //imgEditor.Canvas.FillRect(bounds(0, 0, imgEditor.Width, imgEditor.Height));

  for xf := 0 to grX do
    for yf := 0 to grY do begin
      col := fld[xf, yf];
      if (col = 1) and (grX = 319) then col := 9;
      imgEditor.Canvas.Brush.Color := coltab[col];
      imgEditor.Canvas.FillRect(bounds(xf*factX, yf*factY, factX, factY));
//      imgEditor.Canvas.Pixels[xf*factX, yf*factY] := coltab[3];
    end;

//  imgEditor.Canvas.Brush.Style := bsClear;
//  imgEditor.Canvas.Pen.Color := clRed;
//  imgEditor.canvas.rectangle(0, 0, (size*fact-1), grY*fact - 1);

  imgEditor.Refresh;
end;

end.

