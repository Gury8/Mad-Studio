{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Byte viewer
}
unit byte_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Grids, strutils, lcltype, Spin;

type
  { TfrmByteEditor }
  TfrmByteEditor = class(TForm)
    btnClose : TButton;
    lblFilesize : TLabel;
    btnLoadData : TButton;
    btnExportData : TButton;
    chkHex : TCheckBox;
    chkRowSelect : TCheckBox;
    Label1 : TLabel;
    Label2 : TLabel;
    lblFilePath : TLabel;
    paintBox : TPaintBox;
    scrlBox : TScrollBox;
    editMaxLines : TSpinEdit;
    strGrid : TStringGrid;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormActivate(Sender : TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure btnCloseClick(Sender : TObject);
    procedure btnExportDataClick(Sender : TObject);
    procedure btnLoadDataClick(Sender : TObject);
    procedure chkHexChange(Sender : TObject);
    procedure chkRowSelectChange(Sender : TObject);
    procedure paintBoxPaint(Sender : TObject);
    procedure strGridHeaderClick(Sender : TObject; IsColumn : Boolean; Index : Integer);
    procedure strGridKeyUp(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure strGridMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer);
  private
    fs : TFileStream;
    isDataReader : boolean;
    factX02, factY02 : byte;
    procedure SetDecHex(isHex : boolean);
  public

  end;

var
  frmByteEditor : TfrmByteEditor;

implementation

{$R *.lfm}

uses
  main, lib, common;

{ TfrmByteEditor }

procedure TfrmByteEditor.FormCreate(Sender : TObject);
begin
  isDataReader := false;
  strGrid.ColWidths[0] := 60;
  factX02 := 12; factY02 := 12;
  isDataExport := false;
end;

procedure TfrmByteEditor.FormShow(Sender : TObject);
begin
  propFlagModules[11] := 1;
  formId := formByteEditor;

//  FillByte(exportData, 255, 0);
  editMaxLines.Value := maxLines;
  if maxLines = 0 then
    btnExportData.Enabled := false
  else
    btnExportData.Enabled := true;

end;

procedure TfrmByteEditor.FormActivate(Sender : TObject);
begin
  formId := formByteEditor;
end;

procedure TfrmByteEditor.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  propFlagModules[11] := 0;
end;

procedure TfrmByteEditor.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmByteEditor.btnCloseClick(Sender : TObject);
begin
  isDataExport := false;
  Close;
end;

procedure TfrmByteEditor.btnExportDataClick(Sender : TObject);
begin
  isDataExport := true;
  maxLines := editMaxLines.Value;
  Close;
end;

procedure TfrmByteEditor.btnLoadDataClick(Sender : TObject);
var
  i : integer;
  n : integer;
  cnt : byte = 0;
  rowCnt : integer = 1;
  arr : array[0..15] of string[3];
  filename : string;
begin
  frmMain.dlgOpen.Title := 'Open file';
  if maxLines = 0 then
    frmMain.dlgOpen.Filter := 'All files (*.*)|*.*'
  else begin
    case beType of
      formPmg:
        frmMain.dlgOpen.Filter := 'Player files (*.spr)|*.spr|All files (*.*)|*.*';
      formAnimator:
        frmMain.dlgOpen.Filter := 'Atari Player data files (*.apl)|*.apl|All files (*.*)|*.*';
      formFont:
        frmMain.dlgOpen.Filter := 'Character set files (*.fnt, *.fon, *.set)' +
                                  '|*.fnt;*.fon;*.set|All files (*.*)|*.*';
    end;
  end;

  if frmMain.dlgOpen.Execute then begin
    isDataReader := true;
    isDataExport := false;
    filename := frmMain.dlgOpen.Filename;
    fs := TFileStream.Create(filename, fmOpenRead);
    try
      // Clear string grid
      for i := 0 to Pred(strGrid.ColCount) do
        for n := 0 to Pred(strGrid.RowCount) do
          strGrid.Cells[i, n] := '';

      strGrid.RowCount := 1;

      // Fill string grid
      for i := 0 to fs.Size - 1 do begin
        if (cnt = 16) or (i = fs.Size - 1) then begin
          strGrid.InsertRowWithValues(rowCnt, [inttostr(rowCnt),
            arr[0], arr[1], arr[2], arr[3], arr[4], arr[5], arr[6], arr[7],
            arr[8], arr[9], arr[10], arr[11], arr[12], arr[13], arr[14], arr[15]]);
          cnt := 0;
          Inc(rowCnt);
          for n := 0 to 15 do arr[n] := '0';
        end;
        n := fs.ReadByte;
        arr[cnt] := inttostr(n);
        Inc(cnt);
      end;
      paintBox.Invalidate;
      strGrid.Row := 1;
      strGrid.SetFocus;
      SetDecHex(true);
      lblFilePath.Visible := true;
      lblFilePath.Caption := 'Loaded file: ' + filename;
      lblFilesize.Visible := true;
      lblFilesize.Caption := 'File size: ' + IntToStr(fs.Size) + ' bytes';
    finally
      fs.Free;
    end;
  end;
end;

procedure TfrmByteEditor.chkRowSelectChange(Sender : TObject);
begin
  isDataReader := true;
  if chkRowSelect.Checked then
    strGrid.Options := strGrid.Options + [goRowSelect]
  else
    strGrid.Options := strGrid.Options - [goRowSelect];

  paintBox.Invalidate;
end;

procedure TfrmByteEditor.chkHexChange(Sender : TObject);
begin
  if chkHex.Checked then
    SetDecHex(true)
  else
    SetDecHex(false);
end;
procedure TfrmByteEditor.paintBoxPaint(Sender : TObject);
var
  bin : string;
  x, n : byte;
  yf, y : integer;
  colorx : byte;
  col, row : integer;
  value : byte;
begin
  if not isDataReader then Exit;

  Screen.Cursor := crHourGlass;
  FillByte(exportData, 255, 0);
(*
  with paintBox do begin
    Canvas.Brush.Color := clBlue;
    Canvas.FillRect(0, 0, Width, Height);
    //Canvas.Pen.Color := clRed;
    //Canvas.Line(0, 0, Width, Height);
    for yf := 0 to 255 do begin
      bin := Dec2Bin(yf);
      for x := 0 to 7 do begin
        colorx := StrToInt(bin[x + 1]);
        if colorx = 1 then
          Canvas.Brush.Color := coltab[5]
        else
          Canvas.Brush.Color := clBlack;

        Canvas.FillRect(bounds(x*factX, yf*factY, factX, factY));
      end;
    end;
  end;
*)
  with paintBox do begin
    Canvas.Brush.Color := clBlack;
    Canvas.FillRect(0, 0, Width, Height);
    yf := strGrid.row;

    // Selection
    if not chkRowSelect.Checked then begin
      y := 0;
      for Row := strGrid.Selection.Top to strGrid.Selection.Bottom do begin
        for Col := strGrid.Selection.Left to strGrid.Selection.Right do begin
          //values := values + strGrid.Cells[Col, Row];
  //        if strGrid.Cells[Col, Row] = '' then
  //          continue;

          if chkHex.Checked then begin
            bin := strGrid.Cells[Col, Row];
            value := Hex2Dec(bin);
            exportData[y] := value;
            bin := Dec2Bin(value);
          end
          else begin
            exportData[y] := StrToInt(strGrid.Cells[Col, Row]);
            bin := Dec2Bin(StrToInt(strGrid.Cells[Col, Row]));
          end;

          for x := 0 to 7 do begin
            colorx := StrToInt(bin[x + 1]);
            if colorx = 1 then begin
              if strGrid.Row = yf then
                Canvas.Brush.Color := clBlue
              else
                Canvas.Brush.Color := clRed;
            end
            else
              Canvas.Brush.Color := clBlack;

            Canvas.FillRect(bounds(x*factX02, y*factY02, factX02, factY02));
          end;
          Inc(y);
        end;
      end;
    end
    else begin
      y := 0;
      while yf <= strGrid.RowCount - 1 do begin
        for n := 1 to 15 do begin
          if strGrid.Rows[yf].Strings[n] = '' then
            continue;

          if chkHex.Checked then begin
            bin := strGrid.Rows[yf].Strings[n];
            value := Hex2Dec(bin);
            exportData[y] := value;
            bin := Dec2Bin(value);
          end
          else begin
            exportData[y] := StrToInt(strGrid.Rows[yf].Strings[n]);
            bin := Dec2Bin(StrToInt(strGrid.Rows[yf].Strings[n]));
          end;

          for x := 0 to 7 do begin
            colorx := StrToInt(bin[x + 1]);
            if colorx = 1 then begin
              if strGrid.Row = yf then
                Canvas.Brush.Color := clBlue
              else
                Canvas.Brush.Color := clRed
            end
            else
              Canvas.Brush.Color := clBlack;

            Canvas.FillRect(bounds(x*factX02, y*factY02, factX02, factY02));
          end;
          Inc(y);
        end;
        Inc(yf);
      end;
    end;
  end;
  Screen.Cursor := crDefault;
end;

procedure TfrmByteEditor.strGridHeaderClick(Sender : TObject; IsColumn : Boolean; Index : Integer);
begin
  if not IsColumn then (Sender as TStringGrid).Row := Index;
end;

procedure TfrmByteEditor.strGridKeyUp(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  paintBox.Invalidate;
end;

procedure TfrmByteEditor.strGridMouseUp(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  paintBox.Invalidate;
end;

procedure TfrmByteEditor.SetDecHex(isHex : boolean);
var
  i : integer;
  j : byte;
  byteVal : byte;
begin
  // Hexadecimal view
  if isHex then begin
    for i := 1 to strGrid.RowCount - 1 do
      for j := 1 to 16 do begin
        if strGrid.Cells[j, i] = '' then continue;
        strGrid.Cells[j, i] := Dec2Hex(StrToInt(strGrid.Cells[j, i]));
      end;
  end
  // Decimal view
  else begin
    for i := 1 to strGrid.RowCount - 1 do
      for j := 1 to 16 do begin
        if strGrid.Cells[j, i] = '' then continue;
        byteVal := Hex2Dec(strGrid.Cells[j, i]);
        strGrid.Cells[j, i] := IntToStr(byteVal);
      end;
  end;
end;

end.

