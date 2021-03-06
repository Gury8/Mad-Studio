{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Common library
}
unit lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, strutils, StdCtrls, ComCtrls, ExtCtrls,
  Forms, Dialogs, LCLType, Controls, Graphics, BCTrackbarUpdown, BCMDButton, BCMaterialDesignButton,
  common;

procedure MoveLeft(mx, my : byte; var fld : charType);
procedure MoveRight(mx, my : byte; var fld : charType);
procedure MoveUp(mx, my : byte; var fld : charType);
procedure MoveDown(mx, my : byte; var fld : charType);

procedure MoveLeftPM(mx, my : byte; player : byte; var fld : fldType);
procedure MoveRightPM(mx, my : byte; player : byte; var fld : fldType);
procedure MoveUpPM(mx, my : byte; player : byte; var fld : fldType);
procedure MoveDownPM(mx, my : byte; player : byte; var fld : fldType);

function Dec2hex(getal : Integer) : String;
function Bin2dec(getal : String) : Integer;
function Dec2Bin(a:LongInt) : String;

procedure DefaultFontSet(var fldFontSet : fldFontSetType);

function CheckEditor : boolean;
function AtasciiCode(offset : byte) : string;

procedure FillRectEx(image : TImage; color : TColor; x, y, factX, factY : word);

procedure Debug(text : variant); overload;
procedure Debug(text : string; number : integer); overload;
procedure Debug(text : string; number01, number02 : integer); overload;
procedure Debug(text : string; number01, number02, number03 : integer); overload;

procedure ShowCursor(control : TControl; winControl : TWinControl; cursorType : TCursor);

procedure SetTrackBarUpDown(trackBarUpDown : TBCTrackBarUpDown; startColor, endColor : TColor);

procedure SaveColors;
procedure RetrieveColors;

procedure SetButton(btn : TBCMaterialDesignButton; isEnter : boolean);

var
  isDebug : boolean;

implementation

uses
  src_editor;

procedure MoveLeft(mx, my : byte; var fld : charType);
var
  x, y : byte;
begin
  for x := 1 to mx do
    for y := my downto 0 do begin
      fld[x - 1, y] := fld[x, y];
      fld[x, y] := 0;
    end;
end;

procedure MoveRight(mx, my : byte; var fld : charType);
var
  x, y : byte;
begin
  for x := mx - 1 downto 0 do
    for y := my downto 0 do begin
      fld[x + 1, y] := fld[x, y];
      fld[x, y] := 0;
    end;
end;

procedure MoveUp(mx, my : byte; var fld : charType);
var
  x, y : byte;
begin
  for x := 0 to mx do
    for y := 1 to my do begin
      fld[x, y - 1] := fld[x, y];
      fld[x, y] := 0;
    end;
end;

procedure MoveDown(mx, my : byte; var fld : charType);
var
  x, y : byte;
begin
  for x := 0 to mx do
    for y := my - 1 downto 0 do begin
      fld[x, y + 1] := fld[x, y];
      fld[x, y] := 0;
    end;
end;

procedure MoveLeftPM(mx, my : byte; player : byte; var fld : fldType);
var
  x, y : byte;
begin
  for x := 1 to mx do
    for y := my downto 0 do begin
      fld[player, x - 1, y] := fld[player, x, y];
      fld[player, x, y] := 0;
    end;
end;

procedure MoveRightPM(mx, my : byte; player : byte; var fld : fldType);
var
  x, y : byte;
begin
  for x := mx - 1 downto 0 do
    for y := my downto 0 do begin
      fld[player, x + 1, y] := fld[player, x, y];
      fld[player, x, y] := 0;
    end;
end;

procedure MoveUpPM(mx, my : byte; player : byte; var fld : fldType);
var
  x, y : byte;
begin
  for x := 0 to mx do
    for y := 1 to my do begin
      fld[player, x, y - 1] := fld[player, x, y];
      fld[player, x, y] := 0;
    end;
end;

procedure MoveDownPM(mx, my : byte; player : byte; var fld : fldType);
var
  x, y : byte;
begin
  for x := 0 to mx do
    for y := my downto 0 do begin
      fld[player, x, y + 1] := fld[player, x, y];
      fld[player, x, y] := 0;
    end;
end;

// Returns a string in which the character order of a specified string is reversed
function ReverseString(S : String) : String;
var
  i : Integer;
begin
  Result := '';
  for i := Length(S) downto 1 do
    Result := Result + Copy(S, i, 1) ;
end;

function Bin2Dec(getal : String) : Integer;
var
  i, uitvoer : Integer;
begin
  uitvoer := 0;
  for i := 0 to length(getal) - 1 do
    if getal[length(getal) - i] = '1' then
      uitvoer := uitvoer + 1 shl i;

  Result := uitvoer;
end;

function Dec2Hex(getal : integer) : String;
var
  uitvoer : String;
  getal_met_rest : Real;
  S : Integer;
begin
  while getal > 0 do begin
    getal_met_rest := getal shr 4;  /// 16;
    S := getal - (trunc(getal_met_rest) shl 4);
    if S < 10 then uitvoer := uitvoer + IntToStr(S);
    if S = 10 then uitvoer := uitvoer + 'A';
    if S = 11 then uitvoer := uitvoer + 'B';
    if S = 12 then uitvoer := uitvoer + 'C';
    if S = 13 then uitvoer := uitvoer + 'D';
    if S = 14 then uitvoer := uitvoer + 'E';
    if S = 15 then uitvoer := uitvoer + 'F';
    getal := trunc(getal_met_rest);
  end;

  if uitvoer = '' then
    result := '00'
  else
    result := ReverseString(uitvoer);
end;

function Dec2Bin(a : LongInt) : string;
var
  d : Integer;
  str : String;
  i : byte = 8;
Begin
  str := '';
  while i > 0 do begin
    d := a mod 2;
    str := concat(IntToStr(d), str);
    a := a div 2;
    dec(i);
  end;

  result := str;
End;

// Load default font
procedure DefaultFontSet(var fldFontSet : fldFontSetType);
var
  r, i : byte;
  j : integer;
  bin : string[9];
  rs: TResourceStream;
begin
  // Create a resource stream from font resource
  rs := TResourceStream.Create(HInstance, 'CHRSET', RT_RCDATA);
  try
    // Read font data
    for j := 0 to 1023 do begin
      r := rs.ReadByte;
      bin := IntToBin(r, 8);
      for i := 0 to 7 do
        fldFontSet[i, j] := StrToInt(bin[i + 1]);
    end;
  finally
    rs.Free; // destroy the resource stream
  end;
end;

function CheckEditor : boolean;
begin
  result := true;
  if frmSrcEdit.editor.Lines.Count > 0 then begin
    if MessageDlg('Question',
                  'All your previous work will be erased! Are you sure to copy new listing?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    begin
      result := false;
    end;
  end;
end;

// Calculate code number
function AtasciiCode(offset : byte) : string;
begin
  if (offset >= 0) and (offset <= 63) then
    offset += 32
  else if (offset >= 64) and (offset <= 95) then
    offset -= 64;

  result := IntToStr(offset);
end;

procedure FillRectEx(image : TImage; color : TColor; x, y, factX, factY : word);
begin
  with image.Canvas do begin
    Brush.Color := color;
    Brush.Style := bsSolid;
    FillRect(bounds(x, y, factX, factY));
    //  Pixels[x, y] := col;
  end;
end;

procedure Debug(text : variant); overload;
begin
  if isDebug then
    ShowMessage(text);
end;

procedure Debug(text : string; number : integer); overload;
begin
  if isDebug then
    ShowMessage(text + ' = ' + IntToStr(number));
end;

procedure Debug(text : string; number01, number02 : integer); overload;
begin
  if isDebug then
    ShowMessage(text + ' = ' + IntToStr(number01) + ', ' + IntToStr(number02));
end;

procedure Debug(text : string; number01, number02, number03 : integer); overload;
begin
  if isDebug then
    ShowMessage(text + ' = ' + IntToStr(number01) + ', ' + IntToStr(number02) + ', ' +
                IntToStr(number03));
end;

procedure ShowCursor(control : TControl; winControl : TWinControl; cursorType : TCursor);
var
  i : word;
begin
  control.Cursor := cursorType;
  for i := 0 to winControl.ControlCount - 1 do
    winControl.Controls[i].Cursor := cursorType;
end;

procedure SetTrackBarUpDown(trackBarUpDown : TBCTrackBarUpDown; startColor, endColor : TColor);
begin
  with trackBarUpDown do begin
    ButtonBackground.Gradient1.StartColor := startColor;
    ButtonBackground.Gradient1.EndColor := endColor;
    ButtonBackground.Gradient2.StartColor := endColor;
    ButtonBackground.Gradient2.EndColor := startColor;
  end;
end;

procedure SaveColors;
var
  i : byte;
begin
  //colTabCopy := Assign(colTab);
  //colorValuesCopy := Assign(colorValues);

  for i := 0 to 10 do begin
    colTabCopy[i] := colTab[i];
    colorValuesCopy[i] := colorValues[i];
  end;
end;

procedure RetrieveColors;
var
  i : byte;
begin
  //colTabCopy := Assign(colTab);
  //colorValuesCopy := Assign(colorValues);

  for i := 0 to 10 do begin
    colTab[i] := colTabCopy[i];
    colorValues[i] := colorValuesCopy[i];
  end;
end;

procedure SetButton(btn : TBCMaterialDesignButton; isEnter : boolean);
begin
  if isEnter then begin
    btn.NormalColor := $00CECECE;
    btn.NormalColorEffect := clWhite;
  end
  else begin
    btn.NormalColor := clWhite;
    btn.NormalColorEffect := clSilver;
  end;
end;

end.

