{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Display list editor
}
unit dl_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Menus, ComCtrls, StdCtrls,
  common;

type
  { TfrmDisplayList }
  TfrmDisplayList = class(TForm)
    btnCodeGen : TMenuItem;
    imgEditor: TImage;
    imgSelection: TImage;
    imgSelected: TImage;
    Label1: TLabel;
    memo: TMemo;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    menuDl: TMainMenu;
    MenuItem1: TMenuItem;
    miCloseWin: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem19: TMenuItem;
    miDefaultDl: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    btnColors: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    popUp: TPopupMenu;
    statusBar: TStatusBar;
    ToolBar2: TToolBar;
    ToolButton16: TToolButton;
    btnLoadDl: TToolButton;
    btnSaveDl: TToolButton;
    ToolButton25: TToolButton;
    btnClearDl: TToolButton;
    btnCodeGen2: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender : TObject);
    procedure FormDeactivate(Sender : TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure imgEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure GenCodeProc(Sender: TObject);
    procedure imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure imgSelectionMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure CloseWinProc(Sender: TObject);
    procedure ClearProc(Sender: TObject);
    procedure ColorsProc(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure SetTextClick(Sender: TObject);
    procedure OpenDlProc(Sender: TObject);
    procedure SaveDlAsProc(Sender: TObject);
    procedure SaveDlProc(Sender: TObject);
  private
    { private declarations }
    btn : tMousebutton;
    offs : byte;
    xxx, yyy : integer;
    maxDlByte : Integer;
    isSaveAs : boolean;
    modeType : byte;
    charStatus : byte;  // Character status in character set
    editTextX, editTextY : byte;
    procedure Selection;
    procedure SetModeType(textMode : byte);
    procedure PlotDl(anticMode : byte; xf, yf : integer; draw : boolean);
    procedure PlotByte(xf, yf : integer; index : byte; pixelColor : TColor; draw : boolean);
    procedure SaveDisplayList;
    procedure FillDlBits;
    procedure MemoList;
    procedure DisplayListDefault(dlData : array of byte);
    procedure PutChar(scrPos, y, offset : integer);
    procedure SaveDisplayListExt;
  public
    { public declarations }
    fldFontSet : fldFontSetType;
    grX, grY : integer;
    factX, factY,
    grX02, grY02,
    factX02, factY02 : byte;
    filenamex : string;
    dl : array[0..100] of byte;      // Display list program
    dlBits : array[0..239] of byte;  // Display list bytes
    maxX : byte;
    maxSize : integer;
    modeHeight : byte;
    textMode : byte;
    LMS_offset : byte;
    flagx : byte;
  end;

const
  _OFFSET = 96;
  _OFFSET01 = 48;
  _OFFSET02 = 24;

var
  frmDisplayList: TfrmDisplayList;

implementation

{$R *.lfm}

uses
  main, lib, colors, dl_gen;

var
  dlGr0 : array[0..31] of byte =
    ($70, $70, $70, $42, 0, $30, $2, $2, $2, $2, $2, $2, $2, $2, $2, $2, $2,
     $2, $2, $2, $2, $2, $2, $2, $2, $2, $2, $2, $2, $41, $82, $56);
//  112*112*112*66*64*156*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*65*32*156

{ TfrmDisplayList }

procedure TfrmDisplayList.FormCreate(Sender: TObject);
begin
  DoubleBuffered := true;
  btn := mbMiddle;
  modeType := 0;
  offs := 33;  // Character A
  charStatus := _CHARSET_UPPER;
  SetModeType(0);
  imgEditor.Width := maxX*24;
  imgEditor.Height := modeHeight*24;
  imgSelected.Height := modeHeight*24;
end;

procedure TfrmDisplayList.FormShow(Sender: TObject);
var
  n : integer = 72;
begin
  propFlagModules[7] := 1;
  isChange := true;
  frmMain.Top := 10;
  formId := formDisplayList;

  DefaultFontSet(fldFontSet);
//  imgSelection.Canvas.Brush.Color := clWhite;
  imgSelection.Canvas.Brush.Style := bsSolid;
//  imgSelection.Canvas.FillRect(bounds(0, 0, imgSelection.Width, imgSelection.Height));
  FillRectEx(imgSelection, clWhite, 0, 0, imgSelection.Width, imgSelection.Height);
  Selection;

  filenamex := getDir + 'examples\screen01.dl0';
  caption := programName + ' ' + programVersion + ' - Display list editor (' + filenamex + ')';

  with imgEditor.Canvas do begin
    // Display list editor area
    Brush.Style := bsSolid;

    // Top part
    FillRectEx(imgEditor, colTab[0], 0, 0, imgEditor.Width, n);

    // Middle part
    FillRectEx(imgEditor, coltabFont[0], 0, n - 1, imgEditor.Width, imgEditor.Height - n);

    // Bottom part
    FillRectEx(imgEditor, colTab[0], 0, imgEditor.Height - n - 1, imgEditor.Width, n);
  end;

  with imgSelected.Canvas do begin
    // Display list indicator area
    Brush.Style := bsSolid;

    // Top part of the screen
    FillRectEx(imgSelected, colTab[0], 0, 0, imgEditor.Width, n);

    // Middle part of the screen
    Brush.Color := coltabFont[0];
    FillRect(bounds(0, n - 1, imgEditor.Width, imgEditor.Height - n));

    // Bottom part of the screen
    Brush.Color := colTab[0];
    FillRect(bounds(0, imgEditor.Height - n - 1, imgEditor.Width, n));
  end;

  // Create default display list
  DisplayListDefault(dlGr0);
end;

procedure TfrmDisplayList.FormActivate(Sender : TObject);
begin
  formId := formDisplayList;
end;

procedure TfrmDisplayList.FormDeactivate(Sender : TObject);
begin
  FormStyle := fsNormal;
end;

procedure TfrmDisplayList.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[7] := 0;
  formId := formMain;
end;

procedure TfrmDisplayList.GenCodeProc(Sender: TObject);
begin
  SaveDisplayListExt;

  frmDisplayListGen := TfrmDisplayListGen.Create(Self);
  with frmDisplayListGen do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmDisplayList.imgEditorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
  j, yy : byte;
begin
  xf := X div factX;
  yf := Y div factY;
  if Button = mbRight then begin
    for j := 0 to modeHeight + 6 do begin
      //for i := 0 to maxX - 1 do begin
      //  if (xf >= 8*i) and (xf < 8*(i + 1)) then begin
      //    xf := i shl 3;
      //    xx := i;
      //    break;
      //  end;
      //end;
      if (yf > j shl 3) and (yf <= (j + 1) shl 3) then begin
        yf := j shl 3;
        yy := j;
        editTextY := yy;
        break;
      end;
    end;

    case dl[yy] of
      2: SetModeType(0);
      6: SetModeType(1);
      7: SetModeType(2);
    end;

    xxx := xf;
    yyy := yf;
    btn := Button;
    flagx := dl[yy];

    popUp.PopUp;
    exit;
  end;

  btn := Button;
  xf := X div factX;
  yf := Y div factY;

  case modeType of
    0: PlotDl(2, xf, yf, true);
    1: PlotDl(6, xf, yf, true);
    2: PlotDl(7, xf, yf, true);
    //3: PlotLine01(xf, yf, 1, true);
    //7: PlotLine01(xf, yf, 5, true);
    8: PlotByte(xf, yf, 20, colTab[0], true);
    9: PlotByte(xf, yf, 27, colTab[0], true);
    10: PlotByte(xf, yf, 8, coltab[3], true);
  end;
end;

procedure TfrmDisplayList.imgEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xf, yf : integer;
begin
  xf := X div factX;
  yf := Y div factY;
  statusBar.Panels[1].Text := 'Cursor coordinates' +
                              ' (x: ' + inttostr(xf) + ', y: ' + inttostr(yf) + ')';
//  statusBar.Panels[2].Text := inttostr(dlBits[yf*8]);

  if (xf <= 318) and (yf <= 240) then begin
    case modeType of
      0: PlotDl(2, xf, yf, true);
      1: PlotDl(6, xf, yf, true);
      2: PlotDl(7, xf, yf, true);
      //3: PlotLine01(xf, yf, 1, true);
      //7: PlotLine01(xf, yf, 5, true);
      8: PlotByte(xf, yf, 20, colTab[0], true);
      9: PlotByte(xf, yf, 27, colTab[0], true);
      10: PlotByte(xf, yf, 8, coltab[3], true);
    end;
  end;
end;

procedure TfrmDisplayList.imgEditorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  btn := mbMiddle;
//  MemoList;
end;

procedure TfrmDisplayList.SetModeType(textMode : byte);
begin
  modeType := textMode;

  if textMode = 0 then begin
    maxX := 40;
    modeHeight := 30;
    grY := modeHeight*8;
    factX := 3; factY := 3;
    factX02 := 3; factY02 := 3;
  end
  else if textMode = 1 then begin
    maxX := 20;
    modeHeight := 30;
//    modeSize := 20*modeHeight;
    grY := (modeHeight - 1)*8;
    factX := 6; factY := 3;
    factX02 := 6; factY02 := 3;
  end
  else if textMode = 2 then begin
    maxX := 20;
    modeHeight := 16;
//    modeSize := 10*modeHeight;
    grY := (modeHeight - 1)*8;
    factX := 6; factY := 6;
    factX02 := 6; factY02 := 6;
  end;

  grX02 := 7; grY02 := 7;
  grX := maxX*8;
  maxSize := maxX*modeHeight - 1;
end;

procedure TfrmDisplayList.imgSelectionMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Clear canvas selection
  with imgSelection.Canvas do begin
    Pen.Color := clDefault;
    Rectangle(1, 1, _OFFSET01 + 2, 30);

    Rectangle(_OFFSET - 2, 1, _OFFSET + _OFFSET02 + 2, 30);

    Rectangle(_OFFSET + _OFFSET02*3 - 2, 1, _OFFSET + _OFFSET02*3 + _OFFSET01 + 2, 30);

    Rectangle(_OFFSET + _OFFSET02*3 + _OFFSET - 2, 1,
              _OFFSET + _OFFSET02*3 + _OFFSET + _OFFSET02 + 2, 30);

    Rectangle(_OFFSET*2 + _OFFSET01*3 - 2, 1, _OFFSET*2 + _OFFSET01*4 + 2, 62);
  end;

  { Blank scan lines
   ---------------------------------------------------------------------------}
  if (x >= 0) and (x <= _OFFSET01) then begin
//    showmessage('test');
    modeType := 9;
    modeHeight := 30;
    with imgSelection do begin
      Canvas.Pen.Color := clRed;
      Canvas.Rectangle(1, 1, _OFFSET01 + 2, 30);
    end;
  end
  { Normal character
   ---------------------------------------------------------------------------}
  else if (x >= _OFFSET) and (x <= _OFFSET + _OFFSET02) then begin
    SetModeType(0);
    with imgSelection do begin
      Canvas.Pen.Color := clRed;
      Canvas.Rectangle(_OFFSET - 2, 1, _OFFSET + _OFFSET02 + 2, 30);
    end;
  end
  { Text mode 1 character
   ---------------------------------------------------------------------------}
  else if (x >= _OFFSET + _OFFSET02*3) and
          (x <= _OFFSET + _OFFSET02*3 + _OFFSET01) then
  begin
    SetModeType(1);

    with imgSelection do begin
      Canvas.Pen.Color := clRed;
      Canvas.Rectangle(_OFFSET + _OFFSET02*3 - 2, 1, _OFFSET + _OFFSET02*3 + _OFFSET01 + 2, 30);
    end;
  end
  { Graphics mode 3 pixel
   ---------------------------------------------------------------------------}
  else if (x >= _OFFSET + _OFFSET02*3 + _OFFSET) and
          (x <= _OFFSET + _OFFSET02*3 + _OFFSET + _OFFSET02) then
  begin
    modeType := 10;
    modeHeight := 30;
    with imgSelection do begin
      Canvas.Pen.Color := clRed;
      Canvas.Rectangle(_OFFSET + _OFFSET02*3 + _OFFSET - 2, 1,
                       _OFFSET + _OFFSET02*3 + _OFFSET + _OFFSET02 + 2, 30);
    end;
  end
  { Text mode 2 character
   ---------------------------------------------------------------------------}
   else if (x >= _OFFSET*2 + _OFFSET01*3) and
           (x <= _OFFSET*2 + _OFFSET01*4) then
   begin
     SetModeType(2);
     with imgSelection do begin
       Canvas.Pen.Color := clRed;
       Canvas.Rectangle(_OFFSET*2 + _OFFSET01*3 - 2, 1,
                        _OFFSET*2 + _OFFSET01*4 + 2, 62);
     end;
   end;

  statusBar.Panels[1].Text := inttostr(modeType);
  Selection;
end;

procedure TfrmDisplayList.CloseWinProc(Sender: TObject);
begin
  Close;
end;

procedure TfrmDisplayList.ClearProc(Sender: TObject);
begin
  FormShow(Sender);
end;

procedure TfrmDisplayList.ColorsProc(Sender: TObject);
begin
  frmColors.Show;
end;

procedure TfrmDisplayList.MenuItem14Click(Sender: TObject);
begin
  PlotByte(xxx, yyy + 1, 27, colTab[0], true);
  btn := mbMiddle;
end;

procedure TfrmDisplayList.SetTextClick(Sender: TObject);
var
  i, n : byte;
  ch : char;
  isAtascii : boolean;
  str : string;
begin
//  showmessage(inttostr(dlSave[editTextY.value]) + ' * ' + inttostr(modetype));

  if (dl[editTextY] <> 2) and (dl[editTextY] <> 6) and (dl[editTextY] <> 7) then
    ShowMessage('Entering text in this mode is not allowed!')
  else begin
    str := InputBox('Enter some text for selected display list mode line',
                    'Please type in some information', 'Text example');
    for i := 1 to Length(str) do begin
      ch := str[i];
      isAtascii := false;

      if (ord(ch) >= 32) and (ord(ch) <= 95) then begin
        n := ord(ch) - 32;
        isAtascii := true;
      end
      else if (ord(ch) >= 97) and (ord(ch) <= 122) then begin
        n := ord(ch);
        isAtascii := true;
      end;

      if isAtascii then begin
        if editTextY = 0 then
          PutChar(editTextX + i - 1, editTextY, n)
        else
          PutChar(editTextX + i - 1, editTextY + editTextY, n);

        //fldAtascii[editTextX.Value + i - 1 + editTextY*20] := n;
      end;
    end;
  end;
  flagx := 255;
  btn := mbMiddle;
end;

procedure TfrmDisplayList.OpenDlProc(Sender: TObject);
var
  fil : TextFile;
  i, count : integer;
  str : string;
  dlBytes: TStringList;
  dlOpen : array[0..100] of byte;
begin
  frmMain.dlgOpen.Title := 'Open existing display list file';
  frmMain.dlgOpen.Filter := 'Display list files (*.dl0, *.dl, *.txt)|*.dl0;*.dl;*.txt|' +
                            'All files (*.*)|*.*';
  if frmMain.dlgOpen.Execute then begin
    filenamex := frmMain.dlgOpen.Filename;
    AssignFile(fil, filenamex);
    try
      Reset(fil);
      ReadLn(fil, str);
      CloseFile(fil);
      caption := programName + ' ' + programVersion + ' - Display list editor (' + filenamex + ')';
      dlBytes := TStringList.Create;
      try
        dlBytes.StrictDelimiter := true;
        dlBytes.Delimiter := ',';
        dlBytes.DelimitedText := str;
        count := dlBytes.Count - 1;
        for i := 0 to count do
          if Trim(dlBytes[i]) <> '' then
            dlOpen[i] := StrToInt(dlBytes[i]);
      finally
        dlBytes.Free;
      end;
      DisplayListDefault(dlOpen);
    //finally
    //  closefile(fil);
    //end;
    except
      on E: EInOutError do begin
        ShowMessage('File handling error occurred. Details: ' + E.Message);
      end;
    end;
  end;
end;

procedure TfrmDisplayList.SaveDlAsProc(Sender: TObject);
begin
  isSaveAs := true;
  SaveDlProc(Sender);
end;

procedure TfrmDisplayList.DisplayListDefault(dlData : array of byte);
begin
  Move(dlData, dl, Sizeof(dlData));
  SaveDisplayList;
//  MemoList;
end;

procedure TfrmDisplayList.MemoList;
var
  i : Integer;
  str : string = '';
begin
  memo.Clear;
  for i := 0 to 33 do
    str += IntToStr(dl[i]) + ' ';

  memo.Lines.Add('dl: ' + str);
  memo.Lines.Add('------------------');

  str := '';
  for i := 0 to 239 do
    str += IntToStr(dlBits[i]) + ' ';

  memo.Lines.Add('------------------');
  memo.Lines.Add('dlBits: ' + str);
end;

procedure TfrmDisplayList.FillDlBits;
var
  yf : integer;
  i, n : integer;
  isLMS : boolean = false;
  cnt : byte = 0;
  ptr : byte = 0;

function CheckLMS(value : byte) : boolean;
var
  j : byte;
begin
  result := false;
  for j := 0 to Sizeof(dliLMSAnticModes) do
    if dliLMSAnticModes[j] = value then begin
      result := true;
      break;
    end;
end;

procedure FillDisplayList(mode, lines, offset : byte);
var
  j : byte;
begin
  for j := 0 to lines - 1 do
    dlBits[j + offset] := mode;
end;

begin
  yf := 0;
  for i := 0 to Sizeof(dl) do begin
    if not isLMS then begin
      isLMS := checkLMS(dl[i]);
      if isLMS then begin
//        showmessage('isLMS');
        if dl[i] = _LMS + _ANTIC_MODE_2 then begin
          SetModeType(0);
          PlotDl(2, 0, yf, false);
          FillDisplayList(2, AnticModes[2].scanLines, ptr);
          Inc(yf, AnticModes[2].scanLines);
          Inc(ptr, AnticModes[2].scanLines);
        end
        else if dl[i] = _LMS + _ANTIC_MODE_6 then begin
          SetModeType(1);
          PlotDl(6, 0, yf, false);
          FillDisplayList(6, AnticModes[6].scanLines, ptr);
          Inc(yf, AnticModes[6].scanLines);
          Inc(ptr, AnticModes[6].scanLines);
        end
        else if dl[i] = _LMS + _ANTIC_MODE_7 then begin
          SetModeType(2);
          PlotDl(7, 0, yf, false);
          FillDisplayList(7, AnticModes[7].scanLines, ptr);
          Inc(yf, AnticModes[7].scanLines);
          Inc(ptr, AnticModes[7].scanLines);
        end
        else if dl[i] = _LMS + _ANTIC_MODE_8 then begin
          PlotByte(0, yf, 8, coltab[3], false);
          FillDisplayList(8, AnticModes[8].scanLines, ptr);
          Inc(yf, AnticModes[8].scanLines);
          Inc(ptr, AnticModes[8].scanLines);
        end
      end;
    end;
    if isLMS and (cnt < 3) then begin
//      showmessage('isLMS and cnt < 3');
      Inc(cnt);
      continue;
    end;
    if dl[i] = _JVB then begin
//      showmessage('_JVB');
      break;
//      isJVB := true;
    end;
    if (dl[i] = _ANTIC_MODE_2) or
       (dl[i] = _ANTIC_MODE_6) or
       (dl[i] = _ANTIC_MODE_7) or
       (dl[i] = _ANTIC_MODE_8) then
    begin
      if isLMS then begin
        case dl[i] of
          _ANTIC_MODE_2: SetModeType(0);
          _ANTIC_MODE_6: SetModeType(1);
          _ANTIC_MODE_7: SetModeType(2);
          _ANTIC_MODE_8: PlotByte(0, yf, 8, coltab[3], false);
        end;

        if dl[i] <> _ANTIC_MODE_8 then
          PlotDl(dl[i], 0, yf, false);

        FillDisplayList(dl[i], AnticModes[dl[i]].scanLines, ptr);
        Inc(yf, AnticModes[dl[i]].scanLines);
        Inc(ptr, AnticModes[dl[i]].scanLines);
      //end
      //else begin
      //  ShowMessage('DL mismatch! No LMS defined yet!');
      //  break;
      end;
    end;
    for n := 0 to 7 do begin
      if dl[i] = n shl 4 then begin
        PlotByte(0, yf, 20 + n, colTab[0], false);
        FillDisplayList(n shl 4, AnticModes[20 + n].scanLines, ptr);
//        if AnticModes[20 + n].scanLines < 8 then begin
//          showmessage(inttostr(ptr));
//          nnn := nnn + ptr;
//          mmm := mmm + AnticModes[20 + n].scanLines*3
//        end;
        Inc(yf, AnticModes[20 + n].scanLines);
        Inc(ptr, AnticModes[20 + n].scanLines);
        break;
      end
    end;
    (*
    else if dl[i] = _BLANK_LINES_01 then begin
      PlotByte(0, yf, 20, colTab[0], false);
      FillDisplayList(0, AnticModes[20].scanLines, ptr);
      Inc(yf, AnticModes[20].scanLines);
      Inc(ptr, AnticModes[20].scanLines);
    end
    *)
  end;
  //if not isJVB then
  //  ShowMessage('Jump to start of display list is missing!');
end;

procedure TfrmDisplayList.SaveDisplayList;
var
  cnt : byte = 0;
  cnt02 : byte;
  i : integer;
  n : byte;
  flag : boolean = true;
begin
  FillDlBits;
  FillByte(dl, SizeOf(dl), 0);
  for i := 0 to Sizeof(dlBits) do begin
//    showmessage('dlBits[i] = ' + inttostr(i) + ' * ' + inttostr(dlBits[i]));
    // Check for blank lines
    for n := 0 to 7 do
      if dlBits[i] = n shl 4 then begin
//        showmessage('dlBits[i] = n shl 4 ?? ' + inttostr(dlBits[i]));
        if flag then begin
          dl[cnt] := dlBits[i];
          Inc(cnt);
          flag := false;
          cnt02 := 0;
        end;
        Inc(cnt02);
        flag := cnt02 = AnticModes[20 + n].scanLines;
//        if AnticModes[20 + n].scanLines < 8 then
        break;
      end;

    //if dlBits[i] = _BLANK_LINES_01 then begin
    //  if flag then begin
    //    dlSave[cnt] := dlBits[i];
    //    Inc(cnt);
    //    flag := false;
    //    cnt02 := 0;
    //  end;
    //  Inc(cnt02);
    //  flag := cnt02 = AnticModes[20].scanLines;
    //end
    //if dlBits[i] = _BLANK_LINES_08 then begin
    //  if flag then begin
    //    dlSave[cnt] := dlBits[i];
    //    Inc(cnt);
    //    flag := false;
    //    cnt02 := 0;
    //  end;
    //  Inc(cnt02);
    //  flag := cnt02 = AnticModes[27].scanLines;
    //end;

    if (dlBits[i] = _ANTIC_MODE_2) or
       (dlBits[i] = _ANTIC_MODE_6) or
       (dlBits[i] = _ANTIC_MODE_7) or
       (dlBits[i] = _ANTIC_MODE_8) then
    begin
//      if dlBits[i] = _ANTIC_MODE_7 then
//        showmessage('is 7');
      if flag then begin
        dl[cnt] := dlBits[i];
        Inc(cnt);
        flag := false;
        cnt02 := 0;
      end;
      Inc(cnt02);
      flag := cnt02 = AnticModes[dlBits[i]].scanLines;
    end;
  end;

  maxDlByte := cnt;

  //str := '';
  //for i := 0 to maxDlByte do begin
  //  str += IntToStr(dlSave[i]) + ',';
  //end;
end;

procedure TfrmDisplayList.SaveDisplayListExt;
var
  cnt : byte = 0;
  cnt02 : byte;
  cnt03 : word = 0;
  i : integer;
  n : byte;
  flag : boolean = true;
  isLMS : boolean = false;
//  flag02 : boolean = true;
begin
//  showmessage('dlSave = ' + inttostr(dlSave));

//  Move(dlSave, dl, Sizeof(dlSave));
//  FillDlBits;
  FillByte(dl, SizeOf(dl), 0);
//  showmessage('dlBits = ' + inttostr(dlBits));
  for i := 0 to Sizeof(dlBits) - 1 do begin
    // Check max. number of scanlines
    if cnt03 > 239 then break;

    // Check for blank lines
    for n := 1 to 7 do
      if dlBits[i] = n shl 4 then begin
        if flag then begin
          dl[cnt] := dlBits[i];
          Inc(cnt);
          flag := false;
          cnt02 := 0;
          cnt03 += AnticModes[20 + n].scanLines;
        end;
        Inc(cnt02);
        flag := cnt02 = AnticModes[20 + n].scanLines;
//        if AnticModes[20 + n].scanLines < 8 then
        break;
      end;

    //// Check for LMS
    //if not isLMS then begin
    //  case dlBits[i] of
    //    _ANTIC_MODE_2, _ANTIC_MODE_6, _ANTIC_MODE_7, _ANTIC_MODE_8: begin
    //      isLMS := true;
    //      dlSave[cnt] := _LMS + dlBits[i];
    //      Inc(cnt);
    //      LMS_offset := cnt;
    //      dlSave[cnt] := 0;
    //      dlSave[cnt + 1] := $30;
    //      cnt += 2;
    //      cnt03 += 3;
    //      Continue;
    //    end;
    //  end;
    //end;

    // Check for ANTIC modes
    if (dlBits[i] = _ANTIC_MODE_2) or
       (dlBits[i] = _ANTIC_MODE_6) or
       (dlBits[i] = _ANTIC_MODE_7) or
       (dlBits[i] = _ANTIC_MODE_8) then
    begin
//      showmessage('1. cnt = ' + inttostr(cnt));
      if flag then begin
        dl[cnt] := dlBits[i];

        // Check for LMS
        if not isLMS then
          dl[cnt] := _LMS + dlBits[i];

        Inc(cnt);
        if not isLMS then begin
          isLMS := true;
          LMS_offset := cnt;
          dl[cnt] := 0;
          dl[cnt + 1] := $30;
          cnt += 2;
        end;
        flag := false;
        cnt02 := 0;
        cnt03 += AnticModes[dlBits[i]].scanLines;
      end;
      Inc(cnt02);
      flag := cnt02 = AnticModes[dlBits[i]].scanLines;
    end;
  end;

  dl[cnt] := _JVB;
  dl[cnt + 1] := 150;
  dl[cnt + 2] := 160;

  maxDlByte := cnt + 2;

//  Move(dlSave, dl, Sizeof(dlSave));
end;

procedure TfrmDisplayList.SaveDlProc(Sender: TObject);
var
  fil : TextFile;
  i : integer;
  str : string = '';
begin
  SaveDisplayListExt;

  with frmMain.dlgSave do begin
    Title := 'Save display list to file';
    Filter := 'Display list files (*.dl0, *.dl, *.txt)|*.dl0;*.dl;*.txt|All files (*.*)|*.*';
    Filename := filenamex;

    if Execute then begin
      filenamex := Filename;

      str := '';
      for i := 0 to maxDlByte - 1 do
        str += IntToStr(dl[i]) + ',';

      AssignFile(fil, filenamex);
      try
        rewrite(fil);
        WriteLn(fil, str);
        caption := programName + ' ' + programVersion +
                   ' - Display list editor (' + filenamex + ')';
      except
        on E: EInOutError do
          ShowMessage('File handling error occurred. Details: ' + E.Message);
      end;
    end;

    CloseFile(fil);
  end;
end;

procedure TfrmDisplayList.Selection;
var
  col, xf, yf, offsetmm : integer;
  grX02mm, grY02mm,
  factX02mm, factY02mm : byte;
begin
  { Blank scan lines
   ---------------------------------------------------------------------------}
  xf := 0; yf := 0;
//  PlotByte(xf, yf, 2, colTab[0], false);  //PlotLine01(xf, yf, 7, true);
//  PlotByte(xf + 100, yf, 3, coltab[3], false);  //Plot3(xf, yf, true);
  factX02mm := 3;
  FillRectEx(imgSelection, colTab[0], xf, yf + 3, 6*AnticModes[27].bits, 3*AnticModes[27].scanLines);
  //imgSelection.Canvas.FillRect(bounds(xf*factX02mm + xoffsetmm, yf*3*AnticModes[2].scanLines,
  //                                    factX02mm, 3*AnticModes[2].scanLines));
  { Normal character
   ---------------------------------------------------------------------------}
  xf := 0; yf := 0;
  grX02mm := 7; grY02mm := 7;
  factX02mm := 3; factY02mm := 3;
  offsetmm := _OFFSET;
//  xoffsetmm := offsetmm * 24;
  for yf := 0 to grY02mm do
    for xf := 0 to grX02mm do begin
      col := fldFontSet[xf, offs shl 3 + yf];
//      imgSelection.Canvas.Brush.Color := coltabFont[col];
//      imgSelection.Canvas.FillRect(bounds(xf*factX02mm + offsetmm, yf*factY02mm + 3,
//                                     factX02mm, factY02mm));
      FillRectEx(imgSelection, coltabFont[col],
                 xf*factX02mm + offsetmm, yf*factY02mm + 3, factX02mm, factY02mm);
    end;

  { Text mode 1 character
   ---------------------------------------------------------------------------}
  xf := 0; yf := 0;
  factX02mm := 6; factY02mm := 3;
  Inc(offsetmm, 72);
  for yf := 0 to grY02mm do
    for xf := 0 to grX02mm do begin
      col := fldFontSet[xf, offs shl 3 + yf];

      if col = 1 then col := 2;
      imgSelection.Canvas.Brush.Color := coltab[col];
      imgSelection.Canvas.FillRect(bounds(xf*factX02mm + offsetmm, yf*factY02mm + 3,
                                          factX02mm, factY02mm));
    end;

  { Graphics mode 3 pixel
   ---------------------------------------------------------------------------}
  xf := 0; yf := 0;
  Inc(offsetmm, _OFFSET);
  imgSelection.Canvas.Brush.Color := coltab[3];
  imgSelection.Canvas.FillRect(bounds(xf*factX02mm + offsetmm, yf + 3,
                                      3*AnticModes[8].bits, 3*AnticModes[8].scanLines));

  { Text mode 2 character
   ---------------------------------------------------------------------------}
  xf := 0; yf := 0;
  factX02mm := 6; factY02mm := 6;
  Inc(offsetmm, 72);
  for yf := 0 to grY02mm do
    for xf := 0 to grX02mm do begin
      col := fldFontSet[xf, offs shl 3 + yf];

      if col = 1 then col := 2;
      imgSelection.Canvas.Brush.Color := coltab[col];
      imgSelection.Canvas.FillRect(bounds(xf*factX02mm + offsetmm, yf*factY02mm + 3,
                                     factX02mm, factY02mm));
    end;
end;

// Draw text mode 0, 1 or 2
procedure TfrmDisplayList.PlotDl(anticMode : byte; xf, yf : integer; draw : boolean);
var
  col, i, j, xx, yy : byte;
  dx, dy, offset, offs2, offset03 : integer;
  mHeight : word;
begin
  offs2 := offs;
  dy := yf;
  offset03 := 0;

  if draw then begin
    case btn of
      mbLeft : offs2 := offs;
      mbRight: offs2 := 0;
    else
      exit;
    end;

    if xf > grX then xf := grX;

    case anticMode of
      2:    mHeight := modeHeight + 6;
      6, 7: mHeight := modeHeight - 1;
    end;

    for j := 0 to mHeight do begin
      for i := 0 to maxX - 1 do
        if (xf >= i shl 3) and (xf < (i + 1) shl 3) then begin
          xf := i shl 3;
          xx := i;
          break;
        end;

      if (yf > j shl 3) and (yf <= (j + 1) shl 3) then begin
        yf := j shl 3;
        yy := j;
        break;
      end;
    end;

    //if anticMode = 2 then begin
    //  if xx + yy*40 > maxSize - 1 then Exit
    //end
    // Keep ANTIC mode 7 characters in bound limits
    if anticMode = 7 then
      if yy > 14 then Exit;

    statusBar.Panels[1].Text := 'Cursor coordinates (x: ' +
                                inttostr(xx) + ', y: ' + inttostr(yy) + ')';
  end;

  offset := offs2 shl 3;

  editTextY := yy;
//  fldAtascii[xx + yy*maxX] := offs2;

  imgEditor.Canvas.Brush.Color := coltabFont[0];
  //imgEditor.Canvas.Brush.Color := colTab[0];
  imgEditor.Canvas.FillRect(bounds(0, yf*factY + offset03,
                                   imgEditor.Width, AnticModes[anticMode].scanLines*3));
//  imgEditor.Canvas.FillRect(bounds(0, yf*factY, imgEditor.Width, 3*AnticModes[6].scanLines));

  if anticMode = 2 then
    imgSelected.Canvas.Brush.Color := coltabFont[0]
  else
    imgSelected.Canvas.Brush.Color := colTab[0];

  imgSelected.Canvas.FillRect(bounds(0, yf*factY + offset03,
                                     imgSelected.Width, AnticModes[anticMode].scanLines*3));
  //imgSelected.Canvas.FillRect(bounds(0, yf*factY, imgSelected.Width, 3*AnticModes[6].scanLines));

  for dy := 0 to 7 do
    for dx := 0 to 7 do begin
      col := fldFontSet[dx, dy + offset];
//      fldScreen[xf + dx, yf + dy] := col;
//      if col > 0 then col := charStatus;

      if anticMode = 2 then
        imgSelected.Canvas.Brush.Color := coltabFont[col]
      else
        imgSelected.Canvas.Brush.Color := coltab[col];

      imgSelected.Canvas.FillRect(bounds(dx*factX, (yf + dy)*factY + offset03, factX, factY));
    end;

  if draw then
    for i := 0 to AnticModes[anticMode].scanLines - 1 do begin
//      str += ', ' + IntToStr(dlBits[yy shl 3 + i]);
      if anticMode = 7 then
        dlBits[yy shl 4 + i] := anticMode
      else
        dlBits[yy shl 3 + i] := anticMode;
    end;

  statusBar.Panels[2].Text := 'PlotDl ' + inttostr(anticMode) + ' *** ' + inttostr(yy);
//  statusBar.Panels[3].Text := inttostr(AnticModes[21].scanLines * 3);
end;

// Draw antic mode pixel
procedure TfrmDisplayList.PlotByte(xf, yf : integer; index : byte; pixelColor : TColor;
  draw : boolean);
var
  i, j, xx, yy : byte;
  str : string = '';
begin
//  showmessage(inttostr(modeheight));
  xx := index;

  case index of
    20: index := 0;
    21: index := $10;
    22: index := $20;
    23: index := $30;
    24: index := $40;
    25: index := $50;
    26: index := $60;
    27: index := $70;
  end;

  if draw then begin
    case btn of
      mbLeft, mbRight: begin
      end;
    else
      exit;
    end;

    if xf > grX then xf := grX;

    for j := 0 to modeHeight + 6 do begin
      for i := 0 to maxX - 1 do
        if (xf >= i shl 3) and (xf < (i + 1) shl 3) then begin
          xf := i shl 3;
          break;
        end;

      if (yf > j shl 3) and (yf <= (j + 1) shl 3) then begin
        yf := j shl 3;
        yy := j;
        break;
      end;
    end;

//    showmessage('pre ' + inttostr(index));
    //case index of
    //  20: index := 0;
    //  21: index := $10;
    //  22: index := $20;
    //  23: index := $30;
    //  24: index := $40;
    //  25: index := $50;
    //  26: index := $60;
    //  27: index := $70;
    //end;
//    showmessage('post ' + inttostr(index));

//    dlSave[yy] := index;
  end;

  imgSelected.Canvas.Brush.Color := colTab[0];
  imgSelected.Canvas.FillRect(bounds(0, yf*3, imgSelected.Width, AnticModes[xx].scanLines*3));
  imgSelected.Canvas.Brush.Color := pixelColor;
  imgSelected.Canvas.FillRect(bounds(0, yf*3, AnticModes[xx].bits*3, AnticModes[xx].scanLines*3));
  imgEditor.Canvas.Brush.Color := colTab[0];
  imgEditor.Canvas.FillRect(bounds(0, yf*3, imgEditor.Width, AnticModes[xx].scanLines*3));

  if draw then
    for i := 0 to 7 do begin
      str += ', ' + IntToStr(dlBits[yy shl 3 + i]);
      dlBits[yy shl 3 + i] := index;  //xx;
    end;

//  statusBar.Panels[2].Text := str;
end;

// Draw character
procedure TfrmDisplayList.PutChar(scrPos, y, offset : integer);
var
  col, col02 : byte;
  dx, dy, xf, yf : integer;
//  isAtascii : boolean;
  isInverse : boolean = false;
begin
  // Graphics mode 0
  if flagx = 2 then begin
    xf := scrPos shl 3;
    yf := yyy;  //y shl 2;

    statusBar.Panels[2].Text := inttostr(y) + ' ' + inttostr(yf) + ' ' + inttostr(yyy);

  //  if xf > grX then xf := grX;

  //  if xf mod 8 = 0 then Inc(xf, 8);
    for dy := 0 to modeHeight - 1 do
      for dx := 0 to maxX - 1 do
        if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
          xf := dx shl 3;
          break;
        end;

    if offset > 128 then begin
      Dec(offset, 128);
      isInverse := true;
    end;

    offset := offset shl 3;

    for dy := 0 to 7 do
      for dx := 0 to 7 do begin
        col := fldFontSet[dx, dy + offset];
  //      fldScreen[xf + dx, yf + dy] := col;

        //if isInverse then begin
        //  if col = 1 then col := 0
        //  else if col = 0 then col := 1;
        //end;
        if isInverse then
          col := 1 - col;

        imgEditor.Canvas.Brush.Color := coltabFont[col];
        imgEditor.Canvas.FillRect(bounds((xf + dx)*factX, (yf + dy)*factY, factX, factY));
  //      imgEditor.Canvas.Pixels[(xf + dx)*factX, (yf + dy)*factY] := coltab[col];
      end;
  end

  // Graphics mode 1
  else if flagx = 6 then begin
    xf := scrPos shl 3;
    yf := yyy;  //y shl 2;

    for dy := 0 to modeHeight - 1 do
      for dx := 0 to 19 do
        if (xf >= dx shl 3) and (xf < (dx + 1) shl 3) then begin
          xf := dx shl 3;
          break;
        end;

    col02 := _CHARSET_UPPER;

    if (offset >= 97) and (offset <= 122) then begin
      col02 := _CHARSET_LOWER;
      Dec(offset, 64);
    end
    else if (offset >= 128) and (offset <= 128 + 63) then begin
      col02 := _CHARSET_UPPER_INV;
      Dec(offset, 128);
    end
    else if (offset >= 128 + 97) and (offset <= 128 + 97 + 26) then begin
      col02 := _CHARSET_LOWER_INV;
      Dec(offset, 64);
    end;

    offset := offset shl 3;

    for dy := 0 to 7 do
      for dx := 0 to 7 do begin
        col := fldFontSet[dx, dy + offset];
        if col = 1 then col := col02;
  //      fldScreen[xf + dx, yf + dy] := col;

        imgEditor.Canvas.Brush.Color := coltab[col];
  //      if col02 = _CHARSET_LOWER_INV then
  //        imgEditor.Canvas.FillRect(bounds((xf + dx)*factX+6, (yf + dy)*factY, factX, factY))
  //      else
        imgEditor.Canvas.FillRect(bounds((xf + dx)*factX, (yf + dy)*factY, factX, factY));
      end;
  end;
end;

end.

