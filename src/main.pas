{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Main application module
}
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, Buttons, BCImageButton, jsonConf, StrUtils,
  SynHighlighterPas, SynEditMiscClasses, SynHighlighterAny;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    btnCodeEditor10 : TBCImageButton;
    btnCodeEditor11 : TBCImageButton;
    btnCodeEditor12 : TBCImageButton;
    btnCodeEditor13 : TBCImageButton;
    btnCodeEditor14 : TBCImageButton;
    btnCodeEditor15 : TBCImageButton;
    btnCodeEditor16 : TBCImageButton;
    btnCodeEditor17 : TBCImageButton;
    btnCodeEditor18 : TBCImageButton;
    btnCodeEditor19 : TBCImageButton;
    btnCodeEditor7 : TBCImageButton;
    btnCodeEditor8 : TBCImageButton;
    btnCodeEditor9 : TBCImageButton;
    dlgColors: TColorDialog;
    Image1: TImage;
    images: TImageList;
    MenuItem12 : TMenuItem;
    MenuItem5 : TMenuItem;
    MenuItem7 : TMenuItem;
    mnuAntic3: TMenuItem;
    MenuItem18: TMenuItem;
    menuMain: TMainMenu;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    miExitPrg: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    dlgFolder: TSelectDirectoryDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure PmgAnimatorProc(Sender: TObject);
    procedure SrcEditorProc(Sender: TObject);
    procedure AnticMode45TileEditorProc(Sender : TObject);
    procedure GrEditorProc(Sender: TObject);
    procedure PmgEditorProc(Sender: TObject);
    procedure CharSetEditorProc(Sender: TObject);
    procedure Antic2EditorProc(Sender: TObject);
    procedure DLEditorProc(Sender: TObject);
    procedure TextMode12EditorProc(Sender: TObject);
    procedure AnticMode45EditorProc(Sender: TObject);
    procedure AboutProc(Sender: TObject);
    procedure LoadDefaultColors(Sender: TObject);
    procedure ColorPaletteProc(Sender: TObject);
    procedure LoadColorPalette(Sender: TObject);
    procedure ReleaseInfoProc(Sender: TObject);
    procedure AnticMode3EditorProc(Sender: TObject);
    procedure ViewerProc(Sender : TObject);
    procedure ByteEditorProc(Sender : TObject);
    procedure ExitPrg(Sender: TObject);
  private
    { private declarations }
    procedure LoadOptions;
  public
    { public declarations }
    procedure SaveOptions;
    procedure LoadDefaultPalette;
    procedure LoadColorPaletteProc(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  common, graph, src_editor, pmg, colors, fonts, about, antic2, antic6,
  antic3, antic4, dl_editor, animator, byte_editor, viewer, antic4_tiles;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i : byte;
begin
  btnCodeEditor7.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor8.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor9.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor10.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor11.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor12.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor13.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor14.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor15.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor16.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor17.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor18.LoadFromBitmapResource('BUTTON_GRAY');
  btnCodeEditor19.LoadFromBitmapResource('BUTTON_GRAY');

  dlgOpen.InitialDir := getDir + 'examples\';

  { Atari BASIC options
   ---------------------------------------------------------------------------}
  propAtariBASIC := TStringList.Create;
  propFlagAtariBASIC := TStringList.Create;

  with propAtariBASIC do begin
    Add('-n=Set maximum line length before splitting (default: 120)');
    Add('-l=Output long (readable) program');
    Add('-s=Output short listing program');
    Add('-b=Output binary (.BAS) program. (default)');
    Add('-x=Make binary output protected (un-listable)');
    Add('-f=Output full (long) variable names in binary output');
    Add('-k=Keep comments in the output (binary output mode)');
    Add('-a=In long output, convert comments to pure ASCII');
    Add('-A=Parse and outputs Atari Basic dialect instead of TurboBasicXL');
    Add('-v=Show more parsing information (verbose mode)');
    Add('-q=Don''t show parsing information (quiet mode)');
    Add('-o=Set the output file name or extension (if starts with a dot)');
    Add('-c=Output to standard output instead of a file');
    Add('-O=Defaults to run the optimizer in the parsed program');
    Add('-h=Show help and exit');
  end;

  with propFlagAtariBASIC do begin
    Add('0120');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
  end;

  { Mad Pascal options
   ---------------------------------------------------------------------------}
  propMadPascal := TStringList.Create;
  propFlagMadPascal := TStringList.Create;
  propMadsMP := TStringList.Create;
  propFlagMadsMP := TStringList.Create;

  with propMadPascal do begin
    Add('-diag=Diagnostics mode');
    Add('-define:symbol=Defines the symbol');
    Add('-ipath:<x>=Add <x> to include path');
    Add('-target:<x>=Target system: a8 (default), c64');
    Add('-code:address=Code origin hex address');
    Add('-data:address=Data origin hex address');
    Add('-stack:address=Software stack hex address (size = 64 bytes)');
    Add('-zpage:address=Variables on the zero page hex address (size = 26 bytes)');
  end;

  with propFlagMadPascal do begin
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
  end;

  { Mad Assembler (MADS) options
   ---------------------------------------------------------------------------}
  propMADS := TStringList.Create;
//  propDescrMADS := TStringList.Create;
  propFlagMADS := TStringList.Create;

  with propMADS do begin
    Add('-b:=Generate binary file at specific address');
    Add('-c=Label case sensitivity');
    Add('-d:=Define a label');
    //  -d:label=value  Define a label
    Add('-f=CPU command at first column');
    Add('-hc=Header file for CC65');
    Add('-hm=Header file for MADS');
    Add('-i:=Additional include directories');
    //  -i:path         Additional include directories
    Add('-l=Generate listing');
    Add('-m:=File with macro definition');
    Add('-ml:=margin-left property');
    Add('-o:=Set object file name');
    Add('-p=Print fully qualified file names in listing and error messages');
    Add('-s=Silent mode');
    Add('-x=Exclude unreferenced procedures');
    Add('-t=List label table');
    Add('-u=Warn of unused labels');
    Add('-vu=Verify code inside unreferenced procedures');
  end;

  with propFlagMADS do begin
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
    Add('1');
    Add('0');
    Add('0');
    Add('1');
    Add('0');
    Add('0');
    Add('0');
    Add('0');
  end;

  propMadsMP.Assign(propMADS);
  propFlagMadsMP.Assign(propFlagMADS);

  { FastBasic options
   ---------------------------------------------------------------------------}
  propFastBasic := TStringList.Create;
  propFlagFastBasic := TStringList.Create;

  with propFastBasic do begin
    Add('-d=Enable parser debug options (only useful to debug parser)');
    Add('-n=Don''t run the optimizer, produces same code as 6502 version');
    Add('-prof=Show token usage statistics');
//    -s=<name>      place code into given segment
//    Add('-s=<name>Place code into given segment');
    Add('-v=Show version and exit');
  end;

  with propFlagFastBasic do begin
    Add('0');
    Add('0');
    Add('0');
    Add('0');
  end;

  { Effectus options
   ---------------------------------------------------------------------------}
  propEffectus := TStringList.Create;
  propFlagEffectus := TStringList.Create;

  with propEffectus do begin
    Add('-i=Effectus variable usage list');
    Add('-t=Effectus only translate source to Mad Pascal');
    Add('-z=Variable zero page address');
  end;

  with propFlagEffectus do begin
    Add('0');
    Add('0');
    Add('0');
  end;

  { KickC options
   ---------------------------------------------------------------------------}
  propKickC := TStringList.Create;
  propFlagKickC := TStringList.Create;

  with propKickC do begin
    Add('-a=Generate binary file');
    Add('-e=Execute the assembled binary file using an appropriate emulator depending on the target platform.');
    Add('-E=Only run the preprocessor. Output is sent to standard out.');
  end;

  with propFlagKickC do begin
    Add('1');
    Add('0');
    Add('0');
  end;

  { Antic mode list
   ---------------------------------------------------------------------------}
  AnticModes[2].basicMode := 0;
  AnticModes[2].scanLines := 8;
  AnticModes[2].bits := 8;

  AnticModes[3].basicMode := 255;
  AnticModes[3].scanLines := 10;
  AnticModes[3].bits := 8;

  AnticModes[4].basicMode := 255;
  AnticModes[4].scanLines := 8;
  AnticModes[4].bits := 8;

  AnticModes[5].basicMode := 255;
  AnticModes[5].scanLines := 16;
  AnticModes[5].bits := 8;

  AnticModes[6].basicMode := 1;
  AnticModes[6].scanLines := 8;
  AnticModes[6].bits := 8;

  AnticModes[7].basicMode := 2;
  AnticModes[7].scanLines := 16;
  AnticModes[7].bits := 8;

  AnticModes[8].basicMode := 3;
  AnticModes[8].scanLines := 8;
  AnticModes[8].bits := 8;

  AnticModes[9].basicMode := 4;
  AnticModes[9].scanLines := 4;
  AnticModes[9].bits := 4;

  AnticModes[10].basicMode := 5;
  AnticModes[10].scanLines := 4;
  AnticModes[10].bits := 4;

  AnticModes[11].basicMode := 6;
  AnticModes[11].scanLines := 2;
  AnticModes[11].bits := 2;

  AnticModes[12].basicMode := 255;
  AnticModes[12].scanLines := 1;
  AnticModes[12].bits := 2;

  AnticModes[13].basicMode := 7;
  AnticModes[13].scanLines := 2;
  AnticModes[13].bits := 2;

  AnticModes[14].basicMode := 255;
  AnticModes[14].scanLines := 1;
  AnticModes[14].bits := 2;

  AnticModes[15].basicMode := 8;
  AnticModes[15].scanLines := 1;
  AnticModes[15].bits := 1;

  for i := 0 to 7 do begin
    AnticModes[20 + i].basicMode := 255;
    AnticModes[20 + i].scanLines := i + 1;
    AnticModes[20 + i].bits := 8;  //255;
  end;

//  AnticModes[27].bits := 8;

  editorFont := TFont.Create;
  editorLineHighlightColor := TSynSelectedColor.Create;
  editorSelectedTextBackColor := TSynSelectedColor.Create;
//  editorBackColor := TColor.Create;

  propModules := TStringList.Create;
  with propModules do begin
    Add('SourceCodeEditor');
    Add('GraphicsEditor');
    Add('PlayerMissileEditor');
    Add('CharacterSetEditor');
    Add('AtasciiEditor');
    Add('Textmode12Editor');
    Add('AnticMode45Editor');
    Add('DisplayListEditor');
    Add('PlayerAnimator');
    Add('ColorPalette');
    Add('AnticMode3Editor');
    Add('ByteEditor');
    Add('AnticMode4TileEditor');
  end;

  //pmSize[0].bit := 0
  //pmSize[0].def := 'normal size';
  //pmSize[1].bit := 1
  //pmSize[1].def := 'double size';
  //pmSize[2].bit := 3
  //pmSize[2].def := 'quadruple size';
//  = ('normal size', 'double size', 'quadruple size');

  LoadOptions;

  //btnChrSetEditor.StateNormal := btnSrcEditor.StateNormal;
  //btnChrSetEditor.StateHover := btnSrcEditor.StateHover;
  //btnChrSetEditor.StateClicked := btnSrcEditor.StateClicked;

//  isViewerModal := false;

  imgCharList := TList.Create;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  i : byte;
begin
  Caption := programName + ' ' + programVersion;
  LoadDefaultPalette;
//  frmColors.Show;
  formId := formMain;
  isGrMicPalette := true;
  getDir := ExtractFilePath(Application.ExeName);

  for i := 0 to 12 do
    if propFlagModules[i] = 1 then
       case i of
         0: SrcEditorProc(Sender);
         1: GrEditorProc(Sender);
         2: PmgEditorProc(Sender);
         3: CharSetEditorProc(Sender);
         4: Antic2EditorProc(Sender);
         5: TextMode12EditorProc(Sender);
         6: AnticMode45EditorProc(Sender);
         7: DLEditorProc(Sender);
         8: PmgAnimatorProc(Sender);
         9: ColorPaletteProc(Sender);
         10: AnticMode3EditorProc(Sender);
         11: ByteEditorProc(Sender);
         12: AnticMode45TileEditorProc(Sender);
       end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  formId := formMain;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveOptions;

  propAtariBASIC.Free;
  propFlagAtariBASIC.Free;

  propMadPascal.Free;
  propFlagMadPascal.Free;

  propMadsMP.Free;
  propFlagMadsMP.Free;

  propMADS.Free;
  propFlagMADS.Free;

  propFastBasic.Free;
  propFlagFastBasic.Free;

  propEffectus.Free;
  propFlagEffectus.Free;

  propKickC.Free;
  propFlagKickC.Free;

  editorFont.Free;
  editorLineHighlightColor.Free;
  editorSelectedTextBackColor.Free;

  propModules.Free;

  imgCharList.Free;
end;

{-----------------------------------------------------------------------------
 Load default color palette
 -----------------------------------------------------------------------------}
procedure TfrmMain.LoadDefaultPalette;
var
  r, g, b : byte;
  col : TShape;
  i : integer;
  rs : TResourceStream;
begin
  // Create a resource stream from palette resource
  rs := TResourceStream.Create(HInstance, 'PALETTE', RT_RCDATA);
  try
    // Read color data for color palette
    for i := 0 to 127 do begin
      r := rs.ReadByte; g := rs.ReadByte; b := rs.ReadByte;
      r := rs.ReadByte; g := rs.ReadByte; b := rs.ReadByte;
      colorMem[i] := RGBToColor(r, g, b);
      col := frmColors.FindComponent('color' + IntToStr(i)) as TShape;
      col.Brush.Color := colorMem[i];
    end;
    frmColors.SetDefaultPalette;
  finally
    rs.Free;  // Destroy the resource stream
  end;
end;

{-----------------------------------------------------------------------------
 Load color palette
 -----------------------------------------------------------------------------}
procedure TfrmMain.LoadColorPalette(Sender: TObject);
begin
  LoadColorPaletteProc(Sender);
end;

procedure TfrmMain.LoadColorPaletteProc(Sender: TObject);
var
  filename : string;
  r, g, b : byte;
  fs : TFileStream;
  col : TShape;
  i : integer;
begin
  frmMain.dlgOpen.Title := 'Open existing color palette file';
  dlgOpen.Filter := 'Color palette files (*.pal, *.act)|*.pal;*.act|All files (*.*)|*.*';
  if dlgOpen.Execute then begin
    filename := dlgOpen.Filename;
    fs := TFileStream.Create(Filename, fmOpenRead);
    try
      for i := 0 to 127 do begin
        r := fs.ReadByte; g := fs.ReadByte; b := fs.ReadByte;
        r := fs.ReadByte; g := fs.ReadByte; b := fs.ReadByte;
        colorMem[i] := RGBToColor(r, g, b);
        col := frmColors.FindComponent('color' + IntToStr(i)) as TShape;
        col.Brush.Color := colorMem[i];
      end;
    finally
      fs.Free;
    //except
    //  on E: EFOpenError do
    //    writeln('File handling error occurred. Details: ', E.Message);
    end;
    frmColors.SetDefaultPalette;
  end;
end;

{-----------------------------------------------------------------------------
 About window
 -----------------------------------------------------------------------------}
procedure TfrmMain.AboutProc(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  with frmAbout do
    try
      frmAbout.tabs.ActivePage := frmAbout.tabAbout;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmMain.AnticMode3EditorProc(Sender: TObject);
begin
  frmAntic3.Show;
end;

procedure TfrmMain.SrcEditorProc(Sender: TObject);
begin
  isMemoToEditor := false;
  frmSrcEdit.Show;
end;

procedure TfrmMain.PmgAnimatorProc(Sender: TObject);
begin
  frmAnimator.Show;
end;

procedure TfrmMain.GrEditorProc(Sender: TObject);
begin
  frmGraph.Show;
end;

procedure TfrmMain.PmgEditorProc(Sender: TObject);
begin
//  frmPmg.Cursor := crHourGlass;
  frmPmg.Show;
//  frmPmg.Cursor := crDefault;
end;

procedure TfrmMain.CharSetEditorProc(Sender: TObject);
begin
  frmFonts.Show;
end;

procedure TfrmMain.Antic2EditorProc(Sender: TObject);
begin
  frmAntic2.Show;
end;

procedure TfrmMain.DLEditorProc(Sender: TObject);
begin
  frmDisplayList.Show;
end;

procedure TfrmMain.TextMode12EditorProc(Sender: TObject);
begin
  frmAntic6.Show;
end;

procedure TfrmMain.AnticMode45EditorProc(Sender: TObject);
begin
  frmAntic4.Show;
end;

procedure TfrmMain.ByteEditorProc(Sender : TObject);
begin
  maxLines := 0;
  frmByteEditor.Show;
end;

procedure TfrmMain.ViewerProc(Sender : TObject);
begin
  isAntic4 := true;
  isAntic6 := true;
  frmViewer.Show;
end;

procedure TfrmMain.AnticMode45TileEditorProc(Sender : TObject);
begin
  frmAntic4Tiles.Show;
end;

procedure TfrmMain.LoadDefaultColors(Sender: TObject);
begin
  LoadDefaultPalette;
end;

procedure TfrmMain.ColorPaletteProc(Sender: TObject);
begin
  frmColors.Show;
end;

procedure TfrmMain.ExitPrg(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.ReleaseInfoProc(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  with frmAbout do
    try
      frmAbout.tabs.ActivePage := frmAbout.tabReleaseNotes;
      ShowModal;
    finally
      Free;
    end;
end;

{-----------------------------------------------------------------------------
 Load data from json configuration file
 -----------------------------------------------------------------------------}
procedure TfrmMain.LoadOptions;
var
  c : TJSONConfig;
  i : byte;
  param : string;
begin
  c := TJSONConfig.Create(nil);
  try
    c.Filename := getDir + 'madstudio.json';
    isOutputLog := c.GetValue('/General/outputLog', true);

    isShowPmEditorGrid := c.GetValue('/General/showPmEditorGrid', true);
    isPmMixedColor := c.GetValue('/General/pmMixedColor', true);

    // Selected computer language
    langIndex := c.GetValue('/General/laguageIndex', 0);

    editorFont.Name := c.GetValue('/SourceCodeEditor/fontName', 'Verdana');
    editorFont.Size := c.GetValue('/SourceCodeEditor/fontSize', 12);
    editorFont.Color := c.GetValue('/SourceCodeEditor/fontColor', 10158080);
    editorBackColor := c.GetValue('/SourceCodeEditor/backColor', 15587522);

    editorLineHighlightColor.Background := c.GetValue(
      '/SourceCodeEditor/lineHighlightColor', 13809565);
    editorSelectedTextBackColor.Background := c.GetValue(
      '/SourceCodeEditor/selectedTextBackColor', 11803563);

    strAtariBASICUserLocation := c.GetValue('/AtariBASIC/AtariBASICLocation', '');
    strAtariBASICOutput := c.GetValue('/AtariBASIC/output', 'atari_basic_output.txt');

    for i := 0 to propAtariBASIC.Count - 1 do begin
      param := ExtractDelimited(1, propAtariBASIC[i], ['=']);
      propFlagAtariBASIC[i] := c.GetValue('/AtariBASIC/' + param, '0');
    end;

    strMadPascalUserLocation := c.GetValue('/MadPascal/MadPascalLocation', '');
    strMadsUserLocation := c.GetValue('/MadPascal/MadsLocation', '');

    for i := 0 to propMadPascal.Count - 1 do begin
      param := ExtractDelimited(1, propMadPascal[i], ['=']);
      propFlagMadPascal[i] := c.GetValue('/MadPascal/' + param, '0');
    end;

    for i := 0 to propMadsMP.Count - 1 do begin
      param := ExtractDelimited(1, propMadsMP[i], ['=']);
      propFlagMadsMP[i] := c.GetValue('/MadPascal/' + param, '0');
    end;

//    isMadPascal_o := c.GetValue('/MadPascal/optimizeCode', true);
//    isMadPascal_d := c.GetValue('/MadPascal/diagnosticsMode', false);

    strMadPascalOutput := c.GetValue('/MadPascal/output', 'mp_output.txt');

    strMadsUserLocation02 := c.GetValue('/Mads/MadsLocation', '');
    strMadsOutput := c.GetValue('/Mads/output', 'mads_output.txt');

    for i := 0 to propMads.Count - 1 do begin
      param := ExtractDelimited(1, propMads[i], ['=']);
      propFlagMads[i] := c.GetValue('/Mads/' + param, '0');
    end;

    //isMadsLabelCase_c := c.GetValue('/Mads/labelCase', false);
    //isMadsExcludeUnref_x := c.GetValue('/Mads/excludeUnref', false);
    //isMadsAddLib_i := c.GetValue('/Mads/addLib', false);
    //strMadsAddLib_i := c.GetValue('/Mads/addLibName', strMadsAddLib_i);
    //isMadsBinAddress_b := c.GetValue('/Mads/binAddress', false);
    //intMadsBinAddress_b := c.GetValue('/Mads/binAddressValue', intMadsBinAddress_b);

    strFastBasicUserLocation := c.GetValue('/FastBasic/FastBasicLocation', '');
    strFastBasicOutput := c.GetValue('/FastBasic/output', 'fast_basic_output.txt');

    for i := 0 to propFastBasic.Count - 1 do begin
      param := ExtractDelimited(1, propFastBasic[i], ['=']);
      propFlagFastBasic[i] := c.GetValue('/FastBasic/' + param, '0');
    end;

    strCC65UserLocation := c.GetValue('/CC65/CC65Location', '');
    strCC65Output := c.GetValue('/CC65/output', 'cc65_output.txt');

    strEffectusUserLocation := c.GetValue('/Effectus/EffectusLocation', '');
    strEffectusOutput := c.GetValue('/Effectus/output', 'effectus_output.txt');

    for i := 0 to propEffectus.Count - 1 do begin
      param := ExtractDelimited(1, propEffectus[i], ['=']);
      propFlagEffectus[i] := c.GetValue('/Effectus/' + param, '0');
    end;

    strKickCUserLocation := c.GetValue('/KickC/KickCLocation', '');
//    strEffectusOutput := c.GetValue('/KickC/output', 'kickc_output.txt');

    for i := 0 to propKickC.Count - 1 do begin
      param := ExtractDelimited(1, propKickC[i], ['=']);
      propFlagKickC[i] := c.GetValue('/KickC/' + param, '0');
    end;

    if strAtariBASICUserLocation = '' then
      isAtariBASICUserLocation := false
    else
      isAtariBASICUserLocation := true;

    if strMadPascalUserLocation = '' then
      isMadPascalUserLocation := false
    else
      isMadPascalUserLocation := true;

    if strMadsUserLocation = '' then
      isMadsUserLocation := false
    else
      isMadsUserLocation := true;

    if strMadsUserLocation02 = '' then
      isMadsUserLocation02 := false
    else
      isMadsUserLocation02 := true;

    if strCC65UserLocation = '' then
      isCC65UserLocation := false
    else
      isCC65UserLocation := true;

    if strFastBasicUserLocation = '' then
      isFastBasicUserLocation := false
    else
      isFastBasicUserLocation := true;

    if strEffectusUserLocation = '' then
      isEffectusUserLocation := false
    else
      isEffectusUserLocation := true;

    if strKickCUserLocation = '' then
      isKickCUserLocation := false
    else
      isKickCUserLocation := true;

    // Module view
    for i := 0 to propModules.Count - 1 do begin
      param := ExtractDelimited(1, propModules[i], ['=']);
      propFlagModules[i] := c.GetValue('/ModuleView/' + param, 0);
    end;
  finally
    c.Free;
  end;
end;

{-----------------------------------------------------------------------------
 Save data to json configuration file
 -----------------------------------------------------------------------------}
procedure TfrmMain.SaveOptions;
var
  c : TJSONConfig;
  i : byte;
  param : string;
begin
  c := TJSONConfig.Create(nil);
  try
    c.Formatted := true;
    c.Filename := getDir + 'madstudio.json';

    c.SetValue('/General/outputLog', isOutputLog);
    c.SetValue('/General/showPmEditorGrid', isShowPmEditorGrid);
    c.SetValue('/General/pmMixedColor', isPmMixedColor);

    // Selected computer language
    c.SetValue('/General/laguageIndex', langIndex);

    c.SetValue('/SourceCodeEditor/fontName', editorFont.Name);
    c.SetValue('/SourceCodeEditor/fontSize', editorFont.Size);
    c.SetValue('/SourceCodeEditor/fontColor', editorFont.Color);
    c.SetValue('/SourceCodeEditor/backColor', editorBackColor);
    c.SetValue('/SourceCodeEditor/lineHighlightColor', editorLineHighlightColor.Background);
    c.SetValue('/SourceCodeEditor/selectedTextBackColor', editorSelectedTextBackColor.Background);

    c.SetValue('/AtariBASIC/AtariBASICLocation', strAtariBASICUserLocation);
    c.SetValue('/AtariBASIC/output', strAtariBASICOutput);

    for i := 0 to propAtariBASIC.Count - 1 do begin
      param := ExtractDelimited(1, propAtariBASIC[i], ['=']);
      c.SetValue('/AtariBASIC/' + param, propFlagAtariBASIC[i]);
    end;

    c.SetValue('/MadPascal/MadPascalLocation', strMadPascalUserLocation);
    c.SetValue('/MadPascal/MadsLocation', strMadsUserLocation);

    for i := 0 to propMadPascal.Count - 1 do begin
      param := ExtractDelimited(1, propMadPascal[i], ['=']);
      c.SetValue('/MadPascal/' + param, propFlagMadPascal[i]);
    end;

    for i := 0 to propMadsMP.Count - 1 do begin
      param := ExtractDelimited(1, propMadsMP[i], ['=']);
      c.SetValue('/MadPascal/' + param, propFlagMadsMP[i]);
    end;

//    c.SetValue('/MadPascal/optimizeCode', isMadPascal_o);
//    c.SetValue('/MadPascal/diagnosticsMode', isMadPascal_d);

    c.SetValue('/MadPascal/output', strMadPascalOutput);

    //for i := 0 to propAtariBASIC.Count - 1 do begin
    //  param := ExtractDelimited(1, propAtariBASIC[i], ['=']);
    //  c.SetValue('/AtariBASIC/' + param, propFlagAtariBASIC[i]);
    //end;

    c.SetValue('/Mads/MadsLocation', strMadsUserLocation02);

    for i := 0 to propMads.Count - 1 do begin
      param := ExtractDelimited(1, propMads[i], ['=']);
      c.SetValue('/Mads/' + param, propFlagMads[i]);
    end;

    //c.SetValue('/Mads/labelCase', isMadsLabelCase_c);
    //c.SetValue('/Mads/excludeUnref', isMadsExcludeUnref_x);
    //c.SetValue('/Mads/addLib', isMadsAddLib_i);
    //c.SetValue('/Mads/addLibName', strMadsAddLib_i);
    //c.SetValue('/Mads/binAddress', isMadsBinAddress_b);
    //c.SetValue('/Mads/binAddressValue', intMadsBinAddress_b);
    c.SetValue('/Mads/output', strMadsOutput);

    c.SetValue('/CC65/CC65Location', strCC65UserLocation);
    c.SetValue('/CC65/output', strCC65Output);

    c.SetValue('/FastBasic/FastBasicLocation', strFastBasicUserLocation);
    c.SetValue('/FastBasic/output', strFastBasicOutput);

    for i := 0 to propFastBasic.Count - 1 do begin
      param := ExtractDelimited(1, propFastBasic[i], ['=']);
      c.SetValue('/FastBasic/' + param, propFlagFastBasic[i]);
    end;

    c.SetValue('/Effectus/EffectusLocation', strEffectusUserLocation);

    for i := 0 to propEffectus.Count - 1 do begin
      param := ExtractDelimited(1, propEffectus[i], ['=']);
      c.SetValue('/Effectus/' + param, propFlagEffectus[i]);
    end;

    c.SetValue('/Effectus/output', strEffectusOutput);

    c.SetValue('/KickC/KickCLocation', strKickCUserLocation);

    for i := 0 to propKickC.Count - 1 do begin
      param := ExtractDelimited(1, propKickC[i], ['=']);
      c.SetValue('/KickC/' + param, propFlagKickC[i]);
    end;

//    c.SetValue('/KickC/output', strKickCOutput);

    // Module view
    for i := 0 to propModules.Count - 1 do begin
      param := ExtractDelimited(1, propModules[i], ['=']);
      c.SetValue('/ModuleView/' + param, propFlagModules[i]);
    end;
  finally
    c.Free;
  end;
end;

end.

(*var
//  regn: HRGN;
begin
  //regn := CreateRoundRectRgn(0, 0, ClientWidth, ClientHeight, 18, 18);
  //SetWindowRgn(Handle, regn, True);*)

