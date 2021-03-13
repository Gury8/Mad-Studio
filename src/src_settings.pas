{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2021
  Unit: Source code editor settings
}
unit src_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, lcltype,
  Buttons, ComCtrls, CheckLst, Grids, BCMDButton, BCMaterialDesignButton, types, StrUtils, SynEdit,
  SynHighlighterPas, SynHighlighterAny, AnchorDockPanel;

type
  { TfrmSrcSettings }
  TfrmSrcSettings = class(TForm)
    AnchorDockPanel1 : TAnchorDockPanel;
    btnKickC : TBCMDButton;
    btnGeneral : TBCMDButton;
    btnBasic : TBCMDButton;
    btnMadPascal : TBCMDButton;
    btnMads : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnCC65 : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnBackColors: TSpeedButton;
    btnConfirm : TBCMaterialDesignButton;
    btnCancel : TBCMaterialDesignButton;
    btnKickCLocation : TSpeedButton;
    btnSelectedTextBackColors: TSpeedButton;
    btnCC65Location: TSpeedButton;
    btnLineHighlightColors: TSpeedButton;
    btnMADSLocation: TSpeedButton;
    btnMadsMPLocation: TSpeedButton;
    btnMPLocation: TSpeedButton;
    btnAtariBASICLocation: TSpeedButton;
    btnFastBasicLocation: TSpeedButton;
    chkCC65: TCheckListBox;
    chkKickCLocation : TCheckBox;
    chkKickC : TCheckListBox;
    chkMadPascal: TCheckListBox;
    chkCC65Location: TCheckBox;
    chkEffectus: TCheckListBox;
    chkMadsMP: TCheckListBox;
    chkOutputLog: TCheckBox;
    chkAtariBASIC: TCheckListBox;
    chkMADS: TCheckListBox;
    chkMADSLocation02: TCheckBox;
    chkMadsLocation: TCheckBox;
    chkMPLocation: TCheckBox;
    chkAtariBASICLocation: TCheckBox;
    chkFastBasic: TCheckListBox;
    chkFastBasicLocation: TCheckBox;
    cmbFonts: TComboBox;
    cmbTextSizes: TComboBox;
    dlgColors: TColorDialog;
    Edit7: TEdit;
    editCC65Location: TEdit;
    editKickCLocation : TEdit;
    editCC65Output: TEdit;
    editMADSLocation: TEdit;
    editMADSOutput: TEdit;
    editFastBasicLocation: TEdit;
    editFastBasicOutput: TEdit;
    editMadsMPLocation: TEdit;
    editMadsMPOutput: TEdit;
    editAtariBASICOutput: TEdit;
    editMPLocation: TEdit;
    editAtariBASICLocation: TEdit;
    editMPOutput: TEdit;
    gridKickC : TStringGrid;
    gridMadPascal: TStringGrid;
    gridMadsMp: TStringGrid;
    boxFonts: TGroupBox;
    boxBackColor: TGroupBox;
    boxColor01: TGroupBox;
    boxColor02: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17 : TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    gridAtariBASIC: TStringGrid;
    gridMads: TStringGrid;
    gridFastBasic: TStringGrid;
    gridCC65: TStringGrid;
    Label9: TLabel;
    shapeBackColor: TShape;
    shapeSelectedTextBackColor: TShape;
    shapeLineHighlightColor: TShape;
    shapeTextColor: TShape;
    btnTextColors: TSpeedButton;
    editor: TSynEdit;
    gridEffectus: TStringGrid;
    tabs: TPageControl;
    tabAtariBASIC: TTabSheet;
    tabMadPascal: TTabSheet;
    tabMads: TTabSheet;
    tabGeneral: TTabSheet;
    tabCC65: TTabSheet;
    tabFastBasic: TTabSheet;
    tabEffectus: TTabSheet;
    tabKickC : TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure gridKickCDrawCell(Sender : TObject; aCol, aRow : Integer; aRect : TRect;
      aState : TGridDrawState);
    procedure gridKickCSelectCell(Sender : TObject; aCol, aRow : Integer; var CanSelect : Boolean);
    procedure OptionProc(Sender : TObject);
    procedure BackColorsProc(Sender: TObject);
    procedure ConfirmProc(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    procedure FastBasicLocation(Sender: TObject);
    procedure LineHighlightColorsProc(Sender: TObject);
    procedure MADSLocation(Sender: TObject);
    procedure SelectedTextBackColorsProc(Sender: TObject);
    procedure chkAtariBASICClickCheck(Sender: TObject);
    procedure chkAtariBASICLocationChange(Sender: TObject);
    procedure chkCC65LocationChange(Sender: TObject);
    procedure chkFastBasicLocationChange(Sender: TObject);
    procedure chkMADSClickCheck(Sender: TObject);
    procedure chkMADSLocation02Change(Sender: TObject);
    procedure chkMadsLocationChange(Sender: TObject);
    procedure chkMPLocationChange(Sender: TObject);
    procedure cmbFontsChange(Sender: TObject);
    procedure cmbTextSizesChange(Sender: TObject);
    procedure MPLocation(Sender: TObject);
    procedure MadsMPLocation(Sender: TObject);
    procedure AtariBASICLocation(Sender: TObject);
    procedure CC65Location(Sender: TObject);
    procedure gridAtariBASICDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure gridAtariBASICSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure gridEffectusSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure gridMadPascalDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure gridMadPascalSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure gridMadsDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure gridMadsMpDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure gridMadsMpSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure gridMadsSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure TextColorsProc(Sender: TObject);
    procedure gridEffectusDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure chkKickCLocationChange(Sender : TObject);
    procedure KickCLocationProc(Sender : TObject);
    procedure pageChange(Sender : TObject);
  private
    { private declarations }
    procedure SetParams(props, flags : TStringList; checkList : TCheckListBox; grid : TStringGrid);
    procedure SetEditor;
  public
    { public declarations }
  end;

var
  frmSrcSettings: TfrmSrcSettings;

implementation

{$R *.lfm}

uses
  main, common, src_editor, lib;

{ TfrmSrcSettings }

procedure TfrmSrcSettings.FormCreate(Sender: TObject);
begin
  cmbFonts.items.assign(screen.fonts);
  cmbFonts.ItemIndex := 0;

  tabs.ShowTabs := false;
end;

procedure TfrmSrcSettings.FormShow(Sender: TObject);
begin
  FormStyle := fsSystemStayOnTop;

  case frmSrcEdit.cmbLanguage.ItemIndex of
    0: tabs.ActivePage := tabAtariBASIC;
    1: tabs.ActivePage := tabMadPascal;
    2: tabs.ActivePage := tabMads;
    3: tabs.ActivePage := tabMads;
    4: tabs.ActivePage := tabCC65;
    5: tabs.ActivePage := tabFastBasic;
    6: tabs.ActivePage := tabEffectus;
    7: tabs.ActivePage := tabKickC;
  end;

  chkOutputLog.Checked := isOutputLog;

  editAtariBASICOutput.Text := strAtariBASICOutput;
  editMPOutput.Text := strMadPascalOutput;
  editMadsMPOutput.Text := strMadsOutput;
  editCC65Output.Text := strCC65Output;
  editFastBasicOutput.Text := strFastBasicOutput;

  chkAtariBASICLocation.Checked := isAtariBASICUserLocation;
  editAtariBASICLocation.Text := strAtariBASICUserLocation;

  chkMPLocation.Checked := isMadPascalUserLocation;
  editMPLocation.Text := strMadPascalUserLocation;

  chkMadsLocation.Checked := isMadsUserLocation;
  editMadsMPLocation.Text := strMadsUserLocation;

  chkMadsLocation02.Checked := isMadsUserLocation02;
  editMADSLocation.Text := strMadsUserLocation02;

  chkCC65Location.Checked := isCC65UserLocation;
  editCC65Location.Text := strCC65UserLocation;

  chkKickCLocation.Checked := isKickCUserLocation;
  editKickCLocation.Text := strKickCUserLocation;

  chkFastBasicLocation.Checked := isFastBasicUserLocation;
  editFastBasicLocation.Text := strFastBasicUserLocation;

  chkAtariBASICLocationChange(Sender);
  chkMPLocationChange(Sender);
  chkMadsLocationChange(Sender);
  chkCC65LocationChange(Sender);
  chkKickCLocationChange(Sender);
  chkFastBasicLocationChange(Sender);
  chkMADSLocation02Change(Sender);

  SetParams(propAtariBASIC, propFlagAtariBASIC, chkAtariBASIC, gridAtariBASIC);
  SetParams(propMadPascal, propFlagMadPascal, chkMadPascal, gridMadPascal);
  SetParams(propMadsMP, propFlagMadsMP, chkMadsMP, gridMadsMP);
  SetParams(propMADS, propFlagMADS, chkMADS, gridMads);
  SetParams(propFastBasic, propFlagFastBasic, chkFastBasic, gridFastBasic);
  SetParams(propEffectus, propFlagEffectus, chkEffectus, gridEffectus);
  SetParams(propKickC, propFlagKickC, chkKickC, gridKickC);

  // editor settings
  cmbFonts.Text := editorFont.Name;
  cmbTextSizes.Text := IntToStr(editorFont.Size);
  shapeTextColor.Brush.Color := editorFont.Color;
  shapeBackColor.Brush.Color := editorBackColor;
  shapeLineHighlightColor.Brush.Color := editorLineHighlightColor.Background;
  shapeSelectedTextBackColor.Brush.Color := editorSelectedTextBackColor.Background;

  SetEditor;

  //for i := 0 to propMADS.Count - 1 do begin
  //  param := ExtractDelimited(1, propMADS[i], ['=']);
  //  descr := ExtractDelimited(2, propMADS[i], ['=']);
  //  chkMADS.Items.Add(param + ' (' + descr + ')');
  //
  //  chkMADS.Checked[i] := propFlagMADS[i][1] <> '0';
  //
  //  //if propFlagMADS[i] = '0' then
  //  //  chkMADS.Checked[i] := false
  //  //else
  //  //  chkMADS.Checked[i] := true;
  //end;

  //editMADSBinAddr.Enabled := chkMADS.Checked[0];
  //editMADSMacroDef.Enabled := chkMADS.Checked[6];
  //editMADSObjFile.Enabled := chkMADS.Checked[11];

  //for i := 0 to propFastBasic.Count - 1 do begin
  //  chkFastBasic.Items.Add(propFastBasic[i] + ' (' + propDescrFastBasic[i] + ')');
  //
  //  if propFlagFastBasic[i] = '0' then
  //    chkFastBasic.Checked[i] := false
  //  else
  //    chkFastBasic.Checked[i] := true;
  //end;

  //for i := 0 to propAtariBASIC.Count - 1 do begin
  //  listAtariBASIC.InsertRow(propAtariBASIC[i], propDescrAtariBASIC[i], True);
  //  FirstItemProp := TItemProp.Create(listAtariBASIC);
  //  FirstItemProp.PickList.Add(propFlagAtariBASIC[i] + ' (' + propDescrAtariBASIC[i] + ')');
  //  FirstItemProp.PickList.Add(propFlagAtariBASIC[i] + ' (' + propDescrAtariBASIC[i] + ')');
  //  listAtariBASIC.ItemProps[i] := FirstItemProp;
  //end;

  //for i := 1 to listPmg.RowCount - 1 do begin
  //  k := listPmg.Keys[i];
  //  v := listPmg.Values[k];
  //  showmessage(k + ' * ' + v);
  //end;
end;

procedure TfrmSrcSettings.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmSrcSettings.gridKickCDrawCell(Sender : TObject; aCol, aRow : Integer; aRect : TRect;
  aState : TGridDrawState);
//var
//  s : string;
begin
//  s := gridKickC.Cells[ACol, ARow];

  //if (aRow in [0, 1]) then begin
  //  gridKickC.Canvas.Font.Color := clBlack;
  //  gridKickC.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, s);
  //end
  //else begin
    gridKickC.canvas.Brush.Color := RGBToColor(230, 230, 230);
    gridKickC.canvas.FillRect(aRect);
//  end;
end;

procedure TfrmSrcSettings.gridKickCSelectCell(Sender : TObject; aCol, aRow : Integer;
  var CanSelect : Boolean);
begin
  //if (aRow in [0, 1]) then
  //  gridKickC.Options := gridKickC.Options + [goEditing]
  //else
  gridKickC.Options := gridKickC.Options - [goEditing];
end;

procedure TfrmSrcSettings.SetParams(props, flags : TStringList; checkList : TCheckListBox;
  grid : TStringGrid);
var
  i : integer;
  param, descr : string;
begin
  for i := 0 to props.Count - 1 do begin
    param := ExtractDelimited(1, props[i], ['=']);
    descr := ExtractDelimited(2, props[i], ['=']);

    checkList.Items.Add(param + ' ' + descr);
    checkList.Checked[i] := flags[i][1] <> '0';

    if flags[i].Length > 1 then
      grid.Cells[0, i] := Copy(flags[i], 2, flags[i].Length - 1);
  end;
end;


procedure TfrmSrcSettings.MADSLocation(Sender: TObject);
begin
  frmMain.dlgFolder.Title := 'User defined Mad Assembler directory';

  if frmMain.dlgFolder.Execute then
    editMADSLocation.text := frmMain.dlgFolder.Filename;

  //frmMain.dlgOpen.Title := 'User defined Mad Assembler directory and executable';
  //if frmMain.dlgOpen.Execute then
  //  editMADSLocation.text := frmMain.dlgOpen.Filename;
end;

procedure TfrmSrcSettings.KickCLocationProc(Sender : TObject);
begin
  frmMain.dlgFolder.Title := 'User defined KickC directory';
  if frmMain.dlgFolder.Execute then
    editKickCLocation.text := frmMain.dlgFolder.Filename;
end;

procedure TfrmSrcSettings.chkKickCLocationChange(Sender : TObject);
begin
  editKickCLocation.Enabled := chkKickCLocation.Checked;
  btnKickCLocation.Enabled := chkKickCLocation.Checked;
end;

procedure TfrmSrcSettings.SelectedTextBackColorsProc(Sender: TObject);
begin
  if dlgColors.Execute then begin
    shapeSelectedTextBackColor.Brush.Color := dlgColors.Color;
    SetEditor;
  end;
end;

procedure TfrmSrcSettings.FastBasicLocation(Sender: TObject);
begin
  frmMain.dlgFolder.Title := 'User defined FastBasic directory';

  if frmMain.dlgFolder.Execute then
    editFastBasicLocation.text := frmMain.dlgFolder.FileName;
end;

procedure TfrmSrcSettings.LineHighlightColorsProc(Sender: TObject);
begin
  if dlgColors.Execute then begin
    shapeLineHighlightColor.Brush.Color := dlgColors.Color;
    SetEditor;
  end;
end;

procedure TfrmSrcSettings.chkAtariBASICClickCheck(Sender: TObject);
begin
//  editMaxLineLen.Enabled := chkAtariBASIC.Checked[0];
//  editOutput.Enabled := chkAtariBASIC.Checked[9];

//  if chkAtariBASIC.Checked[9] then
end;

procedure TfrmSrcSettings.chkMADSClickCheck(Sender: TObject);
begin
  //editMADSBinAddr.Enabled := chkMADS.Checked[0];
  //editMADSMacroDef.Enabled := chkMADS.Checked[6];
  //editMADSObjFile.Enabled := chkMADS.Checked[11];
end;

procedure TfrmSrcSettings.chkMADSLocation02Change(Sender: TObject);
begin
  editMADSLocation.Enabled := chkMadsLocation02.Checked;
  btnMADSLocation.Enabled := chkMadsLocation02.Checked;
end;

procedure TfrmSrcSettings.chkAtariBASICLocationChange(Sender: TObject);
begin
  editAtariBASICLocation.Enabled := chkAtariBASICLocation.Checked;
  btnAtariBASICLocation.Enabled := chkAtariBASICLocation.Checked;
end;

procedure TfrmSrcSettings.chkCC65LocationChange(Sender: TObject);
begin
  editCC65Location.Enabled := chkCC65Location.Checked;
  btnCC65Location.Enabled := chkCC65Location.Checked;
end;

procedure TfrmSrcSettings.chkFastBasicLocationChange(Sender: TObject);
begin
  editFastBasicLocation.Enabled := chkFastBasicLocation.Checked;
  btnFastBasicLocation.Enabled := chkFastBasicLocation.Checked;
end;

procedure TfrmSrcSettings.chkMadsLocationChange(Sender: TObject);
begin
  editMadsMPLocation.Enabled := chkMadsLocation.Checked;
  btnMadsMPLocation.Enabled := chkMadsLocation.Checked;
end;

procedure TfrmSrcSettings.chkMPLocationChange(Sender: TObject);
begin
  editMPLocation.Enabled := chkMPLocation.Checked;
  btnMPLocation.Enabled := chkMPLocation.Checked;
end;

procedure TfrmSrcSettings.cmbFontsChange(Sender: TObject);
begin
  SetEditor;
end;

procedure TfrmSrcSettings.cmbTextSizesChange(Sender: TObject);
begin
  SetEditor;
end;

procedure TfrmSrcSettings.MPLocation(Sender: TObject);
begin
  frmMain.dlgFolder.Title := 'User defined Mad Pascal directory';

  if frmMain.dlgFolder.Execute then
    editMPLocation.text := frmMain.dlgFolder.Filename;

  //frmMain.dlgOpen.Title := 'User defined Mad Pascal directory and executable';
  //if frmMain.dlgOpen.Execute then
  //  editMPLocation.text := frmMain.dlgOpen.Filename;
end;

procedure TfrmSrcSettings.MadsMPLocation(Sender: TObject);
begin
  frmMain.dlgFolder.Title := 'User defined Mad Assembler directory';

  if frmMain.dlgFolder.Execute then
    editMadsMPLocation.text := frmMain.dlgFolder.Filename;

  //frmMain.dlgOpen.Title := 'User defined Mad Assembler directory and executable';
  //if frmMain.dlgOpen.Execute then
  //  editMadsMPLocation.text := frmMain.dlgOpen.Filename;
end;

procedure TfrmSrcSettings.AtariBASICLocation(Sender: TObject);
begin
  frmMain.dlgFolder.Title := 'User defined Atari BASIC parser directory';

  if frmMain.dlgFolder.Execute then
    editAtariBASICLocation.text := frmMain.dlgFolder.Filename;

  //frmMain.dlgOpen.Title := 'User defined Atari BASIC parser directory and executable';
  //if frmMain.dlgOpen.Execute then
  //  editAtariBASICLocation.text := frmMain.dlgOpen.Filename;
end;

procedure TfrmSrcSettings.gridAtariBASICDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  s : string;
begin
  s := gridAtariBASIC.Cells[ACol, ARow];

//  if (aRow = 1) or (aRow = 3) then begin
  if (aRow in [0, 9]) then begin
    gridAtariBASIC.Canvas.Font.Color := clBlack;
    gridAtariBASIC.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, s);
  end
  else begin
    gridAtariBASIC.canvas.Brush.Color := RGBToColor(230, 230, 230);
    gridAtariBASIC.canvas.FillRect(aRect);
  end;
end;

procedure TfrmSrcSettings.gridAtariBASICSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if (aRow in [0, 9]) then
    gridAtariBASIC.Options := gridAtariBASIC.Options + [goEditing]
  else
    gridAtariBASIC.Options := gridAtariBASIC.Options - [goEditing];
end;

procedure TfrmSrcSettings.gridMadPascalDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  s : string;
begin
  s := gridMadPascal.Cells[ACol, ARow];

  if (aRow in [1, 2, 3, 4]) then begin
    gridMadPascal.Canvas.Font.Color := clBlack;
    gridMadPascal.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, s);
  end
  else begin
    gridMadPascal.canvas.Brush.Color := RGBToColor(230, 230, 230);
    gridMadPascal.canvas.FillRect(aRect);
  end;
end;

procedure TfrmSrcSettings.gridMadPascalSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if (aRow in [1, 2, 3, 4]) then
    gridMadPascal.Options := gridMadPascal.Options + [goEditing]
  else
    gridMadPascal.Options := gridMadPascal.Options - [goEditing];
end;

procedure TfrmSrcSettings.gridMadsDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  s : string;
begin
  s := gridMads.Cells[ACol, ARow];

  if (aRow in [0, 4, 5, 6, 7, 8, 9, 10, 14]) then begin
    gridMads.Canvas.Font.Color := clBlack;
    gridMads.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, s);
  end
  else begin
    gridMads.canvas.Brush.Color := RGBToColor(230, 230, 230);
    gridMads.canvas.FillRect(aRect);
  end;
end;

procedure TfrmSrcSettings.gridMadsSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if (aRow in [0, 4, 5, 6, 7, 8, 9, 10, 14]) then
    gridMads.Options := gridMads.Options + [goEditing]
  else
    gridMads.Options := gridMads.Options - [goEditing];
end;

procedure TfrmSrcSettings.TextColorsProc(Sender: TObject);
begin
  if dlgColors.Execute then begin
    shapeTextColor.Brush.Color := dlgColors.Color;
    SetEditor;
  end;
end;

procedure TfrmSrcSettings.gridEffectusSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if (aRow in [0, 1]) then
    gridEffectus.Options := gridEffectus.Options + [goEditing]
  else
    gridEffectus.Options := gridEffectus.Options - [goEditing];
end;

procedure TfrmSrcSettings.gridEffectusDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  s : string;
begin
  s := gridEffectus.Cells[ACol, ARow];

  if (aRow in [0, 1]) then begin
    gridEffectus.Canvas.Font.Color := clBlack;
    gridEffectus.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, s);
  end
  else begin
    gridEffectus.canvas.Brush.Color := RGBToColor(230, 230, 230);
    gridEffectus.canvas.FillRect(aRect);
  end;
end;

procedure TfrmSrcSettings.gridMadsMpDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  s : string;
begin
  s := gridMadsMp.Cells[ACol, ARow];

  if (aRow in [0, 4, 5, 6, 7, 8, 9, 10, 14]) then begin
    gridMadsMp.Canvas.Font.Color := clBlack;
    gridMadsMp.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, s);
  end
  else begin
    gridMadsMp.canvas.Brush.Color := RGBToColor(230, 230, 230);
    gridMadsMp.canvas.FillRect(aRect);
  end;
end;

procedure TfrmSrcSettings.gridMadsMpSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if (aRow in [0, 4, 5, 6, 7, 8, 9, 10, 14]) then
    gridMadsMp.Options := gridMadsMp.Options + [goEditing]
  else
    gridMadsMp.Options := gridMadsMp.Options - [goEditing];
end;

procedure TfrmSrcSettings.CC65Location(Sender: TObject);
begin
  frmMain.dlgFolder.Title := 'User defined CC65 directory';
  if frmMain.dlgFolder.Execute then
    editCC65Location.text := frmMain.dlgFolder.Filename;
end;

procedure TfrmSrcSettings.ConfirmProc(Sender: TObject);
var
  i : byte;
begin
  isChange := true;

  // editor settings
  editorFont.Name:= cmbFonts.Text;
  editorFont.Size:= StrToInt(cmbTextSizes.Text);
  editorFont.Color:= shapeTextColor.Brush.Color;
  editorBackColor := shapeBackColor.Brush.Color;
  editorLineHighlightColor.Background := shapeLineHighlightColor.Brush.Color;
  editorSelectedTextBackColor.Background := shapeSelectedTextBackColor.Brush.Color;

//  isMadPascal_d := TCheckBox(chkMadPascal.Controls[0]).Checked;
//  isMadPascal_o := TCheckBox(chkMadPascal.Controls[1]).Checked;

  //isMadsExcludeUnref_x := TCheckBox(chkMadsMP.Controls[0]).Checked;
  //isMadsAddLib_i := TCheckBox(chkMadsMP.Controls[1]).Checked;
  //isMadsBinAddress_b := TCheckBox(chkMadsMP.Controls[2]).Checked;
  //isMadsLabelCase_c := TCheckBox(chkMadsMP.Controls[3]).Checked;
  //
  //strMadsAddLib_i := editAddName_i.Text;
  //intMadsBinAddress_b := editMadsBinAddress_b.Value;

  // Language settings
  isOutputLog := chkOutputLog.Checked;

  strAtariBASICOutput := editAtariBASICOutput.Text;
  strMadPascalOutput := editMPOutput.Text;
  strMadsOutput := editMadsMPOutput.Text;
  strFastBasicOutput := editFastBasicOutput.Text;

  isAtariBASICUserLocation := chkAtariBASICLocation.Checked;
  isMadPascalUserLocation := chkMPLocation.Checked;
  isMadsUserLocation := chkMadsLocation.Checked;
  isMadsUserLocation02 := chkMadsLocation02.Checked;
  isCC65UserLocation := chkCC65Location.Checked;
  isFastBasicUserLocation := chkFastBasicLocation.Checked;
  isKickCUserLocation := chkKickCLocation.Checked;
  strAtariBASICUserLocation := editAtariBASICLocation.Text;
  strMadPascalUserLocation := editMPLocation.Text;
  strMadsUserLocation := editMadsMPLocation.Text;
  strMadsUserLocation02 := editMADSLocation.Text;
  strCC65UserLocation := editCC65Location.Text;
  strFastBasicUserLocation := editFastBasicLocation.Text;
  strKickCUserLocation := editKickCLocation.Text;

  if not isAtariBASICUserLocation then
    strAtariBASICUserLocation := '';

  if not isMadPascalUserLocation then
    strMadPascalUserLocation := '';

  if not isMadsUserLocation then
    strMadsUserLocation := '';

  if not isMadsUserLocation02 then
    strMadsUserLocation02 := '';

  if not isCC65UserLocation then
    strCC65UserLocation := '';

  if not isFastBasicUserLocation then
    strFastBasicUserLocation := '';

  if not isKickCUserLocation then
    strKickCUserLocation := '';

  if chkAtariBASICLocation.Checked and (editAtariBASICLocation.Text = '') then begin
    ShowMessage('Atari BASIC parser location is missing!');
    Exit;
  end;
  if chkMPLocation.Checked and (editMPLocation.Text = '') then begin
    ShowMessage('Mad Pascal location is missing!');
    Exit;
  end;
  if chkMadsLocation.Checked and (editMadsMPLocation.Text = '') then begin
    ShowMessage('Mad Assembler location is missing!');
    Exit;
  end;
  if chkMadsLocation02.Checked and (editMADSLocation.Text = '') then begin
    ShowMessage('Mad Assembler location is missing!');
    Exit;
  end;
  //if chkCC65Location.Checked and (editCC65Location.Text = '') then begin
  //  ShowMessage('CC65 location is missing!');
  //  Exit;
  //end;
  if chkFastBasicLocation.Checked and (editFastBasicLocation.Text = '') then begin
    ShowMessage('FastBasic location is missing!');
    Exit;
  end;
  if chkKickCLocation.Checked and (editKickCLocation.Text = '') then begin
    ShowMessage('KickC location is missing!');
    Exit;
  end;

  // Atari BASIC / Turbo BASIC XL
  for i := 0 to chkAtariBASIC.Items.Count - 1 do begin
    if chkAtariBASIC.Checked[i] then begin
      propFlagAtariBASIC[i] := '1';
      if i in [0, 9] then
        propFlagAtariBASIC[i] := propFlagAtariBASIC[i] + gridAtariBASIC.Cells[0, i];
    end
    else
      propFlagAtariBASIC[i] := '0'
  end;

  if chkAtariBASIC.Checked[9] and (gridAtariBASIC.Cells[0, 9] = '') then begin
//  if editOutput.Enabled and (editOutput.Text = '') then begin
    ShowMessage('Atari BASIC output filename is missing!');
    Exit;
  end;

//  strAtariBASIC_o := editOutput.Text;
//  valAtariBASIC_n := editMaxLineLen.Value;

  // Mad Pascal
  for i := 0 to chkMadPascal.Items.Count - 1 do begin
    if chkMadPascal.Checked[i] then begin
      propFlagMadPascal[i] := '1';
      if i in [1, 2, 3, 4] then
        propFlagMadPascal[i] := propFlagMadPascal[i] + gridMadPascal.Cells[0, i];
    end
    else
      propFlagMadPascal[i] := '0'
  end;

  // Mad Assembler (MADS) for Mad Pascal
  for i := 0 to chkMadsMP.Items.Count - 1 do begin
    if chkMadsMP.Checked[i] then begin
      propFlagMadsMP[i] := '1';
      if i in [0, 4, 5, 6, 7, 8, 9, 10, 14] then
        propFlagMadsMP[i] := propFlagMadsMP[i] + gridMadsMP.Cells[0, i];
    end
    else
      propFlagMadsMP[i] := '0'
  end;

  // Mad Assembler (MADS)
  for i := 0 to chkMADS.Items.Count - 1 do begin
    if chkMADS.Checked[i] then begin
      propFlagMADS[i] := '1';
      if i in [0, 4, 5, 6, 7, 8, 9, 10, 14] then
        propFlagMADS[i] := propFlagMADS[i] + gridMads.Cells[0, i];
    end
    else
      propFlagMADS[i] := '0'
  end;

//  if editMADSObjFile.Enabled and (editMADSObjFile.Text = '') then begin
  if chkMADS.Checked[11] and (gridMads.Cells[0, 11] = '') then begin
    ShowMessage('Mad Assembler object code filename is missing!');
    Exit;
  end;

  // FastBasic
  for i := 0 to chkFastBasic.Items.Count - 1 do begin
    if chkFastBasic.Checked[i] then
      propFlagFastBasic[i] := '1'
    else
      propFlagFastBasic[i] := '0'
  end;

  // Effectus
  for i := 0 to chkEffectus.Items.Count - 1 do begin
    if chkEffectus.Checked[i] then begin
      propFlagEffectus[i] := '1';
      if i in [0, 1] then
        propFlagEffectus[i] := propFlagEffectus[i] + gridEffectus.Cells[0, i];
    end
    else
      propFlagEffectus[i] := '0'
  end;

  // KickC
  for i := 0 to chkKickC.Items.Count - 1 do begin
    if chkKickC.Checked[i] then begin
      propFlagKickC[i] := '1';
//      if i in [0, 1] then
//        propFlagKickC[i] := propFlagKickC[i] + gridKickC.Cells[0, i];
    end
    else
      propFlagKickC[i] := '0'
  end;

  debug(strKickCUserLocation);

  Close;
end;

procedure TfrmSrcSettings.BackColorsProc(Sender: TObject);
begin
  if dlgColors.Execute then begin
    shapeBackColor.Brush.Color := dlgColors.Color;
    SetEditor;
  end;
end;

procedure TfrmSrcSettings.OptionProc(Sender : TObject);
begin
  tabs.TabIndex := TBCMDButton(Sender).Tag;
end;

procedure TfrmSrcSettings.CloseWinProc(Sender: TObject);
begin
  Close;
end;

procedure TfrmSrcSettings.SetEditor;
begin
  editor.Font.Name := cmbFonts.Text;
  editor.Font.Size := StrToInt(cmbTextSizes.Text);
  editor.Font.Color:= shapeTextColor.Brush.Color;
  editor.Color := shapeBackColor.Brush.Color;
  editor.LineHighlightColor.Background := shapeLineHighlightColor.Brush.Color;
  editor.SelectedColor.Background := shapeSelectedTextBackColor.Brush.Color;
end;

procedure TfrmSrcSettings.pageChange(Sender : TObject);
begin

end;

end.

