{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2023
  Unit: Source code editor settings
}
unit src_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, lcltype,
  Buttons, ComCtrls, BCMDButton, BCMaterialDesignButton, StrUtils, SynEdit, SynHighlighterPas,
  SynHighlighterAny, AnchorDockPanel;

type
  { TfrmSrcSettings }
  TfrmSrcSettings = class(TForm)
    AnchorDockPanel1 : TAnchorDockPanel;
    btnBackColors : TSpeedButton;
    btnKickC : TBCMDButton;
    btnGeneral : TBCMDButton;
    btnBasic : TBCMDButton;
    btnLineHighlightColors : TSpeedButton;
    btnMadPascal : TBCMDButton;
    btnMads : TBCMDButton;
    btnFastBasic : TBCMDButton;
    btnCC65 : TBCMDButton;
    btnEffectus : TBCMDButton;
    btnConfirm : TBCMaterialDesignButton;
    btnCancel : TBCMaterialDesignButton;
    btnKickCLocation : TSpeedButton;
    btnCC65Location: TSpeedButton;
    btnMADSLocation: TSpeedButton;
    btnMadsMPLocation: TSpeedButton;
    btnMPLocation: TSpeedButton;
    btnAtariBASICLocation: TSpeedButton;
    btnFastBasicLocation: TSpeedButton;
    btnSelectedTextBackColors : TSpeedButton;
    btnValueMadPascal: TButton;
    btnValueFastBasic: TButton;
    btnValueEffectus: TButton;
    btnValueKickC: TButton;
    btnValueAtariBasic: TButton;
    btnValueMads: TButton;
    btnValueMadsMP: TButton;
    chkKickCLocation : TCheckBox;
    chkCC65Location: TCheckBox;
    chkOutputLog: TCheckBox;
    chkMADSLocation02: TCheckBox;
    chkMadsLocation: TCheckBox;
    chkMPLocation: TCheckBox;
    chkAtariBASICLocation: TCheckBox;
    chkFastBasicLocation: TCheckBox;
    cmbFonts: TComboBox;
    cmbTextSizes: TComboBox;
    dlgColors: TColorDialog;
    editMadPascal: TEdit;
    editCC65Location: TEdit;
    editKickCLocation : TEdit;
    editCC65Output: TEdit;
    editFastBasic: TEdit;
    editEffectus: TEdit;
    editKickC: TEdit;
    editAtariBasic: TEdit;
    editMads: TEdit;
    editMadsMP: TEdit;
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
    boxFonts: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14 : TLabel;
    Label15 : TLabel;
    Label16 : TLabel;
    Label17 : TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    listKickC: TListView;
    listEffectus: TListView;
    listMads: TListView;
    listAtariBasic: TListView;
    listMadPascal: TListView;
    listMadsMP: TListView;
    listFastBasic: TListView;
    shapeBackColor : TShape;
    shapeLineHighlightColor : TShape;
    shapeSelectedTextBackColor : TShape;
    shapeTextColor: TShape;
    btnTextColors: TSpeedButton;
    editor: TSynEdit;
    tabs: TPageControl;
    tabAtariBASIC: TTabSheet;
    tabMadPascal: TTabSheet;
    tabMads: TTabSheet;
    tabGeneral: TTabSheet;
    tabCC65: TTabSheet;
    tabFastBasic: TTabSheet;
    tabEffectus: TTabSheet;
    tabKickC : TTabSheet;
    procedure btnValueProc(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ConfirmProc(Sender: TObject);
    procedure CloseWinProc(Sender: TObject);
    procedure listSettingsDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OptionProc(Sender : TObject);
    procedure BackColorsProc(Sender: TObject);
    procedure FastBasicLocation(Sender: TObject);
    procedure MADSLocation(Sender: TObject);
    procedure chkAtariBASICLocationChange(Sender: TObject);
    procedure chkCC65LocationChange(Sender: TObject);
    procedure chkFastBasicLocationChange(Sender: TObject);
    procedure chkMADSLocation02Change(Sender: TObject);
    procedure chkMadsLocationChange(Sender: TObject);
    procedure chkMPLocationChange(Sender: TObject);
    procedure cmbFontsChange(Sender: TObject);
    procedure cmbTextSizesChange(Sender: TObject);
    procedure MPLocation(Sender: TObject);
    procedure MadsMPLocation(Sender: TObject);
    procedure AtariBASICLocation(Sender: TObject);
    procedure CC65Location(Sender: TObject);
//    procedure gridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
//      aState: TGridDrawState);
    //procedure gridAtariBASICSelectCell(Sender: TObject; aCol, aRow: Integer;
    //  var CanSelect: Boolean);
//    procedure gridEffectusSelectCell(Sender: TObject; aCol, aRow: Integer;
//      var CanSelect: Boolean);
//    procedure gridMadsMpSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
//    procedure gridMadsSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure BackColorDown(Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer);
    procedure BackColorIconProc(Sender: TObject);
    procedure chkKickCLocationChange(Sender : TObject);
    procedure KickCLocationProc(Sender : TObject);
    procedure ButtonHoverEnter(Sender : TObject);
    procedure ButtonHoverLeave(Sender : TObject);
    procedure SetListParams(listView : TListView; props, flags : TStringList);
  private
    { private declarations }
    //procedure SetParams(props, flags : TStringList; checkList : TCheckListBox; grid : TStringGrid);
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

  listMadPascal.Items.Clear;
//  SetListParams(propFastBasic, propFlagFastBasic);
//  SetParams(propMadPascal, propFlagMadPascal, chkMadPascal, gridMadPascal);
  SetListParams(listAtariBASIC, propAtariBASIC, propFlagAtariBASIC);
  SetListParams(listMadPascal, propMadPascal, propFlagMadPascal);
  SetListParams(listMadsMp, propMadsMP, propFlagMadsMP);
  SetListParams(listMads, propMADS, propFlagMADS);
  SetListParams(listFastBasic, propFastBasic, propFlagFastBasic);
  SetListParams(listEffectus, propEffectus, propFlagEffectus);
  SetListParams(listKickC, propKickC, propFlagKickC);

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

//procedure TfrmSrcSettings.SetParams(props, flags : TStringList; checkList : TCheckListBox;
//  grid : TStringGrid);
//var
//  i : integer;
//  param, descr : string;
//begin
//  for i := 0 to props.Count - 1 do begin
//    param := ExtractDelimited(1, props[i], ['=']);
//    descr := ExtractDelimited(2, props[i], ['=']);
//
//    checkList.Items.Add(param + ' ' + descr);
//    checkList.Checked[i] := flags[i][1] <> '0';
//
//    if flags[i].Length > 1 then
//      grid.Cells[0, i] := Copy(flags[i], 2, flags[i].Length - 1);
//  end;
//end;

procedure TfrmSrcSettings.SetListParams(listView : TListView; props, flags : TStringList);
var
  i : integer;
  param, descr : string;
  lvItem : TListItem;
begin
  for i := 0 to props.Count - 1 do begin
    param := ExtractDelimited(1, props[i], ['=']);
    descr := ExtractDelimited(2, props[i], ['=']);

//    listMadPascal.Items.Add(param + ' ' + descr, 'test');

    lvItem := listView.Items.Add;
    lvItem.Caption := param;
    lvItem.SubItems.Add(descr);
    lvItem.Checked := flags[i][1] <> '0';
//    listMadPascal.Checked[i] := flags[i][1] <> '0';

    if flags[i].Length > 1 then
      lvItem.SubItems.Add(Copy(flags[i], 2, flags[i].Length - 1))
    else
      lvItem.SubItems.Add('');
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

procedure TfrmSrcSettings.FastBasicLocation(Sender: TObject);
begin
  frmMain.dlgFolder.Title := 'User defined FastBasic directory';

  if frmMain.dlgFolder.Execute then
    editFastBasicLocation.text := frmMain.dlgFolder.FileName;
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
end;

procedure TfrmSrcSettings.AtariBASICLocation(Sender: TObject);
begin
  frmMain.dlgFolder.Title := 'User defined Atari BASIC parser directory';

  if frmMain.dlgFolder.Execute then
    editAtariBASICLocation.text := frmMain.dlgFolder.Filename;
end;

//procedure TfrmSrcSettings.gridDrawCell(Sender: TObject; aCol, aRow: Integer;
//  aRect: TRect; aState: TGridDrawState);
//var
//  s : string;
//begin
////  s := gridAtariBASIC.Cells[ACol, ARow];
//  s := TStringGrid(Sender).Cells[ACol, ARow];
//
//  //if Sender = gridAtariBASIC then begin
//  //  //  if (aRow = 1) or (aRow = 3) then begin
//  //  if (aRow in [0, 9]) then begin
//  //    gridAtariBASIC.Canvas.Font.Color := clBlack;
//  //    gridAtariBASIC.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, s);
//  //  end
//  //  else begin
//  //    gridAtariBASIC.canvas.Brush.Color := RGBToColor(230, 230, 230);
//  //    gridAtariBASIC.canvas.FillRect(aRect);
//  //  end;
//  //end
//  //else if Sender = gridMadPascal then begin
//  //  if (aRow in [1, 2, 3, 4]) then begin
//  //    gridMadPascal.Canvas.Font.Color := clBlack;
//  //    gridMadPascal.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, s);
//  //  end
//  //  else begin
//  //    gridMadPascal.canvas.Brush.Color := RGBToColor(230, 230, 230);
//  //    gridMadPascal.canvas.FillRect(aRect);
//  //  end;
//  //end
//  //else if Sender = gridMads then begin
//  //  if (aRow in [0, 4, 5, 6, 7, 8, 9, 10, 14]) then begin
//  //    gridMads.Canvas.Font.Color := clBlack;
//  //    gridMads.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, s);
//  //  end
//  //  else begin
//  //    gridMads.canvas.Brush.Color := RGBToColor(230, 230, 230);
//  //    gridMads.canvas.FillRect(aRect);
//  //  end;
//  //end
//  //if Sender = gridEffectus then begin
//  //  if (aRow in [0, 1]) then begin
//  //    gridEffectus.Canvas.Font.Color := clBlack;
//  //    gridEffectus.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, s);
//  //  end
//  //  else begin
//  //    gridEffectus.canvas.Brush.Color := RGBToColor(230, 230, 230);
//  //    gridEffectus.canvas.FillRect(aRect);
//  //  end;
//  //end
//end;

//procedure TfrmSrcSettings.gridAtariBASICSelectCell(Sender: TObject; aCol, aRow: Integer;
//  var CanSelect: Boolean);
//begin
//  if (aRow in [0, 9]) then
//    gridAtariBASIC.Options := gridAtariBASIC.Options + [goEditing]
//  else
//    gridAtariBASIC.Options := gridAtariBASIC.Options - [goEditing];
//end;

//procedure TfrmSrcSettings.gridMadsSelectCell(Sender: TObject; aCol, aRow: Integer;
//  var CanSelect: Boolean);
//begin
//  if (aRow in [0, 4, 5, 6, 7, 8, 9, 10, 14]) then
//    gridMads.Options := gridMads.Options + [goEditing]
//  else
//    gridMads.Options := gridMads.Options - [goEditing];
//end;

//procedure TfrmSrcSettings.gridMadsSelectCell(Sender: TObject; aCol,
//  aRow: Integer; var CanSelect: Boolean);
//begin
//
//end;

procedure TfrmSrcSettings.BackColorDown(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
begin
  BackColorsProc(Sender);
end;

procedure TfrmSrcSettings.BackColorIconProc(Sender: TObject);
begin
  if dlgColors.Execute then begin
    case (Sender as TSpeedButton).Tag of
      0: shapeTextColor.Brush.Color := dlgColors.Color;
      1: shapeBackColor.Brush.Color := dlgColors.Color;
      2: shapeLineHighlightColor.Brush.Color := dlgColors.Color;
      3: shapeSelectedTextBackColor.Brush.Color := dlgColors.Color;
    end;
    SetEditor;
  end;
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
  // editor settings
  editorFont.Name:= cmbFonts.Text;
  editorFont.Size:= StrToInt(cmbTextSizes.Text);
  editorFont.Color:= shapeTextColor.Brush.Color;
  editorBackColor := shapeBackColor.Brush.Color;
  editorLineHighlightColor.Background := shapeLineHighlightColor.Brush.Color;
  editorSelectedTextBackColor.Background := shapeSelectedTextBackColor.Brush.Color;

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

  //// Atari BASIC / Turbo BASIC XL
  //for i := 0 to chkAtariBASIC.Items.Count - 1 do begin
  //  if chkAtariBASIC.Checked[i] then begin
  //    propFlagAtariBASIC[i] := '1';
  //    if i in [0, 9] then
  //      propFlagAtariBASIC[i] := propFlagAtariBASIC[i] + gridAtariBASIC.Cells[0, i];
  //  end
  //  else
  //    propFlagAtariBASIC[i] := '0'
  //end;

  for i := 0 to listAtariBASIC.Items.Count - 1 do begin
    if listAtariBASIC.Items[i].Checked then begin
      propFlagAtariBASIC[i] := '1';
      if i in [0, 9] then
        //propFlagMadPascal[i] := propFlagMadPascal[i] + gridMadPascal.Cells[0, i];
        propFlagAtariBASIC[i] := propFlagAtariBASIC[i] + listAtariBASIC.Items[i].SubItems[1];
    end
    else
      propFlagAtariBASIC[i] := '0'
  end;

//  if chkAtariBASIC.Checked[9] and (gridAtariBASIC.Cells[0, 9] = '') then begin
  if listAtariBASIC.Items[i].Checked and (listAtariBASIC.Items[9].SubItems[1] = '') then begin
    ShowMessage('Atari BASIC output filename is missing!');
    Exit;
  end;

  // Mad Pascal
  //for i := 0 to chkMadPascal.Items.Count - 1 do begin
  //  if chkMadPascal.Checked[i] then begin
  //    propFlagMadPascal[i] := '1';
  //    if i in [1, 2, 3, 4] then
  //      propFlagMadPascal[i] := propFlagMadPascal[i] + gridMadPascal.Cells[0, i];
  //  end
  //  else
  //    propFlagMadPascal[i] := '0'
  //end;

  for i := 0 to listMadPascal.Items.Count - 1 do begin
    if listMadPascal.Items[i].Checked then begin
      propFlagMadPascal[i] := '1';
      if i in [1, 2, 3, 4] then
        //propFlagMadPascal[i] := propFlagMadPascal[i] + gridMadPascal.Cells[0, i];
        propFlagMadPascal[i] := propFlagMadPascal[i] + listMadPascal.Items[i].SubItems[1];
    end
    else
      propFlagMadPascal[i] := '0'
  end;

  // Mad Assembler (MADS) for Mad Pascal
  for i := 0 to listMadsMP.Items.Count - 1 do begin
    if listMadsMP.Items[i].Checked then begin
      propFlagMadsMP[i] := '1';
      if i in [0, 4, 5, 6, 7, 8, 9, 10, 14] then
        //propFlagMadPascal[i] := propFlagMadPascal[i] + gridMadPascal.Cells[0, i];
        propFlagMadsMP[i] := propFlagMadsMP[i] + listMadsMP.Items[i].SubItems[1];
    end
    else
      propFlagMadsMP[i] := '0'
  end;

  // Mad Assembler (MADS)
  //for i := 0 to chkMADS.Items.Count - 1 do begin
  //  if chkMADS.Checked[i] then begin
  //    propFlagMADS[i] := '1';
  //    if i in [0, 4, 5, 6, 7, 8, 9, 10, 14] then
  //      propFlagMADS[i] := propFlagMADS[i] + gridMads.Cells[0, i];
  //  end
  //  else
  //    propFlagMADS[i] := '0'
  //end;

  for i := 0 to listMads.Items.Count - 1 do begin
    if listMads.Items[i].Checked then begin
      propFlagMADS[i] := '1';
      if i in [0, 4, 5, 6, 7, 8, 9, 10, 14] then
        //propFlagMadPascal[i] := propFlagMadPascal[i] + gridMadPascal.Cells[0, i];
        propFlagMADS[i] := propFlagMADS[i] + listMads.Items[i].SubItems[1];
    end
    else
      propFlagMADS[i] := '0'
  end;

////  if editMADSObjFile.Enabled and (editMADSObjFile.Text = '') then begin
//  if chkMADS.Checked[11] and (gridMads.Cells[0, 11] = '') then begin
//    ShowMessage('Mad Assembler object code filename is missing!');
//    Exit;
//  end;

  // FastBasic
  for i := 0 to listFastBasic.Items.Count - 1 do begin
    if listFastBasic.Items[i].Checked then begin
      propFlagFastBasic[i] := '1';
      if i in [3, 4, 7, 8, 9, 10] then
        propFlagFastBasic[i] := propFlagFastBasic[i] + listFastBasic.Items[i].SubItems[1];
    end
    else
      propFlagFastBasic[i] := '0'
  end;

  // Effectus
  for i := 0 to listEffectus.Items.Count - 1 do begin
    if listEffectus.Items[i].Checked then begin
      propFlagEffectus[i] := '1';
//      if i in [0, 1] then
        //propFlagMadPascal[i] := propFlagMadPascal[i] + gridMadPascal.Cells[0, i];
//        propFlagEffectus[i] := propFlagEffectus[i] + listEffectus.Items[i].SubItems[1];
    end
    else
      propFlagEffectus[i] := '0'
  end;

  // KickC
  for i := 0 to listKickC.Items.Count - 1 do begin
    if listKickC.Items[i].Checked then
      propFlagKickC[i] := '1'
    else
      propFlagKickC[i] := '0'
  end;

  Close;
end;

procedure TfrmSrcSettings.BackColorsProc(Sender: TObject);
begin
  if dlgColors.Execute then begin
//    shapeBackColor.Brush.Color := dlgColors.Color;
    (Sender as TShape).Brush.Color := dlgColors.Color;
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

procedure TfrmSrcSettings.listSettingsDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  paramValue : string;
begin
  if TListView(Sender).ItemIndex = -1 then exit;

  if Sender = listAtariBasic then begin
//    ShowMessage('listAtariBasic');
    editAtariBasic.Enabled := TListView(Sender).ItemIndex in [0, 9];
    editAtariBasic.ReadOnly := not editAtariBasic.Enabled;
  end
  else if Sender = listMadPascal then begin
//    ShowMessage('listSettings');
    editMadPascal.Enabled := TListView(Sender).ItemIndex in [1, 2, 3, 4];
    editMadPascal.ReadOnly := not editMadPascal.Enabled;
  end
  else if Sender = listMadsMp then begin
//    ShowMessage('listMadsMp');
    editMadsMp.Enabled := TListView(Sender).ItemIndex in [0, 4, 5, 6, 7, 8, 9, 10, 14];
    editMadsMp.ReadOnly := not editMadsMp.Enabled;
  end
  else if Sender = listMads then begin
//    ShowMessage('listMads');
    editMads.Enabled := TListView(Sender).ItemIndex in [0, 4, 5, 6, 7, 8, 9, 10, 14];
    editMads.ReadOnly := not editMads.Enabled;
  end
  else if Sender = listFastBasic then begin
    editFastBasic.Enabled := TListView(Sender).ItemIndex in [3, 4, 7, 8, 9, 10];
    editFastBasic.ReadOnly := not editFastBasic.Enabled;
  end;
//  else if Sender = listEffectus then begin
////    ShowMessage('listEffectus');
//    editEffectus.Enabled := TListView(Sender).ItemIndex in [0, 1];
//    editEffectus.ReadOnly := not TListView(Sender).ItemIndex in [0, 1];
//  end;

  //showmessage(listMadPascal.ItemIndex.ToString);

  if TListView(Sender).Items[TListView(Sender).ItemIndex].SubItems.Count = 2 then begin
    paramValue := TListView(Sender).Items[TListView(Sender).ItemIndex].SubItems[1];

    if Sender = listAtariBasic then
      editAtariBasic.Text := paramValue
    else if Sender = listMadPascal then
      editMadPascal.Text := paramValue
    else if Sender = listMadsMp then
      editMadsMp.Text := paramValue
    else if Sender = listMads then
      editMads.Text := paramValue
    else if Sender = listFastBasic then
      editFastBasic.Text := paramValue
//    else if Sender = listEffectus then
//      editEffectus.Text := paramValue;
  end;
end;

procedure TfrmSrcSettings.btnValueProc(Sender: TObject);
begin
  if Sender = btnValueAtariBAsic then
    listAtariBAsic.Items[listAtariBAsic.ItemIndex].SubItems[1] := editAtariBAsic.Text
  else if Sender = btnValueMadPascal then
    listMadPascal.Items[listMadPascal.ItemIndex].SubItems[1] := editMadPascal.Text
  else if Sender = btnValueMadsMp then
    listMadsMp.Items[listMadsMp.ItemIndex].SubItems[1] := editMadsMp.Text
  else if Sender = btnValueMads then
    listMads.Items[listMads.ItemIndex].SubItems[1] := editMads.Text
  else if Sender = btnValueFastBasic then
    listFastBasic.Items[listFastBasic.ItemIndex].SubItems[1] := editFastBasic.Text;
//  else if Sender = btnValueEffectus then
//    listEffectus.Items[listEffectus.ItemIndex].SubItems[1] := editEffectus.Text
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

procedure TfrmSrcSettings.ButtonHoverEnter(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, true);
end;

procedure TfrmSrcSettings.ButtonHoverLeave(Sender : TObject);
begin
  SetButton(Sender as TBCMaterialDesignButton, false);
end;

//  with ColorBox1 do
//  begin
//     items.Clear;
//     items.AddObject('clWhite', tobject(clWhite));
//  end;

end.

