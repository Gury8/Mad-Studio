{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: Source code editor
}
unit src_editor;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, FileUtil, SynEdit, SynCompletion, types, Forms, Controls,
  LazFileUtils, Graphics, Dialogs, Menus, ComCtrls, StdCtrls, ExtCtrls, strutils, Process,
  LCLType, Character, SynHighlighterPas, SynHighlighterAny, SynEditTypes, SynEditMiscClasses,
  SynEditMarkupHighAll, SynHighlighterJScript;

type
  { TfrmSrcEdit }
  TfrmSrcEdit = class(TForm)
    btnCompileRun: TToolButton;
    cmbLanguage: TComboBox;
    cmbProp: TComboBox;
    cmbLineBreak : TComboBox;
    editor: TSynEdit;
    dlgFind: TFindDialog;
    lblOutput: TLabel;
    lblOutput02: TLabel;
    MenuItem16: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem26 : TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCut: TMenuItem;
    menuTextEditor: TMainMenu;
    memoMP: TMemo;
    memoMads: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    panel: TPanel;
    dlgReplace: TReplaceDialog;
    sbEditor: TStatusBar;
    SynJScriptSyn: TSynJScriptSyn;
    ToolBar1: TToolBar;
    btnOpenFile: TToolButton;
    ToolButton11: TToolButton;
    btnSaveFile: TToolButton;
    btnNewFile: TToolButton;
    btnCompile: TToolButton;
    btnFindText: TToolButton;
    btnReplaceText: TToolButton;
    btnSettings: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure editorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cmbPropChange(Sender: TObject);
    procedure cmbLanguageChange(Sender: TObject);
    procedure dlgFindFind(Sender: TObject);
    procedure dlgReplaceReplace(Sender: TObject);
    procedure editorChange(Sender: TObject);
    procedure editorClick(Sender: TObject);
    procedure CloseFileProc(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure CloseProc(Sender: TObject);
    procedure PasteCodeProc(Sender: TObject);
    procedure CopyProc(Sender: TObject);
    procedure PasteProc(Sender: TObject);
    procedure CutProc(Sender: TObject);
    procedure UndoProc(Sender: TObject);
    procedure RedoProc(Sender: TObject);
    procedure SettingsProc(Sender: TObject);
    procedure OpenFileProc(Sender: TObject);
    procedure SaveFileProc(Sender: TObject);
    procedure SaveFileAsProc(Sender: TObject);
    procedure CompileParseProc(Sender: TObject);
    procedure FindTextProc(Sender: TObject);
    procedure ReplaceTextProc(Sender: TObject);
    procedure NewFileProc(Sender: TObject);
  private
    { private declarations }
    isSaveAs : boolean;
    isSaveAsFalse : boolean;
    oldLanguage : byte;
    procedure SetParams(AProcess : TProcess; props, flags : TStringList);
    procedure ShowFile(aFile: string);
    procedure Compile(Sender: TObject; tagIndex: byte);
    procedure SaveProc(Sender: TObject);
    procedure SetEditor;
  public
    { public declarations }
    filename : string;
    isEdit : boolean;
    procedure MemoToEditor(Sender: TObject);
  end;

var
  frmSrcEdit: TfrmSrcEdit;

implementation

{$R *.lfm}

uses
  common, main, src_settings, pmg_gen, graph_gen, font_gen, antic2_gen,
  antic6_gen, antic4_gen, dl_gen, anim_gen, antic3_gen;

{ TfrmSrcEdit }

procedure TfrmSrcEdit.FormCreate(Sender: TObject);
var
  SynMarkup: TSynEditMarkupHighlightAllCaret;
begin
  isSaveAs := false;
  isSaveAsFalse := false;

  // Set SynEdit feature for automatic markup of all occurrences of selected word
  SynMarkup := TSynEditMarkupHighlightAllCaret(
      editor.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  SynMarkup.MarkupInfo.FrameColor := clGray;
  SynMarkup.MarkupInfo.Background := clSilver;
  SynMarkup.WaitTime := 100;
  SynMarkup.Trim := true;      // no spaces, if using selection
  SynMarkup.FullWord := true;  // only full words If "Foo" is under caret, do not mark it in "FooBar"
  SynMarkup.IgnoreKeywords := false;
end;

procedure TfrmSrcEdit.FormShow(Sender: TObject);
begin
  propFlagModules[0] := 1;
  isChange := true;

  frmMain.Top := 0;
  formId := formSrcEdit;
  cmbLanguageChange(Sender);
  SetEditor;
  //editor.Font.Name := editorFont.Name;
  //editor.Font.Size := editorFont.Size;
  //editor.Font.Color:= editorFont.Color;
  //editor.Color := editorBackColor;
  //editor.LineHighlightColor := editorLineHighlightColor;

//  showmessage(inttostr(editorFont.Size));
//  editor.Font.Size := editorFont.Size;

//  editorFont.Name:= 'Courier New';
  //  editorFont.Style:= $009B0000;
//    editorFont.Size:= 14;
  //  editorFont.Color:= $009B0000;

  //dlgFind.Top := TfrmSrcEdit.Top;
  //dlgFind.Left := TfrmSrcEdit.Left;
  //dlgReplace.Top := TfrmSrcEdit.Top;
  //dlgReplace.Left := TfrmSrcEdit.Left;
end;

procedure TfrmSrcEdit.FormActivate(Sender: TObject);
begin
  formId := formSrcEdit;
end;

procedure TfrmSrcEdit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  propFlagModules[0] := 0;
end;

procedure TfrmSrcEdit.FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  case Key of
    VK_F12: frmMain.Show;
  end;
end;

procedure TfrmSrcEdit.cmbPropChange(Sender: TObject);
begin
  caption := programName + ' ' + programVersion + ' - Source code editor (' + filename + ')';
  oldLanguage := cmbLanguage.ItemIndex;
end;

//procedure TfrmSrcEdit.FindDialogFind(Sender: TObject);
//begin
//  if (editor.Text = '') then
//    Exit;
//  if (FindDialog.FindText = '') then
//    Exit;
//  if not (frMatchCase in FindDialog.Options) then
//    begin
//      FTextToSearch:=LowerCase(editor.Text);
//      FTextToFind:=LowerCase(FindDialog.FindText);
//    end
//  else
//    begin
//      FTextToSearch:=editor.Text;
//      FTextToFind:=FindDialog.FindText;
//    end;
//  if SuccessfulSearch then
//    HighlightFoundText
//  else ShowNotFoundDlg;
//end;

procedure TfrmSrcEdit.editorChange(Sender: TObject);
begin
  isEdit := true;
  if Pos(' *', caption) = 0 then
    caption := caption + ' *';
end;

procedure TfrmSrcEdit.editorClick(Sender: TObject);
var
  p1 : TPoint;
begin
  p1 := editor.LogicalCaretXY;
  sbEditor.Panels[0].Text := 'Line: ' + IntToStr(p1.y) + ' / Column: ' + IntToStr(p1.x);
end;

procedure TfrmSrcEdit.cmbLanguageChange(Sender: TObject);
var
  newLanguage : byte;
  tempFilename, tempExt : string;
  number : short;
  i, cnt : byte;
begin
  if isEdit then begin
    newLanguage := cmbLanguage.ItemIndex;
    cmbLanguage.ItemIndex := oldLanguage;
    SaveFileAsProc(sender);
    cmbLanguage.ItemIndex := newLanguage;
  end;

  //Atari BASIC / Turbo BASIC XL
  //Mad Pascal
  //Action! / Effectus
  //FastBasic (full version)
  //FastBasic (integer only version)
  //Mad Assembler (MADS)
  //CC65
  //KickC
  //MAC/65
  //Other

  //_ATARI_BASIC    = 0;
  //_TURBO_BASIC_XL = 1;
  //_MAD_PASCAL     = 2;
  //_ACTION         = 3;
  //_FAST_BASIC     = 4;
  //_MADS           = 5;
  //_MAC65          = 6;
  //_CC65           = 7;
  //_KICKC          = 8;

  lblOutput02.Visible := false;
  memoMads.Enabled := false;
  memoMads.Visible := false;

  case cmbLanguage.ItemIndex of
    0: begin
       filename := getDir + 'examples\code0.lst';
       lblOutput.Caption := 'Atari BASIC output';
       btnCompile.Hint := 'Compile Atari BASIC code';
       btnCompileRun.Hint := 'Compile and run Atari BASIC code';
     end;
    1: begin
       filename := getDir + 'examples\code0.pas';
       lblOutput.Caption := 'Mad Pascal output';
       btnCompile.Hint := 'Compile Mad Pascal code';
       btnCompileRun.Hint := 'Compile and run Mad Pascal code';
       lblOutput02.Visible := true;
       memoMads.Enabled := true;
       memoMads.Visible := true;
     end;
    2: begin
       //with cmbProp do begin
       //  Clear;
       //  Items.Add('Action! mode');
       //  Items.Add('Effectus mode');
       //  ItemIndex := 1;
       //end;
       filename := getDir + 'examples\code0.eff';
       lblOutput.Caption := 'Action!/Effectus output';
       btnCompile.Hint := 'Compile Action!/Effectus code';
       btnCompileRun.Hint := 'Compile and run Action!/Effectus code';
     end;
    3 : begin
       with cmbProp do begin
         Clear;
         Items.Add('FastBasic (full version)');
         Items.Add('FastBasic (integer only version)');
         ItemIndex := 0;
       end;
       filename := getDir + 'examples\code0.bas';
       lblOutput.Caption := 'FastBasic output';
       btnCompile.Hint := 'Compile FastBasic code';
       btnCompileRun.Hint := 'Compile and run FastBasic code';
     end;
    4: begin
       filename := getDir + 'examples\code0.asm';
       lblOutput.Caption := 'Mad Assembler (MADS) output';
       btnCompile.Hint := 'Compile Mad Assembler (MADS) code';
       btnCompileRun.Hint := 'Compile and run Mad Assembler (MADS) code';
     end;
    5: begin
       filename := getDir + 'examples\code0.c';
       lblOutput.Caption := 'CC65 output';
       btnCompile.Hint := 'Compile CC65 code';
       btnCompileRun.Hint := 'Compile and run CC65 code';
     end;
    6: begin
       filename := getDir + 'examples\code0.c';
       lblOutput.Caption := 'KickC output';
       btnCompile.Hint := 'Compile KickC code';
       btnCompileRun.Hint := 'Compile and run KickC code';
     end;
    7: begin
       filename := getDir + 'examples\code0.asm';
       lblOutput.Caption := 'MAC/65 output';
       btnCompile.Hint := 'Compile MAC/65 code';
       btnCompileRun.Hint := 'Compile and run MAC/65 code';
     end;
  end;

  cmbProp.Enabled := cmbLanguage.ItemIndex = 3;
  cmbProp.Visible := cmbProp.Enabled;

  // Check if file exists. If not, that file is candidate for user file.
  repeat
    tempFilename := ExtractFileNameWithoutExt(filename);
    tempExt := ExtractFileExt(filename);
    cnt := 0;
    if (IsNumber(tempFilename, length(tempFilename))) then begin
      for i := length(tempFilename) downto 1 do begin
//        number := tempFilename[i];
        Inc(cnt);
//        showmessage(tempFilename[i] + ' *** ' + inttostr(cnt));
        if not IsNumber(tempFilename[i]) then begin
          number := StrToInt(copy(tempFilename, length(tempFilename) - cnt + 2, cnt)) + 1;
          break;
        end;
      end;
//      number := StrToInt(copy(tempFilename, length(tempFilename), 1)) + 1;
//      showmessage('final number = '  + inttostr(number));
//      filename := copy(tempFilename, 1, length(tempFilename) - 1) + IntToStr(number) + tempExt;
        filename := copy(tempFilename, 1, length(tempFilename) - cnt + 1) +
                    IntToStr(number) + tempExt;
    end;
  until not FileExists(filename);

  //// Check for Action!/Effectus
  //if cmbLanguage.ItemIndex = 2 then begin
  //  btnCompile.Enabled := cmbProp.ItemIndex = 1;
  //  btnCompileRun.Enabled := cmbProp.ItemIndex = 1;
  //end
  // Other languages
//  else begin
    btnCompile.Enabled := (cmbLanguage.ItemIndex <> 7) and (cmbLanguage.ItemIndex <> 8);
    btnCompileRun.Enabled := (cmbLanguage.ItemIndex <> 7) and (cmbLanguage.ItemIndex <> 8);
//  end;

  if not isMemoToEditor then
    editor.Lines.Clear;

  memoMP.Lines.Clear;
  memoMads.Lines.Clear;
  caption := programName + ' ' + programVersion + ' - Source code editor (' + filename + ')';
  oldLanguage := cmbLanguage.ItemIndex;
end;

procedure TfrmSrcEdit.dlgFindFind(Sender: TObject);
var
  txtToSearch, txtToFind : string;
  p : integer;
begin
//  dlgFind.Options:= dlgFind.Options + [frWholeWord, frHideEntireScope, frFindNext];
  editor.SelStart := 0;
  editor.HideSelection := False;

  if (frDown in dlgFind.Options) then begin
    txtToSearch := editor.Text;
    txtToFind := dlgFind.FindText;
  end
  else begin
    txtToSearch := ReverseString(editor.Text);
    txtToFind := ReverseString(dlgFind.FindText);
  end;

  if (frMatchCase in dlgFind.Options) then
    p := PosEx(txtToFind, txtToSearch)
  else
    p := PosEx(Uppercase(txtToFind), Uppercase(txtToSearch));

  case (p > 0) of
    false: begin
             ShowMessageFmt('The text "%s" was not found',[dlgFind.FindText]);
    //           editor.SelText.SelLength:=0;
           end;
    true: begin
//          if (frDown in dlgFind.Options)
//           then editor.SelStart:= Pred(p)
//          else editor.SelStart:= Length(editor.Text) - p - Pred(Length(txtToFind));
      //      editor.SelText.SelLength:= Length(txtToFind);

            editor.SelStart := p;
          end;
  end;

  editor.SetFocus();
//      dlgFind.CloseDialog;

//SynEdit1.CaretXY := SynEdit1.NextWordPos;
//SynEdit1.SelectWord;
end;

procedure TfrmSrcEdit.dlgReplaceReplace(Sender: TObject);
var
  k : integer;
  opt : TSynSearchOptions;
begin
  with Sender as TReplaceDialog do begin
    opt := [ssoReplace];
    k := editor.SearchReplaceEx(FindText, ReplaceText, opt, Position);
    if k >= 0 then
      editor.SetFocus()
    else
      Beep();
  end;
end;

procedure TfrmSrcEdit.CloseFileProc(Sender: TObject);
begin
  isSaveAs := true;
  filename := '';
  editor.Lines.Clear;
  caption := programName + ' ' + programVersion + ' - Source code editor';
end;

procedure TfrmSrcEdit.MenuItem12Click(Sender: TObject);
begin
  filename := '';
end;

procedure TfrmSrcEdit.CloseProc(Sender: TObject);
begin
  Close;
end;

procedure TfrmSrcEdit.PasteCodeProc(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    formGraph : begin
      frmGraphGen := TfrmGraphGen.Create(Self);
      with frmGraphGen do
        try
          ShowModal;
        finally
          Free;
        end;
    end;
    formPmg : begin
      frmPmgGen := TfrmPmgGen.Create(Self);
      with frmPmgGen do
        try
          ShowModal;
        finally
          Free;
        end;
    end;
    formFont : begin
      frmFontSetGen := TfrmFontSetGen.Create(Self);
      with frmFontSetGen do
        try
          ShowModal;
        finally
          Free;
        end;
    end;
    formAntic2 : begin
      frmAntic2Gen := TfrmAntic2Gen.Create(Self);
      with frmAntic2Gen do
        try
          ShowModal;
        finally
          Free;
        end;
    end;
    formAntic6 : begin
      frmAntic6Gen := TfrmAntic6Gen.Create(Self);
      with frmAntic6Gen do
        try
          ShowModal;
        finally
          Free;
        end;
    end;
    formDisplayList : begin
      frmDisplayListGen := TfrmDisplayListGen.Create(Self);
      with frmDisplayListGen do
        try
          ShowModal;
        finally
          Free;
        end;
    end;
    formAntic4 : begin
      frmAntic4Gen := TfrmAntic4Gen.Create(Self);
      with frmAntic4Gen do
        try
          ShowModal;
        finally
          Free;
        end;
    end;
    formAnimator : begin
      frmAnimGen := TfrmAnimGen.Create(Self);
      with frmAnimGen do
        try
          ShowModal;
        finally
          Free;
        end;
    end;
    formAntic3 : begin
      frmAntic3Gen := TfrmAntic3Gen.Create(Self);
      with frmAntic3Gen do
        try
          ShowModal;
        finally
          Free;
        end;
    end;
  end;
end;

procedure TfrmSrcEdit.CopyProc(Sender: TObject);
begin
  editor.CopyToClipboard;
end;

procedure TfrmSrcEdit.PasteProc(Sender: TObject);
begin
  editor.PasteFromClipboard;
end;

procedure TfrmSrcEdit.CutProc(Sender: TObject);
//var p1, p2: TPoint;
begin
  editor.CutToClipboard;

  //p1 := editor.LogicalCaretXY;
  //
  //editor.TextBetweenPoints[ Point(p1.y,p1.x), Point(p1.y,p1.x + 255)] :=
  //  UpperCase(editor.TextBetweenPoints[ Point(p1.y,p1.x), Point(p1.y,p1.x + 255)]);
end;

procedure TfrmSrcEdit.UndoProc(Sender: TObject);
begin
  editor.Undo;
end;

procedure TfrmSrcEdit.RedoProc(Sender: TObject);
begin
  editor.Redo;
end;

procedure TfrmSrcEdit.SettingsProc(Sender: TObject);
begin
  //showmessage(ExtractFileDir(filename));
  //showmessage(ExtractFilePath(ExcludeTrailingPathDelimiter(filename)));
  //showmessage(ExtractFilePath(ExpandFileName(filename)));

  frmSrcSettings := TfrmSrcSettings.Create(Self);
  with frmSrcSettings do
    try
      ShowModal;
    finally
      Free;
    end;

  SetEditor;
end;

procedure TfrmSrcEdit.OpenFileProc(Sender: TObject);
begin
  //Atari BASIC / Turbo BASIC XL
  //Mad Pascal
  //Action! / Effectus
  //FastBasic (full version)
  //FastBasic (integer only version)
  //Mad Assembler (MADS)
  //CC65
  //KickC
  //MAC/65
  //Other

  case cmbLanguage.ItemIndex of
    0: frmMain.dlgOpen.Filter :=
         'Atari BASIC / Turbo BASIC XL listed files (*.lst)|*.lst|All files (*.*)|*.*';
    1: frmMain.dlgOpen.Filter :=
         'Pascal files (*.pas)|*.pas|All files (*.*)|*.*';
    2: frmMain.dlgOpen.Filter :=
         'Action!/Effectus files (*.act, *.eff)|*.act;*.eff|All files (*.*)|*.*';
    3: frmMain.dlgOpen.Filter :=
         'FastBasic files (*.bas)|*.bas|All files (*.*)|*.*';
    4: frmMain.dlgOpen.Filter :=
         'Mad Assembler (MADS) files (*.asm)|*.asm|All files (*.*)|*.*';
    5: frmMain.dlgOpen.Filter :=
         'CC65 files (*.c)|*.c|All files (*.*)|*.*';
    6: frmMain.dlgOpen.Filter :=
         'KickC files (*.c)|*.c|All files (*.*)|*.*';
    7: frmMain.dlgOpen.Filter :=
         'MAC/65 files (*.asm)|*.asm|All files (*.*)|*.*';
  end;

  if frmMain.dlgOpen.Execute then begin
    isSaveAs := false;
    isEdit := false;
    filename := frmMain.dlgOpen.Filename;
    editor.Lines.LoadFromFile(filename);
    caption := programName + ' ' + programVersion + ' - Source code editor (' + filename + ')';
  end;
end;

procedure TfrmSrcEdit.SaveProc(Sender: TObject);
var
  srcLines : TStringList;
  i : word;
begin
  if cmbLineBreak.ItemIndex = 0 then begin
    //    editor.Lines.LineBreak := #$9b;
    //    editor.Lines.TextLineBreakStyle:=tlbsCR;
    editor.Lines.SaveToFile(filename);
  end
  else begin
    srcLines := TStringList.Create;
    try
      srcLines.LineBreak := #$9b;
      for i := 0 to editor.Lines.Count - 1 do
        srcLines.Add(editor.Lines[i]);

      srcLines.SaveToFile(filename);
    finally
      if Assigned(srcLines) then
        FreeAndNil(srcLines);
    end;
  end;
  isEdit := false;
  caption := programName + ' ' + programVersion + ' - Source code editor (' + filename + ')';
  sbEditor.Panels[1].Text := 'File ' + filename + ' saved';
end;

procedure TfrmSrcEdit.SaveFileProc(Sender: TObject);
begin
  //if editor.Modified then begin
  //  try
  //    editor.Lines.SaveToFile(filename);
  //  except
  //    SaveAsClick(Sender);
  //  end;
  //end;
//  srcLines := TStringList.Create;

  //Atari BASIC / Turbo BASIC XL
  //Mad Pascal
  //Action! / Effectus
  //FastBasic (full version)/FastBasic (integer only version)
  //Mad Assembler (MADS)
  //CC65
  //KickC
  //MAC/65

  if isSaveAs then
    SaveFileAsProc(Sender)
  else begin
    SaveProc(Sender);
//    editor.Lines.LineBreak := #$9b;
//    editor.Lines.TextLineBreakStyle:=tlbsCR;
//    editor.Lines.SaveToFile(filename);

    //srcLines.LineBreak := #$9b;
    //for i := 0 to editor.Lines.Count - 1 do begin
    //  srcLines.Add(editor.Lines[i]);
    //end;
    //srcLines.SaveToFile(filename);
    //srcLines.Free;
    //isEdit := false;
  end;
//  caption := programName + ' ' + programVersion + ' - Source code editor (' + filename + ')';
//  sbEditor.Panels[1].Text := 'File ' + filename + ' saved';
end;

procedure TfrmSrcEdit.SaveFileAsProc(Sender: TObject);
begin
  case cmbLanguage.ItemIndex of
    0 : begin
      frmMain.dlgSave.Title := 'Save Atari BASIC listing as';
      frmMain.dlgSave.Filter := 'Atari BASIC / Turbo BASIC XL listed files (*.lst)|*.lst|All files (*.*)|*.*';
    end;
    1 : begin
      frmMain.dlgSave.Title := 'Save Mad Pascal listing as';
      frmMain.dlgSave.Filter := 'Pascal files (*.pas)|*.pas|All files (*.*)|*.*';
    end;
    2 : begin
      frmMain.dlgSave.Title := 'Save Action!/Effectus listing as';
      frmMain.dlgSave.Filter := 'Action!/Effectus files (*.act, *.eff)|*.act;*.eff|All files (*.*)|*.*';
    end;
    3 : begin
      frmMain.dlgSave.Title := 'Save FastBasic listing as';
      frmMain.dlgSave.Filter := 'FastBasic files (*.bas)|*.bas|All files (*.*)|*.*';
    end;
    4 : begin
      frmMain.dlgSave.Title := 'Save Mad Assembler (MADS) listing as';
      frmMain.dlgSave.Filter := 'Mad Assembler (MADS) files (*.asm)|*.asm|All files (*.*)|*.*';
    end;
    5 : begin
      frmMain.dlgSave.Title := 'Save CC65 listing as';
      frmMain.dlgSave.Filter := 'CC65 files (*.c)|*.c|All files (*.*)|*.*';
    end;
    6 : begin
      frmMain.dlgSave.Title := 'Save KickC listing as';
      frmMain.dlgSave.Filter := 'KickC files (*.c)|*.c|All files (*.*)|*.*';
    end;
    7 : begin
      frmMain.dlgSave.Title := 'Save MAC/65 listing as';
      frmMain.dlgSave.Filter := 'MAC/65 files (*.asm)|*.asm|All files (*.*)|*.*';
    end;
  end;

  frmMain.dlgSave.Filename := filename;

  if frmMain.dlgSave.Execute then begin
    isSaveAs := false;
    filename := frmMain.dlgSave.Filename;
    SaveProc(Sender);
//    editor.Lines.LineBreak := #$9b;
//    editor.Lines.TextLineBreakStyle:=tlbsCR;
//    editor.Lines.SaveToFile(filename);
//    caption := programName + ' ' + programVersion + ' - Source code editor (' + filename + ')';
    isSaveAsFalse := false;
  end
  else
    isSaveAsFalse := true;
end;

procedure TfrmSrcEdit.CompileParseProc(Sender: TObject);
begin
  Compile(Sender, TToolButton(Sender).Tag);
end;

procedure TfrmSrcEdit.FindTextProc(Sender: TObject);
begin
  dlgFind.Execute;
//  if dlgFind.Execute then
//     dlgFind.CloseDialog;
end;

procedure TfrmSrcEdit.ReplaceTextProc(Sender: TObject);
begin
  dlgReplace.Execute();
end;

procedure TfrmSrcEdit.NewFileProc(Sender: TObject);
begin
  editor.Lines.Clear;
  cmbLanguageChange(Sender);
end;

procedure TfrmSrcEdit.SetParams(AProcess : TProcess; props, flags : TStringList);
var
  i : byte;
  param, param2 : string;
begin
  //if props = propMadPascal then
  //  showmessage('propMadPascal')
  //else if props = propMadsMP then
  //  showmessage('propMadsMP')
  //else
  //  showmessage('other prop');

  for i := 0 to props.Count - 1 do
    if flags[i][1] = '1' then begin
      param := ExtractDelimited(1, props[i], ['=']);
      param2 := ExtractDelimited(1, param, [':']);
//      showmessage('props[i] = ' + props[i] + ' *** param ' + param2);

      if flags[i].Length > 1 then begin
        AProcess.Parameters.Add(param2 + ':' + Copy(flags[i], 2, flags[i].Length - 1));
//        showmessage('1.) ' + param2 + ':' + Copy(flags[i], 2, flags[i].Length - 1));
//        showmessage(inttostr(flags[i].Length) + ' *** 1.) ' + param2 + Copy(flags[i], 2, flags[i].Length - 1));
//        AProcess.Parameters.Add(Copy(flags[i], 2, flags[i].Length - 1));
      end
      else begin
        AProcess.Parameters.Add(param);
//        showmessage('2.) ' + param);
      end;
    end;
end;

procedure TfrmSrcEdit.Compile(Sender : TObject; tagIndex : byte);
var
  AProcess : TProcess;
  filenameTemp, fbType : string;
  strFastBasicUserLocation02 : string;
begin
  // Check source code file modify status
  if isEdit then begin
    SaveFileAsProc(sender);
    if isSaveAsFalse then Exit;
  end else begin
    if (filename = '') or (editor.Lines.Count < 2) then begin
      ShowMessage('There is no source code file to be edited!');
      Exit;
    end;
  end;

  // Run compiler
  AProcess := TProcess.Create(nil);

  case cmbLanguage.ItemIndex of
    { Atari BASIC, Turbo BASIC XL
     ---------------------------------------------------------------------------}
    0: begin
      if isAtariBASICUserLocation then
        AProcess.Executable := strAtariBASICUserLocation + DirectorySeparator + 'basicParser'
      else
        AProcess.Executable := getDir + 'bin' + DirectorySeparator + 'basic_parser' +
                               DirectorySeparator + 'basicParser';

      SetParams(AProcess, propAtariBASIC, propFlagAtariBASIC);
      AProcess.Parameters.Add(AnsiQuotedStr(filename, '"'));
      AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
      AProcess.Execute;

      memoMP.Lines.LoadFromStream(AProcess.Stderr);
    end;
    { Mad Pascal
     ---------------------------------------------------------------------------}
    1: begin
       // mp projects\titler\titler.pas -o
       // mads.exe projects\titler\titler.a65 -x -i:base

       //if isMadsAddLib_i then
       //  AProcess.Parameters.Add('-i:' + getDir + 'bin' + DirectorySeparator + 'mp' + DirectorySeparator + strMadsAddLib_i);

       if isMadPascalUserLocation then
         AProcess.Executable := strMadPascalUserLocation + DirectorySeparator + 'mp'
       else
         AProcess.Executable := getDir + 'bin' + DirectorySeparator + 'mp' +
                                DirectorySeparator + 'mp';

       //  AProcess.Parameters.Add(AnsiQuotedStr(GetCurrentDir + '\bin\roto.pas', '"'));
       AProcess.Parameters.Add(AnsiQuotedStr(filename, '"'));
       SetParams(AProcess, propMadPascal, propFlagMadPascal);
       AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
       AProcess.Execute;
       memoMP.Lines.LoadFromStream(AProcess.Output);
       AProcess.Free;

       AProcess := TProcess.Create(nil);

       if isMadPascalUserLocation then
         AProcess.Executable := strMadsUserLocation + DirectorySeparator + 'mads'
       else
         AProcess.Executable := getDir + 'bin' + DirectorySeparator + 'mp' + DirectorySeparator + 'mads';

       AProcess.Parameters.Add(AnsiQuotedStr(ExtractFileNameWithoutExt(filename) + '.a65', '"'));
       SetParams(AProcess, propMadsMP, propFlagMadsMP);

       AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
       AProcess.Execute;

       memoMads.Lines.LoadFromStream(AProcess.Output);
     end;
    { Effectus/Action!
     ---------------------------------------------------------------------------}
    2: begin
       if isEffectusUserLocation then
         AProcess.Executable := strEffectusUserLocation + DirectorySeparator + 'effectus'
       else
         AProcess.Executable := getDir + 'bin' + DirectorySeparator + 'effectus';

//         ShowMessage(AProcess.Executable);

       AProcess.Parameters.Add(AnsiQuotedStr(filename, '"'));
//         AProcess.Parameters.Add('-r:' + getDir + 'bin' + DirectorySeparator +
//                                 'effectus' + DirectorySeparator + 'lib' + DirectorySeparator);

       SetParams(AProcess, propEffectus, propFlagEffectus);

       AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
       AProcess.Execute;
       AProcess.Free;

       AProcess := TProcess.Create(nil);

//         showmessage('mp');

//         if isMadPascalUserLocation then
//           AProcess.Executable := strMadPascalUserLocation + DirectorySeparator + 'mp'
//         else
         AProcess.Executable := getDir + 'bin' + DirectorySeparator + 'mp' +
                                DirectorySeparator + 'mp';

       AProcess.Parameters.Add(AnsiQuotedStr(ExtractFileNameWithoutExt(filename) + '.pas', '"'));

       SetParams(AProcess, propMadPascal, propFlagMadPascal);

       AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
//         ShowMessage(AProcess.Executable + ' * ' + AProcess.Parameters.Text);
       AProcess.Execute;
       memoMP.Lines.LoadFromStream(AProcess.Output);
       AProcess.Free;

       AProcess := TProcess.Create(nil);

//         if isMadPascalUserLocation then
//           AProcess.Executable := strMadsUserLocation + DirectorySeparator + 'mads'
//         else
       AProcess.Executable := getDir + 'bin' + DirectorySeparator + 'mp' + DirectorySeparator + 'mads';

       AProcess.Parameters.Add(AnsiQuotedStr(ExtractFileNameWithoutExt(filename) + '.a65', '"'));
       SetParams(AProcess, propMadsMP, propFlagMadsMP);

       AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
       AProcess.Execute;

       memoMads.Lines.LoadFromStream(AProcess.Output);

//         ShowMessage(AProcess.Executable + ' * ' + AProcess.Parameters.Text);

//         showmessage('Param - ' + getDir + 'bin' + DirectorySeparator + 'effectus' + DirectorySeparator + 'effectus');
//         memoMP.Lines.LoadFromStream(AProcess.Output);
//         memoMads.Lines.LoadFromStream(AProcess.Stderr);
     end;
    { FastBasic
     ---------------------------------------------------------------------------}
    3: begin
       if cmbProp.ItemIndex = 0 then
         fbType := 'fb'
       else
         fbType := 'fb-int';

//         showmessage(inttostr(cmbProp.ItemIndex));

       isFastBasicUserLocation := true;
       strFastBasicUserLocation02 := strFastBasicUserLocation +
                                     DirectorySeparator + fbType + '.bat';

       AProcess.Executable := strFastBasicUserLocation02;
       AProcess.Parameters.Add(filename);

//         showmessage(strFastBasicUserLocation02 + ' *** ' + filename);

       SetParams(AProcess, propFastBasic, propFlagFastBasic);

       AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
       AProcess.Execute;

       memoMP.Lines.LoadFromStream(AProcess.Stderr);
     end;
    { Mad Assembler (Mads)
     ---------------------------------------------------------------------------}
    4: begin
       if isMadPascalUserLocation then
         AProcess.Executable := strMadsUserLocation02 + DirectorySeparator + 'mads'
       else
         AProcess.Executable := getDir + 'bin' + DirectorySeparator + 'mads' +
                                DirectorySeparator + 'mads';

       AProcess.Parameters.Add(AnsiQuotedStr(filename, '"'));
       SetParams(AProcess, propMADS, propFlagMADS);
       AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
       AProcess.Execute;
       memoMP.Lines.LoadFromStream(AProcess.Output);
     end;
    { CC65
     ---------------------------------------------------------------------------}
    5: begin
       isCC65UserLocation := true;
       AProcess.Executable := strCC65UserLocation + DirectorySeparator + 'cl65';
       AProcess.Parameters.Add('-tatari');
       AProcess.Parameters.Add('-O');
       AProcess.Parameters.Add(filename);
       AProcess.Parameters.Add('-o');
       AProcess.Parameters.Add(ExtractFileNameWithoutExt(filename) + '.xex');
       AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
       AProcess.Execute;
       memoMP.Lines.LoadFromStream(AProcess.Stderr);
     end;
    { KickC
     ---------------------------------------------------------------------------}
    6: begin
       isKickCUserLocation := true;
       //       AProcess.Executable := 'd:\atari\appl\kickc\bin\kickc.bat';
       AProcess.Executable := strKickCUserLocation + DirectorySeparator + 'kickc.bat';
//       debug('AProcess.Executable = ' + AProcess.Executable);
//       AProcess.Executable := 'd:' + DirectorySeparator + 'atari' + DirectorySeparator + 'appl' + DirectorySeparator +
//       'kickc' + DirectorySeparator + 'bin' + DirectorySeparator + 'kickc.bat';
//       filename := ExpandFileName(filename);
       filename := 'examples' + DirectorySeparator + ExtractFileName(filename);
//       ..\examples\atarixl\rasterbars.c
       AProcess.Parameters.Add(filename);
//       AProcess.Parameters.Add(AnsiQuotedStr(filename, '"'));

//       AProcess.Parameters.Add('-a');
//       debug(AnsiQuotedStr(filename, '"'));
//       AProcess.Parameters.Add(ExtractFileNameWithoutExt(filename) + '.xex');

       SetParams(AProcess, propKickC, propFlagKickC);

       AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
       AProcess.Execute;
       memoMP.Lines.LoadFromStream(AProcess.Output);
       memoMads.Lines.LoadFromStream(AProcess.Stderr);

//      ShellExecute(Handle, 'open', 'd:\atari\appl\kickc\bin\kickc.bat', nil, nil, SW_SHOWNORMAL) ;
//       ShellExecute(hWnd, NULL, 'd:\atari\appl\kickc\bin\kickc.bat', filename);
     end;
  end;

  if isOutputLog then
    case cmbLanguage.ItemIndex of
      0: memoMP.lines.SaveToFile(ExtractFilePath(ExpandFileName(filename)) + strAtariBASICOutput);
      1: begin
           memoMP.Lines.SaveToFile(ExtractFilePath(ExpandFileName(filename)) + strMadPascalOutput);
           memoMads.Lines.SaveToFile(ExtractFilePath(ExpandFileName(filename)) + strMadsOutput);
         end;
      2: memoMP.Lines.SaveToFile(ExtractFilePath(ExpandFileName(filename)) + strEffectusOutput);
      3: memoMP.Lines.SaveToFile(ExtractFilePath(ExpandFileName(filename)) + strFastBasicOutput);
      4: memoMP.Lines.SaveToFile(strMadsOutput);
      5: memoMP.Lines.SaveToFile(ExtractFilePath(ExpandFileName(filename)) + strCC65Output);
//      6: memoMP.Lines.SaveToFile(ExtractFilePath(ExpandFileName(filename)) + strCC65Output);
    end;

  // Clean up variables
  AProcess.Free;

//  showmessage(AProcess.Parameters);

  // Run file in Atari emulator
  if tagIndex = 8 then begin
    case cmbLanguage.ItemIndex of
      0: begin
        filenameTemp := ChangeFileExt(filename, '.bas');
        ShowFile(filenameTemp);
      end;
      1, 4: begin
        filenameTemp := ChangeFileExt(filename, '.obx');
        if not FileExistsUTF8(filenameTemp) then
          filenameTemp := ChangeFileExt(filename, '.xex');

        ShowFile(filenameTemp);
      end;
      2, 3, 5, 6: begin
        filenameTemp := ChangeFileExt(filename, '.xex');
        if not FileExistsUTF8(filenameTemp) then
          filenameTemp := ChangeFileExt(filename, '.obx');

        ShowFile(filenameTemp);
      end;
    end;
  end;
  memoMads.Enabled := true;
end;

procedure TfrmSrcEdit.MemoToEditor(Sender: TObject);
begin
  //Atari BASIC / Turbo BASIC XL
  //Action! / Effectus
  //Mad Pascal
  //FastBasic (full version)
  //FastBasic (integer only version)
  //Mad Assembler (MADS)
  //CC65
  //KickC

  case langIndex of
    _ATARI_BASIC: cmbLanguage.ItemIndex := 0;
    _TURBO_BASIC_XL: cmbLanguage.ItemIndex := 0;
    _MAD_PASCAL: cmbLanguage.ItemIndex := 1;
    _ACTION: begin
      cmbLanguage.ItemIndex := 2;
//      cmbProp.ItemIndex := 1;
    end;
    _FAST_BASIC: begin
      cmbLanguage.ItemIndex := 3;
      cmbProp.ItemIndex := 0;
    end;
    _MADS: cmbLanguage.ItemIndex := 4;
    _CC65: cmbLanguage.ItemIndex := 5;
    _KICKC: cmbLanguage.ItemIndex := 6;
    _MAC65: cmbLanguage.ItemIndex := 7;
  end;

  isEdit := false;
  isMemoToEditor := true;
  cmbLanguageChange(Sender);
  FormStyle := fsStayOnTop;
  ShowInTaskBar := stNever;
  Visible := True;
  ShowOnTop;
  SetFocus;
  SaveFileProc(Sender);
//  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
end;

procedure TfrmSrcEdit.ShowFile(aFile: string);
begin
  if FileExistsUTF8(aFile) then
    ShellExecute(Handle, 'open', PChar(afile), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmSrcEdit.SetEditor;
begin
  with editor do begin
    Font.Name := editorFont.Name;
    Font.Size := editorFont.Size;
    Font.Color:= editorFont.Color;
    Color := editorBackColor;
    LineHighlightColor := editorLineHighlightColor;
    SelectedColor := editorSelectedTextBackColor;
  end;
end;

//function TfrmSrcEdit.SuccessfulSearch: boolean;
//var
//  charsToMatch, startPosition: integer;
//  pc: PChar;
//FFindPosition: integer;
//FTextToSearch: string;
//FTextToFind: string;
//
//  procedure ResetPCharAndMatches; inline;
//  begin
//    charsToMatch:=Length(FTextToFind);
//    pc:=PChar(FTextToFind);
//  end;
//
//begin
//  if SameText(editor.SelText, FindDialog.FindText) then
//    startPosition:=editor.SelStart + editor.SelEnd + 1
//  else startPosition:=1;
//
//  Result:=False;
//  FFindPosition:=startPosition;
//  ResetPCharAndMatches;
//  while (FFindPosition <= Length(FTextToSearch)) and (charsToMatch > 0) do
//    begin
//      if (FTextToSearch[FFindPosition] = pc^) then
//        begin
//          Dec(charsToMatch);
//          Inc(pc);
//        end
//      else ResetPCharAndMatches;
//      if (charsToMatch = 0) then
//        Break;
//      Inc(FFindPosition);
//    end;
//  Result:=(charsToMatch = 0);
//end;

procedure TfrmSrcEdit.editorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  editorClick(Sender);
end;

//procedure TfrmSrcEdit.editorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
//begin
  //if (Shift = [ssCtrl]) and (Key = VK_J) then
  //  begin
  //    Key:=0;
  //    SearchForTextInMemo;
  //  end;

  //if (ssCtrl in Shift)and (Key = VK_F) then
  //  dlgFind.Execute
  //else if (ssCtrl in Shift)and (Key = VK_R) then
  //  dlgReplace.Execute;
//end;

end.

