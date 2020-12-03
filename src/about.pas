{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
  Unit: About
}
unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, lcltype, ExtCtrls, lclintf, DTAnalogClock, BCMaterialDesignButton;

type
  { TfrmAbout }
  TfrmAbout = class(TForm)
    btnClose : TBCMaterialDesignButton;
    DTAnalogClock1 : TDTAnalogClock;
    Image1: TImage;
    Image2: TImage;
    imgAppTitle: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15 : TLabel;
    Label16 : TLabel;
    Label17 : TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblVersion: TLabel;
    Label3: TLabel;
    lblReleaseYear: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    StaticText1: TStaticText;
    textEffectus : TStaticText;
    textBGRABitmap : TStaticText;
    textBGRAControls : TStaticText;
    textKickC : TStaticText;
    textMadsVer: TStaticText;
    textMadPascalVer: TStaticText;
    textBasicParserVer: TStaticText;
    textLazarus: TStaticText;
    textFreePascal: TStaticText;
    textFastBasic: TStaticText;
    textCC65: TStaticText;
    tabs: TPageControl;
    tabAbout: TTabSheet;
    tabCredits: TTabSheet;
    tabLicense: TTabSheet;
    tabReleaseNotes: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CloseWinProc(Sender: TObject);
    procedure LazarusClick(Sender: TObject);
    procedure MadsClick(Sender: TObject);
    procedure FPCClick(Sender: TObject);
    procedure stMouseEnter(Sender: TObject);
    procedure stMouseLeave(Sender: TObject);
    procedure MadPascalClick(Sender: TObject);
    procedure FastBasicClick(Sender: TObject);
    procedure CC65Click(Sender: TObject);
    procedure TBXLParserClick(Sender: TObject);
    procedure textBGRABitmapClick(Sender : TObject);
    procedure textBGRAControlsClick(Sender : TObject);
    procedure textEffectusClick(Sender : TObject);
    procedure KickCClick(Sender : TObject);
    procedure btnCloseMouseEnter(Sender : TObject);
    procedure btnCloseMouseLeave(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

uses
  common;

{ TfrmAbout }

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  lblVersion.Caption := programVersion;
  lblReleaseYear.Caption := '(c) 2020';
  textLazarus.Caption := 'Lazarus 2.0.8 x86_64-win64-win32/win64';
  textFreePascal.Caption := 'Free Pascal Compiler 3.0.4';
  textMadsVer.Caption := 'Mad Assembler 2.1.0 build 8';
  textMadPascalVer.Caption := 'Mad Pascal Compiler version 1.6.4';
  textBasicParserVer.Caption := 'TurboBasic XL Parser Tool 10';
  textFastBasic.Caption := 'FastBasic 4.4 - Fast BASIC interpreter';
  textCC65.Caption := 'cc65 - the 6502 C Compiler';
  textEffectus.Caption := 'Effectus 0.5.3 - Action! parser/cross-compiler';
  textKickC.Caption := 'KickC 0.8.4 - C compiler for optimized 6502 code';
  textBGRABitmap.Caption := 'BGRABitmap';
  textBGRAControls.Caption := 'BGRA Controls';
end;

procedure TfrmAbout.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
  end;
end;

procedure TfrmAbout.CloseWinProc(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.LazarusClick(Sender: TObject);
begin
  OpenURL('http://www.lazarus.freepascal.org');
end;

procedure TfrmAbout.MadsClick(Sender: TObject);
begin
  OpenURL('http://mads.atari8.info/');
end;

procedure TfrmAbout.FPCClick(Sender: TObject);
begin
  OpenURL('https://www.freepascal.org/');
end;

procedure TfrmAbout.MadPascalClick(Sender: TObject);
begin
  OpenURL('http://mads.atari8.info/doc/madpascal.html');
end;

procedure TfrmAbout.TBXLParserClick(Sender: TObject);
begin
  OpenURL('https://github.com/dmsc/tbxl-parser');
end;

procedure TfrmAbout.textBGRABitmapClick(Sender : TObject);
begin
  OpenURL('https://bgrabitmap.github.io/');
end;

procedure TfrmAbout.textBGRAControlsClick(Sender : TObject);
begin
  OpenURL('https://bgrabitmap.github.io/bgracontrols/');
end;

procedure TfrmAbout.textEffectusClick(Sender : TObject);
begin
  OpenURL('https://github.com/Gury8/effectus');
end;

procedure TfrmAbout.FastBasicClick(Sender: TObject);
begin
  OpenURL('https://github.com/dmsc/fastbasic');
end;

procedure TfrmAbout.CC65Click(Sender: TObject);
begin
  OpenURL('https://cc65.github.io');
end;

procedure TfrmAbout.KickCClick(Sender : TObject);
begin
  OpenURL('https://gitlab.com/camelot/kickc');
end;

procedure TfrmAbout.stMouseEnter(Sender: TObject);
begin
  TStaticText(Sender).Cursor := crHandPoint;
  TStaticText(Sender).Font.Style := [fsItalic, fsBold];
//  textMadsVer.Cursor := crHandPoint;
//  textMadsVer.Font.Style := [fsItalic, fsBold];
//  textMadsVer.Font.Color := clBlue;
end;

procedure TfrmAbout.stMouseLeave(Sender: TObject);
begin
  TStaticText(Sender).Font.Style := [fsBold];
//  textMadsVer.Font.Style := [fsBold];
end;

procedure TfrmAbout.btnCloseMouseEnter(Sender : TObject);
begin
  btnClose.NormalColor := $00CECECE;
  btnClose.NormalColorEffect := clWhite;
end;

procedure TfrmAbout.btnCloseMouseLeave(Sender : TObject);
begin
  btnClose.NormalColor := clWhite;
  btnClose.NormalColorEffect := clSilver;
end;

end.

