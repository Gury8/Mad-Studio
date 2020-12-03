{
  Program name: Mad Studio
  Author: Boštjan Gorišek
  Release year: 2016 - 2020
}
program madstudio;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, anchordockpkg, common, main, graph, src_editor, pmg,
  colors, fonts, antic2, antic6, antic3, antic4, dl_editor, animator, byte_editor, viewer;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSrcEdit, frmSrcEdit);
  Application.CreateForm(TfrmColors, frmColors);
  Application.CreateForm(TfrmGraph, frmGraph);
  Application.CreateForm(TfrmPmg, frmPmg);
  Application.CreateForm(TfrmFonts, frmFonts);
  Application.CreateForm(TfrmAntic2, frmAntic2);
  Application.CreateForm(TfrmAntic3, frmAntic3);
  Application.CreateForm(TfrmAntic6, frmAntic6);
  Application.CreateForm(TfrmAntic4, frmAntic4);
  Application.CreateForm(TfrmDisplayList, frmDisplayList);
  Application.CreateForm(TfrmAnimator, frmAnimator);
  Application.CreateForm(TfrmByteEditor, frmByteEditor);
  Application.CreateForm(TfrmViewer, frmViewer);
  Application.Run;
end.

