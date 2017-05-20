program PVInputProject;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainWindow, SettingsWindow, DetachedHistoryWindow, InvokerSpellsWindow;

{$R *.res}

begin
  Application.Title:='PVInput';
  RequireDerivedFormResource:=True;

  Application.Initialize;

  Application.CreateForm(TPVInput, PVInput);
  Application.CreateForm(TDetachedHistoryForm, DetachedHistoryForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TInvokerSpellsForm, InvokerSpellsForm);

  Application.Run;
end.

