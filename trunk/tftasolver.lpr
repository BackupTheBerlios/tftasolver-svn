program tftasolver;

{$mode objfpc}{$H+}

// switch TESTMODE on or of globally by editing testmode.conf (found in main path
// of TFTASolver.tftasolver
{$INCLUDE testmode.conf}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,  strutils
  { you can add units after this }
  ,utftaformtftasolver,utftaexpression, utftalogic, utftaobject,
  sjspointertools, utftaformabout,
  {$IFDEF TESTMODE}
  utftaformdebugmessages,
  {$ENDIF}
  utftastringlist, TurboPowerIPro;
  

//{$R tftasolver.res}

{$IFDEF WINDOWS}{$R tftasolver.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TTFTAMainWindow, TFTAMainWindow);
  {$IFDEF TESTMODE}
    TFTAMainWindow.DEBUGLevel:=5;  { 5 is a default value, later to be detailled into different verbosities }
  {$ENDIF}
  TFTAMainWindow.pointerToApplication := Application;
  Application.Run;
end.

