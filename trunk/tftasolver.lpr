program tftasolver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,  strutils
  { you can add units after this }
  ,
  utftaformtftasolver,
  utftaexpression, utftalogik, utftaobject, sjspointertools, utftaformabout,
  utftaformdebugmessages;
  
var
   GlobalDebugLevel : integer;
   i                : longint;

//{$R tftasolver.res}

{$IFDEF WINDOWS}{$R tftasolver.rc}{$ENDIF}

begin

  GlobalDebugLevel := 0; {defaultwert}
  { Auslesen der Kommandozeilenparameter}
  for i:=1 to Paramcount do
  begin
    { DEBUGLEVEL }
    if ParamStr(i) = '--debuglevel' then GlobalDebugLevel := Numb2Dec(ParamStr(i+1),10);
  end;

  Application.Initialize;
  Application.CreateForm(TTFTAMainWindow, TFTAMainWindow);
  TFTAMainWindow.DEBUGLevel:=GlobalDebugLevel;
  TFTAMainWindow.pointerToApplication := Application;
  Application.Run;
end.

