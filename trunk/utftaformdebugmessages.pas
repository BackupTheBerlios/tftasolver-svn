unit utftaformdebugmessages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls , Buttons;

type

  { TFormDebugMessage }

  TFormDebugMessage = class(TForm)
    MemoOutput: TMemo;
  private
    { private declarations }
  public
    { public declarations }
    procedure WriteMessage(Parameter : ansistring) ;
  end; 

var
  FormDebugMessage: TFormDebugMessage;

implementation

{------------------------------------------------------------------------------
  write a messageinto debug window
------------------------------------------------------------------------------}
procedure TFormDebugMessage.WriteMessage(Parameter : ansistring);
begin
  MemoOutput.Append(Parameter);
end;

initialization
  {$I utftaformdebugmessages.lrs}

end.

