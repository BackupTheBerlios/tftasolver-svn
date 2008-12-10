unit utftaformdebugmessages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls , Buttons;

type

  { TFormDebugMeldungen }

  TFormDebugMeldungen = class(TForm)
    MemoAusgabebereich: TMemo;
  private
    { private declarations }
  public
    { public declarations }
    procedure SchreibeMeldung(Parameter : ansistring) ;
  end; 

var
  FormDebugMeldungen: TFormDebugMeldungen;

implementation

{------------------------------------------------------------------------------
  Schreibt eine Meldung ins DebugFenster
------------------------------------------------------------------------------}
procedure TFormDebugMeldungen.SchreibeMeldung(Parameter : ansistring);
begin
  MemoAusgabebereich.Append(Parameter);
end;

initialization
  {$I utftaformdebugmessages.lrs}

end.

