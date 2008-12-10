unit utftaformabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    ButtonClose: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ButtonCloseClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormAbout: TFormAbout;

implementation

{ TFormAbout }


procedure TFormAbout.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;





initialization
  {$I utftaformabout.lrs}

end.

