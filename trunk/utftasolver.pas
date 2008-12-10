unit utftasolver;

{ §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  Author: Simon J. Schilling
  Email: sjschilling@gmx.net
  Year: 2008
  
  Freeware. Enjoy.
  
  §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ }

{$mode objfpc}{$H+}

interface

{ ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
uses
  { System Units }
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls , Buttons,
  { eigene Units }
  tftaausdruck, uFormAbout, uformdebugmeldungen;

{ ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
type

  { Hauptfenster, das was man als Nutzer vom PRogramm primaer sieht. }
  { ######################################################################## }

  { THauptfenster }

  THauptfenster = class(TForm)
    BitBtnAnzeigenStruktur: TBitBtn;
    BitBtnAnzeigenStrukturErgebnis: TBitBtn;
    BitBtnAusdruckBerechnen: TBitBtn;
    BitBtnSichern: TBitBtn;
    BitBtnLaden: TBitBtn;
    BitBtnAusdruckUbernehmen: TBitBtn;
    BitBtnErgebnisSichern: TBitBtn;
    ButtonAbout: TButton;
    Label1: TLabel;
    MemoEingabeAusdruck: TMemo;
    MemoDEBUG: TMemo;
    MemoEingabeAusdruck1: TMemo;
    ProgressBar1: TProgressBar;
    Shape1: TShape;
    StatusBarHauptfenster: TStatusBar;
    TreeViewAusdruckStruktur: TTreeView;
    TreeViewErgebnisStruktur: TTreeView;
    procedure BitBtnAnzeigenStrukturErgebnisClick(Sender: TObject);
    procedure BitBtnAusdruckBerechnenClick(Sender: TObject);
    procedure BitBtnAusdruckUbernehmenClick(Sender: TObject);
    procedure BitBtnAnzeigenStrukturClick(Sender: TObject);
    procedure BitBtnErgebnisSichernClick(Sender: TObject);
    procedure BitBtnLadenClick(Sender: TObject);
    procedure BitBtnSichernClick(Sender: TObject);
    procedure ButtonAboutClick(Sender: TObject);
  private
    { private declarations }
    vDEBUGFenster : TFormDebugMeldungen;
    vDEBUGLevel : integer;
    function HoleDEBUGLevel : integer;
    procedure SetzeDEBUGLevel(Parameter : integer);
  public
    { public declarations }
    TemporalerAusdruck : TTFTAAusdruck;
    property DEBUGLevel : integer read HoleDEBUGLevel write SetzeDEBUGLevel;
    procedure SchreibeDEBUGMeldung(Parameter: ansistring);
  end; 

{ ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
var
  Hauptfenster: THauptfenster;

{ ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
implementation


{ THauptfenster }

{------------------------------------------------------------------------------
  Lese Property vDEBUGLevel
------------------------------------------------------------------------------}
function THauptfenster.HoleDEBUGLevel : integer;
begin
  HoleDEBUGLevel := vDEBUGLevel;
end;
{------------------------------------------------------------------------------
  Schreibe Property vDEBUGLevel
------------------------------------------------------------------------------}
procedure THauptfenster.SetzeDEBUGLevel(Parameter : integer);
begin
  vDEBUGLevel := Parameter;
  if vDEBUGFenster = Nil then
  begin
    vDEBUGFenster:= TFormDebugMeldungen.Create(Application);
    vDEBUGFenster.Show;
  end;
  if DEBUGLevel>0 then SchreibeDEBUGMeldung('Bis auf Weiteres herrscht DebugLevel '+IntToStr(DEBUGLevel));
  MemoDEBUG := vDEBUGFenster.MemoAusgabebereich;
end;
{------------------------------------------------------------------------------
  Schreibt eine Meldung ins DebugFenster
------------------------------------------------------------------------------}
procedure THauptfenster.SchreibeDEBUGMeldung(Parameter : ansistring);
begin
  if vDEBUGFenster <> Nil then vDEBUGFenster.SchreibeMeldung(Parameter);
end;

{ ##############################################################################
  ##############################################################################
  THauptfenster.BitBtnAusdruckUbernehmenClick steuert die Uebernahme der
  Eingaben und die Auswertung derselben.
  ##############################################################################
  #############################################################################}
procedure THauptfenster.BitBtnAusdruckUbernehmenClick(Sender: TObject);
begin
  MemoEingabeAusdruck.Lines.Text := Trim(MemoEingabeAusdruck.Lines.Text)  ;
  TemporalerAusdruck := TTFTAAusdruck.Create;
  TemporalerAusdruck.SetzeDEBUGMemo(MemoDEBUG);
  TemporalerAusdruck.EingabeUbernehmen(Trim(MemoEingabeAusdruck.Lines.Text));
  TreeViewAusdruckStruktur.Items.Clear ;
  TemporalerAusdruck.EingabeAuswerten(TreeViewAusdruckStruktur.Items);
end;

procedure THauptfenster.BitBtnAnzeigenStrukturErgebnisClick(Sender: TObject);
begin
  TreeViewErgebnisStruktur.Visible := TreeViewErgebnisStruktur.Visible xor True;
  if BitBtnAnzeigenStrukturErgebnis.caption = 'Struktur anzeigen' then
     BitBtnAnzeigenStrukturErgebnis.caption := 'Struktur ausblenden'
  else
     BitBtnAnzeigenStrukturErgebnis.caption := 'Struktur anzeigen' ;
end;

procedure THauptfenster.BitBtnAusdruckBerechnenClick(Sender: TObject);
var i : longword;
begin
  for i := 1 to 1000000 do
      TreeViewErgebnisStruktur.Items.Add(nil,IntToStr(i));
end;



  //MemoDEBUG2.Clear;
  //x := TTFTAEreignisliste.Create;

  //for j:=0 to 20 do
  //begin
    //x.Add(Pointer(TTFTATemporalerTerm.Create));
    //TTFTATemporalerTerm(x.Last).SetzeLaenge(j);
  //end;
  //for j:=0 to 20 do
  //begin
    //MemoDEBUG2.Append('Liste ' + IntToStr(j+1) + ': ' +
               //IntToStr(Longword(Pointer(x.Items[j]))) + ' --- ' +
               //IntToStr(TTFTATemporalerTerm(x.Items[j]).HoleLaenge));
  //end;




{ ##############################################################################
  ##############################################################################
  THauptfenster.BitBtnAnzeigenStrukturClick schaltet die
  Baumanzeige ein und aus.
  ##############################################################################
  #############################################################################}
procedure THauptfenster.BitBtnAnzeigenStrukturClick(Sender: TObject);
begin

  TreeViewAusdruckStruktur.Visible := TreeViewAusdruckStruktur.Visible xor True;
  if BitBtnAnzeigenStruktur.caption = 'Struktur anzeigen' then
     BitBtnAnzeigenStruktur.caption := 'Struktur ausblenden'
  else
     BitBtnAnzeigenStruktur.caption := 'Struktur anzeigen' ;

end;

procedure THauptfenster.BitBtnErgebnisSichernClick(Sender: TObject);
begin

end;



{ ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
procedure THauptfenster.BitBtnLadenClick(Sender: TObject);
begin

  StatusBarHauptfenster.Panels.Items[1].Text:='ACHTUNG: NOCH KEINE FUNKTION HINTER ' + BitBtnLaden.Caption;

end;



{ ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
procedure THauptfenster.BitBtnSichernClick(Sender: TObject);
begin

  StatusBarHauptfenster.Panels.Items[1].Text:='ACHTUNG: NOCH KEINE FUNKTION HINTER ' + BitBtnSichern.Caption;

end;

procedure THauptfenster.ButtonAboutClick(Sender: TObject);
var
  FormAbout:TFormAbout;
begin
  FormAbout := TFormAbout.Create(Self);
  try
    FormAbout.ShowModal;
  finally
    FormAbout.Release;
  end;
end;




{ ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
initialization
  {$I utftasolver.lrs}

end.

