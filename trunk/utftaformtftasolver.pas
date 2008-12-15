unit utftaformtftasolver;

{ §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  Author: Simon J. Schilling
  Email: sjschilling@gmx.net
  Year: 2008
  
  Freeware. Enjoy.
  
  §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ }

{$mode objfpc}{$H+}
// switch TESTMODE on or of globally by editing testmode.conf (found in main path
// of TFTASolver.tftasolver
{$INCLUDE testmode.conf}

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
  ComCtrls, ExtCtrls , Buttons, strutils,
  { eigene Units }
  utftaexpression, utftaformabout,
  {$IFDEF TESTMODE}utftaformdebugmessages, {$ENDIF}
  utftaobject;

{ ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
type

  { TFTAMainWindow, das was man als Nutzer vom PRogramm primaer sieht. }
  { ######################################################################## }

  { TTFTAMainWindow }

  TTFTAMainWindow = class(TForm)
    BitBtnShowInputStructure: TBitBtn;
    BitBtnShowOutputStructure: TBitBtn;
    BitBtnCalculate: TBitBtn;
    BitBtnSave: TBitBtn;
    BitBtnLoad: TBitBtn;
    BitBtnGetInputString: TBitBtn;
    BitBtnSaveOutput: TBitBtn;
    ButtonAbout: TButton;
    Label1: TLabel;
    MemoInputString: TMemo;
    {$IFDEF TESTMODE} MemoDEBUG: TMemo; {$ENDIF}
    MemoOutputString: TMemo;
    ProgressBar1: TProgressBar;
    Shape1: TShape;
    StatusBarHauptfenster: TStatusBar;
    TreeViewInputStructure: TTreeView;
    TreeViewOutputStructure: TTreeView;
    procedure BitBtnShowOutputStructureClick(Sender: TObject);
    procedure BitBtnCalculateClick(Sender: TObject);
    procedure BitBtnGetInputStringClick(Sender: TObject);
    procedure BitBtnShowInputStructureClick(Sender: TObject);
    procedure BitBtnSaveOutputClick(Sender: TObject);
    procedure BitBtnLoadClick(Sender: TObject);
    procedure BitBtnSaveClick(Sender: TObject);
    procedure ButtonAboutClick(Sender: TObject);

  private
    {$IFDEF TESTMODE}
      vDEBUGFenster : TFormDebugMeldungen;
      vDEBUGLevel : integer;
      procedure SetzeDEBUGLevel(Parameter : integer);
    {$ENDIF}
  public
    TemporalExpression : TTFTAExpression;
    pointerToApplication : TApplication;
    {$IFDEF TESTMODE}
      property  DEBUGLevel : integer read vDEBUGLevel write SetzeDEBUGLevel;
      procedure SchreibeDEBUGMeldung(Parameter: ansistring);
    {$ENDIF}
  end; 

{ ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
var
  TFTAMainWindow: TTFTAMainWindow;

{ ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
implementation


{ TTFTAMainWindow }

{$IFDEF TESTMODE}
{------------------------------------------------------------------------------
  Schreibe Property vDEBUGLevel
------------------------------------------------------------------------------}
procedure TTFTAMainWindow.SetzeDEBUGLevel(Parameter : integer);
begin
  vDEBUGLevel := Parameter;
  if DEBUGLevel>0 then
  begin
    if not Assigned(vDEBUGFenster) then
    begin
      vDEBUGFenster:= TFormDebugMeldungen.Create(Application);
      vDEBUGFenster.Show;
    end;
    SchreibeDEBUGMeldung('Bis auf Weiteres herrscht DebugLevel '+IntToStr(DEBUGLevel));
    MemoDEBUG := vDEBUGFenster.MemoAusgabebereich;
  end else
  begin
    MemoDEBUG := NIL;
  end;
end;
{$ENDIF}

{$IFDEF TESTMODE}
{------------------------------------------------------------------------------
  Schreibt eine Meldung ins DebugFenster
------------------------------------------------------------------------------}
procedure TTFTAMainWindow.SchreibeDEBUGMeldung(Parameter : ansistring);
begin
  if Assigned(vDEBUGFenster) then vDEBUGFenster.SchreibeMeldung(Parameter);
end;
{$ENDIF}

{ ##############################################################################
  ##############################################################################
  TTFTAMainWindow.BitBtnAusdruckUbernehmenClick steuert die Uebernahme der
  Eingaben und die Auswertung derselben.
  ##############################################################################
  #############################################################################}
procedure TTFTAMainWindow.BitBtnGetInputStringClick(Sender: TObject);
var s : ansistring;
begin

  { within the InputString all Blanks / Whitespaces and Newlines need to be
    deleted; furthermore the user may use "[", "(" and the corresponding
    colsing brackets, internally they are converted to "[" and "]";
    all input is casted to uppercase }
  s := Trim(MemoInputString.Lines.Text);
  s := AnsiReplaceText(s,' ','');
  s := AnsiReplaceText(s,sLineBreak,'');
  s := AnsiReplaceText(s,'(','[');
  s := AnsiReplaceText(s,')',']');
  s := upCase(s);
  
  { write the "cleaned" string back to the input field }
  MemoInputString.Lines.Text := s;

  { create an initial TemporalExpression }
  if assigned(TemporalExpression) then
    //TemporalExpression.Reset
  else
    TemporalExpression := TTFTAExpression.Create;
  {$IFDEF TESTMODE}TemporalExpression.SetDEBUGMemo(MemoDEBUG); {$ENDIF}

  { give input string to TemporalExpression }
  TemporalExpression.InputString := s  ;
  TemporalExpression.pointerToApplication := pointerToApplication;
  TreeViewInputStructure.Items.Clear ;
  TreeViewOutputStructure.Items.Clear;
  self.MemoOutputString.Clear;
  { start building the TermporalTerm and the corresponding TreeView from the
    input string }
  TemporalExpression.ParseInput(TreeViewInputStructure.Items);
  self.BitBtnCalculate.Enabled:=true;

end;

procedure TTFTAMainWindow.BitBtnShowOutputStructureClick(Sender: TObject);
begin
  TreeViewOutputStructure.Visible := TreeViewOutputStructure.Visible xor True;
  if BitBtnShowOutputStructure.caption = 'Struktur anzeigen' then
     BitBtnShowOutputStructure.caption := 'Struktur ausblenden'
  else
     BitBtnShowOutputStructure.caption := 'Struktur anzeigen' ;
end;

{ ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
procedure TTFTAMainWindow.BitBtnCalculateClick(Sender: TObject);
begin

  self.BitBtnCalculate.Enabled:=false;

  TemporalExpression.Simplify;

  { TemporalExpression.TemporalTerm = the TOP event, thus we start with its
    first - and only - child }
  MemoOutputString.Text := TemporalExpression.OutputString;

  { now build OutputTree from TemporalTerm.GetFirstChild }
  TreeViewOutputStructure.Items.Clear;
  TemporalExpression.BuildTreeNodes(TreeViewOutputStructure.Items);
end;


{ ##############################################################################
  ##############################################################################
  TTFTAMainWindow.BitBtnAnzeigenStrukturClick schaltet die
  Baumanzeige ein und aus.
  ##############################################################################
  #############################################################################}
procedure TTFTAMainWindow.BitBtnShowInputStructureClick(Sender: TObject);
begin

  TreeViewInputStructure.Visible := TreeViewInputStructure.Visible xor True;
  if BitBtnShowInputStructure.caption = 'Struktur anzeigen' then
     BitBtnShowInputStructure.caption := 'Struktur ausblenden'
  else
     BitBtnShowInputStructure.caption := 'Struktur anzeigen' ;

end;


{ ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
procedure TTFTAMainWindow.BitBtnSaveOutputClick(Sender: TObject);
var x : TTFTAObject;
    i : Integer;
    
    procedure zaehle(Item : TTFTAObject; iterationlevel : integer);
    var i : Integer;
    begin
      i:=0;
      repeat
        {$IFDEF TESTMODE}SchreibeDEBUGMeldung(Item[i].WriteStatus(iterationlevel));  {$ENDIF}
        if Item[i].HasChildren then
          begin
            zaehle(Item[i],iterationlevel+2);
          end;
        inc(i);
      until i=Item.Count;
    end;
    
begin
  x := self.TemporalExpression.TemporalTerm;
  
  if x.HasChildren then zaehle(x,0);
  {$IFDEF TESTMODE}
    SchreibeDEBUGMeldung('EventListe mit ' + IntToStr(self.TemporalExpression.EventList.Count) + ' Eintraegen...');
    for i := 1 to self.TemporalExpression.EventList.Count do
      SchreibeDEBUGMeldung(self.TemporalExpression.EventList.Items[i-1].TemporalExpr);
  {$ENDIF}
end;



{ ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
procedure TTFTAMainWindow.BitBtnLoadClick(Sender: TObject);
begin

  StatusBarHauptfenster.Panels.Items[1].Text:='ACHTUNG: NOCH KEINE FUNKTION HINTER ' + BitBtnLoad.Caption;

end;



{ ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
procedure TTFTAMainWindow.BitBtnSaveClick(Sender: TObject);
begin

  StatusBarHauptfenster.Panels.Items[1].Text:='ACHTUNG: NOCH KEINE FUNKTION HINTER ' + BitBtnSave.Caption;

end;



procedure TTFTAMainWindow.ButtonAboutClick(Sender: TObject);
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
  {$I utftaformtftasolver.lrs}

end.

