unit utftaformtftasolver;

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
  ComCtrls, ExtCtrls , Buttons, strutils,
  { eigene Units }
  utftaexpression, utftaformabout, utftaformdebugmessages, utftaobject,
   utftalogik;

{ ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
type

  TTFTAObject2 = class
  end;

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
    MemoDEBUG: TMemo;
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
    { private declarations }
    vDEBUGFenster : TFormDebugMeldungen;
    vDEBUGLevel : integer;
    function HoleDEBUGLevel : integer;
    procedure SetzeDEBUGLevel(Parameter : integer);
  public
    { public declarations }
    TemporalExpression : TTFTAExpression;
    pointerToApplication : TApplication;
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
  TFTAMainWindow: TTFTAMainWindow;

{ ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################
  #############################################################################}
implementation


{ TTFTAMainWindow }

{------------------------------------------------------------------------------
  Lese Property vDEBUGLevel
------------------------------------------------------------------------------}
function TTFTAMainWindow.HoleDEBUGLevel : integer;
begin
  HoleDEBUGLevel := vDEBUGLevel;
end;
{------------------------------------------------------------------------------
  Schreibe Property vDEBUGLevel
------------------------------------------------------------------------------}
procedure TTFTAMainWindow.SetzeDEBUGLevel(Parameter : integer);
begin
  vDEBUGLevel := Parameter;
  if vDEBUGFenster = Nil then
  begin
    vDEBUGFenster:= TFormDebugMeldungen.Create(Application);
    vDEBUGFenster.Show;
  end;
  if DEBUGLevel>0 then
  begin
    SchreibeDEBUGMeldung('Bis auf Weiteres herrscht DebugLevel '+IntToStr(DEBUGLevel));
    MemoDEBUG := vDEBUGFenster.MemoAusgabebereich;
  end;
end;
{------------------------------------------------------------------------------
  Schreibt eine Meldung ins DebugFenster
------------------------------------------------------------------------------}
procedure TTFTAMainWindow.SchreibeDEBUGMeldung(Parameter : ansistring);
begin
  if vDEBUGFenster <> Nil then vDEBUGFenster.SchreibeMeldung(Parameter);
end;

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
    TemporalExpression.Destroy;
    
  TemporalExpression := TTFTAExpression.Create;
  
  TemporalExpression.SetDEBUGMemo(MemoDEBUG);

  { give input string to TemporalExpression }
  TemporalExpression.InputString := s  ;

  TreeViewInputStructure.Items.Clear ;

  TemporalExpression.pointerToApplication := pointerToApplication;

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

  function ablauf(theobject :TTFTAObject; theParent : TTFTAList; theIndex : Integer) : boolean;
  var i : Integer = 0;
      changedSelf : boolean = false;
      changedChildren : boolean = false;
  begin
    Result := false;
    repeat { outer loop, continue until neither change in oneself nor in children }

      repeat  { inner loop, continue until no chnage in oneself }
        changedSelf := false; { flag whether in the inner loop a change happend }

        If (not changedSelf) and PANDSplit(theobject, theParent , theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and ANDFalse(theobject, theParent, theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and ORXORFalse(theobject, theParent, theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and PANDFalse(theobject, theParent, theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and SANDFalse(theobject, theParent, theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and NOTFalseTrue(theobject, theParent, theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and ANDTrue(theobject, theParent, theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and ORXORTrue(theobject, theParent, theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and PANDMultiples(theobject, theParent , theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and PANDFalse(theobject, theParent , theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and LawOfIdempotency(theobject, theParent , theIndex, TemporalExpression.EventList ) then
          changedSelf := true;
        If (not changedSelf) and LawOfCompleteness(theobject, theParent , theIndex, TemporalExpression.EventList ) then
          changedSelf := true;
        If (not changedSelf) and PANDPANDTransform(theobject, theParent , theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and SANDPANDTransform(theobject, theParent , theIndex, TemporalExpression.EventList) then
          changedSelf := true;
        If (not changedSelf) and NOTNOT(theobject, theParent , theIndex, TemporalExpression.EventList ) then
          changedSelf := true;

        If changedSelf then
        begin
          theobject := theParent[theIndex];
          Result := true;  { flag that ANY change has happened}
        end;

        self.pointerToApplication.ProcessMessages;

      until not changedSelf;

      changedChildren := False; { flag for change in children }
      i := 0;
      if theobject.HasChildren then
      begin
        repeat
          if ablauf( theobject[i] , theobject.Children, i) then
          begin
            changedChildren := true; { flag, that at least one child was changed }
            //theobject[i] := theobject.Children[i];
          end;
          inc(i);
        until (i>=theobject.Count);
        Result := Result or changedChildren; { true, if self changed or child changed }
      end;

      self.pointerToApplication.ProcessMessages;

    until not changedChildren;

    self.pointerToApplication.ProcessMessages;

  end;  { function ablauf }

begin

  self.BitBtnCalculate.Enabled:=false;

  ablauf(TemporalExpression.TemporalTerm.GetFirstChild, TemporalExpression.TemporalTerm.Children, 0);
  
  MemoOutputString.Text := TemporalExpression.TemporalTerm.GetFirstChild.TemporalExpr;

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
        //SchreibeDEBUGMeldung(IntToStr(i) + ': Adresse ' + PointerAddrStr(Item[i]));
        SchreibeDEBUGMeldung(Item[i].WriteStatus(iterationlevel));
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
  
  SchreibeDEBUGMeldung('EventListe mit ' + IntToStr(self.TemporalExpression.EventList.Count) + ' Eintraegen...');
  
  
  for i := 1 to self.TemporalExpression.EventList.Count do
    SchreibeDEBUGMeldung(self.TemporalExpression.EventList.Items[i-1].TemporalExpr);
  
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

