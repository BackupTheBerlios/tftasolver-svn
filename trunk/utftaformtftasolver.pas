unit utftaformtftasolver;

{ ����������������������������������������������������������������������������

  Author: Simon J. Schilling
  Email: sjschilling@gmx.net
  Year: 2008
  
  Freeware. Enjoy.
  
  ���������������������������������������������������������������������������� }

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
  {$IFDEF TESTMODE}utftaformdebugmessages, sjspointertools, {$ENDIF}
  utftaobject, Menus;

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
    ImageListToolbarOverall: TImageList;
    ImageListMenuOverall: TImageList;
    MainMenuOverall: TMainMenu;
    MemoInputString: TMemo;
    {$IFDEF TESTMODE} MemoDEBUG: TMemo; {$ENDIF}
    MemoOutputString: TMemo;
    MenuItemSaveOutAs: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemSimplify: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemScan: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemClear: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemSaveInAs: TMenuItem;
    ProgressBar1: TProgressBar;
    StatusBarHauptfenster: TStatusBar;
    ToolBarOverall: TToolBar;
    ToolButtonSaveOutAs: TToolButton;
    ToolButton3: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButtonShowTrees: TToolButton;
    ToolButtonSimplify: TToolButton;
    ToolButtonImportTerm: TToolButton;
    ToolButtonExit: TToolButton;
    ToolButtonSaveAs: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonClear: TToolButton;
    ToolButtonHelp: TToolButton;
    TreeViewInputStructure: TTreeView;
    TreeViewOutputStructure: TTreeView;
    procedure MenuItemClearClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemHelpClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemSaveInAsClick(Sender: TObject);
    procedure MenuItemSaveOutAsClick(Sender: TObject);
    procedure MenuItemScanClick(Sender: TObject);
    procedure MenuItemSimplifyClick(Sender: TObject);
    procedure ToolButtonShowTreesClick(Sender: TObject);
  private
      {$IFDEF TESTMODE}
      vDEBUGWindow : TFormDebugMessage;
      vDEBUGLevel : integer;
      procedure SetDEBUGLevel(Parameter : integer);
    {$ENDIF}
  public
    TemporalExpression : TTFTAExpression;
    pointerToApplication : TApplication;
    {$IFDEF TESTMODE}
      property  DEBUGLevel : integer read vDEBUGLevel write SetDEBUGLevel;
      procedure WriteDEBUGMessage(Parameter: ansistring);
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
procedure TTFTAMainWindow.SetDEBUGLevel(Parameter : integer);
begin
  vDEBUGLevel := Parameter;
  if DEBUGLevel>0 then
  begin
    if not Assigned(vDEBUGWindow) then
    begin
      vDEBUGWindow:= TFormDebugMessage.Create(Application);
      vDEBUGWindow.Show;
    end;
    WriteDEBUGMessage('We have DebugLevel '+IntToStr(DEBUGLevel));
    MemoDEBUG := vDEBUGWindow.MemoOutput;
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
procedure TTFTAMainWindow.WriteDEBUGMessage(Parameter : ansistring);
begin
  if Assigned(vDEBUGWindow) then vDEBUGWindow.WriteMessage(Parameter);
end;
{$ENDIF}





procedure TTFTAMainWindow.MenuItemClearClick(Sender: TObject);
begin
  //TemporalExpression.Clear;
  TreeViewInputStructure.Items.Clear ;
  TreeViewOutputStructure.Items.Clear;
  MemoOutputString.Clear;

  self.MenuItemSimplify.Enabled:=false;
  self.ToolButtonSimplify.Enabled:=false;

end;

procedure TTFTAMainWindow.MenuItemExitClick(Sender: TObject);
begin
  close;
end;

procedure TTFTAMainWindow.MenuItemHelpClick(Sender: TObject);
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

procedure TTFTAMainWindow.MenuItemOpenClick(Sender: TObject);
begin
  StatusBarHauptfenster.Panels.Items[1].Text:='NOTE: NOT YET IMPLEMENTED8 ' + MenuItemOpen.Caption;
end;

procedure TTFTAMainWindow.MenuItemSaveInAsClick(Sender: TObject);
begin
  StatusBarHauptfenster.Panels.Items[1].Text:='ACHTUNG: NOCH KEINE FUNKTION HINTER ' + MenuItemSaveInAs.Caption;
end;


procedure TTFTAMainWindow.MenuItemSaveOutAsClick(Sender: TObject);
var x : TTFTAObject;
    i : Integer;

    procedure zaehle(Item : TTFTAObject; iterationlevel : integer);
    var i : Integer;
    begin
      i:=0;
      repeat
        {$IFDEF TESTMODE}WriteDEBUGMessage(Item[i].WriteStatus(iterationlevel));  {$ENDIF}
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
    WriteDEBUGMessage('EventListe mit ' + IntToStr(self.TemporalExpression.EventList.Count) + ' Eintraegen...');
    for i := 1 to self.TemporalExpression.EventList.Count do
      WriteDEBUGMessage(PointerAddrStr(self.TemporalExpression.EventList.Items[i-1])+ ' === ' +
                                          self.TemporalExpression.EventList.Items[i-1].TemporalExpr);
  {$ENDIF}
end;






procedure TTFTAMainWindow.MenuItemScanClick(Sender: TObject);
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

  { clear possible old data }
  MenuItemClearClick(Sender);

  { give input string to TemporalExpression }
  TemporalExpression.InputString := s  ;
  TemporalExpression.pointerToApplication := pointerToApplication;

  { start building the TermporalTerm and the corresponding TreeView from the
    input string }
  TemporalExpression.ParseInput(TreeViewInputStructure.Items);

  self.MenuItemSimplify.Enabled:=true;
  self.ToolButtonSimplify.Enabled:=true;

end;


procedure TTFTAMainWindow.MenuItemSimplifyClick(Sender: TObject);
begin

  self.MenuItemSimplify.Enabled:=false;
  self.ToolButtonSimplify.Enabled:=false;

  TemporalExpression.Simplify;

  { TemporalExpression.TemporalTerm = the TOP event, thus we start with its
    first - and only - child }
  MemoOutputString.Text := TemporalExpression.OutputString;

  { now build OutputTree from TemporalTerm.GetFirstChild }
  TreeViewOutputStructure.Items.Clear;
  TemporalExpression.BuildTreeNodes(TreeViewOutputStructure.Items);

end;

procedure TTFTAMainWindow.ToolButtonShowTreesClick(Sender: TObject);
begin

  TreeViewInputStructure.Visible := TreeViewInputStructure.Visible xor True;
  TreeViewOutputStructure.Visible := TreeViewOutputStructure.Visible xor True;
end;

initialization
  {$I utftaformtftasolver.lrs}

end.

