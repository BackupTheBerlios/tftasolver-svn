unit utftaobject;

{ ############################################################################

  Copyright by Simon J. Schilling (the Author)
  Email: sjschilling@gmx.net
  Year: 2008

  Free Software. Enjoy.

  This file is part of TFTASolver.

    TFTASolver is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    TFTASolver is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TFTASolver.  If not, see <http://www.gnu.org/licenses/>.

  ############################################################################ }

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, Dialogs, ComCtrls, StdCtrls, strutils, contnrs, sjspointertools;

const
  cEventTypeStringArray : array[1..9] of string[30] = ('AND','OR','XOR','PAND','SAND','OTHER','TOP','NOT',
                                                       'BASIC');
  
type

  { forward declarations }
  TTFTAList = class;
  TTFTAObject = class;
  TTFTAEventLookupList = class;
  
  { ######################################################################## }
  TTFTAOperatorType = (tftaEventTypeAND := 1,
                       tftaEventTypeOR,
                       tftaEventTypeXOR,
                       tftaEventTypePAND,
                       tftaEventTypeSAND,
                       tftaEventTypeOTHER,
                       tftaEventTypeTOP,
                       tftaEventTypeNOT,
                       tftaEventTypeBASIC);
  
  { ########################################################################## }

  { ########################################################################## }

  TTFTAEventLookupList = class(TFPObjectList)
  private
    VtheFALSE : TTFTAObject;
    VtheTRUE  : TTFTAObject;
    VDEBUGMemo: TMemo;

    function  NewItemPrivate(EventType               : TTFTAOperatorType;
                            IsBasicEvent            : boolean;
                            IsCoreEvent             : boolean;
                            IsEventSequence         : boolean;
                            IsNegated               : boolean;
                            IsNotCompletelyBuildYet : boolean;
                            IsDisjunct              : boolean;
                            IsExtendedSequence      : boolean;
                            NeedsToBeUpdated        : boolean;
                            PointerToUpdateObject   : TTFTAObject;
                            TemporalExpr            : ansistring) : TTFTAObject;

  protected
    function  GetItem(Index: longword): TTFTAObject;
    procedure SetItem(Index: longword; Item: TTFTAObject);

  public
    constructor Create(pointerToDebugMemo : TMemo);

    function  Add(Item: TTFTAObject): Integer;
    function  ListHoldsObjectAt(Text : ansistring) : TTFTAObject;
    property  DEBUGMemo : TMemo read VDEBUGMemo write VDEBUGMemo;
    property  Items[Index: longword]: TTFTAObject read GetItem write SetItem; default;
    function  NewItem: TTFTAObject;
    function  NewItem(      EventType               : TTFTAOperatorType;
                  		      IsBasicEvent            : boolean;
                  		      IsCoreEvent             : boolean;
                  		      IsEventSequence         : boolean;
                  		      IsNegated               : boolean;
                  		      IsNotCompletelyBuildYet : boolean;
                  		      IsDisjunct              : boolean;
                  		      IsExtendedSequence      : boolean;
                  		      LogicalValue            : boolean;    { <<<<<<<<< !}
                  		      NeedsToBeUpdated        : boolean;
                  		      PointerToUpdateObject   : TTFTAObject;
                  		      TemporalExpr            : ansistring) : TTFTAObject;
    function  NewItem(      EventType               : TTFTAOperatorType;
                  		      IsBasicEvent            : boolean;
                  		      IsCoreEvent             : boolean;
                  		      IsEventSequence         : boolean;
                  		      IsNegated               : boolean;
                  		      IsNotCompletelyBuildYet : boolean;
                  		      IsDisjunct              : boolean;
                  		      IsExtendedSequence      : boolean;
                  		      LogicalValue            : pointer;    { <<<<<<<<< !}
                  		      NeedsToBeUpdated        : boolean;
                  		      PointerToUpdateObject   : TTFTAObject;
                  		      TemporalExpr            : ansistring) : TTFTAObject;
    property  TheFALSEElement : TTFTAObject read VtheFALSE;
    property  TheTRUEElement : TTFTAObject read VtheTRUE;

    pointerToApplication : TApplication;

  end;

  { ########################################################################## }

  TTFTAList = class(TFPObjectList)
  private
    VOwnsObjects : Boolean;
    VOwner : TTFTAObject;
    
  public
    function  Add(Item: TTFTAObject): Integer;
    procedure Clear;
    function  DeleteAllCopies : boolean;
    function  DeleteAllCopiesOfObject(theItem : TTFTAObject) : boolean;
    function  Extract(Item: TTFTAObject):TTFTAObject;
    function  FindNextAfter(theItem : TTFTAObject; StartAfter : Integer) : Integer;
    function  First: TTFTAObject;
    function  IndexOf(Item: TTFTAObject): Integer;
    function  Last: TTFTAObject;
    function  Remove(Item: TTFTAObject): Integer;
    function  GetItem(Index: longword): TTFTAObject;
    function  GetPlainItem(Index: longword): TTFTAObject;
    procedure SetItem(Index: longword; Item: TTFTAObject);
    procedure Assign(Obj: TTFTAList);
    procedure Insert(Index: longword; Item: TTFTAObject);
    property  Items[Index: longword]: TTFTAObject read GetItem write SetItem; default;
    property  OwnsObjects: Boolean read VOwnsObjects write VOwnsObjects;
    property  Owner : TTFTAObject read VOwner write VOwner;

  end;
  
  { ########################################################################## }

  TTFTAObject = class(TObject)

  private
    VChildren : TTFTAList;        { list of all my descendants (children) }
    VDEBUGMemo : TMemo;
    VExpr : ansistring;           { if I'm a BasicEvent, I have to carry my event name }
    VIsBasicEvent : boolean;
    VIsSorted : boolean;
    VIsCoreEvent : boolean;
    VIsDisjunct : boolean ;
    VIsEventSequence : boolean ;
    VIsExtendedSequence : boolean;
    VIsMinimal : boolean;
    VIsNegated : boolean;
    VIsNotCompletelyBuildYet : boolean; { during built-up of an object (from InputString) no TempExpr checking must be performed }
    VIsTrueFalse : integer;
    VNeedsToBeUpdated : boolean;  { true, if before an identical event was modified, then a update to this identical object is needed }
    VPointerToUpdateObject : TTFTAObject; { this points to the object the current object shall be updated to }
    VPosInEventList : longword;   { allows to reference to my own pointer (within eventlist) }
    VType: TTFTAOperatorType;     { what type am I? }

    function  CheckLogicFalse : boolean;
    function  CheckLogicTrue : boolean;
    function  GetChildrenBasicState : boolean;
    function  GetIsBasicEvent : boolean;
    function  GetIsCoreEvent : boolean;
    function  GetIsDisjunct : boolean;
    function  GetIsEventSequence : boolean;
    function  GetIsExtendedSequence : boolean;
    function  GetIsNegated : boolean;
    function  GetTempExpr : ansistring;

    procedure CheckForCoreEvent;
    procedure CheckForDisjunctEvent;
    procedure CheckForEventSequenceEvent;
    procedure CheckForExtendedSequenceEvent;
    procedure SetIsCoreEvent (Parameter : boolean);
    procedure SetIsDisjunct (Parameter : boolean);
    procedure SetIsEventSequence (Parameter : boolean);
    procedure SetIsExtendedSequence (Parameter : boolean);
    procedure SetIsNegated (Parameter : boolean);
    procedure SetTempExpr(theExpr : ansistring);
    procedure SetVType(Parameter : TTFTAOperatorType);

  public
    constructor Create;

    function  AddChild(Item : TTFTAObject) : Integer;
    function  Clone(list : TTFTAEventLookupList) : TTFTAObject;
    function  Count : Longword;
    function  EventTypeToString : string;
    function  ExtractChild(Item: TTFTAObject):TTFTAObject;
    function  GetChild(Index: longword): TTFTAObject;
    function  GetFirstChild: TTFTAObject;
    function  GetLastChild: TTFTAObject;
    function  HasChildren : boolean;
    function  RemoveChild(Item : TTFTAObject) : Integer;
    function  WriteStatus(indent:integer = 0)  : ansistring;

    procedure AssignChildren(Obj: TTFTAList);
    procedure AssignObject(theObject : TTFTAObject);
    procedure CheckTermProperties;
    procedure DEBUGPrint(isUpdate : boolean; eventlist : TTFTAEventLookupList; thestring : ansistring = '');
    procedure DeleteChild(Index: longword);
    procedure InsertChild(Index: longword; Item: TTFTAObject);
    procedure SetChild(Index: longword; Item: TTFTAObject);
    procedure SetIsBasicEvent (Parameter : boolean);
    procedure SetLogicalValue (Parameter : boolean);
    procedure SetLogicalValue (Parameter : pointer);

    property  Children : TTFTAList read VChildren write VChildren;
    property  DEBUGMemo : TMemo read VDEBUGMemo write VDEBUGMemo;
    property  EventType : TTFTAOperatorType read VType write SetVType;
    property  IsAllChildrenAreBasic : boolean read GetChildrenBasicState;
    property  IsBasicEvent : boolean read GetIsBasicEvent write SetIsBasicEvent;
    property  IsCoreEvent : boolean read GetIsCoreEvent write SetIsCoreEvent;
    property  IsEventSequence : boolean read GetIsEventSequence write SetIsEventSequence;
    property  IsNegated : boolean read GetIsNegated  write SetIsNegated ;
    property  IsNotCompletelyBuildYet : boolean read VIsNotCompletelyBuildYet write VIsNotCompletelyBuildYet;
    property  LogicLevel : integer read VIsTrueFalse;
    property  IsDisjunct : boolean read GetIsDisjunct write SetIsDisjunct ;
    property  IsFalse : boolean read CheckLogicFalse;
    property  IsTrue : boolean read CheckLogicTrue;
    property  IsExtendedSequence : boolean read GetIsExtendedSequence write SetIsExtendedSequence  ;
    property  Items[Index: longword]: TTFTAObject read GetChild write SetChild; default;
    property  NeedsToBeUpdated : boolean read VNeedsToBeUpdated write VNeedsToBeUpdated;
    property  PointerToUpdateObject : TTFTAObject read VPointerToUpdateObject write VPointerToUpdateObject;
    property  PosInEventList : longword read VPosInEventList write VPosInEventList;
    property  TemporalExpr : ansistring read GetTempExpr write SetTempExpr;

  end;

implementation


{ ############################################################################ }
{ TTFTAObject }
{------------------------------------------------------------------------------
  Class-Constructor
------------------------------------------------------------------------------}
constructor TTFTAObject.Create;
begin

  inherited;
  self.Children := TTFTAList.Create;
  self.Children.OwnsObjects:=False;     { the EventLookupList owns objects
                                         (the objects.children - list does not! }
  self.Children.Owner := self;          { pointer to object, needed for updates of
                                     object initiate by one of the children }
  self.PointerToUpdateObject := NIL;
  self.NeedsToBeUpdated := false;
  self.IsNotCompletelyBuildYet := false;
  self.VExpr:='NIL';                    { dummy value in order to not get exceptions }

end;

{------------------------------------------------------------------------------
  True, if there are entries in Children, false otherwise
------------------------------------------------------------------------------}
function TTFTAObject.HasChildren : boolean;
begin
  Result := (self.Count > 0);
end;
{------------------------------------------------------------------------------
  True, if there are entries in Children, false otherwise
------------------------------------------------------------------------------}
function  TTFTAObject.GetChildrenBasicState : boolean;
var i : longword;
begin
  Result := True; { Default }
  if self.HasChildren then
     for i:=0 to ( self.Count-1 ) do
         Result := Result and self[i].IsBasicEvent
  else
    Result := False; { no children -> children cant be basic events }
end;
{------------------------------------------------------------------------------
  Calls Children.Remove(Item)
------------------------------------------------------------------------------}
function TTFTAObject.RemoveChild(Item : TTFTAObject) : Integer;
begin
  if Assigned(self.Children) then
    Result := self.Children.Remove(Item)
  else
    Result :=0;
end;
{------------------------------------------------------------------------------
  Add an entry in Children
------------------------------------------------------------------------------}
function TTFTAObject.AddChild(Item : TTFTAObject) :Integer;
begin
  if Assigned(self.Children) then Result := self.Children.Add(Item);
end;
{------------------------------------------------------------------------------
  Assign Children
------------------------------------------------------------------------------}
procedure TTFTAObject.AssignChildren(Obj: TTFTAList);
begin
  if Assigned(self.Children) then self.Children.Assign(Obj);
end;
{------------------------------------------------------------------------------
  Extract a child
------------------------------------------------------------------------------}
function TTFTAObject.ExtractChild(Item: TTFTAObject):TTFTAObject;
begin
  if Assigned(self.Children) then
    Result := self.Children.Extract(Item)
  else
    Result := NIL;
end;
{------------------------------------------------------------------------------
  Returns first child
------------------------------------------------------------------------------}
function TTFTAObject.GetFirstChild :TTFTAObject;
begin
  if Assigned(self.Children) then
    Result := self.Children.First
  else
    Result := NIL;
end;
{------------------------------------------------------------------------------
  Returns child nr. Index
------------------------------------------------------------------------------}
function TTFTAObject.GetChild(Index: longword): TTFTAObject;
begin
  if Assigned(self.Children) then
    Result := self.Children.Items[Index]
  else
    Result := NIL;
end;

procedure TTFTAObject.DeleteChild(Index: longword);
begin
  if Assigned(self.Children) then self.Children.Delete(Index);
end;

{------------------------------------------------------------------------------
  Insert a new child (Item) at pos Index
------------------------------------------------------------------------------}
procedure TTFTAObject.InsertChild(Index: longword; Item: TTFTAObject);
begin
  if Assigned(self.Children) then self.Children.Insert(Index, Item);
end;
{------------------------------------------------------------------------------
  Returns last child
------------------------------------------------------------------------------}
function TTFTAObject.GetLastChild :TTFTAObject;
begin
  if Assigned(self.Children) then
    Result := self.Children.Last
  else
    Result := NIL;
end;
{------------------------------------------------------------------------------
  Set child nr. Index to Item
------------------------------------------------------------------------------}
procedure TTFTAObject.SetChild(Index: longword; Item: TTFTAObject);
begin
  if Assigned(self.Children) then self.Children[Index] := Item;
end;
{------------------------------------------------------------------------------
  Number of children
------------------------------------------------------------------------------}
function TTFTAObject.Count : Longword;
begin
  if Assigned(self.Children) then
    Result := self.Children.Count
  else
    Result := 0;
end;

function TTFTAObject.GetIsBasicEvent : boolean;
begin
  GetIsBasicEvent := self.VIsBasicEvent;
end;

function TTFTAObject.GetIsDisjunct : boolean;
begin
  GetIsDisjunct := self.VIsDisjunct;
end;

function TTFTAObject.GetIsEventSequence : boolean;
begin
  GetIsEventSequence := self.VIsEventSequence;
end;

function TTFTAObject.GetIsExtendedSequence : boolean;
begin
  GetIsExtendedSequence := self.VIsExtendedSequence;
end;

function TTFTAObject.GetIsCoreEvent : boolean;
begin
  GetIsCoreEvent := self.VIsCoreEvent;
end;

function TTFTAObject.GetIsNegated : boolean;
begin
  GetIsNegated := self.VIsNegated;
end;

procedure TTFTAObject.SetIsBasicEvent (Parameter : boolean);
begin
  self.VIsBasicEvent := Parameter;
  if Parameter then
  begin
    self.IsCoreEvent:=true;
    self.IsEventSequence:=true;
    self.VType := tftaEventTypeBASIC;
  end;
end;

procedure TTFTAObject.SetIsDisjunct (Parameter : boolean);
begin
  self.VIsDisjunct := Parameter;
end;

procedure TTFTAObject.SetIsEventSequence (Parameter : boolean);
begin
  self.VIsEventSequence := Parameter;
end;

procedure TTFTAObject.SetIsExtendedSequence (Parameter : boolean);
begin
  self.VIsExtendedSequence := Parameter;
end;

procedure TTFTAObject.SetIsCoreEvent (Parameter : boolean);
begin
  self.VIsCoreEvent := Parameter;
end;

procedure TTFTAObject.SetIsNegated (Parameter : boolean);
begin
  self.VIsNegated := Parameter;
end;

procedure TTFTAObject.SetLogicalValue (Parameter : boolean);
begin
  if Parameter then
    self.VIsTrueFalse := 1
  else
    self.VIsTrueFalse := 0;

  self.SetIsBasicEvent(True);
  self.TemporalExpr:=BoolToString(Parameter);
end;
procedure TTFTAObject.SetLogicalValue (Parameter : pointer);
begin
  if not Assigned(Parameter) then
    self.VIsTrueFalse := -1;
end;
procedure TTFTAObject.SetVType(Parameter : TTFTAOperatorType);
begin
  self.VType := Parameter;
  if self.VType = tftaEventTypeBASIC then
    self.SetIsBasicEvent(true)
  else
    self.SetIsBasicEvent(false);
  if self.VType = tftaEventTypeNOT then
    self.SetIsNegated(true)
  else
    self.SetIsNegated(false);
end;

{------------------------------------------------------------------------------
  Provides the logical expression represented by the term / object
  Scanns through all descendants, which is time consuming
  use with care and as seldom as possible
------------------------------------------------------------------------------}
function TTFTAObject.GetTempExpr : ansistring;
var i,j : longword;
begin
  if not self.IsBasicEvent then
  begin
    if not self.IsNotCompletelyBuildYet then
    begin
      if self.HasChildren then
      begin
        j := self.Count;
        Result := self.EventTypeToString + '[' ;
        if j > 1 then  { and, or, xor, pand, sand operators }
        begin
          for i:=0 to j-2 do
          begin
            Result := Result + self[i].TemporalExpr + ',';
          end;
          Result := Result + self[i+1].TemporalExpr + ']';
        end else  { not operator or one of the above in rare cases, where
                    transformation results in only single parameter }
        begin
          Result := Result + self[0].TemporalExpr + ']';
        end;
      end else
      begin
        Result := '###ERROR11241###' ;
      end;
     end else
     begin
       Result := ''; { if object is just in built-up then it has no TempExpr (because it is still changing) }
     end;
  end else
  begin;
    Result := self.VExpr;
  end;
end;
{------------------------------------------------------------------------------
  Sets the logical expression represented by the term / object
  Only possible with basic events!
------------------------------------------------------------------------------}
procedure TTFTAObject.SetTempExpr(theExpr : ansistring);
begin
  if self.IsBasicEvent then self.VExpr:=theExpr;
end;
{------------------------------------------------------------------------------
  Liefert Typ des Ereignisses (aus VType) als String
------------------------------------------------------------------------------}
function TTFTAObject.EventTypeToString : string;
begin
  Result := cEventTypeStringArray[ord(self.EventType)]
end;

{------------------------------------------------------------------------------
  Liefert als String eine Uebersicht ueber die Eigenschaften des Objekts.�
------------------------------------------------------------------------------}
function TTFTAObject.WriteStatus(indent:integer = 0) : ansistring;
begin
  if self.NeedsToBeUpdated and Assigned(self.PointerToUpdateObject) then
    WriteStatus := PointerAddrStr(self.PointerToUpdateObject)
  else
    WriteStatus := 'NIL';

  {WriteStatus := DupeString(' ',indent) + 'Event '               + TemporalExpr + sLineBreak +
                 DupeString(' ',indent) + ' --- Address : '      + PointerAddrStr(self) + sLineBreak +
                 DupeString(' ',indent) + ' --- Type : '         + EventTypeToString + sLineBreak +
                 DupeString(' ',indent) + ' --- BasicEvent : '   + BoolToString(IsBasicEvent) + sLineBreak +
                 DupeString(' ',indent) + ' --- CoreEvent : '    + BoolToString(IsCoreEvent) + sLineBreak +
                 DupeString(' ',indent) + ' --- Negated : '      + BoolToString(IsNegated) + sLineBreak +
                 DupeString(' ',indent) + ' --- EventSequence : '+ BoolToString(IsEventSequence) + sLineBreak +
                 DupeString(' ',indent) + ' --- LogicalValue : '+ IntToStr(self.LogicLevel) + sLineBreak +
                 DupeString(' ',indent) + ' --- NeedsToBeUpdated : '+ BoolToString(NeedsToBeUpdated) + sLineBreak +
                 DupeString(' ',indent) + ' --- Points to Adress : ' + WriteStatus + sLineBreak +
                 DupeString(' ',indent) + ' --- No. children : ' + IntToStr(Count) + sLineBreak + ' ' ;      }
end;


{------------------------------------------------------------------------------
  makes a clone of the TTFTAobject�
------------------------------------------------------------------------------}
function  TTFTAObject.Clone(list : TTFTAEventLookupList) : TTFTAObject;
begin
  if self.LogicLevel = -1 then
    Result := list.NewItem(self.EventType,self.IsBasicEvent,self.IsCoreEvent,
                           self.IsEventSequence,self.IsNegated,
                           self.IsNotCompletelyBuildYet,self.IsDisjunct,
                           self.IsExtendedSequence,NIL,self.NeedsToBeUpdated,
                           self.PointerToUpdateObject,self.TemporalExpr)
  else
    Result := list.NewItem(self.EventType,self.IsBasicEvent,self.IsCoreEvent,
                           self.IsEventSequence,self.IsNegated,
                           self.IsNotCompletelyBuildYet,self.IsDisjunct,
                           self.IsExtendedSequence,
                           self.IsTrue, { True, if isTrue, and False, if isFalse }
                           self.NeedsToBeUpdated,
                           self.PointerToUpdateObject,self.TemporalExpr);
  Result.Children.Assign(self.Children);
end;

{------------------------------------------------------------------------------
  Assigns theObject to TTFTAobject�
------------------------------------------------------------------------------}
procedure TTFTAObject.AssignObject(theObject : TTFTAObject);
begin
  self.IsCoreEvent:=theObject.IsCoreEvent;
  self.EventType:=theObject.EventType;
  self.IsBasicEvent:=theObject.IsBasicEvent;
  self.IsEventSequence:=theObject.IsEventSequence;
  self.IsNegated:=theObject.IsNegated;
  IsDisjunct:=theObject.IsDisjunct;
  self.IsExtendedSequence:= theObject.IsExtendedSequence;
  self.Children := theObject.Children;
  self.TemporalExpr:=theObject.TemporalExpr;
end;
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
function TTFTAObject.CheckLogicFalse : boolean;
begin
  Result := (self.LogicLevel = 0);
end;
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
function TTFTAObject.CheckLogicTrue : boolean;
begin
  Result := (self.LogicLevel = 1);
end;
{------------------------------------------------------------------------------
  prueft, ob das uebergebene Ereignis ein Kernereignis ist und setzt die
  Eigenschaften entsprechend
------------------------------------------------------------------------------}
procedure TTFTAObject.CheckForCoreEvent;
var tempObject          : TTFTAObject;
    StatusNegated       : boolean;
    StatusCoreEvent  : boolean;
    LoopStatus     : boolean;
    i                   : longword;
begin
  { Ein Ereignis ist genau dann ein Kernereignis (KE), wenn
    - es ein nicht-negiertes Basisereignis ist.
    - es ein sand-Term ist, dessen Kinder (Operanden) alle nicht-negierte
      Kernereignisse sind.
    - es ein negiertes Basisereignis ist.
    - es ein and-Term ist, dessen Kinder (Operanden) alle negierte
      Kernereignisse sind.
    In den ersten beiden Faellen handelt es sich um ein nicht-negiertes KE.
    In den letzten beiden Faellen handelt es sich um ein negiertes KE. }

  If ( self.IsBasicEvent ) or
       ( (self.IsNegated) and (self.Count = 1) ) then
  begin { (negiertes) Basisereignis }
    self.IsCoreEvent:=True;
  end else
  begin { kein Basisereignis }
    { pruefen, ob ein sand Operator da ist und zumindest das erste Kind ein
      Kernereignis ist (Pruefung zur Aufwandsreduktion!) }

    if (self.EventType = tftaEventTypeSAND) and
       (self[0].IsCoreEvent) then
    begin
      { bei sand: alle Kinder sind KE und nicht negiert,
        nur dann ist auch das aktuelle EReignis ein KE! }
      StatusCoreEvent := true; { 1. Kind ist KE, s.o.}
      StatusNegated := False;

      { jetzt alle Kinder durchprobieren }
      for i:=1 to self.Count do
      begin
        { get child no. i }
        tempObject := self[i-1];
        { pruefen des naechsten IsCoreEvent }
        StatusCoreEvent := StatusCoreEvent and tempObject.IsCoreEvent;
        { pruefen des naechsten IsNegated : alle muessen false sein }
        StatusNegated := StatusNegated or tempObject.IsNegated;
        { LoopStatus zeigt an, ob die Schleife verlassen wurde, weil die
          Kinder des Ereignisses anzeigen, dass dieses kein KE ist (-> False)
          oder ob die Schleife verlassen wurde, weil alle Kinder abgearbeitet wurden (-> True) }
        LoopStatus := (StatusCoreEvent) and (not StatusNegated) ;
        { leave for loop if not right config of children }
        if not LoopStatus then break;
      end;

      { wenn Schleife abgebrochen wurde, weil alle Kinder abgearbeitet wurden,
        dann ist LoopStatus immer noch gesetzt und
        dann uebernehme die Ergebnisse der Untersuchung auf das aktuelle EReignis }
      if LoopStatus then
      begin
        self.IsCoreEvent:= StatusCoreEvent;
        self.IsNegated:= StatusNegated;
      end;
    end else
    { kein Basisereignis und kein sand Operator -> pruefen auf and (tftaEventTypeAND) Operator. }
    begin
      if (self.EventType = tftaEventTypeAND) and
         (self[0].IsCoreEvent) then
      begin
        { bei and: alle Kinder sind KE und negiert,
          nur dann ist auch das aktuelle EReignis ein KE! }
        StatusCoreEvent := true; { 1. Kind ist KE, s.o.}
        StatusNegated := True;

        { jetzt alle Kinder durchprobieren }
        for i:=1 to self.Count do
        begin
          { get child no. i }
          tempObject := self[i-1];
          { pruefen des naechsten IsCoreEvent }
          StatusCoreEvent := StatusCoreEvent and tempObject.IsCoreEvent;
          { pruefen des naechsten IsNegated : alle muessen true sein }
            StatusNegated := StatusNegated and tempObject.IsNegated ;
          { LoopStatus zeigt an, ob die Schleife verlassen wurde, weil die
            Kinder des Ereignisses anzeigen, dass dieses kein KE ist (-> False)
            oder ob die Schleife verlassen wurde, weil alle Kinder abgearbeitet wurden (-> True) }
          LoopStatus := (StatusCoreEvent) and (StatusNegated) ;
          if not LoopStatus then break;
        end;

        { wenn Schleife abgebrochen wurde, weil alle Kinder abgearbeitet wurden,
          dann uebernehme die Ergebnisse der Untersuchung auf das aktuelle EReignis }
        if LoopStatus then
        begin
          self.IsCoreEvent := StatusCoreEvent;
          self.IsNegated   := StatusNegated;
        end;
      end; { and Operator}
    end;   {sand Operator - else }
  end;     { kein Basisereignis  - else }

end;


{------------------------------------------------------------------------------
  prueft, ob das uebergebene Ereignis eine Ereignissequenz ist und setzt die
  Eigenschaften entsprechend
------------------------------------------------------------------------------}
procedure TTFTAObject.CheckForEventSequenceEvent;
var tempObject          : TTFTAObject;
    BreakTheLoop    : boolean;
    NumberOfNegatedChildren    : longword;
    NumberOfSequenceChildren  : longword;
    i                   : longword;
begin
  { eine EReignissequenz ist es genau dann, wenn
    - es ein nicht-negiertes Kernereignis ist.
    - ein pand aus EReignissequenzen ist.
    - ein oder mehrere negierte Kernereignisse AND eine Ereignissequenz ist. }

  NumberOfNegatedChildren := 0;
  NumberOfSequenceChildren := 0;

  if (self.IsCoreEvent) and not (self.IsNegated) then
  begin
    self.IsEventSequence := true
  end else
  begin
    { pruefen auf den pand Fall mit Ereignissequenzen }
    if self.EventType = tftaEventTypePAND then
    begin

      BreakTheLoop := False;
      { jetzt alle Kinder durchprobieren }
      for i:=1 to self.Count do
      begin

        { get child no. i }
        tempObject := self[i-1];

        BreakTheLoop := not (tempObject.IsEventSequence);
        if BreakTheLoop then break;
      end;

      { wenn kein BreakTheLoop, sondern alle Kinder regulaer durchlaufen,
        dann ist das aktuelle Ereignis eine Ereignissequenz }
      if not BreakTheLoop then
      begin
        self.IsEventSequence := true;
      end;

    end else
      { pruefen auf den and (tftaEventTypeAND) Fall mit mehreren negierten KE und genau einer Ereignissequenz }
      if self.EventType = tftaEventTypeAND then
      begin

        BreakTheLoop := False;
        { jetzt alle Kinder durchprobieren }
        for i:=1 to self.Count do
        begin

          { get child no. i }
          tempObject := items[i-1];

          if (tempObject.IsCoreEvent) and
             (tempObject.IsNegated) then
          begin
            { Kind ist negierte KE }
            inc(NumberOfNegatedChildren);
          end else
          begin
            if tempObject.IsEventSequence then
            begin
              { Kind ist kein negiertes KE aber selbst EReignissequenz }
              inc(NumberOfSequenceChildren);
            end else
            begin
              { wenn Kind weder negierte KE noch Ereignissequenz,
                dann ist aktuelles EReignis auch keine Ereignissequenz  }
              BreakTheLoop := True;
            end;
          end;
          { Abbruch auch dann, wenn mehr als eine Ereignissequenz... }
          BreakTheLoop := BreakTheLoop or ( NumberOfSequenceChildren > 1 );
          if BreakTheLoop then break;

        end;

        { wenn kein BreakTheLoop, sondern alle Kinder regulaer durchlaufen,
          UND genau eines eine Ereignissequenz war, dann ist das aktuelle
          Ereignis eine Ereignissequenz }
        if (not BreakTheLoop) and (NumberOfSequenceChildren = 1) then
        begin
          self.IsEventSequence := true;
        end;

      end; {Typ = tftaEventTypeAND }

  end; { nicht nicht-negiertes KE }

end;

{------------------------------------------------------------------------------
  prueft, ob das uebergebene Ereignis eine erweitrte EReignissequenz ist und setzt die
  Eigenschaften entsprechend
------------------------------------------------------------------------------}
procedure TTFTAObject.CheckForExtendedSequenceEvent;
begin

end;
{------------------------------------------------------------------------------
  prueft, ob das uebergebene Ereignis Disjunkte Operanden besitzt ist und setzt
  die Eigenschaften entsprechend
------------------------------------------------------------------------------}
procedure TTFTAObject.CheckForDisjunctEvent;
begin

end;

procedure TTFTAObject.DEBUGPrint(isUpdate : boolean; eventlist : TTFTAEventLookupList; thestring : ansistring = '');
begin
  if Assigned(self.DEBUGMemo) then
    if isUpdate then
    begin
      self.DEBUGMemo.Append(thestring);
      self.DEBUGMemo.Append('Update: ' + PointerAddrStr(eventlist[self.PosInEventList]) +
                            ' ::: ' + self.TemporalExpr );
    end else
    begin
      self.DEBUGMemo.Append(thestring);
      self.DEBUGMemo.Append('New   : ' + PointerAddrStr(eventlist[self.PosInEventList]) +
                            ' ::: ' + self.TemporalExpr ) ;
    end;
  eventlist.pointerToApplication.ProcessMessages;
end;
{------------------------------------------------------------------------------
  prueft und setzt ggf. die Eigenschaften eines temporalen Terms wie
  IsDisjunct, IsEventSequence ...
  dazu wird fuer jede Eigenschaft eine separate Routine durchlaufen
------------------------------------------------------------------------------}
procedure TTFTAObject.CheckTermProperties;
begin

    self.CheckForEventSequenceEvent;
    self.CheckForExtendedSequenceEvent;
    self.CheckForDisjunctEvent;
    { a direct check whether the term is negated or not is not necessary as this
      property is set directly while creating each negated object }

end;




{ ############################################################################ }
{ TTFTAList }
function TTFTAList.Add(Item: TTFTAObject): Integer;
begin
  Result := inherited Add(Item);
  //self.Last.DEBUGMemo := self.Owner.DEBUGMemo;
end;

procedure TTFTAList.Assign(Obj: TTFTAList);
begin
  inherited Assign(Obj);
end;

procedure TTFTAList.Clear;
var
  i: longword;
begin
  for i := 0 to self.Count - 1 do
    self.Extract(self[i]);
end;

{ Delete all copies within list}
function TTFTAList.DeleteAllCopies : boolean;
var i : longword;
begin
  { outer loop: for each child do... (repeat until because of number of children
    possibly varying during this operation as doublets are deleted from list) }
  i := 0;
  Result := false;
  repeat
    if self.DeleteAllCopiesOfObject(self[i]) then
      Result := true;
    inc(i);
  until i >= (self.Count - 1);
end;

{ Delete all copies of theItem }
function TTFTAList.DeleteAllCopiesOfObject(theItem : TTFTAObject) : boolean;
var tempObject : TTFTAObject;
    posOfObject : integer = 0;
    somethingDone : boolean = false;
begin
  Result := false;
  self.OwnsObjects := false;
  tempObject := theItem;
  posOfObject := self.IndexOf(theItem);
  if (posOfObject > -1) then
  begin
    while Assigned(self.Extract(theItem)) do
    begin
      { if there are more then one copies of theItem, then the while loop
        is passed at least once with somethingDone allready true;
        in this case a deletion is actually done --> Result = true }
      if somethingDone then Result := true;
      somethingDone := true;
    end;
    if somethingDone then
    begin
      { the first occurrence of the Item has also been extracted, thus
        insert it again (only copies shall be deleted, after all! }
      self.Insert(posOfObject,tempObject);
    end;
  end;
end;

function TTFTAList.Extract(Item: TTFTAObject):TTFTAObject;
begin
  Result := TTFTAObject(inherited Extract(TObject(Item)));
  if Assigned(Result) and Result.NeedsToBeUpdated then
  begin
    Result := Result.PointerToUpdateObject;
  end;
end;

{ search for theItem in TTFTAList, but only deliver Result, if after StartAfter
  ( 0-based )! If after StartAfter no theItem was found default to Result := 0 }
function TTFTAList.FindNextAfter(theItem : TTFTAObject; StartAfter : Integer) : Integer;
var i,j  : longword;
begin
  Result := 0; {default}
  i := StartAfter + 1;
  j := self.Count - 1;
  if i <= j then
  begin

    repeat
      if (theItem = self[i]) then
        Result := i;
      inc(i);
    until (Result <> 0) or (i > j);

  end;
end;

function TTFTAList.First :TTFTAObject;
begin
  Result := TTFTAObject(inherited First);
  if Assigned(Result) and Result.NeedsToBeUpdated then
  begin
    self[0] := Result.PointerToUpdateObject;
    Result := self[0];
  end;
end;

function TTFTAList.GetItem(Index: longword): TTFTAObject;
begin
  Result := TTFTAObject(inherited Items[Index]);
  if Assigned(Result) and Result.NeedsToBeUpdated then
  begin
    Result.DEBUGMemo.Append(PointerAddrStr(Result) + ' --> ' + PointerAddrStr(Result.PointerToUpdateObject));
    writeln(PointerAddrStr(Result) + ' --> ' + PointerAddrStr(Result.PointerToUpdateObject));
    self[Index] := Result.PointerToUpdateObject;
    Result := self[Index];
  end;
end;

function TTFTAList.GetPlainItem(Index: longword): TTFTAObject;
var s:ansistring;
begin
  Result := TTFTAObject(inherited Items[Index]);
end;

function TTFTAList.IndexOf(Item: TTFTAObject): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TTFTAList.Insert(Index: longword; Item: TTFTAObject);
begin
  inherited Insert(Index, Item);
end;

function TTFTAList.Last :TTFTAObject;
begin
  Result := TTFTAObject(inherited Last);
  if Assigned(Result) and Result.NeedsToBeUpdated then
  begin
    self[self.Count-1] := Result.PointerToUpdateObject;
    Result := self[self.Count-1];
  end;
end;

function TTFTAList.Remove(Item: TTFTAObject): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TTFTAList.SetItem(Index: longword; Item: TTFTAObject);
begin
  inherited Items[Index] := Item;
end;

{ ############################################################################ }
{ TTFTAEventLookupList }
constructor TTFTAEventLookupList.Create(pointerToDebugMemo : TMemo);
begin
  inherited Create;
  self.DEBUGMemo  := pointerToDebugMemo;
  self.OwnsObjects:= true; { the EventLookupList owns objects (the objects.children - list does not! }

  { create the FALSE event }
  self.VtheFALSE := self.NewItem( tftaEventTypeBASIC,
                                  true,  {IsBasicEvent}
                                  true,  {IsCoreEvent}
                                  false, {IsEventSequence}
                                  true,  {IsNegated}
                                  false, {IsNotCompletelyBuildYet}
                                  false, {IsDisjunct}
                                  false, {IsExtendedSequence}
                                  false, {LogicalValue}
                                  false, {NeedsToBeUpdatedfalse}
                                  NIL,   {PointerToUpdateObject}
                                  BoolToString(false)  { temporal Expression }
                                 );
  { create the TRUE event }
  self.VtheTRUE := self.NewItem(tftaEventTypeBASIC,
                                true,  {IsBasicEvent}
                                true,  {IsCoreEvent}
                                false, {IsEventSequence}
                                false, {IsNegated}
                                false, {IsNotCompletelyBuildYet}
                                false, {IsDisjunct}
                                false, {IsExtendedSequence}
                                true,  {LogicalValue}
                                false, {NeedsToBeUpdatedfalse}
                                NIL,   {PointerToUpdateObject}
                                BoolToString(true)  { temporal Expression }
                               );
end;

function TTFTAEventLookupList.Add(Item: TTFTAObject): Integer;
begin
  Result := inherited Add(Item);
end;

function TTFTAEventLookupList.NewItem: TTFTAObject;
begin
  Result:=
    self.NewItem( tftaEventTypeOTHER, { type }
                  false,  {IsBasicEvent              }
                  false,  {IsCoreEvent               }
                  false,  {IsEventSequence           }
                  false,  {IsNegated                 }
                  false,  {IsNotCompletelyBuildYet   }
                  false,  {IsDisjunct                }
                  false,  {IsExtendedSequence        }
                  NIL,    {LogicalValue              }
                  false,  {NeedsToBeUpdated          }
                  NIL,    {PointerToUpdateObject     }
                  'DUMMY' {TemporalExpr              }
                 );
end;
{ public: version with options and true/false as logical value }
function TTFTAEventLookupList.NewItem(EventType               : TTFTAOperatorType;
                                      IsBasicEvent            : boolean;
                                      IsCoreEvent             : boolean;
                                      IsEventSequence         : boolean;
                                      IsNegated               : boolean;
                                      IsNotCompletelyBuildYet : boolean;
                                      IsDisjunct              : boolean;
                                      IsExtendedSequence      : boolean;
                                      LogicalValue            : boolean;
                                      NeedsToBeUpdated        : boolean;
                                      PointerToUpdateObject   : TTFTAObject;
                                      TemporalExpr            : ansistring) : TTFTAObject;
begin
  Result:= self.NewItemPrivate(EventType,IsBasicEvent,IsCoreEvent,IsEventSequence,
                          IsNegated,IsNotCompletelyBuildYet,IsDisjunct,IsExtendedSequence,
                          NeedsToBeUpdated,PointerToUpdateObject,TemporalExpr);
  Result.SetLogicalValue(LogicalValue) ;
end;

{ version with options and NIL as logical value }
function TTFTAEventLookupList.NewItem(EventType               : TTFTAOperatorType;
                                      IsBasicEvent            : boolean;
                                      IsCoreEvent             : boolean;
                                      IsEventSequence         : boolean;
                                      IsNegated               : boolean;
                                      IsNotCompletelyBuildYet : boolean;
                                      IsDisjunct              : boolean;
                                      IsExtendedSequence      : boolean;
                                      LogicalValue            : pointer;
                                      NeedsToBeUpdated        : boolean;
                                      PointerToUpdateObject   : TTFTAObject;
                                      TemporalExpr            : ansistring) : TTFTAObject;
begin
  Result:= self.NewItemPrivate(EventType,IsBasicEvent,IsCoreEvent,IsEventSequence,
                          IsNegated,IsNotCompletelyBuildYet,IsDisjunct,IsExtendedSequence,
                          NeedsToBeUpdated,PointerToUpdateObject,TemporalExpr);
  Result.SetLogicalValue(NIL);
end;

{ private version}
function TTFTAEventLookupList.NewItemPrivate( EventType               : TTFTAOperatorType;
                                              IsBasicEvent            : boolean;
                                              IsCoreEvent             : boolean;
                                              IsEventSequence         : boolean;
                                              IsNegated               : boolean;
                                              IsNotCompletelyBuildYet : boolean;
                                              IsDisjunct              : boolean;
                                              IsExtendedSequence      : boolean;
                                              NeedsToBeUpdated        : boolean;
                                              PointerToUpdateObject   : TTFTAObject;
                                              TemporalExpr            : ansistring) : TTFTAObject;
var posInList: longword = 0;
begin
  posInList := self.Add(TTFTAObject.Create);
  Result := self[posInList];

  Result.EventType                  :=  EventType                 ;
  Result.DEBUGMemo                  :=  self.DEBUGMemo            ;
  Result.IsBasicEvent               :=  IsBasicEvent              ;
  Result.IsCoreEvent                :=  IsCoreEvent               ;
  Result.IsEventSequence            :=  IsEventSequence           ;
  Result.IsNegated                  :=  IsNegated                 ;
  Result.IsNotCompletelyBuildYet    :=  IsNotCompletelyBuildYet   ;
  Result.IsDisjunct                 :=  IsDisjunct                ;
  Result.IsExtendedSequence         :=  IsExtendedSequence        ;
  Result.NeedsToBeUpdated           :=  NeedsToBeUpdated          ;
  Result.PointerToUpdateObject      :=  PointerToUpdateObject     ;
  Result.PosInEventList             :=  posInList                 ;
  Result.TemporalExpr               :=  TemporalExpr              ;

end;

function TTFTAEventLookupList.GetItem(Index: longword): TTFTAObject;
begin
  Result := TTFTAObject(inherited Items[Index]);
end;

procedure TTFTAEventLookupList.SetItem(Index: longword; Item: TTFTAObject);
begin
  inherited Items[Index] := Item;
end;

function  TTFTAEventLookupList.ListHoldsObjectAt(Text : ansistring) : TTFTAObject;
var i : longword;
begin
  Result := nil;
  for i:= 1 to self.Count do
  begin
    if AnsiCompareStr(Text,self[i-1].TemporalExpr) <> 0 then
      Result := nil
    else
      Result := self[i-1];
    if Assigned(Result) then break;
  end;
end;

end.

