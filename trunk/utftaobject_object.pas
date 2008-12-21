{ this is an include file to utftaobject.pas }

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

{ ############################################################################ }
{ TTFTAObject }
{------------------------------------------------------------------------------
  Class-Constructor
------------------------------------------------------------------------------}
constructor TTFTAObject.Create;
begin

  inherited Create;
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
  Class-Destructor
------------------------------------------------------------------------------}
destructor TTFTAObject.Destroy;
begin
  // nothing specific yet...
  inherited Destroy;
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
  prueft, ob das uebergebene Ereignis ein Kernereignis ist und setzt die
  Eigenschaften entsprechend
------------------------------------------------------------------------------}
procedure TTFTAObject.CheckForCoreEvent;
var tempObject          : TTFTAObject;
    StatusNegated       : boolean;
    StatusCoreEvent  : boolean;
    LoopStatus     : boolean;
    i                   : Integer;
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

    if (self.IsTypeSAND) and
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
      if (self.IsTypeAND) and
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
  prueft, ob das uebergebene Ereignis Disjunkte Operanden besitzt ist und setzt
  die Eigenschaften entsprechend
------------------------------------------------------------------------------}
procedure TTFTAObject.CheckForDisjunctEvent;
begin
  { nothing right now }
end;
{------------------------------------------------------------------------------
  prueft, ob das uebergebene Ereignis eine Ereignissequenz ist und setzt die
  Eigenschaften entsprechend
------------------------------------------------------------------------------}
procedure TTFTAObject.CheckForEventSequenceEvent;
var tempObject          : TTFTAObject;
    BreakTheLoop    : boolean;
    NumberOfNegatedChildren    : Integer;
    NumberOfSequenceChildren  : Integer;
    i                   : Integer;
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
    if self.IsTypePAND then
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
      if self.IsTypeAND then
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
  True if commutative term
------------------------------------------------------------------------------}
function TTFTAObject.CheckIsCommutative : boolean;
begin
  Result := ( not self.IsTypePAND ) and ( self.Count > 1 );
  { this is the same as (but shorter/quicker than)
     Result := (self.IsTypeAND) or
               (self.IsTypeOR) or
               (self.IsTypeXOR) or
               (self.IsTypeSAND);     }
end;
{------------------------------------------------------------------------------
  true if operator = XXX
------------------------------------------------------------------------------}
function TTFTAObject.CheckIsTypeAND : boolean;
begin
  Result := (self.EventType = tftaEventTypeAND);
end;
function TTFTAObject.CheckIsTypeNOT : boolean;
begin
  Result := (self.EventType = tftaEventTypeNOT);
end;
function TTFTAObject.CheckIsTypePAND : boolean;
begin
  Result := (self.EventType = tftaEventTypePAND);
end;
function TTFTAObject.CheckIsTypeOR : boolean;
begin
  Result := (self.EventType = tftaEventTypeOR);
end;
function TTFTAObject.CheckIsTypeOTHER : boolean;
begin
  Result := (self.EventType = tftaEventTypeOTHER);
end;
function TTFTAObject.CheckIsTypeSAND : boolean;
begin
  Result := (self.EventType = tftaEventTypeSAND);
end;
function TTFTAObject.CheckIsTypeTOP : boolean;
begin
  Result := (self.EventType = tftaEventTypeTOP);
end;
function TTFTAObject.CheckIsTypeXOR : boolean;
begin
  Result := (self.EventType = tftaEventTypeXOR);
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
{------------------------------------------------------------------------------
  makes a clone of the TTFTAobject
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
  Result.IsSorted := self.IsSorted;
  Result.EventLookupList := self.EventLookupList;
end;
{------------------------------------------------------------------------------
  Number of children
------------------------------------------------------------------------------}
function TTFTAObject.Count : Integer;
begin
  if Assigned(self.Children) then
    Result := self.Children.Count
  else
    Result := 0;
end;
{$IfDef TESTMODE}
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
{$EndIf}
{------------------------------------------------------------------------------
  Delete a child
------------------------------------------------------------------------------}
procedure TTFTAObject.DeleteChild(Index: Integer);
begin
  if Assigned(self.Children) then self.Children.Delete(Index);
end;
{------------------------------------------------------------------------------
  Liefert Typ des Ereignisses (aus VType) als String
------------------------------------------------------------------------------}
function TTFTAObject.EventTypeToString : string;
begin
  Result := cEventTypeStringArray[ord(self.EventType)]
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
  Returns child nr. Index
------------------------------------------------------------------------------}
function TTFTAObject.GetChild(Index: Integer): TTFTAObject;
begin
  if Assigned(self.Children) then
    Result := self.Children[Index]
  else
    Result := NIL;
end;
{------------------------------------------------------------------------------
  True, if there are entries in Children, false otherwise
------------------------------------------------------------------------------}
function  TTFTAObject.GetChildrenBasicState : boolean;
var i : Integer;
begin
  Result := True; { Default }
  if self.HasChildren then
     for i:=0 to ( self.Count-1 ) do
         Result := Result and self[i].IsBasicEvent
  else
    Result := False; { no children -> children cant be basic events }
end;
{------------------------------------------------------------------------------
  Reads VPointerToUpdateObject, but if the object VPointerToUpdateObject points
  to itself has NeedsToBeUpdated set, then return
  VPointerToUpdateObject.GetPointerToUpdateObject
------------------------------------------------------------------------------}
function TTFTAObject.GetPointerToUpdateObject : TTFTAObject;
begin
  Result := self.VPointerToUpdateObject;
  if Assigned(Result) and (Result.NeedsToBeUpdated) then
  begin
    self.RedirectMe(Result.VPointerToUpdateObject);
    Result := Result.VPointerToUpdateObject;
  end;
end;
{------------------------------------------------------------------------------
  Provides the logical expression represented by the term / object
  Scanns through all descendants, which is time consuming
  use with care and as seldom as possible
------------------------------------------------------------------------------}
function TTFTAObject.GetTempExpr : ansistring;
var i,j : Integer;
    tempObject : TTFTAObject;
    doubleObject : TTFTAObject;
begin

  { for all parts concerning the scanning and changing of TTFTAObject.IsSorted
    within this function see the algorithm documentation (look for "sorting") }
  if (not self.IsBasicEvent) and (not self.NeedsToBeUpdated) then
  begin
    if not self.IsNotCompletelyBuildYet then
    begin
      if self.HasChildren then
      begin
        j := self.Count;
        Result := '';
        if j > 1 then  { and, or, xor, pand, sand operators }
        begin
          for i:=0 to j-2 do
          begin
            Result := Result + self[i].TemporalExpr + ',';
          end;
          Result := Result + self[i+1].TemporalExpr;
        end else  { not operator or one of the above in rare cases, where
                    transformation results in only single parameter }
        begin
          Result := self[0].TemporalExpr;
          { set own prestored value to the new string derived from the children}
          self.VExpr := Result;
          { no sort necessary, as only one child }
        end;
        Result := self.EventTypeToString + '[' + Result + ']';
      end else
      begin { no basic event, no NeedsToBeUpdated, build completely but no children ?! }
        Result := '###ERROR11241###' ;
      end;
    end else
    begin
     Result := ''; { if object is just in built-up then it has no TempExpr (because it is still changing) }
    end;
  end else
  begin;
    { basic event or outdated event (NeedsToBeUpdated = true).
      Name of basic event is stored in VExpr at creation of basic event;
      "out of order" is stored if NeedsToBeUpdated = true }
    Result := self.PlainTemporalExpr;
  end;

end;
{------------------------------------------------------------------------------
  True, if there are entries in Children, false otherwise
------------------------------------------------------------------------------}
function TTFTAObject.HasChildren : boolean;
begin
  Result := (self.Count > 0);
end;
{------------------------------------------------------------------------------
  Insert a new child (Item) at pos Index
------------------------------------------------------------------------------}
procedure TTFTAObject.InsertChild(Index: Integer; Item: TTFTAObject);
begin
  if Assigned(self.Children) then self.Children.Insert(Index, Item);
end;

//{------------------------------------------------------------------------------
  //Compares itself with a given other TTFTAObject; returns True, if both are
  //identical and false otherwise.
//------------------------------------------------------------------------------}
//function TTFTAObject.IsIdenticalTo(theOtherItem: TTFTAObject) : boolean;
//var numberOfChildren : integer;
    //i : integer = 0;
//begin
  //{ before actually starting the time consuming iterative walkthrough some
    //quick checks are performed. }
  //Result := (self.EventType = theOtherItem.EventType);
  //numberofChildren := self.Count;
  //Result := Result and (numberofChildren = theOtherItem.Count);
  //{ only continue if the quick checks gave true }
  //if Result then
  //begin
    //if (numberofChildren > 0) then
    //begin { now walkthrough the children iteratively }
      //{ both have the same number of children, see second check from above }
      //repeat
        //Result := Result and self[i].IsIdenticalTo(theOtherItem[i]);
        //inc(i);
      //until (not result) or (i = numberOfChildren);
    //end else
    //begin { no children in both events --> compare the names (aka TemporalExpr) }
      //Result := Result and (self.TemporalExpr = theOtherItem.TemporalExpr);
    //end;
  //end;
//end;


{------------------------------------------------------------------------------
  Redirects (updates) object to newItem and sets properties accordingly
------------------------------------------------------------------------------}
procedure TTFTAObject.RedirectMe(newItem : TTFTAObject);
begin
  self.PointerToUpdateObject:=newItem;
  self.Children.Clear;
  self.TemporalExpr:='redirected to ' + PointerAddrStr(newItem);
  self.NeedsToBeUpdated:=true;
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
  Set child nr. Index to Item
------------------------------------------------------------------------------}
procedure TTFTAObject.SetChild(Index: Integer; Item: TTFTAObject);
begin
  if Assigned(self.Children) then self.Children[Index] := Item;
end;
{------------------------------------------------------------------------------
  set IsBasicEvent and properties accordingly
------------------------------------------------------------------------------}
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
{------------------------------------------------------------------------------
  Sets the logical expression represented by the term / object
  Only possible with basic events!
------------------------------------------------------------------------------}
procedure TTFTAObject.SetTempExpr(theExpr : ansistring);
begin
  if (not self.HasChildren) then self.VExpr:=theExpr;
end;
procedure TTFTAObject.SetVType(Parameter : TTFTAOperatorType);
begin
  self.VType := Parameter;
  if self.VType = tftaEventTypeBASIC then
    self.SetIsBasicEvent(true)
  else
    self.SetIsBasicEvent(false);
  if self.VType = tftaEventTypeNOT then
    self.IsNegated := true
  else
    self.IsNegated := false;
end;
{------------------------------------------------------------------------------
  Liefert als String eine Uebersicht ueber die Eigenschaften des Objekts.
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

//  [include file EOF]
