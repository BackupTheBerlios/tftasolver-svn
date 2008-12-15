unit utftalogik;

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
// switch TESTMODE on or of globally by editing testmode.conf (found in main path
// of TFTASolver.tftasolver
{$INCLUDE testmode.conf}

interface

uses
  Classes, SysUtils, Dialogs, ComCtrls, StdCtrls, strutils, utftaobject,
    sjspointertools, utftastringlist;
  

  { "public" functions / Procedures }
  function  SimplificationLoop(theobject :TTFTAObject; theParent : TTFTAList; theIndex : Integer; theEventList : TTFTAEventLookupList) : boolean;
  function  SortOperands(tT :TTFTAObject; tP : TTFTAList; tI : Integer; tEL : TTFTAEventLookupList) : boolean;

implementation

  { "private" functions / Procedures }
  function  ANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  ANDSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  ANDTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  GenericCombine(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  GenericSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList; theType : TTFTAOperatorType) : boolean; forward;
  procedure GenericUpdateObject(oldTerm : TTFTAObject; newTerm : TTFTAObject; eventlist : TTFTAEventLookupList; parentList : TTFTAList; oldObjectListIndex : Integer; callString : ansistring = ''); forward;
  function  LawOfCompleteness(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean; forward;
  function  LawOfIdempotency(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean; forward;
  function  LawOfNonrecurrence(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList ) : boolean; forward;
  function  NOTFalseTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;  forward;
  function  NOTNOT(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;  forward;
  function  ORSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;     forward;
  function  ORXORFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;   forward;
  function  ORXORTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;  forward;
  function  PANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean; forward;
  function  PANDMultiples(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;  forward;
  function  PANDPANDTransform(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;  forward;
  function  PANDSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;  forward;
  function  ScanChildrenSorting(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList; var isChange : boolean) : ansistring; forward;
  function  SANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean; forward;
  function  SANDPANDTransform(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;  forward;
  function  SANDSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean; forward;
  function  XORSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean; forward;

{------------------------------------------------------------------------------
  transform and[ ... False ... ] to False
------------------------------------------------------------------------------}
function ANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var AtLeastOneIsFalse : boolean = False;
    i : Integer;
begin
  Result := False;
  if (currentTerm.EventType = tftaEventTypeAND) then
  begin
    i := 0;
    repeat
      if currentTerm[i] = eventList.TheFALSEElement then
        AtLeastOneIsFalse := True ;
      inc(i);
    until ( i = (currentTerm.Count) ) or ( AtLeastOneIsFalse ) ;

    if ( AtLeastOneIsFalse ) then
    begin
      GenericUpdateObject(currentTerm,eventlist.theFALSEElement,eventlist,theParent,theIndex,'ANDFalse 1');
      Result := True;
    end;
  end;
end;

{------------------------------------------------------------------------------
  split AND with more than two operands
------------------------------------------------------------------------------}
function ANDSplit( term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := GenericSplit(term,theParent,theIndex,eventlist,tftaEventTypeAND);
end;

{------------------------------------------------------------------------------
  transform and[ ... True ... ] to and[ ... ]
------------------------------------------------------------------------------}
function ANDTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := False;
  if (currentTerm.EventType = tftaEventTypeAND) then
  begin
    while Assigned(currentTerm.ExtractChild(eventlist.TheTRUEElement)) do
    begin
      Result := True;
    end;
    if Result then
    begin
      if not currentTerm.HasChildren then
      begin;
        GenericUpdateObject(currentTerm,eventlist.TheTRUEElement,eventlist,theParent,theIndex,'ANDTrue');
      end else
      begin
        if currentTerm.Count = 1 then
        begin;
          GenericUpdateObject(currentTerm,currentTerm[0],eventlist,theParent,theIndex,'ANDTrue');
        end;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Generic combine of commutative terms (incl. sorting)
------------------------------------------------------------------------------}
function GenericCombine(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var myType : TTFTAOperatorType;
    i : integer;
    nowPosInTerm : integer;
    nowSizeArray : integer;
    SameTypeTerms : array of integer;
    numberChildren : integer;
    tempObject : TTFTAObject;
begin
  Result := False;
  myType := currentTerm.EventType;
  if ( (myType = tftaEventTypeAND) or
       (myType = tftaEventTypeSAND) or
       (myType = tftaEventTypeOR) or
       (myType = tftaEventTypeXOR)
     ) then
  begin
    { scan all children and get all objects of same type as currentTerm;
      this is done in a single loop }
    nowPosInTerm := 0;
    nowSizeArray := 0;
    numberChildren := currentTerm.Count;
    repeat
      if (currentTerm[nowPosInTerm].EventType = myType) then
      begin { if same type }
        inc(nowSizeArray);
        SetLength(SameTypeTerms,nowSizeArray); { expand array }
        SameTypeTerms[nowSizeArray-1] := nowPosInTerm; { add current pos to array }
      end;
      inc(nowPosInTerm);
    until (nowPosInTerm = numberChildren);
    if nowSizeArray > 0 then
    begin { at least one same type event was found }
      Result := true;
      {$IfDef TESTMODE}
        currentTerm.DEBUGPrint(true,eventlist,'GenericCombine 1');
      {$ENDIF}
      { for each of the same type events do }
      for i := 0 to nowSizeArray-1 do
      begin
        { get all children of the event and add them to current Event;
          do not yet extract the same type events as this would interfere with the
          indexes stored in SameTypeTerms; adding at the end does not interfere here. }
        nowPosInTerm := 0;
        numberChildren := currentTerm[SameTypeTerms[i]].Count;
        if numberChildren > 0 then
        repeat
          currentTerm.AddChild(currentTerm[SameTypeTerms[i]][nowPosInTerm]);
          inc(nowPosInTerm);
        until (nowPosInTerm = numberChildren);
      end;
      { now extract all SameTypeTerms from currentTerm; in order to minimize
        interference we do this from back to front }
      for i := nowSizeArray-1 downto 0 do
      begin
        currentTerm.DeleteChild(SameTypeTerms[i]);
      end;
      { now sort currentTerm (thereby checking whether an identical object
        already is listed in Eventlist }
      SortOperands(currentTerm,theParent,theIndex,eventlist);
      currentTerm.CheckTermProperties;
      {$IfDef TESTMODE}
        currentTerm.DEBUGPrint(true,eventlist,'GenericCombine 2');
      {$ENDIF}

    end; { if nowSizeArray > 0 }
  end;
end;

{------------------------------------------------------------------------------
  Generic split of terms with more than two operands
  Called by PANDSplit, SANDSplit, ANDSPlit, ORSplit, XORSplit
------------------------------------------------------------------------------}
function GenericSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList; theType : TTFTAOperatorType) : boolean;
var tempObject     : TTFTAObject = NIL;
    currentOperand : TTFTAObject = NIL;
    nextOperand    : TTFTAObject = NIL;
    nextIndex      : Integer = 1;
    termCount      : Integer;
    s1   : ansistring;
    s2   : ansistring;
    expr : ansistring;
begin
  { note: no sorting necessary; if commutative, term should already be sorted and
    as splitting is done from left to right, the order of operands is never altered. }
  Result := False;
  { only the term itself may be modified! its children (their instances!) must not be
    touched, therefore a new object is to be created }
  termCount := term.Count;
  if (term.EventType = theType) and (termCount > 2) then
  begin
    Result := True;
    { check wether a new object is really needed to be created or wether there
      already is an object like the one that needs to be created. In the latter
      case do not create a new one but link the existing one. Checking and linking
      isdone via the EventList. }
    currentOperand := term[0]; { get first child }
    s1 := currentOperand.TemporalExpr;
    nextIndex := 1;
    repeat
      nextOperand := term[nextIndex]; { get second child }
      s2 := nextOperand.TemporalExpr;
      expr := cEventTypeStringArray[ord(theType)] + '[' + s1 + ',' + s2 + ']';
      tempObject := eventlist.ListHoldsObjectAt(expr);
      if not Assigned(tempObject) then
      begin
        { create new xxx object }
        tempObject := eventlist.NewItem;
        tempObject.EventType := theType;
        tempObject.AddChild(currentOperand);
        tempObject.AddChild(nextOperand);
        tempObject.CheckTermProperties;
      end;
      { else take existing object = do nothing }

      { for next loop }
      currentOperand := tempObject;
      s1 := expr;
      inc(nextIndex);

    until nextIndex = termCount;
    { modify term }
    GenericUpdateObject(term,currentOperand,eventlist,theParent,theIndex,'GenericSplit called from ' + cEventTypeStringArray[ord(theType)]);
  end;
end;


procedure GenericUpdateObject(oldTerm : TTFTAObject; newTerm : TTFTAObject; eventList : TTFTAEventLookupList;
                              parentList : TTFTAList; oldObjectListIndex : Integer; callString : ansistring = '');
begin
  { note: no sorting necessary; sorting must be done prior to calling this routine }
  if oldTerm = newTerm then exit;

  oldTerm.PointerToUpdateObject:=newTerm;
  oldTerm.NeedsToBeUpdated:=true;

  { if parentList is provided, then do the update within parentList;
    there is a second possibility: for sorting it is necessary to call SortOperands
    for new terms that do not yet are assigned within the overall TemporalExpr and
    thus do not have a parentList and oldObjectIndex; for those do only redirect
    the pointer to the new term }
  if assigned(parentList) then
  begin
    oldterm := parentList[oldObjectListIndex];

    { next calling of parentList[oldObjectListIndex] should be redirected to new term }
    if oldterm = newTerm then
    begin
      parentList.Owner.CheckTermProperties;
      {$IfDef TESTMODE}
        if Assigned(oldterm.DEBUGMemo) then
          oldterm.DEBUGPrint(true,eventList,callString);
        eventlist.pointerToApplication.ProcessMessages;
      {$ENDIF}
    end else
      ShowMessage('Fatal Error (081210.1221) while processing event ' +
                  PointerAddrStr(oldTerm) + ' ( ' + oldTerm.TemporalExpr + ' ) ' );
  end else
  begin  { case without parentList }
    {$IfDef TESTMODE}
      if Assigned(newterm.DEBUGMemo) then
        newterm.DEBUGPrint(true,eventList,callString + ' (without assigned partenList!)');
      eventlist.pointerToApplication.ProcessMessages;
    {$ENDIF}
  end;

end;

{------------------------------------------------------------------------------
  law of completeness and[x,y] = pand[x,y] xor pand[y,x] xor sand[x,y]
------------------------------------------------------------------------------}
function LawOfCompleteness(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var aTerm,bTerm,cTerm,xTerm,yTerm, tempTerm : TTFTAObject;
    aExpr,bExpr,cExpr,xExpr,yExpr,overallexpr : ansistring;
begin
  Result := False;
  { only the term itself may be modified! its children (their instances!) must not be
    touched, therefore three new objects are to be created }
  if (currentTerm.EventType = tftaEventTypeAND) and (not currentTerm.IsAllChildrenAreBasic) then
  begin
    Result := True;
    { if currentTerm.Count > 2 then split currentTerm first }
    if currentTerm.Count > 2 then
    begin
      if ANDSplit(currentTerm, theParent, theIndex, eventList) then
        currentTerm := theParent[theIndex]
    end;

    { iff x and y are not atomic (i.e. basic events) the original object (the AND term) will become an
      XOR(                     [the original object]
          PAND(x,y),           [the first new object called "a"]
          PAND(y,x),           [the second new object called "b"]
          SAND(X,Y)            [the third new object called "c"]
         )
    }
    { thus, a total of four objects has to be checked for already existing }

    xTerm := currentTerm[0];
    yTerm := currentTerm[1];

    xExpr := xTerm.TemporalExpr;
    yExpr := yTerm.TemporalExpr;

    aExpr := 'PAND[' + xExpr + ',' + yExpr + ']';
    bExpr := 'PAND[' + yExpr + ',' + xExpr + ']';
    cExpr := 'SAND[' + xExpr + ',' + yExpr + ']';
    overallexpr := 'XOR[' + aExpr + ',' + bExpr + ',' + cExpr + ']';

    tempTerm := eventlist.ListHoldsObjectAt(overallexpr);
    if not Assigned(tempTerm) then
    begin  { overall term does not exist }
      aTerm := eventlist.ListHoldsObjectAt(aExpr);
      if not Assigned(aTerm) then
      begin { aExpr term does not exist }
        aTerm := eventlist.NewItem;
        aTerm.EventType := tftaEventTypePAND;
      end;
      bTerm := eventlist.ListHoldsObjectAt(bExpr);
      if not Assigned(bTerm) then
      begin { bExpr term does not exist }
        bTerm := eventlist.NewItem;
        bTerm.EventType := tftaEventTypePAND;
      end;
      cTerm := eventlist.ListHoldsObjectAt(cExpr);
      if not Assigned(cTerm) then
      begin { cExpr term does not exist }
        cTerm := eventlist.NewItem;
        cTerm.EventType := tftaEventTypeSAND;
      end;
      { extract old children from overall term }
      currentTerm.ExtractChild(currentTerm[0]);
      currentTerm.ExtractChild(currentTerm[0]);
      {add new children }
      aTerm.AddChild(xTerm);
      aTerm.AddChild(yTerm);
      aTerm.CheckTermProperties;
      {$IfDef TESTMODE}aTerm.DEBUGPrint(false,eventlist,'LawOfCompleteness 1'); {$ENDIF}

      bTerm.AddChild(yTerm);
      bTerm.AddChild(xTerm);
      bTerm.CheckTermProperties;
      {$IfDef TESTMODE}bTerm.DEBUGPrint(false,eventlist,'LawOfCompleteness 2'); {$ENDIF}

      cTerm.AddChild(xTerm);
      cTerm.AddChild(yTerm);
      cTerm.CheckTermProperties;
      {$IfDef TESTMODE}cTerm.DEBUGPrint(false,eventlist,'LawOfCompleteness 3'); {$ENDIF}

      { add the new children to the overall term }
      currentTerm.AddChild(aTerm);
      currentTerm.AddChild(bTerm);
      currentTerm.AddChild(cTerm);
      { set properties for modified object currentTerm }
      currentTerm.EventType := tftaEventTypeXOR;
      currentTerm.CheckTermProperties;

      currentTerm := theParent[theIndex];
      { as currentTerm is commutative (XOR) and newly created it must be sorted }
      SortOperands(currentTerm, theParent, theIndex, eventlist);

      if currentTerm.TemporalExpr = overallexpr then
      begin
        {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'LawOfCompleteness 4'); {$ENDIF}
        theParent.Owner.CheckTermProperties;
      end else
      begin
        {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'LawOfCompleteness 4.5');  {$ENDIF}
        theParent.Owner.CheckTermProperties;
      end;
    end else
    begin  { overall term does exist }
      GenericUpdateObject(currentTerm,tempTerm,eventlist,theParent,theIndex,'LawOfCompleteness 5');
    end;
  end; { check that overall term is a AND }
end;

{------------------------------------------------------------------------------
  idempotency for and, or, xor, sand
------------------------------------------------------------------------------}
function LawOfIdempotency(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList ) : boolean;
begin
  Result := False;
  { only the term itself may be modified! its children (their instances!) must not be
    touched }
  if ( currentTerm.Count > 1 ) and
     ((currentTerm.EventType = tftaEventTypeAND) or
      (currentTerm.EventType = tftaEventTypeOR) or
      (currentTerm.EventType = tftaEventTypeXOR) or
      (currentTerm.EventType = tftaEventTypeSAND)
     ) then
  begin
    if currentTerm.Children.DeleteAllCopies then
    begin
      Result := True;
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'LawOfIDemtopency 1'); {$ENDIF}
    end;
    { if all children were the same then there is only one child left now.
      SAND, AND, XOR, OR with only one child are the child!}
    if currentTerm.Count = 1 then
    begin
      GenericUpdateObject(currentTerm,currentTerm[0],eventlist,theParent,theIndex,'LawOfIdempotency 2');
      Result := True;
    end;
  end;
end;

{------------------------------------------------------------------------------
  check that events that already occured at a specific time within the term
  do not need to occur again later
------------------------------------------------------------------------------}
function LawOfNonrecurrence(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList ) : boolean;
var op1 : TTFTAObject;
    op2 : TTFTAObject;
    AList : TTFTAList;


    { iteratively scans the op1 (first operand) and adds subterms according to their
      operators, goes down to basic event level or level where an OR / XOR is encountered }
    procedure ScanAndAddToA(theObject : TTFTAObject);
    var i : integer = 0;
        numberChildren : integer;
    begin
      if (theObject.EventType = tftaEventTypePAND) or
         (theObject.EventType = tftaEventTypeAND) or
         (theObject.EventType = tftaEventTypeSAND) then
      begin
        numberChildren := theObject.Count;
        { add theObject to Alist and continue for all children of theObject, then return }
        AList.Add(theObject);
        repeat
          ScanAndAddToA(theObject[i]);
          inc(i);
        until i = numberChildren;
      end else
      begin
        if (theObject.EventType = tftaEventTypeOR) or
           (theObject.EventType = tftaEventTypeXOR) or
           (theObject.EventType = tftaEventTypeBASIC) then
        begin
          { add theObject to Alist and return }
          AList.Add(theObject);
        end else
        begin
          { just return }
        end;
      end;
    end;

    { iteratively scans the op2 (second operand); goes down to basic event level
      or level where an OR / XOR is encountered;
      also checks each new (sub)term for being also included in AList and returns False, if first
      such hit is found; returns true, if no hit is found }
    function ScanAndSearchForHits(theObject : TTFTAObject) : boolean;
    var i : integer = 0;
        numberChildren : integer;
    begin
      Result := True;
      if (theObject.EventType = tftaEventTypePAND) then
      begin
        { check if theObject ist not already listed in AList }
        if AList.IndexOf(theObject) = -1 then
          { continue for last child of theObject, then return }
          Result := Result and ScanAndSearchForHits(theObject[theObject.Count-1])
        else
          Result := false;
      end else  { not PAND }
      begin
        if (theObject.EventType = tftaEventTypeAND) or
           (theObject.EventType = tftaEventTypeSAND) then
        begin
          { check if theObject is not already listed in AList }
          if AList.IndexOf(theObject) = -1 then
          begin
            { continue for all children of theObject, then return }
            numberChildren := theObject.Count;
            repeat
              Result := Result and ScanAndSearchForHits(theObject[i]);
              inc(i);
            until (not Result) or (i = numberChildren);
          end else
          begin
            Result := false;
          end;
        end else { neither PAND nor AND nor SAND }
        begin
          if (theObject.EventType = tftaEventTypeOR) or
             (theObject.EventType = tftaEventTypeXOR) or
             (theObject.EventType = tftaEventTypeBASIC) then
          begin
            { check if theObject is listed in AList }
            Result := (AList.IndexOf(theObject) = -1); { True if not listed, False if listed }
          end else { neither one of PAND, SAND, AND, XOR, OR, BASIC }
          begin
            { just return, i.e. Result := True and another function in TFTALogic shall handle the situation }
          end;
        end;
      end;
    end;

begin
  Result := False;
  if (currentTerm.EventType = tftaEventTypePAND) and (currentTerm.Count = 2) then
  begin
    { create A- and B-lists }
    AList := TTFTAList.Create;
    AList.OwnsObjects:=False;

    op1 := currentTerm[0];
    op2 := currentTerm[1];

    ScanAndAddToA(op1);
    { if ScanAndSearchForHits(op2) yields True, then there are no recurrences
      and thus no need for a change; if it yields False, then there is a
      recurrence and thus the term needs to be updated to theFALSEElement }
    if not ScanAndSearchForHits(op2) then
    begin
      Result := True;
      GenericUpdateObject(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'LawOfNonrecurrence');
    end;

    AList.Destroy;
  end;
end;

{------------------------------------------------------------------------------
  transform not[False|True] to True|False
------------------------------------------------------------------------------}
function NOTFalseTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := False;
  if (currentTerm.EventType = tftaEventTypeNOT) then
  begin
    if ( currentTerm[0] = eventlist.TheFALSEElement ) then
    begin
      Result := True;
      GenericUpdateObject(currentTerm,eventlist.TheTRUEElement,eventlist,theParent,theIndex,'NOTFalseTrue 1');
    end else
    begin
      if ( currentTerm[0] = eventlist.TheTrueElement ) then
      begin
        Result := True;
        GenericUpdateObject(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'NOTFalseTrue 2');
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  transform not[not[x]] to x
------------------------------------------------------------------------------}
function NOTNOT(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
begin
  Result := False;
  if ( currentTerm.EventType = tftaEventTypeNOT    ) and
     ( currentTerm[0].EventType = tftaEventTypeNOT ) then
  begin
    Result := True;
    GenericUpdateObject(currentTerm,currentTerm[0][0],eventlist,theParent,theIndex,'NOTNOT 1');
  end;
end;

{------------------------------------------------------------------------------
  split OR with more than two operands
------------------------------------------------------------------------------}
function ORSplit( term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := GenericSplit(term,theParent,theIndex,eventlist,tftaEventTypeOR);
end;

{------------------------------------------------------------------------------
  transform or[ ... False ... ] to or[ ... ]      or
  transform xor[ ... False ... ] to xor[ ... ]
------------------------------------------------------------------------------}
function ORXORFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := False;
  if (currentTerm.EventType = tftaEventTypeOR) or (currentTerm.EventType = tftaEventTypeXOR) then
  begin
    while Assigned(currentTerm.ExtractChild(eventlist.TheFALSEElement)) do
    begin
      Result := True;
    end;
    if Result then
    begin
      if not currentTerm.HasChildren then { all children were FALSE }
      begin;
        GenericUpdateObject(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'ORXORFalse 1');
      end else
      begin
        if currentTerm.Count = 1 then { all but one child were FALSE -> only one left }
        begin;
          GenericUpdateObject(currentTerm,currentTerm[0],eventlist,theParent,theIndex,'ORXORFalse 2');
        end;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  transform or[ ... True ... ] to True  or
  transform xor[ ... True ... ] to True
------------------------------------------------------------------------------}
function ORXORTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var AtLeastOneIsTrue : boolean = False;
    i : Integer;
begin
  Result := False;
  if (currentTerm.EventType = tftaEventTypeOR) or (currentTerm.EventType = tftaEventTypeXOR) then
  begin
    i := 0;
    repeat
      if currentTerm[i] = eventlist.TheTRUEElement then AtLeastOneIsTrue := True ;
      inc(i);
    until ( i = (currentTerm.Count) ) or
          ( AtLeastOneIsTrue ) ;

    if ( AtLeastOneIsTrue ) then
    begin
      Result := True;
      GenericUpdateObject(currentTerm,eventlist.TheTRUEElement,eventlist,theParent,theIndex,'ORXORTrue 1');
    end;
  end;
end;


{------------------------------------------------------------------------------
  transform pand[x,False] or pand[False,x] pand[x,True] to False
------------------------------------------------------------------------------}
function PANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := False;
  if (currentTerm.EventType = tftaEventTypePAND) and (currentTerm.Count = 2) then
  begin
    if ( currentTerm[0] = eventlist.TheFALSEElement ) or
       ( currentTerm[1] = eventlist.TheFALSEElement ) or
       ( currentTerm[1] = eventlist.TheTRUEElement  ) then
    begin
      Result := True;
      GenericUpdateObject(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'PANDFalse 1');
    end;
  end;
end;


{------------------------------------------------------------------------------
  transform pand[x,x] to False
------------------------------------------------------------------------------}
function PANDMultiples(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := False;
  //currentTerm.DEBUGMemo.Append('PANDMultiples checking on ' + PointerAddrStr(currentTerm) + ' ::: ' + currentTerm.TemporalExpr);
  if (currentTerm.EventType = tftaEventTypePAND) and (currentTerm.Count = 2) then
  begin
    if ( currentTerm[0] = currentTerm[1] ) then
    begin
      //currentTerm.DEBUGMemo.Append('PANDMultiples detected doublets');
      Result := True;
      GenericUpdateObject(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'PANDMultiples 1');
    end;
  end;
end;


{------------------------------------------------------------------------------
  transform pand[x,pand[y,z]] to pand[and[x,y],z]
------------------------------------------------------------------------------}
function PANDPANDTransform(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var tempTerm,x,y,z : TTFTAObject;
    newANDTerm, tempANDTerm     : TTFTAObject;
    expr, overallexpr : ansistring;
    sortOccured : boolean = False;

begin
  Result := False;
  { only the currentTermrm itself may be modified! its children (their instances!) must not be
    touched, therefore a new object has to be created (the and[x,y])}
  if (currentTerm.EventType = tftaEventTypePAND) and (currentTerm[1].EventType = tftaEventTypePAND) and
     (currentTerm.Count = 2) and (currentTerm[1].Count = 2) then
  begin
    Result := True;
    x := currentTerm[0];
    y := currentTerm[1][0];
    z := currentTerm[1][1];
    { check wether a new object is really needed to be created or wether there
      already is an object like the one that needs to be created. In the latter
      case do not create a new one but link the existing one. Checking and linking
      isdone via the EventList. }
    { create new and object }
    newANDTerm := eventlist.NewItem;
    tempANDTerm := newANDTerm; { save pointer to this object for later comparison (see three cases below) }
    newANDTerm.EventType:= tftaEventTypeAND;
    newANDTerm.AddChild(x);
    newANDTerm.AddChild(y);
    newANDTerm.CheckTermProperties;
    sortOccured := SortOperands(newANDTerm,NIL,0,eventlist);
    { after SortOperands newANDTerm holds three possible Objects:
      1) The "old" newANDTerm (as before) as x and y are "in order" and thus no sorting would have
         been necessary (we didn't know that of course)
         --> In this case SortOperands returns false.
      2) A "new" newANDTerm (unlike before) as x and y are "not in order" and thus sorting
         was necessary --- and the "new" newANDTerm was not already listed in EventList
         --> In this case SortOperands returns True and the last newly created
         eventlist.Items still points to the newANDTerm.
      3) Like 2) but the "new" newANDTerm was already listed in EventList
         --> In this case SortOperands returns True and the last newly created
         eventlist.Items points to the now useless "old" newANDTerm which is
         different from the "new" newANDTerm.    }
    If sortOccured then
    begin
      if newANDTerm = tempANDTerm then
      begin
        { the case 2) }
      end else
      begin
        { the case 3) --> free useless element in eventlist }
        eventlist.Delete(tempANDTerm.PosInEventList);
      end;
    end;
    { either way at thispoint newANDTerm holds the pointer to the correct AND[...] }
    expr := newANDTerm.TemporalExpr;
    { expr now holds the TemporalExpression of the newly to be created object;
      now check wether there is the overall to be created object already existing (listed in eventlist)}
    overallexpr := 'PAND[' + expr + ',' + z.TemporalExpr + ']';
    tempTerm := eventlist.ListHoldsObjectAt(overallexpr);
    if not Assigned(tempTerm) then
    begin
      { overall term does not exist }
      tempTerm := newANDTerm;
      { object exists --> extract both children of term and then set new ones }
      currentTerm.ExtractChild(currentTerm[0]);
      currentTerm.ExtractChild(currentTerm[0]);
      { set tempTerm as new first child to currentTerm and op2 as second child to currentTerm }
      currentTerm.AddChild(tempTerm);
      currentTerm.AddChild(z);
      { set properties for modified object currentTerm }
      currentTerm.CheckTermProperties;

      currentTerm := theParent[theIndex];
      if currentTerm.TemporalExpr = overallexpr then
      begin
        {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'PANDPANDTransform 2');  {$ENDIF}
        theParent.Owner.CheckTermProperties;
      end else
      begin
        {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'PANDPANDTransform 2.5'); {$ENDIF}
      end;
    end else
    begin
      { overall term does indeed exist already (= tempTerm); therefore: old object NeedsToBeUpdated = true and
        PointerToUpdateObject is set to tempTerm (so that all other copies of currentTerm do not need to
        do all the checking again but can directly get pointed to the simplification);
        then the pointer of parent to old object is repointed to tempTerm }
      GenericUpdateObject(currentTerm,tempTerm,eventlist,theParent,theIndex,'PANDPANDTransform 3');
    end;
  end;
end;

{------------------------------------------------------------------------------
  split PAND with more than two operands
------------------------------------------------------------------------------}
function PANDSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := GenericSplit(term,theParent,theIndex,eventlist,tftaEventTypePAND);
end;


{------------------------------------------------------------------------------
  transform sand[ ... False ... ] to False
  do not touch sand[ < only TRUEs > ]  (those will later be transformed to TRUE
  transform sand[ ... True ... < not TRUE > ] to false
------------------------------------------------------------------------------}
function SANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var AtLeastOneIsFalse : boolean = False;
    AtLeastOneIsTrue  : boolean = False;
    AtLeastOneIsNormalEvent : boolean = False;
    i : Integer;
begin
  Result := False;
  if (currentTerm.EventType = tftaEventTypeSAND) then
  begin
    i := 0;
    repeat
      if currentTerm[i] = eventlist.TheFALSEElement then AtLeastOneIsFalse := True
      else
        if currentTerm[i] = eventlist.TheTRUEElement then AtLeastOneIsTrue := True
        else
          AtLeastOneIsNormalEvent := True ;
      inc(i);
    until ( i = (currentTerm.Count) ) or
          ( AtLeastOneIsFalse ) or
          ( AtLeastOneIsTrue and AtLeastOneIsNormalEvent );

    if ( AtLeastOneIsFalse ) or
       ( AtLeastOneIsTrue and AtLeastOneIsNormalEvent ) then
    begin
      Result := True;
      GenericUpdateObject(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'SANDFalse 1');
    end;
  end;
end;


{------------------------------------------------------------------------------
  transform sand[x,pand[y,z]] to pand[y,sand[x,z]]
------------------------------------------------------------------------------}
function SANDPANDTransform(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var expr              : ansistring;
    foundpand         : boolean;
    i                 : Integer = 0;
    newSANDTerm       : TTFTAObject;
    overallexpr       : ansistring;
    pandterm          : TTFTAObject;
    pandTermLastChild : TTFTAObject;
    restOfPand        : TTFTAObject;
    sortOccured       : boolean;
    tempTerm          : TTFTAObject;
    tempSANDTerm      : TTFTAObject;
begin
  Result := False;
  { only the term itself may be modified! its children (their instances!) must not be
    touched }
  { 1: Check whether currentTerm is of type SAND }
  if (currentTerm.EventType = tftaEventTypeSAND) then
  begin
    { 2: Seek first Child of type PAND (if none, then exit) }
    i:= 0;
    foundpand := false;
    repeat
      foundpand := (currentTerm[i].EventType = tftaEventTypePAND);
      inc(i);
    until (i = currentTerm.Count) or foundpand;

    if foundpand then  { (at least) one child of the SAND is a PAND }
    begin
      Result := True;

      { extract pandterm from currentTerm }
      pandterm := currentTerm.ExtractChild(currentTerm[i-1]);

      { create new "restofpand" object }
      if (pandTerm.HasChildren) and (pandTerm.Count > 2) then
      begin
        restOfPand := pandTerm.Clone(eventlist); { pandTerm may still exist unchanged at another position }
        pandTermLastChild := restOfPand[restOfPand.Count-1];
        restOfPand.Children.OwnsObjects:=false;
        restOfPand.DeleteChild(restOfPand.Count-1);
        { if restOfPand already exists at another place, then take this instead }
        tempTerm := eventlist.ListHoldsObjectAt(restOfPand.TemporalExpr);
        if tempTerm <> restOfPand then
        begin
          restOfPand.Free;
          restOfPand := tempTerm;
        end else
        begin
          {$IfDef TESTMODE}restOfPand.DEBUGPrint(false,eventlist,'SANDPANDTransform 1'); {$ENDIF}
        end;
      end else
      begin
        if (pandTerm.HasChildren) and (pandTerm.Count = 2) then
        begin
          restOfPand := pandTerm[0];
          pandTermLastChild := pandTerm[1];
        end;
      end;

      { the new SAND is the former SAND now without the first PAND }
      { at this point the SAND should still be sorted, as it was sorted before and
        only the first PAND was extracted (no change of operands' order)
        But now, adding the pandTermLastChild requires resorting...
        for this:
        - we need to clone of the former SAND (currentTerm) as this could be
          used without changes somewhere else
        - add pandTermLastChild to the clone
        - run SortOperands on the clone }
      newSANDTerm := currentTerm.Clone(eventlist);
      tempSANDTerm:= newSANDTerm;
      {$IfDef TESTMODE}newSANDTerm.DEBUGPrint(true,eventlist, 'SANDPANDTRansform 1.5');{$ENDIF}
      newSANDTerm.AddChild(pandTermLastChild);
      newSANDTerm.CheckTermProperties;
      sortOccured := SortOperands(newSANDTerm,NIL,0,eventlist);
      { for the three cases see explanations in PANDPANDTransform }
      If sortOccured then
      begin
        if newSANDTerm = tempSANDTerm then
        begin
          { the case 2) }
        end else
        begin
          { the case 3) --> free useless element in eventlist }
          eventlist.Delete(tempSANDTerm.PosInEventList);
        end;
      end;
      { either way at thispoint newANDTerm holds the pointer to the correct AND[...] }
      expr := newSANDTerm.TemporalExpr;
      overallexpr := 'PAND[' + restOfPand.TemporalExpr + ',' + expr +  ']';

      { check whether complete new term already exists }
      tempTerm := eventlist.ListHoldsObjectAt(overallexpr);
      if not Assigned(tempTerm) then
      begin
        { overall term does not exist }
        tempTerm := newSANDTerm;
        { the new sand object exists, add it an restofPand to currentObject after clearing }
        currentTerm.Children.OwnsObjects:=false;
        currentTerm.Children.Clear; { delete all pointers to old objects }
        { now add restofPand and tempTerm as new children }
        currentTerm.AddChild(restOfPand);
        currentTerm.AddChild(tempTerm);
        currentTerm.EventType := tftaEventTypePAND;
        currentTerm.CheckTermProperties;
        {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'SANDPANDTRansform 4'); {$ENDIF}
      end else { complete new term already exists }
      begin
        GenericUpdateObject(currentTerm,tempTerm,eventlist,theParent,theIndex,'SANDPANDTransform 5');
      end;

    end; { if foundpand }
  end; { if SAND }
end;

{------------------------------------------------------------------------------
  split SAND with more than two operands
------------------------------------------------------------------------------}
function SANDSplit( term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := GenericSplit(term,theParent,theIndex,eventlist,tftaEventTypeSAND);
end;

{ a separate function in addition to the "real" sorting function (SortOperands)
  is needed for the iterative walkdown, as the return
  type for this needs to be ansistring (carrying the TemporalExpr of
  the descendant (thus eliminating the need for a separate walkdown to get
  Child.TemporalExpr)) while the return type for SortOperands needs to be
  boolean in order to notify the calling instance whether any changes have
  taken place. }
{is private function (not listed in interface) }
function ScanChildrenSorting(currentTerm :TTFTAObject; theParent : TTFTAList;
                           theIndex : Integer; eventlist : TTFTAEventLookupList;
                           var isChange : boolean) : ansistring;
var numberChildren : integer = 0;
  i : integer;
  expressionList : TTFTAStringList = NIL;
  tempObject : TTFTAObject;
begin

  if not currentTerm.IsBasicEvent then
  begin
    numberChildren := currentTerm.Count;
    Result := '';
    if numberChildren > 1 then  { and, or, xor, pand, sand operators }
    begin
      expressionList := TTFTAStringList.Create;
      i := 0;
      repeat
        expressionList.Add(ScanChildrenSorting(currentTerm[i],currentTerm.Children,i,eventlist,isChange));
        inc(i);
      until (i = numberChildren);
      { now sort the entries in expressionList,
        if TTFTAStringList.sort returns false, then no sort was performed, i.e.
        no sorting of the currentTerm-children is necessary; if sort returns
        true, then sorting is necessary }
      { only sort if commutative operator and sorting of currentTerm.children necessary,
        otherwise just return temporal expression stored in expressionList.Text }
      if ( (currentTerm.EventType = tftaEventTypeAND) or
           (currentTerm.EventType = tftaEventTypeSAND) or
           (currentTerm.EventType = tftaEventTypeOR) or
           (currentTerm.EventType = tftaEventTypeXOR)
         ) and
         ( expressionList.Sort ) then
      begin
        { if such an event already exists in eventlist, then take this, else
          create a new one }
        isChange := true;
        Result := currentTerm.EventTypeToString + '[' + expressionList.CSText + ']';
        {$IfDef TESTMODE}
          currentTerm.DEBUGPrint(true,eventlist,'ScanChildrenSorting (Aenderung) 1');
        {$ENDIF}

        tempObject := eventlist.ListHoldsObjectAt(Result);
        if not Assigned(tempObject) then
        begin
          tempObject := currentTerm.Clone(eventlist);
          currentTerm.Children.Clear;
          { for each child ... }
          numberChildren := tempObject.Count;
          i := 0;
          repeat
            currentTerm.AddChild(tempObject[expressionList[i].FormerPosition]);
            inc(i);
          until (i = numberChildren);
          currentTerm.CheckTermProperties;
          {$IfDef TESTMODE}
            currentTerm.DEBUGPrint(true,eventlist,'ScanChildrenSorting (Aenderung) 2');
          {$ENDIF}
          { we can free the cloned event (in tempObject) }
          eventlist.Delete(eventlist.Count-1);
        end else
        begin
          { event already listed in eventlist }
          GenericUpdateObject(currentTerm,tempObject,eventlist,theParent,theIndex,'ScanChildrenSorting 3');
        end;

      end;
      Result := currentTerm.EventTypeToString + '[' + expressionList.CSText + ']';
    end else
    begin
      { NOT operator or one of the above in rare cases, where
        transformation results in only single parameter }
      Result := currentTerm.EventTypeToString + '[' +
                ScanChildrenSorting(currentTerm[0],currentTerm.Children,0,eventlist,isChange) + ']';
    end;
  end else
  begin;
    { is basic event }
    Result := currentTerm.PlainTemporalExpr; { name is stored in VExpr at creation of basic event }
  end;

end; { function ScanChildrenSorting }

{------------------------------------------------------------------------------
  main loop for simplifications; calls all other routines
------------------------------------------------------------------------------}
function SimplificationLoop(theobject :TTFTAObject; theParent : TTFTAList; theIndex : Integer; theEventList : TTFTAEventLookupList) : boolean;
var i : Integer = 0;
    changedSelf : boolean = false;
    changedChildren : boolean = false;
begin
  Result := false;
  repeat { outer loop, continue until neither change in oneself nor in children }

    repeat  { inner loop, continue until no chnage in oneself }
      changedSelf := false; { flag whether in the inner loop a change happend }

      If (not changedSelf) and GenericCombine(theobject, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and PANDSplit(theobject, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and PANDFalse(theobject, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and ANDFalse(theobject, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and SANDFalse(theobject, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and ORXORTrue(theobject, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and NOTFalseTrue(theobject, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and NOTNOT(theobject, theParent , theIndex, theEventList ) then
        changedSelf := true;
      If (not changedSelf) and ANDTrue(theobject, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and ORXORFalse(theobject, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and PANDMultiples(theobject, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and LawOfNonrecurrence(theobject, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and LawOfIdempotency(theobject, theParent , theIndex, theEventList ) then
        changedSelf := true;
      If (not changedSelf) and LawOfCompleteness(theobject, theParent , theIndex, theEventList ) then
        changedSelf := true;
      If (not changedSelf) and PANDPANDTransform(theobject, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and SANDPANDTransform(theobject, theParent , theIndex, theEventList) then
        changedSelf := true;

      If changedSelf then
      begin
        theobject := theParent[theIndex];
        Result := true;  { Result is flag that ANY change has happened}
      end;
    until not changedSelf;

    changedChildren := False; { flag for change in children }
    i := 0;
    if theobject.HasChildren then
    begin
      repeat
        if SimplificationLoop( theobject[i] , theobject.Children, i, theEventList) then
        begin
          changedChildren := true; { flag, that at least one child was changed }
        end;
        inc(i);
      until (i>=theobject.Count);
      Result := Result or changedChildren; { true, if self changed or child changed }
    end;
  until not changedChildren;
end;  { function SimplificationLoop }

{------------------------------------------------------------------------------
  sort the operands of commutative terms (AND, OR, XOR, SAND)
------------------------------------------------------------------------------}
function SortOperands(tT :TTFTAObject; tP : TTFTAList; tI : Integer; tEL : TTFTAEventLookupList) : boolean;
var somethingChanged : boolean = false;  { set within ScanChildrenSorting if any changes occur }
begin
  ScanChildrenSorting(tT,tP,tI,tEL,somethingChanged);
  Result := somethingChanged;
end;

{------------------------------------------------------------------------------
  split XOR with more than two operands
------------------------------------------------------------------------------}
function XORSplit( term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := GenericSplit(term,theParent,theIndex,eventlist,tftaEventTypeXOR);
end;

end.

