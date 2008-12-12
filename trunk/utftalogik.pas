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

interface

uses
  Classes, SysUtils, Dialogs, ComCtrls, StdCtrls, strutils, utftaobject,
    sjspointertools;
  
  function  ANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
  function  ANDSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
  function  ANDTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
  function  GenericSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList; theType : TTFTAOperatorType) : boolean;
  procedure GenericUpdateObject(oldTerm : TTFTAObject; newTerm : TTFTAObject; eventlist : TTFTAEventLookupList; parentList : TTFTAList; oldObjectListIndex : Integer; callString : ansistring = '');
  function  LawOfCompleteness(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  LawOfIdempotency(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  LawOfNonrecurrence(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList ) : boolean;
  function  NOTFalseTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  NOTNOT(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  ORSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  ORXORFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  ORXORTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  PANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  PANDMultiples(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  PANDPANDTransform(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  PANDSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  SANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  SANDPANDTransform(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  SANDSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
  function  XORSplit(term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;

implementation

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
    currentTerm.DEBUGPrint(true,eventlist,'ANDFalse 2');
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
  Result := False;
  { only the term itself may be modified! its children (their instances!) must not be
    touched, therefore a new object is to be created }
  //term.DEBUGMemo.Append('GenericSplit with ' + cEventTypeStringArray[ord(theType)]);
  //term.DEBUGMemo.Append('   checking on ' + PointerAddrStr(term) + ' ::: ' + term.TemporalExpr);
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

  oldTerm.PointerToUpdateObject:=newTerm;
  oldTerm.NeedsToBeUpdated:=true;

  oldterm := parentList[oldObjectListIndex];

  { next calling of parentList[oldObjectListIndex] should be redirected to new term }
  if oldterm = newTerm then
  begin
    parentList.Owner.CheckTermProperties;
    if Assigned(oldterm.DEBUGMemo) then
      oldterm.DEBUGPrint(true,eventList,callString);
  end else
    ShowMessage('Fatal Error (081210.1221) while processing event ' +
                PointerAddrStr(oldTerm) + ' ( ' + oldTerm.TemporalExpr + ' ) ' );

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
      ANDSplit(currentTerm, theParent, theIndex, eventList) ;
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

      aTerm.DEBUGPrint(false,eventlist,'LawOfCompleteness 1');

      bTerm.AddChild(yTerm);
      bTerm.AddChild(xTerm);
      bTerm.CheckTermProperties;

      bTerm.DEBUGPrint(false,eventlist,'LawOfCompleteness 2');

      cTerm.AddChild(xTerm);
      cTerm.AddChild(yTerm);
      cTerm.CheckTermProperties;

      cTerm.DEBUGPrint(false,eventlist,'LawOfCompleteness 3');

      { add the new children to the overall term }
      currentTerm.AddChild(aTerm);
      currentTerm.AddChild(bTerm);
      currentTerm.AddChild(cTerm);
      { set properties for modified object currentTerm }
      currentTerm.EventType := tftaEventTypeXOR;
      currentTerm.CheckTermProperties;

      currentTerm := theParent[theIndex];
      if currentTerm.TemporalExpr = overallexpr then
      begin
        currentTerm.DEBUGPrint(true,eventlist,'LawOfCompleteness 4');
        theParent.Owner.CheckTermProperties;
      end else
        currentTerm.DEBUGPrint(true,eventlist,'LawOfCompleteness 4.5');
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
      currentTerm.DEBUGPrint(true,eventlist,'LawOfIDemtopency 1');
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
  transform or[ ... True ... ] to False  or
  transform xor[ ... True ... ] to False
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
    expr, overallexpr : ansistring;
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
    { get event.Temporalexpression, modify it and check wether a similar object
      already exists }
    expr := 'AND[' + x.TemporalExpr + ',' + y.TemporalExpr + ']';
    { expr now holds the TemporalExpression of the newly to be created object;
      now check wether
      a) there is the overall to be created object already existing (listed in eventlist)
      b) only an object as described in expr already exists }
    overallexpr := 'PAND[' + expr + ',' + z.TemporalExpr + ']';
    tempTerm := eventlist.ListHoldsObjectAt(overallexpr);
    if not Assigned(tempTerm) then
    begin
      { overall term does not exist }
      tempTerm := eventlist.ListHoldsObjectAt(expr);
      if not Assigned(tempTerm) then
      begin
        { create new and object }
        tempTerm := eventlist.NewItem;
        tempTerm.EventType:= tftaEventTypeAND;
        { extract the first child [x] from currentTerm and get (not extract! do not change
          the second child of currentTerm!) the first child of the second
          child [y] from currentTerm and put them to tempTerm }
        tempTerm.AddChild(currentTerm.ExtractChild(currentTerm[0]));
        tempTerm.AddChild(y);
        { set properties for object tempTerm }
        tempTerm.CheckTermProperties;

        tempTerm.DEBUGPrint(false,eventlist,'PANDPANDTransform 1');

        { extract second child itself, i.e. change currentTerm }
        currentTerm.ExtractChild(currentTerm[0]);

      end else
      begin
        { object exists --> extract both children of term and then set new ones }
        currentTerm.ExtractChild(currentTerm[0]);
        currentTerm.ExtractChild(currentTerm[0]);
      end;
      { set tempTerm as new first child to currentTerm and op2 as second child to currentTerm }
      currentTerm.AddChild(tempTerm);
      currentTerm.AddChild(z);
      { set properties for modified object currentTerm }
      currentTerm.CheckTermProperties;

      currentTerm := theParent[theIndex];
      if currentTerm.TemporalExpr = overallexpr then
      begin
        currentTerm.DEBUGPrint(true,eventlist,'PANDPANDTransform 2');
        theParent.Owner.CheckTermProperties;
      end else
        currentTerm.DEBUGPrint(true,eventlist,'PANDPANDTransform 2.5');

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
var tempTerm : TTFTAObject;
    pandterm : TTFTAObject;
    pandTermLastChild : TTFTAObject;
    restOfPand : TTFTAObject;
    expr, overallexpr : ansistring;
    i : Integer = 0;
    foundpand : boolean = false;
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
        restOfPand := pandTerm.Clone(eventlist);
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
          restOfPand.DEBUGPrint(false,eventlist,'SANDPANDTransform 1');
        end;
      end else
      begin
        if (pandTerm.HasChildren) and (pandTerm.Count = 2) then
        begin
          restOfPand := pandTerm[0];
          pandTermLastChild := pandTerm[1];
        end;
      end;

      expr := currentTerm.TemporalExpr;
      expr := AnsiLeftStr(expr,Length(expr)-1);
      expr := expr + ',' + pandTermLastChild.TemporalExpr + ']' ;
      overallexpr := 'PAND[' + restOfPand.TemporalExpr + ',' + expr +  ']';

      { check whether complete new term already exists }
      tempTerm := eventlist.ListHoldsObjectAt(overallexpr);
      if not Assigned(tempTerm) then
      begin
        { overall term does not exist }
        tempTerm := eventlist.ListHoldsObjectAt(expr);
        if not Assigned(tempTerm) then
        begin
          { create new sand object which includes the last part of the former pand term }
          tempTerm := currentTerm.Clone(eventlist);
          tempTerm.DEBUGPrint(true,eventlist, 'SANDPANDTRansform 1.5');
          tempTerm.AddChild(pandTermLastChild);

          tempTerm.DEBUGPrint(false,eventlist,'SANDPANDTransform 2');

          currentTerm.Children.OwnsObjects:=false;
          currentTerm.Children.Clear; { delete all pointers to old objects }
          { now add restofPand and tempTerm as new children }
          tempTerm.DEBUGPrint(false,eventlist,'SANDPANDTransform 2.5');
          currentTerm.AddChild(restOfPand);
          currentTerm.AddChild(tempTerm);
          currentTerm.EventType := tftaEventTypePAND;
          currentTerm.CheckTermProperties;

          currentTerm.DEBUGPrint(true,eventlist, 'SANDPANDTRansform 3');

        end else { currentTerm plus last child of pandTerm exists }
        begin
          { the new sand object exists, add it an restofPand to currentObject after clearing }
          currentTerm.Children.OwnsObjects:=false;
          currentTerm.Children.Clear; { delete all pointers to old objects }
          { now add restofPand and tempTerm as new children }
          currentTerm.AddChild(restOfPand);
          currentTerm.AddChild(tempTerm);
          currentTerm.EventType := tftaEventTypePAND;
          currentTerm.CheckTermProperties;

          currentTerm.DEBUGPrint(true,eventlist,'SANDPANDTRansform 4');

        end; { currentTerm plus last child of pandTerm did exist }
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


{------------------------------------------------------------------------------
  split XOR with more than two operands
------------------------------------------------------------------------------}
function XORSplit( term :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := GenericSplit(term,theParent,theIndex,eventlist,tftaEventTypeXOR);
end;

end.

