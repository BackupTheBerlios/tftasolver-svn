unit utftalogic;

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
  function  SimplificationLoop  (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; theEventList : TTFTAEventLookupList) : boolean;
  function  SortOperands        (tT :TTFTAObject; tP : TTFTAList; tI : Integer; tEL : TTFTAEventLookupList) : boolean;

implementation

  { "private" functions / Procedures }
  function  ANDFalse            (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  ANDSplit            (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  ANDTrue             (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  DistributeANDORXOR  (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  DistributePANDXOR   (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  GenericCombine      (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  GenericNOTBoolean   (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList; flagANDtoOR : boolean) : boolean; forward;
  function  GenericSplit        (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList; theType : TTFTAOperatorType) : boolean; forward;
  function  LawOfCompleteness   (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  LawOfIdempotency    (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  LawOfNonrecurrence  (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  NOTANDORXOR         (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  NOTFalseTrue        (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  NOTNOT              (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  ORSplit             (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  ORXORFalse          (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  ORXORTrue           (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  PANDFalse           (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  PANDMultiples       (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  PANDPANDTransform   (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  PANDSplit           (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  ScanChildrenSorting (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList; var isChange : boolean) : ansistring; forward;
  function  SANDFalse           (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  SANDPANDTransform   (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  SANDSplit           (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;
  function  XORSplit            (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean; forward;

  { also private but not primary simplification routines but internal "helper" routines }
  procedure RedirectTerm        (oldTerm : TTFTAObject; newTerm : TTFTAObject; eventlist : TTFTAEventLookupList; parentList : TTFTAList; oldObjectListIndex : Integer; callString : ansistring = ''); forward;



{These routines are designed according to the remarks in
 "TFTASolver - Description of the TFTASolver Algorithms" (Version 146 of 20081227)
 Chapter 2.2. Comments correspond to this chapter.}





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform and[ ... False ... ] to False
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function ANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var AtLeastOneIsFalse : boolean = False;
    i : Integer;
    numberOfChildren : Integer;
begin
  Result := False; { default }
{.....check currentTerm .......................................................}
  if (currentTerm.IsTypeAND) then
  begin
{.....configuration currentTerm ...............................................}
    { for each child do }
    i := 0;
    numberOfChildren := currentTerm.Count;
    repeat
      if currentTerm[i] = eventList.TheFALSEElement then
      begin
        {$IfDef TESTMODE}if not AtLeastOneIsFalse then currentTerm.DEBUGPrint(true,eventlist,'Entered AND False');{$ENDIF}
        AtLeastOneIsFalse := True;
      end;
      inc(i);
    until AtLeastOneIsFalse or (i = numberOfChildren) ;

    if ( AtLeastOneIsFalse ) then
    begin
{.....redirection .............................................................}
      RedirectTerm(currentTerm,eventlist.theFALSEElement,eventlist,theParent,theIndex,'ANDFalse 1');
      Result := True;
    end;
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  split AND with more than two operands
  This function is only a wrapper arround GenericSplit.
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function ANDSplit( currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := GenericSplit(currentTerm,theParent,theIndex,eventlist,tftaEventTypeAND);
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform and[ ... True ... ] to and[ ... ]
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function ANDTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var existingTerm : TTFTAObject;
    AtLeastOneIsTrue : boolean = false;
begin
  Result := False; { default }
{.....check currentTerm .......................................................}
  if (currentTerm.IsTypeAND) then
  begin
{.....configuration currentTerm ...............................................}
    while Assigned(currentTerm.ExtractChild(eventlist.TheTRUEElement)) do
    begin
      {$IfDef TESTMODE}if not AtLeastOneIsTrue then currentTerm.DEBUGPrint(true,eventlist,'Entered ANDTrue');{$ENDIF}
      AtLeastOneIsTrue := True;
    end;
    if AtLeastOneIsTrue then
    begin
      Result := True;
      if currentTerm.Count > 1 then
      begin
{.....redirection .............................................................}
        existingTerm := eventlist.FindIdenticalExisting(currentTerm);
        if Assigned(existingTerm) then
        begin
          RedirectTerm(currentTerm,existingTerm,eventlist,theParent,theIndex,'ANDTrue 1');
        end;
      end else
      begin
        if currentTerm.Count = 1 then
        begin
          { no extra check is needed whether new term already exists, it exists (as child #0) }
          RedirectTerm(currentTerm,currentTerm[0],eventlist,theParent,theIndex,'ANDTrue 2');
        end else
        begin { currentTerm has no children --> currentTerm was AND[TRUE,TRUE,TRUE,...] before }
          RedirectTerm(currentTerm,eventlist.TheTRUEElement,eventlist,theParent,theIndex,'ANDTrue 3');
        end;
      end;
    end;  { Result }
  end;  { isTypeAND }
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  Boolean Disributive Law between AND and OR / XOR
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function DistributeANDORXOR(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var i                : integer;
    numberOfChildren : integer;
    foundORXOR       : boolean;
    cloneTerm        : TTFTAObject;
    tempTerm1        : TTFTAObject;
    newTerm2         : TTFTAObject;
    existingTerm     : TTFTAObject;
begin
  Result := False;
{.....check currentTerm .......................................................}
  if (currentTerm.IsTypeAND) and (currentTerm.Count > 1) then
  begin
    { scan all children for first occurrence of an OR or XOR }
    i := 0;
    numberOfChildren := currentTerm.Count;
    repeat
      foundORXOR := (currentTerm[i].IsTypeOR or currentTerm[i].IsTypeXOR);
      inc(i);
    until foundORXOR or (i = numberOfChildren);
    if foundORXOR then
    begin
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered DistributeANDORXOR');{$ENDIF}
{.....clone currentTerm........................................................}
      cloneTerm := currentTerm.Clone(eventlist);
      {$IfDef TESTMODE}cloneTerm.DEBUGPrint(false,eventlist,'DistributeANDORXOR Created Clone');{$ENDIF}
      currentTerm.Children.Clear;
      currentTerm.Mask;
{.....create newTerm ..........................................................}
      Result := True;
      dec(i);
      { Extract the found term (first occurrence of an OR or XOR) }
      tempTerm1 := cloneTerm[i];
      cloneTerm.DeleteChild(i);
      { for each child of tempTerm1 do }
      i := 0;
      numberOfChildren := tempTerm1.Count;
      repeat
        newTerm2 := eventlist.NewItem;
        {$IfDef TESTMODE}newTerm2.DEBUGPrint(false,eventlist,'DistributeANDORXOR Created New' + IntToStr(i));{$ENDIF}
{.....configure newTerm .......................................................}
        newTerm2.EventType := tftaEventTypeAND;
        newTerm2.AddChild(tempTerm1[i]);
        newTerm2.AddChild(cloneTerm);
        newTerm2.CheckTermProperties;
{.....replace newTerm .........................................................}
        eventlist.ReplaceWithIdentical(newTerm2);
{.....configure currentTerm ...................................................}
        currentTerm.AddChild(newTerm2);
        inc(i);
      until (i = numberOfChildren);
      currentTerm.Unmask;
      currentTerm.EventType:= tempTerm1.EventType;
      currentTerm.CheckTermProperties;
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'DistributeANDORXOR currentTerm intermediate');{$ENDIF}
      { for each of the new children of currentTerm do }
      i := 0;
      numberOfChildren := currentTerm.Count;
      repeat
        GenericCombine(currentTerm[i], currentTerm.Children , i, eventlist);
        inc(i);
      until (i = numberOfChildren);
{.....redirection .............................................................}
      existingTerm := eventlist.FindIdenticalExisting(currentTerm);
      if Assigned(existingTerm) then
      begin
        RedirectTerm(currentTerm,existingTerm,eventlist,theParent,theIndex,'DistributeANDORXOR 1');
      end;
{.....free the clone ..........................................................}
      eventlist.FreeTerm(cloneTerm);
    end;
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  Temporal Disributive Law between OR and PAND
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function DistributeORPAND(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var i : integer;
    numberOfChildren : integer;
    tempTerm1        : TTFTAObject;
    tempTerm2        : TTFTAObject;
    newTerm3         : TTFTAObject;
    existingTerm     : TTFTAObject;
begin
  Result := False;
{ ....check currentTerm .......................................................}
  if (currentTerm.IsTypePAND) and (currentTerm.Count = 2) and (currentTerm[0].IsTypeOR) then
  begin
{ ....clone is not necessary here .............................................}
{ ....create newTerm ..........................................................}
    Result := True;
    {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered DistributeORPAND');{$ENDIF}
    tempTerm1 := currentTerm[0];
    tempTerm2 := currentTerm[1];
    currentTerm.Children.Clear;
    currentTerm.Mask;
    { for each child of tempTerm1 do }
    i := 0;
    numberOfChildren := tempTerm1.Count;
    repeat
      newTerm3 := eventlist.NewItem;
{ ....configure newTerm .......................................................}
      newTerm3.AddChild(tempTerm1[i]);
      newTerm3.AddChild(tempTerm2);
      newTerm3.EventType := tftaEventTypePAND;
      newTerm3.CheckTermProperties;
      {$IfDef TESTMODE}
        newTerm3.DEBUGPrint(false,eventlist,'DistributeORPAND Created New' + IntToStr(i));
      {$ENDIF}
{ ....replace newTerm .........................................................}
      eventlist.ReplaceWithIdentical(newTerm3);
{ ....configure currentTerm ...................................................}
      currentTerm.AddChild(newTerm3);
      inc(i);
    until (i = numberOfChildren);
    currentTerm.Unmask;
    currentTerm.EventType:=tftaEventTypeOR;
    currentTerm.CheckTermProperties;
{ ....redirection .............................................................}
    existingTerm := eventlist.FindIdenticalExisting(currentTerm);
    if Assigned(existingTerm) then
    begin
      RedirectTerm(currentTerm,existingTerm,eventlist,theParent,theIndex,'DistributeORPAND 1');
    end;
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  Temporal Disributive Law between PAND and XOR
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function DistributePANDXOR(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var i : integer;
    numberOfChildren : integer;
    tempTerm1        : TTFTAObject;
    tempTerm2        : TTFTAObject;
    newTerm3         : TTFTAObject;
    existingTerm     : TTFTAObject;
begin
  Result := False;
{ ....check currentTerm .......................................................}
  if (currentTerm.IsTypePAND) and (currentTerm.Count = 2) then
  begin
    if currentTerm[0].IsTypeXOR then
    begin
{ ....clone is not necessary here .............................................}
{ ....create newTerm ..........................................................}
      Result := True;
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered DistributePANDXOR');{$ENDIF}
      tempTerm1 := currentTerm[0];
      tempTerm2 := currentTerm[1];
      currentTerm.Children.Clear;
      currentTerm.Mask;
      { for each child of tempTerm1 do }
      i := 0;
      numberOfChildren := tempTerm1.Count;
      repeat
        newTerm3 := eventlist.NewItem;
{ ....configure newTerm .......................................................}
        newTerm3.AddChild(tempTerm1[i]);
        newTerm3.AddChild(tempTerm2);
        newTerm3.EventType := tftaEventTypePAND;
        newTerm3.CheckTermProperties;
        {$IfDef TESTMODE}
          newTerm3.DEBUGPrint(false,eventlist,'DistributePANDXOR 1 Created New' + IntToStr(i));
        {$ENDIF}
{ ....replace newTerm .........................................................}
        eventlist.ReplaceWithIdentical(newTerm3);
{ ....configure currentTerm ...................................................}
        currentTerm.AddChild(newTerm3);
        inc(i);
      until (i = numberOfChildren);
      currentTerm.Unmask;
      currentTerm.EventType:=tftaEventTypeXOR;
      currentTerm.CheckTermProperties;
{ ....redirection .............................................................}
      existingTerm := eventlist.FindIdenticalExisting(currentTerm);
      if Assigned(existingTerm) then
      begin
        RedirectTerm(currentTerm,existingTerm,eventlist,theParent,theIndex,'DistributePANDXOR 1');
      end;
    end else
    { first child is no XOR, but second child may be... }
    if currentTerm[1].IsTypeXOR then
    begin
{ ....clone is not necessary here .............................................}
{ ....create newTerm ..........................................................}
      Result := True;
      tempTerm1 := currentTerm[1];
      tempTerm2 := currentTerm[0];
      currentTerm.Children.Clear;
      currentTerm.Mask;
      { for each child of tempTerm1 do }
      i := 0;
      numberOfChildren := tempTerm1.Count;
      repeat
        newTerm3 := eventlist.NewItem;
{ ....configure newTerm .......................................................}
        newTerm3.AddChild(tempTerm2);
        newTerm3.AddChild(tempTerm1[i]);
        newTerm3.EventType := tftaEventTypePAND;
        newTerm3.CheckTermProperties;
        {$IfDef TESTMODE}
          newTerm3.DEBUGPrint(false,eventlist,'DistributePANDXOR 2 Created New' + IntToStr(i));
        {$ENDIF}
{ ....replace newTerm .........................................................}
        eventlist.ReplaceWithIdentical(newTerm3);
{ ....configure currentTerm ...................................................}
        currentTerm.AddChild(newTerm3);
        inc(i);
      until (i = numberOfChildren);
      currentTerm.Unmask;
      currentTerm.EventType:=tftaEventTypeXOR;
      currentTerm.CheckTermProperties;
{ ....redirection .............................................................}
      existingTerm := eventlist.FindIdenticalExisting(currentTerm);
      if Assigned(existingTerm) then
      begin
        RedirectTerm(currentTerm,existingTerm,eventlist,theParent,theIndex,'DistributePANDXOR 2');
      end;
    end;
  end;
end;






{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  Generic combine of commutative terms (incl. sorting)
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function GenericCombine(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var myType           : TTFTAOperatorType;
    i                : integer;
    numberOfChildren : integer;
    nowPosInTerm     : integer;
    nowSizeArray     : integer;
    SameTypeTerms    : array of integer;
    tempObject : TTFTAObject;
begin
  Result := False;
  myType := currentTerm.EventType;
{ ....check currentTerm .......................................................}
  if currentTerm.IsCommutative then
  begin
    { scan all children and get all objects of same type as currentTerm;
      this is done in a single loop }
    nowPosInTerm := 0;
    nowSizeArray := 0;
    numberOfChildren := currentTerm.Count;
    repeat
      if (currentTerm[nowPosInTerm].EventType = myType) then
      begin { if same type }
        inc(nowSizeArray);
        SetLength(SameTypeTerms,nowSizeArray); { expand array }
        SameTypeTerms[nowSizeArray-1] := nowPosInTerm; { add current pos to array }
      end;
      inc(nowPosInTerm);
    until (nowPosInTerm = numberOfChildren);
    if nowSizeArray > 0 then
    begin { at least one same type event was found }
{ ....configure currentTerm....................................................}
      Result := true;
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered GenericCombine');{$ENDIF}
      { for each of the same type events do }
      for i := 0 to nowSizeArray-1 do
      begin
        { get all children of the event and add them to current Event;
          do not yet extract the same type events as this would interfere with the
          indexes stored in SameTypeTerms; adding at the end does not interfere here. }
        nowPosInTerm := 0;
        numberOfChildren := currentTerm[SameTypeTerms[i]].Count;
        if numberOfChildren > 0 then
          repeat
            currentTerm.AddChild(currentTerm[SameTypeTerms[i]][nowPosInTerm]);
            inc(nowPosInTerm);
          until (nowPosInTerm = numberOfChildren);
      end;
      { now extract all SameTypeTerms from currentTerm; in order to minimize
        interference we do this from back to front }
      for i := nowSizeArray-1 downto 0 do
      begin
        currentTerm.DeleteChild(SameTypeTerms[i]);
      end;
      { now sort currentTerm (thereby checking whether an identical object
        already is listed in Eventlist }
{ ....configuration continued and redirection .................................}
      SortOperands(currentTerm,theParent,theIndex,eventlist);
      currentTerm.CheckTermProperties;
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'GenericCombine 2');{$ENDIF}
    end; { if nowSizeArray > 0 }
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  Generic split of terms with more than two operands
  Called by PANDSplit, SANDSplit, ANDSPlit, ORSplit, XORSplit
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function GenericSplit(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList; theType : TTFTAOperatorType) : boolean;
var tempTerm1        : TTFTAObject;
    tempTerm2        : TTFTAObject;
    newTerm3         : TTFTAObject;
    i                : Integer;
    numberOfChildren : Integer;
begin
  { note: no sorting necessary; if commutative, term should already be sorted and
    as splitting is done from left to right, the order of operands is never altered. }
  Result := False;
  numberOfChildren := currentTerm.Count;
{ ....check currentTerm .......................................................}
  if (currentTerm.EventType = theType) and (numberOfChildren > 2) then
  begin
    Result := True;
    {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered GenericSplit ' + cEventTypeStringArray[ord(theType)]);{$ENDIF}
    tempTerm1 := currentTerm[0]; { get first child }
    i := 1; { start with second child }
    currentTerm.Mask;
    repeat
      tempTerm2 := currentTerm[i]; { get second child }
      newTerm3 := eventlist.NewItem;
{ ....configure newTerm .......................................................}
      newTerm3.AddChild(tempTerm1);
      newTerm3.AddChild(tempTerm2);
      newTerm3.EventType := theType;
      newTerm3.CheckTermProperties;
      {$IfDef TESTMODE}newTerm3.DEBUGPrint(false,eventlist,'GenericSplit 1 called from ' + cEventTypeStringArray[ord(theType)] + ' ' + IntToStr(i));{$ENDIF}
{ ....replace newTerm .........................................................}
      eventlist.ReplaceWithIdentical(newTerm3);
      tempTerm1 := newTerm3;  { prepare for next loop }
      inc(i);
    until i = numberOfChildren;
{ ....configure currentTerm ...................................................}
    currentTerm.Unmask;
{ ....redirection .............................................................}
    RedirectTerm(currentTerm,tempTerm1,eventlist,theParent,theIndex,'GenericSplit 2 called from ' + cEventTypeStringArray[ord(theType)]);
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  Generic routine for redirection of terms
  (and replacement of terms, if oldTerm is freed afterwards)
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
procedure RedirectTerm(oldTerm : TTFTAObject; newTerm : TTFTAObject; eventList : TTFTAEventLookupList;
                              parentList : TTFTAList; oldObjectListIndex : Integer; callString : ansistring = '');
begin
  { note: no sorting necessary; sorting must be done prior to calling this routine }
  if oldTerm = newTerm then exit;
  {$IfDef TESTMODE}oldTerm.DEBUGPrint(true,eventlist,'Entered RedirectMe');{$ENDIF}


  oldTerm.RedirectMe(newTerm);

  if assigned(parentList) then
  begin
    oldterm := parentList[oldObjectListIndex];

    parentList.Owner.CheckTermProperties;
  end else
  begin  { case without parentList (mainly for SortOperands-routine) }
  end;

end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  law of completeness and[x,y] = pand[x,y] xor pand[y,x] xor sand[x,y]
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function LawOfCompleteness(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var aTerm : TTFTAObject;
    bTerm : TTFTAObject;
    cTerm : TTFTAObject;
    xTerm : TTFTAObject;
    yTerm : TTFTAObject;
    existingTerm : TTFTAObject;
    flagSpeedSearchWasAlreadOn : boolean;
begin
  Result := False;
{ ....check currentTerm .......................................................}
  if (currentTerm.IsTypeAND) and (not currentTerm.IsAllChildrenAreBasic) then
  begin
    Result := True;
    {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered LawOfCompleteness');{$ENDIF}
{ ....configure currentTerm ...................................................}
    { if currentTerm.Count > 2 then split currentTerm first }
    if currentTerm.Count > 2 then
    begin
      ANDSplit(currentTerm, theParent, theIndex, eventList);
      currentTerm := theParent[theIndex];
    end;
    { iff x and y are not atomic (i.e. basic events) the original object (the AND term) will become an
      XOR(                     [the original object]
          PAND(x,y),           [the first new object called "a"]
          PAND(y,x),           [the second new object called "b"]
          SAND(X,Y)            [the third new object called "c"]
         )
    }
    { thus, a total of three objects has to be created and checked for already existing }
    xTerm := currentTerm[0];
    yTerm := currentTerm[1];
{.... clone currentTerm is not necessary ......................................}
{.... create new terms ........................................................}
    aTerm := eventlist.NewItem;
    bTerm := eventlist.NewItem;
    cTerm := eventlist.NewItem;
{.... configure new terms .....................................................}
    aTerm.EventType := tftaEventTypePAND;
    aTerm.AddChild(xTerm);
    aTerm.AddChild(yTerm);
    aTerm.CheckTermProperties;
    {$IfDef TESTMODE}aTerm.DEBUGPrint(false,eventlist,' (LawOfCompleteness) created new ');{$ENDIF}
    bTerm.EventType := tftaEventTypePAND;
    bTerm.AddChild(yTerm);
    bTerm.AddChild(xTerm);
    bTerm.CheckTermProperties;
    {$IfDef TESTMODE}bTerm.DEBUGPrint(false,eventlist,' (LawOfCompleteness) created new ');{$ENDIF}
    cTerm.EventType := tftaEventTypeSAND;
    cTerm.AddChild(xTerm);
    cTerm.AddChild(yTerm);
    cTerm.CheckTermProperties;
    {$IfDef TESTMODE}cTerm.DEBUGPrint(false,eventlist,' (LawOfCompleteness) created new ');{$ENDIF}
{.... replace newTerms ........................................................}
    { no sorting necessary as original AND was sorted, thus xTerm < yTerm and
      thus aTerm < bTerm < cTerm }
    currentTerm.Mask;
    flagSpeedSearchWasAlreadOn := eventlist.SpeedSearchFlagOn; { remember prior state }
    eventlist.SpeedSearchFlagOn:=true;
    eventlist.ReplaceWithIdentical(aTerm);
    eventlist.ReplaceWithIdentical(bTerm);
    eventlist.ReplaceWithIdentical(cTerm);
    eventlist.SpeedSearchFlagOn := flagSpeedSearchWasAlreadOn; { restor prior state }
{.... configure currentTerm ...................................................}
    currentTerm.Children.Clear;
    currentTerm.EventType:= tftaEventTypeXOR;
    currentTerm.AddChild(aTerm);
    currentTerm.AddChild(bTerm);
    currentTerm.AddChild(cTerm);
    currentTerm.Unmask;
    currentTerm.CheckTermProperties;
    {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,' (LawOfCompleteness) configured ');{$ENDIF}
{.... redirect currentTerm ....................................................}
    existingTerm := eventlist.FindIdenticalExisting(currentTerm);
    if Assigned(existingTerm) then
    begin
      RedirectTerm(currentTerm,existingTerm,eventlist,theParent,theIndex,'LawOfCompleteness 1');
    end;
  end; { check that overall term is a AND }
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  idempotency for and, or, xor, sand
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function LawOfIdempotency(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList ) : boolean;
var existingTerm : TTFTAObject = NIL;
begin
{.....check currentTerm .......................................................}
  if currentTerm.IsCommutative then
  begin
{.....configuration currentTerm ...............................................}
    if currentTerm.Children.DeleteAllCopies then
    begin
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered LawOfIdempotency');{$ENDIF}
      Result := True;
      if currentTerm.Count > 1 then
      begin
{.....redirection .............................................................}
        existingTerm := eventlist.FindIdenticalExisting(currentTerm);
        if Assigned(existingTerm) then
        begin
          RedirectTerm(currentTerm,existingTerm,eventlist,theParent,theIndex,'LawOfIdempotency 1');
        end;
      end else
      begin
        { if all children were the same then there is only one child left now.
          SAND, AND, XOR, OR with only one child are the child!}
        if currentTerm.Count = 1 then
        begin
          RedirectTerm(currentTerm,currentTerm[0],eventlist,theParent,theIndex,'LawOfIdempotency 2');
        end;
      end;
    end; { DeleteAllCopies }
  end;   { IsCommutative }
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  check that events that already occured at a specific time within the term
  do not need to occur again later
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function LawOfNonrecurrence(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList ) : boolean;
var tempTerm1 : TTFTAObject;
    tempTerm2 : TTFTAObject;
    tempList  : TTFTAList;


    { iteratively scans the tempTerm1 (first operand) and adds subterms according to their
      operators, goes down to basic event level or level where an OR / XOR is encountered }
    procedure ScanAndAddToList(theTerm : TTFTAObject);
    var i : integer;
        numberOfChildren : integer;
    begin
      if (theTerm.IsTypePAND) or
         (theTerm.IsTypeAND) or
         (theTerm.IsTypeSAND) then
      begin { add theTerm to tempList and continue for all children of theTerm, then return }
        tempList.Add(theTerm);
        i := 0;
        numberOfChildren := theTerm.Count;
        repeat
          ScanAndAddToList(theTerm[i]);
          inc(i);
        until i = numberOfChildren;
      end else
      begin
        if (theTerm.IsTypeOR) or
           (theTerm.IsTypeXOR) or
           (theTerm.IsTypeBASIC) then
        begin { add theTerm to tempList (without scanning the children) and return }
          tempList.Add(theTerm);
        end else
        begin
          { other type --> do nothing }
        end;
      end;
    end;

    { iteratively scans the tempTerm2 (second operand); goes down to basic event level
      or level where an OR / XOR is encountered;
      also checks each new (sub)term for being also included in tempList and returns
      False, if first such hit is found; returns true, if no hit is found }
    function ScanAndSearchForHits(theTerm : TTFTAObject) : boolean;
    var i : integer = 0;
        numberOfChildren : integer;
    begin
      Result := True;
      if (theTerm.IsTypePAND) then
      begin
        { PAND: False if either whole PAND term is listed in templist (aka. "happened before")
                or the last operand of the PAND term is listed in templist }
        if tempList.IndexOf(theTerm) = -1 then { whole term not listed }
          Result := Result and ScanAndSearchForHits(theTerm[theTerm.Count-1])
        else
          Result := false;
      end else  { not PAND }
      begin
        if (theTerm.IsTypeAND) or (theTerm.IsTypeSAND) then
        begin
          { (S)AND: False if either whole term or at least one of the children
                is listed in templist (aka. "happened before") }
          if tempList.IndexOf(theTerm) = -1 then
          begin { whole term not listed  --> check all children}
            numberOfChildren := theTerm.Count;
            repeat
              Result := Result and ScanAndSearchForHits(theTerm[i]);
              inc(i);
            until (not Result) or (i = numberOfChildren);
          end else
          begin
            Result := false;
          end;
        end else { neither PAND nor AND nor SAND }
        begin
          if (theTerm.IsTypeOR) or
             (theTerm.IsTypeXOR) or
             (theTerm.IsTypeBASIC) then
          begin
            { (X)OR/BASIC: False if whole term is listed in templist (aka. "happened before") }
            Result := (tempList.IndexOf(theTerm) = -1); { True if not listed, False if listed }
          end else { neither one of PAND, SAND, AND, XOR, OR, BASIC }
          begin
            { just return, i.e. Result := True and another function in
              TFTALogic shall handle the situation }
          end;
        end;
      end;
    end;

begin
  Result := False;
{.....check currentTerm .......................................................}
  if (currentTerm.IsTypePAND) and (currentTerm.Count = 2) then
  begin
{.....configuration currentTerm ...............................................}
    { create tempList }
    tempList := TTFTAList.Create;
    tempList.OwnsObjects:=False;

    tempTerm1 := currentTerm[0];
    tempTerm2 := currentTerm[1];

    ScanAndAddToList(tempTerm1);
    { if ScanAndSearchForHits(op2) yields True, then there are no recurrences
      and thus no need for a change; if it yields False, then there is a
      recurrence and thus the term needs to be updated to theFALSEElement }
    if not ScanAndSearchForHits(tempTerm2) then
    begin
{.....redirection .............................................................}
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered LawOfNonrecurrence');{$ENDIF}
      Result := True;
      RedirectTerm(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'LawOfNonrecurrence');
    end;
    tempList.Destroy;
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform not[ XXX[ x,y,...] ] to YYY[ not[x], not[y], ... ]
  where YYY is or in case of XXX being AND and
        YYY is AND in case of XXX being OR or XOR
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function  GenericNOTBoolean (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList; flagANDtoOR : boolean) : boolean;
var theOldObject : TTFTAObject;
    newTerm      : TTFTAObject;
    existingTerm : TTFTAObject;
    numberOfChildren : integer;
    i            : integer = 0;
    theNEWType   : TTFTAOperatorType;
    flagNotAllExistedBefore : boolean = false;
begin
  Result := True;
  if flagANDtoOR then
  begin
    theNEWType := tftaEventTypeOR;
    {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered GenericNOTBoolean (AND)');{$ENDIF}
  end else
  begin
    theNEWType := tftaEventTypeAND;
    {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered GenericNOTBoolean (OR/XOR)');{$ENDIF}
  end;

{.....configure currentTerm ...................................................}
  theOldObject := currentTerm[0].Clone(eventlist);
  { clear the given event }
  currentTerm.Children.Clear;
  currentTerm.Mask;

  { for each child do }
  i := 0;
  numberOfChildren := theOldObject.Count;
  repeat
{.....create newTerm ..........................................................}
    newTerm := eventlist.NewItem;
{.....configure newTerm .......................................................}
    newTerm.EventType := tftaEventTypeNOT;
    newTerm.AddChild(theOldObject[i]);
    newTerm.CheckTermProperties;
    {$IfDef TESTMODE}newTerm.DEBUGPrint(false,eventlist,'NOT 2.' + IntToStr(i));{$ENDIF}
{.....replace newTerm .........................................................}
    eventlist.ReplaceWithIdentical(newTerm);
    flagNotAllExistedBefore := true;
{.....configure currentTerm....................................................}
    currentTerm.AddChild(newTerm);
    inc(i);
  until i = numberOfChildren;
  currentTerm.EventType := theNEWType;
  currentTerm.Unmask;
  currentTerm.CheckTermProperties;
{.....redirect currentTerm......................................................}
  if not flagNotAllExistedBefore then
  begin
    existingTerm := eventlist.FindIdenticalExisting(currentTerm);
    RedirectTerm(currentTerm,existingTerm,eventlist,theParent,theIndex,'NOTFalseTrue 1');
  end;
{.....free cloneTerm ..........................................................}
  eventlist.FreeTerm(theOldObject);
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform not[ and [x,y] ] to or [ not[x], not[y] ]
  transform not[ or  [x,y] ] to and[ not[x], not[y] ]
  transform not[ xor [x,y] ] to and[ not[x], not[y] ]
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function  NOTANDORXOR (currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := False;
{.....check currentTerm .......................................................}
  if (currentTerm.IsTypeNOT) then
  begin
{.....configuration currentTerm ...............................................}
    if (currentTerm[0].IsTypeAND) then
    begin
      Result := GenericNOTBoolean (currentTerm, theParent, theIndex, eventlist, true)
    end else
    begin
      if (currentTerm[0].IsTypeOR) or (currentTerm[0].IsTypeXOR) then
        Result := GenericNOTBoolean (currentTerm, theParent, theIndex, eventlist, false)
    end; { type check operand }
  end; { type check term }
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform not[False|True] to True|False
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function NOTFalseTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := False;
{.....check currentTerm .......................................................}
  if (currentTerm.IsTypeNOT) then
  begin
{.....configuration currentTerm ...............................................}
    if ( currentTerm[0] = eventlist.TheFALSEElement ) then
    begin
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered NOTFalseTrue (False)');{$ENDIF}
      Result := True;
{.....redirection currentTerm .................................................}
      RedirectTerm(currentTerm,eventlist.TheTRUEElement,eventlist,theParent,theIndex,'NOTFalseTrue 1');
    end else
    begin
{.....configuration currentTerm ...............................................}
      if ( currentTerm[0] = eventlist.TheTrueElement ) then
      begin
        {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered NOTFalseTrue (True)');{$ENDIF}
        Result := True;
{.....redirection currentTerm .................................................}
        RedirectTerm(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'NOTFalseTrue 2');
      end;
    end;
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform not[not[x]] to x
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function NOTNOT(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList): boolean;
begin
  Result := False;
  if ( currentTerm.IsTypeNOT    ) and ( currentTerm[0].IsTypeNOT ) then
  begin
    Result := True;
    {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered NOTNOT');{$ENDIF}
    RedirectTerm(currentTerm,currentTerm[0][0],eventlist,theParent,theIndex,'NOTNOT 1');
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  split OR with more than two operands
  This function is only a wrapper arround GenericSplit.
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function ORSplit( currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := GenericSplit(currentTerm,theParent,theIndex,eventlist,tftaEventTypeOR);
end;






{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform or[ ... False ... ] to or[ ... ]      or
  transform xor[ ... False ... ] to xor[ ... ]
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function ORXORFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var existingTerm : TTFTAObject;
    AtLeastOneIsFalse : boolean = false;
begin
{.....check currentTerm .......................................................}
  if (currentTerm.IsTypeOR) or (currentTerm.IsTypeXOR) then
  begin
{.....configuration currentTerm ...............................................}
    while Assigned(currentTerm.ExtractChild(eventlist.TheFALSEElement)) do
    begin
      AtLeastOneIsFalse := True;
    end;
    if AtLeastOneIsFalse then
    begin
      Result := True;
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered ORXORFalse');{$ENDIF}
      if currentTerm.Count > 1 then
      begin
{.....redirection .............................................................}
        existingTerm := eventlist.FindIdenticalExisting(currentTerm);
        if Assigned(existingTerm) then
        begin
          RedirectTerm(currentTerm,existingTerm,eventlist,theParent,theIndex,'ORXORFalse 1');
        end;
        {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'ORXORFalse 5');{$ENDIF}
      end else
      begin
        if currentTerm.Count = 1 then
        begin
          { no extra check is needed whether new term already exists, it exists (as child #0) }
          RedirectTerm(currentTerm,currentTerm[0],eventlist,theParent,theIndex,'ORXORFalse 2');
        end else
        begin { currentTerm has no children --> currentTerm was AND[TRUE,TRUE,TRUE,...] before }
          RedirectTerm(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'ORXORFalse 3');
        end;
      end;
    end;  { Result }
  end;  { isTypeOR / XOR }
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform or[ ... True ... ] to True  or
  transform xor[ ... True ... ] to True
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function ORXORTrue(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var AtLeastOneIsTrue : boolean = False;
    i : Integer;
    numberOfChildren : Integer;
begin
  Result := False; { default }
{.....check currentTerm .......................................................}
  if (currentTerm.IsTypeOR) or (currentTerm.IsTypeXOR) then
  begin
{.....configuration currentTerm ...............................................}
    { for each child do }
    i := 0;
    numberOfChildren := currentTerm.Count;
    repeat
      if currentTerm[i] = eventList.TheTRUEElement then
      begin
        {$IfDef TESTMODE}if not AtLeastOneIsTrue then currentTerm.DEBUGPrint(true,eventlist,'Entered ORXORTrue');{$ENDIF}
        AtLeastOneIsTrue := True ;
      end;
      inc(i);
    until AtLeastOneIsTrue or (i = numberOfChildren) ;

    if ( AtLeastOneIsTrue ) then
    begin
{.....redirection .............................................................}
      RedirectTerm(currentTerm,eventlist.TheTRUEElement,eventlist,theParent,theIndex,'ANDFalse 1');
      Result := True;
    end;
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform pand[x,False] or pand[False,x] pand[x,True] to False
  transform pand[True,x] to x
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function PANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
{.....check currentTerm .......................................................}
  Result := False;
  if (currentTerm.IsTypePAND) and (currentTerm.Count = 2) then
  begin
    if ( currentTerm[0] = eventlist.TheFALSEElement ) or
       ( currentTerm[1] = eventlist.TheFALSEElement ) or
       ( currentTerm[1] = eventlist.TheTRUEElement  ) then
    begin
{.....configuration currentTerm not necessary here ............................}
{.....redirection .............................................................}
      Result := True;
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered PANDFalse (V1)');{$ENDIF}
      RedirectTerm(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'PANDFalse 1');
    end
    else
    begin
{.....check currentTerm .......................................................}
      if currentTerm[0] = eventlist.TheTRUEElement then
      begin
{.....redirection .............................................................}
        Result := True;
        {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered PANDFalse (V2)');{$ENDIF}
        RedirectTerm(currentTerm,currentTerm[1],eventlist,theParent,theIndex,'PANDFalse 2');
      end;
    end;
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform pand[x,x] to False
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function PANDMultiples(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := False;
{.....check currentTerm .......................................................}
  if (currentTerm.IsTypePAND) and (currentTerm.Count = 2) then
  begin
    if ( currentTerm[0] = currentTerm[1] ) then
    begin
{.....redirection .............................................................}
      Result := True;
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered PANDMultiples');{$ENDIF}
      RedirectTerm(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'PANDMultiples 1');
    end;
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform pand[x,pand[y,z]] to pand[and[x,y],z]
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function PANDPANDTransform(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var cloneTerm    : TTFTAObject;
    tempTermX    : TTFTAObject;
    tempTermY    : TTFTAObject;
    tempTermZ    : TTFTAObject;
    newTerm      : TTFTAObject;
    existingTerm : TTFTAObject;
begin
  Result := False;
{.....check currentTerm .......................................................}
  if (currentTerm.IsTypePAND) and (currentTerm[1].IsTypePAND) and
     (currentTerm.Count = 2) and (currentTerm[1].Count = 2) then
  begin
    {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered PANDPANDTransform ');{$ENDIF}
{.....clone currentTerm........................................................}
    cloneTerm := currentTerm.Clone(eventlist);
    {$IfDef TESTMODE}cloneTerm.DEBUGPrint(false,eventlist,'(PANDPANDTransform) Created Clone of currentTerm ');{$ENDIF}
    currentTerm.Children.Clear;
    currentTerm.Mask;
{.....create newTerm ..........................................................}
    Result := True;
    tempTermX := cloneTerm[0];
    {$IfDef TESTMODE}tempTermX.DEBUGPrint(false,eventlist,'(PANDPANDTransform) Created XTerm ');{$ENDIF}
    tempTermY := cloneTerm[1][0];
    {$IfDef TESTMODE}tempTermY.DEBUGPrint(false,eventlist,'(PANDPANDTransform) Created YTerm ');{$ENDIF}
    tempTermZ := cloneTerm[1][1];
    {$IfDef TESTMODE}tempTermZ.DEBUGPrint(false,eventlist,'(PANDPANDTransform) Created ZTerm ');{$ENDIF}
    newTerm := eventlist.NewItem;
    existingTerm := newTerm; { remember for later comparison! }
{.....configure newTerm .......................................................}
    newTerm.EventType := tftaEventTypeAND;
    newTerm.AddChild(tempTermX);
    newTerm.AddChild(tempTermY);
    newTerm.CheckTermProperties;
    {$IfDef TESTMODE}newTerm.DEBUGPrint(false,eventlist,'(PANDPANDTransform) Created New ');{$ENDIF}
{ ....configuration currentTerm ...............................................}
    currentTerm.Unmask;
    currentTerm.AddChild(newTerm);
    currentTerm.AddChild(tempTermZ);
    currentTerm.CheckTermProperties;
    {$IfDef TESTMODE}currentTerm.DEBUGPrint(false,eventlist,'(PANDPANDTransform) configured currentTerm (unsorted)');{$ENDIF}
{ ....configuration and redirection currentTerm ...............................}
    SortOperands(currentTerm,theParent,theIndex,eventlist);
    currentTerm := theParent[theIndex];
    {$IfDef TESTMODE}currentTerm.DEBUGPrint(false,eventlist,'(PANDPANDTransform) configured currentTerm (sorted)');{$ENDIF}
{ ....free newTerm ............................................................}
    if currentTerm[0] <> existingTerm then
    begin { newTerm was redirected to an existing term by SortOperands }
      eventlist.FreeTerm(existingTerm);
    end;
{ ....free cloneTerm ..........................................................}
    eventlist.FreeTerm(cloneTerm);
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  split PAND with more than two operands
  This function is only a wrapper arround GenericSplit.
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function PANDSplit(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := GenericSplit(currentTerm,theParent,theIndex,eventlist,tftaEventTypePAND);
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform sand[ ... False ... ] to False
  do not touch sand[ < only TRUEs > ]  (those will later be transformed to TRUE
  transform sand[ ... True ... < not TRUE > ] to false
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function SANDFalse(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var AtLeastOneIsFalse       : boolean = False;
    AtLeastOneIsTrue        : boolean = False;
    AtLeastOneIsNormalEvent : boolean = False;
    i                       : Integer;
    numberOfChildren        : Integer;
    doBreak                 : boolean = False;
begin
{.....check currentTerm .......................................................}
  Result := False;
  if (currentTerm.IsTypeSAND) then
  begin
    i := 0;
    numberOfChildren := currentTerm.Count;
{.....configuration currentTerm not necessary here ............................}
    repeat
      if currentTerm[i] = eventlist.TheFALSEElement then AtLeastOneIsFalse := True
      else
        if currentTerm[i] = eventlist.TheTRUEElement then AtLeastOneIsTrue := True
        else
          AtLeastOneIsNormalEvent := True ;
      inc(i);
      doBreak := AtLeastOneIsFalse or ( AtLeastOneIsTrue and AtLeastOneIsNormalEvent )
    until doBreak or (i = numberOfChildren);
{.....redirection .............................................................}
    if doBreak then
    begin
      Result := True;
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered SANDFalse');{$ENDIF}
      RedirectTerm(currentTerm,eventlist.TheFALSEElement,eventlist,theParent,theIndex,'SANDFalse 1');
    end;
  end;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  transform sand[x,pand[y,z]] to pand[y,sand[x,z]]
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function SANDPANDTransform(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
var cloneCurrentTerm : TTFTAObject;
    clonePANDTerm    : TTFTAObject;
    existingTerm     : TTFTAObject;
    pandTermLastChild: TTFTAObject;
    tempTerm         : TTFTAObject;
    atLeastOneIsPAND : boolean;
    numberOfChildren : Integer;
    i                : Integer;
begin
  Result := False;
{.....check currentTerm .......................................................}
  if (currentTerm.IsTypeSAND) then
  begin
    { Seek first Child of type PAND (if none, then exit) }
    i:= 0;
    numberOfChildren := currentTerm.Count;
    repeat
      atLeastOneIsPAND := (currentTerm[i].IsTypePAND);
      inc(i);
    until atLeastOneIsPAND or ( i = numberOfChildren );
    if atLeastOneIsPAND then  { (at least) one child of the SAND is a PAND }
    begin
      Result := True;
      {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'Entered SANDPANDTransform');{$ENDIF}
{.....clone currentTerm........................................................}
      cloneCurrentTerm := currentTerm.Clone(eventlist);
      {$IfDef TESTMODE}
        cloneCurrentTerm.DEBUGPrint(false,eventlist,'SANDPANDTransform 1 Created Clone');
      {$ENDIF}
      currentTerm.Children.Clear;
      currentTerm.Mask;
{.....clone PAND Term..........................................................}
      clonePANDTerm := cloneCurrentTerm[i-1].Clone(eventlist);
      {$IfDef TESTMODE}clonePANDTerm.DEBUGPrint(false,eventlist,'SANDPANDTransform 2 Created PAND-Clone');{$ENDIF}
{.....configure clone PAND Term................................................}
      numberOfChildren := clonePANDTerm.Count;
      if numberOfChildren = 2 then
      begin
        tempTerm := clonePANDTerm;
        clonePANDTerm := tempTerm[0];
        pandTermLastChild := tempTerm[1];
{.....replace clone PAND Term..................................................}
        eventlist.FreeTerm(tempTerm);
      end else
      begin
{.....configure clone PAND Term................................................}
        pandTermLastChild := clonePANDTerm[numberOfChildren-1];
        clonePANDTerm.DeleteChild(numberOfChildren-1);
{.....replace clone PAND Term..................................................}
        eventlist.ReplaceWithIdentical(clonePANDTerm) ;
      end;
{.....configure clone of currentTerm...........................................}
      cloneCurrentTerm.DeleteChild(i-1);   { keep alle minus the first PAND }
      cloneCurrentTerm.AddChild(pandTermLastChild);
      cloneCurrentTerm.CheckTermProperties;
      existingTerm := cloneCurrentTerm; { remember for later comparison }
{ ....configuration currentTerm ...............................................}
      currentTerm.Unmask;
      currentTerm.AddChild(clonePANDTerm);
      currentTerm.AddChild(cloneCurrentTerm);
      currentTerm.EventType := tftaEventTypePAND;
      currentTerm.CheckTermProperties;
{ ....configuration and redirection currentTerm ...............................}
      SortOperands(currentTerm,theParent,theIndex,eventlist);
      currentTerm := theParent[theIndex];
{ ....free cloneCurrentTerm....................................................}
      if currentTerm[1] <> existingTerm then
      begin { cloneCurrentTerm was redirected to an existing term by SortOperands }
        eventlist.FreeTerm(existingTerm);
      end;
    end; { atLeastOneIsPAND }
  end; { type = SAND }
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  split SAND with more than two operands
  This function is only a wrapper arround GenericSplit.
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function SANDSplit( currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := GenericSplit(currentTerm,theParent,theIndex,eventlist,tftaEventTypeSAND);
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  a separate function in addition to the "real" sorting function (SortOperands)
  is needed for the iterative walkdown, as the return
  type for this needs to be ansistring (carrying the TemporalExpr of
  the descendant (thus eliminating the need for a separate walkdown to get
  Child.TemporalExpr)) while the return type for SortOperands needs to be
  boolean in order to notify the calling instance whether any changes have
  taken place.
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function ScanChildrenSorting(currentTerm :TTFTAObject; theParent : TTFTAList;
                           theIndex : Integer; eventlist : TTFTAEventLookupList;
                           var isChange : boolean) : ansistring;
var i                : integer;
    numberOfChildren : integer;
    expressionList   : TTFTAStringList = NIL;
    existingTerm     : TTFTAObject;
    tempObject       : TTFTAObject;
    wasSorted        : boolean;
    cloneTerm        : TTFTAObject;
    localIsChange    : boolean;
    tempString       : ansistring;
begin

  { test, whether event (and all its children) is already sorted and thus the correct
    string is given by currentTerm.TemporalExpr }
  if currentTerm.IsSorted then
  begin
    Result   := currentTerm.TemporalExpr;
    isChange := false;
    exit;
  end;

  if currentTerm.IsBasicEvent then
  begin;
    { is basic event }
    Result   := currentTerm.TemporalExpr; { name is stored in VExpr at creation of basic event }
    isChange := false; { BASIC: no change of the event }
    exit; { return from this iteration of ScanChildrenSorting }
  end;

  { test for NOT }
  if currentTerm.IsTypeNOT or currentTerm.IsTypeTOP then
  begin
    { NOT or TOP operator }
    Result := ScanChildrenSorting(currentTerm[0],currentTerm.Children,0,eventlist,isChange);
    Result := currentTerm.EventTypeToString + '[' + Result + ']';
    currentTerm.IsSorted := true;
    currentTerm.TemporalExpr := Result;
    { isChange is given by the iterative call of ScanChildrenSort (see two lines above) }
    exit;
  end;

  { no basic event, no NOT, no TOP -->  and, or, xor, pand, sand operators }
  localIsChange := False;
  i := 0;
  numberOfChildren := currentTerm.Count;
  Result := '';
  expressionList := TTFTAStringList.Create;
  repeat
    tempString := ScanChildrenSorting(currentTerm[i],currentTerm.Children,i,eventlist,localIsChange);
    expressionList.Add(tempString);
    isChange := isChange or localIsChange;
    inc(i);
  until (i = numberOfChildren);

  if currentTerm.IsTypePAND then
  begin
    { check whether identical event already exists }
    existingTerm := eventlist.FindIdenticalExisting(currentTerm);
    if Assigned(existingTerm) then
    begin
      RedirectTerm(currentTerm,existingTerm,eventlist,theParent,theIndex,'ScanChildrenSorting 5');
      isChange := true;
    end;
    { either way the following is the correct string... }
    Result := currentTerm.EventTypeToString + '[' + expressionList.CSText + ']';
    currentTerm.IsSorted := true;
    currentTerm.TemporalExpr := Result;
    exit;
  end;

  { should be commutative now }
  if not currentTerm.IsCommutative then
    ShowMessage('Fatal Error (081227.1722) while processing event ' + PointerAddrStr(currentTerm) + ': Should be commutative, right?');

  wasSorted := expressionList.Sort;

  if wasSorted then
  begin
{........ clone currentTerm....................................................}
    cloneTerm := currentTerm.Clone(eventlist);
{........ configure currentTerm................................................}
    currentTerm.Children.Clear;
    { for each child ... }
    i := 0;
    numberOfChildren := cloneTerm.Count;
    repeat
      currentTerm.AddChild(cloneTerm[expressionList[i].FormerPosition]);
      inc(i);
    until (i = numberOfChildren);
    currentTerm.CheckTermProperties;
    {$IfDef TESTMODE}currentTerm.DEBUGPrint(true,eventlist,'ScanChildrenSorting 2');{$ENDIF}
{........ free cloneTerm.......................................................}
    eventlist.FreeTerm(cloneTerm);
    isChange := true;
  end;
{........ redirect currentTerm.................................................}
  existingTerm := eventlist.FindIdenticalExisting(currentTerm);
  if Assigned(existingTerm) then
  begin
    RedirectTerm(currentTerm,existingTerm,eventlist,theParent,theIndex,'ScanChildrenSorting 4');
    { regardless of sorting a change (the re-link) has happened }
    isChange := true;
    Result := currentTerm.EventTypeToString + '[' + expressionList.CSText + ']';
    { do not overwrite the TemporalExpr as this now contains "redirected to ..." }
  end else
  begin
    Result := currentTerm.EventTypeToString + '[' + expressionList.CSText + ']';
    currentTerm.TemporalExpr := Result; { store string for quicker next search }
  end;
  currentTerm.IsSorted := true;
  isChange := isChange or localIsChange;

end; { function ScanChildrenSorting }





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  main loop for simplifications; calls all other routines
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function SimplificationLoop(currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; theEventList : TTFTAEventLookupList) : boolean;
var i               : Integer;
    numberOfChildren: Integer;
    changedSelf     : boolean;
    changedChildren : boolean;
begin
  Result := false;
  changedChildren := false;
  repeat { outer loop, continue until neither change in oneself nor in children }

    repeat  { inner loop, continue until no chnage in oneself }
      changedSelf := false; { flag whether in the inner loop a change happend }

      If (not changedSelf) and GenericCombine(currentTerm, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and PANDSplit(currentTerm, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and PANDFalse(currentTerm, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and ANDFalse(currentTerm, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and SANDFalse(currentTerm, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and ORXORTrue(currentTerm, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and NOTANDORXOR(currentTerm, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and NOTFalseTrue(currentTerm, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and NOTNOT(currentTerm, theParent , theIndex, theEventList ) then
        changedSelf := true;
      If (not changedSelf) and ANDTrue(currentTerm, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and ORXORFalse(currentTerm, theParent, theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and PANDMultiples(currentTerm, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and LawOfNonrecurrence(currentTerm, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and LawOfIdempotency(currentTerm, theParent , theIndex, theEventList ) then
        changedSelf := true;
      If (not changedSelf) and LawOfCompleteness(currentTerm, theParent , theIndex, theEventList ) then
        changedSelf := true;
      If (not changedSelf) and PANDPANDTransform(currentTerm, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and SANDPANDTransform(currentTerm, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and DistributeANDORXOR(currentTerm, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and DistributePANDXOR(currentTerm, theParent , theIndex, theEventList) then
        changedSelf := true;
      If (not changedSelf) and DistributeORPAND(currentTerm, theParent , theIndex, theEventList) then
        changedSelf := true;


      If changedSelf then
      begin
        currentTerm := theParent[theIndex];
        Result := true;  { Result is flag that ANY change has happened}
      end;
    until not changedSelf;

    changedChildren := False; { flag for change in children }
    if currentTerm.HasChildren then
    begin
      i := 0;
      numberOfChildren := currentTerm.Count ;
      repeat
        if SimplificationLoop( currentTerm[i] , currentTerm.Children, i, theEventList) then
        begin
          changedChildren := true; { flag, that at least one child was changed }
        end;
        inc(i);
      until (i = numberOfChildren);
      Result := Result or changedChildren; { true, if self changed or child changed }
    end;
  until not changedChildren;
end;  { function SimplificationLoop }





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  sort the operands of commutative terms (AND, OR, XOR, SAND)
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function SortOperands(tT :TTFTAObject; tP : TTFTAList; tI : Integer; tEL : TTFTAEventLookupList) : boolean;
var somethingChanged : boolean = false;  { set within ScanChildrenSorting if any changes occur }

    procedure ResetSortingFlag(theTerm : TTFTAObject);
    var i : Integer;
        numberOfChildren : Integer;
    begin
      if theTerm.HasChildren then
      begin
        i := 0;
        numberOfChildren := theTerm.Count;
        repeat
          ResetSortingFlag(theTerm[i]);
          theTerm.IsSorted := false;
          inc(i);
        until i =  numberOfChildren;
      end;
    end;

begin
  {$IfDef TESTMODE}tT.DEBUGPrint(true,tEL,'Entered SortOperands');{$ENDIF}
  ScanChildrenSorting(tT,tP,tI,tEL,somethingChanged);
  ResetSortingFlag(tT);
  Result := somethingChanged;
end;





{IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
  split XOR with more than two operands
  This function is only a wrapper arround GenericSplit.
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII}
function XORSplit( currentTerm :TTFTAObject; theParent : TTFTAList; theIndex : Integer; eventlist : TTFTAEventLookupList) : boolean;
begin
  Result := GenericSplit(currentTerm,theParent,theIndex,eventlist,tftaEventTypeXOR);
end;

end.

