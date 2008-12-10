unit utftaexpression;

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
  Forms, Classes, SysUtils, Dialogs, ComCtrls, StdCtrls, strutils, utftaobject
  , sjspointertools;

{ ############################################################################ }
{ ############################################################################ }
{ ############################################################################ }
type
  
  { ######################################################################## }
  TTFTAExpression = class
  private

    DEBUGMemo             : TMemo;
    VEventList            : TTFTAEventLookupList;
    VInputString          : ansistring;
    VInputTree            : TTreeNodes;
    VOutputMCSS           : TStrings;
    VOutputTree           : TTreeNodes;
    VTemporalTerm         : TTFTAObject;
    constructor Create;
    function  InputCheck : boolean;
    function  MakeNewGenericTreeElement(theTree : TTreeNodes; theLevel : TTreeNode; theName : ansistring; theObject : TTFTAObject = NIL) : TTreeNode;
    function  MakeNewInputTreeElement(theLevel : TTreeNode; theName : ansistring; theObject : TTFTAObject = NIL) : TTreeNode;
    function  MakeNewOutputTreeElement(theLevel : TTreeNode; theName : ansistring; theObject : TTFTAObject = NIL) : TTreeNode;
    function  AddNewTFTAObject(theObject : TTFTAObject; theType : TTFTAOperatorType) : TTFTAObject;
    function  ReadCount : longword;
    function  GetNextSubStringBreak (mytext : ansistring) : longword;

    procedure InputStringToTemporalExpression(currentObject : TTFTAObject; theString : ansistring; theIteration : longword);
    procedure TemporalExpressionToTreeNodes(theObjectLevel : TTFTAObject; theTree : TTreeNodes; theTreeLevel : TTreeNode; theIteration : longword);
    property  InputTree : TTreeNodes read VInputTree write VInputTree;
    property  OutputTree : TTreeNodes read VOutputTree write VOutputTree;

  public

    pointerToApplication : TApplication;

    procedure ParseInput(theTree : TTreeNodes);
    procedure BuildTreeNodes(theTree : TTreeNodes);
    procedure SetDEBUGMemo(Parameter : TMemo);
    property  EventList : TTFTAEventLookupList read VEventList write VEventList;
    property  Count : longword read ReadCount;
    property  InputString : ansistring read VInputString write VInputString;
    property  TemporalTerm : TTFTAObject read VTemporalTerm write VTemporalTerm;
    
  end;

{ ############################################################################ }
{ ############################################################################ }
{ ############################################################################ }
implementation

{------------------------------------------------------------------------------
  constructor TTFTAExpression.Create;
------------------------------------------------------------------------------}
constructor TTFTAExpression.Create;
begin
  inherited;
  InputTree  := TTreeNodes.Create(nil);
  OutputTree := TTreeNodes.Create(nil);
end;

{ ############################################################################ }

{------------------------------------------------------------------------------
  TTFTAExpression.ParseInput steuert die Erstellung der Baumstruktur.
  Vorher (im Moment deaktiviert) wird eine Plausibilisierung der Eingabe
  vorgenommen.
------------------------------------------------------------------------------}
function TTFTAExpression.ReadCount : longword;
begin
  Result := TemporalTerm.Children.Count;
end;

{ ############################################################################ }

{------------------------------------------------------------------------------
  TTFTAExpression.ParseInput steuert die Erstellung der Baumstruktur.
  Vorher (im Moment deaktiviert) wird eine Plausibilisierung der Eingabe
  vorgenommen.
------------------------------------------------------------------------------}
procedure TTFTAExpression.ParseInput (theTree : TTreeNodes);
var i            : longword;
    j            : longword;
    topTreeLevel : TTreeNode;
begin

  if ( InputCheck ) then
  begin
    { Set a pointer to the TreeNodes of InputTreeView within Mainwindow }
    InputTree := theTree;

    { Clear the DebugWindow from old messages }
    If Assigned(DEBUGMemo) then
    begin
      DEBUGMemo.Lines.Clear;
      DEBUGMemo.Append('DEBUG:');
    end;
    
    { create the Event-Lookup-List }
    EventList := TTFTAEventLookupList.Create(DEBUGMemo);

    EventList.pointerToApplication := pointerToApplication;

    { create the TemporalTerm (TTFTAObject), TOP level event }
    TemporalTerm := EventList.NewItem(tftaEventTypeTOP,
                                      false, {IsBasicEvent}
                                      false, {IsCoreEvent}
                                      false, {IsEventSequence}
                                      false, {IsNegated}
                                      false, {IsNotCompletelyBuildYet}
                                      false, {IsDisjunct}
                                      false, {IsExtendedSequence}
                                      NIL,   {LogicalValue}
                                      false, {NeedsToBeUpdatedfalse}
                                      NIL,   {PointerToUpdateObject}
                                      'TOP'
                                     );

    if Assigned( DEBUGMemo) then DEBUGMemo.Append('TOP Levels created' + sLineBreak +
                     '  @ Address TemporalTerm: ' + PointerAddrStr(TemporalTerm) + sLineBreak +
                     '  @ Address TOP InputTreeNode: ' + PointerAddrStr(topTreeLevel) + sLineBreak +
                     '  @ Address FALSETerm: ' + PointerAddrStr(EventList.TheFALSEElement) + sLineBreak +
                     '  @ Address TRUETerm: ' + PointerAddrStr(EventList.TheTRUEElement) );

    { call the converting routine;
      provide parameters: a newly created child in TemporalTerm,
                          the whole InputString,
                          IterationsLevel 0 }
    InputStringToTemporalExpression(AddNewTFTAObject(TemporalTerm, tftaEventTypeOTHER), InputString, 0);
    
    { now build InputTree from TemporalTerm }
    TemporalExpressionToTreeNodes(TemporalTerm, InputTree, NIL, 0);

    if Assigned( DEBUGMemo) then
      for i := 0 to EventList.Count -1 do
      begin
        EventList[i].DEBUGPrint(false,EventList);
      end;
    
  end;  { ParseInput}
end;

{------------------------------------------------------------------------------
  TTFTAExpression.InputCheck ist momentan ein Dummy fuer eine
  spaetere Funktion, die den uebergebenen String mit der logischen Funktion
  formal und / oder inhaltlich plausibilisieren soll.
  Momentan wird davon ausgegangen, dass der String formal "sinnvoll" und
  "gueltig" vom Benutzer eingegeben wird.
  Eine manuelle Plausibilisierung kann der Nutzer selbst vornehmen, in dem er
  auf die erzeugte Baumstruktur sieht. Viele ungueltige / unlogische Eingaben
  erscheinen dort als "unsinnige" Aeste.
  ------------------------------------------------------------------------------}
function TTFTAExpression.InputCheck : boolean;
var posBraceOpen : Integer = 0;
    posBraceClose: Integer = 0;
    noBraceOpen  : Integer = 0;
    noBraceClose : Integer = 0;
    lastResult   : boolean = false;
begin

  Result := True;
  { basic checks }
  { 1: check if number of "[" matches number of "]" }
    repeat
      posBraceOpen := PosEx('[',self.InputString,posBraceOpen+1);
      if not (posBraceOpen = 0) then inc(noBraceOpen);
    until (posBraceOpen = 0);
    repeat
      posBraceClose := PosEx(']',self.InputString,posBraceClose+1);
      if not (posBraceClose = 0) then inc(noBraceClose);
    until (posBraceClose = 0);
    lastResult := (noBraceOpen = noBraceClose);
    if not lastResult then
      ShowMessage('Der eigegebene Term hat ' + IntToStr(noBraceOpen) +
                   ' oeffnende Klammern aber '+ IntToStr(noBraceClose) +
                   ' schliessende Klammern. Geben Sie einen korrigierten Term ein.');
    Result := Result and lastResult;
    lastResult := false;

end;


{------------------------------------------------------------------------------
  make new node in XXXTree at theLevel of theTree,
  give it the name provided in theName
  and let its Data property point to theObject (if given, else NIL)
------------------------------------------------------------------------------}
function TTFTAExpression.MakeNewGenericTreeElement(theTree : TTreeNodes; theLevel : TTreeNode; theName : ansistring; theObject : TTFTAObject = NIL) : TTreeNode;
begin

  Result := theTree.AddChild(theLevel,theName);

  if Assigned(theObject) then
    Result.Data := Pointer(theObject)
  else
    Result.Data := NIL;

  Result.MakeVisible;

end;


{------------------------------------------------------------------------------
  call  MakeNewGenericTreeElement with theTree set to InputTree
------------------------------------------------------------------------------}
function TTFTAExpression.MakeNewInputTreeElement(theLevel : TTreeNode; theName : ansistring; theObject : TTFTAObject = NIL) : TTreeNode;
begin

  Result := MakeNewGenericTreeElement(InputTree,theLevel,theName,theObject);
  
end;


{------------------------------------------------------------------------------
  call  MakeNewGenericTreeElement with theTree set to OutputTree
------------------------------------------------------------------------------}
function TTFTAExpression.MakeNewOutputTreeElement(theLevel : TTreeNode; theName : ansistring; theObject : TTFTAObject = NIL) : TTreeNode;
begin

  Result := MakeNewGenericTreeElement(OutputTree,theLevel,theName,theObject);

end;


{------------------------------------------------------------------------------
  make new child in TemporalTerm at the theObject level
------------------------------------------------------------------------------}
function TTFTAExpression.AddNewTFTAObject(theObject : TTFTAObject;
                                          theType : TTFTAOperatorType) : TTFTAObject;
begin

  { object is NOT part of EventList yet --> create new object and set its
    properties and point Result to the newly created object }
  Result := EventList.NewItem;
  
  if Assigned( DEBUGMemo) then
    DEBUGMemo.Append('TTFTAExpression.AddNewTFTAObject from ' +
                      PointerAddrStr(theObject) + ' --- new address: ' +
                      PointerAddrStr(Result)    );

  { put pointer to newly created object to TemporalTerm }
  theObject.AddChild(Result);
  Result.EventType := theType;
  
end;


{------------------------------------------------------------------------------
  Setze den Zeiger zum Debug-Ausgabe-Fenster des Hauptfensters
  Damit ist dieses Ausgabefenster
------------------------------------------------------------------------------}
procedure TTFTAExpression.SetDEBUGMemo(Parameter : TMemo);
begin
  DEBUGMemo := Parameter;
end;

{------------------------------------------------------------------------------
  scanns a given string (mystring) for the next usable expression holding a
  temporal expression.
  Returns the position of the next colon (i.e. break symbol within string)
------------------------------------------------------------------------------}
function TTFTAExpression.GetNextSubStringBreak (mytext : ansistring) : longword;
var i        : longword;
    theCount : longword;
begin

  theCount := 0;

  for i:=1 to Length(mytext) do
    begin
    if (mytext[i] = '[') then inc(theCount);
    if (mytext[i] = ']') and (theCount>0) then dec(theCount);
    if (mytext[i] = ',') and (theCount=0) then
    begin
      Result := i;
      exit;
    end;
  end;
  Result := 0;

end;

{------------------------------------------------------------------------------
  TTFTAExpression.StringZuBaumStruktur is a parser that creates both the
  TTFTAObject structure stored in TemporalTerm and the TTreeNodes structure stored
  in InputTree according to a given temporal Expression provided by theString.

  This parser does not change the content of this expression, this is later
  achieved by a separate procedure and manually called by the user (with
  button "StartCalculation")
------------------------------------------------------------------------------}
procedure TTFTAExpression.InputStringToTemporalExpression(currentObject  : TTFTAObject;
                                                          theString      : ansistring;
                                                          theIteration   : longword);
var
   theNewString           : ansistring;
   theRestString          : ansistring;
   isInfixOperatorThere   : boolean;
   theNextBreak           : longword;
   theTempObject          : TTFTAObject;
   
begin

  theNewString := theString;
  isInfixOperatorThere := false;
  
  { je nachdem, welcher Operator vornansteht, werden 3, 4 oder 5 Zeichen von
  links entfernt, immer entfernt wird zudem das "]" ganz rechts vom String.
  Dann wird ein Flag gesetzt, dass ein Operator ("Prefix") vorhanden war ---
  es gibt ja auch die Moeglichkeit, dass keiner vorhanden war --- und es wird
  ein neuer Knoten in der Treeview erstellt, der den jeweiligen Operatornamen
  enthaehlt. Ein Sonderfall ist der not Operator, da dieser Prefix ist, s.u. }
  if AnsiStartsText('and[',theNewString) then
  begin
    theNewString := AnsiLeftStr(theNewString,Length(theNewString)-1);
    theNewString := AnsiRightStr(theNewString,Length(theNewString)-4);
    isInfixOperatorThere := true;
    currentObject.EventType:=tftaEventTypeAND;
  end else
    if AnsiStartsText('xor[',theNewString) then
    begin
      theNewString := AnsiLeftStr(theNewString,Length(theNewString)-1);
      theNewString := AnsiRightStr(theNewString,Length(theNewString)-4);
      isInfixOperatorThere := true;
      currentObject.EventType:=tftaEventTypeXOR;
    end else
      if AnsiStartsText('or[',theNewString) then
      begin
        theNewString := AnsiLeftStr(theNewString,Length(theNewString)-1);
        theNewString := AnsiRightStr(theNewString,Length(theNewString)-3);
        isInfixOperatorThere := true;
        currentObject.EventType:=tftaEventTypeOR;
      end else
        if AnsiStartsText('pand[',theNewString) then
        begin
          theNewString := AnsiLeftStr(theNewString,Length(theNewString)-1);
          theNewString := AnsiRightStr(theNewString,Length(theNewString)-5);
          isInfixOperatorThere := true;
          currentObject.EventType:=tftaEventTypePAND;
        end else
          if AnsiStartsText('sand[',theNewString) then
          begin
            theNewString := AnsiLeftStr(theNewString,Length(theNewString)-1);
            theNewString := AnsiRightStr(theNewString,Length(theNewString)-5);
            isInfixOperatorThere := true;
            //SetObjectProps(currentObject,theNewString,tftaEventTypeSAND);
            currentObject.EventType:=tftaEventTypeSAND;
          end;

  if (isInfixOperatorThere = true ) then
  { it is one of the following operators: and, or, xor, pand, sand}
  begin
    { flag indicating that currenObject is in Build-Up-process }
    currentObject.IsNotCompletelyBuildYet := true;
    {default value so that the loop is passed at least once }
    theNextBreak := 1;
    while theNextBreak<>0 do
    begin
      theNextBreak := GetNextSubStringBreak(theNewString);
      { results in 0 if there is no further Break, i.e. the last element within
        the input string is reached  }
      { if <>0, i.e. not the last element, then cut off the current element from
        the string as to prepare the string for the next loop pass }
      if theNextBreak <> 0 then
      begin
        theRestString := AnsiRightStr(theNewString,Length(theNewString)-theNextBreak);
        { there is a colon at the end of each element within the string. delete it }
        theNewString := AnsiLeftStr(theNewString,theNextBreak-1);
      end;
      { there are two possibilities:
        1) the object described in theNewString is a truely novel object, i.e. is
           not listed in EventList and thus needs to be created and its children
           (if any) parsed, too.
        2) the object described in theNewString is listed in EventList and thus
           only needs to be linked in. Its childred (if any) are already created, too }
      { the check is done via comparing the provided theExpr with all entries in
        EventList. This is timeconsuming. But pointing to the same objects if there
        are multiple occurences of the same event (object) saves (potentially)
        huge amounts of time as term transformations only happen once }
      theTempObject := EventList.ListHoldsObjectAt(theNewString);
      //if Assigned( DEBUGMemo) then
          //DEBUGMemo.Append('String2Expr: Testing for existance on ' + theNewString + ' --- ' +
                            //PointerAddrStr(theTempObject)  );

      if not Assigned(theTempObject) then
      begin
        { object is NOT part of EventList yet }
        { the tftaEventTypeOTHER is the default value which will be overwritten in
          the next iterative loop when the now newly created element will be handled }
        theTempObject := AddNewTFTAObject(currentObject, tftaEventTypeOTHER);
        { now parse the children (if any) and create them too (iteratively) }
        InputStringToTemporalExpression(theTempObject, theNewString, theIteration+1);

      end else
      begin
        { object IS part of EventList --> just add pointer to it to TemporalTerm;
          there is no need to parse its children, as they already exists }
        currentObject.AddChild(theTempObject);
        //if Assigned( DEBUGMemo) then
          //DEBUGMemo.Append('Added ' + PointerAddrStr(theTempObject) + ' to ' + PointerAddrStr(currentObject)  );
      end;
      
      { go on with next sibling to theTempObject (if any) }
      theNewString := theRestString
    end;
    { at this point, currentObject is completed and thus may from now on have a TempExpr }
    currentObject.IsNotCompletelyBuildYet := false;
    { next, set properties of the current node /treelevel according to the
      properties of its children (if any) }
    currentObject.CheckTermProperties;

  end else
  { "else" denotes that there either is a not[...] term or a no-operator-term,
    i.e. a basic event. }
  begin
    if AnsiStartsText('not[',theNewString) then
    { not is a prefix operator (other than the infix operators from above) and
      thus has only one parameter. The following changes and term-creations are
      basically the same as above, extend only by some adoptions to the only one parameter }
    begin
      currentObject.IsNotCompletelyBuildYet := true;
      { remove the "not[" and the trailing "]" }
      theNewString := AnsiLeftStr(theNewString,Length(theNewString)-1);
      theNewString := AnsiRightStr(theNewString,Length(theNewString)-4);
      //SetObjectProps(currentObject,theNewString,tftaEventTypeNOT);
      currentObject.EventType:=tftaEventTypeNOT;
      { create actual new object or link existing one (see comments from above) }
      theTempObject := EventList.ListHoldsObjectAt(theNewString);
      if not Assigned(theTempObject) then
      begin
        { object is NOT part of EventList yet }
        { the tftaEventTypeOTHER is the default value which will be overwritten in
          the next iterative loop when the now newly created element will be handled }
        theTempObject := AddNewTFTAObject(currentObject, tftaEventTypeOTHER);
        { now parse the children (if any) and create them too (iteratively) }
        InputStringToTemporalExpression(theTempObject, theNewString, theIteration+1);
      end else
      begin
        { object IS part of EventList --> just add pointer to it to TemporalTerm;
          there is no need to parse its children, as they already exists }
        currentObject.AddChild(theTempObject);
      end;
      { at this point, currentObject is completed and thus may from now on have a TempExpr }
      currentObject.IsNotCompletelyBuildYet := false;
      { next, set properties of the current node /treelevel according to the
        properties of its children (if any) }
      currentObject.IsNegated:=true;
      currentObject.CheckTermProperties;
    end else
    { if there is also no NOT operator, then it is a basic event }
    begin
        currentObject.SetIsBasicEvent(true);
        { set "name" (i.e. temporalexpression) only if basic event AND not TRUE or FALSE }
        if (currentObject <> eventlist.TheFALSEElement) and
           (currentObject <> eventlist.TheTRUEElement) then
        begin
          currentObject.TemporalExpr := theNewString;
        end;
        
        //if Assigned( DEBUGMemo) then
          //DEBUGMemo.Append('New Basic Event ' + PointerAddrStr(currentObject) + ' called ' + theNewString );
    end;
  end;
  
  { at this point, currentObject is completed and thus may from now on have a TempExpr }
  currentObject.IsNotCompletelyBuildYet := false;
  
end;

{------------------------------------------------------------------------------
  TTFTAExpression.StringZuBaumStruktur is a parser that creates both the
  TTFTAObject structure stored in TemporalTerm and the TTreeNodes structure stored
  in InputTree according to a given temporal Expression provided by theString.

  This parser does not change the content of this expression, this is later
  achieved by a separate procedure and manually called by the user (with
  button "StartCalculation")
------------------------------------------------------------------------------}
procedure TTFTAExpression.TemporalExpressionToTreeNodes(theObjectLevel : TTFTAObject;
                                                        theTree        : TTreeNodes;
                                                        theTreeLevel   : TTreeNode;
                                                        theIteration   : longword);
var
   i                : longword;
   theTempLevel     : TTreeNode;

begin

  { if theObjectLevel points to a basic event then the treenode's text is the
    name (the TemporalExpr) of the object; otherwise it is the string denoting the type
    of object and the objects children need to be parsed, too }
  if theObjectLevel.EventType <> tftaEventTypeBASIC then
  begin
    theTempLevel := MakeNewGenericTreeElement(theTree,theTreeLevel,theObjectLevel.EventTypeToString,theObjectLevel);
    { now parse its children }
    inc(theIteration);
    for i := 0 to theObjectLevel.Count - 1  do
    begin
      TemporalExpressionToTreeNodes(theObjectLevel[i], theTree, theTempLevel, theIteration);
    end;
  end else
  begin  { Basic Event }
    theTempLevel := MakeNewGenericTreeElement(theTree,theTreeLevel,theObjectLevel.TemporalExpr,theObjectLevel);
  end;

end;


{------------------------------------------------------------------------------
  provide a TTreeNodes Structure of the TemporalTerm
------------------------------------------------------------------------------}
procedure TTFTAExpression.BuildTreeNodes(theTree : TTreeNodes);
begin
  TemporalExpressionToTreeNodes(TemporalTerm, theTree, NIL, 0);
end;
  
end.
