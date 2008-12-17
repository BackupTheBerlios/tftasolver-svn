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
{ TTFTAEventLookupList }
{$IfDef TESTMODE}
  constructor TTFTAEventLookupList.Create(pointerToDebugMemo : TMemo);
{$Else}
  constructor TTFTAEventLookupList.Create;
{$EndIf}
begin
  inherited Create;
  {$IfDef TESTMODE}self.DEBUGMemo  := pointerToDebugMemo; {$EndIf}
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
{------------------------------------------------------------------------------
  Class-Destructor
------------------------------------------------------------------------------}
destructor TTFTAEventLookupList.Destroy;
begin
  // nothing specific yet ...
  inherited Destroy;
end;
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
function TTFTAEventLookupList.Add(Item: TTFTAObject): Integer;
begin
  Result := inherited Add(Item);
end;
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
procedure TTFTAEventLookupList.Delete(Index : Integer);
begin
  self[Index].Free;
  self.List.Delete(Index);
end;
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
function TTFTAEventLookupList.Extract(Item: TTFTAObject):TTFTAObject;
begin
  Result := TTFTAObject(inherited Extract(TObject(Item)));
  if Assigned(Result) and Result.NeedsToBeUpdated then
  begin
    Result := Result.PointerToUpdateObject;
  end;
end;
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
function TTFTAEventLookupList.GetItem(Index: Integer): TTFTAObject;
begin
  Result := TTFTAObject(inherited Items[Index]);
end;
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
procedure TTFTAEventLookupList.Insert(Index: Integer; Item: TTFTAObject);
begin
  inherited Insert(Index, Item);
end;
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
function  TTFTAEventLookupList.ListHoldsObjectAt(Text : ansistring) : TTFTAObject;
var i : Integer;
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
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
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
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
{ public: version with options and true/false as logical value }
function TTFTAEventLookupList.NewItem(EventType: TTFTAOperatorType;IsBasicEvent: boolean;IsCoreEvent: boolean;IsEventSequence: boolean;IsNegated: boolean;IsNotCompletelyBuildYet : boolean;IsDisjunct: boolean;IsExtendedSequence : boolean;LogicalValue : boolean;NeedsToBeUpdated: boolean;PointerToUpdateObject : TTFTAObject;TemporalExpr: ansistring) : TTFTAObject;
begin
  Result:= self.NewItemPrivate(EventType,IsBasicEvent,IsCoreEvent,IsEventSequence,
                          IsNegated,IsNotCompletelyBuildYet,IsDisjunct,IsExtendedSequence,
                          NeedsToBeUpdated,PointerToUpdateObject,TemporalExpr);
  Result.SetLogicalValue(LogicalValue) ;
end;
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
{ public version with options and NIL as logical value }
function TTFTAEventLookupList.NewItem(EventType: TTFTAOperatorType;IsBasicEvent: boolean;IsCoreEvent: boolean;IsEventSequence: boolean;IsNegated: boolean;IsNotCompletelyBuildYet : boolean;IsDisjunct: boolean;IsExtendedSequence : boolean;LogicalValue : pointer;NeedsToBeUpdated: boolean;PointerToUpdateObject : TTFTAObject;TemporalExpr: ansistring) : TTFTAObject;
begin
  Result:= self.NewItemPrivate(EventType,IsBasicEvent,IsCoreEvent,IsEventSequence,
                          IsNegated,IsNotCompletelyBuildYet,IsDisjunct,IsExtendedSequence,
                          NeedsToBeUpdated,PointerToUpdateObject,TemporalExpr);
  Result.SetLogicalValue(NIL);
end;
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
{ private (generic) version}
function TTFTAEventLookupList.NewItemPrivate(EventType: TTFTAOperatorType;IsBasicEvent: boolean;IsCoreEvent: boolean;IsEventSequence: boolean;IsNegated: boolean;IsNotCompletelyBuildYet : boolean;IsDisjunct: boolean;IsExtendedSequence : boolean;NeedsToBeUpdated: boolean;PointerToUpdateObject : TTFTAObject;TemporalExpr: ansistring) : TTFTAObject;
var posInList: Integer = 0;
begin
  posInList := self.Add(TTFTAObject.Create);
  Result := self[posInList];
  Result.EventLookupList := self;

  Result.EventType                  :=  EventType                 ;
  {$IfDef TESTMODE}
  Result.DEBUGMemo                  :=  self.DEBUGMemo            ;
  {$EndIf}
  Result.IsBasicEvent               :=  IsBasicEvent              ;
  Result.IsCoreEvent                :=  IsCoreEvent               ;
  Result.IsEventSequence            :=  IsEventSequence           ;
  Result.IsNegated                  :=  IsNegated                 ;
  Result.IsNotCompletelyBuildYet    :=  IsNotCompletelyBuildYet   ;
  Result.IsDisjunct                 :=  IsDisjunct                ;
  Result.IsExtendedSequence         :=  IsExtendedSequence        ;
  Result.IsSorted                   :=  False                     ;
  Result.NeedsToBeUpdated           :=  NeedsToBeUpdated          ;
  Result.PointerToUpdateObject      :=  PointerToUpdateObject     ;
  Result.PosInEventList             :=  posInList                 ;
  Result.TemporalExpr               :=  TemporalExpr              ;

end;
{------------------------------------------------------------------------------
------------------------------------------------------------------------------}
procedure TTFTAEventLookupList.SetItem(Index: Integer; Item: TTFTAObject);
begin
  inherited Items[Index] := Item;
end;

//  [include file EOF]
