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
{ TTFTAList }
function TTFTAList.Add(Item: TTFTAObject): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TTFTAList.Assign(Obj: TTFTAList);
begin
  inherited Assign(Obj);
end;

procedure TTFTAList.Clear;
begin
  self.List.Clear;
end;

{------------------------------------------------------------------------------
  Class-Constructor
------------------------------------------------------------------------------}
constructor TTFTAList.Create;
begin
  // nothing specific yet...
  inherited Create;
end;

procedure TTFTAList.Delete(Index : Integer);
begin
  self.List.Delete(Index);
end;

{ Delete all copies within list}
function TTFTAList.DeleteAllCopies : boolean;
var i : Integer;
begin
  { outer loop: for each child do... (repeat until because of number of children
    possibly varying during this operation as doublets are deleted from list) }
  i := 0;
  Result := false;
  repeat
    if self.DeleteAllCopiesOfObject(self[i]) then
      Result := true;
    inc(i);
  until i >= (Integer(self.Count) - 1);
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

destructor TTFTAList.Destroy;
begin
  self.Clear;
  inherited Destroy;
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
var i,j  : Integer;
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

function TTFTAList.GetItem(Index: Integer): TTFTAObject;
var tempObject : TTFTAObject;
begin
  Result := TTFTAObject(inherited Items[Index]);
  if Assigned(Result) and Result.NeedsToBeUpdated then
  begin
    tempObject := Result.PointerToUpdateObject;
    {$IfDef TESTMODE}
      if Assigned(Result.DEBUGMemo) then
        Result.DEBUGMemo.Append(PointerAddrStr(Result) + ' --> ' + PointerAddrStr(tempObject));
    {$EndIf}
    Delete(Index);
    Insert(Index,tempObject);
    Result := self.GetItem(Index);
  end;
end;

function TTFTAList.GetPlainItem(Index: Integer): TTFTAObject;
begin
  Result := TTFTAObject(inherited Items[Index]);
end;

function TTFTAList.IndexOf(Item: TTFTAObject): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TTFTAList.Insert(Index: Integer; Item: TTFTAObject);
begin
  inherited Insert(Index, Item);
end;

function TTFTAList.Remove(Item: TTFTAObject): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TTFTAList.SetItem(Index: Integer; Item: TTFTAObject);
begin
  inherited Items[Integer(Index)] := Item;
end;

//  [include file EOF]
