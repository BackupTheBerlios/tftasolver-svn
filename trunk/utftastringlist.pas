unit utftastringlist;

{ ############################################################################

  Copyright by Simon J. Schilling (the Author)
  Email: sjschilling@gmx.net
  Year: 2008

  Free Software. Enjoy.

  This file is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This file is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this file.  If not, see <http://www.gnu.org/licenses/>.

  ############################################################################ }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, contnrs;

type

  TTFTAStringListObject = class(TObject)
  private
    VText : ansistring;
    VData : pointer;
    VIndex: integer;
  public
    property Data : pointer read VData write VData;
    property Index : pointer read VIndex write VIndex;
    property Text : pointer read VText write VText;
  end;

  TTFTAStringList = class(TFPObjectList)
  protected
    function  GetItem(Index: Integer): TTFTAStringListObject;
    procedure SetItem(Index: Integer;  Item: TTFTAStringListObject);

  public
    constructor Create;
    destructor  Destroy;
    function  Add(Item: TTFTAStringListObject): Integer;
    function  Sort : boolean;
    property  Items[Index: Integer]: TTFTAStringListObject read GetItem write SetItem; default;
  end;

implementation

{ the follwoing is defined by OpenPascal for the TFPObjectList.Sort
  type TListSortCompare = function(Item1: Pointer;Item2: Pointer):Integer;
  now we need a function like this which actually compares two objects of
  type TTFTAObject for TTFTAList.Sort (by comparing their TemporalExpr, of course) }
function CompareTwoTTFTAStringListObjects(Item1: Pointer; Item2: Pointer) : Integer;
begin
  if Assigned(Item1) then
  begin
    if Assigned(Item2) then
      { Item1 <> NIL, Item2 <> NIL --> compare TemporalExpr's }
      Result := AnsiCompareStr(TTFTAStringListObject(Item1).Text,
                               TTFTAStringListObject(Item2).Text)
    else
      { Item1 <> NIL, Item2 = NIL --> return 1 (Item1 after Item2) }
      Result := 1;
  end else
  begin
    if Assigned(Item2) then
      { Item1 = NIL, Item2 <> NIL --> return -1 (Item2 after Item1) }
      Result := -1
    else
      { Item1 = NIL, Item2 = NIL --> return 0 (Item1 same as Item2) }
      Result := 0;
  end;
end;


{ ############################################################################ }
{ TTFTAEventLookupList }
constructor TTFTAStringList.Create;
begin
  inherited Create;
end;

function TTFTAStringList.Add(Item: TTFTAStringListObject): Integer;
begin
  Result := inherited Add(Item);
end;

{------------------------------------------------------------------------------
  Class-Destructor
------------------------------------------------------------------------------}
destructor TTFTAStringList.Destroy;
begin
  // nothing specific yet ...
  inherited Destroy;
end;

function TTFTAStringList.GetItem(Index: Integer): TTFTAStringListObject;
begin
  Result := TTFTAStringListObject(inherited Items[Index]);
end;

procedure TTFTAStringList.SetItem(Index: Integer; Item: TTFTAStringListObject);
begin
  inherited Items[Index] := Item;
  self[Index].Index:=Index;
end;

function TTFTAStringList.Sort : boolean;
var numberItems : Integer;
    i : Integer = 0;
    anyChange : boolean = False;
begin
  { it is necessary to differentiate between FPC compiler mode
    and "other" compiler mode (Delphi TurboPAscal etc.), as FPC has a
    different syntax for using function types...
    see: http://www.daniweb.com/forums/post740838-10.html }
  {$ifdef FPC}
    inherited Sort(@CompareTwoTTFTAStringListObjects);
  {$else}
    inherited Sort(CompareTwoTTFTAStringListObjects);
  {$endif}

  Result := True; { default }
  numberItems := self.Count;
  repeat
    anyChange := anyChange or ( self[i].Index <> i );
  until anyChange or (i = numberItems);

  Result := anyChange;
end;


end.

