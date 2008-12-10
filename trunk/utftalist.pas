unit utftalist;

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
  Classes, SysUtils, contnrs, utftaobject;

{ ############################################################################ }
{ ############################################################################ }
{ ############################################################################ }
type

  TTFTAList = class(TFPObjectList)
  private
    VOwnsObjects: Boolean;
  protected
    function  GetItem(Index: longword): TTFTAObject;
    procedure SetItem(Index: longword; Item: TTFTAObject);
  public
    function  Add(Item: TTFTAObject): integer;
    function  Extract(Item: TTFTAObject):TTFTAObject;
    function  First: TTFTAObject;
    function  IndexOf(Item: TTFTAObject): integer;
    function  Last: TTFTAObject;
    function  Remove(Item: TTFTAObject): integer;
    procedure Assign(Obj: TTFTAList);
    procedure Insert(Index: longword; Item: TTFTAObject);
    property  Items[Index: longword]: TTFTAObject read GetItem write SetItem; default;
    property  OwnsObjects: Boolean read VOwnsObjects write VOwnsObjects;

  end;


implementation

{ ############################################################################ }
{ TTFTAList }
function TTFTAList.Add(Item: TTFTAObject): integer;
begin
  Result := inherited Add(Item);
end;

procedure TTFTAList.Assign(Obj: TTFTAList);
begin
  inherited Assign(Obj);
end;

function TTFTAList.Extract(Item: TTFTAObject):TTFTAObject;
begin
  Result := TTFTAObject(inherited Extract(TObject(Item)));
end;

function TTFTAList.First :TTFTAObject;
begin
  Result := TTFTAObject(inherited First);
end;

function TTFTAList.GetItem(Index: longword): TTFTAObject;
begin
  Result := TTFTAObject(inherited Items[Index]);
end;

function TTFTAList.IndexOf(Item: TTFTAObject): integer;
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
end;

function TTFTAList.Remove(Item: TTFTAObject): integer;
begin
  Result := inherited Remove(Item);
end;

procedure TTFTAList.SetItem(Index: longword; Item: TTFTAObject);
begin
  inherited Items[Index] := Item;
end;

end.

