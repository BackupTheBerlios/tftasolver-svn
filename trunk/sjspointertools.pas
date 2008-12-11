unit sjspointertools;

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
  Classes, SysUtils, strutils;
  
  function PointerAddr(theObject : TObject) : Integer;
  function PointerAddrStr(theObject : TObject) : ansistring;
  function BoolToString(b : boolean) : ansistring;

implementation

  function PointerAddrStr(theObject : TObject) : ansistring;
  begin
    Result := IntToHex(integer(theObject),8);
  end;
  
  function PointerAddr(theObject : TObject) : Integer;
  begin
    Result := Integer(theObject);
  end;
  
  {------------------------------------------------------------------------------
    Liefert einen Boolean Wert als String (-> True oder False)
  ------------------------------------------------------------------------------}
  function BoolToString(b : boolean) : ansistring;
  begin
    if b then BoolToString := 'TRUE' else BoolToString := 'FALSE';
  end;

end.

