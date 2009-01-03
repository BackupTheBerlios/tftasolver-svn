unit utftaobject;

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
  Forms, Classes, SysUtils, Dialogs, ComCtrls, StdCtrls,
  StrUtils, contnrs, sjspointertools;

const
  cEventTypeStringArray : array[1..9] of string[30] = ('AND','OR','XOR','PAND','SAND','OTHER','TOP','NOT',
                                                       'BASIC');
  
type

  { forward declarations }
  TTFTAList = class;
  TTFTAObject = class;
  TTFTAEventLookupList = class;
  
  { ######################################################################## }
  TTFTAOperatorType = (tftaEventTypeAND := 1,
                       tftaEventTypeOR,
                       tftaEventTypeXOR,
                       tftaEventTypePAND,
                       tftaEventTypeSAND,
                       tftaEventTypeOTHER,
                       tftaEventTypeTOP,
                       tftaEventTypeNOT,
                       tftaEventTypeBASIC);
  
  { ########################################################################## }

  { ########################################################################## }

  TTFTAEventLookupList = class(TFPObjectList)
  private
    {$IfDef TESTMODE}
      VDEBUGMemo: TMemo;
    {$EndIf}
    VmyApplication : TApplication;
    VtheFALSE : TTFTAObject;
    VtheTRUE  : TTFTAObject;
    VSpeedSearchFlagOn : boolean;

    function  GetItem(Index: Integer): TTFTAObject;
    function  NewItemPrivate(EventType               : TTFTAOperatorType;
                            IsBasicEvent            : boolean;
                            IsCoreEvent             : boolean;
                            IsEventSequence         : boolean;
                            IsNegated               : boolean;
                            IsNotCompletelyBuildYet : boolean;
                            IsDisjunct              : boolean;
                            IsExtendedSequence      : boolean;
                            NeedsToBeUpdated        : boolean;
                            PointerToUpdateObject   : TTFTAObject;
                            TemporalExpr            : ansistring) : TTFTAObject;
    procedure SetItem(Index: Integer; Item: TTFTAObject);
    procedure SetSpeedSearchFlagOn(SetTo : boolean);

  public
    {$IfDef TESTMODE}
      constructor Create(pointerToDebugMemo : TMemo);
    {$Else}
      constructor Create;
    {$EndIf}
    destructor  Destroy;

    function  Add(Item: TTFTAObject): Integer;
    procedure Delete(Index : Integer);
    // function  Extract(Item: TTFTAObject):TTFTAObject;
    function  FindIdenticalExisting(Item : TTFTAObject) : TTFTAObject;
    function  FindIdenticalExisting(Text : ansistring) : TTFTAObject;
    procedure FreeTerm(theTerm : TTFTAObject);
    procedure Insert(Index: Integer; Item: TTFTAObject);
    function  NewItem: TTFTAObject;
    function  NewItem(      EventType               : TTFTAOperatorType;
                  		      IsBasicEvent            : boolean;
                  		      IsCoreEvent             : boolean;
                  		      IsEventSequence         : boolean;
                  		      IsNegated               : boolean;
                  		      IsNotCompletelyBuildYet : boolean;
                  		      IsDisjunct              : boolean;
                  		      IsExtendedSequence      : boolean;
                  		      LogicalValue            : boolean;    { <<<<<<<<< !}
                  		      NeedsToBeUpdated        : boolean;
                  		      PointerToUpdateObject   : TTFTAObject;
                  		      TemporalExpr            : ansistring) : TTFTAObject;
    function  NewItem(      EventType               : TTFTAOperatorType;
                  		      IsBasicEvent            : boolean;
                  		      IsCoreEvent             : boolean;
                  		      IsEventSequence         : boolean;
                  		      IsNegated               : boolean;
                  		      IsNotCompletelyBuildYet : boolean;
                  		      IsDisjunct              : boolean;
                  		      IsExtendedSequence      : boolean;
                  		      LogicalValue            : pointer;    { <<<<<<<<< !}
                  		      NeedsToBeUpdated        : boolean;
                  		      PointerToUpdateObject   : TTFTAObject;
                  		      TemporalExpr            : ansistring) : TTFTAObject;
    procedure Replace(var oldTerm : TTFTAObject; newTerm : TTFTAObject);
    procedure ReplaceWithIdentical(var oldTerm : TTFTAObject);

    {$IfDef TESTMODE}
    property  DEBUGMemo : TMemo read VDEBUGMemo write VDEBUGMemo;
    {$EndIf}
    property  Items[Index: Integer]: TTFTAObject read GetItem write SetItem; default;
    property  pointerToApplication : TApplication read VmyApplication write VmyApplication;
    property  SpeedSearchFlagOn : boolean read VSpeedSearchFlagOn write SetSpeedSearchFlagOn;
    property  TheFALSEElement : TTFTAObject read VtheFALSE;
    property  TheTRUEElement : TTFTAObject read VtheTRUE;

  end;

  { ########################################################################## }

  TTFTAList = class(TFPObjectList)
  private
    VOwnsObjects : Boolean;
    VOwner : TTFTAObject;
    
  public
    constructor Create;
    destructor  Destroy;

    function  Add(Item: TTFTAObject): Integer;
    function  DeleteAllCopies : boolean;
    function  DeleteAllCopiesOfObject(theItem : TTFTAObject) : boolean;
    function  Extract(Item: TTFTAObject):TTFTAObject;
    function  FindNextAfter(theItem : TTFTAObject; StartAfter : Integer) : Integer;
    function  GetItem(Index: Integer): TTFTAObject;
    function  GetPlainItem(Index: Integer): TTFTAObject;
    function  IndexOf(Item: TTFTAObject): Integer;
    function  Remove(Item: TTFTAObject): Integer;
    procedure Assign(Obj: TTFTAList);
    procedure Clear;
    procedure Delete(Index : Integer);
    procedure Insert(Index: Integer; Item: TTFTAObject);
    procedure SetItem(Index: Integer; Item: TTFTAObject);
    property  Items[Index: Integer]: TTFTAObject read GetItem write SetItem; default;
    property  Owner : TTFTAObject read VOwner write VOwner;
    property  OwnsObjects: Boolean read VOwnsObjects write VOwnsObjects;

  end;
  
  { ########################################################################## }

  TTFTAObject = class(TObject)

  private

    VChildren : TTFTAList;        { list of all my descendants (children) }
    {$IfDef TESTMODE}VDEBUGMemo : TMemo;{$EndIf}
    VEventLookupList : TTFTAEventLookuplist;
    VExpr : ansistring;           { if I'm a BasicEvent, I have to carry my event name }
    VIsBasicEvent : boolean;
    VIsCoreEvent : boolean;
    VIsDisjunct : boolean ;
    VIsEventSequence : boolean ;
    VIsExtendedSequence : boolean;
    VIsMasked : boolean;
    VIsMinimal : boolean;
    VIsNegated : boolean;
    VIsNotCompletelyBuildYet : boolean; { during built-up of an object (from InputString) no TempExpr checking must be performed }
    VIsSorted : boolean;
    VIsTrueFalse : integer;
    VNeedsToBeUpdated : boolean;  { true, if before an identical event was modified, then a update to this identical object is needed }
    VPointerToUpdateObject : TTFTAObject; { this points to the object the current object shall be updated to }
    VPosInEventList : Integer;   { allows to reference to my own pointer (within eventlist) }
    VSpeepSearch : boolean;
    VType: TTFTAOperatorType;     { what type am I? }

    function  CheckIsCommutative : boolean;
    function  CheckIsTypeAND     : boolean;
    function  CheckIsTypePAND    : boolean;
    function  CheckIsTypeSAND    : boolean;
    function  CheckIsTypeOR      : boolean;
    function  CheckIsTypeOTHER   : boolean;
    function  CheckIsTypeXOR     : boolean;
    function  CheckIsTypeTOP     : boolean;
    function  CheckIsTypeNOT     : boolean;
    function  CheckLogicFalse    : boolean;
    function  CheckLogicTrue     : boolean;
    function  GetChildrenBasicState : boolean;
    function  GetIsNegatedExtendedCoreEvent : boolean;
    function  GetPointerToUpdateObject : TTFTAObject;
    function  GetTempExpr : ansistring;
    function  GetTempExprDEBUG : ansistring;

    procedure CheckForCoreEvent;
    procedure CheckForDisjunctEvent;
    procedure CheckForEventSequenceEvent;
    procedure CheckForExtendedSequenceEvent;
    procedure SetIsBasicEvent (Parameter : boolean);
    procedure SetTempExpr(theExpr : ansistring);
    procedure SetVType(Parameter : TTFTAOperatorType);

  public
    constructor Create;
    destructor  Destroy;

    function  AddChild(Item : TTFTAObject) : Integer;
    function  Clone(list : TTFTAEventLookupList) : TTFTAObject;
    function  Count : Integer;
    function  EventTypeToString : string;
    function  ExtractChild(Item: TTFTAObject):TTFTAObject;
    function  GetChild(Index: Integer): TTFTAObject;
    function  HasChildren : boolean;
    function  RemoveChild(Item : TTFTAObject) : Integer;
    function  WriteStatus(indent:integer = 0)  : ansistring;

    procedure AssignChildren(Obj: TTFTAList);
    procedure CheckTermProperties;
    {$IfDef TESTMODE}
    procedure DEBUGPrint(isUpdate : boolean; eventlist : TTFTAEventLookupList; thestring : ansistring = '');
    {$EndIf}
    procedure DeleteChild(Index: Integer);
    procedure InsertChild(Index: Integer; Item: TTFTAObject);
    procedure Mask;
    procedure RedirectMe(newItem : TTFTAObject);
    procedure SetChild(Index: Integer; Item: TTFTAObject);
    procedure SetLogicalValue (Parameter : boolean);
    procedure SetLogicalValue (Parameter : pointer);
    procedure Unmask;

    property  Children : TTFTAList read VChildren write VChildren;
    {$IfDef TESTMODE}
    property  DEBUGMemo : TMemo read VDEBUGMemo write VDEBUGMemo;
    {$EndIf}
    property  EventType : TTFTAOperatorType read VType write SetVType;
    property  EventLookupList : TTFTAEventLookuplist read VEventLookupList write VEventLookupList;
    property  IsAllChildrenAreBasic : boolean read GetChildrenBasicState;
    property  IsBasicEvent : boolean read VIsBasicEvent write SetIsBasicEvent;
    property  IsCoreEvent : boolean read VIsCoreEvent write VIsCoreEvent;
    property  IsCommutative : boolean read CheckIsCommutative;
    property  IsDisjunct : boolean read VIsDisjunct write VIsDisjunct ;
    property  IsEventSequence : boolean read VIsEventSequence write VIsEventSequence;
    property  IsExtendedSequence : boolean read VIsExtendedSequence write VIsExtendedSequence  ;
    property  IsFalse : boolean read CheckLogicFalse;
    property  IsNegated : boolean read VIsNegated  write VIsNegated ;
    property  IsNotCompletelyBuildYet : boolean read VIsNotCompletelyBuildYet write VIsNotCompletelyBuildYet;
    property  IsSorted : boolean read VIsSorted write VIsSorted;
    property  IsTrue : boolean read CheckLogicTrue;
    property  IsTypeAND : boolean read CheckIsTypeAND;
    property  IsTypeBASIC : boolean read VIsBasicEvent;
    property  IsTypePAND : boolean read CheckIsTypePAND;
    property  IsTypeSAND : boolean read CheckIsTypeSAND;
    property  IsTypeOR : boolean read CheckIsTypeOR;
    property  IsTypeOTHER : boolean read CheckIsTypeOTHER;
    property  IsTypeTOP : boolean read CheckIsTypeTOP;
    property  IsTypeXOR : boolean read CheckIsTypeXOR;
    property  IsTypeNOT : boolean read CheckIsTypeNOT;
    property  Items[Index: Integer]: TTFTAObject read GetChild write SetChild; default;
    property  LogicLevel : integer read VIsTrueFalse;
    property  NeedsToBeUpdated : boolean read VNeedsToBeUpdated write VNeedsToBeUpdated;
    property  PlainTemporalExpr : ansistring read VExpr;
    property  PointerToUpdateObject : TTFTAObject read GetPointerToUpdateObject write VPointerToUpdateObject;
    property  PosInEventList : Integer read VPosInEventList write VPosInEventList;
    property  SpeedSearchIsSet : boolean read VSpeepSearch write VSpeepSearch;
    property  TemporalExpr : ansistring read GetTempExpr write SetTempExpr;
    property  TemporalExprDEBUG : ansistring read GetTempExprDEBUG;


  end;

implementation

// for better readability the implementation is sourced in different include files

// TTFTAObject
{$INCLUDE utftaobject_object.pas}

// TTFTAList
{$INCLUDE utftaobject_list.pas}

// TTFTAEventLookupList
{$INCLUDE utftaobject_lookuplist.pas}

end.
