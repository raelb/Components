unit BCControl.ObjectInspector;

interface

uses
  Winapi.Messages, System.Classes, System.Types, System.UITypes, System.TypInfo, Vcl.Controls, Vcl.Graphics,
  VirtualTrees, sSkinManager, sPanel;

type
  TBCArrayOfString = array of string;

  TBCObjectInspector = class(TVirtualDrawTree)
  strict private
    FFirstNodeClick: Boolean;
    FInspectedObject: TObject;
    FLastNode: PVirtualNode;
    FSkinManager: TsSkinManager;
    FUnlistedProperties: TStrings;
    function PropertyValueAsString(AObject: TObject; APropertyInfo: PPropInfo): string;
    function UnlistedProperty(const APropertyName: string): Boolean;
    procedure DoClick(const HitInfo: THitInfo; const AIsDblClick: Boolean);
    procedure DoObjectChange;
    procedure SetInspectedObject(const AValue: TObject);
  private
    FUseVCLStyles: Boolean;
    function GetVCLColor(Color: TColor): TColor;
  protected
    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink; override;
    function DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal): Boolean; override;
    procedure Click; override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect); override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure DoFreeNode(ANode: PVirtualNode); override;
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo); override;
    procedure HandleMouseDown(var Message: TWMMouse; var HitInfo: THitInfo); override;
    procedure SetValueAsString(ANode: PVirtualNode; const AValue: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { TODO: There might be a better way to know unlisted properties... }
    procedure AddUnlistedProperties(const AProperties: TBCArrayOfString);
    property InspectedObject: TObject read FInspectedObject write SetInspectedObject;
    property SkinManager: TsSkinManager read FSkinManager write FSkinManager;
    property UseVCLStyles: Boolean read FUseVCLStyles write FUseVCLStyles;
  end;

  TBCObjectInspectorEditLink = class(TInterfacedObject, IVTEditLink)
  strict private
    FColumn: Integer;
    FEditor: TWinControl;
    FImagePanel: TsPanel;
    FObjectInspector: TBCObjectInspector;
    FNode: PVirtualNode;
    procedure DoBitmapButtonClick(Sender: TObject);
    procedure DoComboSelect(Sender: TObject);
    procedure DoComboDblClick(Sender: TObject);
    procedure FreeImagePanel;
    procedure SetValue;
  protected
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditExit(Sender: TObject);
  public
    destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var AMessage: TMessage); stdcall;
    procedure SetBounds(ARect: TRect); stdcall;
  end;

implementation

{$R *.res}

uses
  Winapi.Windows, Winapi.UxTheme, System.SysUtils, System.Math, System.Variants, Vcl.Themes, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Menus, sComboBox, sComboBoxes, sEdit, sSpeedButton, sDialogs;

const
  TYPE_BITMAP = 'TBitmap';

  ShortCuts: array[0..110] of TShortCut = (
    scNone,
    Byte('A') or scCtrl,
    Byte('B') or scCtrl,
    Byte('C') or scCtrl,
    Byte('D') or scCtrl,
    Byte('E') or scCtrl,
    Byte('F') or scCtrl,
    Byte('G') or scCtrl,
    Byte('H') or scCtrl,
    Byte('I') or scCtrl,
    Byte('J') or scCtrl,
    Byte('K') or scCtrl,
    Byte('L') or scCtrl,
    Byte('M') or scCtrl,
    Byte('N') or scCtrl,
    Byte('O') or scCtrl,
    Byte('P') or scCtrl,
    Byte('Q') or scCtrl,
    Byte('R') or scCtrl,
    Byte('S') or scCtrl,
    Byte('T') or scCtrl,
    Byte('U') or scCtrl,
    Byte('V') or scCtrl,
    Byte('W') or scCtrl,
    Byte('X') or scCtrl,
    Byte('Y') or scCtrl,
    Byte('Z') or scCtrl,
    Byte('A') or scCtrl or scAlt,
    Byte('B') or scCtrl or scAlt,
    Byte('C') or scCtrl or scAlt,
    Byte('D') or scCtrl or scAlt,
    Byte('E') or scCtrl or scAlt,
    Byte('F') or scCtrl or scAlt,
    Byte('G') or scCtrl or scAlt,
    Byte('H') or scCtrl or scAlt,
    Byte('I') or scCtrl or scAlt,
    Byte('J') or scCtrl or scAlt,
    Byte('K') or scCtrl or scAlt,
    Byte('L') or scCtrl or scAlt,
    Byte('M') or scCtrl or scAlt,
    Byte('N') or scCtrl or scAlt,
    Byte('O') or scCtrl or scAlt,
    Byte('P') or scCtrl or scAlt,
    Byte('Q') or scCtrl or scAlt,
    Byte('R') or scCtrl or scAlt,
    Byte('S') or scCtrl or scAlt,
    Byte('T') or scCtrl or scAlt,
    Byte('U') or scCtrl or scAlt,
    Byte('V') or scCtrl or scAlt,
    Byte('W') or scCtrl or scAlt,
    Byte('X') or scCtrl or scAlt,
    Byte('Y') or scCtrl or scAlt,
    Byte('Z') or scCtrl or scAlt,
    VK_F1,
    VK_F2,
    VK_F3,
    VK_F4,
    VK_F5,
    VK_F6,
    VK_F7,
    VK_F8,
    VK_F9,
    VK_F10,
    VK_F11,
    VK_F12,
    VK_F1 or scCtrl,
    VK_F2 or scCtrl,
    VK_F3 or scCtrl,
    VK_F4 or scCtrl,
    VK_F5 or scCtrl,
    VK_F6 or scCtrl,
    VK_F7 or scCtrl,
    VK_F8 or scCtrl,
    VK_F9 or scCtrl,
    VK_F10 or scCtrl,
    VK_F11 or scCtrl,
    VK_F12 or scCtrl,
    VK_F1 or scShift,
    VK_F2 or scShift,
    VK_F3 or scShift,
    VK_F4 or scShift,
    VK_F5 or scShift,
    VK_F6 or scShift,
    VK_F7 or scShift,
    VK_F8 or scShift,
    VK_F9 or scShift,
    VK_F10 or scShift,
    VK_F11 or scShift,
    VK_F12 or scShift,
    VK_F1 or scShift or scCtrl,
    VK_F2 or scShift or scCtrl,
    VK_F3 or scShift or scCtrl,
    VK_F4 or scShift or scCtrl,
    VK_F5 or scShift or scCtrl,
    VK_F6 or scShift or scCtrl,
    VK_F7 or scShift or scCtrl,
    VK_F8 or scShift or scCtrl,
    VK_F9 or scShift or scCtrl,
    VK_F10 or scShift or scCtrl,
    VK_F11 or scShift or scCtrl,
    VK_F12 or scShift or scCtrl,
    scCtrl or VK_RETURN,
    scCtrl or VK_SPACE,
    VK_INSERT,
    VK_INSERT or scShift,
    VK_INSERT or scCtrl,
    VK_DELETE,
    VK_DELETE or scShift,
    VK_DELETE or scCtrl,
    VK_BACK or scAlt,
    VK_BACK or scShift or scAlt);

type
  TPropertyArray = array of PPropInfo;

  TBCObjectInspectorNodeRecord = record
    PropertyInfo: PPropInfo;
    PropertyName: string;
    PropertyValue: string;
    PropertyObject: TObject;
    TypeInfo: PTypeInfo;
    HasChildren: Boolean;
    IsBoolean: Boolean;
    IsSetValue: Boolean;
    SetIndex: Integer;
    ReadOnly: Boolean;
  end;
  PBCObjectInspectorNodeRecord = ^TBCObjectInspectorNodeRecord;

{ TBCObjectInspector }

constructor TBCObjectInspector.Create;
var
  LColumn: TVirtualTreeColumn;
begin
  inherited Create(AOwner);

  FUseVCLStyles := False;
  DragOperations := [];
  Header.AutoSizeIndex := 1;
  Header.Options := [hoAutoResize, hoColumnResize];
  { property column }
  LColumn := Header.Columns.Add;
  LColumn.Text := 'Property';
  LColumn.Width := 160;
  LColumn.Options := [coFixed, coAllowClick, coParentColor, coEnabled, coParentBidiMode, coResizable, coVisible, coAllowFocus];
  { value column }
  LColumn := Header.Columns.Add;
  LColumn.Text := 'Value';
  LColumn.Options := [coAllowClick, coParentColor, coEnabled, coParentBidiMode, coResizable, coVisible, coAllowFocus, coEditable];

  IncrementalSearch := isAll;
  Indent := 18;
  EditDelay := 0;
  TextMargin := 4;

  FLastNode := nil;

  TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoChangeScale];
  TreeOptions.MiscOptions := [toEditable, toFullRepaintOnResize, toGridExtensions, toWheelPanning, toEditOnClick];
  TreeOptions.PaintOptions := [toHideFocusRect, toShowButtons, toShowRoot, toShowVertGridLines, toThemeAware];
  TreeOptions.SelectionOptions := [toExtendedFocus];

  FUnlistedProperties := TStringList.Create;
end;

destructor TBCObjectInspector.Destroy;
begin
  FUnlistedProperties.Free;

  inherited;
end;

function TBCObjectInspector.UnlistedProperty(const APropertyName: string): Boolean;
begin
  Result := FUnlistedProperties.IndexOf(APropertyName) <> -1;
end;

procedure TBCObjectInspector.AddUnlistedProperties(const AProperties: TBCArrayOfString);
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(AProperties) - 1 do
    FUnlistedProperties.Add(AProperties[LIndex]);
end;

procedure TBCObjectInspector.Click;
var
  LPNode: PVirtualNode;
  LData: PBCObjectInspectorNodeRecord;
begin
  LPNode := GetFirstSelected;
  if Assigned(LPNode) then
    EditNode(LPNode, Header.Columns.ClickIndex);
  { Checkbox }
  if not FFirstNodeClick and (Header.Columns.ClickIndex = 1) then
  begin
    LData := GetNodeData(LPNode);
    if LData.IsBoolean then
    begin
      if CompareText(LData.PropertyValue, BooleanIdents[True]) = 0 then
        SetValueAsString(LPNode, BooleanIdents[False])
      else
        SetValueAsString(LPNode, BooleanIdents[True]);
    end;
  end;
end;

procedure TBCObjectInspector.SetValueAsString(ANode: PVirtualNode; const AValue: String);
var
  LIntegerSet: TIntegerSet;
  LData, LParentData: PBCObjectInspectorNodeRecord;
  LParentObject: TObject;
begin
  LData := GetNodeData(ANode);

  LParentData := GetNodeData(ANode.Parent);
  LParentObject := nil;
  if Assigned(LParentData) then
    LParentObject := LParentData.PropertyObject;
  if not Assigned(LParentData) then
    LParentObject := FInspectedObject;

  if LData.IsSetValue then
  begin
    Integer(LIntegerSet) := StringToSet(LParentData.PropertyInfo, LParentData.PropertyValue);
    if CompareText(AValue, BooleanIdents[True]) = 0 then
      Include(LIntegerSet, LData.SetIndex)
    else
      Exclude(LIntegerSet, LData.SetIndex);
    SetValueAsString(ANode.Parent, SetToString(LParentData.PropertyInfo, Integer(LIntegerSet)));
  end
  else
  if LData.TypeInfo = System.TypeInfo(TColor) then
    SetPropValue(LParentObject, LData.PropertyName, StringToColor(AValue))
  else
  if LData.TypeInfo = System.TypeInfo(TCursor) then
    SetPropValue(LParentObject, LData.PropertyName, StringToCursor(AValue))
  else
  if LData.TypeInfo = System.TypeInfo(TShortCut) then
    SetPropValue(LParentObject, LData.PropertyName, TextToShortCut(AValue))
  else
  if LData.TypeInfo.Kind in [tkInteger] then
    SetPropValue(LParentObject, LData.PropertyName, StrToInt(AValue))
  else
  if LData.TypeInfo.Kind in [tkFloat] then
    SetPropValue(LParentObject, LData.PropertyName, StrToFloat(AValue))
  else
    SetPropValue(LParentObject, LData.PropertyName, AValue);

  if Assigned(LParentObject) then
    LData.PropertyValue := PropertyValueAsString(LParentObject, LData.PropertyInfo)
  else
    LData.PropertyValue := AValue;

  Invalidate;
end;

procedure TBCObjectInspector.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
var
  LRect: TRect;
  LData: PBCObjectInspectorNodeRecord;
  LSize: TSize;
  LHandle: THandle;
begin
  inherited;

  if Column = 0 then
    Exit;

  { Checkbox }
  LData := GetNodeData(Node);
  if LData.IsBoolean then
  begin
    LRect := CellRect;
    Inc(LRect.Left, 2);

    if UseThemes then
    begin
      LHandle := OpenThemeData(Handle, 'BUTTON');
      if LHandle <> 0 then
      try
        GetThemePartSize(LHandle, Canvas.Handle, BP_CHECKBOX, CBS_CHECKEDNORMAL, nil, TS_DRAW, LSize);
        LRect.Right  := LRect.Left + LSize.cx;
        DrawThemeBackground(LHandle, Canvas.Handle, BP_CHECKBOX, IfThen(CompareText(LData.PropertyValue, BooleanIdents[True]) = 0,
          CBS_CHECKEDNORMAL, CBS_UNCHECKEDNORMAL), LRect, nil);
      finally
        CloseThemeData(LHandle);
      end;
    end
    else
    begin
      LRect.Right  := LRect.Left + GetSystemMetrics(SM_CXMENUCHECK);
      DrawFrameControl(Canvas.Handle, LRect, DFC_BUTTON, IfThen(CompareText(LData.PropertyValue, BooleanIdents[True]) = 0,
        DFCS_CHECKED, DFCS_BUTTONCHECK));
    end;
  end;
  { Colorbox }
  if LData.TypeInfo = System.TypeInfo(TColor) then
  begin
    Canvas.Brush.Color := StringToColor(LData.PropertyValue);
    LRect := CellRect;
    Inc(LRect.Left, 2);
    Inc(LRect.Top, 2);
    Dec(LRect.Bottom, 3);
    LRect.Right := LRect.Left + (LRect.Bottom - LRect.Top);
    Canvas.FillRect(LRect);
    Canvas.Brush.Color := clBlack;
    Canvas.FrameRect(LRect);
  end;
end;

procedure TBCObjectInspector.DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates);
var
  LData: PBCObjectInspectorNodeRecord;
begin
  inherited;
  LData := GetNodeData(Node);
  if LData.HasChildren then
    Include(InitStates, ivsHasChildren);
end;

procedure TBCObjectInspector.DoFreeNode(ANode: PVirtualNode);
var
  LData: PBCObjectInspectorNodeRecord;
begin
  LData := GetNodeData(ANode);
  Finalize(LData^);
  inherited;
end;

procedure TBCObjectInspector.DoPaintNode(var PaintInfo: TVTPaintInfo);
var
  LData, LDataParent: PBCObjectInspectorNodeRecord;
  LString: string;
  LRect: TRect;
  LHandle: THandle;
  LSize: TSize;
  LParentPropertyObject: TObject;
  LColor: Integer;
  LBackGroundColorIsLight: Boolean;

  function BackGroundColorIsLight: Boolean;
  var
    LRGB: TColor;
  begin
    LRGB := ColorToRGB(Color);
    Result := ((LRGB and $FF) + (LRGB shr 8 and $FF) + (LRGB shr 16 and $FF))>= $180;
  end;

begin
  inherited;
  LBackGroundColorIsLight := BackGroundColorIsLight;
  if FUseVCLStyles then
    LBackGroundColorIsLight := False;

  with PaintInfo do
  begin
    LData := GetNodeData(Node);
    LParentPropertyObject := nil;
    if Assigned(Node.Parent) then
    begin
      LDataParent := GetNodeData(Node.Parent);
      if Assigned(LDataParent) then
        LParentPropertyObject := LDataParent.PropertyObject;
    end;
    if not Assigned(LParentPropertyObject) then
      LParentPropertyObject := FInspectedObject;

    if not Assigned(LData) then
      Exit;

    Canvas.Font.Style := [];

    case Column of
      0:
        if Assigned(SkinManager) then
          Canvas.Font.Color := SkinManager.GetActiveEditFontColor
        else
          Canvas.Font.Color := GetVCLColor(clWindowText);
      1:
        begin
          if LBackGroundColorIsLight then
            Canvas.Font.Color := SysColorToSkin(clNavy)
          else
          if Assigned(SkinManager) then
            Canvas.Font.Color := SkinManager.GetActiveEditFontColor
          else
            Canvas.Font.Color := GetVCLColor(clSkyBlue); // if Dark VCL Theme

          if Assigned(LParentPropertyObject) and Assigned(LData.PropertyInfo) and
            IsStoredProp(LParentPropertyObject, LData.PropertyInfo) then
            if not IsDefaultPropertyValue(LParentPropertyObject, LData.PropertyInfo, nil) then
              Canvas.Font.Style := [fsBold];
        end;
    end;

    if LBackGroundColorIsLight and LData.ReadOnly then
      Canvas.Font.Color := SysColorToSkin(clGray);

    if vsSelected in PaintInfo.Node.States then
    begin
      if Assigned(SkinManager) and SkinManager.Active then
        Canvas.Font.Color := SkinManager.GetHighLightFontColor
      else
        Canvas.Font.Color := GetVCLColor(clHighlightText);
    end;

    SetBKMode(Canvas.Handle, TRANSPARENT);

    LRect := ContentRect;
    InflateRect(LRect, -TextMargin, 0);
    Dec(LRect.Right);
    Dec(LRect.Bottom);

    if PaintInfo.Column = 0 then
      LString := LData.PropertyName
    else
    begin
      if LData.IsBoolean then
      begin
        if UseThemes then
        begin
          LHandle := OpenThemeData(Handle, 'BUTTON');
          if LHandle <> 0 then
          try
            GetThemePartSize(LHandle, Canvas.Handle, BP_CHECKBOX, CBS_CHECKEDNORMAL, nil, TS_DRAW, LSize);
            Inc(LRect.Left, LSize.cx + 2);
          finally
            CloseThemeData(LHandle);
          end;
        end
        else
          Inc(LRect.Left, GetSystemMetrics(SM_CXMENUCHECK) + 2);
      end;

      LString := LData.PropertyValue;

      if LData.TypeInfo = System.TypeInfo(TColor) then
      begin
        Inc(LRect.Left, LRect.Bottom - LRect.Top);
        if not IdentToColor(LData.PropertyValue, LColor) then
          LString := ColorToString(StrToInt(LData.PropertyValue));
      end
      else
      if LData.TypeInfo = System.TypeInfo(TShortCut) then
        LString := ShortCutToText(StrToInt(LData.PropertyValue));
    end;

    if Length(LString) > 0 then
      DrawTextW(Canvas.Handle, PWideChar(LString), Length(LString), LRect, DT_TOP or DT_LEFT or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TBCObjectInspector.SetInspectedObject(const AValue: TObject);
begin
  if AValue <> FInspectedObject then
  begin
    FInspectedObject := AValue;
    DoObjectChange;
  end;
end;

function IsBooleanValue(const AValue: string): Boolean;
begin
  Result := (CompareText(AValue, BooleanIdents[True]) = 0) or (CompareText(AValue, BooleanIdents[False]) = 0);
end;

procedure TBCObjectInspector.DoObjectChange;
var
  LPropertyCount: Integer;
  LPropertyArray: TPropertyArray;
  LIndex: Integer;
  LPNode: PVirtualNode;
  LData: PBCObjectInspectorNodeRecord;
  LEditor: TsEdit;
begin
  if not Assigned(FInspectedObject) then
    Exit;

  LPropertyCount := GetPropList(FInspectedObject.ClassInfo, tkProperties, nil);
  SetLength(LPropertyArray, LPropertyCount);
  GetPropList(FInspectedObject.ClassInfo, tkProperties, PPropList(LPropertyArray));

  BeginUpdate;
  Clear;
  NodeDataSize := SizeOf(TBCObjectInspectorNodeRecord);

  LEditor := TsEdit.Create(nil);
  try
    DefaultNodeHeight := LEditor.Height + 2;
  finally
    LEditor.Free;
  end;

  for LIndex := 0 to LPropertyCount - 1 do
  begin
    LPNode := AddChild(nil);
    LData := GetNodeData(LPNode);
    LData.PropertyInfo := LPropertyArray[LIndex];
    LData.TypeInfo := LData.PropertyInfo^.PropType^;
    LData.PropertyName := string(LPropertyArray[LIndex].Name);
    if UnlistedProperty(LData.PropertyName) then
    begin
      DeleteNode(LPNode);
      Continue;
    end;
    LData.PropertyValue := PropertyValueAsString(FInspectedObject, LPropertyArray[LIndex]);
    LData.IsBoolean := (LData.TypeInfo.Kind = tkEnumeration) and IsBooleanValue(LData.PropertyValue);
    LData.HasChildren := (LData.TypeInfo.Kind = tkSet) or
      ((LData.TypeInfo.Kind = tkClass) and (LData.TypeInfo.Name <> TYPE_BITMAP) and (LData.PropertyValue <> ''));
    LData.ReadOnly := Assigned(LData.PropertyInfo) and not Assigned(LData.PropertyInfo.SetProc);
    if LData.TypeInfo.Kind = tkClass then
    begin
      LData.PropertyObject := GetObjectProp(FInspectedObject, LData.PropertyInfo);
      if not Assigned(LData.PropertyObject) then
      begin
        DeleteNode(LPNode);
        Continue;
      end;
    end;
  end;

  EndUpdate;
end;

function TBCObjectInspector.DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal): Boolean;
var
  LIndex: Integer;
  LData, LParentData, LNewData: PBCObjectInspectorNodeRecord;
  LObject: TObject;
  LCollection: TCollection;
  LPNode: PVirtualNode;
  LPropertyArray: TPropertyArray;
  LPropertyCount: Integer;
  LSetTypeData: PTypeData;
  LSetAsIntValue: Longint;
  LParentObject: TObject;
begin
  Result := True;

  LData := GetNodeData(Node);

  LParentData := GetNodeData(Node.Parent);
  if Assigned(LParentData) then
    LParentObject := LParentData.PropertyObject
  else
    LParentObject := FInspectedObject;

  if (LData.TypeInfo.Kind = tkClass) and (LData.PropertyValue <> '') then
  begin
    if LParentObject is TCollection then
      LObject := LData.PropertyObject
    else
      LObject := GetObjectProp(LParentObject, LData.PropertyInfo);

    if LObject is TCollection then
    begin
      LCollection := LObject as TCollection;
      for LIndex := 0 to LCollection.Count - 1 do
      begin
        LPNode := AddChild(Node);
        LNewData := GetNodeData(LPNode);
        LNewData.PropertyInfo := nil;
        LNewData.PropertyName := 'Item[' + IntToStr(LIndex) + ']';
        LNewData.PropertyValue := '(' + LCollection.ItemClass.ClassName +')';
        LNewData.TypeInfo := LCollection.ItemClass.ClassInfo;
        LNewData.HasChildren := True;
        LNewData.PropertyObject := LCollection.Items[LIndex];
      end;
    end
    else
    if Assigned(LObject) then
    begin
      LPropertyCount := GetPropList(LObject.ClassInfo, tkProperties, nil);
      SetLength(LPropertyArray, LPropertyCount);
      GetPropList(LObject.ClassInfo, tkProperties, PPropList(LPropertyArray));

      for LIndex := 0 to LPropertyCount - 1 do
      begin
        LPNode := AddChild(Node);
        LNewData := GetNodeData(LPNode);
        LNewData.PropertyInfo := LPropertyArray[LIndex];
        LNewData.PropertyName := string(LPropertyArray[LIndex].Name);
        LNewData.PropertyValue := PropertyValueAsString(LObject, LPropertyArray[LIndex]);
        LNewData.TypeInfo := LNewData.PropertyInfo^.PropType^;
        LNewData.IsBoolean := (LNewData.TypeInfo.Kind = tkEnumeration) and IsBooleanValue(LNewData.PropertyValue);
        LNewData.HasChildren := (LNewData.TypeInfo.Kind = tkSet) or
          ((LNewData.TypeInfo.Kind = tkClass) and (LNewData.TypeInfo.Name <> TYPE_BITMAP) and (LNewData.PropertyValue <> ''));
        if LNewData.TypeInfo.Kind = tkClass then
          LNewData.PropertyObject := GetObjectProp(LObject, LNewData.PropertyInfo);
        LNewData.ReadOnly := Assigned(LNewData.PropertyInfo) and not Assigned(LNewData.PropertyInfo.SetProc);
      end;
    end;
  end
  else
  if LData.TypeInfo.Kind = tkSet then
  begin
    LSetTypeData := GetTypeData(GetTypeData(LData.TypeInfo)^.CompType^);
    LSetAsIntValue := GetOrdProp(LParentObject, LData.PropertyInfo);

    for LIndex := LSetTypeData.MinValue to LSetTypeData.MaxValue do
    begin
      LPNode := AddChild(Node);
      LNewData := GetNodeData(LPNode);
      LNewData.PropertyInfo := LData.PropertyInfo; //nil;
      LNewData.PropertyName := GetEnumName(GetTypeData(LData.TypeInfo)^.CompType^, LIndex);
      LNewData.PropertyValue := BooleanIdents[LIndex in TIntegerSet(LSetAsIntValue)];
      LNewData.TypeInfo := nil;
      LNewData.IsBoolean := IsBooleanValue(LNewData.PropertyValue);
      LNewData.HasChildren := False;
      LNewData.IsSetValue := True;
      LNewData.SetIndex := LIndex;
    end;
  end;
  ChildCount := Self.ChildCount[Node];
end;

function TBCObjectInspector.PropertyValueAsString(AObject: TObject; APropertyInfo: PPropInfo): string;
var
  LPropertyType: PTypeInfo;
  LTypeKind: TTypeKind;

  function SetAsString(AValue: Longint): string;
  var
    LIndex: Integer;
    LBaseType: PTypeInfo;
  begin
    LBaseType := GetTypeData(LPropertyType)^.CompType^;
    Result := '[';
    for LIndex := 0 to SizeOf(TIntegerSet) * 8 - 1 do
      if LIndex in TIntegerSet(AValue) then
      begin
        if Length(Result) <> 1 then
          Result := Result + ',';
        Result := Result + GetEnumName(LBaseType, LIndex);
      end;
    Result := Result + ']';
  end;

  function IntegerAsString(ATypeInfo: PTypeInfo; AValue: Longint): String;
  var
    LIdent: string;
    LIntToIdent: TIntToIdent;
  begin
    LIntToIdent := FindIntToIdent(ATypeInfo);
    if Assigned(LIntToIdent) and LIntToIdent(AValue, LIdent) then
      Result := LIdent
    else
      Result := IntToStr(AValue);
  end;

  function CollectionAsString(Collection: TCollection): String;
  begin
    Result := '(' + Collection.ClassName + ')';
  end;

  function OrdAsString: String;
  var
    LValue: Longint;
  begin
    LValue := GetOrdProp(AObject, APropertyInfo);
    case LTypeKind of
      tkInteger:
        Result := IntegerAsString(LPropertyType, LValue);
      tkChar:
        Result := Chr(LValue);
      tkSet:
        Result := SetAsString(LValue);
      tkEnumeration:
        Result := GetEnumName(LPropertyType, LValue);
    end;
  end;

  function FloatAsString: String;
  var
    LValue: Extended;
  begin
    LValue := GetFloatProp(AObject, APropertyInfo);
    Result := FloatToStr(LValue);
  end;

  function StrAsString: String;
  begin
    Result := GetWideStrProp(AObject, APropertyInfo);
  end;

  function ObjectAsString: String;
  var
    LValue: TObject;
  begin
    LValue := GetObjectProp(AObject, APropertyInfo);
    if not Assigned(LValue) then
      Result := ''
    else
      Result := '(' + LValue.ClassName + ')';
  end;

begin
  LPropertyType := APropertyInfo^.PropType^;
  LTypeKind := LPropertyType^.Kind;
  case LTypeKind of
    tkInteger, tkChar, tkEnumeration, tkSet:
      Result := OrdAsString;
    tkFloat:
      Result := FloatAsString;
    tkString, tkLString, tkWString, tkUString:
      Result := StrAsString;
    tkClass:
      Result := ObjectAsString;
  end;
end;

procedure TBCObjectInspector.HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo);
begin
  inherited;
  DoClick(HitInfo, True);
  Click;
end;

procedure TBCObjectInspector.HandleMouseDown(var Message: TWMMouse; var HitInfo: THitInfo);
begin
  inherited;
  DoClick(HitInfo, False);
end;

procedure TBCObjectInspector.DoClick(const HitInfo: THitInfo; const AIsDblClick: Boolean);
var
  LData: PBCObjectInspectorNodeRecord;
begin
  if not AIsDblClick then
    FFirstNodeClick := HitInfo.HitNode <> FLastNode
  else
    FFirstNodeClick := False;
  ClearSelection;
  Selected[HitInfo.HitNode] := True;
  if not AIsDblClick then
    FLastNode := HitInfo.HitNode;

  LData := GetNodeData(HitInfo.HitNode);
  if (HitInfo.HitColumn = 0) and (hiOnItemRight in HitInfo.HitPositions) or
    (HitInfo.HitColumn = 1) and Assigned(LData.TypeInfo) and (LData.TypeInfo.Kind in [tkClass, tkSet]) then
    Expanded[HitInfo.HitNode] := not Expanded[HitInfo.HitNode];
end;

procedure TBCObjectInspector.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  LData: PBCObjectInspectorNodeRecord;
begin
  LData := GetNodeData(Node);
  Allowed := (Column > 0) and not LData.ReadOnly and not LData.IsBoolean and not LData.IsSetValue and
    ((LData.TypeInfo.Kind <> tkClass) or (LData.TypeInfo.Kind = tkClass) and (LData.TypeInfo.Name = TYPE_BITMAP)) and
    (LData.TypeInfo.Kind <> tkSet);
end;

function TBCObjectInspector.DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;
begin
  //inherited;
  Result := TBCObjectInspectorEditLink.Create;
end;

function TBCObjectInspector.GetVCLColor(Color: TColor): TColor;
var
  LStyle: TCustomStyleServices;
begin
  Result := Color;
  if FUseVCLStyles then
  begin
    LStyle := TStyleManager.ActiveStyle;
    if not LStyle.IsSystemStyle then
      Result := LStyle.GetSystemColor(Color);
  end;
end;

{ TSTVirtualGridEditLink }

destructor TBCObjectInspectorEditLink.Destroy;
begin
  FreeImagePanel;
  if Assigned(FEditor) then
    if FEditor.HandleAllocated then
      PostMessage(FEditor.Handle, CM_RELEASE, 0, 0);
  inherited;
end;

procedure TBCObjectInspectorEditLink.FreeImagePanel;
begin
  if Assigned(FImagePanel) then
  begin
    FImagePanel.Free;
    FImagePanel := nil;
  end;
end;

procedure TBCObjectInspectorEditLink.EditExit(Sender: TObject);
begin
  FObjectInspector.EndEditNode;
end;

procedure TBCObjectInspectorEditLink.EditKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #27:
      begin
        FObjectInspector.CancelEditNode;
        Key := #0;
      end;
    #13:
      begin
        FObjectInspector.EndEditNode;
        Key := #0;
      end;
  else
    inherited;
  end
end;

procedure TBCObjectInspectorEditLink.DoBitmapButtonClick(Sender: TObject);
var
  LOpenPictureDialog: TsOpenPictureDialog;
  LSavePictureDialog: TsSavePictureDialog;
  LData: PBCObjectInspectorNodeRecord;
  LBitmap: Vcl.Graphics.TBitmap;
  LImagePanelShape: TShape;
  LImage: TImage;
begin
  LData := FObjectInspector.GetNodeData(FNode);
  LBitmap := LData.PropertyObject as Vcl.Graphics.TBitmap;

  case (Sender as TsSpeedButton).Tag of
    0:
      begin
        if Assigned(FImagePanel) then
          FreeImagePanel
        else
        begin
          FImagePanel := TsPanel.Create(nil);
          FImagePanel.Parent := FObjectInspector;
          FImagePanel.BevelOuter := bvNone;
          FImagePanel.Height := FObjectInspector.Header.Columns.Items[0].Width - 2;
          FImagePanel.Width := FImagePanel.Height;
          FImagePanel.Top := FEditor.BoundsRect.Top;
          FImagePanel.Left := 1;
          LImagePanelShape := TShape.Create(FImagePanel);
          LImagePanelShape.Parent := FImagePanel;
          LImagePanelShape.Align := alClient;
          LImage := TImage.Create(FImagePanel);
          LImage.AlignWithMargins := True;
          LImage.Parent := FImagePanel;
          LImage.Align := alClient;
          LImage.Margins.Left := 2;
          LImage.Margins.Top := 2;
          LImage.Canvas.StretchDraw(LImage.BoundsRect, LBitmap);
        end;
      end;
    1:
      begin
        if Assigned(FImagePanel) then
          FreeImagePanel;

        LOpenPictureDialog := TsOpenPictureDialog.Create(FObjectInspector);
        try
          if LOpenPictureDialog.Execute then
            LBitmap.LoadFromFile(LOpenPictureDialog.FileName);
        finally
          LOpenPictureDialog.Free;
        end;
      end;
    2:
      begin
        LSavePictureDialog := TsSavePictureDialog.Create(FObjectInspector);
        try
          if LSavePictureDialog.Execute then
            LBitmap.SaveToFile(LSavePictureDialog.FileName);
        finally
          LSavePictureDialog.Free;
        end;
      end
  end;
end;

procedure TBCObjectInspectorEditLink.DoComboDblClick(Sender: TObject);
var
  LIndex: Integer;
  LEditor: TsComboBox;
begin
  LEditor := FEditor as TsComboBox;
  LIndex := LEditor.ItemIndex;
  if LIndex = -1 then
    LIndex := LEditor.Items.IndexOf(LEditor.Text);
  Inc(LIndex);
  if LIndex > LEditor.GetCount - 1 then
    LIndex := 0;
  LEditor.ItemIndex := LIndex;
  SetValue;
end;

function TBCObjectInspectorEditLink.BeginEdit: Boolean;
begin
  Result := True;
  if Assigned(FEditor) then
  begin
    FEditor.Show;
    FEditor.SetFocus;
  end;
end;

function TBCObjectInspectorEditLink.CancelEdit: Boolean;
begin
  Result := True;
  if Assigned(FEditor) then
    FEditor.Hide;
end;

function TBCObjectInspectorEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  LData: PBCObjectInspectorNodeRecord;

  procedure CreateEdit;
  begin
    FEditor := TsEdit.Create(nil);
    with FEditor as TsEdit do
    begin
      Visible := False;
      Parent := Tree;
      Font.Assign(FObjectInspector.Canvas.Font);
      OnKeyPress := EditKeyPress;
      Text := LData.PropertyValue;
    end;
  end;

  procedure CreateEnumerationComboBox;
  var
    LIndex: Integer;
    LTypeData: PTypeData;
  begin
    FEditor := TsComboBox.Create(nil);
    with FEditor as TsComboBox do
    begin
      Visible := False;
      Parent := Tree;
      Font.Assign(FObjectInspector.Canvas.Font);
      Text := LData.PropertyValue;
      LTypeData := GetTypeData(LData.TypeInfo);
      OnSelect := DoComboSelect;
      OnDblClick := DoComboDblClick;

      for LIndex := LTypeData.MinValue to LTypeData.MaxValue do
        Items.Add(GetEnumName(LData.TypeInfo, LIndex));
    end;
  end;

  procedure CreateColorComboBox;
  begin
    FEditor := TsColorBox.Create(nil);
    with FEditor as TsColorBox do
    begin
      Visible := False;
      ColorRectWidth := 14;
      Parent := Tree;
      Font.Assign(FObjectInspector.Canvas.Font);
      Style := Style + [cbCustomColor];
      OnSelect := DoComboSelect;
      Selected := StringToColor(LData.PropertyValue);
    end;
  end;

  procedure CreateShortCutComboBox;
  var
    LIndex: Integer;
  begin
    FEditor := TsComboBox.Create(nil);
    with FEditor as TsComboBox do
    begin
      Visible := False;
      Parent := Tree;
      Font.Assign(FObjectInspector.Canvas.Font);
      Text := ShortCutToText(StrToInt(LData.PropertyValue));
      OnSelect := DoComboSelect;
      OnDblClick := DoComboDblClick;

      for LIndex := 1 to High(ShortCuts) do
        Items.Add(ShortCutToText(ShortCuts[LIndex]));
    end;
  end;

  procedure CreateBitmapButtons;
  var
    LButton: TsSpeedButton;

    procedure CreateBitmapButton(const ATag: Integer; const AAlign: TAlign; const ACaption: string; const AResourceName: string);
    begin
      LButton := TsSpeedButton.Create(FEditor);
      with LButton do
      begin
        Tag := ATag;
        Flat := True;
        Align := AAlign;
        Parent := FEditor;
        Font.Assign(FObjectInspector.Canvas.Font);
        OnClick := DoBitmapButtonClick;
        Caption := ACaption;
        Width := (Tree.Header.Columns.Items[Column].Width div 2) - 2;
        SkinData.SkinSection := 'TOOLBUTTON';
        Glyph.LoadFromResourceName(hInstance, AResourceName);
        Width := 50;
      end;
    end;

  begin
    FEditor := TsPanel.Create(nil);
    with FEditor as TsPanel do
    begin
      Parent := Tree;
      BevelOuter := bvNone;
      Visible := False;
    end;

    CreateBitmapButton(2, alLeft, 'Save', 'BITMAPSAVE');
    CreateBitmapButton(1, alLeft, 'Load', 'BITMAPOPEN');
    CreateBitmapButton(0, alLeft, 'View', 'BITMAPVIEW');
  end;

begin
  Result := True;

  FObjectInspector := Tree as TBCObjectInspector;
  FNode := Node;
  FColumn := Column;

  if Assigned(FEditor) then
  begin
    FEditor.Free;
    FEditor := nil;
  end;

  LData := FObjectInspector.GetNodeData(Node);

  if LData.TypeInfo = System.TypeInfo(TColor) then
    CreateColorComboBox
  else
  if LData.TypeInfo = System.TypeInfo(TCursor) then
    CreateEdit // TODO: TCursor combobox
  else
  if LData.TypeInfo = System.TypeInfo(TShortCut) then
    CreateShortCutComboBox
  else
  if LData.TypeInfo.Name = TYPE_BITMAP then
    CreateBitmapButtons
  else
  case LData.TypeInfo.Kind of
    tkInteger, tkInt64, tkChar, tkFloat, tkString, tkLString, tkWString, tkUString:
      CreateEdit;
    tkEnumeration:
      CreateEnumerationComboBox;
  end;
end;

procedure TBCObjectInspectorEditLink.DoComboSelect(Sender: TObject);
begin
  FObjectInspector.EndEditNode;
end;

procedure TBCObjectInspectorEditLink.SetValue;
begin
  if not Assigned(FEditor) then
    Exit;

  if FEditor is TsEdit then
    FObjectInspector.SetValueAsString(FNode, (FEditor as TsEdit).Text)
  else
  if FEditor is TsComboBox then
    FObjectInspector.SetValueAsString(FNode, (FEditor as TsComboBox).Text)
  else
  if FEditor is TsColorBox then
    FObjectInspector.SetValueAsString(FNode, ColorToString((FEditor as TsColorBox).Selected));
end;

function TBCObjectInspectorEditLink.EndEdit: Boolean;
begin
  SetValue;
  FEditor.Hide;

  Result := True;
end;

function TBCObjectInspectorEditLink.GetBounds: TRect;
begin
  if Assigned(FEditor) then
  begin
    Result := FEditor.BoundsRect;
    if FEditor is TsPanel then
      Result.Height := FObjectInspector.NodeHeight[FNode];
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TBCObjectInspectorEditLink.ProcessMessage(var AMessage: TMessage);
begin
  if Assigned(FEditor) then
    FEditor.WindowProc(AMessage);
end;

procedure TBCObjectInspectorEditLink.SetBounds(ARect: TRect);
var
  LLeft: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  FObjectInspector.Header.Columns.GetColumnBounds(FColumn, LLeft, ARect.Right);
  if Assigned(FEditor) then
    FEditor.BoundsRect := ARect;
end;

end.
