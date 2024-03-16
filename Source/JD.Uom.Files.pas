unit JD.Uom.Files;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.IniFiles,
  JD.Uom;

type
  TUOMFile = class;
  TUOMFileItem = class;

  /// <summary>
  /// Specifies the type of UOM entry - how it shall be registered.
  /// </summary>
  TUOMFileItemType = (utMetric, utSimple, utFormula);

  /// <summary>
  /// (NOT READY)
  /// Encapsulates the contents of an INI file to load/save UOMs dynamically.
  /// https://github.com/djjd47130/JD-UOM/issues/59
  /// </summary>
  TUOMFile = class(TComponent)
  private
    FFilename: String;
    FItems: TObjectList<TUOMFileItem>;
    procedure SetFilename(const Value: String);
    function GetItem(const Index: Integer): TUOMFileItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Filename: String read FFilename write SetFilename;
    function Load: TUOMFile;
    function Save: TUOMFile;
    function RegisterAllUOMs: TUOMFile;
    function LoadFromFile(const AFilename: String): TUOMFile;
    function Count: Integer;
    function Add: TUOMFileItem;
    property Items[const Index: Integer]: TUOMFileItem read GetItem; default;
  end;

  /// <summary>
  /// (NOT READY)
  /// Encapsulates a single UOM entry within INI file.
  /// Could be 1 of multiple types, as defined by TUOMFileItemType.
  /// Properties depend on that specified item type.
  /// </summary>
  TUOMFileItem = class(TPersistent)
  private
    FOwner: TUOMFile;
    FItemType: TUOMFileItemType;
    FNameSingular: String;
    FNamePlural: String;
    FSuffix: String;
    FCategory: String;
    FSystems: String;
    FFactor: Double;
    FFromBase: String;
    FToBase: String;
    FIsBase: Boolean;
    FInclude: String;
    FBaseUOM: String;
    procedure SetItemType(const Value: TUOMFileItemType);
    procedure SetCategory(const Value: String);
    procedure SetFactor(const Value: Double);
    procedure SetFromBase(const Value: String);
    procedure SetIsBase(const Value: Boolean);
    procedure SetNamePlural(const Value: String);
    procedure SetNameSingular(const Value: String);
    procedure SetSuffix(const Value: String);
    procedure SetSystems(const Value: String);
    procedure SetToBase(const Value: String);
    procedure SetInclude(const Value: String);
    procedure SetBaseUOM(const Value: String);
  public
    constructor Create(AOwner: TUOMFile);
    destructor Destroy; override;
    procedure RegisterUOM;
    procedure UnregisterUOM;
    function GetRegisteredUOM: TUOM;
  published
    property ItemType: TUOMFileItemType read FItemType write SetItemType;
    property NameSingular: String read FNameSingular write SetNameSingular;
    property NamePlural: String read FNamePlural write SetNamePlural;
    property Suffix: String read FSuffix write SetSuffix;
    property Category: String read FCategory write SetCategory;
    property Systems: String read FSystems write SetSystems;
    property Factor: Double read FFactor write SetFactor;
    property FromBase: String read FFromBase write SetFromBase;
    property ToBase: String read FToBase write SetToBase;
    property IsBase: Boolean read FIsBase write SetIsBase;
    property Include: String read FInclude write SetInclude;
    property BaseUOM: String read FBaseUOM write SetBaseUOM;
  end;

function StringToMetricUnit(const Value: string): TUOMMetricUnit;
function StringToMetricUnits(const Value: String): TUOMMetricUnits;
function MetricUnitToString(const Value: TUOMMetricUnit): String;
function MetricUnitsToString(const Value: TUOMMetricUnits): String;

implementation

uses
  System.IOUtils,
  System.TypInfo;

function StringToMetricUnit(const Value: string): TUOMMetricUnit;
var
  EnumValue: Integer;
begin
  EnumValue := GetEnumValue(TypeInfo(TUOMMetricUnit), Value);
  Result := TUOMMetricUnit(EnumValue);
end;

function StringToMetricUnits(const Value: String): TUOMMetricUnits;
var
  L: TStringList;
  X: Integer;
  T: String;
begin
  Result:= [];
  L:= TStringList.Create;
  try
    L.Delimiter:= ',';
    L.DelimitedText:= Value;
    for X := 0 to L.Count-1 do begin
      T:= L[X];
      Result:= Result + [StringToMetricUnit(T)];
    end;
  finally
    L.Free;
  end;
end;

function MetricUnitToString(const Value: TUOMMetricUnit): String;
begin
  Result := GetEnumName(TypeInfo(TUOMMetricUnit), Ord(Value));
end;

function MetricUnitsToString(const Value: TUOMMetricUnits): String;
var
  V: TUOMMetricUnit;
begin
  Result:= '';
  for V := Low(TUOMMetricUnit) to High(TUOMMetricUnit) do begin
    if V in Value then begin
      if Result <> '' then Result:= Result + ',';
      Result:= Result + MetricUnitToString(V);
    end;
  end;
end;

{ TUOMFile }

constructor TUOMFile.Create(AOwner: TComponent);
begin
  inherited;
  FItems:= TObjectList<TUOMFileItem>.Create(True);
end;

destructor TUOMFile.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TUOMFile.Add: TUOMFileItem;
begin
  Result:= TUOMFileItem.Create(Self);
  FItems.Add(Result);
end;

function TUOMFile.Count: Integer;
begin
  Result:= FItems.Count;
end;

function TUOMFile.GetItem(const Index: Integer): TUOMFileItem;
begin
  Result:= FItems[Index];
end;

procedure TUOMFile.SetFilename(const Value: String);
begin
  FFilename := Value;
end;

function TUOMFile.RegisterAllUOMs: TUOMFile;
var
  X: Integer;
begin
  for X := 0 to FItems.Count-1 do begin
    FItems[X].RegisterUOM;
  end;
  Result:= Self;
end;

function TUOMFile.Load: TUOMFile;
var
  I: TIniFile;
  Sections: TStringList;
  X: Integer;
  procedure DoLoadUOM;
  var
    NS, NP, Suf, Cat, Sys: String;
    IsBase: Boolean;
    FromBase, ToBase: String;
    Factor: Double;
    MUS: String;
    MB: String;
    procedure AddUOM(const AType: TUOMFileItemType);
    var
      Itm: TUOMFileItem;
    begin
      Itm:= TUOMFileItem.Create(Self);
      try
        Itm.FItemType:= AType;
        Itm.FNameSingular:= NS;
        Itm.FNamePlural:= NP;
        Itm.FSuffix:= Suf;
        Itm.FCategory:= Cat;
        case AType of
          utMetric: begin
            Itm.FInclude:= MUS;
            Itm.FBaseUOM:= MB;
            Itm.FSystems:= 'Metric';
          end;
          utSimple: begin
            Itm.FSystems:= Sys;
            Itm.FFactor:= Factor;
            Itm.FIsBase:= IsBase;
          end;
          utFormula: begin
            Itm.FSystems:= Sys;
            Itm.FFromBase:= FromBase;
            Itm.FToBase:= ToBase;
            Itm.FIsBase:= IsBase;
          end;
        end;
      finally
        FItems.Add(Itm);
      end;
    end;
  begin
    NS:= Sections[X];
    NP:= I.ReadString(NS, 'Plural', '');
    Suf:= I.ReadString(NS, 'Suffix', '');
    Cat:= I.ReadString(NS, 'Category', '');
    Sys:= I.ReadString(NS, 'Systems', '');
    MUS:= I.ReadString(NS, 'Include', '');
    MB:= I.ReadString(NS, 'BaseUOM', '');
    if Cat = '' then
      raise EUOMException.Create('Category must be assigned!');
    if Suf = '' then
      raise EUOMException.Create('Suffix must be assigned!');
    //Metric standard UOM
    if Pos('Metric:', NS) = 1 then begin
      Delete(NS, 1, 7);
      if MUS = '' then
        raise EUOMException.Create('Metric UOMs must specificy units!');
      AddUOM(utMetric);
    end else begin
      FromBase:= I.ReadString(NS, 'FromBase', '');
      ToBase:= I.ReadString(NS, 'ToBase', '');
      Factor:= I.ReadFloat(NS, 'Factor', 0);
      IsBase:= I.ReadBool(NS, 'IsBase', False);
      if NP = '' then
        NP:= NS + 's';
      if Sys = '' then
        raise EUOMException.Create('Systems must be assigned!');
      if Factor <> 0 then begin
        //Simple UOM using factor...
        AddUOM(utSimple);
      end else begin
        //Advanced UOM using formula...
        if FromBase = '' then
          raise EUOMException.Create('Convert from base formula cannot be blank!');
        if ToBase = '' then
          raise EUOMException.Create('Convert to base formula cannot be blank!');
        AddUOM(utFormula);
      end;
    end;
  end;
begin
  FItems.Clear;
  I:= TIniFile.Create(FFilename);
  try
    Sections:= TStringList.Create;
    try
      I.ReadSections(Sections);
      for X := 0 to Sections.Count-1 do begin
        DoLoadUOM;
      end;
    finally
      Sections.Free;
    end;
  finally
    I.Free;
  end;
  Result:= Self;
end;

function TUOMFile.LoadFromFile(const AFilename: String): TUOMFile;
begin
  FFilename:= AFilename;
  Result:= Load;
end;

function TUOMFile.Save: TUOMFile;
var
  I: TIniFile;
  L: TStringList;
  X: Integer;
  Itm: TUOMFileItem;
  S: String;
begin
  I:= TIniFile.Create(FFilename);
  try

    //First, erase everything in the file...
    L:= TStringList.Create;
    try
      I.ReadSections(L);
      for X := 0 to L.Count-1 do begin
        I.EraseSection(L[X]);
      end;
    finally
      L.Free;
    end;

    //Now, populate file with all items...
    for X := 0 to FItems.Count-1 do begin
      Itm:= FItems[X];
      if Itm.FItemType = utMetric then
        S:= 'Metric:'+Itm.FNameSingular
      else
        S:= Itm.FNameSingular;
      I.WriteString(S, 'Category', Itm.FCategory);
      I.WriteString(S, 'Suffix', Itm.FSuffix);
      case Itm.FItemType of
        utMetric: begin
          I.WriteString(S, 'Include', Itm.FInclude);
          I.WriteString(S, 'BaseUOM', Itm.FBaseUOM);
        end;
        utSimple: begin
          I.WriteString(S, 'Plural', Itm.FNamePlural);
          I.WriteString(S, 'Systems', Itm.FSystems);
          I.WriteBool(S, 'IsBase', Itm.FIsBase);
          I.WriteFloat(S, 'Factor', Itm.FFactor);
        end;
        utFormula: begin
          I.WriteString(S, 'Plural', Itm.FNamePlural);
          I.WriteString(S, 'Systems', Itm.FSystems);
          I.WriteBool(S, 'IsBase', Itm.FIsBase);
          I.WriteString(S, 'FromBase', Itm.FFromBase);
          I.WriteString(S, 'ToBase', Itm.FToBase);
        end;
      end;
    end;

  finally
    I.Free;
  end;
  Result:= Self;
end;

{ TUOMFileItem }

constructor TUOMFileItem.Create(AOwner: TUOMFile);
begin
  FOwner:= AOwner;

end;

destructor TUOMFileItem.Destroy;
begin

  inherited;
end;

function TUOMFileItem.GetRegisteredUOM: TUOM;
begin
  Result:= TUOMUtils.GetUOMByName(FNameSingular);
end;

procedure TUOMFileItem.RegisterUOM;
var
  MU: TUOMMetricUnits;
begin
  case FItemType of
    utMetric: begin
      MU:= StringToMetricUnits(FInclude);
      TUOMMetricUtils.ProduceUOMs(FCategory, FNameSingular, FSuffix, MU, FBaseUOM);
    end;
    utSimple: begin
      TUOMUtils.RegisterSimpleUOM(FCategory, FNameSingular, FNamePlural,
        FSuffix, FSystems, FFactor);
    end;
    utFormula: begin
      TUOMUtils.RegisterUOM(FCategory, FNameSingular, FNamePlural, FSuffix,
        FSystems, FFromBase, FToBase);
    end;
  end;
end;

procedure TUOMFileItem.UnregisterUOM;
begin
  TUOMUtils.UnregisterUOM(GetRegisteredUOM);
end;

procedure TUOMFileItem.SetBaseUOM(const Value: String);
begin
  FBaseUOM := Value;
end;

procedure TUOMFileItem.SetCategory(const Value: String);
begin
  FCategory := Value;
end;

procedure TUOMFileItem.SetFactor(const Value: Double);
begin
  FFactor := Value;
end;

procedure TUOMFileItem.SetFromBase(const Value: String);
begin
  FFromBase := Value;
end;

procedure TUOMFileItem.SetInclude(const Value: String);
begin
  FInclude := Value;
end;

procedure TUOMFileItem.SetIsBase(const Value: Boolean);
begin
  FIsBase := Value;
end;

procedure TUOMFileItem.SetItemType(const Value: TUOMFileItemType);
begin
  FItemType := Value;
end;

procedure TUOMFileItem.SetNamePlural(const Value: String);
begin
  FNamePlural := Value;
end;

procedure TUOMFileItem.SetNameSingular(const Value: String);
begin
  FNameSingular := Value;
end;

procedure TUOMFileItem.SetSuffix(const Value: String);
begin
  FSuffix := Value;
end;

procedure TUOMFileItem.SetSystems(const Value: String);
begin
  FSystems := Value;
end;

procedure TUOMFileItem.SetToBase(const Value: String);
begin
  FToBase := Value;
end;

end.
