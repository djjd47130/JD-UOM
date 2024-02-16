unit JD.Uom.Common;

interface

uses
  Winapi.Windows, System.SysUtils
  //, Vcl.Graphics
  ;

const
  PartOfNumber = ['0'..'9', '.', ','];
  NumFormat = '#,###,###,###,##0.#############';

type
  TUOMSystem = (ustAny, ustMetric, ustUSCustomary, ustImperial);
  TUOMSystems = set of TUOMSystem;


implementation

{
function FormatToWidth(const AValue: Double; const AWidth: Integer;
  const ADC: HDC; const ASuffix: String = ''; const APrefix: String = ''): String;
var
  C: TCanvas;
  //VW, PW, SW: Integer;
begin
  C:= TCanvas.Create;
  try
    C.Handle:= ADC;

    //PW:= C.TextWidth(APrefix);
    //SW:= C.TextWidth(ASuffix);
    //VW:= AWidth - PW - SW;

    Result:= APrefix + FormatFloat(NumFormat, AValue) + ASuffix;
    if C.TextWidth(Result) > AWidth then begin
      Result:= FloatToStrF(AValue, ffExponent, 8, 4);



    end;

  finally
    C.Free;
  end;
end;

}


end.
