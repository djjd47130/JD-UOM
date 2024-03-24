object dmDWS: TdmDWS
  OldCreateOrder = False
  Height = 166
  Width = 265
  object DWS: TDelphiWebScript
    Left = 72
    Top = 32
  end
  object JDUOM: TdwsUnit
    Script = DWS
    Enumerations = <
      item
        Name = 'TUOMType'
        Elements = <
          item
            Name = 'uomFactor'
          end
          item
            Name = 'uomFormula'
          end
          item
            Name = 'uomMetric'
          end
          item
            Name = 'uomCombo'
          end>
      end>
    Functions = <
      item
        Name = 'UOM'
        Parameters = <
          item
            Name = 'Expr'
            DataType = 'String'
          end>
        ResultType = 'Float'
        OnEval = JDUOMFunctionsUOMEval
      end
      item
        Name = 'RegisterSimpleUOM'
        Parameters = <
          item
            Name = 'Category'
            DataType = 'String'
          end
          item
            Name = 'NameSingular'
            DataType = 'String'
          end
          item
            Name = 'NamePlural'
            DataType = 'String'
          end
          item
            Name = 'Suffix'
            DataType = 'String'
          end
          item
            Name = 'Systems'
            DataType = 'String'
          end
          item
            Name = 'Factor'
            DataType = 'Float'
          end
          item
            Name = 'Aliases'
            DataType = 'String'
          end>
        ResultType = 'TUOM'
        OnEval = JDUOMFunctionsRegisterSimpleUOMEval
      end
      item
        Name = 'RegisterUOM'
        Parameters = <
          item
            Name = 'Category'
            DataType = 'String'
          end
          item
            Name = 'NameSingular'
            DataType = 'String'
          end
          item
            Name = 'NamePlural'
            DataType = 'String'
          end
          item
            Name = 'Suffix'
            DataType = 'String'
          end
          item
            Name = 'Systems'
            DataType = 'String'
          end
          item
            Name = 'FromBase'
            DataType = 'String'
          end
          item
            Name = 'ToBase'
            DataType = 'String'
          end
          item
            Name = 'Aliases'
            DataType = 'String'
          end>
        ResultType = 'TUOM'
        OnEval = JDUOMFunctionsRegisterUOMEval
      end
      item
        Name = 'RegisterBaseUOM'
        Parameters = <
          item
            Name = 'Category'
            DataType = 'String'
          end
          item
            Name = 'UOM'
            DataType = 'String'
          end>
        OnEval = JDUOMFunctionsRegisterBaseUOMEval
      end
      item
        Name = 'FindUOM'
        Parameters = <
          item
            Name = 'UOMName'
            DataType = 'String'
          end>
        ResultType = 'TUOM'
        OnEval = JDUOMFunctionsFindUOMEval
      end
      item
        Name = 'Power'
        Parameters = <
          item
            Name = 'Base'
            DataType = 'Float'
          end
          item
            Name = 'Exponent'
            DataType = 'Float'
          end>
        ResultType = 'Float'
        OnEval = JDUOMFunctionsPowerEval
      end
      item
        Name = 'Convert'
        Parameters = <
          item
            Name = 'Value'
            DataType = 'Float'
          end
          item
            Name = 'FromUOM'
            DataType = 'String'
          end
          item
            Name = 'ToUOM'
            DataType = 'String'
          end>
        ResultType = 'Float'
        OnEval = JDUOMFunctionsConvertEval
      end
      item
        Name = 'UOMString'
        Parameters = <
          item
            Name = 'Value'
            DataType = 'Float'
          end
          item
            Name = 'UOM'
            DataType = 'String'
          end
          item
            Name = 'Shorten'
            DataType = 'Boolean'
            HasDefaultValue = True
            DefaultValue = 'False'
          end>
        ResultType = 'String'
        OnEval = JDUOMFunctionsUOMStringEval
      end
      item
        Name = 'Sqr'
        Parameters = <
          item
            Name = 'Value'
            DataType = 'Float'
          end>
        ResultType = 'Float'
        OnEval = JDUOMFunctionsSqrEval
      end
      item
        Name = 'Cube'
        Parameters = <
          item
            Name = 'Value'
            DataType = 'Float'
          end>
        ResultType = 'Float'
        OnEval = JDUOMFunctionsCubeEval
      end
      item
        Name = 'BaseUOM'
        Parameters = <
          item
            Name = 'Category'
            DataType = 'String'
          end>
        ResultType = 'TUOM'
        OnEval = JDUOMFunctionsBaseUOMEval
      end>
    Records = <
      item
        Name = 'TUOM'
        Members = <
          item
            Name = 'UOMType'
            DataType = 'TUOMType'
            ReadOnly = False
          end
          item
            Name = 'ParentUOM'
            DataType = 'String'
            ReadOnly = False
          end
          item
            Name = 'Category'
            DataType = 'String'
            ReadOnly = False
          end
          item
            Name = 'NameSingular'
            DataType = 'String'
            ReadOnly = False
          end
          item
            Name = 'NamePlural'
            DataType = 'String'
            ReadOnly = False
          end
          item
            Name = 'Suffix'
            DataType = 'String'
            ReadOnly = False
          end
          item
            Name = 'Systems'
            DataType = 'String'
            ReadOnly = False
          end
          item
            Name = 'Factor'
            DataType = 'Float'
            ReadOnly = False
          end
          item
            Name = 'FromBase'
            DataType = 'String'
            ReadOnly = False
          end
          item
            Name = 'ToBase'
            DataType = 'String'
            ReadOnly = False
          end
          item
            Name = 'Aliases'
            DataType = 'String'
            ReadOnly = False
          end>
        Properties = <>
      end>
    UnitName = 'JD.UOM'
    StaticSymbols = False
    Left = 119
    Top = 32
  end
end
