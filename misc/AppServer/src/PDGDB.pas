unit PDGDB;

interface
uses superobject;

type
  IPDGConnectionPool = interface;
  IPDGConnection = interface;
  IPDGContext = interface;
  IPDGCommand = interface;

  IPDGConnectionPool = interface(ISuperObject)
    ['{27621D9A-AAE9-4E24-82F5-A18D84E415F3}']
    function GetConnection: IPDGConnection;
  end;

  IPDGConnection = interface(ISuperObject)
  ['{843E105A-B8E0-42A9-AFA0-CF5AA843DB8B}']
    function newContext(Options: ISuperObject = nil): IPDGContext; overload;
    function newCommand(Options: ISuperObject = nil): IPDGCommand; overload;
    function newContext(const Options: string): IPDGContext; overload;
    function newCommand(const Options: string): IPDGCommand; overload;
  end;

  IPDGContext = interface(ISuperObject)
  ['{51992399-2D1A-47EF-9DB1-C5654325F41B}']
    function newCommand(Options: ISuperObject = nil): IPDGCommand; overload;
    function newCommand(const Options: string): IPDGCommand; overload;
    function Execute(Command: IPDGCommand; params: ISuperObject = nil): ISuperObject; overload;
    function Execute(Command: IPDGCommand; params: array of const): ISuperObject; overload;
    function Execute(Command: IPDGCommand; const params: string): ISuperObject; overload;
    function Execute(Command: IPDGCommand; const params: Variant): ISuperObject; overload;
  end;

  IPDGCommand = interface(ISuperObject)
  ['{A39B974A-96EA-4047-A57B-A2B3EBE7BABD}']
    function Execute(params: ISuperObject = nil; context: IPDGContext = nil): ISuperObject; overload;
    function Execute(params: array of const; context: IPDGContext = nil): ISuperObject; overload;
    function Execute(const params: string; context: IPDGContext = nil): ISuperObject; overload;
    function Execute(const params: Variant; context: IPDGContext = nil): ISuperObject; overload;
  end;

implementation

end.
