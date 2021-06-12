unit Common;

interface

uses
  Classes;

type
  TChecksum = class(TObject)
  public
    checksum: string;
  end;

  TCheckSumFile = class(TObject)
  public
    constructor Create(AFileName: string); virtual; abstract;
    procedure ToStringList(slOut: TStringList); virtual; abstract;
    function SingleFileChecksum(AFileName: string): string; virtual; abstract;
    function MergeLine(AFileName, ACheckSum: string): string; virtual; abstract;
  end;

implementation

end.
