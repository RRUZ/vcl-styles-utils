unit uRegisterVisualStyles;

interface

procedure Register;

implementation

uses
  System.Classes, uVisualStylePreview;

procedure Register;
begin
  RegisterComponents('VisualStyles', [TVisualStylePreview]);
end;

end.
