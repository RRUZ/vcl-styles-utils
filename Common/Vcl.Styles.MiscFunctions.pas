unit Vcl.Styles.MiscFunctions;

interface

var
  GlobalMainThreadID : TThreadID = 0;

function ExecutingInMainThread : boolean;

implementation

uses
  WinApi.Windows;

function ExecutingInMainThread : boolean;
begin
  Result := (GlobalMainThreadID = 0) or (GlobalMainThreadID = GetCurrentThreadID);
end;

end.
