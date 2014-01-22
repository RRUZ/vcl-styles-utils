//**************************************************************************************************
//
// Unit Vcl.Styles.InnoSetup.StyleHooks
// unit for the VCL Styles Utils
// http://code.google.com/p/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is  Vcl.Styles.InnoSetup.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
//
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Vcl.Styles.InnoSetup.StyleHooks;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Styles.Utils.Forms,
  Vcl.Styles.Utils.SysStyleHook,
  Vcl.Styles.Utils.StdCtrls,
  Vcl.Styles.Utils.ComCtrls,
  Vcl.GraphUtil,
  Vcl.Controls;

type
  TRichEditViewerStyleHook = class(TSysScrollingStyleHook)
  protected
    function GetBorderSize: TRect; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

  TNewCheckListBoxStyleHook = class(TSysScrollingStyleHook)
  protected
    function GetBorderSize: TRect; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

  TNewButtonStyleHook = class(TSysButtonStyleHook)
  protected
    function CheckIfParentBkGndPainted: Boolean; override;
  end;

  TNewMemoStyleHook = class(TSysMemoStyleHook)
  public
    constructor Create(AHandle: THandle); override;
  end;

  TWizardFormStyleHook = class(TSysDialogStyleHook)
  public
    constructor Create(AHandle: THandle); override;
  end;

  TNewListBoxStyleHook = class(TSysListBoxStyleHook)
  public
    constructor Create(AHandle: THandle); override;
  end;

  TFolderTreeViewStyleHook = class(TSysTreeViewStyleHook)
  public
    constructor Create(AHandle: THandle); override;
  end;

implementation

{ TRichEditViewerStyleHook }

constructor TRichEditViewerStyleHook.Create(AHandle: THandle);
begin
  inherited;
  HookedDirectly := True;
  OverridePaint := False;
  OverridePaintNC := True;
  OverrideFont := False;
end;

destructor TRichEditViewerStyleHook.Destroy;
begin

  inherited;
end;

function TRichEditViewerStyleHook.GetBorderSize: TRect;
begin
  Result := inherited GetBorderSize;
  if (SysControl.HasBorder) then
    begin
      Result := Rect(2, 2, 2, 2);
    end;
end;

procedure TRichEditViewerStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

{ TNewCheckListBoxStyleHook }

constructor TNewCheckListBoxStyleHook.Create(AHandle: THandle);
begin
  inherited;
  HookedDirectly:=True;
  OverridePaint := False;
  OverridePaintNC := True;
  OverrideFont := False;
end;

destructor TNewCheckListBoxStyleHook.Destroy;
begin

  inherited;
end;

function TNewCheckListBoxStyleHook.GetBorderSize: TRect;
begin
  Result := inherited GetBorderSize;
  if (SysControl.HasBorder) then
    begin
      Result := Rect(2, 2, 2, 2);
    end;
end;

procedure TNewCheckListBoxStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

{ TNewButtonStyleHook }

function TNewButtonStyleHook.CheckIfParentBkGndPainted: Boolean;
begin
  Result := True;
end;

{ TNewMemo }

constructor TNewMemoStyleHook.Create(AHandle: THandle);
begin
  inherited;
  HookedDirectly := True;
end;

{ TSetupForm }

constructor TWizardFormStyleHook.Create(AHandle: THandle);
begin
  inherited;
  HookedDirectly := True;
end;

{ TNewListBox }

constructor TNewListBoxStyleHook.Create(AHandle: THandle);
begin
  inherited;
  HookedDirectly := True;
end;

{ TFolderTreeView }

constructor TFolderTreeViewStyleHook.Create(AHandle: THandle);
begin
  inherited;
  HookedDirectly := True;
end;

end.
