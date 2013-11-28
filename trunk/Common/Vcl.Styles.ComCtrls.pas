{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.ComCtrls                                                                         }
{ unit for the VCL Styles Utils                                                                    }
{ http://code.google.com/p/vcl-styles-utils/                                                       }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is Vcl.Styles.ComCtrls.pas.                                                    }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.ComCtrls;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Themes,
  System.Types,
  Vcl.Styles.ControlWnd;

type
  TProgressBarOrientation = (pbHorizontal, pbVertical);

  TProgressBarWnd = class(TControlWnd)
  private
    FOrientation: TProgressBarOrientation;
    function GetBarRect: TRect;
    function GetBorderWidth: Integer;
    function GetPercent: Single;
    function GetMax: Integer;
    function GetMin: Integer;
    function GetPosition: Integer;
  protected
    procedure WndProc(var Message: TMessage); override;
    property Orientation: TProgressBarOrientation read FOrientation default pbHorizontal;
    property BarRect: TRect read GetBarRect;
    property BorderWidth: Integer read GetBorderWidth;
    property Max: Integer read GetMax;
    property Min: Integer read GetMin;
    property Position: Integer read GetPosition;
  public
    constructor Create(AHandle: THandle); override;
  end;

implementation

uses
  Vcl.Graphics,
  System.SysUtils,
  //System.IOUtils,
  Winapi.CommCtrl;

{ TEditWnd }

constructor TProgressBarWnd.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
  if (GetWindowLong(AHandle, GWL_STYLE) And PBS_VERTICAL)<>0 then
    FOrientation:=pbVertical
  else
    FOrientation:=pbHorizontal;
end;

function TProgressBarWnd.GetBarRect: TRect;
begin
  Result := TRect.Create(0, 0, Width, Height);
  InflateRect(Result, -BorderWidth, -BorderWidth);
end;

function TProgressBarWnd.GetBorderWidth: Integer;
begin
 Result:=0;
end;

function TProgressBarWnd.GetMax: Integer;
begin
  Result := SendMessage(Handle, PBM_GetRange, 0, 0);
end;

function TProgressBarWnd.GetMin: Integer;
begin
  Result := SendMessage(Handle, PBM_GetRange, 1, 0);
end;

function TProgressBarWnd.GetPercent: Single;
var
  LMin, LMax, LPos: Integer;
begin
  LMin := Min;
  LMax := Max;
  LPos := Position;
  if (LMin >= 0) and (LPos >= LMin) and (LMax >= LPos) and (LMax - LMin <> 0) then
    Result := (LPos - LMin) / (LMax - LMin)
  else
    Result := 0;
end;

function TProgressBarWnd.GetPosition: Integer;
begin
  Result := SendMessage(Handle, PBM_GETPOS, 0, 0);
end;

   {
procedure Addlog(const msg : string);
begin
   TFile.AppendAllText('C:\Dephi\google-code\vcl-styles-utils\log.txt',msg+sLineBreak);
end;
   }
procedure TProgressBarWnd.WndProc(var Message: TMessage);
var
  DC: HDC;
  LDetails: TThemedElementDetails;
  W, LPos : Integer;
  LRect, FillR  : TRect;
  LCanvas: TCanvas;
  lpPaint: TPaintStruct;
begin


  case Message.Msg of

    WM_PAINT:
      begin
        DC := HDC(Message.WParam);
        LCanvas := TCanvas.Create;
        try
          if DC <> 0 then
            LCanvas.Handle := DC
          else
            LCanvas.Handle := BeginPaint(Handle, lpPaint);


          //Frame
          LRect := BarRect;
          if Orientation = pbHorizontal then
            LDetails := StyleServices.GetElementDetails(tpBar)
          else
            LDetails := StyleServices.GetElementDetails(tpBarVert);

          //Addlog(Format('Frame R  Width %d Height %d',[R.Width, R.Height]));

          StyleServices.DrawElement(LCanvas.Handle, LDetails, LRect);

          //Bar
          InflateRect(LRect, -1, -1);
          if Orientation = pbHorizontal then
            W := LRect.Width
          else
            W := LRect.Height;

          //Addlog(Format('GetPosition %d',[GetPosition]));
          //Addlog(Format('GetPercent %n',[GetPercent]));

          LPos := Round(W * GetPercent);

          //Addlog(Format('Pos %d',[Pos]));

          FillR := LRect;
          if Orientation = pbHorizontal then
          begin
            FillR.Right := FillR.Left + LPos;
            LDetails := StyleServices.GetElementDetails(tpChunk);
          end
          else
          begin
            FillR.Top := FillR.Bottom - LPos;
            LDetails := StyleServices.GetElementDetails(tpChunkVert);
          end;

          StyleServices.DrawElement(LCanvas.Handle, LDetails, FillR);


          if DC = 0 then
            EndPaint(Handle, lpPaint);
        finally
          LCanvas.Handle := 0;
          LCanvas.Free;
        end;
      end;

    WM_ERASEBKGND:
      begin
        Message.Result := 1;
      end;
  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

end.
