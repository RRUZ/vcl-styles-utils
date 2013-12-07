{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.InnoSetup                                                                        }
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
{ The Original Code is  Vcl.Styles.InnoSetup.pas.                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.InnoSetup;

interface

Procedure  Done;

implementation

{
  TNewEdit = class(TEdit)
  TNewMemo = class(TMemo)
  TNewComboBox = class(TComboBox)    ok
  TNewListBox = class(TListBox)
  TNewButton = class(TButton)        ok
  TNewCheckBox = class(TCheckBox)
  TNewRadioButton = class(TRadioButton)
}

uses
  KOLDetours,
  Winapi.Windows,
  Winapi.UxTheme,
  Winapi.Messages,
  System.Generics.Collections,
  System.SysUtils,
  {$IFDEF DEBUG}
  System.IOUtils,
  {$ENDIF}
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Themes,
  Vcl.Graphics,
  Vcl.Styles.Form,
  Vcl.Styles.StdCtrls,
  Vcl.Styles.ExtCtrls,
  Vcl.Styles.ComCtrls,

  Vcl.Styles.EditWnd,
  Vcl.Styles.StaticWnd,
  Vcl.Styles.ComboBoxWnd,
  Vcl.Styles.ButtonWnd;

type
  TThemedInnoControls = class
  private
  class var
    FHook_WH_CALLWNDPROC: HHook;
  protected
    class function HookActionCallBackWndProc(nCode: Integer; wParam: wParam;
      lParam: lParam): LRESULT; stdcall; static;
    procedure InstallHook;
    procedure RemoveHook;
  public
    constructor Create; overload;
    destructor Destroy; override;
  end;


var
  StaticTextWndList : TObjectDictionary<HWND, TStaticTextWnd>;
  EditWndList: TObjectDictionary<HWND, TEditTextWnd>;
  ComboBoxWndList: TObjectDictionary<HWND, TComboBoxWnd>;
  CheckBoxWndList: TObjectDictionary<HWND, TCheckBoxTextWnd>;
  BtnWndArrayList : TObjectDictionary<HWND, TButtonWnd>;
  PanelWndList : TObjectDictionary<HWND, TPanelWnd>;
  NotebookPageWndList : TObjectDictionary<HWND, TNotebookPagelWnd>;
  NotebookWndList : TObjectDictionary<HWND, TNotebookWnd>;
  FormWndArrayList : TObjectDictionary<HWND, TFormWnd>;
  ProgressBarWndArrayList : TObjectDictionary<HWND, TProgressBarWnd>;
  ClassesList : TDictionary<HWND, string>;

  ThemedInnoControls: TThemedInnoControls;
  GetSysColorOrgPointer: Pointer = nil;
  var TrampolineGetSysColor : function (nIndex: Integer): DWORD; stdcall;

{$IFDEF DEBUG}
procedure Addlog(const msg : string);
begin
   TFile.AppendAllText('C:\Dephi\google-code\vcl-styles-utils\log.txt',Format('%s %s %s',[FormatDateTime('hh:nn:ss.zzz', Now),  msg, sLineBreak]));
end;
{$ENDIF}

{ TThemedSysControls }

constructor TThemedInnoControls.Create;
begin
  inherited;
  FHook_WH_CALLWNDPROC := 0;
  InstallHook;

  StaticTextWndList := TObjectDictionary<HWND, TStaticTextWnd>.Create([doOwnsValues]);
  EditWndList:= TObjectDictionary<HWND, TEditTextWnd>.Create([doOwnsValues]);
  ComboBoxWndList:= TObjectDictionary<HWND, TComboBoxWnd>.Create([doOwnsValues]);
  CheckBoxWndList:= TObjectDictionary<HWND, TCheckBoxTextWnd>.Create([doOwnsValues]);
  BtnWndArrayList := TObjectDictionary<HWND, TButtonWnd>.Create([doOwnsValues]);
  PanelWndList := TObjectDictionary<HWND, TPanelWnd>.Create([doOwnsValues]);
  NotebookPageWndList := TObjectDictionary<HWND, TNotebookPagelWnd>.Create([doOwnsValues]);
  NotebookWndList := TObjectDictionary<HWND, TNotebookWnd>.Create([doOwnsValues]);
  FormWndArrayList := TObjectDictionary<HWND, TFormWnd>.Create([doOwnsValues]);
  ProgressBarWndArrayList  := TObjectDictionary<HWND, TProgressBarWnd>.Create([doOwnsValues]);
  ClassesList := TDictionary<HWND, string>.Create;
end;

destructor TThemedInnoControls.Destroy;
begin
  RemoveHook;

  StaticTextWndList.Free;
  EditWndList.Free;
  ComboBoxWndList.Free;
  CheckBoxWndList.Free;
  BtnWndArrayList.Free;
  PanelWndList.Free;
  ProgressBarWndArrayList.Free;
  FormWndArrayList.Free;
  ClassesList.Free;
  NotebookPageWndList.Free;
  NotebookWndList.Free;
  inherited;
end;

{$IFDEF DEBUG}
function WM_To_String(WM_Message: Integer): string;
begin
  case WM_Message of
    $0000: Result := 'WM_NULL';
    $0001: Result := 'WM_CREATE';
    $0002: Result := 'WM_DESTROY';
    $0003: Result := 'WM_MOVE';
    $0005: Result := 'WM_SIZE';
    $0006: Result := 'WM_ACTIVATE';
    $0007: Result := 'WM_SETFOCUS';
    $0008: Result := 'WM_KILLFOCUS';
    $000A: Result := 'WM_ENABLE';
    $000B: Result := 'WM_SETREDRAW';
    $000C: Result := 'WM_SETTEXT';
    $000D: Result := 'WM_GETTEXT';
    $000E: Result := 'WM_GETTEXTLENGTH';
    $000F: Result := 'WM_PAINT';
    $0010: Result := 'WM_CLOSE';
    $0011: Result := 'WM_QUERYENDSESSION';
    $0012: Result := 'WM_QUIT';
    $0013: Result := 'WM_QUERYOPEN';
    $0014: Result := 'WM_ERASEBKGND';
    $0015: Result := 'WM_SYSCOLORCHANGE';
    $0016: Result := 'WM_EndSESSION';
    $0017: Result := 'WM_SYSTEMERROR';
    $0018: Result := 'WM_SHOWWINDOW';
    $0019: Result := 'WM_CTLCOLOR';
    $001A: Result := 'WM_WININICHANGE or WM_SETTINGCHANGE';
    $001B: Result := 'WM_DEVMODECHANGE';
    $001C: Result := 'WM_ACTIVATEAPP';
    $001D: Result := 'WM_FONTCHANGE';
    $001E: Result := 'WM_TIMECHANGE';
    $001F: Result := 'WM_CANCELMODE';
    $0020: Result := 'WM_SETCURSOR';
    $0021: Result := 'WM_MOUSEACTIVATE';
    $0022: Result := 'WM_CHILDACTIVATE';
    $0023: Result := 'WM_QUEUESYNC';
    $0024: Result := 'WM_GETMINMAXINFO';
    $0026: Result := 'WM_PAINTICON';
    $0027: Result := 'WM_ICONERASEBKGND';
    $0028: Result := 'WM_NEXTDLGCTL';
    $002A: Result := 'WM_SPOOLERSTATUS';
    $002B: Result := 'WM_DRAWITEM';
    $002C: Result := 'WM_MEASUREITEM';
    $002D: Result := 'WM_DELETEITEM';
    $002E: Result := 'WM_VKEYTOITEM';
    $002F: Result := 'WM_CHARTOITEM';
    $0030: Result := 'WM_SETFONT';
    $0031: Result := 'WM_GETFONT';
    $0032: Result := 'WM_SETHOTKEY';
    $0033: Result := 'WM_GETHOTKEY';
    $0037: Result := 'WM_QUERYDRAGICON';
    $0039: Result := 'WM_COMPAREITEM';
    $003D: Result := 'WM_GETOBJECT';
    $0041: Result := 'WM_COMPACTING';
    $0044: Result := 'WM_COMMNOTIFY { obsolete in Win32}';
    $0046: Result := 'WM_WINDOWPOSCHANGING';
    $0047: Result := 'WM_WINDOWPOSCHANGED';
    $0048: Result := 'WM_POWER';
    $004A: Result := 'WM_COPYDATA';
    $004B: Result := 'WM_CANCELJOURNAL';
    $004E: Result := 'WM_NOTIFY';
    $0050: Result := 'WM_INPUTLANGCHANGEREQUEST';
    $0051: Result := 'WM_INPUTLANGCHANGE';
    $0052: Result := 'WM_TCARD';
    $0053: Result := 'WM_HELP';
    $0054: Result := 'WM_USERCHANGED';
    $0055: Result := 'WM_NOTIFYFORMAT';
    $007B: Result := 'WM_CONTEXTMENU';
    $007C: Result := 'WM_STYLECHANGING';
    $007D: Result := 'WM_STYLECHANGED';
    $007E: Result := 'WM_DISPLAYCHANGE';
    $007F: Result := 'WM_GETICON';
    $0080: Result := 'WM_SETICON';
    $0081: Result := 'WM_NCCREATE';
    $0082: Result := 'WM_NCDESTROY';
    $0083: Result := 'WM_NCCALCSIZE';
    $0084: Result := 'WM_NCHITTEST';
    $0085: Result := 'WM_NCPAINT';
    $0086: Result := 'WM_NCACTIVATE';
    $0087: Result := 'WM_GETDLGCODE';
    $0088: Result := 'WM_SYNCPAINT';
    $00A0: Result := 'WM_NCMOUSEMOVE';
    $00A1: Result := 'WM_NCLBUTTONDOWN';
    $00A2: Result := 'WM_NCLBUTTONUP';
    $00A3: Result := 'WM_NCLBUTTONDBLCLK';
    $00A4: Result := 'WM_NCRBUTTONDOWN';
    $00A5: Result := 'WM_NCRBUTTONUP';
    $00A6: Result := 'WM_NCRBUTTONDBLCLK';
    $00A7: Result := 'WM_NCMBUTTONDOWN';
    $00A8: Result := 'WM_NCMBUTTONUP';
    $00A9: Result := 'WM_NCMBUTTONDBLCLK';
    // edit control messages start (todo: add more if needed)
    $00B0: Result := 'EM_GETSEL';
    $00B1: Result := 'EM_SETSEL';
    $00B2: Result := 'EM_GETRECT';
    $00B3: Result := 'EM_SETRECT';
    $00B4: Result := 'EM_SETRECTNP';
    $00B5: Result := 'EM_SCROLL';
    $00B6: Result := 'EM_LINESCROLL';
    $00B7: Result := 'EM_SCROLLCARET';
    $00B8: Result := 'EM_GETMODIFY';
    $00B9: Result := 'EM_SETMODIFY';
    $00BA: Result := 'EM_GETLINECOUNT';
    $00BB: Result := 'EM_LINEINDEX';
    $00BC: Result := 'EM_SETHANDLE';
    $00BD: Result := 'EM_GETHANDLE';
    $00BE: Result := 'EM_GETTHUMB';
    $00C1: Result := 'EM_LINELENGTH';
    $00C2: Result := 'EM_REPLACESEL';
    $00C4: Result := 'EM_GETLINE';
    $00C5: Result := 'EM_LIMITTEXT';
    $00C6: Result := 'EM_CANUNDO';
    $00C7: Result := 'EM_UNDO';
    $00C8: Result := 'EM_FMTLINES';
    $00C9: Result := 'EM_LINEFROMCHAR';
    $00CB: Result := 'EM_SETTABSTOPS';
    $00CC: Result := 'EM_SETPASSWORDCHAR';
    $00CD: Result := 'EM_EMPTYUNDOBUFFER';
    $00CE: Result := 'EM_GETFIRSTVISIBLELINE';
    $00CF: Result := 'EM_SETREADONLY';
    $00D0: Result := 'EM_SETWORDBREAKPROC';
    $00D1: Result := 'EM_GETWORDBREAKPROC';
    $00D2: Result := 'EM_GETPASSWORDCHAR';
    $00D3: Result := 'EM_SETMARGINS';
    $00D4: Result := 'EM_GETMARGINS';
    $00D5: Result := 'EM_GETLIMITTEXT';
    $00D6: Result := 'EM_POSFROMCHAR';
    $00D7: Result := 'EM_CHARFROMPOS';
    // edit control messages end
    // scrollbar control messages start
    $00E0: Result := 'SBM_SETPOS';
    $00E1: Result := 'SBM_GETPOS';
    $00E2: Result := 'SBM_SETRANGE';
    $00E3: Result := 'SBM_GETRANGE';
    $00E4: Result := 'SBM_ENABLE_ARROWS';
    $00E6: Result := 'SBM_SETRANGEREDRAW';
    $00E9: Result := 'SBM_SETSCROLLINFO';
    $00EA: Result := 'SBM_GETSCROLLINFO';
    $00EB: Result := 'SBM_GETSCROLLBARINFO';
    // scrollbar control messages end
    // button control messages start
    $00F0: Result := 'BM_GETCHECK';
    $00F1: Result := 'BM_SETCHECK';
    $00F2: Result := 'BM_GETSTATE';
    $00F3: Result := 'BM_SETSTATE';
    $00F4: Result := 'BM_SETSTYLE';
    $00F5: Result := 'BM_CLICK';
    $00F6: Result := 'BM_GETIMAGE';
    $00F7: Result := 'BM_SETIMAGE';
    $00F8: Result := 'BM_SETDONTCLICK';
    // button control messages end
    $0100: Result := 'WM_KEYFIRST or WM_KEYDOWN';
    $0101: Result := 'WM_KEYUP';
    $0102: Result := 'WM_CHAR';
    $0103: Result := 'WM_DEADCHAR';
    $0104: Result := 'WM_SYSKEYDOWN';
    $0105: Result := 'WM_SYSKEYUP';
    $0106: Result := 'WM_SYSCHAR';
    $0107: Result := 'WM_SYSDEADCHAR';
    $0108: Result := 'WM_KEYLAST';
    $010D: Result := 'WM_IME_STARTCOMPOSITION';
    $010E: Result := 'WM_IME_ENDCOMPOSITION';
    $010F: Result := 'WM_IME_COMPOSITION or WM_IME_KEYLAST';
    $0110: Result := 'WM_INITDIALOG';
    $0111: Result := 'WM_COMMAND';
    $0112: Result := 'WM_SYSCOMMAND';
    $0113: Result := 'WM_TIMER';
    $0114: Result := 'WM_HSCROLL';
    $0115: Result := 'WM_VSCROLL';
    $0116: Result := 'WM_INITMENU';
    $0117: Result := 'WM_INITMENUPOPUP';
    $011F: Result := 'WM_MENUSELECT';
    $0120: Result := 'WM_MENUCHAR';
    $0121: Result := 'WM_ENTERIDLE';
    $0122: Result := 'WM_MENURBUTTONUP';
    $0123: Result := 'WM_MENUDRAG';
    $0124: Result := 'WM_MENUGETOBJECT';
    $0125: Result := 'WM_UNINITMENUPOPUP';
    $0126: Result := 'WM_MENUCOMMAND';
    $0127: Result := 'WM_CHANGEUISTATE';
    $0128: Result := 'WM_UPDATEUISTATE';
    $0129: Result := 'WM_QUERYUISTATE';
    $0132: Result := 'WM_CTLCOLORMSGBOX';
    $0133: Result := 'WM_CTLCOLOREDIT';
    $0134: Result := 'WM_CTLCOLORLISTBOX';
    $0135: Result := 'WM_CTLCOLORBTN';
    $0136: Result := 'WM_CTLCOLORDLG';
    $0137: Result := 'WM_CTLCOLORSCROLLBAR';
    $0138: Result := 'WM_CTLCOLORSTATIC';
    $0140: Result := 'CB_GETEDITSEL';
    $0141: Result := 'CB_LIMITTEXT';
    $0142: Result := 'CB_SETEDITSEL';
    $0143: Result := 'CB_ADDSTRING';
    $0144: Result := 'CB_DELETESTRING';
    $0145: Result := 'CB_DIR';
    $0146: Result := 'CB_GETCOUNT';
    $0147: Result := 'CB_GETCURSEL';
    $0148: Result := 'CB_GETLBTEXT';
    $0149: Result := 'CB_GETLBTEXTLEN';
    $014A: Result := 'CB_INSERTSTRING';
    $014B: Result := 'CB_RESETCONTENT';
    $014C: Result := 'CB_FINDSTRING';
    $014D: Result := 'CB_SELECTSTRING';
    $014E: Result := 'CB_SETCURSEL';
    $014F: Result := 'CB_SHOWDROPDOWN';
    $0150: Result := 'CB_GETITEMDATA';
    $0151: Result := 'CB_SETITEMDATA';
    $0152: Result := 'CB_GETDROPPEDCONTROLRECT';
    $0153: Result := 'CB_SETITEMHEIGHT';
    $0154: Result := 'CB_GETITEMHEIGHT';
    $0155: Result := 'CB_SETEXTENDEDUI';
    $0156: Result := 'CB_GETEXTENDEDUI';
    $0157: Result := 'CB_GETDROPPEDSTATE';
    $0158: Result := 'CB_FINDSTRINGEXACT';
    $0159: Result := 'CB_SETLOCALE';
    $015A: Result := 'CB_GETLOCALE';
    $015B: Result := 'CB_GETTOPINDEX';
    $015C: Result := 'CB_SETTOPINDEX';
    $015D: Result := 'CB_GETHORIZONTALEXTENT';
    $015E: Result := 'CB_SETHORIZONTALEXTENT';
    $015F: Result := 'CB_GETDROPPEDWIDTH';
    $0160: Result := 'CB_SETDROPPEDWIDTH';
    $0161: Result := 'CB_INITSTORAGE';
    $0163: Result := 'CB_MULTIPLEADDSTRING';
    $0164: Result := 'CB_GETCOMBOBOXINFO';
    $0200: Result := 'WM_MOUSEFIRST or WM_MOUSEMOVE';
    $0201: Result := 'WM_LBUTTONDOWN';
    $0202: Result := 'WM_LBUTTONUP';
    $0203: Result := 'WM_LBUTTONDBLCLK';
    $0204: Result := 'WM_RBUTTONDOWN';
    $0205: Result := 'WM_RBUTTONUP';
    $0206: Result := 'WM_RBUTTONDBLCLK';
    $0207: Result := 'WM_MBUTTONDOWN';
    $0208: Result := 'WM_MBUTTONUP';
    $0209: Result := 'WM_MBUTTONDBLCLK';
    $020A: Result := 'WM_MOUSEWHEEL or WM_MOUSELAST';
    $0210: Result := 'WM_PARENTNOTIFY';
    $0211: Result := 'WM_ENTERMENULOOP';
    $0212: Result := 'WM_EXITMENULOOP';
    $0213: Result := 'WM_NEXTMENU';
    $0214: Result := 'WM_SIZING';
    $0215: Result := 'WM_CAPTURECHANGED';
    $0216: Result := 'WM_MOVING';
    $0218: Result := 'WM_POWERBROADCAST';
    $0219: Result := 'WM_DEVICECHANGE';
    $0220: Result := 'WM_MDICREATE';
    $0221: Result := 'WM_MDIDESTROY';
    $0222: Result := 'WM_MDIACTIVATE';
    $0223: Result := 'WM_MDIRESTORE';
    $0224: Result := 'WM_MDINEXT';
    $0225: Result := 'WM_MDIMAXIMIZE';
    $0226: Result := 'WM_MDITILE';
    $0227: Result := 'WM_MDICASCADE';
    $0228: Result := 'WM_MDIICONARRANGE';
    $0229: Result := 'WM_MDIGETACTIVE';
    $0230: Result := 'WM_MDISETMENU';
    $0231: Result := 'WM_ENTERSIZEMOVE';
    $0232: Result := 'WM_EXITSIZEMOVE';
    $0233: Result := 'WM_DROPFILES';
    $0234: Result := 'WM_MDIREFRESHMENU';
    $0281: Result := 'WM_IME_SETCONTEXT';
    $0282: Result := 'WM_IME_NOTIFY';
    $0283: Result := 'WM_IME_CONTROL';
    $0284: Result := 'WM_IME_COMPOSITIONFULL';
    $0285: Result := 'WM_IME_SELECT';
    $0286: Result := 'WM_IME_CHAR';
    $0288: Result := 'WM_IME_REQUEST';
    $0290: Result := 'WM_IME_KEYDOWN';
    $0291: Result := 'WM_IME_KEYUP';
    $02A1: Result := 'WM_MOUSEHOVER';
    $02A2: Result := 'WM_NCMOUSELEAVE';
    $02A3: Result := 'WM_MOUSELEAVE';
    $0300: Result := 'WM_CUT';
    $0301: Result := 'WM_COPY';
    $0302: Result := 'WM_PASTE';
    $0303: Result := 'WM_CLEAR';
    $0304: Result := 'WM_UNDO';
    $0305: Result := 'WM_RENDERFORMAT';
    $0306: Result := 'WM_RENDERALLFORMATS';
    $0307: Result := 'WM_DESTROYCLIPBOARD';
    $0308: Result := 'WM_DRAWCLIPBOARD';
    $0309: Result := 'WM_PAINTCLIPBOARD';
    $030A: Result := 'WM_VSCROLLCLIPBOARD';
    $030B: Result := 'WM_SIZECLIPBOARD';
    $030C: Result := 'WM_ASKCBFORMATNAME';
    $030D: Result := 'WM_CHANGECBCHAIN';
    $030E: Result := 'WM_HSCROLLCLIPBOARD';
    $030F: Result := 'WM_QUERYNEWPALETTE';
    $0310: Result := 'WM_PALETTEISCHANGING';
    $0311: Result := 'WM_PALETTECHANGED';
    $0312: Result := 'WM_HOTKEY';
    $0317: Result := 'WM_PRINT';
    $0318: Result := 'WM_PRINTCLIENT';
    $031F: Result := 'WM_DWMNCRENDERINGCHANGED';
    $0358: Result := 'WM_HANDHELDFIRST';
    $035F: Result := 'WM_HANDHELDLAST';
    $0380: Result := 'WM_PENWINFIRST';
    $038F: Result := 'WM_PENWINLAST';
    $0390: Result := 'WM_COALESCE_FIRST';
    $039F: Result := 'WM_COALESCE_LAST';
    $03E0: Result := 'WM_DDE_FIRST or WM_DDE_INITIATE';
    $03E1: Result := 'WM_DDE_TERMINATE';
    $03E2: Result := 'WM_DDE_ADVISE';
    $03E3: Result := 'WM_DDE_UNADVISE';
    $03E4: Result := 'WM_DDE_ACK';
    $03E5: Result := 'WM_DDE_DATA';
    $03E6: Result := 'WM_DDE_REQUEST';
    $03E7: Result := 'WM_DDE_POKE';
    $03E8: Result := 'WM_DDE_EXECUTE or WM_DDE_LAST';
    $0400: Result := 'WM_USER';
    // progress bar
    $0401: Result := 'PBM_SETRANGE';
    $0402: Result := 'PBM_SETPOS';
    $0403: Result := 'PBM_DELTAPOS';
    $0404: Result := 'PBM_SETSTEP';
    $0405: Result := 'PBM_STEPIT';
    $0406: Result := 'PBM_SETRANGE32';
    $0407: Result := 'PBM_GETRANGE';
    $0408: Result := 'PBM_GETPOS';
    $0409: Result := 'PBM_SETBARCOLOR';
    $040A: Result := 'PBM_SETMARQUEE';
    $040D: Result := 'PBM_GETSTEP';
    $040E: Result := 'PBM_GETBKCOLOR';
    $040F: Result := 'PBM_GETBARCOLOR';
    $0410: Result := 'PBM_SETSTATE';
    $0411: Result := 'PBM_GETSTATE';
    // misc
    $0469: Result := 'UDM_SETBUDDY';
    $046A: Result := 'UDM_GETBUDDY';
    $102C: Result := 'LVM_GETITEMSTATE';
    $8000: Result := 'WM_APP';
  else
    Result := 'Unknown(' + IntToStr(WM_Message) + ')';
  end; {Case}
end;
{$ENDIF}

class function TThemedInnoControls.HookActionCallBackWndProc(nCode: Integer;
  wParam: wParam; lParam: lParam): LRESULT;
var
  C: array [0 .. 256] of Char;
  sClassName : string;
begin
    Result := CallNextHookEx(FHook_WH_CALLWNDPROC, nCode, wParam, lParam);
    if (nCode < 0) then
     Exit;

    if (StyleServices.Enabled) and not (StyleServices.IsSystemStyle) then
    begin

      if not ClassesList.ContainsKey(PCWPStruct(lParam)^.hwnd) then
      begin
        GetClassName(PCWPStruct(lParam)^.hwnd, C, 256);
        ClassesList.Add(PCWPStruct(lParam)^.hwnd, C);
      end;

      if ClassesList.ContainsKey(PCWPStruct(lParam)^.hwnd) then
      begin
        sClassName:=ClassesList[PCWPStruct(lParam)^.hwnd];

        {$IFDEF DEBUG}
//        if (SameText(sClassName,'TEdit')) then
//        Addlog(sClassName+' '+WM_To_String(PCWPStruct(lParam)^.message)+
//        ' WParam '+IntToHex(PCWPStruct(lParam)^.wParam, 8) +
//        ' lParam '+IntToHex(PCWPStruct(lParam)^.lParam, 8) );
        {$ENDIF}

        if SameText(sClassName,'TNewButton') then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (BtnWndArrayList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               BtnWndArrayList.Add(PCWPStruct(lParam)^.hwnd, TButtonWnd.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TWizardForm') or SameText(sClassName,'TSetupForm')   then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (FormWndArrayList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               FormWndArrayList.Add(PCWPStruct(lParam)^.hwnd, TFormWnd.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TNewComboBox') then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (ComboBoxWndList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               ComboBoxWndList.Add(PCWPStruct(lParam)^.hwnd, TComboBoxWnd.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TNewCheckBox') then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (CheckBoxWndList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               CheckBoxWndList.Add(PCWPStruct(lParam)^.hwnd, TCheckBoxTextWnd.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TEdit') then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (EditWndList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               EditWndList.Add(PCWPStruct(lParam)^.hwnd, TEditTextWnd.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TNewStaticText') then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (StaticTextWndList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               StaticTextWndList.Add(PCWPStruct(lParam)^.hwnd, TStaticTextWnd.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if (SameText(sClassName,'TNewProgressBar')) then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (ProgressBarWndArrayList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               ProgressBarWndArrayList.Add(PCWPStruct(lParam)^.hwnd, TProgressBarWnd.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if (SameText(sClassName,'TNewNotebook')) then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (NotebookWndList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               NotebookWndList.Add(PCWPStruct(lParam)^.hwnd, TNotebookWnd.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if (SameText(sClassName,'TNewNotebookPage')) then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (NotebookPageWndList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               NotebookPageWndList.Add(PCWPStruct(lParam)^.hwnd, TNotebookPagelWnd.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if (SameText(sClassName,'TPanel')) then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (PanelWndList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               PanelWndList.Add(PCWPStruct(lParam)^.hwnd, TPanelWnd.Create(PCWPStruct(lParam)^.hwnd));
        end

        ;
      end;
    end;
end;

procedure TThemedInnoControls.InstallHook;
begin
  FHook_WH_CALLWNDPROC := SetWindowsHookEx(WH_CALLWNDPROC, @TThemedInnoControls.HookActionCallBackWndProc, 0, GetCurrentThreadId);
end;

procedure TThemedInnoControls.RemoveHook;
begin
  if FHook_WH_CALLWNDPROC <> 0 then
    UnhookWindowsHookEx(FHook_WH_CALLWNDPROC);
end;

{

function ColorToRGB(Color: TColor): Longint;
begin
  if Color < 0 then
    Result := GetSysColor(Color and $000000FF) else
    Result := Color;
end;
}

function GetSysColorHook(nIndex: Integer): DWORD; stdcall;
begin
//  if nIndex = COLOR_BTNFACE then
//  begin
//   Result:= StyleServices.GetSystemColor(clBtnFace);
//   Addlog(IntToHex(Result, 8));
//  end
//  else
   Result:= StyleServices.GetSystemColor(nIndex or $FF000000);
end;

Procedure  Done;
begin
if Assigned(ThemedInnoControls) then
  begin
    ThemedInnoControls.Free;
    ThemedInnoControls:=nil;
  end;

  InterceptRemove(@TrampolineGetSysColor,@GetSysColorHook);
  //RedirectProcedure(@GetSysColorHook, @Winapi.Windows.GetSysColor);
end;

var
  ThemeLibrary: THandle;

initialization

  ThemedInnoControls:=nil;
  if StyleServices.Available then
  begin
   ThemedInnoControls := TThemedInnoControls.Create;
   GetSysColorOrgPointer  := GetProcAddress(GetModuleHandle('user32.dll'), 'GetSysColor');
   @TrampolineGetSysColor := InterceptCreate(GetSysColorOrgPointer, @GetSysColorHook);
   //RedirectProcedure(GetSysColorOrgPointer, @GetSysColorHook);
  end;

finalization
   Done;
end.