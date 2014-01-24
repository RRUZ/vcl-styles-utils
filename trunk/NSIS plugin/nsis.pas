{
    Original Code from
    (C) 2001 - Peter Windridge

    Code in seperate unit and some changes
    2003 by Bernhard Mayer

    Fixed and formatted by Brett Dever
    http://editor.nfscheats.com/

    2014 Rodrigo Ruz  - theroadtodelphi.wordpress.com
    Added comptability for Delphi UNICODE Versions.

    simply include this unit in your plugin project and export
    functions as needed
}

unit nsis;

interface

uses
  windows, CommCtrl, SysUtils;

type
  VarConstants = (
    INST_0,       // $0
    INST_1,       // $1
    INST_2,       // $2
    INST_3,       // $3
    INST_4,       // $4
    INST_5,       // $5
    INST_6,       // $6
    INST_7,       // $7
    INST_8,       // $8
    INST_9,       // $9
    INST_R0,      // $R0
    INST_R1,      // $R1
    INST_R2,      // $R2
    INST_R3,      // $R3
    INST_R4,      // $R4
    INST_R5,      // $R5
    INST_R6,      // $R6
    INST_R7,      // $R7
    INST_R8,      // $R8
    INST_R9,      // $R9
    INST_CMDLINE, // $CMDLINE
    INST_INSTDIR, // $INSTDIR
    INST_OUTDIR,  // $OUTDIR
    INST_EXEDIR,  // $EXEDIR
    INST_LANG,    // $LANGUAGE
    __INST_LAST
    );
  TVariableList = INST_0..__INST_LAST;

type
  PluginCallbackMessages = (
    NSPIM_UNLOAD,    // This is the last message a plugin gets, do final cleanup
    NSPIM_GUIUNLOAD  // Called after .onGUIEnd
    );
  TNSPIM = NSPIM_UNLOAD..NSPIM_GUIUNLOAD;
  //TPluginCallback = function (const NSPIM: Integer): Pointer;
  TExecuteCodeSegment = function (const funct_id: Integer; const parent: HWND): Integer;  stdcall;
  Tvalidate_filename = procedure (const filename: PAnsiChar); cdecl;
  TRegisterPluginCallback = function (const DllInstance: HMODULE; const CallbackFunction: Pointer): Integer; stdcall;


  pexec_flags_t = ^exec_flags_t;
  exec_flags_t = record
    autoclose: Integer;
    all_user_var: Integer;
    exec_error: Integer;
    abort: Integer;
    exec_reboot: Integer;
    reboot_called: Integer;
    XXX_cur_insttype: Integer;
    plugin_api_version: Integer;
    silent: Integer;
    instdir_error: Integer;
    rtl: Integer;
    errlvl: Integer;
    alter_reg_view: Integer;
    status_update: Integer;
  end;

  pextrap_t = ^extrap_t;
  extrap_t = record
    exec_flags: Pointer; // exec_flags_t;
    exec_code_segment: Pointer; //  TFarProc;
    validate_filename: Pointer; // Tvalidate_filename;
    RegisterPluginCallback: Pointer; //TRegisterPluginCallback;
  end;

  pstack_t = ^stack_t;
  stack_t = record
    next: pstack_t;
    text: PAnsiChar;
  end;

var
  g_stringsize: integer;
  g_stacktop: ^pstack_t;
  g_variables: PAnsiChar;
  g_hwndParent: HWND;
  g_hwndList: HWND;
  g_hwndLogList: HWND;

  g_extraparameters: pextrap_t;
  func : TExecuteCodeSegment;
  extrap : extrap_t;

procedure Init(const hwndParent: HWND; const string_size: integer; const variables: PAnsiChar; const stacktop: pointer; const extraparameters: pointer = nil);

function LogMessage(Msg : AnsiString): BOOL;
function Call(NSIS_func : AnsiString) : Integer;
function PopString(): AnsiString;
procedure PushString(const str: AnsiString='');
function GetUserVariable(const varnum: TVariableList): AnsiString;
procedure SetUserVariable(const varnum: TVariableList; const value: AnsiString);
procedure NSISDialog(const text, caption: AnsiString; const buttons: integer);

implementation

procedure Init(const hwndParent: HWND; const string_size: integer; const variables: PAnsiChar; const stacktop: pointer; const extraparameters: pointer = nil);
begin
  g_stringsize := string_size;
  g_hwndParent := hwndParent;
  g_stacktop   := stacktop;
  g_variables  := variables;
  g_hwndList := 0;
  g_hwndList := FindWindowEx(FindWindowEx(g_hwndParent, 0, '#32770', nil), 0,'SysListView32', nil);
  g_extraparameters := extraparameters;
  if g_extraparameters<>nil then
   extrap := g_extraparameters^;
end;

function Call(NSIS_func : AnsiString) : Integer;
var
  NSISFun: Integer; //The ID of nsis function
begin
  Result := 0;
  NSISFun := StrToIntDef(String(NSIS_func), 0);
  if (NSISFun <> 0) and (g_extraparameters <> nil) then
    begin
      @func := extrap.exec_code_segment;
      NSISFun := NSISFun - 1;
      Result := func(NSISFun, g_hwndParent);
    end;
end;

function LogMessage(Msg : AnsiString): BOOL;
var
  ItemCount : Integer;
  item: TLVItem;
begin
  Result := FAlse;
  if g_hwndList = 0 then exit;
  FillChar( item, sizeof(item), 0 );
  ItemCount := SendMessage(g_hwndList, LVM_GETITEMCOUNT, 0, 0);
  item.iItem := ItemCount;
  item.mask := LVIF_TEXT;
  item.pszText := PChar(PAnsiChar(Msg));
  ListView_InsertItem(g_hwndList, item );
  ListView_EnsureVisible(g_hwndList, ItemCount, TRUE);
end;

function PopString(): AnsiString;
var
  th: pstack_t;
begin
  if integer(g_stacktop^) <> 0 then begin
    th := g_stacktop^;
    Result := PAnsiChar(@th.text);
    g_stacktop^ := th.next;
    GlobalFree(HGLOBAL(th));
  end;
end;

procedure PushString(const str: AnsiString='');
var
  th: pstack_t;
begin
  if integer(g_stacktop) <> 0 then begin
    th := pstack_t(GlobalAlloc(GPTR, SizeOf(stack_t) + g_stringsize));
    lstrcpynA(@th.text, PAnsiChar(str), g_stringsize);
    th.next := g_stacktop^;
    g_stacktop^ := th;
  end;
end;

function GetUserVariable(const varnum: TVariableList): AnsiString;
begin
  if (integer(varnum) >= 0) and (integer(varnum) < integer(__INST_LAST)) then
    Result := g_variables + integer(varnum) * g_stringsize
  else
    Result := '';
end;

procedure SetUserVariable(const varnum: TVariableList; const value: AnsiString);
begin
  if (value <> '') and (integer(varnum) >= 0) and (integer(varnum) < integer(__INST_LAST)) then
    lstrcpyA(g_variables + integer(varnum) * g_stringsize, PAnsiChar(value))
end;

procedure NSISDialog(const text, caption: AnsiString; const buttons: integer);
begin
  MessageBox(g_hwndParent, PChar(String(text)), PChar(String(caption)), buttons);
end;

begin

end.

