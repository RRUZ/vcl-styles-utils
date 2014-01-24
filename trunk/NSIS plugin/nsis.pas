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

  {$IFDEF UNICODE}
  pstack_tW = ^stack_tW;
  stack_tW = record
    next: pstack_tW;
    text: PChar;
  end;
  {$ENDIF}

var
  g_stringsize : integer;
  g_stacktopA  : ^pstack_t;
  g_variablesA : PAnsiChar;
  {$IFDEF UNICODE}
  g_stacktopW  : ^pstack_tW;
  g_variablesW : PChar;
  {$ENDIF}
  g_hwndParent : HWND;
  g_hwndList   : HWND;
  g_hwndLogList: HWND;

  g_extraparameters: pextrap_t;
  func : TExecuteCodeSegment;
  extrap : extrap_t;

  procedure InitA(const hwndParent: HWND; const string_size: integer; const variables: PAnsiChar; const stacktop: pointer; const extraparameters: pointer = nil);
  function LogMessageA(Msg : AnsiString): BOOL;
  function CallA(NSIS_func : AnsiString) : Integer;
  function PopStringA(): AnsiString;
  procedure PushStringA(const str: AnsiString='');
  function GetUserVariableA(const varnum: TVariableList): AnsiString;
  procedure SetUserVariableA(const varnum: TVariableList; const value: AnsiString);
  procedure NSISDialogA(const text, caption: AnsiString; const buttons: integer);
 {$IFDEF UNICODE}
  procedure InitW(const hwndParent: HWND; const string_size: integer; const variables: PChar; const stacktop: pointer; const extraparameters: pointer = nil);
  function LogMessageW(Msg : String): BOOL;
  function CallW(NSIS_func : String) : Integer;
  function PopStringW(): String;
  procedure PushStringW(const str: String='');
  function GetUserVariableW(const varnum: TVariableList): String;
  procedure SetUserVariableW(const varnum: TVariableList; const value: String);
  procedure NSISDialogW(const text, caption: String; const buttons: integer);
 {$ENDIF}


implementation

procedure InitA(const hwndParent: HWND; const string_size: integer; const variables: PAnsiChar; const stacktop: pointer; const extraparameters: pointer = nil);
begin
  g_stringsize := string_size;
  g_hwndParent := hwndParent;
  g_stacktopA   := stacktop;
  g_variablesA  := variables;
  g_hwndList := 0;
  g_hwndList := FindWindowEx(FindWindowEx(g_hwndParent, 0, '#32770', nil), 0,'SysListView32', nil);
  g_extraparameters := extraparameters;
  if g_extraparameters<>nil then
   extrap := g_extraparameters^;
end;


{$IFDEF UNICODE}
procedure InitW(const hwndParent: HWND; const string_size: integer; const variables: PChar; const stacktop: pointer; const extraparameters: pointer = nil);
begin
  g_stringsize := string_size;
  g_hwndParent := hwndParent;
  g_stacktopW  := stacktop;
  g_variablesW := variables;
  g_hwndList   := 0;
  g_hwndList   := FindWindowEx(FindWindowEx(g_hwndParent, 0, '#32770', nil), 0,'SysListView32', nil);
  g_extraparameters := extraparameters;
  if g_extraparameters<>nil then
   extrap := g_extraparameters^;
end;
{$ENDIF}


function CallA(NSIS_func : AnsiString) : Integer;
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

{$IFDEF UNICODE}
function CallW(NSIS_func : String) : Integer;
var
  NSISFun: Integer; //The ID of nsis function
begin
  Result := 0;
  NSISFun := StrToIntDef(NSIS_func, 0);
  if (NSISFun <> 0) and (g_extraparameters <> nil) then
    begin
      @func := extrap.exec_code_segment;
      NSISFun := NSISFun - 1;
      Result := func(NSISFun, g_hwndParent);
    end;
end;
{$ENDIF}


function LogMessageA(Msg : AnsiString): BOOL;
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

{$IFDEF UNICODE}
function LogMessageW(Msg : String): BOOL;
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
  item.pszText := PChar(Msg);
  ListView_InsertItem(g_hwndList, item );
  ListView_EnsureVisible(g_hwndList, ItemCount, TRUE);
end;
{$ENDIF}


function PopStringA(): AnsiString;
var
  th: pstack_t;
begin
  if integer(g_stacktopA^) <> 0 then begin
    th := g_stacktopA^;
    Result := PAnsiChar(@th.text);
    g_stacktopA^ := th.next;
    GlobalFree(HGLOBAL(th));
  end;
end;


{$IFDEF UNICODE}
function PopStringW(): String;
var
  th: pstack_tW;
begin
  if integer(g_stacktopW^) <> 0 then begin
    th := g_stacktopW^;
    Result := PChar(@th.text);
    g_stacktopW^ := th.next;
    GlobalFree(HGLOBAL(th));
  end;
end;
{$ENDIF}


procedure PushStringA(const str: AnsiString='');
var
  th: pstack_t;
begin
  if integer(g_stacktopA) <> 0 then begin
    th := pstack_t(GlobalAlloc(GPTR, SizeOf(stack_t) + g_stringsize));
    lstrcpynA(@th.text, PAnsiChar(str), g_stringsize);
    th.next := g_stacktopA^;
    g_stacktopA^ := th;
  end;
end;

{$IFDEF UNICODE}
procedure PushStringW(const str: String='');
var
  th: pstack_tW;
begin
  if integer(g_stacktopA) <> 0 then begin
    th := pstack_tW(GlobalAlloc(GPTR, SizeOf(stack_t) + g_stringsize));
    lstrcpynW(@th.text, PChar(str), g_stringsize);
    th.next := g_stacktopW^;
    g_stacktopW^ := th;
  end;
end;
{$ENDIF}

function GetUserVariableA(const varnum: TVariableList): AnsiString;
begin
  if (integer(varnum) >= 0) and (integer(varnum) < integer(__INST_LAST)) then
    Result := g_variablesA + integer(varnum) * g_stringsize
  else
    Result := '';
end;

{$IFDEF UNICODE}
function GetUserVariableW(const varnum: TVariableList): String;
begin
  if (integer(varnum) >= 0) and (integer(varnum) < integer(__INST_LAST)) then
    Result := g_variablesW + integer(varnum) * g_stringsize
  else
    Result := '';
end;
{$ENDIF}

procedure SetUserVariableA(const varnum: TVariableList; const value: AnsiString);
begin
  if (value <> '') and (integer(varnum) >= 0) and (integer(varnum) < integer(__INST_LAST)) then
    lstrcpyA(g_variablesA + integer(varnum) * g_stringsize, PAnsiChar(value))
end;

{$IFDEF UNICODE}
procedure SetUserVariableW(const varnum: TVariableList; const value: String);
begin
  if (value <> '') and (integer(varnum) >= 0) and (integer(varnum) < integer(__INST_LAST)) then
    lstrcpyW(g_variablesW + integer(varnum) * g_stringsize, PChar(value))
end;
{$ENDIF}

procedure NSISDialogA(const text, caption: AnsiString; const buttons: integer);
begin
  MessageBox(g_hwndParent, PChar(String(text)), PChar(String(caption)), buttons);
end;

{$IFDEF UNICODE}
procedure NSISDialogW(const text, caption: String; const buttons: integer);
begin
  MessageBox(g_hwndParent, PChar(String(text)), PChar(String(caption)), buttons);
end;
{$ENDIF}


begin

end.

