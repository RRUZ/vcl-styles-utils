# Fix: `TSysDialogStyleHook.GetIcon` fails on Win64

**File:** `Common/Vcl.Styles.Utils.Forms.pas`
**Function:** `TSysDialogStyleHook.GetIcon`
**Symptom:** styled system dialogs show no icon when the app is built for Win64.
**Severity:** cosmetic (missing icon), but one of the two changes is a latent buffer overrun.

---

## Background

`GetIcon` tries several sources for a dialog's icon in order, and falls back to the window class's
registered icon if none of the `WM_GETICON` sends return a handle:

```pascal
if TmpHandle = 0 then
begin
  { Get instance }
  GetClassName(Handle, @Buffer, sizeof(Buffer));
  FillChar(Info, sizeof(Info), 0);
  Info.cbSize := sizeof(Info);

  if GetClassInfoEx(GetWindowLong(Handle, GWL_HINSTANCE), @Buffer, Info) then
  begin
    TmpHandle := Info.hIconSm;
    if TmpHandle = 0 then
      TmpHandle := Info.HICON;
  end
end;
```

That fallback is exactly the path taken for most Windows-owned dialogs, which typically do not answer
`WM_GETICON`. It contains two bugs.

---

## Bug 1 — `GetWindowLong` truncates the `HINSTANCE` on Win64

`GetWindowLong` returns a 32-bit `Longint`. `GWL_HINSTANCE` asks for a module handle, which is
pointer-sized — 64 bits under Win64. The upper half is discarded, `GetClassInfoEx` is handed a
meaningless instance, the lookup fails, `TmpHandle` stays `0`, and no icon is drawn.

This compiles without a warning and works perfectly on Win32, which is why it survived.

The documented replacement is `GetWindowLongPtr` with the `GWLP_` constant. On Win32,
`GetWindowLongPtr` is defined as `GetWindowLong` and `GWLP_HINSTANCE = GWL_HINSTANCE`, so the fix is
behaviour-preserving there.

```pascal
{ before }
if GetClassInfoEx(GetWindowLong(Handle, GWL_HINSTANCE), @Buffer, Info) then

{ after }
if GetClassInfoEx(HINST(GetWindowLongPtr(Handle, GWLP_HINSTANCE)), @Buffer, Info) then
```

The `HINST` cast is needed because `GetWindowLongPtr` returns a signed `LONG_PTR` while
`GetClassInfoEx` takes an unsigned `HINST`.

### Note on the other `GetWindowLong` calls in this unit

They are correct as written and are deliberately left alone. `GWL_STYLE` (line 1235,
`GWL_EXSTYLE`) really is a 32-bit value, so `GetWindowLong` is the right call there. Only the
handle- and pointer-valued indices (`GWL_HINSTANCE`, `GWL_WNDPROC`, `GWL_HWNDPARENT`, `GWL_USERDATA`)
need the `Ptr` form — and line 285 already uses `GetWindowLongPtr` for `GWL_USERDATA`.

---

## Bug 2 — `GetClassName` is told the buffer is twice its real size

```pascal
Buffer: array [0 .. 255] of Char;   { 256 WideChars = 512 bytes }
...
GetClassName(Handle, @Buffer, sizeof(Buffer));   { passes 512 }
```

`GetClassName`'s `nMaxCount` is a **character** count, not a byte count. `Char` is `WideChar`, so
`sizeof(Buffer)` is 512 while the buffer only holds 256 characters. The API is therefore permitted to
write up to 512 characters — 1024 bytes — into a 512-byte stack buffer.

In practice window class names are short, so this has probably never been observed. It is still a
stack overrun waiting for a long class name, and it is trivially avoidable:

```pascal
{ before }
GetClassName(Handle, @Buffer, sizeof(Buffer));

{ after }
GetClassName(Handle, @Buffer, Length(Buffer));
```

`Length` on a static array yields the element count (256), which is what the API wants.

This bug is platform-independent — it affects Win32 exactly as much as Win64. It is included here
only because it sits on the adjacent line inside the same `if` block; **it is an independent change
and can reasonably be split into its own commit.**

---

## The patch

```diff
   if TmpHandle = 0 then
   begin
     { Get instance }
-    GetClassName(Handle, @Buffer, sizeof(Buffer));
+    { nMaxCount is a character count, not a byte count }
+    GetClassName(Handle, @Buffer, Length(Buffer));
     FillChar(Info, sizeof(Info), 0);
     Info.cbSize := sizeof(Info);
 
-    if GetClassInfoEx(GetWindowLong(Handle, GWL_HINSTANCE), @Buffer, Info) then
+    { GetWindowLong truncates an HINSTANCE to 32 bits on Win64 }
+    if GetClassInfoEx(HINST(GetWindowLongPtr(Handle, GWLP_HINSTANCE)), @Buffer, Info) then
     begin
       TmpHandle := Info.hIconSm;
       if TmpHandle = 0 then
         TmpHandle := Info.HICON;
     end
   end;
```

---

## Verification

`Vcl.Styles.Utils.Forms.pas` compiled standalone with Delphi 10.4 (compiler 34.0), `-W+ -H+`:

| | Result |
|---|---|
| `dcc64` (Win64) | 18,824 lines, 0 errors |
| `dcc32` (Win32) | 21,654 lines, 0 errors |

**Not verified at runtime.** Confirming the fix visually means running a Win64 host application that
hooks a system dialog whose icon comes from the window class rather than from `WM_GETICON` — the icon
should now appear where it previously did not, with no change to the Win32 behaviour.

---

## Where this came from

Found while auditing a large Delphi application for Win64 compatibility. The dominant failure mode
across that audit was a pointer- or handle-sized value cast through a 32-bit `Integer` / `Longint` /
`Cardinal`: it compiles silently, truncates, and then either fails quietly (as here) or dereferences
garbage. `GetWindowLong(…, GWL_HINSTANCE)` is one of the standard instances of that pattern.

The rest of `vcl-styles-utils` was swept in the same audit and came out clean — no inline assembly,
no pointer-to-`Integer` truncation, and no `SetWindowLong(GWL_WNDPROC)` subclassing (the library
hooks through DDetours, which has a full x64 decoder). This was the only finding.
