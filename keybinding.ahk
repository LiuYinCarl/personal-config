; AHK Version: https://www.autohotkey.com/download/1.1/AutoHotkey_1.1.33.10_setup.exe

; Put this script into C:\ProgramData\Microsoft\Windows\Start Menu\Programs\StartUp
; to autoload while computer start.

; keymap manual https://wyagd001.github.io/zh-cn/docs/misc/Remap.htm


DisableKeyBoardAndMouse(f=0, mouse=0, message:="Keyboard and Mouse Locked! `nPress Alt+F2 to unlocks") { 
  static allkeys, ExcludeKeys:="LButton,RButton"
  if !allkeys {
    s:="||NumpadEnter|Home|End|PgUp|PgDn|Left|Right|Up|Down|Del|Ins|"
    Loop, 254
      k:=GetKeyName(Format("VK{:0X}",A_Index))
        , s.=InStr(s, "|" k "|") ? "" : k "|"
    For k,v in {Control:"Ctrl",Escape:"Esc"}
      s:=StrReplace(s, k, v)
    allkeys:=Trim(s, "|")
  }

  f:=f ? "On":"Off"
  if mouse
  ExcludeKeys:=""
  For k,v in StrSplit(allkeys,"|")
    if v not in %ExcludeKeys%
      Hotkey, *%v%, Block_Input, %f% UseErrorLevel
  Block_Input:
  if message!=
  Progress, B1 M fs12 ZH0, %message%
  if (f="Off")
  Progress, Off
  Return
}

!F1::
DisableKeyBoardAndMouse(1,1)  ; Disable all keyboard or mouse buttons
return

!F2::
DisableKeyBoardAndMouse(0)  ; Enable all keyboard and mouse buttons
Return

; don't use hotkey in Windows Remote Desktop Application
; exchange LCtrl and CapsLock
#IfWinNotActive ahk_class TscShellContainerClass
CapsLock::LCtrl
LCtrl::CapsLock
RWin::Esc
#IfWinNotActive
