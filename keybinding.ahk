;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 注释
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 将该脚本的快捷方式放置于 C:\ProgramData\Microsoft\Windows\Start Menu\Programs\StartUp
; 即可实现开机启动

; Realforce 机械键盘内置快捷键
; Fn+F1，开启默认浏览器
; Fn+F2，开启默认邮件客户端
; Fn+F3，计算器
; Fn+F4，媒体播放器
; Fn+F5，上一曲
; Fn+F6，暂停
; Fn+F7，下一曲
; Fn+F8，停止播放
; Fn+F9，浏览器收藏夹
; Fn+F10，打开资源管理器，具体打开的文件夹可以在驱动中设置
; Fn+F11，交换Caps Lock和Ctrl
; Fn+F12，键盘锁，先在驱动里面设置需要失效的键，然后开启即可

; 按键映射
; https://wyagd001.github.io/zh-cn/docs/misc/Remap.htm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 工具函数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;禁用按键的函数。
BlockKeyboardInputs()
{
    ; 定义除了快捷键 Pause 之外的所有按键
    static keys
    keys = Space,Enter,Tab,Esc,BackSpace,Del,Ins,Home,End,PgDn,PgUp,Up,DownS
    ,Left,Right,CtrlBreak,ScrollLock,PrintScreen,CapsLock,AppsKey,LWin,LWin,NumLock
    ,Numpad0,Numpad1,Numpad2,Numpad3,Numpad4,Numpad5,Numpad6,Numpad7,Numpad8
    ,Numpad9,NumpadDot,NumpadDiv,NumpadMult,NumpadAdd,NumpadSub,NumpadEnter
    ,NumpadIns,NumpadEnd,NumpadDown,NumpadPgDn,NumpadLeft,NumpadClear
    ,NumpadRight,NumpadHome,NumpadUp,NumpadPgUp,NumpadDel,Media_Next
    ,Media_Play_Pause,Media_Prev,Media_Stop,Volume_Down,Volume_Up,Volume_Mute
    ,Browser_Back,Browser_Favorites,Browser_Home,Browser_Refresh,Browser_Search
    ,Browser_Stop,Launch_App1,Launch_App2,Launch_Mail,Launch_Media
    ,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21,F22
    ,1,2,3,4,5,6,7,8,9,0,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    ,?,&,é,",',(,-,è,_,?,à,),=,$,?,ù,*,~,#,{,[,|,``,\,^,@,],},;,:,!,?,.,/,§,<,>,vkBC


    ; blockKeyBoard 只能设置为 "On" 或者 "Off"
    static blockKeyBoard := "Off"
    if (blockKeyBoard == "Off") {
        blockKeyBoard = "On"
    } else {
        blockKeyBoard = "Off"
    }

    ; 解析 keys 字符串, `, 表示解析的分隔符是 ","
    Loop, Parse, keys, `,
    ; 将 keys 变量中的所有按键映射为 blockKeyBoard 状态(On 或者 Off)
    ; 如果为 On, 按键无效, 如果为 Off, 按键保持为原有功能
    Hotkey, *%A_LoopField%, KeyboardDummyLabel, %blockKeyBoard% UseErrorLevel
    Return

; Hotkey 函数需要一个 label, 所以这里写一个无效的 label
KeyboardDummyLabel:
    Return
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 按键映射
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 在Windows 远程桌面中不使用快捷键
#IfWinNotActive ahk_class TscShellContainerClass
CapsLock::LCtrl
LCtrl::CapsLock
RWin::Esc
#IfWinNotActive

Pause::BlockKeyboardInputs()
