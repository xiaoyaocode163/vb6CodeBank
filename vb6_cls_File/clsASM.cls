Option Explicit
Private Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long
Private Declare Function WriteProcessMemory Lib "kernel32" (ByVal hProcess As Long, ByVal lpBaseAddress As Any, lpBuffer As Any, ByVal nSize As Long, lpNumberOfBytesWritten As Long) As Long
Private Declare Function CreateRemoteThread Lib "kernel32" (ByVal hProcess As Long, lpThreadAttributes As Any, ByVal dwStackSize As Long, lpStartAddress As Long, lpParameter As Any, ByVal dwCreationFlags As Long, lpThreadId As Long) As Long
Private Declare Function VirtualFreeEx Lib "kernel32" (ByVal hProcess As Long, lpAddress As Any, ByVal dwSize As Long, ByVal dwFreeType As Long) As Long
Private Declare Function VirtualAllocEx Lib "kernel32" (ByVal hProcess As Long, lpAddress As Any, ByVal dwSize As Long, ByVal flAllocationType As Long, ByVal flProtect As Long) As Long
Private Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" (ByVal lpPrevWndFunc As Long, ByVal hwnd As Long, ByVal Msg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Private Declare Function OpenProcess Lib "kernel32" (ByVal dwDesiredAccess As Long, ByVal bInheritHandle As Long, ByVal dwProcessId As Long) As Long
Private Declare Function WaitForSingleObject Lib "kernel32" (ByVal hHandle As Long, ByVal dwMilliseconds As Long) As Long
 
Const PAGE_EXECUTE_READWRITE = &H40
Const MEM_COMMIT = &H1000
Const MEM_RELEASE = &H8000
Const MEM_DECOMMIT = &H4000
Const PROCESS_ALL_ACCESS = &H1F0FFF
Const WAIT_TIMEOUT = &H100
Const INFINITE = &HFFFFFFFF
Dim OPcode As String
Dim AlloAddr1 As Long
Sub AlloAddr(i As Long)
AlloAddr1 = i
End Sub
 
Function Get_Result() As String
Dim i As Long
ReDim AsmCode(Len(OPcode) / 2 - 1) As Byte
For i = 0 To UBound(AsmCode)
    AsmCode(i) = CByte("&H" & Mid(OPcode, i * 2 + 1, 2))
Next
Get_Result = CallWindowProc(VarPtr(AsmCode(0)), 0, 0, 0, 0)
End Function
 
Function Get_Code() As String
Get_Code = OPcode
End Function
 
'Function Run_ASM(pid As Long) As Long
 
'Dim i As Long, tmp_Addr As Long, RThwnd As Long, h As Long
'ReDim AsmCode(Len(OPcode) / 2 - 1) As Byte
'For i = 0 To UBound(AsmCode)
'    AsmCode(i) = CByte("&H" & Mid(OPcode, i * 2 + 1, 2))   '把指令字符串OPcode变成字节数组
'Next
'h = OpenProcess(PROCESS_ALL_ACCESS, False, pid)
'If h = 0 Then
'MsgBox ("h=0")
'End If
'tmp_Addr = VirtualAllocEx(h, ByVal 0&, UBound(AsmCode) + 1, MEM_COMMIT, PAGE_EXECUTE_READWRITE)
'If tmp_Addr = 0 Then
'MsgBox ("tmp_addr=null")
'End If
'WriteProcessMemory h, ByVal tmp_Addr, ByVal VarPtr(AsmCode(0)), UBound(AsmCode) + 1, ByVal 0&
'RThwnd = CreateRemoteThread(h, ByVal 0&, 0, ByVal tmp_Addr, ByVal 0&, ByVal 0&, ByVal 0&)
'If RThwnd = 0 Then
'MsgBox ("rthwnd=null")
'End If
'VirtualFreeEx h, tmp_Addr, UBound(AsmCode) + 1, MEM_DECOMMIT
'CloseHandle RThwnd
'CloseHandle h
'OPcode = ""
'End Function
 
'Function Run_ASM(pid As Long) As Long
 
 
'Dim i As Long, tmp_Addr As Long, RThwnd As Long, h As Long, ret As Long
'ReDim AsmCode(Len(OPcode) / 2 - 1) As Byte
'For i = 0 To UBound(AsmCode)
'    AsmCode(i) = CByte("&H" & Mid(OPcode, i * 2 + 1, 2))
' Next
'  h = OpenProcess(PROCESS_ALL_ACCESS, False, pid)
'  tmp_Addr = VirtualAllocEx(h, ByVal 0&, UBound(AsmCode) + 1, MEM_COMMIT, PAGE_EXECUTE_READWRITE)
'  WriteProcessMemory h, ByVal tmp_Addr, ByVal VarPtr(AsmCode(0)), UBound(AsmCode) + 1, ByVal 0&
'  MsgBox Hex(tmp_Addr)
'   RThwnd = CreateRemoteThread(h, ByVal 0&, 0, ByVal tmp_Addr, ByVal 0&, ByVal 0&, ByVal 0&)
'  Do
'  ret = WaitForSingleObject(RThwnd, 100)
'  DoEvents
'  Loop Until ret <> WAIT_TIMEOUT
'  VirtualFreeEx h, ByVal tmp_Addr, 0, ByVal MEM_DECOMMIT
'  CloseHandle RThwnd
'  CloseHandle h
'  OPcode = ""
'  End Function
''简单说明下，前面楼上提示WaitForSingleObject h,-1是错的，h是游戏进程，如果要等它结束的话，
'程序肯定假死直到你把游戏关了，而我们要等的是我们注入的代码被执行了，所以应该是 WaitForSingleObject RThwnd,-1，
'不过这样的话程序还是会假死，直到最前台的程序是我们注入的那个程序，所以还是用DO LOOP UNTIL 加DOEVENT就不会假死了...
 
 
Function Run_ASM(pid As Long) As Long
 
Dim i As Long, tmp_Addr As Long, RThnd As Long, h As Long, ret As Long
Dim insertto As Long
ReDim AsmCode(Len(OPcode) / 2 - 1) As Byte
For i = 0 To UBound(AsmCode)
    AsmCode(i) = CByte("&H" & Mid(OPcode, i * 2 + 1, 2))
Next
h = OpenProcess(PROCESS_ALL_ACCESS, False, pid)
If h Then
tmp_Addr = VirtualAllocEx(h, ByVal 0, UBound(AsmCode) + 1, MEM_COMMIT, PAGE_EXECUTE_READWRITE)
End If
If tmp_Addr = 0 Then
MsgBox ("tmp_Addr=Null")
End If
MsgBox tmp_Addr
If tmp_Addr Then
insertto = WriteProcessMemory(h, ByVal tmp_Addr, ByVal VarPtr(AsmCode(0)), UBound(AsmCode) + 1, ByVal 0&)
End If
If insertto = 0 Then
MsgBox ("insertto=Null")
End If
If insertto Then
RThnd = CreateRemoteThread(h, ByVal 0&, ByVal 40&, ByVal tmp_Addr, ByVal 0&, ByVal 0&, ByVal 0&)
End If
If RThnd = 0 Then
MsgBox ("RThwnd = Null")
End If
Do
ret = WaitForSingleObject(RThnd, 100)
'DoEvents
Loop Until ret <> WAIT_TIMEOUT
'ret = WaitForSingleObject(RThnd, INFINITE)
MsgBox Hex(ret)
CloseHandle RThnd
VirtualFreeEx h, ByVal tmp_Addr, 0, ByVal MEM_DECOMMIT
CloseHandle h
OPcode = ""
End Function
 
Function Int2Hex(Value As Long, n As Long) As String '高地位互换   比如:&h12345678 转换后 &h78563412
Dim tmp1 As String, tmp2 As String, i As Long
tmp1 = Right("0000000" + Hex(Value), n)
For i = 0 To Len(tmp1) / 2 - 1
tmp2 = tmp2 + Mid(tmp1, Len(tmp1) - 1 - 2 * i, 2)
Next i
Int2Hex = tmp2
End Function
 
Function Leave() As Long
OPcode = OPcode + "C9"
End Function
 
Function Pushad() As Long   '核对
OPcode = OPcode + "60"
End Function
 
Function Popad() As Long    '核对
OPcode = OPcode + "61"
End Function
 
Function Nop() As Long
OPcode = OPcode + "90"
End Function
 
Function ret() As Long    '核对
OPcode = OPcode + "C3"
End Function
 
Function RetA(i As Long) As Long
OPcode = OPcode + Int2Hex(i, 4)
End Function
 
Function IN_AL_DX() As Long
OPcode = OPcode + "EC"
End Function
 
Function TEST_EAX_EAX() As Long
OPcode = OPcode + "85C0"
End Function
 
'Add
'+++++++++++++++++++++++++++++++++++
Function Add_EAX_EDX() As Long
OPcode = OPcode + "03C2"
End Function
 
Function Add_EBX_EAX() As Long
OPcode = OPcode + "03D8"
End Function
 
Function Add_EAX_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "0305" + Int2Hex(i, 8)
End Function
 
Function Add_EBX_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "031D" + Int2Hex(i, 8)
End Function
 
Function Add_EBP_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "032D" + Int2Hex(i, 8)
End Function
 
Function Add_EAX(i As Long) As Long
OPcode = OPcode + "05" + Int2Hex(i, 8)
End Function
 
Function Add_EBX(i As Long) As Long
OPcode = OPcode + "83C3" + Int2Hex(i, 8)
End Function
 
Function Add_ECX(i As Long) As Long
OPcode = OPcode + "83C1" + Int2Hex(i, 8)
End Function
 
Function Add_EDX(i As Long) As Long
OPcode = OPcode + "83C2" + Int2Hex(i, 8)
End Function
 
Function Add_ESI(i As Long) As Long
OPcode = OPcode + "83C6" + Int2Hex(i, 8)
End Function
Function Add_EDI(i As Long) As Long
OPcode = OPcode + "83C7" + Int2Hex(i, 8)
End Function
 
Function Add_ESP(i As Long) As Long
OPcode = OPcode + "83C4" + Int2Hex(i, 8)
End Function
 
'Call
'+++++++++++++++++++++++++++++++++++
Function Call_EAX() As Long    '核对
OPcode = OPcode + "FFD0"
End Function
 
Function Call_EBX() As Long     '核对
OPcode = OPcode + "FFD3"
End Function
 
Function Call_ECX() As Long
OPcode = OPcode + "FFD1"
End Function
 
Function Call_EDX() As Long
OPcode = OPcode + "FFD2"
End Function
 
Function Call_ESI() As Long
OPcode = OPcode + "FFD2"
End Function
 
Function Call_ESP() As Long
OPcode = OPcode + "FFD4"
End Function
 
Function Call_EBP() As Long
OPcode = OPcode + "FFD5"
End Function
 
Function Call_EDI() As Long
OPcode = OPcode + "FFD7"
End Function
 
Function Call_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "FF15" + Int2Hex(i, 8)
End Function
 
Function Call_DWORD_Ptr_EAX() As Long
OPcode = OPcode + "FF10"
End Function
 
Function Call_DWORD_Ptr_EBX() As Long
OPcode = OPcode + "FF13"
End Function
 
'Cmp
'+++++++++++++++++++++++++++++++++++
Function Cmp_EAX(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "83F8" + Int2Hex(i, 2)
Else
OPcode = OPcode + "3D" + Int2Hex(i, 8)
End If
End Function
 
Function Cmp_EAX_EDX() As Long
OPcode = OPcode + "3BC2"
End Function
 
Function Cmp_EAX_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "3B05" + Int2Hex(i, 8)
End Function
 
Function Cmp_DWORD_Ptr_EAX(i As Long) As Long
OPcode = OPcode + "3905" + Int2Hex(i, 8)
End Function
 
'DEC
'+++++++++++++++++++++++++++++++++++
Function Dec_EAX() As Long    '核对
OPcode = OPcode + "48"
End Function
 
Function Dec_EBX() As Long
OPcode = OPcode + "4B"
End Function
 
Function Dec_ECX() As Long
OPcode = OPcode + "49"
End Function
 
Function Dec_EDX() As Long
OPcode = OPcode + "4A"
End Function
 
'Idiv
'+++++++++++++++++++++++++++++++++++
Function Idiv_EAX() As Long
OPcode = OPcode + "F7F8"
End Function
 
Function Idiv_EBX() As Long
OPcode = OPcode + "F7FB"
End Function
 
Function Idiv_ECX() As Long
OPcode = OPcode + "F7F9"
End Function
 
Function Idiv_EDX() As Long
OPcode = OPcode + "F7FA"
End Function
 
'Imul
'+++++++++++++++++++++++++++++++++++
Function Imul_EAX_EDX() As Long
OPcode = OPcode + "0FAFC2"
End Function
 
Function Imul_EAX(i As Long) As Long
OPcode = OPcode + "6BC0" + Int2Hex(i, 2)
End Function
 
Function ImulB_EAX(i As Long) As Long
OPcode = OPcode + "69C0" + Int2Hex(i, 8)
End Function
 
'INC
'+++++++++++++++++++++++++++++++++++
Function Inc_EAX() As Long
OPcode = OPcode + "40"
End Function
 
Function Inc_EBX() As Long
OPcode = OPcode + "43"
End Function
 
Function Inc_ECX() As Long
OPcode = OPcode + "41"
End Function
 
Function Inc_EDX() As Long
OPcode = OPcode + "42"
End Function
 
Function Inc_EDI() As Long
OPcode = OPcode + "47"
End Function
 
Function Inc_ESI() As Long
OPcode = OPcode + "46"
End Function
 
Function Inc_DWORD_Ptr_EAX() As Long
OPcode = OPcode + "FF00"
End Function
 
Function Inc_DWORD_Ptr_EBX() As Long
OPcode = OPcode + "FF03"
End Function
 
Function Inc_DWORD_Ptr_ECX() As Long
OPcode = OPcode + "FF01"
End Function
 
Function Inc_DWORD_Ptr_EDX() As Long
OPcode = OPcode + "FF02"
End Function
 
'JMP/JE/JNE
'+++++++++++++++++++++++++++++++++++
Function JMP_EAX() As Long
OPcode = OPcode + "FFE0"
End Function
 
'Mov
Function Mov_DWORD_Ptr_EAX(i As Long) As Long  'mov[xxx],eax
OPcode = OPcode + "A3" + Int2Hex(i, 8)
End Function
 
Function Mov_EAX(i As Long) As Long   '核对
OPcode = OPcode + "B8" + Int2Hex(i, 8)
End Function
 
Function Mov_EBX(i As Long) As Long   '核对
OPcode = OPcode + "BB" + Int2Hex(i, 8)
End Function
 
Function Mov_ECX(i As Long) As Long
OPcode = OPcode + "B9" + Int2Hex(i, 8)
End Function
 
Function Mov_EDX(i As Long) As Long
OPcode = OPcode + "BA" + Int2Hex(i, 8)
End Function
 
Function Mov_ESI(i As Long) As Long
OPcode = OPcode + "BE" + Int2Hex(i, 8)
End Function
 
Function Mov_ESP(i As Long) As Long
OPcode = OPcode + "BC" + Int2Hex(i, 8)
End Function
 
Function Mov_EBP(i As Long) As Long
OPcode = OPcode + "BD" + Int2Hex(i, 8)
End Function
 
Function Mov_EDI(i As Long) As Long
OPcode = OPcode + "BF" + Int2Hex(i, 8)
End Function
 
Function Mov_EBX_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "8B1D" + Int2Hex(i, 8)
End Function
 
Function Mov_ECX_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "8B0D" + Int2Hex(i, 8)
End Function
 
Function Mov_EAX_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "A1" + Int2Hex(i, 8)
End Function
 
Function Mov_EDX_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "8B15" + Int2Hex(i, 8)
End Function
 
Function Mov_ESI_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "8B35" + Int2Hex(i, 8)
End Function
 
Function Mov_ESP_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "8B25" + Int2Hex(i, 8)
End Function
 
Function Mov_EBP_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "8B2D" + Int2Hex(i, 8)
End Function
 
Function Mov_EAX_DWORD_Ptr_EAX() As Long
OPcode = OPcode + "8B00"
End Function
 
Function Mov_EAX_DWORD_Ptr_EBP() As Long
OPcode = OPcode + "8B4500"
End Function
 
Function Mov_EAX_DWORD_Ptr_EBX() As Long
OPcode = OPcode + "8B03"
End Function
 
Function Mov_EAX_DWORD_Ptr_ECX() As Long
OPcode = OPcode + "8B01"
End Function
 
Function Mov_EAX_DWORD_Ptr_EDX() As Long
OPcode = OPcode + "8B02"
End Function
 
Function Mov_EAX_DWORD_Ptr_EDI() As Long
OPcode = OPcode + "8B07"
End Function
 
Function Mov_EAX_DWORD_Ptr_ESP() As Long
OPcode = OPcode + "8B0424"
End Function
 
Function Mov_EAX_DWORD_Ptr_ESI() As Long
OPcode = OPcode + "8B06"
End Function
 
Function Mov_EAX_DWORD_Ptr_EAX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B40" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B80" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EAX_DWORD_Ptr_ESP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B4424" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B8424" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EAX_DWORD_Ptr_EBX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B43" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B83" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EAX_DWORD_Ptr_ECX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B41" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B81" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EAX_DWORD_Ptr_EDX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B42" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B82" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EAX_DWORD_Ptr_EDI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B47" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B87" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EAX_DWORD_Ptr_EBP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B45" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B85" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EAX_DWORD_Ptr_ESI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B46" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B86" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EBX_DWORD_Ptr_EAX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B58" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B98" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EBX_DWORD_Ptr_ESP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B5C24" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B9C24" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EBX_DWORD_Ptr_EBX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B5B" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B9B" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EBX_DWORD_Ptr_ECX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B59" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B99" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EBX_DWORD_Ptr_EDX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B5A" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B9A" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EBX_DWORD_Ptr_EDI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B5F" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B9F" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EBX_DWORD_Ptr_EBP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B5D" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B9D" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EBX_DWORD_Ptr_ESI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B5E" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B9E" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_ECX_DWORD_Ptr_EAX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B48" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B88" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_ECX_DWORD_Ptr_ESP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B4C24" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B8C24" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_ECX_DWORD_Ptr_EBX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B4B" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B8B" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_ECX_DWORD_Ptr_ECX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B49" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B89" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_ECX_DWORD_Ptr_EDX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B4A" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B8A" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_ECX_DWORD_Ptr_EDI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B4F" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B8F" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_ECX_DWORD_Ptr_EBP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B4D" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B8D" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_ECX_DWORD_Ptr_ESI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B4E" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B8E" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EDX_DWORD_Ptr_EAX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B50" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B90" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EDX_DWORD_Ptr_ESP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B5424" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B9424" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EDX_DWORD_Ptr_EBX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B53" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B93" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EDX_DWORD_Ptr_ECX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B51" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B91" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EDX_DWORD_Ptr_EDX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B52" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B92" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EDX_DWORD_Ptr_EDI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B57" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B97" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EDX_DWORD_Ptr_EBP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B55" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B95" + Int2Hex(i, 8)
End If
End Function
 
Function Mov_EDX_DWORD_Ptr_ESI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B56" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8B96" + Int2Hex(i, 8)     '核实
End If
End Function
 
Function Mov_EBX_DWORD_Ptr_EAX() As Long
OPcode = OPcode + "8B18"
End Function
 
Function Mov_EBX_DWORD_Ptr_EBP() As Long
OPcode = OPcode + "8B5D00"
End Function
 
Function Mov_EBX_DWORD_Ptr_EBX() As Long
OPcode = OPcode + "8B1B"
End Function
 
Function Mov_EBX_DWORD_Ptr_ECX() As Long
OPcode = OPcode + "8B19"
End Function
 
Function Mov_EBX_DWORD_Ptr_EDX() As Long
OPcode = OPcode + "8B1A"
End Function
 
Function Mov_EBX_DWORD_Ptr_EDI() As Long
OPcode = OPcode + "8B1F"
End Function
 
Function Mov_EBX_DWORD_Ptr_ESP() As Long
OPcode = OPcode + "8B1C24"
End Function
 
Function Mov_EBX_DWORD_Ptr_ESI() As Long
OPcode = OPcode + "8B1E"
End Function
Function Mov_ECX_DWORD_Ptr_EAX() As Long
OPcode = OPcode + "8B08"
End Function
 
Function Mov_ECX_DWORD_Ptr_EBP() As Long
OPcode = OPcode + "8B4D00"
End Function
 
Function Mov_ECX_DWORD_Ptr_EBX() As Long
OPcode = OPcode + "8B0B"
End Function
 
Function Mov_ECX_DWORD_Ptr_ECX() As Long
OPcode = OPcode + "8B09"
End Function
 
Function Mov_ECX_DWORD_Ptr_EDX() As Long
OPcode = OPcode + "8B0A"
End Function
 
Function Mov_ECX_DWORD_Ptr_EDI() As Long
OPcode = OPcode + "8B0F"
End Function
 
Function Mov_ECX_DWORD_Ptr_ESP() As Long
OPcode = OPcode + "8B0C24"
End Function
 
Function Mov_ECX_DWORD_Ptr_ESI() As Long
OPcode = OPcode + "8B0E"
End Function
 
Function Mov_EDX_DWORD_Ptr_EAX() As Long
OPcode = OPcode + "8B10"
End Function
 
Function Mov_EDX_DWORD_Ptr_EBP() As Long
OPcode = OPcode + "8B5500"
End Function
 
Function Mov_EDX_DWORD_Ptr_EBX() As Long
OPcode = OPcode + "8B13"
End Function
 
Function Mov_EDX_DWORD_Ptr_ECX() As Long
OPcode = OPcode + "8B11"
End Function
 
Function Mov_EDX_DWORD_Ptr_EDX() As Long
OPcode = OPcode + "8B12"
End Function
 
Function Mov_EDX_DWORD_Ptr_EDI() As Long
OPcode = OPcode + "8B17"
End Function
 
Function Mov_EDX_DWORD_Ptr_ESI() As Long
OPcode = OPcode + "8B16"
End Function
 
Function Mov_EDX_DWORD_Ptr_ESP() As Long
OPcode = OPcode + "8B1424"
End Function
 
Function Mov_EAX_EBP() As Long
OPcode = OPcode + "8BC5"
End Function
 
Function Mov_EAX_EBX() As Long
OPcode = OPcode + "8BC3"
End Function
 
Function Mov_EAX_ECX() As Long
OPcode = OPcode + "8BC1"
End Function
 
Function Mov_EAX_EDI() As Long
OPcode = OPcode + "8BC7"
End Function
 
Function Mov_EAX_EDX() As Long
OPcode = OPcode + "8BC2"
End Function
 
Function Mov_EAX_ESI() As Long
OPcode = OPcode + "8BC6"
End Function
 
Function Mov_EAX_ESP() As Long
OPcode = OPcode + "8BC4"
End Function
 
Function Mov_EBX_EBP() As Long
OPcode = OPcode + "8BDD"
End Function
 
Function Mov_EBX_EAX() As Long
OPcode = OPcode + "8BD8"
End Function
 
Function Mov_EBX_ECX() As Long
OPcode = OPcode + "8BD9"
End Function
 
Function Mov_EBX_EDI() As Long
OPcode = OPcode + "8BDF"
End Function
 
Function Mov_EBX_EDX() As Long
OPcode = OPcode + "8BDA"
End Function
 
Function Mov_EBX_ESI() As Long
OPcode = OPcode + "8BDE"
End Function
 
Function Mov_EBX_ESP() As Long
OPcode = OPcode + "8BDC"
End Function
 
Function Mov_ECX_EBP() As Long
OPcode = OPcode + "8BCD"
End Function
 
Function Mov_ECX_EAX() As Long
OPcode = OPcode + "8BC8"
End Function
 
Function Mov_ECX_EBX() As Long
OPcode = OPcode + "8BCB"
End Function
 
Function Mov_ECX_EDI() As Long
OPcode = OPcode + "8BCF"
End Function
 
Function Mov_ECX_EDX() As Long
OPcode = OPcode + "8BCA"
End Function
 
Function Mov_ECX_ESI() As Long
OPcode = OPcode + "8BCE"
End Function
 
Function Mov_ECX_ESP() As Long
OPcode = OPcode + "8BCC"
End Function
 
Function Mov_EDX_EBP() As Long
OPcode = OPcode + "8BD5"
End Function
 
Function Mov_EDX_EBX() As Long
OPcode = OPcode + "8BD3"
End Function
 
Function Mov_EDX_ECX() As Long
OPcode = OPcode + "8BD1"
End Function
 
Function Mov_EDX_EDI() As Long
OPcode = OPcode + "8BD7"
End Function
 
Function Mov_EDX_EAX() As Long
OPcode = OPcode + "8BD0"
End Function
 
Function Mov_EDX_ESI() As Long
OPcode = OPcode + "8BD6"
End Function
 
Function Mov_EDX_ESP() As Long
OPcode = OPcode + "8BD4"
End Function
 
Function Mov_ESI_EBP() As Long
OPcode = OPcode + "8BF5"
End Function
 
Function Mov_ESI_EBX() As Long
OPcode = OPcode + "8BF3"
End Function
 
Function Mov_ESI_ECX() As Long
OPcode = OPcode + "8BF1"
End Function
 
Function Mov_ESI_EDI() As Long
OPcode = OPcode + "8BF7"
End Function
 
Function Mov_ESI_EAX() As Long
OPcode = OPcode + "8BF0"
End Function
 
Function Mov_ESI_EDX() As Long
OPcode = OPcode + "8BF2"
End Function
 
Function Mov_ESI_ESP() As Long
OPcode = OPcode + "8BF4"
End Function
 
Function Mov_ESP_EBP() As Long
OPcode = OPcode + "8BE5"
End Function
 
Function Mov_ESP_EBX() As Long
OPcode = OPcode + "8BE3"
End Function
 
Function Mov_ESP_ECX() As Long
OPcode = OPcode + "8BE1"
End Function
 
Function Mov_ESP_EDI() As Long
OPcode = OPcode + "8BE7"
End Function
 
Function Mov_ESP_EAX() As Long
OPcode = OPcode + "8BE0"
End Function
 
Function Mov_ESP_EDX() As Long
OPcode = OPcode + "8BE2"
End Function
 
Function Mov_ESP_ESI() As Long
OPcode = OPcode + "8BE6"
End Function
 
Function Mov_EDI_EBP() As Long
OPcode = OPcode + "8BFD"
End Function
 
Function Mov_EDI_EAX() As Long
OPcode = OPcode + "8BF8"
End Function
 
Function Mov_EDI_EBX() As Long
OPcode = OPcode + "8BFB"
End Function
 
Function Mov_EDI_ECX() As Long
OPcode = OPcode + "8BF9"
End Function
 
Function Mov_EDI_EDX() As Long
OPcode = OPcode + "8BFA"
End Function
 
Function Mov_EDI_ESI() As Long
OPcode = OPcode + "8BFE"
End Function
 
Function Mov_EDI_ESP() As Long
OPcode = OPcode + "8BFC"
End Function
Function Mov_EBP_EDI() As Long
OPcode = OPcode + "8BDF"
End Function
 
Function Mov_EBP_EAX() As Long
OPcode = OPcode + "8BE8"
End Function
 
Function Mov_EBP_EBX() As Long
OPcode = OPcode + "8BEB"
End Function
 
Function Mov_EBP_ECX() As Long
OPcode = OPcode + "8BE9"
End Function
 
Function Mov_EBP_EDX() As Long
OPcode = OPcode + "8BEA"
End Function
 
Function Mov_EBP_ESI() As Long
OPcode = OPcode + "8BEE"
End Function
 
Function Mov_EBP_ESP() As Long
OPcode = OPcode + "8BEC"
End Function
'Push
'+++++++++++++++++++++++++++++++++++
Function Push(i As Long) As Long  '已核对
If i <= 255 Then
OPcode = OPcode + "6A" + Int2Hex(i, 2)
Else
OPcode = OPcode + "68" + Int2Hex(i, 8)
End If
End Function
 
Function Push_DWORD_Ptr(i As Long) As Long
OPcode = OPcode + "FF35" + Int2Hex(i, 8)
End Function
 
Function Push_EAX() As Long    '核对
OPcode = OPcode + "50"
End Function
 
Function Push_ECX() As Long
OPcode = OPcode + "51"
End Function
 
Function Push_EDX() As Long
OPcode = OPcode + "52"
End Function
 
Function Push_EBX() As Long
OPcode = OPcode + "53"
End Function
Function Push_ESP() As Long
OPcode = OPcode + "54"
End Function
 
Function Push_EBP() As Long
OPcode = OPcode + "55"
End Function
 
Function Push_ESI() As Long
OPcode = OPcode + "56"
End Function
 
Function Push_EDI() As Long
OPcode = OPcode + "57"
End Function
'LEA
Function Lea_EAX_DWORD_Ptr_EAX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D40" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D80" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EAX_DWORD_Ptr_EBX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D43" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D83" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EAX_DWORD_Ptr_ECX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D41" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D81" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EAX_DWORD_Ptr_EDX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D42" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D82" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EAX_DWORD_Ptr_ESI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D46" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D86" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EAX_DWORD_Ptr_ESP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D40" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D80" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EAX_DWORD_Ptr_EBP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D4424" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D8424" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EAX_DWORD_Ptr_EDI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D47" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D87" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EBX_DWORD_Ptr_EAX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D58" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D98" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EBX_DWORD_Ptr_ESP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D5C24" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D9C24" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EBX_DWORD_Ptr_EBX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D5B" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D9B" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EBX_DWORD_Ptr_ECX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D59" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D99" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EBX_DWORD_Ptr_EDX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D5A" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D9A" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EBX_DWORD_Ptr_EDI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D5F" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D9F" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EBX_DWORD_Ptr_EBP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D5D" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D9D" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EBX_DWORD_Ptr_ESI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D5E" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D9E" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_ECX_DWORD_Ptr_EAX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D48" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D88" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_ECX_DWORD_Ptr_ESP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D4C24" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D8C24" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_ECX_DWORD_Ptr_EBX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D4B" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D8B" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_ECX_DWORD_Ptr_ECX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D49" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D89" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_ECX_DWORD_Ptr_EDX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D4A" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D8A" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_ECX_DWORD_Ptr_EDI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D4F" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D8F" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_ECX_DWORD_Ptr_EBP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D4D" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D8D" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_ECX_DWORD_Ptr_ESI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D4E" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D8E" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EDX_DWORD_Ptr_EAX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D50" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D90" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EDX_DWORD_Ptr_ESP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D5424" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D9424" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EDX_DWORD_Ptr_EBX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D53" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D93" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EDX_DWORD_Ptr_ECX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D51" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D91" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EDX_DWORD_Ptr_EDX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D52" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D92" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EDX_DWORD_Ptr_EDI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D57" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D97" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EDX_DWORD_Ptr_EBP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D55" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D95" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EDX_DWORD_Ptr_ESI_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D56" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D96" + Int2Hex(i, 8)
End If
End Function
 
'POP
Function Pop_EAX() As Long
OPcode = OPcode + "58"
End Function
 
Function Pop_EBX() As Long
OPcode = OPcode + "5B"
End Function
 
Function Pop_ECX() As Long
OPcode = OPcode + "59"
End Function
 
Function Pop_EDX() As Long
OPcode = OPcode + "5A"
End Function
  
Function Pop_ESI() As Long   '核对
OPcode = OPcode + "5E"
End Function
 
Function Pop_ESP() As Long
OPcode = OPcode + "5C"
End Function
 
Function Pop_EDI() As Long  '核对
OPcode = OPcode + "5F"
End Function
 
Function Pop_EBP() As Long
OPcode = OPcode + "5D"
End Function
 
Function Mov_DWORD_Ptr_EAX_EAX(i As Long) As Long 'mov[eax],eax
OPcode = OPcode + "8900" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_EAX_EBX(i As Long) As Long 'mov[eax],ebx
OPcode = OPcode + "8918" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_EAX_ECX(i As Long) As Long 'mov[eax],ecx
OPcode = OPcode + "8908" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_EAX_EDX(i As Long) As Long 'mov[eax],edx
OPcode = OPcode + "8910" + Int2Hex(i, 8)
End Function
 
 
Function Mov_DWORD_Ptr_EBX_EAX(i As Long) As Long 'mov[ebx],eax
OPcode = OPcode + "8903" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_EBX_EBX(i As Long) As Long 'mov[ebx],ebx
OPcode = OPcode + "891B" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_EBX_ECX(i As Long) As Long 'mov[ebx],ecx
OPcode = OPcode + "890B" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_EBX_EDX(i As Long) As Long 'mov[ebx],edx
OPcode = OPcode + "8913" + Int2Hex(i, 8)
End Function
 
 
Function Mov_DWORD_Ptr_ECX_EAX(i As Long) As Long 'mov[ecx],eax
OPcode = OPcode + "8901" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_ECX_EBX(i As Long) As Long 'mov[ecx],ebx
OPcode = OPcode + "8919" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_ECX_ECX(i As Long) As Long 'mov[ecx],ecx
OPcode = OPcode + "8909" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_ECX_EDX(i As Long) As Long 'mov[ecx],edx
OPcode = OPcode + "8911" + Int2Hex(i, 8)
End Function
 
 
Function Mov_DWORD_Ptr_EDX_EAX(i As Long) As Long 'mov[edx],eax
OPcode = OPcode + "8902" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_EDX_EBX(i As Long) As Long 'mov[edx],ebx
OPcode = OPcode + "891A" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_EDX_ECX(i As Long) As Long 'mov[edx],ecx
OPcode = OPcode + "890A" + Int2Hex(i, 8)
End Function
 
Function Mov_DWORD_Ptr_EDX_EDX(i As Long) As Long 'mov[edx],edx
OPcode = OPcode + "8912" + Int2Hex(i, 8)
End Function
 
'新增几个
Function Mov_ESI_DWORD_Ptr_EAX_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8B70" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8BB0" + Int2Hex(i, 8)
End If
End Function
 
Function MOV_PTR_ECX_ADD_EAX(i As Long) As Long  'mov    [ecx+$20], eax
If i <= 255 Then
OPcode = OPcode + "8941" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8981" + Int2Hex(i, 8)
End If
End Function
 
 
'修改两个
Function Lea_EAX_DWORD_ESP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D4424" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D8424" + Int2Hex(i, 8)
End If
End Function
 
Function Lea_EAX_DWORD_EBP_Add(i As Long) As Long
If i <= 255 Then
OPcode = OPcode + "8D45" + Int2Hex(i, 2)
Else
OPcode = OPcode + "8D85" + Int2Hex(i, 8)
End If
End Function

