; 操作光标

; vstart在CPU内存空间寻址时会自动加上这个前缀;
; ds:[标号] -> 编译为: ds:[标号+0x7c00]
section code vstart=0x7c00
    ; 编译时常量，编译后不占汇编偏移量（汇编地址）
    CRT_L equ 0x3D4
    CRT_H equ 0x3D5
    ; 设置光标位置为第10行第5列
    ; mov al, 0x0F ; 设置光标位置低字节的索引号
    ; out 0x3D4, al ; 向CRT控制器的索引寄存器端口发送低字节索引号
    ; mov al, 0x04 ; 设置光标位置低字节为4（从0开始计数）
    ; out 0x3D5, al ; 向CRT控制器的数据寄存器端口发送低字节数据

    ; mov al, 0x0E ; 设置光标位置高字节的索引号
    ; out 0x3D4, al ; 向CRT控制器的索引寄存器端口发送高字节索引号
    ; mov al, 0x09 ; 设置光标位置高字节为9（从0开始计数）
    ; out 0x3D5, al ; 向CRT控制器的数据寄存器端口发送高字节数据

    mov al, 0x0F
    mov dx, CRT_L
    out dx, al
    mov dx, CRT_H
    mov al, 0x04
    out dx, al

    mov al,0x0E
    mov dx, CRT_L
    out dx, al
    mov al, 0x09
    mov dx, CRT_H
    out dx, al

    jmp $

    times (510-($-$$)) db 0
    dw 0xaa55

