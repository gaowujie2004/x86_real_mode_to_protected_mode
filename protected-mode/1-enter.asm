; 目标：进入保护模式

; 下面是具体的步骤：
; 1. 写内存段的描述符表，安装全局描述符表，加载GDTR
; 2. 设置CRO控制寄存器PE位为1，从而进入保护模式
; 3. 刷新各个段描述符高速缓存器内容，这些内容即描述符表，决定了内存的访问长度的限制、以及默认操作尺寸
; 说明：第三步很重要


SECTION MBR_CODE
    ;此处代码还是实模式，NASM默认操作尺寸是16位，即[bits 16]，标识CPU的默认操作尺寸是16位
    ;实模式下CPU工作机制，内存寻址还是：段寄存器*16+段内偏移量（有效地址），即使我们的CPU是32位的，地址线至少32根，只要在实模式下，它就这样工作，只使用其中的20根地址线
    ;1.得到GDT的段地址、偏移地址
    [bits 16]
    mov edx, 0
    mov eax, [cs:gpt_base+0x7c00]   ;32位CPU，当然可以使用32位通用寄存器；实模式与此无关。 实模式决定的是内存的保护和内存寻址范围
    mov ebx, 16
    div ebx            ; ax:dx / r/m16 = ax ...... dx      ax / r/m8 = al ...... ah
                       ; ax*r/m16 = dx:ax      al*r/m8 = ax
    mov ds, ax         ;只需要eax中的低16位
    mov bx, dx


    ;2.向GDT写入段描述符
    ;#0空描述符，CPU的规定
    mov dword [bx+0x00], 0x0000_0000
    mov dword [bx+0x04], 0x0000_0000 
    ;#1号，文本模式显存字符缓冲区，0xb_8000
    mov dword [bx+0x08], 0x004F_92B8
    mov dword [bx+0x0c], 0x8000_FFFF

    mov word [cs:gpt_limit], 15; 界限值=表总字节数-1，也等于最后一字节偏移量（一共两个描述符，一个描述符占8字节）

    ;3.将GDT首地址和界限载入GDTR
    lgdt [cs:gpt_limit+0x7c00]

    ;4.进入保护模式
    mov eax, cr0
    or  eax, 0b0000_0001
    mov cr0, eax

    

    






    


    jmp $
    
    
    gpt_limit dw 00             ;界限值=表总字节数-1，也等于最后一字节偏移量（一共两个描述符，一个描述符占8字节）
    gpt_base  dd 0x0000_7e00    ;全局描述符表，安装起始位置
    

    times 510-($-$$) db 0x00
    db 0x55, 0xaa
