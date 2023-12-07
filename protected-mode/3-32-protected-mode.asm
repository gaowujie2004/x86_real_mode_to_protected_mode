; 目标：进入32位保护模式，而不是16位保护模式
; 进入32位保护模式的秘诀在于：CS描述符高速缓存器D位=1，即要刷新它，让它重新加载描述符，而不是使用实模式或16位保护模式遗留下来的内容。



SECTION MBR_CODE
    ;此处代码还是实模式，NASM默认操作尺寸是16位，即[bits 16]，标识CPU的默认操作尺寸是16位
    ;实模式下CPU工作机制，内存寻址还是：段寄存器*16+段内偏移量（有效地址），即使我们的CPU是32位的，地址线至少32根，只要在实模式下，它就这样工作，只使用其中的20根地址线
    ;1.得到GDT的段地址、偏移地址
    mov ax, cs
    mov ss, ax
    mov sp, 0x7c00


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
    mov dword [bx+0x08], 0x8000_FFFF
    mov dword [bx+0x0c], 0x0040_920b
    ;创建#2描述符，保护模式下的代码段描述符
    mov dword [bx+0x10],0x7c00_01ff
    mov dword [bx+0x14],0x0040_9800


    ;导致崩溃
    ;mov word [cs:gpt_limit], 15; 界限值=表总字节数-1，也等于最后一字节偏移量（一共两个描述符，一个描述符占8字节）
    mov word [cs:gpt_limit+0x7c00], 23; 界限值=表总字节数-1，也等于最后一字节偏移量（一共两个描述符，一个描述符占8字节）

    ;3.将GDT首地址和界限载入GDTR
    lgdt [cs:gpt_limit+0x7c00]

    ;3.打开A20地址线
    in al,0x92                                   ;南桥芯片内的端口
    or al,0000_0010B
    out 0x92,al                                  ;打开A20
    ;3.屏蔽外中断
    cli                                          ;保护模式下中断机制尚未建立，应
                                                 ;禁止中断


    ;4.进入保护模式
    mov eax, cr0
    or  eax, 1
    mov cr0, eax


    ;4.刷新CS段描述符高速缓存器，让默认操作尺寸为32位，
    ; --------------------------重点---------------------------------进入flush代码以后，16位汇编指令将不再被兼容，可能出现问题。
    jmp 0b00000000_00010_000:flush           ;CR0 PE位控制寻找方式，当前PE=1，描述符寻址
                                             ;段的位置，现在是选择子了

    ;5.保护模式，寻址模式将发生改变，不再是段地址*16+有效地址得到真实地址；段寄存器的内容现在是选择子，用来在GDT中选择一个描述符放入段描述符高速缓冲区
    ;向字符缓冲区写入数据, 'GWJ@QBL'
    [bits 32]  
flush:   
    mov ax, 0b00000000_0000_1_000
    mov es, ax                                     ;es段寄存器，也叫es段选择器，现在处于保护模式，内容是选择子，它有3部分组成：1）描述符索引；2）TI位；3）
    ;mov es, 0x08 和偏移量对应
    mov byte [es:0x00], 'G'
    mov byte [es:0x01], 0b0100_0000

    mov byte [es:0x02], 'W'
    mov byte [es:0x03], 0b0100_0000
    
    mov byte [es:0x04], 'J'
    mov byte [es:0x05], 0b0100_0000
    
    mov byte [es:0x06], '@'
    mov byte [es:0x07], 0b0100_0000

    mov byte [es:0x08], 'Q'
    mov byte [es:0x09], 0b0100_0000

    mov byte [es:0x0a], 'B'
    mov byte [es:0x0b], 0b0100_0000

    mov byte [es:0x0c], 'L'
    mov byte [es:0x0d], 0b0100_0000


    ;屏蔽可屏蔽中断，低功耗将不会被打破。
    hlt
    
    
    gpt_limit dw 00             ;界限值=表总字节数-1，也等于最后一字节偏移量（一共两个描述符，一个描述符占8字节）
    gpt_base  dd 0x0000_7e00    ;全局描述符表，安装起始位置
    

    times 510-($-$$) db 0x00
    db 0x55, 0xaa
