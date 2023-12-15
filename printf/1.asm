; 目标：put_string函数

code_seg_sel        equ     0x08               ;0b00000000_00001_000，代码段选择子
stack_seg_sel       equ     0x10               ;0b00000000_00010_000，栈段选择子
all_data_seg_sel    equ     0x18               ;0b00000000_00011_000，4GB数据段选择子


SECTION MBR_CODE
    mov ax, cs
    mov ss, ax
    mov sp, 0x7c00

    mov bx, [cs:pgdt+0x7c00+0x02]
    ;2.向GDT写入段描述符
    ;#0空描述符，CPU的规定
    mov dword [bx+0x00], 0x0000_0000
    mov dword [bx+0x04], 0x0000_0000 
    ;#1 保护模式下的代码段描述符                   ;TODO：大坑啊，SECTION vstart=0x7c00，此时代码段描述符的基地址应该是0，那段界限也该调整了，不能是0x7c00
    mov dword [bx+0x08], 0x7c00_01ff            
    mov dword [bx+0x0c], 0x0040_9800
    ;#2 栈段                                     ;TODO：向上扩展的栈段。段的扩展方向只是用来确定边界，与栈的push方向无关
    mov dword [bx+0x10], 0x7c00_fffe
    mov dword [bx+0x14], 0x00cf_9600
    ;#3 0-4GB全局数据描述符
    mov dword [bx+0x18], 0x0000_FFFF
    mov dword [bx+0x1c], 0x00CF_9200

    mov word [cs:pgdt+0x7c00], 4*8-1            ;界限值=表总字节数-1，也等于最后一字节偏移量（一共两个描述符，一个描述符占8字节）
    ;3.将GDT首地址和界限载入GDTR
    lgdt [cs:pgdt+0x7c00]

    ;4.打开A20地址线
    in al,0x92                                   ;南桥芯片内的端口
    or al,0000_0010B
    out 0x92,al                                  ;打开A20

    ;5.屏蔽外中断
    cli                                          ;保护模式下中断机制尚未建立，应禁止中断


    ;6.进入保护模式
    mov eax, cr0
    or  eax, 1
    mov cr0, eax


    xchg bx, bx

    ;7.刷新CS段描述符高速缓存器，让默认操作尺寸为32位，
    jmp code_seg_sel:flush                      ;CR0 PE位控制寻找方式，当前PE=1，描述符寻址
                                                ;段的位置，现在是选择子了

    ;8.保护模式，寻址模式将发生改变，不再是段地址*16+有效地址得到物理地址；段寄存器的内容现在是选择子，用来在GDT中选择一个描述符放入段描述符高速缓冲区
    [bits 32]  
 flush: 
    ;刷新其他段的选择器
    mov ax, stack_seg_sel
    mov ss, ax
    xor esp, esp
    mov ax, all_data_seg_sel
    mov ds, ax

    mov ebx, msg1 + 0x7c00                      ;TODO：ds是0-4GB选择子，基地址是0。偏移量可要注意了
    call put_string

    mov ebx, msg2 + 0x7c00
    call put_string

    jmp $
    hlt                                         ;屏蔽可屏蔽中断，低功耗将不会被打破。
    
    
    pgdt    dw 00                               ;GDT界限值=表总字节数-1，也等于最后一字节偏移量（一共两个描述符，一个描述符占8字节）
            dd 0x0000_7e00                      ;全局描述符表，安装起始位置   
    
    msg1    db 'GaoWuJie2004', 0x0d,0x0a
            db '@QinBeiLei',   0x0d,0x0a
            db '@2019-10-13',  0

    msg2:   db 0x0d,0x0a, 0x0d,0x0a, 0x0d,0x0a,
            db '2023-12-12 19:19:00', 0



;------------------------------------------------------------------------
put_char:                                       ;输入：cl=ASCII码

    video_card_index_port   equ     0x3d4       ;显卡的功能索引寄存器端口
    video_card_data_port    equ     0x3d5       ;显卡的数据寄存器端口
    cursor_h8_port          equ     0x0e        ;光标寄存器索引端口，高8位
    cursor_l8_port          equ     0x0f        ;光标寄存器索引端口，低8位， 00表示光标在第0行0列，80表示光标在1行0列，这里的值其实是偏移量。行、列从0开始。

    CR                      equ     0x0d        ;CR回车ASCII码值，当前首行
    LF                      equ     0x0a        ;LF换行ASCII码值，垂直下行，不是下行的首行
    
    push edi
    push esi
    push ax
    push bx
    push dx
    
    push ds
    push es

 ;取当前光标位置，BX=光标位置
 .get_cursor:                                       
    mov dx, video_card_index_port
    mov al, cursor_l8_port
    out dx, al                                  
    mov dx, video_card_data_port
    in al, dx
    mov bl, al                                  ;低8位，光标位置获取到，放入bl暂存

    mov dx, video_card_index_port
    mov al, cursor_h8_port
    out dx, al                                  
    mov dx, video_card_data_port
    in al, dx                                   
    mov bh, al                                  ;高8位，光标位置获取到，放入bh暂存
 

 ;选择跳转
 .process_jmp:
    cmp cl, CR
    je .put_cr                                  ;相等则跳转，是回车
    
    cmp cl, LF
    je .put_lf                                  ;相等则跳转，是换行

    jmp .put_other

 ;首行，BX=光标为当前首行
 .put_cr:
    mov dx, 0
    mov ax, bx
    mov bl, 80
    div bl                                      ;dx:ax/80=ax....dx， 

    mul bl                                      ;al*bl=ax    ax是当前行的首行索引，得再乘80，才是偏移量
    mov bx, ax
    jmp .set_cursor

 ;垂直下一行, BX=垂直下一行
 .put_lf:
    add bx, 80                                  
    jmp .scroll_check

 ;可显示的字符，cl=字符值
 .put_other:               
    mov ax, all_data_seg_sel
    mov ds, ax                     
    mov esi, 0xb_8000
    xor eax, eax                                ;eax的高位可能不干净，清楚
    mov ax, bx
    shl eax, 1                                  ;乘2
    mov [esi + eax], cl    
    add bx, 1                                   ;推进光标

 ;光标越界检查，是否需要滚动屏幕
 .scroll_check:
    cmp bx, 2000                                ;25*80=2000, 最后一个偏移量1999
    jl .set_cursor                              ;<则跳转

 ;接下来是>=，滚动屏幕
 .scroll_screnn:                                ;第1行移到第0行，。。。。最后一行置空。去除最后1行，共24行，一共24*80个字符
    sub bx, 80                                  ;向上移了一行，光标也需要对应移动
    ;24*80/4, [es:edi] <- [ds:esi]
    mov ax, ds
    mov es, ax
    mov edi, 0
    mov esi, 160
    mov ecx, 24*80*2/4                          ;应该是字符数*2才是字节数
    cld                                         ;DF=0，edi、esi方向增加
    rep movsd                                   ;edi、esi步长是4（双字）rep movsd，32位保护模式时，使用的是ecx
    ;最后一行置空（黑底白字空格字符）
    mov ecx, 40                                 ;本来是要循环160次的，但现在每次四个字节操。 TODO:Think-CPU和数据总线拥有更宽的数据通路，所执行的指令个数就会减少，充分体现了性能的优化
  .loop_cls:
    mov dword [esi], 0x07200720                 ;在上一步rep movsd结束时，esi偏移量是24*80*2，符合预期。（行从0开始）
    add esi, 2 
    loop .loop_cls        

 ;向外设写数据，BX=光标位置
 .set_cursor:                                   
    mov dx, video_card_index_port
    mov al, cursor_l8_port
    out dx, al
    mov dx, video_card_data_port
    mov al, bl 
    out dx, al                                   ;文本模式光标寄存器低8位

    mov dx, video_card_index_port
    mov al, cursor_h8_port
    out dx, al
    mov dx, video_card_data_port
    mov al, bh
    out dx, al                                   ;文本模式光标寄存器高8位
    
 .put_char_return:
    pop es
    pop ds

    pop dx
    pop bx
    pop ax
    pop esi
    pop edi
    ret

put_string:                                     ;输入：ds:ebx字符串首地址，0x00结束
                                                ;输出：无
    push ecx
    push ebx

 .each_char:                                 
    mov cl, [ebx]
    or cl, cl                                   ;为了性能，or 操作数全是寄存器，并且不会改变or的值，影响ZF标志位（其他标志位的影响不确定）TODO-Optimize：操作数全是寄存器，没有立即数，也没有内存寻址，所以很快，而且指令所占字节数少
    jz .put_string_return                       ;等于零则跳转（ZF=1)
    call put_char                               ;输入：cl=ASCII码
    inc ebx
    jmp .each_char

 .put_string_return:
    pop ebx
    pop eax
    ret
;------------------------------------------------------------------------

    times 510-($-$$) db 0x00
    db 0x55, 0xaa
