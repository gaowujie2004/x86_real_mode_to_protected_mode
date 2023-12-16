; 目标：读取0号硬盘数据
;TODO-Tips      需要重点提示
;TODO-BUG       当初的bug
;TODO-Optimize  优化点

code_seg_sel        equ     0x08               ;0b00000000_00001_000，代码段选择子
stack_seg_sel       equ     0x10               ;0b00000000_00010_000，栈段选择子
all_data_seg_sel    equ     0x18               ;0b00000000_00011_000，4GB数据段选择子

disk_hard_start_lba equ     50
disk_hard_buffer    equ     0x7e00

SECTION MBR_CODE
    mov ax, cs
    mov ss, ax
    mov sp, 0x7c00

    mov bx, [cs:pgdt+0x7c00+0x02]
    ;2.向GDT写入段描述符
    ;#0空描述符，CPU的规定
    mov dword [bx+0x00], 0x0000_0000
    mov dword [bx+0x04], 0x0000_0000 
    ;#1 保护模式下的代码段描述符                   ;TODO-BUG：大坑啊，SECTION vstart=0x7c00，此时代码段描述符的基地址应该是0，那段界限也该调整了，不能是0x7c00
    mov dword [bx+0x08], 0x7c00_01ff            
    mov dword [bx+0x0c], 0x0040_9800
    ;#2 栈段                                     ;TODO-Tips：向上扩展的栈段。段的扩展方向只是用来确定边界，与栈的push方向无关
    mov dword [bx+0x10], 0x6c00_0FFF             ;段长度0x1000、段界限：0xFFF，基地址：0x6c00
    mov dword [bx+0x14], 0x0040_9200
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



    ;7.刷新CS段描述符高速缓存器，让默认操作尺寸为32位，
    jmp code_seg_sel:flush                      ;CR0 PE位控制寻找方式，当前PE=1，描述符寻址
                                                ;段的位置，现在是选择子了

    ;8.保护模式，寻址模式将发生改变，不再是段地址*16+有效地址得到物理地址；段寄存器的内容现在是选择子，用来在GDT中选择一个描述符放入段描述符高速缓冲区
    [bits 32]  
 flush: 
    ;刷新其他段的选择器
    mov ax, stack_seg_sel
    mov ss, ax
    mov esp,0x1000                              ;TODO-Tips:栈段长度是0x1000
    mov ax, all_data_seg_sel
    mov ds, ax

    xchg bx, bx                                 ;TODO-Tips: Bochs的魔术断点，这条指令不会对bx做任何修改
    mov eax, disk_hard_start_lba
    mov ebx, disk_hard_buffer
    call read_disk_hard_0


    jmp $
    hlt                                         ;屏蔽可屏蔽中断，低功耗将不会被打破。
    
    
    pgdt    dw 00                               ;GDT界限值=表总字节数-1，也等于最后一字节偏移量（一共两个描述符，一个描述符占8字节）
            dd 0x0000_7e00                      ;全局描述符表，安装起始位置   
    


;------------------------------------------------------------------------
read_disk_hard_0:                               ;从硬盘读取一个逻辑扇区，0表示主硬盘
                                                ;输入： eax=起始逻辑扇区号（28bit）
                                                ;      ds:ebx=数据缓冲区起始地址
    disk_hard_data_port           equ 0x1f0     ;硬盘数据端口（16-bit）

    disk_hard_errcode_port        equ 0x1f1     ;硬盘错误码端口

    disk_hard_sectors_count_port  equ 0x1f2     ;读取硬盘扇区个数
                                                
    disk_hard_lba_1_port          equ 0x1f3     ;0-7位
    disk_hard_lba_2_port          equ 0x1f4     ;8-15位
    disk_hard_lba_3_port          equ 0x1f5     ;16-23位
    disk_hard_lba_4_port          equ 0x1f6     ;24-28位，高4位其他数据

    disk_hard_command_state_port  equ 0x1f7     ;硬盘命令/状态端口
    push eax
    push edx
    push ecx
    push ebx

    mov ecx, eax                                ;暂存eax

    ;读扇区个数
    mov dx, disk_hard_sectors_count_port
    mov al, 0x01
    out dx, al
    ;指定起始LBA(28-bit)
    mov dx, disk_hard_lba_1_port
    mov al, cl                                  ;ecx暂存eax
    out dx, al                                  ;LBA 0-7位

    mov dx, disk_hard_lba_2_port
    mov al, ch
    out dx, al                                  ;LBA 8-15位

    mov dx, disk_hard_lba_3_port
    shr eax, 16                                 ;EAX高16位是干净的。EAX的16-32位，移到AX寄存器
    out dx, al                                  ;LBA 16-23位

    mov dx, disk_hard_lba_4_port
    mov al, ah
    or al, 0B1110_0000
    and al, 0b1110_1111                         ;高4位
    out dx, al                                  ;LBA 16-23位

    ;请求读命令
    mov dx, disk_hard_command_state_port
    mov al, 0x20
    out dx, al

    ;循环等待硬盘
    mov dx, disk_hard_command_state_port
 .loop_wait:                                    ;TODO-Optimize：只能这样等待吗？CPU和硬盘的速度差异非常非常大，等待的过程CPU都能执行非常非常多条指令了。
    in al, dx
    test al, 0B0000_0001                        ;低1位如果是1，则说明有错误
    jnz .error                                  ;不等于0，跳转；低1位是1，有错误

    and al, 0B1000_1000                         ;不关注的位，清零
    cmp al, 0B0000_1000                         ;硬盘不忙&&硬盘数据已经准备OK
    jne .loop_wait                              ;jnz不等于，说明数据还不能读取                                                 

    ;目前只读一个扇区（512byte），一次2byte，共256次
    ;ds:ebx=数据缓冲区起始地址
    mov ecx, 256
    mov dx, disk_hard_data_port
 .loop_read:                                    ;TODO-Optimize：一次才传输2byte数据；硬盘控制器能不能也有一个Buffer，等它的Buffer满了，我再一次性读取？但CPU数据通路才4byte（32位CPU）还是得读取好多次。DMA 吗？？
    in ax, dx
    mov [ebx], ax
    add ebx, 2
    loop .loop_read
    jmp .return

 .error:                                        ;TODO-Todo：代办项，加载内核时，对错误打印。
 
 .return:
    pop ebx
    pop ecx
    pop edx 
    pop eax
    ret
;------------------------------------------------------------------------

    times 510-($-$$) db 0x00
    db 0x55, 0xaa
