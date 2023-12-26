      


      all_data_seg_sel    equ     0B00000000_00001_000      ;0x08，4GB数据段选择子
      mbr_code_seg_sel    equ     0B00000000_00010_000      ;0x10，初始化代码段（mbr）
      core_stack_seg_sel  equ     0B00000000_00011_000      ;0x18，初始化栈段（mbr、core）
      video_buf_seg_sel   equ     0B00000000_00100_000      ;0x20，初始化栈段（mbr、core）


      core_base_address equ 0x00040000                      ;内核程序的起始物理内存地址
      core_start_sector equ 0x00000001                      ;内核的起始逻辑扇区号 

SECTION MBR
      mov ax, cs
      mov ss, ax
      mov sp, 0x7c00

  .create_gdt:
      mov bx, [cs:pgdt+0x7c00+0x02]
      ;#0空描述符
      mov dword [bx+0x00], 0x0000_0000
      mov dword [bx+0x04], 0x0000_0000 
      ;#1 4GB数据段
      mov dword [bx+0x08], 0x0000_FFFF
      mov dword [bx+0x0c], 0x00CF_9200
      ;#2 初始代码段（mbr）
      mov dword [bx+0x10], 0x7c00_01FF          ;范围：0x7c00 - 0x7dff；Base=0x7c00；
      mov dword [bx+0x14], 0x0040_9800
      ;#3 初始栈段（mbr与内核）                                    
      mov dword [bx+0x18], 0x6c00_0FFF          ;范围：0x6c00 - 0x7bff；Base=0x6c00          
      mov dword [bx+0x1c], 0x0040_9200
      ;#4 文本模式缓冲区                                  
      mov dword [bx+0x20], 0x8000_7FFF          ;范围：0xb_8000 - 0xb_ffff；Base=0xb_8000
      mov dword [bx+0x24], 0x0040_920B

  .gdt_limt:
      mov word [cs:pgdt+0x7c00], 5*8-1          ;mbr代码段不可读，但此时还未进入保护模式，CS段描述符高速缓存器中基地址=0（预置的）

  .lgdt:
      lgdt [cs:pgdt+0x7c00]

  .a20_address:
      in al,0x92                                ;南桥芯片内的端口
      or al,0000_0010B
      out 0x92,al                               ;打开A20
  
  .disable_interrupt:
      cli                                       ;保护模式下中断机制与实模式不同，而且还未建立中断表

  .enter_protected_mode:
      mov eax, cr0
      or  eax, 1
      mov cr0, eax

      jmp mbr_code_seg_sel:.flush               ;CR0 PE位控制寻找方式，当前PE=1，描述符寻址
                                                ;段的位置，现在是选择子了


      [bits 32]  
  .flush:                                       ;刷新所有段寄存器，CS中D=1，32位保护模式
      mov ax, core_stack_seg_sel
      mov ss, ax
      mov esp,0x1000                            ;TODO-Tips：栈段长度是0x1000
      mov ax, all_data_seg_sel
      mov ds, ax

 .load_core:
      ;ds=4gb
      mov eax, core_start_sector
      mov ebx, core_base_address
      call read_disk_hard_0

      mov edx, 0
      mov eax, [core_base_address]
      mov ebx, 512                              
      div ebx                                   ;edx:eax / ecx = eax......edx

      or edx, edx
      jnz @1                                    ;不等于0，说明还有数据，商得+1，但已经读取过一次，扯平了
      dec eax
   @1:
      or eax, eax                               ;等于0说明，内核刚好512或少于512字节
      jz .setup_core_desc
      mov ecx, eax
      mov eax, core_start_sector
      mov ebx, core_base_address
   .core_read_more:
      inc eax
      add ebx, 512
      call read_disk_hard_0
      loop .core_read_more
      
 .setup_core_desc:                             ;安装内核段描述符,不先安装描述符，内存就无法访问                
 ;ds=4GB、edi=内核起始物理地址、esi=GDT起始物理地址
      mov edi, core_base_address
      mov esi, [0x7c00+pgdt+0x02]               ;GDT起始物理地址，不能使用cs，目前是只能执行，不能读

      ;1 内核公共代码段全局描述符
      mov eax, [edi+0x04]                       ;公共代码段起始汇编地址
      mov ebx, [edi+0x08]                       ;内核数据段起始汇编地址
      sub ebx, eax
      dec ebx                                   ;公共代码段界限
      add eax, edi                              ;公共代码段基地址，段起始汇编地址+内核起始物理地址=重定位后的物理地址
      mov ecx, 0x0040_9800                      ;公共代码段属性
                                                ; G DB L AVL=0100、段界限=0000 | P DPL S=1001、TYPE=1000（只执行）
      call make_gdt_descriptor                           
      mov [esi+0x28], eax
      mov [esi+0x2c], edx

      ;2 内核数据段全局描述符
      mov eax, [edi+0x08]                       ;内核数据段起始汇编地址
      mov ebx, [edi+0x0c]                       ;内核代码段起始汇编地址
      sub ebx, eax
      dec ebx                                   ;内核数据段界限
      add eax, edi                              ;内核数据段基地址，段起始汇编地址+内核起始物理地址=重定位后的物理地址
      mov ecx, 0x0040_9200                      ;内核数据段属性
                                                ; G DB L AVL=0100、段界限=0000 | P DPL S=1001、TYPE=0010（可读可写）
      call make_gdt_descriptor
      mov [esi+0x30], eax
      mov [esi+0x34], edx

      ;3 内核代码段全局描述符
      mov eax, [edi+0x0c]                       ;内核代码段起始汇编地址
      mov ebx, [edi+0x00]                       ;内核总字节数
      sub ebx, eax
      dec ebx                                   ;内核代码段界限
      add eax, edi                              ;内核代码段基地址，段起始汇编地址+内核起始物理地址=重定位后的物理地址
      mov ecx, 0x0040_9800                      ;内核代码段属性
                                                ; G DB L AVL=0100、段界限=0000 | P DPL S=1001、TYPE=1000（只执行）
      call make_gdt_descriptor
      mov [esi+0x38], eax
      mov [esi+0x3c], edx
 
 .flush_gdt:
      mov word [pgdt+0x7c00], 8*8-1
      lgdt [pgdt+0x7c00]
 
 .enter_core:
      jmp far [edi+0x10]                        ;间接远转移，因为是在32位保护模式，
                                                ;所以是代码段选择子:段内偏移量，而内核代码start标号处就是段内偏移量
      
 pgdt dw 00                       
      dd 0x0000_7e00                            ;全局描述符表，安装起始物理内存位置   
      

;------------------------------------------------------------------------
make_gdt_descriptor:                            ;生成一个64位全局描述符表的描述符
                                                ;输入：EAX=线性基地址
                                                ;      EBX=段界限
                                                ;      ECX=属性（各属性位都在原始
                                                ;      位置，其它没用到的位置0） 
                                                ;返回：EDX(H32):EAX(L32)=完整的描述符
      push ebx
      push ecx
      
      mov edx, eax                              ;暂存eax(线性基地址)的高16位
      
   .limit_attr_reset:
   ;把段界限(ebx)、段属性(ecx)不用的位清零                           
      and ebx, 0x000_fffff                      
      and ecx, 0x00f0_ff00                     

   .make_l32_eax:
      shl eax, 16 
      or  ax, bx

   .make_h32_edx:
      and edx, 0xffff_0000                      ;描述符高32位段基地址部分
      rol edx, 8
      bswap edx

      xor bx, bx                                
      or  edx, ebx                              ;描述符高32位段界限部分

      or edx, ecx                               ;描述符高32位段属性部分


      pop ecx
      pop ebx
      ret
;------------------------------------------------------------------------



;------------------------------------------------------------------------
read_disk_hard_0:                               ;从硬盘读取一个逻辑扇区，0表示主硬盘
                                                ;输入： eax=起始逻辑扇区号（28bit）
                                                ;      ds:ebx=数据缓冲区起始地址
                                                ;输出： 无

      disk_hard_data_port           equ 0x1f0   ;硬盘数据端口（16-bit）

      disk_hard_errcode_port        equ 0x1f1   ;硬盘错误码端口

      disk_hard_sectors_count_port  equ 0x1f2   ;读取硬盘扇区个数
                                                
      disk_hard_lba_1_port          equ 0x1f3   ;0-7位
      disk_hard_lba_2_port          equ 0x1f4   ;8-15位
      disk_hard_lba_3_port          equ 0x1f5   ;16-23位
      disk_hard_lba_4_port          equ 0x1f6   ;24-28位，高4位其他数据

      disk_hard_command_state_port  equ 0x1f7   ;硬盘命令/状态端口
      push eax
      push edx
      push ecx
      push ebx

      mov ecx, eax                              ;暂存eax

      ;读扇区个数
      mov dx, disk_hard_sectors_count_port
      mov al, 0x01
      out dx, al
      ;指定起始LBA(28-bit)
      mov dx, disk_hard_lba_1_port
      mov al, cl                                ;ecx暂存eax
      out dx, al                                ;LBA 0-7位

      mov dx, disk_hard_lba_2_port
      mov al, ch
      out dx, al                                ;LBA 8-15位

      mov dx, disk_hard_lba_3_port
      shr eax, 16                               ;EAX高16位是干净的。EAX的16-32位，移到AX寄存器
      out dx, al                                ;LBA 16-23位

      mov dx, disk_hard_lba_4_port
      mov al, ah
      or al, 0B1110_0000
      and al, 0b1110_1111                       ;高4位
      out dx, al                                ;LBA 16-23位

      ;请求读命令
      mov dx, disk_hard_command_state_port
      mov al, 0x20
      out dx, al

      ;循环等待硬盘
      mov dx, disk_hard_command_state_port
  .loop_wait:                                   ;TODO-Optimize：只能这样等待吗？CPU和硬盘的速度差异非常非常大，等待的过程CPU都能执行非常非常多条指令了。
      in al, dx
      test al, 0B0000_0001                      ;低1位如果是1，则说明有错误
      jnz .error                                ;不等于0，跳转；低1位是1，有错误

      and al, 0B1000_1000                       ;不关注的位，清零
      cmp al, 0B0000_1000                       ;硬盘不忙&&硬盘数据已经准备OK
      jne .loop_wait                            ;jnz不等于，说明数据还不能读取                                                 

      ;目前只读一个扇区（512byte），一次2byte，共256次
      ;ds:ebx=数据缓冲区起始地址
      mov ecx, 256
      mov dx, disk_hard_data_port
  .loop_read:                                   ;TODO-Optimize：一次才传输2byte数据；硬盘控制器能不能也有一个Buffer，等它的Buffer满了，我再一次性读取？但CPU数据通路才4byte（32位CPU）还是得读取好多次。DMA 吗？？
      in ax, dx
      mov [ebx], ax
      add ebx, 2
      loop .loop_read
      jmp .return

  .error:                                       ;TODO-Todo：代办项，加载内核时，对错误打印。

  .return:
      pop ebx
      pop ecx
      pop edx 
      pop eax
      ret  
;------------------------------------------------------------------------

      times 510-($-$$) db 0x00
      db 0x55, 0xaa
