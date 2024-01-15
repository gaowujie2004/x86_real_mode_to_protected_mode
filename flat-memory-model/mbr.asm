
      flat_core_code_seg_sel  equ   0x0008      ;内核（0特权级）4GB代码段选择子
      flat_core_data_seg_sel  equ   0x0010      ;内核（0特权级）4GB数据段选择子（栈段）
      flat_user_code_seg_sel  equ   0x001b      ;用户（3特权级）4GB代码段选择子
      flat_user_data_seg_sel  equ   0x0023      ;用户（3特权级）4GB数据段选择子（栈段）

      core_base_address       equ   0x0004_0000 ;内核程序的起始物理地址
      core_start_sector       equ   0x0000_0001 ;内核程序的起始逻辑扇区号

      core_pdt_address        equ   0x0002_0000 ;内核全局空间页目录表起始物理地址
      core_low_1mb_pt_address equ   0x0002_1000 ;内核全局空间，低端物理1MB映射页表物理地址，第一个页表的物理地址
      ;0x8000_0000-0x8010_0000（虚拟地址）映射到 0x0000_0000-0x0010_0000（物理地址）    

SECTION MBR vstart=0x7c00
      mov ax, cs
      mov ss, ax
      mov sp, 0x7c00

      ;此时是实模式
 .create_gdt:
      mov eax, [cs:pgdt+0x02]
      mov edx, 0
      mov ebx, 16       
      div ebx                                   ;edx:eax/r32 -> eax......edx
      mov ds, ax                                ;商=段地址
      mov ebx, edx                              ;余数=段内偏移量

      ;#0空描述符
      mov dword [ebx+0x00], 0x0000_0000
      mov dword [ebx+0x04], 0x0000_0000 
      ;#1 内核（0特权级）4GB代码段
      mov dword [ebx+0x08], 0x0000_ffff
      mov dword [ebx+0x0c], 0x00cf_9800         
      ;#2 内核（0特权级）4GB数据段、栈段
      mov dword [ebx+0x10], 0x0000_ffff           
      mov dword [ebx+0x14], 0x00cf_9200
      ;#3 用户（3特权级）4GB代码段                            
      mov dword [bx+0x18], 0x0000_ffff                    
      mov dword [bx+0x1c], 0x00cf_f800
      ;#4 用户（3特权级）4GB数据段、栈段                              
      mov dword [bx+0x20], 0x0000_ffff
      mov dword [bx+0x24], 0x00cf_f200
      
  .gdt_limt:
      mov word [cs:pgdt], 5*8-1

  .lgdt:
      lgdt [cs:pgdt]

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

      jmp flat_core_code_seg_sel:flush          ;CR0.PE位控制寻找方式，当前PE=1，描述符寻址
                                                ;段的位置，现在是选择子


      [bits 32]  
  flush:                                        ;刷新所有段寄存器，CS中D位=1，32位保护模式
      mov ax, flat_core_data_seg_sel
      mov ds, ax
      mov es, ax
      mov fs, ax
      mov gs, ax
      mov ss, ax
      mov esp, 0x7c00


 .load_core:
      mov eax, core_start_sector
      mov ebx, core_base_address
      call read_disk_hard_0

      mov edx, 0
      mov eax, [core_base_address]
      mov ebx, 512                              
      div ebx                                   ;edx:eax / ebx = eax......edx

      or edx, edx
      jnz @1                                    ;不等于0，说明还有数据，商得+1，但已经读取过一次，扯平了
      dec eax
   @1:
      or eax, eax                               ;等于0说明，内核刚好512或少于512字节
      jz .open_page
      mov ecx, eax
      mov eax, core_start_sector
      mov ebx, core_base_address
   .core_read_more:
      inc eax
      add ebx, 512
      call read_disk_hard_0
      loop .core_read_more

 .open_page:
      mov ebx, core_pdt_address                 ;页目录表起始物理地址

      ;清空页目录表
      mov ecx, 1024
      mov esi, 0                 
   .clear_pdt:
      mov dword [ebx+esi], 0
      add esi, 4
      loop .clear_pdt
   
   .init_item:                                  ;0x8000_0000-0x8010_0000（虚拟地址）映射到 0x0000_0000-0x0010_0000（物理地址）
      mov dword [ebx+0xffc], 0x0002_0003 ;倒数第一个目录项，TODO-Tips: 可用来定位页目录表自身
      mov dword [ebx+0x800], 0x0002_1003 ;线性地址0x8000_0000对应的目录项

      ;设置低端1MB物理内存对应的页表项
      mov ebx, core_low_1mb_pt_address          ;页表物理地址
      mov esi, 0
      mov edi, 0
   .set_pt:
      mov eax, esi
      or eax, 0x0000_0003
      mov [ebx+edi], eax           ;页表项值
      add edi, 4                                ;页表偏移量
      add esi, 0x1000                           ;页物理地址（低端1MB物理页）
      cmp edi, 256                              ;1MB内存需要256个物理页
      jl .set_pt                                ;小于256，则继续循环

      ;清空剩余页表项
   .clear_pt:
      mov dword [ebx+edi], 0
      cmp edi, 1024
      jl .clear_pt

   .pdbr:
      mov eax, core_pdt_address
      mov cr3, eax


   .change_gdtr:
      ;gdt
      sgdt [pgdt]
      add dword [pgdt+0x02], 0x8000_0000
      lgdt [pgdt]

   .open:
      ;分页部件开启
      mov eax, cr0
      or eax, 0x8000_0000
      mov cr0, eax
      
      ;将堆栈映射到高端，这是非常容易被忽略的一件事。应当把内核的所有东西
      ;都移到高端，否则，一定会和正在加载的用户任务局部空间里的内容冲突，
      ;而且很难想到问题会出在这里。 
      add esp, 0x8000_0000
 
 .enter_core: 
      ;已开启分页部件
      jmp [0x8004_0004]                         ;间接近转移，EIP <- M[0x8004_0004]
       
      
;------------------------------------------------------------------------
read_disk_hard_0:                               ;从硬盘读取一个逻辑扇区，0表示主硬盘
                                                ;输入：EAX=起始逻辑扇区号（28bit）
                                                ;      DS:EBX=数据缓冲区起始地址

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

      pgdt  dw 00                       
            dd 0x0000_7e00                            ;全局描述符表，起始物理地址  

      times 510-($-$$) db 0x00
      db 0x55, 0xaa
