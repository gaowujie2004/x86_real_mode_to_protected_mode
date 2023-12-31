      video_card_index_port   equ     0x3d4                 ;显卡的功能索引寄存器端口
      video_card_data_port    equ     0x3d5                 ;显卡的数据寄存器端口
      cursor_h8_port          equ     0x0e                  ;光标寄存器索引端口，高8位
      cursor_l8_port          equ     0x0f                  ;光标寄存器索引端口，低8位， 00表示光标在第0行0列，80表示光标在1行0列，这里的值其实是偏移量。行、列从0开始。
      CR                      equ     0x0d                  ;CR回车ASCII码值，当前首行
      LF                      equ     0x0a                  ;LF换行ASCII码值，垂直下行，不是下行的首行
      
      
      all_data_seg_sel    equ     0B00000000_00001_000      ;0x08，4GB数据段选择子
      mbr_code_seg_sel    equ     0B00000000_00010_000      ;0x10，初始化代码段（mbr）
      core_stack_seg_sel  equ     0B00000000_00011_000      ;0x18，初始化栈段（mbr、core）
      video_buf_seg_sel   equ     0B00000000_00100_000      ;0x20，初始化栈段（mbr、core）

      sys_routine_seg_sel equ     0B00000000_00101_000      ;0x28，内核公共代码段选择子  
      core_data_seg_sel   equ     0B00000000_00110_000      ;0x30，内核数据段选择子 
      core_code_seg_sel   equ     0B00000000_00111_000      ;0x38，内核代码段选择子

      user_program_start_sector     equ   50                ;用户程序所在逻辑扇区号（LBA）

;=============================== header STR =================================
SECTION header  vstart=0
      ;以下是系统核心的头部，用于加载核心程序 
      core_length       dd file_end             ;内核程序总长度#00

      sys_routine_seg   dd section.sys_routine.start
                                                ;内核公共代码段位置#04

      core_data_seg     dd section.core_data.start
                                                ;内核数据段位置#08

      core_code_seg     dd section.core_code.start
                                                ;内核代码段位置#0c


      core_entry        dd start                ;内核代码段入口点#10
                        dw core_code_seg_sel
;=============================== header END =========================================


;============================== sys_routine STR ==============================
      [bits 32]
SECTION sys_routine vstart=0
 put_char:                                      ;打印一个字符
                                                ;输入：cl=ASCII码
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
      mov bl, al                                ;低8位，光标位置获取到，放入bl暂存

      mov dx, video_card_index_port
      mov al, cursor_h8_port
      out dx, al                                  
      mov dx, video_card_data_port
      in al, dx                                   
      mov bh, al                                ;高8位，光标位置获取到，放入bh暂存


      ;选择跳转
   .process_jmp:
      cmp cl, CR
      je .put_cr                                ;相等则跳转，是回车

      cmp cl, LF
      je .put_lf                                ;相等则跳转，是换行

      jmp .put_other

      ;首行，BX=光标为当前首行
   .put_cr:
      mov dx, 0
      mov ax, bx
      mov bl, 80
      div bl                                    ;dx:ax/80=ax....dx， 

      mul bl                                    ;al*bl=ax    ax是当前行的首行索引，得再乘80，才是偏移量
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
      xor eax, eax                              ;eax的高位可能不干净，清楚
      mov ax, bx
      shl eax, 1                                ;乘2
      mov [esi + eax], cl    
      add bx, 1                                 ;推进光标

      ;光标越界检查，是否需要滚动屏幕
   .scroll_check:
      cmp bx, 2000                              ;25*80=2000, 最后一个偏移量1999
      jl .set_cursor                            ;<则跳转

      ;接下来是>=，滚动屏幕
   .scroll_screnn:                              ;第1行移到第0行，。。。。最后一行置空。去除最后1行，共24行，一共24*80个字符
      sub bx, 80                                ;向上移了一行，光标也需要对应移动
      ;24*80/4, [es:edi] <- [ds:esi]
      mov ax, ds
      mov es, ax
      mov edi, 0
      mov esi, 160
      mov ecx, 24*80*2/4                        ;应该是字符数*2才是字节数
      cld                                       ;DF=0，edi、esi方向增加
      rep movsd                                 ;edi、esi步长是4（双字）rep movsd，32位保护模式时，使用的是ecx
      ;最后一行置空（黑底白字空格字符）
      mov ecx, 40                               ;本来是要循环160次的，但现在每次四个字节操。 TODO:Think-CPU和数据总线拥有更宽的数据通路，所执行的指令个数就会减少，充分体现了性能的优化
      .loop_cls:
      mov dword [esi], 0x07200720               ;在上一步rep movsd结束时，esi偏移量是24*80*2，符合预期。（行从0开始）
      add esi, 2 
      loop .loop_cls        

      ;向外设写数据，BX=光标位置
   .set_cursor:                                   
      mov dx, video_card_index_port
      mov al, cursor_l8_port
      out dx, al
      mov dx, video_card_data_port
      mov al, bl 
      out dx, al                                ;文本模式光标寄存器低8位

      mov dx, video_card_index_port
      mov al, cursor_h8_port
      out dx, al
      mov dx, video_card_data_port
      mov al, bh
      out dx, al                                ;文本模式光标寄存器高8位

   .put_char_return:
      pop es
      pop ds

      pop dx
      pop bx
      pop ax
      pop esi
      pop edi
      ret


 put_string:                                    ;文本模式下格式化字符串打印
                                                ;输入：ds:ebx字符串首地址，0x00结束
                                                ;输出：无
      push ecx
      push ebx

   .each_char:                                 
      mov cl, [ebx]
      or cl, cl                                 ;为了性能，or 操作数全是寄存器，并且不会改变or的值，影响ZF标志位（其他标志位的影响不确定）TODO-Optimize：操作数全是寄存器，没有立即数，也没有内存寻址，所以很快，而且指令所占字节数少
      jz .put_string_return                     ;等于零则跳转（ZF=1)
      call put_char                             ;输入：cl=ASCII码
      inc ebx
      jmp .each_char

   .put_string_return:
      pop ebx
      pop ecx
      retf

 make_gdt_descriptor:                           ;生成一个64位全局描述符表的描述符
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
      retf


 install_gdt_descriptor:                        ;将一个64位描述符安装到GDT中
                                                ;输入：EDX(h32):EAX(l32)=描述符
                                                ;输出：CX=当前描述符选择子
      push ebx
      push ds
      push es 

      ;ds=core_data、es=4GB、因为是线性地址，那么基地址应该是0故es是4gb段选择子
      mov bx, core_data_seg_sel
      mov ds, bx
      mov bx, all_data_seg_sel
      mov es, bx

      ;1.计算新描述符在表中偏移量
      sgdt [pgdt]
      movzx ebx, word [pgdt]                    ;旧gdt界限值
      inc bx                                    ;界限值+1等于长度，即新描述符在表中的偏移量
                                                ;TODO-Tips：重点关注，加电预置的GDT界限是0xFFFF，第一次安装时0xFFFF+1=0x1_0000，但bx只存储2byte，进位舍弃
                                                ;若使用ebx，则进位还会保留，因为ebx 4byte
      add ebx, [pgdt+2]                         ;偏移量+gdt起始线性地址=新描述符在GDT中的线性地址

      ;2.安装                                                                 
      mov [es:ebx], eax                     
      mov [es:ebx+4], edx

      ;3.更新GDT界限值
      add word [pgdt], 8

      ;4.加载新的gdt
      lgdt [pgdt]

      ;5.计算新描述符对应的选择子
      mov cx, [pgdt]
      shr cx, 3                                ;ecx除8，描述符在表中的索引
      shl cx, 3                                ;将索引值移到选择子索引位，低3为位中TI=0，RPL=00


      pop es
      pop ds
      pop ebx
      retf
 read_disk_hard_0:                              ;从硬盘读取一个逻辑扇区，0表示主硬盘
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
      retf
 
 
 read_disk_hard_custom:                         ;读取主硬盘指定字节数，内部实际读取扇区数是向上舍入的
                                                ;输入：ds:ebx = 硬盘数据缓冲区
 
 clear:                                         ;清除屏幕字符
      pushad

      mov ax, all_data_seg_sel
      mov ds, ax
      mov edi, 0xb_8000
      mov ecx, 1000                             ;200*2/4
   .loop_clear:
      mov dword [edi], 0x0720_0720              ;0B0000_0111=0x07
      add edi, 2
      loop .loop_clear

   .rest_cursor:
      mov dx, video_card_index_port
      mov al, cursor_l8_port
      out dx, al
      mov dx, video_card_data_port
      mov al, 0
      out dx, al

      mov dx, video_card_index_port
      mov al, cursor_h8_port
      out dx, al
      mov dx, video_card_data_port
      mov al, 0
      out dx, al

   .return:
      popad
      retf

 allocate_memory:                               ;为用户程序动态分配内存
                                                ;输入：ECX=希望分配的字节数
                                                ;输出：ECX=分配的内存的起始线性地址
      push ds
      push eax
      push ebx

      mov ax, core_data_seg_sel
      mov ds, ax

      mov eax, [ram_alloc]                      ;[ram_alloc]=起始线性地址
      mov ebx, eax                              ;ebx暂存分配的地址，最为最终结果
      add eax, ecx                              ;eax下一次分配的起始线性地址
      
      mov ecx, ebx

   .up_align:
   ;强制ebx，4字节向上对齐
      mov ebx, eax
      and ebx, 0xffff_fffc                      ;低两位清零，向下4字节对齐
      add ebx, 4                                ;再加上4，使ebx向上4字节对齐。比如数值5，最终向上4字节对齐就是8

      test eax, 0B11                            ;若eax本身就4字节对齐，ebx的结果就大了4。
      cmovnz eax, ebx                           ;第两位不等于0，说明eax不是4字节对齐；若等于0则不改变eax的值
   
   .write:
      mov [ram_alloc], eax

   .return:
      pop ebx
      pop eax
      pop ds
      retf

 terminateUserProgram:                          ;结束用户程序，控制进入内核
      retf
;============================== sys_routine END =====================================


;============================== core_data STR ================================
SECTION core_data   vstart=0
      pgdt        dw 00                         ;GDT界限=长度-1
                  dd 0x0000_0000                ;GDT起始线性地址

      ram_alloc   dd 0x0010_0000                ;用户程序动态内存分配起始线性地址（未开启分页就是物理地址）

      core_buf    times 2048 db 0               ;内核数据缓冲区，不用被缓冲区吓到，本质上就是连续的内存，方便内核对数据进行加工、操作的

      esp_pointer dd 0x0000_0000                ;暂存内核ESP
      ;-------------------------SALT--------------------
      salt:
      salt_1      db '@put_string'
                  times 256-($-salt_1) db 0
                  dd put_string
                  dw sys_routine_seg_sel

      salt_2      db '@read_head_disk'
                  times 256-($-salt_2) db 0
                  dd read_disk_hard_0
                  dw sys_routine_seg_sel

      salt_last:
      salt_3      db '@terminateProgram'
                  times 256-($-salt_3) db 0
                  dd return_pointer
                  dw core_code_seg_sel 

      salt_item_size    equ   $-salt_last
      salt_item_count   equ   ($-salt)/salt_item_size       ;常量不占汇编地址
      ;-------------------------SALT--------------------

      msg_enter_core          db            'Core enter success................', 0
      msg_load_relocate_ok    db 0x0d,0x0a, 'User program load relocate success', 0
      msg_start_user_program  db 0x0d,0x0a, 'Start enter User program..........', 0

      cpu_brand0              db 0x0d,0x0a, 'Down is cpu brand info:', 0x0d,0x0a, 0x20,0x20,0x20,0x20, 0
      cpu_brand               times 49 db 0,                ;存放cpuinfo需48byte，额外的结束0，共49byte
;============================== core_data END =======================================


;============================== core_code STR =================================
SECTION core_code   vstart=0
 load_relocate_user_program:                    ;加载重定位用户程序
                                                ;输入：ESI=起始逻辑扇区号
                                                ;返回：AX=指向用户程序头部的段选择子
      push ds
      push es
      pushad

   .get_user_program_size:
   ;ds=core_data
      mov ax, core_data_seg_sel
      mov ds, ax

      mov eax, esi
      mov ebx, core_buf      
      call sys_routine_seg_sel:read_disk_hard_0

      mov eax, [core_buf]                       ;core_buf 0-3是用户程序长度字段(字节单位)
      mov ebx, eax                              
      and ebx, 0xffff_fe00                      ;低9位清零
      add ebx, 512                              ;向上512对齐

      test eax,0x0000_01ff                      ;eax原先是不是512字节对齐？只test低9位
      cmovnz eax, ebx                           ;低9位不是0说明不是512对齐，使用对齐结果

   .allocate_memory:                           
      mov ecx, eax                              ;实际需要申请的内存数量(字节单位)
      ;输入ecx=预期分配的字节数、输出：ecx=分配的内存起始线性地址
      call sys_routine_seg_sel:allocate_memory
      mov ebx, ecx                              ;暂存分配的线性地址

   .load_user_program:
   ;eax 用户程序所占字节数(512对齐)
      push ebx                                  ;暂存为用户程序分配的内存（线性地址）toA
      shr eax, 9                                ;eax/512，2^9=512
      mov ecx, eax                              ;用户程序所占扇区数
      ;ds=4GB
      mov ax, all_data_seg_sel
      mov ds, ax
      mov eax, esi                              
      .read_more:
            ;ebx用户程序分配的内存起始线性地址，所以ds必须得是0-4GB（0为段基址）
            call sys_routine_seg_sel:read_disk_hard_0
            inc eax
            add ebx, 512
            loop .read_more
   ;ds=4GB
   .setup_user_program_descriptor:              
      pop edi                                   ;用户程序分配的内存（线性地址）    toA          

      ;文件头段描述符
      mov eax, edi                              ;段描述符基地址
      mov ebx, [edi+0x04]                       ;文件头大小
      dec ebx                                   ;段界限
      mov ecx, 0x0040_9200                      ;数据段属性; G DB L AVL=0100、段界限=0、P DPL S=1001、TYPE=0010
      call sys_routine_seg_sel:make_gdt_descriptor
      call sys_routine_seg_sel:install_gdt_descriptor
      mov [edi+0x04], cx                        ;文件头大小字段以后是该段的选择子

      ;代码段描述符
      mov eax, edi
      add eax, [edi+0x0c]                       ;代码段基地址
      mov ebx, [edi+0x10]                       ;代码段长度
      dec ebx                                  
      mov ecx, 0x0040_9800                      ;代码段属性；G DB L AVL=0100、段界限=0 | P DPL S=1001、TYPE=1000（只执行）
      call sys_routine_seg_sel:make_gdt_descriptor
      call sys_routine_seg_sel:install_gdt_descriptor
      mov [edi+0x0c], cx                        ;TODO-Tips：以后需要


      ;数据段描述符
      mov eax, edi
      add eax, [edi+0x14]                       ;数据段基地址
      mov ebx, [edi+0x18]                       ;数据段长度
      dec ebx                                  
      mov ecx, 0x0040_9200                      ;数据段属性；G DB L AVL=0100、段界限=0 | P DPL S=1001、TYPE=0010（可读可写）
      call sys_routine_seg_sel:make_gdt_descriptor
      call sys_routine_seg_sel:install_gdt_descriptor
      mov [edi+0x14], cx                        ;TODO-Tips：以后需要

      ;栈段描述符
      mov eax, edi
      add eax, [edi+0x1c]                       ;栈段基地址
      mov ebx, [edi+0x20]                       ;栈段长度
      dec ebx                                  
      mov ecx, 0x0040_9200                      ;栈段属性；G DB L AVL=0100、段界限=0 | P DPL S=1001、TYPE=0010（可读可写）
      call sys_routine_seg_sel:make_gdt_descriptor
      call sys_routine_seg_sel:install_gdt_descriptor
      mov [edi+0x1c], cx                        ;TODO-Tips：以后需要


   ;重定位用户符号地址
   .salt_relocate:
      mov ax, [edi+0x04]                        ;用户程序头部段选择子
      mov es, ax
      mov ax, core_data_seg_sel
      mov ds, ax
      
      mov ecx, [es:0x24]
      mov edi, 0x28                             ;u-salt偏移量
      mov esi, salt                             ;c-salt偏移量       
      cld
   ;es:edi <- ds:esi                      
   ;es=user_head、edi=u-salt表起始偏移量 
   ;ds=core_data、esi=c-salt表起始偏移量
   ;1.因为是修改用户程序中的salt表，u-salt是外围循环
   @for_user_salt:                              ;TODO-Think：想了好久
      push edi
      push ecx
      
         ;循环core_salt
         mov ecx, salt_item_count
         mov esi, salt
         @for_core_salt:
            push ecx
            push edi
            push esi
            
            mov ecx, 256
            repz cmpsb                          ;[es:edi]-[ds:esi] == 0 OR ecx!=0 才继续循环
            jne @for_core_salt_continue         ;[es:edi]!=[ds:esi]（ZF=0），说明串中有不相同的

            ;匹配上了,esi恰好指向后一个item的地址（因为ecx=256）。 修改[es:edi] 前六个字节，为 [esi]第256开始的6byte
            mov eax, [esi]                      ;c-salt item公共函数段内偏移量
            mov [es:edi-256], eax

            mov ax, [esi+0x04]                  ;c-salt item公共函数的段选择子
            mov [es:edi-252], ax
         @for_core_salt_continue:
            pop esi
            pop edi
            pop ecx
            add esi, salt_item_size
            loop @for_core_salt

      pop ecx
      pop edi
      add edi, 256
      loop @for_user_salt


      popad
      mov ax, [es:0x04]                         ;用户程序头部的段选择子
      pop es
      pop ds
      ret
 ;------------------------------------------------------------
 start:
      call sys_routine_seg_sel:clear

      ;ds=core_data
      mov ax, core_data_seg_sel
      mov ds, ax

      mov ebx, msg_enter_core
      call sys_routine_seg_sel:put_string

 .printf_cpu_info:
      mov ebx, cpu_brand0
      call sys_routine_seg_sel:put_string

      mov eax, 0x80000002
      cpuid
      mov [cpu_brand + 0x00], eax
      mov [cpu_brand + 0x04], ebx
      mov [cpu_brand + 0x08], ecx
      mov [cpu_brand + 0x0c], edx

      mov eax, 0x80000003
      cpuid
      mov [cpu_brand + 0x10], eax
      mov [cpu_brand + 0x14], ebx
      mov [cpu_brand + 0x18], ecx
      mov [cpu_brand + 0x1c], edx

      mov eax, 0x80000004
      cpuid
      mov [cpu_brand + 0x20], eax
      mov [cpu_brand + 0x24], ebx
      mov [cpu_brand + 0x28], ecx
      mov [cpu_brand + 0x2c], edx

      mov ebx, cpu_brand
      call sys_routine_seg_sel:put_string

 .enter_user_program:
      mov esi, user_program_start_sector
      call load_relocate_user_program

      mov ebx, msg_load_relocate_ok
      call sys_routine_seg_sel:put_string
      
      mov [esp_pointer], esp
      mov ds, ax                                ;load_relocate_user_program的返回值，用户程序头部段选择子

      jmp far [0x08]                            ;间接远转移（必须指明far），因为这是32位保护模式，所以用的是段选择子。

 return_pointer:
      mov ax, core_data_seg_sel
      mov ds, ax

      mov ax, core_stack_seg_sel
      mov ss, ax
      mov esp, [esp_pointer]

      mov ebx, msg_enter_core
      call sys_routine_seg_sel:put_string
 
      ;可以继续执行其他程序
      ;可以执行清理内存的任务

      hlt
;============================== core_code END =============================



SECTION tail
 file_end: