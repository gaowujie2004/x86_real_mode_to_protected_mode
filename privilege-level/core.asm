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

 make_seg_descriptor:                           ;生成一个64位全局描述符表的描述符
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

 make_gate_descriptor:                          ;生成一个64位门描述符
                                                ;输入：EAX=目标代码段32位偏移地址
                                                ;      BX=目标代码段选择子
                                                ;      CX=门描述符属性
                                                ;返回：EDX(h32):EAX(l32)
      push ebx 

   .make_h32:
      mov edx, eax
      mov dx, cx 

   .make_l32:
      and eax, 0x0000_ffff                      ;eax高16位已经由edx存储
      shl ebx, 16
      or eax, ebx
   
   .ref:
      pop ebx
      retf
 
 install_gdt_descriptor:                        ;将64位描述符安装到GDT中
                                                ;输入：EDX(h32):EAX(l32)=描述符
                                                ;输出：CX=当前描述符选择子， TI=0、RPL=00
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





;============================== sys_routine END =====================================


;============================== core_data STR ================================
SECTION core_data   vstart=0
      pgdt        dd 0x0000_0000                ;暂存GDTR的数据，低两位是界限

      ram_alloc   dd 0x0010_0000                ;用户程序动态内存分配起始线性地址（未开启分页就是物理地址）

      core_buf    times 2048 db 0               ;内核数据缓冲区，不用被缓冲区吓到，本质上就是连续的内存，方便内核对数据进行加工、操作的

      esp_pointer dd 0x0000_0000                ;暂存内核ESP

      tcb_head    dd 0x0000_0000                ;任务控制块链表
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

      msg_test_call_gate      db 0x0d,0x0a, 'In core, test call gate...........', 0

      cpu_brand0              db 0x0d,0x0a, 'Down is cpu brand info:', 0x0d,0x0a, 0x20,0x20,0x20,0x20, 0
      cpu_brand               times 49 db 0,                ;存放cpuinfo需48byte，额外的结束0，共49byte
;============================== core_data END =======================================


;============================== core_code STR =================================
SECTION core_code   vstart=0
 append_tcb:                                    ;添加tcb到tcb链表尾部
                                                ;输入：ECX=tcb起始线性地址
      push ds
      push es
      push eax
      push ebx

   ;ds=core_data、es=4GB
   .seg:
      mov ax, core_data_seg_sel
      mov ds, ax
      mov ax, all_data_seg_sel
      mov es, ax

   .new_node_next_clear:
      mov dword [es:ecx+0x00], 0                ;因为是线性地址，所以使用es（0-4gb)段

   .is_empty:
      mov eax, [tcb_head]
      or eax, eax
      jz .empty
   
   .find_last:
      mov ebx, eax                              ;暂存eax（当前节点地址）
      mov eax, [es:ebx+0x00]                    ;第一个节点的.next的值
      or eax, eax
      jnz .find_last

   .set_last_node:
      mov [es:ebx+0x00], ecx                    ;最后一个tcb的起始线性地址
      jmp .ref

   .empty:
      mov [tcb_head], ecx

   .ref:
      pop ebx
      pop eax
      pop es
      pop ds
 install_ldt_descriptor:                        ;在ldt中安装一个描述符
                                                ;输入：EDX(h32):EAX(l32)=64位段描述符
                                                ;      EBX=tcb起始线性地址
                                                ;输出：cx=描述符选择子
      push ds
      push esi

      mov cx, all_data_seg_sel
      mov ds, cx
      
      mov esi, [ebx+0x0c]                       ;LDT起始线性地址
      xor ecx, ecx
      mov cx, [ebx+0x0a]                        ;LDT段界限
      inc cx                                    ;LDT长度，TODO-Tips：不能是inc ecx
      
      ;安装新段描述符
      mov [esi+ecx], eax                        ;新描述符的起始线性地址
      mov [esi+ecx+0x04], edx

      ;新的LDT段界限
      add cx, 7
      mov [ebx+0x0a], cx

      ;返回新段描述符的选择子
      shr cx, 3                                 ;cx/8
      shl cx, 3                                 ;将索引左移3位，空出TI、RPL位
      or  cx, 0B00000000_00000_100              ;TI=1，RPL=00

      pop esi
      pop ds
      ret


 load_relocate_user_program:                    ;加载重定位用户程序，通过栈传递参数
                                                ;输入：push 用户程序起始逻辑扇区号
                                                ;      push 当前tcb起始线性地址
                                                ;返回：无
      pushad
      push ds
      push es

      ;DS=core_data
      ;ES=4GB
      mov ax, core_data_seg_sel
      mov ds, ax
      mov ax, all_data_seg_sel
      mov es, ax

      mov ebp, esp
      ;ebp+11*4 = tcb起始线性地址
      ;ebp+12*4 = 用户程序起始LBA

   ;esi=tcb起始线性地址
   ;tcb起始线性地址
      mov esi, [ss:ebp+11*4]  

   .ldt:             
      mov ecx, 160                              ;为用户程序的LDT分配20个描述符
      call sys_routine_seg_sel:allocate_memory  ;ecx=分配内存的起始线性地址

      ;放入tcb中
      mov dword [es:esi+0x0c], esi              ;LDT起始线性地址
      mov word [es:esi+0x0a], 0xffff            ;LDT段界限
                                                ;与GDT格式完全一样
   .get_user_program_size:
      mov eax, [ss:ebp+12*4]                    ;用户程序起始逻辑扇区号
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

      ;用户程序起始线性地址登记到tcb
      mov [es:esi+0x06], ebx

   .load_user_program:
   ;ds=4gb
   ;eax 用户程序所占字节数(512对齐)
      push ebx                                  ;暂存为用户程序分配的内存（线性地址）toA
      shr eax, 9                                ;eax/512，2^9=512
      mov ecx, eax                              ;用户程序所占扇区数
      ;ds=4GB
      mov ax, all_data_seg_sel
      mov ds, ax
      mov eax, [ss:ebp+12*4]                    ;用户程序起始LBA                          
      .read_more:
            ;ebx用户程序分配的内存起始线性地址，所以ds必须得是0-4GB（0为段基址）
            call sys_routine_seg_sel:read_disk_hard_0
            inc eax
            add ebx, 512
            loop .read_more

   
   
   .setup_user_program_descriptor:              ;DS=4GB、EDI=用户程序起始线性地址（将被修改）、ESI=tcb起始线性地址
      pop edi                                   ;用户程序分配的内存（线性地址）    toA          

      ;文件头段描述符
      mov eax, edi                              ;段描述符基地址
      mov ebx, [edi+0x04]                       
      dec ebx                                   ;段界限
      mov ecx, 0x0040_f200                      ;数据段属性; G DB L AVL=0100、段界限=0 | P DPL S=1_11_1、TYPE=0010；TODO-Tips：DPL=3
      call sys_routine_seg_sel:make_seg_descriptor

      mov ebx, esi
      call install_ldt_descriptor
      or cx, 0B00000000_00000_011               ;RPL=3
      mov [edi+0x04], cx                        ;文件头大小字段以后是该段的选择子
      mov [esi+0x044],cx                        ;tcb登记程序头部选择子

      ;代码段描述符
      mov eax, edi
      add eax, [edi+0x0c]                       ;代码段基地址
      mov ebx, [edi+0x10]                       
      dec ebx                                   ;段界限
      mov ecx, 0x0040_f800                      ;代码段属性；G DB L AVL=0100、段界限=0 | P DPL S=1_11_1、TYPE=1000（只执行）；TODO-Tips：DPL=3
      call sys_routine_seg_sel:make_seg_descriptor

      mov ebx, esi                              ;tcb起始线性地址
      call install_ldt_descriptor
      or cx, 0B00000000_00000_011               ;RPL=3
      mov [edi+0x0c], cx                        ;登记代码段选择子到用户程序头部


      ;数据段描述符
      mov eax, edi
      add eax, [edi+0x14]                       ;数据段基地址
      mov ebx, [edi+0x18]                       
      dec ebx                                   ;段界限
      mov ecx, 0x0040_f200                      ;数据段属性；G DB L AVL=0100、段界限=0 | P DPL S=1111、TYPE=0010（可读可写）
      call sys_routine_seg_sel:make_seg_descriptor

      mov ebx, esi                              ;tcb起始线性地址
      call install_ldt_descriptor
      or cx, 0B00000000_00000_011               ;RPL=3
      mov [edi+0x14], cx                        ;登记数据段到用户程序头部

      ;栈段描述符
      mov eax, edi
      add eax, [edi+0x1c]                       ;栈段基地址
      mov ebx, [edi+0x20]                       
      dec ebx                                   ;段界限
      mov ecx, 0x0040_f200                      ;栈段属性；G DB L AVL=0100、段界限=0 | P DPL S=1111、TYPE=0010（可读可写）
      call sys_routine_seg_sel:make_seg_descriptor

      mov ebx, esi                              ;tcb起始线性地址
      call install_ldt_descriptor
      or cx, 0B00000000_00000_011  
      mov [edi+0x1c], cx                        ;登记栈段选择子到用户程序头部


   
   .salt_relocate:                              ;重定位用户符号地址，ES=4GB、DS=core_data、EDI=用户程序起始线性地址
      ; mov ax, [edi+0x04]                      ;用户程序头部段选择子，LDT还没有生效，只能先使用4GB段
      ; mov es, ax                             

      mov ax, core_data_seg_sel
      mov ds, ax
      
      mov ecx, [es:edi+0x24]                    ;u-salt条目个数
      add edi, 0x28                             ;u-salt偏移量
      mov esi, salt                             ;c-salt偏移量       
      cld
   ;es:edi <- ds:esi                      
   ;es=4GB、edi=u-salt表起始偏移量 
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
            or ax, 0B00000000_00000_0_11        ;RPL==用户程序特权级CPL==3，以用户程序自己的特权级使用调用门
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
   
   ;DS=core_data、ES=4GB、ESI=tcb起始线性地址
   .create_PL_stack:                            ;创建不同特权级的栈段，放在LDT中
      mov esi, [ss:ebp+11*4]                    ;tcb起始线性地址
      
      ;创建0特权级栈_4kb长度
      mov ecx, 0
      mov [es:esi+0x1a], ecx                    ;段界限，实际长度是0+1
      inc ecx
      shl ecx, 12                               ;栈实际所占字节数(4KB对齐)
      push ecx
      call sys_routine_seg_sel:allocate_memory  ;输入ecx（希望分配字节数）、输出ecx（分配内存的起始线性地址）
      mov [es:esi+0x1e], ecx                    ;0特权级栈基地址,感觉是多余的,完全可以从LDT中获取到.
     
      mov eax, ecx                              ;栈内存基地址
      mov ebx, [es:esi+0x1a]                    ;段界限
      mov ecx, 0x00c0_9200                      ;DPL=0
      call sys_routine_seg_sel:make_seg_descriptor
      mov ebx, esi
      call install_ldt_descriptor               ;栈段描述符在LDT中的选择子
      and cx, 0B11111111_11111_1_00             ;RPL=00，默认就是00，这行代码可以忽略， CPL==0，CPL要与栈特权级时时刻刻相同
      mov [es:esi+0x22], cx                     ;0特权级栈选择子（在LDT中）
      pop dword [es:esi+0x24]                   ;长度（所占字节数）


      ;创建1特权级栈，CPL==RPL==栈段DPL
      mov ecx, 0
      mov [es:esi+0x28], ecx                    ;段界限，实际长度是0+1
      inc ecx
      shl ecx, 12                               ;栈实际所占字节数(4KB对齐)
      push ecx
      call sys_routine_seg_sel:allocate_memory  ;输入ecx（希望分配字节数）、输出ecx（分配内存的起始线性地址）
      mov [es:esi+0x2c], ecx                    ;1特权级栈基地址,感觉是多余的,完全可以从LDT中获取到.
     
      mov eax, ecx                              ;栈段基地址
      mov ebx, [es:esi+0x28]                    ;段界限
      mov ecx, 0x00c0_b200                      ;DPL=1
      call sys_routine_seg_sel:make_seg_descriptor
      mov ebx, esi
      call install_ldt_descriptor               ;栈段描述符在LDT中的选择子
      or cx, 0B00000000_00000_0_01              ;RPL=01，默认00
      mov [es:esi+0x30], cx                     ;1特权级栈选择子（在LDT中）
      pop dword [es:esi+0x32]                   ;长度（所占字节数）

      ;创建2特权级栈，CPL==RPL==栈段DPL
      mov ecx, 0
      mov [es:esi+0x36], ecx                    ;段界限，实际长度是0+1
      inc ecx
      shl ecx, 12                               ;栈实际所占字节数(4KB对齐)
      push ecx
      call sys_routine_seg_sel:allocate_memory  ;输入ecx（希望分配字节数）、输出ecx（分配内存的起始线性地址）
      mov [es:esi+0x3a], ecx                    ;1特权级栈基地址,感觉是多余的,完全可以从LDT中获取到.
     
      mov eax, ecx                              ;栈段基地址
      mov ebx, [es:esi+0x36]                    ;段界限
      mov ecx, 0x00c0_d200                      ;DPL=2
      call sys_routine_seg_sel:make_seg_descriptor
      mov ebx, esi
      call install_ldt_descriptor               ;栈段描述符在LDT中的选择子
      or cx, 0B00000000_00000_0_10              ;RPL=02，默认00
      mov [es:esi+0x3e], cx                     ;2特权级栈选择子（在LDT中）
      pop dword [es:esi+0x40]                   ;长度（所占字节数）

   .ldt_to_gdt:                                 ;安装LDT(整个表)到GDT中，因为LDT也是个内存段
      mov eax, [es:esi+0x0c]                    ;LDT起始线性地址
      movzx ebx, word [es:esi+0x0a]             ;LDT长度-1（整个表的长度-1）,LDT当前界限，最大64KB
      mov ecx, 0x0000_8200                      ;LDT的属性。TODO-Think：为什么DPL=0
      call sys_routine_seg_sel:make_seg_descriptor
      call sys_routine_seg_sel:install_gdt_descriptor ;CX=当前描述符选择子， TI=0、RPL=00
      mov [es:esi+0x10], cx                     ;回填到tcb，LDT在GDT中的选择子

      

   .return:
      pop es
      pop ds
      popad
      ret
 ;------------------------------------------------------------
 
 ;ds=core_data
 start:
      call sys_routine_seg_sel:clear

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

 .public_func_call_game:                        ;不同特权级之间进行控制转移，可以通过调用门来完成
      mov edi, salt
      mov ecx, salt_item_count
   .for_to_gate:                                ;为每个条目安装门描述符
      push ecx                                  ;暂存ecx

      mov eax, [edi+256]                        ;公共函数（目标代码）段内偏移量
      mov bx, [edi+260]                         ;公共函数（目标代码）段选择子
      mov cx, 0B1_11_0_1100_000_00000           ;调用门描述符属性，DPL=3，要求（CPL&&RPL <= 门描述符DPL) && CPL >= 目标代码段描述符DPL
      call sys_routine_seg_sel:make_gate_descriptor
      call sys_routine_seg_sel:install_gdt_descriptor
                                                ;将调用门描述符安装到GDT
                        
      mov [edi+260], cx                         ;回填调用门描述符选择子

      pop ecx
      add edi, salt_item_size
      loop .for_to_gate
 
   .test_call_gate:
      mov ebx, msg_test_call_gate
      call far [salt_1 + 256]                     ;最终发现选择子选择的是门描述符，丢弃偏移量，使用门描述符中的信息。

 .create_tcb:
      mov ecx, 0x46                             ;tcb size
      call sys_routine_seg_sel:allocate_memory  ;ecx=分配内存的起始线性地址
      call append_tcb

 ;ds=用户程序头部段
 .enter_user_program:
      push dword 50
      push ecx                                  ;ecx=分配内存的起始线性地址、也等于当前tcb起始线性地址
      call load_relocate_user_program

      mov ebx, msg_load_relocate_ok
      call sys_routine_seg_sel:put_string
      
      mov [esp_pointer], esp
      ;ds=用户程序头部段
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