      ;任务：协同式任务切换。
      ;用户程序1 LBA=50、用户程序2 LBA=70
      
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
      push ebp


   .chech_RPL:
      ;TODO-Tips：从这里也能看出，RPL是由操作系统控制的，CPU只负责检查RPL与CPL的合法性，不负责鉴别PRL的真实性，真实性由操作系统鉴别。
      ;操作系统很显然是知道请求者的CPL的，这样就可以判断并修改RPL
      mov ebp, esp
      mov cx, ds
      mov dx, [ss:ebp+6*4]                      ;调用者CS
      arpl cx, dx                               ;调整ds
      mov ds, cx


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
      pop ebp
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

 initiative_task_switch:                        ;主动进行任务切换
                                                ;输入：无、 输出：无
      ;怎么进行任务切换？从TCB链表找到繁忙的任务，即当前调用initiative_task_switch的任务的tcb，从这个tcb开始向后找一个空闲的tcb
      ;若向后找不到，则从头开始找，都没找到就退出，任务不切换；若找到了，则把找到的空闲节点的状态反转（变为0xffff，即将开始运行这个任务）
      ;将旧任务（当前任务）的tcb状态也反转一下，变为0，表示空闲。
      ;jmp far 指向tss选择子，即可由硬件完成任务切换
      pusha
      push ds
      push es

   ;DS=4GB、ES=core_data
      mov ax, all_data_seg_sel
      mov ds, ax
      mov ax, core_data_seg_sel
      mov es, ax


      mov eax, [es:tcb_head]
   ;从头找一个繁忙的tcb（当前任务）
   .find_buzy_tcb:
      cmp word [eax+0x04], 0xffff               ;EAX当前TCB节点起始线性地址
      cmove esi, eax                            ;找到繁忙节点,ESI=繁忙TCB起始线性地址
      je .tail_find_idle_tcb                    ;找到繁忙节点                     
      mov eax, [eax+0x00]                       ;没找到，cur=cur.next
      jmp .find_buzy_tcb


   ;ESI=繁忙TCB起始线性地址,留给最后使用
   ;EDI=空闲TCB起始线性地址,留给最后使用
   ;从繁忙节点的下一个节点开始找空闲节点
   .tail_find_idle_tcb:
      mov eax, [eax+0x00]                       ;EAX=繁忙TCB的下一个TCB起始线性地址
      or eax, eax                               ;是否到尾部?
      jz .haed_find_idle_tab                     ;Y,从头找空闲节点
      cmp word [eax+0x04], 0                    ;N,没有到尾部
      cmove edi, eax                            
      je .ok                                     
      jmp .tail_find_idle_tcb

   ;从头开始找空闲节点
   .haed_find_idle_tab:
      mov eax, [es:tcb_head]
      .for:
      cmp eax, esi
      je .return                                ;cur=繁忙节点,还没找到空闲,说明根本没有空闲任务
      cmp word [eax+0x04], 0
      cmove edi, eax                            ;为0，则赋值
      je .ok
      mov eax, [eax+0x00]                       ;不相等,cur=cur.next
      jmp .for

   ;ESI=繁忙TCB起始线性地址
   ;EDI=空闲TCB起始线性地址
   ;反转状态，准备切换
   .ok:
      not word [esi+0x04]                       ;将旧任务TCB状态改为空闲
      not word [edi+0x04]                       ;将空闲任务TCB改为忙,即将切换到该任务
      jmp far [edi+0x14]                        ;CPU硬件任务切换
      ;edi空闲TCB即即将要切换到该任务执行
      ;CPU发现以edi+0x14为首的6byte内存数据
      ;其中低2byte数据是TSS选择子,CPU开始执行任务切换的工作

   .return:
      pop es
      pop ds
      popa
      retf

 terminate_current_task:                        ;终止当前任务,
      ;设置当前繁忙的TCB节点为0x3333, 后续由do_task_clear负责内存等清理操作
      ;然后在切换到其他任务,这和do_switch很像

      ;当前任务都要结束了,没必要再保存当前任务的状态了.
      ;TODO-Tips: 注意,最后没有[ret]

      ;DS=4GB、ES=core_data
      mov ax, all_data_seg_sel
      mov ds, ax
      mov ax, core_data_seg_sel
      mov es, ax

      ;从TCB链中找到一个繁忙的任务，就是当前要结束的任务。
      ;因为是要结束的用户任务，调用了全局空间的公共函数，此时还是在当前任务，只不过是全局空间罢了。
      
      mov eax, [es:tcb_head]
   .find_buzy_tcb:
      cmp word [eax+0x04], 0xffff
      je .skil
      ;继续寻找下一个
      mov eax, [eax+0x00]                       ;cur=cur.next
      jmp .find_buzy_tcb                        ;不可能发生找不到繁忙的节点，如果有那就是出BUG了

   ;EAX=繁忙的TCB
   .skil:
      mov word [eax+0x04], 0x3333


      mov eax, [es:tcb_head]                    ;从队首找空闲节点
   .find_idle_tcb:
      cmp word [eax+0x04], 0
      je .start_jmp
      mov eax, [eax+0x00]
      jmp .find_idle_tcb
      
   .start_jmp:
      not word [eax+0x04]                       ;EAX=从收队开始找的一个空闲节点
      jmp far  [eax+0x14]                       ;硬件任务切换
      
  ;TODO-Tips: 没有【ret】，因为任务已经终止

 do_task_clear:                                 ;任务清理操作, TODO-Todo: 未完成
      ;搜索TCB链表，找到状态为终止的节点
      ;将节点从链表中拆除
      ;回收任务占用的各种资源（可以从它的TCB中找到）
      ;TODO-Tips: 要在GDT中删除该任务的LDT,导致GDT中后面的描述符都要整体移动,耗费性能.
      ;TODO-Optimize: 现代操作系统还在使用LDT吗? 我感觉没有了吧? 后面有分页式内存管理
      retf
;============================== sys_routine END =====================================


;============================== core_data STR ================================
SECTION core_data   vstart=0
      pgdt        dw 0                          ;GDT界限=长度-1
                  dd 0x0000_0000                ;GDT起始线性地址

      ram_alloc   dd 0x0010_0000                ;用户程序动态内存分配起始线性地址（未开启分页就是物理地址）

      core_buf    times 2048 db 0               ;内核数据缓冲区，不用被缓冲区吓到，本质上就是连续的内存，方便内核对数据进行加工、操作的


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

      
      salt_3      db '@terminateProgram'
                  times 256-($-salt_3) db 0
                  dd terminate_current_task
                  dw sys_routine_seg_sel
      
      salt_last:
      salt_4      db '@taskSwitch'
                  times 256-($-salt_4) db 0
                  dd initiative_task_switch
                  dw sys_routine_seg_sel    

      salt_item_size    equ   $-salt_last
      salt_item_count   equ   ($-salt)/salt_item_size       ;常量不占汇编地址
      ;-------------------------SALT--------------------

      msg_enter_core          db            'Core enter success................', 0
      msg_load_relocate_ok    db 0x0d,0x0a, '[Core Task]: User program load relocate success', 0
      msg_start_user_program  db 0x0d,0x0a, '[Core Task]: Start enter User program..........', 0
      msg_again_enter_core    db 0x0d,0x0a, '[COre Task]: Core again enter success !!!!!!!!!', 0       

      msg_test_call_gate      db 0x0d,0x0a, 'In core, test call gate...........', 0

      msg_core_task_run       db 0x0d,0x0a, '[Core Task]: core task runing CPL=0', 0

      msg_core_hlt            db 0x0d,0x0a, '[Core Task]: not more task, core hlt', 0

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
      ret
 ;------------------------------------------------------------
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
 ;------------------------------------------------------------
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
      mov esi, [ss:ebp+11*4]  

   .ldt:             
      mov ecx, 160                              ;为用户程序的LDT分配20个描述符
      call sys_routine_seg_sel:allocate_memory  ;ecx=分配内存的起始线性地址

      ;放入tcb中
      mov dword [es:esi+0x0c], ecx              ;LDT起始线性地址
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
   ;DS=4GB
   ;EAX=用户程序所占字节数(512对齐)
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

   
   
   .setup_user_program_descriptor:              ;DS=4GB、EDI=用户程序起始线性地址（将被修改）、ESI=TCB起始线性地址
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
      mov [edi+0x04], cx                        ;文件头大小字段以后=程序头部选择子
      mov [esi+0x044],cx                        ;程序头部选择子，登记到TCB

      ;代码段描述符
      mov eax, edi
      add eax, [edi+0x0c]                       ;代码段基地址
      mov ebx, [edi+0x10]                       
      dec ebx                                   ;段界限
      mov ecx, 0x0040_f800                      ;代码段属性；G DB L AVL=0100、段界限=0 | P DPL S=1_11_1、TYPE=1000（只执行）；TODO-Tips：DPL=3
      call sys_routine_seg_sel:make_seg_descriptor

      mov ebx, esi                              ;TCB起始线性地址
      call install_ldt_descriptor
      or cx, 0B00000000_00000_011               ;RPL=3
      mov [edi+0x0c], cx                        ;代码段选择子到用户程序头部相关字段


      ;数据段描述符
      mov eax, edi
      add eax, [edi+0x14]                       ;数据段基地址
      mov ebx, [edi+0x18]                       
      dec ebx                                   ;段界限
      mov ecx, 0x0040_f200                      ;数据段属性；G DB L AVL=0100、段界限=0 | P DPL S=1111、TYPE=0010（可读可写）
      call sys_routine_seg_sel:make_seg_descriptor

      mov ebx, esi                              ;TCB起始线性地址
      call install_ldt_descriptor
      or cx, 0B00000000_00000_011               ;RPL=3
      mov [edi+0x14], cx                        ;数据段到用户程序头部相关字段

      ;栈段描述符
      mov eax, edi
      add eax, [edi+0x1c]                       ;栈段基地址
      mov ebx, [edi+0x20]                       
      dec ebx                                   ;段界限
      mov ecx, 0x0040_f200                      ;栈段属性；G DB L AVL=0100、段界限=0 | P DPL S=1111、TYPE=0010（可读可写）
      call sys_routine_seg_sel:make_seg_descriptor

      mov ebx, esi                              ;TCB起始线性地址
      call install_ldt_descriptor
      or cx, 0B00000000_00000_011  
      mov [edi+0x1c], cx                        ;栈段选择子到用户程序头部相关字段

   
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
   ;DS=core_data、ESI=c-salt表起始偏移量
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
   
   ;DS=core_data、ES=4GB、ESI=TCB起始线性地址
   .create_PL_stack:                            ;创建不同特权级的栈段，放在LDT中
      mov esi, [ss:ebp+11*4]                    ;tcb起始线性地址
      
      ;创建0特权级栈_4kb长度，并登记到TCB
      mov ecx, 0
      mov [es:esi+0x1a], ecx                    ;段界限，实际长度是0+1
      inc ecx
      shl ecx, 12                               ;栈实际所占字节数(4KB对齐)
      push ecx
      call sys_routine_seg_sel:allocate_memory  ;输入ecx（希望分配字节数）、输出ecx（分配内存的起始线性地址）
      mov [es:esi+0x1e], ecx                    ;0特权级栈基地址,感觉是多余的,完全可以从LDT中获取到.
      ;栈内存段在LDT中安装
      mov eax, ecx                              ;栈内存基地址
      mov ebx, [es:esi+0x1a]                    ;段界限
      mov ecx, 0x00c0_9200                      ;DPL=0
      call sys_routine_seg_sel:make_seg_descriptor
      mov ebx, esi
      call install_ldt_descriptor               
      and cx, 0B11111111_11111_1_00             ;RPL=00，默认就是00，这行代码可以忽略， CPL==0，CPL要与栈特权级时时刻刻相同
      mov [es:esi+0x22], cx                     ;0特权级栈选择子（在LDT中），登记到TCB
      pop dword [es:esi+0x24]                   ;栈长度（所占字节数），登记到TCB


      ;创建1特权级栈，CPL==RPL==栈段DPL，并登记到TCB
      mov ecx, 0
      mov [es:esi+0x28], ecx                    ;段界限，实际长度是0+1
      inc ecx
      shl ecx, 12                               ;栈实际所占字节数(4KB对齐)
      push ecx
      call sys_routine_seg_sel:allocate_memory  ;输入ecx（希望分配字节数）、输出ecx（分配内存的起始线性地址）
      mov [es:esi+0x2c], ecx                    ;1特权级栈基地址,感觉是多余的,完全可以从LDT中获取到.
      ;栈内存段在LDT中安装
      mov eax, ecx                              ;栈段基地址
      mov ebx, [es:esi+0x28]                    ;段界限
      mov ecx, 0x00c0_b200                      ;DPL=1
      call sys_routine_seg_sel:make_seg_descriptor
      mov ebx, esi
      call install_ldt_descriptor
      or cx, 0B00000000_00000_0_01              ;RPL=01，默认00
      mov [es:esi+0x30], cx                     ;1特权级栈选择子（在LDT中），登记到TCB
      pop dword [es:esi+0x32]                   ;栈长度（所占字节数），登记到TCB

      ;创建2特权级栈，CPL==RPL==栈段DPL，并登记到TCB
      mov ecx, 0
      mov [es:esi+0x36], ecx                    ;段界限，实际长度是0+1
      inc ecx
      shl ecx, 12                               ;栈实际所占字节数(4KB对齐)
      push ecx
      call sys_routine_seg_sel:allocate_memory  ;输入ecx（希望分配字节数）、输出ecx（分配内存的起始线性地址）
      mov [es:esi+0x3a], ecx                    ;1特权级栈基地址,感觉是多余的,完全可以从LDT中获取到.
      ;栈内存段在LDT中安装
      mov eax, ecx                              ;栈段基地址
      mov ebx, [es:esi+0x36]                    ;段界限
      mov ecx, 0x00c0_d200                      ;DPL=2
      call sys_routine_seg_sel:make_seg_descriptor
      mov ebx, esi
      call install_ldt_descriptor
      or cx, 0B00000000_00000_0_10              ;RPL=02，默认00
      mov [es:esi+0x3e], cx                     ;2特权级栈选择子（在LDT中）记录到TCB
      pop dword [es:esi+0x40]                   ;栈长度（所占字节数）记录到TCB

   .ldt_to_gdt:                                 ;安装LDT(整个表)到GDT中，因为LDT也是个内存段
      mov eax, [es:esi+0x0c]                    ;LDT起始线性地址
      movzx ebx, word [es:esi+0x0a]             ;LDT长度-1（整个表的长度-1）,LDT当前界限，最大64KB
      mov ecx, 0x0000_8200                      ;LDT的属性。TODO-Think：为什么DPL=0
      call sys_routine_seg_sel:make_seg_descriptor
      call sys_routine_seg_sel:install_gdt_descriptor ;CX=当前描述符选择子， TI=0、RPL=00
      mov [es:esi+0x10], cx                     ;回填到TCB，LDT在GDT中的选择子

   .create_tss:                                 ;创建当前任务的TSS
      ;创建并将TSS登记到TCB中
      mov ecx, 104                              ;TSS基准尺寸
      dec ecx
      mov [es:esi+0x12], cx                     ;登记到TCB，TSS段界限（16位）
      inc ecx
      call sys_routine_seg_sel:allocate_memory
      mov [es:esi+0x14], ecx                    ;登记到TCB，TSS起始线性地址

      ;ECX=TSS起始线性地址
      ;ESI=TCB起始线性地址
      ;初始化TSS各个字段
      mov word [es:ecx+0x00], 0                 ;前一个任务的TSS段选择子

      stack_field:
      mov eax, [es:esi+0x24]                    
      mov dword [es:ecx+4], eax                 ;0特权级ESP

      mov ax, [es:esi+0x22]                     
      mov word [es:ecx+8], ax                   ;0特权级SS

      mov eax, [es:esi+0x32]                    
      mov dword [es:ecx+12], eax                ;1特权级ESP

      mov ax, [es:esi+0x30]                     
      mov word [es:ecx+16], ax                  ;1特权级SS

      mov eax, [es:esi+0x40]                    
      mov dword [es:ecx+20], eax                ;2特权级ESP

      mov ax, [es:esi+0x3e]                     
      mov word [es:ecx+24], ax                  ;2特权级SS

      PDBR_field:
      mov dword [es:ecx+28], 0                  ;CR3，当前为开启分页，暂时为0

      ;第一次切换任务时，通用寄存器的内容不重要。
      ;以后【切换任务】时旧任务的各个状态会被CPU自动保存到旧任务的TSS中
      
      eflags_field:                             ;IOPL、IF位相当重要，确保万无一失
                                                ;当CPL>IOPL时不允许执行popf、iret，否则触发中断；可以执行cli、sti，但没有任何效果
      pushf
      pop eax                                   ;EAX获取EFLAGS值
      or eax, 0x0000_0200                       ;IF=1响应中断、OF DF IF TF=0010=0x2
      and eax, 0xffff_4fff                      ;0100=0x4、IOPL=0，在数值上CPL<=IOPL则I/O读写不受限制
      mov [es:ecx+36], eax                      ;TSS.EFLAGS
      
      eip_cs:
      mov ebx, [es:esi+0x06]                    ;用户程序起始线性地址

      mov eax, [es:ebx+0x08]                    ;用户程序入口（已被重定位过）（在代码段内的偏移量）
      mov [es:ecx+32], eax                      ;TSS.EIP

      mov ax, [es:ebx+0x0c]                     ;用户程序代码段段选择子（已被重定位过）
      mov [es:ecx+76], ax                       ;TSS.CS

      ;TODO-Tips：应该担心ds是内核数据段，切换到用户态，用户程序就可以访问到内核数据了吗？
      ;TODO-Todo：重点关注

      .ss_field:
      mov ax, [es:ebx+0x1c]                     ;用户程序栈段选择子（已被重定位过）
                                                ;用户程序的栈内存目前不是内核动态分配，是由用户程序指定大小
      mov [es:ecx+80], ax
     
      .ds_field:
      mov ax, [es:ebx+0x04]                     ;用户程序头部段选择子
      mov [es:ecx+84], ax                       ;TSS.ds=头部选择子

      .es_fs_gs_field:
      mov word [es:ecx+72], 0                   ;TSS.es
      mov word [es:ecx+88], 0                   ;TSS.fs
      mov word [es:ecx+92], 0                   ;TSS.gs
      mov ax, [es:ebx+0x1c]
      mov word [es:ecx+80], ax                  ;TSS.ss 

      
      ldt_field:
      mov ax, [es:esi+0x10]
      mov [es:ecx+96], ax                       ;LDT段选择子（在GDT中）

      iomap_field:
      mov dword [es:ecx+100], 0x0067_0000       ;T=0、I/O映射基地址=0x0067=103（无IO许可位）

   .tss_to_gdt:
      mov eax, [es:esi+0x14]                    ;TSS起始线性地址
      movzx ebx, word [es:esi+0x12]             ;段界限
      mov ecx, 0x0000_8900                      ;TSS内存段描述符属性
                                                ;P DPL S=1000 \ TYPE=1001，B位=0
      call sys_routine_seg_sel:make_seg_descriptor
      call sys_routine_seg_sel:install_gdt_descriptor
      mov [es:esi+0x18], cx                     ;登记TSS选择子到TCB，PRL=00， CPL&RPL <=0 才能访问该数据段。
                                                ;TODO-Tips：从这里也能看出，RPL是由操作系统控制的，CPU只负责检查RPL与CPL的合法性，不负责鉴别PRL的真实性，真实性由操作系统鉴别。
      

   .return:
      pop es
      pop ds
      popad
      ret 8                                     ;过程的编写者最清楚栈中有几个参数，丢弃8byte的参数，即过程返回到调用本函数的下一条指令后，ESP <- ESP+8
 ;------------------------------------------------------------
 create_user_program:                           ;创建一个用户程序
                                                ;输入：EDI=用户程序起始逻辑扇区号
                                                ;输出：无
      push ecx
      push ebx

   .create_tcb:
      mov ecx, 0x46                             ;tcb size
      call sys_routine_seg_sel:allocate_memory  ;ecx=分配内存的起始线性地址
      call append_tcb

   .load_relocate:
      push dword edi
      push ecx                                  ;ecx=分配内存的起始线性地址、也等于当前tcb起始线性地址
      call load_relocate_user_program

      mov ebx, msg_load_relocate_ok
      call sys_routine_seg_sel:put_string
   
   .return:
      pop ecx
      pop ebx
      ret
 ;------------------------------------------------------------
 ;DS=core_data、ES=4GB
start:
      call sys_routine_seg_sel:clear

      mov ax, core_data_seg_sel
      mov ds, ax

      mov ax, all_data_seg_sel
      mov es, ax

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

 .create_core_tcb:
      mov ecx, 0x46
      call sys_routine_seg_sel:allocate_memory  ;输出：ECX=TCB起始线性地址
      call append_tcb                           ;输入：ECX
      mov esi, ecx
 ;ESI=TCB起始线性地址                              
 .create_core_tss:
      mov ecx, 103
      mov [es:esi+0x12], ecx                    ;登记TSS界限到TCB
      inc ecx                                   ;TSS长度
      call sys_routine_seg_sel:allocate_memory  ;输出：ECX=TSS起始线性地址
      
      mov [es:esi+0x14], ecx                    ;登记TSS基地址到TCB
      mov word [es:esi+0x04], 0xffff            ;登记任务状态到TCB（繁忙）即该TCB接下来即将被运行
      
      ;初始化TSS各个字段
      mov word [es:ecx+0], 0                    ;上一个任务的TSS选择子（现代操作系统不使用）
      ;0特权级的内核任务不需要不同的特权级栈（不能call到低特权级任务）
      mov dword [es:ecx+28], 0                  ;CR3
      mov word [es:ecx+96], 0                   ;LDT选择子（在GDT中）内核任务不需要，内核任务的内存描述符在GDT中安装
      mov dword [es:ecx+100], 0x0067_0000       ;0x67=I/O映射基地址，0特权级I/O读写不限制

 .core_tss_to_gdt:
      mov eax, [es:esi+0x14]                    ;TSS基地址
      movzx ebx, word [es:esi+0x12]             ;TSS界限值
      mov ecx, 0x0000_8900                      ;TSS描述符属性                                 
      call sys_routine_seg_sel:make_seg_descriptor
                                                ;G DB L AVL=0000、段界限=0
                                                ;P DPL S=1000、TYPE=1001，其中TYPE的第2位是B位，表示忙不忙，切换到一个任务时CPU会自动将该位置为1，在内存中也是吗？还只是在描述符高速缓冲器中修改？。
                                                ;TODO-Tips：一会用Bochs测试一下，我猜测是高速缓冲器和内存都修改。猜测正确
      call sys_routine_seg_sel:install_gdt_descriptor

      mov [es:esi+0x18], cx                     ;登记TSS选择子（在GDT中）TI=0、RPL=0到TCB

      ;ltr r16/m16 将TSS选择子（GDT中）送到tr寄存器，
      ;然后再去GDT中加载对应的TSS描述符到tr的描述符高速缓冲器中，并将B位置为1（繁忙）
      ;任务寄存器TR中的内容是任务存在的标志，该内容也决定了当前任务是谁。
      ;下面的指令为当前正在执行的0特权级任务“程序管理器”后补手续（TSS）。
      ;当前任务是内核任务，TR指向内核任务的TSS
      ltr cx                                    

      mov ebx, msg_core_task_run
      call sys_routine_seg_sel:put_string


 .load_relocate_user_program:
      mov edi, 50                               ;用户程序LBA
      call create_user_program

      mov edi, 70                               ;第二个用户程序LBA
      call create_user_program
 
 .do_switch:
      xchg bx, bx
      call sys_routine_seg_sel:initiative_task_switch

      mov ebx, msg_again_enter_core
      call sys_routine_seg_sel:put_string 

      ;任务清理操作
      ; call sys_routine_seg_sel:do_task_clear


      mov ebx, [tcb_head]
 .find_ready:                                   ;找是否还有空闲任务
      cmp word [es:ebx+0x04], 0                 ;当前TCB是否是空闲任务
      je .do_switch                             ;当前TCB是空闲
      mov ebx, [es:ebx+0x00]                    ;继续找下一个, cur=cur.next
      or ebx, ebx
      jnz .find_ready                            ;TCB链表没遍历完
      
      ;没有空闲任务了,内核睡眠
      mov ebx, msg_core_hlt
      call sys_routine_seg_sel:put_string
      hlt
;============================== core_code END =============================



SECTION tail
 file_end: