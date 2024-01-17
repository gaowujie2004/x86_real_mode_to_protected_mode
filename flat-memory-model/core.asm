      ;任务：内核程序，已开启分页模式
      ;用户程序1 LBA=50、用户程序2 LBA=70
      
      video_card_index_port   equ     0x3d4                       ;显卡的功能索引寄存器端口
      video_card_data_port    equ     0x3d5                       ;显卡的数据寄存器端口
      cursor_h8_port          equ     0x0e                        ;光标寄存器索引端口，高8位
      cursor_l8_port          equ     0x0f                        ;光标寄存器索引端口，低8位， 00表示光标在第0行0列，80表示光标在1行0列，这里的值其实是偏移量。行、列从0开始。
      CR                      equ     0x0d                        ;CR回车ASCII码值，当前首行
      LF                      equ     0x0a                        ;LF换行ASCII码值，垂直下行，不是下行的首行

      rtc_index_port          equ     0x70                        ;NMI打开/关闭在最高位，1关闭、0打开
      rtc_data_port           equ     0x71
      
     
      flat_core_code_seg_sel  equ   0x0008                        ;内核（0特权级）4GB代码段选择子
      flat_core_data_seg_sel  equ   0x0010                        ;内核（0特权级）4GB数据段选择子（栈段）
      flat_user_code_seg_sel  equ   0x001b                        ;用户（3特权级）4GB代码段选择子
      flat_user_data_seg_sel  equ   0x0023                        ;用户（3特权级）4GB数据段选择子（栈段）

      text_buffer_lin_address equ   0x800b_8000                   ;文本模式缓冲区起始线性地址


      ;0x8000_0000-0x8010_0000（虚拟地址）映射到 0x0000_0000-0x0010_0000（物理地址） 
      core_pdt_address        equ   0x0002_0000                   ;内核全局空间页目录表起始物理地址
      core_low_1mb_pt_address equ   0x0002_1000                   ;内核全局空间，低端物理1MB映射页表物理地址，第一个页表的物理地址


      app1_start_sector       equ     50                          ;用户程序1所在逻辑扇区号（LBA）
      app2_start_sector       equ     70                          ;用户程序2所在逻辑扇区号（LBA）


      idt_linear_address      equ     0x8001_f000                 ;中断描述符表的起始线性地址

      core_lin_alloc_at       equ     0x8010_0000                 ;内核中可用于分配的内存的起始线性地址
      core_lin_tcb_addr       equ     0x8001_f800                 ;内核任务TCB的高端线性地址

;=============================== header STR =================================
SECTION header vstart=0x8004_0000
      core_length      dd file_end              ;核心程序总长度#00

      core_entry       dd start                 ;核心代码段入口点#04
;=============================== header END =========================================


;============================== sys_routine STR ==============================
      [bits 32]
SECTION sys_routine vfollows=header
 put_char:                                      ;打印一个字符
                                                ;输入：cl=ASCII码
      pusha

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
      je .put_cr                                ;相等则跳转，是回车，首行

      cmp cl, LF
      je .put_lf                                ;相等则跳转，是换行，垂直下行

      jmp .put_other

      ;首行，BX=光标为当前首行
   .put_cr:
      mov ax, bx
      mov bl, 80
      div bl                                    ;ax/bl=al....ah， 

      mul bl                                    ;al*bl=ax    ax是当前行的首行索引，得再乘80，才是偏移量
      mov bx, ax
      jmp .set_cursor

      ;垂直下一行, BX=垂直下一行
   .put_lf:
      add bx, 80                                  
      jmp .scroll_check

      ;可显示的字符，cl=字符值
   .put_other:
      mov eax, 0                                ;eax高16位可能不干净                                          
      mov ax, bx
      shl eax, 1                                ;乘2
      mov [text_buffer_lin_address + eax], cl    
      inc bx                                    ;推进光标

      ;光标越界检查，是否需要滚动屏幕
   .scroll_check:
      cmp bx, 2000                              ;25*80=2000, 最后一个偏移量1999
      jl .set_cursor                            ;<则跳转

      ;接下来是>=，滚动屏幕
   .scroll_screnn:                              ;第1行移到第0行，。。。。最后一行置空。去除最后1行，共24行，一共24*80个字符
      sub bx, 80                                ;向上移了一行，光标也需要对应移动
      ;24*80/4, [es:edi] <- [ds:esi]
      
      mov edi, text_buffer_lin_address + 0
      mov esi, text_buffer_lin_address + 160    ;段选择子指向的描述符基地址是0，4GB内存区域
      mov ecx, 24*80*2/4                        ;应该是字符数*2才是字节数
      cld                                       ;DF=0，edi、esi方向增加
      rep movsd                                 ;edi、esi步长是4（双字）rep movsd，32位保护模式时，使用的是ecx
      ;最后一行置空（黑底白字空格字符）
      mov ecx, 40                               ;本来是要循环160次的，但现在每次四个字节操。 TODO:Think-CPU和数据总线拥有更宽的数据通路，所执行的指令个数就会减少，充分体现了性能的优化
      mov esi, 3840
      .loop_cls:
      mov dword [esi], 0x07200720               ;在上一步rep movsd结束时，esi偏移量是24*80*2，符合预期。（行从0开始）
      add esi, 4 
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
      popa
      ret


 put_string:                                    ;文本模式下格式化字符串打印
                                                ;输入：ds:ebx字符串首地址，0x00结束
                                                ;输出：无
      push ecx
      push ebx
      pushf

      cli

   .each_char:                                 
      mov cl, [ebx]
      or cl, cl                                 ;为了性能，or 操作数全是寄存器，并且不会改变or的值，影响ZF标志位（其他标志位的影响不确定）TODO-Optimize：操作数全是寄存器，没有立即数，也没有内存寻址，所以很快，而且指令所占字节数少
      jz .put_string_return                     ;等于零则跳转（ZF=1)
      call put_char                             ;输入：cl=ASCII码
      inc ebx
      jmp .each_char

   .put_string_return:
      popf
      pop ebx
      pop ecx
      ret

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
      ret

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
   
   .return:
      pop ebx
      ret
 
 install_gdt_descriptor:                        ;将64位描述符安装到GDT中
                                                ;输入：EDX(h32):EAX(l32)=描述符
                                                ;输出：CX=当前描述符选择子， TI=0、RPL=00
      push ebx
      

      ;1.计算新描述符在表中偏移量
      sgdt [pgdt]
      movzx ebx, word [pgdt]                    ;旧gdt界限值
      inc bx                                    ;界限值+1等于长度，即新描述符在表中的偏移量
                                                ;TODO-Tips：重点关注，加电预置的GDT界限是0xFFFF，第一次安装时0xFFFF+1=0x1_0000，但bx只存储2byte，进位舍弃
                                                ;若使用ebx，则进位还会保留，因为ebx 4byte
      add ebx, [pgdt+2]                         ;偏移量+gdt起始线性地址=新描述符在GDT中的线性地址

      ;2.安装                                                                 
      mov [ebx], eax                     
      mov [ebx+4], edx

      ;3.更新GDT界限值
      add word [pgdt], 8

      ;4.加载新的gdt
      lgdt [pgdt]

      ;5.计算新描述符对应的选择子
      mov cx, [pgdt]
      shr cx, 3                                ;ecx除8，描述符在表中的索引
      shl cx, 3                                ;将索引值移到选择子索引位，低3为位中TI=0，RPL=00


      pop ebx
      ret
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

   .chech_RPL:
      ;TODO-Tips：从这里也能看出，RPL是由操作系统控制的，CPU只负责检查RPL与CPL的合法性，不负责鉴别PRL的真实性，真实性由操作系统鉴别。
      ;操作系统很显然是知道请求者的CPL的，这样就可以判断并修改RPL
      ;在平坦模式下调用该函数，采用的是相对调用，不会压入CS，那么此时从栈中取出的CS就是错的
      ;保险的方法是从TCB链表中取出CS?
      xchg bx, bx
      mov cx, ds                    
      call get_current_task_pl                  ;输出：DX=调用者CS
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
      pop ebx
      pop ecx
      pop edx 
      pop eax
      ret
 
 
 read_disk_hard_custom:                         ;读取主硬盘指定字节数，内部实际读取扇区数是向上舍入的
                                                ;输入：ds:ebx = 硬盘数据缓冲区
 
 clear:                                         ;清除屏幕字符
      pushad
      
      mov edi, text_buffer_lin_address
      mov ecx, 1000                             ;2000*2/4
   .loop_clear:
      mov dword [edi], 0x0720_0720              ;0B0000_0111=0x07
      add edi, 4
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
      ret

 allocate_memory:                               ;动态分配内存，在当前任务自己的虚拟内存空间里分配的
                                                ;输入：ECX=希望分配的字节数
                                                ;输出：ECX=分配的内存的起始线性地址（当前任务自己的虚拟内存空间中的线性地址）
      push ebx
      ;在TCB链表中找到当前任务（状态为繁忙的）TCB节点
      ;从中取出内存分配的起始线性地址，基于这个地址开始分配内存

      mov ebx, [tcb_head]                       ;TCB第一个节点起始线性地址
   ;一定存在繁忙的节点
   .find_current_task_tcb:                 
      cmp word [ebx+0x04], 0xffff            ;cur TCB 状态      
      je .find_ok              
      mov ebx, [ebx+0x00]                    ;cur=cur.next
      jmp .find_current_task_tcb                
      
   .find_ok:
      call task_allocate_memory

   .return:
      pop ebx
      ret

 task_allocate_memory:                          ;为指定任务的虚拟内存空间分配内存
                                                ;输入：EBX=指定任务的TCB节点线性地址
                                                ;     ECX=分配的字节数
                                                ;输出：ECX=本次分配内存的起始线性地址
      ;从当前TCB中获取内存分配起始线性地址（这个线性地址是当前任务的虚拟内存空间的）
      push eax

      push ebx                                  ;to A

      mov ebx, [ebx+6]                          ;本次内存分配的起始线性地址
      mov eax, ebx                              ;eax暂存：本次内存分配的起始线性地址
      add ecx, ebx                              ;下一次内存分配的起始线性地址

      push ecx                                  ;to B，下一次内存分配的起始线性地址

      ;将起始和结束地址，本次内存分配占用多少个物理页
      and ebx, 0xffff_f000
      and ecx, 0xffff_f000
      
   .next:
      ;为当前线性地址安装物理页
      call alloc_install_a_page;输入ebx
      add ebx, 0x1000                           ;下一个物理页
      cmp ebx, ecx
      jle .next                                 ;当ebx、ecx在一个物理页内，此时他俩相等
      
   .force_align:
      ;将下一次内存分配的起始线性地址强制按4字节对齐（32位CPU）
      pop ecx                                   ;B，下一次内存分配的起始线性地址
      mov ebx, ecx
      and ecx, 0xffff_fffc                      ;4字节对齐意味着低两位必须是0，先让其向下4字节对齐
      add ecx, 4
      test ebx, 0x0000_0003                     
      cmovz ecx, ebx                            ;等于0，即ecx本身是4字节对齐；ebx=原始ecx

   .write:
      pop ebx                                   ;A，指定任务的TCB节点线性地址

      mov [ebx+6], ecx                       ;更新，下一次内存分配的起始线性地址

      mov ecx, eax                              ;return-value: 本次内存分配的起始线性地址

      pop eax
      ret

 alloc_install_a_page:                          ;为一个线性地址分配一个物理页
                                                ;并安装在当前任务的层级分页结构中（页目录表、页表中）
                                                ;输入：EBX=页的线性地址
                                                ;输出：无
      push eax
      push esi
      push ecx

   ;判断当前线性地址是否有页目录项（是否分配页表）、页表项（是否分配物理页）；没有就分配、页表、物理页然后安装

   .check_page_table:
      ;1.当前线性地址是否分配页表
      ;相当于把页目录表当做普通的物理页来访问，获取目录项的值，页目录表内偏移量作为页内偏移量
      ;页目录表索引（高10位）*4 得到的是页目录表内偏移量，值是对应页表起始物理地址
      mov esi, ebx
      and esi, 0xffc0_0000                      ;保留高10位，其余位清零
      shr esi, 20                               ;将高10位移到低10位（右移22位），然后乘以4（左移2位），即右移20位。页目录表内偏移量移到低12位，作为页内偏移量
      or esi, 0xffff_f000                       ;高20位变为1，自有妙用：得到的是页目录表自身物理地址

      ;检查对应的页表是否分配
      test dword [esi], 0x0000_0001             ;目录项P位
      jnz .check_physics_page                   ;不等于0说明分配页表了
      ;等于0，开始分配页表并登记
      call alloc_a_4kb_page ;物理页作为页表
      or eax, 0x0000_0007                       ;目录项属性，0111-US-RW-P，3特权级可访问
      mov [esi], eax                            ;登记:页目录表项中记录页表物理地址

   .clear_page_table:
      ;清空分配的页表。
      ;这样做：把页表当做一个物理页，页目录表索引作为页表索引，这样定位到页表，把页表当做普通的物理页，变量值作为页内偏移量，就可定位到1个页表内1024个页表项
      mov esi, ebx
      and esi, 0xffc0_0000                      ;只留页目录表索引
      shr esi, 10                               ;页目录表索引，变为页表索引
      or  esi, 0xffc0_0000                      ;高10位全变为1
      mov ecx, 1024
    .each_clear_table_item:
      mov dword [esi], 0
      add esi, 4 
      loop .each_clear_table_item


   .check_physics_page:
      ;2.检查该线性地址对应的页表项（物理页）是否存在
      ;相当于把页表当做物理页，页表内偏移量作为页内偏移量访问得到物理页，最终得到页表项的值
      mov esi, ebx
      and esi, 0xffff_f000                      ;清除页内偏移部分
      shr esi, 10                               ;中间10位右移12位，作为页内偏移量，但现在还是索引，需要乘以4变为偏移量（左移2两位）
      or esi, 0xffc0_0000                       ;高10位为1

      test dword [esi], 0x0000_0001             ;页表项P位
      jnz .return                               ;不等于0说明分配物理页了
      ;等于0，没有分配物理页，开始分配并登记（页表中登记）
      call alloc_a_4kb_page
      or eax, 0x0000_0007
      mov [esi], eax                      ;登记：页表项中记录物理页的物理地址


   .return:
      pop ecx
      pop esi
      pop eax
      ret

 alloc_a_4kb_page:                              ;在物理内存中分配一个4KB物理页
                                                ;输入：无
                                                ;输出：EAX=4KB物理页的起始物理地址
      ;在内存中找到空闲的页，然后返回。
      ;从头开始搜索位串，查找空闲的页。具体地说，就是找到第一个为“0”的比特，并记下它在整个位串中的位置，然后再置1
      push ebx

      ;bts（Bit Test and Set）测试位串中的某个位置的比特，用该位置的比特值设置EFLAGS寄存器的CF标志，然后将该位置的比特置“1”。
      ;bts r/m16, r16、bts r/m32, r32
      xor eax, eax
   .find_idle_position:
      bts [page_bit_map], eax
      jnc .ok                                   ;CF=0说明空闲，退出循环
      ;CF=1，非空闲继续找
      inc eax
      cmp eax, page_map_len*8                   ;字节数*8是比特位数
      jl .find_idle_position
      ;没有空闲页，正确的做法是将较少使用的页换出到硬盘，这部分页再分配给需要使用的程序
      mov ebx, msg_not_page
      call put_string
      hlt

   .ok:
      shl eax, 12                               ;eax索引*4096，空闲页物理地址
      
      pop ebx
      ret

 create_copy_cur_pdir:                          ;创建用户任务页目录表
                                                ;输入：无
                                                ;输出: EAX=新页目录表的物理地址
      ;把内核的页目录表的内容复制给一块新的内存，并返回这块内存的物理地址
      push esi
      push edi
      push ecx
      push ebx

      call alloc_a_4kb_page ;EAX=新页目录表起始物理地址
      mov ebx, eax                              ;此线性地址对应当前(core)任务页目录表倒数第二个目录项
      or ebx, 0x0000_0007                       ;0111，US、RW、P
      mov [0xfffff_ff8], ebx                    ;EBX高20位=新页目录表起始物理地址
                                                ;TODO-Think: 这是什么意思？分页模式下即便知道物理地址也不能直接访问，需要通过迂回的方式访问

      invlpg [0xfffff_ff8]                      ;刷新0xfffff_ff8线性地址在TLB中对应的条目，在内存中修改了，但在TLB中是不同步的，需手动刷新

      ;[es:edi] <- [ds:esi]
      ;把内核的页目录表内容复制到新的页目录表
      mov edi, 0xffff_e000                      ;该线性地址对应新的页目录起始位置
      mov esi, 0xffff_f000                      ;该线性地址对应内核页目录表起始位置，该线性地址低12位就是目录表内的偏移量
      mov ecx, 1024                             ;表目录项个数
      cld                                       
      rep movsd                                 ;mov dword [es:edi], [ds:esi]

      pop ebx
      pop ecx
      pop edi
      pop esi
      ret
 
 
 initiative_task_switch:                        ;主动进行任务切换
                                                ;输入：无、 输出：无
      ;怎么进行任务切换？从TCB链表找到繁忙的任务，即当前调用initiative_task_switch的任务的tcb，从这个tcb开始向后找一个空闲的tcb
      ;若向后找不到，则从头开始找，都没找到就退出，任务不切换；若找到了，则把找到的空闲节点的状态反转（变为0xffff，即将开始运行这个任务）
      ;将旧任务（当前任务）的tcb状态也反转一下，变为0，表示空闲。
      push eax
      push ebx
      push esi
      push edi

      ;0个任务或1个任务
      mov eax, [tcb_head]
      or eax, eax
      jz .return                                ;EAX=0,一个任务都没有
      mov eax, [eax+0x00]                       ;cur=cur.next
      or eax, eax
      jz .return 


      mov eax, [tcb_head]
   ;从头找一个繁忙的TCB（当前任务）
   .find_buzy_tcb:
      cmp word [eax+0x04], 0xffff               ;EAX当前TCB节点起始线性地址
      cmove esi, eax                            ;找到繁忙节点，ESI=当前任务TCB起始线性地址
      je .tail_find_idle_tcb                    ;找到繁忙节点                     
      mov eax, [eax+0x00]                       ;没找到，cur=cur.next
      jmp .find_buzy_tcb


   ;ESI=当前任务TCB起始线性地址，留给最后使用
   ;EDI=新任务TCB起始线性地址，留给最后使用
   ;从繁忙节点的下一个节点开始找空闲节点
   .tail_find_idle_tcb:
      mov eax, [eax+0x00]                       ;EAX=繁忙TCB的下一个TCB起始线性地址
      or eax, eax                               ;是否到尾部?
      jz .haed_find_idle_tab                    ;Y，从头找空闲节点
      cmp word [eax+0x04], 0                    ;N，没有到尾部
      cmove edi, eax                            
      je .ok                                     
      jmp .tail_find_idle_tcb

   ;从头开始找空闲节点
   .haed_find_idle_tab:
      mov eax, [tcb_head]
      .for:
      cmp eax, esi
      je .return                                ;cur=当前任务TCB，还没找到空闲，说明根本没有空闲任务
      cmp word [eax+0x04], 0
      cmove edi, eax                            ;为0，则赋值
      je .ok
      mov eax, [eax+0x00]                       ;不相等，cur=cur.next
      jmp .for

   ;ESI=当前任务TCB起始线性地址
   ;EDI=新任务TCB起始线性地址
   ;反转状态，准备切换
   .ok:
      not word [esi+0x04]                       ;将旧任务TCB状态改为空闲
      not word [edi+0x04]                       ;将空闲任务TCB改为忙,即将切换到该任务

   ;保存旧任务（当前任务）的状态
   .save_old_task_info:                   
      mov eax, cr3
      mov [esi+22], eax                         ;TCB.CR3
      mov dword [esi+26], .return               ;TCB.EIP，TODO-Tips: 很重要
      mov [esi+30], cs                          ;TCB.CS
      mov [esi+32], ss                          ;TCB.SS
      mov [esi+34], ds                          ;TCB.DS
      mov [esi+36], es                          ;TCB.ES
      mov [esi+38], fs                          ;TCB.FS
      mov [esi+40], gs                          ;TCB.GS
      
      ;TODO-Think: 不理解。EAX/EBX/ESI/EDI不用保存，在任务恢复执行时将自动从栈中弹出并恢复
      ;TODO-Think: 用户任务进入该函数内，SS、ESP变为0特权级栈（用户局部内存空间）

      mov [esi+50], ecx                         ;TCB.ECX
      mov [esi+54], edx                         ;TCB.EDX
      mov [esi+66], ebp                         ;TCB.EBP
      mov [esi+70], esp                         ;TCB.ESP
      pushf
      pop dword [esi+74]                        ;TCB.EFLAGS

   .resume_task_execute:
      jmp resume_task_execute                   ;TODO-Think: 可以用call吗？
      

   .return:
      pop edi
      pop esi
      pop ebx
      pop eax
      ret

 resume_task_execute:                           ;恢复指定(新)任务的执行
                                                ;输入：EDI=指定任务TCB线性起始地址
                                                ;输出：无
      ;将指定任务的状态从TCB恢复到寄存器
      ;TODO-BuDong: 不理解
      mov eax, [edi+10]                         ;ESP0
      mov [tss+4], eax                          ;新任务的0特权级ESP
                                                ;TSS.SS0=flat_core_data_seg_sel。创建内核时已设置

      ;新任务是内核任务：TCB.CS.DPL=0，不进行栈切换（不改变SS、ESP）
      ;新任务是用户任务（从未运行过的）：TCB.CS.DPL=3，则进行栈切换，将从栈中恢复3特权级栈
      ;新任务是用户任务（以前运行过）：TCB.CS.DPL=0，为什么是0？当前函数的调用者是0x70中断处理程序，它是内核空间下，故CS.DPL=0，或者说SS.DPL=0
                                                
                                                
      mov eax, [edi+22]                         ;TCB.CR3
      mov cr3, eax

      mov ds, [edi + 34]
      mov es, [edi + 36]
      mov fs, [edi + 38]
      mov gs, [edi + 40]
      mov eax, [edi + 42]
      mov ebx, [edi + 46]
      mov ecx, [edi + 50]
      mov edx, [edi + 54]
      mov esi, [edi + 58]
      mov ebp, [edi + 66]

      cmp word [edi+30], 3                      ;TCB.CS.DPL == 3? 和比较TCB.SS是一样的
      je .stack_switch                          ;新任务CS.DPL=3，意味着栈切换了，执行ret、iret时会将栈中旧的SS、ESP弹栈，赋值给SS、ESP寄存器
      ;CPL==TCB.SS.DPL==0，可以直接切换栈，因为CPL==栈DPL。CPU要求时时刻刻CPL==SS.DPL
      mov ss, [edi+32]                          ;TCB.SS  0特权级栈
      mov esp, [edi+70]                         ;TCB.ESP 0特权级栈

   .stack_switch:
      ;新任务的CS.DPL=3，模拟栈切换返回过程
      push word [edi+32]                        ;TCB.SS   3特权级
      push dword [edi+70]                       ;TCB.ESP  3特权级
      
   .do_sw:
      ;无论push的是word还是dword，实际压栈的还是dword大小的数据，不足dword自动补零
      push dword [edi+74]                       ;TCB.EFLAGS
      push word  [edi+30]                       ;TCB.CS
      push dword [edi+26]                       ;TCB.EIP                                     

      mov edi, [edi+62]

      iret
      ;栈切换
      ;jmp、call、ret、iret CPU固件会比较CPL和CS.DPL（目标，即将执行的代码段）
      ;若一样，则不切换栈（不改变SS、ESP）；若不一样，则发生栈切换，具体过程如下：
      ;1. 目标CS.DPL在当前TSS中选择同特权级的栈（同特权级的SS、ESP）
      ;2. 临时保存旧的SS、ESP
      ;3. 改变SS、ESP寄存器为TSS中同特权级的栈
      ;4. 将旧的SS、ESP压栈
 
 get_current_task_pl:                           ;获取当前任务的特权级
                                                ;输入：无
                                                ;输出：DX=任务的特权级
      push eax

      mov eax, [tcb_head]
   ;从头找当前任务TCB
   .find_buzy_tcb:
      cmp word [eax+0x04], 0xffff               ;EAX当前TCB节点起始线性地址
      je .ok                                    ;找到当前节点               
      mov eax, [eax+0x00]                       ;没找到，cur=cur.next
      jmp .find_buzy_tcb

   ;EAX=当前任务TCB起始线性地址
   .ok:
      mov dx, [eax+78]

   .return:
      pop eax
      ret
 
 terminate_current_task:                        ;终止当前任务,
      ;设置当前繁忙的TCB节点为0x3333, 后续由do_task_clear负责内存等清理操作
      ;然后在切换到其他任务,这和do_switch很像

      ;当前任务都要结束了,没必要再保存当前任务的状态了.
      ;TODO-Tips: 注意,最后没有[ret]

      ;从TCB链中找到一个繁忙的任务，就是当前要结束的任务。
      ;因为是要结束的用户任务，调用了全局空间的公共函数，此时还是在当前任务，只不过是全局空间罢了。
      
      mov eax, [tcb_head]
   .find_buzy_tcb:
      cmp word [eax+0x04], 0xffff
      je .skil
      ;继续寻找下一个
      mov eax, [eax+0x00]                       ;cur=cur.next
      jmp .find_buzy_tcb                        ;不可能发生找不到繁忙的节点，如果有那就是出BUG了

   ;EAX=繁忙的TCB
   .skil:
      mov word [eax+0x04], 0x3333


      mov eax, [tcb_head]                    ;从队首找空闲节点
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
      ret

 external_interrupt_handle:                     ;通用的中断处理过程(硬件中断)
      push eax
                                                ;TODO-BUG: 这是很重要的，系统中有很多的外中断，得向8259A发送中断结束命令，要不然8259A就只生成一次中断信号，因为没有收到中断结束信号
      mov al,0x20                               ;中断结束命令EOI
      out 0xa0,al                               ;向从片发送
      out 0x20,al                               ;向主片发送

      pop eax
      iretd

 inside_interrupt_handle:                       ;通用的异常处理(内中断)过程
      push eax
      push ebx
     
      mov ebx, msg_exception
      call put_string
      hlt

      pop ebx
      pop eax
      iret

 rtc_0x70_interrupt_handle:                     ;实时时钟RTC外中断
      push eax
      push ebx

      ;TODO-Tips:可以放在最后吗?
      mov al,0x20                               ;中断结束命令EOI 
      out 0xa0,al                               ;向从片发送 
      out 0x20,al                               ;向主片发送

      ;读取一下,清空RTC-C寄存器
      mov al, 0x0C
      out rtc_index_port, al
      in al, rtc_data_port

      mov ebx, msg_0x70_interrupt
      call put_string

      call initiative_task_switch
      
      pop ebx
      pop eax
      iret

 sys_call_0x88_interrupt_handle:                ;系统调用中断处理程序，软中断
                                                ;输入：EAX=功能号（0-3）
      call [sys_api+eax*4]                      ;间接近调用，EIP <- M[地址]
                                                ;sys_api相当于数组名（数字起始地址）、eax是数组索引、*4得到在数组内的偏移量
      iret

;============================== sys_routine END =====================================


;============================== core_data STR ================================
SECTION core_data vfollows=sys_routine
      pgdt        dw 0                          ;GDT界限=长度-1
                  dd 0x0000_0000                ;GDT起始线性地址
      
      pidt        dw 0                          ;IDT界限=长度-1
                  dd 0x0000_0000                ;IDT起始线性地址

      tss         times 128 db 0                ;TODO-Why: 实际104，但为什么分配128？可能是出于内存对齐的考虑？


      core_buf    times 2048 db 0               ;内核数据缓冲区，不用被缓冲区吓到，本质上就是连续的内存，方便内核对数据进行加工、操作的


      tcb_head    dd 0x0000_0000                ;任务控制块链表

      page_bit_map      db  0xff,0xff,0xff,0xff,0xff,0xff,0x55,0x55
                        db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                        db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                        db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                        ;上面是低1MB物理内存的分配情况，其中0x55 0x55，这个不用担心
                        db  0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55
                        db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                        db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                        db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
      page_map_len      equ $-page_bit_map

      ;----------------------------------系统api-----------
      sys_api           dd put_string
                        dd read_disk_hard_0
                        dd clear
                        dd allocate_memory
      ;----------------------------------系统api-----------

      msg_enter_core          db            'Core enter success................', 0
      msg_load_relocate_ok    db 0x0d,0x0a, '[Core Task]: User program load relocate success', 0
      msg_start_user_program  db 0x0d,0x0a, '[Core Task]: Start enter User program..........', 0
      msg_again_enter_core    db 0x0d,0x0a, '[COre Task]: Core again enter success !!!!!!!!!', 0       

      msg_test_call_gate      db 0x0d,0x0a, 'In core, test call gate...........', 0

      msg_core_task_run       db 0x0d,0x0a, '[Core Task]: core task runing CPL=0', 0

      msg_core_hlt            db 0x0d,0x0a, '[Core Task]: not more task, core hlt', 0

      msg_0x70_interrupt      db 0x0d,0x0a, '[0x70]: hhhh, This is Test!.........', 0

      msg_exception           db 0x0d,0x0a, '[exception_handler_interrupt]: hlt..', 0 

      msg_flush               db 0x0d,0x0a, '[Core Task]: flush success..........', 0 

      msg_not_page            db 0x0d,0x0a, '[Sys]: ********No more pages********', 0

      cpu_brand0              db 0x0d,0x0a, 'Down is cpu brand info:', 0x0d,0x0a, 0x20,0x20,0x20,0x20, 0
      cpu_brand               times 49 db 0,                ;存放cpuinfo需48byte，额外的结束0，共49byte
;============================== core_data END =======================================


;============================== core_code STR =================================
SECTION core_code vfollows=core_data
 append_tcb:                                    ;添加tcb到tcb链表尾部
                                                ;输入：ECX=tcb起始线性地址
      push eax
      push ebx
      pushf

      cli                                       ;防止在添加任务的时候，进行任务切换（时钟1秒切换一次）


   .new_node_next_clear:
      mov dword [ecx+0x00], 0                   ;因为是线性地址，所以使用es（0-4gb)段

   .is_empty:
      mov eax, [tcb_head]
      or eax, eax
      jz .empty
   
   .find_last:
      mov ebx, eax                              ;暂存eax（当前节点地址）
      mov eax, [ebx+0x00]                       ;第一个节点的.next的值
      or eax, eax
      jnz .find_last

   .set_last_node:
      mov [ebx+0x00], ecx                       ;最后一个tcb的起始线性地址
      jmp .return

   .empty:
      mov [tcb_head], ecx

   .return:
      sti
      popf
      pop ebx
      pop eax
      ret
 ;------------------------------------------------------------
 install_ldt_descriptor:                        ;在ldt中安装一个描述符
                                                ;输入：EDX(h32):EAX(l32)=64位段描述符
                                                ;      EBX=tcb起始线性地址
                                                ;输出：cx=描述符选择子
      push esi
      
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
      ret
 ;------------------------------------------------------------
 load_relocate_user_program:                    ;加载重定位用户程序，通过栈传递参数
                                                ;输入：PUSH 用户程序起始逻辑扇区号
                                                ;      PUSH 当前TCB起始线性地址
                                                ;返回：无
      pushad

      mov ebp, esp
      ;ebp+9*4 = TCB起始线性地址
      ;ebp+10*4 = 用户程序起始LBA

      ;ESI=TCB起始线性地址
      mov esi, [ss:ebp+9*4]  

      ;TODO-Think: why? 假定以前创建过用户任务。此时内核任务页目录表的前半部分是有内容的，还保留着前一个用户任务的相关表项。
      ;如果不清除，那么，在内存分配的时候，内存分配例程会以为以前已经分配过，会使用前一个用户任务的相关物理页
      mov ebx, 0xffff_f000                      ;可定位到页目录表自身（把页目录表当做物理页）
      mov ecx, 512                              ;页目录表共1024个目录项，只清空0-2GB
   .clear:
      mov dword [ebx], 0
      add ebx, 4
      loop .clear
      
   .flush_tlb:
      mov eax, cr3
      mov cr3, eax

         
   .get_user_program_size:
      mov eax, [ss:ebp+10*4]                    ;用户程序起始逻辑扇区号
      mov ebx, core_buf      
      call read_disk_hard_0

      mov eax, [core_buf]                       ;core_buf 0-3是用户程序长度字段(字节单位)
      mov ebx, eax                              
      and ebx, 0xffff_fe00                      ;低9位清零
      add ebx, 512                              ;向上512对齐

      test eax,0x0000_01ff                      ;eax原先是不是512字节对齐？只test低9位
      cmovnz eax, ebx                           ;低9位不是0说明不是512对齐，使用对齐结果

   .task_allocate_memory:                           
      mov ecx, eax                              ;实际需要申请的内存数量(字节单位)
      mov ebx, esi                              ;TCB起始线性地址
      ;输入: ECX=预期分配的字节数、EBX=TCB起始线性地址；
      ;输出: ECX=内存分配的起始线性地址
      call task_allocate_memory
      mov ebx, ecx                              ;用户程序分配的线性地址

   .load_user_program:                          ;EAX=用户程序所占字节数(512对齐)
      shr eax, 9                                ;EAX/512，2^9=512
      mov ecx, eax                              ;用户程序所占扇区数
      mov eax, [ss:ebp+10*4]                    ;用户程序起始LBA                          
      .read_more:
            ;ebx用户程序分配的内存起始线性地址，所以ds必须得是0-4GB（0为段基址）
            call read_disk_hard_0
            inc eax
            add ebx, 512
            loop .read_more

   
   ;ESI=TCB起始线性地址
   .create_stack:                               ;创建不同特权级的栈段，登记到TCB
      mov esi, [ss:ebp+9*4]                     ;TCB起始线性地址
      
      ;动态分配3特权栈
      mov ecx, 4096
      mov ebx, esi
      call task_allocate_memory
      mov ecx, [esi+6]                          ;TCB.下一次内存分配的起始线性地址
      mov dword [esi+70], ecx                   ;TCB.ESP3

      ;创建用于中断和调用门的0特权级栈空间
      mov ecx, 4096
      mov ebx, esi
      call task_allocate_memory
      mov ecx, [esi+6]                          ;TCB.下一次内存分配的起始线性地址
      mov dword [esi+10], ecx                   ;TCB.ESP0

   .create_user_pdt:
      call create_copy_cur_pdir
      mov [esi+22], eax                         ;TCB.CR3  

   .fill_tcb:
      eflags_field:                             ;IOPL、IF位相当重要，确保万无一失；当CPL>IOPL时不允许执行popf、iret，否则触发中断；可以执行cli、sti，但没有任何效果
      pushf
      pop eax                                   ;EAX获取EFLAGS值
      or eax, 0x0000_0200                       ;IF=1响应中断、OF DF IF TF=0010=0x2
      and eax, 0xffff_4fff                      ;0100=0x4、IOPL=0，在数值上CPL<=IOPL则I/O读写不受限制
      mov [esi+74], eax                         ;TCB.EFLAGS

      seg_field:
      mov ax, flat_user_data_seg_sel
      mov bx, flat_user_code_seg_sel
      mov [esi+40], ax                          ;TCB.GS
      mov [esi+38], ax                          ;TCB.FS
      mov [esi+36], ax                          ;TCB.ES
      mov [esi+34], ax                          ;TCB.DS
      mov [esi+32], ax                          ;TCB.SS
      mov [esi+30], bx                          ;TCB.CS

      eip_field:
      mov eax, [0x04]                           ;0x04是程序入口点，用户程序加载到起始线性地址0x0000
                                                ;当前任务还在内核中，页目录表是内核的，但内核页目录表与用户任务页目录表内容一致
      mov [esi+26], eax                         ;TCB.EIP 

   .return:
      popad
      ret 8                                     ;过程的编写者最清楚栈中有几个参数，丢弃8byte的参数，即过程返回到调用本函数的下一条指令后，ESP <- ESP+8
 ;------------------------------------------------------------
 create_user_program:                           ;创建一个用户程序
                                                ;输入：EDI=用户程序起始逻辑扇区号
                                                ;输出：无
      push ecx
      push ebx

   .create_tcb:
      mov ecx, 128                              ;TCB Size，TODO-Think: 实际大小80
      call allocate_memory                      ;ECX=TCB分配内存的起始线性地址（内核任务的全局空间下）
      mov dword [ecx+78], flat_user_code_seg_sel;自定义TCB字段：任务字段，用于系统api判断调用者RPL与真实特权级是否匹配
      mov dword [ecx+6], 0                      ;TCB.用户任务虚拟内存空间中，下一个用于内存分配的起始线性地址。低2GB是任务的私有空间
      mov word [ecx+4], 0                       ;TCB.状态=空闲
      call append_tcb

   .load_relocate:
      push dword edi                            ;EDI=用户程序起始逻辑扇区号
      push ecx                                  ;ECX=当前用户任务TCB起始线性地址
      call load_relocate_user_program

      mov ebx, msg_load_relocate_ok
      call put_string
   
   .return:
      pop ebx
      pop ecx
      ret
 ;------------------------------------------------------------  
start:
      call clear

      mov ebx, msg_enter_core
      call put_string

      ;确定IDT的起始位置
      ;安装IDT描述符,前20个是CPU内部中断,暂时提供一个统一的先兜着; 其他的一直到255,也弄一个统一的兜着,但0x70号中断需要单独处理
      ;IDT描述符安装完成后，开始加载IDTR，让IDTR48位寄存器保存IDT起始线性地址&界限
 .inside_interrupt:
      mov eax, inside_interrupt_handle          ;目标代码段32位偏移地址
      mov bx, flat_core_code_seg_sel            ;目标代码段选择子
      mov cx, 0x8e00                            ;中断门描述符属性，0特权级
      call make_gate_descriptor
      
      mov ebx, idt_linear_address
      xor edi, edi
   .for_inside_install:
      mov [ebx+edi*8], eax                      ;缺陷门描述符低32位
      mov [ebx+edi*8+4], edx                    ;缺陷门描述符高32位
      inc edi
      cmp edi, 19
      jle .for_inside_install
  
 .external_interrupt:
      mov eax, external_interrupt_handle        ;目标代码段32位偏移地址
      mov bx, flat_core_code_seg_sel            ;目标代码段选择子
      mov cx, 0x8e00                            ;中断门描述符属性，0特权级
      call make_gate_descriptor

      mov ebx, idt_linear_address
   .for_install_external:
      mov [ebx+edi*8], eax
      mov [ebx+edi*8+4], edx
      inc edi
      cmp edi, 255
      jle .for_install_external                 ;edi<=255,则循环
 
 .0x70_external_interrupt:
      mov eax, rtc_0x70_interrupt_handle        ;目标代码段32位偏移地址
      mov bx, flat_core_code_seg_sel            ;目标代码段选择子
      mov cx, 0x8e00                            ;中断门描述符属性，0特权级
      call make_gate_descriptor

      mov ebx, idt_linear_address
      mov [ebx+0x70*8], eax
      mov [ebx+0x70*8+4], edx
 
 .sys_call_0x88_interrupt:                      ;系统调用中断门描述符
      mov eax, sys_call_0x88_interrupt_handle   ;目标代码段32位偏移地址
      mov bx, flat_core_code_seg_sel            ;目标代码段选择子
      mov cx, 0xee00                            ;中断门描述符属性，3特权级！！！（用户程序 int 0x88）
      call make_gate_descriptor

      mov ebx, idt_linear_address
      mov [ebx+0x88*8], eax
      mov [ebx+0x88*8+4], edx


 .load_idt:
      mov word [pidt], 256*8-1                  ;长度(字节数)-1
      mov dword [pidt+2], idt_linear_address
      lidt [pidt]

 .set_8259a:
      ;设置8259A控制芯片 ？ 在保护模式下，如果计算机系统的可编程中断控制器芯片还是8259A，那就得重新进行初始化
      ;重新初始化8259A芯片的原因: 其主片的中断向量和处理器的异常向量冲突
      mov al, 0B0001_0001
      out 0x20, al                              ;主片CW1,边沿触发/联级方式
      mov al, 0x20
      out 0x21, al                              ;主片CW2,设置主片的起始中断号
      mov al, 0B0000_0100
      out 0x21, al                              ;主片CW3,第三个引脚(IR2)与从片相连
      mov al, 0B0000_0001
      out 0x21, al                              ;主片CW4,位1设置非自动结束

      mov al, 0B0001_0001
      out 0xa0, al                              ;从片CW1,边沿触发/联级方式
      mov al, 0x70
      out 0xa1, al                              ;从片CW2,起始中断向量号
      mov al, 0x02 
      out 0xa1, al                              ;从片CW3,主片IR2与从片相连
      mov al, 0B0000_0001 
      out 0xa1, al                              ;从片CW4

 .set_rtc:                                      ;初始化实时时钟芯片
      ;RTC-B寄存器端口,设置开启哪些中断,目前只开启[更新周期结束中断]
      mov al, 0x0B                              ;操作B寄存器
      out rtc_index_port, al
      mov al, 0b0001_0010                         
      out rtc_data_port, al                     ;RTC-B寄存器:中断允许开关,目前只允许[更新周期结束中断]

      ;RTC-C寄存器(中断发生，中断类型)读取即清零,这样中断才会持续产生。
      mov al, 0x0c                              ;高1位——NMI为0,即打开NMI引脚信号
      out rtc_index_port, al
      in al, rtc_data_port
    
      ;设置8259A中断屏蔽寄存器
      ;0是开放，1是断开该中断引脚
      in al, 0xa1                               ;从片端口
      and al, 0b1111_1110                       ;1-7位保持原位，0位即从片的IR0引脚
      out 0xa1, al

      sti                                       ;所有工作完成,开放外中断,可屏蔽外中断在mbr时已屏蔽



 .printf_cpu_info:
      mov ebx, cpu_brand0
      call put_string

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
      call put_string

      
 .create_core_task:
      cli                                       ;TODO-Tips:创建任务的过程中，关闭外中断

    .create_tcb:
      mov ecx, core_lin_tcb_addr                ;TCB分配改为手动分配
      mov word [ecx+4], 0xffff                  ;TCB状态繁忙，该内核任务即将被运行
      mov dword [ecx+6], core_lin_alloc_at      ;登记内核中可用于内存分配的起始线性地址
      mov dword [ecx+78], flat_core_code_seg_sel;自定义TCB字段：任务字段，用于系统api判断调用者RPL与真实特权级是否匹配
      call append_tcb                           ;输入：ECX

   .init_tss:
      ;因特权级之间的转移而发生栈切换时，本系统只会发生3到0的切换（所有任务使用全局一个TSS）。因此，
      ;只需要TSS中设置SS0，且必须是0特权级的栈段选择子。
      mov word [tss+8], flat_core_data_seg_sel  ;SS0
      mov dword [tss+100], 0x0067_0000          ;0x67=I/O映射基地址=103，I/O映射空

   .tss_to_gdt:
      mov eax, tss                              ;TSS起始线性地址
      mov ebx, 103                              ;TSS界限值
      mov ecx, 0x0000_8900                      ;TSS描述符属性                                 
      call make_seg_descriptor
                                                ;G DB L AVL=0000、段界限=0
                                                ;P DPL S=1000、TYPE=1001，其中TYPE的第2位是B位，表示忙不忙，切换到一个任务时CPU会自动将该位置为1，在内存中也是吗？还只是在描述符高速缓冲器中修改？。
                                                ;TODO-Tips：一会用Bochs测试一下，我猜测是高速缓冲器和内存都修改。猜测正确
      call install_gdt_descriptor

      ;ltr r16/m16 将TSS选择子（GDT中）送到tr寄存器，
      ;然后再去GDT中加载对应的TSS描述符到tr的描述符高速缓冲器中，并将B位置为1（繁忙）
      ;任务寄存器TR中的内容是任务存在的标志，该内容也决定了当前任务是谁。
      ;下面的指令为当前正在执行的0特权级任务“程序管理器”后补手续（TSS）。
      ;以后都不修改 TR
      ltr cx                                 

      mov ebx, msg_core_task_run
      call put_string


 .create_user_task:
      mov edi, app1_start_sector                ;用户程序LBA
      call create_user_program

      mov edi, app2_start_sector                ;第二个用户程序LBA
      call create_user_program
      
      sti                                       ;任务都创建好了，开放外中断1

 .do_switch:
      ;任务清理操作
      call do_task_clear
      jmp .do_switch
;============================== core_code END =============================


SECTION tail
 file_end: