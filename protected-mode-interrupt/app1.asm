;=============================== header STA ===============================
SECTION header  vstart=0
      file_length       dd file_end             ;文件总长度#0x00

      header_length     dd header_end           ;文件头段长度#0x04

      code_entry        dd start                ;程序入口#0x08
      code_seg          dd section.code.start   ;代码段位置#0x0c
      code_length       dd code_end             ;代码段长度#0x10

      data_seg          dd section.data.start   ;数据段位置#0x14
      data_length       dd data_end             ;数据段长度#0x18

      stack_seg         dd section.stack.start  ;栈段段位置#0x1c
      stack_length      dd stack_end            ;栈段长度#0x20

      ;-----------------------------------------符号地址映射表
      salt_count        dd (header_end-salt)/256 ;#0x24

      salt:                                      ;#0x28
      PrintfString      db '@put_string',0
                        times 256-($-PrintfString) db 0

      ReadHardDisk      db '@read_head_disk',0
                        times 256-($-ReadHardDisk) db 0

      TerminateProgram  db '@terminateProgram',0
                        times 256-($-TerminateProgram) db 0

      TaskSwitch        db '@taskSwitch', 0
                        times 256-($-TaskSwitch) db 0
      ;-----------------------------------------符号地址映射表

      

      header_end:
;=============================== header END ===============================


      [bits 32]  
;=============================== data STA =================================
SECTION data vstart=0
      msg_start         db 0x0d,0x0a, 0x20,0x20,0x20,0x20, '[App-1]: 1111',0
      buffer            times 1024 db  0

      data_end:
;=============================== data END =================================



;=============================== stack STA ================================
SECTION stack vstart=0
      times 2048 db 0xa0
      stack_end:
;=============================== stack END ================================





;=============================== code STA =================================
SECTION code vstart=0
 start:
      mov ax, ds                                ;ds=头部段
      mov fs, ax
      mov gs, ax

      mov ax, [fs:0x14]
      mov ds, ax

      mov ax, [fs:0x1c]
      mov ss, ax
      mov esp, stack_end

   .for_printf:
      mov ebx, msg_start
      call far [fs:PrintfString]                ;间接远调用，必须要指定far
      jmp .for_printf


 code_end:
;=============================== code END =================================


SECTION tail
 file_end: