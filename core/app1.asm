;=============================== header seg ===============================
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
      salt_count        dw (header_end-salt)/256

      salt:
      PrintfString      db '@put_string', 0
                        times 256-($-PrintfString) db 0

      ReadHardDisk      db '@read_head_disk',0
                        times 256-($-ReadHardDisk) db 0

      TerminateProgram  db  '@TerminateProgram'
                        times 256-($-TerminateProgram) db 0

      

      header_end:
;================================= END ====================================


      [bits 32]  
;=============================== data seg =================================
SECTION data vstart=0

 data_end:
;================================= END ====================================



;=============================== stack seg ================================
SECTION stack vstart=0
 stack_end:
;================================= END ====================================




;=============================== code seg =================================
SECTION code vstart=0
 start:
      mov ax, 90
 code_end:
;================================= END ====================================


SECTION tail
 file_end: