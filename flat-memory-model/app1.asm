;=============================== header STA ===============================
SECTION header  vstart=0
      
      program_length   dd file_end              ;程序总长度#0x00
      entry_point      dd start                ;程序入口点#0x04
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