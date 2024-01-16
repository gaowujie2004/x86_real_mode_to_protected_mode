;=============================== header STA ===============================
SECTION header  vstart=0
      program_length   dd file_end              ;程序总长度#0x00
      entry_point      dd start                 ;程序入口点#0x04
;=============================== header END ===============================


      [bits 32]  
;=============================== data STA =================================
SECTION data vfollows=header
      msg_start         db 0x0d,0x0a, 0x20,0x20,0x20,0x20, '[App-2]: 2222',0
      buffer            times 1024 db  0

      data_end:
;=============================== data END =================================



;=============================== code STA =================================
SECTION code vfollows=data
 start:

   .for_printf:
      mov ebx, msg_start
      mov eax, 0
      int 0x88
      jmp .for_printf

 code_end:
;=============================== code END =================================


SECTION tail
 file_end: