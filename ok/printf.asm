;本章任务：显示字符串

TEXT_MODE_START equ 0xb800

section MBR_code align=16 vstart=0x7c00   ;影响本段内的标号偏移地址
  ;初始化数据段
  mov ax, cs
  mov ds, ax
  ;初始化栈内存
  xor ax, ax
  mov ss, ax
  mov sp, ax   ;0:0 push之后变为 0000:FFFE，距离0000:7c00还有很长距离


  mov si, msg0   ;--------------------------------------------受vstart影响，编译后是：mov si, offset msg0+7c00（是一个确切的值）
  mov cl, 0b0100_0000
  call _showString   ;----------------------------------------函数调用使用的是相对位移量，不受vstart影响
  jmp $

  msg0:     db 'GaoWuJie@QBL   '
  msg0_end: db 0

  ;功能：显示字符串到中间,字符串是以0x00结尾
  ;入参：1）ds:si执行字符串首地址、2）cl=color
  _showString:
    push ax
    push es
    push di
    push si

    mov ax, TEXT_MODE_START
    mov es, ax
    mov di, 160*12+80-(msg0_end-msg0)/2*2             ;------------------------------这是int除法，结果是商（整数）不是小数，取最近的向上偶数
    mov ah, cl
    for_write_video_ram:
      mov al, [si]  ;-------------------------------------------没有使用当前段的标号，所有不受vstart影响                                             
      cmp al, 0
      je for_write_video_ram_end

      mov es:[di], ax
      inc si
      add di, 0x2
      jmp for_write_video_ram
    for_write_video_ram_end:  
    

    pop si
    pop di
    pop es
    pop ax
    ret

  times 510-($-$$) db 0x00
  db 0x55, 0xaa

section stack