;任务：让RTC芯片定期发出一个中断，当这个中断发生的时候，还能执行我们自己编写的代码，来访问CMOS RAM，在屏幕上显示一个动态走动的时间

;TODO-Tips      需要重点提示
;TODO-BUG       当初的bug
;TODO-Optimize  优化点

;b8000 + 12*160 + 60   时间
;b8000 + 13*160 + 60   .loop_hlt 变色

rtc_index_port      equ     0x70                ;NMI打开/关闭在最高位，1关闭、0打开
rtc_data_port       equ     0x71


SECTION MBR_CODE vstart=0
    mov ax, cs
    mov ss, ax
    mov sp, 0x7c00


    ;1.安装中断向量表
    mov ax, 0
    mov ds, ax                        
    cli                                         ;TODO-Tips：0x70中断向量在安装时，应该屏蔽硬中断，安装完后再开放，避免不可预料的情况
    mov word [0x70*4], 0x7c00 + rtc_interrupt_0x70   ;TODO-Tips：物理地址 M[0x70*4]=中断处理程序的IP、M[0x70*4+2]=...CS
    mov [0x70*4+2], cs
    

    ;2.RTC B寄存器端口，控制中断类型，目前只开放【更新周期结束中断】
    mov al, 0x0b
    out rtc_index_port, al
    mov al, 0b0001_0010                         
    out rtc_data_port, al                                  ;RTC B寄存器-中断允许开关，目前只允许【更新结束中断】

    ;3.RTC C寄存器（中断发生，中断类型）读取即清零，这样中断才会持续产生。
    mov al, 0x0c                                ;高1位——NMI为0，即打开NMI引脚信号
    out rtc_index_port, al
    in al, rtc_data_port
    
    ;4.设置8259A中断屏蔽寄存器
    ;0是开放，1是关闭该中断引脚
    in al, 0xa1
    and al, 0b1111_1110                         ;1-7位保持原位，0位即从片的IR0引脚
    out 0xa1, al

    sti                                         ;TODO-Tips：一切中断初始化就绪后，再允许CPU处理硬件外中断，否则，即使有中断信号进入CPU，CPU也不处理


    mov ax, 0xb800
    mov es, ax
    mov byte [es:13*160 + 60], '@'
 .loop_idel:
    not byte [es:13*160 + 61]
    hlt
    jmp .loop_idel

;--------------------------------------------------------------------------------------
rtc_interrupt_0x70:                             ;更新周期结束中断，读取CMOS-RAM，显示时间              
                                                ;显示位置：12*160 + 60
    push ax
    push es

 .safe_read_cmos_ram:
    ;能否安全地读CMOS-RAM数据，在当前【更新周期结束中断】内是多余的判断
    mov al, 0x0a
    out rtc_index_port, al
    in al, rtc_data_port
    test al, 0B1000_0000                        ;最高位为0，即可安全地访问CMOS-RAM
    jnz  .safe_read_cmos_ram                    ;不等于0跳转


 .interrupt_check:
    ;RTC read C寄存器中断类型
    mov al, 0x0c
    out rtc_index_port, al                        
    in al, rtc_data_port                        ;读取RTC C寄存器内容，读取后该寄存器自动清零

    mov ah, al
    and ah, 0b1001_0000
                                                ;严格检测：位7——中断请求标志（是否有中断）；位6——周期性中断标志；位5——闹钟标志；位4——更新结束标志；3-0位保留位
    cmp ah, 0b1001_0000                         ;位7——中断请求标志；位4——更新结束标志
    jne .error                                  ;不等于则跳转
 
 .save_time_data:
    ;保存CMOS-RAM中的日期数据，最后压入年，LIFO
    mov al, 0x00
    out rtc_index_port, al
    in al, rtc_data_port                        ;秒
    push ax                                     ;不用慌，弹栈时只要ax的低8位——al即可

    mov al, 0x02
    out rtc_index_port, al
    in al, rtc_data_port                        ;分
    push ax                                     

    mov al, 0x04
    out rtc_index_port, al
    in al, rtc_data_port                        ;时
    push ax                                  


    mov al, 0x07
    out rtc_index_port, al
    in al, rtc_data_port                        ;日
    push ax 

    mov al, 0x08
    out rtc_index_port, al
    in al, rtc_data_port                        ;月
    push ax 

    mov al, 0x09
    out rtc_index_port, al
    in al, rtc_data_port                      ;年
    push ax 

 .show_time:
    mov ax, 0xb800
    mov es, ax
    mov bx, 12*160+30*2

    ;年
    pop ax
    call bcd_to_ascii                           
    mov byte [es:bx], ah                 ;TODO-Tips：高位先展示
    mov byte [es:bx+2], al                 

    mov byte [es:bx+4], '-' 

    ;月
    pop ax
    call bcd_to_ascii                           
    mov byte [es:bx+6], ah                 
    mov byte [es:bx+8], al   

    mov byte [es:bx+10], '-' 

    ;日
    pop ax
    call bcd_to_ascii                           
    mov byte [es:bx+12], ah                 
    mov byte [es:bx+14], al  

    mov byte [es:bx+16], ' ' 

    ;时
    pop ax
    call bcd_to_ascii                           
    mov byte [es:bx+18], ah                 
    mov byte [es:bx+20], al  

    mov byte [es:bx+22], ':' 
    not byte [es:bx+23]

    ;分
    pop ax
    call bcd_to_ascii                           
    mov byte [es:bx+24], ah                 
    mov byte [es:bx+26], al  

    mov byte [es:bx+28], ':' 
    not byte [es:bx+29]


    ;秒
    pop ax
    call bcd_to_ascii                           
    mov byte [es:bx+30], ah                 
    mov byte [es:bx+32], al  

 .error:                                        ;TODO-Todo：代办项，以后添加错误的显示
    
    mov al,0x20                                 ;中断结束命令EOI 
    out 0xa0,al                                 ;向从片发送 
    out 0x20,al                                 ;向主片发送

 .return:
    pop es
    pop ax
    iret
;--------------------------------------------------------------------------------------


;--------------------------------------------------------------------------------------
bcd_to_ascii:                                   ;BCD编码的值转为ASCII码值         
                                                ;输入：AL
                                                ;输出：AX
    mov ah, al

    and al, 0b0000_1111                         ;处理al低4位      
    add al, 0x30 

    shr ah, 4                                   ;处理al高4位
    and ah,0x0f                                 ;TODO-Todo：不必要的             
    add ah, 0x30         

    ret
;--------------------------------------------------------------------------------------

    times 510-($-$$) db 0x00
    db 0x55, 0xaa
