mov ax, 0xb800
mov es, ax

mov ah, 0x40
mov al, 'G'
mov es:[160*12+37*2], ax

mov al, 'W'
mov es:[160*12+38*2], ax

mov al, 'J'
mov es:[160*12+39*2], ax


jmp $
times 510-($-$$) db 0
db 0x55, 0xaa
