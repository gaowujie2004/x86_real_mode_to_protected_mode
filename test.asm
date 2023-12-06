[bits 16]    ;后续代码，默认CPU操作尺寸32bit
mov eax, 0xFFFFFFF0
mov eax, [eax]

jmp $

times 510-($-$$) db 0x00
db 0x55, 0xaa