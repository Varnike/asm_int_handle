.186
.model tiny
.code
org 100h

Start:		cli			; NOTE turn off cpu ints
					; откл внешние прерывания
		xor di, di
		mov es, di
		mov di, 09h * 4
;	=	=	=	=	=	=	
		mov ax, es:[di]		; аддресс старого отработчика
		mov old09ofs, ax
		mov ax, es:[di + 2]
		mov old09seg, ax
;	=	=	=	=	=	=

		mov es:[di], offset New09 ; поместить в int table

		push cs			; our segment
		pop ax

		mov es:[di + 2], ax
		sti			; turn on ints
		
dWait:		in al, 60h
		cmp al, 1
		jne dWait
		
		mov ax, 3100h		; TSR
		mov dx, offset HappyEnd
		shr dx, 4		; в параграфах
		inc dx			; если нацело не делится
		int 21h

;-------------------------------------------------
; dont destroy regs(save with push and pop) NOTE
;-------------------------------------------------
New09		proc

		push ax di es

		mov di, 0b800h
		mov es, di
		mov di, (5 * 80 + 80/2) * 2

		mov ah, 4eh
		cld


Next:		in al, 60h		; POLLING - BAD
		stosw
; мигнуть старшим битом!	
;		in al, 61h		; Send ACK to kb
;		mov ah, al
;		or al, 80h
;		out 61h, al
;		mov al, ah
;		out 61h, al

;		mov al, 20h
;		out 20h, al		; send E01

		pop ax di es
;		iret			; ret from int(data in STACK)
					; 3 words! seg, addr, flags
		db 0eah			; JMP FAR NOTE
old09ofs	dw 0			; самоизменяющийся код
old09seg	dw 0			; переходим к старому обработ.

		endp
;-------------------------------------------------

;		and di, 0FFFh

;		xor cx, cx
;		dec cx

Delay:		nop
		nop
		loop Delay
		
		cmp al, 1
		jne Next

		mov ax, 4c00h
		int 21h


;-------------------------------------------------
HappyEnd:
end		Start
