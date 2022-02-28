.186
.model tiny
.code
org 100h
; xlab


locals

Start:		jmp main

;=================================================
INCLUDE 	config.asm
;=================================================


;=================================================
;********	RESIDENT	********
;=================================================
New09		proc

		push ax bx cx dx si di bp es ds

;		mov di, 0b800h
;		mov es, di
;		mov di, (5 * 80 + 80/2) * 2

		in al, 60h
		
		cmp al, HOT_KEY		; check if hotkey pressed
		jne @@not_hot
; if hotkey pressed
		in al, 61h		; Send ACK to kb
		mov ah, al
		or al, 80h
		out 61h, al
		xchg al, ah
		out 61h, al

		mov al, 20h
		out 20h, al

		
		not drflag		; change draw flag
		xor al, al
		cmp al, cs:[drflag] 	; NOTE end here
		je @@restore

		;here smth...
		
@@restore:	mov si, offset scrn_buff
		mov di, (160d*HPOS+VPOS)
		mov cx, 8h
		push cs			; use ?
		pop ds
		push VIDEOSEG
		pop es


		pop ds es bp di si dx cx bx ax
		iret			; if hotkey pressed, 
					; dont jmp to old handler 


@@not_hot:	pop ds es bp di si dx cx bx ax

		db 0eah			; JMP FAR 
old09ofs	dw 0			; самоизменяющийся код
old09seg	dw 0			; переходим к старому обработ.
drflag		db 0
scrn_buff	db 72d dup(?)

New09		endp
;=================================================



;-------------------------------------------------
; Draws hex value of given number(from stack)
;-------------------------------------------------
; Entry: 1: position to start drawing
;	 2: number to draw
;-------------------------------------------------
num_draw	proc

		push bp
		mov bp, sp

		mov bx, [bp + 4]
		mov cx, 16d
		mov si, 4d
		mov di, offset num_str

		push cs
		pop es

		call itoa
		cld				; itoa uses std
		
		mov si, offset num_str
		push VIDEOSEG
		pop es
		mov di, [bp + 6]
		add di, (160d*(HPOS+1)+VPOS+2)

		shr cx, 2d			; cx = 4d

		mov ah, COLOR

@@print_num:	lodsb				; ds:si
		stosw				; es:di
		loop @@print_num
		
		pop bp
		ret

num_draw	endp
;-------------------------------------------------


;-------------------------------------------------
; Entry: _addr - stack bias to get pushed reg value
;	 _pos  - pos to start draw (0 - start of frame)
;-------------------------------------------------
.set_draw	macro _addr, _pos
		push _pos
		push [bp + _addr]
		
		call num_draw

		pop cx
		pop cx 

		endm
;-------------------------------------------------



;-------------------------------------------------
New08		proc
		
		pushf				; NOTE
		call  dword ptr cs:[old08ofs]	; NOTE

		push ax
		xor ax, ax
		cmp al, cs:[drflag]
		je @@exit

		push bx cx dx si di es ds

		push bp
		mov bp, sp

		push cs	
		pop ds	
		
		call draw

		.set_draw 16, 0			; ax
		.set_draw 14, 160d		; bx
		.set_draw 12, 320d		; cx
		.set_draw 10, 480d		; dx

		pop bp ds es di si dx cx bx

@@exit:		pop ax
		iret				; NOTE

New08		endp

old08ofs	dw 0h
old08seg	dw 0h

num_str		db 5 dup('(')
;-------------------------------------------------


;=================================================
INCLUDE 	draw.asm
INCLUDE		strlen.asm
;=================================================

 
;-------------------------------------------------
; Entry: _int   - interrupt number
;	 _str   - string to save old int address
;	 _handl - new handler
;-------------------------------------------------
.set_int	macro _int, _addr, _handl
		xor di, di
		mov es, di
		mov di, _int * 4

		cli			; NOTE turn off cpu ints

		mov ax, es:[di]		; аддресс старого отработчика
		mov _addr, ax
		mov ax, es:[di + 2]
		mov _addr + 2,  ax

		mov es:[di], offset _handl ; поместить в int table

		push cs			; our segment
		pop ax

		mov es:[di + 2], ax

		sti			; turn on ints

		endm 
;-------------------------------------------------


;=================================================
;********	NOT RESIDENT	********
;=================================================

main:		.set_int 09h, old09ofs, New09
		.set_int 08h, old08ofs,	New08

		mov ax, 3100h		; TSR
		mov dx, offset main
		shr dx, 4		; в параграфах
		inc dx			; если нацело не делится
		int 21h
;------------------------------------------------



HappyEnd:
end		Start
