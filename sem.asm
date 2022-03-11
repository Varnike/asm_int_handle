.186
.model tiny
.code
org 100h


locals

Start:		jmp main

;=================================================
INCLUDE 	config.asm
;=================================================


;=================================================
;********	RESIDENT	********
;=================================================
New09		proc

		push ax bx cx dx si di bp es

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


		push ds			; change current segment address
		push cs			; use ?
		pop ds

		not drflag		; change draw flag
		xor al, al
		cmp al, cs:[drflag] 	; NOTE end here
		jne @@save

; restore old frame data to video mem
		mov di, 160d * HPOS + VPOS
		push VIDEOSEG
		pop es
		mov si, offset vbuff


@@restore:	mov cx, 6

		call strncpy

		add di, 160d - LEN * 2
		cmp di, 160d * HPOS + VPOS + (160d - LEN*2)*(HEIGHT+1)
		jb @@restore

; save old frame to buffer
@@save:		push cs
		pop es
		push VIDEOSEG
		pop ds
		mov si, 160d * HPOS + VPOS
		mov di, offset vbuff

@@save_loop:	mov cx, 6

		call strncpy

		add si, 160d - LEN * 2
		cmp si, 160d * HPOS + VPOS + (160d - LEN*2)*(HEIGHT+1)
		jb @@save_loop


@@skip:		pop ds			; restore ds
	
		mov al, 20h
		out 20h, al

		pop es bp di si dx cx bx ax
		iret			; if hotkey pressed, 
					; dont jmp to old handler 


@@not_hot:	mov ah, 4eh		; not hotkey, continue
		cld

		pop es bp di si dx cx bx ax

		db 0eah			; JMP FAR 
old09ofs	dw 0			; самоизменяющийся код
old09seg	dw 0			; переходим к старому обработ.
drflag		db 0
vbuff	db 72d dup(?)

New09		endp
;=================================================


;-------------------------------------------------
; vram_cmp
;-------------------------------------------------
; Entry: DS:SI - address of tmp buffer
;	 ES:DI - address of video mem start
;	 DS:BX - address of saved frame
;-------------------------------------------------
vram_cmp	proc

@@cmp_loop:	repe cmpsb		; compare until first diff
					; or buffs are equal
		jne @@not_eq
		ret

@@not_eq:	dec di			; if characters are diff
	
		mov al, es:[di]		; update saved image
		inc di

		mov bx, si
		add bx, offset vbuff - offset tmp - 1

		mov ds:[bx], al

		xor ax, ax
		cmp cx, ax

		jne @@cmp_loop		; continue comparing

		ret
vram_cmp	endp

;-------------------------------------------------

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
		push cs
		pop es
		mov di, [bp + 6]
		add di, offset tmp + LEN * 2 + 2

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
		
		
		push ax
		xor ax, ax
		cmp al, cs:[drflag]
		je @@exit

		push bx cx dx si di es ds

		push bp
		mov bp, sp
	
		push cs	
		pop ds		
		push VIDEOSEG
		pop es

		push 160d * HPOS + VPOS
		pop di
		mov si, offset tmp
		mov bx, offset vbuff

; comparing prev frame with real image
@@cmp:		mov cx, LEN * 2

		call vram_cmp

		add di, 160d - LEN * 2
		cmp di, 160d * HPOS + VPOS + (160d - LEN*2)*(HEIGHT+1)
		jb @@cmp

		call draw			; draw frame

; draw all regs values
		.set_draw 16, 0			; ax
		.set_draw 14, LEN*2d		; bx
		.set_draw 12, LEN*4d		; cx
		.set_draw 10, LEN*6d		; dx

; transfer frame from tmp to video mem
		mov di, 160d * HPOS + VPOS
		push VIDEOSEG
		pop es
		mov si, offset tmp

@@transfer:	mov cx, 6

		call strncpy

		add di, 160d - LEN * 2
		cmp di, 160d * HPOS + VPOS + (160d - LEN*2)*(HEIGHT+1)
		jb @@transfer

		pop bp ds es di si dx cx bx

@@exit:		pushf	
		call  dword ptr cs:[old08ofs]

		pop ax
		iret

New08		endp

old08ofs	dw 0h
old08seg	dw 0h

tmp		db 72d dup(?)
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
