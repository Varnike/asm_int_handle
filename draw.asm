.set_draw	macro
		mov cx, LEN
		mov si, bp
				
		call DrawLine

		add di, 160d - LEN * 2
		endm
;-------------------------------------------------


draw		proc

		push bp

		mov bp, offset style1_top

; calculate top left frame position
		mov ax, VPOS
		mov dx, 160d
		mul dx
		add ax, HPOS
		mov di, ax

		mov ah, COLOR		; frame color
		push VIDEOSEG
		pop es

		.set_draw		; draw top line
		
		add bp, 3h		; set next line style

; srart drawing middle lines
		mov bx, 1h
cloop:		.set_draw

		inc bx
		cmp bx, HEIGHT
		jne cloop


; set next line style
		add bp, 3h

		.set_draw		; draw lower line
		pop bp
		ret

draw		endp
;=================================================

; preset styles
style1_top	db 0c9h, 0cdh, 0bbh, 0bah, " ", 0bah, 0c8h, 0cdh, 0bch

;-------------------------------------------------
; Draw line in a frame
;
; Entry: AH - color style
;	 CX - length of line
;	 SI - addr of array with 3 symbols
;	 DI - line start addr
; Nnote: ES = videoseg addr (0b800h)
; Exit:	 None
; Destr: AX, CX, DI, SI
;-------------------------------------------------

DrawLine	proc 		

		cld
		lodsb
		stosw		
		lodsb	

		sub cx, 2

		jcxz @@stoploop

		rep stosw

@@stoploop:	lodsb
		stosw
	
		ret

DrawLine	endp
;-------------------------------------------------

