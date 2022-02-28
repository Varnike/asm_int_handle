;-------------------------------------------------
; itoa 
;-------------------------------------------------
; Entry: bx - number to transform
;	 cx  - bit depth of the system(CX = 2\8\16)
;	 si  - number of chars to write in string
;	 di  - array in memory where to store 
;	 	the resulting '$'-terminated string
; Exit:  None
; Destr: AX, BX, CX, DX, SI, DI
;-------------------------------------------------
itoa		proc
	
		std			; start from the end of string
		add di, si

		mov al, '$'
		stosb

		mov ax, bx

		mov dx, cx		; bit mask
		dec dx

@@convert:	and ax, dx

		cmp ax, 9h
		ja @@hex_symb

		add ax, '0'
		jmp @@save

@@hex_symb:	add ax, 'A' - 10d

@@save:		stosb
		
		mov ax, bx
		call DecDigit
		mov bx, ax

		cmp si, 0
		dec si

		jne @@convert

		ret

itoa		endp
;-------------------------------------------------



;-------------------------------------------------
; DecDigit - div number by 2/8/16(depends on bit 
; depth of system)
;-------------------------------------------------
; Entry: AX - number to div
;	 CL - bit depth of the system (cx = 2,8,10,16)
; Exit:	 AX - div number
;	 DX - remainder of the division(if cx == 10)
; Destr: AX
;-------------------------------------------------
DecDigit	proc
		cmp cx, 10d
		je @@dec_sys
		ja @@hex_sys

		cmp cl, 8h
		je @@oct_sys

		sar ax, 1h
		ret

@@dec_sys:	mov bx, 10d
		div bx
		ret

@@hex_sys:	sar ax, 4h
		ret

@@oct_sys:	sar ax, 3h
		ret

DecDigit	endp

;-------------------------------------------------


;-------------------------------------------------
; strncpy - copies the first num(CX value) characters
; of source string to destination string.
;-------------------------------------------------
; Entry: DS:SI - source string address
;	 ES:DI - destination string address
;	 CX - number(num) of characters to copy
; Note:  Sets DF to 0
;
;	 If num is bigger than src string length,
;	 after copying '$' to dest, all the 
;	 following characters will be garbage
;
;	 If num is fewer than src, '$' wont be
;	 added at the end of dest!
;
; Exit:  None
; Destr: SI, DI
;-------------------------------------------------
strncpy		proc
		
		cld	
		repnz movsw
		
		ret		
strncpy		endp
;-------------------------------------------------


