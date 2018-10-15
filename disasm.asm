.model small
.stack 100h

.data
		 ;  0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
	OPK db 06h, 06h, 06h, 06h, 06h, 06h, 47h, 45h, 43h, 43h, 43h, 43h, 43h, 43h, 47h, 45h ; 0
		db 05h, 05h, 05h, 05h, 05h, 05h, 47h, 45h, 53h, 53h, 53h, 53h, 53h, 53h, 47h, 45h ; 1
		db 07h, 07h, 07h, 07h, 07h, 07h, 66h, 12h, 5Dh, 5Dh, 5Dh, 5Dh, 5Dh, 5Dh, 66h, 13h ; 2
		db 61h, 61h, 61h, 61h, 61h, 61h, 66h, 01h, 0Eh, 0Eh, 0Eh, 0Eh, 0Eh, 0Eh, 66h, 04h ; 3
		db 1Bh, 1Bh, 1Bh, 1Bh, 1Bh, 1Bh, 1Bh, 1Bh, 14h, 14h, 14h, 14h, 14h, 14h, 14h, 14h ; 4
		db 47h, 47h, 47h, 47h, 47h, 47h, 47h, 47h, 45h, 45h, 45h, 45h, 45h, 45h, 45h, 45h ; 5
		db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh ; 6
		db 2Fh, 2Ch, 2Ah, 21h, 24h, 2Bh, 22h, 20h, 31h, 2Eh, 30h, 2Dh, 27h, 26h, 28h, 25h ; 7
		db 62h, 62h, 62h, 62h, 5Eh, 5Eh, 41h, 41h, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 34h, 3Ch, 45h ; 8  90 - REG ANALIZE
		db 41h, 41h, 41h, 41h, 41h, 41h, 41h, 41h, 09h, 11h, 08h, 5Fh, 48h, 46h, 51h, 32h ; 9
		db 3Ch, 3Ch, 3Ch, 3Ch, 3Dh, 3Eh, 0Fh, 10h, 5Eh, 5Eh, 5Bh, 5Ch, 37h, 38h, 54h, 55h ; A
		db 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch, 3Ch ; B
		db 0FFh, 0FFh, 4Dh, 4Dh, 35h, 33h, 3Ch, 3Ch, 0FFh, 0FFh, 4Eh, 4Eh, 1Dh, 1Ch, 1Eh, 1Fh ; C
		db 63h, 63h, 63h, 63h, 03h, 02h, 0FFh, 60h, 16h, 16h, 16h, 16h, 16h, 16h, 16h, 16h ; D
		db 3Bh, 3Ah, 39h, 23h, 1Ah, 1Ah, 44h, 44h, 08h, 29h, 29h, 29h, 1Ah, 1Ah, 44h, 44h ; E
		db 36h, 0FFh, 4Ch, 4Bh, 17h, 0Dh, 64h, 64h, 0Ah, 58h, 0Ch, 5Ah, 0Bh, 59h, 65h, 65h ; F 
		
			  ;  0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
	formatas db 00h, 00h, 00h, 00h, 16h, 16h, 10h, 10h, 00h, 00h, 00h, 00h, 16h, 16h, 10h, 10h ; 0
			 db 00h, 00h, 00h, 00h, 16h, 16h, 10h, 10h, 00h, 00h, 00h, 00h, 16h, 16h, 10h, 10h ; 1
			 db 00h, 00h, 00h, 00h, 16h, 16h, 0Fh, 11h, 00h, 00h, 00h, 00h, 16h, 16h, 0Fh, 11h ; 2
			 db 00h, 00h, 00h, 00h, 16h, 16h, 10h, 11h, 00h, 00h, 00h, 00h, 16h, 16h, 10h, 11h ; 3
			 db 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh ; 4
			 db 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh ; 5
			 db 11h, 11h, 11h, 11h, 11h, 11h, 11h, 11h, 11h, 11h, 11h, 11h, 11h, 11h, 11h, 11h ; 6
			 db 06h, 06h, 06h, 06h, 06h, 06h, 06h, 06h, 06h, 06h, 06h, 06h, 06h, 06h, 06h, 06h ; 7
			 db 02h, 02h, 02h, 02h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h, 01h, 12h, 01h, 13h ; 8
			 db 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 0Fh, 11h, 11h, 07h, 11h, 11h, 11h, 11h, 11h ; 9
			 db 08h, 08h, 08h, 08h, 11h, 11h, 11h, 11h, 16h, 16h, 11h, 11h, 11h, 11h, 11h, 11h ; A
			 db 0Ah, 0Ah, 0Ah, 0Ah, 0Ah, 0Ah, 0Ah, 0Ah, 0Ah, 0Ah, 0Ah, 0Ah, 0Ah, 0Ah, 0Ah, 0Ah ; B
			 db 11h, 11h, 0Dh, 11h, 12h, 12h, 13h, 13h, 11h, 11h, 0Dh, 11h, 1Dh, 14h, 11h, 11h ; C
			 db 05h, 05h, 05h, 05h, 0Bh, 0Bh, 11h, 11h, 04h, 04h, 04h, 04h, 04h, 04h, 04h, 04h ; D
			 db 06h, 06h, 06h, 06h, 0Ch, 0Ch, 0Ch, 0Ch, 15h, 15h, 07h, 06h, 0Ch, 0Ch, 0Ch, 0Ch ; E
			 db 11h, 11h, 11h, 11h, 11h, 11h, 03h, 03h, 11h, 11h, 11h, 11h, 11h, 11h, 0Eh, 0Eh ; F 
	
	k_AAA db 'AAA', 20h
	k_AAD db 'AAD', 20h
	k_AAM db 'AAM', 20h
	k_AAS db 'AAS', 20h
	k_ADC db 'ADC', 20h
	k_ADD db 'ADD', 20h
	k_AND db 'AND', 20h
	k_CALL db 'CALL', 20h
	k_CBW db 'CBW', 20h
	k_CLC db 'CLC', 20h
	k_CLD db 'CLD', 20h
	k_CLI db 'CLI', 20h
	k_CMC db 'CMC', 20h
	k_CMP db 'CMP', 20h
	k_CMPSB db 'CMPSB', 20h
	k_CMPSW db 'CMPSW', 20h
	k_CWD db 'CWD', 20h
	k_DAA db 'DAA', 20h
	k_DAS db 'DAS', 20h
	k_DEC db 'DEC', 20h
	k_DIV db 'DIV', 20h
	k_ESC db 'ESC', 20h
	k_HLT db 'HLT', 20h
	k_IDIV db 'IDIV', 20h
	k_IMUL db 'IMUL', 20h
	k_IN db 'IN', 20h
	k_INC db 'INC', 20h
	k_INT db 'INT', 20h
	k_INT3 db 'INT 3', 20h
	k_INTO db 'INTO', 20h
	k_IRET db 'IRET', 20h
	k_JA db 'JA', 20h
	k_JAE db 'JAE', 20h
	k_JBE db 'JBE', 20h
	k_JCXZ db 'JCXZ', 20h
	k_JE db 'JE', 20h
	k_JG db 'JG', 20h
	k_JGE db 'JGE', 20h
	k_JL db 'JL', 20h
	k_JLE db 'JLE', 20h
	k_JMP db 'JMP', 20h
	k_JNAE db 'JNAE', 20h
	k_JNE db 'JNE', 20h
	k_JNO db 'JNO', 20h
	k_JNP db 'JNP', 20h
	k_JNS db 'JNS', 20h
	k_JO db 'JO', 20h
	k_JP db 'JP', 20h
	k_JS db 'JS', 20h
	k_LAHF db 'LAHF', 20h
	k_LDS db 'LDS', 20h
	k_LEA db 'LEA', 20h
	k_LES db 'LES', 20h
	k_LOCK db 'LOCK', 20h
	k_LODSB db 'LODSB', 20h
	k_LODSW db 'LODSW', 20h
	k_LOOP db 'LOOP', 20h
	k_LOOPE db 'LOOPE', 20h
	k_LOOPNE db 'LOOPNE', 20h
	k_MOV db 'MOV', 20h
	k_MOVSB db 'MOVSB', 20h
	k_MOVSW db 'MOVSW', 20h
	k_MUL db 'MUL', 20h
	k_NEG db 'NEG', 20h
	k_XCHG db 'XCHG', 20h
	k_NOT db 'NOT', 20h
	k_OR db 'OR', 20h
	k_OUT db 'OUT', 20h
	k_POP db 'POP', 20h
	k_POPF db 'POPF', 20h
	k_PUSH db 'PUSH', 20h
	k_PUSHF db 'PUSHF', 20h
	k_RCL db 'RCL', 20h
	k_RCR db 'RCR', 20h
	k_REP db 'REP', 20h
	k_REPNZ db 'REPNZ', 20h
	k_RET db 'RET', 20h
	k_RETF db 'RETF', 20h
	k_ROL db 'ROL', 20h
	k_ROR db 'ROR', 20h
	k_SAHF db 'SAHF', 20h
	k_SAR db 'SAR', 20h
	k_SBB db 'SBB', 20h
	k_SCASB db 'SCASB', 20h
	k_SCASW db 'SCASW', 20h
	k_SHL db 'SHL', 20h
	k_SHR db 'SHR', 20h
	k_STC db 'STC', 20h
	k_STD db 'STD', 20h
	k_STI db 'STI', 20h
	k_STOSB db 'STOSB', 20h
	k_STOSW db 'STOSW', 20h
	k_SUB db 'SUB', 20h
	k_TEST db 'TEST', 20h
	k_WAIT db 'WAIT', 20h
	k_XLAT db 'XLAT', 20h
	k_XOR db 'XOR', 20h
	
	r_AX db 'ax', 20h
	r_AL db 'al', 20h
	r_AH db 'ah', 20h
	r_BX db 'bx', 20h
	r_BL db 'bl', 20h
	r_BH db 'bh', 20h
    r_CX db 'cx', 20h
	r_CL db 'cl', 20h
	r_CH db 'ch', 20h
    r_DX db 'dx', 20h
	r_DL db 'dl', 20h
	r_DH db 'dh', 20h
    r_SP db 'sp', 20h
    r_BP db 'bp', 20h
    r_SI db 'si', 20h
    r_DI db 'di', 20h
	r_DS db 'ds', 20h
	r_ES db 'es', 20h
	r_CS db 'cs', 20h
	r_SS db 'ss', 20h

	
	error1 db 'Truksta parametru', 0Dh,0Ah, 24h
	error2 db 'Negalima atidaryti ivesties failo', 0Dh,0Ah, 24h
	error3 db 'Negalima atidaryti isvesties failo', 0Dh,0Ah, 24h
	instruct db 'Naudojimas: programa failopavadinimas', 0Dh,0Ah, 24h
    noid db 'Neatpazinta', 20h
	fileout db 'Disas.asm', 00h
	filein db 255 dup(00h)
	
	infh dw 00h									   ;filehandlai
	outfh dw 00h
	plus db "+", 20h
	comma db ",", 20h
	colon db ":"
	opbrack db "[", 20h
	clbrack db "]", 20h
	h db "h", 20h
	space db 20h
	
	dsv db 00h
	w db 00h
	modas db 00h
	reg db 00h
	rm db 00h
	port db 00h
	
	offs dw 0000h									;laiko adresa
	count db 0FFh									;laiko, kiek nuskaite
	baitas db 00h									;laiko nuskaityta baita
	
	endl db 0Ah, 0Dh, 24h
	input db 00h, 255 dup ("$")
	output db 00h, 255 dup ("$")
	stdinarr db 255 , 255 dup ("$")
	line db 00h, 255 dup ("$")
	;printarr db 256 dup ("$")
	tabnr db 00h
	pref db 00h
	
.code
;---------------------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------------------
;MACROS

call_start macro reg1, reg2, reg3, reg4									;sutvarko kiekviena rigistra funkcijos pradzioj
	push reg1
	push reg2
	push reg3
	push reg4 
	
	xor reg1, reg1
	xor reg2, reg2
	xor reg3, reg3
	xor reg4, reg4
endm

call_end macro reg1, reg2, reg3, reg4									;sutvarko kiekviena rigistra funkcijos pabaigoj
	pop reg4
	pop reg3
	pop reg2
	pop reg1
endm

check macro baitas, kom, ilgis
	local @@toliau
	cmp al, baitas&b
	jne @@toliau
	put_string k_&kom, ilgis
@@toliau:
endm

check_reg macro kom0, l0, kom1, l1, kom2, l2, kom3, l3, kom4, l4, kom5, l5, kom6, l6, kom7, l7
	mov al, reg
	check 000, kom0, l0
	check 001, kom1, l1
	check 010, kom2, l2
	check 011, kom3, l3
	check 100, kom4, l4
	check 101, kom5, l5
	check 110, kom6, l6
	check 111, kom7, l7
endm
	
	
find_com macro kom, nr, ilgis
	cmp al, nr
	jne ne_&kom
	put_string k_&kom, ilgis	
	
ne_&kom:	
endm

check_sek macro baitas
	local @@kitas
	cmp al, baitas
	jne @@kitas
	call SEK_&baitas
	
@@kitas:
endm

cmp_reg macro baitai, reg_w0, reg_w1, reg1, reg2
	cmp al, baitai
	jne ne_&baitai
	find_reg_macro reg_w0, reg_w1, reg1, reg2 
	jmp find_reg_ret
	
ne_&baitai:
endm

regrm_macro macro constreg, regrm				;kviecia reg arba rm
	local @@end_macro
	
	xor cx, cx
	xor ax, ax
	mov cl, constreg
	mov al, regrm
	call find_reg
	
	cmp cl, 11b
	je @@end_macro
	call modas_poslinkis	
	
@@end_macro:
endm
	
find_reg_macro macro reg_w0, reg_w1, reg1, reg2						;al registro adresavimo baito  
	local @@ne_11, @@w_01, @@toliau, @@end							;analize, kai cl'e yra modas
	
	xor bx, bx
	cmp cl, 11b
	jne @@ne_11
	
	cmp w, 00b
	jne @@w_01
	put_string reg_w0, 03h
	jmp @@end
	
@@w_01:
	put_string reg_w1, 03h 
	jmp @@end
	
@@ne_11:
	put_string opbrack, 02h
	cmp al, 110b													;tiesioginis adresas
	jne @@toliau
	cmp cl, 00b
	jne @@toliau
	call du_baitai
	jmp @@end
	
@@toliau:
	put_string reg1, 03h
	cmp al, 011b
	ja @@end
	
	put_string plus, 02h                     
	put_string reg2, 03h
	
@@end:	
endm

put_string macro string, len
	push bx
	push cx
	lea bx, string
	mov cx, len
	call mov_string_line
	pop cx
	pop bx
endm

; print_array macro array, len
	; local @@loop
	; push cx
	; push dx
	; xor dx, dx
	; xor ch, ch
	
	; mov cl, len
	; xor si, si
	; xor di, di

; @@loop:
	; mov dl, ds:[array + si]
	; ror dx, 04h
	; call hex_to_ascii
	; mov ds:[printarr + di], dl
	; inc di
	; shr dx, 0Ch
	; call hex_to_ascii
	; mov ds:[printarr + di], dl
	; inc di
	
	; inc si
	; dec cx
	; cmp cx, 00h
	; jne @@loop
	
	; call print_endl
	; mov al, len
	; mov bx, 02h
	; mul bl
	; mov cl, al
	; dec bx
	; xor ax, ax
	; lea dx, printarr
	; mov ah, 40h
	; int 21h
	; call print_endl	
	; pop dx
	; pop cx
; endm	
;---------------------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------------------
;KODAS
start:
	mov dx, @data
	mov ds, dx
	xor dx, dx
 
	xor cx, cx		
	mov cl, es:[80h]

	cmp cl, 00h
	jne no_stdin
	
	lea dx, stdinarr
	mov ah, 0Ah
	int 21h
	
	mov al, [stdinarr + 1]
	cmp al, 00h
	jne no_exit
	call exit
	
no_exit:
	call stdin_to_in
	jmp no_error
	
no_stdin:	
	mov ah, es:[82h]
	mov al, es:[83h]
	cmp ax, 2F3Fh
	jne no_instruct
	
	lea dx, instruct
	mov ah, 09h 
	int 21h	
	call exit

no_instruct:	
	mov si,	82h
	
filename:
	mov al, es:[si + bx]
	mov ds:[filein + bx + 2], al
	inc bx

	cmp al, 0Dh
	jne filename

	mov [filein + 1], bl
	dec bx
	mov [filein + bx + 2], 00h

	mov ax, 3D00h
	lea dx, filein + 2
	int 21h
	mov infh, ax

	jnc open_file
	lea dx, error2
	mov ah, 09h
	int 21h	
	call exit

open_file:
	call read
	
no_error:
	mov ax, 3C00h				;sukuria isvesties faila
	xor cx, cx
	lea dx, fileout
	int 21h
	mov outfh, ax
	
	xor ax, ax
	xor dx, dx
	
main_loop:
	call offset_func	

no_offset:
	call get_byte
	xor bx, bx
	mov bl, baitas
	mov dx, bx
	mov al, [OPK + bx]
	
	cmp al, 66h
	jne ne_prefix
	mov pref, bl
	jmp no_offset
	
ne_prefix:	
	cmp al, 4Bh
	jne ne_REP
	put_string k_REP, 04h
	jmp no_offset
	
ne_REP:	
	cmp al, 4Ch
	jne ne_REPNZ
	put_string k_REPNZ, 06h
	jmp no_offset	
	
ne_REPNZ:
	
	call find_kom_func
	call check_sek_func

main_loop_end:	
	call mov_line_out
	call print_output
		
	call_end ax, bx, cx, dx
	jmp main_loop
	
	;call exit
	
;---------------------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------------------
;FUNKCIJOS	

SEK_00h:
	push ax										;nagrineja i dl ideta OPK ir randa sekancius baitasus						
	call baito_analize
	
	cmp dsv, 01h
	jne d_0
	
	regrm_macro 11b, reg
	put_string comma, 02h
	call prefix
	regrm_macro modas, rm
	jmp SEK_00h_ret 
	
d_0:
	call prefix
	regrm_macro modas, rm
	put_string comma, 02h
	regrm_macro 11b, reg
	
SEK_00h_ret:
	pop ax
ret	

SEK_01h:
	push ax
	call baito_analize
	mov w, 01b
	
	cmp dsv, 01b
	jne SEK_01h_d_0
	
	call seg_reg
	put_string comma, 02h
	call prefix
	regrm_macro modas, rm
	jmp SEK_01h_ret
	
SEK_01h_d_0:
	call prefix
	regrm_macro modas, rm
	put_string comma, 02h
	call seg_reg
	
SEK_01h_ret:
	pop ax
ret	
	
SEK_02h:									;nagrineja i dl ideta OPK ir randa sekancius baitus	
	push ax
	call baito_analize
	check_reg ADD, 04h, OR, 03h, ADC, 04h, SBB, 04h, AND, 04h, SUB, 04h, XOR, 04h, CMP, 04h
	call prefix
	regrm_macro modas, rm
	put_string comma, 02h
	call sw
	pop ax
ret

SEK_03h:
	push ax
	call baito_analize
	check_reg TEST, 05h, NOT, 04h, NOT, 04h, NEG, 04h, MUL, 04h, IMUL, 05h, DIV, 04h, IDIV, 05h	
	call prefix
	regrm_macro modas, rm
	put_string comma, 02h
	
	cmp reg, 000b
	jne SEK_03h_ret
	call bojb_bovb
	
SEK_03h_ret:	
	pop ax
ret	

SEK_04h:					;TEST ir MOV su bojb [bovb]
	push ax
	call baito_analize
	call prefix
	regrm_macro modas, rm
	put_string comma, 02h
	mov dsv, 00b
	call sw
	pop ax
ret

SEK_05h:
	push ax
	push bx
	call baito_analize
	check_reg ROL, 04h, ROR, 04h, RCL, 04h, RCR, 04h, SHL, 04h, SHR, 04h, SAR, 04h, SAR, 04h
	call prefix
	regrm_macro modas, rm
	put_string comma, 02h
	
	cmp dsv, 01b
	je SEK_05h_cl
	
	mov dx, 01h
	lea bx, line
	call mov_byte
	put_string h, 02h
	jmp SEK_05h_ret
	
SEK_05h_cl:
	put_string opbrack, 02h
	put_string r_CL, 03h
	put_string clbrack, 02h
	
SEK_05h_ret:
	pop bx
	pop ax
ret

SEK_06h:
call_start ax, bx, dx, dx
	call get_byte
	mov al, baitas
	mov dx, offs
	
	cmp al, 80h
	jb plesti_0
	mov ah, 0FFh
	jmp sudeti
	
plesti_0:
	mov ah, 00h
	
sudeti:
	add dx, ax
	lea bx, line
	xchg dh, dl
	call mov_byte
	xchg dh, dl
	call mov_byte
	put_string h, 02h
	
call_end ax, bx, dx, dx	
ret

SEK_07h:
	push ax
	call get_byte
	mov cl, baitas
	call get_byte
	mov ch, baitas
	call get_byte
	mov al, baitas
	call get_byte
	mov ah, baitas
	
	mov bx, 10h
	mul bx
	
	add ax, cx
	jno SEK_07h_ret
	inc dx
	
SEK_07h_ret:
	lea bx, line
	call mov_byte
	mov dl, ah
	call mov_byte
	mov dl, al
	call mov_byte
	put_string clbrack, 02h
	pop ax
ret

SEK_08h:
	push ax
	call get_dsv_w
	
	cmp dsv, 01b
	je SEK_08h_d_1
	
	call mov_akum
	put_string comma, 02h
	put_string opbrack, 02h
	call du_baitai
	put_string clbrack, 02h
	jmp SEK_08h_ret
	
SEK_08h_d_1:
	put_string opbrack, 02h
	call du_baitai
	put_string clbrack, 02h
	put_string comma, 02h
	call mov_akum

SEK_08h_ret:
	pop ax
ret	
	
SEK_09h:
	push ax
	call get_dsv_w
	call bojb_bovb
	pop ax
ret

SEK_0Ah:
	push ax
	push dx
	and dl, 00001000b
	shr dl, 03h
	mov w, dl
	pop dx
	and dl, 00000111b
	mov reg, dl
	mov modas, 11b
	
	call prefix
	regrm_macro modas, reg
	put_string comma, 02h
	call bojb_bovb
	
	pop dx
	pop ax
ret

SEK_0Bh:   ;du baitai
	push ax
	call get_byte
	pop ax
ret

SEK_0Ch:
	push ax
	call get_dsv_w
	and dl, 00001000b
	shr dl, 03h
	mov port, dl 
	
	cmp dsv, 01b
	je SEK_0Ch_d_1
	
	call mov_akum
	put_string comma, 02h
	call dx_port
	jmp SEK_0Ch_ret

SEK_0Ch_d_1:
	call dx_port
	put_string comma, 02h
	call mov_akum
	
SEK_0Ch_ret:
	pop ax
ret

SEK_0Dh:
	push ax
	call du_baitai
	pop ax
ret

SEK_0Eh:
	push ax
	call baito_analize
	check_reg INC, 04h, DEC, 04h, CALL, 05h, CALL, 05h, JMP, 04h, JMP, 04h, PUSH, 05h, PUSH, 05h
	mov w, 01b
	call prefix
	regrm_macro modas, rm
	pop ax
ret	

SEK_0Fh:
	push ax
	mov w, 01b
	mov modas, 11b
	push dx
	and dl, 00000111b
	mov reg, dl
	pop dx
	call prefix
	regrm_macro modas, reg
	pop ax
ret

SEK_10h:
	push ax
	push dx
	and dl, 00011000b
	mov reg, dl
	pop dx
	call seg_reg
	pop ax
ret

SEK_12h:
	push ax
	call baito_analize
	mov w, 01h
	regrm_macro 11b, reg
	put_string comma, 02h
	regrm_macro modas, rm	
	pop ax
ret	

SEK_13h:
	push ax
	call baito_analize
	call prefix
	regrm_macro modas, rm
	pop ax
ret	

SEK_14h:
	push ax
	call vienas_baitas
	put_string h, 02h
	pop ax
ret

SEK_15h:
	push ax
	push bx
	push dx
	
	call get_byte
	mov al, baitas
	call get_byte
	mov ah, baitas
	mov dx, offs
	add dx, ax
	lea bx, line
	xchg dh, dl
	call mov_byte
	xchg dh, dl
	call mov_byte
	put_string h, 02h
	
	pop dx
	pop bx
	pop ax
ret

SEK_16h:
	push ax
	call get_dsv_w
	call mov_akum
	put_string comma, 02h
	call bojb_bovb
	pop ax
ret
	
;---------------------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------------------
; pagalbines

bojb_bovb:
	cmp w, 01b
	je bojb_bovb_du_baitai
	call vienas_baitas
	put_string h, 02h
	jmp  bojb_bovb_ret
	
bojb_bovb_du_baitai:
	call du_baitai
	
bojb_bovb_ret:
ret

prefix:
	call_start ax, bx, cx, dx
	mov dl, pref
	cmp dl, 00h
	je no_prefix
	
	cmp dl, 26h
	jne ne_es
	put_string r_ES, 03h
	
ne_es:
	cmp dl, 2Eh
	jne ne_cs
	put_string r_CS, 03h
	
ne_cs:
	cmp dl, 36h
	jne ne_ss
	put_string r_SS, 03h
	
ne_ss:
	cmp dl, 3Eh
	jne ne_ds
	put_string r_DS, 03h

ne_ds:
	put_string colon, 01h
	mov pref, 00h
	
no_prefix:
call_end ax, bx, cx, dx
ret
	

mov_string_line:			;bx'e esanti stringa perkelia i line masyva
	push ax
	push dx
	xor ax, ax
	xor dx, dx
	xor si, si
	
	mov dl, [line]
	mov di, dx
	inc di	
	add [line], cl
	
mov_string_loop:
	mov dl, ds:[bx + si]
	mov ds:[line + di], dl
	inc si
	inc di
	loop mov_string_loop
	
	pop dx
	pop ax
ret

mov_line_out:
	call_start ax, bx, cx, dx
	mov bl, [output]
	mov di, bx
	inc di
	
	mov ah, tabnr
	mov al, 08h
	sub al, ah
	xor ah, ah
	
	mov cl, 02h
	mul cl
	mov cx, ax
	add [output], cl

tab_loop:
	mov ds:[output + di], 20h
	inc di
	loop tab_loop
	
	mov tabnr, 00h
	
	
	put_string endl, 01h
	mov cl, [line]
	mov si, 01h
	
	
	mov bl, [output]
	mov di, bx
	inc di
	
	add [output], cl					;sutvarko masyvus tolesniam naudojimui
	mov [line], 00h
	
line_out_loop:
	mov al, ds:[line + si]
	mov ds:[output + di], al
	inc si
	inc di
	loop line_out_loop
	 
	call_end ax, bx, cx, dx
ret

find_kom_func:
	find_com AAA, 01h, 04h
	find_com AAD, 02h, 04h
	find_com AAM, 03h, 04h
	find_com AAS, 04h, 04h
	find_com ADC, 05h, 04h
	find_com ADD, 06h, 04h
	find_com AND, 07h, 04h
	find_com CALL, 08h, 05h
	find_com CBW, 09h, 04h
	find_com CLC, 0Ah, 04h
	find_com CLD, 0Bh, 04h
	find_com CLI, 0Ch, 04h
	find_com CMC, 0Dh, 04h
	find_com CMP, 0Eh, 04h
	find_com CMPSB, 0Fh, 06h
	find_com CMPSW, 10h, 06h
	find_com CWD, 11h, 04h
	find_com DAA, 12h, 04h
	find_com DAS, 13h, 04h
	find_com DEC, 14h, 04h
	find_com DIV, 15h, 04h
	find_com ESC, 16h, 04h
	find_com HLT, 17h, 04h
	find_com IDIV, 18h, 05h
	find_com IMUL, 19h, 05h
	find_com IN, 1Ah, 03h
	find_com INC, 1Bh, 04h
	find_com INT, 1Ch, 04h
	find_com INT3, 1Dh, 06h
	find_com INTO, 1Eh, 05h
	find_com IRET, 1Fh, 05h
	find_com JA, 20h, 03h
	find_com JAE, 21h, 04h
	find_com JBE, 22h, 04h
	find_com JCXZ, 23h, 05h
	find_com JE, 24h, 03h
	find_com JG, 25h, 03h
	find_com JGE, 26h, 04h
	find_com JL, 27h, 03h
	find_com JLE, 28h, 04h
	find_com JMP, 29h, 04h
	find_com JNAE, 2Ah, 05h
	find_com JNE, 2Bh, 04h
	find_com JNO, 2Ch, 04h
	find_com JNP, 2Dh, 04h
	find_com JNS, 2Eh, 04h
	find_com JO, 2Fh, 03h
	find_com JP, 30h, 03h
	find_com JS, 31h, 03h
	find_com LAHF, 32h, 05h
	find_com LDS, 33h, 04h
	find_com LEA, 34h, 04h
	find_com LES, 35h, 04h
	find_com LOCK, 36h, 05h
	find_com LODSB, 37h, 06h
	find_com LODSW, 38h, 06h
	find_com LOOP, 39h, 05h
	find_com LOOPE, 3Ah, 06h
	find_com LOOPNE, 3Bh, 07h
	find_com MOV, 3Ch, 04h
	find_com MOVSB, 3Dh, 06h
	find_com MOVSW, 3Eh, 06h
	find_com MUL, 3Fh, 04h
	find_com NEG, 40h, 04h
	find_com XCHG, 41h, 05h
	find_com NOT, 42h, 04h
	find_com OR, 43h, 03h
	find_com OUT, 44h, 04h
	find_com POP, 45h, 04h
	find_com POPF, 46h, 05h
	find_com PUSH, 47h, 05h
	find_com PUSHF, 48h, 06h
	find_com RCL, 49h, 04h
	find_com RCR, 4Ah, 04h
	find_com RET, 4Dh, 04h
	find_com RETF, 4Eh, 05h
	find_com ROL, 4Fh, 04h
	find_com ROR, 50h, 04h
	find_com SAHF, 51h, 05h
	find_com SAR, 52h, 04h
	find_com SBB, 53h, 04h
	find_com SCASB, 54h, 06h
	find_com SCASW, 55h, 06h
	find_com SHL, 56h, 04h
	find_com SHR, 57h, 04h
	find_com STC, 58h, 04h
	find_com STD, 59h, 04h
	find_com STI, 5Ah, 04h
	find_com STOSB, 5Bh, 06h
	find_com STOSW, 5Ch, 06h
	find_com SUB, 5Dh, 04h
	find_com TEST, 5Eh, 05h
	find_com WAIT, 5Fh, 05h
	find_com XLAT, 60h, 05h
	find_com XOR, 61h, 04h
	
	cmp al, 0FFh
	jne zinomas
	put_string noid, 0Ch
	jmp main_loop_end
	
zinomas:
ret

check_sek_func:

	xor bx, bx
	mov bl, dl
	mov al, [formatas + bx]
	check_sek 00h
	check_sek 01h
	check_sek 02h
	check_sek 03h
	check_sek 04h
	check_sek 05h
	check_sek 06h
	check_sek 07h
	check_sek 08h
	check_sek 09h
	check_sek 0Ah
	check_sek 0Bh
	check_sek 0Ch
	check_sek 0Dh
	check_sek 0Eh
	check_sek 0Fh
	check_sek 10h
	check_sek 12h
	check_sek 13h
	check_sek 14h
	check_sek 15h
	check_sek 16h
	
ret

get_byte:														;ideda sekanti baita is input i baitas
	call_start bx, dx, bx, dx
	mov bl, [input]												;tikrina, ar nepriejo pabaigos
	cmp bl, count
	je call_read
	
	inc bx
	mov dl, [input + bx]
	mov baitas, dl
	inc [input]
	jmp no_read
	
call_read:															
	call read
	mov dl, [input + 1]
	inc [input]
	mov baitas, dl
	
no_read:
	inc offs 
	call add_maskod
	call_end bx, dx, bx, dx
ret

read:
	cmp count, 0FEh					;jei paskutinikart perskaite maziau - baigia
	jb read_exit
	
	lea dx, input + 1				;perskaito 255 simbolius ir palieka pirma masyvo baita nusk. simboliu kiekiui
	mov ax, 3F00h
	mov bx, infh
	mov cx, 0FEh 
	int 21h
	xor si, si
	mov count, al					;saugo kiek nuskaite kitam skaitymui
	mov [input], 00h				;saugo vieta, kiek baitu nuskaityta

	cmp ax, 00h						;tikrina ar neperskaite 0
	jne read_no_exit
	
read_exit:
	call exit
	
read_no_exit:
ret

mov_byte:			;is dl'o baita ideda i masyva, kurio adresas bx
	push bx
	push cx
	push dx
	xor ch, ch
	xor dh, dh
	
	mov cl, ds:[bx]
	mov di , cx
	inc di
	
	ror dx, 04h
	call hex_to_ascii
	mov ds:[bx + di], dl
	inc di
	
	shr dx, 0Ch
	call hex_to_ascii
	mov ds:[bx + di], dl
	add byte ptr ds:[bx], 02h
	
	pop dx
	pop cx
	pop bx
ret

vienas_baitas:
	call_start ax, bx, cx, dx
	
	call get_byte
	mov dl, baitas
	lea bx, line
	call mov_byte
	
	call_end ax, bx, cx, dx
ret

du_baitai:
	call_start bx, dx, bx, dx
	
	call vienas_baitas
	call vienas_baitas
	call sukeisti
	put_string h, 02h
	
	call_end bx, dx, bx, dx
ret

sukeisti:														;sukeicia du baitus vietomis line'e
	call_start ax, bx, cx, dx
	mov cx, 02h
	mov bl, [line]
	dec bl
	
sukeisti_l:
	mov al, [line + bx - 2]
	mov dl, [line + bx]
	mov [line + bx - 2], dl
	mov [line + bx], al	
	inc bx
	loop sukeisti_l
		
	call_end ax, bx, cx, dx
ret

baito_analize:							;atskiria modas, reg ir r/m bitus												;analizuoja baitasa, esanti dl
	call get_dsv_w	
	call get_byte
	mov dl, baitas
	push dx
	and dl, 11000000b
	shr dl, 06h
	mov modas, dl
	pop dx
	push dx
	and dl, 00111000b
	shr dl, 03h
	mov reg, dl
	pop dx
	and dl, 00000111b
	mov rm, dl
ret

get_dsv_w:								;atskiria dsv ir w bitus is dl'o	
	push dx
	and dl, 00000010b
	shr dl, 01h
	mov dsv, dl
	pop dx
	push dx
	and dl, 00000001b
	mov w, dl
	pop dx
ret

sw:
	cmp w, 00b
	jne sw_w_1
	
	call vienas_baitas
	put_string h, 02h
	jmp sw_ret

sw_w_1:
	cmp dsv, 00b
	jne sw_s_1
	
	call du_baitai
	jmp sw_ret

sw_s_1:
	call isplesti_zenkla
	
sw_ret:
ret

isplesti_zenkla:
	call_start ax, bx, cx, dx
	lea bx, line	
	call get_byte
	cmp baitas, 80h
	jb plesti_nuliais
	
	mov dl, 0FFh
	call mov_byte
	mov dl, baitas
	call mov_byte
	jmp isplesti_zenkla_ret
	
plesti_nuliais:
	mov dl, 00h	
	call mov_byte
	mov dl, baitas
	call mov_byte
	
isplesti_zenkla_ret:
	put_string h, 02h
	call_end ax, bx, cx, dx
ret	

mov_akum:
	cmp w, 01b
	je mov_ax
	
	put_string r_AL, 03h
	jmp mov_akum_ret
	
mov_ax:
	put_string r_AX, 03h
	
mov_akum_ret:
ret

dx_port:
	cmp port, 01b
	je mov_dx
	
	call vienas_baitas
	put_string h, 02h
	jmp dx_port_ret
	
mov_dx:
	put_string opbrack, 02h
	put_string r_DX, 03h
	put_string clbrack, 02h
	
dx_port_ret:
ret

seg_reg:
	cmp reg, 010b
	ja seg_reg_ja
	jb seg_reg_jb
	put_string r_SS, 03h
	jmp seg_reg_ret
	
seg_reg_ja:
	put_string r_DS, 03h
	jmp seg_reg_ret

seg_reg_jb:
	put_string r_ES, 03h

seg_reg_ret:
ret

hex_to_ascii:											;dl'e esanti simboli pavercia i ascii hex skaiciu
	add dl, 30h
	cmp dl, 3Ah
	jb hex_to_ascii_ret
	add dl, 07h
hex_to_ascii_ret:
ret

modas_poslinkis:
	cmp modas, 01b
	jne modas_10
	
	put_string plus, 02h
	call isplesti_zenkla
	jmp modas_poslinkis_ret
	
modas_10:	
	cmp modas, 10b
	jne modas_poslinkis_ret
	
	put_string plus, 02h
	call du_baitai
	jmp modas_poslinkis_ret
	
modas_poslinkis_ret:
	put_string clbrack, 02h
ret

find_reg:									;randa al esancio baito registra arba rm
	cmp_reg 000b, r_AL, r_AX, r_BX, r_SI
	cmp_reg 001b, r_CL, r_CX, r_BX, r_DI
	cmp_reg 010b, r_DL, r_DX, r_BP, r_SI
	cmp_reg 011b, r_BL, r_BX, r_BP, r_DI
	cmp_reg 100b, r_AH, r_SP, r_SI, r_SI
	cmp_reg 101b, r_CH, r_BP, r_DI, r_DI
	cmp_reg 110b, r_DH, r_SI, r_BP, r_BP
	cmp_reg 111b, r_BH, r_DI, r_BX, r_BX
	
find_reg_ret:
ret
	
offset_func:
call_start ax, bx, cx, dx

	mov ax, offs	
	mov cx, 100h
	div cx
	
	xchg al, dl
	lea bx, output
	call mov_byte
	xchg al, dl
	call mov_byte
	
	xor bh, bh
	mov bl, [output]
	inc bl
	mov [output + bx], 3Ah
	mov [output + bx + 1], 20h
	add [output], 02h
	
call_end ax, bx, cx, dx
ret

add_maskod:
call_start ax, bx, cx, dx
	inc tabnr
	
	mov bl, [output]
	inc bx
	mov dl, baitas
	
	ror dx, 04h
	call hex_to_ascii
	mov [output + bx], dl
	inc bx
	
	shr dx, 0Ch
	call hex_to_ascii
	mov [output + bx], dl
	add [output], 02h
	
call_end ax, bx, cx, dx
ret	

stdin_to_in:
call_start ax, bx, cx, dx
	
	mov al, [stdinarr + 1]				
	cmp al, 01h
	ja continue
	call exit
	
continue:	
	mov bx, 02h							;darom dvigubai maziau
	div bl
	mov cl, al
	mov count, cl						;issisaugom kiek baitu
	
	mov si, 02h
	mov di, 01h
	mov bx, 10h
	
stdin_loop:
	mov dl, ds:[stdinarr + si]
	inc si
	call ascii_to_hex
	mov al, dl
	mov dl, ds:[stdinarr + si]
	inc si
	call ascii_to_hex
	
	mul bl
	add al, dl
	mov ds:[input + di], al
	inc di
	
	dec cx
	cmp cx, 00h
	jne stdin_loop
	
	mov [input], 00h
call_end ax, bx, cx, dx
ret	

ascii_to_hex:
	cmp dl, 41h
	jb numb
	sub dl, 37h
	jmp ascii_to_hex_ret
numb:
	sub dl, 30h
ascii_to_hex_ret:
ret	
;--------------------------------------------------------
;--------------------------------------------------------
show_line:
call_start ax, bx, cx, dx
	mov bx, 01h
	mov cl, [line]
	lea dx, line + 1
	mov ah, 40h
	int 21h
	call print_endl
call_end ax, bx, cx, dx
ret

print_output:
call_start ax, bx, cx, dx
	
	mov al, [input]
	cmp al, count
	je print
	
	mov cl, [output]						
	cmp cl, 0C8h
	ja print
	jmp no_print
	
print:
	mov cl, [output]
	lea dx, output + 1
	mov ax, 4000h
	mov bx, outfh
	int 21h
	
	mov [output], 00h
	
no_print:
call_end ax, bx, cx, dx
ret
	
print_endl:
	push dx
	push ax
	lea dx, endl
	mov ah, 09h
	int 21h
	pop ax
	pop dx
ret 
	
exit:
	pop cx
	mov ah, 4Ch
	mov al, 0h
	int 21h
end start