data segment
		recordbuf db 1024 dup(0)
		rbc dw 0				    ;record buf count
		rcount dw 0
		rrest dw 0
		playbuf db 1024 dup(0)
		pbc dw 0 					;play buf count
		pcount dw 0
		plength dw 0
		prest dw 0
		sbc dw 0            ;send buf count
		scount dw 0
		abuf db 1024 dup(0)
		abc dw 0            ;accept buf count
        tmpabc dw 0         ;debug
		acount dw 0
		endl db 0dh,0ah,'$'
		oldseg dw ?
		oldoff dw ?
		first db 1
		rlenmax db 0
		slen db 0
		rlen db 0
		msg0 db 'press key to choose mode:',0dh,0ah
		db '0:single mode(recorder)',0dh,0ah
		db '1:double mode(wechat)',0dh,0ah
		db '2:double mode(interphone)',0dh,0ah,'$'
		msg1 db 'Single Mode',0dh,0ah
		db 'press r to record.',0dh,0ah
		db 'press p to play your record.',0dh,0ah
		db 'press esc to exit.',0dh,0ah,'$'
		msg2 db 'message sent.',0dh,0ah,'$'
		msg3 db 'press p to play your record.',0dh,0ah,'$'
		msg4 db 'you received a message, press p to play.',0dh,0ah,'$'
		msgr db 'Recording, Press E to exit.',0dh,0ah,'$'
		msgp db 'Playing...',0dh,0ah,'$'
        msgs db 'Sending...',0dh,0ah,'$'
        msgsendsucc db 'Send Success.',0dh,0ah,'$'
		msgerr0 db 'create file error!',0dh,0ah,'$'
		msgerr1 db 'write file error!',0dh,0ah,'$'
		msgerr2 db 'open file error!',0dh,0ah,'$'		
		msgerr3 db 'read file error!',0dh,0ah,'$'
		rfilename db 'D:\STUDENT\f0.dat',0
		pfilename db 'D:\STUDENT\f1.dat',0
		playfilename dw 0
		ipmsg0 db 'Interphone Mode',0dh,0ah
		db 'Press R and speak.',0dh,0ah
		db 'Press Esc to exit',0dh,0ah,'$'
		ipmsg1 db 'Speaking, Press E to exit.',0dh,0ah,'$'
		wcmsg0 db 'WeChat Mode',0dh,0ah
		db 'Press R and speak.',0dh,0ah
		db 'Press Esc to exit',0dh,0ah,'$'
		wcmsg1 db 'Speaking, Press E to send.',0dh,0ah,'$'
		wcmsg2 db 'No message received!',0dh,0ah,'$'
        msgtmp db 'Write.',0dh,0ah,'$'
		newmsgrcvd db 0
		opensuc db 0
		msgreceived db 'You have received a message, Press P to play.',0dh,0ah
        ll equ $-msgreceived   ;length of msgreceived
		curfile dw 0
		rhandle dw 0
		phandle dw 0
		shandle dw 0
		ahandle dw 0
		delaynum db 0
		play_delaynum db 0
		record_delaynum db 0
		sendbyte db 0
		recordsucc db 1
		headcount db 0
		rheadcount db 0
        intbuf db 0
        intbufempty db 1
data ends
code segment
		assume cs:code,ds:data
start:
		mov ax,data
		mov ds,ax
		mov ah,09h
		mov dx,offset msg0
		int 21h  ;print msg0
keyboard0:
		mov ah,1
		int 16h
		jz keyboard0 				  ;no key press,loop
		mov ah,07h
		int 21h						 ;clear input buffer
		cmp al,48
		jz singlemode
		cmp al,49
		jz wcmode				       ;wechat mode
		cmp al,50
        jz ipmodepre                      ;interphone mode
		cmp al,27
		jz exit						 ;exit
		jmp keyboard0
exit:
		mov ah,4ch
		int 21h
ipmodepre:
        jmp ipmode
singlemode:
		mov ah,09h
		mov dx,offset msg1
		int 21h
keyboard1:
		mov ah,1
		int 16h
		jz keyboard1 				  ;no key press,loop
		mov ah,07h
		int 21h						 ;clear input buffer
		cmp al,'r'
		jz callsrecord
		cmp al,'p'
		jz callplay
		cmp al,27
		jz exit
		jmp keyboard1
callsrecord:
		mov record_delaynum, 50
		mov recordsucc, 1
		call srecord

		mov cx,rrest
		mov prest,cx
		mov cx,rcount
		mov plength,cx
		
		jmp singlemode
callplay:
		cmp recordsucc, 0
		jz singlemode
		mov play_delaynum, 50
		mov playfilename, offset rfilename
		call play
		jmp singlemode

wcmode:
		mov ax,data
		mov ds,ax
		mov al,1eh     ;8253 counter 0, method 3
		mov dx,0e483h
		out dx,al
        mov al,4          ;15625bps*16
		mov dx,0e480h     ;8253 counter0, clk for 8251
		out dx,al

		mov dx,0e4b9h   ;8251
		mov al,0
		out dx,al
		out dx,al
		out dx,al
		mov al,40h
		out dx,al
		mov al,4eh      ;word length:8, asynchronous
		out dx,al
		mov al,27h
		out dx,al

		cli
		mov ax,350eh   ;save old interrupt vector
		int 21h
		mov oldseg,es
		mov oldoff,bx

		mov ax,code    ;set pci interrupt
		mov ds,ax
		mov dx,offset irqwc
		mov ax,250eh
		int 21h

		mov dx,0ec4ch
		mov al,43h
		out dx,al
		inc dx
		mov al,1dh
		out dx,al
		in al,21h
		and al,10111111b
		out 21h,al
		mov ax,data
		mov ds,ax
		sti
		mov ah,09h
		mov dx,offset wcmsg0
		int 21h  ;print wcmsg0
		mov rheadcount, 0

		mov ah,3ch
		mov dx,offset pfilename
		xor cx,cx
		int 21h						;create file
		jnc wccreatesuccess
wcerr0:						;create error
		mov ah,09h
		mov dx,offset msgerr0
		int 21h
		jmp wcmode
wccreatesuccess:
		mov ahandle,ax				  ;file rhandle
		mov abc,0
		mov acount,0
		mov opensuc,1
		mov intbufempty, 1
keyboardwc:
waitint:
        cmp intbufempty,0
        jz intnext
		mov ah,1
		int 16h
        jz waitint                     ;no key press,loop
		mov ah,07h
        int 21h                    ;clear inputbuffer
		cmp al,'p'
        jz wcp1
        cmp al,'r'
        jz wcrecord
        cmp al,27
        jnz waitint
        jmp wcexit
wcp1:
        call wcplay
        jmp wcexit
intnext:
		mov si,abc
        mov al,intbuf
		mov abuf[si],al
		inc abc
bufclear:
        mov intbufempty, 1
        cmp abc,1024
		jl keyboardwcnext
wcwrite:
        mov bx,ahandle
		mov cx,1024
		mov dx,offset abuf
		mov ah,40h
		int 21h    				     ;write into file

        jnc afterwrite
wcerr1:						;write error
		mov ah,09h
		mov dx,offset msgerr1
		int 21h
		jmp wcmode
		;set abuf 0?
afterwrite:
        cmp acount,0
        jnz writenormal
        mov al,abuf[0]
        mov ah,abuf[1]
        mov plength, ax
writenormal:
        mov dx,abc
		mov abc,0
		inc acount
        mov cx,acount
        cmp cx,plength
        jl keyboardwcnext
        ;mark1
        mov ah,09h
        mov dx,offset msg4
        int 21h 
        mov ah,3eh
        mov bx,ahandle
        int 21h
        jmp keyboardwc
keyboardwcnext:
		mov ah,1
		int 16h
        jz pre                  ;no key press,loop
		mov ah,07h
		int 21h						 ;clear input buffer
		cmp al,'r'
		jz wcrecord
		cmp al,'p'
		jz wcp
		cmp al,27
		jz wcexit
pre:        jmp keyboardwc
wcp:
		call wcplay
		jmp keyboardwc
wcrecord:
		;mov ah,09h
		;mov dx,offset wcmsg1
		;int 21h  ;print wcmsg1
		mov recordsucc, 1
		mov record_delaynum, 255
		call srecord
        ;cmp recordsucc, 0
        ;jz keyboardwc        ;mark!!!!
		call sendfile
               ; jmp keyboardwc
wcexit:
		cmp opensuc,1
		jnz wcexitnext
        mov bx,ahandle
		mov ah,3eh					;close file
		int 21h
wcexitnext:
		cli
		in al,21h
		or al,40h
		out 21h,al
		mov dx,0ec4ch
		mov al,42h
		out dx,al
		mov dx,oldoff
		mov ds,oldseg
		mov ax,250eh
		int 21h
		sti		       ;recover interrupt vec
		mov ah,4ch
		int 21h

;以下是对讲模式
ipmode:
		mov ax,data
		mov ds,ax
		mov al,1eh     ;8253 counter 0, method 3
		mov dx,0e483h
		out dx,al
        mov al,4          ;15625bps*16
		mov dx,0e480h     ;8253 counter0, clk for 8251
		out dx,al

		mov dx,0e4b9h   ;8251
		mov al,0
		out dx,al
		out dx,al
		out dx,al
		mov al,40h
		out dx,al
		mov al,4eh      ;word length:8, asynchronous
		out dx,al
		mov al,27h
		out dx,al

		cli
		mov ax,350eh   ;save old interrupt vector
		int 21h
		mov oldseg,es
		mov oldoff,bx

		mov ax,code    ;set pci interrupt
		mov ds,ax
		mov dx,offset irqip
		mov ax,250eh
		int 21h

		mov dx,0ec4ch
		mov al,43h
		out dx,al
		inc dx
		mov al,1dh
		out dx,al
		in al,21h
		and al,10111111b
		out 21h,al
		mov ax,data
		mov ds,ax
		sti
		mov ah,09h
		mov dx,offset ipmsg0
		int 21h  ;print ipmsg0

keyboard2:
		mov ah,1
		int 16h
		jz keyboard2 				  ;no key press,loop
		mov ah,07h
		int 21h						 ;clear input buffer
		cmp al,'r'
		jz iprecord
		cmp al,27
		jz ipexit
		jmp keyboard2

iprecord:
		mov ah,09h
		mov dx,offset ipmsg1
		int 21h  ;print ipmsg1
ipr:
		mov dx,0e49ah				  ;adc,in-2,microphone
		out dx,al
		in al,dx
		mov sendbyte,al
swait:
        mov dx,0e4b9h
        in al,dx
        test al,1
        jnz sendok
        mov ah,1
        int 16h
        jz swait
        cmp al,27
        jz ipexit
        jmp swait
sendok:
		mov al,sendbyte
		mov dx,0e4b8h
		out dx,al
        mov delaynum,255               ;15625Hz
		call delay
        call delay
        mov ah,1
		int 16h
		jz ipr						     ;no key press,continue
		mov ah, 7
		int 21h
		cmp al,'e'
		jz iprecordexit						;press e,exit
		cmp al,27
		jz ipexit
		jmp ipr
iprecordexit:
		mov dx,0e4b9h
		in al,dx
		test al,1
		jz iprecordexit
		mov al,0
		mov dx,0e4b8h
		out dx,al
		jmp ipmode
ipexit:
		cli
		in al,21h
		or al,40h
		out 21h,al
		mov dx,0ec4ch
		mov al,42h
		out dx,al
		mov dx,oldoff
		mov ds,oldseg
		mov ax,250eh
		int 21h
		sti		       ;recover interrupt vec
		mov ah,4ch
		int 21h

srecord proc near
		push ax
		push bx
		push cx
		push dx
		push si
		mov ah,3ch
		mov dx,offset rfilename
		xor cx,cx
		int 21h						;create file
		jnc createsuccess
err0:						;create error
		mov ah,09h
		mov dx,offset msgerr0
		int 21h
		mov recordsucc,0
		jmp singlemode
createsuccess:
		mov rhandle,ax				  ;file rhandle
		mov ah,09h
		mov dx,offset msgr
		int 21h
		mov rbc,0
		mov rcount,0
		mov rrest,0

recording:
		mov dx,0e49ah				  ;adc,in-2,microphone
		out dx,al
        mov al,record_delaynum
        mov delaynum,al
        call delay
		call delay
		in al,dx
		mov si,rbc
		mov recordbuf[si],al
		inc rbc
		cmp rbc,1024
		jl writesuccess
		inc rcount
		mov rbc,0     				  ;buffer overflow,write to file
		mov bx,rhandle
		mov cx,1024
		mov dx,offset recordbuf
		mov ah,40h
		int 21h    				     ;write into file
		jnc writesuccess
err1:						;write error
		mov ah,09h
		mov dx,offset msgerr1
		int 21h
		mov recordsucc,0
		jmp singlemode
writesuccess:
		mov ah,1
		int 16h
		jz recording						     ;no key press,continue
		mov ah,7
		int 21h
		cmp al,'e'
		jz recordexit						  ;press e,exit
		jmp recording

recordexit:
		cmp rbc,0
				jz recordrec
		mov bx,rhandle
		mov cx,rbc
		mov dx,offset recordbuf
		mov ah,40h
		int 21h						 ;write into file
		jc err1
		mov bx,rhandle
		mov ah,3eh					;close file
		int 21h
		mov cx,rbc
		mov rrest,cx
		
recordrec:

	    pop si
	    pop dx
	    pop cx
	    pop bx
	    pop ax
	    ret
srecord endp

wcplay proc near
		push ax
		push bx
		push cx
		push dx
		push si

		cmp newmsgrcvd,0
        ;jz pret

        mov dx,offset pfilename
		mov al,0
		mov ah,3dh
		int 21h
        jnc wcp_opensuccess
wcp_err2:                           ;open error
		mov ah,09h
		mov dx,offset msgerr2
		int 21h
        jmp wcp_rec
wcp_opensuccess:
		mov phandle,ax
        ;;;
        mov bx,ax
        mov cx,0
        mov dx,0
        mov al,0
        mov ah,42h
        int 21h

		mov ah,09h
		mov dx,offset msgp
		int 21h

		mov pcount,0
		mov pbc,0
wcp_readplayfile:
		mov ah,3fh
		mov dx,offset playbuf
		mov bx,phandle
		mov cx,1024
		int 21h				     ;read 1024 bytes
        jnc wcp_playing
wcp_err3:                           ;read error
		mov ah,09h
		mov dx,offset msgerr3
		int 21h
        jmp wcp_rec

wcp_playing:
        cmp pcount,0
        jnz normal
        mov al,playbuf[0]
        mov ah,playbuf[1]
        mov plength,ax
normal:
		mov si,pbc
		mov al,playbuf[si]
		mov dx,0e490h 				;dac
		out dx,al
        mov cl,255
        mov delaynum,cl
        call delay
		call delay
		inc pbc
		cmp pbc,1024
        jl wcp_playing
		mov pbc,0
		inc pcount
		mov cx,pcount
        ;inc cx
		cmp cx,plength
        jb wcp_readplayfile
        ;mark2
wcp_rec:
		mov bx,phandle
		mov ah,3eh					;close file
		int 21h
		pop si
	    pop dx
	    pop cx
	    pop bx
	    pop ax
		ret

wcplay endp


play proc near
		push ax
		push bx
		push cx
		push dx
		push si


		mov dx,playfilename
		mov al,0
		mov ah,3dh
		int 21h
		jnc opensuccess
err2:						;open error
		mov ah,09h
		mov dx,offset msgerr2
		int 21h
				jmp playrec
opensuccess:
		mov phandle,ax
		mov ah,09h
		mov dx,offset msgp
		int 21h

		mov pcount,0
		cmp plength,0
        jz restplaying
		mov pbc,0
readplayfile:
		mov ah,3fh
		mov dx,offset playbuf
		mov bx,phandle
		mov cx,1024
		int 21h				     ;read 1024 bytes
		jnc playing
err3:						;read error
		mov ah,09h
		mov dx,offset msgerr3
		int 21h
				jmp playrec

playing:
		mov si,pbc
		mov al,playbuf[si]
		mov dx,0e490h 				;dac
		out dx,al
        mov cl,play_delaynum
        mov delaynum,cl
        call delay
		call delay
		inc pbc
		cmp pbc,1024
		jl playing
		mov pbc,0
		inc pcount
		mov cx,pcount
		cmp cx,plength
        jb readplayfile

restplaying:
		mov pbc,0
	    mov cx,prest		      ;rest bytes
		mov ah,3fh
		mov dx,offset playbuf
		mov bx,phandle
		int 21h				     ;read 1024 bytes
		jc err3
rp:
		mov si,pbc
		mov al,playbuf[si]
		mov dx,0e490h 				;dac
		out dx,al
		mov delaynum,100
		call delay
		inc pbc
		cmp pbc,cx
		jl rp
playrec:
		mov bx,phandle
		mov ah,3eh					;close file
		int 21h
		pop si
	    pop dx
	    pop cx
	    pop bx
	    pop ax
		ret
play endp

sendfile proc near
		push ax
		push bx
		push cx
		push dx
		push si

		mov dx,offset rfilename
		mov al,0
		mov ah,3dh
		int 21h
		jnc sopensuccess
serr2:						;open error
		mov ah,09h
		mov dx,offset msgerr2
		int 21h
		jmp sendrec
sopensuccess:
		mov phandle,ax

		mov scount,0
		mov sbc,0

		mov headcount, 0
sendhead:
swaithead:
		mov dx,0e4b9h
        in al,dx
        test al,1
        jnz sendokwchead
        mov ah,1
        int 16h
        jz swaithead
        cmp al,27
        jnz swaithead
        jmp sendexit
sendokwchead:
		mov ax,rcount
		cmp headcount, 1
		jl lower
		mov al, ah ;higher bits
lower:
		mov dx,0e4b8h
		out dx,al
		inc headcount
		cmp headcount, 2
		jl sendhead
		
		mov ah,09h
        mov dx,offset  msgs
        int 21h

readsendfile:
		mov ah,3fh
		mov dx,offset playbuf
		mov bx,phandle
		mov cx,1024
		int 21h				     ;read 1024 bytes
		jnc sending
serr3:						;read error
		mov ah,09h
		mov dx,offset msgerr3
		int 21h
		jmp sendrec

sending:
        ;mov ah,09h
        ;mov dx,offset msgs
        ;int 21h

        mov si,sbc
swaitwc:
        mov dx,0e4b9h
        in al,dx
        test al,1
        jnz sendready
        mov ah,1
        int 16h
        jz swaitwc
        cmp al,27
        jz sendexit
        jmp swaitwc
sendready:
		mov al,playbuf[si]
		mov dx,0e4b8h
		out dx,al

		mov ah,1
		int 16h
		jz sendokwc						     ;no key press,continue
		mov ah, 7
		int 21h
		cmp al,27
		jz sendexit
		
sendokwc:
		inc sbc
		cmp sbc,1024
		jl sending
		mov sbc,0
		inc scount
		mov cx,scount
		cmp cx,rcount
		jl readsendfile
        mov dx,offset msgsendsucc
        mov ah,09h
        int 21h

		
sendexit:
sendrec:
		mov bx,phandle
		mov ah,3eh					;close file
		int 21h
		pop si
	    pop dx
	    pop cx
	    pop bx
	    pop ax
		ret
sendfile endp




delay proc near
		push ax
		push dx
		mov dx,0e483h       ;8253 counter1, clk0:1mhz,out0:10khz, method 0
	    mov al,50h
	    out dx,al
	    mov dx,0e48bh       ;8255a-port a input
	    mov al,9bh
	    out dx,al
	    mov al,delaynum		  ;1mhz/10khz
	    mov dx,0e481h
	    out dx,al
nnn:
	    mov dx,0e488h       ;8255a-port a
	    in al,dx
	    and al,1
	    cmp al,0
	    je nnn
	    pop dx
	    pop ax
	    ret
delay endp

irqip proc far
		push ax
		push dx
		push cx
		push ds
		push si
		mov ax,data
		mov ds,ax
		mov dx,0e4b8h
		in al,dx
		mov dx,0e490h 				;dac
		out dx,al

		mov al,20h
		out 20h,al
		mov dx,0ec4dh
		mov al,1dh
		out dx,al
		pop si
		pop ds
		pop cx
		pop dx
		pop ax
		iret
irqip  endp

irqwc proc far
		push ax
		push bx
		push dx
		push cx
		push ds
		push si
		mov ax,data
		mov ds,ax
		mov dx,0e4b8h
		in al,dx
        mov intbuf,al
        mov intbufempty,0

		mov al,20h
		out 20h,al
		mov dx,0ec4dh
		mov al,1dh
		out dx,al
		pop si
		pop ds
		pop cx
		pop dx
		pop bx
		pop ax
		iret
irqwc endp
code ends
end start
