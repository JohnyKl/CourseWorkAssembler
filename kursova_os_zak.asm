progdata segment para 
    inputFilename db "h:\file1.txt", 0 		
	copiedFileName db "h:\file2.txt", 0 
	processedFileName db "h:\file3.txt", 0
	;inputFilename db 128 dup (0)		
	;copiedFileName db 128 dup (0)		
	;processedFileName db 128 dup (0) 
	
	FILESIZE DW ?
	
	infilehandle   dw ?
	outfilehandleC dw ?   
	outfilehandleP dw ?
	                                  	
	buffer  db 100 dup (?)
	buffer1 db 100 dup (?)
	
	error_str db "Parametr Error", 24h
	CodeNewLine DB 13,10 	
			
progdata ends

progstack segment para stack 'stack'
          dw 256 dup (?)
progstack ends  

progcode segment para     
    
;---------------------------------------------------  
;Input a file name from a command line
;---------------------------------------------------
f_n	proc  far:
    	mov al, es:[si]
    	mov [bx], al
    	inc bx
    	inc si
    	mov al, es:[si]
    	cmp al, cl
    	jnz f_n
    	mov al,0
    	mov [bx],al
    	ret
f_n 	ENDP 
;------------------------------------------------------ 
;find a size of the file that will be copied
;---------------------------------------------------
find_file_size	proc  far:
        MOV	AH,42H	
        mov al,2
        MOV	BX,infilehandle	
        MOV	CX,0
        MOV	DX,0
        INT	21H	
        JC	d_error
        MOV	FILESIZE,AX	
        
        mov ah,42h
        MOV	AL,0 ;return pointer on file start
        MOV	CX,0
        MOV	DX,0
        INT	21H
    
        ret
find_file_size endp 
;------------------------------------------------------    
;read the file that will be copied
;------------------------------------------------------ 
read_file proc far:
        MOV	AH,3FH	
        MOV	BX,infilehandle	
        MOV	CX,FILESIZE
        LEA	DX,buffer	
        INT	21H
        JC	d_error
    
        ret
read_file endp

;------------------------------------------------------ 
;find identificators in the file
;------------------------------------------------------ 
find_identificators proc far: 
        push ds
        pop es
        
        mov bx,0h
        
        lea si,buffer
        lea di,buffer1 
        mov cx,1 
    
next_symbol:lodsb 
            
            cmp ax, 0h
            je return
            cmp ax, 30h
            jl not_identif
            cmp ax,3Ah
            jl identif
            cmp ax,41h
            jl not_identif
            cmp ax,5Bh    
            jl identif
            cmp ax,5Fh
            je identif
            cmp ax,60h
            je not_identif
            cmp ax,7Bh    
            jl identif  
            jae not_identif
not_identif:cmp bx,0h
            jnz next_symbol
            mov bx, 1h
            mov ax, ' '
            stosb
            jmp next_symbol             

identif:mov bx, 0h 
        stosb
        jmp next_symbol 
    
return: ret
find_identificators endp

;------------------------------------------------------ 

;------------------------------------------------------ 
write_in_file proc far: 
        lea di, buffer1
        lea si, buffer1
        
        push ds
        pop es
        
        mov cx,0ffffffffh   
        xor al,al
        repne scasb
        
        sub di,si
        dec di
        mov cx,di
        
        MOV	AH,40H 
        mov bx,outfilehandleP
    	LEA DX,buffer1
        INT	21H
    
        ret
write_in_file endp

;------------------------------------------------------
;copy a file                                                        
;------------------------------------------------------ 
copy_file proc far: 
        mov cx, FILESIZE
        
        MOV	AH,40H 
        mov bx,outfilehandleC
    	LEA DX,buffer
        INT	21H
    
        ret
copy_file endp

;------------------------------------------------------                                                        


begin proc far 
assume cs:progcode,ds:progdata,ss:progstack   
    
start:  push ds
        sub  ax,ax
        push ax

        mov ax,progdata
        mov ds,ax

;------------------------------------------------------- 

;open an input file
;-------------------------------------------------------
      	mov ah,3dh
    	mov al,0
    	lea dx, inputFilename
    	int 21h     
    	
    	jnc vper1
    	jmp d_error
vper1:	mov infilehandle,ax 

	    call find_file_size
;-------------------------------------------------------

;create an output copied file
;------------------------------------------------------
    	mov ah, 3ch
    	mov cx,0
     	mov dx, offset copiedFileName
    	int 21h
    	
    	jnc vper2
    	jmp d_error
vper2:	mov outfilehandleC,ax
;---------------------------------------------------- 


;create an output processed file
;------------------------------------------------------
    	mov ah, 3ch
    	mov cx,0
     	mov dx, offset processedFileName
    	int 21h 
    	
    	jnc vper3
    	jmp d_error
vper3:	mov outfilehandleP,ax
;----------------------------------------------------
;----------------------------------------------------- 
        mov dx, offset buffer
read_f: call read_file
        call copy_file        
        call find_identificators        
        call write_in_file
        
        mov buffer,0

;close files and exit
;---------------------------------------------------
exit:   mov ah,3eh
    	mov bx, infilehandle
    	int 21h
    	jc d_error
    	mov ah,3eh
    	mov bx, outfilehandleC
    	int 21h
    	jc d_error 
    	mov ah,3eh
    	mov bx, outfilehandleP
    	int 21h
    	jc d_error
    	iret
;---------------------------------------------------
;displaying an error
;---------------------------------------------------
d_error:mov ah,9
        lea dx, error_str
        int 21h
	    iret 
;---------------------------------------------------------------
   begin endp
progcode ends
         end start


