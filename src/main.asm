.model tiny
.386
.code
org 100h
LOCALS @@
start:
jmp main

draw_color equ 0CCh ; 22
bg_color equ 0BBh ; FF
on  db 0DBh, draw_color
off db 0DBh, bg_color

video_mode equ 1
line_width equ 80
max_x equ 40
max_y equ 25

d_x dw 0
d_y dw 0
sign_x dw 0
sign_y dw 0
pd_x dw 0
pd_y dw 0
dl_es dw 0
el dw 0
x dw 0
y dw 0
error dw 0

proc drawLine
    x1 equ word ptr [bp+10]
    y1 equ word ptr [bp+8]
    x2 equ word ptr [bp+6]
    y2 equ word ptr [bp+4]
    push bp
    mov bp, sp
    pusha


    mov ax, x2
    sub ax, x1
    mov d_x, ax
    mov ax, y2
    sub ax, y1
    mov d_y, ax


    mov sign_x, 0
    cmp d_x, 0
      jg @@greaterSignX
      jl @@lesserSignX
      jmp @@afterSignX
    @@greaterSignX:
      mov sign_x, 1
      jmp @@afterSignX
    @@lesserSignX:
      mov sign_x, -1
    @@afterSignX:

    mov sign_y, 0
    cmp d_y, 0
      jg @@greaterSignY
      jl @@lesserSignY
      jmp @@afterSignY
    @@greaterSignY:
      mov sign_y, 1
      jmp @@afterSignY
    @@lesserSignY:
      mov sign_y, -1
    @@afterSignY:


    cmp d_x, 0
    jge @@notLesserDX
      neg d_x
    @@notLesserDX:

    cmp d_y, 0
    jge @@notLesserDY
      neg d_y
    @@notLesserDY:


    mov ax, d_y
    cmp d_x, ax
    jl @@dyLesser
      mov ax, sign_x
      mov pd_x, ax

      mov pd_y, 0

      mov ax, d_y
      mov dl_es, ax

      mov ax, d_x
      mov el, ax

      jmp @@afterDyLesser
    @@dyLesser:
      mov pd_x, 0

      mov ax, sign_y
      mov pd_y, ax

      mov ax, d_x
      mov dl_es, ax

      mov ax, d_y
      mov el, ax
    @@afterDyLesser:


    mov ax, x1
    mov x, ax
    mov ax, y1
    mov y, ax


    mov ax, el
    shr ax, 1
    mov error, ax


    mov si, word ptr on

    mov ax, y
    mov bx, line_width
    mul bx
    mov bx, x
    sal bx, 1
    add bx, ax
    mov word ptr es:bx, si

    mov cx, 0
    @@again:
      mov ax, dl_es
      sub error, ax
      cmp error, 0
      jge @@errNotLesser
        mov ax, el
        add error, ax
        mov ax, sign_x
        add x, ax
        mov ax, sign_y
        add y, ax
      jmp @@afterErr
      @@errNotLesser:
        mov ax, pd_x
        add x, ax
        mov ax, pd_y
        add y, ax
      @@afterErr:

      mov ax, y
      mov bx, line_width
      mul bx
      mov bx, x
      sal bx, 1
      add bx, ax
      mov word ptr es:bx, si
    inc cx
    cmp cx, el
    jl @@again

    popa
    pop bp
    ret 8
endp drawLine

; origin_x dd 15.0
; origin_y dd 15.0
; origin_z dd 5.0
; lines_count dw 12
; lines dd 10.0, 10.0, 0.0, 10.0, 20.0, 0.0, \
;          10.0, 20.0, 0.0, 20.0, 20.0, 0.0, \
;          20.0, 20.0, 0.0, 20.0, 10.0, 0.0, \
;          20.0, 10.0, 0.0, 10.0, 10.0, 0.0, \
;          10.0, 10.0, 10.0, 10.0, 20.0, 10.0, \
;          10.0, 20.0, 10.0, 20.0, 20.0, 10.0, \
;          20.0, 20.0, 10.0, 20.0, 10.0, 10.0, \
;          20.0, 10.0, 10.0, 10.0, 10.0, 10.0, \
;          10.0, 10.0, 0.0, 10.0, 10.0, 10.0, \
;          10.0, 20.0, 0.0, 10.0, 20.0, 10.0, \
;          20.0, 20.0, 0.0, 20.0, 20.0, 10.0, \
;          20.0, 10.0, 0.0, 20.0, 10.0, 10.0,
; lines_int_cached dw 0
; lines_int dw 72 dup (?)

origin_x dd 12.5
origin_y dd 12.5
origin_z dd 4.0
lines_count dw 8
lines dd 5.0, 5.0, 0.0, 5.0, 20.0, 0.0, \
         5.0, 20.0, 0.0, 20.0, 20.0, 0.0, \
         20.0, 20.0, 0.0, 20.0, 5.0, 0.0, \
         20.0, 5.0, 0.0, 5.0, 5.0, 0.0, \
         5.0, 5.0, 0.0, 12.5, 12.5, 8.0, \
         5.0, 20.0, 0.0, 12.5, 12.5, 8.0, \
         20.0, 5.0, 0.0, 12.5, 12.5, 8.0, \
         20.0, 20.0, 0.0, 12.5, 12.5, 8.0
lines_int_cached dw 0
lines_int dw 48 dup (?)

proc turnX
    angle equ word ptr [bp+4]
    push bp
    mov bp, sp
    pusha

    mov lines_int_cached, 0

    mov cx, lines_count
    sal cx, 1
    mov si, offset lines
    finit

    push angle ; 15
    mov bx, sp
    fild word ptr [bx] ; 15
    add sp, 2

    fchs ; -15

    fldpi ; -15 pi
    fmulp ; -15*pi

    push 180
    mov bx, sp
    fild word ptr [bx] ; -15*pi 180
    add sp, 2

    fdivp ; -15*pi/180
    fsincos ; cos(-15*pi/180) sin(-15*pi/180)

    @@nextPoint:
    fld dword ptr [si]+8  ; raw_z cos(-15*pi/180) sin(-15*pi/180)
    fld origin_z          ; origin_z raw_z cos(-15*pi/180) sin(-15*pi/180)
    fsubp                 ; z cos(-15*pi/180) sin(-15*pi/180)
    fld dword ptr [si]+4  ; raw_y z cos(-15*pi/180) sin(-15*pi/180)
    fld origin_y          ; origin_y raw_y z cos(-15*pi/180) sin(-15*pi/180)
    fsubp                 ; y z cos(-15*pi/180) sin(-15*pi/180)
    fld st(1)             ; z y z cos(-15*pi/180) sin(-15*pi/180)
    fmul st(0), st(4)     ; z*sin(-15*pi/180) y z cos(-15*pi/180) sin(-15*pi/180)
    fld st(1)             ; y z*sin(-15*pi/180) y z cos(-15*pi/180) sin(-15*pi/180)
    fmul st(0), st(4)     ; y*cos(-15*pi/180) z*sin(-15*pi/180) y z cos(-15*pi/180) sin(-15*pi/180)
    faddp                 ; y*cos(15*pi/180)+z*sin(-15*pi/180) y z cos(-15*pi/180) sin(-15*pi/180)
    fld origin_y          ; origin_y y*cos(15*pi/180)+z*sin(-15*pi/180) y z cos(-15*pi/180) sin(-15*pi/180)
    faddp                 ; origin_y+y*cos(15*pi/180)+z*sin(-15*pi/180) y z cos(-15*pi/180) sin(-15*pi/180)
    fstp dword ptr [si]+4 ; y z cos(-15*pi/180) sin(-15*pi/180)
    ; -------------------
    fchs                  ; -y z cos(-15*pi/180) sin(-15*pi/180)
    fmul st(0), st(3)     ; -y*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    fld st(1)             ; z -y*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    fmul st(0), st(3)     ; z*cos(-15*pi/180) -y*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    faddp                 ; z*cos(-15*pi/180)-y*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    fld origin_z          ; origin_z z*cos(-15*pi/180)-y*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    faddp                 ; origin_z+z*cos(-15*pi/180)-y*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    fstp dword ptr [si]+8 ; z cos(-15*pi/180) sin(-15*pi/180)
    fstp st(0)            ; cos(-15*pi/180) sin(-15*pi/180)

    add si, 12
    dec cx
    cmp cx, 0
    jne @@nextPoint

    popa
    pop bp
    ret 2
endp turnX

proc turnY
    angle equ word ptr [bp+4]
    push bp
    mov bp, sp
    pusha

    mov lines_int_cached, 0

    mov cx, lines_count
    sal cx, 1
    mov si, offset lines
    finit

    push angle ; 15
    mov bx, sp
    fild word ptr [bx] ; 15
    add sp, 2

    fchs ; -15

    fldpi ; -15 pi
    fmulp ; -15*pi

    push 180
    mov bx, sp
    fild word ptr [bx] ; -15*pi 180
    add sp, 2

    fdivp ; -15*pi/180
    fsincos ; cos(-15*pi/180) sin(-15*pi/180)

    @@nextPoint:
    fld dword ptr [si]+8  ; raw_z cos(-15*pi/180) sin(-15*pi/180)
    fld origin_z          ; origin_z raw_z cos(-15*pi/180) sin(-15*pi/180)
    fsubp                 ; z cos(-15*pi/180) sin(-15*pi/180)
    fld dword ptr [si]    ; raw_x z cos(-15*pi/180) sin(-15*pi/180)
    fld origin_x          ; origin_x raw_x z cos(-15*pi/180) sin(-15*pi/180)
    fsubp                 ; x z cos(-15*pi/180) sin(-15*pi/180)
    fld st(1)             ; z x z cos(-15*pi/180) sin(-15*pi/180)
    fmul st(0), st(4)     ; z*sin(-15*pi/180) x z cos(-15*pi/180) sin(-15*pi/180)
    fld st(1)             ; x z*sin(-15*pi/180) x z cos(-15*pi/180) sin(-15*pi/180)
    fmul st(0), st(4)     ; x*cos(-15*pi/180) z*sin(-15*pi/180) x z cos(-15*pi/180) sin(-15*pi/180)
    faddp                 ; x*cos(15*pi/180)+z*sin(-15*pi/180) x z cos(-15*pi/180) sin(-15*pi/180)
    fld origin_x          ; origin_x x*cos(15*pi/180)+z*sin(-15*pi/180) x z cos(-15*pi/180) sin(-15*pi/180)
    faddp                 ; origin_x+x*cos(15*pi/180)+z*sin(-15*pi/180) x z cos(-15*pi/180) sin(-15*pi/180)
    fstp dword ptr [si]   ; x z cos(-15*pi/180) sin(-15*pi/180)
    ; -------------------
    fchs                  ; -x z cos(-15*pi/180) sin(-15*pi/180)
    fmul st(0), st(3)     ; -x*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    fld st(1)             ; z -x*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    fmul st(0), st(3)     ; z*cos(-15*pi/180) -x*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    faddp                 ; z*cos(-15*pi/180)-x*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    fld origin_z          ; origin_z z*cos(-15*pi/180)-x*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    faddp                 ; origin_z+z*cos(-15*pi/180)-x*sin(-15*pi/180) z cos(-15*pi/180) sin(-15*pi/180)
    fstp dword ptr [si]+8 ; z cos(-15*pi/180) sin(-15*pi/180)
    fstp st(0)            ; cos(-15*pi/180) sin(-15*pi/180)

    add si, 12
    dec cx
    cmp cx, 0
    jne @@nextPoint

    popa
    pop bp
    ret 2
endp turnY

proc turnZ
    angle equ word ptr [bp+4]
    push bp
    mov bp, sp
    pusha

    mov lines_int_cached, 0

    mov cx, lines_count
    sal cx, 1
    mov si, offset lines
    finit

    push angle ; 15
    mov bx, sp
    fild word ptr [bx] ; 15
    add sp, 2

    fldpi ; 15 pi
    fmulp ; 15*pi

    push 180
    mov bx, sp
    fild word ptr [bx] ; 15*pi 180
    add sp, 2

    fdivp ; 15*pi/180
    fsincos ; cos(15*pi/180) sin(15*pi/180)

    @@nextPoint:
    fld dword ptr [si]+4  ; raw_y cos(15*pi/180) sin(15*pi/180)
    fld origin_y          ; origin_y raw_y cos(15*pi/180) sin(15*pi/180)
    fsubp                 ; y cos(15*pi/180) sin(15*pi/180)
    fld dword ptr [si]    ; raw_x y cos(15*pi/180) sin(15*pi/180)
    fld origin_x          ; origin_x raw_x y cos(15*pi/180) sin(15*pi/180)
    fsubp                 ; x y cos(15*pi/180) sin(15*pi/180)
    fld st(1)             ; y x y cos(15*pi/180) sin(15*pi/180)
    fmul st(0), st(4)     ; y*sin(15*pi/180) x y cos(15*pi/180) sin(15*pi/180)
    fld st(1)             ; x y*sin(15*pi/180) x y cos(15*pi/180) sin(15*pi/180)
    fmul st(0), st(4)     ; x*cos(15*pi/180) y*sin(15*pi/180) x y cos(15*pi/180) sin(15*pi/180)
    faddp                 ; x*cos(15*pi/180)+y*sin(15*pi/180) x y cos(15*pi/180) sin(15*pi/180)
    fld origin_x          ; origin_x x*cos(15*pi/180)+y*sin(15*pi/180) x y cos(15*pi/180) sin(15*pi/180)
    faddp                 ; origin_x+x*cos(15*pi/180)+y*sin(15*pi/180) x y cos(15*pi/180) sin(15*pi/180)
    fstp dword ptr [si]   ; x y cos(15*pi/180) sin(15*pi/180)
    ; -------------------
    fchs                  ; -x y cos(15*pi/180) sin(15*pi/180)
    fmul st(0), st(3)     ; -x*sin(15*pi/180) y cos(15*pi/180) sin(15*pi/180)
    fld st(1)             ; y -x*sin(15*pi/180) y cos(15*pi/180) sin(15*pi/180)
    fmul st(0), st(3)     ; y*cos(15*pi/180) -x*sin(15*pi/180) y cos(15*pi/180) sin(15*pi/180)
    faddp                 ; y*cos(15*pi/180)-x*sin(15*pi/180) y cos(15*pi/180) sin(15*pi/180)
    fld origin_y          ; origin_y y*cos(15*pi/180)-x*sin(15*pi/180) y cos(15*pi/180) sin(15*pi/180)
    faddp                 ; origin_y+y*cos(15*pi/180)-x*sin(15*pi/180) y cos(15*pi/180) sin(15*pi/180)
    fstp dword ptr [si]+4 ; y cos(15*pi/180) sin(15*pi/180)
    fstp st(0)            ; cos(15*pi/180) sin(15*pi/180)

    add si, 12
    dec cx
    cmp cx, 0
    jne @@nextPoint

    popa
    pop bp
    ret 2
endp turnZ

proc updateLines
    push bp
    mov bp, sp
    pusha

    finit

    mov ax, lines_count
    mov cx, 6
    mul cx
    mov cx, ax
    mov si, offset lines
    mov di, offset lines_int
    @@nextLine:
    fld dword ptr [si]
    fistp word ptr [di]

    add si, 4
    add di, 2
    dec cx
    cmp cx, 0
    jne @@nextLine

    mov lines_int_cached, 1

    popa
    pop bp
    ret
endp updateLines

proc redraw
    push bp
    mov bp, sp
    pusha

    cmp lines_int_cached, 1
    je @@afterCacheUpdate
    call updateLines
    @@afterCacheUpdate:

    ; clear screen
    mov ah, 06h
    mov al, 0
    mov bh, bg_color
    mov ch, 0
    mov cl, 0
    mov dh, max_y-1
    mov dl, max_x-1
    int 10h

    mov cx, lines_count
    mov bx, offset lines_int
    nextLine:
    push word ptr [bx]
    push word ptr [bx+2]
    push word ptr [bx+6]
    push word ptr [bx+8]
    call drawLine
    add bx, 12
    dec cx
    cmp cx, 0
    jne nextLine

    popa
    pop bp
    ret
endp redraw

stop_flag db 0
old_kb_handler dd 0

kb_handler proc far
    push ax
    push ds
    push es
    push cs
    pop ds

    in al, 60h
    cmp al, 01
    jne @@not_esc
    mov stop_flag, 1
    @@not_esc:

    cmp al, 1Eh
    jne @@not_a
    push 5
    call turnY
    call redraw
    @@not_a:

    cmp al, 20h
    jne @@not_d
    push -5
    call turnY
    call redraw
    @@not_d:

    cmp al, 11h
    jne @@not_w
    push 5
    call turnX
    call redraw
    @@not_w:

    cmp al, 1Fh
    jne @@not_s
    push -5
    call turnX
    call redraw
    @@not_s:

    cmp al, 10h
    jne @@not_q
    push 5
    call turnZ
    call redraw
    @@not_q:

    cmp al, 12h
    jne @@not_e
    push -5
    call turnZ
    call redraw
    @@not_e:

    ; eoi for kb
    in al, 61h
    or al, 80h
    out 61h, al

    ; eoi for pic
    mov al, 20h
    out 20h, al

    pop es
    pop ds
    pop ax

    iret
kb_handler endp

main:
    ; save old kb handler
    mov ah, 35h
    mov al, 09h
    int 21h
    mov word ptr old_kb_handler, bx
    mov word ptr old_kb_handler+2, es

    ; set new kb handler
    mov ah, 25h
    mov al, 09h
    lea dx, kb_handler
    int 21h

    ; switch video mode
    mov ah, 0
    mov al, video_mode
    int 10h

    ; disable blinking
    mov ax, 01003h
    mov bx, 0
    int 10h

    ; disable underscore
    mov ah, 1
    mov ch, 3Fh
    int 10h

    ; video memory pointer
    push 0B800h
    pop es

    call redraw

    waitForStop:
    cmp stop_flag, 0
    je waitForStop

  exit:
    ; restore old kb handler
    mov ah, 25h
    mov al, 09h
    mov dx, word ptr old_kb_handler
    mov ds, word ptr old_kb_handler+2
    int 21h

    mov ah, 0
    mov al, 3
    int 10h

    mov ah, 4Ch
    int 21h

end start
