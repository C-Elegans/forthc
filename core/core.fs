: + {l [
    Pop (R 1),
    Pop (R 0),
    Op Add (R 0) (R 0) (R 1),
    PushR (R 1)
    ]
    } ;
    
: - {l [
    Pop (R 1),
    Pop (R 0),
    Op Sub (R 0) (R 0) (R 1),
    PushR (R 1)
    ]
    } ;
: and {l [
    Pop (R 1),
    Pop (R 0),
    Op And (R 0) (R 0) (R 1),
    PushR (R 1)
    ]
    } ;
: or {l [
    Pop (R 1),
    Pop (R 0),
    Op Or (R 0) (R 0) (R 1),
    PushR (R 1)
    ]
    } ;
: xor {l [
    Pop (R 1),
    Pop (R 0),
    Op Xor (R 0) (R 0) (R 1),
    PushR (R 1)
    ]
    } ;
: /
  {
  pushlr
  pop r1, r6
  pop r0, r6
  call _div
  push r0, r6
  pop r0
  jmp r0} ;
: .
  {
  pop r0, r6
  jmp _print_hex
  }
  ;

: < {
    pop r1, r6
    pop r0, r6
    cmp r0, r1
    mov r0, 0
    jmp.ge 1f
    not r0
    1:
    push r0, r6
    ret } ;

: > {
    pop r1, r6
    pop r0, r6
    cmp r0, r1
    mov r0, 0
    jmp.le 1f
    not r0
    1:
    push r0, r6
    ret } ;
: = {
    pop r1, r6
    pop r0, r6
    cmp r0, r1
    mov r0, 0
    jmp.ne 1f
    not r0
    1:
    push r0, r6
    ret }
;
: <> = invert ;
: invert {
      pop r0, r6
      not r0
      push r0, r6
      ret } ;
: >= < invert ;
: <= > invert ;

: nip {
      pop r0, r6
      pop r1, r6
      push r0, r6
      ret } ;

: rot {
      pop r0, r6
      pop r1, r6
      pop r2, r6
      push r1, r6
      push r0, r6
      push r2, r6
      ret } ;

: over {
       ld r0, [r6+2]
       push r0, r6
       ret
       } ;

: tuck {
       pop r0, r6
       pop r1, r6
       push r0, r6
       push r1, r6
       push r0, r6
       ret } ;

: swap {l [
       Pop (R 0),
       Pop (R 1),
       Push (R 0),
       Push (R 1)
} ;

: >r {
     pop r0, r6
     push r0
     ret } ;

: r> {
     pop r0
     push r0, r6
     ret } ;

: delay-ms {
	   pop r0, r6
	   st [0xff06], r0
	   1:
	   ld r0, [0xff06]
	   test r0, r0
	   jmp.ne 1b
	   ret
	   } ;
