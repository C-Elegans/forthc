: + [
    pop r1
    pop r0
    add r0, r1
    push r0
    ret
    |] ;
    
: - [
    pop r1
    pop r0
    sub r0, r1
    push r0
    ret
    |] ;
: and [
    pop r1
    pop r0
    and r0, r1
    push r0
    ret
    |] ;

: or [
    pop r1
    pop r0
    or r0, r1
    push r0
    ret
    |] ;

: xor [
    pop r1
    pop r0
    xor r0, r1
    push r0
    ret
    |] ;
: /
  [
  pushlr
  pop r1, r6
  pop r0, r6
  call _div
  push r0, r6
  pop r0
  jmp r0|] ;
: .
  [
  pop r0, r6
  jmp _print_hex
  |]
  ;

: < [
    pop r1, r6
    pop r0, r6
    cmp r0, r1
    mov r0, 0
    jmp.ge 1f
    not r0
    1:
    push r0, r6
    ret |] ;

: > [
    pop r1, r6
    pop r0, r6
    cmp r0, r1
    mov r0, 0
    jmp.le 1f
    not r0
    1:
    push r0, r6
    ret |] ;
: = [
    pop r1, r6
    pop r0, r6
    cmp r0, r1
    mov r0, 0
    jmp.ne 1f
    not r0
    1:
    push r0, r6
    ret |]
;
: <> = invert ;
: invert [
      pop r0, r6
      not r0
      push r0, r6
      ret |] ;
: >= < invert ;
: <= > invert ;

: nip [
      pop r0, r6
      pop r1, r6
      push r0, r6
      ret |] ;

: rot [
      pop r0, r6
      pop r1, r6
      pop r2, r6
      push r1, r6
      push r0, r6
      push r2, r6
      ret |] ;

: over [
       ld r0, [r6+2]
       push r0, r6
       ret
       |] ;

: tuck [
       pop r0, r6
       pop r1, r6
       push r0, r6
       push r1, r6
       push r0, r6
       ret |] ;

: swap [
       pop r0, r6
       pop r1, r6
       push r0, r6
       push r1, r6
       ret |] ;

: >r [
     pop r0, r6
     push r0
     ret |] ;

: r> [
     pop r0
     push r0, r6
     ret |] ;

: delay-ms [
	   pop r0, r6
	   st [0xff06], r0
	   1:
	   ld r0, [0xff06]
	   test r0, r0
	   jmp.ne 1b
	   ret
	   |] ;
