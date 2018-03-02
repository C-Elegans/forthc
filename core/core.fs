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
: not [
      pop r0, r6
      not r0
      push r0, r6
      ret |] ;
: >= < not ;
: <= > not ;

	
