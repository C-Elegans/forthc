: main 1 1 + . ;

: test
    [ ld r0, [r6] |]
    [ push r0, r6 |]
    ;

: .
  [
  pop r0, r6
  call _print_hex

  |]
  ;
 
