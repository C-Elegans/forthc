: testif
  dup
  10 > if
    dup
    dup
    20 > if
      .
    else
      drop
    then
    .
  else
    drop
  then
;
: main 
  0
  begin
    dup
    .
    1 +
    dup
    30 delay-ms
    256 >
  until
  
    ; 




