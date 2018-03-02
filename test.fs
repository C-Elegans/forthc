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
: main 0
       9 testif
       12 testif
       30 testif
    ; 




