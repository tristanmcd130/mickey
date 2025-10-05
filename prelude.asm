loco 8
stod 4093
stod 4095
loco heap
stod heap_start
loco 3968
subd heap_start
subd c2
stod heap
call main
halt

heap_start: 0
fp: 0
tmp: 0
c1: 1
c2: 2

eq:
lodl 1
subl 2
jzer eq_true
loco 0
retn
eq_true:
loco 1
retn

ne:
lodl 1
subl 2
jnze ne_true
loco 0
retn
ne_true:
loco 1
retn

ge: ; x - y >= 0 => x >= y
lodl 1
subl 2
jpos ge_true
loco 0
retn
ge_true:
loco 1
retn

lt: ; x - y < 0 => x < y
lodl 1
subl 2
jneg lt_true
loco 0
retn
lt_true:
loco 1
retn

and:
lodl 1
jzer and_false
lodl 2
and_false:
retn

or:
lodl 1
jnze or_true
lodl 2
or_true:
retn