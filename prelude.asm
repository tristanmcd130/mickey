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

mul:
desp 1
loco 0
stol 0
mul_cond:
lodl 2
jzer mul_end
subd c1
stol 2
lodl 0
addl 3
stol 0
jump mul_cond
mul_end:
lodl 0
insp 1
retn

div:
desp 1
loco 0
stol 0
div_cond:
lodl 2
subl 3
jneg div_end
stol 2
lodl 0
addd c1
stol 0
jump div_cond
div_end:
lodl 0
insp 1
retn

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