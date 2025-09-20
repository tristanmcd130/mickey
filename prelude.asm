call main:
halt

fp: 0

tmp: 0

getlocal: ; offset in ac
addd fp:
pshi
pop
retn

setlocal: ; value on stack, offset in ac
addd fp:
insp 1
popi
desp 2
retn

add:
lodl 1
addl 2
retn

sub:
lodl 1
subl 2
retn

neg:
loco 0
subl 1
retn

c1: 1

mul:
desp 1
loco 0
stol 0
mulCond:
lodl 2
jzer mulEnd:
subd c1:
stol 2
lodl 0
addl 3
stol 0
jump mulCond:
mulEnd:
lodl 0
insp 1
retn

eq:
lodl 1
subl 2
jzer eqTrue:
loco 0
retn
eqTrue:
loco 1
retn

ne:
lodl 1
subl 2
jnze neTrue:
loco 0
retn
neTrue:
loco 1
retn

ge: ; x - y >= 0 => x >= y
lodl 1
subl 2
jpos geTrue:
loco 0
retn
geTrue:
loco 1
retn

lt: ; x - y < 0 => x < y
lodl 1
subl 2
jneg ltTrue:
loco 0
retn
ltTrue:
loco 1
retn

gt: ; y < x => x > y
lodl 1
push
lodl 3
push
call lt:
insp 2
retn

le: ; y >= x => x <= y
lodl 1
push
lodl 3
push
call ge:
insp 2
retn

and:
lodl 1
jzer andFalse:
lodl 2
andFalse:
retn

or:
lodl 1
jnze orTrue:
lodl 2
orTrue:
retn

not:
loco 1
subl 1
retn