call main:
halt

getlocal:
addd lp:
pshi
pop
retn

setlocal:
addd lp:
insp 1
popi
desp 2
retn

not:
lodl 1
jzer not0:
loco 0
retn
not0:
loco 1
retn

neg:
loco 0
subl 1
retn

or:
lodl 1
jzer or0:
retn
or0:
lodl 2
retn

and:
lodl 1
jzer and0:
lodl 2
retn
and0:
retn

lt:
lodl 1
subl 2
jneg lt0:
loco 0
retn
lt0:
loco 1
retn

le:
lodl 2
subl 1
jpos le0:
loco 0
retn
le0:
loco 1
retn

eq:
lodl 1
subl 2
jzer eq0:
loco 0
retn
eq0:
loco 1
retn

ne:
lodl 1
subl 2
jzer ne0:
loco 1
retn
ne0:
loco 0
retn

gt:
lodl 2
subl 1
jneg gt0:
loco 0
retn
gt0:
loco 1
retn

ge:
lodl 1
subl 2
jpos ge0:
loco 0
retn
ge0:
loco 1
retn

add:
lodl 1
addl 2
retn

sub:
lodl 1
subl 2
retn

mul:
loco 0
push ; args are now at 2 and 3
lodl 3
mul0:
jzer mul1:
lodl 0
addl 2
stol 0
lodl 3
subd c1:
stol 3
jump mul0:
mul1:
pop
retn

div:
loco 0
push
div0:
lodl 2
jneg div1:
subl 3
stol 2
lodl 0
addd c1:
stol 0
jump div0:
div1:
pop
subd c1:
retn

alloc:
lodd lastBlock:
push ; block in 0, retn addr in 1, words in 2
allocWhile:
jzer allocNull:
pshi
pop
jzer allocUnused:
allocUpdateBlock:
lodl 0
addd c2:
pshi
pop
stol 0
jump allocWhile:
allocUnused:
lodl 0
addd c1:
pshi
pop
subl 2
jneg allocUpdateBlock:
loco 1
push
lodl 1
popi
pop
addd c3:
retn
allocNull:
loco 1
push
lodd lastBlock:
addd c1:
pshi
pop
addd lastBlock:
addd c3:
popi
addd c1:
stod tmp:
lodl 2
push
lodd tmp:
popi
addd c1:
stod tmp:
lodd lastBlock:
push
lodd tmp:
popi
subd c2:
stod lastBlock:
addd c3:
insp 1
retn

free:
loco 0
push ; c0 in 0, retn addr in 1, ptr in 2
lodl 2
subd c3:
popi
retn

fp: 0
lp: 0
tmp: 0
c1: 1
c2: 2
c3: 3
lastBlock: heap: