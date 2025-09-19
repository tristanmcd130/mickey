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