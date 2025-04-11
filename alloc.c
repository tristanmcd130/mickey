struct block {
	bool in_use;
	int size;
	struct block *prev;
	int data[size]; // the data is actually stored inside the struct, it's not a pointer to somewhere else
};

struct block *last_block = NULL;

int *alloc(int words) {
	struct block *block = last_block;
	while(!(block == NULL || (!block->in_use && block->size >= words)))
		block = block->prev;
	if(block == NULL) {
		struct block new_block;
		new_block.in_use = true;
		new_block.size = words;
		new_block.prev = last_block;
		last_block = new_block;
		return new_block.data;
	}
	block->in_use = true;
	return block.data;
}

void free(int *ptr) {
	ptr[-3] = false; // in other words, set the associated block's in_use to false
}

/*
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
; block->size >= words
; => block->size - words >= 0
subl 2
jneg allocUpdateBlock:
loco 1
push
lodl 1
popi
pop
addd c2:
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
addd c2:
insp 1
retn

free:
loco 0
push ; c1 in 0, retn addr in 1, ptr in 2
lodl 2
subd c3:
popi
retn

lastBlock: heap:

heap:
0
0
0
*/