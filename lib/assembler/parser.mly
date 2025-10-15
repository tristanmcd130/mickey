%token COLON
%token LODD
%token STOD
%token ADDD
%token SUBD
%token JPOS
%token JZER
%token JUMP
%token LOCO
%token LODL
%token STOL
%token ADDL
%token SUBL
%token JNEG
%token JNZE
%token CALL
%token PSHI
%token POPI
%token PUSH
%token POP
%token RETN
%token SWAP
%token INSP
%token DESP
%token HALT
%token <string> LABEL
%token <int> INT
%token <string> STRING
%token <string> CHAR
%token EOF
%start <Common.Instruction.t list> program
%%

program: is = instruction*; EOF	{is}

instruction:
| LODD; a = arg		{ILodd a}
| STOD; a = arg		{IStod a}
| ADDD; a = arg		{IAddd a}
| SUBD; a = arg		{ISubd a}
| JPOS; a = arg		{IJpos a}
| JZER; a = arg		{IJzer a}
| JUMP; a = arg		{IJump a}
| LOCO; a = arg		{ILoco a}
| LODL; i = INT		{ILodl i}
| STOL; i = INT		{IStol i}
| ADDL; i = INT		{IAddl i}
| SUBL; i = INT		{ISubl i}
| JNEG; a = arg		{IJneg a}
| JNZE; a = arg		{IJnze a}
| CALL; a = arg		{ICall a}
| PSHI				{IPshi}
| POPI				{IPopi}
| PUSH				{IPush}
| POP				{IPop}
| RETN				{IRetn}
| SWAP				{ISwap}
| INSP; i = INT		{IInsp i}
| DESP; i = INT		{IDesp i}
| HALT				{IHalt}
| l = LABEL; COLON	{ILabel l}
| i = INT			{IInt i}
| s = STRING		{IString s}
| c = CHAR			{IChar c}
| l = LABEL			{ILodd (Label l)}

arg:
| i = INT	{Int i}
| l = LABEL	{Label l}
| c = CHAR	{Char c}