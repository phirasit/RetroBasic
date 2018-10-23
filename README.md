# RetroBasic

Retro Basic is a Microsoft Basic compiler written in haskell. This program will
convert B-code to Basic code which is easy to interprete.

# Installation

run ```make``` which will produce an executable file.
Note: Please make sure you have a working ghc.

# Detail Description of the task

Retro Basic

in the memory of Microsoft Basic (4K Basic)

A program to print 1 to 10

10 A = 1
20 IF 10 < A 60
30 PRINT A
40 A = A + 1
50 GOTO 20
60 STOP

A program to sum 1 to 10

10 A = 1
20 S = 0
30 IF 10 < A 70
40 S = S + A
50 A = A + 1
60 GOTO 30
70 PRINT S
80 STOP

A program in Retro Basic consists of lines.  Each line starts with line_number follows by a statement.  Statements are a) assignment b) if c) print d) goto e) stop.  An assignment is "id = exp" where id is {A..Z}.  An expression is binary op +/- between id and constant.
An if statement is "IF cond line_number", where cond is binary op {<,=} between id and constant.  A print statement is "PRINT id".  An goto statement is "GOTO line_num". A stop statement is "STOP".  A constant is {0..100}.

Here is Retro Basic grammar:

pgm := line pgm | EOF
line := line_num stmt
stmt := asgmnt | if | print | goto | stop
asgmnt := id = exp
exp := term | term + term | term - term
term := id | const
if := IF cond line_num
cond := term < term | term = term
print := PRINT id
goto := GOTO line_num
stop := STOP

id is {A..Z}
const is {1..100}
line_num is {1..1000}

Your job is to write a compiler to translate the source of Retro Basic to the "intermediate code".  Here is the intermediate code (B-code):

B-code is stored in an array of cells (32-bit) where the input source is tokenised (to help speed up the run-time interpreter).  Each token consists of two consecutive cells: type, value (a tuple). 

line_num    {#line, num}     num is {1..1000}
id          {#id, ref}       ref is index 1..26 corresponded to A..Z
const       {#const, value}  value is {1..100}
IF          {#if, 0}
GOTO        {#goto, num}     num is {1..1000}
PRINT       {#print, 0}
STOP        {#stop, 0}
+           {#op, 1}
-           {#op, 2}
<           {#op, 3}
=           {#op, 4}

Example of the translation of a line into B-code

10 A = 1
{#line, 10} {#id, A} {#op, 4} {#const, 1}

30 IF 10 < A 70
{#line, 30} {#if, 0} {#const, 10} {#op, 3} {#id, A} {#goto, 70}  

***  line_num became "goto"

Here is the coding of B-code type:

#line   10
#id     11
#const  12
#if     13
#goto   14
#print  15
#stop   16
#op     17

I will provide an B-code interpreter so that you can "run" your output to check that you correctly compile your source and output the correct B-code.  The input to B-code interpreter is a sequence of integer representing the B-code cell, with 0 represents the end of file.

For example the above two lines

10 A = 1
30 IF 10 < A 70
...

B-code

10 10 11 1 17 4 12 1
10 30 13 0 12 10 17 3 11 1 14 70
...
0

What you must submit:
1)  A report describes your compiler.  It consists of two parts
1.1   a scanner, how you scan the source and separate characters into token
1.2   a parser, how you check the sequence of token that it is correct according to the grammar. 
2)  Your pseudo code of the compiler
3)  You will get a bonus if you also submit an actual working code. (You can use any of your favourite programming language, give me the link to your working code which I can try, both source and executable)

Depend on your programming skill, this project can take a few days upto a week of programming if you want to produce an actual working code.  You can check your output B-code with my interpreter.  You will spend a lot less time if you just describe your compiler in pseudo code (or in plain narrative).  But it is more fun to have a code that is actually work!

Due date  Monday 5 November 2018, at 4pm.

last update 21 Oct 2018

