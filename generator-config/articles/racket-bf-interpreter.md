### Brainfuck 

Brainfuck is one of the most known esoteric programming languages. 
It's syntax has only eight commands similar to Turing machine's ones:

- `>` : increment data pointer (move to the next memory cell);
- `<` : decrement data pointer (move to the previous memory cell);
- `+` : increment the data byte at the data pointer;
- `-` : decrement the data byte at the data pointer;
- `.` : output the data byte at the data pointer as ASCII character;
- `,` : input one byte and put it's ASCII code at the data pointer;
- `[` : if the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump  it forward to the command after the *matching* `]`command (square brackets are used for loop calculations) 
- `]` : if the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching `[` command.


A program on Brainfuck is a sequence of these commands. 'Hello world' example written on Brainfuck listed below.

```Brainfuck
// prints "Hello World" 
++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.

```

### Racket

I am really love functional programming languages in general and Lisp (and it's dialects such as OCaml, Scheme and Racket) in particular. So, my first Brainfuck interpreter will be written on Racket.

### Brainfuck interpreter

The first thing that is really important and needed to be described is how we can represent Brainfuck's (BF) memory.

I suggest to represent it as a dictionary with keys - memory addresses and values -  data at the address. So, memory `0 0 25 0 52 …` will be stored as `((2 . 25) (4 . 52))`. 

Also we have to store the data pointer. Data pointer is just a number that represent current position in memory that will be processed. So, the memory can be stored as a `(<data pointer> . (<memory dict>))`.

Let's define several simple util functions:

```
(define (mem-create mem-state cur-pointer) (list cur-pointer mem-state))
(define (mem-create-empty) (mem-create '() 0))
(define (mem-pointer mem) (car mem))
(define (mem-state mem) (cadr mem))
(define (mem-get mem) (dict-ref (mem-state mem) (mem-pointer mem) 0))
```

Now we can define the first BF's operations for `>` and `<` commands:

```scheme
(define (mem-pointer-op mem op) (mem-create (mem-state mem) (op (mem-pointer mem))))
(define (mem-pointer-inc mem) (mem-pointer-op mem inc-mem-pointer))
(define (mem-pointer-dec mem) (mem-pointer-op mem dec-mem-pointer))
```

Functions `inc-mem-pointer` and `dec-mem-pointer` implement increment and decrement operations in multiplicative group of integers modulo 65536 (memory size for our program = 256 bytes).

The functions for inc/decrement memory data (`+` and '-') are very simple too:

```scheme
(define (mem-value-op mem op)
  (let* ([_mem-state (mem-state mem)]
         [_mem-pointer (mem-pointer mem)]
         [cur (dict-ref _mem-state _mem-pointer 0)])
    (mem-create (dict-set _mem-state _mem-pointer (op cur)) _mem-pointer)))
(define (mem-value-inc mem) (mem-value-op mem inc-mem-value))
(define (mem-value-dec mem) (mem-value-op mem dec-mem-value))
```

Functions `inc-mem-value` and `dec-mem-value` implement increment and decrement operations in  multiplicative group of integers modulo 256 (memory cell in our program can contain only 8-bits values).

But I/O functions (`.` and `,`) are realy difficult. Actually, no:

```scheme
(define (mem-output mem) (integer->char (mem-get mem)))
(define (mem-input mem input)
  (let ([cur-pointer (mem-pointer mem)])
    (mem-create
     (dict-set (mem-state mem) cur-pointer (char->integer input))
     cur-pointer)))
```

Almost all command are implemented now. The only `[` and `]` left. These commands are used for jumps over another program commands. Only two jumps are possible: from `[` to the matching `]` and from `]` to the matching `[`. We can predefine jump table that will be contain possible jumps for the given program. 

```Scheme
(define (jump-table-create input)
  (define (iter i s stack res)
    (if (empty? s) res
        (let ([car-s (car s)]
              [cdr-s (cdr s)]
              [inc-i (inc i)])
          (cond
            [(eq? car-s #\[)
             (iter inc-i cdr-s (stack-push stack i) res)]
            [(eq? car-s #\]) 
             (let ([_stack-peek (stack-peek stack)])
               (iter inc-i cdr-s (stack-pop stack) (dict-set (dict-set res _stack-peek i) i _stack-peek)))]
            [else
             (iter inc-i cdr-s stack res)]))))
  (iter 0 (string->list input) '() '()))
```

So, now we have implemented all of the BF's commands and generator for the jump table. We can the given BF program string:

```Scheme
(define (bf-process-str str)
  (define (list-tail s k) (string->list (substring s k (string-length s))))
  (define (bf-tick mem j-table i input)
    (cond
      [(eq? input #\>) (list '() (mem-pointer-inc mem))]
      [(eq? input #\<) (list '() (mem-pointer-dec mem))]
      [(eq? input #\+) (list '() (mem-value-inc mem))]
      [(eq? input #\-) (list '() (mem-value-dec mem))]
      [(eq? input #\.) (list (mem-output mem) mem)]
      [(eq? input #\,) (list '() (mem-input mem))]
      [else (list '() mem)]))
  (let ([j-table (jump-table-create str)])
    (define (iter i s mem res)
      (if (empty? s) res
          (let ([car-s (car s)]
                [cdr-s (cdr s)]
                [inc-i (inc i)])
            (define (process-lb-or-rb cond)
              (let ([new-i (inc (dict-ref j-table i 0))])
                (if cond
                    (iter new-i (list-tail str new-i) mem res)
                    (iter inc-i cdr-s mem res))))
            (cond
              [(eq? car-s #\[) (process-lb-or-rb (eq? (mem-get mem) 0))]
              [(eq? car-s #\]) (process-lb-or-rb (not (eq? (mem-get mem) 0)))]
              [else
               (let* ([tick-res (bf-tick mem j-table i (car s))]
                      [new-mem (cadr tick-res)]
                      [output (car tick-res)])
                 (iter inc-i cdr-s new-mem (if (char? output) (append res (list output)) res)))]))))
    (list->string (iter 0 (string->list str) (mem-create-empty) '()))))
```

Now we can try to run this interpreter:

```scheme
> (bf-process-str "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")
"Hello World\n"
```

There are a couple of functions that can be refactored but I am so lazy for it.

**<u>[GitHub repository](https://github.com/wlad031/brainfuck-interpreter)</u>**.
