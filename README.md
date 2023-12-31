# ParserProject

LLM Prompts: 

1. For this assignment, you will use Racket, a functional programming language, to write a simple parser. Note that we’re writing only a parser, not a full interpreter (although interpreters in Racket aren’t that difficult). Also, your program only needs to pass a verdict on the syntactical correctness of the program, not produce a full parse tree. Your code should have a function called parse, which takes one parameter—the name of the file of source code to be processed. From the input line of the DrRacket environment: (parse "source1.txt") It should return a string: either “Accept”, indicating the program is syntactically correct; or a message as to which line the first syntax error was found. So output will be either: Accept or something like: Syntax error found on line 25 Coding considerations: • One of the main goals of this assignment is to give you some experience with functional programming. Thus, your program is expected to have most (70% or more) of functions return either a Maybe or Either type. (See the notes on Functional Programming for more information about these monadic types.) The project will be easier if you choose one or the other of those forms and use it consistently. Either is more flexible than Maybe, but slightly more complex to use. • Your source code should be in a single file, should read the input file from the default directory, and should not require/assume a particular structure of subdirectories. Here is the grammar: program -> linelist $$ linelist -> line linelist | epsilon line -> label stmt linetail label -> id: | epsilon linetail -> stmt+ | epsilon stmt -> id = expr; 	| if (boolean) stmt; 	| while (boolean) linelist endwhile; 	| read id; 	| write expr; 	| goto id; 	| gosub id; 	| return; 	| break; 	| end; boolean -> true | false | expr bool-op expr bool-op -> < | > | >= | <= | <> | = expr -> id etail | num etail | (expr) etail -> + expr | - expr | * expr | / expr | epsilon id -> [a-zA-Z][a-zA-Z0-9]* num -> numsign digit digit* numsign -> + | - | epsilon The regular-expression notation sometimes throws people off. An id is an alphabetic character followed by 0 or more alphanumeric characters. There is no maximum length for an id. A reserved word (if, while, goto, gosub, etc) may not be used as a statement label. gotohere: if (x > 5) y = y+1 is valid; goto: if (x > 5) y = y+1 is a syntax error. 
2. Will you write what is left
3. Can you do the parse-expr function
4. Okay here is my code so far, what's left to do? [code inserted]
5. I am getting this error: parser1.rkt:54:24: string-index: unbound identifier in: string-index location...: parser1.rkt:54:24 Here is my prompt: racket parser1.rkt source1.txt Here is my code: [code inserted]
6. I am getting this error: parser1.rkt:107:4: cond: bad syntax (`else' clause must be last) at: (else (Left "Syntax error in stmt")) in: (cond ((starts-with? str "if (") (let* ((bool-end (string-index str ")")) (bool-str (substring str 4 bool-end)) (bool-result (parse-boolean bool-str))) (if (is-Right bool-result) (parse-stmt (substring str (+ bool-end 1))) (Left "Syntax error in if stat... location...: parser1.rkt:107:4 context...: /Applications/Racket v8.10/collects/racket/private/cond.rkt:31:23: loop [repeats 3 more times] /Applications/Racket v8.10/collects/racket/private/cond.rkt:21:13: go Here is my code: [code inserted]
7. I am getting this error: parser1.rkt:150:23: butlast: unbound identifier in: butlast location...: parser1.rkt:150:23 Here is my code: [code inserted]
8. The output is not a line number. This is the output: Syntax error on line startLabel: x = 5;
9. Did all the grammar get covered
10. Can you do what is missing
11. Where should I add linetail

Screenshot of output with source file:
<img width="879" alt="Screenshot 2023-10-19 at 10 00 31 AM" src="https://github.com/ttaxzn/ParserProject/assets/71516184/0352649a-3bef-4d1c-a9d0-a458b9c1368a">


