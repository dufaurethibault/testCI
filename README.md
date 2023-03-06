# Marcel (Glados)

## How to build Marcel:

### Requirements:

- ghc
- cabal

### Building:

```sh
cabal build && mv dist/build/marcel/marcel .
```

## Introduction to the language:

For testing purposes here is a sample hello world program:
```ts
function int main = [] begin
    print("Hello World!\n");
    return 0;
end
```

You can run it with `marcel hello_world.marcel`

### Types:
- `int` - 64 bit integer
- `entier` - French alias type for `int`
- `float` - 64 bit floating point number
- `flottant` - French alias type for `float`
- `caracteres` - French alias type for `string`
- `char` - 8 bit standard character
- `caractere` - French alias type for `char`
- `string` - string (encoded by your system LOCALE) 
- `bool` - Boolean
- `any` - Any type value (like js variables)
- `tout` - French alias type for `any`

### Misc:
- `vrai` - French alias value for `true`
- `faux` - French alias value for `false`

### Functions:

Functions are defined with the `function` keyword.
The return type is specified after the function name.
The function name is followed by a list of arguments.
The arguments are specified as a list of `type name` pairs.
The function body is specified between `begin` and `end`.

```ts
function int add = [int a, int b] begin
    // code here
end
```

### Main:
The main is the entry of the program, it will fail to compile if it is not in the form.
```ts
function int main = [] begin

end
```
Or if the main is declared multiple times.

### Variables:

Variables are defined with a `type`.
The variable name is followed by an optional assignment.
The assignment is specified with the `=` operator.

```ts
type name = value;
```

### Control flow:

The language supports:
- `if` statements

```ts
if condition begin
    // do something
end
```

- `if`/`else` statements

```ts
if condition begin
    // do something
end else begin
    // do something else
end
```

- `if`/`elseif`/`else` statements

```ts

if condition begin
    // do something
end elseif condition2 begin
    // do something
end elseif condition3 begin
    // do something else
end else begin
    // do something else
end
```

And so on ...

- `while` loops

```ts
while condition begin
    // do something
end
```

- `for` loops

```ts
for int i = 0; i < 10; i = i + 1 begin
    // do something
end
```

- `break` statements
```ts
int i = 0;
while true begin
    i = i + 1;
    if i == 10 begin
        break;
    end
end
```

- `return` statements
```ts
function int add = [int a, int b] begin
    return a + b;
end
```

### Operators:

- `+` - Addition
- `-` - Subtraction
- `*` - Multiplication
- `/` - Division
- `%` - Modulo
- `==` - Equality
- `!=` - Inequality
- `>` - Greater than
- `<` - Less than
- `>=` - Greater than or equal to
- `<=` - Less than or equal to

### Comments:

Comments are like C-style long comments:
```ts
/*
All of this is commented
*/
```

### Function calls:

You can call functions this way:
```ts
func(arg1, arg2, arg3);
```

### Printing:

You can print to the console with the `print` function:
```ts
print("Hello World!\n");
print(5);
```

### Side-Effects:

Most of the side effects are occured by the left operand (Except for char and string types).

```ts
string s = "toto" + 5; // results in "toto5"
string s = 5 + "toto"; // results in "5toto"

3 + 0.5 // results in 4 (3.5 rounded) as Int
0.5 + 3 // results in 3.5 as Float

string s = 'a' + 'b' // results in "ab"
string s = "abc" + "def";
```

### Strings:

Strings are ASCII encoded with UNICODE the same way as Haskell handles it by
loading your LOCALE.

### IO:

The only IO function is the main when you are outside of the main data will be buffered.

```ts
// This will be buffered so nothing is printed when the function is processed
function int printing_a_lot = [] begin
    for int i = 0; i < 1000; i = i + 1 begin
        print("a");
    end
    return 0;
end

function int main = [] begin
    printing_a_lot(); // print will be done when the function will return
    return 0;
end
```

This means that every statement is the main will be IO compliant the rest will be buffered for TCO and RAM.

### Development:

The Interpreter works with a stack based system and an accumulator (register rax).

The Interpreter will use the accumulator to store the last value.

For example the expression:

```ts
3 + 2 * 5
```

Will produce thoses differents states for the accu:
```ts
5 -> 10 -> 13
```

The accumulator will also be used for keeping track of return values.

The stack will be used to track variables and functions calls and function arguments.

Every time a function is called a new stack frame is pushed with the arguments.

This stack will also generate a specific variable to know when to return and when to break a control flow block.

```hs
data VM = VM {
    _vm_stack :: [VMStack],
    _vm_accu :: Value,
    _returning :: [Bool],
    _break :: [Bool],
    _vm_funcs :: [Function],
    _vm_buffer :: String
} deriving (Show)
```