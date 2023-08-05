# Zlick
Zlick is a toy programming language I made with the aim to learn more about how programming languages work under the hood. The implementation draws inspiration from [craftinginterpreters](https://craftinginterpreters.com/) book.

# zlick_vm
This repo contains a bytecode compiler and an virtual machine for the bytecode generated by the compiler.
There is also an interpreter implementation of Zlick in [this repo](https://github.com/thrombe/zlick).

# Examples
Zlick's loops are like C's loops, but they ditched the parentheses for a more laid-back coding experience. Who needs parentheses anyway? Loops also supports continue and break statements as an effort to avoid gotos.
```
for let n = 6; n>0; n = n-1; {
    if n < 3 and n>1 {
        continue;
    }
    print n;
}
```

### Object Oriented Orientation
Zlick is object oriented with support for classes and inheritance. Behold the might of Zlick's classes and their magical methods
```
class Box {
    // init method is executed when a new object of this class is created.
    init() {
        self.size = 2;
    }

    area() {
        return self.size * self.size;
    }
}

class Rect < Box {
    init() {
        self.size = 3;
    }
}

print Rect().area(); // 9.0
```

### Inbuilt functions
Zlick might not have a never-ending arsenal of inbuilt functions, but hey, it has two spectacular ones: `clock()` and `print()`. Use them wisely :).
```
let start = clock();
// some computation.
let end = clock();

print end - start;
```

### Closures
Zlick also supports closures for all your closure needs. Legends said closures were all you need to build your own classes.
```
fn Person(name, age) {
    let name = name;
    let age = age;

    fn get(pname) {
        if pname == "name" {
            return name;
        } else if pname == "age" {
            return age;
        }
    }

    fn set(pname, val) {
        if pname == "name" {
            name = val;
        } else if pname == "age" {
            age = val;
        }
    }

    fn _method(method) {
        if method == "get" {
            return get;
        } else if method == "set" {
            return set;
        }
    }

    return _method;
}

let p1 = Person("amongesh", 9);
let p2 = Person("sussika", 12);

print p1("get")("name"); // amongesh

print p2("get")("age"); // 12
p2("set")("age", 99);
print p2("get")("age"); // 99
```


### print fib(100000000000);
Implement your terribly efficient fabonacci function in zlick just like other languages.
```
fn fib(n) {
    if n <= 1 {
        return n;
    } else {
        return fib(n-2) + fib(n-1);
    }
}

print fib(8);
```
here is how the generated bytecode looks when pretty printed (using the -Dprint-bytecode flag).
```
----- fib -----
byte no. |    opcode    | args
   0       GetLocal        01 (1)
   2       Constant        00 (0) [1.0e+00]
   4       GreaterThan
   5       LogicalNot
   6       JmpIfFalse      07 00 (7)
   9       Pop
  10       GetLocal        01 (1)
  12       Return
  13       Jmp             15 00 (21)
  16       Pop
  17       GetGlobal       01 (1) [fib]
  19       GetLocal        01 (1)
  21       Constant        02 (2) [2.0e+00]
  23       Subtract
  24       Call            01 (1)
  26       GetGlobal       03 (3) [fib]
  28       GetLocal        01 (1)
  30       Constant        04 (4) [1.0e+00]
  32       Subtract
  33       Call            01 (1)
  35       Add
  36       Return
  37       ConstNone
  38       Return
----- <script> -----
byte no. |    opcode    | args
   0       Closure         [fn(0)] closure[local|index]()
   3       DefineGlobal    01 (1) [fib]
   5       GetGlobal       02 (2) [fib]
   7       Constant        03 (3) [8.0e+00]
   9       Call            01 (1)
  11       Print
  12       ConstNone
  13       Return
----- <code end> -----
```

# Zig version
compile using zig 10.1

# how to run
```sh
# run
$ zig build run -- <zlick file path>

# project specific options
$ zig build -h

```
