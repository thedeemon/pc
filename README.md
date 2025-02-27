## Precise Calculator (console one).

When possible performs calculations using unlimited precision rational numbers.
Otherwise falls back to 80-bit reals. Supports variables and some functions.

Examples:

    6 + 6*6 - 23.1 ** (2/3)
    x = 0xABCD ^ (21 & 31) | 0x80
    hex x   (shows result in hex)
    bin 42  (shows in binary)
    y = exp (1/x) + sin x
    z = 2 ** it - y + ln 0.2   ('it' is a name for last result)
    factors 111111
    vars  (shows names of all defined variables so far)

    operators: +, -, *, /, % (mod), ^ (xor), & (and), | (or), ** (power)
    functions: bin hex ln factors sin cos tan asin acos atan exp
    :f or :full - show many digits of reals
    :s or :short - show shorter version of reals

### Technical details
Written in D language. Uses Pegged for parsing:
<https://github.com/PhilippeSigaud/Pegged/>

### Building
Using DUB: get the files, cd to the project folder, 
run: `dub build --build=release`

It will get the dependecies automatically and build the target executable.

### License
Copyright (C) 2013 Dmitry Popov, Infognition Co. Ltd.

Permission is hereby granted, free of charge, to any person obtaining a copy 
of this software and associated documentation files (the "Software"), to deal 
in the Software without restriction, including without limitation the rights 
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
copies of the Software, and to permit persons to whom the Software is furnished
to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all 
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS 
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR 
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER 
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN 
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

