module main;
import std.stdio, std.range, std.algorithm, std.conv, std.typecons, std.bigint, std.exception, std.math, std.typetuple;
import pegged.grammar, std.string : strip, format;

mixin(grammar(`
  Arithmetic:
    Sentence < Assign / Expr
    Expr     < Factor (Add / Sub)*
    Add      < "+" Factor
    Sub      < "-" Factor
    Factor   < Multip (Mul / Div)*
    Mul      < "*" Multip
    Div      < "/" Multip
    Multip   < PrimaryN (BinOp PrimaryN)?
    BinOp    < "**" / "&" / "|" / "^" / "%"
    PrimaryN < Neg / Primary
    Primary  < Parens / HexNum / FloatNum / Number / FunCall / Variable
    Parens   < :"(" Expr :")"
    Neg      <- "-" Primary
    Number   < ~([0-9]+)
    FloatNum <- ~(Number "." Number)
    HexNum   <~ "0x" ([0-9A-Fa-f])+
    Variable <- identifier
    Assign   < identifier "=" Expr
    FunCall  < identifier Primary
`));

/* type exp = Int of BigInt | Div of exp * exp | Real of real */

interface Exp {
    real asReal();
    bool isInteger();
    BigInt asInt();
    string toString();
    string toStringPrecise();
    Exp mul(Exp x);
    Exp add(Exp x);
    Exp inPower(long n);
}

BigInt z0 = BigInt(0);
BigInt z1 = BigInt(1);

Exp match(alias intCase, alias divCase, alias realCase)(Exp x) 
{
    Int ix = cast(Int) x; if (ix) return intCase(ix);
    Div dx = cast(Div) x; if (dx) return divCase(dx);
    Real rx = cast(Real) x; if (rx) return realCase(rx);
    throw new Exception("unknown type in Exp match");
}

class Int : Exp {
    real asReal() { return to!real(toString()); }
    bool isInteger() { return true; }
    BigInt asInt() { return v; }
    Exp mul(Exp x)
    {
        return x.match!((Int ix) => new Int(v * ix.v),
                        (Div dx) => dx.mul(this),
                        (Real rx) => new Real(rx.asReal * this.asReal));
    }

    Exp add(Exp x)
    {
        return x.match!((Int ix) => new Int(v + ix.v), 
                        (Div dx) => dx.add(this),
                        (Real rx) => new Real(rx.asReal + this.asReal));
    }

    this(string s) { v = BigInt(s); }
    this(BigInt x) { v = x; }
    this(int x)    { v = BigInt(x); }
    this(long x)   { v = BigInt(x); }
    
    override string toString() { 
        string res;
        void f(const(char)[] s) { res ~= s; }
        v.toString(&f, "%d"); 
        return res;
    }

    string toStringPrecise() { return toString(); }

    Exp inPower(long n) 
    { 
        if (n==0) return new Int(1);
        if (n>0)  return new Int(v ^^ n); 
        else      return div(new Int(1), new Int(v ^^ abs(n)));
    }

protected:
    BigInt v;
}

enum FloatMode { Short, Full }
FloatMode fmode = FloatMode.Short;
string showReal(real v) { return fmode==FloatMode.Short ? v.to!string : format("%.25g", v); }


class Div : Exp {
    real asReal() 
    { 
        if (b.isInteger && b.asInt == z0) return 0.0; // x/0 = 0
        return a.asReal() / b.asReal(); 
    }

    bool isInteger() { return b.isInteger() && a.isInteger() && b.asInt() == z1; }

    BigInt asInt()  
    {
        if (b.isInteger() && b.asInt() == z1)
            return a.asInt();
        throw new Exception("Div.asInt called when it's not integer");
    }

    override string toString() 
    { 
        if (this.isInteger) return a.toString();
        return asReal.showReal;
    }

    string toStringPrecise() { return "(" ~ a.toStringPrecise ~ "/" ~ b.toStringPrecise ~ ")"; }

    Exp mul(Exp x)
    {
        return x.match!((Int ix) => div(a.mul(ix), b),
                        (Div dx) => div(a.mul(dx.a), b.mul(dx.b)),
                        (Real rx) => new Real(this.asReal * rx.asReal));
    }

    Exp add(Exp x)
    {
        return x.match!(
            (Int ix) => div(a.add(b.mul(x)), b),
            (Div dx) { 
                if (!dx.b.isInteger || !b.isInteger) 
                    throw new Exception("dunno how to add when denominator is not integer");
                if (dx.b.asInt == b.asInt) return div(a.add(dx.a), b);
                BigInt common = lcm(b.asInt, dx.b.asInt);
                BigInt ak = common / b.asInt;
                BigInt xk = common / dx.b.asInt;
                return div( a.mul(new Int(ak)).add( dx.a.mul(new Int(xk)) ), new Int(common));            
            },
            (Real rx) => new Real(this.asReal + rx.asReal)); 
    }


    this(Exp num, Exp den) { a = num; b = den; simplify(); }
    this(BigInt num, BigInt den) { a = new Int(num); b = new Int(den); simplify(); }
    this(int num, int den) { a = new Int(num); b = new Int(den); simplify(); }

    Exp inPower(long n) { return div(a.inPower(n), b.inPower(n));  }

protected:
    Exp a, b; // a/b -- num/den

    void simplify()
    {
        if (a.isInteger && a.asInt == z0) // 0 / x == 0/1
        {
            if (!b.isInteger || b.asInt != z1)
                b = new Int(1);
            return;
        }
        if (a.isInteger && a.asInt == z1 && !b.isInteger) { // 1 / (x/y) == y/x
            Div db = cast(Div) b;
            if (db) {
                a = db.b;
                b = db.a;
            }
        }
        fixSigns();        
        if (a.isInteger && b.isInteger) {
            BigInt num = abs(a.asInt);
            BigInt den = abs(b.asInt);

            auto divisor = gcd(num, den);
            if (divisor != z1) {
                a = new Int(a.asInt / divisor);
                b = new Int(den / divisor);
            }
        }
    }

    void fixSigns()
    {
        if (b.asReal < 0.0) {
            auto m1 = new Int(-1);
            b = b.mul(m1);
            a = a.mul(m1);
        }
    }
}


class Real : Exp
{
    real asReal() { return v; }
    bool isInteger() { return false; }
    BigInt asInt() { throw new Exception("Real.asInt"); }
    override string toString() { return v.showReal; }
    string toStringPrecise() { return toString(); }
    Exp mul(Exp x) 
    { 
        return x.match!((Int ix) => new Real(v * ix.asReal),
                        (Div dx) => new Real(v * dx.asReal),
                        (Real rx) => new Real(v * rx.v));
    }

    Exp add(Exp x)
    { 
        return x.match!((Int ix) => new Real(v + ix.asReal),
                        (Div dx) => new Real(v + dx.asReal),
                        (Real rx) => new Real(v + rx.v));
    }

    Exp inPower(long n) { return new Real(v ^^ n); }

    this(real x) { v = x; }
protected:
    real v;
}

BigInt gcd(BigInt a, BigInt b)
{
    enforce(a >= z0); 
    enforce(b >= z0);
    while (b) {
        auto t = b;
        b = a % b;
        a = t;
    }
    return a;
}

BigInt lcm(BigInt a, BigInt b) { return a * b / gcd(a,b); }

Exp div(Exp a, Exp b)
{
    if (cast(Real) a !is null || cast(Real) b !is null) return new Real(a.asReal / b.asReal);
    Div d = new Div(a,b);
    if (d.isInteger) return new Int(d.asInt);
    return d;
}

Exp mkExp(string s)
{
    auto dot = s.countUntil('.');
    if (dot < 0) return new Int(s);
    int nAfterDot = s.length - dot - 1;
    Exp num = new Int(s[0..dot] ~ s[dot+1..$]);
    Exp den = new Int(BigInt(10) ^^ nAfterDot);
    return new Div(num, den);
}

Exp binop(string op, Exp a, Exp b)
{
    if (op != "**") {
        if (!a.isInteger)
            throw new Exception("first arg of " ~op~ " must be integer");
        if (!b.isInteger) 
            throw new Exception("second arg of " ~op~ " must be integer");
    }
    switch(op) {
        case "**": return b.isInteger ? a.inPower(b.asInt.toLong) : new Real(a.asReal ^^ b.asReal);
        case "&":  return new Int(a.asInt.toLong & b.asInt.toLong);
        case "|":  return new Int(a.asInt.toLong | b.asInt.toLong);
        case "^":  return new Int(a.asInt.toLong ^ b.asInt.toLong);
        case "%":  return new Int(a.asInt % b.asInt);
        default: throw new Exception("unknown binary op " ~ op);
    }
}

Exp hex(Exp x)
{
    if (x.isInteger) {
        string res, sign = (x.asInt < 0) ? "-" : "";
        abs(x.asInt).toString((s) {res ~= s;}, "%X");                 
        writeln("hex(",x,") = ", sign, "0x", res);
    }
    return x;
}

string[] add01(string[] ss) { return chain(ss.map!(s => "0" ~ s), ss.map!(s => "1" ~ s)).array; }

Exp bin(Exp x)
{
    if (x.isInteger) {        
        string[] binStrings = ["0","1"].add01.add01.add01;    
        string[char] bins = ['_':""];
        foreach(i,c; "0123456789ABCDEF")
            bins[c] = binStrings[i];
        string res, sign = (x.asInt < 0) ? "-" : "";
        abs(x.asInt).toString((s) {res ~= s;}, "%X");                 
        write("bin(",x,") = ", sign);
        foreach(c; res) write(bins[c]);
        writeln();
    }
    return x;
}

Exp factors(Exp x)
{
    if (x.isInteger) {
        auto wheel = [4,2,4,2,4,6,2,6].map!(i => BigInt(i)).array.cycle;
        BigInt nxt() { BigInt t = wheel.front; wheel.popFront(); return t; }
        auto possible_primes = chain([BigInt(2),BigInt(3),BigInt(5)], recurrence!((a,n) => a.front + nxt())(BigInt(7)));
        auto X = x.asInt.abs;
        BigInt[] res;
        foreach(n; possible_primes.until!(x => x*x > X)) {
            while(X % n == z0) {
                res ~= n;
                X /= n;
            }
        }    
		if (X > z1) res ~= X;
        writeln("factors: ", res);
    }
    return x;
}

alias funNames = TypeTuple!("sin", "cos", "tan", "asin", "acos", "atan", "exp");

Exp funCall(string fn, Exp x)
{
    switch(fn) {
        foreach(s; funNames) 
            mixin("case \"" ~s~ "\" : return new Real(" ~s~ "(x.asReal));\n");
        case "ln" : return new Real(log(x.asReal));
        case "hex": return hex(x);
        case "bin": return bin(x);
        case "factors" : return factors(x);
        default: throw new Exception("unknown function " ~ fn);
    }
}

Exp[string] env;

Exp eval(ParseTree p)
{
    switch (p.name)
    {
        case "Arithmetic.Expr":
            Exp v = new Int(0);
            foreach(child; p.children) v = v.add(eval(child));
            return v;
        case "Arithmetic.Factor":
            Exp v = new Int(1);
            foreach(child; p.children) v = v.mul( eval(child) );
            return v;
        case "Arithmetic.Number":
        case "Arithmetic.FloatNum": return mkExp(p.matches[0]);
        case "Arithmetic.HexNum": return new Int(p.matches[0]); 
        case "Arithmetic.Sub": return eval(p.children[0]).mul(new Int(-1));
        case "Arithmetic.Div": return div(new Int(1), eval(p.children[0]));
        case "Arithmetic":     
        case "Arithmetic.Sentence": 
        case "Arithmetic.Primary":  
        case "Arithmetic.PrimaryN": 
        case "Arithmetic.Add": 
        case "Arithmetic.Mul": 
        case "Arithmetic.Parens":   return eval(p.children[0]);
        case "Arithmetic.Neg":      return eval(p.children[0]).mul(new Int(-1));
        case "Arithmetic.Assign": 
            Exp e = eval(p.children[0]);
            env[p.matches[0]] = e;
            return e;
        case "Arithmetic.Variable":
            auto var = p.matches[0];
            if (var == "vars") writeln("variables: ", env.keys); 
            else if (var in env) return env[var]; 
            else writeln("Unknown variable: ", var);
            return new Int(0);
        case "Arithmetic.Multip":
            if (p.children.length==1) return eval(p.children[0]); else
            if (p.children.length==3) return binop(p.children[1].matches[0], eval(p.children[0]), eval(p.children[2])); else {
                writeln(p);
                throw new Exception("bad number of args in Multip");                
            }
        case "Arithmetic.FunCall":    return funCall(p.matches[0], eval(p.children[0]));
        default:
            writeln(p);
            throw new Exception("unknown parse node");            
    }
}

void showHelp()
{
    writeln("Examples:");
    writeln("6 + 6*6 - 23.1 ** (2/3)");
    writeln("x = 0xABCD ^ (21 & 31) | 0x80");
    writeln("hex x   (shows result in hex)");
    writeln("bin 42  (shows in binary)");
    writeln("y = exp (1/x) + sin x");
    writeln("z = 2 ** it - y + ln 0.2   ('it' is a name for last result)");
    writeln("vars  (shows names of all defined variables so far)");
    writeln("operators: +, -, *, /, % (mod), ^ (xor), & (and), | (or), ** (power)");
    write("functions: bin hex factors ln ");
    foreach(fn; funNames) write(fn, " ");
    writeln("\n:f or :full - show many digits of reals");
    writeln(":s or :short - show shorter version of reals");
    writeln("Empty line to quit.");
}

void process(string line)
{
	if (line=="?") return showHelp();
	if (line[0..2] == ":f") { fmode = FloatMode.Full; return writeln("full mode set"); }
	if (line[0..2] == ":s") { fmode = FloatMode.Short; return writeln("short mode set"); }
	auto pt = Arithmetic(line);
	if (pt.successful) {
		try {
			auto e = eval(pt);
			if (e.isInteger) writeln("int: ", e);
			else if (cast(Real) e) writeln("real: ", e);
			else writeln("real: ", e.toString, " ratio: ", e.toStringPrecise);
			env["it"] = e;
		} catch (Exception ex) {
			writeln("Error: ", ex.msg);
		}
	} else 
		writeln("parse failed");
}

void main(string[] argv)
{
    string line;
    env["Pi"] = new Real(PI);//mkExp("3.14159265");
	if (argv.length > 1) 
		return process(argv[1..$].join(" "));
    writeln("Welcome to Precise Calculator. Enter expressions now. '?' for help.");
    bool readLine() { write("> "); line = readln(); return line !is null; }
    while(readLine()) {
        line = strip(line);
        if (line.length == 0) break;
		process(line);
    }
}
