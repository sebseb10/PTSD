import java.util.Map;
import java.util.HashMap;

abstract class AExpr {
    abstract public String toString();
    abstract public int eval(Map<String,Integer> env);
    abstract public AExpr simplify();
    abstract public boolean equals(Object obj);
}

class CstI extends AExpr {
    protected final int i;

    public CstI(int i) {
        this.i = i;
    }

    public String toString() {
        return "" + i;
    }

    public int eval(Map<String,Integer> env) {
        return i;
    }

    public AExpr simplify() {
        return this;
    }
    public boolean equals(Object obj) {
        if (obj instanceof CstI) {
            CstI c = (CstI)obj;
            return this.i == c.i;
        } else {
            return false;
        }
    }
}

class Var extends AExpr {
    protected final String name;

    public Var(String name) {
        this.name = name;
    }

    public String toString() {
        return name;
    }

    public int eval(Map<String,Integer> env) {
        return env.get(name);
    }

    public AExpr simplify() {
        return this;
    }
    public boolean equals(Object obj) {
        if (obj instanceof Var) {
            Var v = (Var)obj;
            return this.name.equals(v.name);
        } else {
            return false;
        }
    }
}

abstract class Binop extends AExpr {
    protected final AExpr e1, e2;

    public Binop(AExpr e1, AExpr e2) {
        this.e1 = e1;
        this.e2 = e2;
    }
}

class Add extends Binop {
    public Add (AExpr e1, AExpr e2) {
         super(e1, e2);
    }
    public String toString() {
        return "(" + e1.toString() + " + " + e2.toString() + ")";
    }
    public int eval(Map<String,Integer> env) {
        return e1.eval(env) + e2.eval(env);
    }
    public AExpr simplify() {
        AExpr s1 = e1.simplify();
        AExpr s2 = e2.simplify();
        if (s1 instanceof CstI && ((CstI)s1).i == 0) return s2;
        else if (s2 instanceof CstI && ((CstI)s2).i == 0) return s1;
        else return new Add(s1, s2);
    }
    public boolean equals(Object obj) {
        if (obj instanceof Add) {
            Add a = (Add)obj;
            return this.e1.equals(a.e1) && this.e2.equals(a.e2);
        } else {
            return false;
        }
    }

}

class Mul extends Binop {
    public Mul (AExpr e1, AExpr e2) {
        super(e1, e2);
    }
    public String toString() {
        return "(" + e1.toString() + " * " + e2.toString() + ")";
    }
    public int eval(Map<String,Integer> env) {
        return e1.eval(env) * e2.eval(env);
    }
    public AExpr simplify() {
        AExpr s1 = e1.simplify();
        AExpr s2 = e2.simplify();
        if (s1 instanceof CstI && ((CstI)s1).i == 1) return s2;
        else if (s2 instanceof CstI && ((CstI)s2).i == 1) return s1;
        else if (s1 instanceof CstI && ((CstI)s1).i == 0) return new CstI(0);
        else if (s2 instanceof CstI && ((CstI)s2).i == 0) return new CstI(0);
        else return new Mul(s1, s2);
    }
    public boolean equals(Object obj) {
        if (obj instanceof Mul) {
            Mul m = (Mul)obj;
            return this.e1.equals(m.e1) && this.e2.equals(m.e2);
        } else {
            return false;
        }
    }
}

class Sub extends Binop {
    public Sub (AExpr e1, AExpr e2) {
        super(e1, e2);
    }
    public String toString() {
        return "(" + e1.toString() + " - " + e2.toString() + ")";
    }
    public int eval(Map<String,Integer> env) {
        return e1.eval(env) - e2.eval(env);
    }
    public AExpr simplify() {
        AExpr s1 = e1.simplify();
        AExpr s2 = e2.simplify();
        if (s1 instanceof CstI && ((CstI)s1).i == 0) return s2;
        else if (s2 instanceof CstI && ((CstI)s2).i == 0) return s1;
        else if (s1.equals(s2)) return new CstI(0);
        else return new Sub(s1, s2);
    }
    public boolean equals(Object obj) {
        if (obj instanceof Sub) {
            Sub s = (Sub)obj;
            return this.e1.equals(s.e1) && this.e2.equals(s.e2);
        } else {
            return false;
        }
    }
}


public class SimpleAExpr {
    public static void main(String[] args) {
        AExpr e1 = new CstI(42);
        AExpr e2 = new Var("x");
        AExpr e3 = new Add(new CstI(42), new CstI(67));
        AExpr e4 = new Mul (new CstI(42), new Var("x"));
        AExpr e5 = new Sub (new CstI(42), new CstI(21));

        System.out.println(e1.toString());
        System.out.println(e2.toString());
        System.out.println(e3.toString());
        System.out.println(e4.toString());
        System.out.println(e5.toString());
    }
}