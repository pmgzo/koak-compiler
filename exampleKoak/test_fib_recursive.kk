def fib(x : int) : int
    if x < 3 then
        1
    else
        fib(x - 1) + fib(x - 2);

def itest1(): int
    fibi(5);

def itest2(): int
    fibi(10);

def itest3(): int
    fibi(1);

def itest4(): int
    fibi(18);

def dtest1(): double
    -1.0;

def dtest2(): double
    -1.0;

def dtest3(): double
    -1.0;

def dtest4(): double
    -1.0;

iexpectedRes1 = 5;
iexpectedRes2 = 55;
iexpectedRes3 = 1;
iexpectedRes4 = 2584;
dexpectedRes1 = -1.0;
dexpectedRes2 = -1.0;
dexpectedRes3 = -1.0;
dexpectedRes4 = -1.0;

returnType = 0;
testId = 1;
