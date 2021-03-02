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

iexpected_res1 = 5;
iexpected_res2 = 55;
iexpected_res3 = 1;
iexpected_res4 = 2584;
dexpected_res1 = -1.0;
dexpected_res2 = -1.0;
dexpected_res3 = -1.0;
dexpected_res4 = -1.0;

returnType = 0;
testId = 1;
