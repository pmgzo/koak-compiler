def factorial(a: int): int
    if a < 2 then
        a
    else
        a * fact(a - 1);

def itest1(): int
    fact(5);

def itest2(): int
    fact(7);

def itest3(): int
    fact(10);

def itest4(): int
    fact(15);

def dtest1(): double
    -1.0;

def dtest2(): double
    -1.0;

def dtest3(): double
    -1.0;

def dtest4(): double
    -1.0;

iexpected_res1 = 120;
iexpected_res2 = 5040;
iexpected_res3 = 3628800;
iexpected_res4 = 1307674368000;
dexpected_res1 = -1.0;
dexpected_res2 = -1.0;
dexpected_res3 = -1.0;
dexpected_res4 = -1.0;

returnType = 0;
testId = 0;
