def factorial(a: int): int
    (if a < 2 then
        a
    else
        a * fact(a - 1));

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

iexpectedRes1 = 120;
iexpectedRes2 = 5040;
iexpectedRes3 = 3628800;
iexpectedRes4 = 1307674368000;
dexpectedRes1 = -1.0;
dexpectedRes2 = -1.0;
dexpectedRes3 = -1.0;
dexpectedRes4 = -1.0;

returnType = 0;
testId = 0;
