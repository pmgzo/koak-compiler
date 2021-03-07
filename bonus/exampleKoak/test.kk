def sum(a : int, b : int): int
    a + b;

def fact2(a : int): int
    res = 1:
    (while a != 0 do
        res = res * a:
        a = a - 1):
    res;

def itest1(): int
    sum(sum(5, 6), -sum(-4, -7));

def itest2(): int
    sum(2, -5);

def itest3(): int
    fact2(1);

def itest4(): int
    fact2(15);

def dtest1(): double
    -1.0;

def dtest2(): double
    -1.0;

def dtest3(): double
    -1.0;

def dtest4(): double
    -1.0;

iexpectedRes1 = 22;
iexpectedRes2 = -3;
iexpectedRes3 = 1;
iexpectedRes4 = 1307674368000;
dexpectedRes1 = -1.0;
dexpectedRes2 = -1.0;
dexpectedRes3 = -1.0;
dexpectedRes4 = -1.0;

returnType = 0;
testId = 7;
