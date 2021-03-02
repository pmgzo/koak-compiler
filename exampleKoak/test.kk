def sum(a : int, b : int): int
    a + b;

def fact2(a : int): int
    res = 1:
    (while a != 0 do
        res = res * a:
        a = a - 1)
    :
    res;

def itest1(): int
    sum(sum(5, 6), sum(-4 -7));

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

iexpected_res1 = 0;
iexpected_res2 = -3;
iexpected_res3 = 1;
iexpected_res4 = 1307674368000;
dexpected_res1 = -1.0;
dexpected_res2 = -1.0;
dexpected_res3 = -1.0;
dexpected_res4 = -1.0;

returnType = 0;
testId = 7;
