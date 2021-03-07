def test(x : double) : double x - 1.0;

def itest1(): int
    -1;

iexpectedRes1 = -1;
dexpectedRes1 = 5.0;

def dtest1(): double
    test(6.0);

def itest2(): int
    -1;

iexpectedRes2 = -1;
dexpectedRes2 = 10.0;

def dtest2(): double
    test(11.0);

def itest3(): int
    -1;

iexpectedRes3 = -1;
dexpectedRes3 = -2.0;

def dtest3(): double
    test(-1.0);

def itest4(): int
    -1;

iexpectedRes4 = -1;
dexpectedRes4 = 998.0;

def dtest4(): double
    test(999.0);

returnType = 1;
testId = 6;