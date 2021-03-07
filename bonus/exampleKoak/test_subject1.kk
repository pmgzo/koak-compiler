def test(x : double) : double x + 2.0;

def itest1(): int
    -1;

iexpectedRes1 = -1;
dexpectedRes1 = 8.0;

def dtest1(): double
    test(6.0);

def itest2(): int
    -1;

iexpectedRes2 = -1;
dexpectedRes2 = 1.0;

def dtest2(): double
    test(-1.0);

def itest3(): int
    -1;

iexpectedRes3 = -1;
dexpectedRes3 = -1.0;

def dtest3(): double
    test(-3.0);

def itest4(): int
    -1;

iexpectedRes4 = -1;
dexpectedRes4 = 131.0;

def dtest4(): double
    test(129.0);

returnType = 1;
testId = 5;