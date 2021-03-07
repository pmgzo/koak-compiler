def test(x : double) : double (1.0 + 2.0 + x) * (x + (1.0 + 2.0));

def itest1(): int
    -1;

iexpectedRes1 = -1;
dexpectedRes1 = 146.41;

def dtest1(): double
    test(9.1);

def itest2(): int
    -1;

iexpectedRes2 = -1;
dexpectedRes2 = 16.81;

def dtest2(): double
    test(1.1);

def itest3(): int
    -1;

iexpectedRes3 = -1;
dexpectedRes3 = 81.0;

def dtest3(): double
    test(6.0);

def itest4(): int
    -1;

iexpectedRes4 = -1;
dexpectedRes4 = 81.0;

def dtest4(): double
    test(6.0);

returnType = 1;
testId = 3;
