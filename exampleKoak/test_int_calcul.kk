def test(x : int) : int 1 + 2 + x;

def test3(x : int) : int (1 + 2 + x) * (x + (1 + 2));

def itest1(): int
    test(7);

iexpectedRes1 = 10;
dexpectedRes1 = -1.0;

def dtest1(): double
    -1.0;

def itest2(): int
    test3(10);

iexpectedRes2 = 169;
dexpectedRes2 = -1.0;

def dtest2(): double
    -1.0;

def itest3(): int
    test3(105);

iexpectedRes3 = 11664;
dexpectedRes3 = -1.0;

def dtest3(): double
    -1.0;

def itest4(): int
    test(3);

iexpectedRes4 = 6;
dexpectedRes4 = -1.0;

def dtest4(): double
    -1.0;

returnType = 0;
testId = 4;