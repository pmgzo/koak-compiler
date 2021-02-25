def test(x : int) : int 1 + 2 + x;

def test3(x : int) : int (1 + 2 + x) * (x + (1 + 2));

def itest1(): int
    test(7);

iexpected_res1 = 10;
dexpected_res1 = -1.0;

def dtest1(): double
    -1.0;

def itest2(): int
    test3(10);

iexpected_res2 = 169;
dexpected_res2 = -1.0;

def dtest2(): double
    -1.0;

def itest3(): int
    test3(105);

iexpected_res3 = 11664;
dexpected_res3 = -1.0;

def dtest3(): double
    -1.0;

def itest4(): int
    test(3);

iexpected_res4 = 6;
dexpected_res4 = -1.0;

def dtest4(): double
    -1.0;

returnType = 0;
testId = 4;