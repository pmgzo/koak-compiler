def test(x : double) : double (1.0 + 2.0 + x) * (x + (1.0 + 2.0));

def itest1(): int
    -1;

iexpected_res1 = -1;
dexpected_res1 = 146.41;

def dtest1(): double
    test(9.1);

def itest2(): int
    -1;

iexpected_res2 = -1;
dexpected_res2 = 16.81;

def dtest2(): double
    test(1.1);

def itest3(): int
    -1;

iexpected_res3 = -1;
dexpected_res3 = 81.0;

def dtest3(): double
    test(6.0);

def itest4(): int
    -1;

iexpected_res4 = -1;
dexpected_res4 = 81.0;

def dtest4(): double
    test(6.0);

returnType = 1;
testId = 3;
