def fct1(break: double): double
    (if break == 1.0 then 5.0
    else 6.0);

def fct2(a : double, b: double) : double
    a * b;

def fct3(i: double) : double
    if i == 3.0 then 1.0;

def fct4(a: double, c: double) : double
    if a > 7.0 then
        (if a == 7.0 then 90.0
        else 12.0)
    else c;

def dtest1(): double
    fct1(1.0);

def dtest2(): double
    fct2(9.0, 70.0);

def dtest3(): double
    fct3(2.0);

def dtest4(): double
    fct4(7.0, 4.0);

dexpectedRes1 = 5.0;
dexpectedRes2 = 630.0;
dexpectedRes3 = 0.0;
dexpectedRes4 = 4.0;

def itest1(): int
    -1;

def itest2(): int
    -1;

def itest3(): int
    -1;

def itest4(): int
    -1;

iexpectedRes1 = -1;
iexpectedRes2 = -1;
iexpectedRes3 = -1;
iexpectedRes4 = -1;

returnType = 1;
testId = 15;
