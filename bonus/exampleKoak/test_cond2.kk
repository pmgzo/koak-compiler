def fct1(break: double): double
    if (break == 1.0) == !(5 > 0) then
       5.0
    else 6.0;

def fct2(break: double): double
    if (break == 1.0) == !!(5 > 0) then
       5.0
    else 6.0;

def dtest1(): double
    fct1(1.0);

def dtest2(): double
    fct1(2.0);

def dtest3(): double
    fct1(1.0);

def dtest4(): double
    fct1(2.0);

dexpectedRes1 = 6.0;
dexpectedRes2 = 5.0;
dexpectedRes3 = 6.0;
dexpectedRes4 = 5.0;

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
