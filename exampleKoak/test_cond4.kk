def add(a: int): int
    if ((1 + 1) > a) then
       1
    else 2;

def add2(a: int): int
    if (1 + 1 > a) then
       5
    else 10;

def add3(a: int): int
    if ((1 + 1 > a) != !(a == 0)) then
       5
    else 10;

def itest1(): int
    add(0) + add2(0);

def itest2(): int
    add(0) + add2(3);

def itest3(): int
    add3(0);

def itest4(): int
    add3(1);

iexpectedRes1 = 6;
iexpectedRes2 = 11;
iexpectedRes3 = 5;
iexpectedRes4 = 10;

def dtest1(): double
    -1.0;

def dtest2(): double
    -1.0;
    
def dtest3(): double
    -1.0;
    
def dtest4(): double
    -1.0;

dexpectedRes1 = -1;
dexpectedRes2 = -1;
dexpectedRes3 = -1;
dexpectedRes4 = -1;

returnType = 0;
testId = 18;