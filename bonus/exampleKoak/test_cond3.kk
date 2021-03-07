def fct4(a: double): double
    if !(a > 1.1) then 1.1
    else 1.2;

def w(a: double): double
    i = 1:
    (while !(i > 10) do
        i = i + 1:
        a = a + 1.0):
    a;

def w2(a: double): double
    i = 11:
    (if ((i > 10) == (a == 3.0)) then
        3.3
    else 3.4);

def w3(a: double): double
    i1 = 11.1:
    (if !(a < 3.0) then
        i1 = i1 + 0.9):
    i1;

def dtest1(): double
    fct4(1.1);

def dtest2(): double
    w(1.0);
    
def dtest3(): double
    w2(3.0);
    
def dtest4(): double
    w3(4.0);

dexpectedRes1 = 1.1;
dexpectedRes2 = 11.0;
dexpectedRes3 = 3.3;
dexpectedRes4 = 12.0;

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
testId = 17;