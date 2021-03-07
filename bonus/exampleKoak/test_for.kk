def fct1(break : int): int
    a = 0:
    (for id = 2, id < break, 1 in
        a = a * 1 + id):
    a;

def fct2(break : int) : int
    i = 34:
    (for id = 0, id < break, 1 in
        i = i + 1):
    i;

def fct3(break : int) : int
    i = 0:
    (for id = 0, id < break, 1 in
        i = i + 1):
    i;

def fct4(break : int) : int
    v = 1:
    (for id = 0, id < break, 1 in
        v = v * 2):
    v;

def itest1(): int
    fct1(3);

def itest2(): int
    fct2(9);

def itest3(): int
    fct3(9);

def itest4(): int
    fct4(4);

iexpectedRes1 = 2;
iexpectedRes2 = 43;
iexpectedRes3 = 9;
iexpectedRes4 = 16;

def dtest1(): double
    -1.0;

def dtest2(): double
    -1.0;

def dtest3(): double
    -1.0;

def dtest4(): double
    -1.0;

dexpectedRes1 = -1.0;
dexpectedRes2 = -1.0;
dexpectedRes3 = -1.0;
dexpectedRes4 = -1.0;

returnType = 0;
testId = 12;
