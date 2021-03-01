def fct1(break : double) : double
    a = 0.0:
    (for id = 2.0, id < break, 1.0 in
        a = a * 1.0 + id):
    a;

def fct2(break : double) : double
    i = 34.0:
    (for id = 0.0, id < break, 1.0 in
        i = i + 1.0):
    i;

def fct3(break : double) : double
    i = 0.0:
    (for id = 0.0, id < break, 1.0 in
        i = i + 1.0):
    i;

def fct4(break : double) : double
    v = 1.0:
    (for id = 0.0, id < break, 1.0 in
        v = v * 2.0):
    v;


def dtest1(): double
    fct1(3.0);

def dtest2(): double
    fct2(9.0);

def dtest3(): double
    fct3(9.0);

def dtest4(): double
    fct4(4.0);

dexpected_res1 = 2.0;
dexpected_res2 = 43.0;
dexpected_res3 = 9.0;
dexpected_res4 = 16.0;

def itest1(): int

def itest2(): int
    -1;

def itest3(): int
    -1;

def itest4(): int
    -1;

iexpected_res1 = -1;
iexpected_res2 = -1;
iexpected_res3 = -1;
iexpected_res4 = -1;


returnType = 1;
testId = 13;
