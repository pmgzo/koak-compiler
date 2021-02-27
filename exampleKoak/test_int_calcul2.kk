def add(x : int, y : int) : int
    x + y;

def sub(x : int, y : int) : int
    x - y;

def div(x : int, y : int) : int
    x / y;

def mul(x : int, y : int) : int
    x * y;

def itest1(): int
    sub(add(div(-mul(2, 5), 3), -5), -10);

def itest2(): int
    a = 5:
    b = 6 + 1:
    c = add(a, b):
    d = -sub(15, c):
    e = a + b + c * d - a * ((b + c + 1) / 2);

def itest3(): int
    a = 5:
    b = 6 + 1:
    c = add(a, b):
    d = -sub(15, c):
    e = a + 1 > b - 2:
    f = e != 1:
    g = f == 0:
    h = !g:
    i = !!h;

def itest4(): int
    a = 3:
    a;

iexpected_res1 = 2;
iexpected_res2 = -74;
iexpected_res3 = 0;
iexpected_res4 = 3;



def dtest1(): double
    -1.0;

def dtest2(): double
    -1.0;

def dtest3(): double
    -1.0;

def dtest4(): double
    -1.0;

dexpected_res1 = -1.0;
dexpected_res2 = -1.0;
dexpected_res3 = -1.0;
dexpected_res4 = -1.0;

returnType = 0;
testId = 8;
