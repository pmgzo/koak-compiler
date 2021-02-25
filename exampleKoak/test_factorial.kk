def factorial(a: int): int
    if a < 2 then
        a
    else
        a * fact(a - 1);

def itest1(): int
    fact(7);

def dtest1(): double
    -1.0;

iexpected_res = 5048;
dexpected_res = -1.0;

returnType = 0;