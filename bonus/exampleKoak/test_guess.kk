def youtwo(val: int): int
    (if val == 2 then 1
    else 0);

def younot(a : int): int
    (if a == 0 then 1
    else 0);

def youplay(a: int) : int
    (if a < 9 then
        (if a < 8 then
            if a < 7 then 7
        )
    else 8);

def you23(a: int) : int
    if a == 23 then
        1
    else 0;

def itest1(): int
    youtwo(2);

def itest2(): int
    younot(0);

def itest3(): int
    youplay(10);

def itest4(): int
    you23(23);

iexpectedRes1 = 1;
iexpectedRes2 = 1;
iexpectedRes3 = 8;
iexpectedRes4 = 1;

dexpectedRes1 = -1.0;
dexpectedRes2 = -1.0;
dexpectedRes3 = -1.0;
dexpectedRes4 = -1.0;

def dtest1(): double
    -1.0;

def dtest2(): double
    -1.0;

def dtest3(): double
    -1.0;

def dtest4(): double
    -1.0;

returnType = 0;
testId = 14;