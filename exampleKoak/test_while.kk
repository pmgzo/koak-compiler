def While(max : int) : int
    i = 0:
    (while i < max do
        i = i + 1):
    i + 2;

def While2(max : int) : int
    i = 0:
    j = 0:
    (while i < max do
        i = i + 1:
        while j < i * max do
            j = j + 1
        ):
    j + 2;

def While2plus(max : int) : int
    res = 0:
    i = 0:
    j = 0:
    (while i < max do
        i = i + 1:
        (while j < i * max do
            j = j + 1:
            res = While(j + 2))
        ):
    res;

def itest1(): int
    While(5);

def itest2(): int
    While2(5);

def itest3(): int
    While2plus(5);

def itest4(): int
    a = While2plus(While(4)) / 2;

iexpected_res1 = 6;
iexpected_res2 = 21;
iexpected_res3 = 22;
iexpected_res4 = 11;



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
testId = 10;
