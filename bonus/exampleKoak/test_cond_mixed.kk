def IfElse(x : int) : int
    if x > 0
       then 1
    else 0;

def While2(max : int) : int
    i = 0:
    j = 0:
    (while i < max do
        i = i + 1:
        while j < i * max do
            j = j + 1
    ):
    j + 2;

def For2(max : int) : int
    a = 0:
    (for i = 0, i < max, 1 in
        (for j = 0, j < i * max, j = j + 1 in
            (for e = 0, e < j * max, e = e + While2(j) in
                a = a + e
            )
        )
    ):
    5;

def For3(max : int) : int
    a = 0:
    (for i = 0, i < max, 1 in
        (for j = 0, j < i * max, j = j + 1 in
            for e = 0, e < j * max, e = e + While2(j) in
                a = a + e
        )
    ):
    a;

def For4(max : int) : int
    a = 0:
    b = 0:
    (for i = 0, i < max, 1 in
        (for j = 0, j < i * max, j = j + 1 in
            (for e = 0, e < j * max, e = e + While2(j) in
                a = a + e):
            if a == 152
               then b = 100
        )
    ):
    b;

def itest1(): int
    For2(5);

def itest2(): int
    IfElse(IfElse(While2(5)));

def itest3(): int
    For3(5);

def itest4(): int
    a = (For4(5) + 50) * 2;

iexpectedRes1 = 5;
iexpectedRes2 = 1;
iexpectedRes3 = 152;
iexpectedRes4 = 300;



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
testId = 11;
