def fibi(x : int) : int
    a = 1:
    b = 1:
    c = 0:
    for i = 3, i < x, 1 in
        c = a + b:
        a = b:
        b = c,
    b;

fibi(5);
fibi(10);
