def test(x : int) : int 1 + 2 + x;

test(0);
test(78);
test(105);


def test2(x : double) : double (1 + 2 + x) * (x + (1 + 2));

test2(0);
test2(78);
test2(105);


def test3(x : int) : int (1 + 2 + x) * (x + (1 + 2));

test3(0);
test3(78);
test3(105);
