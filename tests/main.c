#include <stdio.h>

int itest1();
double dtest1();

int itest2();
double dtest2();

int itest3();
double dtest3();

int itest4();
double dtest4();

extern int iexpectedRes1;
extern int iexpectedRes2;
extern int iexpectedRes3;
extern int iexpectedRes4;
extern double dexpectedRes1;
extern double dexpectedRes2;
extern double dexpectedRes3;
extern double dexpectedRes4;

extern int returnType; //0 == int, 1 == double

int iprintTest(char *test_name, int res, int expectedRes)
{
    if (res == expectedRes) {
        printf("\033[1;32mSuccess %s\n\033[0m", test_name);
    } else {
        printf("\033[1;31mFailed %s\033[0m\n expected this = %d, receive this %d\n", test_name, expectedRes, res);
        return (84);
    }
    return (0);
}

int dprintTest(char *test_name, double res, double expectedRes)
{
    if (res == expectedRes) {
        printf("\033[1;32mSuccess %s\n\033[0m", test_name);
    } else {
        printf("\033[1;31mFailed %s\033[0m\n expected this = %f, received this %f\n", test_name, expectedRes, res);
    }
    return (0);
}

int main(int ac, char **av, char **env)
{
    if (returnType == 0) {
        iprintTest(av[1], itest1(), iexpectedRes1);
        iprintTest(av[1], itest2(), iexpectedRes2);
        iprintTest(av[1], itest3(), iexpectedRes3);
        iprintTest(av[1], itest4(), iexpectedRes4);
    }
    else if (returnType == 1) {
        dprintTest(av[1], itest1(), dexpectedRes1);
        dprintTest(av[1], itest2(), dexpectedRes2);
        dprintTest(av[1], itest3(), dexpectedRes3);
        dprintTest(av[1], itest4(), dexpectedRes4);
    }
    return (0);
}
