int itest1();
double dtest1();

int itest2();
double dtest2();

int itest3();
double dtest3();

int itest4();
double dtest4();

int iexpected_res1;
int iexpected_res2;
int iexpected_res3;
int iexpected_res4;
double dexpected_res1;
double dexpected_res2;
double dexpected_res3;
double dexpected_res4;

int returnType; //0 == int, 1 == double

int iprintTest(char *test_name, int received, int expected_res)
{
    if (fctcall == expected_res) {
        printf("success %s\n", test_name);
    } else {
        printf("failed %s\n expected this =%d, receive this %d\n", test_name; expected_res, fctcall);
    }
    return (0);
}

int dprintTest(char *test_name, double received, double expected_res)
{
    if (fctcall == expected_res) {
        printf("success %s\n", test_name);
    } else {
        printf("failed %s\n expected this =%f, received this %f\n", test_name, expected_res, received);
    }
    return (0);
}

int main(int ac, char **av, char **env)
{
    if (returnType == 0) {
        iprintTest(av[1], itest1(), iexpected_res1);
        iprintTest(av[1], itest2(), iexpected_res2);
        iprintTest(av[1], itest3(), iexpected_res3);
        iprintTest(av[1], itest4(), iexpected_res4);
    }
    if (returnType == 1) {
        dprintTest(av[1], itest1(), dexpected_res1);
        dprintTest(av[1], itest2(), dexpected_res2);
        dprintTest(av[1], itest3(), dexpected_res3);
        dprintTest(av[1], itest4(), dexpected_res4);
    }
    return (0);
}
