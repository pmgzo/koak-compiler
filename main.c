#include <stdio.h>
// int fct1(int a, int b);//mod

int caller();
double testf(double a);

int cond1();
float cond2();
int not1();
double minus1();
int condIf();
int condIfElse(int a);
int condIECB(int a);
int while1(int a);
int while11(int a);
int while12(int a);
int fact(int a);
int for1(int a);
int whileImbr(int a);
int whileImbrGlobalVar(int a);
extern int global1;

int main()
{
    // printf("%d\n", fct5(1, 2));// 5 mod5
    // printf("%d\n", fct1(5, 8));    // 13

    printf("%d\n", caller());    // 13
    printf("%f\n", testf(9.0));  // 9.0 + 4.0
    printf("%d\n", cond1(9.0));  // 1
    printf("%f\n", cond2(9.0));  // 0
    printf("%d\n", not1());      // 1
    printf("%f\n", minus1());    //-17
    printf("%d\n", condIf(9));    //9
    printf("%d\n", condIfElse(9)); //-11
    // printf("%d\n", condIECB(3));    //6
    // printf("%d\n", condIECB(6));    //5
    printf("%d\n", while1(1)); //90

    printf("%d\n", while11(0)); //5
    printf("%d\n", while12(0)); //2 if a <= 2

    printf("%d\n", fact(11)); //39 916 800
    printf("%d\n", for1(0)); //5
    printf("%d\n", whileImbr(0)); //25
    printf("%d\n", whileImbrGlobalVar(0)); //27
    printf("%d\n", global1);

    // printf("%d\n", for1(0)); //5
    return (0);
}