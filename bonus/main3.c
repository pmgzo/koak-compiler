#include <stdio.h>
// int fct1(int a, int b);//mod

/* int caller(); */
/* double testf(double a); */

/* int cond1(); */
/* float cond2(); */
/* int not1(); */
/* double minus1(); */
/* int condIf(); */
/* int condIfElse(int a); */
/* int condIECB(int a); */
/* int while1(int a); */
/* int while11(int a); */
/* int while12(int a); */
/* int fact(int a); */
/* int for1(int a); */
/* int whileImbr(int a); */
/* int whileImbrGlobalVar(int a); */
/* extern int global1; */



int iadd(int a, int b);
int icaller();
double itestf(double a);
int icond1();
int ifor2(int a);
int iwhile1(int a);
int ifor3(int a);

/* float icond2(); */
/* int inot1(); */
/* double iminus1(); */
/* int icondIf(); */
/* int icondIfElse(int a); */
/* int icondIECB(int a); */
/* int iwhile1(int a); */
/* int iwhile11(int a); */
/* int iwhile12(int a); */
/* int ifact(int a); */
/* int ifor1(int a); */
/* int iwhileImbr(int a); */
/* int iwhileImbrGlobalVar(int a); */
/* extern int iglobal1; */

int main()
{
    // printf("%d\n", fct5(1, 2));// 5 mod5
    // printf("%d\n", fct1(5, 8));    // 13

    /* printf("%d\n", caller());    // 13 */
    /* printf("%f\n", testf(9.0));  // 9.0 + 4.0 */
    /* printf("%d\n", cond1(9.0));  // 1 */
    /* printf("%f\n", cond2(9.0));  // 0 */
    /* printf("%d\n", not1());      // 1 */
    /* printf("%f\n", minus1());    //-17 */
    /* printf("%d\n", condIf(9));    //9 */
    /* printf("%d\n", condIfElse(9)); //-11 */
    /* // printf("%d\n", condIECB(3));    //6 */
    /* // printf("%d\n", condIECB(6));    //5 */
    /* printf("%d\n", while1(1)); //90 */

    /* printf("%d\n", while11(0)); //5 */
    /* printf("%d\n", while12(0)); //2 if a <= 2 */

    /* printf("%d\n", fact(11)); //39 916 800 */
    /* printf("%d\n", for1(0)); //5 */
    /* printf("%d\n", whileImbr(0)); //25 */
    /* printf("%d\n", whileImbrGlobalVar(0)); //27 */
    /* printf("%d\n", global1); */

    /* // printf("%d\n", for1(0)); //5 */




    printf("%d\n", iadd(1, 3));    // 4
    printf("%d\n", icaller());    // 13
    printf("%f\n", itestf(9.0));  // 9.0 + 4.0
    printf("%d\n", icond1(9.0));  // 1
    printf("%d\n", ifor2(17));  // 22
    printf("%d\n", iwhile1(0));  // 5
    printf("%d\n", ifor2(10));  // 15

    /* printf("%f\n", icond2(9.0));  // 0 */
    /* printf("%d\n", inot1());      // 1 */
    /* printf("%f\n", iminus1());    //-17 */
    /* printf("%d\n", icondIf(9));    //9 */
    /* printf("%d\n", icondIfElse(9)); //-11 */
    /* // printf("%d\n", icondIECB(3));    //6 */
    /* // printf("%d\n", icondIECB(6));    //5 */
    /* printf("%d\n", iwhile1(1)); //90 */

    /* printf("%d\n", iwhile11(0)); //5 */
    /* printf("%d\n", iwhile12(0)); //2 if a <= 2 */

    /* printf("%d\n", ifact(11)); //39 916 800 */
    /* printf("%d\n", ifor1(0)); //5 */
    /* printf("%d\n", iwhileImbr(0)); //25 */
    /* printf("%d\n", iwhileImbrGlobalVar(0)); //27 */
    /* printf("%d\n", iglobal1); */
    return (0);
}
