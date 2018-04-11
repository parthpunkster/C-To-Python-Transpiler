#include <stdio.h>

int main()
{
    int x = 1;
    int y = 1;
    char a = 'a';
    int array [3] = {1,2,3};
    float b = 10.02;
	FILE *file;
    file = fopen("tt.txt", "r");
    if (x==1)
    {
        printf("First if loop");
    }
    if ((x == 1) && (y==1))
    {
        printf("second if loop %i",x);
    }
    if ((x == 1) && (y==1))
    {
        printf("second if loop %i",x);
    }
    if ((x == 2) || (y == 1))
    {
        printf("%c",a);
    }
    int counter = 1;
    while (x<=5)
    {
        printf("%i",x);
        x++;
    }
    y = y-1;
    if (!(x==1))
    {
        printf("%f",b);
    }

    return 0;
}
