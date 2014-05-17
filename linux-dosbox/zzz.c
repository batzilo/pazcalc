#include <stdio.h>

#define M 6
#define N 5
#define Q 7

int main() {
    int i,j,k;

    int a[M][N];
    int b[N][Q];
    int c[M][Q];

    for (i=0; i<M; i++) {
        for (j=0; j<N; j++) {
            a[i][j] = i+j;
            printf("%d ", a[i][j]);
        }
        printf("\n");
    }

    for (i=0; i<N; i++) {
        for (j=0; j<Q; j++) {
            b[i][j] = i+j;
            printf("%d ", b[i][j]);
        }
        printf("\n");
    }

    for (i=0; i<M; i++) {
        for (j=0; j<Q; j++) {
            c[i][j] = 0;
            for (k=0; k<N; k++) {
                c[i][j] = c[i][j] + a[i][k] * b[k][j];
            }
        }
    }

    for (i=0; i<M; i++) {
        for (j=0; j<Q; j++) {
            printf("%d ", c[i][j]);
        }
        printf("\n");
    }

    return 0;
}
