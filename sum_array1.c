#include <x86intrin.h>
#include <stdio.h>

double sum(double a[], int n) {
    __m128d vsum = _mm_set1_pd(0.0);
    for (int i = 0; i < n; i += 2)
    {
        __m128d v = _mm_load_pd(&a[i]);
        vsum = _mm_add_pd(vsum, v);
    }
    vsum = _mm_hadd_pd(vsum, vsum);
    return _mm_cvtsd_f64(vsum);
}

int main() {
    int n = 10;
    double arr[n];
    for (int i = 0; i < n; i++) {
        arr[i] = 5.0;
    }
    double v = sum(arr,n);
    printf("%f\n",v);
    return EXIT_SUCCESS;
}
