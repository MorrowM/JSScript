fn pow(m, n) {
    var res = 1;
    while (n != 0) {
        res = res * m;
        n = n + (-1);
    }
    return res;
}