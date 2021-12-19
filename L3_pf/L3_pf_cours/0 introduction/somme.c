int somme(int n) {
    int i;
    int somme = 0;

    for (i = 1; i <= n; i++) {
        somme += 2 * i;
    }

    return somme;
}
