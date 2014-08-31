// A program that simulates a game of raquetball in which the first
// player has a chance of winning her volley 0.6, and a chance of
// winning the opponent's volley 0.5

#include <time.h>
#include <stdlib.h>
#include <stdio.h>

#define TRUE (1==1)
#define FALSE (!TRUE)

double random_0_to_1() {
    return (double) rand() / (double) RAND_MAX;
}

int racquetball() {
    unsigned int player1 = 0;
    unsigned int player2 = 0;

    do {
        if (random_0_to_1() < 0.6)
            player1++;
        
        if (player1 == 21)
            break;

        if (random_0_to_1() > 0.5)
            player2++;
    } while (player1 < 21 && player2 < 21);

    return player1 == 21;
}

int main(int argc, const char* argv[]) {
    if (argc < 2) {
        printf("Usage: %s <number of times to test>\n", argv[0]);
        exit(1);
    }

    srand(time(NULL));
    unsigned long long num_tests = atoll(argv[1]);

    unsigned long long player1_wins = 0;

    for (unsigned long long i = 0; i < num_tests; i++)
        if (racquetball())
            player1_wins++;

    double prob = (double) player1_wins / (double) num_tests;

    printf("Player1 won %.2f%%\n", prob * 100);

    return 0;
}
