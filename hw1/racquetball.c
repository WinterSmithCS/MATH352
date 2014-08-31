// A program that simulates a game of raquetball in which the first
// player has a chance of winning her volley 0.6, and a chance of
// winning the opponent's volley 0.5

#include <time.h>
#include <stdlib.h>
#include <stdio.h>

#define TRUE (1==1)
#define FALSE (!TRUE)

enum {PLAYER1, PLAYER2};

#define P1_VOLLEY_CHANCE (0.6)
#define P2_VOLLEY_CHANCE (0.5)

double random_0_to_1() {
    return (double) rand() / (double) RAND_MAX;
}

int volley(double chance) {
    return random_0_to_1() < chance;
}

int racquetball() {
    int score[2];
    score[PLAYER1] = 0;
    score[PLAYER2] = 0;

    int player1_serve = TRUE;

    do {
        if (player1_serve) {
            while (volley(P1_VOLLEY_CHANCE) && score[PLAYER1] < 21)
                score[PLAYER1]++;
        } else {
            while(volley(P2_VOLLEY_CHANCE) && score[PLAYER2] < 21)
                score[PLAYER2]++;
        }
        player1_serve = !player1_serve;
    } while (score[PLAYER1] < 21 && score[PLAYER2] < 21);

    return score[PLAYER1] == 21;
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
