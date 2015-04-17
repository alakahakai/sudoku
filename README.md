# sudoku

  A Haskell program to solve Sodoku puzzles, takes file and command line text inputs

  With parallelism built in, compiled using RTS options: -N

  Ray Qiu <ray.qiu@gmail.com>, April 2015

  --

  rqiu-mbp15:sudoku rqiu$ dist/build/sudoku/sudoku "000000013400200000600000000000460500010000007200500000000031000000000420080000000 000000013020500000000000000103000070000802000004000000000340500670000200000010000"
  
Sudoku puzzle:
▒ ▒ ▒ ▒ ▒ ▒ ▒ 1 3
4 ▒ ▒ 2 ▒ ▒ ▒ ▒ ▒
6 ▒ ▒ ▒ ▒ ▒ ▒ ▒ ▒
▒ ▒ ▒ 4 6 ▒ 5 ▒ ▒
▒ 1 ▒ ▒ ▒ ▒ ▒ ▒ 7
2 ▒ ▒ 5 ▒ ▒ ▒ ▒ ▒
▒ ▒ ▒ ▒ 3 1 ▒ ▒ ▒
▒ ▒ ▒ ▒ ▒ ▒ 4 2 ▒
▒ 8 ▒ ▒ ▒ ▒ ▒ ▒ ▒

Single solution:
8 2 7 9 4 5 6 1 3
4 3 1 2 8 6 9 7 5
6 5 9 1 7 3 8 4 2
3 7 8 4 6 2 5 9 1
5 1 4 3 9 8 2 6 7
2 9 6 5 1 7 3 8 4
9 4 2 8 3 1 7 5 6
1 6 3 7 5 9 4 2 8
7 8 5 6 2 4 1 3 9

Sudoku puzzle:
▒ ▒ ▒ ▒ ▒ ▒ ▒ 1 3
▒ 2 ▒ 5 ▒ ▒ ▒ ▒ ▒
▒ ▒ ▒ ▒ ▒ ▒ ▒ ▒ ▒
1 ▒ 3 ▒ ▒ ▒ ▒ 7 ▒
▒ ▒ ▒ 8 ▒ 2 ▒ ▒ ▒
▒ ▒ 4 ▒ ▒ ▒ ▒ ▒ ▒
▒ ▒ ▒ 3 4 ▒ 5 ▒ ▒
6 7 ▒ ▒ ▒ ▒ 2 ▒ ▒
▒ ▒ ▒ ▒ 1 ▒ ▒ ▒ ▒

Single solution:
5 4 9 7 2 8 6 1 3
3 2 8 5 6 1 4 9 7
7 1 6 4 9 3 8 2 5
1 8 3 6 5 4 9 7 2
9 5 7 8 3 2 1 4 6
2 6 4 1 7 9 3 5 8
8 9 2 3 4 7 5 6 1
6 7 1 9 8 5 2 3 4
4 3 5 2 1 6 7 8 9
