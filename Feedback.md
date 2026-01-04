Womancala
Functionality (70+5=75/73 points)
* Game mechanics:                                 20 points
* Exact game solver:                                 15 points
* Cut-off depth solver:                                 12 points
* Reasonable evaluation function:                 2 points
* Avoiding unnecessary work:                            1/3 points
   * Rather than checking rateGame, you should be checking instead of maximumBy/minimumBy. As it is you only avoid unnecessary work on the second-to-last layer, but evaluate exhaustively if there are moves < depth that lead to a win.
* Command-line interface:                         10 points
   * Lacks a llittle polish, but quite nice!
* Move and verbose flags:                         5 points
* Error-handling:                                 4/5 points
   * error in whoMIghtWin
* Makefile:                                        1 point
* Interactive:                                        5 points
   * Doesn’t default to empty board
Design (25/27 points)
* Well-designed data types                        8 points
* Well-decomposed functions                        10 points
   * What a beautiful main!
* Good module decomposition                        2 points
* Good variable names                                1/2 points
   * pointsSideTwo
* Efficient/idiomatic code                        4/5 points
   * calling winner twice in rateGame when you’re already doing a case on it.
   * calls rateGame at every depth, even if it ignores it is definitely odd. Should be okay because of laziness, but tripleToTuple could just be (s,t) -> (rateGame s,s).
