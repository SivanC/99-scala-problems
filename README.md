# [99 Scala Problems](https://aperiodic.net/pip/scala/s-99/)
A repository containing my solutions to 99 Scala problems (linked in header).

## Rules/Approach
1. Approach the problem first by myself, then consult reference materials, then
   consult solution/internet.
2. Attempt to solve as many problems "from scratch" (i.e. minimal helper methods
   etc.) and with solutions to previous problems. Some problems ask for a
   "direct solution" as an extension of previous problems, (e.g. #13 with #10);
   for those problems I will use helper methods for the "indirect" problems.
3. Write and pass tests for every problem.
4. Write impressions for problems or groups of problems. What I learned, my
   approach, difficulty, etc.

## Impressions
Stars denote difficulty, from 1 (easiest) to 3 (hardest), as listed in the original problem set

Problems 1-6(\*): Trivial. These will make a good foundation for harder list
problems. Learned some about exceptions and a lot about testing with MUnit.
Wishing there were a subdivision that existed between individual tests and
suites/classes.

Problem 7(\*\*): Struggled desperately to keep some semblance of typing smaller than
`Any` and failed. At first attempted to avoid any helper methods but ended up
using `foldLeft`. Consulted reference docs for `flatten` and the solution but
didn't understand and wasn't satisfied with the approach respectively.

Problems 8/9(\*\*): Did these in minutes, a big step down from the previous
2-star. Guess it shows which methods are really useful and which can be
rewritten easily if needed. Tail recursion and accumulators keep winning.
