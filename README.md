# [99 Scala Problems](https://aperiodic.net/pip/scala/s-99/)
A repository containing my solutions to 99 Scala problems (linked in header).

## Rules/Approach
1. Approach the problem first by myself, then consult reference materials, then
   consult solution/internet
2. Attempt to solve as many problems "from scratch" (i.e. minimal helper methods
   etc.) and with solutions to previous problems
3. Write and pass tests for every problem
4. Write impressions for problems or groups of problems. What I learned, my
   approach, difficulty, etc.

## Impressions
Stars denote difficulty, from 1-3, as listed in the original problem set

Problems 1-6(\*): Trivial. These will make a good foundation for harder list
problems. Learned some about exceptions and a lot about testing with MUnit.
Wishing there were a subdivision that existed between individual tests and
suites/classes.

Problem 7(\*\*): Struggled desperately to keep some semblance of typing smaller than
`Any` and failed. At first attempted to avoid any helper methods but ended up
using `foldLeft`. Consulted reference docs for `flatten` and the solution but
didn't understand and wasn't satisfied with the approach respectively.
