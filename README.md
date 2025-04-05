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

Problems 10/11(\*): Pretty simple, did learn a bit about using a second "mini"
accumulator that is added to the accumulator at the start of each run. Wonder
how much having to reverse the lists impacts the run time (obv it is O(n), but
compared to a non-linked list approach what is the time save looking like)

Problem 12(\*\*): Straightforward, but enjoyable execution with the loop
expansion inside the case clause. Using `list` instead of `tail` in the solution
gave me a hard time when testing, as Scalatest doesn't handle the infinite
recursion very well.

Problem 13(\*\*): This one was more challenging than I expected, and I spent a
lot of time mapping out the different recursion conditions. To my surprise, I
was able to write it with only 1 variable tracking both the length and character
of the run, and also make the function tail recursive.

Problems 14/15(\*, \*\*): Not sure why 15 is two stars, both easily handled with
the usual tail recursion strategy so far.

Problem 16(\*\*): Surprisingly interesting problem, had to think about how to
track the index of each element (accumulator), the proper way to handle the
first element (make its index one), and desired behavior when n is less than 1
or greater than the list size.

Problem 17(\*): Challenging for a one-star problem. Have to decide what to do
when the length is non-positive.

Problem 18(\*\*): Required a lot of thought to be put into edge cases and
expected behavior. I decided to make the slice work no matter the order of the
first two parameters, as that felt a little more flexible and robust, although
it might lead to incorrect assumptions that one value is greater than another to
the unaware.

Problem 19(\*\*): Finally a non-tail recursion problem (well, indirectly it
was). Glad to be able to use a previous method for an easy solution. I like that
they had me implement negative numbers as well.

## Bonus: Testing Library
At first, I used MUnit for my testing, as that was the library that was
suggested on the scala docs website, and it seemed fairly
simple/straightforward, which is what I was looking for. However, I found that I
didn't like its verbosity, and even when writing wrapper functions for the test
methods, I had trouble exporting them to each of my test objects once I isolated
them. I found that Scalatest maintained the simplicity of MUnit while also being
less verbose, allowing me to sub-categorize tests beyond suites, and choose the
testing style that was best for me.
