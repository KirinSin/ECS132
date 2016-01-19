# Homework 1

Due Monday, January 18.

## Note on Submission Packages

In any Homework assignment, include all the files for problems due on the same date in the same .tar file, no subdirectories. When the TA's grading script unpacks the file, all the files should be in the directory from which he issued the unpack command. Be sure to use exactly the same function names, file names etc. as in the specs!

## Problem 1

Items come in that need to be packed in boxes, in order of arrival. The maximum allowable weight per box is wmax; if an item would cause a box to go overweight, a new box must be started. Denote the weights of the items by W1, W2, W3, ..., with these values being stochastically independent. Suppose for each i, Wi is equal to 1, 2 or 3 with probability 1/3 each, and wmax = 4. Find the following probabilities:

- There are 3 items in Box 1. (Note: In probability problems, phrasing like "3" means "exactly 3.")
- The total weight in Box 1 is under 4.
- The weight of the first item placed into Box 2 is 1.
- Given that the weight of the first item placed into Box 2 is 1, the probability that the first item in Box 1 was 1.
- Write R simulation code (read ahead to Section 2.14) to check your above answers. (When asked to do such a thing, include your R code in your submission.)

## Problem 2

Jack and Jill are playing the board game in Sec. 2.11. Jack has somehow been given a head start, and he is currently (time 0) at square 2, while Jill is at 0. They take turns simultaneously, so at time 1, each of them will be at whatever square his/her first die roll resulted in (including bonus roll, if any). Find the following probabilities concerning the situation at time 1:

- Jack is at square 0.
- Jill has overtaken Jack.
- Neither Jack nor Jill had a bonus roll, if we are told that he is at the same square as Jill.
- Write R simulation code (read ahead to Section 2.14) to check your above answers.