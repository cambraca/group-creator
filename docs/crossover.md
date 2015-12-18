21 people (0 to 20)
"individuals" (ways of grouping the people): A, B
result individual: C

A: 8,10,0|3,16,4,13|7,20|17,15,2|12|1,6,19|9,14,18|5,11
B: 11,8|14,18,0,4|2,15,1|5,20,10|12,3|6,19,17|9,13,16,7

crossover algorithm:

1. start from left in A, pick the first group from which 
   all people haven't been selected yet (the first time
   it'll be the first group)
   
   C: 8,10,0
   
2. start from the right in B, do the same

   C: 8,10,0|9,13,16,7

3. go back to 1 and keep going until there are no more left

   C: 8,10,0|9,13,16,7|17,15,2|12,3|1,6,19|5,11

4. now start handling group "parts" (people left are 4,14,18,20).
   using the same order, join the rest of the groups in "pairs".
   the groups left are, in order:
   
   3,16,4,13|6,19,17|7,20|5,20,10|12|2,15,1|9,14,18|14,18,0,4|11,8
   
   filtering by only the people left:
   
   4||20|20|||14,18|14,18,4|
   
   removing empty groups:

   4|20|20|14,18|14,18,4
   
   joining by pairs:
   
   4,20|20,14,18|14,18,4
   
   removing duplicate people:
   
   4,20|14,18|
   
   and, again, removing empty groups:
   
   4,20|14,18
   
   we append these groups to the result:
   
   C: 8,10,0|9,13,16,7|17,15,2|12,3|1,6,19|5,11|4,20|14,18
   
   et voilà!