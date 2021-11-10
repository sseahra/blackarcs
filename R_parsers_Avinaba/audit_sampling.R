VERY_LARGE_NUMBER = 12497500 # 5000 choose 2 (pessimistic no. of comparisons)

for(iter in 1:VERY_LARGE_NUMBER){
  runif(1)
}

my_binom_list <- runif(VERY_LARGE_NUMBER)