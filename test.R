library(devtools)

load_all()

x = c("bounded-strongly-correlated", "uncorrelated", "similar-weights")
y = str_to_shortcut(x, sep = "", split = "-")
print(y)

stop()

x = c(
  "BenchmarkTTP1_n100_p0.8_c20_01.csv",
  "BenchmarkTTP1_n100_p0.8_c15_1943.csv")

x = gsub(".csv", "", x)

res = parseString(x, split = "_", which = 2:5, types = "inii", names = c("n", "p", "c", "repl"), as.df = TRUE)
