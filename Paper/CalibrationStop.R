dds <- read.delim("C:/Users/Admin/Desktop/dds.log")


cleaned <- c()
count <- 1

for(i in 2:(length(dds$objective_function)-1)){
  if(dds$objective_function[i+1] <= dds$objective_function[i]){
    cleaned[count] <- dds$objective_function[i +1]
    count <- count + 1}
}

plot(cleaned, type = "l")


dds <- dds[(nrow(dds)-100):nrow(dds),]

min(dds$objective_function)

sorted <- sort(dds$objective_function, decreasing = F)

plot(sorted)
