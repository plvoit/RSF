In <- read.table("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/9999In/thread1/Output/River_Flow.out", quote="\"", comment.char="", skip = 1, header = T)
Out <- read.table("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/9999Out/thread1/Output/River_Flow.out", quote="\"", comment.char="", skip = 1, header = T)
NoIrri  <- read.table("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/UrucuiaNoIrri/thread1_best/Output/River_Flow.out", quote="\"", comment.char="", skip = 1, header = T)

plot(In$X15, type = "l", ylim = c(0,3500), cex.axis = 0.8, ylab = expression("Runoff [m" ^3*"]"),
     xlab = "Year", cex.lab = 0.8, cex.axis = 0.8)
lines(Out$X15, type = "l", col = "red" )
lines(NoIrri$X15, type = "l", col = "blue")
legend("topright", legend = c("Input", "Output","No irrigation"), col = c("black", "red", "blue"), cex = 0.8,  lty=1)


sum(c(17539.4121,163435.359,11667488,7096.49756,10930.666,163106.422,231813.75,4827368.5,30105.9023,19025.0098,9011.99023,2145229.75,63216.8594,17399.2969,21586.9277,13843.1885,23651.2246,38988.3359,6930296))
