exam_data <- data.frame(
  name = c("John", "Alice", "Bob"),
  score = c(85, 90, 78),
  attempts = c(1, 2, 1),
  qualify = c(TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)
print(exam_data)
exam_data$grade <- c("B", "A", "C")
exam_data <- rbind(
  exam_data,
  data.frame(
    name = "David",
    score = 88,
    attempts = 1,
    qualify = TRUE,
    grade = "B",
    stringsAsFactors = FALSE
  )
)
exam_data <- exam_data[order(exam_data$score), ]
print(exam_data)
write.csv(exam_data, "exam_data.csv", row.names = FALSE)
