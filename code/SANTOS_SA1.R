# APM1205 Linear Models — Summative Assessment 1
# Run from the project root: source("code/SANTOS_SA1.R")

pkgs <- c("ggplot2", "car", "lmtest", "broom")
need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(need)) install.packages(need, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(ggplot2); library(car); library(lmtest); library(broom)
})
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

path <- "data/SANTOS_SeoulBikeData.csv"
if (!file.exists(path)) {
  z <- tempfile(fileext = ".zip")
  url <- "https://archive.ics.uci.edu/static/public/560/seoul+bike+sharing+demand.zip"
  download.file(url, z, mode = "wb", quiet = TRUE)
  members <- unzip(z, list = TRUE)$Name
  csv <- members[grepl("SeoulBikeData\\.csv$", members, ignore.case = TRUE)][1]
  if (is.na(csv)) stop("Download SeoulBikeData.csv from the UCI page and place it in data/.")
  unzip(z, files = csv, exdir = "data", junkpaths = TRUE)
  file.rename(file.path("data", basename(csv)), path)
  unlink(z)
}
first <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
sep <- if (grepl(";", first, fixed = TRUE) && !grepl(",", first, fixed = TRUE)) ";" else ","
d <- read.csv(path, sep = sep, fileEncoding = "UTF-8-BOM",
              check.names = FALSE, stringsAsFactors = FALSE)
names(d) <- trimws(sub("^\ufeff", "", names(d)))
needed <- c("Rented Bike Count", "Hour", "Temperature(°C)", "Humidity(%)",
 "Wind speed (m/s)", "Visibility (10m)", "Dew point temperature(°C)",
 "Solar Radiation (MJ/m2)", "Rainfall(mm)", "Snowfall (cm)", "Seasons",
 "Holiday", "Functioning Day")
missing <- setdiff(needed, names(d))
if (length(missing)) stop(paste("Missing expected columns:", paste(missing, collapse=", ")))
d$Date <- as.Date(d$Date, format = "%d/%m/%Y")
d$Seasons <- factor(d$Seasons, levels = c("Winter","Spring","Summer","Autumn"))
d$Holiday <- factor(d$Holiday)
d$`Functioning Day` <- factor(d$`Functioning Day`)
d$Hour <- as.numeric(d$Hour)
d$`Rented Bike Count` <- as.numeric(d$`Rented Bike Count`)
a <- d[complete.cases(d[, needed]), needed]
write.csv(data.frame(Metric=c("Raw rows","Analysis rows","Rows removed"),
 Value=c(nrow(d),nrow(a),nrow(d)-nrow(a))),
 "output/tables/SANTOS_data_quality.csv", row.names=FALSE)
capture.output(summary(a), file="output/tables/SANTOS_descriptive_summary.txt")

# Exploratory data analysis
p1 <- ggplot(a, aes(Hour, `Rented Bike Count`)) +
 geom_point(alpha=.12) + geom_smooth(method="loess") +
 labs(title="Bike rentals by hour", x="Hour", y="Rented bike count") + theme_minimal()
ggsave("output/figures/SANTOS_rentals_by_hour.png", p1, width=8, height=5, dpi=300)
p2 <- ggplot(a, aes(`Temperature(°C)`, `Rented Bike Count`)) +
 geom_point(alpha=.12) + geom_smooth(method="lm") +
 labs(title="Bike rentals and temperature", x="Temperature (°C)", y="Rented bike count") + theme_minimal()
ggsave("output/figures/SANTOS_rentals_vs_temperature.png", p2, width=8, height=5, dpi=300)
p3 <- ggplot(a, aes(Seasons, `Rented Bike Count`)) + geom_boxplot() +
 labs(title="Bike rentals by season", x="Season", y="Rented bike count") + theme_minimal()
ggsave("output/figures/SANTOS_rentals_by_season.png", p3, width=8, height=5, dpi=300)

# Simple linear regression
m1 <- lm(`Rented Bike Count` ~ `Temperature(°C)`, data=a)
capture.output(summary(m1), file="output/tables/SANTOS_simple_model_summary.txt")
write.csv(tidy(m1, conf.int=TRUE), "output/tables/SANTOS_simple_model_coefficients.csv", row.names=FALSE)
write.csv(glance(m1), "output/tables/SANTOS_simple_model_fit.csv", row.names=FALSE)

# Multiple regression
form <- `Rented Bike Count` ~ Hour + `Temperature(°C)` + `Humidity(%)` +
 `Wind speed (m/s)` + `Visibility (10m)` + `Dew point temperature(°C)` +
 `Solar Radiation (MJ/m2)` + `Rainfall(mm)` + `Snowfall (cm)` +
 Seasons + Holiday + `Functioning Day`
m2 <- lm(form, data=a)
capture.output(summary(m2), file="output/tables/SANTOS_multiple_model_summary.txt")
write.csv(tidy(m2, conf.int=TRUE), "output/tables/SANTOS_multiple_model_coefficients.csv", row.names=FALSE)
write.csv(glance(m2), "output/tables/SANTOS_multiple_model_fit.csv", row.names=FALSE)
capture.output(anova(m1,m2), file="output/tables/SANTOS_nested_model_comparison.txt")
capture.output(anova(m2), file="output/tables/SANTOS_multiple_model_anova.txt")

# Least-squares matrix estimate: beta_hat solves min ||y - X beta||^2.
X <- model.matrix(m2); y <- model.response(model.frame(m2))
b <- qr.solve(X,y)
write.csv(data.frame(Term=colnames(X), Estimate=as.numeric(b)),
 "output/tables/SANTOS_least_squares_matrix_estimates.csv", row.names=FALSE)
writeLines(c("Model: y = X beta + epsilon",
 "Least-squares estimate: beta_hat = (X'X)^(-1)X'y when X has full column rank.",
 "R uses QR-based methods for numerical stability.",
 paste("Design matrix:",nrow(X),"rows x",ncol(X),"columns; rank =",qr(X)$rank)),
 "output/tables/SANTOS_matrix_method_notes.txt")

# Dummy variables and ANOVA
m_season <- lm(`Rented Bike Count` ~ Seasons, data=a)
capture.output(anova(m_season), file="output/tables/SANTOS_season_one_way_anova.txt")
write.csv(tidy(m_season, conf.int=TRUE), "output/tables/SANTOS_season_model_coefficients.csv", row.names=FALSE)
m_int <- lm(`Rented Bike Count` ~ Seasons * `Functioning Day`, data=a)
capture.output(anova(m_int), file="output/tables/SANTOS_two_way_anova_interaction.txt")
write.csv(tidy(m_int, conf.int=TRUE), "output/tables/SANTOS_interaction_model_coefficients.csv", row.names=FALSE)
p4 <- ggplot(a, aes(Seasons, `Rented Bike Count`, color=`Functioning Day`,
 group=`Functioning Day`)) + stat_summary(fun=mean, geom="line", linewidth=1) +
 stat_summary(fun=mean, geom="point", size=2) +
 labs(title="Season and functioning-day interaction", x="Season",
 y="Mean rented bike count", color="Functioning day") + theme_minimal()
ggsave("output/figures/SANTOS_season_functioning_interaction.png", p4, width=8, height=5, dpi=300)

# Diagnostics
png("output/figures/SANTOS_multiple_model_diagnostics.png", width=1600, height=1200, res=180)
par(mfrow=c(2,2)); plot(m2); dev.off()
capture.output(bptest(m2), file="output/tables/SANTOS_breusch_pagan_test.txt")
capture.output(vif(m2), file="output/tables/SANTOS_variance_inflation_factors.txt")

# Holdout validation (80/20)
set.seed(1205)
ix <- sample(seq_len(nrow(a)), floor(.8*nrow(a)))
train <- a[ix,]; test <- a[-ix,]
mv <- lm(form, data=train)
pred <- predict(mv, newdata=test)
metrics <- data.frame(Metric=c("Test RMSE","Test MAE"),
 Value=c(sqrt(mean((test$`Rented Bike Count`-pred)^2)),
 mean(abs(test$`Rented Bike Count`-pred))))
write.csv(metrics, "output/tables/SANTOS_validation_metrics.csv", row.names=FALSE)

comparison <- data.frame(
 Model=c("Simple regression","Multiple regression","Season x functioning day"),
 R_squared=c(summary(m1)$r.squared,summary(m2)$r.squared,summary(m_int)$r.squared),
 Adjusted_R_squared=c(summary(m1)$adj.r.squared,summary(m2)$adj.r.squared,summary(m_int)$adj.r.squared),
 AIC=c(AIC(m1),AIC(m2),AIC(m_int)))
write.csv(comparison, "output/tables/SANTOS_model_comparison.csv", row.names=FALSE)
capture.output(comparison, metrics,
 file="output/tables/SANTOS_final_model_summary.txt")
message("Analysis finished. Use output/tables and output/figures to complete the report.")
