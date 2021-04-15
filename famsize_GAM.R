### SET UP ####################
# set up working space
setwd('/vol/tensusers2/hmueller/PRED_BALDEY/GAMs')

# load file
my_baldey <- read.delim('./my_baldey2.txt', sep='\t')

# load packages (install not installed packages
wants <- c('mgcv', 'dplyr', 'corrplot', 'itsadug', 'mgcViz')
has <- wants %in% rownames(installed.packages())
if(any(!has))install.packages(wants[!has])
lapply(wants, library, character.only = TRUE)

### PREPROCESSING ####################
# logtransform fip
my_baldey$logfip <- log(my_baldey$fip_length + 1)

# transform variables
my_baldey$rcp <- as.character(my_baldey$rcp)
my_baldey$transcription <- as.character(my_baldey$transcription)
my_baldey$subject <- as.factor(my_baldey$subject)
my_baldey$word <- as.factor(my_baldey$word)

# determine words with missing information
my_baldey$include[is.na(my_baldey$logfip) | is.na(my_baldey$family_size) | is.na(my_baldey$family_size_oa)] <- FALSE

# adjust AR.start
for (i in 1:nrow(my_baldey)) {
    if (my_baldey$AR.start[i] & my_baldey$include[i] == FALSE) {
        n=i+1
        while (my_baldey$include[n] == FALSE) {
            n=n+1
        }
        my_baldey$AR.start[n] <- TRUE
    }
}

#subset data
my_family <- my_baldey[my_baldey$include,]

prefixed <- subset(my_family, one_suffix == 0 & one_prefix == 1 & one_compound == 0 & one_circumfix == 0)
suffixed <- subset(my_family, one_suffix == 1 & one_prefix == 0 & one_compound == 0 & one_circumfix == 0)
affixed <- subset(my_family, (one_suffix == 1 | one_prefix == 1) & one_compound == 0 & one_circumfix == 0)
mono <- my_family[(my_family$transcription == my_family$rcp) & my_family$one_suffix == 0 & my_family$one_prefix == 0 & my_family$one_compound == 0 & my_family$one_circumfix == 0,]
all <- rbind(affixed, mono)

### ALL ####################
#scale variables
all_c <- all %>% mutate_each_(list(~scale(.) %>% as.vector),
                                 vars = c("logRT", "logtrial", "logRTprev", 'logfip', 'logword_duration', 'logPhonND', 'logfamily_size', 'logsemantic_family_size', 'logfamily_size_oa', 'logcontinuations', 'logmorph_continuations'))

# explore correlations
continuous_pred = c("logtrial", 'logword_duration', 'logfip', 'logPhonND')
corrplot(cor(all_c[, continuous_pred], use = "complete.obs"), method = "number")

# decorrelation duration, fip and phonND using PCA
prin_comp <- prcomp(all_c[, c('logword_duration', 'logfip', 'logPhonND')], center = TRUE, scale = TRUE)
all_c <- cbind(all_c, prin_comp$x)
continuous_pred = c("logtrial", 'PC1', 'PC2', 'PC3')
corrplot(cor(all_c[, continuous_pred], use = "complete.obs"), method = "number")

### GAMMs

# ML for comparison when fixed effect structure different
# fREML for comparison when only random effect structure different
# fREML: lower=better
# use 'k' to restrict wiggliness

### BASELINE
ALLbase1 <- bam(logRT ~ s(PC1) + s(PC2) + s(PC3) + s(logtrial) + s(PC1, subject, bs='re') + s(PC2, subject, bs='re') + s(PC3, subject, bs='re') + s(logtrial, subject, bs='re'), data=all_c, method='fREML', nthreads=24)

ALLbase2 <- bam(logRT ~ s(PC1) + s(PC2) + s(PC3) + s(logtrial) + s(PC1, subject, bs='re') + s(PC2, subject, bs='re') + s(logtrial, subject, bs='re'), data=all_c, method='fREML', nthreads=24)

compareML(ALLbase1, ALLbase2) # stick with ALLbase2

ALLbase3 <- bam(logRT ~ s(PC1) + s(PC2) + s(PC3) + s(logtrial) + s(PC1, subject, bs='re') + s(PC2, subject, bs='re') + s(logtrial, subject, bs='fs', m=1), data=all_c, method='fREML', nthreads=24)

compareML(ALLbase2, ALLbase3) # stick with ALLbase3

# auto-correlation
crho = as.vector(acf(residuals(ALLbase3), plot=FALSE)$acf)[2] 
reads=24)
ALLbase3a <- bam(logRT ~ s(PC1) + s(PC2) + s(PC3) + s(logtrial) + s(PC1, subject, bs='re') + s(PC2, subject, bs='re') + s(logtrial, subject, bs='fs', m=1), data=all_c, method='fREML', AR.start=all_c$AR.start, rho=crho, nthreads=24)

compareML(ALLbase3, ALLbase3a) # stick with ALLbase3a

### EXPERIMENTAL PREDICTORS

ALLfamily <- bam(logRT ~ s(family_size) + s(PC1) + s(PC2) + s(PC3) + s(logtrial) + s(PC1, subject, bs='re') + s(PC2, subject, bs='re') + s(logtrial, subject, bs='fs', m=1), data=all_c, method='fREML', AR.start=all_c$AR.start, rho=crho, nthreads=24)

compareML(ALLbase3a, ALLfamily) # family size plays a role

# visualization
p_ALLfamily <- getViz(ALLfamily)
print(plot(p_ALLfamily), ask = FALSE)

