list(type = "MONOLIX", method = "SAEM", path = ".", parameters = list(
    MONOLIXversion = "5.1.0", names = c(Te = "Te", p = "p", "omega(Te)" = "omega(Te)", 
    "omega(p)" = "omega(p)"), values = c(Te = 52.058866, p = 14.654815, 
    "omega(Te)" = 0.646833, "omega(p)" = 0.2), stderrors = c(Te = 4.37455621, 
    p = 1.30966038, "omega(Te)" = 0.07314029, "omega(p)" = 0), 
    rse = c(Te = 8.403095, p = 8.936724, "omega(Te)" = 11.307446, 
    "omega(p)" = 0), FLAGestimated = c(Te = 1, p = 1, "omega(Te)" = 1, 
    "omega(p)" = 0), correlationmatrix = structure(c(1, 0.031348, 
    0.441, 0, 0.031348, 1, 0.11829, 0, 0.441, 0.11829, 1, 0, 
    0, 0, 0, 1), .Dim = c(4L, 4L), .Dimnames = list(c(Te = "Te", 
    p = "p", "omega(Te)" = "omega(Te)", "omega(p)" = "omega(p)"
    ), c(Te = "Te", p = "p", "omega(Te)" = "omega(Te)", "omega(p)" = "omega(p)"
    ))), covariancematrix = structure(c(19.1367420344496, 0.179598427063934, 
    0.141100732630929, 0, 0.179598427063934, 1.71521031094174, 
    0.011330873711974, 0, 0.141100732630929, 0.0113308737119745, 
    0.00534950202128408, 0, 0, 0, 0, 0), .Dim = c(4L, 4L), .Dimnames = list(
        c(Te = "Te", p = "p", "omega(Te)" = "omega(Te)", "omega(p)" = "omega(p)"
        ), c(Te = "Te", p = "p", "omega(Te)" = "omega(Te)", "omega(p)" = "omega(p)"
        )))), objectivefunction = list(OBJ = 465.418, AIC = 471.418, 
    BIC = 479.234, BICc = 480.62), trans_randeffects = c("exp(phi)", 
"exp(phi)"), inv_trans_randeffects = c("log(psi)", "log(psi)"
), covariates = list(covNames = character(0), covTransformation = structure(list(), .Names = character(0)), 
    catNames = character(0), catReference = structure(list(), .Names = character(0)), 
    catCategories = structure(list(), .Names = character(0))), 
    PROJECTINFO = list(COMMENT = "Weibull", TOOL = "MONOLIX", 
        TOOLVERSION = "MLX2019R1", DATA = "../../01-04a-X2-TTE/data.csv", 
        DOSINGTYPES = "", COVNAMES = "WT", CATNAMES = "SEXF", 
        CATCATEGORIES = "[]", REGRESSIONNAMES = "", PARAMNAMES = c("Te", 
        "p"), PARAMTRANS = c("exp(phi)", "exp(phi)"), PARAMINVTRANS = c("log(psi)", 
        "log(psi)"), COVARIATENAMES = c("WT", "SEXF"), COVARIATESUSED = "", 
        BETACOVNAMES = "", BETACOVTRANS = "", BETACATNAMES = "", 
        BETACATREFERENCE = ""), rawParameterInfo = list(fixedEffects = list(
        names = c(Te = "Te", p = "p"), values = c(Te = 52.058866, 
        p = 14.654815), stderr = c(Te = 4.37455621, p = 1.30966038
        ), rse = c(Te = 8.403095, p = 8.936724), estimated = c(Te = 1, 
        p = 1), distribution_info = c("log(psi)", "log(psi)")), 
        randomEffects = list(names = c("omega(Te)" = "omega(Te)", 
        "omega(p)" = "omega(p)"), values = c("omega(Te)" = 0.646833, 
        "omega(p)" = 0.2), stderr = c("omega(Te)" = 0.07314029, 
        "omega(p)" = 0), rse = c("omega(Te)" = 11.307446, "omega(p)" = 0
        ), estimated = c("omega(Te)" = 1, "omega(p)" = 0)), correlation = list(
            names = structure(character(0), .Names = character(0)), 
            values = structure(numeric(0), .Names = character(0)), 
            stderr = structure(numeric(0), .Names = character(0)), 
            rse = structure(numeric(0), .Names = character(0)), 
            estimated = structure(numeric(0), .Names = character(0))), 
        covariate = list(names = structure(character(0), .Names = character(0)), 
            values = structure(numeric(0), .Names = character(0)), 
            stderr = structure(numeric(0), .Names = character(0)), 
            rse = structure(numeric(0), .Names = character(0)), 
            estimated = structure(numeric(0), .Names = character(0)))))
