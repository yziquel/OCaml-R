ExcludeVars <- c("R_SESSION_TMPDIR","R_HISTFILE")
IncludeVars <- Sys.getenv()
IncludeVars <- IncludeVars[grep("^R_",names(IncludeVars),perl=TRUE)]
cat("(** Functor to properly set up R environment variables. *)\n")
cat("\n")
cat("module type Environment =\n")
cat("sig\n")
cat("  val env : (string * string) list\n")
cat("end\n")
cat("\n")
cat("module Standard : Environment = struct\n")
cat("  let env = [\n")
for (i in 1:length(IncludeVars)){
	if (names(IncludeVars)[i] %in% ExcludeVars)
		next
	cat('    "',names(IncludeVars)[i],'", "',IncludeVars[i],'";\n',sep='')
}
cat("  ]\n")
cat("end\n\n")
