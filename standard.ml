module Standard : Environment = struct

  let name="OCaml-R"
  let options = ["--vanilla"; "--slave"]
  let signal_handlers = false
  let env = [
    "R_ARCH", "";
    "R_BROWSER", "xdg-open";
    "R_BZIPCMD", "/bin/bzip2";
    "R_DOC_DIR", "/usr/share/R/doc";
    "R_DVIPSCMD", "/usr/bin/dvips";
    "R_GZIPCMD", "/bin/gzip";
    "R_HOME", "/usr/lib64/R";
    "R_INCLUDE_DIR", "/usr/share/R/include";
    "R_LATEXCMD", "/usr/bin/latex";
    "R_LIBS_SITE", "/usr/local/lib/R/site-library:/usr/lib/R/site-library:/usr/lib/R/library";
    "R_LIBS_USER", "~/R/x86_64-pc-linux-gnu-library/2.10";
    "R_MAKEINDEXCMD", "/usr/bin/makeindex";
    "R_PAPERSIZE", "letter";
    "R_PAPERSIZE_USER", "a4";
    "R_PDFLATEXCMD", "/usr/bin/pdflatex";
    "R_PDFVIEWER", "/usr/bin/xdg-open";
    "R_PLATFORM", "x86_64-pc-linux-gnu";
    "R_PRINTCMD", "/usr/bin/lpr";
    "R_RD4DVI", "ae";
    "R_RD4PDF", "times,hyper";
    "R_SHARE_DIR", "/usr/share/R/share";
    "R_TEXI2DVICMD", "/usr/bin/texi2dvi";
    "R_UNZIPCMD", "/usr/bin/unzip";
    "R_ZIPCMD", "/usr/bin/zip";
  ]

end
