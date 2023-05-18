label_pvalue_manual <- Vectorize(function(x, signifSign = "\u22C6", type = 1, digits = 2) {
  itype <- pmatch(type, c("katex", "latex2exp", "richtext", "plain", "latex"))
  if (is.na(itype)) stop('"type" must be one of "katex", "latex2exp", "richtext", "plain" or "latex".')
  
  signifSign <- if (x > 0.1) {
    ""
  } else if (x > 0.05) {
    "."
  } else if (x > 0.005) {
    paste0(rep(signifSign, 1), collapse = "")
  } else if (x > 0.0005) {
    paste0(rep(signifSign, 2), collapse = "")
  } else {
    paste0(rep(signifSign, 3), collapse = "")
  }
  
  if (is.na(x)) return(NA)
  
  if (x == 0) {
    out <- "0"
  } 
  else {
    negativeFlag <- sign(x) == -1
    
    x <- abs(x)
    log10Base <- log10(x)
    
    if (between(log10Base, -2, 3)) {
      
      out <- if (log10Base < 0) as.character(signif(x, digits)) else as.character(round(x, digits))
      
    } 
    else {
      
      log10Base <- floor(log10Base)
      x <- x * 10^-log10Base
      
      out <- if (itype %in% c(1:2, 5)) {
        if (round(x, digits) == 1) {
          paste0("10^{", log10Base, "}")
        } 
        else {
          paste0(round(x, digits), " \\times 10^{", log10Base, "}")
        }
      }
      else if (itype == 3) {
        if (round(x, digits) == 1) {
          paste0("10<sup>", log10Base, "</sup>")
        } 
        else {
          paste0(round(x, digits), " \u00b7 10<sup>", log10Base, "</sup>")
        }
      }
      else if (itype == 4) {
        if (round(x, digits) == 1) {
          paste0("10^", log10Base, "")
        } 
        else {
          paste0(round(x, digits), " \u00b7 10^", log10Base, "")
        }
      }
      else {
        stop(paste0("Unimplemented superscript format for type: ", type))
      }
    }
    
    out <- str_remove(out, "^1 ")
    
    if (negativeFlag) out <- paste("-", out)
    
  }
  
  if (itype == 1) {
    if (signifSign != "" & signifSign != ".") {
      signifSign <- paste0("\\hphantom{} ^{", signifSign, "}")
    }
    katex::katex_html(paste0(out, signifSign), preview = T)
  }
  else if (itype == 2) {
    as.character(latex2exp::TeX(paste0("$", out, "$", signifSign)))
  }
  else if (itype == 3) {
    paste0("<span>", out, " ", signifSign, "</span>")
  }
  else if (itype == 4) {
    paste0(out, signifSign)
  }
  else if (itype == 5) {
    paste0("$", out, "\\;", signifSign, "$")
  }
  else {
    stop(paste0("Unimplemented return format for type: ", type))
  }
}, USE.NAMES = F, SIMPLIFY = T)
