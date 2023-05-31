require(ggpubr)

theme_set(
  theme() +
    theme_pubr(
      legend = "right",
      base_family = "CMU Serif"
    ) +
    theme(
      title = element_text(face = "bold",
                           size = 16),
      strip.text = element_text(face = "bold",
                                size = 16),
      plot.title = element_text(face = "plain",
                                size = 20,
                                hjust = .5),
      legend.title = element_text(hjust = .5),
      aspect.ratio = 1
    )
)

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

format_num_for_table <- Vectorize(function(n, digits = .1, nsmall = 1) {
  if (is.na(n) | is.nan(n) | !is.numeric(n)) stop('"n" must be a real defined number.')
  if (!is.finite(n)) {
    if (sign(n) == -1) return("$-\\infty$") else return("$\\infty$")
  }
  paste0("$", format(n, nsmall = nsmall, digits = digits), "$")
}, USE.NAMES = F, SIMPLIFY = T)


sign0 <- function(x) {
  tryCatch({
    s <- sign(x)
    s[s == 0] <- 1
    s
  },
  error = function(DUMMY) print(x))
}

abslog_breaks <- function(base, n = 10) {
  n_default = n
  function(x, n = n_default) {
    normal_range <- range(x, na.rm = T)
    if (normal_range[1] < 0 & normal_range[2] > 0) {
      if (n %% 2 == 0) n <- n - 1
      ns <- floor(n / 2)
      negative_breaks <- -rev(scales::log_breaks(n = ns, base = base)(c(1, abs(normal_range[1]))))
      positive_breaks <- scales::log_breaks(n = ns, base = base)(c(1, normal_range[2]))
      
      return(c(negative_breaks, 0, positive_breaks))
    }
    else if (all(normal_range <= 0)) {
      negative_range <- rev(abs(normal_range))
      includes_zero <- negative_range[1] == 0
      if (includes_zero) {
        negative_range[1] <- 1
        return(c(-rev(scales::log_breaks(n = n - 1, base = base)(negative_range)), 0))
      }
      else {
        return(-rev(scales::log_breaks(n = n, base = base)(negative_range))) 
      }
    }
    else if (all(normal_range >= 0)) {
      includes_zero <- normal_range[1] == 0
      if (includes_zero) {
        normal_range[1] <- 1
        return(c(0, scales::log_breaks(n = n, base = base)(normal_range)))
      }
      else {
        return(scales::log_breaks(n = n, base = base)(normal_range)) 
      }
    }
    else {
      stop(paste0("Weird range: {", range[1], "; ", range[2], "}!"))
    }
  }
}

abslog <- function(base, mult = 1, add = 1) {
  scales::trans_new(
    name = "abslog",
    transform = function(x) {
      sign0(x) * log(abs(x * mult + add * sign0(x)), base)
    },
    inverse = function(x) {
      (base^abs(x) - add) * sign0(x) / mult
    },
    breaks = abslog_breaks(base = base),
    domain = c(-Inf, Inf)
  )
}

label_prettify_scientific <- function(parse, digits, baseRange = c(-1, 2)) {
  Vectorize(function(x) {
    if (is.null(x) || is.na(x)) return(NA)
    if (is.numeric(x)) {
      if (x != 0) {
        negative <- sign(x) == -1
        if (negative) x <- abs(x)
        base <- log10(x)
        base <- sign(base) * floor(abs(base))
        x <- if (baseRange[2] < base | base < baseRange[1]) {
          formatC(x, digits = digits, format = "e", drop0trailing = TRUE)
        }
        else {
          formatC(x, digits = digits, format = "f", drop0trailing = TRUE)
        }
        if (negative) x <- paste0("-", x)
      }
      else {
        x <- "0"
      }
    }
    if (!is.character(x)) stop(paste0("Cannot parse type of \"", class(x), "\"."))
    if (!str_detect(x, "e\\+|e\\-")) return(x)
    out <- if (str_detect(x, "e\\+")) {
      parts <- str_split_fixed(x, "(?=e)", 2)
      parts[1] <- round(as.numeric(parts[1]), digits = digits)
      x <- paste0(parts, collapse = "")
      paste0("$", str_replace(x, "((?<=^-{0,1})1)*e\\+", " \\\\times\\\\, 10^{"),"}$")
    }
    else if (str_detect(x, "e\\-")) {
      parts <- str_split_fixed(x, "(?=e)", 2)
      parts[1] <- round(as.numeric(parts[1]), digits = digits)
      x <- paste0(parts, collapse = "")
      paste0("$", str_replace(x, "((?<=^-{0,1})1)*e\\-", " \\\\times\\\\, 10^{-"),"}$")
    }
    else {
      stop("Invalid format!")
    }
    if (parts[1] == 0) return("0")
    out <- str_remove(out, "(?<=^\\$)-{0,1} \\\\times\\\\, ")
    if (parse) return(latex2exp::TeX(out)) else out
  })
}

label_prettify_scientific_beta <- function(parse, digits, baseRange = c(-1, 2), close = .01) {
  Vectorize(function(x) {
    close_to_one <- F
    if (is.null(x) || is.na(x)) return(NA)
    if (is.numeric(x)) {
      if (x != 0) {
        close_to_one <- (1 - x) < close
        if (close_to_one) x <- 1 - x
        base <- log10(x)
        base <- sign(base) * floor(abs(base))
        x <- if (baseRange[2] < base | base < baseRange[1]) {
          formatC(x, digits = digits, format = "e", drop0trailing = TRUE)
        }
        else {
          formatC(x, digits = digits, format = "f", drop0trailing = TRUE)
        }
      }
      else {
        x <- "0"
      }
    }
    if (!is.character(x)) stop(paste0("Cannot parse type of \"", class(x), "\"."))
    if (!str_detect(x, "e\\+|e\\-")) return(x)
    out <- if (str_detect(x, "e\\+")) {
      parts <- str_split_fixed(x, "(?=e)", 2)
      parts[1] <- round(as.numeric(parts[1]), digits = digits)
      x <- paste0(parts, collapse = "")
      paste0(str_replace(x, "((?<=^-{0,1})1)*e\\+", " \\\\times\\\\, 10^{"),"}")
    }
    else if (str_detect(x, "e\\-")) {
      parts <- str_split_fixed(x, "(?=e)", 2)
      parts[1] <- round(as.numeric(parts[1]), digits = digits)
      x <- paste0(parts, collapse = "")
      paste0(str_replace(x, "((?<=^-{0,1})1)*e\\-", " \\\\times\\\\, 10^{-"),"}")
    }
    else {
      stop("Invalid format!")
    }
    if (parts[1] == 0) return("0")
    out <- str_remove(out, "(?<=^)-{0,1} \\\\times\\\\, ")
    if (close_to_one) out <- paste0("1 - ", out)
    out <- paste0("$", out, "$")
    if (parse) return(latex2exp::TeX(out)) else out
  })
}

prettify_scientific <- label_prettify_scientific(
  parse = TRUE, 
  digits = 2, 
  baseRange = c(-2, 2)
)

align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]
  
  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
  
  # there can be multiple guides within one legend box  
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]
    
    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2
    
    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }
  
  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
} 