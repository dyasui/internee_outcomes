;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "preamble"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("biblatex" "backend=biber" "style=authoryear-comp") ("natbib" "") ("longtable" "") ("wrapfig" "") ("rotating" "") ("hyperref" "") ("amsmath" "") ("amsfonts" "") ("amsthm" "") ("amssymb" "") ("graphicx" "") ("capt-of" "") ("caption" "singlelinecheck=false") ("enumitem" "") ("setspace" "") ("tikz" "") ("float" "") ("ulem" "normalem") ("tabularray" "") ("pdflscape" "") ("afterpage" "") ("adjustbox" "")))
   (TeX-run-style-hooks
    "caption"
    "enumitem"
    "setspace"
    "tikz"
    "float"
    "ulem"
    "tabularray"
    "pdflscape"
    "afterpage"
    "adjustbox")
   (TeX-add-symbols
    '("tinytableTabularrayStrikeout" 1)
    '("tinytableTabularrayUnderline" 1)))
 :latex)

