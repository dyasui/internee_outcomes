;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "outline"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("natbib" "") ("amsfonts" "") ("amsthm" "") ("caption" "singlelinecheck=false") ("enumitem" "") ("setspace" "") ("tikz" "") ("tabularray" "") ("float" "") ("pdflscape" "") ("afterpage" "") ("biblatex" "backend=biber" "style=authoryear-comp") ("adjustbox" "") ("inputenc" "utf8") ("fontenc" "T1") ("graphicx" "") ("longtable" "") ("wrapfig" "") ("rotating" "") ("ulem" "normalem") ("amsmath" "") ("amssymb" "") ("capt-of" "") ("hyperref" "")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "preamble"
    "../tables/reg1_tbl"
    "../tables/reg_altint"
    "../tables/regAB"
    "../tables/reg2_tbl"
    "../tables/reg3_tbl"
    "../figures/wageeffect"
    "../tables/reghet_tbl"
    "article"
    "art12"
    "inputenc"
    "fontenc"
    "graphicx"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "amssymb"
    "capt-of"
    "hyperref")
   (LaTeX-add-labels
    "sec:org7f6828c"
    "sec:org3e5cdba"
    "sec:org65c63af"
    "sec:orgf6e2e7c"
    "sec:orgc8ab16d"
    "tbl:wra_demog"
    "sec:orge3b0c98"
    "tbl:mlp_sumstats"
    "Migration-differences"
    "fig:group-incwage-dist"
    "sec:org2e92206"
    "sec:orgcde17d2"
    "fig:intern_map"
    "eq:predint"
    "sec:orgdba97a7"
    "eq:mainreg"
    "sec:orga78eeed"
    "sec:altint"
    "eq:didreg"
    "sec:org9711a40"
    "sec:orgf45ce46"
    "sec:org57daf4c"
    "fig:internmentmap"
    "fig:1940JAmap"
    "fig:internee-ages"
    "fig:pr_intern"
    "tbl:cmp-cty"))
 :latex)

