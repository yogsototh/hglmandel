#!/usr/bin/env zsh

function writeTOC() {
    # Create the TOC
cat <<END
> <center><hr style="width:30%;float:left;border-color:#CCCCD0;margin-top:1em"/><span class="sc"><b>Table of Content</b></span><hr style="width:30%;float:right;border-color:#CCCCD0;margin-top:1em"/></center>
> 
> begindiv(toc)
>
END


    # get a list of 
    # depth anchor name
    grep -e '<h.' **/*.lhs | perl -pe 's#.*<h([2-6]) id="#\1 #;s#"[^>]*># "#; s#<.*#"#' |
    while read num anchor title; do
        print -n -- '> '
        while ((num-->2)); do print -n -- "  "; done
        print -- '* <a href="#'$anchor'">'${title[2,-2]}'</a>'
    done

cat <<END
>
> enddiv
END
}

cat <<END
-----
isHidden:       false
menupriority:   1
kind:           article
created_at:     2012-02-08T15:17:53+02:00
en: title: Haskell OpenGL Mandelbrot
en: subtitle: A kind of real world example
fr: title: Afficher mandelbrot en OpenGL avec Haskell
fr: subtitle: Un exemple d'utilisation d'Haskell
author_name: Yann Esposito
author_uri: yannesposito.com
tags:
  - Haskell
  - programming
  - functional
  - tutorial
-----
blogimage("mandelbrot_3D.png","3D Mandelbrot Set")

begindiv(intro)

en: %tldr A progressive real world example.
fr: %tlal Un exemple progressif de programmation avec Haskell.

END

writeTOC

cat <<END

enddiv

END


for fic in **/*.lhs(.N); do
    contains_haskell=$(( $( egrep '^>' $fic | wc -l) > 0 ))
    ((contains_haskell)) && \
        print -- "\n<hr/><a href=\"code/$fic\" class=\"cut\">${fic:h}/<strong>${fic:t}</strong></a>\n"
    cat $fic
    ((contains_haskell)) && \
        print -- "\n<a href=\"code/$fic\" class=\"cut\">${fic:h}/<strong>${fic:t}</strong> </a>\n"
done | perl -pe 'BEGIN{$/="";} s#((^>.*\n)+)#<div class="codehighlight">\n<code class="haskell">\n$1</code>\n</div>\n#mg' | perl -pe 's#^> ?##' | perl -pe 's/^ #/#/'
