all:    slidesDutch.html ../index.html
  
slidesDutch.html: slidesDutch.tmp pre.html post.html
	cat pre.html slidesDutch.tmp post.html > $@
slidesDutch.tmp: slidesDutch.md
	pandoc -t slidy -o $@ $<
../index.html: slidesDutch.html
	cp slidesDutch.html ../index.html
clean:
	rm slide*.html *.tmp

knit: slidesKnit.html
slidesKnit.html: slidesDutch.md
	pandoc -s -S -t dzslides --mathjax -o $@ $<