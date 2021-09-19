all: ../index.html
  
slidesDutch.tmp: slidesDutch.md
	pandoc -t slidy -o $@ $<
slidesDutch.html: slidesDutch.tmp pre.html post.html
	cat pre.html slidesDutch.tmp post.html > $@
../index.html: slidesDutch.html
	cp slidesDutch.html ../index.html
clean:
	rm slide*.html *.tmp

