all: ../index.html
  
slidesDutch.tmp: slidesDutch.md
	pandoc --self-contained -t slidy -o $@ $<
slidesDutch.html: slidesDutch.tmp pre.html post.html
	cat pre.html slidesDutch.tmp post.html > $@
../index.html: slidesDutch.tmp
	cp slidesDutch.tmp ../index.html
clean:
	rm slide*.html *.tmp

