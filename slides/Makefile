all: slides.html slidesDutch.html ../index.html

slides.html: slides.md
	pandoc --offline -s -t slidy -o $@ $<
slidesDutch2.html: slidesDutch2.tmp pre.html post.html
	cp pre.html slidesDutch2.tmp post.html > $@
slidesDutch2.tmp: slidesDutch.md
	pandoc -t slidy -o $@ $<
slidesDutch.html: slidesDutch.md
	pandoc --offline -s -t slidy -o $@ $<
../index.html: slidesDutch.html
	cp slidesDutch.html ../index.html
clean:
	rm slide*.html *.tmp
