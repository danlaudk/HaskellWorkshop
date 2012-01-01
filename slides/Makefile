all: slides.html slidesDutch.html slidesDutch.tmp

slides.html: slides.md
	pandoc --offline -s -t slidy -o $@ $<
slidesDutch.html: slidesDutch.tmp pre.html post.html
	copy pre.html+slidesDutch.tmp+post.html slidesDutch.html
slidesDutch.tmp: slidesDutch.md
	pandoc -t slidy slidesDutch.md -o $@ $<
clean:
	-rm -f slidesDutch.html
