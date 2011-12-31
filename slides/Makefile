all: slides.html slidesDutch.html

slides.html: slides.md
	pandoc --offline -s -t slidy -o $@ $<
slidesDutch.html: slidesDutch.md
	pandoc --offline -s -t slidy -o $@ $<
    
clean:
	-rm -f slidesDutch.html
