all: ../index.html
  
slidesEnglish.tmp: slidesEnglish.md
	pandoc --self-contained -t slidy -o $@ $<
slidesEnglish.html: slidesEnglish.tmp pre.html post.html
	cat pre.html slidesEnglish.tmp post.html > $@
slidesDutch.tmp: slidesDutch.md
	pandoc --self-contained -t slidy -o $@ $<
slidesDutch.html: slidesDutch.tmp pre.html post.html
	cat pre.html slidesDutch.tmp post.html > $@
../index.html: slidesDutch.html slidesEnglish.html
	cp slidesEnglish.html ../index.html
clean:
	rm slide*.html *.tmp

