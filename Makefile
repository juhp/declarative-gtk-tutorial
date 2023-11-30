PANDOC = pandoc

# --slide-level=2

slidy:
	$(PANDOC) -s -t slidy -o slides.html slides.md

self:
	$(PANDOC) -t slidy --self-contained -o index.html slides.md

upload:
	$(PANDOC) -t slidy --self-contained -o index.html slides.md
	scp index.html slides.md fedorapeople.org:talks/gnome-asia-2023-declaritive-gtk/
