DISTRIBUTION = debian
RELEASE = latest
VERSION := $(shell cat ../../VERSION)
TARNAME := macaulay2_$(VERSION)

deb: .image-built-$(DISTRIBUTION)-$(RELEASE) $(TARNAME).orig.tar.gz
	tar -xf $(TARNAME).orig.tar.gz
	cp -r debian $(TARNAME)
	sudo chown -R 1000:0 $(CURDIR)
	docker run \
		--entrypoint bash \
		-v $(CURDIR):/home/macaulay/ \
		m2-$(DISTRIBUTION)-$(RELEASE)-build \
		-c 'cd $(TARNAME) && dpkg-buildpackage'

# build tarball (essentially the same as "make dist")
$(TARNAME).orig.tar.gz:
	cd ../../.. && git archive --prefix=$(TARNAME)/ \
		-o $(CURDIR)/M2.tar HEAD
	git submodule update --init ../../Macaulay2/editors/emacs
	cd ../../Macaulay2/editors/emacs && \
		git archive \
			--prefix=$(TARNAME)/M2/Macaulay2/editors/emacs/ \
			-o $(CURDIR)/M2-emacs.tar HEAD
	tar --concatenate --file=M2.tar M2-emacs.tar
	gzip M2.tar
	mv M2.tar.gz $(TARNAME).orig.tar.gz
	rm *.tar

.image-built-%:
	$(MAKE) -C ../docker/$(DISTRIBUTION) build-image \
		RELEASE=$(RELEASE)
	touch $@

clean:
	rm -rf .image-built-* macaulay2-$
