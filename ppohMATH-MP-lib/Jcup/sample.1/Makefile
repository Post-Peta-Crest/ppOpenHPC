include Mkinclude

all:
	cd $(COMMONDIR) ; make ; cd ..
	cd $(LANDDIR) ; make ; cd ..
	cd $(ICEDIR) ; make ; cd ..
	cd $(ATMDIR) ; make ; cd ..
	cd $(OCNDIR) ; make ; cd ..
	cd $(CHMDIR) ; make ; cd ..

clean:
	cd bin ; rm * ; cd ../
	cd $(LANDDIR) ; make clean ; cd ../
	cd $(ICEDIR) ; make clean ; cd ../
	cd $(ATMDIR) ; make clean ; cd ../
	cd $(OCNDIR) ; make clean ; cd ../
	cd $(CHMDIR) ; make clean ; cd ../
	cd $(CCNDIR) ; make clean ; cd ../
	cd $(COMMONDIR) ; make clean ; cd ../
