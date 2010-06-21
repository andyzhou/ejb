compile: 
	cp -f ./src/ejb.app ./ebin/ejb.app
	cd src && erl -make -smp -Wall

clean:
	@rm -rf ebin/*.beam