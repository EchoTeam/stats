all:
	./rebar compile

clean:
	./rebar clean

check: all
	./rebar eunit
