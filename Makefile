ERL=erl
REBAR=./rebar


all: compile

compile: 
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -fv erl_crash.dump
