PROJECT = mixpanel

all: app

# Application
rebar:
	wget https://raw.github.com/wiki/rebar/rebar/rebar && chmod u+x rebar

deps: rebar
	@./rebar get-deps

app: deps
	@./rebar compile

clean:
	@./rebar clean
	rm -f test/*.beam
	rm -f erl_crash.dump

# Tests
tests: clean app eunit ct

eunit:
	@./rebar eunit skip_deps=true

ct:
	@./rebar ct skip_deps=true

# Dialyzer
build-plt:
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib mnesia inets crypto public_key ssl

dialyze:
	@dialyzer --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns

.PHONY: app clean
