.PHONY: all run test clean

REBAR=$(shell which rebar || echo ./rebar)

all: 	$(REBAR)
		$(REBAR) get-deps
		$(REBAR) compile

run:
		erl +Bd -noinput -noshell -sname erlysocks \
			-boot start_sasl -config sys.config -s lager -s erlysocks_app

test:
		$(REBAR) eunit skip_deps=true

clean:
		$(REBAR) clean
		$(REBAR) delete-deps
		rm -rf ./rebar
		rm -rf ./ebin
		rm -rf ./deps
		rm -rf ./log
		rm -rf ./erl_crash.dump

# Get rebar if it doesn't exist

REBAR_URL=https://cloud.github.com/downloads/basho/rebar/rebar

./rebar:
	erl -noinput -noshell -s inets -s ssl \
		-eval '{ok, _} = httpc:request(get, {"${REBAR_URL}", []}, [], [{stream, "${REBAR}"}])' \
		-s init stop
	chmod +x ${REBAR}
