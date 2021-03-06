SHELL         := /usr/bin/env bash
NAME          := gundeck
VERSION       := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER  ?= 0
BUILD_LABEL   ?= local
BUILD         := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
EXE_IT        := dist/$(NAME)-integration
EXE_TESTS     := dist/$(NAME)-tests
EXE_BENCH     := dist/$(NAME)-bench
EXE_SCHEMA    := dist/$(NAME)-schema
DEB           := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DEB_IT        := $(NAME)-integration_$(VERSION)+$(BUILD)_amd64.deb
DEB_SCHEMA    := $(NAME)-schema_$(VERSION)+$(BUILD)_amd64.deb
SDIST         := dist/$(NAME)-$(VERSION).tar.gz
KEIRETSU_ENV  ?= ../.env

default: clean install

.PHONY: init
init:
	mkdir -p dist

.PHONY: install
install: init
	stack install --pedantic --test --bench --no-run-benchmarks --local-bin-path=dist
	cp "$(shell stack path --dist-dir)/build/$(NAME)-integration/$(NAME)-integration" dist/
	cp "$(shell stack path --dist-dir)/build/$(NAME)-tests/$(NAME)-tests" dist/
	cp "$(shell stack path --dist-dir)/build/$(NAME)-bench/$(NAME)-bench" dist/

.PHONY: compile
compile:
	stack build --pedantic --test --bench --no-run-benchmarks --no-copy-bins

.PHONY: clean
clean:
	stack clean
	-rm -rf dist
	-rm -f .metadata

.PHONY: sdist
sdist: init $(SDIST)

$(SDIST):
	stack sdist
	cp "../../libs/$(NAME)-types/$(shell stack path --dist-dir)/$(NAME)-types-$(VERSION).tar.gz" dist/
	cp "$(shell stack path --dist-dir)/$(NAME)-$(VERSION).tar.gz" dist/

.PHONY: dist
dist: sdist install $(DEB) $(DEB_IT) $(DEB_SCHEMA) .metadata

$(DEB):
	makedeb --name=$(NAME) \
	 --version=$(VERSION) \
	 --debian-dir=deb \
	 --build=$(BUILD) \
	 --architecture=amd64 \
	 --output-dir=dist

$(DEB_IT):
	makedeb --name=$(NAME)-integration \
	 --version=$(VERSION) \
	 --debian-dir=deb-it \
	 --build=$(BUILD) \
	 --architecture=amd64 \
	 --output-dir=dist

$(DEB_SCHEMA):
	makedeb --name=$(NAME)-schema \
		--version=$(VERSION) \
		--debian-dir=schema/deb \
		--build=$(BUILD) \
		--architecture=amd64 \
		--output-dir=dist

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata

.PHONY: test
test: install
	$(EXE_TESTS)

.PHONY: bench
bench: install
	mkdir -p dist/bench
	$(EXE_BENCH) --output dist/bench/result.html

.PHONY: integration
integration: install
	LOG_LEVEL=Info keiretsu --run $(EXE_IT) --delay 2000 --env $(KEIRETSU_ENV)

.PHONY: integration-list
integration-list: install
	LOG_LEVEL=Info keiretsu --run '$(EXE_IT) -l; sleep 1' --env $(KEIRETSU_ENV)

integration-%: install
	LOG_LEVEL=Info keiretsu --run '$(EXE_IT) --pattern="$*"' --env $(KEIRETSU_ENV)

.PHONY: db
db: db-reset

.PHONY: db-reset
db-reset: install
	$(EXE_SCHEMA) --keyspace $(NAME)_test --replication-factor 1 --reset

.PHONY: db-migrate
db-migrate: install
	$(EXE_SCHEMA) --keyspace $(NAME)_test --replication-factor 1
