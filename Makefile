.DEFAULT_GOAL := help

clean: ## Clean up build artefacts
	@cd ./producer && stack clean
	@cd ./consumer && stack clean

build: clean install ## Build producer and consumer Docker containers
	@cp ~/.local/bin/producer ./producer/tmp
	@cd ./producer && docker build --no-cache --rm -t producer .
	@cp ~/.local/bin/consumer ./consumer/tmp
	@cd ./consumer && docker build --no-cache --rm -t consumer .

install: ## Compile producer and consumer
	@cd ./producer && stack install
	@cd ./consumer && stack install

up: build ## Start services
	@docker-compose up

help: ## Print available tasks
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY:
	build
	clean
	install
	up
	help
