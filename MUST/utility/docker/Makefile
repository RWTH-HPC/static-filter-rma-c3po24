registry = registry.git-ce.rwth-aachen.de/hpc-research/correctness/must

all: image-ci image-formatting

tag-ci = $(registry)/ci

image-ci:
	docker build --pull -t $(tag-ci) -f Dockerfile-ci .

push-ci: image-ci login
	docker push $(tag-ci)

tag-formatting = $(registry)/ci/formatting

image-formatting:
	docker build --pull -t $(tag-formatting) -f Dockerfile-formatting .

push-formatting: image-formatting login
	docker push $(tag-formatting)

login:
	docker login registry.git-ce.rwth-aachen.de
