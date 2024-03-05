
FROM ubuntu:22.04 as build

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Europe/Berlin

RUN apt-get update
RUN apt-get -y -qq install software-properties-common
RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN apt-get update --fix-missing

RUN apt-get -y -qq install build-essential cmake gcc-10 mpich libxml2-dev python3 python gfortran wget git graphviz


#MUST
ADD . /build
WORKDIR /build
RUN mkdir /mustinstall
RUN cmake . -DCMAKE_INSTALL_PREFIX=/mustinstall -DCMAKE_BUILD_TYPE=Release && make install -j8 install-prebuilds && export PATH=/mustinstall/bin:$PATH

RUN apt-get autoremove -yq && apt-get clean -yq



FROM ubuntu:22.04 as install
ENV PATH=/mustinstall/bin:$PATH
COPY --from=build /mustinstall /mustinstall
