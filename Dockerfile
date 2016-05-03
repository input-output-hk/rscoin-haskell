FROM debian:jessie

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 && \
    echo 'deb http://download.fpcomplete.com/debian jessie main'| tee /etc/apt/sources.list.d/fpco.list

RUN apt-get update && apt-get install  -y stack wget autoconf libtool bzip2 && \
    git clone https://github.com/serokell/rscoin.git && cd rscoin && \
    stack setup && \
    stack build && mkdir /app && cp bin/* /app/ && cd / && rm -rvf /rscoin /root/.stack && \
    apt-get remove -y stack wget autoconf libtool bzip2 && \
    apt-get autoremove -y && rm -rvf /var/lib/apt/lists/*

EXPOSE 3000
