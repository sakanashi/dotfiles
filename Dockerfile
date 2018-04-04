FROM alpine:3.6

# install basic-libraries/python/awscli from v3.6 repository
RUN apk --update --no-cache add \
     build-base \
     curl \
     gcc \
     git \
     groff \
     less \
     libffi-dev \
     libstdc++ \
     libxml2-dev \
     libxslt-dev \
     make \
     mysql-client \
     ncurses \
     openssl \
     python \
     ruby \
     ruby-bundler \
     ruby-dev \
     ruby-irb \
     ruby-rdoc \
     tree \
     zip && \
   wget https://bootstrap.pypa.io/ez_setup.py -O - | python && \
   easy_install awscli s3cmd
