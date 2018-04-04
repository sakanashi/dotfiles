FROM alpine:3.6

# install basic-libraries/python/awscli from v3.6 repository
RUN apk --update --no-cache add \
     build-base \
     curl \
     gcc \
     git \
     groff \
     libstdc++ \
     make \
     mysql-client \
     ncurses \
     libxml2-dev \
     libxslt-dev \
     libffi-dev \
     zip \
     tree \
     python \
     openssl \
     ruby \
     ruby-bundler \
     ruby-dev \
     ruby-rdoc \
     ruby-irb && \
   wget https://bootstrap.pypa.io/ez_setup.py -O - | python && \
   easy_install awscli s3cmd
