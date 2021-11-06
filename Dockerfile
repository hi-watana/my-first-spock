FROM fpco/stack-build

# stackインストール
RUN stack --version

COPY . /app
WORKDIR /app

# stack の初期設定
RUN stack setup
RUN stack --jobs 1 build --fast --pedantic
