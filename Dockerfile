FROM ubuntu-latest as builder

# Install dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    ghc \
    cabal-install

RUN cabal update

COPY . /app

WORKDIR /app

RUN cabal install --only-dependencies

RUN cabal build

RUN mv ./dist/build/marcel/Marcel ./marcel-bin

FROM ubuntu-latest as runner

COPY --from=builder /app/marcel-bin /usr/local/bin/marcel

