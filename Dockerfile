FROM haskell:8

WORKDIR /opt/example

RUN apt-get update && \
    apt-get install -y libpq-dev


RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./game.cabal /opt/game.cabal


# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
#RUN cabal install --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/example
RUN cabal install

ADD . .

CMD ["cabal new-run game"]
