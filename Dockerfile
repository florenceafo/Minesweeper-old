FROM haskell:8

WORKDIR /opt/minesweeper

ADD . /opt/minesweeper

RUN apt-get update && \
    apt-get install -y libpq-dev


RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./game.cabal /opt/minesweeper/game.cabal


# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
# COPY ./Main.hs /opt/minesweeper/Main.hs
# COPY ./game /opt/minesweeper/game.cabal
COPY . /opt/minesweeper
RUN cabal install --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/minesweeper
RUN cabal install --overwrite-policy=always


#ENV PATH /opt/minesweeper
#ENV PATH="/opt/gtk/bin:${PATH}"
#COPY --from=build /opt/build/bin .
EXPOSE $PORT

CMD ["/opt/minesweeper/start.sh", $PORT]
