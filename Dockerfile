FROM fpco/stack-build:latest as build

  WORKDIR /root/project
  COPY stack.yaml .
  RUN stack setup

  COPY package.yaml .
  RUN stack build --only-dependencies

  COPY . .
  RUN stack install

FROM phusion/baseimage:latest

  RUN apt-get update && apt-get install -y libmysqlclient-dev

  COPY --from=build /root/.local/bin/mittens /
  CMD /mittens --source-host=$SOURCE_HOST --source-user=$SOURCE_USER --source-port=$SOURCE_PORT --source-password=$SOURCE_PASSWORD --source-database=$SOURCE_DATABASE --dest-host=$DEST_HOST --dest-user=$DEST_USER --dest-port=$DEST_PORT --dest-password=$DEST_PASSWORD --dest-database=$DEST_DATABASE
