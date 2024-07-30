IMAGE_NAME=opensuse-addr2line
CWD=`dirname $0`

INTERACTIVE=
if [ -t 0 ]; then
    INTERACTIVE="--interactive"
fi

docker build -t $IMAGE_NAME -f $CWD/docker/Dockerfile-opensuse $CWD/docker
docker run \
    --tty \
    $INTERACTIVE \
    --rm \
    --volume $(dirname $(dirname `which cargo`)):/cargo \
    --env CARGO_HOME=/cargo \
    --volume `pwd`:/addr2line \
    --workdir /addr2line \
    $IMAGE_NAME \
    bash -c "cargo build --release --features bin && ./scripts/benchmark-addr2line.sh"
