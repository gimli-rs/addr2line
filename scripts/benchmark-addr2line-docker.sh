IMAGE_NAME=opensuse-addr2line
CWD=`dirname $0`

docker build -t $IMAGE_NAME -f $CWD/docker/Dockerfile-opensuse .
docker run -v `pwd`:/addr2line -t $IMAGE_NAME bash -c "cd /addr2line && cargo build --release --features bin && ./scripts/benchmark-addr2line.sh"
