FROM mysql:8-oraclelinux8
LABEL maintainer "tylern@jlab.org"

ADD calib.sql.gz /docker-entrypoint-initdb.d/
ADD calib_user.sql.gz /docker-entrypoint-initdb.d/

ENV MYSQL_USER clas_offline
ENV MYSQL_ALLOW_EMPTY_PASSWORD yes

EXPOSE 3306 33060
CMD ["mysqld", "--max-connections=10000", "--interactive-timeout=10","--wait-timeout=10"]

