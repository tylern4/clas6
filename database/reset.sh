
mysql -h 172.21.139.25 -u root -P 3306 -e 'SET GLOBAL max_connections = 100000; SET GLOBAL wait_timeout = 10; SET GLOBAL interactive_timeout = 10;SHOW VARIABLES LIKE "max_connections"; SHOW VARIABLES LIKE "wait_timeout"; SHOW VARIABLES LIKE "interactive_timeout";'
