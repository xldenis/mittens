mysql -P 3000 -h 0.0.0.0  -uroot < config/init.sql
mysql -P 5000 -h 0.0.0.0  -uroot < config/init_master.sql
sysbench oltp_read_write prepare --mysql-user=root --mysql-host='0.0.0.0' --mysql-port=5000 --time=100 --db-ps-mode=disable --threads=4 --tables=100
