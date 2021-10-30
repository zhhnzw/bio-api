### 准备工作

挂载磁盘

```bash
# 在 node-1 上执行
$ mkdir /mnt/disks
$ for vol in mysql redis app; do
    mkdir /mnt/disks/$vol
    mount -t tmpfs $vol /mnt/disks/$vol
done
```

### 部署mysql

```bash
$ kubectl create -f mysql.yaml
persistentvolume/local-mysql-pv created
persistentvolumeclaim/local-mysql-claim created
configmap/mysql-config created
statefulset.apps/mysql created
service/mysql-svc created
```

#### 测试

测试是否正常运行

```bash
$ mysql -uroot -proot -P30000 -h192.168.0.105  # 连node的ip
mysql: [Warning] Using a password on the command line interface can be insecure.
Welcome to the MySQL monitor.  Commands end with ; or \g.
Your MySQL connection id is 9
Server version: 8.0.12 MySQL Community Server - GPL

Copyright (c) 2000, 2018, Oracle and/or its affiliates. All rights reserved.

Oracle is a registered trademark of Oracle Corporation and/or its
affiliates. Other names may be trademarks of their respective
owners.

Type 'help;' or '\h' for help. Type '\c' to clear the current input statement.

mysql>
```

在node上检查文件

```bash
$ cd /mnt/disks/mysql
$ ls
auto.cnf           client-cert.pem    ibtmp1             server-cert.pem
binlog.000001      client-key.pem     mysql              server-key.pem
binlog.000002      ib_buffer_pool     mysql.ibd          sys
binlog.index       ib_logfile0        performance_schema undo_001
ca-key.pem         ib_logfile1        private_key.pem    undo_002
ca.pem             ibdata1            public_key.pem
```
可见，文件已经写到node上，当Pod被重建的时候，会寻找满足pvc要求的pv对应所在的node，也会被调度到之前运行的node上，因此Pod被重建后数据也不会丢失。

### 部署redis

```bash
$ kubectl create -f redis.yaml
persistentvolume/local-redis-pv created
persistentvolumeclaim/local-redis-claim created
configmap/redis-conf created
statefulset.apps/redis created
service/redis-svc created
```

#### 测试

```bash
$ redis-cli -h 192.168.0.105 -p 30001
192.168.0.105:30001>
```

### 其他

mysql 集群搭建参考：

https://www.jianshu.com/p/eec2fd2d7080

https://kubernetes.io/zh/docs/tasks/run-application/run-replicated-stateful-application/