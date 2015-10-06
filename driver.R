try(system("hadoop fs -ls /data/gcbretadvn/work/ishan/Genetic_Algorithm", intern = TRUE))

try(system("hadoop fs -rm -r /data/gcbretadvn/work/ishan/Genetic_Algorithm/output1"))

#a="hadoop jar /opt/cloudera/parcels/CDH/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-2.0.0-mr1-cdh4.5.0.jar -file mapper.R -mapper mapper.R -file reducer.R -reducer reducer.R -input /data/gcbretadvn/work/ishan/Genetic_Algorithm/modeldata_sample_lessvars.csv -output /data/gcbretadvn/work/ishan/Genetic_Algorithm/output1"
#a="hadoop jar /opt/cloudera/parcels/CDH/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-2.0.0-mr1-cdh4.5.0.jar -D mapred.map.tasks=10 -D mapred.reduce.tasks=1 -file mapper.R -mapper mapper.R -input /data/gcbretadvn/work/ishan/Genetic_Algorithm/modeldata_sample_lessvars.csv -output /data/gcbretadvn/work/ishan/Genetic_Algorithm/output1 -cmdenv population_size=7"

#a="hadoop jar /opt/cloudera/parcels/CDH/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-2.5.0-mr1-cdh5.3.3.jar -D mapred.map.tasks=5 -D mapred.reduce.tasks=5 -file mapper.R -file reducer1.R -mapper mapper.R -reducer reducer1.R -input /data/gcbretadvn/work/ishan/Genetic_Algorithm/modeldata_sample_lessvars.csv -output /data/gcbretadvn/work/ishan/Genetic_Algorithm/output1 -cmdenv population_size=7"

#a="hadoop jar /opt/cloudera/parcels/CDH/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-2.5.0-mr1-cdh5.3.3.jar -D mapred.map.tasks=5 -D mapred.reduce.tasks=5 -file mapper.R -file Genetic_Algorithm_local3.R -mapper mapper.R -reducer Genetic_Algorithm_local3.R -input /data/gcbretadvn/work/ishan/Genetic_Algorithm/modeldata_sample_lessvars.csv -output /data/gcbretadvn/work/ishan/Genetic_Algorithm/output1 -cmdenv population_size=7"

#a="hadoop jar /opt/cloudera/parcels/CDH/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-2.5.0-mr1-cdh5.3.3.jar -D mapred.map.tasks=5 -file mapper.R -mapper mapper.R -input /data/gcbretadvn/work/ishan/Genetic_Algorithm/modeldata_sample_lessvars.csv -output /data/gcbretadvn/work/ishan/Genetic_Algorithm/output1 -cmdenv population_size=7"

#a="hadoop jar /opt/cloudera/parcels/CDH/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-2.5.0-mr1-cdh5.3.3.jar -D mapred.map.tasks=5 -D mapred.reduce.tasks=5 -file mapper.R -file reducer2.R -mapper mapper.R -reducer reducer2.R -input /data/gcbretadvn/work/ishan/Genetic_Algorithm/modeldata_sample.csv -output   /data/gcbretadvn/work/ishan/Genetic_Algorithm/output1"

#Below is running in like 20 mins - small 2MB data and 5 mappers and 5 reducers
a="hadoop jar /opt/cloudera/parcels/CDH/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-2.5.0-mr1-cdh5.3.3.jar -D mapred.map.tasks=5 -D mapred.reduce.tasks=5 -file mapper.R -file reducer3.R -mapper mapper.R -reducer reducer3.R -input  /data/gcbretadvn/work/ishan/Genetic_Algorithm/modeldata_sample_lessvars.csv -output  /data/gcbretadvn/work/ishan/Genetic_Algorithm/output1"


#a="hadoop jar /opt/cloudera/parcels/CDH/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-2.5.0-mr1-cdh5.3.3.jar -D mapred.map.tasks=10 -D mapred.reduce.tasks=10 -file mapper.R -file reducer3.R -mapper mapper.R -reducer reducer3.R -input  /data/gcbretadvn/work/ishan/Genetic_Algorithm/modeldata_sample.csv -output /data/gcbretadvn/work/ishan/Genetic_Algorithm/output1"

a="hadoop jar /opt/cloudera/parcels/CDH/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-2.5.0-mr1-cdh5.3.3.jar -D mapred.map.tasks=10 -D mapred.reduce.tasks=10 -D mapreduce.task.timeout=0 -file mapper.R -file reducer3.R -mapper mapper.R -reducer reducer3.R -input  /data/gcbretadvn/work/ishan/Genetic_Algorithm/modeldata_sample.csv -output /data/gcbretadvn/work/ishan/Genetic_Algorithm/output1"



try(system(a, intern = TRUE))

try(system("rm /home/ic10636/Genetic_Algorithm/v2/output/part*"))

try(system("hadoop fs -getmerge /data/gcbretadvn/work/ishan/Genetic_Algorithm/output1 /home/ic10636/Genetic_Algorithm/v2/output/parts"))

#eval, parse, string concatenate

