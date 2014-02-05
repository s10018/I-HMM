I-HMM
======

We implement I-HMM described in <http://www.cis.temple.edu/~yates/papers/2011-conll-fhmm-lang-model.pdf>.

Required
======

You need [sbt](http://www.scala-sbt.org/) for performing this program.

How to performed
======

The sample as below

    sbt "run train -train train.txt -layer 3 -state 8 -c 3 -dump model.txt"
    sbt "run decode test.txt model.txt"


How to make jar file
======

You can make the jar file by commands below.

    sbt assembly

You can see `target/scala-2.10/I-HMM.jar` appear after the commands are performed.

Run:

    java -jar target/scala-2.10/I-HMM.jar -train train.txt -layer 3 -state 8 -c 3 -dump model.txt
    java -jar target/scala-2.10/I-HMM.jar decode test.txt model.txt


Reference
====

Huang et. al. (Language Models as Representations for Weakly-Supervised NLP Tasks)[http://www.cis.temple.edu/~yates/papers/2011-conll-fhmm-lang-model.pdf], CoNLL 2011

