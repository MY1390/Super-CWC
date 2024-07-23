# Super-cwc-lcc Package

This program supports Super-CWC and Super-LCC, which are fast consistency-based feature (attribute) selection algorithms. 
Super-CWC does not require any adjustment parameter,
while Super-LCC requires a threshold parameter.
The greater the threshold is, the looser the feature elimination criterion of LCC becomes.  Therefore, with a large threshold, Super-LCC will eliminates more features (attributes) and will run faster.
Almost always, in particular when you apply Super-LCC to large datasets, Super-LCC with the threshold zero will return the same outputs as Super-CWC, but is a few times slower than Super-CWW.  On the other hand, with a positive threshold, SUper-LCC will be a few times faster than Super-CWC
IMPORTANT NOTE.  This program is a trial version.  For this reason, the functions that this program supports are limited.  For example, only Bayesian risk can be used as the selecting measure used by Super-LCC.  Also, this program may include bugs.  If you discover any bugs, please send information to the following adress.  
kilhoshin314@gmail.com (Kilho Shin, Professor, Gakushuin University)
IMPORTANT NOTE.  You can use and modify the codes as you like as far as your porpose is for research and you do not distribute the codes.  A detailed license will be presented, when a distributable version of the codes is released
Command Usage:
Examples
> java -jar super-cwc-trial.jar -i foo -o foo-selected -a lcc -s su -m br -t 0.001 -l high
> java -jar super-cwc-trial.jar --input foo --output foo-selected --algorithm cwc --log none
> java -jar super-cwc-trial.jar -i foo
Usage: 
  -i <path> | --input <path>
        A path to an input ARFF file w/o the extension .arff
  -o <path> | --output <path>
        A path to an output ARFF file w/o the extension .arff
  -a <cwc,lcc> | --algorithm <cwc,lcc>
        A feature selection algorithm: cwc (default) or lcc
  -s <su,mi,br,mc> | --sort <su,mi,br,mc>
        A statistical measure to sort features: su (symmetric uncertainty, default), mi (mutual information), br (Bayesian risk) or mc (Matthew's correlation coefficient)
  -m <br> | --measure <br>
        A statistical measure to select features: br (Bayes risk, default)
  -t <value> | --threshold <value>
        Threshold: 0.0 (default)
  -l <hi,none> | --log <hi,none>
        Level of detail of log: hi (default) or none
Exept the --input parameter, any of the parameters can be left out. 
--input  A path to a source arff file is specified.  The extension .arff must be omitted.  The format of the file can be dense or sparse.
--output  A path to a destination arff file, which includes only the selected featuresbut includes all of the instances, is specified.  The extension .arff must be omitted.  The format of the file is always dense.  If generation of a destination file is not necessary, this argument can be omitted.
--sort  Super-CWC and Super-LCC sort features (attributes) in the order of the individual relevance of the features to class labels before starting selection.  This argument specifies the measure to use for the sorting.
0:  Symmetric uncertainty
1:  Mutual information
2:  Bayesian risk a.k.a. Inconsistency rate
3:  Matthew's correlation coefficient
--measure  Super-LCC selects features so that the selected feature set has the score in the measure spcified by this argument does not exceed the threshold specified by the --threshold argument.  In the current trial version, only the Bayesian risk is supported. 
--threshold  Super-LCC selects features so that the selected feature set has the score in the measure specified by the --measure argument does not exceed the threshold specified this argument.  When the specified value is zero, even if the --algorithm argument specifies LCC, Super-CWC will be used, since the function of Super-LCC is the same as Super-CWC and Super-CWC is faster than Super-LCC in this case. 
The threshold has to be selected within the interval (0, (c-1)/c], where c is the number of classes (e.g. If the class is binary, that is, a class label is either 0 or 1, c = 2 holds).  But, usually, a very small value should be specified.  With a too large threshold, the feature elimination criterion of Super-LCC will become too loose, and Super-LCC may eliminate all of the features.  Because Super-LCC is very fast, you can also select an appropriate threshold on a trial-and-error basis.
--log  IF the value "hi" is specified, a log file will be generated. 
