### Welcome

This is a software application made in r shiny and is used for the recovery and/or fix of data files with information coherence problems, through the use of user-defined rules, for the detection and repair of errors in the data.

This application supports extension data files: txt, csv, xls, xlsx, xml and json.

This application check row by row the file uploaded by the user, detecting inconsistencies in the data previously defined by conditions created by the user. It also corrects these problems by replacing them with user-defined values.

This application has two options: Substitution and Reference

Substitution option, fixes the data file with information coherence problems by replacing the corrupted data with values assigned by the user, allowing previously define rules for the error detection in the attributes of the data file with information coherence problems and how these are repaired with values substitutes. 
Look at the following image to use this option:

<center>
<img src="../images/substitution.gif" alt="An example upload datafile with errors using rules to fix it"  border="5" class="centerbox"/>
</center>

The Reference option, corrects data file with information coherence problems by replacing the corrupt data with referenced values of one or more data files with correct data, defined by the user, allowing  previously define rules for error detection in the attributes of the data file with problems of consistency of information and selected the references to the file or more, with correct data that will replace the data corrupt.

Look at the following image to use this option:

<center>
<img src="../images/reference.gif" alt="An example upload datafile with errors using rules to fix it"  border="5" class="centerbox"/>
</center>

Thanks for using this software