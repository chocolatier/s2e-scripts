# s2e-scripts
Scripts I write to deal with S2E

## Dependencies

dwarf-tools

## Usage

```
./s2eDebug /path/to/s2e/project/ s2e-out-x
```
Defaults to s2e-last if no output directory is specified.

Assumes that the project directory name is name of binary

Exit with C-c or C-d (EOF)

### Realistic Usage

```
echo -e "command" | ./s2eDebug /path/to/s2e/project/ s2e-out-x > foo.txt
```
### Defined commands

* getForks : Finds all the locations s2e forked at
* countForks : Counts the number of forks at a paricular location
* findTestCases : Finds all the test cases s2e generated
* getDeadEnds : Find all the dead ends the DynamicEarlyTerminate plugin inserted
* getStatus : Finds all the status codes a program exited with. (Doesn't catch the related states or any other information)
* countStatus : Counts the number of times a state was terminated with a certain code
* countStatus2 : Counts the number of times the tracked process was unloaded with a certain code
