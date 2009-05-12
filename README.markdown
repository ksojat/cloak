# CLOAK #

Cloak is a simplistic automation tool written in [Clojure][]. It's heavily
inspired by ruby [rake][].

Usage
----------

When invoked from a command line without parameters, `cloak` looks for
a file named `CLOAK` in the current directory (this file should define
tasks to be performed) and executes `:default` task.

You can also use the following options:

* `-h` to display help
* `-f cloak_file` to use non default cloak file
* `-d` to describe available task
* `-t` run cloak without executing any actions

You can also specify a series of task names (without leading colons),
which will be executed in sequence (from left to right).

Cloak provides two kinds of tasks: tasks and file tasks.

File tasks are geared towards *creating* files. Dependencies of a file
task can be other files and tasks. If a dependency is a file,
modification time of source and target files is compared before
executing the task.

rosado.cloak.actions provides a couple of basic file operations:
`exists?`, `copy` , `move`, `rm`, `sh` (executing a shell
command). Windows users must be careful to use a "cmd ..." as a
parameter to `sh` when running a batch script.

Syntax
----------

Tasks:

		(task :task-name [:task :other-task]
			  "Task description"
			  (action1 arg)
			  (action args))

Where task's name should be a keyword, task's dependencies should be a
vector of task names or file task names (optional), description should
be a string (optional).

File tasks:

	 (file "file_name" ["other.xml" :some-task]
	 	   "Description"
	 	   (action-to-generate-file "file_name"))

File tasks can't be specified as targets on the command line.

Compiling Cloak
---------------

To compile Cloak run `ant compile` or `ant jar`. See below for
dependencies.

Compiling Clojure sources
-------------------------

It works (look at this project's CLOAK file) but the following must be
true for it to work:

* directory with sources *AND* "classes" directory must be in your classpath
* the "classes" directory must exists when the JVM starts up to execute the `compile` function (otherwise strange errors might occur).

What it isn't
-------------

It's not an `ant` or `maven` replacement.

Dependencies
------------

To run the tests you'll need test-is from [clojure-contrib][contrib].

TODO
----------

* copying files over ssh
* more complete set of actions for file operations
* better error reporting

[rake]:http://rake.rubyforge.org/
[mylibs]:https://github.com/rosado/libs4clj/tree
[clojure]:http://clojure.org/
[contrib]:http://sourceforge.net/projects/clojure-contrib
