# CLOAK #

Cloak is a project automation tool written in [Clojure][].

Usage
----------

When invoked from a command line without parameters, `cloak` looks for
a file named `CLOAK` in the current directory (this file should define
tasks to be performed) and executes `:default` task.

You can also use the following options:

* `-h` to display help
* `-f cloak_file` to use non default cloak file
* `-d` to describe available task
* `-q` to specify a series of task names witch will be executed by Cloak

cloak.actions provides a couple of basic file operations:
`exists?`, `copy` , `move`, `rm`, `sh` (executing a shell
command). Windows users must be careful to use a "cmd ..." as a
parameter to `sh` when running a batch script.

Syntax
----------

Tasks:

		(task
            #^{:doc "Task description"}
            task-name #{'task 'other-task}
			  (action1 arg)
			  (action args))

Where task's name should be a symbol, task's dependencies should be a
set of task names, description should go in :doc metadata associated with
task name.

Compiling Cloak
---------------

To compile Cloak run `ant jar`.

[rake]:http://rake.rubyforge.org/
[clojure]:http://clojure.org/
