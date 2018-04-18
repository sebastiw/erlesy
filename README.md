# erlesy

ErlEsy is a simple graphing tool for Erlang. It allows for automatic generation of state machine diagrams out of Erlang source files. It works by parsing an .erl source file and building a digraph out of it. The digraph can be then transformed into any format that will allow for its graphical representation.

## Building

### rebar3

ErlEsy can be build by rebar3

`rebar3 get-deps`
`rebar3 compile`

You can also use the makefile, with these directives:
`make` - fetches deps and compiles erlesy
`make shell` - starts a erlesy shell

## Running 

To run ErlEsy simply start the application by calling

`application:start(jsx).`
`application:start(erlesy).`

Then you can create any file by using

`-spec create_graph(string(), [string()], dot | json) -> ok.`
`otp_parser:create_graph(FileName, IncludeFiles, Mode).`

* FileName should be a path to the .erl file you want to graph
* IncludeFiles is a list of paths towards include files
* Mode is either dot or json

## Output and graphing

ErlEsy currently supports two forms of output, a [DOT (.gv)](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) formatted file or a JSON file.

### DOT

DOT file can be used e.g. at http://www.webgraphviz.com/


## Example

Suppose your project structure is

* `project/src/your_fsm.erl`: The FSM you want to graph
* `project/include/`: Path to include files used by the fsm

Then execute the following in the erlesy folder:

* `rebar3 shell`
* `application:start(jsx),application:start(erlesy).
* `otp_parser:create_graph("/absolute_path_to_project/project/src/your_fsm.erl",["/absolute_path_to_project/project/include/"], dot].`

This will create a `.gv` dot file in the same folder the source is located in.
