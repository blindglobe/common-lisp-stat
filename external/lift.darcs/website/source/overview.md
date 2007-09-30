{include header.md}
{set-property title Log5 - overview}

### Overview 

the bird's eye view looks like this: You define *categories* for your application. These might look like

    (defcategory motion)

    (defcategory energy)

    (defcategory physics (or energy motion))

    (defcategory planner)

and so forth. Categories are sort of like Lisp `*features*` with names. They can be simple (like `motion`) or boolean combinations (like `physics`). When you write a typical log *message*, you use a combination of categories to describe it:

    (log-for (and physics (not file-system) trace)
    	     "starting widget simulation")
    
or 

    (log-for (or planner motion) "Planning path for agent ~a" (name agent))

You start a *sender* using `start-sender` (surprise!). You specify what kind of sender it is (e.g., a stream sender or a database sender or an HTML sender or whatever) and pass along whatever arguments are needed to create it. You also specify the categories and the *outputs* the sender will send. Categories were discussed above; a sender's outputs are a list of named properties defined with defoutput. For example: 

    (defoutput time (get-universal-time))

    (defoutput virtual-memory (os-get-virtual-memory))

    (defoutput current-database (name *db*)))

Outputs can compute anything that makes sense for your program (though they ought to compute it quickly if you don't want logging to kill performance!). Some outputs are special and predefined. For example, the output `message` refers to the string created by the log message statement (e.g., the `log-for` examples above). The output `context` refers to the current *context* (the last of our five players).

The context is a carry-all you can use to specify whatever important is happening in the global environment. If you're writing a web-application; the context might track the current session ID; A planner might track the current agent and so forth. Information from the context is added to the end of each log message sent and so functions as a variable portion in contrast to the fixed structure of the sender's output.

{include footer.md}