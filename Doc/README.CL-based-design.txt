
CL based design needs to consider the packaging components, to ensure
that the final packaging works.

Some guidelines:  packages should have methods which self-describe
exported commands. 

We can do this via lisp functions -- need a macro to provide
reflection of this information.

can we load packages and add symbols to an existing package --
i.e. want to be able to load into curent package or another package as
specified, i.e. 
(load-lisp-stat-package package-to-load
                        package-space-to-infect)

(default is ls-user)


