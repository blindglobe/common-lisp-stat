{include header.md}
{set-property title Log5 - Frequently Asked Questions}

# Frequently Asked Questions about log5

{table-of-contents :start 2 :depth 3}

## Does once count as frequently?

> Yes.

## The expanded categories specifications look very inefficient.

> This isn't a question but we'll address it in any case. If you look at `category-specs`, you'll some expansions that look pretty poor. For example:

>        #<category 18: (OR (OR TRACE (OR INFO (OR WARN (OR ERROR FATAL))))
>                        (OR PLANNER EXECUTIVE)) -> (TRACE+ AGENT)>

> All of those nested `or`s are silly, it's true but remember that these logical forms are only evaluated when you *start* a sender -- not with every logging attempt. As such, this slight inefficiency is not worth worrying about.

## What is planned for log5? Will there be a movie?

> I'm using log5 in my daily work and am pretty happy with it. That said, here are some of the items left on the wish list:

>    * rolling logs
>    * other senders (e.g., console log, text messaging) -- but I'll probably never write these
>    * speed (though it is already fast)
>    * reporting

> Reporting is probably the biggest hole but it's probably worth either starting another project or trying to make log5's output look enough like log4j that one could use Chainsaw.

## What else?

> If you have a question that isn't addressed here, please let the log5 [development list][log5-email] know and we'll try to answer it soon.

{include footer.md}

