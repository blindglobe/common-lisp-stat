;;; -*- mode: lisp -*-

;;; Time-stamp: <2012-11-02 17:58:24 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       missing-data.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Template header file

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".



;;; Missing data handling is critical - initial thought for this is to
;;; have a class which provides scalars which have type "missing" and
;;; perhaps the "supposed" statistical-typing, i.e. continuous ratio, 
;;; ordinal, nominal, etc.   Also have metadata to describe where this
;;; came from.
;;; 
;;; Then we can gensym a value per-dataset per-missing type.  This
;;; strategy (using gensyms to represent a particular
;;; missing-data-type instance could be useful if we do not have to
;;; maintain equality across CL execution runs.
;;;
;;; Different types of missing:
;;; * censored data
;;; * unobserved data
;;; * coarsened measurement data
;;;
;;; but they can be placed into a generalized framework (see the work
;;; of van der Laan, Robins, etc...).
;;;







#|
On Fri, Nov 2, 2012 at 4:24 PM, Tamas Papp <tkpapp@gmail.com> wrote:
>
> On Fri, Nov 02 2012, A.J. Rossini <blindglobe@gmail.com> wrote:
>
>> On Fri, Nov 2, 2012 at 10:20 AM, Tamas Papp <tkpapp@gmail.com> wrote:
>>
>>
>> lots of good and exciting things, with only a minor caveat
>>
>>
>>> On Fri, Nov 02 2012, A.J. Rossini <blindglobe@gmail.com> wrote:
>>
>>>> For missing data -- the same.  We need extensions of the numbers to
>>>> include infinity, missingness-categories, as well as nominal and
>>>> ordinal categorical variable storage structures.
>>>
>>> I would just use NIL to denote missing data, but maybe you guys have
>>> other requirements.
>>
>> I do -- my thesis eons ago was on interval censoring, and I really do
>> want missing data (including censored and coarsened) to be first class
>> objects and not afterthoughts..  So this would be an extension to a
>> data class, not just "nil", since there are different levels of
>> knowledge that are possible, and it would be critical to ensure that
>> we have appropriate metadata to provide hints as to conformance of
>> data generating processes to data analysis procedures (and such hints
>> can be strictly followed, i.e. "you've broken key assumptions, stop it
>> this instant", or weakly followed -- "yes, I'm modeling binary data
>> with linear regression, but it might be reasonable").
>
> Perhaps my perspective is biased because I only do two kinds of
> statistics: preliminary checks using simple moments, and full-fledged
> Bayesian analysis.  For the first, I usually just ignore missing data
> (eg when calculating moments, pretend that observation is not there),
> and for the second, the reason for missing data (censoring? truncation?
> etc) belongs in the model, and I need to deal with that on a
> case-by-case basis for each model & dataset.
>
> Maybe we could include a generic function
>
> (defgeneric missing-data? (data)
>   (:method (data)
>      nil)
>   (:method ((data null))
>      t))
>
> so that functions (eg mean, sd, variance, quantiles) could ignore data
> for which missing-data? returns T, and the user could define
>
> (defmethod missing-data? ((data missing-because-the-dog-ate-it))
>   t)
>
> to introduce new kinds of missing data.

It's a start, and we can start from that (and the following can be down-graded to manage such a DSL-approach), but a medium quality implementation would be closer to:  
I'd rather have some form of localizable parameter *missing-data-management*  that suggests:

1. throw-exception-because-we-want-to-experiment-with-assumptions

2. ignore/drop-missing-data  (with "by-variable, by-observation, by...." options)

3. forbid-missing-data  (and so through an exception)

4. impute-missing-data   (with "imputation model" which would describe how to replace, by mean of observed, by draw from empirical distribution, by draw from model-based distribution...)

and this would then suggest how to dispatch the algorithm (with exception handling to be able to drop in what might be missing.  Of course you'd need a way to detect (and different ways to do that, on a variable, on an observation, on a single value), and ideally, a representation as well (SAS uses ".", R uses "NA", I'd think of gensym-like-symbols or something like that, that might be identifiable across instances and implementations, ideally).

best,
-tony

blindglobe@gmail.com
Muttenz, Switzerland.
"Commit early,commit often, and commit in a repository from which we can easily roll-back your mistakes" (AJR, 4Jan05).

Drink Coffee:  Do stupid things faster with more energy!
|#
