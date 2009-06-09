tester: A test harness for Common Lisp
======================================

Table of contents
-----------------

 * Description
 * Author
 * Author comments
 * Documentation
 * Platforms
 * Dependencies
 * Installation
 * Configuration
 * Licence
 * Notes
 * Examples
 * Open Source 

Description
-----------

A test harness allows Common Lisp APIs to be automatically tested.

Author
------

Kevin Layer, Franz Inc.

Author comments
---------------

This source code has been previously available with Allegro
Common Lisp, and is now being made available to the general
public.

Platforms
---------

All ACL versions.

Dependencies
------------

None if tester.cl is loaded directly, otherwise asdf if you load
via asdf.

Installation
------------

Install the source in your favorite location, load tester.cl
directly or the asd file as per the asdf documentation.

Configuration
-------------

See the documentation link below.

Usage Documentation
-------------------

[Test Harness Documentation]
(http://www.franz.com/support/documentation/current/doc/test-harness.htm)

License
-------

The test source code is licensed under the terms of the Lisp
Lesser GNU Public License, known as the LLGPL. The LLGPL consists
of a preamble and the LGPL. Where these conflict, the preamble
takes precedence. test is referenced in the preamble as the
LIBRARY.

Notes
-----

There are more notes about tester in the link provided in the
documentation section above.

For an acl-compat version suitable for running in non-acl Common-Lisps
see Kevin Rosenberg's ptester repository:

    git://git.b9.com/ptester.git/

Examples and Information
------------------------

See section 1.3 of the documentation link in #8 above.

Open Source
-----------

This project's homepage is <http://opensource.franz.com>. There is an 
informal community support and development mailing list 
[opensource@franz.com](http://opensource.franz.com/mailinglist.html) 
for these open source projects. We encourage you to take advantage by 
subscribing to the list.  Once you're subscribed, email to 
<opensource@franz.com> with your questions, comments, suggestions, 
and patches.
