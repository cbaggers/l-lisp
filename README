# Hi

This project inspired me before I knew what lisp was, I'd love to get it running on top of some of the more portable libraries the lisp games folks are using these days. I'm not actively working on this but I have just pushed it here as a 'one day' project or maybe just to look at it and smile.

# Original Docs

  ---------------------------------------
    L-Lisp: Lindenmayer systems in Lisp
  ---------------------------------------

L-Lisp is a framework for generating and simulating plants and fractals in
Common Lisp.

This is the first public version of L-Lisp.  Currently only CMU Common Lisp
is supported, but porting to other CL implementations should be relatively
easy.  For Unix/X11 systems, OpenGL previews are possible, but the framework
does not depend on them.

L-Lisp is released under the GNU General Public License (GPL), see the file
COPYING for details.


INSTALLATION

It is recommended to download and the OpenGL bindings for CMUCL first.
Get them from: http://www.ii.uib.no/~knute/lisp/lisp.html

The build system used is MK-DEFSYSTEM, this is included with most new
distributions of CMUCL as far as I know.  In case it is not, you can get
it directly from CVS:
http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/clocc/clocc/src/defsystem-3.x/

In l-lisp.system, change the *l-lisp-dir* path to point to your installation
directory.  Then, compiling and loading should be as easy as:

 (mk:compile-system :l-lisp)
 (mk:load-system :l-lisp)

(You might see some notes during complation; ignore them.  Hopefully no
warnings or errors, though.)

To test your installation, go to the LSX (L-SYSTEM-EXAMPLES) package and
try out some of the classes in the examples.lisp file, for instance:

 (in-package :lsx)
 (rewrite-and-preview (make-instance 'snowflake) "snowflake.eps"
                      :depth 4)
 (rewrite-and-raytrace (make-instance 'stree) "stree.pov")
 (gl-preview (make-instance 'stree))

The two first examples depend on the programs "gv" and "povray", and
the last requires that the OpenGL/Xlib bindings are present and working.


DOCUMENTATION

My thesis "L-Systems, Twining Plants, Lisp" includes documentation on
L-Lisp and an introduction to L-systems in general.  An XVI version
without images is included in the doc/ directory, for the full version
go to: http://www.ii.uib.no/~knute/lsystems/llisp.html


NOTES

This was my first Common Lisp project, and a great learning experience.
There are probably some traces of my novice mistakes in my code, feel free
to point them out to me.  :-)

Send questions, feedback, bug reports, etc, to knute@ii.uib.no.  You can
often find me on #lisp (irc.openprojects.net), my nick is Soulman.

Hack away, and have fun.


Knut Arild Erstad <knute@ii.uib.no>
