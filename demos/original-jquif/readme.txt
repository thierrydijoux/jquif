Jquif demo

What you need ?
This project use the freespider web components to generate CGI, so you have to install them if you want to compile the project.
A web server supporting CGI: apache is a good choice, but i use lighttpd. I've tested this demo with both but on linux. So i can't help you with windows.

How to setup the project ?
in the directory data, there are 3 folders:
  jqdata: copy this one under the cgi-bin directory, it contains the template and a test file
  jquerydata: all the graphical elements and CSS, copy it under your /var/www
  js: all the javascript needed to work, copy it under your /var/www

Compile the project and copy the compiled jquery under your cgi-bin.
Launch your favorite browser and go to http://localhost/cgi-bin/jquery

That's all !

If you have any questions, suggestions or want to give a hand, a thread is open on the lazarus forum at this URL:
http://www.lazarus.freepascal.org/index.php/topic,15982.0.html
Ask for all here.

A web site is open, all the informations of the project will be here.
URL is http://jquif.thierrydijoux.com/
