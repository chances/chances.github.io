---
layout: post
date: 2015-02-20
title: My New Blog
permalink: /blog/my-new-blog/
category: uncategorized
---

Check it out, this is my brand new personal website! It's been over a year since I've spent some time to really sit down and work on my website, and even that was only a little podunk, university hosted, placeholder with my face stickered on. For a while now, it's been time I finally buckled down and created a presence for myself that I could genuinely be proud of, but there's a problem.

I've grown increasingly tired, as have many others, of writing web pages from scratch. And yet I am still neglecting to make use of the many tools and utilities available for today's web developer. Fortunately, my negelct is quickly fading. Things like [npm](http://npmjs.org), [Bower](http://bower.io), [Yeoman](http://yeoman.io), and [Gulp](http://gulpjs.org) have been increasing productivity for literally hundreds of thousands of developers.

<!--more-->

<figure class="quote">
<blockquote>Node.js programs are developed using JavaScript, this means ... it’s possible to merge web and backend teams into one unit which makes things much more efficient.</blockquote>
<figcaption class="citation"><span>Cian O'Maidin, <cite><a href="http://www.nearform.com/nodecrunch/node-js-becoming-go-technology-enterprise/">Why Node is Becoming the Go-To Technology</a></cite></span></figcaption>
</figure>

It's quite surprising how much these tools are actually being used. Take [Bower](http://bower.io), for example:

![bower download stats][bower-badge]

Anyway, what this boils down to is simple: I need to start religiously using these tools and others like them if I want to stay as productive as possible.

## Congratulations, it's a blog! <small>And more...</small>

I've never really thought I'd do any blogging, but the opportunity has presented itself, so I might as well take advantage of it. I can't completely put my finger on what exactly I'll be posting here, but I think it's safe to say it'll at least be some technical discussions, learning materials, insights, and perhaps even a few personal venting sessions while I pursue various projects.

Speaking of projects, check out the rest of the site.

I'll be using this and other areas of the site as a portfolio of sorts; another place to show off, basically. There's already a page listing [a few notable projects](/projects) I've worked on that definitely wouldn't mind being seen by other pairs of eyes. I've also got a few other projects up my sleeves and look forward to releasing them out into the world. But until then, there'll always be new ideas pushing their way out of my head, and this is the perfect place to house them.

## Let's get technical

This website is a <abbr class="subtle" title="I just made this up.">pseudo-dynamic</abbr> static site generated with [Jekyll](http://jekyllrb.org) and scaffolded with [Jekyllized](https://github.com/sondr3/generator-jekyllized). It makes use of the [Lanyon](http://lanyon.getpoole.com) Jekyll theme with a few of my own <abbr class="subtle" title="Check out the social buttons in the sidebar and footer.">customizations</abbr> thrown in. The Jekyllized Yeoman generator provided a nice, albeit unfinished, gulpfile providing most of the tasks I needed to get the site up and running. It even includes [GitHub Pages](https://github.com/pages) integration, automating deployment by pushing a specific branch to GitHub.

With regard to the gulpfile, I'm modifying it for my needs, specifically by adding tasks to automatically create skeletons for new blog posts and pages. Eventually, I hope to be able to manage the bulk of my site with gulp and my text editor. For other facts about this website, see the [About](/about) page.

The source code for this site is available on [GitHub](https://github.com/chances/chances.github.io).

## Thanks for reading!

[bower-badge]: https://nodei.co/npm/bower.png?downloads=true
