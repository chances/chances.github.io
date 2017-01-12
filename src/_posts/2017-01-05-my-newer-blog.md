---
date: 2017-01-05
title: My New(er) Blog
permalink: blog/my-newer-blog
category: uncategorized
---

Out with the old, in with the new(er)!

Although I enjoyed [revamping](/blog/my-new-blog) my website back in 2015, I failed to foresee how much the tools I put in place would get in my way. And, damn, did they get in the way! Making _minor_ changes was hard enough, let alone trying to write a blog post. I was even managing **two** _different_ Git repositories for **one** static website. All these pain points capitalized on each other creating a tooling nightmare.

Today, I've decided to switch out the toolbox and use something better.

<!--more-->

<figure class="quote">
<blockquote>I need to start religiously using these tools and others like them if I want to stay as productive as possible.</blockquote>
<figcaption class="citation"><span>Chance Snow, <cite><a href="/blog/my-new-blog">My New Blog</a></cite></span></figcaption>
</figure>

## Religiosity is not the answer...

Religiously using a bunch of tools to stay productive is just asking for trouble. I shouldn't torture myself and spend an hour tooling around before I even get to work. I'm better off watching a monkey try to use a hammer.

They say you learn the most when you fail. My failure is evident judging by the <abbr class="subtle" title="I backdated all the posts shown through the last two years...">absence of posts</abbr> on this blog. I realize I can't keep my tools out of the way entirely, but I can _simplify_ the process of using them. I can fix this nightmare with a different tool, a _better_ tool.

## Fostering productivity with flexibility

Doing more with <abbr class="subtle" title="In my end-user interface, at least. There's still a fair amount of dependencies attached to the site, but nearly as many direct dependents.">less</abbr> using [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)).

Haskell's generality affords its users the opportunity to make virtually anything they please and it's [standard library](https://hackage.haskell.org/package/base) is very robust. [The Haskell Tool Stack](https://haskellstack.org) makes project setup a breeze and is tightly integrated with the Haskell ecosystem as a whole.

### How Haskell and Stack help me

My workflow is greatly simplified:

- No longer am I bogged down with [gulp](http://gulpjs.com/), [ruby](https://jekyllrb.com/), or [bower](https://bower.io/) and my train wreck of an imperatively defined build tool-chain.
- I can now deploy any change in **minutes** not hours.
- [Uncompiled source code](https://github.com/chances/chances.github.io/tree/hakyll) is in the _same_ Git repository as the [built site](https://github.com/chances/chances.github.io/tree/master).
- Simply running `make watch` builds the entire site and watches for changes as they happen
- Did I mention speed? (Native performance, FTW!)

This means I can (_finally_) write blog posts _when the mood strikes_ and not feel disheartened at the hand of a cumbersome workflow.

### Downsides

Learning curve of Haskell is <abbr class="subtle" title="But that's normal when learning a new language...">steep</abbr>:

- Monads
- Functors
- Type mismatches
- Other headaches

Rarely, dependency hell sneaks up on me, but it's curtailed by Stack rather well.

## Technical details

This is a static site generated with [Hakyll](https://jaspervdj.be/hakyll) and scaffolded with [The Haskell Tool Stack](https://haskellstack.org). Source code is available in full on [GitHub](https://github.com/chances/chances.github.io).

## Thanks for reading!
