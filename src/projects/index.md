---
layout: page
title: Projects
permalink: /projects/
---

<div id="projects">
  <ul>
    {% for project in site.data.projects %}
    <li>
      <h3>
        {% if project.github %}
        {% comment %} <!-- Conditionally get a short name for the project. --> {% endcomment %}
        {% assign short_name = project.short_name %}
        {% unless short_name %}
          {% assign short_name = project.name %}
        {% endunless %}
        <a href="https://github.com/chances/{{ project.github }}" class="project-link github">
          <span class="devicons devicons-github_badge" title="View {{ short_name }} on GitHub"></span>
        </a>
        {% endif %}
        {% if project.link %}
        <a href="{{ project.link }}">{{ project.name }}</a>
        {% else %}
        {{ project.name }}
        {% endif %}
        <small>{{ project.description }}</small>
      </h3>
    </li>
    {% endfor %}
  </ul>
</div>
