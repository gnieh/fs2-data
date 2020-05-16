---
title: Documentation
---

`fs2-data` consists of several modules, handling a different data formatw, and sub-modules for each data format, adding more features (but also more dependencies). The core module for each data format has no other dependency than `fs2` and provides tools to parse and transform data in a streaming manner.

Available data modules are:
<% @items.find_all("/documentation/*/index.md").each do |sub| %>
 - [<%= sub[:title] %>](<%= sub.path %>) - <%= sub[:description] %>
<% end %>
