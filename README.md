<p align="center">
  <a href="#"><img src="./docs/images/banner.png" /></a>
</p>

<p align="center">
  A <em>completely</em> customizable framework <br/>
  for building rich text editors in ClojureScript.
</p>
<br/>

<p align="center">
  <a href="#principles"><strong>Principles</strong></a> ·
  <a href="https://beautiful-haupia-5c51c7.netlify.app/"><strong>Demo</strong></a>
</p>
<br/>

<p align="center">
  <a href="#"><img src="./docs/images/awesome-demo.gif" /></a> ·
</p>
<br/>
</p>

### Principles

- Schema-less core. Rich's core logic assumes very little about the schema of the data you'll be editing, which means that there are no assumptions baked into the library that'll trip you up when you need to go beyond the most basic use cases.

- Nested document model. The document model used for Rich is a nested, recursive tree, just like the DOM itself. This means that creating complex components like tables or nested block quotes are possible for advanced use cases. But it's also easy to keep it simple by only using a single level of hierarchy.

- Parallel to the DOM. Rich's data model is based on the DOM—the document is a nested tree, it uses selections and ranges, and it exposes all the standard event handlers. This means that advanced behaviors like tables or nested block quotes are possible. Pretty much anything you can do in the DOM, you can do in Rich.

- Intuitive commands. Rich documents are edited using "commands", that are designed to be high-level and extremely intuitive to write and read, so that custom functionality is as expressive as possible. This greatly increases your ability to reason about your code.

- Collaboration-ready data model. The data model Rich uses—specifically how operations are applied to the document—has been designed to allow for collaborative editing to be layered on top, so you won't need to rethink everything if you decide to make your editor collaborative.

- Clear "core" boundaries. With a plugin-first architecture, and a schema-less core, it becomes a lot clearer where the boundary is between "core" and "custom", which means that the core experience doesn't get bogged down in edge cases.

