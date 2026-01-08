# Canary

Canary is a tool for automatically reporting runtime exceptions to issue
trackers. Developed by the University of Zurich for internal projects using
OCaml.

## Doctrine

When using OCaml, one should almost always use error-aware return types such as
`option` or `result` to help structure application in such a way that any
problems that could be reasonably expected will be handled gracefully. However,
exceptional circumstances (or situations that you don't expect to have to deal
with) should still raise exceptions. Usually, an exception in production is
*exceptional* enough to demand the immediate attention of developers. That is
what Canary is for. Canary provides a standard interface for exception reporters,
in order to accelerate the development of reporters as well as facilitate
multi-reporting, or easier transitioning between different reporters.
