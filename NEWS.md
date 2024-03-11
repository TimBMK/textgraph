# textgraph 1.1.0

- added functionality to save and load textgraph topic objects
- added functionality to visualize rwr terms
- added functionality for `explore_topics()`: open result without saving it; print top terms as table; different output formats
- added the option to calculate document relevance in `calculate_dynamic_topics()` and `calculate_topics()` as page rank in a document-document graph
- performance increases in `calculate_dynamic_topics()` and `calculate_topics()` via data.table implementation
- bug fixes in `calculate_dynamic_topics()`, `calculate_topics()`, `classify_documents()`, `explore_topics()`
- fixed erroneous `widyr` version requirement

# textgraph 1.0

* Initial Release, including core functions, vignettes, and documentation
