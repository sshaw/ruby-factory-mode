# ruby-factory-mode

Minor mode for Ruby test object generation libraries. Currently supports
[factory_girl](https://github.com/thoughtbot/factory_girl) and [Fabrication](https://github.com/paulelliott/fabrication)
and only under Rails.

Pull requests welcome!

## Requirements

`inflections`, [YASnippet](https://github.com/capitaomorte/yasnippet).

## Usage

`ruby-factory-mode` is enabled via `ruby-mode` when a Rails model or test factory is opened.

### Key Bindings

<kbd>C-c f t</kbd> - Toggle model and factory

### Snippets

#### factory_girl

Binding | Snippet
--------|------------------------------------|
`aft`   | `after :hook do |model| ... end`   |
`deff`  | `FactoryGirl.define do ... end`    |
`fac`   | `factory :model do ... end`        |
`seq`   | `sequence(:attribute) { |i| ... }` |

...

#### Fabrication

Binding | Snippet
--------|-----------------------------------------|
`aft`   | `after_hook do |model| ... end`         |
`aftb`  | `after_build do |model| ... end`        |
`aftc`  | `after_create do |model| ... end`       |
`aftv`  | `after_validation do |model| ... end`   |
`fab`   | `Fabricator :model do ... end`          |
`seq`   | `sequence(:attribute) { |i| ... }`      |
`tra`   | `transient :attribute`                  |

...
