# ruby-factory-mode

Emacs minor mode for Ruby test object generation libraries. Currently supports
[factory_girl](https://github.com/thoughtbot/factory_girl) and [Fabrication](https://github.com/paulelliott/fabrication)
and only under Rails.

Pull requests welcome!

## Requirements

* [inflections](https://github.com/eschulte/jump.el)
* If you want to use the snippets you'll need [YASnippet](https://github.com/capitaomorte/yasnippet)  `v0.8.1` or greater

## Usage

`ruby-factory-mode` is enabled via `ruby-mode` when a Rails model or test factory is opened.

### Key Bindings

<kbd>C-c f t</kbd> - Toggle model and factory

### Snippets

#### factory_girl

Binding | Snippet
--------|------------------------------------|
`aft`   | `after :hook do |model| ... end`   |
`aftb`  | `after :before do |model| ... end` |
`aftc`  | `after :create do |model| ... end` |
`afts`  | `after :stub do |model| ... end`   |
`bef`   | `before :create do |model| ... end`|
`deff`  | `FactoryGirl.define do ... end`    |
`fac`   | `factory :model do ... end`        |
`initw` | `initialize_with { ... }`          |
`seq`   | `sequence(:attribute) { |i| ... }` |
`trai`  | `trait :attribute do ... end`      |
`tran`  | `transient do ... end`             |


#### Fabrication

Binding | Snippet
--------|-----------------------------------------|
`aft`   | `after_hook do |model| ... end`         |
`aftb`  | `after_build do |model| ... end`        |
`aftc`  | `after_create do |model| ... end`       |
`aftv`  | `after_validation do |model| ... end`   |
`fab`   | `Fabricator :model do ... end`          |
`initw` | `initialize_with { ... }`               |
`seq`   | `sequence(:attribute) { |i| ... }`      |
`tran`  | `transient :attribute`                  |

## See Also

* [rspec-mode](https://github.com/pezra/rspec-mode)

## TODO

* Non-Rails projects
* More sophisticated snippets

## Author

Skye Shaw (skye.shaw ~AT~ gmail.com)
