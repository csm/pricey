# pricey

Clojure library/app for figuring out how much your cloud costs you.

Lots of ideas taken from <https://github.com/powdahound/ec2instances.info>

## Quick Start

1. Have an AWS account. Have assets in that account, managed by
   [Asgard](https://github.com/Netflix/asgard) or now,
   [Spinnaker](http://www.spinnaker.io/).
2. Have credentials that let you read info from EC2. You'll need to
   pass your credentials to the `--access-key` and `--secret-key`
   options. Otherwise, you can put them in your `~/.aws/credentials`
   file, and specify the `--profile` argument.

And thus,

    $ lein run -- --profile test --daily
    ┌───────────────────┬──────────┬──────────┬──────────┐
    │               App │    Stack │ EC2 Cost │ EBS Cost │
    ├───────────────────┼──────────┼──────────┼──────────┤
    │           service │      dev │   $ 1.25 │   $ 0.24 │
    │           service │     test │   $ 1.25 │   $ 0.24 │
    │             Total │        - │   $ 2.50 │   $ 0.48 │
    └───────────────────┴──────────┴──────────┴──────────┘

Which is approximately what you will see for two clusters
`service-dev` and `service-test`, both running on `t2.medium` instance
types, and each with a 8GB root EBS device and another 30GB EBS
device. This is in the `us-west-2` region (the default).

Use the `--help` option for more goodies.

## Missing Stuff

* Tracking reserved or spot instances. Assumes everything is
  on-demand.
* Anything besides EC2 or EBS.
