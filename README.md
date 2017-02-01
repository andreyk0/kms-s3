# Uses AWS KMS to encrypt/decrypt S3 data.

[Releases](https://github.com/andreyk0/kms-s3/releases)


```bash
A utility to maintain KMS-encrypted files in S3.

Usage: kms-s3 [-v|--verbose] [-V|--version] [-p|--aws-profile ARG]
              [-r|--region ARG] (-k|--kms-key KEY_ARN) (-s|--s3-uri ARG)
              [-f|--file ARG] COMMAND
  Encrypt/Upload a file to S3 or Download/Decrypt it.Source:
  https://github.com/andreyk0/kms-s3

Available options:
  -h,--help                Show this help text
  -v,--verbose             Be verbose.
  -V,--version             Print version and exit.
  -p,--aws-profile ARG     AWS profile. Default config entry will be used if not
                           given.
  -r,--region ARG          AWS region.
  -k,--kms-key KEY_ARN     KMS master key, e.g.
                           arn:aws:kms:us-east-1:123456789012:alias/testkey, see
                           a list of keys in IAM console.
  -s,--s3-uri ARG          S3 URI
  -f,--file ARG            Name of the local file

Available commands:
  get                      Get file from S3 and decrypt.
  put                      Put file to S3 and encrypt.
```

# S3 bucket SSE

Currently this tool assumes that S3 bucket implements an [AES256 SSE](https://aws.amazon.com/blogs/security/how-to-prevent-uploads-of-unencrypted-objects-to-amazon-s3/).


# Development

To build it from source you need [haskell stack](https://docs.haskellstack.org/en/stable/README/)
At the moment it requres a local git clone of [amazonka](https://github.com/brendanhay/amazonka/tree/develop)
library on the same level as this project, should be able to remove that when
newer APIs are officially published. Make sure you checkout the *develop*
branch, at the moment it matters because it has a few important bug fixes.
For reference, everything compiles fine against amazonka dfe1ce5db10bba7d3fa7420d3da629731976047a.

To build static linux binaries: [dockerized stack toolchain](https://github.com/andreyk0/docker-haskell-platform-alpine).
Makefile assumes it's in your PATH.
