# Uses AWS KMS to encrypt/decrypt S3 data.

[Releases](https://github.com/andreyk0/kms-s3/releases)

Note that KMS key parameter is only needed to encrypt and ignored
during decryption (S3 uses object metadata to load the key).

```bash
A utility to maintain KMS-encrypted files in S3.

Usage: kms-s3 ([-V|--version] | [-v|--verbose] [-p|--aws-profile ARG]
              [-r|--region ARG] [-k|--kms-key KEY_ARN] [-s|--s3-uri ARG]
              [-f|--file ARG] [COMMAND])
  Encrypt/Upload a file to S3 or Download/Decrypt it.Source:
  https://github.com/andreyk0/kms-s3

Available options:
  -h,--help                Show this help text
  -V,--version             Print version and exit.
  -v,--verbose             Be verbose.
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

# Install

```bash
curl -L https://raw.githubusercontent.com/gilt/kms-s3/master/install | /bin/bash
```


# S3 bucket SSE

Currently this tool assumes that S3 bucket implements an [AES256 SSE](https://aws.amazon.com/blogs/security/how-to-prevent-uploads-of-unencrypted-objects-to-amazon-s3/).

Example [bucket policy](eg-bucket-policy.json) that implements SSE.
Example [lifecycle](eg-bucket-lifecycle.json) policy that enables versioning (you really need that to prevent accidental data loss).


# Development

To build it from source you need [haskell stack](https://docs.haskellstack.org/en/stable/README/)
To build static linux binaries: [dockerized stack toolchain](https://github.com/andreyk0/docker-haskell-platform-alpine).
Makefile assumes it's in your PATH.
