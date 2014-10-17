README_aws.txt

Date: 10/14/2014

- Launched EC2 ts.micro instance with ID i-16dd9efb to configure prism on AMI. 
-- to login: 
ssh -i ~/Dropbox/eyal_ec2.pem  ec2-user@54.172.200.124

--To install emacs on instance: 
>> sudo yum install emacs
-- To install PRISM: 
>> curl http://sato-www.cs.titech.ac.jp/prism/download/prism22a1_linux.tar.gz -o prism22a1_linux.tar.gz
>>  tar -xvzf prism22a1_linux.tar.gz 

-- To add PRISM to path, in ~/.bashrc: 
"
## PRISM                                                                  
export PATH=~/prism/bin:$PATH
"

-- Install Git
>> sudo yum install git

-- To get GIJoe: 
>>  git clone git@github.com:edechter/GrammarInduction.git

-- Saved AMI as GIJoePrism ami-6ce95104
-- Stopped  i-16dd9efb and launched i-f52b2b1e (r3.2xlarge) with AMI GIJoePrism
-- to login: ssh -i ~/Dropbox/eyal_ec2.pem ec2-user@ec2-54-166-192-222.compute-1.amazonaws.com







23650
