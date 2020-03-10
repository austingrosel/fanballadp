sudo yum -y install python36

curl -O https://bootstrap.pypa.io/get-pip.py
python3 get-pip.py --user

pip3 install virtualenv
virtualenv venv --python=python36
source venv/bin/activate
pip3 install -r requirements.txt

sudo yum -y install docker
sudo service docker start