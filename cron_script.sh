cd fanballadp
source venv/bin/activate
sudo docker run -d -p 4444:4444 --shm-size=8g selenium/standalone-chrome
python adp.py RunAll --local-scheduler --date-interval 2020-02-04-`date +%Y-%m-%d`
sudo docker stop $(sudo docker ps -aq)
deactivate
