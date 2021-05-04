## AWS Widgets for R

(Work in progress)

#### Install
```
brew install r pandoc
Rscript -e 'install.packages("devtools", repos = "http://cran.us.r-project.org")'
Rscript -e 'devtools::install_github("srizzo/awswidgets")'
```
   

#### Download/view traces/logs              
```
git clone https://github.com/srizzo/awswidgets.git && cd awswidgets
export PATH="${PWD}/bin:$PATH"  

export START_TIME=${START_TIME:-"$(date -v '-6H' "+%Y-%m-%d %H:%M:%S")"}
export END_TIME=${END_TIME:-"$(date "+%Y-%m-%d %H:%M:%S")"}

xray-fetch-query 'http.url CONTAINS "[/api/path/]"'
xray-view

export LOG_GROUP_NAMES="/ecs/app"
cloudwatch-fetch-query "@message like '[message]'"
cloudwatch-view
```
