library(fflr)
library(ffscrapr)
library(ggplot2)
library(gganimate)
library(plotly)
library(ggimage)
library(ggh4x)
library(readr)

#ffscrapr

#cookies={"swid": "{9FD32A86-1182-4CAD-B5AB-7EE3B41FB49A}",
  #"espn_s2": "AECOSyKMziz1LEuqBbnBNbJBC88fe6giYS91L8QYbj7nxKCUKDuQ%2Bd%2F9MlIr9MbsBfySROG%2FM2RT%2FoI9dVPONemls201ApfgLM49SY7%2Bdgfp8WN6iY%2FPZY6JEGzYskPxOXi6JW4KEGhOig%2FHCMf%2FwRC1u5aJdZAS1TSDGc0lBXFcKZkWAQzX8W64pn82RUBrHV59z4zFuD3BHR54VhWontIj1p2XhWw2dluidyo4MCNGvIU8VcxGQvpBdd1aGIYkn%2FNYgd65CztouPoFRdD6zBKkfE3CypBvv0GphEx3cthNla6HDNFen6xjs5qaXGnXMoCT6DTzs89NwvD74H0OBjad"}

#write.csv(DataFrame Name, "Path to export the DataFrame\\File Name.csv", row.names=FALSE)
#cookie_csv <- data.frame(espn_s2 = c("AECOSyKMziz1LEuqBbnBNbJBC88fe6giYS91L8QYbj7nxKCUKDuQ%2Bd%2F9MlIr9MbsBfySROG%2FM2RT%2FoI9dVPONemls201ApfgLM49SY7%2Bdgfp8WN6iY%2FPZY6JEGzYskPxOXi6JW4KEGhOig%2FHCMf%2FwRC1u5aJdZAS1TSDGc0lBXFcKZkWAQzX8W64pn82RUBrHV59z4zFuD3BHR54VhWontIj1p2XhWw2dluidyo4MCNGvIU8VcxGQvpBdd1aGIYkn%2FNYgd65CztouPoFRdD6zBKkfE3CypBvv0GphEx3cthNla6HDNFen6xjs5qaXGnXMoCT6DTzs89NwvD74H0OBjad"),
                         #swid = c("9FD32A86-1182-4CAD-B5AB-7EE3B41FB49A"))
#write.csv(cookie_csv, "cookies\\espn_cookie.csv", row.names=FALSE)

#create a folder called cookies
#create a csv file in the cookies folder
#the csv file should have a column with 2 columns
#2 columns should be s2 and swid cookie values
cookie_df <- read.csv("cookies\\espn_cookie.csv", encoding = "UTF-8")
cookie_df

TAN_ESPN_S2=cookie_df$espn_s2
TAN_ESPN_swid=cookie_df$swid

#TAN_ESPN_S2="AECOSyKMziz1LEuqBbnBNbJBC88fe6giYS91L8QYbj7nxKCUKDuQ%2Bd%2F9MlIr9MbsBfySROG%2FM2RT%2FoI9dVPONemls201ApfgLM49SY7%2Bdgfp8WN6iY%2FPZY6JEGzYskPxOXi6JW4KEGhOig%2FHCMf%2FwRC1u5aJdZAS1TSDGc0lBXFcKZkWAQzX8W64pn82RUBrHV59z4zFuD3BHR54VhWontIj1p2XhWw2dluidyo4MCNGvIU8VcxGQvpBdd1aGIYkn%2FNYgd65CztouPoFRdD6zBKkfE3CypBvv0GphEx3cthNla6HDNFen6xjs5qaXGnXMoCT6DTzs89NwvD74H0OBjad"
#TAN_SWID="{9FD32A86-1182-4CAD-B5AB-7EE3B41FB49A}"



conn <- espn_connect(
  season = 2021,
  league_id = 53948101,
  espn_s2 = TAN_ESPN_S2,
  swid = TAN_ESPN_swid
)

ff_logos_df <- data.frame(image = sample(c("ff_logos/camel_outline_xtra5.png"),
                                            replace = TRUE))

files <- list.files(path="ff_logos", pattern="*.png", full.names=TRUE, recursive=FALSE)
ff_logos_df <- data.frame(image = c(files))
ff_logos_df$team_id <- c(4,8,5,9,6,3,2,11,1,10,7)
ff_logos_df

team_id_pic <- c(4,8,5,9,6,3,2,10,1,11,7)

#Jay - 1
#Carter - 2
#Merry - 3
#Boof - 4
#Chafey - 5
#Luke - 6
#West - 7
#Carson - 8
#Shannon - 9 
#Zombies - 10

ff_league(conn)

#Left Join: jointdataset <- merge(ChickWeight, LabResults, by = 'Diet', all.x= TRUE)

py_data <- read.csv("glucose_formula_2022_week_12", encoding = "UTF-8")
py_data

id_teams <- py_data[,c("win_id","Win_Team_first","Win_Team_name")]
id_teams <- id_teams[!duplicated(id_teams[c(1,3)]),]
colnames(id_teams) <- c("team_id", "Team_first","Team_name")

py_win_id = py_data[,c("Week","win_id","win_score","lose_score")]
colnames(py_win_id) <- c("Week",'team_id','team_score','against_score')

py_lose_id = py_data[,c("Week","lose_id","lose_score","win_score")]
colnames(py_lose_id) <- c("Week",'team_id','team_score','against_score')

py_score_id = rbind(py_win_id,py_lose_id)
py_score_id <- py_score_id[order(py_score_id$Week),]
py_score_id$csum <- ave(py_score_id$team_score, py_score_id$team_id, FUN=cumsum)
py_score_id$against_csum <- ave(py_score_id$against_score, py_score_id$team_id, FUN=cumsum)
py_score_id
jointdataset <- merge(py_score_id, id_teams, by = 'team_id', all.x= TRUE)
jointdataset <- merge(jointdataset, ff_logos_df, by = 'team_id', all.x= TRUE)
jointdataset <- jointdataset[order(jointdataset$Week),]
jointdataset <- jointdataset[jointdataset$Week < 13,]
jointdataset



#current week plot
week = 12
curr_week_df <- jointdataset[jointdataset$Week == week,]
repeat_week <- rep(week, each=10)
repeat_df <- data.frame(d = repeat_week)
curr_week_df$avg_csum <- curr_week_df[["csum"]]/repeat_df$d
curr_week_df$against_csum_avg <- curr_week_df[["against_csum"]]/repeat_df$d
divide_v <- data.frame(d=c(week))
avg_pts = mean(curr_week_df[["csum"]])/divide_v$d
min_pts <- min(curr_week_df$csum)/divide_v$d
max_pts <- max(curr_week_df$csum)/divide_v$d
diff_pts <- (max_pts - min_pts)
offset_label <- max_pts - diff_pts * .1


low_x <- min_pts
high_x <- max_pts
low_y <- min_pts
high_y <- max_pts
x_cord_label <- c(low_x,low_x,high_x,high_x)
y_cord_label <- c(high_y,low_y,low_y,high_y)
xy_cord <- data.frame (x_cord  = c(low_x,low_x,high_x,high_x),
                       y_cord = c(high_y,low_y,low_y,high_y)
)


avg_pts_label <- paste("Average Pts:", avg_pts, sep=" ")
min_score_text <- c(min_pts,avg_pts)
lowhigh_graph_label <- c("Low Points Scored\nHigh Points Against","Low Points Scored\nLow Points Against",
                        "High Points Scored\nLow Points Against","High Points Scored\nHigh Points Against")

color_c <- c("blue","green","orange","red","yellow","black","grey","blue","black","blue")
color_c <- ""
primary_color = "black"
background_c = "white"
image_background = "grey"

p <- ggplot() +
  geom_hline(yintercept = avg_pts,
             color = primary_color) +
  geom_vline(xintercept = avg_pts,
             color = primary_color) +
  geom_image(data = curr_week_df,
             aes(x=curr_week_df[['avg_csum']],y=curr_week_df[['against_csum_avg']],
                 image=curr_week_df[['image']]),color = image_background,
             size = .06,) +
  geom_image(data = curr_week_df,
             aes(x=curr_week_df[['avg_csum']],y=curr_week_df[['against_csum_avg']],image=curr_week_df[['image']]),
             size = .05) +
  geom_text(label = lowhigh_graph_label,
            data = xy_cord,
            aes(x=xy_cord[['x_cord']],y=xy_cord[['y_cord']]),
            check_overlap = TRUE,
            vjust=c("inward","inward","inward","inward"),
            hjust=c("inward","inward","inward","inward")) +
  xlab("Average Points Against") +
  ylab("Average Points Scored") +
  theme(plot.background=element_rect(fill = background_c),
        panel.background = element_rect(fill = background_c),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=14, color = primary_color),
        axis.text.x = element_text(size=15, color = primary_color),
        axis.text.y = element_text(size=15, color = primary_color))
p

###########


p <- ggplot(jointdataset, aes(x=jointdataset[['Week']],y=jointdataset[['csum']],color=jointdataset[['team_id']])) +
  geom_point(aes(frame = jointdataset[['Week']])) +
  theme(legend.position = "none")
animate(p)


p <- ggplot(jointdataset, 
            aes(x=jointdataset[['Week']],y=jointdataset[['csum']],group=jointdataset[['team_id']],colour=jointdataset[['Team_name']])) +
  geom_line()
p

p + 
  transition_reveal(jointdataset[['Week']]) +
  view_follow()

transparent <- function(img) {
  magick::image_fx(img, expression = "0.5*a", channel = "alpha")
}

p <- ggplot(jointdataset, 
            aes(x=jointdataset[['Week']],y=jointdataset[['csum']],image=jointdataset[['image']])) +
  geom_image(aes(frame = jointdataset[['Week']]),size = .05,image_fun = transparent) +
  theme(legend.position = "none") +
  view_follow(fixed_y = TRUE) +
  shadow_wake(wake_length = 0.1)
p
animate(p,end_pause = 10,fps = 2.5)


x <- list(
  title = "X Title"
)
y <- list(
  title = "Y Title"
)

ggplotly(p) %>%
  layout(title = "title",xaxis = x, yaxis = y)
