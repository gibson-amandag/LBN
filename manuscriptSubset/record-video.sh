#!/bin/bash

# ffmpeg is calling the program
# f avfoundation - This allows the capturing of webcam input on mac
# frame rate is x
# -i camera input
# vcodec and pix_fmt part of quicktime fix
# -flags Adds headers to each segment
# -f segment Enables segments
# Segment time in sec
# strftime Have to do with allowing file time pieces https://ffmpeg.org/ffmpeg-formats.html
# save as mp4
# ???? for segment format options
# reset timestamps start the new segment timestamp back over from 0
# -vf Add the time stamp within the video output
# save the file with this name

#video name - no default. Type after name of script in command line. Ex D013
#cameraNumber - default to 0. This is the input number for the camera that will be recorded from

# 2022-11-06 - Added %Z to the filename for the timezone, also included in the timestamp text on the video
# Tried to add index counter for the segments to prevent overwritting, but wasn't successful
# at getting this to incorporate into the output file name
# -hls_flags second_level_segment_index

# Be sure to appropriately edit "editUserPath" in the second to last line for draw text

#removed
# -framerate 30 \
# -pix_fmt uyvy422 \

videoName=${1:-}
cameraNumber=${2:-0}

ffmpeg \
-f avfoundation \
-framerate 30 \
-pixel_format yuyv422 \
-video_size 640x480 \
-i "$cameraNumber:none" \
-vcodec libx264 \
-pix_fmt yuv420p \
-flags +global_header \
-vsync "2" \
-f segment \
-segment_time 3600 \
-segment_atclocktime 1 \
-strftime 1 \
-segment_format mp4 \
-segment_format_options movflags=+faststart \
-reset_timestamps 1 \
-vf "drawtext='fontfile=/editUserPath/myLabVideos/fonts/Roboto-Regular.ttf':fontsize=16:fontcolor=yellow:box=1:boxcolor=black@0.3:text='${videoName} %{localtime\:%Y-%m-%d %X %Z}'" \
~/myLabVideos/${videoName}/${videoName}_%Y-%m-%d_%H-%M-%S-%Z.mp4