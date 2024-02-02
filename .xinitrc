##> [https://wiki.gentoo.org/wiki/IBus#Configuration]
export XMODIFIERS=@im=ibus
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus

# Use `xim` in case some Electron apps (like Chromium) refuse to work with IBus
# export GTK_IM_MODULE=xim
# export QT_IM_MODULE=xim

ibus-daemon -drx
##< End IBus

xrdb -merge ~/.Xresources
nm-applet &
setxkbmap -option ctrl:nocaps
eval `ssh-agent -s`
emacs --daemon
exec dbus-launch --exit-with-session i3
