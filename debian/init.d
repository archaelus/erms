#! /bin/sh
#
# skeleton	Example initscript
#		This file should be used to construct scripts to be
#		placed in /etc/init.d.
#
# Author:	Miquel van Smoorenburg <miquels@cistron.nl>.
#		Ian Murdock <imurdock@gnu.ai.mit.edu>.
#
#		Please remove the "Author" lines above and replace them
#		with your own name if you copy and modify this script.
#
# Version:	@(#)skeleton  2.85-23  28-Jul-2004  miquels@cistron.nl
#

set -e

# Include erms defaults if available
if [ -f /etc/default/erms ] ; then
	. /etc/default/erms
fi

PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin:/usr/lib/erlang/bin
DESC=$ERMS_NAME
NAME=erms
DAEMON=/usr/lib/erlang/bin/erl

PIDFILE=/var/run/$NAME.pid
SCRIPTNAME=/etc/init.d/$NAME

NODE=${ERMS_NODE}@${ERMS_HOST}

START_ARGS="${ERMS_ERL_FLAGS} -name $NODE -boot releases/${ERMS_REL}/start -config priv/erms -pa lib/*/ebin -detached -heart"

HEART_COMMAND="$SCRIPTNAME heart_start"

# Gracefully exit if the package has been removed.
test -x $DAEMON || exit 0

#
#	Function that starts the daemon/service.
#  --chuid $USER:100
d_start() {
  export HEART_COMMAND
	#start-stop-daemon --chdir $ERMS_HOME --start --quiet --user $ERMS_USER --name beam \
	#	--exec $DAEMON -- $START_ARGS
  cd $ERMS_HOME
  # increase the number of file descriptors
  ulimit -n 65535
  su - $ERMS_USER -c "env HEART_COMMAND='$HEART_COMMAND' $DAEMON $START_ARGS"
}

#
#	Function that stops the daemon/service.
#
d_stop() {
  cd $ERMS_HOME
	echo -e "rpc:call('$NODE', init, stop, []), init:stop().\n" | $DAEMON -name con@$ERMS_HOST -setcookie `cat $ERMS_HOME/.erlang.cookie`
}

#
#	Function that sends a SIGHUP to the daemon/service.
#
d_reload() {
	start-stop-daemon --stop --quiet --pidfile $PIDFILE \
		--name $NAME --signal 1
}

case "$1" in
  start)
	echo -n "Starting $DESC: $NAME"
	d_start
	echo "."
	;;
  stop)
	echo -n "Stopping $DESC: $NAME"
	d_stop
	echo "."
	;;
  #reload)
	#
	#	If the daemon can reload its configuration without
	#	restarting (for example, when it is sent a SIGHUP),
	#	then implement that here.
	#
	#	If the daemon responds to changes in its config file
	#	directly anyway, make this an "exit 0".
	#
	# echo -n "Reloading $DESC configuration..."
	# d_reload
	# echo "done."
  #;;
  restart|force-reload)
	#
	#	If the "reload" option is implemented, move the "force-reload"
	#	option to the "reload" entry above. If not, "force-reload" is
	#	just the same as "restart".
	#
	echo -n "Restarting $DESC: $NAME"
	d_stop
	sleep 1
	d_start
	echo "."
	;;
  shell)
    cd $ERMS_HOME
    $DAEMON -name con@${ERMS_HOST} -config priv/erms -remsh $NODE -setcookie `cat $ERMS_HOME/.erlang.cookie`
  ;;
  remote)
    cd $ERMS_HOME
    $DAEMON -name con@${ERMS_HOST} -config priv/erms -setcookie `cat $ERMS_HOME/.erlang.cookie`
  ;;
  single)
    cd $ERMS_HOME
    su - $ERMS_USER -c "$DAEMON -name $NODE -config priv/erms -pa lib/*/ebin"
  ;;
  heart_start)
    cd $ERMS_HOME
    logger -p err -t $NAME 'ERMS System heartbeat timeout! Restarting...'
    $DAEMON $START_ARGS
  ;;
  *)
	# echo "Usage: $SCRIPTNAME {start|stop|restart|reload|force-reload}" >&2
	echo "Usage: $SCRIPTNAME {start|stop|restart|force-reload|shell|single|remote}" >&2
	exit 1
	;;
esac

exit 0
