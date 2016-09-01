#!/bin/bash

# tmux completion
# See: http://www.debian-administration.org/articles/317 for how to write more.
# Usage: Put "source bash_completion_tmux.sh" into your .bashrc
# Based upon the example at http://paste-it.appspot.com/Pj4mLycDE

_tmux_expand () 
{ 
    [ "$cur" != "${cur%\\}" ] && cur="$cur"'\';
    if [[ "$cur" == \~*/* ]]; then
        eval cur=$cur;
    else
        if [[ "$cur" == \~* ]]; then
            cur=${cur#\~};
            COMPREPLY=($( compgen -P '~' -u $cur ));
            return ${#COMPREPLY[@]};
        fi;
    fi
}

_tmux_filedir () 
{ 
    local IFS='
';
    _tmux_expand || return 0;
    if [ "$1" = -d ]; then
        COMPREPLY=(${COMPREPLY[@]} $( compgen -d -- $cur ));
        return 0;
    fi;
    COMPREPLY=(${COMPREPLY[@]} $( eval compgen -f -- \"$cur\" ))
}

function _tmux_complete_client() {
    local IFS=$'\n'
    local cur="${1}"
    COMPREPLY=( ${COMPREPLY[@]:-} $(compgen -W "$(tmux -q list-clients | cut -f 1 -d ':')" -- "${cur}") )
}
function _tmux_complete_session() {
    local IFS=$'\n'
    local cur="${1}"
    COMPREPLY=( ${COMPREPLY[@]:-} $(compgen -W "$(tmux -q list-sessions | cut -f 1 -d ':')" -- "${cur}") )
}
function _tmux_complete_window() {
    local IFS=$'\n'
    local cur="${1}"
    local session_name="$(echo "${cur}" | sed 's/\\//g' | cut -d ':' -f 1)"
    local sessions

    sessions="$(tmux -q list-sessions | sed -re 's/([^:]+:).*$/\1/')"
    if [[ -n "${session_name}" ]]; then
        sessions="${sessions}
$(tmux -q list-windows -t "${session_name}" | sed -re 's/^([^:]+):.*$/'"${session_name}"':\1/')"
    fi
    cur="$(echo "${cur}" | sed -e 's/:/\\\\:/')"
    sessions="$(echo "${sessions}" | sed -e 's/:/\\\\:/')"
    COMPREPLY=( ${COMPREPLY[@]:-} $(compgen -W "${sessions}" -- "${cur}") )
}

_tmux() {
    local cur prev
    local i cmd cmd_index option option_index
    local opts=""
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    if [ ${prev} == -f ]; then
        _tmux_filedir
    else
    # Search for the command
    local skip_next=0
    for ((i=1; $i<=$COMP_CWORD; i++)); do
        if [[ ${skip_next} -eq 1 ]]; then
            #echo "Skipping"
            skip_next=0;
        elif [[ ${COMP_WORDS[i]} != -* ]]; then
            cmd="${COMP_WORDS[i]}"
            cmd_index=${i}
            break
        elif [[ ${COMP_WORDS[i]} == -f ]]; then
            skip_next=1
        fi
    done

    # Search for the last option command
    skip_next=0
    for ((i=1; $i<=$COMP_CWORD; i++)); do
        if [[ ${skip_next} -eq 1 ]]; then
            #echo "Skipping"
            skip_next=0;
        elif [[ ${COMP_WORDS[i]} == -* ]]; then
            option="${COMP_WORDS[i]}"
            option_index=${i}
            if [[ ${COMP_WORDS[i]} == -- ]]; then
                break;
            fi
        elif [[ ${COMP_WORDS[i]} == -f ]]; then
            skip_next=1
        fi
    done

    if [[ $COMP_CWORD -le $cmd_index ]]; then
        # The user has not specified a command yet
        local all_commands="$(tmux -q list-commands | cut -f 1 -d ' ')"
        COMPREPLY=( ${COMPREPLY[@]:-} $(compgen -W "${all_commands}" -- "${cur}") )
    else        
        case ${cmd} in
            attach-session|attach)
            case "$prev" in
                -t) _tmux_complete_session "${cur}" ;;
                *) options="-t -d" ;;
            esac ;;
            detach-client|detach)
            case "$prev" in
                -t) _tmux_complete_client "${cur}" ;;
                *) options="-t" ;;
            esac ;;
            lock-client|lockc)
            case "$prev" in
                -t) _tmux_complete_client "${cur}" ;;
                *) options="-t" ;;
            esac ;;
            lock-session|locks)
            case "$prev" in
                -t) _tmux_complete_session "${cur}" ;;
                *) options="-t -d" ;;
            esac ;;
            new-session|new)
            case "$prev" in
                -t) _tmux_complete_session "${cur}" ;;
                -[n|d|s]) options="-d -n -s -t --" ;;
                *) 
                if [[ ${COMP_WORDS[option_index]} == -- ]]; then
                    _command_offset ${option_index}
                else
                    options="-d -n -s -t --"
                fi
                ;;
            esac
            ;;
            refresh-client|refresh)
            case "$prev" in
                -t) _tmux_complete_client "${cur}" ;;
                *) options="-t" ;;
            esac ;;
            rename-session|rename)
            case "$prev" in
                -t) _tmux_complete_session "${cur}" ;;
                *) options="-t" ;;
            esac ;;
            source-file|source) _tmux_filedir ;;
            has-session|has|kill-session)
            case "$prev" in
                -t) _tmux_complete_session "${cur}" ;;
                *) options="-t" ;;
            esac ;;
            suspend-client|suspendc)
            case "$prev" in
                -t) _tmux_complete_client "${cur}" ;;
                *) options="-t" ;;
            esac ;;
            switch-client|switchc)
            case "$prev" in
                -c) _tmux_complete_client "${cur}" ;;
                -t) _tmux_complete_session "${cur}" ;;
                *) options="-l -n -p -c -t" ;;
            esac ;;

            send-keys|send)
            case "$option" in
                -t) _tmux_complete_window "${cur}" ;;
                *) options="-t" ;;
            esac ;;
          esac # case ${cmd}
        fi # command specified
      fi # not -f 

      if [[ -n "${options}" ]]; then
          COMPREPLY=( ${COMPREPLY[@]:-} $(compgen -W "${options}" -- "${cur}") )
      fi

      return 0

}
complete -F _tmux tmux

# END tmux completion

# tmux command completion
#
# tmux completion
# by Fahd Shariff
#
_tmux() {
  # an array of commands and their options
  declare -A tmux_cmd_map
  tmux_cmd_map=( ["attach-session"]="-dr -t target-session" \
                 ["bind-key"]="-cnr -t key-table key command arguments" \
                 ["break-pane"]="-d -t target-pane" \
                 ["capture-pane"]="-b buffer-index -E end-line -S start-line -t target-pane" \
                 ["choose-buffer"]="-t target-window template" \
                 ["choose-client"]="-t target-window template" \
                 ["choose-session"]="-t target-window template" \
                 ["choose-window"]="-t target-window template" \
                 ["clear-history"]="-t target-pane" \
                 ["clock-mode"]="-t target-pane" \
                 ["command-prompt"]="-I inputs -p prompts -t target-client template" \
                 ["confirm-before"]="-p prompt -t target-client command" \
                 ["copy-mode"]="-u -t target-pane" \
                 ["delete-buffer"]="-b buffer-index" \
                 ["detach-client"]="-P -s target-session -t target-client" \
                 ["display-message"]="-p -c target-client -t target-pane message" \
                 ["display-panes"]="-t target-client" \
                 ["find-window"]="-t target-window match-string" \
                 ["has-session"]="-t target-session" \
                 ["if-shell"]="shell-command command" \
                 ["join-pane"]="-dhv -p percentage|-l size -s src-pane -t dst-pane" \
                 ["kill-pane"]="-a -t target-pane" \
                 ["kill-server"]="kill-server" \
                 ["kill-session"]="-t target-session" \
                 ["kill-window"]="-t target-window" \
                 ["last-pane"]="-t target-window" \
                 ["last-window"]="-t target-session" \
                 ["link-window"]="-dk -s src-window -t dst-window" \
                 ["list-buffers"]="list-buffers" \
                 ["list-clients"]="-t target-session" \
                 ["list-commands"]="list-commands" \
                 ["list-keys"]="-t key-table" \
                 ["list-panes"]="-as -t target" \
                 ["list-sessions"]="list-sessions" \
                 ["list-windows"]="-a -t target-session" \
                 ["load-buffer"]="-b buffer-index path" \
                 ["lock-client"]="-t target-client" \
                 ["lock-server"]="lock-server" \
                 ["lock-session"]="-t target-session" \
                 ["move-window"]="-dk -s src-window -t dst-window" \
                 ["new-session"]="-d -n window-name -s session-name -t target-session -x width -y height command" \
                 ["new-window"]="-adk -n window-name -t target-window command" \
                 ["next-layout"]="-t target-window" \
                 ["next-window"]="-a -t target-session" \
                 ["paste-buffer"]="-dr -s separator -b buffer-index -t target-pane" \
                 ["pipe-pane"]="-t target-pane-o command" \
                 ["previous-layout"]="-t target-window" \
                 ["previous-window"]="-a -t target-session" \
                 ["refresh-client"]="-t target-client" \
                 ["rename-session"]="-t target-session new-name" \
                 ["rename-window"]="-t target-window new-name" \
                 ["resize-pane"]="-DLRU -t target-pane adjustment" \
                 ["respawn-pane"]="-k -t target-pane command" \
                 ["respawn-window"]="-k -t target-window command" \
                 ["rotate-window"]="-DU -t target-window" \
                 ["run-shell"]="command" \
                 ["save-buffer"]="-a -b buffer-index" \
                 ["select-layout"]="-np -t target-window layout-name" \
                 ["select-pane"]="-lDLRU -t target-pane" \
                 ["select-window"]="-lnp -t target-window" \
                 ["send-keys"]="-t target-pane key " \
                 ["send-prefix"]="-t target-pane" \
                 ["server-info"]="server-info" \
                 ["set-buffer"]="-b buffer-index data" \
                 ["set-environment"]="-gru -t target-session name value" \
                 ["set-option"]="-agsuw -t target-session|target-window option value" \
                 ["set-window-option"]="-agu -t target-window option value" \
                 ["show-buffer"]="-b buffer-index" \
                 ["show-environment"]="-g -t target-session" \
                 ["show-messages"]="-t target-client" \
                 ["show-options"]="-gsw -t target-session|target-window" \
                 ["show-window-options"]="-g -t target-window" \
                 ["source-file"]="path" \
                 ["split-window"]="-dhvP -p percentage|-l size -t target-pane command" \
                 ["start-server"]="start-server" \
                 ["suspend-client"]="-t target-client" \
                 ["swap-pane"]="-dDU -s src-pane -t dst-pane" \
                 ["swap-window"]="-d -s src-window -t dst-window" \
                 ["switch-client"]="-lnp -c target-client -t target-session" \
                 ["unbind-key"]="-acn -t key-table key" \
                 ["unlink-window"]="-k -t target-window" )
 
   declare -A tmux_alias_map
   tmux_alias_map=( ["attach"]="attach-session" \
                  ["detach"]="detach-client" \
                  ["has"]="has-session" \
                  ["lsc"]="list-clients" \
                  ["lscm"]="list-commands" \
                  ["ls"]="list-sessions" \
                  ["lockc"]="lock-client" \
                  ["locks"]="lock-session" \
                  ["new"]="new-session" \
                  ["refresh"]="refresh-client" \
                  ["rename"]="rename-session" \
                  ["showmsgs"]="show-messages" \
                  ["source"]="source-file" \
                  ["start"]="start-server" \
                  ["suspendc"]="suspend-client" \
                  ["switchc"]="switch-client" \
                  ["breakp"]="break-pane" \
                  ["capturep"]="target-pane]" \
                  ["displayp"]="display-panes" \
                  ["findw"]="find-window" \
                  ["joinp"]="join-pane" \
                  ["killp"]="kill-pane" \
                  ["killw"]="kill-window" \
                  ["lastp"]="last-pane" \
                  ["last"]="last-window" \
                  ["linkw"]="link-window" \
                  ["lsp"]="list-panes" \
                  ["lsw"]="list-windows" \
                  ["movew"]="move-window" \
                  ["neww"]="new-window" \
                  ["nextl"]="next-layout" \
                  ["next"]="next-window" \
                  ["pipep"]="pipe-pane" \
                  ["prevl"]="previous-layout" \
                  ["prev"]="previous-window" \
                  ["renamew"]="rename-window" \
                  ["resizep"]="resize-pane" \
                  ["respawnp"]="respawn-pane" \
                  ["respawnw"]="respawn-window" \
                  ["rotatew"]="rotate-window" \
                  ["selectl"]="select-layout" \
                  ["selectp"]="select-pane" \
                  ["selectw"]="select-window" \
                  ["splitw"]="[shell-command]" \
                  ["swapp"]="swap-pane" \
                  ["swapw"]="swap-window" \
                  ["unlinkw"]="unlink-window" \
                  ["bind"]="bind-key" \
                  ["lsk"]="list-keys" \
                  ["send"]="send-keys" \
                  ["unbind"]="unbind-key" \
                  ["set"]="set-option" \
                  ["setw"]="set-window-option" \
                  ["show"]="show-options" \
                  ["showw"]="show-window-options" \
                  ["setenv"]="set-environment" \
                  ["showenv"]="show-environment" \
                  ["confirm"]="confirm-before" \
                  ["display"]="display-message" \
                  ["clearhist"]="clear-history" \
                  ["deleteb"]="delete-buffer" \
                  ["lsb"]="list-buffers" \
                  ["loadb"]="load-buffer" \
                  ["pasteb"]="paste-buffer" \
                  ["saveb"]="save-buffer" \
                  ["setb"]="set-buffer" \
                  ["showb"]="show-buffer" \
                  ["if"]="if-shell" \
                  ["lock"]="lock-server" \
                  ["run"]="run-shell" \
                  ["info"]="server-info" )
 
   local cur="${COMP_WORDS[COMP_CWORD]}"
   local prev="${COMP_WORDS[COMP_CWORD-1]}"
   COMPREPLY=()
 
   # completing an option
   if [[ "$cur" == -* ]]; then
     #tmux options
     if [[ "$prev" == "tmux" ]]; then
         COMPREPLY=( $( compgen -W "-2 -8 -c -f -L -l -q -S -u -v -V" -- $cur ) )
     else
         #find the tmux command so that we can complete the options
         local cmd="$prev"
         local i=$COMP_CWORD
         while [[ "$cmd" == -* ]]
         do
             cmd="${COMP_WORDS[i]}"
             ((i--))
         done
 
         #if it is an alias, look up what the alias maps to
         local alias_cmd=${tmux_alias_map[$cmd]}
         if [[ -n ${alias_cmd} ]]
         then
             cmd=${alias_cmd}
         fi
 
         #now work out the options to this command
         local opts=""
         for opt in ${tmux_cmd_map[$cmd]}
         do
              if [[ "$opt" == -* ]]; then
                  len=${#opt}
                  i=1
                  while [ $i -lt $len ]; do
                      opts="$opts -${opt:$i:1}"
                      ((i++))
                  done
              fi
         done
         COMPREPLY=($(compgen -W "$opts" -- ${cur}))
     fi
   else
     COMPREPLY=($(compgen -W "$(echo ${!tmux_cmd_map[@]} ${!tmux_alias_map[@]})" -- ${cur}))
   fi
   return 0
}
complete -F _tmux tmux
#END tmux command completion
