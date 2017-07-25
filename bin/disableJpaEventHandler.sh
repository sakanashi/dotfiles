# eclipseでjpa ivent handler が無限ループする対応
# https://stackoverflow.com/questions/19649847/eclipse-kepler-jpa-project-change-event-handler-waiting/28049565#28049565
cd ~/.p2/pool/ # macはたぶんここ

mkdir -p disabled
mkdir -p disabled/features
mkdir -p disabled/plugins

mv plugins/org.eclipse.jpt.* disabled/plugins
mv features/org.eclipse.jpt.* disabled/features
