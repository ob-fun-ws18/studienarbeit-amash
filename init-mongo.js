use amash;

db.plugins.insert([
		{key: "de.scandio.confluence.plugins.pocketquery"},
		{key: "de.scandio.confluence.plugins.diary"},
		{key: "de.scandio.confluence.plugins.task-reminder"},
		{key: "com.jibrok.atlassian.jira.plugins.filter-matrix"}
	]
);

db.vendors.insert([
		{key: "1210714"},
		{key: "1210578"}
	]
);

/* Either uncomment this or run --dbsetup on first start
db.rankings.insert(
	{"application": "confluence"},
	{"application": "jira"},
	{"application": "bitbucket"}
);
*/

use admin;
db.createUser({user: "admin", pwd: "123", roles: ["root"]});