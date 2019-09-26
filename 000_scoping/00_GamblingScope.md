---
title: "PIPE-870: Analysing Responsible Gambling Tools"
author: "Darya Vanichkina"
date: "26/09/2019"
output: 
  redoc::redoc:
    keep_md: true
---

<div class="redoc" id="redoc-codechunk-1">


</div>

## 1. Project details

- Client: A.Prof. Sally Gainsbury
- Faculty: Psychology
- Research centre: Brain and Mind Centre

### Researcher Availability

- Who is the active person helping?
- How available are they to participate in this study?


## 2. Project summary

### Research context

Research commissioned by consortium of online wagering operators, “Responsible Wagering Australia”.

This research aims to 

1. Understand use of responsible gambling tools (e.g., setting deposit limits, using temporary time-outs, enacting self-exclusion orders) among online wagering customers (e.g., sports and race bettors) in terms of what types of customers are using the tools and the apparent impact of tool use on gambling behaviours (e.g., time and money spent); and 
2. Evaluate the impact of interventions in the form of messages/notifications that will encourage use of responsible gambling tools on use of tools and subsequent gambling behaviours.

This is the first project in Australia to examine customer account data from multiple online wagering operators and one of the first internationally to do so. It will be the first project to conduct and evaluate interventions to encourage consumer protection tool use across multiple operators.

Online wagering is the fastest growing form of gambling in Australia and harms related to online wagering are rising dramatically, particularly among young men. Interventions to encourage use of consumer protection tools are critical to reduce harms experienced by individuals, families, and the broader community. However, preliminary research by the team indicates that less than one-quarter of online wagering customers use limit-setting tools and less than 10% use time-out tools

Further funding could potentially be requested.

### Client needs

- Describe the use of responsible gambling tools (RGTs) in 12 months of historical data. 
- Build software that can reapply this description to new data, so that it can be harnessed in intervention-based experiments.
- Build software that identifies subpopulations in the customers, building on expertise and prior literature around the characteristics of high risk gamblers.

### Current data
- We currently have 12-months of historical data from 4 major Australian online wagering operators. All a random sub-section of clients: Operator 1 N=2,000, operator 2 N=10,000, Operator 3 N=2000, operator 4 N=10,000. 
The data is in the form of excel spreadsheets. Some initial data cleaning has been done and list of variables created. We expect further data from other operators. There are 16,125 “veteran” customers so far. These are customers who have registered prior to 2017-07-01 history (and did not close their account within our time-frame) in which case we have the full 12-months of wagering.
Of there veterans customers, only 5,391 (33.43%) actually wager during our time-frame. For all customers, 37.69% (8,292/22,000) wagered during our time-frame.
- Nick Ho prescribed the data format and performed some cleaning, so should be relatively prepared, although not all wagering operators have provided data yet. Initial analysis at https://pages.github.sydney.edu.au/niho7662/transactional_behaviour/exploreData_operators234.html

## 3. Project Implementation

### Project plan

- (.5 week) Become acquainted with data, assisted by Nick Ho’s prior work.
- (1 week) Develop software to report statistics on usage and parameters of RGTs per operator or cohort. Much of this has been done, and just needs packaging for reusability.
- (1 week) Develop techniques to identify population segments suggesting high vs low risk, guided by research expertise and/or data. Segment statistical results by these groups.
- (3 weeks) Further analysis directed by the researchers towards describing customer behaviours around RGTs, such as:
o Behaviour before and after applying RGTs
o Amount of deposit limits
o Length of exclusion periods
o Repeated use of tools and their cancellation


### Scheduling

- Post-doc for intervention study starts July

### Deliverables

- Tool to segment datasets into different customer groups.
- Tool to report RGT usage for a particular dataset.
- Documentation and packaging to make them usable.

### SIH skills required

- Heterogeneous time series analysis
- Skewed data
- R (for continuity with Nick Ho’s work)

### In scope

### Out of scope
- More than 6 weeks FTE work


### Acceptance Criteria
- Software analyses behaviours around RGT use.
- Analysis can easily be applied by someone with basic capabilities in R to a new dataset.

### Handover plan
- Provide documented, reusable analysis library on GitHub Enterprise

## 4. Project Evaluation

### SIH Benefits (Summary)
- Towards informing government policy.
- Work could be funded.

### Cost (FTE weeks)
- 6 weeks FTE

***

SIH staff will fill out the below

***

## 5. Essential criteria

 | |
|:------ | :------ |
Sydney Researcher | Y: Sally Gainsbury
Clear project statement | Y
Data quality | Y
Solvable | Y 
Domain Expert | Y
Planned | Y
Strategic Plan | Y
Data Science | Y
Priority | Y: Health
Deliverables | Y
Infrastructure | Y

## 6. Value and Impact

### Drives the success of larger groups, larger collaborations or high-profile projects.

- Score: 8/10
- Details:
Name of Research Group/Centre: Gambling Treatment and Research Clinic, Brain and Mind Centre
Name of Collaboration: Problematic Risk Taking and Emerging Technologies (PRET) BMC Team – a multidisciplinary collaboration across USyd


### Publications that are high-impact - foundational, highly cited, with potential for wider adoption of methods and knock-on outcomes.

- Score: 8/10
1. Target Journal: Psychology of Addictive Behaviors (Impact Factor 2.543)
- Name of Authors: Gainsbury, Heirene, Blaszczynski (name of SIH data analyst)
- Proposed Paper Title: Investigating consumer protection tool use among online wagering customers

2. Target Journal: Addiction, Impact Factor 5.789
- Name of Authors: Gainsbury, Heirene, Blaszczynski (name of SIH data analyst)
- Proposed Paper Title: Subpopulations of online wagering consumers: A risk profile

3. Target Journal: Addictive Behaviors, Impact Factor 2.686
- Name of Authors: Gainsbury, Heirene, Blaszczynski (name of SIH data analyst)
- Proposed Paper Title: Impact of online wagering consumer protection tools

### Potential for patents, start-ups, open-source code, policy influence.

- Policy influence 8/10
- The Commonwealth Government is committed to increasing consumer protection on online wagering sites and has launched a National Consumer Protection Framework. This includes a recommendation (#5) that operators offer customers opportunities to set voluntary limits on their wagering and that customers should be promoted about setting or reviewing limits on a regular basis. Recommendation 13 states that nationally consistent and standard messaging should be introduced that would assist efforts in responsible gambling. This research is consistent with this framework and will have a significant impact on national and international gambling policy and regulation.


### Develop funding opportunities for the university and/or SIH, such as large CoE-scale grants and further involvement with successful and well funded groups.

- Score: 8/10
- Name of Grant Application: National Centre for Responsible Gambling (US) large grant: Comparison of use of consumer protection tools across international wagering sites 
- Date of Proposed Submission: September 2020

### Total score

32/40 (refer to Rubric for scoring guide)

## 7. KPI

 | |
|:------ | :------ |
Papers SIH Coauthors (*measure only, not KPI) | N
Number of Papers: Papers SIH Acknowledged | Y - Number of Papers: 3
Collaborators who publish | Name: Sally Gainsbury
Collaborators who have grants | Name: Sally Gainsbury
Grant applications created | Y: National Centre for Responsible Gambling (US) large grant 2020
Grants project supporting | Name of Grant:
Software released for public use | Y/N: TBA
New Clients | Y
KPI Score | 7/6
