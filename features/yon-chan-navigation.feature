Feature: Moving between posts in yon-chan thread view

  Scenario: Attempting to move down then up few posts
    When I render "small-thread.json" as "/g/"
    When I go to beginning of buffer
    Then the cursor should be on line "1"
    And I press "n"
    Then the cursor should be on line "10"
    And I press "n"
    Then the cursor should be on line "13"
    And I press "p"
    Then the cursor should be on line "10"
    And I press "p"
    Then the cursor should be on line "1"

  Scenario: Attempting to move up from the first post
    When I render "small-thread.json" as "/g/"
    When I go to beginning of buffer
    Then the cursor should be on line "1"
    And I press "p"
    Then the cursor should be on line "1"
    And I press "n"
    Then the cursor should be on line "10"
    And I press "n"
    Then the cursor should be on line "13"
    And I press "n"
    Then the cursor should be on line "16"


  Scenario: Attempting to move past last post
    When I render "small-thread.json" as "/g/"
    # last post at 40
    When I go to line "40"
    And I press "n"
    Then the cursor should be on line "40"

  Scenario: Attempting to move up to last post, from the very end of the buffer
    When I render "small-thread.json" as "/g/"
    When I go to end of buffer
    # end at 1220
    Then the cursor should be on line "45"
    And I press "p"
    Then the cursor should be on line "40"

  Scenario: Jumping multiple posts down
    When I render "small-thread.json" as "/g/"
    When I go to beginning of buffer
    Then the cursor should be on line "1"
    And I press "C-u 3 n"
    Then the cursor should be on line "16"
    And I press "C-u 3 p"
    Then the cursor should be on line "1"

  Scenario: Jumping multiple posts up
    When I render "small-thread.json" as "/g/"
    When I go to line "40"
    And I press "C-u 3 p"
    Then the cursor should be on line "23"
    And I press "C-u 3 n"
    Then the cursor should be on line "40"

  Scenario: Trying to jump out of bounds up
    When I render "small-thread.json" as "/g/"
    When I go to point "40"
    And I press "C-u 100 p"
    Then the cursor should be on line "1"
    And I press "C-u 100 n"
    Then the cursor should be on line "40"

  Scenario: Trying to jump out of bounds down
    When I render "small-thread.json" as "/g/"
    When I go to beginning of buffer
    Then the cursor should be on line "1"
    And I press "C-u 100 n"
    Then the cursor should be on line "40"
    And I press "C-u 100 p"
    Then the cursor should be on line "1"
