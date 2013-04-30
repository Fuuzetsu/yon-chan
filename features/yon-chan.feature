Feature: Greentext
  
  Scenario: Highlighting greentext single-line
    When I insert:
    """
    <span class="quote">>Single line greentext</span>
    """
    When I go to the beginning of the buffer
    And I press "C-c C-r gr"
    Then I should see exactly:
    """
    >Single line greentext
    """

  Scenario: Highlighting greentext multi-line
    When I insert:
    """
    <span class="quote">>Multi line 
    green
    text</span>
    """
    When I go to the beginning of the buffer
    And I press "C-c C-r gr"
    Then I should see exactly:
    """
    >Multi line
    green
    text
    """

  Scenario: Highlighting empty greentext
    When I insert:
    """
    <span class="quote">></span>
    """
    When I go to the beginning of the buffer
    And I press "C-c C-r gr"
    Then I should see exactly:
    """
    >
    """
