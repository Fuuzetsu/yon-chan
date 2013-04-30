Feature: Greentext
  
  Scenario: Highlighting greentext single-line
    When I insert:
    """
    <span class="quote">>Single line greentext</span>
    """
    And I press "C-c C-r gr"
    Then I should see:
    """
    Single line greentext
    """

  Scenario: Highlighting greentext multi-line
    When I insert:
    """
    <span class="quote">>Multi line 
    green
    text</span>
    """
    And I press "C-c C-r gr"
    Then I should see:
    """
    Multi line
    green
    text
    """

  Scenario: Highlighting empty greentext
    When I insert:
    """
    <span class="quote">></span>
    """
    And I press "C-c C-r gr"
    Then I should see:
    """
    >
    """
