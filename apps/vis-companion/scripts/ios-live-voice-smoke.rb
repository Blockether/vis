# Prepare simulator tests after CocoaPods and ios-prepare.mjs.
# Run xcodebuild test with the LiveVoiceSmokeTests scheme and an owned simulator ID.
# Deny microphone access to com.blockether.viscompanion before the test run.
# Use an iOS 17 or newer simulator. Do not use a physical device.
require 'xcodeproj'

root = File.expand_path('..', __dir__)
project = Xcodeproj::Project.open(File.join(root, 'ios/App/App.xcodeproj'))
app = project.targets.find { |target| target.name == 'App' }
abort 'Prepare the iOS app first.' unless app

name = 'LiveVoiceSmokeTests'
target = project.targets.find { |item| item.name == name }
target ||= project.new_target(:unit_test_bundle, name, :ios, '17.0')
target.add_dependency(app) unless target.dependencies.any? { |dependency| dependency.target == app }
group = project.main_group.find_subpath(name, true)
source = '../../native/ios-test/LiveVoiceSmokeTests.swift'
reference = group.files.find { |file| file.path == source } || group.new_file(source)
unless target.source_build_phase.files_references.include?(reference)
  target.add_file_references([reference])
end

target.build_configurations.each do |config|
  app_config = app.build_configurations.find { |item| item.name == config.name }
  config.base_configuration_reference = app_config.base_configuration_reference
  config.build_settings.merge!({
    'PRODUCT_NAME' => '$(TARGET_NAME)',
    'PRODUCT_BUNDLE_IDENTIFIER' => 'com.blockether.viscompanion.LiveVoiceSmokeTests',
    'GENERATE_INFOPLIST_FILE' => 'YES',
    'SWIFT_VERSION' => '5.0',
    'SUPPORTED_PLATFORMS' => 'iphonesimulator',
    'IPHONEOS_DEPLOYMENT_TARGET' => '17.0',
    'CODE_SIGNING_ALLOWED' => 'NO',
    'TEST_HOST' => '$(BUILT_PRODUCTS_DIR)/App.app/App',
    'BUNDLE_LOADER' => '$(TEST_HOST)',
    'LD_RUNPATH_SEARCH_PATHS' => ['$(inherited)', '@executable_path/Frameworks', '@loader_path/Frameworks'],
  })
end
project.save

# Keep the normal build scheme when Xcode disables automatic schemes.
app_scheme_path = File.join(project.path, 'xcshareddata/xcschemes/App.xcscheme')
unless File.exist?(app_scheme_path)
  app_scheme = Xcodeproj::XCScheme.new
  app_scheme.add_build_target(app)
  app_scheme.set_launch_target(app)
  app_scheme.save_as(project.path, 'App', true)
end

scheme = Xcodeproj::XCScheme.new
scheme.add_build_target(app)
scheme.add_test_target(target)
scheme.set_launch_target(app)
scheme.test_action.build_configuration = 'Debug'
scheme.save_as(project.path, name, true)
puts 'Prepared LiveVoiceSmokeTests. Use an owned simulator with microphone access denied.'
