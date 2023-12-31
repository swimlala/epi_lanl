CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-14T23:00:41Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8D   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9`   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9x   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9|   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ct   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  sL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   `   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20181214230041  20210617131511  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               4   4DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؕ�Ɗ��@ؕ�Ɗ��11  @ؕ廻ΐ@ؕ廻ΐ@6��u�l@6��u�l�c�n���V�c�n���V11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  BA  >���?fff@   @Fff@�  @�ff@�33@�  A   A��A#33AA��Aa��A���A���A���A���A�33A�  A�  A�  B ffB  BffBffB ffB(��B/��B8  B@  BHffBP��BXffB`ffBhffBpffBx��B�  B���B�  B�ffB�33B�ffB�33B�33B�  B���B�  B�33B�  B���B�33B�ffB�ffB�ffB�  B̙�B�33B�33B�ffB�ffB���B���B���B�ffB�33B�  B�  B���C   C33C�C  CL�C
33C�CL�C33C  CL�C�C  C33C�C�fC   C"33C$  C%��C(  C*33C,�C-�fC0  C233C433C6�C7��C:  C<�C>33C@�CA�fCD�CF33CH�CI�fCL�CNL�CP�CQ�fCT33CV33CW�fCZ33C\�C]�fC`33CbffCdL�Cf�Ch  Ci�fCl�Cn�Cp  Cr33Ct  Cu�fCx33Cz�C{�fC~�C�  C��3C��C��C��3C��C��C��C��3C��C��C�&fC��C��fC��3C��3C�  C��C��C��C��C�&fC�&fC�&fC��C�&fC�&fC�&fC��C��3C��3C��C��C�&fC�33C��C��3C�  C��C��C�&fC��C��3C��3C��C�&fC�33C��C��3C��C��C�  C�ٚC��3C��3C��C��C��C��C�&fC��C��3C�  C�  C��C��C��C�&fC��C��C�&fC��C�&fC�&fC�&fC��C�&fC�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC�  C��fC��3C��3C��3C�  C�  C��C��C��C��C��C��C��C��C��C��C�&fC�  C�ٚC��C��C��3C��3C�  D@ D�3D��D
S3D�D� D�3DS3DfD��Dy�D   D"��D%Y�D'�3D*� D-�D/�fD2�D4��D6��D9l�D;ٚD>S3D@�3DCS3DE�3DHS3DJ�fDML�DO��DRFfDT� DW9�DY�3D\9�D^� Da@ Dc�fDfY�DifDk�3Dn&fDp�3Ds33Du��Dx33Dz�fD|�3D,�D�� D�3D�6fD�i�D���D��3D�� D�#3D�L�D�vfD�� D�� D��3D��D�@ D�` D��3D���D���D��fD�fD�6fD�I�D�ffD��3D���D�� D��3D��3D��fD���D� D�)�D�@ D�Y�D�vfD���D���D��fD��D�	�D�,�D�VfD�p D���D��3D��3D��3D�3D�33D�VfD�|�D�� D�� D�ٚD���D�fD�6fD�S3D�s3Dę�DŰ D��fD��fD��fD���D��D�#3D�,�D�6fD�@ D�FfD�L�D�P D�FfD�@ D�<�D�33D�)�D��D��D�3D��fD��3D���D��fD�� D��fD�ɚD��3D��3D��D�fD�3D橚D穚D�fD�fD�fD�fD� D���D�ɚD��3D���D���D�  D� D�#3D�33D�FfD�\�D�s3D�s3D���D��fD�� D���D��fE �3E3E� E+3EC3E�fE�E3E�fE	�fE+3E0 E8 E� E� E0 E0 E�3E��E@ EA�E�fE� EA�EA�E� E�fE 33E!��E"� E$fE%fE&��E'p E(њE*.fE+�3E,��E-��E.��E0H E1�fE2� E43E5A�E6y�E7�fE99�E:h E;��E<�3E?�3EB�EFD�EI8 EL�fEO��ER�3EU�3EX�E\+3E_  EbVfEeFfEh�fEk� En�fEq��Eu�ExfE{c3E{��E|��E}#3E}�3E~6fE~��E�fE��E�e�E��fE� �>���>���>���>���>���>L��>���>L��>���>���>���>���>���>���>���>L��>L��>���>���>���>���?   ?��?��?��?L��?fff?���?�33?���?�ff@ff@��@333@Fff@Y��@l��@�  @���@���@�ff@�  @���@ə�@ٙ�@�ff@�ffA   AffAffA��A33A$��A+33A1��A9��AA��AH  AP  AVffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441411444414414141144111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?L��?�33@   @fff@�  @�ff@�33@�  A  A��A+33AI��Ai��A���A���A���A���A�33A�  A�  A�  BffB
  BffBffB"ffB*��B1��B:  BB  BJffBR��BZffBbffBjffBrffBz��B�  B���B�  B�ffB�33B�ffB�33B�33B�  B���B�  B�33B�  B���B�33B�ffB�ffB�ffB�  B͙�B�33B�33B�ffB�ffBᙚB���B���B�ffB�33B�  B�  B���C � C�3C��C� C��C
�3C��C��C�3C� C��C��C� C�3C��CffC � C"�3C$� C&L�C(� C*�3C,��C.ffC0� C2�3C4�3C6��C8L�C:� C<��C>�3C@��CBffCD��CF�3CH��CJffCL��CN��CP��CRffCT�3CV�3CXffCZ�3C\��C^ffC`�3Cb�fCd��Cf��Ch� CjffCl��Cn��Cp� Cr�3Ct� CvffCx�3Cz��C|ffC~��C�@ C�33C�Y�C�L�C�33C�L�C�Y�C�L�C�33C�L�C�Y�C�ffC�L�C�&fC�33C�33C�@ C�L�C�L�C�Y�C�Y�C�ffC�ffC�ffC�Y�C�ffC�ffC�ffC�L�C�33C�33C�L�C�Y�C�ffC�s3C�Y�C�33C�@ C�L�C�Y�C�ffC�Y�C�33C�33C�L�C�ffC�s3C�L�C�33C�L�C�Y�C�@ C��C�33C�33C�L�C�L�C�Y�C�Y�C�ffC�L�C�33C�@ C�@ C�L�C�Y�C�Y�C�ffC�Y�C�Y�C�ffC�Y�C�ffC�ffC�ffC�Y�C�ffC�ffC�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�ffC�@ C�&fC�33C�33C�33C�@ C�@ C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�ffC�@ C��C�L�C�L�C�33C�33C�@ D` D�3D��D
s3D9�D  D�3Ds3D&fD��D��D @ D"ٚD%y�D(3D*� D-,�D/�fD2,�D4��D7�D9��D;��D>s3D@�3DCs3DE�3DHs3DJ�fDMl�DO��DRffDT� DWY�DY�3D\Y�D^� Da` Dc�fDfy�Di&fDk�3DnFfDp�3DsS3DuٚDxS3Dz�fD|�3DL�D�� D�3D�FfD�y�D���D��3D�  D�33D�\�D��fD�� D�� D�3D�)�D�P D�p D��3D���D���D�fD�&fD�FfD�Y�D�vfD��3D���D�� D��3D��3D��fD�	�D�  D�9�D�P D�i�D��fD���D���D��fD���D��D�<�D�ffD�� D���D��3D��3D�3D�#3D�C3D�ffD���D�� D�� D��D�	�D�&fD�FfD�c3DÃ3Dĩ�D�� D��fD��fD��fD��D��D�33D�<�D�FfD�P D�VfD�\�D�` D�VfD�P D�L�D�C3D�9�D�,�D��D�3D�fD�3D���D��fD�� D��fD�ٚD��3D��3D���D��fD��3D湚D繚D�fD�fD�fD�fD�� D�ɚD�ٚD��3D���D���D� D�  D�33D�C3D�VfD�l�D��3D��3D���D��fD�� D���D��fE �3E3E� E33EK3E�fE��E3E�fE	�fE33E8 E@ E� E� E8 E8 E�3E��EH EI�E�fE� EI�EI�E� E�fE ;3E!��E"� E$fE%fE&��E'x E(ٚE*6fE+�3E,��E-��E/�E0P E1�fE2� E43E5I�E6��E7�fE9A�E:p E;��E<�3E?�3EB�EFL�EI@ EL�fEO��ER�3EU�3EX��E\33E_( Eb^fEeNfEh�fEk� En�fEq��Eu!�ExfE{k3E|�E|��E}+3E}�3E~>fE~��E�fE��E�i�E��fE��G�O�G�O�G�O�G�O�G�O�?333G�O�?333?L��G�O�G�O�G�O�G�O�?L��G�O�G�O�?333G�O�?L��G�O�?L��?�  G�O�G�O�?���?�ff?�33?���?�33@ff@33@&ff@9��@S33@fff@y��@�ff@�  @���@���@�ff@�  @���@ٙ�@陚@�ffA33A  AffAffA��A#33A,��A333A9��AA��AI��AP  AX  A^ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441411444414414141144111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ @ %@ �@ *@ O@ #�@ )�@ /�@ 6�@ >@ E�@ R�@ `B@ m�@ {�@ �7@ ��@ �(@ �~@ �&@ ��@ �#@ �@ �q@@�@ @+�@:@G�@V@dZ@qS@~�@��@�H@��@��@�2@�7@�;@�4@��@�@*@""@/@=q@K�@X�@e�@t�@�@��@�a@��@�^@ƨ@�O@�@�L@��@	�@6@&�@3�@@�@N�@Z�@i�@x�@��@��@�z@�r@�k@�o@�h@�@�e@ �@�@�@)�@5�@DD@SI@_�@k�@z�@��@��@�(@�~@��@�*@�#@�@��@@o@g@+�@:�@I@V@bN@qS@�W@��@��@��@�F@��@є@ލ@��@��@	�@�@"�@/�@<�@K�@Yn@ff@uk@��@��@�a@�Y@��@ƨ@Ӡ@��@�L@�E@	�@�@&�@3�@@,@O0@]�@k�@x&@��@�@��@�@�k@�@�h@�@�e@@�@�@+@8�@FQ@R�@^�@l�@{�@��@�<@��@��@�w@��@�#@�y@��@	�@	b@	
@	-@	<@	Ji@	V@	bN@	qS@	�@	��@	��@	��@	�9@	�>@	��@	�;@	��@	�9@
�@
�@
""@
/�@
>@
Lu@
Z@
hs@
uk@
�@
�h@
�a@
��@
�^@
�@
��@
�T@
��@
��@�@B@&�@4�@B8@O�@]�@j@x�@�|@�$@��@�r@�@��@�h@�@�@@�@�@(G@5�@C�@Q�@_�@m�@{�@��@��@�5@��@��@�*@��@�y@��@j@@g@-@9X@F�@UU@�@+@uk@�2@�@X�@�y@�@7L@�@�@{@[z@�(@�(@/�@uk@�@��@B8@�p@�W@	�@M$@�h@խ@�@^5@�@�@*S@m�@�~@�@8�@}�@@�@K�@�@�#@ �@g@��@��@5�@y�@�j@�e@7�@z�@�j@��@?}@�W@��@  @A�@�@��@  @@�@~�@��@��@:@x&@��@�@ 4�@ r@ �r@ �(@!&�@!c�@!��@!�t@"*@"O0@"��@"Ĝ@#  @#<@#ww@#��@#�L@$+@$hs@$�4@$�T@% �@%^�@%�a@%�t@&6@&V@&�u@&��@'V@'K�@'��@'ȴ@(�@(DD@(�W@(��@(��@)7�@)t�@)�-@)��@*,`@*g�@*��@*��@+6@+Q=@+��@+�J@+��@,6�@,n�@,��@,��@-o@-G�@-}�@-�-@-�@.�@.M�@.�d@.�F@.�4@/!s@/V�@/��@/��@/�e@0)�@0`A@0��@0��@1 �@15@@1k�@1��@1�h@2@2E�@2~K@2��@2��@3)�@3bN@3�U@3�
@4@4K�@4��@4��@4��@57L@5m�@5�M@5�@6""@6[z@6��@6��@7o@7N�@7��@8]@8��@9&;@9��@:F�@:��@;`�@;�7@<@�@<�@=N�@=�Y@>_�@?�@?t�@@O@@�7@A/@A�@B> @B�Z@CN�@C�@DZ@D��@Eb�@F�@Fl�@G
�@Gp�@H�@H�T@I-�@I��@J[@J�M@K6�@K��@LK@Lє@MV@M�#@N[z@O�@O��@P
�@P��@Q�;@S)�@T�<@U�t@WF�@X�@Y��@[&�@\�@]�L@_33@`�@a��@c5�@d��@e��@g8�@h�u@i��@kB8@k�d@kĜ@l]@l>�@lv�@l�@m6@mLv@m��@m�C@n*G�O�G�O�G�O�G�O�G�O�@ ^G�O�@ ^@ G�O�G�O�G�O�G�O�@ G�O�G�O�@ ^G�O�@ G�O�@ @ jG�O�G�O�@ @ v@ %@ �@ 	�@ 
�@ J@ V@ b@ @ *@ 6@ B@ O@ 
@  �@ #�@ %�@ (G@ +@ .l@ 1'@ 4�@ 6�@ 9X@ <�@ ?}@ B8@ FQ@ I@ K�@ O0@ R�@ UU@ X�@ [zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A��A���A�A�A�A�A�VA��A�&�A�-A�-A�/A�-A�-A�/A�/A�+A�-A�-A�/A�33A�33A�33A�33A�5?A�7LA�7LA�5?A�7LA�7LA�9XA�;dA�;dA�9XA�33A�33A�33A�7LA�7LA�7LA�1'A�&�A�1A���A�p�A���A�XA�t�A�\)A���A�p�A�bA�/A��A�1'A���A��#A���A�+A���A�O�A��hA�%A���A�`BA�JA���A��wA��hA��A�A�A��yA��A�l�A��A���A�9XA��A��yA�{A���A�^5A�p�A���A��DA�p�A���A��HA�O�A�A��^A��\A�G�A��
A��A�+A� �A�dZA�1'A�r�A��A�&�A�XA�ffA�ƨA���A��A��A�  A�oA�M�A��\A�G�A��A��FA��+A�;dA��A��7A�33A�ƨA���A�1'A�l�A~��Ax��Au��As�
Aq;dAq/Ap��Ak&�Ai�Af�Aa�A]dZAZȴAXQ�AW?}AVAU�^ATĜAR��AQ%AN�AM��AL��AJ�AGG�AE��AD �ACVAA�wA@��A?`BA>�HA>�uA>r�A>{A<�DA;ƨA:�9A9�7A8��A7
=A5G�A4�`A4��A4v�A4VA49XA4 �A4JA3�A3��A0�A.^5A-��A,�9A+7LA)��A(z�A'��A'7LA&��A%�#A%�PA$��A"��A!G�A��A�/A5?AVA^5Al�A�A��A9XAx�AjAt�A�9A�;A��A�-A+A��A�AA�AE�A�9AAoA	�AjA?}A �A�RA�AƨA?}A�A(�A ��A $�@�dZ@��@���@�A�@�  @��P@�33@�~�@��@�V@�Q�@�S�@�-@�^@�p�@���@�@��@��#@�1@��T@�Z@�t�@�^5@�(�@��H@�E�@�X@���@��
@��T@Ł@��7@�\)@�%@���@���@��@��7@�M�@���@�Z@��F@��#@���@��!@���@��P@���@�ƨ@� �@��@�V@�"�@�V@��9@��R@���@�Ĝ@���@� �@�K�@��R@�x�@�Ĝ@�  @���@��`@���@�ȴ@�O�@�l�@��H@���@��@��D@�@{��@z~�@y��@w��@v{@r��@q��@o��@n$�@l�@k"�@i�^@iX@hb@f�R@eV@c��@b��@ahs@`  @^{@\�@[ƨ@[��@YG�@X�9@V��@V5?@T�@R��@P�`@P��@PĜ@N�y@M`B@K�F@J~�@H��@G\)@F�y@Ep�@D�/@D(�@C��@Co@A�@?��@>��@=O�@;�@;@:�!@9�^@8��@7+@6{@4�D@333@2��@1�7@0��@0��@0r�@/�@.��@.5?@-�@+t�@*�\@)��@)G�@(�@'��@'��@&��@$�@$1@"n�@"�@ ��@ 1'@ 1'@|�@ff@V@��@Z@dZ@dZ@�H@-@�`@��@�@V@O�@��@C�@J@x�@b@\)@�@�@V@9X@�m@�@@
�!@	��@	�@��@bN@K�@
=@ff@��@O�@�/@9X@dZ@��@�@&�?�\)?��?�dZ?�1'?�K�?�9X?�t�?�hs?��?�w?�D?�?�^?�r�?�+?��T?䛦?�!?�bN?��?��?�(�?ۅ?��?ٺ^?��?�ȴ?ԛ�?�Z?ѩ�?�&�?� �?�\)?��?�V?̋D?�ƨ?�~�?�=q?�X?ȴ9?�Q�?���?�l�?��y?��y?ě�?��?�M�?�&�?�A�?��;?��R?�V?��?�1?�ƨ?�"�?�x�?���?�r�?�Q�?��9?���?�X?��#?��#?���?��?�=q?��?�^5?�~�?�~�?�^5?���?���A�A���A�%A�%A�A�A�%A�%A�%A�%A�%A�%A�A�A�  A���A��A���A���A���A�  A��A��A��mA���A���A���A���A���A���A���A���A���A���A�%A�1A�%A�A�A�A�A�A�A�A�1A�{A��A��A��A�"�A�(�A�-A�-A�-A�-A�/A�-A�-A�-A�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A�  A��A���A�A�A�A�A�VA��A�&�A�-A�-A�/A�-A�-A�/A�/A�+A�-A�-A�/A�33A�33A�33A�33A�5?A�7LA�7LA�5?A�7LA�7LA�9XA�;dA�;dA�9XA�33A�33A�33A�7LA�7LA�7LA�1'A�&�A�1A���A�p�A���A�XA�t�A�\)A���A�p�A�bA�/A��A�1'A���A��#A���A�+A���A�O�A��hA�%A���A�`BA�JA���A��wA��hA��A�A�A��yA��A�l�A��A���A�9XA��A��yA�{A���A�^5A�p�A���A��DA�p�A���A��HA�O�A�A��^A��\A�G�A��
A��A�+A� �A�dZA�1'A�r�A��A�&�A�XA�ffA�ƨA���A��A��A�  A�oA�M�A��\A�G�A��A��FA��+A�;dA��A��7A�33A�ƨA���A�1'A�l�A~��Ax��Au��As�
Aq;dAq/Ap��Ak&�Ai�Af�Aa�A]dZAZȴAXQ�AW?}AVAU�^ATĜAR��AQ%AN�AM��AL��AJ�AGG�AE��AD �ACVAA�wA@��A?`BA>�HA>�uA>r�A>{A<�DA;ƨA:�9A9�7A8��A7
=A5G�A4�`A4��A4v�A4VA49XA4 �A4JA3�A3��A0�A.^5A-��A,�9A+7LA)��A(z�A'��A'7LA&��A%�#A%�PA$��A"��A!G�A��A�/A5?AVA^5Al�A�A��A9XAx�AjAt�A�9A�;A��A�-A+A��A�AA�AE�A�9AAoA	�AjA?}A �A�RA�AƨA?}A�A(�A ��A $�@�dZ@��@���@�A�@�  @��P@�33@�~�@��@�V@�Q�@�S�@�-@�^@�p�@���@�@��@��#@�1@��T@�Z@�t�@�^5@�(�@��H@�E�@�X@���@��
@��T@Ł@��7@�\)@�%@���@���@��@��7@�M�@���@�Z@��F@��#@���@��!@���@��P@���@�ƨ@� �@��@�V@�"�@�V@��9@��R@���@�Ĝ@���@� �@�K�@��R@�x�@�Ĝ@�  @���@��`@���@�ȴ@�O�@�l�@��H@���@��@��D@�@{��@z~�@y��@w��@v{@r��@q��@o��@n$�@l�@k"�@i�^@iX@hb@f�R@eV@c��@b��@ahs@`  @^{@\�@[ƨ@[��@YG�@X�9@V��@V5?@T�@R��@P�`@P��@PĜ@N�y@M`B@K�F@J~�@H��@G\)@F�y@Ep�@D�/@D(�@C��@Co@A�@?��@>��@=O�@;�@;@:�!@9�^@8��@7+@6{@4�D@333@2��@1�7@0��@0��@0r�@/�@.��@.5?@-�@+t�@*�\@)��@)G�@(�@'��@'��@&��@$�@$1@"n�@"�@ ��@ 1'@ 1'@|�@ff@V@��@Z@dZ@dZ@�H@-@�`@��@�@V@O�@��@C�@J@x�@b@\)@�@�@V@9X@�m@�@@
�!@	��@	�@��@bN@K�@
=@ff@��@O�@�/@9X@dZ@��@�@&�?�\)?��?�dZ?�1'?�K�?�9X?�t�?�hs?��?�w?�D?�?�^?�r�?�+?��T?䛦?�!?�bN?��?��?�(�?ۅ?��?ٺ^?��?�ȴ?ԛ�?�Z?ѩ�?�&�?� �?�\)?��?�V?̋D?�ƨ?�~�?�=q?�X?ȴ9?�Q�?���?�l�?��y?��y?ě�?��?�M�?�&�?�A�?��;?��R?�V?��?�1?�ƨ?�"�?�x�?���?�r�?�Q�?��9?���?�X?��#?��#?���?��?�=q?��?�^5?�~�?�~�?�^5?���?���A�A���A�%A�%A�A�A�%A�%A�%A�%A�%A�%A�A�A�  A���A��A���A���A���A�  A��A��A��mA���A���A���A���A���A���A���A���A���A���A�%A�1A�%A�A�A�A�A�A�A�A�1A�{A��A��A��A�"�A�(�A�-A�-A�-A�-A�/A�-A�-A�-A�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B}�B~�B~�B~�B}�B~�B~�B~�B|�B}�B|�B|�B|�B|�B|�B|�B|�B|�B|�B}�B|�B|�B}�B|�B}�B|�B|�B|�B|�B|�B|�B|�B}�B|�B|�B|�B}�B|�B|�B|�B|�B|�B|�B|�By�BdZBcTBz�Bt�Br�Bv�Bu�Br�Bq�Bv�Bx�Bu�B{�By�Bz�By�By�Bz�B|�B{�B{�Bz�Bw�Bz�B|�B�B�%B�B|�Bz�Bx�BiyBcTBS�BN�BD�B;dB7LB%�B�B�B�BVB��B��B�B�B�yB�`B�#B��BÖB�-B�uBx�Bm�BbNBS�B9XB)�B�BJB
��B
�fB
��B
ɺB
�wB
�3B
�B
�B
��B
��B
��B
��B
��B
�JB
�DB
�+B
}�B
t�B
;dB
�B
B	�B	�#B	�/B	��B	�?B	�B	�uB	o�B	O�B	B�B	/B	#�B	�B	�B	hB	%B��B�B�B�sB�B��BŢB��B�dB�LB�3B�?BÖB��B��B��BɺBƨBÖBŢBB�RB�?B�3B�'B�!B�B�B�B�B�B�B��B��B��B�oB�VB�JB�DB�DB�7B�+B�B�B� B� B}�Bx�Bt�Bo�Bl�BjBgmBgmBe`BcTB`BB]/B\)BZBYBW
BVBR�BR�BP�BI�BG�BC�BB�B@�B>wB=qB9XB<jB:^B:^B9XB8RB6FB8RB6FB6FB6FB5?B49B49B6FB5?B49B33B49B33B49B49B33B5?B49B49B49B49B49B5?B7LB8RB9XB8RB9XB9XB;dB9XB9XB?}BD�BH�BVBcTBv�B�B�=B��B��B�-BƨB��B��B�
B�mB	1B	�B	33B	E�B	YB	bNB	w�B	�B	�JB	�VB	�{B	��B	�-B	�^B	�wB	ÖB	ĜB	��B	��B	�B	�B	�/B	�HB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
JB
bB
bB
\B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
%�B
%�B
%�B
'�B
'�B
+B
+B
-B
.B
1'B
0!B
1'B
2-B
49B
5?B
5?B
7LB
9XB
9XB
;dB
;dB
<jB
<jB
<jB
>wB
A�B
B�B
C�B
E�B
E�B
E�B
F�B
F�B
H�B
H�B
J�B
L�B
L�B
L�B
N�B
N�B
N�B
O�B
O�B
P�B
R�B
R�B
T�B
T�B
VB
W
B
XB
XB
XB
[#B
[#B
]/B
\)B
]/B
_;B
^5B
_;B
`BB
bNB
bNB
cTB
dZB
cTB
dZB
e`B
e`B
gmB
hsB
iyB
jB
k�B
k�B
m�B
o�B
p�B
q�B
q�B
r�B
s�B
s�B
t�B
t�B
u�B
t�B
v�B
u�B
v�B
v�B
x�B
x�B
y�B
y�B
{�B
{�B
|�B
}�B
}�B
}�B
�B
�B
�B
�B
�+B
�%B
�7B
�7B
�=B
�DB
�DB
�PB
�VB
�VB
�bB
�bB
�bB
�hB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�VB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�!B
�'B
�3B
�3B
�9B
�9B
�?B
�LB
�LB
�RB
�RB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�^B
�XB
�^B|�B�B}�B}�B~�B~�B}�B}�B}�B}�B}�B|�B~�B}�B~�B}�B~�B~�B� B~�B}�B|�B~�B�B~�B}�B}�B~�B~�B~�B}�B~�B}�B~�B� B}�B}�B}�B}�B~�B~�B}�B}�B~�B~�B}�B|�B}�B|�B}�B|�B|�B|�B|�B|�B|�B}�B|�B|�B|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B}�B~�B~�B~�B}�B~�B~�B~�B|�B}�B|�B|�B|�B|�B|�B|�B|�B|�B|�B}�B|�B|�B}�B|�B}�B|�B|�B|�B|�B|�B|�B|�B}�B|�B|�B|�B}�B|�B|�B|�B|�B|�B|�B|�By�BdIBcBBz�Bt�Br�Bv�Bu�Br�Bq�Bv�Bx�Bu�B{�By�Bz�By�By�Bz�B|�B{�B{�Bz�Bw�Bz�B|�B�B� B�B|�Bz�Bx�BiwBcRBS�BN�BD�B;dB7MB%�B�B�B�BYB��B��B�B�B�B�gB�*B��BÞB�5B�~Bx�Bm�BbXBTB9cB*B�BVB
��B
�sB
�B
��B
��B
�BB
�*B
�B
�B
��B
��B
��B
��B
�]B
�XB
�?B
~	B
t�B
;yB
�B
5B	�B	�:B	�FB	�B	�WB	� B	��B	o�B	O�B	B�B	/4B	#�B	�B	�B	�B	AB�B��B�B�B�.B�B��B��B��B�lB�TB�`BøB��B��B��B��B��BûB��BµB�xB�fB�ZB�OB�IB�DB�>B�>B�9B�9B�.B��B��B��B��B��B�xB�sB�sB�gB�[B�JB�JB�2B�2B~'ByBt�Bo�Bl�Bj�Bg�Bg�Be�Bc�B`zB]gB\bBZVBYQBWDBV?BS-BS.BQ!BI�BG�BC�BB�B@�B>�B=�B9�B<�B:�B:�B9�B8�B6�B8�B6�B6�B6�B5�B4B4B6�B5�B4�B3|B4�B3}B4�B4�B3~B5�B4�B4�B4�B4�B4�B5�B7�B8�B9�B8�B9�B9�B;�B9�B9�B?�BD�BIBVfBc�Bw1B��B��B��B�B��B�#B�QB�fB׎B��B	�B	9B	3�B	F5B	Y�B	b�B	xkB	��B	��B	��B	�"B	��B	��B	�B	�*B	�LB	�UB	͉B	ОB	��B	��B	��B	�B	�AB	�\B	�_B	�{B	��B	��B	��B	��B	��B	��B
 �B
�B

B
B
AB
\B
^B
[B
wB
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
$�B
'B
'B
'B
)B
) B
,5B
,7B
.FB
/NB
2dB
1`B
2iB
3rB
5�B
6�B
6�B
8�B
:�B
:�B
<�B
<�B
=�B
=�B
=�B
?�B
B�B
C�B
EB
GB
GB
GB
H B
H"B
J1B
J4B
LCB
NRB
NUB
NWB
PfB
PiB
PkB
QtB
QwB
RB
T�B
T�B
V�B
V�B
W�B
X�B
Y�B
Y�B
Y�B
\�B
\�B
^�B
]�B
^�B
`�B
_�B
`�B
bB
dB
dB
e!B
f)B
e%B
f.B
g6B
g8B
iHB
jPB
kXB
laB
miB
mkB
ozB
q�B
r�B
s�B
s�B
t�B
u�B
u�B
v�B
v�B
w�B
v�B
x�B
w�B
x�B
x�B
z�B
z�B
{�B
{�B
~B
~B
B
�B
�B
�B
�5B
�<B
�NB
�ZB
�sB
�rB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�)B
�6B
�AB
�HB
�UB
�aB
�nB
�sB
��B
��B
��B
��G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�B
�%B
�+B
�6B
�6B
�QB
�fB
��B
��B
��B
��B
��B
��B
�B
�B
�'B
�=B
�XB
�hB
�~B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B|�B��B}�B}�B~�B~�B}�B}�B}�B}�B}�B|�B~�B}�B~�B}�B~�B~�B�B~�B}�B|�B~�B��B~�B}�B}�B~�B~�B~�B}�B~�B}�B~�B�B}�B}�B}�B}�B~�B~�B}�B}�B~�B~�B}�B|�B}�B|�B}�B|�B|�B|�B|�B|�B|�B}�B|�B|�B|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201812142300412021061413555820210614135558202106171313472021061713134720210617131347201812142300412021061413555820210614135558202106171313472021061713134720210617131347PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018121423004120181214230041  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018121423004120181214230041QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018121423004120181214230041QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151120210617131511IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                