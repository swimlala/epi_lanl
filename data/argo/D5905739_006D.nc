CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:43Z creation      
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
resolution        =���   axis      Z        @  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  P$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `d   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  dt   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     @  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   |   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20180724220243  20210617131452  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�c�^ot@�c�^ot11  @�c�8�0@�c�8�0@6�����@6������c�"h	ԕ�c�"h	ԕ11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@   @9��@y��@���@�  @�  A��A  A!��A<��A`  A���A�  A���A�33A�  Aљ�A���A���B ffB  BffB33B ffB'��B0  B8ffB@��BI33BPffBW��B_��Bg��Bp  Bx  B�  B�33B�ffB�33B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�33B�  B���B���B�  B�33B�33B�33B�  B�  B���B�ffB���B뙚B���B�33B���B�  B���C  C33CL�C  C	�3C��C�CL�CL�CL�CL�C33CL�C33C�C   C!�fC#��C%��C(�C*�C,�C.L�C0L�C2�C4�C633C7��C9�fC<  C>33C@33CB�CD  CFL�CH�CJ  CLL�CN�CO�fCR33CT�CU�fCX�CZ�C[��C^�C_�fCa�3Cc�fCf�Ch33Cj  Ck�3Cm�fCo�fCr�CtL�Cv33Cw�fCz  C|  C~�C��C��C�  C��fC��fC��3C�  C��C��C��C��C�&fC�&fC�&fC�  C��fC��fC��fC��fC��fC��3C��3C�  C��C��C��C�&fC�&fC�&fC��C��fC��3C�  C��C��C��C�&fC�&fC��C��fC��3C��3C��C��C��C�&fC��C��fC��3C��C��C��C��C��C�&fC��C��fC��fC��fC��3C��3C��3C��3C�  C��C��C��C��C��C�  C��3C��3C��3C��fC��fC��fC��3C��3C�  C��C��C�&fC�  C��fC��3C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��3C��fC�ٚC�  C��C��3C��C��C�  C��C�&fC��C�  C��3C�  C��C�  C��3C�  C��3C�  C�33C��C��3C�&fD fD��D� D	@ D� D@ D��Ds3D�D�fDy�D33D ��D#�fD&` D)  D+� D.33D0��D39�D5�fD8  D:` D<� D?�DAs3DC�3DF&fDHs3DJ�3DM,�DOy�DQ�fDT  DVy�DX�fD[@ D]��D`,�Db�3De�Dg��Dj  Dl` Dn�3Dp��Ds9�DuffDw�3Dy��D{� D}��D�3D��D�� D���D�fD� D�fD�  D�,�D�6fD�C3D�S3D�ffD��3D���D���D�� D�3D�,�D�P D�l�D���D�� D��fD�  D�&fD�L�D�y�D�� D���D��fD�  D�S3D�vfD�� D�� D���D�#3D�I�D�s3D���D��3D�� D��D�3D��D�0 D�I�D�ffD�� D�� D���D���D�� D�ٚD���D���D�	�D��D�&fD�0 D�<�D�L�D�\�D�ffD�vfDȃ3DɖfDʠ Dˬ�D̶fDͼ�D�ɚD���D��3D�� D�� D�� D�ɚD��3D��3D׶fDذ D٣3Dڜ�Dۓ3D܉�D�y�D�l�D�\�D�S3D�C3D�33D�&fD�3D�  D���D��fD��fD��fD�fD��D�fD�fD�p D�i�D�\�D�S3D�I�D�<�D�0 D�&fD��D�fD��D�  D��fD�� D��fD���D���D���D��3E ��E�fE,�E#3E�3E��E��E	Y�E
�fE��E#3E��E��E�fE��EA�E��E��E��E)�Ex E�3E�E9�EnfE�fE!+3E"S3E#vfE$�3E&  E'C3E(\�E)� E*�fE,a�E-s3E.�3E/��E1ffE2x E3�fE4�fE6\�E7p E8�E9� E;c3E<y�E?�fEB�3EE�fEH�3EK��EO$�ER9�EU�3EX��E[��E^�3Ea�fEe	�Eh+3Ek)�En\�Eo3Eo�3Ep6fEp� Eq��Er�Er� EsL�Et3Et�fEuC3Eu�fEvx Ew1�Ew��Exa�Ey�Ey�3Ez.f>L��>���>���>���>L��>���>���>���>���>L��>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���?��?��?��?L��?�  ?���?�33?���?�ff@   @��@,��@9��@L��@fff@y��@�ff@�33@���@�ff@�ff@�  @���@�ff@�  @���@���A��A33A33A��A!��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144411444144441444441414444144111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ?L��?�33@   @Y��@���@���@�  @�  A	��A  A)��AD��Ah  A���A�  A���A�33A�  Aՙ�A���A���BffB
  BffB33B"ffB)��B2  B:ffBB��BK33BRffBY��Ba��Bi��Br  Bz  B�  B�33B�ffB�33B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�33B�  B���B���B�  B�33B�33B�33B�  B�  B���B�ffB���B왚B���B�33B���B�  C ffC� C�3C��C� C
33CL�C��C��C��C��C��C�3C��C�3C��C � C"ffC$L�C&L�C(��C*��C,��C.��C0��C2��C4��C6�3C8L�C:ffC<� C>�3C@�3CB��CD� CF��CH��CJ� CL��CN��CPffCR�3CT��CVffCX��CZ��C\L�C^��C`ffCb33CdffCf��Ch�3Cj� Cl33CnffCpffCr��Ct��Cv�3CxffCz� C|� C~��C�L�C�Y�C�@ C�&fC�&fC�33C�@ C�L�C�L�C�L�C�Y�C�ffC�ffC�ffC�@ C�&fC�&fC�&fC�&fC�&fC�33C�33C�@ C�L�C�Y�C�Y�C�ffC�ffC�ffC�L�C�&fC�33C�@ C�L�C�L�C�Y�C�ffC�ffC�L�C�&fC�33C�33C�L�C�L�C�Y�C�ffC�L�C�&fC�33C�L�C�L�C�Y�C�L�C�Y�C�ffC�L�C�&fC�&fC�&fC�33C�33C�33C�33C�@ C�L�C�Y�C�Y�C�Y�C�L�C�@ C�33C�33C�33C�&fC�&fC�&fC�33C�33C�@ C�L�C�L�C�ffC�@ C�&fC�33C�@ C�L�C�Y�C�Y�C�L�C�Y�C�Y�C�L�C�Y�C�Y�C�L�C�L�C�Y�C�Y�C�L�C�L�C�L�C�@ C�33C�&fC��C�@ C�L�C�33C�L�C�Y�C�@ C�L�C�ffC�L�C�@ C�33C�@ C�Y�C�@ C�33C�@ C�33C�@ C�s3C�Y�C�33C�ffD &fD��D  D	` D� D` D��D�3D9�D�fD��DS3D!�D#�fD&� D)  D+� D.S3D0ٚD3Y�D5�fD8  D:� D<� D?9�DA�3DC�3DFFfDH�3DJ�3DML�DO��DQ�fDT@ DV��DYfD[` D]��D`L�Db�3De9�Dg��Dj  Dl� Dn�3Dq�DsY�Du�fDw�3DyٚD{� D}��D�3D���D�  D�	�D�fD�  D�&fD�0 D�<�D�FfD�S3D�c3D�vfD��3D���D�ɚD�� D�3D�<�D�` D�|�D���D�� D��fD� D�6fD�\�D���D�� D���D�fD�0 D�c3D��fD�� D�� D��D�33D�Y�D��3D���D��3D�� D���D�3D�,�D�@ D�Y�D�vfD�� D�� D���D���D�� D��D���D�	�D��D�)�D�6fD�@ D�L�D�\�D�l�D�vfDǆfDȓ3DɦfDʰ D˼�D��fD���D�ٚD���D��3D�� D�� D�� D�ٚD��3D��3D��fD�� Dٳ3Dڬ�Dۣ3Dܙ�D݉�D�|�D�l�D�c3D�S3D�C3D�6fD�#3D� D���D��fD��fD��fD��fD��D�fD�fD� D�y�D�l�D�c3D�Y�D�L�D�@ D�6fD�,�D�&fD��D� D�fD�  D��fD���D���D���D��3E ɚE�fE4�E+3E�3E�E��E	a�E
�fE��E+3E��E��E�fE��EI�E��E�E��E1�E� E�3E�EA�EvfE�fE!33E"[3E#~fE$�3E&( E'K3E(d�E)� E*�fE,i�E-{3E.�3E0�E1nfE2� E3�fE4�fE6d�E7x E8�E:  E;k3E<��E?�fEB�3EE�fEH�3EL�EO,�ERA�EU�3EX��E[��E^�3Ea�fEe�Eh33Ek1�End�Eo3Eo�3Ep>fEp� Eq��Er!�Er� EsT�Et3Et�fEuK3EvfEv� Ew9�Ew��Exi�Ey�Ey�3Ez6f?333G�O�G�O�G�O�?333?L��G�O�G�O�G�O�?333G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�G�O�G�O�?L��G�O�?L��G�O�G�O�G�O�G�O�?fffG�O�G�O�?���?�ff?�  ?ٙ�?�33@ff@33@   @9��@L��@Y��@l��@�33@���@�ff@�33@���@�ff@�ff@�  @���@�ff@�  @���AffA��A33A33A!��A)��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144411444144441444441414444144111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            @ @ %@ �@ �@ �@ !s@ (�@ /�@ 7L@ =q@ D�@ P�@ _�@ m�@ z�@ �7@ ��@ ��@ ��@ ��@ �|@ �#@ �@ �q@v@�@
@,`@:�@I@Wb@c�@o�@}�@�D@��@�A@��@�>@є@ލ@�@�,@�@{@""@/�@=q@K@X@ff@t�@��@��@�U@��@�@ƨ@�O@�H@��@��@�@6@$.@2�@A�@P�@\)@i!@ww@�|@��@�m@�@�^@�@�@�@�e@@@[@*S@7L@DD@Q=@^5@k�@{�@�7@��@��@��@��@�|@��@�@�@j@o@ @-@:@I�@V@b�@r�@~�@�D@��@��@�9@�>@��@܀@�4@�~@�@�@"�@1'@=q@I@X@e�@t�@��@��@�U@��@�R@ƨ@�O@�@��@�9@�@6@%�@3�@A�@O0@]�@k�@y�@�+@��@�@��@�^@�@խ@�@�@  @V@�@*S@8�@FQ@S�@`B@k�@z3@��@��@��@��@�2@��@�#@�@�@	�@	�@	g@	-�@	<@	H]@	S�@	bN@	qS@	~�@	�P@	�H@	��@	��@	�>@	��@	܀@	�(@	�~@
%@
�@
!s@
/�@
>@
Lu@
Z@
g�@
t�@
��@
��@
�U@
��@
��@
Ĝ@
�C@
��@
�@@
��@
�@�@'�@33@?}@M�@\)@j@x�@�|@�u@��@�r@�k@��@�h@�`@�@^@@�@)�@7L@DD@Q=@^5@k.@z�@�7@��@��@��@�&@�|@܀@��@��@�@@ @,`@9X@G�@T�@b�@s_@�@�D@��@��@$�@b�@��@�@,`@r@�^@�@K�@��@��@*S@t�@�&@�@N�@��@��@
@`A@�m@�H@""@bN@�y@�T@"�@a�@�y@�@!s@`A@�m@��@"�@b�@�5@�y@+@o�@��@��@6�@v@�9@�@-@hs@�(@Ӡ@�@FQ@�W@�R@��@*S@b�@��@Ӡ@�@E�@~�@�@�@0x@l�@�M@�@&;@e�@��@��@ 
@ \)@ ��@ �t@!B@!X@!�<@!�
@"6@"V�@"�0@"׹@#�@#UU@#�0@#�\@$*@$S�@$�u@$�C@%V@%K@%�+@%�>@%�Q@&:@&v@&��@&��@'(�@'e�@'��@'�t@(@(M�@(�+@(�2@(�9@)4�@)m:@)��@)��@*�@*SI@*�P@*ƨ@+]@+:@+s_@+�@+�@,[@,T�@,��@,@,�,@-/�@-e	@-�H@-��@.�@.:@.m�@.�(@.׹@/J@/?}@/s_@/��@/�#@0V@0A�@0uk@0��@0�t@1�@1B8@1uk@1��@1��@2b@2B8@2uk@2�A@2܀@3b@3D�@3y�@3�f@3�H@4�@4Ji@4�@4�9@4�@5�@5Q�@5�@5��@5��@6
@6R�@6�@7X@7��@8`�@8��@9��@9��@:�I@;5�@;��@<7�@<�C@=:�@=�[@><@>�O@?hs@?�E@@^5@@�4@Az�@B1@B�i@C*@C��@D�@D��@EB8@E�w@F;d@F�H@G]�@Gխ@Ht@H��@I��@J]@J��@K*@K��@L%�@L��@M5�@M��@NDD@N��@OX�@O�@Pj@Q�@S&�@TX@U��@W�@Xa�@Y�-@[�@\v@]��@_B@`T�@a�@c@dV@e��@e��@f1'@f}�@f�c@g�@gK�@g��@g��@h�@ho�@h�4@h�@i(�@ix&@i��@i��@jD�@juk@j�w@ ^G�O�G�O�G�O�@ ^@ G�O�G�O�G�O�@ ^G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�G�O�G�O�@ G�O�@ G�O�G�O�G�O�G�O�@ �G�O�G�O�@ @ v@ �@ 1@ 	�@ 
�@ J@ �@ b@ o@ �@ �@ �@ �@ �@ g@ !s@ #�@ &�@ (�@ +�@ -�@ /�@ 2�@ 5�@ 8�@ ;d@ >�@ A�@ D�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A�$�A�&�A�&�A�+A�&�A�&�A�$�A��A� �A�+A��Aʥ�A��mA�ZA�33AĲ-AăA�r�A�l�A�^5A�O�A�M�A�;dA�bA���AÛ�A�l�A��`A�ȴA��A�%A�K�A��TA��HA�^5A��A�t�A��mA���A�JA���A��!A�-A�t�A���A��!A��A�  A���A��A��A�\)A��
A�`BA�Q�A�G�A�1'A�oA��HA���A���A�\)A��A�ƨA�Q�A�+A���A��A�K�A���A��A�r�A��jA��mA��9A���A���A�S�A���A��A�
=A�S�A�%A���A��A��yA�&�A��TA�5?A��/A��A��+A�I�A��HA���A�oA��^A�7LA��yA��wA��7A�r�A�G�A���A�?}A��A��/A��9A���A��A�/A�9XA�-A�+A���A���A��DA���A��A��+A�%A���A�1'A�ĜA��A�l�A��A�A"�A|��A|JA{�A{VAy�Ax�HAwƨAvQ�AuS�AtffAst�Ap��Ap5?AnbAi�Ag�Ae�Ae%AdVAc��AbE�AahsA^(�AZ�DAV��AU�AU|�AR��ANr�AK�^AI�AH�!AGdZAF��AE�PAEVADĜAC��ACoAAG�A?��A=�7A;�^A:��A:z�A:bNA9�#A8$�A7��A6�yA6ZA5�A5�A3��A3O�A2�A0�A/�A/��A.(�A+�#A)�
A(�\A'G�A%��A$v�A"��A!AA�A��A��A��Ar�A�FAz�A�;A�A33A�`A�!A�+AbNA��A\)A��A�A�yA�jAA�A|�A��A��A�^A~�A��A;dAbA�/A-A�mA�A
�A	
=An�A��A7LA�yA�A�A1A��A�jA�;A��AG�A b@��@��m@�ff@�A�@��y@��^@��9@��@���@�bN@���@�S�@��@�S�@��@�@��@��@�33@�~�@�V@���@�|�@�\)@�Ĝ@��u@��@�v�@���@���@��w@��@�1@�E�@�V@��@�p�@��\@�|�@�M�@�@��@�7L@�%@�A�@���@�ff@�n�@�E�@�$�@�&�@��D@���@��H@�5?@���@���@�5?@�V@���@�+@�ff@�V@�ƨ@���@���@�&�@�Z@�ȴ@���@�/@�I�@~E�@|z�@{S�@yG�@x1'@v��@up�@tz�@r��@qx�@o
=@m��@lj@kdZ@jJ@h��@f�@e@c�@aX@_��@\��@[@Y��@W\)@U��@T�j@R-@QG�@PbN@NV@M/@L1@K33@J~�@G�P@F�y@F��@C�m@B�@AX@@A�@?�P@>�y@>E�@=?}@<I�@;dZ@:�!@:-@8�`@8�9@8Q�@6{@4��@3ƨ@2�@1hs@1%@0��@/��@.��@-@-?}@,�@+S�@*�\@)7L@(bN@&�+@&5?@%@%`B@$��@#t�@"��@!��@!7L@ �9@   @�@5?@�h@��@�@��@��@��@@�-@?}@�@I�@�@C�@�H@hs@��@l�@�y@�+@ff@V@O�@��@�@�m@dZ@
�\@
J@	x�@Ĝ@bN@�w@|�@\)@
=@@��@�
@��@��@  �?��h?�1?���?�E�?��/?�o?�&�??�V?�C�?��?�Q�?�P?��?�z�?�!?��?�;d?���?�ƨ?�~�?���?׍P?�$�?�?}?��
?�n�?�G�?Ѓ?�  ?�\)?��?��?���?���?�j?��H?�~�?ə�?�X?ȓu?���?�
=?�ff?��?�33?���?��`?�;d?��R?�v�?�p�?�I�?�(�?�ƨ?���?�?�=q?��?�^5?�=q?�=q?�=q?�=q?�=q?�=q?�=q?��?�^5?�=q?�=q?�~�?�~�?�~�?�~�?���?���?���?�A� �A��A��A� �A�&�A�"�A�&�A�$�A�$�A�$�A�&�A�&�A�(�A�&�A�(�A�(�A�$�A�&�A� �A��A��A� �A� �A�"�A� �A�&�A�&�A�&�A�&�A�+A�(�A�"�A� �A� �A�&�A�&�A�&�A�$�A�$�A�$�A�&�A�(�A�+A�+A�-A�-A�$�A�$�A�&�A�&�A�$�A� �A�&�A�(�A�"�A�{A��A�"�A�+A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            A�$�A�$�A�&�A�&�A�+A�&�A�&�A�$�A��A� �A�+A��Aʥ�A��mA�ZA�33AĲ-AăA�r�A�l�A�^5A�O�A�M�A�;dA�bA���AÛ�A�l�A��`A�ȴA��A�%A�K�A��TA��HA�^5A��A�t�A��mA���A�JA���A��!A�-A�t�A���A��!A��A�  A���A��A��A�\)A��
A�`BA�Q�A�G�A�1'A�oA��HA���A���A�\)A��A�ƨA�Q�A�+A���A��A�K�A���A��A�r�A��jA��mA��9A���A���A�S�A���A��A�
=A�S�A�%A���A��A��yA�&�A��TA�5?A��/A��A��+A�I�A��HA���A�oA��^A�7LA��yA��wA��7A�r�A�G�A���A�?}A��A��/A��9A���A��A�/A�9XA�-A�+A���A���A��DA���A��A��+A�%A���A�1'A�ĜA��A�l�A��A�A"�A|��A|JA{�A{VAy�Ax�HAwƨAvQ�AuS�AtffAst�Ap��Ap5?AnbAi�Ag�Ae�Ae%AdVAc��AbE�AahsA^(�AZ�DAV��AU�AU|�AR��ANr�AK�^AI�AH�!AGdZAF��AE�PAEVADĜAC��ACoAAG�A?��A=�7A;�^A:��A:z�A:bNA9�#A8$�A7��A6�yA6ZA5�A5�A3��A3O�A2�A0�A/�A/��A.(�A+�#A)�
A(�\A'G�A%��A$v�A"��A!AA�A��A��A��Ar�A�FAz�A�;A�A33A�`A�!A�+AbNA��A\)A��A�A�yA�jAA�A|�A��A��A�^A~�A��A;dAbA�/A-A�mA�A
�A	
=An�A��A7LA�yA�A�A1A��A�jA�;A��AG�A b@��@��m@�ff@�A�@��y@��^@��9@��@���@�bN@���@�S�@��@�S�@��@�@��@��@�33@�~�@�V@���@�|�@�\)@�Ĝ@��u@��@�v�@���@���@��w@��@�1@�E�@�V@��@�p�@��\@�|�@�M�@�@��@�7L@�%@�A�@���@�ff@�n�@�E�@�$�@�&�@��D@���@��H@�5?@���@���@�5?@�V@���@�+@�ff@�V@�ƨ@���@���@�&�@�Z@�ȴ@���@�/@�I�@~E�@|z�@{S�@yG�@x1'@v��@up�@tz�@r��@qx�@o
=@m��@lj@kdZ@jJ@h��@f�@e@c�@aX@_��@\��@[@Y��@W\)@U��@T�j@R-@QG�@PbN@NV@M/@L1@K33@J~�@G�P@F�y@F��@C�m@B�@AX@@A�@?�P@>�y@>E�@=?}@<I�@;dZ@:�!@:-@8�`@8�9@8Q�@6{@4��@3ƨ@2�@1hs@1%@0��@/��@.��@-@-?}@,�@+S�@*�\@)7L@(bN@&�+@&5?@%@%`B@$��@#t�@"��@!��@!7L@ �9@   @�@5?@�h@��@�@��@��@��@@�-@?}@�@I�@�@C�@�H@hs@��@l�@�y@�+@ff@V@O�@��@�@�m@dZ@
�\@
J@	x�@Ĝ@bN@�w@|�@\)@
=@@��@�
@��@��@  �?��h?�1?���?�E�?��/?�o?�&�??�V?�C�?��?�Q�?�P?��?�z�?�!?��?�;d?���?�ƨ?�~�?���?׍P?�$�?�?}?��
?�n�?�G�?Ѓ?�  ?�\)?��?��?���?���?�j?��H?�~�?ə�?�X?ȓu?���?�
=?�ff?��?�33?���?��`?�;d?��R?�v�?�p�?�I�?�(�?�ƨ?���?�?�=q?��?�^5?�=q?�=q?�=q?�=q?�=q?�=q?�=q?��?�^5?�=q?�=q?�~�?�~�?�~�?�~�?���?���?���?�A� �A��A��A� �A�&�A�"�A�&�A�$�A�$�A�$�A�&�A�&�A�(�A�&�A�(�A�(�A�$�A�&�A� �A��A��A� �A� �A�"�A� �A�&�A�&�A�&�A�&�A�+A�(�A�"�A� �A� �A�&�A�&�A�&�A�$�A�$�A�$�A�&�A�(�A�+A�+A�-A�-A�$�A�$�A�&�A�&�A�$�A� �A�&�A�(�A�"�A�{A��A�"�A�+A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
�B
��B
=B	7BDBPBPBJBPBVBVBbB�B"�B)�B.B<jBW
BbNBt�B�%B�{B��B��B��B�XB�B  B�B$�B�B�BT�BaHBr�Bw�B�B��B�B�-B�9B�FB�LB�LB�FB�FB�LB�LB�LB�RB�XB�dB�jB�}B�}B�}B��B��B��B�}B�wB�RB�XB�B��B��B��B��B�VB�1Bp�BZBK�BI�B@�B:^B6FB&�B��B��B�B�B��B�B��B��B��B�{B�bB�DB�1Bl�BL�B@�B6FB49B2-B0!B.B(�B�BPBB
��B
�yB
�fB
��B
ŢB
��B
�-B
�'B
��B
��B
�bB
�B
u�B
p�B
dZB
VB
P�B
M�B
H�B
A�B
9XB
1'B
%�B
�B
�B
\B
  B
B	�B	�#B	ȴB	�dB	�RB	�'B	�B	��B	��B	�%B	s�B	bNB	\)B	XB	F�B	0!B	$�B	�B	uB	
=B	
=B	B	B��B��B��B�B�yB�/B�B��B��B��BɺBɺBƨBÖB��B�wB�XB�LB�?B�B�B��B��B��B��B�{B�hB�JB�%B�%B�Bz�Bu�Bs�Bp�Bq�Bs�Bu�Bp�Br�Bv�Bw�Bw�Bv�Bt�Br�Bp�Bs�Bv�Bv�Bu�Bu�Bu�Bt�Bu�Bu�Bu�Bm�Bt�Bs�Bl�BaHB_;B[#BYBM�BE�BB�BA�B@�B?}B<jB<jB=qB<jB:^B<jB@�BA�B<jB:^B8RB6FB8RB7LB7LB5?B6FB7LB6FB7LB?}Be`BgmBo�Be`BC�B?}BD�BN�BVBXB[#BdZBl�Br�B�+B�hB��B��BĜBŢB�#B�mB�`B��B��B	'�B	2-B	D�B	P�B	XB	`BB	e`B	l�B	o�B	�B	� B	�B	�%B	�VB	�{B	��B	��B	��B	�B	�-B	�^B	�}B	ǮB	ɺB	��B	��B	�B	�/B	�TB	�ZB	�mB	�B	�B	�B	��B	��B	��B	��B	��B
B
B
B
B
1B
	7B
PB
\B
bB
bB
uB
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
&�B
(�B
(�B
,B
,B
.B
/B
0!B
1'B
33B
33B
5?B
7LB
6FB
:^B
;dB
<jB
=qB
>wB
>wB
?}B
?}B
A�B
A�B
A�B
C�B
D�B
E�B
E�B
G�B
H�B
I�B
J�B
K�B
J�B
K�B
M�B
M�B
N�B
O�B
O�B
P�B
Q�B
S�B
S�B
VB
VB
W
B
W
B
YB
YB
ZB
[#B
\)B
\)B
]/B
^5B
_;B
_;B
`BB
aHB
dZB
e`B
ffB
e`B
ffB
ffB
gmB
gmB
gmB
gmB
hsB
jB
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
o�B
p�B
p�B
p�B
q�B
q�B
s�B
s�B
t�B
t�B
t�B
t�B
w�B
x�B
x�B
z�B
{�B
}�B
� B
� B
�B
�B
�%B
�%B
�1B
�7B
�=B
�DB
�JB
�VB
�VB
�\B
�hB
�hB
�uB
�{B
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
�B
�B
�B
�B
�B
�'B
�'B
�-B
�3B
�-B
�9B
�?B
�?B
�?B
�?B
�FB
�?B
�?B
�?B
�FB
�FB
�?B
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�LB
�FB
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
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            B
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
̩B
B	B!B.B/B)B0B6B7BDBoB"�B)�B-�B<NBV�Bb4Bt�B�B�bB��B��B��B�AB�B��BxB$�BBmBT�Ba5Br�Bw�B��B�pB��B�B�*B�8B�>B�?B�9B�:B�AB�AB�BB�HB�OB�[B�bB�vB�vB�wB�}B�~B�B�yB�tB�OB�VB�B��B��B��B��B�WB�2Bp�BZBK�BI�B@�B:aB6JB&�B��B��B�B�B��B�B��B��B��B��B�lB�NB�<Bl�BL�B@�B6RB4FB2;B0/B.#B)B�B`B/B
��B
�B
�xB
�B
ŵB
��B
�AB
�;B
�B
��B
�xB
�5B
u�B
p�B
drB
VB
P�B
M�B
H�B
A�B
9sB
1BB
%�B
�B
�B
yB
 B
7B	��B	�BB	��B	��B	�rB	�HB	�#B	��B	��B	�GB	s�B	bqB	\LB	X4B	F�B	0FB	%B	�B	�B	
cB	
dB	FB	4B�"B�B��B��B�B�ZB�;B�)B�$B�B��B��B��B��B��B��B��B�}B�pB�FB�4B�/B�#B��B��B��B��B�B�[B�[B�<B{Bu�Bs�Bp�Bq�Bs�Bu�Bp�Br�BwBxBxBwBt�Br�Bp�Bs�Bw	Bw	BvBvBvBt�BvBvBvBm�BuBs�Bl�Ba�B_�B[kBY_BNBE�BB�BA�B@�B?�B<�B<�B=�B<�B:�B<�B@�BA�B<�B:�B8�B6�B8�B7�B7�B5�B6�B7�B6�B7�B?�Be�Bg�Bo�Be�BC�B?�BD�BO=BVkBXyB[�Bd�Bl�Bs%B��B��B�B�RB�!B�*BۮB��B��B�zB��B	(�B	2�B	E;B	Q�B	X�B	`�B	f
B	m8B	pNB	��B	��B	��B	��B	�B	�<B	�^B	�yB	��B	��B	��B	�0B	�RB	ȆB	ʕB	϶B	��B	� B	�B	�;B	�DB	�YB	�tB	�B	�B	��B	��B	��B	��B	��B
B
B
&B
)B
	>B

FB
bB
qB
yB
|B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
%B
(!B
*1B
*3B
-HB
-KB
/ZB
0dB
1lB
2uB
4�B
4�B
6�B
8�B
7�B
;�B
<�B
=�B
>�B
?�B
?�B
@�B
@�B
B�B
B�B
CB
EB
FB
G"B
G%B
I3B
J<B
KDB
LNB
MVB
LSB
M[B
OjB
OlB
PuB
Q}B
Q�B
R�B
S�B
U�B
U�B
W�B
W�B
X�B
X�B
Z�B
Z�B
[�B
\�B
]�B
]�B
^�B
_�B
aB
aB
bB
cB
f,B
g5B
h=B
g9B
hAB
hDB
iMB
iOB
iQB
iTB
j\B
ljB
nxB
n{B
n}B
o�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
r�B
s�B
s�B
u�B
u�B
v�B
v�B
v�B
v�B
y�B
z�B
z�B
}B
~B
�.B
�@B
�EB
�XB
�eB
�|B
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
�B
�"B
�'B
�3B
�?B
�SB
�XB
�kB
�pB
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
�	B
�B
�B
�"B
�5B
�JB
�`B
�tB
��B
��B
��B
��B
��B
��B
�B
�B
�.B
�8B
�SB
�hB
�wB
�{B
�}B
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
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202432021061413551320210614135513202106171311372021061713113720210617131137201807242202432021061413551320210614135513202106171311372021061713113720210617131137PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024320180724220243  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024320180724220243QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024320180724220243QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145220210617131452IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                