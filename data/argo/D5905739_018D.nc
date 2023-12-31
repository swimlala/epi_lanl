CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  	   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:51Z creation      
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
resolution        =���   axis      Z        H  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  P0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ֌   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   <   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � \   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                        HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar            HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        (   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �|   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �|   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � |Argo profile    3.1 1.2 19500101000000  20180724220251  20210617131457  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�l�`�v@�l�`�v11  @�l��gp@�l��gp@6ڡ!B@6ڡ!B�c�~��L��c�~��L�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@ff@@  @�  @�  @�33@�33A   A  A&ffAA��A`  A�  A�33A�  A�  A���A�  A���A�B ffB��B��B  B��B'��B0ffB7��B@  BHffBPffBX  B_��Bg��Bp  Bw��B�  B�33B�  B�  B�  B�  B�33B���B�33B�33B�ffB�33B���B�  B���B���B���B�  B�33B̙�B�ffB�ffB�ffB�ffB�33B�33B���B���B���B���B���B�ffC �C�C  C  C�C
�C  C  C�fC��C�fC33C�C�CL�C33C   C"�C$33C&  C'��C*  C,33C.  C/��C1�fC433C6  C7��C:�C<L�C>33C@  CB33CD�CF  CH33CJ�CL  CN  CO�fCR�CT33CV  CW�fCZ�C\  C]�fC`33Cb�Cc�fCf33Ch�Ci��Cl  Cn�Cp  Cq�fCt  Cv33Cx  Cy�fC|�C~  C��C��C��3C��fC�  C��C��C��C�  C��fC��C��C��C�  C��C�  C�  C��C��C�  C�&fC��C��C��C��3C��C��C��3C��C��C��3C��C�  C��3C�  C��C��C��3C�  C��C�  C��3C��C�&fC��C�  C��C��C��3C��C�&fC��C��3C�  C��C�&fC�&fC�&fC��C��fC�  C��C��C�  C��fC��C�  C��fC��fC��3C��3C�  C��C��C��C�  C��fC��fC��3C��3C�  C��C��C��C��C�&fC��C��fC��3C�  C��C�  C�  C��C��3C�  C��C��C��3C��fC��3C�  C��C�&fC��C��3C�  C��C�&fC�  C��fC�  C�  C��C��C�&fC�&fC�33C��C��C�&fC��C��fD��Dy�DfD	��D  D�fD� D@ DfD�3D� DS3D"3D$� D'�fD*` D-&fD/�3D2s3D5  D7�fD:3D<�3D?�DA� DD�DF��DI3DK� DM�3DPl�DR��DU�fDX�DZ��D]�D_��DbL�Dd�3Dg�fDj9�Dl�3Do�fDr` Du�Dw��DzffD|� DL�D���D�I�D���D�� D�#3D�ffD���D���D�FfD�� D��3D�fD�\�D���D�ٚD�  D�` D��fD��3D��D�9�D�i�D�� D�ɚD��fD�#3D�L�D�|�D���D��3D��D�6fD�c3D�� D��fD���D�6fD�i�D��fD���D�3D�P D��fD��fD�	�D�P D���D��3D�fD�P D���D��3D���D�6fD�l�DĦfD�� D�3D�FfD�s3DʦfD��fD�� D� D�)�D�FfD�` D�vfDӌ�Dԣ3DնfD�� D��D���D� D��D�6fD�I�D�ffD�|�D��3D� D�fD��fD���D�  D�fD�)�D�FfD�\�D�y�D왚D���D�� D�� D��fD�3D�)�D�C3D�Y�D�l�D�� D���D��fD���D��3D���D�� D��3D��fE t�E �fE��E�E� E( E��E6fE�3EA�E� EP Ea�E��E
  E3E��E� E�E!�E��E��E3E�Es3E�E�3ES3EL�E��E1�E+3E � E!� E#�E$ E%� E&�3E(3E)�E*�fE+� E,��E.h E/Y�E0�fE2fE3�E4q�E5њE6�3E83E9s3E:��E<fE?<�EA� EE3EH��EK�fEN� EQ�3ET�fEXfE[33E^8 Ea{3EdNfEgnfEj�3EmњEq Et@ Ew  Ezd�E}��E�=�E�� E�m�E�� E���E��E��3E�4�E�� E� E�VfE���E���E�ZfE�� E�� E�P�E���E�� E�%�E�3E���E�3E�i�E���E��E�RfE���E�  E�T >���>���>���>���>���>���>���>���>���>���>���>���>���?   ?   ?��>���?333?��?��?333?�  ?fff?���?�ff?�  ?���?�33@��@   @333@@  @S33@l��@�  @���@�33@�33@���@���@�33@�  @���@陚@�ffA33A	��A  AffA   A&ffA.ffA6ffA<��AD��AK33AS33AX  A`  Ah  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414144444141414411411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?fff?�33@&ff@`  @�  @�  @�33@�33A  A  A.ffAI��Ah  A�  A�33A�  A�  Ař�A�  A���A���BffB
��B��B  B!��B)��B2ffB9��BB  BJffBRffBZ  Ba��Bi��Br  By��B�  B�33B�  B�  B�  B�  B�33B���B�33B�33B�ffB�33B���B�  B���B���B���B�  B�33B͙�B�ffB�ffB�ffB�ffB�33B�33B���B���B���B���B���B�ffC ��C��C� C� C��C
��C� C� CffCL�CffC�3C��C��C��C�3C � C"��C$�3C&� C(L�C*� C,�3C.� C0L�C2ffC4�3C6� C8L�C:��C<��C>�3C@� CB�3CD��CF� CH�3CJ��CL� CN� CPffCR��CT�3CV� CXffCZ��C\� C^ffC`�3Cb��CdffCf�3Ch��CjL�Cl� Cn��Cp� CrffCt� Cv�3Cx� CzffC|��C~� C�&fC�L�C�33C�&fC�@ C�Y�C�Y�C�L�C�@ C�&fC�L�C�Y�C�Y�C�@ C�Y�C�@ C�@ C�Y�C�L�C�@ C�ffC�Y�C�L�C�L�C�33C�L�C�L�C�33C�Y�C�L�C�33C�L�C�@ C�33C�@ C�Y�C�L�C�33C�@ C�Y�C�@ C�33C�L�C�ffC�Y�C�@ C�Y�C�Y�C�33C�L�C�ffC�Y�C�33C�@ C�Y�C�ffC�ffC�ffC�L�C�&fC�@ C�L�C�Y�C�@ C�&fC�L�C�@ C�&fC�&fC�33C�33C�@ C�L�C�Y�C�Y�C�@ C�&fC�&fC�33C�33C�@ C�L�C�L�C�Y�C�L�C�ffC�L�C�&fC�33C�@ C�Y�C�@ C�@ C�Y�C�33C�@ C�L�C�L�C�33C�&fC�33C�@ C�L�C�ffC�L�C�33C�@ C�Y�C�ffC�@ C�&fC�@ C�@ C�L�C�Y�C�ffC�ffC�s3C�L�C�L�C�ffC�L�C�&fD�D��D&fD	��D@ D�fD� D` D&fD�3D� Ds3D"33D%  D'�fD*� D-FfD/�3D2�3D5  D7�fD:33D<�3D?9�DA� DD9�DF��DI33DK� DN3DP��DS�DU�fDX9�DZ��D]9�D_ٚDbl�Dd�3Dg�fDjY�Dm3Do�fDr� Du,�DwٚDz�fD|� Dl�D��D�Y�D���D�� D�33D�vfD���D��D�VfD�� D��3D�&fD�l�D���D��D�0 D�p D��fD��3D��D�I�D�y�D�� D�ٚD�fD�33D�\�D���D���D��3D��D�FfD�s3D�� D��fD��D�FfD�y�D��fD���D�#3D�` D��fD��fD��D�` D���D��3D�&fD�` D���D��3D�	�D�FfD�|�DĶfD�� D�#3D�VfDɃ3DʶfD��fD�  D�  D�9�D�VfD�p D҆fDӜ�DԳ3D��fD�� D���D��D�  D�,�D�FfD�Y�D�vfDߌ�D�3D� D��fD��fD���D� D�&fD�9�D�VfD�l�D뉚D쩚D���D�� D�� D�fD�#3D�9�D�S3D�i�D�|�D�� D���D��fD���D��3D���D�� D��3D��fE |�EfE��E�E� E0 E��E>fE�3EI�E� EX Ei�E��E
 E3E��E� E!�E)�E��E��E3E�E{3E�E�3E[3ET�E��E9�E33E � E!� E#�E$ E%� E&�3E(3E)�E*�fE+� E-�E.p E/a�E0�fE2&fE3�E4y�E5ٚE6�3E8#3E9{3E:��E<&fE?D�EB  EE#3EH��EK�fEN� EQ�3ET�fEXfE[;3E^@ Ea�3EdVfEgvfEj�3EmٚEq EtH Ew( Ezl�E}��E�A�E�� E�q�E�� E���E��E��3E�8�E�� E� E�ZfE���E���E�^fE�� E�� E�T�E���E�� E�)�E��3E���E�3E�m�E�ŚE��E�VfE���E� E�X G�O�G�O�G�O�G�O�?L��G�O�?L��G�O�G�O�G�O�G�O�G�O�?fffG�O�?�  G�O�?fffG�O�G�O�?���?���G�O�?�33?���?�ff@   @ff@��@,��@@  @S33@`  @s33@�ff@�  @���@�33@�33@���@ə�@�33@�  @���@���A33A33A��A  AffA(  A.ffA6ffA>ffAD��AL��AS33A[33A`  Ah  Ap  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414144444141414411411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @ �@ %@ V@ {@ O@ ""@ )�@ 0x@ 6�@ =q@ F�@ R�@ _�@ m:@ z3@ ��@ �0@ �5@ �~@ ��@ �*@ �#@ �y@ � @j@b@
@-@9X@G�@V@c�@p�@}�@�D@��@��@��@�>@�7@��@�@�,@�@�@"�@0x@>�@K�@X@ff@s_@�@��@�@�Y@�^@�W@��@�@�L@�E@
�@6@$�@2�@@,@M�@]�@j@x&@�@��@�@��@��@�c@�[@�T@�@^@V@�@+@7�@DD@R�@`�@m:@y�@��@��@��@�!@�w@�*@�t@�@�q@v@o@�@-�@:�@G�@V�@c�@p�@~K@�D@�H@��@��@��@��@��@��@��@�@�@#�@0x@<@K@Yn@ff@s_@��@��@�@��@�@��@�C@��@�@@�9@
=@B@&�@3�@@�@M$@\�@k.@x�@�@�$@�m@�@�@�@�
@�@�@ �@V@�@)�@7L@C�@SI@`B@l�@{�@��@��@��@��@��@�@�t@�y@��@	�@	�@	 �@	-�@	:@	I@	V�@	bN@	qS@	�W@	�P@	��@	�A@	�F@	Ĝ@	�C@	��@	�4@	��@
�@
*@
#�@
/�@
<@
K�@
X�@
e	@
r�@
�@
��@
�@
�Y@
��@
�W@
Ӡ@
��@
�@
��@	�@�@&;@3�@B8@O0@^5@j@v@�p@��@��@�@��@��@�[@�@�@ �@�@�@(G@6�@D�@S�@`B@l�@z�@��@�<@��@�!@�&@��@�#@�y@��@v@�@g@-@<@H]@S�@�/@!s@g@�@�Y@:�@�@�7@�@hs@��@��@I�@�0@��@,`@x&@�2@�@N�@�u@�@[@bN@�A@��@/@r�@��@��@;d@�@ƨ@�@Q=@��@�/@#�@hs@�-@��@FQ@�@�t@#�@l�@��@�Y@;d@�p@�o@@X�@��@�@(G@o�@�F@��@A�@�|@�@�@S�@��@��@  @ c�@ ��@ �@!'�@!i�@!�M@!�y@")�@"i!@"��@"��@#)�@#l�@#�@#�4@$,`@$n�@$��@$�@%5@@%x�@%�@%�E@&@�@&�@&�W@'J@'Q�@'��@'�#@( @(b�@(��@(��@)+@)n�@)��@)�@*6�@*x&@*��@*��@+;d@+x�@+�R@+��@,1�@,n�@,��@,�@-!s@-\�@-��@-Ӡ@.�@.Ji@.�@.�w@.��@/5@@/r@/�f@/��@0""@0]�@0��@0�\@1@1Lu@1�+@1��@1�Q@2<@2y�@2�9@2��@3,`@3g�@3��@3��@4�@4Wb@4�@4��@5v@5>�@5r�@5�Y@5�@6�@6Yn@6�#@6��@7	�@7D�@7~�@7��@7��@80x@8i!@8��@8�#@9{@9N�@9�>@:oF@:�H@;O�@;��@<i�@=V@=~�@> �@>��@?*S@?��@@1'@@�|@A7�@A�h@BB�@B��@C~�@C�y@D��@D�Y@E�u@F  @F��@Go@G�F@H"�@H�J@I/�@I�|@Ji�@J��@Ki!@K�Q@Le	@L�E@M�u@M� @N�P@O @O��@PC�@Q�<@R@TB@U��@V��@XO@Y�@Z�/@\2�@]�|@^�7@`4�@ai!@b�w@dB@ex&@f�
@h6�@ip�@jխ@l)�@mn�@n�c@p&�@qt@r�O@t*@uv�@v�@x(�@xb�@x��@x�M@y&;@yy�@y�@y��@zK�@z~�@z��@{^@{M�@{�I@{�@|�@|`�@|�	@|܀@}&;@}p�@}�RG�O�G�O�G�O�G�O�@ G�O�@ G�O�G�O�G�O�G�O�G�O�@ �G�O�@ jG�O�@ �G�O�G�O�@ @ �G�O�@ %@ �@ �@ 
=@ 
�@ �@ @ @ @ {@ �@ B@ O@ [@ g@ "�@ $�@ '�@ )�@ ,`@ /@ 1�@ 4�@ 7�@ :�@ =q@ @,@ DD@ F�@ Ji@ M�@ P�@ S�@ V�@ Z@ \)@ _�@ b�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aа!AжFAд9AиRAиRAоwAмjAоwA�ĜA���A���A�A�ƨA�ƨA�ĜA�ĜA�Aк^AН�AЇ+A�r�A��yA�dZA��Aʹ9A�jA�$�A���Ḁ�A̋DA���A��A�z�A���A�ZA�ƨA�7LAħ�Aã�A�K�A�VAuA�bA���A�VA�?}A��FA�-A���A�Q�A�^5A��jA���A��RA���A��\A�-A���A�hsA���A�`BA��A��;A��A�A�S�A�Q�A���A��A�z�A�;dA�
=A�ȴA��A��`A���A�1'A���A��uA�l�A��/A���A�Q�A��mA�ȴA���A�\)A��A���A��A�|�A��A��A��A���A��-A��A�l�A��7A�ƨA��A��DA�ffA�9XA�p�A�(�A���A�/A��`A�I�A���A�VA��A�K�A���A���A�5?A���A��A��HA���A�XA�^5A�E�A���A�{A�G�A��mA���A��7A�S�A�%A��yA��A��/A�|�A�ZA���A�VA�A�A��PA|�Az��Aw��Au/AqK�An1'Ak��Aj�yAi�Ag�Ag\)Ae��AdAb��Aa
=A_%A]S�A\�uA[t�AY�AWC�AV��AV�AT�RARI�APr�AO��ANZAJ  AH�9AD�\A@ZA>�A>-A;��A:I�A9XA8�`A8$�A6�HA5�A4�+A3��A2�HA2�A/�FA-�7A,�`A,��A,A�A,�A,1A+��A+�A+S�A*�A);dA'/A&v�A%��A$��A#;dA"��A"  A �jA�FA�A|�AoA�wAx�A�A��A��AA��A��A��A��A�AA�AI�A�
AO�A��AbA1'AQ�A��AS�A
�DA	?}A9XAI�A%Av�AA��A�hA ~�@�ƨ@���@��P@��h@��j@�b@�|�@��^@��+@�l�@�&�@�b@�ƨ@�t�@��T@�&�@�dZ@�v�@͡�@��
@�V@��w@���@���@�j@���@��u@�dZ@��@��9@���@��!@�-@���@��R@��y@��`@�n�@�X@�G�@�z�@���@���@��;@�C�@���@�5?@��P@���@�&�@�1@���@�V@�hs@���@l�@}�@|�@yx�@y��@y��@xbN@v��@uV@tj@q�^@p��@o�P@m�@k��@j~�@jn�@i�^@g+@e@ct�@b~�@`��@_�P@_�@]?}@\Z@Y�#@Y&�@X��@W�P@VE�@V{@T��@Tj@S@Q�7@Nff@N@M/@K"�@J-@I7L@HA�@Fff@Dz�@B��@B�\@A�#@@�u@>��@=��@;S�@9��@8�@7\)@6ȴ@5��@3�
@3C�@2M�@1G�@0�u@/;d@.ff@-�-@,Z@+�m@*n�@)7L@(Q�@'��@'K�@'
=@%@%?}@$Z@#t�@"J@!&�@�@�+@��@�@S�@=q@&�@�@b@�@��@�-@`B@��@Z@C�@C�@��@�^@��@�@��@;d@v�@@`B@�/@9X@t�@
�!@	�#@	&�@��@A�@+@v�@��@?}@�@(�@�F@��@=q@�7@ Ĝ@ b?�;d?�5??�p�?��?�"�?���?�?�Z?�hs?�bN?�\)?���?�I�?�dZ?�9?�l�?��?�o?�-?�G�?ߝ�?��?��m?�?��#?���?�ff?ա�?Լj?���?�-?�%?��;?�;d?�5??��?̋D?�"�?ʟ�?�~�?���?�7L?�Q�?���?�K�?Ƈ+?�E�?ě�?�33?�-?�&�?�A�?�\)?���?��h?�O�?��D?�(�?�dZ?�?�?�?�"�?�=q?���?��#?���?���?��H?�"�?�ƨ?�(�?���?�p�?���?�V?���?��?�;d?�\)?�\)?���?��w?��;?�  ?� �?� �?�bN?��?���?�Ĝ?��`?�%?�G�?�G�?�hs?���?��AХ�AУ�AХ�AП�AП�Aа!Aа!Aд9AмjA���AиRAд9Aк^Aк^AжFAжFAжFAжFAиRAиRAжFAжFAв-Aв-Aд9Aд9Aд9Aд9Aд9AжFAжFAжFAк^AиRAиRAк^AоwAоwAмjAмjAмjAоwAоwAоwA�A�ƨA���A���A���A���A���A�ĜA���A���A�ĜA�ĜA�ĜA�ƨA�ƨA�ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Aа!AжFAд9AиRAиRAоwAмjAоwA�ĜA���A���A�A�ƨA�ƨA�ĜA�ĜA�Aк^AН�AЇ+A�r�A��yA�dZA��Aʹ9A�jA�$�A���Ḁ�A̋DA���A��A�z�A���A�ZA�ƨA�7LAħ�Aã�A�K�A�VAuA�bA���A�VA�?}A��FA�-A���A�Q�A�^5A��jA���A��RA���A��\A�-A���A�hsA���A�`BA��A��;A��A�A�S�A�Q�A���A��A�z�A�;dA�
=A�ȴA��A��`A���A�1'A���A��uA�l�A��/A���A�Q�A��mA�ȴA���A�\)A��A���A��A�|�A��A��A��A���A��-A��A�l�A��7A�ƨA��A��DA�ffA�9XA�p�A�(�A���A�/A��`A�I�A���A�VA��A�K�A���A���A�5?A���A��A��HA���A�XA�^5A�E�A���A�{A�G�A��mA���A��7A�S�A�%A��yA��A��/A�|�A�ZA���A�VA�A�A��PA|�Az��Aw��Au/AqK�An1'Ak��Aj�yAi�Ag�Ag\)Ae��AdAb��Aa
=A_%A]S�A\�uA[t�AY�AWC�AV��AV�AT�RARI�APr�AO��ANZAJ  AH�9AD�\A@ZA>�A>-A;��A:I�A9XA8�`A8$�A6�HA5�A4�+A3��A2�HA2�A/�FA-�7A,�`A,��A,A�A,�A,1A+��A+�A+S�A*�A);dA'/A&v�A%��A$��A#;dA"��A"  A �jA�FA�A|�AoA�wAx�A�A��A��AA��A��A��A��A�AA�AI�A�
AO�A��AbA1'AQ�A��AS�A
�DA	?}A9XAI�A%Av�AA��A�hA ~�@�ƨ@���@��P@��h@��j@�b@�|�@��^@��+@�l�@�&�@�b@�ƨ@�t�@��T@�&�@�dZ@�v�@͡�@��
@�V@��w@���@���@�j@���@��u@�dZ@��@��9@���@��!@�-@���@��R@��y@��`@�n�@�X@�G�@�z�@���@���@��;@�C�@���@�5?@��P@���@�&�@�1@���@�V@�hs@���@l�@}�@|�@yx�@y��@y��@xbN@v��@uV@tj@q�^@p��@o�P@m�@k��@j~�@jn�@i�^@g+@e@ct�@b~�@`��@_�P@_�@]?}@\Z@Y�#@Y&�@X��@W�P@VE�@V{@T��@Tj@S@Q�7@Nff@N@M/@K"�@J-@I7L@HA�@Fff@Dz�@B��@B�\@A�#@@�u@>��@=��@;S�@9��@8�@7\)@6ȴ@5��@3�
@3C�@2M�@1G�@0�u@/;d@.ff@-�-@,Z@+�m@*n�@)7L@(Q�@'��@'K�@'
=@%@%?}@$Z@#t�@"J@!&�@�@�+@��@�@S�@=q@&�@�@b@�@��@�-@`B@��@Z@C�@C�@��@�^@��@�@��@;d@v�@@`B@�/@9X@t�@
�!@	�#@	&�@��@A�@+@v�@��@?}@�@(�@�F@��@=q@�7@ Ĝ@ b?�;d?�5??�p�?��?�"�?���?�?�Z?�hs?�bN?�\)?���?�I�?�dZ?�9?�l�?��?�o?�-?�G�?ߝ�?��?��m?�?��#?���?�ff?ա�?Լj?���?�-?�%?��;?�;d?�5??��?̋D?�"�?ʟ�?�~�?���?�7L?�Q�?���?�K�?Ƈ+?�E�?ě�?�33?�-?�&�?�A�?�\)?���?��h?�O�?��D?�(�?�dZ?�?�?�?�"�?�=q?���?��#?���?���?��H?�"�?�ƨ?�(�?���?�p�?���?�V?���?��?�;d?�\)?�\)?���?��w?��;?�  ?� �?� �?�bN?��?���?�Ĝ?��`?�%?�G�?�G�?�hs?���?��AХ�AУ�AХ�AП�AП�Aа!Aа!Aд9AмjA���AиRAд9Aк^Aк^AжFAжFAжFAжFAиRAиRAжFAжFAв-Aв-Aд9Aд9Aд9Aд9Aд9AжFAжFAжFAк^AиRAиRAк^AоwAоwAмjAмjAмjAоwAоwAоwA�A�ƨA���A���A���A���A���A�ĜA���A���A�ĜA�ĜA�ĜA�ƨA�ƨA�ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
`BB
_;B
`BB
`BB
`BB
_;B
_;B
_;B
_;B
`BB
^5B
^5B
^5B
^5B
^5B
_;B
^5B
^5B
ZB
ZB
XB
N�B
D�B
C�B
L�B
M�B
G�B
F�B
Q�B
S�B
YB
�=B
��B
�B
�FB
B
�B
�fB
�fB
�B
�B%BhB�B33B<jBq�B�{B�B�!B�LB�wB�wB��B��B��B��B��B��B�B�/B�NB�mB��BBBPB�B�B�B�B�B!�B#�B'�B,B1'BYB^5B\)BS�BR�BO�BC�BB�B@�B:^B6FB5?B2-B.B$�BPB��B�mB�B��B�wB�dB�9B�dB��B��B�B�5B�5B�5B��BǮB��B�FB�9B�B��B��B�PB�B� B}�B{�Bw�Bs�B`BBH�B/BuB%B
��B
��B
��B
�B
�B
�mB
�/B
��B
��B
ȴB
B
�=B
L�B
5?B
PB	�ZB	��B	�FB	�\B	s�B	jB	`BB	S�B	I�B	A�B	33B	+B	"�B	�B	�B	�B	{B	bB	VB	\B	oB	bB	VB	B	B��B�B�;B��B��B��B��B�bB�B�B�B�B�B�B� B� B~�B}�Bx�Bv�By�By�Bw�Bv�Bv�Bv�Bx�Bx�By�Bv�Bv�Bs�Bs�Bp�BjBk�BhsBe`BaHBdZBcTBcTBaHB_;B\)B[#BZBXBYBW
BW
BT�BW
BVBO�BQ�BN�BO�BL�BK�BH�BF�BF�BE�BB�BB�B@�B@�B>wB>wB>wB<jB<jB<jB8RB9XB9XB8RB?}BE�BG�BF�BC�B=qB8RB9XB;dB<jB=qB]/Bp�Bw�BL�BVBy�B�bB�{B��B��B��B��B��B�qB��B��B�HB�`B��B	B	�B	�B	�B	�B	33B	B�B	M�B	`BB	r�B	w�B	�VB	��B	��B	�{B	��B	��B	�3B	�-B	�B	�'B	�jB	B	ƨB	��B	�
B	�B	�/B	�`B	�B	��B	��B	��B
B
B
B
B
%B
+B
DB
bB
uB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
$�B
&�B
&�B
'�B
)�B
,B
0!B
/B
0!B
2-B
2-B
33B
49B
6FB
7LB
8RB
9XB
9XB
;dB
<jB
=qB
?}B
@�B
A�B
A�B
C�B
C�B
F�B
F�B
F�B
G�B
H�B
J�B
J�B
K�B
L�B
L�B
M�B
O�B
Q�B
Q�B
R�B
R�B
T�B
T�B
VB
W
B
XB
ZB
\)B
]/B
^5B
^5B
`BB
aHB
bNB
bNB
cTB
cTB
dZB
ffB
ffB
ffB
ffB
hsB
gmB
hsB
iyB
jB
k�B
k�B
k�B
m�B
m�B
n�B
o�B
o�B
p�B
q�B
s�B
t�B
s�B
t�B
u�B
u�B
w�B
x�B
w�B
x�B
y�B
z�B
{�B
{�B
|�B
|�B
}�B
}�B
~�B
~�B
�B
�B
�B
�B
�+B
�+B
�+B
�7B
�=B
�DB
�JB
�PB
�\B
�hB
�oB
�oB
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
�B
�B
�B
�B
�!B
�'B
�'B
�-B
�-B
�9B
�?B
�?B
�?B
�?B
�FB
�LB
�RB
�RB
�RB
�RB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�RB
�XB
�XB
�^B
�XB
�XB
�XB
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
`BB
_;B
_;B
aHB
bNB
_;B
`BB
_;B
`BB
]/B
^5B
aHB
_;B
_;B
_;B
_;B
`BB
`BB
_;B
^5B
`BB
`BB
`BB
_;B
`BB
_;B
`BB
`BB
`BB
_;B
`BB
`BB
_;B
`BB
_;B
`BB
_;B
_;B
_;B
_;B
`BB
_;B
_;B
_;B
^5B
`BB
`BB
_;B
`BB
_;B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
]/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               B
`B
_B
`B
`B
`B
_B
_B
_B
_B
`B
^B
^B
^B
^B
^B
_B
^B
^B
Y�B
Y�B
W�B
N�B
DB
CyB
L�B
M�B
G�B
F�B
Q�B
S�B
X�B
�$B
��B
��B
�/B
�xB
�B
�PB
�QB
�vB
�BBUB�B3!B<YBq�B�kB��B�B�=B�iB�iBʴB˺B˻B��B��B��B�B�&B�EB�eB��B �BBJB|B�B�B�B�B!�B#�B'�B,B1'BYB^6B\*BS�BR�BO�BC�BB�B@�B:cB6KB5EB23B.B$�BXB��B�vB�B��B��B�nB�DB�oB��B�B�*B�CB�CB�DB�BǾB��B�VB�JB�,B�B��B�cB�,B�B~	B{�Bw�Bs�B`YBH�B/2B�B=B
�B
��B
��B
�B
�B
�B
�KB
�B
��B
��B
­B
�[B
L�B
5^B
oB	�yB	��B	�fB	�|B	s�B	j�B	`cB	TB	I�B	A�B	3VB	+&B	"�B	�B	�B	�B	�B	�B	}B	�B	�B	�B	B	BB	5B�B��B�fB��B�B��B��B��B�9B�3B�4B�4B�;B�5B�/B�0B+B~%ByBv�BzBzBxBv�Bv�Bv�ByByBzBw BwBs�Bs�Bp�Bj�Bk�Bh�Be�Ba�Bd�Bc�Bc�Ba�B_yB\hB[bBZ]BXPBYXBWKBWLBU@BWMBVGBP#BR0BOBP$BMBLBH�BF�BF�BE�BB�BB�B@�B@�B>�B>�B>�B<�B<�B<�B8�B9�B9�B8�B?�BE�BG�BF�BC�B=�B8�B9�B;�B<�B=�B]�BqBx/BM/BViBzCB��B��B�B�B�BB�EB�`B��B�DB�lB��B��B�LB	�B	B	,B	#B	9B	3�B	C4B	N{B	`�B	s^B	x�B	�
B	�8B	�;B	�8B	�lB	�zB	��B	��B	��B	��B	�<B	�dB	ǀB	ͩB	��B	��B	�B	�HB	�vB	��B	��B	��B
�B
B
B
 B
)B
2B
NB
oB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
$B
&B
(B
("B
)+B
+:B
-IB
1eB
0bB
1kB
3yB
3|B
4�B
5�B
7�B
8�B
9�B
:�B
:�B
<�B
=�B
>�B
@�B
A�B
B�B
CB
EB
EB
H)B
H,B
H/B
I8B
JAB
LQB
LTB
M]B
NfB
NhB
OqB
Q�B
S�B
S�B
T�B
T�B
V�B
V�B
W�B
X�B
Y�B
[�B
]�B
^�B
_�B
_�B
b
B
cB
dB
dB
e'B
e)B
f2B
hAB
hCB
hFB
hIB
jXB
iUB
j^B
kfB
loB
mwB
mzB
m}B
o�B
o�B
p�B
q�B
q�B
r�B
s�B
u�B
v�B
u�B
v�B
w�B
w�B
y�B
z�B
y�B
z�B
|B
}
B
~B
~B
B
!B
�*B
�,B
�5B
�7B
�HB
�\B
�hB
�sB
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
�B
�B
�B
�%B
�8B
�=B
�PB
�UB
�iB
�nB
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
� B
�B
�B
�B
�B
�,B
�3B
�HB
�aB
�qB
��B
��B
��B
��B
��B
��B
�B
�B
�2B
�FB
�VB
�eB
�uB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�'B
�6B
�FB
�UB
�eB
�aB
�jB
�nB
�vB
�tB
�vB
�zB
�}B
�B
��B
�B
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
`B
_B
_B
a B
b&B
_B
`B
_B
`B
]B
^B
a!B
_B
_B
_B
_B
`B
`B
_B
^B
`B
`B
`B
_B
`B
_B
`B
`B
`B
_B
`B
`B
_B
`B
_B
`B
_B
_B
_B
_B
`B
_B
_B
_B
^B
`B
`B
_B
`B
_B
^B
^B
^B
^B
^B
^B
^B
_B
^B
]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202512021061413552520210614135525202106171312112021061713121120210617131211201807242202512021061413552520210614135525202106171312112021061713121120210617131211PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025120180724220251  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025120180724220251QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025120180724220251QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145720210617131457IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                