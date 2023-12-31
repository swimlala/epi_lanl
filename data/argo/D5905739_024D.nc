CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-01T17:03:39Z creation      
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
_FillValue                 �  _X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  cL   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                      HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                      HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                      HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � <Argo profile    3.1 1.2 19500101000000  20180801170339  20210617131500  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�s�OY�@�s�OY�11  @�s��C0@�s��C0@6�$�LD|@6�$�LD|�c�3:}��c�3:}�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @ff@Fff@�  @���@�  @���A��A33A$��A@  A^ffA�  A���A���A���A���A�  A�33A�  B ffBffBffB  B   B(ffB0ffB8ffB@��BHffBO��BX  B`ffBhffBp��BxffB�  B�33B�33B�  B�  B�33B�33B�33B�33B�33B�  B�33B�  B���B���B���B���B�  B���B�ffBЙ�B���B���B�  B�ffB�ffB�ffB왚B�  B�  B�  B�  C   C  C33C33C�C
  C�C  C  C�fC�C  C  C  C  C�fC 33C"33C$�C&33C(  C*L�C,ffC.�C033C2  C4L�C6L�C8�C:L�C<33C>�C@  CA��CD�CF33CHL�CJ33CK�fCN33CP  CQ��CT  CV�CX33CZ33C\L�C^�C_�fCb  Cd  Cf33Ch�Ci�fCl33Cn�Cp  Cr�CtL�Cv�Cw�fCz  C|33C~�C�fC�  C�&fC��C��3C��C��C��3C�  C�&fC��C��3C��C�  C��fC�  C��C�  C��fC��fC�  C��C�&fC��C��fC�  C��C��C�  C��fC��3C�  C��C�&fC�&fC��C��3C�  C��C��C�&fC�&fC�  C��fC��3C�  C��C��C�&fC��C�ٚC��3C��fC��3C��3C�  C�  C��C�&fC�  C�ٚC��fC��fC��fC��3C�  C��C�&fC��C��3C�  C�  C��C�&fC��C�  C��C��C�  C��fC�  C��C�33C��C�  C��C�&fC��C��3C��C�  C��fC�  C��C��C��C��3C��C�&fC�&fC��3C��C�  C��fC��C��3C��fC��3C��C�&fC��C��3C��C��C�&fC��C��3C��C��C�  C��fC�  C��fD@ DٚDy�D
  D�3Dl�D�D��DFfD��D��D9�D!� D$�fD',�D)� D,�fD/FfD2�D4�3D7l�D:�D<��D?�3DB9�DDٚDGy�DJ�DL�fDO33DQ��DT&fDV�fDY  D[�fD^�D`��DcfDe�fDhfDj�fDm�Do� Dr  Dt��DwFfDy� D|fD~��D���D���D�6fD��fD�ɚD��D�S3D��3D��3D�3D�S3D��fD�� D���D�&fD�\�D���D��3D�	�D�C3D�vfD��3D���D�)�D�c3D���D�ٚD�3D�FfD�s3D��3D��3D�fD�33D�l�D��3D��fD�fD�6fD�ffD��3D�ɚD�� D�3D�9�D�c3D���D���D�� D�fD�@ D�ffD���D���D���D��D� D�0 D�VfD�vfDș�DɶfD��3D��fD�3D�6fD�S3D�l�Dщ�Dҩ�D�ɚD�� D�3D�33D�VfD�vfDڣ3D�ɚD�� D�fD�6fD�ffD��D⹚D���D��D�I�D�|�D��D��fD� D�C3D�l�D�fD�� D���D�#3D�I�D�s3D���D��3D���D��D�)�D�6fD�VfD�y�D���D��3E d�E � E�fEfE�3E33E�3ES3E�fEt�E�E�fE3E�fE�3E	ٚE{3E�3E� EFfEQ�E^fE��E��E��E��E� E�E3E�3E� E$�E.fE � E!�3E"� E$3E%��E&�3E'ɚE)Y�E*\�E+�E,� E.` E/VfE0�fE2  E3�E4L�E5��E6�3E8( E9i�E:��E;��E>�EB0 EE@ EHnfEK�fEN��EQ�fET�3EX�EZ��E^#3Ea@ Ed� Egs3Ej��Em� Ep�3Et	�Ew0 EzFfE}S3E�T�E���E�]�E�� E��fE�fE���E���E�P E��3E�ݚE�$ E���E���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���?   ?333?L��?333?L��?fff?L��?fff?���?���?�33?�  ?ٙ�?�33@ff@��@   @,��@@  @L��@Y��@l��@�  @���@�ff@���@�ff@�ff@�  @���@�ff@�33@�ffA   A  A  AffA��A$��A,��A4��A<��AC33AK33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441441444414111411411411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?L��?�  @&ff@fff@�  @���@�  @���A	��A33A,��AH  AfffA�  A���A���A���A���A�  A�33A�  BffB
ffBffB  B"  B*ffB2ffB:ffBB��BJffBQ��BZ  BbffBjffBr��BzffB�  B�33B�33B�  B�  B�33B�33B�33B�33B�33B�  B�33B�  B���B���B���B���B�  B���B�ffBљ�B���B���B�  B�ffB�ffB�ffB홚B�  B�  B�  B�  C � C� C�3C�3C��C
� C��C� C� CffC��C� C� C� C� CffC �3C"�3C$��C&�3C(� C*��C,�fC.��C0�3C2� C4��C6��C8��C:��C<�3C>��C@� CBL�CD��CF�3CH��CJ�3CLffCN�3CP� CRL�CT� CV��CX�3CZ�3C\��C^��C`ffCb� Cd� Cf�3Ch��CjffCl�3Cn��Cp� Cr��Ct��Cv��CxffCz� C|�3C~��C�33C�@ C�ffC�L�C�33C�Y�C�L�C�33C�@ C�ffC�L�C�33C�Y�C�@ C�&fC�@ C�Y�C�@ C�&fC�&fC�@ C�L�C�ffC�L�C�&fC�@ C�Y�C�Y�C�@ C�&fC�33C�@ C�Y�C�ffC�ffC�Y�C�33C�@ C�L�C�L�C�ffC�ffC�@ C�&fC�33C�@ C�L�C�Y�C�ffC�L�C��C�33C�&fC�33C�33C�@ C�@ C�L�C�ffC�@ C��C�&fC�&fC�&fC�33C�@ C�Y�C�ffC�L�C�33C�@ C�@ C�Y�C�ffC�L�C�@ C�L�C�Y�C�@ C�&fC�@ C�L�C�s3C�L�C�@ C�L�C�ffC�Y�C�33C�Y�C�@ C�&fC�@ C�L�C�Y�C�L�C�33C�L�C�ffC�ffC�33C�Y�C�@ C�&fC�L�C�33C�&fC�33C�L�C�ffC�L�C�33C�L�C�Y�C�ffC�L�C�33C�L�C�Y�C�@ C�&fC�@ C�&fD` D��D��D
@ D�3D��D,�D��DffD�D��DY�D"  D$�fD'L�D*  D,�fD/ffD2,�D4�3D7��D:9�D<��D?�3DBY�DD��DG��DJ9�DL�fDOS3DQ��DTFfDV�fDY@ D[�fD^9�D`��Dc&fDe�fDh&fDj�fDm,�Do� Dr@ DtٚDwffDz  D|&fD~��D���D���D�FfD��fD�ٚD��D�c3D��3D��3D�#3D�c3D��fD�� D���D�6fD�l�D���D��3D��D�S3D��fD��3D���D�9�D�s3D���D��D�#3D�VfD��3D��3D��3D�fD�C3D�|�D��3D��fD�fD�FfD�vfD��3D�ٚD�  D�#3D�I�D�s3D���D�ɚD�� D�&fD�P D�vfD���D���D���D���D�  D�@ D�ffDǆfDȩ�D��fD��3D�fD�#3D�FfD�c3D�|�Dљ�Dҹ�D�ٚD�  D�#3D�C3D�ffDنfDڳ3D�ٚD�  D�&fD�FfD�vfD��D�ɚD���D�)�D�Y�D��D��D��fD�  D�S3D�|�D�fD�� D��D�33D�Y�D�3D���D��3D���D��D�9�D�FfD�ffD���D���D��3E l�E  E�fEfE�3E;3E�3E[3E�fE|�E	�E�fE#3E�fE�3E	�E�3E�3E� ENfEY�EffE��E�E�E��E� E�E3E�3E� E,�E6fE � E!�3E"� E$3E%��E&�3E'њE)a�E*d�E+�E,� E.h E/^fE0�fE2( E3	�E4T�E5��E6�3E80 E9q�E:��E;��E>�EB8 EEH EHvfEK�fEN��EQ�fET�3EX�EZ��E^+3EaH Ed� Eg{3Ej��En  Ep�3Et�Ew8 EzNfE}[3E�X�E���E�a�E�� E��fE�
fE���E���E�T E��3E��E�( E���E�ŚG�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�?L��G�O�G�O�G�O�G�O�?L��G�O�?fff?�  ?���G�O�?���?�ffG�O�?�ff?�33G�O�?ٙ�?�33@   @��@��@&ff@,��@@  @L��@`  @l��@y��@�ff@�  @���@�ff@���@�ff@�ff@�  @���@�ff@�33A33A  A  A  AffA$��A,��A4��A<��AD��AK33AS33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441441444414111411411411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               @ @ �@ V@ *@ O@ !s@ (�@ /@ 7L@ >�@ FQ@ Q�@ ^�@ m:@ |?@ ��@ ��@ ��@ �~@ �w@ ��@ �#@ ��@ �q@j@@g@-@:�@I@V@bN@p�@~�@��@��@��@��@�>@��@��@�@��@�@*@"�@0x@=q@K�@X�@e	@s_@�@��@�@��@��@�@��@��@��@��@�@B@'�@33@@�@N�@\)@i�@ww@�|@�#@�@�@�k@�c@�
@�@�@  @�@O@(�@5�@E�@SI@`B@n�@z�@��@��@��@��@�&@��@܀@��@��@�@�@�@+@:�@I@Wb@dZ@o�@�@��@�<@�A@��@��@є@��@�4@�~@�@{@#�@0x@<�@Lu@Yn@ff@t�@��@�@�U@��@��@ƨ@��@�H@��@�E@	�@B@&;@2�@@�@P�@\�@i!@x�@�@�h@�m@�r@��@�@խ@�@�@@V@�@(�@7�@E�@Q�@^5@l�@z�@��@�<@��@��@�w@��@�#@��@��@	v@	@	[@	+�@	:@	H]@	V�@	e	@	qS@	|?@	�D@	�<@	��@	�9@	@	�7@	ލ@	�@	�,@
�@
@
 �@
.l@
<�@
K@
Z@
hs@
t�@
�@
�\@
�@
�@
�^@
ƨ@
Ӡ@
��@
�L@
��@�@�@&;@5�@A�@N�@\�@k�@x�@�p@�$@�m@��@��@�@�h@�`@�@ �@�@[@(G@7�@DD@P�@`B@l�@y�@��@��@��@�-@�w@�|@��@�(@�q@�@�@ @,`@8�@G�@S�@�@+�@s_@��@v@Lu@�$@��@"�@l�@�9@��@DD@��@��@�@j@�-@��@FQ@��@��@#�@oF@��@�Q@F�@��@�O@�@]�@�@�`@(�@m�@��@�@7L@{�@��@@I@�\@Ӡ@�@`A@�A@��@*S@o�@��@��@E�@��@ψ@*@Yn@��@��@&;@g�@��@��@-�@o�@��@�q@ 8�@ {�@ �@! �@!C�@!�+@!�@"�@"P�@"�u@"��@#*@#V@#��@#�h@$�@$[z@$��@$�;@% @%`�@%��@%��@&$/@&b�@&�@&��@'g@'^�@'�@'��@( @(_�@(�a@(܀@)�@)X@)��@)Ӡ@*@*O�@*�P@*�o@+1@+D�@+�@+��@+��@,:�@,v�@,��@,��@-.l@-m:@-�Y@-��@.&�@.dZ@.��@.�T@/""@/`�@/�a@/�;@0
@0^5@0��@0��@1 �@1bN@1�(@1�@2%�@2g@2��@2�@3(�@3i!@3��@3�@4&;@4e	@4��@4�@5!s@5^5@5��@5��@6@6Q=@6��@6�@7�@7C�@7�@7�@7��@87�@8uk@8�9@8��@9-@9i!@9�4@9��@:Z@:��@;�@;��@<p�@=!s@=�u@>%@>��@?"�@?�i@@8�@@�A@AM$@A��@B`�@B�7@Cy�@C��@D��@E@E�@E��@F��@G#�@G��@HA�@H�!@IV@I��@Jff@Jψ@Ki!@L  @L`B@L�@My�@N1@N��@O�@O�z@P$/@QqS@R�t@T(�@U�p@V�l@X&;@Y�@Z�*@\0x@]k�@^�W@`O@a��@b��@d$/@e��@f��@hg@iww@jȴ@l�@m�d@n��@pC@qi�@r��@t@um�@u�Z@v�@vC�@v�@v��@w�@wB8G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�@ G�O�G�O�G�O�G�O�@ G�O�@ �@ j@ �G�O�@ �@ vG�O�@ v@ %G�O�@ 1@ 	�@ 
=@ �@ �@ V@ @ @ o@ {@ �@ 6@ B@ O@ [@  @ !s@ #�@ &�@ (�@ +�@ -�@ 0x@ 4�@ 6�@ :@ =q@ @,@ B�@ FQ@ I�@ M$@ P�@ SI@ V�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A��TA��HA��mA��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A�E�A�33A��yA�-A�VAš�A�K�A�hsA���A��A�G�A�ƨA�^5A�?}A�A���A�x�A�t�A��mA�C�A�  A��TA��RA��A�%A�v�A��A��;A��9A�A�E�A��TA�~�A�A���A�r�A��7A���A�E�A���A���A���A�ffA��A�A�9XA���A�"�A��A�oA�XA���A�XA��RA��A�&�A�x�A��!A�=qA�ĜA�oA���A�t�A�"�A�/A�;dA���A�9XA��A��;A��-A�33A�A�K�A���A���A��A���A�|�A�A��9A�=qA�33A�VA�ȴA�C�A�  A�
=A�(�A{dZAs"�Ap�`Ao��Al�+AjĜAhĜAg��Af�uAdv�AdI�AdQ�Ad�!Ab�!AbJAahsA^E�A]�hA]dZA]�A\�DAY��AW�AU��AT�AS
=AQƨAQ%APz�AO�wAMƨAM
=ALv�AJ�uAH  AGC�AF��AD�9ABZA@ĜA?�A?\)A=�A<(�A;�PA:�RA:jA9C�A7�-A4�uA3�mA3hsA2�A25?A1S�A0�A/&�A-�A,ĜA,=qA+oA)��A)�A(��A(bNA'�A'hsA'&�A%�^A$�jA#XA"�/A"bA!G�A�;A?}AA{A-A�;A��A�wA�A33A��AI�AA�-A33A�9AJA;dAƨA�`A{AJA��A|�A��A
JA	K�A�+A�A=qA{A��A�`Ar�A5?A�Ao@��@��-@�x�@���@�dZ@�~�@��@���@�o@�x�@�1@��y@�ff@�@���@��@��#@�ƨ@�x�@��@��@��@�@�\)@�@߅@�~�@�%@�Z@׾w@׶F@պ^@���@�ƨ@�/@���@þw@�-@��T@��@��F@��#@��T@�o@�ȴ@��@��^@� �@��!@��@��!@�?}@� �@��+@�@�r�@���@�V@�1'@�I�@��@���@��m@�@��@���@��m@�+@�-@��D@���@�t�@��-@��@��D@�;d@���@���@��D@~��@}�@y�7@v5?@t�@q��@q�@p1'@o
=@l�@kdZ@j��@g�;@g|�@fȴ@eO�@dI�@co@a��@_�@_�w@]p�@\z�@[33@Zn�@Xr�@W�@U�-@S�F@Q�^@Nȴ@L��@K�F@K@I&�@H�u@Fȴ@E@D��@C��@Ct�@CC�@A��@@��@?�@>$�@=V@;�m@:n�@9G�@8A�@7l�@6V@4j@3�m@2�!@1%@0�`@0Ĝ@/\)@/+@.ȴ@-�-@,��@,�@*��@)��@(��@( �@'\)@&��@&@%?}@$�@#"�@"�@!hs@ �u@�w@K�@�-@�@ƨ@o@�@�@Q�@�;@
=@5?@`B@�/@Z@�F@�H@�@��@Ĝ@l�@
=@E�@�@?}@O�@z�@�m@��@"�@
=q@	�@	G�@bN@��@�@E�@�h@�@��@o@��@^5@M�@ Ĝ?���?��?�ȴ?���?�G�?���??�1?�dZ?���?�ff?�?}?���?��?�hs?�Ĝ?�v�?�/?�ƨ?ڟ�?���?�1'?׍P?�ȴ?�`B?�z�?�33?�J?ѩ�?У�?�  ?ϝ�?��?�V?�p�?�V?�dZ?���?�=q?���?�Q�?�+?�?��
?°!?��7?�A�?�;d?�5??��-?��?�1?���?�?���?���?��^?��^?��#?���?�=q?�=q?���?��H?�C�?�ƨ?�j?���?�p�?��h?���?��?��?�{?�{?�5?A���A���A���A���A���A���A���A���A���A��
A���A��
A���A��/A��;A��;A��TA��`A��mA��mA��`A��mA��TA��TA��/A��;A��;A��;A��HA��TA��HA��TA��HA��HA��`A��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               A��
A��TA��HA��mA��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A�E�A�33A��yA�-A�VAš�A�K�A�hsA���A��A�G�A�ƨA�^5A�?}A�A���A�x�A�t�A��mA�C�A�  A��TA��RA��A�%A�v�A��A��;A��9A�A�E�A��TA�~�A�A���A�r�A��7A���A�E�A���A���A���A�ffA��A�A�9XA���A�"�A��A�oA�XA���A�XA��RA��A�&�A�x�A��!A�=qA�ĜA�oA���A�t�A�"�A�/A�;dA���A�9XA��A��;A��-A�33A�A�K�A���A���A��A���A�|�A�A��9A�=qA�33A�VA�ȴA�C�A�  A�
=A�(�A{dZAs"�Ap�`Ao��Al�+AjĜAhĜAg��Af�uAdv�AdI�AdQ�Ad�!Ab�!AbJAahsA^E�A]�hA]dZA]�A\�DAY��AW�AU��AT�AS
=AQƨAQ%APz�AO�wAMƨAM
=ALv�AJ�uAH  AGC�AF��AD�9ABZA@ĜA?�A?\)A=�A<(�A;�PA:�RA:jA9C�A7�-A4�uA3�mA3hsA2�A25?A1S�A0�A/&�A-�A,ĜA,=qA+oA)��A)�A(��A(bNA'�A'hsA'&�A%�^A$�jA#XA"�/A"bA!G�A�;A?}AA{A-A�;A��A�wA�A33A��AI�AA�-A33A�9AJA;dAƨA�`A{AJA��A|�A��A
JA	K�A�+A�A=qA{A��A�`Ar�A5?A�Ao@��@��-@�x�@���@�dZ@�~�@��@���@�o@�x�@�1@��y@�ff@�@���@��@��#@�ƨ@�x�@��@��@��@�@�\)@�@߅@�~�@�%@�Z@׾w@׶F@պ^@���@�ƨ@�/@���@þw@�-@��T@��@��F@��#@��T@�o@�ȴ@��@��^@� �@��!@��@��!@�?}@� �@��+@�@�r�@���@�V@�1'@�I�@��@���@��m@�@��@���@��m@�+@�-@��D@���@�t�@��-@��@��D@�;d@���@���@��D@~��@}�@y�7@v5?@t�@q��@q�@p1'@o
=@l�@kdZ@j��@g�;@g|�@fȴ@eO�@dI�@co@a��@_�@_�w@]p�@\z�@[33@Zn�@Xr�@W�@U�-@S�F@Q�^@Nȴ@L��@K�F@K@I&�@H�u@Fȴ@E@D��@C��@Ct�@CC�@A��@@��@?�@>$�@=V@;�m@:n�@9G�@8A�@7l�@6V@4j@3�m@2�!@1%@0�`@0Ĝ@/\)@/+@.ȴ@-�-@,��@,�@*��@)��@(��@( �@'\)@&��@&@%?}@$�@#"�@"�@!hs@ �u@�w@K�@�-@�@ƨ@o@�@�@Q�@�;@
=@5?@`B@�/@Z@�F@�H@�@��@Ĝ@l�@
=@E�@�@?}@O�@z�@�m@��@"�@
=q@	�@	G�@bN@��@�@E�@�h@�@��@o@��@^5@M�@ Ĝ?���?��?�ȴ?���?�G�?���??�1?�dZ?���?�ff?�?}?���?��?�hs?�Ĝ?�v�?�/?�ƨ?ڟ�?���?�1'?׍P?�ȴ?�`B?�z�?�33?�J?ѩ�?У�?�  ?ϝ�?��?�V?�p�?�V?�dZ?���?�=q?���?�Q�?�+?�?��
?°!?��7?�A�?�;d?�5??��-?��?�1?���?�?���?���?��^?��^?��#?���?�=q?�=q?���?��H?�C�?�ƨ?�j?���?�p�?��h?���?��?��?�{?�{?�5?A���A���A���A���A���A���A���A���A���A��
A���A��
A���A��/A��;A��;A��TA��`A��mA��mA��`A��mA��TA��TA��/A��;A��;A��;A��HA��TA��HA��TA��HA��HA��`A��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B$�B$�B$�B%�B$�B%�B%�B$�B$�B$�B$�B$�B$�B$�B$�B%�B$�B%�B%�B$�B$�B$�B$�B$�B$�B$�B&�BbNB�#B!�B-B"�B2-BdZBl�BjBbNBB�B49B6FBD�BI�BO�BhsB�B�=B�bB�hB�bB�\B�PB�bB�hB�hB�bB�\B�%By�Bv�Bs�Bq�Bs�Bu�Bt�Bs�Bo�Bm�Bk�Bn�Br�Bw�Bw�By�B�B�B�B|�B}�B|�B~�B{�Bw�Bo�BZBW
BM�BF�B<jB)�B�BB��B�mB�B��B�B��B��B�uBgmB33B5?B(�BuB
��B
�TB
�B
��B
��B
��B
ŢB
�'B
��B
�B
cTB
)�B
B	��B	ÖB	�!B	��B	�hB	�JB	�%B	� B	|�B	�=B	�JB	�uB	{�B	�1B	�B	q�B	p�B	m�B	jB	q�B	hsB	\)B	N�B	F�B	:^B	33B	/B	33B	-B	�B	uB	VB	B��B��B��B�B�NB�B�B��BÖB�wB�dB�^B�?B�'B��B��B��B��B��B��B�oB�\B�PB�B�B�B}�Bz�Bx�Bx�Bv�Bt�Bs�Bp�Bm�BiyBgmBffBcTBaHB[#BZBYBR�BN�BL�BI�BH�BE�B@�B@�BA�BA�B@�B?}B=qB<jB<jB8RB6FB7LB6FB5?B5?B33B1'B2-B-B.B-B,B+B)�B+B&�B(�B(�B'�B)�B'�B%�B%�B$�B)�B(�B$�B&�B&�B'�B'�B&�B'�B%�B(�B(�B-B,B'�B'�B'�B1'B0!B49B33B:^B>wBe`B�B�PB��B��B�dBBƨB��B�dB�B�B��B	DB	�B	"�B	�B	)�B	2-B	=qB	I�B	T�B	\)B	iyB	n�B	r�B	|�B	�B	�{B	��B	�!B	�B	�B	�'B	�^B	��B	ƨB	ɺB	��B	�B	�B	�5B	�5B	�HB	�fB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B

=B
JB
JB
VB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
&�B
&�B
'�B
)�B
,B
0!B
1'B
2-B
33B
33B
49B
7LB
8RB
8RB
:^B
;dB
:^B
<jB
=qB
?}B
@�B
A�B
B�B
C�B
D�B
E�B
F�B
F�B
I�B
I�B
J�B
L�B
L�B
K�B
N�B
N�B
O�B
O�B
P�B
P�B
R�B
S�B
T�B
T�B
VB
W
B
XB
YB
YB
[#B
[#B
\)B
]/B
_;B
^5B
aHB
aHB
bNB
cTB
e`B
dZB
ffB
ffB
hsB
hsB
jB
iyB
jB
k�B
l�B
m�B
m�B
m�B
p�B
p�B
p�B
r�B
r�B
q�B
s�B
s�B
t�B
t�B
u�B
v�B
v�B
x�B
x�B
y�B
y�B
z�B
|�B
}�B
}�B
}�B
~�B
~�B
�B
�B
�B
�B
�B
�1B
�1B
�1B
�=B
�=B
�JB
�VB
�PB
�\B
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
�B
�B
�B
�!B
�'B
�'B
�-B
�3B
�9B
�?B
�?B
�FB
�LB
�LB
�LB
�RB
�LB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B$�B$�B$�B$�B%�B$�B%�B$�B$�B$�B$�B%�B&�B$�B%�B$�B$�B$�B$�B$�B$�B$�B$�B#�B$�B%�B$�B$�B%�B$�B$�B$�B$�B%�B$�B%�B%�B$�B$�B$�B%�B%�B$�B%�B%�B$�B$�B$�B$�B$�B$�B$�B$�B$�B%�B$�B$�B$�B$�B$�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               B$�B$�B$�B%�B$�B%�B%�B$�B$�B$�B$�B$�B$�B$�B$�B%�B$�B%�B%�B$�B$�B$�B$�B$�B$�B$�B&�Bb3B�	B!�B,�B"�B2BdABlsBjgBb7BBxB4#B60BD�BI�BO�Bh_B��B�*B�PB�VB�QB�KB�@B�RB�YB�ZB�TB�OB�By�Bv�Bs�Bq�Bs�Bu�Bt�Bs�Bo�Bm�Bk~Bn�Br�Bw�Bw�By�B�B�	B�
B|�B}�B|�B~�B{�Bw�Bo�BZBWBM�BF�B<nB* B�BB��B�sB�B��B�$B��B��B�~BgvB3<B5IB) B�B
��B
�_B
�)B
��B
��B
��B
ŰB
�5B
��B
�(B
cdB
*B
"B	��B	æB	�1B	��B	�yB	�[B	�7B	�B	}B	�PB	�^B	��B	{�B	�GB	�(B	q�B	p�B	m�B	j�B	q�B	h�B	\BB	N�B	F�B	:yB	3NB	/7B	3OB	-+B	�B	�B	tB	+B��B��B��B��B�oB�8B�&B�BùB��B��B��B�dB�LB�B��B��B��B��B��B��B��B�zB�IB�JB�7B~B{ByByBv�Bt�Bs�Bp�Bm�Bi�Bg�Bf�Bc�BazB[VBZPBYKBS&BOBMBI�BH�BE�B@�B@�BA�BA�B@�B?�B=�B<�B<�B8�B6�B7�B6�B5}B5}B3rB1fB2lB-NB.TB-OB,JB+DB*?B+EB'-B):B):B(5B*AB(6B&*B&*B%%B*DB)?B%&B'3B'3B(;B(;B'5B(<B&0B)CB)DB-\B,WB(?B(@B(@B1xB0rB4�B3�B:�B>�Be�B�cB��B�B�CB��B��B�B�?B��B�|B�B�lB	�B	B	#XB	<B	*�B	2�B	>B	JQB	U�B	\�B	jB	o;B	sVB	}�B	��B	�+B	�EB	��B	��B	��B	��B	�B	�GB	�oB	ʄB	ӿB	��B	��B	�B	�B	�$B	�EB	�aB	�}B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
CB
SB
VB
eB
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
$B
%B
($B
(&B
)0B
+?B
-NB
1jB
2sB
3{B
4�B
4�B
5�B
8�B
9�B
9�B
;�B
<�B
;�B
=�B
>�B
@�B
A�B
B�B
DB
EB
FB
G#B
H,B
H.B
KCB
KFB
LPB
N^B
NaB
M^B
PrB
PuB
Q~B
Q�B
R�B
R�B
T�B
U�B
V�B
V�B
W�B
X�B
Y�B
Z�B
Z�B
\�B
\�B
]�B
^�B
aB
`B
cB
cB
d&B
e/B
g>B
f;B
hJB
hMB
j\B
j_B
lnB
kkB
ltB
m}B
n�B
o�B
o�B
o�B
r�B
r�B
r�B
t�B
t�B
s�B
u�B
u�B
v�B
v�B
w�B
x�B
x�B
z�B
{B
|	B
|B
}B
%B
�-B
�0B
�3B
�;B
�>B
�OB
�ZB
�hB
�{B
�zB
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
�B
�&B
�1B
�?B
�JB
�UB
�ZB
�hB
�uB
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
�B
�B
�%B
�*B
�@B
�]B
�qB
��B
��B
��B
��B
��B
��B
�B
�B
�1B
�GB
�VB
�mB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�'B
�<B
�EB
�HB
�LB
�OB
�QB
�TB
�^B
�`B$�B$�B$�B$�B%�B$�B%�B$�B$�B$�B$�B%�B&�B$�B%�B$�B$�B$�B$�B$�B$�B$�B$�B#�B$�B%�B$�B$�B%�B$�B$�B$�B$�B%�B$�B%�B%�B$�B$�B$�B%�B%�B$�B%�B%�B$�B$�B$�B$�B$�B$�B$�B$�B$�B%�B$�B$�B$�B$�B$�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808011703392021061413553020210614135530202106171312292021061713122920210617131229201808011703392021061413553020210614135530202106171312292021061713122920210617131229PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018080117033920180801170339  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018080117033920180801170339QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018080117033920180801170339QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150020210617131500IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                