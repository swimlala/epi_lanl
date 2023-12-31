CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:03:04Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   |   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    @   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        `   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180724220304  20210722161420  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�o�[��@�o�[��11  @�o��K�@�o��K�@*jOv`@*jOv`�cJ�~($�cJ�~($11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  ?�  @   @Fff@�ff@�33@�33@�33A��A  A$��AC33A^ffA���A���A�  A�  A���Aљ�A���A���A�33B��BffBffB   B(ffB0  B7��B@  BHffBPffBX��B`ffBg��BpffBx��B�ffB�  B�33B�33B���B�  B�  B���B���B�  B���B���B�  B�ffB�33B�33B�33B�  B�33B�33B�33B�33B�33B�  B�  B�  B�  B�  B�  B���B���B�ffC   C��C�CffC�C	��C  C  C�fC  C  C�C  C  C  C  C   C"�C$  C&  C'�fC*33C,33C.�C0  C2�C4  C5�fC8�C:�C<�C>  C@  CB  CD  CF  CH  CI�fCK�fCN  CP  CR  CT  CU�fCW�fCY�fC[�fC]�fC_��Cb33Cd33Cf  Ch  Ci��Cl�Cn  Co�fCr�Ct33Cv  Cx33Cz�C{�fC~�C�&fC��C��C��C��C�  C��C�&fC��C��3C��C�  C��fC�  C��C�  C��fC��3C��C�&fC��C�  C�  C��fC��C�  C�  C��C��C��C��C��C�  C��3C��C��C�  C�  C��fC��C��C��3C��C��C�  C�  C�  C��3C��3C��fC��fC��C��C�  C�&fC��C��C��C�  C��3C�  C��C��C�  C�  C��fC��C��C��C��C��C��C��C�  C��3C��3C��fC��fC��fC��C��C��C��C��C�  C��3C�  C��3C��3C�  C�  C��3C��3C��3C�  C��fC�  C��3C�  C�  C�  C��3C��C��C��C�  C��C��C�  C�&fC�&fC��C��C�  C��C��C�  C�  C�  C�  C��3C��3C��fC�ٚC��C�33C��3C��D �D y�D�D��D��D� D3D
��D` D��D�3D,�D� DL�D��DS3D!�fD$y�D'  D)��D,@ D.��D1` D43D6� D9� D<33D?  DA��DDffDG33DI��DL�fDO� DRl�DUS3DX&fDZ��D]� D`y�Dc,�De�3Dh�fDk33Dm� Dp� Ds�Du�3Dx9�Dz��D|ٚDs3D�	�D�P D��fD�� D�33D��fD��3D�  D�s3D�ɚD�  D��fD��D�I�D��fD�  D�` D��fD�fD�s3D���D�#3D�p D�� D�3D�\�D��fD���D�9�D�|�D��3D�	�D�L�D��3D��fD�33D���D�ٚD�  D�l�D�� D� D�\�D��3D�	�D�\�D��3D���D�C3D���D�ٚD�#3D�y�DƼ�D�3D�L�Dʙ�D��fD�&fD�i�D϶fD��3D�)�D�\�DԐ D��3D���D�)�D�c3Dڌ�Dۼ�D��fD�fD�<�D�\�D�3D�fD�ٚD�	�D�)�D�L�D�l�D��D�� D���D�3D�<�D�i�D�D�� D���D�6fD�i�D��fD��3D��3D�  D�33D�ffD�� D��3E �E ��E6fE�fEh E�fE��E,�E� E` E�fE��E#3E� EQ�E�E	~fE
�E
�3EI�E��Eq�E E�fE�fE��E( ES3E{3E1�EY�E|�E� E� Ep E� E�3E��E ` E!q�E"�fE$( E%9�E&I�E'�3E(� E*�E+�3E,ɚE-��E.�3E0��E1��E2�3E4~fE5�3E6�3E7��E9{3E:�fE;��E<�fE@	�ECK3EF�fEI,�ELP EO� ER� EU� EXњE\�E_NfEb` Eek3Eh|�Ek�3Eo3Er,�Eu33ExC3E{>fE~�fE��E�BfE�� E�`�E��E���E�3E�� E�@�E�ŚE�RfE�fE�� E�fE�� E�G3E�� E�VfE���E�k3E�3E���E� �E���E�8�E��fE�O3E�� E��fE�ZfE�� E���E�?3E���E���E�!�E���E�� E�!�E�` E���?L��?L��?L��?L��?L��?333?L��?fff?L��?L��?L��?L��?L��?L��?L��?�  ?fff?fff?�  ?���?���?�ff?�ff?�ff?�33?�  ?ٙ�?ٙ�?�33@ff@33@   @&ff@9��@Fff@S33@`  @y��@�ff@�  @���@�33@���@���@�33@���@�ff@�33@�  @���A��A33A��A33A!��A(  A0  A8  A@  AFffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441144444441441111441114111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?�  @   @fff@�ff@�33@�33@�33A	��A  A,��AK33AfffA���A���A�  A�  A���Aՙ�A���A���B��B	��BffBffB"  B*ffB2  B9��BB  BJffBRffBZ��BbffBi��BrffBz��B�ffB�  B�33B�33B���B�  B�  B���B���B�  B���B���B�  B�ffB�33B�33B�33B�  B�33B�33B�33B�33B�33B�  B�  B�  B�  B�  B�  B���B���B�ffC � CL�C��C�fC��C
L�C� C� CffC� C� C��C� C� C� C� C � C"��C$� C&� C(ffC*�3C,�3C.��C0� C2��C4� C6ffC8��C:��C<��C>� C@� CB� CD� CF� CH� CJffCLffCN� CP� CR� CT� CVffCXffCZffC\ffC^ffC`L�Cb�3Cd�3Cf� Ch� CjL�Cl��Cn� CpffCr��Ct�3Cv� Cx�3Cz��C|ffC~��C�ffC�Y�C�L�C�Y�C�L�C�@ C�L�C�ffC�L�C�33C�L�C�@ C�&fC�@ C�Y�C�@ C�&fC�33C�Y�C�ffC�Y�C�@ C�@ C�&fC�L�C�@ C�@ C�Y�C�Y�C�L�C�L�C�L�C�@ C�33C�Y�C�L�C�@ C�@ C�&fC�L�C�L�C�33C�Y�C�Y�C�@ C�@ C�@ C�33C�33C�&fC�&fC�Y�C�Y�C�@ C�ffC�Y�C�L�C�L�C�@ C�33C�@ C�Y�C�L�C�@ C�@ C�&fC�Y�C�Y�C�L�C�Y�C�L�C�L�C�L�C�@ C�33C�33C�&fC�&fC�&fC�L�C�L�C�L�C�L�C�L�C�@ C�33C�@ C�33C�33C�@ C�@ C�33C�33C�33C�@ C�&fC�@ C�33C�@ C�@ C�@ C�33C�Y�C�Y�C�L�C�@ C�L�C�L�C�@ C�ffC�ffC�L�C�L�C�@ C�L�C�L�C�@ C�@ C�@ C�@ C�33C�33C�&fC��C�L�C�s3C�33C�Y�D 9�D ��D,�D��D�D� D33D
ٚD� D�D�3DL�D� Dl�D��Ds3D"fD$��D'  D)��D,` D.��D1� D433D6� D9� D<S3D?  DAٚDD�fDGS3DJ�DL�fDO� DR��DUs3DXFfD[�D]� D`��DcL�De�3Dh�fDkS3Dn  Dp� Ds9�Du�3DxY�Dz��D|��D�3D��D�` D��fD�� D�C3D��fD��3D�0 D��3D�ٚD�0 D��fD���D�Y�D��fD� D�p D��fD�&fD��3D���D�33D�� D�� D�#3D�l�D��fD���D�I�D���D��3D��D�\�D��3D��fD�C3D���D��D�0 D�|�D�� D�  D�l�D��3D��D�l�D��3D�	�D�S3D���D��D�33Dŉ�D���D�3D�\�Dʩ�D��fD�6fD�y�D��fD�3D�9�D�l�DԠ D��3D�	�D�9�D�s3Dڜ�D���D��fD�&fD�L�D�l�D�3D��fD��D��D�9�D�\�D�|�D��D�� D���D�#3D�L�D�y�D�D�� D�	�D�FfD�y�D��fD��3D�3D�0 D�C3D�vfD�� D��3E 	�E ��E>fE�fEp EfE��E4�E� Eh E�fE��E+3E� EY�E�E	�fE
�E
�3EQ�E��Ey�E E�fE�fE�E0 E[3E�3E9�Ea�E��E� E� Ex E� E�3E��E h E!y�E"�fE$0 E%A�E&Q�E'�3E)  E*�E+�3E,њE-��E.�3E0��E1��E2�3E4�fE5�3E6�3E7��E9�3E:�fE;��E<�fE@�ECS3EF�fEI4�ELX EO� ER� EU� EXٚE\$�E_VfEbh Ees3Eh��Ek�3Eo3Er4�Eu;3ExK3E{FfE~�fE��E�FfE�� E�d�E���E���E�3E�� E�D�E�ɚE�VfE�fE�� E�fE�� E�K3E�� E�ZfE���E�o3E�3E���E�$�E���E�<�E��fE�S3E�� E��fE�^fE�� E���E�C3E���E���E�%�E���E�� E�%�E�d E���G�O�G�O�G�O�G�O�G�O�?���?�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�?�ffG�O�G�O�?�33?�  ?���?ٙ�G�O�G�O�?�ff?�33@   G�O�@��@��@&ff@333@@  @Fff@Y��@fff@s33@�  @���@�ff@�  @���@�33@���@ə�@�33@���@�ff@�33A   AffA��A33A��A#33A)��A0  A8  A@  AH  ANffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441144444441441111441114111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @ �@ �@ *@ �@ "�@ )�@ 0x@ 7L@ =q@ FQ@ SI@ ^�@ m�@ {�@ ��@ �0@ ��@ ��@ ��@ �|@ ��@ �m@ �q@@@g@,`@9X@G�@V@c�@r@~�@�D@�H@��@�F@@��@ލ@��@�,@�@�@!s@/�@<�@Ji@X�@g�@t�@�d@�@�@��@�@ƨ@��@ލ@��@  @
=@�@(�@6�@C�@P�@]�@i�@v@��@��@�@��@��@�c@�[@�@�Y@ �@�@O@(�@6�@DD@R�@_�@m:@z3@��@��@��@�~@��@��@��@��@�q@@@�@,`@:@G�@UU@bN@o�@~K@��@��@�A@�9@��@ψ@�/@��@��@1@�@""@/�@<@K�@X�@e�@t�@�@�\@�a@�Y@��@ƨ@խ@�@�@��@
�@�@&;@5?@A�@M�@\�@i�@v@�@�$@�m@��@�@��@�@�@�Y@  @J@�@(�@6�@E�@SI@`B@m�@{�@��@��@�5@�-@�&@��@�@��@�q@	�@	o@	 @	,`@	:@	G�@	T�@	bN@	oF@	|�@	�P@	��@	�A@	��@	��@	��@	ލ@	�@	�~@
�@
�@
"�@
/�@
=q@
I�@
Z@
g�@
t�@
�@
�@
��@
�Y@
�R@
�J@
��@
��@
�@
�9@
�@�@&;@3�@A�@N�@[z@i�@v�@�p@��@�m@�f@�@ȴ@�
@�T@�Y@�Q@�@O@(�@5�@E�@SI@`B@m:@{�@�7@�0@��@��@��@�|@�t@��@�q@j@@�@,`@9X@F�@S�@`�@qS@�@�D@��@��@�9@��@��@��@�@��@�@{@[z@�y@�y@/�@uk@��@��@D�@�D@�7@B@_�@�5@�@5?@~K@�c@@_�@��@�@?}@��@׹@%�@r@�2@V@Z@�A@�@;d@��@�|@�@_�@�A@�@@5?@z3@��@�~@?}@�+@��@o@X�@�@�y@0x@ww@��@�@Q�@�a@�(@5@@�@�c@ {@ ]�@ ��@ �@!<�@!��@!��@"{@"\�@"�(@"�y@#/@#v@#�@$ �@$FQ@$�D@$��@%B@%`A@%�M@%��@&6�@&}�@&��@'�@'T�@'��@'�@(/@(t�@(��@)@)Ji@)�i@)׹@* �@*e�@*�Y@*�@+8�@+�@+��@,�@,O�@,�u@,խ@-6@-X�@-�H@-܀@.[@.`A@.��@.��@/ @/`�@/��@/�/@0�@0]�@0��@0܀@1�@1X@1��@1��@2{@2T�@2�u@2��@3@3S�@3�0@3խ@4B@4Z�@4��@4�#@5�@5\)@5��@5�h@6O@6\�@6��@6��@7!s@7bN@7��@7�@8&;@8e	@8�A@8�@9(G@9hs@9��@9�@:)�@:j@:��@:�(@;*S@;m�@;��@;�4@<,`@<l�@<�@@=oF@=��@>n�@>��@?��@@&;@@�z@A�@A��@BQ�@B��@CB8@C�^@Dm:@D��@EX@F
=@F~�@F�@G��@H�@H��@IB8@I�@J.l@J�4@KZ�@K�O@LLu@M�@M|?@M�@Nk�@O#�@O�T@P@P�C@Q�@SSI@T�9@Uխ@W,`@X��@Z  @[DD@\�d@]�(@_F�@`�0@a�@c1�@d�@e��@gS�@h�a@i��@k2�@l�T@m��@o'�@p��@q��@s&�@t�$@uψ@w,`@x�7@y��@{'�@|�<@}�H@1�@�B�@���@���@�F�@��F@���@�J�@��4@���@�F�@���@��	@�=@�h@��p@��@��@��@��@�,�@�U�@�qS@��I@���@�ލ@��,@��G�O�G�O�G�O�G�O�G�O�@ �@ vG�O�G�O�G�O�G�O�G�O�G�O�G�O�@ vG�O�G�O�@ %@ �@ �@ 1G�O�G�O�@ �@ 	�@ 
=G�O�@ �@ �@ V@ �@ @ �@ �@ *@ �@ �@ �@ �@ �@  �@ "�@ $�@ '�@ )�@ +�@ -�@ 0x@ 33@ 5�@ 8�@ ;d@ ?}@ B8@ D�@ G�@ K@ N�@ Q�@ T�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�&�A�"�A�$�A�-A�/A�1'A�/A�-A�-A�-A�+A�+A�&�A�(�A� �A�  A���A�~�A�;dA���A��#A�K�A�ZA�  A���Aԣ�A�`BA�%A�O�A�-A�VA�-A��A�{A�VA�t�A���A��#A�E�A���A��/A�;dA�+A��A�oA�+A�VA��A��A�^5A��A�VA�A�ĜA���A�?}A�A��jA�z�A���A�VA�+A��wA�Q�A��uA���A���A�ȴA��A��TA�oAy�7Ao��AkO�Ae��AaAXE�AT5?AM�AHE�AB��A@~�A>�A=`BA;��A:-A8jA7��A7K�A6�/A6��A6�+A65?A6bA6A5��A5�mA5�TA5�TA5�;A5�#A5�
A5��A5�^A5�-A5�A5��A5��A5��A5��A5�hA5�7A5�A5l�A5S�A5/A4��A4�`A4�/A4��A4ĜA4�!A4ffA41'A3��A3hsA3�A2��A2E�A2  A1�wA1�A1oA0=qA/�-A.��A-t�A,ZA+?}A*$�A(v�A'�A%��A$��A#G�A!��A!�A"5?A"�A#"�A#G�A#C�A"9XA!hsA ��A �9A ��A ~�A n�A -A�hA��AƨAl�A��A��A�;A�A��A��A��A��A�-A�^A�#A�#A��A��A�hAO�A+A��A�uA �A��AM�A��AXA��An�A  A��AjAAI�A��AO�AoA�/A
=A�HA��A"�A�/A��A��A��A=qA�;AC�A
�A
v�A
ZA
�A	�A�A�9A5?A�^A�7A��At�A�+A9XA�
AO�A��A�A�FA;dAO�AA��A�\A�
A��Ap�A ��A ��A ��A �A V@��@�t�@�=q@��-@���@�z�@�I�@�ƨ@��@�;d@�@�O�@�9X@���@�;d@�ȴ@�^5@��7@���@���@��@��@�7L@�%@���@�O�@�b@���@�b@�Ĝ@�\)@�t�@ϝ�@�p�@��m@�&�@��@�dZ@���@�K�@��@��y@��m@���@�ƨ@���@�Q�@��@���@�1@���@�@��y@��!@��u@���@���@�bN@���@� �@���@�@��@�+@���@���@��H@���@���@�5?@�&�@�bN@��P@��@�hs@���@�dZ@���@�(�@}��@{@y��@w��@u�@sdZ@r=q@qhs@o��@m�T@lj@i��@f�R@e/@d1@b�H@ahs@`��@_l�@\z�@Z�@Y7L@Wl�@U�-@TI�@R�\@Q�#@O�w@M�-@L1@J~�@HbN@F�+@E��@D��@C@AG�@?�@>�+@=`B@<z�@:��@9G�@8  @7\)@6ȴ@6@4�@3�@2n�@0��@.ȴ@-�@,�D@*^5@)�#@(bN@'|�@%��@$1@"��@"n�@!��@ r�@K�@�T@?}@Z@dZ@��@�@%@�u@�P@�@v�@�@�@�m@@^5@X@bN@��@ff@�-@��@�@C�@33@
=q@	�7@bN@ �@\)@�@��@�T@�@Z@�F@"�@=q@�@x�@%@ Ĝ@ A�?���?�V?��?���?�V?�1?�?�^5?��?�
=?���?���?�F?�-?�hs?��;?��?�{?���?��m?�dZ?�^5?��?��?�K�?�E�?��?䛦?�t�?�!?�J?�%?�bN?� �?�\)?�V?�p�?��?�1?�C�?ڟ�?�=q?�X?���?�b?�l�?�E�?���?�S�?�J?�A�?�\)?��?���?�ƨ?��#?�r�?�ȴ?�`B?�S�?��?���?�\)?�v�?��?�V?��m?��H?��?�X?�r�?��P?���?��T?�`B?��j?���?��F?���?��!?��?�J?�J?��?�M�?��!?�33?��F?�9X?��/?���?�ff?�+?��?�1'?�Q�?�r�?��u?��9?���?��?�X?�x�?���?��^?��#?��?�=qA�"�A�$�A�"�A�"�A�$�A�&�A�(�A�+A�+A�+A�+A�-A�+A�-A�$�A�$�A�(�A�&�A�(�A�&�A�&�A�&�A�"�A�$�A�$�A�$�A�"�A� �A��A� �A� �A�&�A�$�A�&�A� �A�$�A�+A�+A�-A�-A�-A�/A�/A�1'A�1'A�/A�/A�/A�-A�+A�-A�-A�-A�-A�+A�-A�-A�+A�-A�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A�&�A�"�A�$�A�-A�/A�1'A�/A�-A�-A�-A�+A�+A�&�A�(�A� �A�  A���A�~�A�;dA���A��#A�K�A�ZA�  A���Aԣ�A�`BA�%A�O�A�-A�VA�-A��A�{A�VA�t�A���A��#A�E�A���A��/A�;dA�+A��A�oA�+A�VA��A��A�^5A��A�VA�A�ĜA���A�?}A�A��jA�z�A���A�VA�+A��wA�Q�A��uA���A���A�ȴA��A��TA�oAy�7Ao��AkO�Ae��AaAXE�AT5?AM�AHE�AB��A@~�A>�A=`BA;��A:-A8jA7��A7K�A6�/A6��A6�+A65?A6bA6A5��A5�mA5�TA5�TA5�;A5�#A5�
A5��A5�^A5�-A5�A5��A5��A5��A5��A5�hA5�7A5�A5l�A5S�A5/A4��A4�`A4�/A4��A4ĜA4�!A4ffA41'A3��A3hsA3�A2��A2E�A2  A1�wA1�A1oA0=qA/�-A.��A-t�A,ZA+?}A*$�A(v�A'�A%��A$��A#G�A!��A!�A"5?A"�A#"�A#G�A#C�A"9XA!hsA ��A �9A ��A ~�A n�A -A�hA��AƨAl�A��A��A�;A�A��A��A��A��A�-A�^A�#A�#A��A��A�hAO�A+A��A�uA �A��AM�A��AXA��An�A  A��AjAAI�A��AO�AoA�/A
=A�HA��A"�A�/A��A��A��A=qA�;AC�A
�A
v�A
ZA
�A	�A�A�9A5?A�^A�7A��At�A�+A9XA�
AO�A��A�A�FA;dAO�AA��A�\A�
A��Ap�A ��A ��A ��A �A V@��@�t�@�=q@��-@���@�z�@�I�@�ƨ@��@�;d@�@�O�@�9X@���@�;d@�ȴ@�^5@��7@���@���@��@��@�7L@�%@���@�O�@�b@���@�b@�Ĝ@�\)@�t�@ϝ�@�p�@��m@�&�@��@�dZ@���@�K�@��@��y@��m@���@�ƨ@���@�Q�@��@���@�1@���@�@��y@��!@��u@���@���@�bN@���@� �@���@�@��@�+@���@���@��H@���@���@�5?@�&�@�bN@��P@��@�hs@���@�dZ@���@�(�@}��@{@y��@w��@u�@sdZ@r=q@qhs@o��@m�T@lj@i��@f�R@e/@d1@b�H@ahs@`��@_l�@\z�@Z�@Y7L@Wl�@U�-@TI�@R�\@Q�#@O�w@M�-@L1@J~�@HbN@F�+@E��@D��@C@AG�@?�@>�+@=`B@<z�@:��@9G�@8  @7\)@6ȴ@6@4�@3�@2n�@0��@.ȴ@-�@,�D@*^5@)�#@(bN@'|�@%��@$1@"��@"n�@!��@ r�@K�@�T@?}@Z@dZ@��@�@%@�u@�P@�@v�@�@�@�m@@^5@X@bN@��@ff@�-@��@�@C�@33@
=q@	�7@bN@ �@\)@�@��@�T@�@Z@�F@"�@=q@�@x�@%@ Ĝ@ A�?���?�V?��?���?�V?�1?�?�^5?��?�
=?���?���?�F?�-?�hs?��;?��?�{?���?��m?�dZ?�^5?��?��?�K�?�E�?��?䛦?�t�?�!?�J?�%?�bN?� �?�\)?�V?�p�?��?�1?�C�?ڟ�?�=q?�X?���?�b?�l�?�E�?���?�S�?�J?�A�?�\)?��?���?�ƨ?��#?�r�?�ȴ?�`B?�S�?��?���?�\)?�v�?��?�V?��m?��H?��?�X?�r�?��P?���?��T?�`B?��j?���?��F?���?��!?��?�J?�J?��?�M�?��!?�33?��F?�9X?��/?���?�ff?�+?��?�1'?�Q�?�r�?��u?��9?���?��?�X?�x�?���?��^?��#?��?�=qA�"�A�$�A�"�A�"�A�$�A�&�A�(�A�+A�+A�+A�+A�-A�+A�-A�$�A�$�A�(�A�&�A�(�A�&�A�&�A�&�A�"�A�$�A�$�A�$�A�"�A� �A��A� �A� �A�&�A�$�A�&�A� �A�$�A�+A�+A�-A�-A�-A�/A�/A�1'A�1'A�/A�/A�/A�-A�+A�-A�-A�-A�-A�+A�-A�-A�+A�-A�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	&�B	&�B	&�B	&�B	&�B	&�B	&�B	'�B	&�B	&�B	&�B	&�B	'�B	(�B	+B	49B	@�B	S�B	S�B	Q�B	M�B	A�B	=qB	�B	 �B	�B	�B	!�B	�=B
.B
ÖB
�
B
��B
�B
��B
=B+BK�B�+B�9BÖB�yB��B%BbBB�sB�B�TB��BŢB��B��B��B�\B�7B�B}�BR�BQ�B,B$�B
�mB
�B
n�B
`BB
49B
hB	�B	��B	��B	 �B	�B��B��B��B�BjB7LB#�B+BB��B��B��B��BJB�B$�B8RBH�BS�Bs�B�B�JB��B��B��B��B�B�B�LB�wB��B�HB�yB�B��B��B	B	1B	oB	�B	+B	9XB	L�B	]/B	dZB	gmB	k�B	n�B	s�B	�B	�=B	��B	��B	�-B	�XB	ƨB	��B	��B	��B	�B	�`B	�B	�B	��B
B

=B
JB
JB
JB
+B
%B	��B	��B
  B
uB
7LB
@�B
G�B
J�B
L�B
M�B
N�B
M�B
M�B
M�B
L�B
K�B
L�B
N�B
S�B
T�B
VB
bNB
ffB
gmB
iyB
k�B
l�B
o�B
r�B
r�B
u�B
{�B
}�B
�B
�B
�B
�B
~�B
|�B
t�B
p�B
l�B
[#B
VB
Q�B
L�B
I�B
A�B
@�B
?}B
9XB
9XB
7LB
7LB
7LB
<jB
;dB
7LB
5?B
6FB
<jB
?}B
@�B
@�B
?}B
=qB
<jB
<jB
?}B
?}B
<jB
:^B
:^B
6FB
6FB
5?B
:^B
8RB
49B
7LB
5?B
1'B
/B
-B
(�B
-B
.B
-B
-B
)�B
'�B
+B
(�B
)�B
(�B
'�B
'�B
'�B
)�B
&�B
&�B
&�B
%�B
&�B
%�B
%�B
$�B
"�B
"�B
!�B
 �B
#�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
JB
JB
1B

=B
JB
+B
B
%B
PB

=B
+B
B
  B
B
B
%B
B
B
B
B
B
%B
	7B
1B

=B
+B
+B
oB
{B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
&�B
(�B
(�B
,B
/B
0!B
1'B
1'B
2-B
49B
5?B
5?B
7LB
9XB
;dB
;dB
>wB
?}B
@�B
A�B
C�B
C�B
D�B
D�B
F�B
G�B
J�B
I�B
L�B
L�B
M�B
N�B
N�B
N�B
Q�B
Q�B
R�B
S�B
T�B
VB
W
B
XB
YB
[#B
[#B
\)B
]/B
_;B
_;B
`BB
`BB
aHB
aHB
bNB
bNB
cTB
dZB
e`B
ffB
e`B
ffB
e`B
gmB
hsB
hsB
iyB
k�B
k�B
m�B
m�B
n�B
o�B
p�B
q�B
r�B
s�B
r�B
s�B
t�B
u�B
w�B
w�B
x�B
y�B
y�B
y�B
z�B
z�B
{�B
|�B
{�B
|�B
}�B
}�B
~�B
~�B
�B
�B
�B
�B
�B
�%B
�%B
�1B
�+B
�1B
�7B
�=B
�7B
�DB
�DB
�JB
�PB
�PB
�VB
�VB
�bB
�bB
�bB
�oB
�hB
�oB
�oB
�{B
�{B
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
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�3B
�3B
�3B
�?B
�9B
�FB
�FB
�LB
�LB
�RB
�RB
�XB
�XB
�^B
�^B
�jB
�jB
�qB
�wB
�wB
�}B
��B
��B
��B
B
B
ĜB
ÖB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ŢB
ŢB
ŢB
ƨB
ŢB
ƨB
ŢB
ƨB
ŢB
ŢB
ƨB
ƨB
ƨB
ŢB
ŢB	&�B	%�B	&�B	&�B	&�B	'�B	'�B	&�B	&�B	&�B	&�B	%�B	&�B	%�B	'�B	&�B	%�B	&�B	%�B	&�B	&�B	%�B	&�B	&�B	&�B	%�B	&�B	%�B	&�B	&�B	&�B	&�B	%�B	%�B	&�B	&�B	'�B	&�B	&�B	&�B	&�B	%�B	&�B	&�B	&�B	&�B	&�B	&�B	&�B	'�B	&�B	&�B	&�B	&�B	'�B	&�B	&�B	'�B	&�B	&�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               B	&�B	&�B	&�B	&�B	&�B	&�B	&�B	'�B	&�B	&�B	&�B	&�B	'�B	(�B	*�B	4B	@dB	S�B	S�B	Q�B	M�B	AmB	=UB	�B	 �B	�B	�B	!�B	�#B
-�B
�}B
��B
��B
��B
��B
&B*�BK�B�B�$BÁB�dB��BBOBB�aB�yB�CB��BőB��B��B�}B�MB�(B�B}�BR�BQ�B+�B$�B
�aB
��B
n�B
`7B
4.B
]B	�B	��B	��B	 �B	�B��B��B��B�BjtB7AB#�B B �B��B��B��B��BBB�B$�B8LBH�BS�Bs�B�B�FB��B��B��B��B�B�B�MB�xB��B�JB�|B�B��B��B	B	7B	vB	�B	+
B	9`B	L�B	]9B	ddB	gxB	k�B	n�B	s�B	�B	�KB	��B	��B	�<B	�hB	ƹB	��B	��B	�
B	�0B	�sB	�B	�B	��B
B

SB
`B
aB
aB
CB
=B	��B	��B
 B
�B
7gB
@�B
G�B
J�B
L�B
M�B
N�B
M�B
M�B
M�B
L�B
K�B
L�B
N�B
TB
U"B
V(B
bsB
f�B
g�B
i�B
k�B
l�B
o�B
r�B
r�B
u�B
|B
~B
�EB
�EB
�FB
�@B
(B
}B
t�B
p�B
l�B
[TB
V5B
RB
L�B
I�B
A�B
@�B
?�B
9�B
9�B
7�B
7�B
7�B
<�B
;�B
7�B
5xB
6�B
<�B
?�B
@�B
@�B
?�B
=�B
<�B
<�B
?�B
?�B
<�B
:�B
:�B
6�B
6�B
5�B
:�B
8�B
4~B
7�B
5�B
1mB
/bB
-UB
)>B
-VB
.]B
-XB
-XB
*GB
(;B
+NB
)BB
*IB
)CB
(>B
(?B
(?B
*LB
'9B
':B
':B
&5B
'<B
&6B
&7B
%1B
#&B
#&B
"!B
!B
$.B
!B
!B
B
B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
�B
�B
�B
�B
�B

�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
�B

�B
�B
�B
*B
:B
OB
XB
iB
�B
 �B
 �B
!�B
"�B
$�B
'�B
)�B
)�B
,�B
0B
1B
2B
2B
3#B
52B
6;B
6>B
8NB
:]B
<lB
<pB
?�B
@�B
A�B
B�B
D�B
D�B
E�B
E�B
G�B
H�B
K�B
J�B
NB
NB
OB
PB
PB
PB
S4B
S7B
T@B
UIB
VRB
W[B
XdB
YmB
ZxB
\�B
\�B
]�B
^�B
`�B
`�B
a�B
a�B
b�B
b�B
c�B
c�B
d�B
e�B
f�B
g�B
f�B
g�B
f�B
iB
jB
jB
k!B
m0B
m3B
oBB
oEB
pOB
qXB
r`B
siB
trB
u{B
txB
u�B
v�B
w�B
y�B
y�B
z�B
{�B
{�B
{�B
|�B
|�B
}�B
~�B
}�B
~�B
�B
�B
��B
��B
�B
�B
�B
�!B
�*B
�3B
�6B
�EB
�AB
�JB
�SB
�\B
�YB
�iB
�lB
�uB
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
��B
��B
��B
��B
�B
�B
�B
�$B
�2B
�>B
�IB
�OB
�ZB
�iB
�uB
�tB
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
�B
�B
�B
� B
�,B
�7B
�<B
�DB
�JB
�OB
�[B
�jB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�(B
�DB
�SB
�hB
�yB
��B
��B
��B
��B
��B
��B
�B
�B
�'B
�;B
�JB
�aB
�uB
ńB
ƚB
ǯB
ǾB
��B
��B
��B
�	B
�B
�(B
�=B
�MB
�\B
�rB
̀B
̐B
̠B
̯B
̿B
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
��B	&�B	%�B	&�B	&�B	&�B	'�B	'�B	&�B	&�B	&�B	&�B	%�B	&�B	%�B	'�B	&�B	%�B	&�B	%�B	&�B	&�B	%�B	&�B	&�B	&�B	%�B	&�B	%�B	&�B	&�B	&�B	&�B	%�B	%�B	&�B	&�B	'�B	&�B	&�B	&�B	&�B	%�B	&�B	&�B	&�B	&�B	&�B	&�B	&�B	'�B	&�B	&�B	&�B	&�B	'�B	&�B	&�B	'�B	&�B	&�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242203042021061413573620210614135736202107221611362021072216113620210722161136201807242203042021061413573620210614135736202107221611362021072216113620210722161136PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422030420180724220304  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030420180724220304QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030420180724220304QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216142020210722161420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                