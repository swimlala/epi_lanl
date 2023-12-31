CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:52Z creation      
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
resolution        =���   axis      Z        x  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    LL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  Pl   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  e   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  u|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ü   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   <   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar            HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       (   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � tArgo profile    3.1 1.2 19500101000000  20180724220252  20210617131458  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�o
֩+�@�o
֩+�11  @�o
���0@�o
���0@6�2�I�@6�2�I��c�>BZ�c�c�>BZ�c11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @ff@9��@y��@�  @�  @�  A   AffA$��AC33Aa��A~ffA�  A���A���A���Aљ�A���A�  B   B  B  B  B   B'��B/��B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�ffB�33B���B�  B�33B�ffB�ffB�33B�ffB�33B���B�  B�  B�  B�33B�ffB�33B�  BǙ�B˙�B�33B�  B���B���B���B�  B���B���BB�33B�  B���C   C  C�C�CL�C	��C�fC33CffC  C  C33C33C33C�fC�fC   C"  C$�C&�C(�C*33C,33C-�fC/�fC2  C3�fC5�fC7�fC9�fC;�fC>�C@33CB33CD33CF33CH�CJ�CL  CM�fCP33CR  CS�fCV33CX�CZ�C\  C]�fC`33Cb�Cd  Ce�fCg�fCj33Cl�Cn�Cp  Cq�fCtL�Cv33Cx33Cz�C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C��C��C�  C�&fC�&fC��C��C��C��C��fC�  C��C��C��3C��C��3C��fC��3C��C��C��C��C��C�&fC�&fC�&fC�33C��C�ٚC��fC��C��C�ٚC�ٚC�ٚC��fC��fC�ٚC��C��C��fC��fC��C�33C��C��3C��fC��fC��fC��fC��fC��3C�  C��C��C��C�  C��fC��3C��C�  C��fC�  C��C��C�&fC��C�  C��C��C��C��fC�  C��C�  C��fC��3C��C��C��C��fC��3C��C��C�&fC��C��3C��C��C��C��3C�  C��C�  C��fC��3C�  C��C�&fC��C��fC��3C�  C�&fC�&fC�&fC�  C��C��C�  C��fD33D�3D��D
,�D�3Dy�D  D� D�3DFfD��D�fD"  D$� D'Y�D)�3D,�3D/,�D1� D4Y�D6��D9� D<�D>��DA�DC�fDF,�DH�fDKS3DM��DPs3DR�3DUy�DX  DZy�D\��D_� Db  Dds3Df�3Dis3Dk��Dns3Dp� Ds@ Du�fDxfDzs3D|y�D~ٚD���D�ɚD���D�&fD�VfD��3D���D��fD��fD�  D��D�<�D�c3D��fD���D���D���D�&fD�P D���D��fD�  D�,�D�\�D�� D���D��3D� D�L�D�|�D��fD�ٚD��D�<�D�ffD���D��3D��D�3D�<�D�i�D���D��3D�ٚD�  D��D�C3D�c3D�� D��fD��fD��fD���D��D�<�D�\�D�vfD3DöfD���D�3D�)�D�C3D�ffDʃ3D˦fD��3D���D�&fD�C3D�ffDҐ Dӹ�D���D�3D�&fD�L�D�i�Dډ�DۦfD�ɚD���D� D�33D�S3D�|�D� D��3D���D��D�0 D�S3D� D�fD��fD���D�,�D�P D�p D�3D��D��fD��3D���D��D�  D�6fD�33D�C3D�VfD�i�D�y�E H E �fEVfE�fEc3E�Es3E��E~fE  E�E!�E��E	�fE
� Eq�E~fE�3E�E$�E1�E�fE��EH ENfE�fE�3E^fEc3E�fE� E ` E!X E"�fE#��E%4�E&� E'�fE)�E)�fE+VfE,��E.�E/3E0i�E1� E2�3E3�3E5C3E6�3E7�E933E:{3E;�fE>��EA��ED�fEH!�EK EN[3EQVfET��EWɚEZ�fE^ Ea6fEdffEgC3Ej��Em�fEp��Es��Ev�3Ez!�E}FfE�&fE��3E�2fE���E�X E���E�q�E�fE�VfE���E��fE�T�E��fE���E�2fE���E�� E�.fE�nfE�� E�( E�d�E��fE���E�X E��3E��fE�D�E�� E��3E�, E��fE��fE��>���>L��?   >���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���?   >���>���>���>���>���>���?��?��?L��?�  ?���?�33?�  ?�33@ff@��@&ff@9��@L��@fff@s33@���@�33@�  @���@���@�ff@�33@�  @���@�ffA33A33A��A��A   A(  A0  A8  A>ffAH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141441444414441444144141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ?fff?�  @&ff@Y��@���@�  @�  @�  A  AffA,��AK33Ai��A�33A�  A���A���A���Aՙ�A���A�  B  B
  B  B  B"  B)��B1��B:  BB  BJ  BQ��BZ  Bb  Bj  Br  Bz  B�ffB�33B���B�  B�33B�ffB�ffB�33B�ffB�33B���B�  B�  B�  B�33B�ffB�33B�  Bș�B̙�B�33B�  B���B���B���B�  B���B���B�B�33B�  B���C � C� C��C��C��C
L�CffC�3C�fC� C� C�3C�3C�3CffCffC � C"� C$��C&��C(��C*�3C,�3C.ffC0ffC2� C4ffC6ffC8ffC:ffC<ffC>��C@�3CB�3CD�3CF�3CH��CJ��CL� CNffCP�3CR� CTffCV�3CX��CZ��C\� C^ffC`�3Cb��Cd� CfffChffCj�3Cl��Cn��Cp� CrffCt��Cv�3Cx�3Cz��C|� C~� C�@ C�@ C�@ C�@ C�@ C�@ C�33C�@ C�@ C�@ C�L�C�L�C�L�C�L�C�@ C�ffC�ffC�Y�C�Y�C�L�C�L�C�&fC�@ C�Y�C�L�C�33C�L�C�33C�&fC�33C�L�C�Y�C�Y�C�Y�C�Y�C�ffC�ffC�ffC�s3C�L�C��C�&fC�L�C�L�C��C��C��C�&fC�&fC��C�L�C�L�C�&fC�&fC�L�C�s3C�Y�C�33C�&fC�&fC�&fC�&fC�&fC�33C�@ C�L�C�L�C�Y�C�@ C�&fC�33C�Y�C�@ C�&fC�@ C�L�C�Y�C�ffC�L�C�@ C�L�C�Y�C�L�C�&fC�@ C�L�C�@ C�&fC�33C�Y�C�Y�C�L�C�&fC�33C�L�C�L�C�ffC�Y�C�33C�L�C�Y�C�L�C�33C�@ C�Y�C�@ C�&fC�33C�@ C�L�C�ffC�L�C�&fC�33C�@ C�ffC�ffC�ffC�@ C�L�C�Y�C�@ C�&fDS3D�3D��D
L�D�3D��D@ D  D�3DffD�D�fD"@ D$� D'y�D*3D,�3D/L�D1� D4y�D7�D9� D<,�D>��DA9�DC�fDFL�DH�fDKs3DN�DP�3DS3DU��DX  DZ��D]�D_� Db  Dd�3Dg3Di�3Dl�Dn�3Dq  Ds` Du�fDx&fDz�3D|��D~��D���D�ٚD��D�6fD�ffD��3D���D��fD��fD� D�,�D�L�D�s3D��fD���D���D�	�D�6fD�` D���D��fD� D�<�D�l�D�� D�ɚD��3D�  D�\�D���D��fD��D��D�L�D�vfD���D��3D���D�#3D�L�D�y�D���D��3D��D� D�,�D�S3D�s3D�� D��fD��fD��fD��D�)�D�L�D�l�D��fD£3D��fD���D�3D�9�D�S3D�vfDʓ3D˶fD��3D��D�6fD�S3D�vfDҠ D�ɚD���D�3D�6fD�\�D�y�Dڙ�D۶fD�ٚD���D�  D�C3D�c3D��D� D��3D���D��D�@ D�c3D� D�fD��fD��D�<�D�` D� D�3D��D��fD��3D�	�D��D�0 D�FfD�C3D�S3D�ffD�y�D���E P E �fE^fE�fEk3E�E{3E�E�fE E�E)�E��E	�fE
� Ey�E�fE�3E!�E,�E9�E�fEɚEP EVfE�fE�3EffEk3E�fE� E h E!` E"�fE#��E%<�E&� E'�fE)	�E)�fE+^fE,��E.$�E/3E0q�E1� E2�3E43E5K3E6�3E7�E9;3E:�3E;�fE>��EA��ED�fEH)�EK ENc3EQ^fET��EWњEZ�fE^ Ea>fEdnfEgK3Ej��Em�fEp��Es��Ev�3Ez)�E}NfE�*fE��3E�6fE���E�\ E� �E�u�E�fE�ZfE���E��fE�X�E��fE���E�6fE���E�� E�2fE�rfE�� E�, E�h�E��fE� �E�\ E��3E��fE�H�E�� E��3E�0 E��fE��fE��G�O�?333G�O�?L��G�O�?L��G�O�G�O�?333G�O�G�O�G�O�G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�?fffG�O�?���?�ff?�  ?���?�33@   @��@&ff@9��@Fff@Y��@l��@�33@���@���@�33@�  @���@ə�@�ff@�33@�  @���A33A33A33A��A!��A(  A0  A8  A@  AFffAP  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141441444414441444144141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     @ �@ �@ V@ �@ �@ ""@ (�@ /�@ 6�@ <�@ FQ@ SI@ `B@ l�@ z�@ �7@ ��@ ��@ ��@ ��@ ��@ �t@ �@ ��@j@@
@+�@:@G�@UU@bN@p�@~K@��@��@�A@�F@�>@ψ@��@�4@��@1@*@#�@0x@<�@K@X�@ff@t�@�@�@�@�M@��@ƨ@Ӡ@��@�@@��@
=@6@$�@1�@A�@N�@[z@i�@ww@��@�u@�z@��@�@��@��@�@�Y@^@@�@(G@5�@DD@Q�@`B@m�@{�@��@��@�(@��@�&@�@��@�m@�@�@�@ @-�@;d@I@V@c�@p�@}�@�P@��@��@�F@�>@��@��@��@��@�@{@!s@/@>�@K�@Yn@ff@s_@��@��@�a@�Y@�R@��@Ӡ@�H@��@��@
=@�@$�@33@@�@N�@\�@j@x&@��@��@�y@�!@�@��@׹@�`@��@  @@�@(G@7L@C�@P�@^�@m�@|?@��@��@�5@��@�2@��@�/@��@�@	@	�@	g@	*S@	7�@	E�@	S�@	a�@	n�@	~�@	��@	�<@	��@	��@	�J@	є@	�/@	�(@	��@
v@
@
 �@
/@
=q@
K�@
Yn@
g�@
t@
�W@
��@
�a@
��@
��@
��@
�O@
�@
��@
�E@
=@�@&�@3�@?}@N�@\�@i�@v@�p@�$@��@��@�^@ȴ@׹@�`@�e@^@�@�@*S@7L@C�@Q�@`�@m:@y�@��@�0@��@��@��@�o@��@�@��@v@@�@-@;d@G�@S�@�T@+@uk@�@v@M�@�0@�H@+@t�@�@@K@��@��@ �@hs@�r@��@<�@�@�c@@T�@��@ލ@#�@j@�!@� @<@�W@�J@
=@M�@�@�
@O@^5@�y@�@+�@oF@�~@�Y@3�@t�@��@�@@/@oF@�!@�@1'@r@�-@��@-�@k.@�A@�@!s@`A@�a@܀@�@Z�@��@�t@ [@ `�@ ��@ �@!$�@!ff@!��@!�`@"%�@"i!@"��@"�y@#+@#l�@#�f@#��@$.l@$m�@$��@$�4@%+�@%k�@%��@%��@&'�@&ff@&�(@&��@'g@'\)@'��@'�h@(�@(Q=@(��@(�@)	�@)E�@)�d@)��@)�Q@*>@*|�@*�@*� @+3�@+r@+�-@+�@,1'@,m�@,�@,�@-+@-i!@-��@-�@.$�@.a�@.�@.��@/�@/X@/�0@/�O@0�@0Q=@0�\@0�|@1�@1Ji@1��@1ƨ@2�@2E�@2�|@2�J@3%@3DD@3��@3��@3��@47�@4t�@4�!@4��@5%�@5`�@5��@5��@6�@6FQ@6�W@6��@6�@7/@7i!@7��@7�#@8�@8O1@8��@8�&@93�@9��@:S�@:ȴ@;=q@;�4@<^�@<є@={�@=�@>`B@?	�@?x&@@�@@��@A5�@A�4@BJi@B��@C^�@C��@Dm:@D�
@Ev�@E��@F|�@G�@G�@H�@H�@I�@I�-@JI�@J�r@KD�@K�t@L;d@L��@MV�@M�@Nx&@O�@O��@P6@QP�@R��@S��@Uc�@V��@X�@YQ=@Z�2@\�@]i!@^�&@`6@as_@b�@d�@eo�@f�@h@iV�@j�@lb@mZ�@n�9@o�e@qR�@r��@t
�@uI@v� @v�@w7�@woF@w��@w�e@xD�@x|�@x�|@yj@yS�@y��@y�t@z(�@z\�@z�M@zލ@{,`@{z2@{��@{�q@|@�@|�C@|��@}v@}M$@}�dG�O�@ ^G�O�@ G�O�@ G�O�G�O�@ ^G�O�G�O�G�O�G�O�@ G�O�G�O�G�O�@ G�O�G�O�G�O�@ G�O�G�O�@ �G�O�@ @ v@ �@ �@ 	�@ 
=@ �@ V@ b@ �@ �@ �@ �@ �@ [@ g@ ""@ $�@ '�@ *S@ -@ /�@ 2�@ 4�@ 7�@ ;d@ >@ A�@ DD@ G�@ K@ N�@ Q=@ UUG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�&�A�&�A�/A�1'A�1'A�1'A�1'A�+A�-A�+A�"�A�/A�9XA�=qA�=qA�?}A�=qA�1'A�(�A���A��A��A���Aω7AρAσAρA�~�A�z�A�p�A�x�A�r�A�hsA�=qA�JA�VA���A�dZA���Aŧ�A�r�AËDA�`BA�^5A��
A���A��A�r�A�Q�A���A��A�n�A�9XA���A�G�A�5?A��A��A�Q�A�/A�bA���A���A�|�A��A�v�A�G�A���A�VA�+A�
=A��;A�Q�A��A��A��mA� �A���A��A���A���A�bNA�
=A��PA�
=A���A�r�A�
=A��hA�A�A��DA�dZA�$�A���A�dZA���A��-A�|�A�(�A��A��A��A��A��A��A�;dA���A�Q�A�ƨA�`BA�7LA��
A���A���A���A�/A��TA��\A��DA��A�`BA���A�-A��A�G�A��jA�Az-As�Ap-An9XAlAj �Ah��Aex�AahsA`9XA_�#A_��A_\)A_%A^r�A]�PA\��A[�AZ��AYAV�AUC�AS�AS%AR��AR9XAP~�AM��AJ�!AH�`AH-AG�;AG�wAG%ADr�A@z�A>  A=ƨA=�A<�uA:-A7C�A5�A3��A2ĜA2(�A1�A/|�A. �A-G�A,^5A*�A)�A)C�A($�A&VA$�RA$�\A#t�A"�uA!�-A ��A�hA�\A�;A�A�jA�+A�A�A�#AffAE�A5?A{A��A��A�7A?}A7LA�wA�A�9A^5A��A�wA��AXA�A�!A�TA�A7LA
ȴA	�TA(�A�A;dAĜAAA��Az�AffA^5AZAM�A�;AG�A%A�AdZA 1@�v�@�X@�A�@��`@��R@���@�@�b@�"�@�\@��@�@�R@�n�@�&�@�~�@�bN@�?}@���@�ƨ@��H@҇+@��@�t�@�C�@�bN@��H@�C�@��#@�M�@���@��j@�%@�
=@�K�@�@�V@�`B@�{@�x�@��R@��@���@�O�@�j@��P@��-@��`@�ȴ@��@�X@��j@��@�o@�n�@���@���@;d@K�@~E�@~$�@{�@x��@y��@x �@u@tI�@r�!@q��@p�9@p �@nȴ@nV@m/@k�m@k@i&�@f�y@e�@c�
@b�!@a7L@`�@_K�@^5?@]V@[�
@Z��@Y��@W�@V��@T��@T1@R=q@Q%@P1'@M�@L�@J�!@I��@H��@G�@G
=@E��@E�@D��@C�@B�H@@�u@@A�@>ȴ@=@<�D@;@9��@8�`@8 �@7
=@5�@4�@41@2M�@1�7@01'@/|�@.��@-��@,z�@+�
@+t�@*��@(bN@&�@&5?@%�@$�@$j@#�
@#@!��@ �@  �@|�@p�@j@�
@n�@�7@�9@1'@�P@+@$�@�-@�j@9X@dZ@�\@��@%@  @�y@ff@@?}@z�@�@
��@
�\@	hs@Ĝ@1'@l�@
=@V@�@�@z�@(�@dZ@��@��@=q@x�@G�@ bN?���?�{?�dZ?���?�K�?��?�!?�%?�|�?�(�?��H?�x�?���?�K�?�?���?�o?���?�;d?��?ܬ?��m?�=q?ٙ�?���?֧�?ԛ�?��?�n�?У�?�  ?�\)?�V?�/?�/?�V?�(�?�ƨ?�dZ?�?�^5?ɺ^?�7L?�Q�?ǍP?�E�?ļj?Õ�?�J?���?�A�?���?�{?���?�j?�ƨ?�C�?���?�~�?�^5?�~�?�~�?��H?�"�?�"�?�dZ?�dZ?�ƨ?�1?�j?��?��h?�{?���?�v�?���?��?���?���?��?�;d?�|�?�|�?��w?��w?��;?� �?�A�?�bN?��?���?�Ĝ?��`?�%?�G�?�hs?��7?��7?���?��A�&�A�&�A�(�A�+A�&�A�&�A�$�A�(�A�+A�(�A�(�A�(�A�(�A�&�A�&�A�$�A�(�A�+A�"�A�$�A�&�A�"�A�+A�$�A�&�A�&�A�(�A�(�A�(�A�&�A�$�A�(�A�-A�/A�/A�1'A�1'A�33A�1'A�33A�1'A�1'A�1'A�1'A�33A�-A�(�A�-A�-A�+A�-A�-A�&�A�&�A�$�A� �A��A�"�A�-A�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     A�&�A�&�A�/A�1'A�1'A�1'A�1'A�+A�-A�+A�"�A�/A�9XA�=qA�=qA�?}A�=qA�1'A�(�A���A��A��A���Aω7AρAσAρA�~�A�z�A�p�A�x�A�r�A�hsA�=qA�JA�VA���A�dZA���Aŧ�A�r�AËDA�`BA�^5A��
A���A��A�r�A�Q�A���A��A�n�A�9XA���A�G�A�5?A��A��A�Q�A�/A�bA���A���A�|�A��A�v�A�G�A���A�VA�+A�
=A��;A�Q�A��A��A��mA� �A���A��A���A���A�bNA�
=A��PA�
=A���A�r�A�
=A��hA�A�A��DA�dZA�$�A���A�dZA���A��-A�|�A�(�A��A��A��A��A��A��A�;dA���A�Q�A�ƨA�`BA�7LA��
A���A���A���A�/A��TA��\A��DA��A�`BA���A�-A��A�G�A��jA�Az-As�Ap-An9XAlAj �Ah��Aex�AahsA`9XA_�#A_��A_\)A_%A^r�A]�PA\��A[�AZ��AYAV�AUC�AS�AS%AR��AR9XAP~�AM��AJ�!AH�`AH-AG�;AG�wAG%ADr�A@z�A>  A=ƨA=�A<�uA:-A7C�A5�A3��A2ĜA2(�A1�A/|�A. �A-G�A,^5A*�A)�A)C�A($�A&VA$�RA$�\A#t�A"�uA!�-A ��A�hA�\A�;A�A�jA�+A�A�A�#AffAE�A5?A{A��A��A�7A?}A7LA�wA�A�9A^5A��A�wA��AXA�A�!A�TA�A7LA
ȴA	�TA(�A�A;dAĜAAA��Az�AffA^5AZAM�A�;AG�A%A�AdZA 1@�v�@�X@�A�@��`@��R@���@�@�b@�"�@�\@��@�@�R@�n�@�&�@�~�@�bN@�?}@���@�ƨ@��H@҇+@��@�t�@�C�@�bN@��H@�C�@��#@�M�@���@��j@�%@�
=@�K�@�@�V@�`B@�{@�x�@��R@��@���@�O�@�j@��P@��-@��`@�ȴ@��@�X@��j@��@�o@�n�@���@���@;d@K�@~E�@~$�@{�@x��@y��@x �@u@tI�@r�!@q��@p�9@p �@nȴ@nV@m/@k�m@k@i&�@f�y@e�@c�
@b�!@a7L@`�@_K�@^5?@]V@[�
@Z��@Y��@W�@V��@T��@T1@R=q@Q%@P1'@M�@L�@J�!@I��@H��@G�@G
=@E��@E�@D��@C�@B�H@@�u@@A�@>ȴ@=@<�D@;@9��@8�`@8 �@7
=@5�@4�@41@2M�@1�7@01'@/|�@.��@-��@,z�@+�
@+t�@*��@(bN@&�@&5?@%�@$�@$j@#�
@#@!��@ �@  �@|�@p�@j@�
@n�@�7@�9@1'@�P@+@$�@�-@�j@9X@dZ@�\@��@%@  @�y@ff@@?}@z�@�@
��@
�\@	hs@Ĝ@1'@l�@
=@V@�@�@z�@(�@dZ@��@��@=q@x�@G�@ bN?���?�{?�dZ?���?�K�?��?�!?�%?�|�?�(�?��H?�x�?���?�K�?�?���?�o?���?�;d?��?ܬ?��m?�=q?ٙ�?���?֧�?ԛ�?��?�n�?У�?�  ?�\)?�V?�/?�/?�V?�(�?�ƨ?�dZ?�?�^5?ɺ^?�7L?�Q�?ǍP?�E�?ļj?Õ�?�J?���?�A�?���?�{?���?�j?�ƨ?�C�?���?�~�?�^5?�~�?�~�?��H?�"�?�"�?�dZ?�dZ?�ƨ?�1?�j?��?��h?�{?���?�v�?���?��?���?���?��?�;d?�|�?�|�?��w?��w?��;?� �?�A�?�bN?��?���?�Ĝ?��`?�%?�G�?�hs?��7?��7?���?��A�&�A�&�A�(�A�+A�&�A�&�A�$�A�(�A�+A�(�A�(�A�(�A�(�A�&�A�&�A�$�A�(�A�+A�"�A�$�A�&�A�"�A�+A�$�A�&�A�&�A�(�A�(�A�(�A�&�A�$�A�(�A�-A�/A�/A�1'A�1'A�33A�1'A�33A�1'A�1'A�1'A�1'A�33A�-A�(�A�-A�-A�+A�-A�-A�&�A�&�A�$�A� �A��A�"�A�-A�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��BK�BaHBcTBcTBv�B�bB��B�^B�NB�mB�yB�sB�mB�mB�yB�B �B8RBE�BaHBiyBw�B�B�\B�hB�{B�{B��B��B��B��B��B��B�B�-B�3B�3B��B��B�uB��B��B��B��B�{B�bB�PB�=B�+B�B�B~�B|�By�Bt�BjBe`BbNB^5BYBQ�BA�B+B�BPBB��B�B�B�mB��B�-B��Bz�BjBe`B^5BE�B33B(�B�B�B{BoBB
�mB
�yB
�#B
��B
u�B
k�B
Q�B
hB	��B	��B	�B	p�B	_;B	Q�B	G�B	.B	!�B	�B	�B	�B	�B	oB	bB	bB	bB	bB		7B	%B	B	B��B��B��B��B�B�;B��B�wB�wB��B��B�}B�-B��B��B�B�-B�B��B��B�VB�+B�B|�By�Bt�Bw�Bz�Bw�Bs�Bs�Bq�Bq�Bl�Bm�BjBhsBgmBe`BffBe`BffBffBhsBhsBm�Bk�BffBdZBcTBbNBaHB`BB`BB`BB`BB^5B[#B\)BZBW
BW
BT�BS�BR�BR�BF�BD�BB�BA�B@�B>wB<jB:^B<jB:^B9XB7LB8RB8RB7LB7LB7LB7LB7LB6FB6FB6FB49B33B33B33B49B2-B49B5?B5?B5?B33B49B33B49B33B6FB5?B5?B=qBVBdZB��B��B�jB�5B��B��B�FB�B�3B�XB�'B�XBBȴB�B�HB	PB	�B	%�B	:^B	A�B	<jB	6FB	=qB	H�B	ZB	[#B	]/B	cTB	hsB	t�B	~�B	�JB	�\B	�uB	��B	��B	��B	�-B	�XB	B	��B	�
B	�/B	�/B	�TB	�sB	�sB	�yB	�B	�B	��B	��B	��B
B
B
1B
DB
\B
uB
{B
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
$�B
'�B
&�B
)�B
+B
,B
.B
0!B
1'B
1'B
2-B
2-B
33B
49B
49B
5?B
6FB
7LB
:^B
9XB
;dB
<jB
<jB
?}B
?}B
A�B
C�B
C�B
E�B
D�B
D�B
G�B
H�B
I�B
I�B
K�B
L�B
M�B
N�B
N�B
N�B
Q�B
R�B
R�B
T�B
T�B
T�B
VB
W
B
W
B
YB
YB
YB
]/B
]/B
^5B
`BB
`BB
bNB
bNB
cTB
dZB
dZB
e`B
ffB
ffB
hsB
hsB
iyB
jB
k�B
m�B
l�B
m�B
m�B
o�B
p�B
p�B
q�B
s�B
s�B
s�B
t�B
v�B
u�B
v�B
w�B
x�B
x�B
x�B
y�B
y�B
z�B
{�B
{�B
|�B
}�B
~�B
� B
�B
�B
�B
�%B
�%B
�+B
�=B
�=B
�DB
�DB
�PB
�\B
�\B
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
��B
��B
�B
�B
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
�FB
�LB
�FB
�FB
�LB
�LB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�^B
�^B
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
�RB
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
�^B
�XB
�XB
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
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     B
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
�B
�B
�B
�B
�B
�B
��B
��B
�B
�B
�B
�B
��BK�Ba2Bc?Bc?Bv�B�NB��B�KB�;B�[B�gB�bB�\B�]B�jBrB �B8DBE�Ba;BilBw�B��B�QB�^B�qB�rB��B��B��B��B��B�{B��B�(B�/B�0B��B��B�sB��B��B��B��B�|B�cB�RB�?B�.B�B�B~�B|�By�Bt�Bj�BegBbVB^=BY BQ�BA�B+B�B[B$B�B��B�B�zB�B�;B��Bz�Bj�BepB^EBE�B3DB)B�B�B�B�B3B
�B
�B
�8B
��B
u�B
k�B
RB
B	�B	��B	�)B	p�B	_SB	RB	G�B	.-B	!�B	�B	�B	�B	�B	�B	~B	B	B	�B		UB	DB	%B	9B�B�B�B��B��B�^B�B��B��B��B��B��B�SB��B��B�)B�UB�CB�B��B�B�UB�0B}BzBt�Bw�B{Bw�Bs�Bs�Bq�Bq�Bl�Bm�Bj�Bh�Bg�Be�Bf�Be�Bf�Bf�Bh�Bh�Bm�Bk�Bf�Bd�Bc�Bb�Ba�B`{B`{B`|B`}B^pB[_B\eBZZBWGBWHBU<BT7BS1BS2BF�BD�BB�BA�B@�B>�B<�B:�B<�B:�B9�B7�B8�B8�B7�B7�B7�B7�B7�B6�B6�B6�B4�B3B3�B3�B4�B2{B4�B5�B5�B5�B3�B4�B3�B4�B3�B6�B5�B5�B=�BVaBd�B��B�$B��BޢB�jB�EB��B�zB��B��B��B��B�B�=B؜B��B	�B	5B	&|B	:�B	B(B	=B	6�B	>B	I^B	Z�B	[�B	]�B	d
B	i,B	uxB	�B	�B	�!B	�=B	�}B	��B	��B	�B	�/B	�iB	ͪB	��B	�B	�B	�<B	�^B	�`B	�iB	�B	��B	��B	��B	��B
B
B
	7B
MB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
&B
&B
)&B
(!B
+7B
,@B
-IB
/XB
1hB
2qB
2sB
3|B
3B
4�B
5�B
5�B
6�B
7�B
8�B
;�B
:�B
<�B
=�B
=�B
@�B
@�B
CB
EB
EB
G"B
FB
F"B
I6B
J?B
KHB
KJB
MZB
NcB
OlB
PtB
PwB
PzB
S�B
T�B
T�B
V�B
V�B
V�B
W�B
X�B
X�B
Z�B
Z�B
Z�B
^�B
^�B
_�B
bB
bB
d B
d#B
e,B
f4B
f7B
g@B
hIB
hKB
j[B
j^B
kgB
loB
mxB
o�B
n�B
o�B
o�B
q�B
r�B
r�B
s�B
u�B
u�B
u�B
v�B
x�B
w�B
x�B
y�B
z�B
z�B
z�B
|B
|B
}B
~B
~B
"B
�+B
�6B
�AB
�UB
�ZB
�eB
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
�B
�B
�#B
�1B
�<B
�CB
�MB
�bB
�fB
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
��B
��B
��B
�B
�B
�B
�+B
�*B
�EB
�ZB
�oB
��B
��B
��B
��B
��B
��B
�B
�B
�1B
�GB
�UB
�lB
�{B
��B
��B
��B
��B
��B
��B
��B
� B
�B
�%B
�5B
�DB
�TB
�\B
�`B
�\B
�`B
�hB
�fB
�hB
�lB
�nB
�rB
�tB
�~B
�vB
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
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202522021061413552720210614135527202106171312172021061713121720210617131217201807242202522021061413552720210614135527202106171312172021061713121720210617131217PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025220180724220252  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025220180724220252QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025220180724220252QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145820210617131458IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                