CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:03Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191703  20181005191703  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               @A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׿d��-1   @׿e)�,@5aG�z��d"��`B1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      @A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC!�fC#�fC%�fC'�fC)�fC,  C.�C0�C2�C4�C6�C8�C:  C<  C=�fC@  CA�fCD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX�CZ  C\  C^�C`�Cb  Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz�C|�C~�C�  C�  C�  C�  C��C��C��C�  C��3C�  C��C��C�  C��3C��3C��3C��3C��3C��fC��fC��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C��C�  C��3C��C��C�  C��3C�  C��C��C��3C��3C�  C��3C��C��C��C��C��C��C�  C��3C�  C��3C�  C��C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C��C��C��C��C�  C��C��C��C��C�  C�  C�  C��3C�  C�  C��C�  C��3C��3C�  C�  C�  C��C��C��3C��3C�  C�  C��3C�  C��3C��3C��C��C�  C��fC��3C��C�  C��3C�  C��C��3C�  C��C�  C�  C�  C��C��C��D   D � D  D�fD  Dy�D��D� D��Dy�D  D�fD��D� D  Dy�D��D� D��D	� D
fD
�fDfD�fD  Dy�D��Dy�D  D��DfD� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  Dy�D��D� DfD� D��D� D  Dy�D  D�fD fD � D ��D!� D"fD"� D"��D#y�D$  D$� D%  D%� D&fD&�fD'fD'� D'��D(y�D)  D)� D*fD*� D+  D+� D+��D,� D-fD-� D.  D.� D/  D/� D0  D0� D1�D1�fD2  D2� D3fD3�fD4  D4�fD5  D5y�D5��D6� D7fD7�fD8  D8� D9fD9�fD:  D:� D;  D;�fD<  D<� D=  D=�fD>  D>�fD?  D?�fD@fD@� DA  DA�fDB  DBy�DB��DC� DD  DD� DD��DEy�DE��DFy�DF��DG� DH  DHy�DI  DI� DI��DJy�DJ��DKy�DL  DLy�DM  DM� DM��DN� DN��DOy�DP  DP� DP��DQy�DR  DRy�DS  DS� DT  DT� DU  DU� DU��DVy�DW  DW� DW��DXy�DY  DY� DZ  DZ�fD[fD[�fD\fD\�fD]fD]� D]��D^y�D_  D_� D_��D`y�D`��Da�fDb  Dby�Dc  Dc� Dc��Ddy�Dd��De� Df  Df� Dg  Dg� DhfDhy�Di  Di� Dj  Dj� Dj��Dk� Dk��Dly�Dm  Dm�fDnfDn� Dn��Doy�DpfDp�fDqfDq� Dq��Dr� Ds  Ds� Dt  Dt�fDu  Duy�Dv  Dv� Dw  Dw� Dw��Dy��D�/\D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	p�B
=B
=B!
=B)
=B1
=B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��B��B��B��B��B��B��RB��RB��B��B��B��B��B��B��B��B��BąBȸRB̅BЅBԅBظRBܸRB��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C (�C"(�C$(�C&(�C((�C*(�C,B�C.\)C0\)C2\)C4\)C6\)C8\)C:B�C<B�C>(�C@B�CB(�CDB�CFB�CHB�CJB�CLB�CNB�CP\)CRB�CTB�CVB�CX\)CZB�C\B�C^\)C`\)CbB�CdB�Cf\)Ch\)CjB�ClB�CnB�CpB�CrB�CtB�CvB�Cx\)Cz\)C|\)C~\)C�!HC�!HC�!HC�!HC�.C�.C�.C�!HC�{C�!HC�.C�.C�!HC�{C�{C�{C�{C�{C��C��C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�.C�!HC�!HC�.C�!HC�{C�.C�.C�!HC�{C�!HC�.C�.C�{C�{C�!HC�{C�.C�.C�.C�.C�.C�.C�!HC�{C�!HC�{C�!HC�.C�.C�!HC�{C�!HC�!HC�!HC�!HC�{C�!HC�!HC�{C�!HC�.C�.C�.C�.C�.C�.C�!HC�.C�.C�.C�.C�!HC�!HC�!HC�{C�!HC�!HC�.C�!HC�{C�{C�!HC�!HC�!HC�.C�.C�{C�{C�!HC�!HC�{C�!HC�{C�{C�.C�:�C�!HC��C�{C�.C�!HC�{C�!HC�.C�{C�!HC�.C�!HC�!HC�!HC�.C�.C�.D �D ��D�D�
D�D�>D
>D��D
>D�>D�D�
D
>D��D�D�>D
>D��D	
>D	��D

D
�
D
D�
D�D�>D
>D�>D�D�qD
D��D�D�
D
D��D�D��D�D��D�D��D�D��D�D��D�D�
D�D��D�D��D�D�>D
>D��D
D��D
>D��D�D�>D�D�
D 
D ��D!
>D!��D"
D"��D#
>D#�>D$�D$��D%�D%��D&
D&�
D'
D'��D(
>D(�>D)�D)��D*
D*��D+�D+��D,
>D,��D-
D-��D.�D.��D/�D/��D0�D0��D1qD1�
D2�D2��D3
D3�
D4�D4�
D5�D5�>D6
>D6��D7
D7�
D8�D8��D9
D9�
D:�D:��D;�D;�
D<�D<��D=�D=�
D>�D>�
D?�D?�
D@
D@��DA�DA�
DB�DB�>DC
>DC��DD�DD��DE
>DE�>DF
>DF�>DG
>DG��DH�DH�>DI�DI��DJ
>DJ�>DK
>DK�>DL�DL�>DM�DM��DN
>DN��DO
>DO�>DP�DP��DQ
>DQ�>DR�DR�>DS�DS��DT�DT��DU�DU��DV
>DV�>DW�DW��DX
>DX�>DY�DY��DZ�DZ�
D[
D[�
D\
D\�
D]
D]��D^
>D^�>D_�D_��D`
>D`�>Da
>Da�
Db�Db�>Dc�Dc��Dd
>Dd�>De
>De��Df�Df��Dg�Dg��Dh
Dh�>Di�Di��Dj�Dj��Dk
>Dk��Dl
>Dl�>Dm�Dm�
Dn
Dn��Do
>Do�>Dp
Dp�
Dq
Dq��Dr
>Dr��Ds�Ds��Dt�Dt�
Du�Du�>Dv�Dv��Dw�Dw��Dw�qDy�HD�7�D�˅111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AظRAؼjAؼjAؼjA���AؼjAؾwAظRAغ^AظRAظRAش9AضFAؼjAغ^Aز-Aا�A�A�7LA���Aҥ�A�XA�bNA�{AˍPAʬA���A��A�C�A�VA���A�M�A�ȴAÇ+A�33A�  A��A��/AA�1A��wA�p�A�
=A�ZA���A���A��wA�M�A��-A�A�?}A��uA���A���A��A�%A��/A��#A�+A��9A��A��A��A��FA�t�A��A��hA�JA���A�ffA��A�+A�VA�-A��/A�/A�z�A�VA�bNA��uA��A�`BA�?}A�|�A���A�bA�"�A�bA�n�A��A�?}A��A��;A�?}A��A��^A��#A���A�"�A���A|I�A{��Aw�As/AqXAn=qAj��AiC�Ah=qAdA�A_�mA^�A]|�A\�!A\bNA\-A[+AY��AXbNAW�TAWAW�wAW�-AV�`AT�`AS�AQ`BAP1'AO&�AN��AN�DAM�;AM�AK�hAH��AD�/ACt�AB{AA�wAA�AA�A?�#A=��A=?}A=%A;��A:jA8�A8ffA6v�A4$�A1x�A/�A/x�A.��A.ZA,�uA)�A'�A&�uA%�A$v�A#�wA#"�A!+A��A�AȴA�
A�A��A�A1A�;AXA�A9XA�AJA�FAl�A��A�yAbAĜA��A%AE�AG�A
~�A
�A	��A��A�A�A�7A�A�`A�9A~�A5?At�A��A�A �yA n�A J@���@��@�hs@��@��@�`B@�l�@���@�n�@�E�@�5?@�J@�@�/@��@��@��@�\@�J@�1@�@�@�;d@�@���@��#@��@�@��@�G�@�9@�Z@�^5@��@���@ى7@���@֏\@�@ա�@�x�@�j@�l�@�;d@�+@�@҇+@���@ѩ�@љ�@�G�@�%@�Ĝ@��@θR@��@���@�O�@���@�V@���@�j@���@�C�@�ȴ@���@�V@�Z@��m@ǍP@�\)@Ƈ+@���@�Z@� �@Å@��H@�v�@�@���@�x�@�G�@���@�1@���@�v�@���@�O�@��@��@���@�dZ@�
=@���@���@��\@�E�@�=q@��@���@���@�x�@���@�Z@��@��P@�@���@�M�@��@��#@���@�/@��@��@��@��u@�Z@��@�t�@�33@���@�-@�@��@�x�@�X@���@�b@���@�dZ@�
=@�@���@��y@�ȴ@�ff@�J@��-@�p�@�?}@�7L@�/@�&�@��@�V@���@���@�j@�1'@�1@���@��m@��
@���@��P@�+@�~�@��@�@���@��@��`@��9@�1'@���@�C�@���@�@���@�z�@�(�@�  @���@��F@�+@��@�
=@���@�E�@���@��^@���@��7@��7@��7@�x�@�G�@�?}@�b@��m@��@�o@�ff@�E�@�$�@�@��@��^@���@�hs@�?}@���@��j@�Ĝ@��9@���@�(�@���@�"�@���@���@��@�/@��j@���@�z�@�Q�@�1'@�9X@�I�@�b@���@��F@��@��R@�$�@�J@��T@��-@���@���@���@��@�V@�M�@���@�/@��`@���@�z�@�b@��w@���@���@�t�@�\)@�;d@�ȴ@���@�`B@�%@��@��@���@�j@���@��F@���@��;@��;@��w@�S�@�33@�ȴ@���@�x�@��@���@��9@��D@���@�Ĝ@� �@���@���@�J@��@���@���@�X@��@���@��u@�1'@��@�  @���@��@~4@nL0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AظRAؼjAؼjAؼjA���AؼjAؾwAظRAغ^AظRAظRAش9AضFAؼjAغ^Aز-Aا�A�A�7LA���Aҥ�A�XA�bNA�{AˍPAʬA���A��A�C�A�VA���A�M�A�ȴAÇ+A�33A�  A��A��/AA�1A��wA�p�A�
=A�ZA���A���A��wA�M�A��-A�A�?}A��uA���A���A��A�%A��/A��#A�+A��9A��A��A��A��FA�t�A��A��hA�JA���A�ffA��A�+A�VA�-A��/A�/A�z�A�VA�bNA��uA��A�`BA�?}A�|�A���A�bA�"�A�bA�n�A��A�?}A��A��;A�?}A��A��^A��#A���A�"�A���A|I�A{��Aw�As/AqXAn=qAj��AiC�Ah=qAdA�A_�mA^�A]|�A\�!A\bNA\-A[+AY��AXbNAW�TAWAW�wAW�-AV�`AT�`AS�AQ`BAP1'AO&�AN��AN�DAM�;AM�AK�hAH��AD�/ACt�AB{AA�wAA�AA�A?�#A=��A=?}A=%A;��A:jA8�A8ffA6v�A4$�A1x�A/�A/x�A.��A.ZA,�uA)�A'�A&�uA%�A$v�A#�wA#"�A!+A��A�AȴA�
A�A��A�A1A�;AXA�A9XA�AJA�FAl�A��A�yAbAĜA��A%AE�AG�A
~�A
�A	��A��A�A�A�7A�A�`A�9A~�A5?At�A��A�A �yA n�A J@���@��@�hs@��@��@�`B@�l�@���@�n�@�E�@�5?@�J@�@�/@��@��@��@�\@�J@�1@�@�@�;d@�@���@��#@��@�@��@�G�@�9@�Z@�^5@��@���@ى7@���@֏\@�@ա�@�x�@�j@�l�@�;d@�+@�@҇+@���@ѩ�@љ�@�G�@�%@�Ĝ@��@θR@��@���@�O�@���@�V@���@�j@���@�C�@�ȴ@���@�V@�Z@��m@ǍP@�\)@Ƈ+@���@�Z@� �@Å@��H@�v�@�@���@�x�@�G�@���@�1@���@�v�@���@�O�@��@��@���@�dZ@�
=@���@���@��\@�E�@�=q@��@���@���@�x�@���@�Z@��@��P@�@���@�M�@��@��#@���@�/@��@��@��@��u@�Z@��@�t�@�33@���@�-@�@��@�x�@�X@���@�b@���@�dZ@�
=@�@���@��y@�ȴ@�ff@�J@��-@�p�@�?}@�7L@�/@�&�@��@�V@���@���@�j@�1'@�1@���@��m@��
@���@��P@�+@�~�@��@�@���@��@��`@��9@�1'@���@�C�@���@�@���@�z�@�(�@�  @���@��F@�+@��@�
=@���@�E�@���@��^@���@��7@��7@��7@�x�@�G�@�?}@�b@��m@��@�o@�ff@�E�@�$�@�@��@��^@���@�hs@�?}@���@��j@�Ĝ@��9@���@�(�@���@�"�@���@���@��@�/@��j@���@�z�@�Q�@�1'@�9X@�I�@�b@���@��F@��@��R@�$�@�J@��T@��-@���@���@���@��@�V@�M�@���@�/@��`@���@�z�@�b@��w@���@���@�t�@�\)@�;d@�ȴ@���@�`B@�%@��@��@���@�j@���@��F@���@��;@��;@��w@�S�@�33@�ȴ@���@�x�@��@���@��9@��D@���@�Ĝ@� �@���@���@�J@��@���@���@�X@��@���@��u@�1'@��@�  @���@��@~4@nL0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BƨBÖB��BŢB�B�B�BB�B��B��B+B�B7LBA�BL�BQ�BW
B[#B[#B\)Be`Bl�Bo�Br�Bv�B{�B� B�B�DB�VB�bB�hB�\B�\B�oB��B��B��B��B��B��B��B�oB�+B|�Br�BiyBcTB_;BYBQ�B@�B%�B�BB��B�B�yBȴB�^B�B�hBr�BP�B8RB&�B�BJB
��B
�B
�sB
�TB
�
B
��B
�B
��B
ƨB
��B
�'B
��B
�7B
hsB
@�B
6FB
�B
  B	�B	�#B	ǮB	�^B	�!B	��B	|�B	u�B	q�B	p�B	q�B	p�B	hsB	]/B	VB	T�B	W
B	YB	ZB	XB	O�B	G�B	?}B	6FB	0!B	0!B	2-B	/B	+B	�B	JB�B�fB�;B�/B�#B�B��B��B��B��B��BƨBB�wB�RB�B��B��B��B��B��B��B��B�uB�uB�oB�hB�\B�DB�+B�B�B~�B|�B|�B{�B{�B|�Bz�By�Bw�Bs�Bp�Bn�Bl�BjBe`B`BB]/BXBS�BM�BI�BG�BE�BC�BB�BF�BK�BM�BQ�BT�BVBW
BXBYB\)B]/B`BBffBgmBgmBgmBjBk�Bl�Bm�Bu�By�B{�B{�B|�B|�B|�B|�B|�B{�Bz�Bx�Bx�B�B� B�B�hB�{B��B��B��B��B�B�-B�9B�9B�3B�FB�RB�XB�^B�jB�}B��B��B��BBĜBŢBŢBƨBȴB��B��B��B��B��B��B��B��B��B��B�B�)B�5B�;B�HB�HB�HB�HB�BB�;B�HB�sB�sB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	B	B	%B	DB	uB	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	&�B	(�B	-B	/B	1'B	49B	6FB	7LB	9XB	<jB	>wB	A�B	B�B	C�B	D�B	E�B	E�B	G�B	I�B	J�B	N�B	P�B	T�B	W
B	XB	XB	ZB	^5B	bNB	gmB	k�B	k�B	l�B	m�B	o�B	v�B	z�B	~�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�DB	�JB	�PB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�?B	�?B	�?B	�LB	�RB	�XB	�^B	�jB	�qB	�qB	�qB	�qB	�wB	�wB	�wB	�wB	�wB	�qB	�qB	�wB	�wB	B	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�;B	�ZB	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
EB
%F222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BƨBÖB��BŢB�B�B�BB�B��B��B+B�B7LBA�BL�BQ�BW
B[#B[#B\)Be`Bl�Bo�Br�Bv�B{�B� B�B�DB�VB�bB�hB�\B�\B�oB��B��B��B��B��B��B��B�oB�+B|�Br�BiyBcTB_;BYBQ�B@�B%�B�BB��B�B�yBȴB�^B�B�hBr�BP�B8RB&�B�BJB
��B
�B
�sB
�TB
�
B
��B
�B
��B
ƨB
��B
�'B
��B
�7B
hsB
@�B
6FB
�B
  B	�B	�#B	ǮB	�^B	�!B	��B	|�B	u�B	q�B	p�B	q�B	p�B	hsB	]/B	VB	T�B	W
B	YB	ZB	XB	O�B	G�B	?}B	6FB	0!B	0!B	2-B	/B	+B	�B	JB�B�fB�;B�/B�#B�B��B��B��B��B��BƨBB�wB�RB�B��B��B��B��B��B��B��B�uB�uB�oB�hB�\B�DB�+B�B�B~�B|�B|�B{�B{�B|�Bz�By�Bw�Bs�Bp�Bn�Bl�BjBe`B`BB]/BXBS�BM�BI�BG�BE�BC�BB�BF�BK�BM�BQ�BT�BVBW
BXBYB\)B]/B`BBffBgmBgmBgmBjBk�Bl�Bm�Bu�By�B{�B{�B|�B|�B|�B|�B|�B{�Bz�Bx�Bx�B�B� B�B�hB�{B��B��B��B��B�B�-B�9B�9B�3B�FB�RB�XB�^B�jB�}B��B��B��BBĜBŢBŢBƨBȴB��B��B��B��B��B��B��B��B��B��B�B�)B�5B�;B�HB�HB�HB�HB�BB�;B�HB�sB�sB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	B	B	%B	DB	uB	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	&�B	(�B	-B	/B	1'B	49B	6FB	7LB	9XB	<jB	>wB	A�B	B�B	C�B	D�B	E�B	E�B	G�B	I�B	J�B	N�B	P�B	T�B	W
B	XB	XB	ZB	^5B	bNB	gmB	k�B	k�B	l�B	m�B	o�B	v�B	z�B	~�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�DB	�JB	�PB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�?B	�?B	�?B	�LB	�RB	�XB	�^B	�jB	�qB	�qB	�qB	�qB	�wB	�wB	�wB	�wB	�wB	�qB	�qB	�wB	�wB	B	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�;B	�ZB	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
EB
%F222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191703                              AO  ARCAADJP                                                                    20181005191703    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191703  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191703  QCF$                G�O�G�O�G�O�8000            