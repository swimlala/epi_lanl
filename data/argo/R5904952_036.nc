CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:14Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190514  20181005190514  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               $A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׸e���1   @׸e��I�@1�I�^�c�hr� �1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      $A   A   A   @@  @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC�fC  C�fC�fC  C  C   C"  C#�fC%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Ck�fCm�fCp  Cr  Ct�Cv  Cw�fCz  C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C��C��C�  C��3C��3C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D��Dy�DfD�fD  D� D  D� D  D� D  D� D  Dy�D	  D	� D
fD
� D  D� DfD� D  D� D  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D  Dy�D��D� DfD� D  D�fD  D� D  D� D  D� DfD� D��Dy�D  D� D  Dy�D��D � D!  D!� D"  D"y�D"��D#� D$  D$� D%fD%� D%��D&y�D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D8��D9y�D:  D:� D;  D;�fD<  D<y�D=  D=� D>  D>y�D>��D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DEfDE� DF  DF� DF��DG� DH  DH� DIfDI� DI��DJ� DKfDK� DK��DL� DM  DM� DM��DN� DO  DO� DPfDP� DP��DQ� DR  DR� DSfDS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`y�Da  Day�Da��Dby�Db��Dc� DdfDd�fDe  Dey�De��Df� Dg  Dg�fDh  Dhy�Di  Di�fDj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn�fDo  Doy�Dp  Dp� Dp��Dq� DrfDr� Ds  Ds� Dt  Dt� DufDu�fDu��Dv� Dw  Dw� Dw��Dy��D�;3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Dz�@�p�@�=q@�
>A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A��\A�\)B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C �C�C�C�C�C
�C�C�C�C�RC�RC�C�RC�RC�C�C �C"�C#�RC%�RC(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ+�CL+�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Ca�RCd�Cf�Ch�Cj�Ck�RCm�RCp�Cr�Ct+�Cv�Cw�RCz�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��)C��)C��C��C��C��C��C��C��C��C��C��C��)C��)C��C��C��C��)C��)C��)C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��)C��)C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D�D~D
�D��D{D�{D{D�{D{D�{D{D�{D{D~D	{D	�{D

�D
�{D{D�{D
�D�{D{D�{D{D~D{D�{D{D��D{D�{D{D�{D{D�{D{D�{D{D~D�D�{D
�D�{D{D��D{D�{D{D�{D{D�{D
�D�{D�D~D{D�{D{D~D�D �{D!{D!�{D"{D"~D"�D#�{D${D$�{D%
�D%�{D%�D&~D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D1�D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8~D8�D9~D:{D:�{D;{D;��D<{D<~D={D=�{D>{D>~D>�D?�{D@{D@�{DA{DA�{DB{DB��DC{DC�{DD{DD�{DE
�DE�{DF{DF�{DF�DG�{DH{DH�{DI
�DI�{DI�DJ�{DK
�DK�{DK�DL�{DM{DM�{DM�DN�{DO{DO�{DP
�DP�{DP�DQ�{DR{DR�{DS
�DS�{DT{DT~DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]~D^{D^�{D_{D_�{D`{D`~Da{Da~Da�Db~Db�Dc�{Dd
�Dd��De{De~De�Df�{Dg{Dg��Dh{Dh~Di{Di��Dj{Dj�{Dk{Dk�{Dl{Dl~Dm{Dm�{Dn{Dn��Do{Do~Dp{Dp�{Dp�Dq�{Dr
�Dr�{Ds{Ds�{Dt{Dt�{Du
�Du��Du�Dv�{Dw{Dw�{Dw�HDy�HD�=pD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�hsA�hsA�jA�n�A�t�A�p�A�z�A�|�A�z�A�|�A�|�AԁAԁAԃAԃAԃAԅAԃAԃAԅAԅAԇ+Aԇ+Aԉ7Aԉ7Aԉ7Aԉ7AԋDAԋDAԍPAԍPAԍPAԏ\AԑhAԑhAԓuAԅA�ȴA���A���AΑhA�oA�~�A��A�+AˋDA˃A�r�A�;dA��HA�?}A�hsA���A�r�A��Aǩ�AƧ�A�ƨA�+A��AċDA�%A�JA�hsA��#A��hA�Q�A�/A��A�ffA���A�`BA�M�A��A��yA��A�ĜA��A��TA�dZA�z�A�ƨA��A��A�
=A�
=A�=qA�7LA��HA�I�A�VA�E�A��+A��\A��jA��A���A��wA�XA�~�A���A�r�A�^5A�?}A��TA���A��+A�
=A|�Aw�PAt��Aq��An=qAlv�AkVAi��Ah��Afz�AdffAaA_A]�A\  A[oAYl�AWoAV��AVI�AQ33AN�uAM�FAMS�AL^5AJffAH��AH  ADȴABA�AA��A?�hA:�+A8ffA8�A7/A6 �A5XA4��A4�A4A�A3S�A0z�A.z�A-�hA,��A+p�A*�A'�FA&�`A&�+A%��A$ZA"=qA �A��A+Ar�A�FA1AffAC�A�!A�`AE�A=qAr�A��A?}AdZA�A�A��A�yA
��A
(�A
  A	�A	A�\Ap�A��A��A�AO�A;dA%A��AAĜAZA7LA��AJA{A��A�A��Ax�A�A �AVA(�A�7A&�AȴAZA(�AƨAbA(�A��A 9XA (�A �\A A�@�l�@��@���@�z�@�ff@�J@�C�@�+@���@�9X@�Q�@� �@�\)@��y@�@�ȴ@�\@�D@�V@�I�@�"�@�S�@�$�@�bN@�dZ@ڏ\@�E�@ӕ�@ӥ�@�1@��
@���@ӍP@�"�@Л�@��
@ЋD@��H@�n�@��@�C�@���@���@��H@ȋD@�V@�^5@�Ĝ@�$�@���@�1@�1@��;@��#@�1'@�E�@�o@�~�@�J@�j@Ý�@���@�x�@�/@���@�t�@��R@��#@��@�l�@��#@�%@��@���@���@��/@��@�1@�n�@��#@���@���@�%@�j@�9X@�j@���@���@���@��9@��/@�^5@��!@���@�o@���@�  @�%@��@���@�j@�9X@��@�l�@�1@�Q�@�1@��@�K�@���@��!@�ff@��@��@�7L@���@���@��@�r�@�A�@�(�@�1@���@�|�@�l�@�|�@�33@���@�V@�p�@��@��9@���@��u@��@�bN@�Z@�Q�@�I�@�9X@��@���@��P@��@�C�@�+@��H@�-@��7@��@��u@�A�@�b@��@���@��@��\@�^5@��@��-@�7L@��@��u@�Z@�(�@���@��w@�C�@��@��+@�$�@��T@��-@��@��@���@��@�dZ@��\@�M�@��@��@��7@�V@�%@��/@��m@�\)@�dZ@�|�@��H@��+@�5?@���@��@��@�b@���@�"�@��@�
=@�@�
=@���@��+@�~�@��+@�~�@���@�&�@�Ĝ@��m@���@�K�@�+@���@�J@��@���@��h@�x�@�V@��@���@��u@��D@�z�@�r�@�I�@��@�ƨ@��@���@��\@�v�@�5?@���@��7@�x�@�O�@�7L@��@���@��`@�Ĝ@���@�9X@���@���@��P@�K�@�ȴ@��+@��+@��@��^@��@�/@�%@��@��@�j@�Z@�9X@�1@��m@���@�t�@�;d@��@���@�4@}ԕ@n�M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ffA�hsA�hsA�jA�n�A�t�A�p�A�z�A�|�A�z�A�|�A�|�AԁAԁAԃAԃAԃAԅAԃAԃAԅAԅAԇ+Aԇ+Aԉ7Aԉ7Aԉ7Aԉ7AԋDAԋDAԍPAԍPAԍPAԏ\AԑhAԑhAԓuAԅA�ȴA���A���AΑhA�oA�~�A��A�+AˋDA˃A�r�A�;dA��HA�?}A�hsA���A�r�A��Aǩ�AƧ�A�ƨA�+A��AċDA�%A�JA�hsA��#A��hA�Q�A�/A��A�ffA���A�`BA�M�A��A��yA��A�ĜA��A��TA�dZA�z�A�ƨA��A��A�
=A�
=A�=qA�7LA��HA�I�A�VA�E�A��+A��\A��jA��A���A��wA�XA�~�A���A�r�A�^5A�?}A��TA���A��+A�
=A|�Aw�PAt��Aq��An=qAlv�AkVAi��Ah��Afz�AdffAaA_A]�A\  A[oAYl�AWoAV��AVI�AQ33AN�uAM�FAMS�AL^5AJffAH��AH  ADȴABA�AA��A?�hA:�+A8ffA8�A7/A6 �A5XA4��A4�A4A�A3S�A0z�A.z�A-�hA,��A+p�A*�A'�FA&�`A&�+A%��A$ZA"=qA �A��A+Ar�A�FA1AffAC�A�!A�`AE�A=qAr�A��A?}AdZA�A�A��A�yA
��A
(�A
  A	�A	A�\Ap�A��A��A�AO�A;dA%A��AAĜAZA7LA��AJA{A��A�A��Ax�A�A �AVA(�A�7A&�AȴAZA(�AƨAbA(�A��A 9XA (�A �\A A�@�l�@��@���@�z�@�ff@�J@�C�@�+@���@�9X@�Q�@� �@�\)@��y@�@�ȴ@�\@�D@�V@�I�@�"�@�S�@�$�@�bN@�dZ@ڏ\@�E�@ӕ�@ӥ�@�1@��
@���@ӍP@�"�@Л�@��
@ЋD@��H@�n�@��@�C�@���@���@��H@ȋD@�V@�^5@�Ĝ@�$�@���@�1@�1@��;@��#@�1'@�E�@�o@�~�@�J@�j@Ý�@���@�x�@�/@���@�t�@��R@��#@��@�l�@��#@�%@��@���@���@��/@��@�1@�n�@��#@���@���@�%@�j@�9X@�j@���@���@���@��9@��/@�^5@��!@���@�o@���@�  @�%@��@���@�j@�9X@��@�l�@�1@�Q�@�1@��@�K�@���@��!@�ff@��@��@�7L@���@���@��@�r�@�A�@�(�@�1@���@�|�@�l�@�|�@�33@���@�V@�p�@��@��9@���@��u@��@�bN@�Z@�Q�@�I�@�9X@��@���@��P@��@�C�@�+@��H@�-@��7@��@��u@�A�@�b@��@���@��@��\@�^5@��@��-@�7L@��@��u@�Z@�(�@���@��w@�C�@��@��+@�$�@��T@��-@��@��@���@��@�dZ@��\@�M�@��@��@��7@�V@�%@��/@��m@�\)@�dZ@�|�@��H@��+@�5?@���@��@��@�b@���@�"�@��@�
=@�@�
=@���@��+@�~�@��+@�~�@���@�&�@�Ĝ@��m@���@�K�@�+@���@�J@��@���@��h@�x�@�V@��@���@��u@��D@�z�@�r�@�I�@��@�ƨ@��@���@��\@�v�@�5?@���@��7@�x�@�O�@�7L@��@���@��`@�Ĝ@���@�9X@���@���@��P@�K�@�ȴ@��+@��+@��@��^@��@�/@�%@��@��@�j@�Z@�9X@�1@��m@���@�t�@�;d@��@���@�4@}ԕ@n�M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
gmB
hsB
hsB
hsB
hsB
gmB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
hsB
gmB
hsB
hsB
hsB
gmB
gmB
ffB
e`B
t�B
ȴB
�/B
�mB
�sB
�B
�B
�B
��BB+B�B,B`BB~�B��B�qB��B��B�B��BuB�B�B{BDB1B�B!�B&�B'�B1'B;dBL�B�%B��B�VBz�B�oB��B�dB�}B��B�wB�^B��B�+BQ�B!�B�fB�B��B��B�Bw�Bl�B^5B9XBJB
�5B
��B
|�B
p�B
ffB
ffB
gmB
iyB
m�B
k�B
%�B
JB	�fB	ɺB	�'B	��B	��B	�{B	��B	�bB	�1B	z�B	l�B	[#B	K�B	@�B	8RB	1'B	(�B	�B	�B	uB	1B��B��B��B�B�B�mB�ZB�/B��B��B��B�jB�FB�?B�3B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�RBB��B�B�
B�B�B�BB��B	DB	�B	�B	"�B	$�B	$�B	)�B	-B	+B	/B	5?B	7LB	9XB	:^B	9XB	7LB	7LB	9XB	/B	33B	33B	%�B	.B	J�B	M�B	M�B	G�B	H�B	:^B	2-B	)�B	�B	�B	�B	�B	�B	�B	�B	#�B	33B	5?B	33B	-B	#�B	!�B	 �B	#�B	%�B	 �B	�B	�B	PB	B	DB	uB	�B	{B	�B	�B	�B	�B	!�B	�B	�B	�B	uB	JB		7B	
=B	�B	!�B	#�B	"�B	,B	1'B	<jB	@�B	A�B	=qB	9XB	5?B	>wB	=qB	>wB	:^B	7LB	6FB	=qB	@�B	D�B	D�B	B�B	?}B	<jB	:^B	9XB	;dB	<jB	A�B	J�B	P�B	Q�B	Q�B	P�B	P�B	R�B	VB	XB	YB	`BB	e`B	hsB	m�B	o�B	p�B	v�B	�1B	�DB	�PB	�\B	�uB	��B	��B	��B	��B	��B	�B	�!B	�'B	�LB	�dB	�qB	�wB	�}B	��B	B	B	B	ÖB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�;B	�;B	�;B	�;B	�BB	�BB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
%B
%B
+B
1B
�B
EB
(�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
gmB
hsB
hsB
hsB
hsB
gmB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
hsB
gmB
hsB
hsB
hsB
gmB
gmB
ffB
e`B
t�B
ȴB
�/B
�mB
�sB
�B
�B
�B
��BB+B�B,B`BB~�B��B�qB��B��B�B��BuB�B�B{BDB1B�B!�B&�B'�B1'B;dBL�B�%B��B�VBz�B�oB��B�dB�}B��B�wB�^B��B�+BQ�B!�B�fB�B��B��B�Bw�Bl�B^5B9XBJB
�5B
��B
|�B
p�B
ffB
ffB
gmB
iyB
m�B
k�B
%�B
JB	�fB	ɺB	�'B	��B	��B	�{B	��B	�bB	�1B	z�B	l�B	[#B	K�B	@�B	8RB	1'B	(�B	�B	�B	uB	1B��B��B��B�B�B�mB�ZB�/B��B��B��B�jB�FB�?B�3B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�RBB��B�B�
B�B�B�BB��B	DB	�B	�B	"�B	$�B	$�B	)�B	-B	+B	/B	5?B	7LB	9XB	:^B	9XB	7LB	7LB	9XB	/B	33B	33B	%�B	.B	J�B	M�B	M�B	G�B	H�B	:^B	2-B	)�B	�B	�B	�B	�B	�B	�B	�B	#�B	33B	5?B	33B	-B	#�B	!�B	 �B	#�B	%�B	 �B	�B	�B	PB	B	DB	uB	�B	{B	�B	�B	�B	�B	!�B	�B	�B	�B	uB	JB		7B	
=B	�B	!�B	#�B	"�B	,B	1'B	<jB	@�B	A�B	=qB	9XB	5?B	>wB	=qB	>wB	:^B	7LB	6FB	=qB	@�B	D�B	D�B	B�B	?}B	<jB	:^B	9XB	;dB	<jB	A�B	J�B	P�B	Q�B	Q�B	P�B	P�B	R�B	VB	XB	YB	`BB	e`B	hsB	m�B	o�B	p�B	v�B	�1B	�DB	�PB	�\B	�uB	��B	��B	��B	��B	��B	�B	�!B	�'B	�LB	�dB	�qB	�wB	�}B	��B	B	B	B	ÖB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�;B	�;B	�;B	�;B	�BB	�BB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
%B
%B
+B
1B
�B
EB
(�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190514                              AO  ARCAADJP                                                                    20181005190514    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190514  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190514  QCF$                G�O�G�O�G�O�8000            