CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:14Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190614  20181005190614  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              .A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�kJ�n�1   @�k�>��@03t�j~��cv��"��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     .A   A   A   @�33@�  A   A   A@  A^ffA�  A�  A�  A�  A���A�  A�  A�  B   BffBffB  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCo�fCr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C��C�  C��3C��3C�  C�  C�  C��C�  C��C�  C�  D   D � D  D� D  D�fD  Dy�D  D�fD  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  Dy�D��D� D  D� D  D� DfD�fD  D�fD  D� D  Dy�D��Dy�D��D� D  D� D  Dy�D  D� D  D� D��D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"y�D"��D#� D$  D$y�D$��D%y�D&  D&� D'  D'� D(  D(�fD)  D)� D*fD*�fD+fD+�fD,fD,� D-  D-� D.  D.� D.��D/y�D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5�fD6  D6� D7  D7� D7��D8� D9fD9� D9��D:� D;fD;� D<  D<�fD=  D=y�D>fD>� D?  D?� D@fD@� DA  DA� DB  DBy�DB��DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DIfDI�fDJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQfDQ� DR  DR� DS  DS�fDT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[  D[y�D\  D\� D]fD]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dm��Dny�Do  Do� Dp  Dpy�Dq  Dq�fDrfDr�fDs  Ds� Dt  Dty�Du  Du�fDvfDv�fDwfDw�fDw�fDy��D�A�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A(�A$(�AD(�Ab�\A�{A�{A�{A�{A��HA�{A�{A�{B
=B	p�Bp�B
=B!
=B(��B1
=B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��B��B��B�Q�B�Q�B��B��B��RB��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�C(�CB�CB�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>\)C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CR\)CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�Cn(�Cp(�CrB�CtB�CvB�Cx\)CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�{C�!HC�!HC�.C�!HC�{C�{C�!HC�!HC�!HC�.C�!HC�.C�!HC�!HD �D ��D�D��D�D�
D�D�>D�D�
D�D��D�D��D�D��D�D�
D	�D	��D
�D
��D�D��D�D�>D
>D��D�D��D�D��D
D�
D�D�
D�D��D�D�>D
>D�>D
>D��D�D��D�D�>D�D��D�D��D
>D��D�D��D�D��D�D��D
>D��D�D��D �D ��D!�D!��D"�D"�>D#
>D#��D$�D$�>D%
>D%�>D&�D&��D'�D'��D(�D(�
D)�D)��D*
D*�
D+
D+�
D,
D,��D-�D-��D.�D.��D/
>D/�>D0�D0��D1�D1��D2�D2��D3�D3�
D4�D4��D5�D5�
D6�D6��D7�D7��D8
>D8��D9
D9��D:
>D:��D;
D;��D<�D<�
D=�D=�>D>
D>��D?�D?��D@
D@��DA�DA��DB�DB�>DC
>DC��DD�DD��DE�DE��DF
DF��DG�DG��DH�DH��DI
DI�
DJ�DJ�
DK�DK��DL�DL��DM�DM��DN�DN��DO
>DO��DP�DP��DQ
DQ��DR�DR��DS�DS�
DT�DT�>DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ�
D[�D[�>D\�D\��D]
D]��D^�D^�>D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di�
Dj�Dj��Dk�Dk��Dl�Dl�>Dm�Dm��Dn
>Dn�>Do�Do��Dp�Dp�>Dq�Dq�
Dr
Dr�
Ds�Ds��Dt�Dt�>Du�Du�
Dv
Dv�
Dw
Dw�
Dw�
Dy�gD�J>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�5?A�33A�/A�1'A�-A�;dA�E�A�?}A�E�A�G�A�E�A�G�A�C�A�A�A�I�A�?}A�=qA�=qA�;dA�;dA�5?A�33A�+A��A���A˾wA��A�n�A�1A�1A�ĜA�?}A���A��A�$�A��+A�E�A�bNA���A��A��A���A�-A���A�A�|�A�%A��+A��;A��FA�p�A�{A��^A�r�A� �A���A�?}A��wA�O�A�A�(�A��A�$�A��7A�-A�bA�XA��A�oA�$�A�ȴA��jA��mA��RA�A�oA�{A���A�A�ƨA�E�A�-A��A�ƨA�A���A�7LA�`BA��A�x�A�Q�A�ĜA��HA�1'A��A���A��wA�x�A��A�r�A�XA~E�Aq33Ao`BAnjAhA�Ab��AZbAS��AP1'AKO�AF�ADȴACƨAC\)AC"�AB �AB  ABJAAp�A@bNA@ �A?p�A>��A=�mA=dZA<ĜA;�;A:�9A9�A8�!A7��A5��A37LA2�A2A�A1��A0ffA/��A/l�A/XA.�A-ƨA+7LA'��A$�!A#XA"��A"ZA!��A!�A;dAhsA�A��A"�AĜA�A�A�A �A��A��AVA��AA`BAXA��A�\A  A�A~�AA�A�#A
��A	�mA�!A�A�#A?}A��A��A9XA�
AS�Ar�A\)A ��A �DA z�A ~�A 1'@�K�@��+@��@��@���@�;d@��@�5?@���@�@��`@��+@�ff@�@�ȴ@�hs@��@���@���@��H@���@�Q�@�|�@�|�@��@�bN@��@��@�G�@�  @�@�F@�F@㕁@�|�@�+@��y@��@�v�@�{@��/@ߕ�@ޗ�@��@�O�@� �@�S�@ڰ!@�?}@֧�@�V@�C�@�33@�  @�X@ղ-@���@��#@��#@��#@�@�V@���@��;@�^5@��T@���@Ѳ-@�hs@с@ѩ�@ѩ�@�&�@���@�Ĝ@Ѓ@��@υ@�+@Η�@�V@�p�@���@�(�@�@�G�@ǥ�@Ƨ�@�@��@��/@ģ�@�j@�ƨ@�33@�-@��@�V@�@��D@�"�@�E�@��@��@�ƨ@��w@�|�@��H@��@��@�9X@�"�@��@���@�X@���@�/@�1'@��@�K�@�dZ@�K�@�S�@�
=@��y@���@��7@���@���@�/@�%@��/@��u@�bN@���@��@�"�@��@��R@��\@�=q@�-@�@���@�X@��j@� �@���@�S�@��@�~�@�5?@��T@���@���@��@�O�@�?}@���@���@�A�@�1'@�b@��;@��P@�l�@�S�@�+@��@��y@���@��@���@�p�@�?}@���@��@�Z@�1'@��@��m@��@�\)@��@��!@�^5@�=q@���@�7L@��@��u@��m@�
=@��y@��H@���@��!@�V@��@��^@�`B@��`@���@�(�@���@��@�l�@�K�@�33@�o@���@���@�^5@�E�@��@���@���@��@�O�@�&�@��`@��D@�I�@��@�  @��
@�l�@�S�@�C�@�o@��R@��+@�^5@�-@��@��@�J@���@��+@���@��h@���@���@��/@���@�Ĝ@��/@��@���@�Ĝ@�z�@��@�ƨ@��@�
=@��y@���@��+@�ff@�M�@���@��^@���@��7@�7L@��@���@���@���@���@�Q�@�9X@�  @�C�@��H@���@�-@�&�@���@�bN@�1'@��m@���@�o@��@���@��\@�M�@��@�J@��#@��-@���@�G�@���@�z�@�A�@��;@��P@���@�\)@�+@�E9@{!-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�5?A�33A�/A�1'A�-A�;dA�E�A�?}A�E�A�G�A�E�A�G�A�C�A�A�A�I�A�?}A�=qA�=qA�;dA�;dA�5?A�33A�+A��A���A˾wA��A�n�A�1A�1A�ĜA�?}A���A��A�$�A��+A�E�A�bNA���A��A��A���A�-A���A�A�|�A�%A��+A��;A��FA�p�A�{A��^A�r�A� �A���A�?}A��wA�O�A�A�(�A��A�$�A��7A�-A�bA�XA��A�oA�$�A�ȴA��jA��mA��RA�A�oA�{A���A�A�ƨA�E�A�-A��A�ƨA�A���A�7LA�`BA��A�x�A�Q�A�ĜA��HA�1'A��A���A��wA�x�A��A�r�A�XA~E�Aq33Ao`BAnjAhA�Ab��AZbAS��AP1'AKO�AF�ADȴACƨAC\)AC"�AB �AB  ABJAAp�A@bNA@ �A?p�A>��A=�mA=dZA<ĜA;�;A:�9A9�A8�!A7��A5��A37LA2�A2A�A1��A0ffA/��A/l�A/XA.�A-ƨA+7LA'��A$�!A#XA"��A"ZA!��A!�A;dAhsA�A��A"�AĜA�A�A�A �A��A��AVA��AA`BAXA��A�\A  A�A~�AA�A�#A
��A	�mA�!A�A�#A?}A��A��A9XA�
AS�Ar�A\)A ��A �DA z�A ~�A 1'@�K�@��+@��@��@���@�;d@��@�5?@���@�@��`@��+@�ff@�@�ȴ@�hs@��@���@���@��H@���@�Q�@�|�@�|�@��@�bN@��@��@�G�@�  @�@�F@�F@㕁@�|�@�+@��y@��@�v�@�{@��/@ߕ�@ޗ�@��@�O�@� �@�S�@ڰ!@�?}@֧�@�V@�C�@�33@�  @�X@ղ-@���@��#@��#@��#@�@�V@���@��;@�^5@��T@���@Ѳ-@�hs@с@ѩ�@ѩ�@�&�@���@�Ĝ@Ѓ@��@υ@�+@Η�@�V@�p�@���@�(�@�@�G�@ǥ�@Ƨ�@�@��@��/@ģ�@�j@�ƨ@�33@�-@��@�V@�@��D@�"�@�E�@��@��@�ƨ@��w@�|�@��H@��@��@�9X@�"�@��@���@�X@���@�/@�1'@��@�K�@�dZ@�K�@�S�@�
=@��y@���@��7@���@���@�/@�%@��/@��u@�bN@���@��@�"�@��@��R@��\@�=q@�-@�@���@�X@��j@� �@���@�S�@��@�~�@�5?@��T@���@���@��@�O�@�?}@���@���@�A�@�1'@�b@��;@��P@�l�@�S�@�+@��@��y@���@��@���@�p�@�?}@���@��@�Z@�1'@��@��m@��@�\)@��@��!@�^5@�=q@���@�7L@��@��u@��m@�
=@��y@��H@���@��!@�V@��@��^@�`B@��`@���@�(�@���@��@�l�@�K�@�33@�o@���@���@�^5@�E�@��@���@���@��@�O�@�&�@��`@��D@�I�@��@�  @��
@�l�@�S�@�C�@�o@��R@��+@�^5@�-@��@��@�J@���@��+@���@��h@���@���@��/@���@�Ĝ@��/@��@���@�Ĝ@�z�@��@�ƨ@��@�
=@��y@���@��+@�ff@�M�@���@��^@���@��7@�7L@��@���@���@���@���@�Q�@�9X@�  @�C�@��H@���@�-@�&�@���@�bN@�1'@��m@���@�o@��@���@��\@�M�@��@�J@��#@��-@���@�G�@���@�z�@�A�@��;@��P@���@�\)@�+@�E9@{!-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�`B	?}B	[#B	|�B	��B	�)B
�B
e`B
�-B
�BB
��B#�B@�BVB_;Bo�B�1B��B�B�qBB��B�5B�HB�fB�B�B��B��BBDB{B�B$�B$�B�B�B1'BE�BK�B8RBuB��B��B:^BW
BR�BO�BI�B@�B1'B�BVB%B�B��B@�B+B
�`B
��B
��B
�FB
��B
��B
�=B
gmB
E�B
C�B
B�B
A�B
>wB
8RB
(�B	�NB	��B	l�B	 �B	oB	%B�;B�FB�=Bm�B_;BT�BS�BVBW
BW
BW
B[#BdZB}�B�!B�LB�LB�XB�dB�}B��B��B��BȴB��B��B�/B�B��B	  B	hB	�B	"�B	'�B	'�B	&�B	$�B	"�B	PB��B��B��B��B��B�B�B�B�TB�BB�B�B��B��BȴBŢBĜBĜBŢBĜBBŢBŢBɺB��B��B��B�B�B�)B�/B�ZB�sB�yB�sB�mB�TB�;B�/B�/B�/B�/B�5B�/B�ZB�`B�mB�B�B�B�B�B�B��B�B�B�B�B�yB�yB��B��B	B��B�B�B�yB�B��B��B��B	%B	hB	\B	PB	�B	#�B	%�B	%�B	+B	.B	2-B	49B	5?B	7LB	:^B	>wB	B�B	D�B	E�B	A�B	>wB	<jB	:^B	7LB	5?B	33B	.B	%�B	%�B	$�B	1'B	?}B	N�B	T�B	W
B	W
B	W
B	W
B	XB	ZB	[#B	\)B	]/B	\)B	\)B	\)B	`BB	aHB	cTB	cTB	cTB	bNB	cTB	bNB	cTB	dZB	e`B	hsB	hsB	iyB	m�B	p�B	m�B	l�B	l�B	k�B	k�B	m�B	n�B	o�B	o�B	q�B	r�B	t�B	y�B	|�B	}�B	|�B	|�B	�B	�B	�1B	�7B	�\B	�hB	��B	��B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�B	�B	�B	�3B	�^B	�jB	�wB	�wB	�wB	�wB	�wB	�wB	�}B	�wB	�}B	�}B	��B	��B	��B	��B	ÖB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�;B	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�mB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
+B
+B
B
B
+B
+B
%B
	7B
DB
DB
VB
bB
bB
bB
bB
\B
\B
bB
bB
bB
bB
bB
bB
\B
bB
bB
bB
bB
bB
hB
oB
{B
{B
uB
uB
{B
{B
{B
�B
�B
{B
�B
{B
uB
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
6`22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�`B	?}B	[#B	|�B	��B	�)B
�B
e`B
�-B
�BB
��B#�B@�BVB_;Bo�B�1B��B�B�qBB��B�5B�HB�fB�B�B��B��BBDB{B�B$�B$�B�B�B1'BE�BK�B8RBuB��B��B:^BW
BR�BO�BI�B@�B1'B�BVB%B�B��B@�B+B
�`B
��B
��B
�FB
��B
��B
�=B
gmB
E�B
C�B
B�B
A�B
>wB
8RB
(�B	�NB	��B	l�B	 �B	oB	%B�;B�FB�=Bm�B_;BT�BS�BVBW
BW
BW
B[#BdZB}�B�!B�LB�LB�XB�dB�}B��B��B��BȴB��B��B�/B�B��B	  B	hB	�B	"�B	'�B	'�B	&�B	$�B	"�B	PB��B��B��B��B��B�B�B�B�TB�BB�B�B��B��BȴBŢBĜBĜBŢBĜBBŢBŢBɺB��B��B��B�B�B�)B�/B�ZB�sB�yB�sB�mB�TB�;B�/B�/B�/B�/B�5B�/B�ZB�`B�mB�B�B�B�B�B�B��B�B�B�B�B�yB�yB��B��B	B��B�B�B�yB�B��B��B��B	%B	hB	\B	PB	�B	#�B	%�B	%�B	+B	.B	2-B	49B	5?B	7LB	:^B	>wB	B�B	D�B	E�B	A�B	>wB	<jB	:^B	7LB	5?B	33B	.B	%�B	%�B	$�B	1'B	?}B	N�B	T�B	W
B	W
B	W
B	W
B	XB	ZB	[#B	\)B	]/B	\)B	\)B	\)B	`BB	aHB	cTB	cTB	cTB	bNB	cTB	bNB	cTB	dZB	e`B	hsB	hsB	iyB	m�B	p�B	m�B	l�B	l�B	k�B	k�B	m�B	n�B	o�B	o�B	q�B	r�B	t�B	y�B	|�B	}�B	|�B	|�B	�B	�B	�1B	�7B	�\B	�hB	��B	��B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�B	�B	�B	�3B	�^B	�jB	�wB	�wB	�wB	�wB	�wB	�wB	�}B	�wB	�}B	�}B	��B	��B	��B	��B	ÖB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�;B	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�mB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
+B
+B
B
B
+B
+B
%B
	7B
DB
DB
VB
bB
bB
bB
bB
\B
\B
bB
bB
bB
bB
bB
bB
\B
bB
bB
bB
bB
bB
hB
oB
{B
{B
uB
uB
{B
{B
{B
�B
�B
{B
�B
{B
uB
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
6`22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190614                              AO  ARCAADJP                                                                    20181005190614    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190614  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190614  QCF$                G�O�G�O�G�O�8000            