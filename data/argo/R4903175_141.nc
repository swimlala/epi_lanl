CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-08-29T09:01:36Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݌   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20220829090136  20220829090136  4903175 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7231                            2B  A   NAVIS_A                         0968                            170425                          863 @�ꔔz�d1   @��-��@3U?|�h�cѲ-V1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B`ffBg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ Dԃ3D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�p�@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB@G�BHG�BO�HBW�HB`G�Bgz�Bo�HBw�HB�HB��B��B��B��B��B��B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�C�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCr�Cs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�B=D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D¿
D��
D�?
D�
Dÿ
D��
D�?
D�
DĿ
D��
D�?
D�
Dſ
D��
D�?
D�
Dƿ
D��
D�?
D�
Dǿ
D��
D�?
D�
Dȿ
D��
D�?
D�
Dɿ
D��
D�?
D�
Dʿ
D��
D�?
D�
D˿
D��
D�?
D�
D̿
D��
D�?
D�
DͿ
D��
D�?
D�
Dο
D��
D�?
D�
DϿ
D��
D�?
D�
Dп
D��
D�?
D�
Dѿ
D��
D�?
D�
Dҿ
D��
D�?
D�
Dӿ
D��
D�?
DԂ=DԿ
D��
D�?
D�
Dտ
D��
D�?
D�
Dֿ
D��
D�?
D�
D׿
D��
D�?
D�
Dؿ
D��
D�?
D�
Dٿ
D��
D�?
D�
Dڿ
D��
D�?
D�
Dۿ
D��
D�?
D�
Dܿ
D��
D�?
D�
Dݿ
D��
D�?
D�
D޿
D��
D�?
D�
D߿
D��
D�?
D�
D�
D��
D�?
D�
D�
D���D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��
D��
D�?
D�=D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D��=D��
D��
D�?
D�
D��=D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��yA�33A��TA܁A�ZA�33A�
=AڑhA�&�A�I�A؉7A�=qA�jA�JAՋDA��AԾwA���A��`A�ƨA���A�x�A�C�A���Aʡ�A�n�A�XAĮA��-A��-A��^A�?}A��\A���A�;dA��+A�ZA�A�A��A�-A���A��A��PA�^5A���A�1A�jA�A���A��A��\A�dZA�G�A�+A�/A���A��-A��!A�%A�5?A�z�A�ffA�33A�A���A��+A��A�&�A���A��RA���A�G�A�\)A�%A��/A�t�A�%A���A�JA�^5A��A�1A���A��A��/A�%A�  A�-A}�A{��AwAr�Apz�Ao�;An-Am
=Al�DAlAh��Aa�wA`9XA]��AX  AT��AO"�AL�AL=qAK��AI��AF��AD�DAB�yAB �A@�A>Q�A<-A9`BA8{A5�FA5O�A5&�A4��A3�A2��A0�A/��A.�A-oA*ffA)�TA)�7A((�A'�A%K�A#��A#�7A"�A"9XA"1'A"bA!p�A!VA �A!�
A!�FA �A�A�9AM�A;dA(�A�TA�9A;dA�TA�DA�9A  A�A�^AI�AA�A-AhsA
��A	A�A{A�A�A��A�Ap�A J@���@�@���@��!@���@�/@�bN@��R@�r�@��m@�K�@��@�@��m@�n�@�G�@���@�D@�z�@�Z@�  @���@�F@��@�{@�j@�Q�@��;@�o@�n�@��T@�/@䛦@�t�@���@�@��@��#@��@��@�  @��@�E�@ݡ�@�&�@��`@�r�@�;d@���@ؓu@׍P@�C�@�33@���@֗�@և+@�@�V@��@��@�ff@�Q�@�@�^5@�@�1'@�l�@��@ɡ�@�&�@��@ț�@�1'@�  @ǍP@�K�@�~�@�Q�@�S�@�
=@°!@�=q@��^@�hs@�&�@���@�Ĝ@�o@���@�O�@��@�X@�Ĝ@�9X@���@��w@��H@��+@�5?@���@�?}@��D@�Z@�Z@�9X@� �@�ƨ@���@�+@���@�5?@���@��T@���@�hs@��@���@�Ĝ@��@�I�@��
@���@���@��@�t�@�33@��@��@���@��+@�n�@�M�@�5?@���@�X@���@��j@��D@�Q�@���@��w@�;d@��R@���@�^5@�5?@��#@��@�hs@�X@��@���@�b@��w@���@��P@�|�@�\)@�+@�@��@���@�^5@�$�@���@���@��@��/@��@���@�j@�Q�@�  @��@�S�@�"�@���@�~�@�$�@�hs@�?}@�G�@�G�@�&�@��/@�z�@�1'@�1@��m@��
@�ƨ@��w@��@��P@�l�@�K�@�"�@�
=@���@�E�@��@��T@��@�%@�Ĝ@���@�A�@��@��F@�t�@�K�@�
=@���@�v�@�J@���@�x�@�hs@�?}@��@��@�Ĝ@���@��D@�z�@�j@�bN@� �@�l�@�33@��@��@���@�V@��@��T@���@���@��@�r�@�(�@���@�\)@��H@��\@�~�@�^5@�E�@�E�@�E�@�M�@�^5@�ff@�ff@�^5@�-@�J@���@��T@�@���@�`B@��@��9@���@��@�j@�Z@�(�@���@��
@��w@�|�@��@���@���@�v�@�{@��@���@�`B@�7L@���@��j@���@��D@�bN@�(�@�ƨ@�+@��H@���@�=q@�J@��@���@���@�G�@��@���@���@�j@�(�@���@�C�@��@���@��+@�M�@�$�@�5?@��@��@��-@�G�@�7L@�/@��@��@�Ĝ@���@��@�bN@�I�@�1'@��@�  @��w@��@�\)@�"�@�
=@��H@��@��^@���@�p�@�X@�G�@�G�@�?}@���@��u@��@�z�@�j@�(�@�  @�S�@��\@��#@���@���@�@��h@�`B@�O�@��@���@���@��j@���@�z�@�j@�Z@�A�@�b@�1@|�@;d@�@~ff@}�T@}�h@}?}@}V@|�@|�D@|9X@{��@{o@z��@z�H@z�!@z��@y�^@xQ�@x  @w��@wl�@v�y@vv�@up�@u�@t��@t��@t9X@t1@sC�@rJ@q��@q�#@qhs@q�@pĜ@pr�@pb@o�;@o��@o�P@o\)@n�@m��@m�h@mV@l�D@k��@kt�@k@j��@j�!@j=q@iG�@h��@hr�@g�;@f�@fv�@fff@f5?@e��@e�@d��@d�@d��@dj@d(�@c�m@cƨ@c��@c��@cS�@b�!@a��@a�^@a7L@a%@`�u@`  @_\)@^�R@^�+@^v�@^V@^E�@^{@^@]�-@]`B@]V@\�j@\z�@\Z@\9X@\(�@[t�@Z��@Zn�@Y��@Y&�@X�`@X��@Xr�@X1'@W�@W�P@W
=@Vȴ@V��@VV@VE�@V$�@U�@U�h@U�@T�@Tz�@TZ@T(�@T�@S��@R�!@R�\@R�\@R�\@R�\@R~�@RM�@R-@R-@Q��@PĜ@P�@PbN@Pb@O�@OK�@O+@Nȴ@Nff@N$�@M��@M/@L�@L��@Lz�@L9X@L1@Kt�@J�H@J�!@J^5@I�#@I��@I��@H�`@Hr�@H  @G|�@G;d@G+@F��@F�y@F�@F��@E�@Ep�@E?}@D�@Dz�@D�@C�m@C��@Ct�@B�@BM�@A�#@A��@Ahs@AG�@A�@@�@?��@?��@?K�@>�y@>ff@>$�@=�@=@=p�@=�@<�j@<1@;ƨ@;ƨ@;dZ@:�\@:M�@:�@:J@:J@9�^@97L@8��@8Ĝ@8�@8bN@8A�@7��@7;d@7�@6ȴ@6�R@6��@6��@6v�@5�T@5V@4�j@4I�@3�
@3�F@3�@3�@3S�@2�@2=q@1��@1��@1G�@0��@0A�@/�@/�@/l�@/+@.ff@.E�@.5?@-@,�/@,1@+�m@+ƨ@+��@+dZ@+"�@*�H@*��@*^5@*-@*J@)�^@)�^@)��@)7L@)%@(��@(�9@(�@(b@'�w@'+@&��@&��@&��@&��@&��@'�@'+@'�@&�@&�+@&V@%�@%��@%��@%�@%p�@%`B@%O�@$��@$j@$�@$1@$1@$1@#��@#��@$1@$(�@#�m@#dZ@#33@#33@#C�@#S�@#dZ@#dZ@#S�@#"�@"�@"�H@"��@"~�@"^5@"=q@"-@"J@!�@!�^@!��@!��@!7L@!�@ �`@ �9@ bN@ b@   @�@�P@|�@+@�@
=@��@�@�@�@�@ȴ@ff@{@@�h@�@p�@O�@O�@?}@�@V@��@�@�D@I�@1@�m@�F@dZ@"�@@��@��@n�@�@7L@�@�@�@�@�@��@�@ �@  @�;@��@�@�@�@�P@\)@�@��@v�@ff@E�@@��@�-@p�@/@�@j@�
@��@dZ@"�@��@n�@�#@7L@�@��@�`@��@�u@bN@1'@�@�w@\)@;d@��@�@ȴ@�R@�R@��@V@E�@$�@$�@$�@{@@�h@/@�@�@�/@��@�j@�@�@�D@z�@j@Z@(�@1@�m@ƨ@��@�@S�@"�@"�@o@
��@
=q@
J@	��@	�#@	��@	��@	��@	�7@	�7@	�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��yA�33A��TA܁A�ZA�33A�
=AڑhA�&�A�I�A؉7A�=qA�jA�JAՋDA��AԾwA���A��`A�ƨA���A�x�A�C�A���Aʡ�A�n�A�XAĮA��-A��-A��^A�?}A��\A���A�;dA��+A�ZA�A�A��A�-A���A��A��PA�^5A���A�1A�jA�A���A��A��\A�dZA�G�A�+A�/A���A��-A��!A�%A�5?A�z�A�ffA�33A�A���A��+A��A�&�A���A��RA���A�G�A�\)A�%A��/A�t�A�%A���A�JA�^5A��A�1A���A��A��/A�%A�  A�-A}�A{��AwAr�Apz�Ao�;An-Am
=Al�DAlAh��Aa�wA`9XA]��AX  AT��AO"�AL�AL=qAK��AI��AF��AD�DAB�yAB �A@�A>Q�A<-A9`BA8{A5�FA5O�A5&�A4��A3�A2��A0�A/��A.�A-oA*ffA)�TA)�7A((�A'�A%K�A#��A#�7A"�A"9XA"1'A"bA!p�A!VA �A!�
A!�FA �A�A�9AM�A;dA(�A�TA�9A;dA�TA�DA�9A  A�A�^AI�AA�A-AhsA
��A	A�A{A�A�A��A�Ap�A J@���@�@���@��!@���@�/@�bN@��R@�r�@��m@�K�@��@�@��m@�n�@�G�@���@�D@�z�@�Z@�  @���@�F@��@�{@�j@�Q�@��;@�o@�n�@��T@�/@䛦@�t�@���@�@��@��#@��@��@�  @��@�E�@ݡ�@�&�@��`@�r�@�;d@���@ؓu@׍P@�C�@�33@���@֗�@և+@�@�V@��@��@�ff@�Q�@�@�^5@�@�1'@�l�@��@ɡ�@�&�@��@ț�@�1'@�  @ǍP@�K�@�~�@�Q�@�S�@�
=@°!@�=q@��^@�hs@�&�@���@�Ĝ@�o@���@�O�@��@�X@�Ĝ@�9X@���@��w@��H@��+@�5?@���@�?}@��D@�Z@�Z@�9X@� �@�ƨ@���@�+@���@�5?@���@��T@���@�hs@��@���@�Ĝ@��@�I�@��
@���@���@��@�t�@�33@��@��@���@��+@�n�@�M�@�5?@���@�X@���@��j@��D@�Q�@���@��w@�;d@��R@���@�^5@�5?@��#@��@�hs@�X@��@���@�b@��w@���@��P@�|�@�\)@�+@�@��@���@�^5@�$�@���@���@��@��/@��@���@�j@�Q�@�  @��@�S�@�"�@���@�~�@�$�@�hs@�?}@�G�@�G�@�&�@��/@�z�@�1'@�1@��m@��
@�ƨ@��w@��@��P@�l�@�K�@�"�@�
=@���@�E�@��@��T@��@�%@�Ĝ@���@�A�@��@��F@�t�@�K�@�
=@���@�v�@�J@���@�x�@�hs@�?}@��@��@�Ĝ@���@��D@�z�@�j@�bN@� �@�l�@�33@��@��@���@�V@��@��T@���@���@��@�r�@�(�@���@�\)@��H@��\@�~�@�^5@�E�@�E�@�E�@�M�@�^5@�ff@�ff@�^5@�-@�J@���@��T@�@���@�`B@��@��9@���@��@�j@�Z@�(�@���@��
@��w@�|�@��@���@���@�v�@�{@��@���@�`B@�7L@���@��j@���@��D@�bN@�(�@�ƨ@�+@��H@���@�=q@�J@��@���@���@�G�@��@���@���@�j@�(�@���@�C�@��@���@��+@�M�@�$�@�5?@��@��@��-@�G�@�7L@�/@��@��@�Ĝ@���@��@�bN@�I�@�1'@��@�  @��w@��@�\)@�"�@�
=@��H@��@��^@���@�p�@�X@�G�@�G�@�?}@���@��u@��@�z�@�j@�(�@�  @�S�@��\@��#@���@���@�@��h@�`B@�O�@��@���@���@��j@���@�z�@�j@�Z@�A�@�b@�1@|�@;d@�@~ff@}�T@}�h@}?}@}V@|�@|�D@|9X@{��@{o@z��@z�H@z�!@z��@y�^@xQ�@x  @w��@wl�@v�y@vv�@up�@u�@t��@t��@t9X@t1@sC�@rJ@q��@q�#@qhs@q�@pĜ@pr�@pb@o�;@o��@o�P@o\)@n�@m��@m�h@mV@l�D@k��@kt�@k@j��@j�!@j=q@iG�@h��@hr�@g�;@f�@fv�@fff@f5?@e��@e�@d��@d�@d��@dj@d(�@c�m@cƨ@c��@c��@cS�@b�!@a��@a�^@a7L@a%@`�u@`  @_\)@^�R@^�+@^v�@^V@^E�@^{@^@]�-@]`B@]V@\�j@\z�@\Z@\9X@\(�@[t�@Z��@Zn�@Y��@Y&�@X�`@X��@Xr�@X1'@W�@W�P@W
=@Vȴ@V��@VV@VE�@V$�@U�@U�h@U�@T�@Tz�@TZ@T(�@T�@S��@R�!@R�\@R�\@R�\@R�\@R~�@RM�@R-@R-@Q��@PĜ@P�@PbN@Pb@O�@OK�@O+@Nȴ@Nff@N$�@M��@M/@L�@L��@Lz�@L9X@L1@Kt�@J�H@J�!@J^5@I�#@I��@I��@H�`@Hr�@H  @G|�@G;d@G+@F��@F�y@F�@F��@E�@Ep�@E?}@D�@Dz�@D�@C�m@C��@Ct�@B�@BM�@A�#@A��@Ahs@AG�@A�@@�@?��@?��@?K�@>�y@>ff@>$�@=�@=@=p�@=�@<�j@<1@;ƨ@;ƨ@;dZ@:�\@:M�@:�@:J@:J@9�^@97L@8��@8Ĝ@8�@8bN@8A�@7��@7;d@7�@6ȴ@6�R@6��@6��@6v�@5�T@5V@4�j@4I�@3�
@3�F@3�@3�@3S�@2�@2=q@1��@1��@1G�@0��@0A�@/�@/�@/l�@/+@.ff@.E�@.5?@-@,�/@,1@+�m@+ƨ@+��@+dZ@+"�@*�H@*��@*^5@*-@*J@)�^@)�^@)��@)7L@)%@(��@(�9@(�@(b@'�w@'+@&��@&��@&��@&��@&��@'�@'+@'�@&�@&�+@&V@%�@%��@%��@%�@%p�@%`B@%O�@$��@$j@$�@$1@$1@$1@#��@#��@$1@$(�@#�m@#dZ@#33@#33@#C�@#S�@#dZ@#dZ@#S�@#"�@"�@"�H@"��@"~�@"^5@"=q@"-@"J@!�@!�^@!��@!��@!7L@!�@ �`@ �9@ bN@ b@   @�@�P@|�@+@�@
=@��@�@�@�@�@ȴ@ff@{@@�h@�@p�@O�@O�@?}@�@V@��@�@�D@I�@1@�m@�F@dZ@"�@@��@��@n�@�@7L@�@�@�@�@�@��@�@ �@  @�;@��@�@�@�@�P@\)@�@��@v�@ff@E�@@��@�-@p�@/@�@j@�
@��@dZ@"�@��@n�@�#@7L@�@��@�`@��@�u@bN@1'@�@�w@\)@;d@��@�@ȴ@�R@�R@��@V@E�@$�@$�@$�@{@@�h@/@�@�@�/@��@�j@�@�@�D@z�@j@Z@(�@1@�m@ƨ@��@�@S�@"�@"�@o@
��@
=q@
J@	��@	�#@	��@	��@	��@	�7@	�7@	�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��^A�K�A��;A���A��!A��A�^5A�A��A��9A�|�A��HAН�A�ȴA�A�/A�t�A�^5A�^5A�33A�7LA�r�A�XA���A� �A�-A��A��A�ffA�VA��hA�t�A�Q�A�ZA���A�9XA��RA��A��TA��A��
A��A�33A�{A���A�^5A��HA���A��A��RA���A�?}A���A�ĜA�bA�jAɥ�A�|�AռjA�E�A�E�A��A��A��mA�
=A�VA�\)A�"�A���A�O�A�A���A��A�A�K�A�9XA�bA�oA�p�A�A���A��^A��PA��B S�B �?B ��B ��B+BD�BM�BI�B�bB1BJ�Bk�B��B�-B��B��B��Be`BT�B]/BÖB�B��BbBF�B�PB�fB�jB��B�#B�}B��B�B�#BJB{BI�BH�B=qB/B�B\B �B�B\B	7B<jB33B�B�B�B1'BN�BG�BW
B_;BW
BM�BYB_;BbNB;dB#�B)�B>wB5?B�BoBB�BPB��B�B+BM�BE�B>wBK�Bk�B]/BF�B>wB7LB �BM�BR�BZB@�B2-BJ�BE�B[#Bo�B`BBl�Bp�Br�BiyB`BBcTBx�Bs�Bm�Bp�Bw�Br�B}�B�=B�VB�\B�JB�1B�+B�Bx�Bw�Bw�B�B�B}�B�B�B�B�B�B�DB�VB��B�PB�1B�=B�DB�%B�\B�hB�uB�oB�7B� B�B�JB��B��B��B��B��B��B��B��B�dB��BŢB�B�B��B	B��B	VB	uB	�B	#�B	,B	&�B	(�B	'�B	!�B	�B	hB	VB	+B	8RB	8RB	9XB	>wB	B�B	B�B	@�B	8RB	+B	8RB	I�B	O�B	H�B	C�B	E�B	I�B	F�B	A�B	K�B	J�B	H�B	D�B	J�B	S�B	T�B	Q�B	O�B	J�B	L�B	H�B	I�B	L�B	Q�B	P�B	M�B	H�B	J�B	M�B	J�B	I�B	J�B	K�B	R�B	S�B	R�B	P�B	M�B	N�B	P�B	P�B	M�B	Q�B	P�B	O�B	K�B	K�B	L�B	M�B	L�B	I�B	E�B	C�B	?}B	@�B	E�B	A�B	@�B	<jB	:^B	;dB	8RB	1'B	+B	)�B	/B	33B	5?B	5?B	5?B	49B	49B	49B	1'B	/B	-B	)�B	,B	'�B	-B	.B	.B	)�B	)�B	%�B	$�B	)�B	(�B	$�B	$�B	"�B	"�B	,B	.B	)�B	&�B	$�B	%�B	)�B	,B	.B	/B	/B	2-B	1'B	.B	,B	(�B	"�B	�B	�B	�B	�B	�B	uB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	(�B	&�B	'�B	'�B	%�B	$�B	"�B	!�B	�B	�B	�B	{B	�B	hB	DB		7B		7B		7B	DB	B	B	%B	+B	B	B	%B	B		7B	JB	DB	DB	JB	PB	JB	
=B	1B	B	B	  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�B�B�B�B�B�B�B�B�B�B�B�yB�mB�`B�B�B�mB�B�B�B�sB�fB�fB�`B�TB�`B�ZB�ZB�TB�mB�yB�yB�yB�B�B�mB�sB�fB�mB�B�B�yB�yB�ZB�mB�mB�sB�sB�mB�fB�ZB�TB�ZB�`B�TB�TB�;B�B�;B�NB�HB�NB�HB�BB�5B�#B�#B�;B�5B�)B�B�B��B��B�#B�mB�mB�mB�ZB�TB�`B�ZB�`B�mB�fB�`B�`B�fB�`B�ZB�ZB�ZB�HB�TB�HB�5B�;B�;B�;B�BB�;B�5B�#B�B�B�#B�)B�B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BƨBŢBɺBǮBǮBƨBǮBȴBǮBŢBB�}BB�}B�wB�qBÖBĜB��B�}B�wB��B��B�}B�wB�jB�jB�dB�dB�XB�?B�-B�-B�RB�?B�FB�3B�3B�3B�FB�XB�^B�^B�^B�XB�RB�LB�FB�FB�FB�LB�LB�FB�9B�B�B�!B�B�-B�9B�3B�3B�-B�!B�B�B�'B�'B�!B�'B�B�B�B�B�B�B�B�B�B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B�{B�hB�oB��B�{B�uB�uB��B�{B�uB�hB�bB�\B�VB�oB�hB�VB�JB�oB�oB�oB�bB�VB�PB�\B�bB�\B�\B�VB�JB�JB�VB�VB�VB�PB�DB�1B�B�B�%B�%B�%B�1B�+B�+B�B�B�B�B�B�B� B�B�B�B�B� B}�B�B~�By�Bw�By�B~�B~�B}�B|�B{�Bz�B{�By�Bz�By�By�Bz�Bx�Bu�Bv�Bv�Bu�Bs�Bq�Br�Br�Bu�By�B{�Bx�Bw�Bv�Bt�Br�Bp�Bo�Bq�Bp�Bq�Bq�Bp�Bp�Bo�Bm�Bk�Bk�Bn�Bp�Bp�Bo�Bo�Bn�Bn�Bm�BjBjBl�Bn�Bo�Bn�Bl�Bk�BjBiyBiyBiyBhsBiyBhsBhsBiyBhsBgmBhsBhsBffBe`BffBe`Be`Be`BffBhsBgmBjBjBiyBk�Bk�Bl�Bl�Bl�Bl�Bl�Bk�BiyBk�Bk�Bn�Bo�Bn�Bn�Bn�Bm�Bm�Bm�Bl�BjBhsBjBiyBjBiyBhsBhsBiyBgmBgmBffBdZBdZBjBk�BjBiyBgmBe`BdZBdZBffBgmBffBffBffBe`BcTBaHBaHB`BBbNBbNB`BB_;B^5B^5BZBW
BL�B(�B$�B&�B&�B%�B$�B"�B"�B#�B'�B'�B'�B%�B#�B#�B#�B#�B#�B"�B#�B#�B$�B%�B$�B$�B#�B"�B"�B#�B#�B"�B �B�B�B�B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuB{B�B�B�B�B�B�B�B�B�B�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 A��^A�K�A��;A���A��!A��A�^5A�A��A��9A�|�A��HAН�A�ȴA�A�/A�t�A�^5A�^5A�33A�7LA�r�A�XA���A� �A�-A��A��A�ffA�VA��hA�t�A�Q�A�ZA���A�9XA��RA��A��TA��A��
A��A�33A�{A���A�^5A��HA���A��A��RA���A�?}A���A�ĜA�bA�jAɥ�A�|�AռjA�E�A�E�A��A��A��mA�
=A�VA�\)A�"�A���A�O�A�A���A��A�A�K�A�9XA�bA�oA�p�A�A���A��^A��PA��B S�B �?B ��B ��B+BD�BM�BI�B�bB1BJ�Bk�B��B�-B��B��B��Be`BT�B]/BÖB�B��BbBF�B�PB�fB�jB��B�#B�}B��B�B�#BJB{BI�BH�B=qB/B�B\B �B�B\B	7B<jB33B�B�B�B1'BN�BG�BW
B_;BW
BM�BYB_;BbNB;dB#�B)�B>wB5?B�BoBB�BPB��B�B+BM�BE�B>wBK�Bk�B]/BF�B>wB7LB �BM�BR�BZB@�B2-BJ�BE�B[#Bo�B`BBl�Bp�Br�BiyB`BBcTBx�Bs�Bm�Bp�Bw�Br�B}�B�=B�VB�\B�JB�1B�+B�Bx�Bw�Bw�B�B�B}�B�B�B�B�B�B�DB�VB��B�PB�1B�=B�DB�%B�\B�hB�uB�oB�7B� B�B�JB��B��B��B��B��B��B��B��B�dB��BŢB�B�B��B	B��B	VB	uB	�B	#�B	,B	&�B	(�B	'�B	!�B	�B	hB	VB	+B	8RB	8RB	9XB	>wB	B�B	B�B	@�B	8RB	+B	8RB	I�B	O�B	H�B	C�B	E�B	I�B	F�B	A�B	K�B	J�B	H�B	D�B	J�B	S�B	T�B	Q�B	O�B	J�B	L�B	H�B	I�B	L�B	Q�B	P�B	M�B	H�B	J�B	M�B	J�B	I�B	J�B	K�B	R�B	S�B	R�B	P�B	M�B	N�B	P�B	P�B	M�B	Q�B	P�B	O�B	K�B	K�B	L�B	M�B	L�B	I�B	E�B	C�B	?}B	@�B	E�B	A�B	@�B	<jB	:^B	;dB	8RB	1'B	+B	)�B	/B	33B	5?B	5?B	5?B	49B	49B	49B	1'B	/B	-B	)�B	,B	'�B	-B	.B	.B	)�B	)�B	%�B	$�B	)�B	(�B	$�B	$�B	"�B	"�B	,B	.B	)�B	&�B	$�B	%�B	)�B	,B	.B	/B	/B	2-B	1'B	.B	,B	(�B	"�B	�B	�B	�B	�B	�B	uB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	(�B	&�B	'�B	'�B	%�B	$�B	"�B	!�B	�B	�B	�B	{B	�B	hB	DB		7B		7B		7B	DB	B	B	%B	+B	B	B	%B	B		7B	JB	DB	DB	JB	PB	JB	
=B	1B	B	B	  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�B�B�B�B�B�B�B�B�B�B�B�yB�mB�`B�B�B�mB�B�B�B�sB�fB�fB�`B�TB�`B�ZB�ZB�TB�mB�yB�yB�yB�B�B�mB�sB�fB�mB�B�B�yB�yB�ZB�mB�mB�sB�sB�mB�fB�ZB�TB�ZB�`B�TB�TB�;B�B�;B�NB�HB�NB�HB�BB�5B�#B�#B�;B�5B�)B�B�B��B��B�#B�mB�mB�mB�ZB�TB�`B�ZB�`B�mB�fB�`B�`B�fB�`B�ZB�ZB�ZB�HB�TB�HB�5B�;B�;B�;B�BB�;B�5B�#B�B�B�#B�)B�B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BƨBŢBɺBǮBǮBƨBǮBȴBǮBŢBB�}BB�}B�wB�qBÖBĜB��B�}B�wB��B��B�}B�wB�jB�jB�dB�dB�XB�?B�-B�-B�RB�?B�FB�3B�3B�3B�FB�XB�^B�^B�^B�XB�RB�LB�FB�FB�FB�LB�LB�FB�9B�B�B�!B�B�-B�9B�3B�3B�-B�!B�B�B�'B�'B�!B�'B�B�B�B�B�B�B�B�B�B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B�{B�hB�oB��B�{B�uB�uB��B�{B�uB�hB�bB�\B�VB�oB�hB�VB�JB�oB�oB�oB�bB�VB�PB�\B�bB�\B�\B�VB�JB�JB�VB�VB�VB�PB�DB�1B�B�B�%B�%B�%B�1B�+B�+B�B�B�B�B�B�B� B�B�B�B�B� B}�B�B~�By�Bw�By�B~�B~�B}�B|�B{�Bz�B{�By�Bz�By�By�Bz�Bx�Bu�Bv�Bv�Bu�Bs�Bq�Br�Br�Bu�By�B{�Bx�Bw�Bv�Bt�Br�Bp�Bo�Bq�Bp�Bq�Bq�Bp�Bp�Bo�Bm�Bk�Bk�Bn�Bp�Bp�Bo�Bo�Bn�Bn�Bm�BjBjBl�Bn�Bo�Bn�Bl�Bk�BjBiyBiyBiyBhsBiyBhsBhsBiyBhsBgmBhsBhsBffBe`BffBe`Be`Be`BffBhsBgmBjBjBiyBk�Bk�Bl�Bl�Bl�Bl�Bl�Bk�BiyBk�Bk�Bn�Bo�Bn�Bn�Bn�Bm�Bm�Bm�Bl�BjBhsBjBiyBjBiyBhsBhsBiyBgmBgmBffBdZBdZBjBk�BjBiyBgmBe`BdZBdZBffBgmBffBffBffBe`BcTBaHBaHB`BBbNBbNB`BB_;B^5B^5BZBW
BL�B(�B$�B&�B&�B%�B$�B"�B"�B#�B'�B'�B'�B%�B#�B#�B#�B#�B#�B"�B#�B#�B$�B%�B$�B$�B#�B"�B"�B#�B#�B"�B �B�B�B�B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuB{B�B�B�B�B�B�B�B�B�B�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220829090136                              AO  ARCAADJP                                                                    20220829090136    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220829090136  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20220829090136  QCF$                G�O�G�O�G�O�8000            