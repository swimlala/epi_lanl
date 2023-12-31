CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-03-27T12:00:38Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `h   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɘ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ̀   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210327120038  20210327120038  4903175 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               YA   AO  7231                            2B  A   NAVIS_A                         0968                            170425                          863 @�h���c�1   @�h�����@2Z^5?|��c�7KƧ�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         YA   A   F   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΃3D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�I�D�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�Bz�B�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC޸C�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCh�Ci�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP�zDP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh��Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
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
D�{�D��
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
D΂=Dο
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
D�
DԿ
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
D�{�D׿
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
D�
D��
D�?
D�
D��
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
D�=D�H�D�b=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��`A��mA��TA��`A��mA��A��A��A��A��`A��mA��A��HA���A�ĜAǥ�Aǥ�AǶFA�t�A�`BA�ZA�C�A�+A���A��A��TA��;A���A���A�ȴA�ƨA�AƼjAƴ9AƧ�Aơ�AƸRA�ƨAƾwAƟ�A���A��`A��A�S�A�z�A��/A���A��mA���AǑhAǅA�(�A��Aƺ^A�VAőhAčPA�t�A�|�A���A�S�A�dZA���A�hsA��A���A��A�ƨA�{A�%A���A��A���A���A�t�A��;A�|�A��#A��A�oA���A��A��A�A�A��^A�C�A�M�A�bNA���A�ĜA��A��DA�x�A���A�$�A��DA�9XA��;A��FA�(�A��`A��yA�n�A�^5A���A��A�A��A�A�z�A��mA�XA��A��9A�hsA��!A;dAyt�Ar��ArJApZAnr�Ak�#Aj�HAh��Ac�A`ZA^�A]ƨA[��AYK�AV�AS�AOoAL�`AK��AJ�/AI�FAF�AE7LADn�AC�
AC7LAA�A>��A=�PA=
=A;��A8��A8Q�A7��A7�A6�9A6A5oA4  A3x�A3&�A2M�A1+A05?A/A/A.��A-S�A,��A,A*1'A)�A)�A)VA'�A#A!�A!"�A ~�A`BA�A�mA$�A�hAS�A�A`BA��A�AdZA��A�+AVA5?A  A�;A��AC�A��A��A^5AJAQ�Ax�A�A��A7LA%AffA�A��A�A
�A
�RA
��A
VA	"�A�\AI�AbNAbA��A��A�hA
=A��A�\AJAA��A�hA�TAl�AAJA�A?}A ��A ��@���@�ff@�/@�;d@�5?@�l�@��y@�;d@�n�@�/@�@�`B@�Ĝ@�b@@웦@�1@�@�ff@�/@���@��/@�  @�dZ@�33@�o@��H@柾@旍@旍@�{@�V@�\)@�@�%@�(�@߅@��@�C�@���@�@�5?@�V@�A�@�|�@�t�@�dZ@�S�@�33@�~�@ٺ^@��@ؓu@�(�@�l�@�v�@���@���@�%@�I�@�ƨ@ӥ�@Ӆ@�S�@�E�@�`B@���@��/@�z�@� �@ϝ�@�ȴ@�$�@Ͳ-@���@���@�33@��@�ȴ@��T@���@�r�@��;@�C�@��@Ƨ�@�^5@���@��#@�@Ų-@Ł@�1'@�@�@�~�@�@\@�-@���@��-@�`B@���@� �@�ȴ@�E�@��\@�ff@��^@��@��j@��D@�1'@��@���@��@�C�@�
=@�ȴ@��@���@�`B@�V@��@��@�I�@�(�@��@��
@��m@��@�K�@��@���@���@�v�@�@���@��@�`B@��@���@�1@��@�K�@��@�M�@���@��@�bN@�b@�ƨ@�|�@�o@�~�@�5?@�5?@���@�O�@��@��@��j@��u@�bN@��@�|�@���@�n�@�=q@�5?@�-@�{@��@���@�`B@��@���@�z�@���@��P@�t�@�"�@�@�o@��@�v�@�n�@�ff@��@�G�@�Ĝ@��@�j@� �@�1@�  @��@�ƨ@��@�|�@���@�V@��^@�G�@��@� �@�1@���@��P@�\)@�o@���@�=q@�J@���@�@�p�@��/@�  @��m@�C�@��!@�V@�$�@�J@���@��@���@��h@�`B@�?}@�&�@���@�I�@�1'@��@���@�ƨ@�t�@�S�@�o@�@��y@��y@��@�^5@�=q@�-@�{@���@���@���@���@�O�@��j@���@�j@��@��P@�dZ@�
=@��@��\@��T@�x�@�?}@�%@��@���@�I�@��@�dZ@���@���@��+@�v�@�V@�E�@�-@��@�p�@�&�@��u@�(�@���@��
@��@�t�@�"�@�o@��H@��\@�^5@�{@��@��T@��#@���@��-@�`B@�V@���@��@�I�@�(�@�  @��F@�l�@�K�@��@�
=@��H@���@�^5@�-@��@��^@���@��@�X@��/@��9@��@��@�9X@�(�@���@��;@���@��F@��@���@���@�\)@�"�@�
=@��@���@�V@�{@��@�@�hs@�/@��@�Ĝ@�I�@��@�P@;d@~�@~V@~{@}��@}�h@|�/@|I�@|�@{��@{dZ@{"�@{"�@{o@{o@z�@z��@z~�@z^5@z�@y�7@x��@x��@w�;@v{@up�@u�@tZ@s��@sdZ@sS�@sC�@s33@s"�@r�H@r~�@q�@q�7@q%@pr�@o��@ol�@oK�@o
=@n�R@m�-@mO�@l��@l��@l�@lj@l1@k��@k�F@ko@j�\@j=q@i��@i�#@i�#@i�^@i7L@i7L@i&�@h�@h1'@hb@g�;@g|�@gl�@gl�@g�@fff@eV@d�D@d�D@dz�@dZ@d1@cƨ@cdZ@b�H@bn�@b-@a��@a��@`��@`�@`�@`r�@`1'@_�@_�@_;d@^�@^��@^��@^�+@^v�@]�@]�@]`B@\��@\��@[�m@["�@Z�!@Z�\@Y�#@Yx�@Y%@XA�@W�@W;d@V��@Vȴ@Vv�@T��@Tz�@Tz�@TZ@T9X@S�
@S�@R�@R~�@R-@RJ@Q��@Q�^@Q�^@Q��@Q&�@QX@Q�@P�9@PQ�@O��@O��@O|�@O\)@O+@N��@N{@M@M�@L�/@L��@Lz�@K��@K�F@K�@Ko@J�!@Jn�@JJ@I�#@I�@H�9@H��@H�u@H�u@H�@HbN@HQ�@Hb@G|�@GK�@G
=@F�+@F$�@E�-@Ep�@EV@D��@DZ@D1@C��@Ct�@CS�@B��@B-@BJ@A��@AX@A�@@�@?�@?��@?�P@?+@>�R@>V@=�T@=@=�h@=�@=`B@=O�@=V@<�j@<�D@<j@<9X@;�
@;ƨ@;ƨ@;��@;t�@;@:��@:�\@:=q@:�@9�@9�^@9��@9��@9��@9hs@9�@8�`@8Ĝ@8�9@8�@81'@8b@7��@6�y@6�+@6v�@6ff@6V@6V@5��@4��@4�@4��@4Z@4(�@4�@41@4�@41@41@41@41@41@3�@2�@2�!@2�\@2M�@1��@1��@1�7@1X@1&�@1�@0��@0r�@0Q�@01'@0b@/�@/K�@/�@/
=@.��@.��@.V@.E�@.5?@.{@.@-�T@-�-@-�@-/@,��@,Z@,�@+�m@+��@+S�@+S�@+33@+o@*��@*n�@*-@*J@)��@)�#@)��@)�^@)��@)hs@)G�@(��@(��@(Q�@( �@(  @'�@'�w@'��@'|�@'K�@'K�@'K�@';d@';d@'
=@&��@&$�@&@%�@%�@%�h@%O�@%?}@%?}@%?}@%?}@%?}@$��@$�/@$z�@$9X@#��@#�F@#S�@#@"�@"��@"��@"��@"M�@"-@"J@!��@!�7@!X@!7L@!%@ ��@ �@ bN@ 1'@  �@�@��@K�@��@��@$�@�h@�@�@�@z�@�@�m@�
@��@��@dZ@"�@��@J@��@��@��@x�@X@7L@%@��@r�@Q�@1'@ �@b@�@��@�P@K�@�@ȴ@ff@{@��@�h@p�@�@��@z�@I�@�@�m@��@C�@33@"�@@�H@^5@�@��@�@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��`A��mA��TA��`A��mA��A��A��A��A��`A��mA��A��HA���A�ĜAǥ�Aǥ�AǶFA�t�A�`BA�ZA�C�A�+A���A��A��TA��;A���A���A�ȴA�ƨA�AƼjAƴ9AƧ�Aơ�AƸRA�ƨAƾwAƟ�A���A��`A��A�S�A�z�A��/A���A��mA���AǑhAǅA�(�A��Aƺ^A�VAőhAčPA�t�A�|�A���A�S�A�dZA���A�hsA��A���A��A�ƨA�{A�%A���A��A���A���A�t�A��;A�|�A��#A��A�oA���A��A��A�A�A��^A�C�A�M�A�bNA���A�ĜA��A��DA�x�A���A�$�A��DA�9XA��;A��FA�(�A��`A��yA�n�A�^5A���A��A�A��A�A�z�A��mA�XA��A��9A�hsA��!A;dAyt�Ar��ArJApZAnr�Ak�#Aj�HAh��Ac�A`ZA^�A]ƨA[��AYK�AV�AS�AOoAL�`AK��AJ�/AI�FAF�AE7LADn�AC�
AC7LAA�A>��A=�PA=
=A;��A8��A8Q�A7��A7�A6�9A6A5oA4  A3x�A3&�A2M�A1+A05?A/A/A.��A-S�A,��A,A*1'A)�A)�A)VA'�A#A!�A!"�A ~�A`BA�A�mA$�A�hAS�A�A`BA��A�AdZA��A�+AVA5?A  A�;A��AC�A��A��A^5AJAQ�Ax�A�A��A7LA%AffA�A��A�A
�A
�RA
��A
VA	"�A�\AI�AbNAbA��A��A�hA
=A��A�\AJAA��A�hA�TAl�AAJA�A?}A ��A ��@���@�ff@�/@�;d@�5?@�l�@��y@�;d@�n�@�/@�@�`B@�Ĝ@�b@@웦@�1@�@�ff@�/@���@��/@�  @�dZ@�33@�o@��H@柾@旍@旍@�{@�V@�\)@�@�%@�(�@߅@��@�C�@���@�@�5?@�V@�A�@�|�@�t�@�dZ@�S�@�33@�~�@ٺ^@��@ؓu@�(�@�l�@�v�@���@���@�%@�I�@�ƨ@ӥ�@Ӆ@�S�@�E�@�`B@���@��/@�z�@� �@ϝ�@�ȴ@�$�@Ͳ-@���@���@�33@��@�ȴ@��T@���@�r�@��;@�C�@��@Ƨ�@�^5@���@��#@�@Ų-@Ł@�1'@�@�@�~�@�@\@�-@���@��-@�`B@���@� �@�ȴ@�E�@��\@�ff@��^@��@��j@��D@�1'@��@���@��@�C�@�
=@�ȴ@��@���@�`B@�V@��@��@�I�@�(�@��@��
@��m@��@�K�@��@���@���@�v�@�@���@��@�`B@��@���@�1@��@�K�@��@�M�@���@��@�bN@�b@�ƨ@�|�@�o@�~�@�5?@�5?@���@�O�@��@��@��j@��u@�bN@��@�|�@���@�n�@�=q@�5?@�-@�{@��@���@�`B@��@���@�z�@���@��P@�t�@�"�@�@�o@��@�v�@�n�@�ff@��@�G�@�Ĝ@��@�j@� �@�1@�  @��@�ƨ@��@�|�@���@�V@��^@�G�@��@� �@�1@���@��P@�\)@�o@���@�=q@�J@���@�@�p�@��/@�  @��m@�C�@��!@�V@�$�@�J@���@��@���@��h@�`B@�?}@�&�@���@�I�@�1'@��@���@�ƨ@�t�@�S�@�o@�@��y@��y@��@�^5@�=q@�-@�{@���@���@���@���@�O�@��j@���@�j@��@��P@�dZ@�
=@��@��\@��T@�x�@�?}@�%@��@���@�I�@��@�dZ@���@���@��+@�v�@�V@�E�@�-@��@�p�@�&�@��u@�(�@���@��
@��@�t�@�"�@�o@��H@��\@�^5@�{@��@��T@��#@���@��-@�`B@�V@���@��@�I�@�(�@�  @��F@�l�@�K�@��@�
=@��H@���@�^5@�-@��@��^@���@��@�X@��/@��9@��@��@�9X@�(�@���@��;@���@��F@��@���@���@�\)@�"�@�
=@��@���@�V@�{@��@�@�hs@�/@��@�Ĝ@�I�@��@�P@;d@~�@~V@~{@}��@}�h@|�/@|I�@|�@{��@{dZ@{"�@{"�@{o@{o@z�@z��@z~�@z^5@z�@y�7@x��@x��@w�;@v{@up�@u�@tZ@s��@sdZ@sS�@sC�@s33@s"�@r�H@r~�@q�@q�7@q%@pr�@o��@ol�@oK�@o
=@n�R@m�-@mO�@l��@l��@l�@lj@l1@k��@k�F@ko@j�\@j=q@i��@i�#@i�#@i�^@i7L@i7L@i&�@h�@h1'@hb@g�;@g|�@gl�@gl�@g�@fff@eV@d�D@d�D@dz�@dZ@d1@cƨ@cdZ@b�H@bn�@b-@a��@a��@`��@`�@`�@`r�@`1'@_�@_�@_;d@^�@^��@^��@^�+@^v�@]�@]�@]`B@\��@\��@[�m@["�@Z�!@Z�\@Y�#@Yx�@Y%@XA�@W�@W;d@V��@Vȴ@Vv�@T��@Tz�@Tz�@TZ@T9X@S�
@S�@R�@R~�@R-@RJ@Q��@Q�^@Q�^@Q��@Q&�@QX@Q�@P�9@PQ�@O��@O��@O|�@O\)@O+@N��@N{@M@M�@L�/@L��@Lz�@K��@K�F@K�@Ko@J�!@Jn�@JJ@I�#@I�@H�9@H��@H�u@H�u@H�@HbN@HQ�@Hb@G|�@GK�@G
=@F�+@F$�@E�-@Ep�@EV@D��@DZ@D1@C��@Ct�@CS�@B��@B-@BJ@A��@AX@A�@@�@?�@?��@?�P@?+@>�R@>V@=�T@=@=�h@=�@=`B@=O�@=V@<�j@<�D@<j@<9X@;�
@;ƨ@;ƨ@;��@;t�@;@:��@:�\@:=q@:�@9�@9�^@9��@9��@9��@9hs@9�@8�`@8Ĝ@8�9@8�@81'@8b@7��@6�y@6�+@6v�@6ff@6V@6V@5��@4��@4�@4��@4Z@4(�@4�@41@4�@41@41@41@41@41@3�@2�@2�!@2�\@2M�@1��@1��@1�7@1X@1&�@1�@0��@0r�@0Q�@01'@0b@/�@/K�@/�@/
=@.��@.��@.V@.E�@.5?@.{@.@-�T@-�-@-�@-/@,��@,Z@,�@+�m@+��@+S�@+S�@+33@+o@*��@*n�@*-@*J@)��@)�#@)��@)�^@)��@)hs@)G�@(��@(��@(Q�@( �@(  @'�@'�w@'��@'|�@'K�@'K�@'K�@';d@';d@'
=@&��@&$�@&@%�@%�@%�h@%O�@%?}@%?}@%?}@%?}@%?}@$��@$�/@$z�@$9X@#��@#�F@#S�@#@"�@"��@"��@"��@"M�@"-@"J@!��@!�7@!X@!7L@!%@ ��@ �@ bN@ 1'@  �@�@��@K�@��@��@$�@�h@�@�@�@z�@�@�m@�
@��@��@dZ@"�@��@J@��@��@��@x�@X@7L@%@��@r�@Q�@1'@ �@b@�@��@�P@K�@�@ȴ@ff@{@��@�h@p�@�@��@z�@I�@�@�m@��@C�@33@"�@@�H@^5@�@��@�@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B =qB <jB <jB <jB <jB <jB =qB =qB <jB <jB <jB <jB ;dB ;dB :^B ;dB ;dB :^B ;dB :^B :^B :^B :^B @�B A�B A�B A�B B�B B�B A�B B�B B�B C�B C�B G�B M�B gmB �=B ��B �B ��B ��B �B ��BB#�BF�BO�B[#B}�B�uB�qB��BÖBBɺB��B�HB��B��B��BB �B'�BD�BO�B\)Be`BgmBk�B~�B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B�oB�VB�7B�B~�Bz�B|�B�%B�bB�hB�uB�VB~�Bk�BM�B9XB%�B�BB��B�;B��B��B�!B��B�VB}�Bm�B[#BB�B1'B�B ��B �B �fB �B ��B ÖB �LB ��B ��B ��B �VB �7B }�B w�B p�B jB e`B cTB `BB aHB n�B v�B z�B {�B z�B z�B �B �7B ��B ��B ��B ��B �B �!B �!B �LB ÖB ɺB ��B �B �)B �BB �fB �sB �B ��B ��B ��B ��B ��B ��B �B �B �B �
B ��B ɺB ǮB ÖB ŢB ÖB ȴB ��B B �^B �qB �?B �LB �^B �^B �^B ŢB ɺB ɺB ��B ��B ��B ��B ��B ��B ��B ŢB B ŢB ƨB ŢB ÖB �}B �jB �dB �qB �}B �qB �dB �XB �jB ŢB ɺB ��B ��B ��B ��B �B �
B �B �
B �#B �)B �#B �B �B �B �)B �#B �B �
B ��B ��B ��B ŢB ��B ǮB ��B �FB �?B �FB �dB �qB ÖB ��B ǮB ĜB �wB �XB �LB �FB �LB �dB ŢB ƨB ĜB ŢB ǮB ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B �
B �
B �B �;B �NB �HB �NB �TB �ZB �B �B �B �B �B �B �B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B  B  B  BB  B ��B ��B ��B  BB%B	7B	7B
=BDB
=B1B1BDBDBJBVBbB{B�B�B�B�B!�B"�B"�B"�B$�B'�B(�B'�B(�B(�B,B,B.B0!B1'B49B49B5?B8RB8RB8RB8RB9XB9XB9XB9XB9XB:^B<jB=qB>wB>wB?}B?}B>wB>wB=qB<jB;dB;dB<jB<jB=qB>wB?}B@�B?}B?}B@�B@�B@�BA�BC�BD�BD�BD�BD�BD�BD�BE�BF�BG�BG�BH�BI�BJ�BJ�BK�BK�BK�BK�BL�BL�BK�BM�BO�BP�BP�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BS�BT�BVBVBW
BW
BW
BXBW
BW
BW
BW
BW
BVBVBT�BT�BVBT�BVBVBW
BW
BW
BXBXBXBZB[#B[#BZBZB[#B[#B[#B\)B\)B]/B]/B^5B^5B_;B_;B`BBcTBcTBcTBcTBcTBdZBe`BffBffBgmBgmBgmBhsBhsBhsBiyBhsBhsBjBk�Bk�Bk�Bk�Bk�Bl�Bl�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bp�Bq�Bq�Bq�Bq�Br�Br�Br�Br�Bs�Bs�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bu�Bv�Bv�Bv�Bv�Bw�Bx�Bx�Bx�Bx�Bx�By�Bz�Bz�B{�B{�B{�B|�B|�B}�B}�B~�B~�B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�+B�+B�+B�+B�1B�1B�1B�1B�7B�=B�=B�=B�=B�DB�DB�=B�=B�=B�DB�DB�DB�DB�DB�DB�DB�DB�PB�PB�PB�VB�\B�\B�\B�\B�\B�\B�\B�\B�bB�bB�bB�hB�hB�oB�oB�oB�oB�oB�uB�uB�uB�uB�uB�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�!B�'B�'B�'B�'B�'B�-B�-B�-B�-B�-B�3B�3B�3B�3B�3B�3B�3B�3B�9B�9B�9B�9B�9B�?B�?B�9B�9B�?B�?B�?B�?B�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�LB�LB�LB�LB�RB�RB�RB�RB�RB�LB�RB�XB�XB�XB�XB�XB�^B�XB�XB�XB�XB�XB�XB�XB�XB�^B�^B�^B�^B�dB�dB�dB�dB�dB�dB�dB�dB�jB�jB�jB�jB�jB�qB�qB�jB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�wB�wB�wB�wB�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBBBBBBBBBBBBBBÖBBÖBÖBÖBBBBBÖBÖBĜBĜBĜBĜBĜBĜBŢBŢBŢBŢBĜBĜBŢBƨBƨBƨBƨBƨBƨBŢBƨBǮBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBǮBǮBǮBȴBǮBǮBǮBǮBǮBǮBǮBȴBȴBȴBȴBȴBɺB��B��B��B��B��4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B =qB <jB <jB <jB <jB <jB =qB =qB <jB <jB <jB <jB ;dB ;dB :^B ;dB ;dB :^B ;dB :^B :^B :^B :^B @�B A�B A�B A�B B�B B�B A�B B�B B�B C�B C�B G�B M�B gmB �=B ��B �B ��B ��B �B ��BB#�BF�BO�B[#B}�B�uB�qB��BÖBBɺB��B�HB��B��B��BB �B'�BD�BO�B\)Be`BgmBk�B~�B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B�oB�VB�7B�B~�Bz�B|�B�%B�bB�hB�uB�VB~�Bk�BM�B9XB%�B�BB��B�;B��B��B�!B��B�VB}�Bm�B[#BB�B1'B�B ��B �B �fB �B ��B ÖB �LB ��B ��B ��B �VB �7B }�B w�B p�B jB e`B cTB `BB aHB n�B v�B z�B {�B z�B z�B �B �7B ��B ��B ��B ��B �B �!B �!B �LB ÖB ɺB ��B �B �)B �BB �fB �sB �B ��B ��B ��B ��B ��B ��B �B �B �B �
B ��B ɺB ǮB ÖB ŢB ÖB ȴB ��B B �^B �qB �?B �LB �^B �^B �^B ŢB ɺB ɺB ��B ��B ��B ��B ��B ��B ��B ŢB B ŢB ƨB ŢB ÖB �}B �jB �dB �qB �}B �qB �dB �XB �jB ŢB ɺB ��B ��B ��B ��B �B �
B �B �
B �#B �)B �#B �B �B �B �)B �#B �B �
B ��B ��B ��B ŢB ��B ǮB ��B �FB �?B �FB �dB �qB ÖB ��B ǮB ĜB �wB �XB �LB �FB �LB �dB ŢB ƨB ĜB ŢB ǮB ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B �
B �
B �B �;B �NB �HB �NB �TB �ZB �B �B �B �B �B �B �B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B  B  B  BB  B ��B ��B ��B  BB%B	7B	7B
=BDB
=B1B1BDBDBJBVBbB{B�B�B�B�B!�B"�B"�B"�B$�B'�B(�B'�B(�B(�B,B,B.B0!B1'B49B49B5?B8RB8RB8RB8RB9XB9XB9XB9XB9XB:^B<jB=qB>wB>wB?}B?}B>wB>wB=qB<jB;dB;dB<jB<jB=qB>wB?}B@�B?}B?}B@�B@�B@�BA�BC�BD�BD�BD�BD�BD�BD�BE�BF�BG�BG�BH�BI�BJ�BJ�BK�BK�BK�BK�BL�BL�BK�BM�BO�BP�BP�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BS�BT�BVBVBW
BW
BW
BXBW
BW
BW
BW
BW
BVBVBT�BT�BVBT�BVBVBW
BW
BW
BXBXBXBZB[#B[#BZBZB[#B[#B[#B\)B\)B]/B]/B^5B^5B_;B_;B`BBcTBcTBcTBcTBcTBdZBe`BffBffBgmBgmBgmBhsBhsBhsBiyBhsBhsBjBk�Bk�Bk�Bk�Bk�Bl�Bl�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bp�Bq�Bq�Bq�Bq�Br�Br�Br�Br�Bs�Bs�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bu�Bv�Bv�Bv�Bv�Bw�Bx�Bx�Bx�Bx�Bx�By�Bz�Bz�B{�B{�B{�B|�B|�B}�B}�B~�B~�B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�+B�+B�+B�+B�1B�1B�1B�1B�7B�=B�=B�=B�=B�DB�DB�=B�=B�=B�DB�DB�DB�DB�DB�DB�DB�DB�PB�PB�PB�VB�\B�\B�\B�\B�\B�\B�\B�\B�bB�bB�bB�hB�hB�oB�oB�oB�oB�oB�uB�uB�uB�uB�uB�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�!B�'B�'B�'B�'B�'B�-B�-B�-B�-B�-B�3B�3B�3B�3B�3B�3B�3B�3B�9B�9B�9B�9B�9B�?B�?B�9B�9B�?B�?B�?B�?B�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�LB�LB�LB�LB�RB�RB�RB�RB�RB�LB�RB�XB�XB�XB�XB�XB�^B�XB�XB�XB�XB�XB�XB�XB�XB�^B�^B�^B�^B�dB�dB�dB�dB�dB�dB�dB�dB�jB�jB�jB�jB�jB�qB�qB�jB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�wB�wB�wB�wB�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBBBBBBBBBBBBBBÖBBÖBÖBÖBBBBBÖBÖBĜBĜBĜBĜBĜBĜBŢBŢBŢBŢBĜBĜBŢBƨBƨBƨBƨBƨBƨBŢBƨBǮBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBǮBǮBǮBȴBǮBǮBǮBǮBǮBǮBǮBȴBȴBȴBȴBȴBɺB��B��B��B��B��4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210327120038                              AO  ARCAADJP                                                                    20210327120038    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210327120038  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210327120038  QCF$                G�O�G�O�G�O�8000            