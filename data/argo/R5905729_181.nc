CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-11T09:01:35Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230411090135  20230411090135  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�"�"",v1   @�"շ��2@+ݥ��v�d�@N��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��BýqB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC6�C7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
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
D�;�D�
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
D��D��
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
D��
D�?
D�
D��
D��
D�?
D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�I�A�I�A�M�A�O�A�Q�A�Q�A�S�A�S�A�VA�VA�XA�XA�ZA�\)A�ZA�G�A�9XA�9XA�=qA�?}A�9XA�7LA�/A�"�A��A�bA�
=A�JA�  A�XA��A��A��A�-A�A͗�A͑hA�;dA��yA�l�A�$�Aʩ�A���A�~�A���A�O�A���A�K�A���A��9A���A���A�E�A�ƨA��
A��;A���A�1'A�$�A�1'A�I�A�A���A�|�A�XA�G�A��/A�9XA�33A���A��A�jA��
A��wA��uA��/A~�9Az1'At�/Ar1Aq/Ap �Al�Ak/AiƨAfA�Ab��A\�jAXv�AV=qAS��AQ��AOdZAJ�AG�AE�AC��AB��AA�A?�PA=%A9�;A7�;A4M�A3dZA2�yA25?A133A/�-A/K�A/oA.�uA-p�A+G�A*��A)�A(A�A'VA&�`A&ȴA&�RA&{A%��A%+A$��A$(�A"�`A"�\A!O�A Q�A�mA��A�A�AȴA��A%A�\Ar�AjAQ�A1A
=An�A�A�mA��A��A|�AK�AȴA�A��A�A��A
=A��A��A �A��A�hA33A��A�A�AƨA�A�AbNA�;A`BA�A��A�`A�!AE�A�A1A��A�;A��A�-A�hA�AXA
=A��Av�AbNAA�wA|�A�A��An�A-A�A  A�FA�7AS�A+A
�RA
ZA	�
A	dZA�`A�AQ�A1A��A�mA�^A��A�A|�A/AA�A�TA\)A%A�`AȴA��An�A-A�FAx�A�!A=qA�7A33A �A ĜA �+A jA I�A  �@��m@�33@�33@�C�@�K�@�;d@���@�@��`@� �@��@�|�@�K�@�^5@���@�`B@�/@��@���@�E�@��T@��^@���@�O�@���@�j@�1'@��@�;d@�@�!@�ff@�-@�-@���@���@��@��/@��D@��@�\@�K�@�/@��m@�F@���@�ff@�h@���@��@㝲@�t�@�"�@�@���@�-@�^@��@� �@���@�"�@���@�^5@�-@��T@�x�@�G�@��`@���@�Ĝ@�9X@���@�l�@�;d@���@�~�@ٙ�@�Ĝ@�(�@׍P@ְ!@��#@�G�@��@ԃ@�(�@��
@�t�@��@��@�`B@��@Ь@��@υ@���@ΰ!@�5?@�x�@̣�@�A�@��@�C�@��@ʗ�@��#@�hs@���@�j@�9X@�j@���@�dZ@Ɨ�@Ɨ�@�"�@Ų-@�%@�1'@�t�@�C�@�@°!@+@��T@��@���@��@�Z@�I�@��D@��/@��u@�9X@��F@�S�@��y@��y@���@�v�@��@�O�@��/@�Q�@� �@�b@��m@��@�@��!@�ff@��@�@��7@�`B@�?}@���@�r�@�1@��w@�t�@��y@�ȴ@��+@�ff@�E�@�$�@�@�@�x�@�&�@���@��/@���@�1'@��@��+@�J@�?}@�Ĝ@�bN@� �@�S�@�33@��@��@��+@�v�@�J@�@��h@�O�@���@�bN@�I�@� �@�  @�ƨ@�\)@��@���@�=q@���@�G�@�V@��@�I�@��;@�t�@�ȴ@�^5@�@�Ĝ@�I�@�1@���@��@�dZ@��y@��\@�-@�@��7@�?}@���@��D@�Z@�1'@��@�ƨ@�33@��\@�J@��T@��^@��7@�V@��j@�z�@�bN@�I�@���@�;d@�o@���@�M�@�@��^@�x�@�/@�V@��j@��D@�I�@� �@�b@�b@��@��@�1@��m@��@��@���@���@�ff@�=q@�{@��T@���@�`B@�&�@���@� �@�  @���@��
@��@��@�S�@�"�@�
=@��R@�~�@�^5@�E�@��@��T@��h@�X@���@���@�I�@���@�dZ@�K�@�;d@���@��\@�^5@���@���@�?}@��/@��D@��@�Z@�1'@�ƨ@���@�l�@��@���@���@��R@��R@�v�@�=q@�@���@�x�@��/@�Z@�b@�ƨ@���@��P@�dZ@�33@��H@���@�ff@�E�@��@��T@��^@�hs@�%@��`@���@��j@�Z@��@��w@�S�@�@���@�^5@��@��@�M�@�V@��@��@�@�p�@�V@��j@�bN@�1@K�@�@
=@~�y@~�@~��@~E�@}�h@}V@|��@{�
@{ƨ@{ƨ@{ƨ@{S�@zM�@y�^@xĜ@x  @w�P@w\)@w�@v�@v��@u�T@uV@tj@t9X@sƨ@rM�@qG�@q�@p��@p��@p�9@pr�@pb@o�;@o��@o|�@oK�@nȴ@nE�@m�T@mp�@l��@lI�@k�
@kdZ@j�H@j~�@j-@i��@i��@h�9@h1'@g�P@g\)@g+@f�+@ep�@eV@dI�@c�
@c��@ct�@c@bn�@a��@ax�@a7L@`Ĝ@`bN@`bN@`Q�@` �@_\)@_
=@^��@^�y@^�R@^�+@^E�@]�@]V@\�@\I�@[�
@[ƨ@[C�@Z��@Z-@Y��@Y�#@Y��@Y%@X�@Xr�@X1'@X  @W�@W;d@V�y@V��@V�+@VV@U@U?}@T�D@T1@SS�@R�!@Rn�@RM�@R�@Q�^@Qhs@QX@P��@O��@N�R@M@M�h@M/@L��@L��@L(�@Kt�@J~�@IG�@I%@H��@H��@HbN@H  @G�P@F�R@F$�@F{@F{@E�@E@E?}@D��@D�D@DI�@Cƨ@C"�@B��@B~�@Bn�@B-@A��@A��@AG�@@bN@?�;@?�@?�@?��@>�R@>5?@>�@>E�@<�j@;o@:�!@;"�@:�!@:=q@9�#@9��@9��@9�7@9G�@8Ĝ@8r�@81'@7�;@7�@7|�@7K�@6��@6��@6ff@6V@6V@5�@5��@5�h@5�@4��@4Z@4I�@4(�@3ƨ@3dZ@3"�@2�H@2�\@2�@1�@0��@0��@0r�@/�;@/�P@/l�@/;d@/�@.��@.E�@.$�@.{@-�@-@-�-@-�h@-�h@-�h@-p�@-V@,�/@,��@,j@,I�@,(�@,�@,1@,1@+��@+��@+��@+��@+�m@+ƨ@+t�@+o@*�H@*��@*=q@*�@*J@)��@)hs@)G�@)&�@)�@)�@)%@(��@(bN@( �@'�@'�w@'��@'+@&E�@&5?@&E�@%�@%`B@$z�@#��@#�F@#t�@#S�@#33@"��@"=q@!�@!�^@!G�@ ��@ �9@ Q�@ 1'@  �@   @�@�;@��@l�@�@��@5?@��@�-@�@p�@/@��@�j@��@z�@j@Z@9X@9X@�m@��@�@S�@33@o@�H@��@��@~�@=q@�@�@�#@��@��@hs@X@7L@&�@�9@1'@�@|�@l�@l�@\)@;d@�@��@ȴ@�+@V@5?@@�T@O�@�/@��@�j@�@��@z�@j@Z@Z@Z@Z@9X@(�@1@�F@t�@C�@�@~�@M�@J@��@�@�^@��@x�@G�@�@�`@�`@Ĝ@Ĝ@�9@�u@bN@1'@�@�w@|�@\)@;d@�@��@��@�y@�@ȴ@�R@v�@$�@@�@�T@��@@�-@��@�h@�@O�@?}@?}@/@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�I�A�I�A�I�A�M�A�O�A�Q�A�Q�A�S�A�S�A�VA�VA�XA�XA�ZA�\)A�ZA�G�A�9XA�9XA�=qA�?}A�9XA�7LA�/A�"�A��A�bA�
=A�JA�  A�XA��A��A��A�-A�A͗�A͑hA�;dA��yA�l�A�$�Aʩ�A���A�~�A���A�O�A���A�K�A���A��9A���A���A�E�A�ƨA��
A��;A���A�1'A�$�A�1'A�I�A�A���A�|�A�XA�G�A��/A�9XA�33A���A��A�jA��
A��wA��uA��/A~�9Az1'At�/Ar1Aq/Ap �Al�Ak/AiƨAfA�Ab��A\�jAXv�AV=qAS��AQ��AOdZAJ�AG�AE�AC��AB��AA�A?�PA=%A9�;A7�;A4M�A3dZA2�yA25?A133A/�-A/K�A/oA.�uA-p�A+G�A*��A)�A(A�A'VA&�`A&ȴA&�RA&{A%��A%+A$��A$(�A"�`A"�\A!O�A Q�A�mA��A�A�AȴA��A%A�\Ar�AjAQ�A1A
=An�A�A�mA��A��A|�AK�AȴA�A��A�A��A
=A��A��A �A��A�hA33A��A�A�AƨA�A�AbNA�;A`BA�A��A�`A�!AE�A�A1A��A�;A��A�-A�hA�AXA
=A��Av�AbNAA�wA|�A�A��An�A-A�A  A�FA�7AS�A+A
�RA
ZA	�
A	dZA�`A�AQ�A1A��A�mA�^A��A�A|�A/AA�A�TA\)A%A�`AȴA��An�A-A�FAx�A�!A=qA�7A33A �A ĜA �+A jA I�A  �@��m@�33@�33@�C�@�K�@�;d@���@�@��`@� �@��@�|�@�K�@�^5@���@�`B@�/@��@���@�E�@��T@��^@���@�O�@���@�j@�1'@��@�;d@�@�!@�ff@�-@�-@���@���@��@��/@��D@��@�\@�K�@�/@��m@�F@���@�ff@�h@���@��@㝲@�t�@�"�@�@���@�-@�^@��@� �@���@�"�@���@�^5@�-@��T@�x�@�G�@��`@���@�Ĝ@�9X@���@�l�@�;d@���@�~�@ٙ�@�Ĝ@�(�@׍P@ְ!@��#@�G�@��@ԃ@�(�@��
@�t�@��@��@�`B@��@Ь@��@υ@���@ΰ!@�5?@�x�@̣�@�A�@��@�C�@��@ʗ�@��#@�hs@���@�j@�9X@�j@���@�dZ@Ɨ�@Ɨ�@�"�@Ų-@�%@�1'@�t�@�C�@�@°!@+@��T@��@���@��@�Z@�I�@��D@��/@��u@�9X@��F@�S�@��y@��y@���@�v�@��@�O�@��/@�Q�@� �@�b@��m@��@�@��!@�ff@��@�@��7@�`B@�?}@���@�r�@�1@��w@�t�@��y@�ȴ@��+@�ff@�E�@�$�@�@�@�x�@�&�@���@��/@���@�1'@��@��+@�J@�?}@�Ĝ@�bN@� �@�S�@�33@��@��@��+@�v�@�J@�@��h@�O�@���@�bN@�I�@� �@�  @�ƨ@�\)@��@���@�=q@���@�G�@�V@��@�I�@��;@�t�@�ȴ@�^5@�@�Ĝ@�I�@�1@���@��@�dZ@��y@��\@�-@�@��7@�?}@���@��D@�Z@�1'@��@�ƨ@�33@��\@�J@��T@��^@��7@�V@��j@�z�@�bN@�I�@���@�;d@�o@���@�M�@�@��^@�x�@�/@�V@��j@��D@�I�@� �@�b@�b@��@��@�1@��m@��@��@���@���@�ff@�=q@�{@��T@���@�`B@�&�@���@� �@�  @���@��
@��@��@�S�@�"�@�
=@��R@�~�@�^5@�E�@��@��T@��h@�X@���@���@�I�@���@�dZ@�K�@�;d@���@��\@�^5@���@���@�?}@��/@��D@��@�Z@�1'@�ƨ@���@�l�@��@���@���@��R@��R@�v�@�=q@�@���@�x�@��/@�Z@�b@�ƨ@���@��P@�dZ@�33@��H@���@�ff@�E�@��@��T@��^@�hs@�%@��`@���@��j@�Z@��@��w@�S�@�@���@�^5@��@��@�M�@�V@��@��@�@�p�@�V@��j@�bN@�1@K�@�@
=@~�y@~�@~��@~E�@}�h@}V@|��@{�
@{ƨ@{ƨ@{ƨ@{S�@zM�@y�^@xĜ@x  @w�P@w\)@w�@v�@v��@u�T@uV@tj@t9X@sƨ@rM�@qG�@q�@p��@p��@p�9@pr�@pb@o�;@o��@o|�@oK�@nȴ@nE�@m�T@mp�@l��@lI�@k�
@kdZ@j�H@j~�@j-@i��@i��@h�9@h1'@g�P@g\)@g+@f�+@ep�@eV@dI�@c�
@c��@ct�@c@bn�@a��@ax�@a7L@`Ĝ@`bN@`bN@`Q�@` �@_\)@_
=@^��@^�y@^�R@^�+@^E�@]�@]V@\�@\I�@[�
@[ƨ@[C�@Z��@Z-@Y��@Y�#@Y��@Y%@X�@Xr�@X1'@X  @W�@W;d@V�y@V��@V�+@VV@U@U?}@T�D@T1@SS�@R�!@Rn�@RM�@R�@Q�^@Qhs@QX@P��@O��@N�R@M@M�h@M/@L��@L��@L(�@Kt�@J~�@IG�@I%@H��@H��@HbN@H  @G�P@F�R@F$�@F{@F{@E�@E@E?}@D��@D�D@DI�@Cƨ@C"�@B��@B~�@Bn�@B-@A��@A��@AG�@@bN@?�;@?�@?�@?��@>�R@>5?@>�@>E�@<�j@;o@:�!@;"�@:�!@:=q@9�#@9��@9��@9�7@9G�@8Ĝ@8r�@81'@7�;@7�@7|�@7K�@6��@6��@6ff@6V@6V@5�@5��@5�h@5�@4��@4Z@4I�@4(�@3ƨ@3dZ@3"�@2�H@2�\@2�@1�@0��@0��@0r�@/�;@/�P@/l�@/;d@/�@.��@.E�@.$�@.{@-�@-@-�-@-�h@-�h@-�h@-p�@-V@,�/@,��@,j@,I�@,(�@,�@,1@,1@+��@+��@+��@+��@+�m@+ƨ@+t�@+o@*�H@*��@*=q@*�@*J@)��@)hs@)G�@)&�@)�@)�@)%@(��@(bN@( �@'�@'�w@'��@'+@&E�@&5?@&E�@%�@%`B@$z�@#��@#�F@#t�@#S�@#33@"��@"=q@!�@!�^@!G�@ ��@ �9@ Q�@ 1'@  �@   @�@�;@��@l�@�@��@5?@��@�-@�@p�@/@��@�j@��@z�@j@Z@9X@9X@�m@��@�@S�@33@o@�H@��@��@~�@=q@�@�@�#@��@��@hs@X@7L@&�@�9@1'@�@|�@l�@l�@\)@;d@�@��@ȴ@�+@V@5?@@�T@O�@�/@��@�j@�@��@z�@j@Z@Z@Z@Z@9X@(�@1@�F@t�@C�@�@~�@M�@J@��@�@�^@��@x�@G�@�@�`@�`@Ĝ@Ĝ@�9@�u@bN@1'@�@�w@|�@\)@;d@�@��@��@�y@�@ȴ@�R@v�@$�@@�@�T@��@@�-@��@�h@�@O�@?}@?}@/@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�;B	�B
bB
�B
+B
 �B
�B
�B
�B	��B	��B
%�B
0!B
5?B
8RB
gmB
�bB
��B
�B��B��B�B��B�B�BB�yB��B��B�{Bt�BO�B7LB+B�B�BJBPB
��B
�mB
��B
�!B
u�B
W
B
8RB
1B	��B	�HB	�^B	��B	��B	��B	��B	z�B	{�B	jB	B�B	/B	
=B	  B	�B	PB		7B	B�B��B	�B	JB	/B	(�B	 �B	�B	uB	(�B	 �B	K�B	S�B	S�B	R�B	R�B	iyB	m�B	iyB	ffB	iyB	��B	��B	�bB	��B	�-B	�3B	�3B	�B	�?B	�9B	�XB	B	B	�B	�
B	�ZB	��B	��B	��B	��B	��B	��B
	7B
\B
{B
�B
�B
oB
VB
�B
 �B
"�B
#�B
"�B
%�B
%�B
$�B
)�B
2-B
8RB
7LB
49B
:^B
:^B
:^B
:^B
A�B
=qB
<jB
=qB
C�B
C�B
A�B
>wB
?}B
B�B
D�B
G�B
I�B
I�B
H�B
F�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
K�B
J�B
I�B
J�B
L�B
L�B
I�B
G�B
I�B
G�B
I�B
G�B
I�B
K�B
J�B
H�B
J�B
J�B
J�B
F�B
D�B
C�B
C�B
F�B
H�B
G�B
H�B
M�B
M�B
L�B
L�B
K�B
K�B
G�B
@�B
D�B
C�B
C�B
F�B
E�B
D�B
D�B
B�B
?}B
>wB
9XB
8RB
5?B
7LB
9XB
;dB
:^B
:^B
<jB
:^B
:^B
8RB
=qB
>wB
>wB
<jB
9XB
5?B
6FB
7LB
8RB
8RB
7LB
33B
33B
33B
33B
2-B
,B
.B
1'B
0!B
0!B
.B
-B
,B
-B
-B
/B
0!B
1'B
1'B
0!B
.B
+B
.B
,B
/B
+B
$�B
�B
JB
1B
	7B
JB

=B
+B
B
  B
+B
	7B
1B
+B
1B
%B
B
B	��B
B
B
  B
B
B
B
B
B
B
B
B
B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
+B
1B

=B

=B
JB
DB
JB
{B
bB
hB
\B
\B
oB
oB
hB
hB
\B
\B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
�B
#�B
#�B
"�B
"�B
#�B
"�B
"�B
#�B
"�B
!�B
 �B
"�B
"�B
!�B
 �B
�B
 �B
 �B
!�B
�B
!�B
!�B
!�B
�B
 �B
�B
�B
 �B
�B
�B
!�B
#�B
$�B
$�B
$�B
"�B
#�B
$�B
$�B
%�B
$�B
$�B
%�B
&�B
%�B
%�B
$�B
"�B
"�B
%�B
'�B
'�B
'�B
&�B
'�B
)�B
)�B
+B
)�B
(�B
,B
+B
)�B
+B
,B
-B
-B
.B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
/B
.B
.B
,B
/B
0!B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
/B
33B
49B
49B
33B
33B
33B
33B
33B
2-B
33B
49B
49B
49B
33B
33B
33B
2-B
33B
33B
49B
33B
6FB
7LB
5?B
6FB
6FB
5?B
6FB
5?B
7LB
9XB
;dB
:^B
:^B
9XB
;dB
:^B
;dB
<jB
>wB
?}B
?}B
>wB
>wB
?}B
>wB
=qB
;dB
<jB
>wB
?}B
@�B
@�B
@�B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
@�B
@�B
B�B
C�B
B�B
A�B
@�B
C�B
B�B
B�B
D�B
D�B
E�B
H�B
K�B
K�B
I�B
J�B
J�B
I�B
H�B
I�B
J�B
K�B
K�B
M�B
M�B
M�B
M�B
M�B
L�B
L�B
L�B
M�B
L�B
O�B
O�B
N�B
L�B
K�B
N�B
M�B
O�B
P�B
R�B
Q�B
Q�B
Q�B
P�B
P�B
Q�B
R�B
Q�B
O�B
Q�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
VB
VB
W
B
VB
VB
W
B
W
B
XB
XB
YB
YB
YB
YB
W
B
YB
YB
[#B
[#B
YB
YB
[#B
[#B
\)B
]/B
]/B
]/B
\)B
]/B
_;B
_;B
^5B
_;B
`BB
`BB
_;B
^5B
`BB
aHB
aHB
`BB
`BB
`BB
_;B
`BB
aHB
aHB
aHB
bNB
aHB
aHB
bNB
cTB
cTB
cTB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
dZB
dZB
dZB
dZB
dZB
ffB
gmB
hsB
gmB
gmB
gmB
gmB
e`B
e`B
ffB
ffB
iyB
iyB
iyB
iyB
hsB
hsB
hsB
hsB
l�B
l�B
l�B
k�B
k�B
k�B
jB
l�B
n�B
n�B
m�B
m�B
l�B
m�B
n�B
n�B
m�B
m�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
p�B
m�B
m�B
o�B
n�B
k�B
jB
l�B
p�B
o�B
o�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
p�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
t�B
s�B
t�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
y�B
z�B
z�B
z�B
y�B
y�B
|�B
~�B
|�B
|�B
{�B
}�B
~�B
� B
� B
� B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�%B
�%B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�1B
�7B
�7B
�7B
�1B
�1B
�1B
�7B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�PB
�VB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�PB
�PB
�PB
�VB
�VB
�VB
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
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
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�;B	�B
bB
�B
+B
 �B
�B
�B
�B	��B	��B
%�B
0!B
5?B
8RB
gmB
�bB
��B
�B��B��B�B��B�B�BB�yB��B��B�{Bt�BO�B7LB+B�B�BJBPB
��B
�mB
��B
�!B
u�B
W
B
8RB
1B	��B	�HB	�^B	��B	��B	��B	��B	z�B	{�B	jB	B�B	/B	
=B	  B	�B	PB		7B	B�B��B	�B	JB	/B	(�B	 �B	�B	uB	(�B	 �B	K�B	S�B	S�B	R�B	R�B	iyB	m�B	iyB	ffB	iyB	��B	��B	�bB	��B	�-B	�3B	�3B	�B	�?B	�9B	�XB	B	B	�B	�
B	�ZB	��B	��B	��B	��B	��B	��B
	7B
\B
{B
�B
�B
oB
VB
�B
 �B
"�B
#�B
"�B
%�B
%�B
$�B
)�B
2-B
8RB
7LB
49B
:^B
:^B
:^B
:^B
A�B
=qB
<jB
=qB
C�B
C�B
A�B
>wB
?}B
B�B
D�B
G�B
I�B
I�B
H�B
F�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
K�B
J�B
I�B
J�B
L�B
L�B
I�B
G�B
I�B
G�B
I�B
G�B
I�B
K�B
J�B
H�B
J�B
J�B
J�B
F�B
D�B
C�B
C�B
F�B
H�B
G�B
H�B
M�B
M�B
L�B
L�B
K�B
K�B
G�B
@�B
D�B
C�B
C�B
F�B
E�B
D�B
D�B
B�B
?}B
>wB
9XB
8RB
5?B
7LB
9XB
;dB
:^B
:^B
<jB
:^B
:^B
8RB
=qB
>wB
>wB
<jB
9XB
5?B
6FB
7LB
8RB
8RB
7LB
33B
33B
33B
33B
2-B
,B
.B
1'B
0!B
0!B
.B
-B
,B
-B
-B
/B
0!B
1'B
1'B
0!B
.B
+B
.B
,B
/B
+B
$�B
�B
JB
1B
	7B
JB

=B
+B
B
  B
+B
	7B
1B
+B
1B
%B
B
B	��B
B
B
  B
B
B
B
B
B
B
B
B
B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
+B
1B

=B

=B
JB
DB
JB
{B
bB
hB
\B
\B
oB
oB
hB
hB
\B
\B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
�B
#�B
#�B
"�B
"�B
#�B
"�B
"�B
#�B
"�B
!�B
 �B
"�B
"�B
!�B
 �B
�B
 �B
 �B
!�B
�B
!�B
!�B
!�B
�B
 �B
�B
�B
 �B
�B
�B
!�B
#�B
$�B
$�B
$�B
"�B
#�B
$�B
$�B
%�B
$�B
$�B
%�B
&�B
%�B
%�B
$�B
"�B
"�B
%�B
'�B
'�B
'�B
&�B
'�B
)�B
)�B
+B
)�B
(�B
,B
+B
)�B
+B
,B
-B
-B
.B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
/B
.B
.B
,B
/B
0!B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
/B
33B
49B
49B
33B
33B
33B
33B
33B
2-B
33B
49B
49B
49B
33B
33B
33B
2-B
33B
33B
49B
33B
6FB
7LB
5?B
6FB
6FB
5?B
6FB
5?B
7LB
9XB
;dB
:^B
:^B
9XB
;dB
:^B
;dB
<jB
>wB
?}B
?}B
>wB
>wB
?}B
>wB
=qB
;dB
<jB
>wB
?}B
@�B
@�B
@�B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
@�B
@�B
B�B
C�B
B�B
A�B
@�B
C�B
B�B
B�B
D�B
D�B
E�B
H�B
K�B
K�B
I�B
J�B
J�B
I�B
H�B
I�B
J�B
K�B
K�B
M�B
M�B
M�B
M�B
M�B
L�B
L�B
L�B
M�B
L�B
O�B
O�B
N�B
L�B
K�B
N�B
M�B
O�B
P�B
R�B
Q�B
Q�B
Q�B
P�B
P�B
Q�B
R�B
Q�B
O�B
Q�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
VB
VB
W
B
VB
VB
W
B
W
B
XB
XB
YB
YB
YB
YB
W
B
YB
YB
[#B
[#B
YB
YB
[#B
[#B
\)B
]/B
]/B
]/B
\)B
]/B
_;B
_;B
^5B
_;B
`BB
`BB
_;B
^5B
`BB
aHB
aHB
`BB
`BB
`BB
_;B
`BB
aHB
aHB
aHB
bNB
aHB
aHB
bNB
cTB
cTB
cTB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
dZB
dZB
dZB
dZB
dZB
ffB
gmB
hsB
gmB
gmB
gmB
gmB
e`B
e`B
ffB
ffB
iyB
iyB
iyB
iyB
hsB
hsB
hsB
hsB
l�B
l�B
l�B
k�B
k�B
k�B
jB
l�B
n�B
n�B
m�B
m�B
l�B
m�B
n�B
n�B
m�B
m�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
p�B
m�B
m�B
o�B
n�B
k�B
jB
l�B
p�B
o�B
o�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
p�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
t�B
s�B
t�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
y�B
z�B
z�B
z�B
y�B
y�B
|�B
~�B
|�B
|�B
{�B
}�B
~�B
� B
� B
� B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�%B
�%B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�1B
�7B
�7B
�7B
�1B
�1B
�1B
�7B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�PB
�VB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�PB
�PB
�PB
�VB
�VB
�VB
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
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
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230411090135                              AO  ARCAADJP                                                                    20230411090135    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230411090135  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230411090135  QCF$                G�O�G�O�G�O�0               