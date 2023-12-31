CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:04Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140804  20181024140804  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @ן[��fl1   @ן\���@4�Q��c�^5?|�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�  @���A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C�C
�C  C  C�fC  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6  C8  C:  C<  C>�C@�CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� DfD�fDfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D�fDfD� D  D�fDfD�fD  D� D��D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#�fD$  D$y�D$��D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DC��DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DOy�DP  DP� DP��DQ� DR  DR� DS  DSy�DT  DTy�DU  DUy�DV  DV�fDW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDyg
D�$�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~{@��
@�
=A�A?�A_�A�A���A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBHG�BPG�BXG�B_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��qC�RC�RC�RC�C
�C�RC�RC޸C�RC�RC�RC޸C�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC0�C2�C3�RC5�RC7�RC9�RC;�RC>�C@�CA�RCC�RCE�RCG�RCI�RCK�RCM�RCO޸CQ�RCS�RCU�RCW޸CY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��C��)C��)C��\C��\C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��\C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�Dw�D�D~DzD�zDzD~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�Dw�D�D~D�D�zDzD~D�D�zDzD�zD�D~D��D~D�D~D�D~D�D�zD�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D"zD"~D"�D#�zD#�D$w�D$��D%~D%�D&~D'zD'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D6zD6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DCzDC~DC��DD~DD�DE~DE�DF~DF�DG~DG�DH�zDH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DNw�DN�DOw�DO�DP~DP��DQ~DQ�DR~DR�DSw�DS�DTw�DT�DUw�DU�DV�zDV�DW~DW��DX~DX�DY~DY�DZ~DZ�D[~D\zD\~D]zD]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj�zDj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Dow�Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�zDyeD�#�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ȴA˺^A�
=A��HA�Aʧ�A�Q�Aɺ^A�`BA�  A�ĜAȡ�A�r�A�E�A�+A��A���AǮAǑhA�t�A�G�A�;dA�&�A�{A�{A�bA��A��;AƼjAƥ�A�|�A�p�A�Q�A�33A�{A�  A���A�VA�7LA�7LA��A�dZA��HA�|�A��A�ȴA©�A¡�ADA�r�A�A���A���A�;dA��
A�ffA�`BA��7A�p�A��;A�|�A��mA�XA���A���A��A��\A�/A�I�A�
=A�bNA�hsA��TA��A�XA�A��;A�$�A�x�A�bNA�v�A�hsA�=qA�|�A��TA�9XA��A��#A��A�O�A�Q�A���A��A��\A���A�A�ĜA�-A�~�A���A� �A��uA�1A~bA}�Azv�Au�Aqt�AoAm�wAlbAj�Ahn�Ad�`Aa�PA^ȴA]x�A\�AZ�!AVv�AT9XARZAQ\)AQAP�`AP��API�AL�yAI|�AG
=AD�A@JA=��A;t�A9�A8�A5�A3oA1t�A0�A01'A0�A0JA0A/�A/�PA/A.��A.�yA.^5A,ĜA,n�A+O�A)��A)�PA)XA(v�A'��A&��A%oA#p�A�A�DAC�A�+AVA��A�RA��A�uA�DAr�A��AQ�A�A �A�\A�A�
A%Ax�A�uA�#A
�9A
�+A
VA	�A	��A��A�AjAx�Ar�A�A7LA�Az�AbA��A 9X@�bN@�  @�ƨ@�t�@�@�n�@�C�@�=q@�@��@��@�\)@�ff@�/@�I�@�F@�P@�|�@�
=@�!@��@�&�@�@⟾@�G�@���@�1@�M�@���@�9X@ۍP@�dZ@�"�@�@ڏ\@ؓu@�C�@��y@֏\@�V@��@Ձ@���@��@҇+@���@�bN@��;@υ@��@��@�p�@˅@Ɂ@Ȭ@�b@ǅ@��/@�`B@� �@Ǖ�@�t�@��@�@ƸR@Ƨ�@���@�\)@�@�5?@��@��j@�1@��m@��@��!@��-@�G�@�%@��@��j@���@�j@��
@��P@��@�ȴ@���@��@�G�@��`@�&�@�G�@���@��\@�@���@���@��#@�I�@�j@��u@��
@���@���@�|�@��@��7@���@�Q�@�|�@���@�~�@��H@��@�;d@�\)@�K�@��y@�{@���@��-@���@��@���@���@���@�O�@��@�&�@�Ĝ@���@���@�@�O�@��@�r�@���@�C�@�;d@��@��R@��T@��-@�\)@�Z@���@�&�@���@���@��9@�r�@�A�@�ƨ@�@�=q@�^5@�$�@�=q@�5?@��@���@�p�@�&�@���@�r�@�b@�\)@�;d@���@��R@�^5@�=q@���@�/@��`@���@���@��D@��@�1'@��;@���@�t�@�
=@��@���@���@�~�@�E�@��@���@���@��^@��-@��h@�O�@���@���@��u@�b@��
@���@�dZ@�"�@��@��@�ȴ@���@�^5@�-@���@�p�@�?}@��@�  @��P@�K�@�K�@�33@�"�@�o@���@��+@�=q@��#@�p�@�7L@�%@�Ĝ@��9@��@��@�b@��@���@�t�@�K�@�+@�o@��@��#@�p�@�O�@�&�@���@��/@��j@��u@�A�@�  @��w@�|�@�l�@�o@��!@�-@��T@��-@�?}@�b@���@�~�@�V@�-@���@��h@�7L@�%@��`@��`@���@�Q�@���@���@�K�@��@��y@��R@�E�@��@���@���@��7@��7@�p�@�?}@�&�@��@�Ĝ@�I�@��;@��P@�\)@�K�@�IR@|2�@h��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ȴA˺^A�
=A��HA�Aʧ�A�Q�Aɺ^A�`BA�  A�ĜAȡ�A�r�A�E�A�+A��A���AǮAǑhA�t�A�G�A�;dA�&�A�{A�{A�bA��A��;AƼjAƥ�A�|�A�p�A�Q�A�33A�{A�  A���A�VA�7LA�7LA��A�dZA��HA�|�A��A�ȴA©�A¡�ADA�r�A�A���A���A�;dA��
A�ffA�`BA��7A�p�A��;A�|�A��mA�XA���A���A��A��\A�/A�I�A�
=A�bNA�hsA��TA��A�XA�A��;A�$�A�x�A�bNA�v�A�hsA�=qA�|�A��TA�9XA��A��#A��A�O�A�Q�A���A��A��\A���A�A�ĜA�-A�~�A���A� �A��uA�1A~bA}�Azv�Au�Aqt�AoAm�wAlbAj�Ahn�Ad�`Aa�PA^ȴA]x�A\�AZ�!AVv�AT9XARZAQ\)AQAP�`AP��API�AL�yAI|�AG
=AD�A@JA=��A;t�A9�A8�A5�A3oA1t�A0�A01'A0�A0JA0A/�A/�PA/A.��A.�yA.^5A,ĜA,n�A+O�A)��A)�PA)XA(v�A'��A&��A%oA#p�A�A�DAC�A�+AVA��A�RA��A�uA�DAr�A��AQ�A�A �A�\A�A�
A%Ax�A�uA�#A
�9A
�+A
VA	�A	��A��A�AjAx�Ar�A�A7LA�Az�AbA��A 9X@�bN@�  @�ƨ@�t�@�@�n�@�C�@�=q@�@��@��@�\)@�ff@�/@�I�@�F@�P@�|�@�
=@�!@��@�&�@�@⟾@�G�@���@�1@�M�@���@�9X@ۍP@�dZ@�"�@�@ڏ\@ؓu@�C�@��y@֏\@�V@��@Ձ@���@��@҇+@���@�bN@��;@υ@��@��@�p�@˅@Ɂ@Ȭ@�b@ǅ@��/@�`B@� �@Ǖ�@�t�@��@�@ƸR@Ƨ�@���@�\)@�@�5?@��@��j@�1@��m@��@��!@��-@�G�@�%@��@��j@���@�j@��
@��P@��@�ȴ@���@��@�G�@��`@�&�@�G�@���@��\@�@���@���@��#@�I�@�j@��u@��
@���@���@�|�@��@��7@���@�Q�@�|�@���@�~�@��H@��@�;d@�\)@�K�@��y@�{@���@��-@���@��@���@���@���@�O�@��@�&�@�Ĝ@���@���@�@�O�@��@�r�@���@�C�@�;d@��@��R@��T@��-@�\)@�Z@���@�&�@���@���@��9@�r�@�A�@�ƨ@�@�=q@�^5@�$�@�=q@�5?@��@���@�p�@�&�@���@�r�@�b@�\)@�;d@���@��R@�^5@�=q@���@�/@��`@���@���@��D@��@�1'@��;@���@�t�@�
=@��@���@���@�~�@�E�@��@���@���@��^@��-@��h@�O�@���@���@��u@�b@��
@���@�dZ@�"�@��@��@�ȴ@���@�^5@�-@���@�p�@�?}@��@�  @��P@�K�@�K�@�33@�"�@�o@���@��+@�=q@��#@�p�@�7L@�%@�Ĝ@��9@��@��@�b@��@���@�t�@�K�@�+@�o@��@��#@�p�@�O�@�&�@���@��/@��j@��u@�A�@�  @��w@�|�@�l�@�o@��!@�-@��T@��-@�?}@�b@���@�~�@�V@�-@���@��h@�7L@�%@��`@��`@���@�Q�@���@���@�K�@��@��y@��R@�E�@��@���@���@��7@��7@�p�@�?}@�&�@��@�Ĝ@�I�@��;@��P@�\)@�K�@�IR@|2�@h��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
 �B
%�B
/B
0!B
49B
6FB
J�B
bNB
�B
�hB
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
�B
�XB
�dB
�jB
ŢB
��B
�B
��B	7B�B�B!�B6FB<jBC�BF�Bu�BǮB��B{B$�B-B>wBH�BK�BL�BN�BO�BW
BW
B`BBdZBgmBjBx�B�JB��B��B��B��B��B��B��B��B��B��B��B�\B{�Bn�BhsB\)BM�B:^B1'B+B"�B�BPBVBJBB��B�B��B�LB��B�BcTBJ�B(�BPB
�B
�B
��B
�B
\)B
G�B
7LB
.B
#�B
bB
DB	��B	�)B	ÖB	�FB	�B	��B	��B	�DB	v�B	dZB	T�B	L�B	D�B	;dB	%�B	�B	�B	�B	�B	�B	�B	uB	B��B�B�;B��BǮBB��B�dB�9B�'B�'B�FBĜB��B��B��B��B��B��B��B��B��B�B�B�#B�5B�/B�/B�;B�B��BB�FB��B�oB�7B�1B�JB�DB�DB�DB�DB�=B�=B�PB�bB�VB�PB�DB�=B�=B�+B�B�B�B�B�B�B�B�B�B�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�!B�'B�!B�!B�!B�-B�LB�XB�XB�^B�dB�qB�wB�}B�}B�}B�}B�}BɺB��B��B��B��B��B��B�
B�B�;B�mB�sB�B�B�B�B��B��B��B��B��B��B	
=B	oB	hB	VB	\B	VB	PB	VB	uB	�B	�B	�B	�B	 �B	#�B	%�B	&�B	%�B	'�B	)�B	,B	0!B	49B	8RB	:^B	<jB	@�B	F�B	F�B	F�B	G�B	H�B	L�B	P�B	R�B	XB	ZB	YB	^5B	e`B	gmB	cTB	`BB	cTB	jB	n�B	p�B	r�B	s�B	t�B	o�B	m�B	o�B	s�B	r�B	x�B	{�B	|�B	~�B	~�B	�B	�B	�B	�%B	�7B	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�XB	�qB	��B	��B	B	B	ÖB	ŢB	ÖB	��B	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�;B	�HB	�NB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
+B
+B
+B
	7B
	7B

=B
DB

=B

=B
DB
PB
PB
VB
VB
VB
VB
\B
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
uB
uB
{B
�B
�B
�B
�B
0;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
 �B
%�B
/B
0!B
49B
6FB
J�B
bNB
�B
�hB
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
�B
�XB
�dB
�jB
ŢB
��B
�B
��B	7B�B�B!�B6FB<jBC�BF�Bu�BǮB��B{B$�B-B>wBH�BK�BL�BN�BO�BW
BW
B`BBdZBgmBjBx�B�JB��B��B��B��B��B��B��B��B��B��B��B�\B{�Bn�BhsB\)BM�B:^B1'B+B"�B�BPBVBJBB��B�B��B�LB��B�BcTBJ�B(�BPB
�B
�B
��B
�B
\)B
G�B
7LB
.B
#�B
bB
DB	��B	�)B	ÖB	�FB	�B	��B	��B	�DB	v�B	dZB	T�B	L�B	D�B	;dB	%�B	�B	�B	�B	�B	�B	�B	uB	B��B�B�;B��BǮBB��B�dB�9B�'B�'B�FBĜB��B��B��B��B��B��B��B��B��B�B�B�#B�5B�/B�/B�;B�B��BB�FB��B�oB�7B�1B�JB�DB�DB�DB�DB�=B�=B�PB�bB�VB�PB�DB�=B�=B�+B�B�B�B�B�B�B�B�B�B�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�!B�'B�!B�!B�!B�-B�LB�XB�XB�^B�dB�qB�wB�}B�}B�}B�}B�}BɺB��B��B��B��B��B��B�
B�B�;B�mB�sB�B�B�B�B��B��B��B��B��B��B	
=B	oB	hB	VB	\B	VB	PB	VB	uB	�B	�B	�B	�B	 �B	#�B	%�B	&�B	%�B	'�B	)�B	,B	0!B	49B	8RB	:^B	<jB	@�B	F�B	F�B	F�B	G�B	H�B	L�B	P�B	R�B	XB	ZB	YB	^5B	e`B	gmB	cTB	`BB	cTB	jB	n�B	p�B	r�B	s�B	t�B	o�B	m�B	o�B	s�B	r�B	x�B	{�B	|�B	~�B	~�B	�B	�B	�B	�%B	�7B	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�XB	�qB	��B	��B	B	B	ÖB	ŢB	ÖB	��B	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�;B	�HB	�NB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
+B
+B
+B
	7B
	7B

=B
DB

=B

=B
DB
PB
PB
VB
VB
VB
VB
\B
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
uB
uB
{B
�B
�B
�B
�B
0;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140804                              AO  ARCAADJP                                                                    20181024140804    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140804  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181024140804  QCF$                G�O�G�O�G�O�0               