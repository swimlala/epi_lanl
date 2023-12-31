CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-12-02T10:01:44Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221202100144  20221202100144  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�SK��1   @�S����@)Ձ$�/�d��l�C�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  BhffBpffBxffB�  B���B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DUy�DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D\��D]� D^fD^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~fD~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@��
@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBPG�BW�HB_�HBhG�BpG�BxG�B�HB��qB��B��B�#�B��qB��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC(�C)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��\C��)C��C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D zD ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,w�D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DUw�DU��DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\��D]~D^zD^�zD^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~DgzDg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D~zD~~D~�D~D�D�?
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
D��
D�?
D�
D��
D��
D�?
D��=D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�p�A�n�A�jA�jA�hsA�jA�l�A�l�A�p�A�t�A�v�A�r�A�v�A�t�A�t�A�XA�VA�
=A�=qA�ZA�ffA�p�A�z�AׁA׉7Aם�A׶FA��HA���A�JA��A��/A�/Aֺ^AՅA��yA��#A�-A�ƨA��#A�+A�Q�A�&�A�{A�x�A���A�XA�{A�G�A��A��A�I�A���A�\)A��A�{A��hA�z�A���A�5?A�I�A��FA�|�A�ȴA��A�{A��A�^5A�~�A���A�?}A�O�A�oA�A��A~�9A|�A{|�A{;dA{oAz  Av�\ArI�Ap�DAnbAg�Afn�Af$�Ae��Ac�;A^�jAXjAVA�AT9XAL��AG�wAEoADZACƨAB�9AA�A=�-A9\)A7�PA6VA4ȴA3S�A2�/A2M�A1�A0r�A01A.�A,��A*n�A&�!A$��A#�TA#oA"�RA"�!A"�\A"I�A!�FA!A�hA��A��AK�A��A�AA�A�#A"�A��A�An�A�uAbNA��A�A�A  A��A�A&�A-A�hA�uA�PA�!AbNA{Ap�A
�DA
�uA
M�A	\)A	%A�A�/A  A�!A�TA33A%A�HA��A~�A1A��A�A�
A�A�A��AXA\)A �A��AO�A��A�DA~�A�RA�`A�jAbA�-A\)A�`A��A�9An�A�TAG�A �DA M�A (�A {A   @��;@���@�@�V@��m@���@��H@�=q@���@�hs@�/@���@�1@�;d@��-@��`@�A�@�@�O�@��@���@��D@�A�@��
@�P@�K�@�+@�
=@�ȴ@@�v�@��T@�x�@�V@�w@ꗍ@�$�@��@�A�@��@��@�j@�(�@���@�{@�7L@�1@�l�@���@�E�@��#@�&�@�A�@�1@ۥ�@�"�@ڇ+@�-@ٲ-@�`B@��@�r�@�t�@���@��@���@�r�@��@�l�@���@���@с@���@�z�@�1@Ϯ@�|�@�+@��H@���@Χ�@�@�hs@�&�@�j@�K�@�ȴ@ʏ\@�n�@�$�@�hs@ȋD@�t�@Ƨ�@�E�@ŉ7@ēu@�  @��
@öF@Õ�@�l�@�\)@�dZ@�\)@���@�-@���@�hs@���@��@�j@�b@�"�@���@�^5@��#@��7@�X@��@��@�Ĝ@���@���@�C�@�"�@�
=@��y@��!@�E�@��-@�&�@��u@�9X@��@��@��y@�J@�hs@���@�A�@� �@��@�;d@�o@��y@��!@��\@�~�@�E�@�5?@��@��@��7@�X@�&�@���@��/@�Ĝ@���@�r�@�1@�t�@��@�ȴ@�V@�hs@��@��/@���@���@�z�@��;@��@��@�V@�r�@�t�@��@�v�@�{@��@���@�O�@���@�Ĝ@���@���@��@��@���@�M�@�J@��@��^@�&�@���@�bN@��m@�S�@���@���@���@���@���@���@���@�~�@�^5@�E�@�$�@�@���@���@��j@��D@�j@�I�@�A�@�(�@�b@�ƨ@��P@�dZ@��@�^5@�J@�@��#@��7@�hs@�X@�?}@���@��9@���@�bN@�1@��
@��P@�;d@�+@��@���@��R@�-@�J@��@��#@���@���@�X@�&�@��9@�j@�(�@���@���@��R@��@���@��h@�/@��@��@�r�@��@��@�S�@�+@��@�n�@�{@���@���@��u@�Q�@�(�@�1@�t�@��@��@��R@�n�@�=q@�5?@�$�@��T@�x�@�z�@��w@�S�@�;d@��@��y@�ȴ@��!@�~�@�E�@�$�@��-@�Ĝ@�z�@� �@���@���@��@�\)@�n�@��@��#@���@���@��h@��@�`B@�?}@��@���@�I�@��@
=@~v�@}��@}��@}�@}`B@}V@|�j@|(�@{�@{"�@z��@z-@y��@yhs@xĜ@xA�@x �@wl�@v5?@u@u��@up�@uO�@t��@t�/@tj@st�@r�@r~�@rM�@q�@q��@qG�@q�@p�`@p��@p�@p1'@p1'@o�;@o�P@o;d@n��@n��@m`B@l�j@lj@lI�@l�@k�m@k�
@k��@k"�@j�H@j��@j��@j�!@j��@jn�@j^5@j-@i�@iG�@i7L@hĜ@h1'@g��@g;d@f��@fȴ@f�R@f�R@f��@fv�@f5?@f{@e�T@e��@e?}@d�D@d1@c�F@c��@c��@c��@ct�@co@b�H@b~�@b=q@bM�@b=q@a�^@`Ĝ@`�u@`�@`Q�@_�w@^��@^v�@^ff@^V@^5?@^{@]�@]O�@\��@\(�@[t�@Z��@ZM�@Y�^@YX@Y7L@Y%@X��@XĜ@X�9@X�9@X  @W\)@V�y@Vȴ@V��@V5?@U�@Up�@U/@UV@T��@T��@T�j@T(�@T�@Sƨ@SdZ@RM�@Q�#@Q��@Qx�@Q�@P��@PQ�@O�@O\)@O�@N��@N{@M��@M`B@M/@Lj@K�F@KdZ@Ko@J�@J�!@J~�@Jn�@JM�@JJ@I��@IX@H��@HĜ@H�u@Hr�@HA�@H �@G�@G��@F�@F�R@F�+@E�T@E��@E�T@E�T@E�T@E�T@E�-@E�h@E`B@D��@D�j@D��@D(�@C�
@CS�@Co@C@B�H@B=q@A��@A&�@@�9@@r�@@A�@?�;@?;d@?�@>��@>ȴ@>5?@>$�@=�T@=?}@<��@<�/@<�j@<��@<Z@<(�@;��@;�
@;�@:�H@:�\@:^5@9�7@8bN@8  @7\)@6��@5�@5�@5`B@5V@4��@4�D@4�@3S�@2�@17L@0�@01'@0  @0  @/�;@/��@/�P@/\)@/K�@/+@.�y@.�@.�@.�@.ȴ@.�R@.��@.�+@.@-��@-�-@-��@-��@-�@-/@,�j@,z�@,I�@,(�@,�@,1@+��@+��@+�m@+�
@+ƨ@+��@+��@+�@+S�@+o@*�H@*~�@*M�@*J@)�^@)hs@)X@)G�@)&�@(��@(�9@(��@(�u@(�u@(r�@(b@'�w@'|�@'K�@'+@'�@&�@&��@%�@%`B@%O�@%?}@%?}@%?}@%/@$��@$��@$I�@#�m@#�@#"�@"��@"n�@"-@!��@!hs@!&�@ Ĝ@ �u@ A�@�;@l�@�@��@�R@v�@ff@ff@E�@{@@��@p�@V@�/@��@�@j@(�@�m@ƨ@�F@�F@��@33@�H@��@�\@M�@J@�@�#@�#@��@7L@��@�9@�9@��@bN@ �@�@  @b@b@�@��@|�@\)@;d@�y@�y@�R@�+@$�@��@O�@��@Z@(�@��@C�@C�@C�@C�@C�@C�@C�@S�@S�@C�@"�@@�@�H@�!@^5@-@�@x�@G�@&�@%@�@&�@��@Q�@  @�w@��@�P@|�@|�@l�@;d@�@�@v�@ff@ff@5?@{@��@��@�@�@p�@`B@`B@O�@/@V@��@�/@�@Z@�
@ƨ@��@�@dZ@S�@C�@"�@"�@o@@
�@
�H@
��@
��@
��@
~�@
^5@
�@	��@	�#@	��@	��@	X@	&�@	&�@	%@��@�`@�`@�`@�`@�`@�`@��@�9@�u@bN@A�@b@�w@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�p�A�n�A�jA�jA�hsA�jA�l�A�l�A�p�A�t�A�v�A�r�A�v�A�t�A�t�A�XA�VA�
=A�=qA�ZA�ffA�p�A�z�AׁA׉7Aם�A׶FA��HA���A�JA��A��/A�/Aֺ^AՅA��yA��#A�-A�ƨA��#A�+A�Q�A�&�A�{A�x�A���A�XA�{A�G�A��A��A�I�A���A�\)A��A�{A��hA�z�A���A�5?A�I�A��FA�|�A�ȴA��A�{A��A�^5A�~�A���A�?}A�O�A�oA�A��A~�9A|�A{|�A{;dA{oAz  Av�\ArI�Ap�DAnbAg�Afn�Af$�Ae��Ac�;A^�jAXjAVA�AT9XAL��AG�wAEoADZACƨAB�9AA�A=�-A9\)A7�PA6VA4ȴA3S�A2�/A2M�A1�A0r�A01A.�A,��A*n�A&�!A$��A#�TA#oA"�RA"�!A"�\A"I�A!�FA!A�hA��A��AK�A��A�AA�A�#A"�A��A�An�A�uAbNA��A�A�A  A��A�A&�A-A�hA�uA�PA�!AbNA{Ap�A
�DA
�uA
M�A	\)A	%A�A�/A  A�!A�TA33A%A�HA��A~�A1A��A�A�
A�A�A��AXA\)A �A��AO�A��A�DA~�A�RA�`A�jAbA�-A\)A�`A��A�9An�A�TAG�A �DA M�A (�A {A   @��;@���@�@�V@��m@���@��H@�=q@���@�hs@�/@���@�1@�;d@��-@��`@�A�@�@�O�@��@���@��D@�A�@��
@�P@�K�@�+@�
=@�ȴ@@�v�@��T@�x�@�V@�w@ꗍ@�$�@��@�A�@��@��@�j@�(�@���@�{@�7L@�1@�l�@���@�E�@��#@�&�@�A�@�1@ۥ�@�"�@ڇ+@�-@ٲ-@�`B@��@�r�@�t�@���@��@���@�r�@��@�l�@���@���@с@���@�z�@�1@Ϯ@�|�@�+@��H@���@Χ�@�@�hs@�&�@�j@�K�@�ȴ@ʏ\@�n�@�$�@�hs@ȋD@�t�@Ƨ�@�E�@ŉ7@ēu@�  @��
@öF@Õ�@�l�@�\)@�dZ@�\)@���@�-@���@�hs@���@��@�j@�b@�"�@���@�^5@��#@��7@�X@��@��@�Ĝ@���@���@�C�@�"�@�
=@��y@��!@�E�@��-@�&�@��u@�9X@��@��@��y@�J@�hs@���@�A�@� �@��@�;d@�o@��y@��!@��\@�~�@�E�@�5?@��@��@��7@�X@�&�@���@��/@�Ĝ@���@�r�@�1@�t�@��@�ȴ@�V@�hs@��@��/@���@���@�z�@��;@��@��@�V@�r�@�t�@��@�v�@�{@��@���@�O�@���@�Ĝ@���@���@��@��@���@�M�@�J@��@��^@�&�@���@�bN@��m@�S�@���@���@���@���@���@���@���@�~�@�^5@�E�@�$�@�@���@���@��j@��D@�j@�I�@�A�@�(�@�b@�ƨ@��P@�dZ@��@�^5@�J@�@��#@��7@�hs@�X@�?}@���@��9@���@�bN@�1@��
@��P@�;d@�+@��@���@��R@�-@�J@��@��#@���@���@�X@�&�@��9@�j@�(�@���@���@��R@��@���@��h@�/@��@��@�r�@��@��@�S�@�+@��@�n�@�{@���@���@��u@�Q�@�(�@�1@�t�@��@��@��R@�n�@�=q@�5?@�$�@��T@�x�@�z�@��w@�S�@�;d@��@��y@�ȴ@��!@�~�@�E�@�$�@��-@�Ĝ@�z�@� �@���@���@��@�\)@�n�@��@��#@���@���@��h@��@�`B@�?}@��@���@�I�@��@
=@~v�@}��@}��@}�@}`B@}V@|�j@|(�@{�@{"�@z��@z-@y��@yhs@xĜ@xA�@x �@wl�@v5?@u@u��@up�@uO�@t��@t�/@tj@st�@r�@r~�@rM�@q�@q��@qG�@q�@p�`@p��@p�@p1'@p1'@o�;@o�P@o;d@n��@n��@m`B@l�j@lj@lI�@l�@k�m@k�
@k��@k"�@j�H@j��@j��@j�!@j��@jn�@j^5@j-@i�@iG�@i7L@hĜ@h1'@g��@g;d@f��@fȴ@f�R@f�R@f��@fv�@f5?@f{@e�T@e��@e?}@d�D@d1@c�F@c��@c��@c��@ct�@co@b�H@b~�@b=q@bM�@b=q@a�^@`Ĝ@`�u@`�@`Q�@_�w@^��@^v�@^ff@^V@^5?@^{@]�@]O�@\��@\(�@[t�@Z��@ZM�@Y�^@YX@Y7L@Y%@X��@XĜ@X�9@X�9@X  @W\)@V�y@Vȴ@V��@V5?@U�@Up�@U/@UV@T��@T��@T�j@T(�@T�@Sƨ@SdZ@RM�@Q�#@Q��@Qx�@Q�@P��@PQ�@O�@O\)@O�@N��@N{@M��@M`B@M/@Lj@K�F@KdZ@Ko@J�@J�!@J~�@Jn�@JM�@JJ@I��@IX@H��@HĜ@H�u@Hr�@HA�@H �@G�@G��@F�@F�R@F�+@E�T@E��@E�T@E�T@E�T@E�T@E�-@E�h@E`B@D��@D�j@D��@D(�@C�
@CS�@Co@C@B�H@B=q@A��@A&�@@�9@@r�@@A�@?�;@?;d@?�@>��@>ȴ@>5?@>$�@=�T@=?}@<��@<�/@<�j@<��@<Z@<(�@;��@;�
@;�@:�H@:�\@:^5@9�7@8bN@8  @7\)@6��@5�@5�@5`B@5V@4��@4�D@4�@3S�@2�@17L@0�@01'@0  @0  @/�;@/��@/�P@/\)@/K�@/+@.�y@.�@.�@.�@.ȴ@.�R@.��@.�+@.@-��@-�-@-��@-��@-�@-/@,�j@,z�@,I�@,(�@,�@,1@+��@+��@+�m@+�
@+ƨ@+��@+��@+�@+S�@+o@*�H@*~�@*M�@*J@)�^@)hs@)X@)G�@)&�@(��@(�9@(��@(�u@(�u@(r�@(b@'�w@'|�@'K�@'+@'�@&�@&��@%�@%`B@%O�@%?}@%?}@%?}@%/@$��@$��@$I�@#�m@#�@#"�@"��@"n�@"-@!��@!hs@!&�@ Ĝ@ �u@ A�@�;@l�@�@��@�R@v�@ff@ff@E�@{@@��@p�@V@�/@��@�@j@(�@�m@ƨ@�F@�F@��@33@�H@��@�\@M�@J@�@�#@�#@��@7L@��@�9@�9@��@bN@ �@�@  @b@b@�@��@|�@\)@;d@�y@�y@�R@�+@$�@��@O�@��@Z@(�@��@C�@C�@C�@C�@C�@C�@C�@S�@S�@C�@"�@@�@�H@�!@^5@-@�@x�@G�@&�@%@�@&�@��@Q�@  @�w@��@�P@|�@|�@l�@;d@�@�@v�@ff@ff@5?@{@��@��@�@�@p�@`B@`B@O�@/@V@��@�/@�@Z@�
@ƨ@��@�@dZ@S�@C�@"�@"�@o@@
�@
�H@
��@
��@
��@
~�@
^5@
�@	��@	�#@	��@	��@	X@	&�@	&�@	%@��@�`@�`@�`@�`@�`@�`@��@�9@�u@bN@A�@b@�w@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�`B
  B
VB
�B
�B
#�B
&�B
,B
5?B
A�B
Q�B
YB
`BB
aHB
[#B
W
B
L�B
-B
s�B
�{Br�B�BI�BffB]/BcTB]/Bw�B{�Bx�Bn�B_;BL�BD�B>wBB�B?}B'�B#�B+B�B��B�sB��B��B�=B�+Bk�B/B
�yB
ȴB
��B
T�B
6FB
/B
�B
hB
PB
B	��B	��B	�B	�B	�9B	��B	��B	�VB	aHB	y�B	}�B	t�B	cTB	A�B	"�B	�B	oB�B�B�fB�B�yB�;B��B�LB��B�^B�wB�^B�dBB��B�jB�wB�qB�?B��B��B��B�jB��B�TB��B	�B	�B	"�B	(�B	.B	1'B	N�B	s�B	�DB	��B	��B	�B	�jB	��B	�B	�NB	�B	��B
B
B	��B	�B	�`B	��B	��B	�qB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�jB	�B	�mB	�B	�mB	��B	ǮB	ŢB	��B	�fB	��B
�B
PB
PB
VB
bB
oB
{B
�B
{B
hB
VB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
VB
PB
hB
\B
VB
JB
JB
DB
1B
B
B	��B	��B
  B	��B	��B
%B
%B
%B
%B
B
%B
+B
+B
+B
%B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
  B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
%B
B
B
B
B
B
B
B
B
B
%B
%B
%B
B
B
B
%B
1B
1B
1B
+B
%B
%B
+B
+B
	7B
DB
	7B
1B
+B

=B
DB
JB
VB
PB
PB
bB
hB
hB
oB
uB
uB
{B
{B
uB
uB
{B
{B
�B
�B
�B
�B
{B
uB
oB
{B
{B
{B
oB
�B
�B
�B
�B
�B
{B
{B
uB
�B
�B
�B
�B
�B
�B
!�B
!�B
 �B
!�B
"�B
"�B
�B
!�B
$�B
$�B
$�B
$�B
%�B
$�B
"�B
"�B
#�B
"�B
!�B
$�B
&�B
(�B
(�B
(�B
(�B
'�B
'�B
'�B
'�B
%�B
$�B
#�B
(�B
+B
)�B
,B
-B
.B
-B
-B
,B
+B
,B
+B
)�B
,B
/B
.B
-B
/B
/B
/B
.B
.B
0!B
/B
0!B
1'B
1'B
1'B
33B
33B
2-B
1'B
1'B
49B
49B
49B
49B
33B
33B
2-B
2-B
33B
33B
49B
2-B
0!B
33B
6FB
7LB
6FB
8RB
7LB
5?B
5?B
9XB
9XB
9XB
9XB
7LB
8RB
8RB
8RB
:^B
<jB
<jB
;dB
9XB
;dB
=qB
=qB
=qB
>wB
>wB
>wB
;dB
:^B
8RB
9XB
;dB
>wB
>wB
=qB
?}B
?}B
?}B
>wB
>wB
=qB
<jB
@�B
@�B
@�B
@�B
@�B
?}B
=qB
@�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
A�B
B�B
C�B
E�B
F�B
G�B
G�B
G�B
F�B
F�B
F�B
G�B
I�B
I�B
H�B
H�B
H�B
H�B
I�B
J�B
H�B
H�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
R�B
R�B
R�B
R�B
S�B
S�B
Q�B
S�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
W
B
W
B
VB
XB
XB
XB
XB
YB
ZB
ZB
[#B
[#B
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
[#B
\)B
]/B
]/B
]/B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
\)B
[#B
]/B
]/B
\)B
[#B
[#B
]/B
_;B
_;B
^5B
^5B
]/B
^5B
]/B
]/B
^5B
^5B
_;B
`BB
aHB
bNB
bNB
bNB
bNB
bNB
aHB
`BB
aHB
bNB
cTB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
cTB
bNB
dZB
cTB
cTB
bNB
dZB
e`B
e`B
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
ffB
gmB
iyB
iyB
iyB
iyB
jB
jB
jB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
jB
jB
iyB
k�B
k�B
jB
l�B
m�B
m�B
l�B
l�B
k�B
l�B
k�B
k�B
l�B
l�B
k�B
k�B
l�B
l�B
m�B
l�B
k�B
l�B
l�B
m�B
n�B
n�B
n�B
m�B
o�B
o�B
n�B
n�B
o�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
o�B
n�B
q�B
q�B
q�B
q�B
r�B
t�B
s�B
t�B
s�B
s�B
r�B
q�B
s�B
t�B
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
y�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
x�B
y�B
y�B
y�B
x�B
x�B
y�B
z�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�B
�%B
�%B
�B
�%B
�+B
�+B
�+B
�+B
�1B
�7B
�7B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�=B
�=B
�=B
�7B
�7B
�=B
�DB
�DB
�=B
�=B
�DB
�JB
�PB
�PB
�PB
�VB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�PB
�VB
�PB
�PB
�VB
�\B
�VB
�\B
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�bB
�bB
�bB
�bB
�bB
�bB
�\B
�\B
�\B
�VB
�\B
�bB
�bB
�bB
�hB
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
�hB
�oB
�oB
�oB
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
�uB
�uB
�oB
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
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�`B
  B
VB
�B
�B
#�B
&�B
,B
5?B
A�B
Q�B
YB
`BB
aHB
[#B
W
B
L�B
-B
s�B
�{Br�B�BI�BffB]/BcTB]/Bw�B{�Bx�Bn�B_;BL�BD�B>wBB�B?}B'�B#�B+B�B��B�sB��B��B�=B�+Bk�B/B
�yB
ȴB
��B
T�B
6FB
/B
�B
hB
PB
B	��B	��B	�B	�B	�9B	��B	��B	�VB	aHB	y�B	}�B	t�B	cTB	A�B	"�B	�B	oB�B�B�fB�B�yB�;B��B�LB��B�^B�wB�^B�dBB��B�jB�wB�qB�?B��B��B��B�jB��B�TB��B	�B	�B	"�B	(�B	.B	1'B	N�B	s�B	�DB	��B	��B	�B	�jB	��B	�B	�NB	�B	��B
B
B	��B	�B	�`B	��B	��B	�qB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�jB	�B	�mB	�B	�mB	��B	ǮB	ŢB	��B	�fB	��B
�B
PB
PB
VB
bB
oB
{B
�B
{B
hB
VB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
VB
PB
hB
\B
VB
JB
JB
DB
1B
B
B	��B	��B
  B	��B	��B
%B
%B
%B
%B
B
%B
+B
+B
+B
%B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
  B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
%B
B
B
B
B
B
B
B
B
B
%B
%B
%B
B
B
B
%B
1B
1B
1B
+B
%B
%B
+B
+B
	7B
DB
	7B
1B
+B

=B
DB
JB
VB
PB
PB
bB
hB
hB
oB
uB
uB
{B
{B
uB
uB
{B
{B
�B
�B
�B
�B
{B
uB
oB
{B
{B
{B
oB
�B
�B
�B
�B
�B
{B
{B
uB
�B
�B
�B
�B
�B
�B
!�B
!�B
 �B
!�B
"�B
"�B
�B
!�B
$�B
$�B
$�B
$�B
%�B
$�B
"�B
"�B
#�B
"�B
!�B
$�B
&�B
(�B
(�B
(�B
(�B
'�B
'�B
'�B
'�B
%�B
$�B
#�B
(�B
+B
)�B
,B
-B
.B
-B
-B
,B
+B
,B
+B
)�B
,B
/B
.B
-B
/B
/B
/B
.B
.B
0!B
/B
0!B
1'B
1'B
1'B
33B
33B
2-B
1'B
1'B
49B
49B
49B
49B
33B
33B
2-B
2-B
33B
33B
49B
2-B
0!B
33B
6FB
7LB
6FB
8RB
7LB
5?B
5?B
9XB
9XB
9XB
9XB
7LB
8RB
8RB
8RB
:^B
<jB
<jB
;dB
9XB
;dB
=qB
=qB
=qB
>wB
>wB
>wB
;dB
:^B
8RB
9XB
;dB
>wB
>wB
=qB
?}B
?}B
?}B
>wB
>wB
=qB
<jB
@�B
@�B
@�B
@�B
@�B
?}B
=qB
@�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
A�B
B�B
C�B
E�B
F�B
G�B
G�B
G�B
F�B
F�B
F�B
G�B
I�B
I�B
H�B
H�B
H�B
H�B
I�B
J�B
H�B
H�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
R�B
R�B
R�B
R�B
S�B
S�B
Q�B
S�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
W
B
W
B
VB
XB
XB
XB
XB
YB
ZB
ZB
[#B
[#B
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
[#B
\)B
]/B
]/B
]/B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
\)B
[#B
]/B
]/B
\)B
[#B
[#B
]/B
_;B
_;B
^5B
^5B
]/B
^5B
]/B
]/B
^5B
^5B
_;B
`BB
aHB
bNB
bNB
bNB
bNB
bNB
aHB
`BB
aHB
bNB
cTB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
cTB
bNB
dZB
cTB
cTB
bNB
dZB
e`B
e`B
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
ffB
gmB
iyB
iyB
iyB
iyB
jB
jB
jB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
jB
jB
iyB
k�B
k�B
jB
l�B
m�B
m�B
l�B
l�B
k�B
l�B
k�B
k�B
l�B
l�B
k�B
k�B
l�B
l�B
m�B
l�B
k�B
l�B
l�B
m�B
n�B
n�B
n�B
m�B
o�B
o�B
n�B
n�B
o�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
o�B
n�B
q�B
q�B
q�B
q�B
r�B
t�B
s�B
t�B
s�B
s�B
r�B
q�B
s�B
t�B
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
y�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
x�B
y�B
y�B
y�B
x�B
x�B
y�B
z�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�B
�%B
�%B
�B
�%B
�+B
�+B
�+B
�+B
�1B
�7B
�7B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�=B
�=B
�=B
�7B
�7B
�=B
�DB
�DB
�=B
�=B
�DB
�JB
�PB
�PB
�PB
�VB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�PB
�VB
�PB
�PB
�VB
�\B
�VB
�\B
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�bB
�bB
�bB
�bB
�bB
�bB
�\B
�\B
�\B
�VB
�\B
�bB
�bB
�bB
�hB
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
�hB
�oB
�oB
�oB
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
�uB
�uB
�oB
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
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221202100144                              AO  ARCAADJP                                                                    20221202100144    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221202100144  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221202100144  QCF$                G�O�G�O�G�O�4000            