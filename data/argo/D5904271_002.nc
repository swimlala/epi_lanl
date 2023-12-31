CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  V   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2014-07-22T01:11:08Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
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
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      
_FillValue               conventions       Argo reference table 23          7�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    7�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        >�E�vQ�        7�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    7�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        7�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9    PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9$   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9(   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9,   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        X  90   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  F�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  I�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  W8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  Z�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  g�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  u@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  x�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ň   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    Ȉ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ˈ   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue               conventions       YYYYMMDDHHMISS        ,  Έ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �t   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     δ   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     ��         ��Argo profile    3.1 1.2 19500101000000  20140722011108  20181023142215  5904271 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  4744_0188_002                   2C  D   NAVIS_A                         863 @֖X�t� 1   @֖Y`�  @+�����c�p��
=1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�#�B��B��B��B��B��B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�C�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC6�C7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ��D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~DjzDj~Dj�Dk~Dk�Dl~DmzDm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A�
=A�%A���A��HAҲ-A�x�A�v�A�t�A�jA�ffA�ffA�hsA�hsA�bNA�\)A�XA�VA�XA�VA�S�A�O�A�O�A�O�A�G�A�C�A�A�A�A�A�A�A�?}A�9XA�(�A�  A��TA���Aʗ�A�n�A�AA��A���A�%A���A�K�A��A�;dA�\)A�VA���A��9A�ƨA�dZA��A�~�A�n�A�5?A�VA�A��^A�hsA�hsA�
=A�`BA���A��RA��A���A�VA��^A��A��A��\A�33A���A�hsA��wA�JA��FA��+A��A��#A�G�A��uA�G�A��
A��-A�?}AzM�Av�As��Ak`BAd^5Ab5?AaA^n�AY��AV�AT$�ASAP�DAL-AJ��AF��ADȴACdZA?hsA<�A:�uA89XA7O�A7A5��A3�TA29XA1\)A133A1+A0�A.bNA+��A*=qA*�yA*��A)�FA)VA(��A'��A&�HA& �A$��A$ �A#ƨA#33A#33A#�A#VA$ffA$�9A$ffA"{A"I�A#"�A#�hA#\)A!�A $�A��AXAv�AA%An�A�A�A|�A��A�;A�mA�A�^A��An�A+A��A�uA�A�A
=A�jA-A�A�A�AhsA�`A�\AbNAQ�AI�AAA��Al�A?}A
=A�HA�A��A�RA�A=qA�A�-AC�A�A��A
�jA
E�A
bA	�TA	��A	`BA	�A�/A��A�uAA�A�AK�A33A�AM�AA�At�A�9A�uA��AjAA�TA�TAƨA��A\)A+A�A�A�\A(�AS�A�A �!A ffA b@�\)@��@��!@���@�X@�?}@��@�V@�Ĝ@�A�@���@�@���@�^5@�x�@�/@��`@���@��H@�^5@�@��@��j@�9X@�@��@��@�E�@�@��T@�@��@�bN@�(�@��m@�\)@��@�~�@�5?@��^@�Ĝ@�1'@�@���@�M�@��@��@�bN@��
@�F@�l�@��@�\@�-@�G�@��@��@�C�@�!@��@�Ĝ@�r�@߮@�E�@�X@�A�@��;@��@ڏ\@�-@�?}@ش9@�z�@�j@�I�@�  @�+@��@�n�@��T@թ�@Չ7@��@Դ9@��
@Ӆ@�l�@�@ҏ\@�V@�5?@�-@��@љ�@���@�r�@ϕ�@ΰ!@�@͉7@�hs@�%@�bN@���@�@��@���@�r�@�A�@ǅ@�ff@ź^@ř�@�?}@ģ�@� �@�K�@�V@���@�`B@���@��u@�Z@��m@�C�@��@���@�^5@���@�p�@�&�@�Ĝ@�I�@��;@�dZ@��@�~�@��T@�/@��@���@��;@�;d@��@�^5@���@�7L@���@��;@�|�@�"�@���@���@���@�G�@���@�bN@�9X@��m@�l�@�o@��!@�=q@���@��@�j@���@��F@�S�@�o@�o@��y@�ȴ@�~�@�V@�=q@��@�J@��@���@�&�@�9X@���@��@�|�@�t�@�dZ@�33@���@��!@��\@�ff@��@���@�p�@��`@�Q�@�ƨ@���@�t�@�C�@�
=@�ȴ@��!@��+@�E�@��@�7L@���@���@��@�9X@�(�@�1@��@��
@��@���@���@�|�@�\)@�33@�o@���@���@���@�O�@�Ĝ@��@�Z@�(�@��;@��@�\)@�K�@��@�n�@�{@��@���@�`B@��@��@��j@�Z@�b@��@��P@�K�@�"�@�o@���@�$�@��^@�?}@���@�A�@��m@�l�@�o@��R@���@���@��@��@��j@�r�@�I�@��;@���@�|�@�+@�ȴ@���@�v�@�E�@���@��h@�&�@��u@� �@��m@���@�t�@�33@��y@���@�E�@��#@��h@�X@�%@�Ĝ@���@�Q�@�1@��m@�ƨ@���@�dZ@�C�@��@��y@���@�^5@�$�@���@���@���@�O�@��j@�j@�A�@� �@�b@�ƨ@��F@��@�+@��H@��\@�ff@�{@��T@��^@��7@�x�@��@��@�Q�@�(�@\)@+@
=@}�@}@}O�@}/@|�@{��@{��@{33@z��@z�@y�7@x�`@x  @w�P@v��@v�+@vff@v5?@v{@u��@u`B@u?}@t�@t�j@t�D@tI�@s��@s��@sC�@r�H@r�\@r-@q��@q�^@q%@pĜ@p�@pbN@pA�@pb@o��@o
=@nff@m@m�h@m?}@l�j@l9X@k��@k�
@kƨ@k�F@k�@k33@j�\@j�@i��@h��@h��@h��@hĜ@h�@h1'@g��@g�@f5?@e��@e�-@e�-@e�h@e�@d��@d(�@cdZ@c"�@b��@b~�@a��@a7L@`A�@_��@_;d@^��@^��@^v�@^ff@^{@]��@]�@]O�@]�@]V@\�@\z�@[�F@Z�H@Z��@Z�\@Z=q@Y��@Y�^@YG�@Y�@Xr�@W�@W�;@W�@W|�@WK�@V�@Vv�@V@U@Up�@UV@T�@Tz�@S��@S��@S"�@R�@R�\@R~�@RM�@R-@Q��@Qx�@Q&�@P��@P�@O�@OK�@O�@O
=@N�y@N5?@M�@Lj@K�F@Kt�@J�!@J=q@J�@I��@I��@I��@IG�@H��@HQ�@H1'@H  @G�P@GK�@G�@F�@F��@Fff@FE�@F{@E�h@EV@D�/@DZ@C��@C��@Ct�@CS�@B�H@B��@B�!@BM�@B�@A�@Ax�@@�`@@1'@?�;@?�@?|�@?\)@>�y@>ff@>@=@=�h@=�@=p�@=/@<�@<�j@<�@;ƨ@;��@;S�@:��@:-@9�#@9hs@8��@8bN@8Q�@8 �@7�@7��@7|�@7|�@7\)@7+@6�R@65?@6@5�T@5��@5@5�-@5��@5`B@4�@4z�@3ƨ@333@2�\@2^5@2�@1�7@1X@1%@0��@0�9@0r�@0b@/��@/��@/�P@/K�@.��@.ff@-�@-��@-p�@-V@,�D@,9X@,�@+��@+dZ@+33@*��@*J@)�@)��@)��@)G�@)%@(��@(��@(  @'l�@'+@'
=@&�y@&ȴ@&�R@&v�@&5?@&{@%@%V@$��@#ƨ@#S�@"��@"��@"�\@"�\@"�\@"��@"M�@"�@!��@!X@!%@ Ĝ@  �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�A�
=A�%A���A��HAҲ-A�x�A�v�A�t�A�jA�ffA�ffA�hsA�hsA�bNA�\)A�XA�VA�XA�VA�S�A�O�A�O�A�O�A�G�A�C�A�A�A�A�A�A�A�?}A�9XA�(�A�  A��TA���Aʗ�A�n�A�AA��A���A�%A���A�K�A��A�;dA�\)A�VA���A��9A�ƨA�dZA��A�~�A�n�A�5?A�VA�A��^A�hsA�hsA�
=A�`BA���A��RA��A���A�VA��^A��A��A��\A�33A���A�hsA��wA�JA��FA��+A��A��#A�G�A��uA�G�A��
A��-A�?}AzM�Av�As��Ak`BAd^5Ab5?AaA^n�AY��AV�AT$�ASAP�DAL-AJ��AF��ADȴACdZA?hsA<�A:�uA89XA7O�A7A5��A3�TA29XA1\)A133A1+A0�A.bNA+��A*=qA*�yA*��A)�FA)VA(��A'��A&�HA& �A$��A$ �A#ƨA#33A#33A#�A#VA$ffA$�9A$ffA"{A"I�A#"�A#�hA#\)A!�A $�A��AXAv�AA%An�A�A�A|�A��A�;A�mA�A�^A��An�A+A��A�uA�A�A
=A�jA-A�A�A�AhsA�`A�\AbNAQ�AI�AAA��Al�A?}A
=A�HA�A��A�RA�A=qA�A�-AC�A�A��A
�jA
E�A
bA	�TA	��A	`BA	�A�/A��A�uAA�A�AK�A33A�AM�AA�At�A�9A�uA��AjAA�TA�TAƨA��A\)A+A�A�A�\A(�AS�A�A �!A ffA b@�\)@��@��!@���@�X@�?}@��@�V@�Ĝ@�A�@���@�@���@�^5@�x�@�/@��`@���@��H@�^5@�@��@��j@�9X@�@��@��@�E�@�@��T@�@��@�bN@�(�@��m@�\)@��@�~�@�5?@��^@�Ĝ@�1'@�@���@�M�@��@��@�bN@��
@�F@�l�@��@�\@�-@�G�@��@��@�C�@�!@��@�Ĝ@�r�@߮@�E�@�X@�A�@��;@��@ڏ\@�-@�?}@ش9@�z�@�j@�I�@�  @�+@��@�n�@��T@թ�@Չ7@��@Դ9@��
@Ӆ@�l�@�@ҏ\@�V@�5?@�-@��@љ�@���@�r�@ϕ�@ΰ!@�@͉7@�hs@�%@�bN@���@�@��@���@�r�@�A�@ǅ@�ff@ź^@ř�@�?}@ģ�@� �@�K�@�V@���@�`B@���@��u@�Z@��m@�C�@��@���@�^5@���@�p�@�&�@�Ĝ@�I�@��;@�dZ@��@�~�@��T@�/@��@���@��;@�;d@��@�^5@���@�7L@���@��;@�|�@�"�@���@���@���@�G�@���@�bN@�9X@��m@�l�@�o@��!@�=q@���@��@�j@���@��F@�S�@�o@�o@��y@�ȴ@�~�@�V@�=q@��@�J@��@���@�&�@�9X@���@��@�|�@�t�@�dZ@�33@���@��!@��\@�ff@��@���@�p�@��`@�Q�@�ƨ@���@�t�@�C�@�
=@�ȴ@��!@��+@�E�@��@�7L@���@���@��@�9X@�(�@�1@��@��
@��@���@���@�|�@�\)@�33@�o@���@���@���@�O�@�Ĝ@��@�Z@�(�@��;@��@�\)@�K�@��@�n�@�{@��@���@�`B@��@��@��j@�Z@�b@��@��P@�K�@�"�@�o@���@�$�@��^@�?}@���@�A�@��m@�l�@�o@��R@���@���@��@��@��j@�r�@�I�@��;@���@�|�@�+@�ȴ@���@�v�@�E�@���@��h@�&�@��u@� �@��m@���@�t�@�33@��y@���@�E�@��#@��h@�X@�%@�Ĝ@���@�Q�@�1@��m@�ƨ@���@�dZ@�C�@��@��y@���@�^5@�$�@���@���@���@�O�@��j@�j@�A�@� �@�b@�ƨ@��F@��@�+@��H@��\@�ff@�{@��T@��^@��7@�x�@��@��@�Q�@�(�@\)@+@
=@}�@}@}O�@}/@|�@{��@{��@{33@z��@z�@y�7@x�`@x  @w�P@v��@v�+@vff@v5?@v{@u��@u`B@u?}@t�@t�j@t�D@tI�@s��@s��@sC�@r�H@r�\@r-@q��@q�^@q%@pĜ@p�@pbN@pA�@pb@o��@o
=@nff@m@m�h@m?}@l�j@l9X@k��@k�
@kƨ@k�F@k�@k33@j�\@j�@i��@h��@h��@h��@hĜ@h�@h1'@g��@g�@f5?@e��@e�-@e�-@e�h@e�@d��@d(�@cdZ@c"�@b��@b~�@a��@a7L@`A�@_��@_;d@^��@^��@^v�@^ff@^{@]��@]�@]O�@]�@]V@\�@\z�@[�F@Z�H@Z��@Z�\@Z=q@Y��@Y�^@YG�@Y�@Xr�@W�@W�;@W�@W|�@WK�@V�@Vv�@V@U@Up�@UV@T�@Tz�@S��@S��@S"�@R�@R�\@R~�@RM�@R-@Q��@Qx�@Q&�@P��@P�@O�@OK�@O�@O
=@N�y@N5?@M�@Lj@K�F@Kt�@J�!@J=q@J�@I��@I��@I��@IG�@H��@HQ�@H1'@H  @G�P@GK�@G�@F�@F��@Fff@FE�@F{@E�h@EV@D�/@DZ@C��@C��@Ct�@CS�@B�H@B��@B�!@BM�@B�@A�@Ax�@@�`@@1'@?�;@?�@?|�@?\)@>�y@>ff@>@=@=�h@=�@=p�@=/@<�@<�j@<�@;ƨ@;��@;S�@:��@:-@9�#@9hs@8��@8bN@8Q�@8 �@7�@7��@7|�@7|�@7\)@7+@6�R@65?@6@5�T@5��@5@5�-@5��@5`B@4�@4z�@3ƨ@333@2�\@2^5@2�@1�7@1X@1%@0��@0�9@0r�@0b@/��@/��@/�P@/K�@.��@.ff@-�@-��@-p�@-V@,�D@,9X@,�@+��@+dZ@+33@*��@*J@)�@)��@)��@)G�@)%@(��@(��@(  @'l�@'+@'
=@&�y@&ȴ@&�R@&v�@&5?@&{@%@%V@$��@#ƨ@#S�@"��@"��@"�\@"�\@"�\@"��@"M�@"�@!��@!X@!%@ Ĝ@  �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�;B
�;B
�;B
�5B
�;B
�;B
�BB
�BB
�NB
�TB
�ZB
�`B
�fB
�mB
�sB
�yB
�yB
�yB
�yB
�yB
�yB
�B
�B
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B��B'�B:^BC�BiyB�+B��B��B�B�9B�qBÖB�jB�RB�XB�9B�FB�9B�9B�B��B��B��B��B�uB�\B�JB�\B�PB�DB�hB�\B~�Bo�B^5BI�B1'B�B  B�TB��B��B~�BH�B$�B
��B
�B
�qB
�uB
ffB
(�B
B	��B	�B	��B	iyB	I�B	E�B	E�B	>wB	#�B	oB	�B	�B	'�B	%�B	�B	  B�B�ZB�B��B��BɺBǮBĜBBĜB��B�mB�B�B�B�/B�B��B�B��B	%B	�B	0!B	6FB	7LB	5?B	33B	;dB	D�B	D�B	T�B	q�B	o�B	��B	��B	��B	��B	�B	��B	�B	�yB	�BB	�
B	��B	��B	��B	�B	�B	�5B	�;B	�;B	�5B	�/B	�fB	��B
B
1B
1B
1B
B	��B	�B	�B	��B	��B	��B	��B
%B
+B
1B
DB
PB
uB
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
$�B
$�B
&�B
(�B
)�B
+B
-B
)�B
"�B
�B
 �B
+B
,B
-B
-B
.B
.B
/B
0!B
0!B
1'B
1'B
0!B
.B
-B
-B
(�B
'�B
,B
0!B
/B
.B
/B
/B
0!B
0!B
0!B
0!B
/B
/B
.B
,B
)�B
+B
)�B
(�B
(�B
(�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
'�B
&�B
&�B
%�B
$�B
$�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
uB
uB
oB
oB
oB
oB
hB
hB
hB
hB
hB
oB
oB
bB
VB
PB
PB
PB
DB

=B

=B

=B
	7B
	7B
	7B
DB
DB
DB
DB

=B
DB
DB

=B
DB
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
DB
DB

=B

=B

=B

=B

=B

=B
	7B
	7B
	7B
1B
1B
1B
1B
1B
1B
	7B
	7B
1B
1B
1B
1B
+B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B
	7B

=B

=B

=B
DB
DB
DB

=B
DB
DB
DB
DB
PB
JB
JB
JB
JB
JB
PB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
VB
VB
VB
W
B
VB
VB
W
B
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�?B
�?B
�(B
�DB
�fB
ߑB
��B
��B
�WB
�bB
�B
�uB
�fB
�gB
�tB
�B
�B
�B
�B
�wB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B�B��B-�B?�BD�BjaB�aB�)B�B�0B��B��BƧB��B�fB��B��B��B��B�hB��B�~B��B�?B�=B��B�"B��B��B�WB��B��B�B�@Bs�Bb�BN�B68B�B�B�B�?B�8B�BR�B2^B	�B
�/B
��B
��B
y�B
44B
�B	ҨB	�yB	��B	tjB	MVB	G�B	J�B	H�B	)�B	uB	sB	&B	3B	*B	!'B	�B��B�B��B�B��B�4BȎB�%B�0B��B�B��B��B�
B�*B�B�6B�NB�tB�,B	�B	�B	2DB	7�B	8�B	7�B	5!B	<2B	E�B	D�B	SfB	s�B	l�B	��B	��B	�)B	�'B	��B	��B	�B	��B	��B	�GB	��B	�/B	լB	��B	وB	��B	߹B	�8B	��B	ߒB	�SB	�YB
B

gB
	3B
HB
�B	�_B	�rB	�'B	�B	��B	�MB	��B
3B
FB
	�B
�B
6B
�B
�B
�B
MB
ZB
B
CB
 @B
 WB
"?B
"�B
#B
%B
%xB
'�B
)TB
*�B
+�B
.RB
,"B
$�B
�B
!5B
+dB
,�B
-�B
-�B
.�B
.WB
/�B
0�B
1�B
1�B
1hB
0�B
/�B
-@B
/
B
*�B
(DB
+�B
0�B
0+B
.iB
/#B
/pB
0�B
0�B
0�B
0MB
/�B
/�B
/6B
.UB
*�B
,.B
*�B
)�B
*B
)^B
(�B
)�B
(cB
'B
('B
(B
(nB
(�B
'�B
'�B
&NB
%�B
&6B
"EB
!KB
!rB
 'B
~B
AB
�B
�B
{B
�B
aB
WB
9B
B
�B
B
[B
�B
�B
B
hB
NB
B
B
IB
�B
^B
OB
�B
>B
B
�B
uB
?B
�B
�B
B
B
�B

B
[B
�B
�B
HB
�B
�B
�B
uB
9B
�B
�B

�B
SB
B
	�B

�B
	�B
�B
bB
xB
�B
yB
�B
�B
B
�B
{B
�B
�B
�B
�B
uB
�B
�B
�B
|B
YB
rB
%B
�B
�B
�B
�B
PB
B

{B

�B

LB

+B

�B
	�B
	�B
		B
�B
	|B
	�B

@B
	xB
�B
	.B
	B
	�B
�B
	�B

1B

B
	�B
	�B
	�B
BB

�B

�B

�B
	�B

B

�B

�B
�B
�B
B

�B
�B
B
B
�B
wB
qB
B
�B
B
<B
�B
B
.B
�B
�B
B
\B
�B
�B
/B
�B
�B
�B
B
�B
�B
B
TB
)B
fB
B
�B
�B
�B
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
FB
B
cB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
EB
�B
{B
�B
sB
�B
�B
�B
�B
�B
�B
�B
B
/B
�B
B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
YB
�B
�B
�B
 B
�B
B
3B
MB
�B
�B
`B
�B
TB
B
4B
 1B
 2B
 B
 B
 _B
 5B
!B
!hB
!3B
"B
!�B
"\B
"�B
"�B
"�B
"�B
#fB
$kB
$�B
$eB
%kB
%�B
&aB
'
B
'eB
'fB
'IB
(,B
(wB
(GB
)%B
)dB
)qB
) B
)8B
*=B
*bB
*�B
*�B
*�B
*�B
+VB
+_B
+GB
+_B
,rB
-rB
-�B
-�B
.kB
.UB
/yB
/hB
/LB
/sB
0zB
0LB
0JB
0MB
1yB
1SB
1^B
1kB
1�B
2�B
2{B
2dB
2hB
3zB
3�B
4B
4�B
4uB
4fB
5^B
4�B
5\B
5�B
5�B
5�B
6�B
6�B
6�B
7�B
7�B
7�B
7mB
7�B
7�B
82B
8�B
9B
9�B
9xB
:,B
:�B
:�B
:|B
:�B
:�B
;�B
;�B
;�B
;�B
<�B
<�B
=B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
AB
A�B
A�B
A�B
B�B
A�B
A�B
CB
CB
CB
B�B
C�B
DB
DB
D�B
D�B
D�B
D�B
D�B
D�B
EB
D�B
E�B
F B
E�B
E�B
E�B
E�B
E�B
E�B
FB
G.B
F�B
G�B
G�B
G�B
G�B
HB
G�B
H+B
H�B
H�B
H�B
IB
I;B
IRB
I#B
I�B
I�B
JB
I�B
J�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
LQB
LdB
L�B
L�B
MB
M B
NB
N$B
OB
NKB
O8B
N�B
OB
N�B
O B
O6B
O)B
O'B
PB
PB
P)B
Q2B
QB
QCB
Q2B
Q;B
QB
Q1B
Q�B
RB
RB
RWB
RB
R,B
RB
REB
R�B
S=B
SB
SB
SB
SxB
S�B
T�B
T}B
U7B
U�B
VZB
V"B
V B
WB
V2B
VgB
W�B
XNB
X(B
X7B
XgB
YKB
Y@B
YLB
YAB
YGB
Y5B
YDB
Y}B
Y{B
ZIB
Z�B
ZqB
ZaB
[LB
[DB
[B
[3B
\JB
\wB
\TB
\XB
\�B
\�B
\�B
]sB
]ZB
]\B
]RB
]�B
^�B
^�B
_jB
_]B
_IB
_FB
_kB
_gB
__B
_�B
_qB
`^B
`}B
`�B
`�B
_vB
_�B
_�B
_�B
_IB
_]B
`�B
`UB
a`B
bQB
bgB
bvB
b�B
c�B
c}B
cnB
cdB
ccB
ceB
ceB
c�B
d�B
d�B
d�B
d�B
c�B
c}B
d�B
e�B
e�B
e�B
e�B
e}B
e�B
e�B
e�B
e�B
euB
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
jB
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
nB
m�B
o:B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
qB
p�B
q�B
q�B
sB
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<�J<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��<2�<g9<^r8<#�
<F؞<U��<�Cs<E��<�HN<#�
<#�
<R�<AH<#�
<#�
<#�
<7`�<#�
<#�
<#�
<#�
<C&�<#�
<<�^<#�
<#�
<87�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.03 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810180917412018101809174120181018091741  0188                            052512                          AO  ARCAADJP                                                                    20140722011108    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140722011108  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140722011108  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20181018091741  QC  PRES            @�33D�� G�O�                PM  ARSQCTM V1.1                                                                20181018091741  QC  PSAL            @�33D�� G�O�                PM  ARSQOWGUV1.0                                                                20181023142215  IP                  G�O�G�O�G�O�                