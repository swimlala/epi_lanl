CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:18:49Z creation;2022-06-04T19:18:49Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191849  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               /A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @����	{1   @����@/��t��cxZ�11   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B���B���C�fC�fC  C  C
L�C�3C�fC�fC� C��C�fC�fC  C  C�C �C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4�C6  C7�fC:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl�Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB(G�B/�HB8G�B?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B�#�B��B��B��B��B��B��B��B��B�W
B��>B��qB��qC޸C޸C�RC�RC
EC��C޸C޸CxRC�C޸C޸C�RC�RC�C �C!�RC#޸C%�RC'�RC)�RC+�RC-�RC/�RC1�RC4�C5�RC7޸C9�RC;�RC=޸C?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCb�Cc�RCe�RCg�RCi�RCl�Cm�RCo޸Cq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
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
D�=D�B=D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D�
D��
D��
D�?
D�
D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��cA��fA�oA��A��A�	7A��A��A��A��A�A��(A���A��+A��DA���A��.A��A�GA�A��A��A��+A�ٴA;BA͛=A͚�A͙�A͗�A͗YA͒:A͑hA͏\A͎�A͏(A͎�A͎�A͎�A͎�A͍A͌�A͌�A͌A͋DA͊�A͊	AͅSÀ A�u%A��rA˕�A�PAŏ\A�ܒA��A��A���A���A���A��A���A�wfA��A��GA��-A��DA�(A��(A��IA���A��IA�7LA�dZA���A��zA��A�,�A�qA�DA�{JA�I�A��^A�ʌA��&A�`�A�>BA��A�[�A���A~��Av�OAsl�An@AjiDAg��A`��A]�AZ8AV;AR�AO�nAL��AIg�AGp�AE�AB�A?�,A=n/A;��A:'RA8XyA6ɆA5��A4��A2��A1�fA1͟A0�A0sA/|�A. \A+�_A)�A'(�A%&�A#��A#
=A"�VA!]dA�/A�.AS&A!�A��AA�A�pAp;A>BAϫAoA�Al�A�AںA��A<�A��A�A�}A��A�wA.IA��A��A��A
=AH�A��A�=A��A�XA�7AN<A�A_pA�Az�A�!A͟AYKA��A��A�Aw�A�MA�HA��A	A�$A�A4A
�6A
�AA
4�A
CA
($A	�A	�kA	[�A	R�A�+A�qA�Ab�A/�ACA�As�AF�A�Aw�AJ�A�A�LAcAݘAY�AQA~A��A_A=qAA��AخA�ARTA�A ��A ߤA ��A e�A 6zA �@��C@���@�G�@�kQ@�  @��P@��@�� @�3�@���@��=@�E9@�u�@�U�@��,@�-�@��@��V@�W?@���@�Ov@��@�9�@��p@�@�G@��)@��3@�~�@�A�@�ی@�oi@��@��@���@�R�@��@�e,@��@�@��@��>@�Dg@��@�:�@�0�@�PH@�e@�-w@��D@�@�B�@��@�]d@�V@�1�@�}V@�x@ߢ�@��v@�s�@��@ݝ�@�4�@��@ܛ�@��@�@ڡb@�a|@٬q@�>�@؃�@�$�@׳�@�g�@��@֚@��o@�s�@�\�@�J#@�<6@��5@Ԅ�@��@ӷ@�G�@�#:@ѣn@�E9@��"@��@�֡@Ї�@��@ϗ$@�Ɇ@�`�@�7�@���@�t�@�A�@���@�oi@�D�@�4@��@��N@�e,@��'@ʙ1@�PH@��@ɸ�@�<6@���@ȳh@�l"@�U2@�<�@Ƿ@�k�@�E9@��@ƿ�@�-@Ŵ�@Ń{@��@ĔF@�:*@�1'@�z�@�C�@��>@å@�(�@�@�j@��@��@��"@�͟@��@�'R@��d@�o @��@���@�q@��h@��@��v@��@���@�O�@��@���@�K^@�%�@���@��
@���@�/�@���@��@�oi@�?�@�'R@�e@���@��q@�{J@�K�@�(@��}@�S�@�	@���@��'@�\�@���@��@�[�@�M�@�خ@�a@�!�@���@���@�1�@���@�O@��@��}@�p;@�C-@��@��o@�@�e�@��8@��@�YK@��K@���@��@�l"@��@��	@���@�v�@�7@���@�:�@�ں@��@�q�@�`�@�?�@���@���@�y�@��|@��R@�ff@���@���@���@�x@�#�@��)@�S�@�{@��d@�b�@�&@��$@���@�q@�V�@�)�@���@�zx@�,�@��L@�\�@�A�@���@�G�@��@���@�@�X�@���@��<@�j@�;�@�{@���@��$@�;d@��@�ff@��D@��^@���@���@�{�@�c @��@�o @�F@�33@�8�@�*0@���@�Ɇ@���@�l"@�U2@�Ft@���@��M@�{J@�o @��@��@���@�~(@�*�@�b@���@��@���@���@�H�@��c@��@�8�@���@�Q�@�Ĝ@���@�~�@�N�@��@���@�zx@�H�@�C@��H@���@�E�@��@�˒@��f@��@��r@�n�@�:*@��+@���@�|@�F�@�$t@��@�ں@��@�c�@�,=@���@���@�^�@�$t@��@���@��Y@�@�@��@��@��@���@�P�@�(@���@��@���@��b@�1'@��6@��@�S&@�'�@�ߤ@���@���@�+k@��@���@��9@�˒@��w@��n@��	@���@�Vm@�#�@���@��@��@���@��Y@�ff@�Ta@�?�@�(�@���@���@�|�@�G�@�q@��K@���@���@�]d@�>B@�
�@�@��@�@Mj@~��@~=q@}��@}�N@}�h@|�@|�u@|r�@|@{�@{�@zz@z0U@y�@y��@y+@x�O@xI�@x�@w�}@wA�@v��@vJ�@vJ@u��@u��@u�@t��@t��@tZ@t�@s�0@s��@s.I@r��@r��@r	@qDg@pr�@o�F@oC@n��@n�s@n��@m�@m!�@l�e@l��@lq@lj@l,=@k��@k+@j��@j�@j
�@iS&@h�)@h/�@g�@gt�@g$t@g i@fں@f�x@fH�@e�H@e�@d7�@c��@c
=@bn�@b
�@a�H@aIR@a�@`�@`2�@_�@_v`@_>�@^�@^�r@^@�@]�@]*0@\S�@[�@[��@[�@Zں@Z�@Y��@Y`B@X��@X�/@XɆ@X�4@X9X@W�@W,�@V�M@V��@VE�@U�)@U�h@U?}@U#�@T�|@T��@TU2@T%�@S��@S��@S�	@SS�@R��@Rz@ROv@RR�@Rh
@R4@Q��@Q<6@Q�@P��@P|�@O�@OdZ@O�@N��@M�@M�@MDg@M%@L��@LtT@LG@K��@J�@J��@J��@Jl�@J@I�@I��@I�N@I�^@I�=@H�	@H�@G�$@F��@Fq�@FL0@F($@E�.@E�@Eu�@EQ�@E�@E;@D��@DD�@C�Q@Cx@C
=@B�x@B_�@B6�@A��@A��@A2a@A(�@A!�@@�@@N�@?�;@?.I@?(@>��@>)�@=��@=�T@=�@=�@=�@=ԕ@=��@=w2@==�@<��@<��@<2�@;�A@;�K@;Mj@:ں@:�@:1�@:	@9��@9F@9�@9%@8�[@8��@8g8@7��@7F�@6��@6�}@6��@6GE@5�>@5��@5^�@5q@4�@4(�@3�*@3]�@38@2�@2�L@2kQ@20U@1��@1��@1k�@12a@0�5@0��@0:�@0	�@/��@/��@/�@.�H@.��@.C�@.�@-��@-��@-�j@-��@-��@-j@-�@,u�@,1'@,�@+�@+��@+�@+��@+t�@+\)@+.I@*��@*�@*�R@*YK@)ԕ@)w2@)`B@)F@)%F@(�5@(Q�@(�@'��@'��@'|�@'S@&�x@&n�@&C�@&.�@&e@&
�@%�Z@%��@%5�@$�f@$�)@$�9@$��@$�I@$��@$u�@$Xy@$M@#�&@#�@#��@#e�@#"�@"�2@"��@"a|@";�@"�@"�@!��@!��@!��@!�7@!L�@!�@ �_@ V�@ (�@��@�k@��@�4@A�@o@��@.�@�@ �@��@�#@�@��@�M@S&@&�@�)@H@�
@��@�$@�@�!@c @R�@6�@@��@ \@�$@�O@��@��@�z@j@� @�0@�[@�	@j�@K�@�8@YK@4@��@k�@@�	@�@�/@�p@�j@��@�e@]d@�q@�@�@~�@M�@0U@@ԕ@��@p�@�@��@��@��@�@�@��@��@��@�@@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��cA��fA�oA��A��A�	7A��A��A��A��A�A��(A���A��+A��DA���A��.A��A�GA�A��A��A��+A�ٴA;BA͛=A͚�A͙�A͗�A͗YA͒:A͑hA͏\A͎�A͏(A͎�A͎�A͎�A͎�A͍A͌�A͌�A͌A͋DA͊�A͊	AͅSÀ A�u%A��rA˕�A�PAŏ\A�ܒA��A��A���A���A���A��A���A�wfA��A��GA��-A��DA�(A��(A��IA���A��IA�7LA�dZA���A��zA��A�,�A�qA�DA�{JA�I�A��^A�ʌA��&A�`�A�>BA��A�[�A���A~��Av�OAsl�An@AjiDAg��A`��A]�AZ8AV;AR�AO�nAL��AIg�AGp�AE�AB�A?�,A=n/A;��A:'RA8XyA6ɆA5��A4��A2��A1�fA1͟A0�A0sA/|�A. \A+�_A)�A'(�A%&�A#��A#
=A"�VA!]dA�/A�.AS&A!�A��AA�A�pAp;A>BAϫAoA�Al�A�AںA��A<�A��A�A�}A��A�wA.IA��A��A��A
=AH�A��A�=A��A�XA�7AN<A�A_pA�Az�A�!A͟AYKA��A��A�Aw�A�MA�HA��A	A�$A�A4A
�6A
�AA
4�A
CA
($A	�A	�kA	[�A	R�A�+A�qA�Ab�A/�ACA�As�AF�A�Aw�AJ�A�A�LAcAݘAY�AQA~A��A_A=qAA��AخA�ARTA�A ��A ߤA ��A e�A 6zA �@��C@���@�G�@�kQ@�  @��P@��@�� @�3�@���@��=@�E9@�u�@�U�@��,@�-�@��@��V@�W?@���@�Ov@��@�9�@��p@�@�G@��)@��3@�~�@�A�@�ی@�oi@��@��@���@�R�@��@�e,@��@�@��@��>@�Dg@��@�:�@�0�@�PH@�e@�-w@��D@�@�B�@��@�]d@�V@�1�@�}V@�x@ߢ�@��v@�s�@��@ݝ�@�4�@��@ܛ�@��@�@ڡb@�a|@٬q@�>�@؃�@�$�@׳�@�g�@��@֚@��o@�s�@�\�@�J#@�<6@��5@Ԅ�@��@ӷ@�G�@�#:@ѣn@�E9@��"@��@�֡@Ї�@��@ϗ$@�Ɇ@�`�@�7�@���@�t�@�A�@���@�oi@�D�@�4@��@��N@�e,@��'@ʙ1@�PH@��@ɸ�@�<6@���@ȳh@�l"@�U2@�<�@Ƿ@�k�@�E9@��@ƿ�@�-@Ŵ�@Ń{@��@ĔF@�:*@�1'@�z�@�C�@��>@å@�(�@�@�j@��@��@��"@�͟@��@�'R@��d@�o @��@���@�q@��h@��@��v@��@���@�O�@��@���@�K^@�%�@���@��
@���@�/�@���@��@�oi@�?�@�'R@�e@���@��q@�{J@�K�@�(@��}@�S�@�	@���@��'@�\�@���@��@�[�@�M�@�خ@�a@�!�@���@���@�1�@���@�O@��@��}@�p;@�C-@��@��o@�@�e�@��8@��@�YK@��K@���@��@�l"@��@��	@���@�v�@�7@���@�:�@�ں@��@�q�@�`�@�?�@���@���@�y�@��|@��R@�ff@���@���@���@�x@�#�@��)@�S�@�{@��d@�b�@�&@��$@���@�q@�V�@�)�@���@�zx@�,�@��L@�\�@�A�@���@�G�@��@���@�@�X�@���@��<@�j@�;�@�{@���@��$@�;d@��@�ff@��D@��^@���@���@�{�@�c @��@�o @�F@�33@�8�@�*0@���@�Ɇ@���@�l"@�U2@�Ft@���@��M@�{J@�o @��@��@���@�~(@�*�@�b@���@��@���@���@�H�@��c@��@�8�@���@�Q�@�Ĝ@���@�~�@�N�@��@���@�zx@�H�@�C@��H@���@�E�@��@�˒@��f@��@��r@�n�@�:*@��+@���@�|@�F�@�$t@��@�ں@��@�c�@�,=@���@���@�^�@�$t@��@���@��Y@�@�@��@��@��@���@�P�@�(@���@��@���@��b@�1'@��6@��@�S&@�'�@�ߤ@���@���@�+k@��@���@��9@�˒@��w@��n@��	@���@�Vm@�#�@���@��@��@���@��Y@�ff@�Ta@�?�@�(�@���@���@�|�@�G�@�q@��K@���@���@�]d@�>B@�
�@�@��@�@Mj@~��@~=q@}��@}�N@}�h@|�@|�u@|r�@|@{�@{�@zz@z0U@y�@y��@y+@x�O@xI�@x�@w�}@wA�@v��@vJ�@vJ@u��@u��@u�@t��@t��@tZ@t�@s�0@s��@s.I@r��@r��@r	@qDg@pr�@o�F@oC@n��@n�s@n��@m�@m!�@l�e@l��@lq@lj@l,=@k��@k+@j��@j�@j
�@iS&@h�)@h/�@g�@gt�@g$t@g i@fں@f�x@fH�@e�H@e�@d7�@c��@c
=@bn�@b
�@a�H@aIR@a�@`�@`2�@_�@_v`@_>�@^�@^�r@^@�@]�@]*0@\S�@[�@[��@[�@Zں@Z�@Y��@Y`B@X��@X�/@XɆ@X�4@X9X@W�@W,�@V�M@V��@VE�@U�)@U�h@U?}@U#�@T�|@T��@TU2@T%�@S��@S��@S�	@SS�@R��@Rz@ROv@RR�@Rh
@R4@Q��@Q<6@Q�@P��@P|�@O�@OdZ@O�@N��@M�@M�@MDg@M%@L��@LtT@LG@K��@J�@J��@J��@Jl�@J@I�@I��@I�N@I�^@I�=@H�	@H�@G�$@F��@Fq�@FL0@F($@E�.@E�@Eu�@EQ�@E�@E;@D��@DD�@C�Q@Cx@C
=@B�x@B_�@B6�@A��@A��@A2a@A(�@A!�@@�@@N�@?�;@?.I@?(@>��@>)�@=��@=�T@=�@=�@=�@=ԕ@=��@=w2@==�@<��@<��@<2�@;�A@;�K@;Mj@:ں@:�@:1�@:	@9��@9F@9�@9%@8�[@8��@8g8@7��@7F�@6��@6�}@6��@6GE@5�>@5��@5^�@5q@4�@4(�@3�*@3]�@38@2�@2�L@2kQ@20U@1��@1��@1k�@12a@0�5@0��@0:�@0	�@/��@/��@/�@.�H@.��@.C�@.�@-��@-��@-�j@-��@-��@-j@-�@,u�@,1'@,�@+�@+��@+�@+��@+t�@+\)@+.I@*��@*�@*�R@*YK@)ԕ@)w2@)`B@)F@)%F@(�5@(Q�@(�@'��@'��@'|�@'S@&�x@&n�@&C�@&.�@&e@&
�@%�Z@%��@%5�@$�f@$�)@$�9@$��@$�I@$��@$u�@$Xy@$M@#�&@#�@#��@#e�@#"�@"�2@"��@"a|@";�@"�@"�@!��@!��@!��@!�7@!L�@!�@ �_@ V�@ (�@��@�k@��@�4@A�@o@��@.�@�@ �@��@�#@�@��@�M@S&@&�@�)@H@�
@��@�$@�@�!@c @R�@6�@@��@ \@�$@�O@��@��@�z@j@� @�0@�[@�	@j�@K�@�8@YK@4@��@k�@@�	@�@�/@�p@�j@��@�e@]d@�q@�@�@~�@M�@0U@@ԕ@��@p�@�@��@��@��@�@�@��@��@��@�@@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�xB�DB�xB��B��B��B�*B�DB��B��B��B�	B��B��B��B��B�XB�	B��B��B��B�lB��B�B�FB�FB�FB�`B�zB�zB��B��B��B��B��B��B��B��B�zB��B�zB��B�zB��B��B�zB��B��B�BɠB�B	8�B	XEB	��B	��B	�B	�#B	��B	��B
 B
�B
a|B
|B
��B
��B
��BB�Bp�Bq'Bt�ByXB!B
�#B
��B�B�B�B
��B
�KB
��B
w2B
88B
�B	�B	�B	�_B	��B	�+B	��B	|�B	w�B	kB	_!B	Q�B	3�B	;B	�B	+B��B�B	 4B��B�B�@B��B�B�B��B��B��B��B�MB��B�VB��B�HB��BÖB�9B�%B�9B��B� B�[B��BżB��B�B�2B�2B�[B�NB��B�B�eB�B�fB	�B	&�B	2�B	B[B	IRB	QB	X�B	X�B	Z7B	i*B	x8B	��B	��B	�&B	�SB	�xB	�'B	� B	�>B	�yB	��B	�WB	�B	�vB	�JB	�B	��B	� B	�oB	�^B	�HB	�HB	ѷB	��B	ؓB	өB	ªB	�(B	�B	�	B	˒B	��B	�}B	ңB	�CB	�B	�-B	�2B	�B	�B	��B	�B	�QB	�B	��B	�>B	�B	�
B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�LB	��B	�kB	��B	�B	�AB	��B	��B	��B	��B	�XB	��B	��B	�jB	��B	�B	��B	��B	�^B	�XB	�*B	��B	�XB	�rB	��B	�^B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�dB	��B	��B	�B	��B	�.B	�"B	�B	��B	��B	�qB	��B	��B	��B	��B	�qB	��B	�]B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�PB	��B	��B	��B	��B	�xB	��B	��B	��B	�RB	��B	��B	�XB	�^B	��B	�0B	��B	��B	��B	�0B	��B	�xB	�*B	�B	�xB	�B	�B	��B	��B	�(B	�]B	�wB	��B
 iB
 iB
 4B
B
 �B
�B
 �B
 �B
 �B
 B
 4B
  B	��B	�cB	�}B	�cB	�HB
  B	��B	��B
  B
  B
 �B
�B
�B
AB
�B
�B
MB
3B
MB
MB
�B
GB
�B
uB
[B
AB
�B
gB
�B
�B
'B
�B
�B
;B
B
 4B
�B
;B
 �B
oB
�B
B
�B
YB
EB
EB
B
9B
�B
uB
�B
B
�B
�B
[B
�B
{B
B
B
�B
;B
B
B
 B
 iB
 �B
 �B
 �B
 �B
 �B
 iB
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
AB
B
[B
[B
�B
�B
�B
B
B
aB
{B
aB
�B
�B
{B
{B
aB
MB
gB
B
B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
EB
�B
�B
�B
�B
�B
�B
�B
	B
	�B

	B

�B

�B

�B

�B
�B
JB
�B
DB
�B
�B
�B
jB
pB
�B
�B
�B
BB
\B
\B
�B
�B
�B
TB
�B
�B
&B
[B
@B
�B
aB
{B
�B
�B
�B
hB
B
�B
4B
B
�B
�B
:B
�B
hB
B
�B
4B
�B
�B
B
 B
�B
:B
�B
B
B
�B
�B
�B
B
TB
�B
�B
�B
�B
&B
�B
�B
oB
�B
�B
�B
�B
B
B
�B
�B
MB
2B
�B
gB
gB
SB
$B
�B
_B
+B
_B
�B
�B
B
B
�B
�B
7B
�B
�B
WB
�B
�B
�B
�B
�B
�B
�B
;B
�B
�B
 'B
�B
 �B
!-B
!bB
!bB
"B
"B
"4B
"�B
"�B
#B
#:B
#nB
#�B
#�B
#�B
$tB
$�B
%B
$�B
&�B
'�B
($B
)*B
)�B
)�B
*KB
*eB
*�B
*�B
+6B
+�B
+�B
+�B
,"B
,"B
,"B
,qB
,�B
,�B
,�B
-)B
-wB
-]B
-wB
-�B
-�B
./B
./B
./B
.B
.cB
.}B
/ B
/OB
/�B
/�B
/�B
/�B
/�B
/�B
0B
0UB
0!B
0!B
0;B
0�B
1'B
1[B
1[B
1[B
2GB
2aB
2aB
2�B
2�B
3hB
3�B
4B
49B
4�B
4�B
5B
5?B
5?B
5ZB
5�B
6B
6`B
6zB
6�B
6�B
7�B
7fB
7�B
7�B
8B
88B
8RB
8�B
8�B
8RB
8�B
9XB
:B
:^B
:�B
:�B
:�B
:�B
:�B
;dB
;�B
;�B
;�B
;�B
;�B
<PB
<�B
=<B
=B
="B
=�B
>B
>�B
>�B
?B
?cB
?.B
?cB
?cB
?}B
?�B
@B
?�B
?�B
?cB
?cB
?�B
?�B
@B
@�B
AB
@�B
@�B
@OB
@OB
@�B
A B
AUB
A;B
AUB
AoB
A;B
A�B
BuB
BuB
B'B
B�B
B�B
C-B
C-B
CGB
CaB
C�B
C�B
C�B
DgB
D�B
E9B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
F%B
F�B
F�B
GEB
F�B
F�B
G+B
G_B
G�B
G�B
G�B
G�B
HB
H1B
H�B
I�B
J=B
J�B
KDB
K�B
K�B
K�B
L0B
LdB
LdB
L�B
MB
M�B
N�B
N�B
N�B
N�B
N�B
OB
N�B
OB
N�B
O�B
OBB
O�B
O�B
P.B
PHB
PbB
PbB
P�B
P�B
P�B
QB
Q4B
Q�B
R:B
R�B
S&B
S�B
T,B
T,B
T,B
TFB
TFB
T{B
TaB
TaB
T�B
T�B
U2B
U�B
U�B
VSB
V�B
W
B
W
B
W
B
V�B
W
B
W$B
WsB
W�B
W�B
XB
XyB
X�B
YB
YB
Y�B
Y�B
ZB
ZkB
Z7B
Z�B
[	B
[	B
[=B
[�B
[�B
[�B
\�B
\]B
\xB
\�B
\�B
\�B
]/B
]/B
]dB
]~B
]�B
^OB
^�B
_!B
_B
_pB
_�B
_�B
_�B
_�B
`'B
`�B
`�B
`�B
aB
abB
a|B
abB
a�B
a�B
a|B
a�B
bhB
bhB
b�B
b�B
b�B
b�B
b�B
cB
c:B
c�B
dtB
d�B
d�B
d�B
e,B
eFB
e,B
eFB
ezB
e�B
e�B
e�B
f2B
f�B
f�B
gB
gB
f�B
g8B
g�B
h
B
hXB
h>B
h$B
h�B
i*B
i*B
i_B
i_B
iyB
iyB
i_B
i�B
j0B
jKB
jeB
jB
jB
jB
j�B
jB
j�B
j�B
kB
kB
kB
kkB
k�B
k�B
lB
l"B
l=B
lWB
lqB
l�B
l�B
l�B
l�B
mB
m)B
m�B
m�B
m�B
ncB
n}B
ncB
n}B
n�B
n�B
o5B
o�B
o�B
o�B
oiB
o�B
o�B
o�B
o�B
p!B
pB
poB
p�B
q[B
qAB
q[B
q�B
r-B
r|B
r|B
r|B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t9B
tTB
t9B
tnB
tnB
tTB
t�B
uB
u?B
utB
u�B
u�B
v+B
v+B
v+B
v+B
vFB
v+B
vB
vzB
wLB
w�B
w�B
x8B
xRB
x�B
x�B
x�B
x�B
y>B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
{B
{0B
{dB
{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�xB�DB�xB��B��B��B�*B�DB��B��B��B�	B��B��B��B��B�XB�	B��B��B��B�lB��B�B�FB�FB�FB�`B�zB�zB��B��B��B��B��B��B��B��B�zB��B�zB��B�zB��B��B�zB��B��B�BɠB�B	8�B	XEB	��B	��B	�B	�#B	��B	��B
 B
�B
a|B
|B
��B
��B
��BB�Bp�Bq'Bt�ByXB!B
�#B
��B�B�B�B
��B
�KB
��B
w2B
88B
�B	�B	�B	�_B	��B	�+B	��B	|�B	w�B	kB	_!B	Q�B	3�B	;B	�B	+B��B�B	 4B��B�B�@B��B�B�B��B��B��B��B�MB��B�VB��B�HB��BÖB�9B�%B�9B��B� B�[B��BżB��B�B�2B�2B�[B�NB��B�B�eB�B�fB	�B	&�B	2�B	B[B	IRB	QB	X�B	X�B	Z7B	i*B	x8B	��B	��B	�&B	�SB	�xB	�'B	� B	�>B	�yB	��B	�WB	�B	�vB	�JB	�B	��B	� B	�oB	�^B	�HB	�HB	ѷB	��B	ؓB	өB	ªB	�(B	�B	�	B	˒B	��B	�}B	ңB	�CB	�B	�-B	�2B	�B	�B	��B	�B	�QB	�B	��B	�>B	�B	�
B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�LB	��B	�kB	��B	�B	�AB	��B	��B	��B	��B	�XB	��B	��B	�jB	��B	�B	��B	��B	�^B	�XB	�*B	��B	�XB	�rB	��B	�^B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�dB	��B	��B	�B	��B	�.B	�"B	�B	��B	��B	�qB	��B	��B	��B	��B	�qB	��B	�]B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�PB	��B	��B	��B	��B	�xB	��B	��B	��B	�RB	��B	��B	�XB	�^B	��B	�0B	��B	��B	��B	�0B	��B	�xB	�*B	�B	�xB	�B	�B	��B	��B	�(B	�]B	�wB	��B
 iB
 iB
 4B
B
 �B
�B
 �B
 �B
 �B
 B
 4B
  B	��B	�cB	�}B	�cB	�HB
  B	��B	��B
  B
  B
 �B
�B
�B
AB
�B
�B
MB
3B
MB
MB
�B
GB
�B
uB
[B
AB
�B
gB
�B
�B
'B
�B
�B
;B
B
 4B
�B
;B
 �B
oB
�B
B
�B
YB
EB
EB
B
9B
�B
uB
�B
B
�B
�B
[B
�B
{B
B
B
�B
;B
B
B
 B
 iB
 �B
 �B
 �B
 �B
 �B
 iB
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
AB
B
[B
[B
�B
�B
�B
B
B
aB
{B
aB
�B
�B
{B
{B
aB
MB
gB
B
B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
EB
�B
�B
�B
�B
�B
�B
�B
	B
	�B

	B

�B

�B

�B

�B
�B
JB
�B
DB
�B
�B
�B
jB
pB
�B
�B
�B
BB
\B
\B
�B
�B
�B
TB
�B
�B
&B
[B
@B
�B
aB
{B
�B
�B
�B
hB
B
�B
4B
B
�B
�B
:B
�B
hB
B
�B
4B
�B
�B
B
 B
�B
:B
�B
B
B
�B
�B
�B
B
TB
�B
�B
�B
�B
&B
�B
�B
oB
�B
�B
�B
�B
B
B
�B
�B
MB
2B
�B
gB
gB
SB
$B
�B
_B
+B
_B
�B
�B
B
B
�B
�B
7B
�B
�B
WB
�B
�B
�B
�B
�B
�B
�B
;B
�B
�B
 'B
�B
 �B
!-B
!bB
!bB
"B
"B
"4B
"�B
"�B
#B
#:B
#nB
#�B
#�B
#�B
$tB
$�B
%B
$�B
&�B
'�B
($B
)*B
)�B
)�B
*KB
*eB
*�B
*�B
+6B
+�B
+�B
+�B
,"B
,"B
,"B
,qB
,�B
,�B
,�B
-)B
-wB
-]B
-wB
-�B
-�B
./B
./B
./B
.B
.cB
.}B
/ B
/OB
/�B
/�B
/�B
/�B
/�B
/�B
0B
0UB
0!B
0!B
0;B
0�B
1'B
1[B
1[B
1[B
2GB
2aB
2aB
2�B
2�B
3hB
3�B
4B
49B
4�B
4�B
5B
5?B
5?B
5ZB
5�B
6B
6`B
6zB
6�B
6�B
7�B
7fB
7�B
7�B
8B
88B
8RB
8�B
8�B
8RB
8�B
9XB
:B
:^B
:�B
:�B
:�B
:�B
:�B
;dB
;�B
;�B
;�B
;�B
;�B
<PB
<�B
=<B
=B
="B
=�B
>B
>�B
>�B
?B
?cB
?.B
?cB
?cB
?}B
?�B
@B
?�B
?�B
?cB
?cB
?�B
?�B
@B
@�B
AB
@�B
@�B
@OB
@OB
@�B
A B
AUB
A;B
AUB
AoB
A;B
A�B
BuB
BuB
B'B
B�B
B�B
C-B
C-B
CGB
CaB
C�B
C�B
C�B
DgB
D�B
E9B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
F%B
F�B
F�B
GEB
F�B
F�B
G+B
G_B
G�B
G�B
G�B
G�B
HB
H1B
H�B
I�B
J=B
J�B
KDB
K�B
K�B
K�B
L0B
LdB
LdB
L�B
MB
M�B
N�B
N�B
N�B
N�B
N�B
OB
N�B
OB
N�B
O�B
OBB
O�B
O�B
P.B
PHB
PbB
PbB
P�B
P�B
P�B
QB
Q4B
Q�B
R:B
R�B
S&B
S�B
T,B
T,B
T,B
TFB
TFB
T{B
TaB
TaB
T�B
T�B
U2B
U�B
U�B
VSB
V�B
W
B
W
B
W
B
V�B
W
B
W$B
WsB
W�B
W�B
XB
XyB
X�B
YB
YB
Y�B
Y�B
ZB
ZkB
Z7B
Z�B
[	B
[	B
[=B
[�B
[�B
[�B
\�B
\]B
\xB
\�B
\�B
\�B
]/B
]/B
]dB
]~B
]�B
^OB
^�B
_!B
_B
_pB
_�B
_�B
_�B
_�B
`'B
`�B
`�B
`�B
aB
abB
a|B
abB
a�B
a�B
a|B
a�B
bhB
bhB
b�B
b�B
b�B
b�B
b�B
cB
c:B
c�B
dtB
d�B
d�B
d�B
e,B
eFB
e,B
eFB
ezB
e�B
e�B
e�B
f2B
f�B
f�B
gB
gB
f�B
g8B
g�B
h
B
hXB
h>B
h$B
h�B
i*B
i*B
i_B
i_B
iyB
iyB
i_B
i�B
j0B
jKB
jeB
jB
jB
jB
j�B
jB
j�B
j�B
kB
kB
kB
kkB
k�B
k�B
lB
l"B
l=B
lWB
lqB
l�B
l�B
l�B
l�B
mB
m)B
m�B
m�B
m�B
ncB
n}B
ncB
n}B
n�B
n�B
o5B
o�B
o�B
o�B
oiB
o�B
o�B
o�B
o�B
p!B
pB
poB
p�B
q[B
qAB
q[B
q�B
r-B
r|B
r|B
r|B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t9B
tTB
t9B
tnB
tnB
tTB
t�B
uB
u?B
utB
u�B
u�B
v+B
v+B
v+B
v+B
vFB
v+B
vB
vzB
wLB
w�B
w�B
x8B
xRB
x�B
x�B
x�B
x�B
y>B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
{B
{0B
{dB
{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105236  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191849  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191849  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191849                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041857  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041857  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                