CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-03-14T21:41:37Z creation;2023-03-14T21:41:38Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230314214137  20230314215635  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�Sp1   @���]L;@0�\(��d�/��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B���B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv33Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�3D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @G�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B�#�B�W
B��qB��qB��qB��B��B��B�#�B��B��B��B��B��B��B׽qB۽qB��B��B��B��B��B��B��B��B��C�RC�C�C�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC@�CB�CC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCv+�Cw�RCy޸C{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DLzDL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D�=D�B=D�
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
D�/
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��8A��A���A�ܒAˀ4A�� A��A���AʧAʈ�A�s�A�g�A�\�A�]/A�S[A�@�A�Z�Aɷ�A�xlA�`�A�a�A�g8A�gA�aHA�^A�\�A�^jA�P�A�6zA��A���A��A��KA�ܒAȿAȃ�A�v�A�r�A�m�A�j�Aȋ�AȾ�A�%zA�$A��]A�6�A�E�Aþ�A��BA�%zA�qvA��HA���A�g8A�VA�GEA�t�A���A�A�A�?HA��A�hA�dZA���A��vA�CA��1A�_�A��2A��A���A���A�ߤA�)�A���A�<jA�M6A��A���A�{�A�ʌA��	A�:*A�
�A��A�ZA���A��A�e�A�sMA�$tA�{�A�aA�ffA��A�~A���A�d�A�49A���A��VA���A�6�A��A}�)A{$tAvAtS&Ar�yAm�AlG�AjO�Af�Ab�A_�A\�jAX��AU��AT��AS�APRTAL�HAKFtAJ�AE��AC��AA�^AA,=A@�fA=e�A;A:�A9qvA7�A5�;A3&A1�A.�!A)�fA(|A'!�A%:�A$_A"VmA 3�A�oA+A�AqAN�AMA��A�sA�*A��AkQA��A��A=�A��A/�A_A��A��A'�A�A��A�KAh�A,=A!�A�A�A�A�DA33A�A�sA�pA��A��A=�A�A�AA�}AI�A�!A/�A��A��A=A�A
�3A
_A
�A��A��A�dAƨA��A��A	lAqA!A�A��A��A�\A"�A��AI�A�A��A6�AhsA��AbNA �BA @�@�v�@���@��	@��j@�M@�5�@��@�33@��@���@��;@�Y�@�y>@��@��M@���@�E�@�@��@�!@�Ta@�,=@�ƨ@�E�@�P�@�$@�1�@�@�}@��@�Mj@��@�B[@�X@�	l@�tT@�q�@�%�@��s@��@�8�@���@�!�@��g@�M@�e�@�{J@��@��r@��@ް�@ތ@�*0@�^5@�$@���@��6@۝�@�&@ڒ�@�($@٩�@�hs@�ں@�V�@���@�C�@�Ɇ@֓u@���@��@�e�@�x�@��M@�m�@�	@��K@ь~@�A�@�@��@Ю}@�oi@ЋD@��B@ϓ�@Ή�@��K@�%F@̥z@��@˼�@ˉ7@��8@�z�@ɳ�@���@�1�@�xl@�}V@��@��@��@�w�@��@��9@Ų�@�.I@đ�@�m]@��@���@¯O@���@��>@�@��_@�H�@��>@��7@��@��L@�Q�@��N@�2a@��2@���@�0U@�خ@�>�@���@��b@��O@�C-@�+�@���@�+@��5@���@���@�~�@�G@��H@��@��P@�l�@�A�@�4�@�,�@��6@�/�@�(�@���@���@�ԕ@���@���@�n/@�9�@�Mj@�qv@��4@���@�J�@���@�~(@��Y@���@�͟@���@�Ta@��}@��M@��@�!�@��)@���@�o�@�@���@���@�z@�/�@��4@��@��@���@���@�?}@��	@��@��@�_�@�e@��D@��S@�zx@�F@��f@��@�y>@�S�@���@��n@�X@��'@�u%@�h
@�V�@��@�`B@��@�ȴ@��@�\�@�҉@��@�~�@�Ov@� �@�˒@�]�@���@��+@�v�@�j@�L0@�"h@��@�{J@�q@��R@�S�@�2�@�"h@�b@��.@��Z@��o@��@�F�@�ȴ@��@�{�@�j@���@��'@�s�@�Y@�@���@�_@��$@�iD@�Mj@�<6@�(�@�@���@�U2@�_@��@���@�_p@�&@��"@��<@�~�@�g8@�K^@�_@��W@���@�`B@�7L@� i@��	@���@��@���@���@�B[@�!�@���@��6@���@���@�iD@�Y�@�33@���@��y@��R@���@�@��+@��Q@���@�F�@��@���@�K^@���@�� @��@�*�@�qv@��@��I@��.@�n�@�C-@�x@��[@�:�@��@��@��v@���@�2a@���@��j@��h@�C�@��>@��K@���@�p�@�iD@�:�@�@���@�S�@�'R@��>@���@�!-@�	l@��@��[@��m@���@���@�e�@��Q@�b�@��@��	@��|@��@��&@�n/@�[W@�Mj@�0�@�֡@�)�@�@dZ@~�,@~�6@~�@~��@~)�@}�d@}(�@|�/@|u�@{�Q@{��@{v`@{\)@{dZ@{�*@{��@z�@z6�@zQ@z��@{!-@{]�@{+@z��@z�s@z��@y�@yN<@xQ�@wF�@v��@u�3@uq@tی@t�/@t�@tl"@t[�@tD�@t�@sخ@s&@r�m@r�A@r8�@r@qA @p��@p,=@ot�@oP�@n��@nC�@n\�@n�@nB[@m2a@loi@kخ@kF�@j��@j�@j�B@j�h@jq�@j��@j��@j�1@j�r@js�@jR�@j@�@j+k@i@h��@h�D@hoi@h`�@g˒@gP�@g�@f+k@e�@d��@d`�@cdZ@cC@b�@b��@b��@b�@a�t@a`B@a+@`�5@`�@`c�@_��@_@^�R@^��@^kQ@^4@\�f@\�@\"h@[��@[�@@[�@Z��@Zh
@Z�@Y�~@Y%F@Y4@X�@X�_@W�A@W|�@W,�@Vں@Vz@V�@U�=@U(�@T��@T�@S��@R�@Rq�@R.�@R�@Q��@QJ�@P�@Pz�@P  @O�@O��@O�@O
=@N�8@N��@N��@NH�@M��@L�|@L��@Lm�@K{J@J��@Jں@JTa@I�@IB�@Hl"@G��@F��@F�F@F0U@F�@E�Z@E��@Ea�@E=�@E�@D��@D,=@C��@C�$@Cj�@B�h@BkQ@BV@B=q@A�.@A�t@A�@@�@@��@@e�@?��@>?@>_@=��@=�@=�@=@=�n@=x�@=?}@<��@<��@<�@:ߤ@:z@:YK@:$�@9�@9��@9*0@8�5@8��@86@7�m@7��@7��@6��@6+k@6u@5�N@5�t@5�7@5c�@5L�@5F@5;@4�@4��@4C-@3��@2�@2��@2�F@2�r@2H�@2�@1�@1:�@0�4@0�@0|�@0m�@0e�@0U2@0�@/��@/t�@/33@/�@.�c@.�}@.c @-��@-��@-O�@-V@,��@,2�@+�a@+y�@+a@+a@+a@+U�@+H�@*�@*��@*��@*��@*�6@*l�@*5?@*
�@)��@)��@)=�@)V@(��@(��@(�e@(�.@(S�@'��@'��@'�@&�}@&p;@&?@%hs@%(�@%�@$��@$��@$7�@#�@#��@#��@#�{@#.I@#(@"��@"h
@"E�@"+k@"�@!�@!zx@!(�@ �5@ ��@ H@ @�[@8@��@c @!�@&�@	@��@��@�j@��@N<@#�@�$@��@w�@oi@bN@�@dZ@O@�@�m@�@^5@	@��@�N@j@2a@�@�[@��@:�@�@��@�	@|�@o�@W?@4�@
=@��@�@�m@z@^5@R�@?@��@��@m]@G�@#�@;@��@�|@�K@�@��@[�@:�@1@��@��@��@{J@=@��@�6@kQ@$�@J@�.@��@�@A @;@�?@�9@��@��@�_@V�@@��@{J@W?@33@ i@͟@v�@=q@�>@��@!�@�v@��@��@�e@��@~(@*�@�@�@��@�}@�F@�V@|�@v`@\)@,�@
�]@
��@
��@
xl@
J�@
�@	��@	��@	�S111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��8A��A���A�ܒAˀ4A�� A��A���AʧAʈ�A�s�A�g�A�\�A�]/A�S[A�@�A�Z�Aɷ�A�xlA�`�A�a�A�g8A�gA�aHA�^A�\�A�^jA�P�A�6zA��A���A��A��KA�ܒAȿAȃ�A�v�A�r�A�m�A�j�Aȋ�AȾ�A�%zA�$A��]A�6�A�E�Aþ�A��BA�%zA�qvA��HA���A�g8A�VA�GEA�t�A���A�A�A�?HA��A�hA�dZA���A��vA�CA��1A�_�A��2A��A���A���A�ߤA�)�A���A�<jA�M6A��A���A�{�A�ʌA��	A�:*A�
�A��A�ZA���A��A�e�A�sMA�$tA�{�A�aA�ffA��A�~A���A�d�A�49A���A��VA���A�6�A��A}�)A{$tAvAtS&Ar�yAm�AlG�AjO�Af�Ab�A_�A\�jAX��AU��AT��AS�APRTAL�HAKFtAJ�AE��AC��AA�^AA,=A@�fA=e�A;A:�A9qvA7�A5�;A3&A1�A.�!A)�fA(|A'!�A%:�A$_A"VmA 3�A�oA+A�AqAN�AMA��A�sA�*A��AkQA��A��A=�A��A/�A_A��A��A'�A�A��A�KAh�A,=A!�A�A�A�A�DA33A�A�sA�pA��A��A=�A�A�AA�}AI�A�!A/�A��A��A=A�A
�3A
_A
�A��A��A�dAƨA��A��A	lAqA!A�A��A��A�\A"�A��AI�A�A��A6�AhsA��AbNA �BA @�@�v�@���@��	@��j@�M@�5�@��@�33@��@���@��;@�Y�@�y>@��@��M@���@�E�@�@��@�!@�Ta@�,=@�ƨ@�E�@�P�@�$@�1�@�@�}@��@�Mj@��@�B[@�X@�	l@�tT@�q�@�%�@��s@��@�8�@���@�!�@��g@�M@�e�@�{J@��@��r@��@ް�@ތ@�*0@�^5@�$@���@��6@۝�@�&@ڒ�@�($@٩�@�hs@�ں@�V�@���@�C�@�Ɇ@֓u@���@��@�e�@�x�@��M@�m�@�	@��K@ь~@�A�@�@��@Ю}@�oi@ЋD@��B@ϓ�@Ή�@��K@�%F@̥z@��@˼�@ˉ7@��8@�z�@ɳ�@���@�1�@�xl@�}V@��@��@��@�w�@��@��9@Ų�@�.I@đ�@�m]@��@���@¯O@���@��>@�@��_@�H�@��>@��7@��@��L@�Q�@��N@�2a@��2@���@�0U@�خ@�>�@���@��b@��O@�C-@�+�@���@�+@��5@���@���@�~�@�G@��H@��@��P@�l�@�A�@�4�@�,�@��6@�/�@�(�@���@���@�ԕ@���@���@�n/@�9�@�Mj@�qv@��4@���@�J�@���@�~(@��Y@���@�͟@���@�Ta@��}@��M@��@�!�@��)@���@�o�@�@���@���@�z@�/�@��4@��@��@���@���@�?}@��	@��@��@�_�@�e@��D@��S@�zx@�F@��f@��@�y>@�S�@���@��n@�X@��'@�u%@�h
@�V�@��@�`B@��@�ȴ@��@�\�@�҉@��@�~�@�Ov@� �@�˒@�]�@���@��+@�v�@�j@�L0@�"h@��@�{J@�q@��R@�S�@�2�@�"h@�b@��.@��Z@��o@��@�F�@�ȴ@��@�{�@�j@���@��'@�s�@�Y@�@���@�_@��$@�iD@�Mj@�<6@�(�@�@���@�U2@�_@��@���@�_p@�&@��"@��<@�~�@�g8@�K^@�_@��W@���@�`B@�7L@� i@��	@���@��@���@���@�B[@�!�@���@��6@���@���@�iD@�Y�@�33@���@��y@��R@���@�@��+@��Q@���@�F�@��@���@�K^@���@�� @��@�*�@�qv@��@��I@��.@�n�@�C-@�x@��[@�:�@��@��@��v@���@�2a@���@��j@��h@�C�@��>@��K@���@�p�@�iD@�:�@�@���@�S�@�'R@��>@���@�!-@�	l@��@��[@��m@���@���@�e�@��Q@�b�@��@��	@��|@��@��&@�n/@�[W@�Mj@�0�@�֡@�)�@�@dZ@~�,@~�6@~�@~��@~)�@}�d@}(�@|�/@|u�@{�Q@{��@{v`@{\)@{dZ@{�*@{��@z�@z6�@zQ@z��@{!-@{]�@{+@z��@z�s@z��@y�@yN<@xQ�@wF�@v��@u�3@uq@tی@t�/@t�@tl"@t[�@tD�@t�@sخ@s&@r�m@r�A@r8�@r@qA @p��@p,=@ot�@oP�@n��@nC�@n\�@n�@nB[@m2a@loi@kخ@kF�@j��@j�@j�B@j�h@jq�@j��@j��@j�1@j�r@js�@jR�@j@�@j+k@i@h��@h�D@hoi@h`�@g˒@gP�@g�@f+k@e�@d��@d`�@cdZ@cC@b�@b��@b��@b�@a�t@a`B@a+@`�5@`�@`c�@_��@_@^�R@^��@^kQ@^4@\�f@\�@\"h@[��@[�@@[�@Z��@Zh
@Z�@Y�~@Y%F@Y4@X�@X�_@W�A@W|�@W,�@Vں@Vz@V�@U�=@U(�@T��@T�@S��@R�@Rq�@R.�@R�@Q��@QJ�@P�@Pz�@P  @O�@O��@O�@O
=@N�8@N��@N��@NH�@M��@L�|@L��@Lm�@K{J@J��@Jں@JTa@I�@IB�@Hl"@G��@F��@F�F@F0U@F�@E�Z@E��@Ea�@E=�@E�@D��@D,=@C��@C�$@Cj�@B�h@BkQ@BV@B=q@A�.@A�t@A�@@�@@��@@e�@?��@>?@>_@=��@=�@=�@=@=�n@=x�@=?}@<��@<��@<�@:ߤ@:z@:YK@:$�@9�@9��@9*0@8�5@8��@86@7�m@7��@7��@6��@6+k@6u@5�N@5�t@5�7@5c�@5L�@5F@5;@4�@4��@4C-@3��@2�@2��@2�F@2�r@2H�@2�@1�@1:�@0�4@0�@0|�@0m�@0e�@0U2@0�@/��@/t�@/33@/�@.�c@.�}@.c @-��@-��@-O�@-V@,��@,2�@+�a@+y�@+a@+a@+a@+U�@+H�@*�@*��@*��@*��@*�6@*l�@*5?@*
�@)��@)��@)=�@)V@(��@(��@(�e@(�.@(S�@'��@'��@'�@&�}@&p;@&?@%hs@%(�@%�@$��@$��@$7�@#�@#��@#��@#�{@#.I@#(@"��@"h
@"E�@"+k@"�@!�@!zx@!(�@ �5@ ��@ H@ @�[@8@��@c @!�@&�@	@��@��@�j@��@N<@#�@�$@��@w�@oi@bN@�@dZ@O@�@�m@�@^5@	@��@�N@j@2a@�@�[@��@:�@�@��@�	@|�@o�@W?@4�@
=@��@�@�m@z@^5@R�@?@��@��@m]@G�@#�@;@��@�|@�K@�@��@[�@:�@1@��@��@��@{J@=@��@�6@kQ@$�@J@�.@��@�@A @;@�?@�9@��@��@�_@V�@@��@{J@W?@33@ i@͟@v�@=q@�>@��@!�@�v@��@��@�e@��@~(@*�@�@�@��@�}@�F@�V@|�@v`@\)@,�@
�]@
��@
��@
xl@
J�@
�@	��@	��@	�S111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
m]B
m�B
m�B
o�B
�iB
�#B
��B
��B
��B
��B
��B
��B
��B
��B
�_B
��B
�B
�#B
��B
�B
֡B
ٴB
چB
�#B
یB
یB
��B
یB
��B
�gB
��B
��B
�FB
�SB
��B
�QB
��B
��B
��B(�B��B��B%�BTB�>B�BuB+�B+B+B�B�B�B��B�B�%B��B��B�B�QB��B�xB��B�B�Bn�BP.BJ�B5ZB2|B3�B/�B$�BQB�B�B�cB�B��B�BB�qB�^B��B�UB��B��B�4Bn�BOB8�B=BaB
�LB
�B
��B
��B
�B
��B
�1B
t�B
S�B
=<B
*KB
#�B
�B
bB	�B	�\B	�@B	�1B	�B	��B	��B	�gB	l�B	c�B	S@B	?HB	+6B	"NB	B	HB	
=B	�B	�B�B��B�B	�B	MB	�B	IB	�B	�B	�B	7B	 vB	�B	�B�sB�VB�B�B�QB�_B��B��B�B��B	 iB	�B	EB		�B	0B		RB	
XB	dB	�B	2B	?B	�B	�B	'�B	2-B	<�B	A�B	D�B	H�B	P�B	UMB	^OB	dtB	g�B	i�B	kQB	i�B	iyB	j�B	k�B	l�B	mwB	oOB	r�B	xB	��B	��B	��B	��B	�oB	�uB	�{B	�yB	�]B	�5B	�5B	��B	��B	�9B	��B	��B	�KB	�B	��B	�nB	�B	�~B	�B	��B	�sB	��B	��B	��B	��B	��B	��B	�B	��B	��B	�vB	�9B	��B	�3B	��B	�MB	�hB	�B	��B	�%B	��B	�+B	��B	�B	�JB	�0B	�B	��B	�qB	�wB	�B	��B	��B	�B	��B	ªB	�gB	ňB	�zB	ɠB	�~B	�(B	�\B	�vB	�}B	�[B	�FB	�B	�gB	�yB	��B	�EB	�1B	��B	�xB	�IB	�IB	�/B	�pB	�'B	��B	�B	�NB	�B	�|B	�B	�B	�B	��B	�nB	�nB	��B	�B	�@B	�B	�tB	��B	�B	�zB	��B	�B	��B	�B	��B	�mB	��B	�B	�B	��B	�B	�B	��B	��B	�B	�
B	��B	��B	��B	�B	�B	�]B	��B	��B	�5B	�5B	��B	��B	��B	�OB	��B	�=B	�/B	�B	�[B	�vB	�aB	�GB	�MB	�MB	�B	��B	��B	�TB	��B	��B	�TB	��B	�zB	�RB	�lB	��B	�$B	��B	�*B	��B	��B	��B	�qB	�VB	��B	�]B	�BB	�B	�0B	�6B	��B	��B	��B	��B	��B	��B	��B	�*B	�B	��B	��B	�B	�"B	�<B	��B	�(B	��B	��B	��B	��B	�}B	�}B
 �B
�B
�B
�B
{B
SB
�B
	B

	B
�B

	B
�B

�B
B
VB
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
B
�B
[B
�B
B
}B
B
TB
�B
�B
�B
�B
B
�B
�B
2B
gB
2B
�B
�B
�B
B
B
�B
?B
�B
�B
�B
�B
�B
�B
�B
�B
$B
+B
QB
kB
�B
�B
#B
qB
�B
xB
�B
]B
]B
xB
�B
�B
B
/B
~B
dB
�B
�B
�B
�B
�B
xB
�B
)B
dB
IB
�B
�B
B
B
�B
5B
B
VB
 �B
 'B
!-B
!-B
!�B
!bB
!bB
!|B
"B
"4B
"�B
"�B
#�B
#�B
#�B
$&B
$ZB
$�B
%`B
%,B
%B
%zB
%�B
&LB
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)B
)*B
)B
)*B
)DB
)*B
)DB
)yB
)_B
)�B
)�B
)�B
)�B
)�B
)�B
*0B
)�B
)*B
(�B
'mB
(�B
'8B
$�B
"4B
$@B
$@B
$@B
$&B
#�B
# B
"NB
#:B
$B
$�B
%�B
,B
+kB
*KB
&�B
"B
!|B
!bB
!�B
"4B
"�B
#�B
$ZB
$tB
$�B
%zB
%�B
%�B
%�B
%�B
%�B
%zB
%`B
%�B
%�B
&2B
'mB
'B
&LB
'�B
)DB
*KB
*0B
*eB
)�B
)�B
)�B
)�B
)DB
*B
*B
*�B
+6B
+�B
+�B
+�B
,�B
-)B
-�B
-�B
.IB
.B
-�B
-�B
-�B
.}B
0UB
0�B
0�B
1[B
1�B
2�B
4�B
7B
7�B
8RB
88B
8RB
8lB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9$B
9�B
:�B
;�B
<B
<6B
<6B
<�B
<�B
<�B
=B
=<B
=B
<PB
<B
;�B
;�B
;dB
;�B
<�B
>(B
@ B
=�B
<PB
;�B
;�B
<PB
<�B
=B
>]B
>�B
@�B
A B
A�B
BB
DB
EB
EB
EB
E�B
E�B
ESB
ESB
ESB
EmB
D�B
D�B
D3B
BAB
BB
BB
B[B
B[B
B�B
B�B
CGB
C�B
C�B
C�B
D�B
E9B
E�B
FB
E�B
E9B
D�B
D�B
D�B
F�B
F�B
G_B
G�B
G�B
G�B
G�B
HfB
HKB
HKB
H�B
HB
IB
IB
H�B
H�B
G�B
GzB
G�B
H�B
H�B
H�B
H�B
H�B
HKB
HKB
IB
I�B
I�B
I�B
J=B
K)B
K�B
LB
LdB
LJB
L~B
L�B
L�B
L�B
MB
L�B
MB
NB
NVB
N<B
N�B
O\B
O�B
O�B
PHB
P�B
P�B
QNB
R B
R�B
R�B
S@B
S@B
S[B
SuB
S�B
S�B
S�B
TB
TaB
T�B
T�B
T�B
U�B
U�B
VB
U�B
VB
VmB
VmB
V�B
W$B
W?B
XB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
Z7B
ZB
ZQB
Z�B
Z�B
[=B
\�B
\�B
\�B
\�B
]/B
]�B
^5B
^�B
^�B
_!B
_�B
`'B
`B
`BB
`\B
`�B
a-B
a�B
a�B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
c:B
cnB
c�B
c�B
c�B
c�B
dB
d&B
d�B
d�B
eB
e�B
e�B
e�B
e�B
ffB
f�B
g8B
gmB
gRB
g8B
f�B
f�B
f�B
g8B
gRB
g�B
h
B
h>B
h$B
hXB
hsB
hsB
hsB
hsB
h�B
iB
iDB
i_B
i_B
iyB
i�B
i�B
jB
jB
j�B
k6B
kkB
k�B
k�B
k�B
lB
lWB
l�B
l�B
m]B
m�B
m�B
m�B
n�B
n�B
o B
n�B
n�B
oOB
o�B
o�B
o�B
pUB
poB
p;B
poB
p�B
p�B
p�B
p�B
qAB
q�B
r-B
r�B
r�B
r�B
sMB
s3B
shB
s�B
t9B
tnB
tnB
uZB
u�B
utB
utB
uZB
u�B
u�B
vFB
vzB
v�B
v�B
vzB
v�B
w2B
wB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xRB
xRB
xRB
x�B
x�B
y$B
y$B
y�B
y�B
y�B
y�B
y�B
y�B
z*B
z*B
z*B
z*B
z�B
z�B
z�B
z�B
z�B
{JB
{�B
{�B
{�B
{�B
{�B
|B
{�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}�B
}�B
~B
~BB
~BB
~wB
~�B
~�B
B
HB
cB
HB
cB
}B
� B
�4B
�OB
��B
��B
��B
��B
�B
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
�-B
��B
��B
��B
��B
�3B
�MB
��B
��B
�gB
��B
��B
�B
�B
�B
�B
�SB
�SB
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
m]B
m�B
m�B
o�B
�iB
�#B
��B
��B
��B
��B
��B
��B
��B
��B
�_B
��B
�B
�#B
��B
�B
֡B
ٴB
چB
�#B
یB
یB
��B
یB
��B
�gB
��B
��B
�FB
�SB
��B
�QB
��B
��B
��B(�B��B��B%�BTB�>B�BuB+�B+B+B�B�B�B��B�B�%B��B��B�B�QB��B�xB��B�B�Bn�BP.BJ�B5ZB2|B3�B/�B$�BQB�B�B�cB�B��B�BB�qB�^B��B�UB��B��B�4Bn�BOB8�B=BaB
�LB
�B
��B
��B
�B
��B
�1B
t�B
S�B
=<B
*KB
#�B
�B
bB	�B	�\B	�@B	�1B	�B	��B	��B	�gB	l�B	c�B	S@B	?HB	+6B	"NB	B	HB	
=B	�B	�B�B��B�B	�B	MB	�B	IB	�B	�B	�B	7B	 vB	�B	�B�sB�VB�B�B�QB�_B��B��B�B��B	 iB	�B	EB		�B	0B		RB	
XB	dB	�B	2B	?B	�B	�B	'�B	2-B	<�B	A�B	D�B	H�B	P�B	UMB	^OB	dtB	g�B	i�B	kQB	i�B	iyB	j�B	k�B	l�B	mwB	oOB	r�B	xB	��B	��B	��B	��B	�oB	�uB	�{B	�yB	�]B	�5B	�5B	��B	��B	�9B	��B	��B	�KB	�B	��B	�nB	�B	�~B	�B	��B	�sB	��B	��B	��B	��B	��B	��B	�B	��B	��B	�vB	�9B	��B	�3B	��B	�MB	�hB	�B	��B	�%B	��B	�+B	��B	�B	�JB	�0B	�B	��B	�qB	�wB	�B	��B	��B	�B	��B	ªB	�gB	ňB	�zB	ɠB	�~B	�(B	�\B	�vB	�}B	�[B	�FB	�B	�gB	�yB	��B	�EB	�1B	��B	�xB	�IB	�IB	�/B	�pB	�'B	��B	�B	�NB	�B	�|B	�B	�B	�B	��B	�nB	�nB	��B	�B	�@B	�B	�tB	��B	�B	�zB	��B	�B	��B	�B	��B	�mB	��B	�B	�B	��B	�B	�B	��B	��B	�B	�
B	��B	��B	��B	�B	�B	�]B	��B	��B	�5B	�5B	��B	��B	��B	�OB	��B	�=B	�/B	�B	�[B	�vB	�aB	�GB	�MB	�MB	�B	��B	��B	�TB	��B	��B	�TB	��B	�zB	�RB	�lB	��B	�$B	��B	�*B	��B	��B	��B	�qB	�VB	��B	�]B	�BB	�B	�0B	�6B	��B	��B	��B	��B	��B	��B	��B	�*B	�B	��B	��B	�B	�"B	�<B	��B	�(B	��B	��B	��B	��B	�}B	�}B
 �B
�B
�B
�B
{B
SB
�B
	B

	B
�B

	B
�B

�B
B
VB
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
B
�B
[B
�B
B
}B
B
TB
�B
�B
�B
�B
B
�B
�B
2B
gB
2B
�B
�B
�B
B
B
�B
?B
�B
�B
�B
�B
�B
�B
�B
�B
$B
+B
QB
kB
�B
�B
#B
qB
�B
xB
�B
]B
]B
xB
�B
�B
B
/B
~B
dB
�B
�B
�B
�B
�B
xB
�B
)B
dB
IB
�B
�B
B
B
�B
5B
B
VB
 �B
 'B
!-B
!-B
!�B
!bB
!bB
!|B
"B
"4B
"�B
"�B
#�B
#�B
#�B
$&B
$ZB
$�B
%`B
%,B
%B
%zB
%�B
&LB
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)B
)*B
)B
)*B
)DB
)*B
)DB
)yB
)_B
)�B
)�B
)�B
)�B
)�B
)�B
*0B
)�B
)*B
(�B
'mB
(�B
'8B
$�B
"4B
$@B
$@B
$@B
$&B
#�B
# B
"NB
#:B
$B
$�B
%�B
,B
+kB
*KB
&�B
"B
!|B
!bB
!�B
"4B
"�B
#�B
$ZB
$tB
$�B
%zB
%�B
%�B
%�B
%�B
%�B
%zB
%`B
%�B
%�B
&2B
'mB
'B
&LB
'�B
)DB
*KB
*0B
*eB
)�B
)�B
)�B
)�B
)DB
*B
*B
*�B
+6B
+�B
+�B
+�B
,�B
-)B
-�B
-�B
.IB
.B
-�B
-�B
-�B
.}B
0UB
0�B
0�B
1[B
1�B
2�B
4�B
7B
7�B
8RB
88B
8RB
8lB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9$B
9�B
:�B
;�B
<B
<6B
<6B
<�B
<�B
<�B
=B
=<B
=B
<PB
<B
;�B
;�B
;dB
;�B
<�B
>(B
@ B
=�B
<PB
;�B
;�B
<PB
<�B
=B
>]B
>�B
@�B
A B
A�B
BB
DB
EB
EB
EB
E�B
E�B
ESB
ESB
ESB
EmB
D�B
D�B
D3B
BAB
BB
BB
B[B
B[B
B�B
B�B
CGB
C�B
C�B
C�B
D�B
E9B
E�B
FB
E�B
E9B
D�B
D�B
D�B
F�B
F�B
G_B
G�B
G�B
G�B
G�B
HfB
HKB
HKB
H�B
HB
IB
IB
H�B
H�B
G�B
GzB
G�B
H�B
H�B
H�B
H�B
H�B
HKB
HKB
IB
I�B
I�B
I�B
J=B
K)B
K�B
LB
LdB
LJB
L~B
L�B
L�B
L�B
MB
L�B
MB
NB
NVB
N<B
N�B
O\B
O�B
O�B
PHB
P�B
P�B
QNB
R B
R�B
R�B
S@B
S@B
S[B
SuB
S�B
S�B
S�B
TB
TaB
T�B
T�B
T�B
U�B
U�B
VB
U�B
VB
VmB
VmB
V�B
W$B
W?B
XB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
Z7B
ZB
ZQB
Z�B
Z�B
[=B
\�B
\�B
\�B
\�B
]/B
]�B
^5B
^�B
^�B
_!B
_�B
`'B
`B
`BB
`\B
`�B
a-B
a�B
a�B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
c:B
cnB
c�B
c�B
c�B
c�B
dB
d&B
d�B
d�B
eB
e�B
e�B
e�B
e�B
ffB
f�B
g8B
gmB
gRB
g8B
f�B
f�B
f�B
g8B
gRB
g�B
h
B
h>B
h$B
hXB
hsB
hsB
hsB
hsB
h�B
iB
iDB
i_B
i_B
iyB
i�B
i�B
jB
jB
j�B
k6B
kkB
k�B
k�B
k�B
lB
lWB
l�B
l�B
m]B
m�B
m�B
m�B
n�B
n�B
o B
n�B
n�B
oOB
o�B
o�B
o�B
pUB
poB
p;B
poB
p�B
p�B
p�B
p�B
qAB
q�B
r-B
r�B
r�B
r�B
sMB
s3B
shB
s�B
t9B
tnB
tnB
uZB
u�B
utB
utB
uZB
u�B
u�B
vFB
vzB
v�B
v�B
vzB
v�B
w2B
wB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xRB
xRB
xRB
x�B
x�B
y$B
y$B
y�B
y�B
y�B
y�B
y�B
y�B
z*B
z*B
z*B
z*B
z�B
z�B
z�B
z�B
z�B
{JB
{�B
{�B
{�B
{�B
{�B
|B
{�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}�B
}�B
~B
~BB
~BB
~wB
~�B
~�B
B
HB
cB
HB
cB
}B
� B
�4B
�OB
��B
��B
��B
��B
�B
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
�-B
��B
��B
��B
��B
�3B
�MB
��B
��B
�gB
��B
��B
�B
�B
�B
�B
�SB
�SB
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230314214129  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230314214137  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230314214137  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230314214138                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230314214138  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230314214138  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230314215635                      G�O�G�O�G�O�                