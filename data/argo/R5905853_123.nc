CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-24T03:44:42Z creation;2022-06-24T03:44:43Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220624034442  20220624040150  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               {A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��e�1M�1   @��fD���@..��O�;�c���-V1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8ffB@  BG33BP  BX  B`  Bh  BpffBxffB��B�  B�33B���B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C 33C�fC  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C433C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT�CV  CX  CZ�C[�fC]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ D�|�D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D��3D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@~{@�
=@�
=A�AA�A_�A�A�A�A�A�A�A�A�A�B�HB�B�HB�HB'�HB/�HB8G�B?�HBG{BO�HBW�HB_�HBg�HBpG�BxG�Bz�B��B�#�B��qB��B��qB��B��B��B��B��B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C +�C޸C�RC�RC�RC	޸C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC4+�C5�RC7޸C9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO޸CQ�RCT�CU�RCW�RCZ�C[޸C]޸C_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D�zDzD~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG��DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D�=D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D�{�Dٿ
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
D��=D��
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
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A�OBA�R�A�S�A�V9A�^�A�]�A�a|A�]dA�YA�Z�A�\�A�ZA�`�A�]/A�XEA�YA�R A�1�A�A��#AέAΙ�A�CA�l�Aș1A�"A���A��sAƴ9A�~A�4A�	A�бA�!�A�A��3A��#A�A�]�A���A���A�N<A��A�ߤA��4A�!A���A��A�]/A�	lA���A���A��qA�(A�˒A��2A�FtA�!�A���A��5A�)�A�?A���A��hA��A�yrA�%A���A�*�A���A���A�˒A��A�{�A�6�A�q�A���A��SA��A���A�yrA�J#A���A�L�A�`BA�TaA}z�AzzAx��AvoAs&Ao�Ai�AfS�A_\�A["�AXm]AS�jAP��AJ�AG�9AD�7AAh�A=0�A;�A8[WA6X�A5�tA2�NA/�<A.C�A-C-A,��A+�nA*��A*2aA)�A)��A(��A(�qA(E�A'	A&�XA&i�A%��A%��A$�A#8A#<�A#E9A"�9A"�DA!�YA خA!8�A!��A!S�A �A ˒A W?A�A~�Ap;A��A��Ao�AffA.IAe,A��A��A��AFAl�AC-A	A:�A��A&�A�A	lA8�A�A�A�[AGA�7A��A=A�	A|AƨA
�*A
m]A
QA
:�A
 �A	�PA	g8A	H�A�A��A8�A�>A��Ao A6A%A��A"hA��A�tA�"AU�A�Aw�A��A�A�jA6zAA�A�A �vA V@��Z@���@�o @���@�@���@�j�@���@�:�@��g@��<@�($@�Y�@���@�K^@���@��@���@���@���@��b@��@�$@�q�@�	�@��@�
=@�1'@���@��@���@�q@�h@�	@�dZ@���@�q@�B[@�,=@���@��z@��a@��n@�j�@��)@�4@�Y@��N@�n/@��@�u%@�X�@��@�y>@�q@�e�@�Ov@�0U@��@��a@�o�@��]@��@�l"@�M�@��@�bN@��@�&�@�7�@�/@܌�@�&@ڞ�@�u%@�ݘ@�<6@�Ɇ@׬q@��@�V@��)@Ր�@�|�@Ӭq@��[@Ұ�@ҥz@ҍ�@��@�a�@Ч@�J@ύP@��@�Ĝ@�+k@�c�@̬@ˮ@ʝI@��.@��@�@O@�+@�Ɇ@�-�@�o @��@�:*@�?}@�ں@Ĉ�@ç�@�4@�&�@��@�Y@��@¾�@�V@���@��F@��"@�X@�-w@��p@�w�@�:*@��@��@�)_@��P@���@�g8@��@���@���@�]�@�F�@���@�l"@�6�@��X@���@�\�@�O@��@��F@�1�@���@�x@���@���@���@���@���@�<6@���@��@���@�Y@�ѷ@���@�`�@�,=@��z@���@���@�O@��q@�|@�?}@�͟@�y>@��@��@�a@��@��u@�v�@��d@�0�@���@�p;@�$@��@��@���@��f@�S�@��@���@�Ft@��@��M@�F@��P@��@��6@�H�@��@���@�u�@�F�@��p@�	@��)@�ƨ@�x�@��S@�0�@��e@��.@��F@�k�@�e�@�Mj@��@��@���@��@��@���@�6z@�IR@�=�@�>�@��@�q@��@���@��@�~�@�YK@�n�@�q@��m@��@�n/@�O@�9�@�-w@�C@���@���@���@��o@�7@�u@���@��@�m]@��@��/@���@�|�@�L0@��@�� @���@���@�!�@�� @���@��@�g�@�6z@��P@��<@���@�l"@�1�@�!@��@��@�/@��@��U@�z@���@��7@�k�@�+�@��[@��@�z�@�4n@�@��@��[@�X�@��@��y@��X@���@�YK@���@���@���@��P@�W?@�O@�L�@�$t@���@��@���@���@��@���@�M@��@��]@��g@��~@�S�@�<6@� \@���@�`�@�  @��@@�T�@�7L@�;@��b@��+@�I�@�@��}@�iD@���@���@�|�@�	@��r@��h@��@���@���@�9X@�'R@��@��@��)@�خ@��K@�y�@��@���@���@���@���@�`�@�;�@�	@���@���@�b�@���@�A�@��}@��q@��k@�X�@�.I@�ߤ@��@�H�@�$@�O@�]@e�@~p;@~^5@~+k@}��@}p�@|�D@|'R@{��@{{J@{t�@z��@zW�@z
�@y�@y0�@x��@x��@x�p@x[�@x �@x�@w��@v��@vp;@vW�@v.�@u�T@u��@u@t��@s�+@s��@s��@s�V@sA�@r�x@rGE@r($@q��@q��@q��@qw2@q4@pɆ@o�;@o33@n��@mԕ@m�"@ma�@mG�@m%F@l�	@lѷ@l�.@k�m@kdZ@j�@j��@j��@jh
@i�t@iT�@i&�@h��@hĜ@h�e@gy�@gW?@gF�@g,�@gS@f��@f��@fkQ@fTa@f0U@eϫ@e�@e(�@d��@d7�@cخ@cA�@b��@b$�@a2a@`�@_X�@^W�@]�@]G�@\�v@\��@\Ft@[�r@[�@[��@[8@Z�"@Z�!@Z��@Z��@Y��@X�/@XS�@XA�@X4n@X@WA�@W(@V�<@V�F@U��@Tѷ@S��@R��@R$�@Q��@Q�@Q��@Q!�@P�_@P�@O��@O�@N�A@M�)@L�`@L[�@L>B@K�@K9�@J�!@J5?@I��@Iq@H�@HA�@H�@G�w@Ga@G@F�@Fa|@F �@E�j@E�@E^�@E	l@D�@D�Y@D�@DQ�@D2�@C��@C+@B�H@BOv@A�o@A��@AVm@@�@@��@@�@?��@?�P@?\)@?@>ں@>�@>&�@=�^@=o @=Dg@<�`@<I�@;�@;y�@;�@:�<@:$�@9��@9��@9|@9rG@9!�@8ѷ@8?�@7�}@7iD@7�@6��@6��@6_�@6$�@6�@5�.@5��@5�#@5��@5u�@54@4��@4�_@4'R@3�Q@3�V@3�4@3S�@3'�@3�@2��@2��@2��@2�\@2?@2�@1�>@1��@1�@1��@1x�@1p�@1m]@1[W@1B�@1�@0��@0�@0��@0w�@02�@/�@/� @/�@/��@/�	@/x@/]�@/U�@/6z@/+@/�@.�"@.͟@.{�@.H�@-��@-f�@,�v@,��@,�D@,!@+�{@+a@+F�@*��@*��@*n�@*YK@*3�@)��@)�@)Q�@)�@(��@(�@(j@(b@'��@'F�@'@&�B@&�@&xl@&!�@%�>@%�@%�h@%[W@%2a@$�@$g8@$1@#�0@#;d@"�"@"�B@"� @"i�@"4@!�@!��@!�@!e,@!�@ �@ �@ :�@��@�@qv@O@Y@ i@�y@�x@GE@�@ԕ@�X@x�@-w@��@��@(�@x@��@ƨ@��@��@K�@�@�@��@�s@��@c @:*@��@�9@s�@4@�@�@��@�@��@~(@$@�@�{@y�@>�@�H@�x@{�@c @Ov@:*@�H@��@�"@�~@}�@f�@=�@��@�@�`@��@ѷ@��@g8@>B@M@�g@��@o�@K�@33@ i@��@��@}V@_�@.�@�D@ϫ@�7@B�@(�@�@��@�u@c�@4n@�]@��@��@a@P�@�@�@�<@�h@q�@.�@�@��@�-@w2@S&@?}@V@�@�`@֡@�$@�z@oi@7@�}@�@�F@qv@4�@+@�@
҉@
��@
��@
��@
Ta@
6�@
1�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A�OBA�R�A�S�A�V9A�^�A�]�A�a|A�]dA�YA�Z�A�\�A�ZA�`�A�]/A�XEA�YA�R A�1�A�A��#AέAΙ�A�CA�l�Aș1A�"A���A��sAƴ9A�~A�4A�	A�бA�!�A�A��3A��#A�A�]�A���A���A�N<A��A�ߤA��4A�!A���A��A�]/A�	lA���A���A��qA�(A�˒A��2A�FtA�!�A���A��5A�)�A�?A���A��hA��A�yrA�%A���A�*�A���A���A�˒A��A�{�A�6�A�q�A���A��SA��A���A�yrA�J#A���A�L�A�`BA�TaA}z�AzzAx��AvoAs&Ao�Ai�AfS�A_\�A["�AXm]AS�jAP��AJ�AG�9AD�7AAh�A=0�A;�A8[WA6X�A5�tA2�NA/�<A.C�A-C-A,��A+�nA*��A*2aA)�A)��A(��A(�qA(E�A'	A&�XA&i�A%��A%��A$�A#8A#<�A#E9A"�9A"�DA!�YA خA!8�A!��A!S�A �A ˒A W?A�A~�Ap;A��A��Ao�AffA.IAe,A��A��A��AFAl�AC-A	A:�A��A&�A�A	lA8�A�A�A�[AGA�7A��A=A�	A|AƨA
�*A
m]A
QA
:�A
 �A	�PA	g8A	H�A�A��A8�A�>A��Ao A6A%A��A"hA��A�tA�"AU�A�Aw�A��A�A�jA6zAA�A�A �vA V@��Z@���@�o @���@�@���@�j�@���@�:�@��g@��<@�($@�Y�@���@�K^@���@��@���@���@���@��b@��@�$@�q�@�	�@��@�
=@�1'@���@��@���@�q@�h@�	@�dZ@���@�q@�B[@�,=@���@��z@��a@��n@�j�@��)@�4@�Y@��N@�n/@��@�u%@�X�@��@�y>@�q@�e�@�Ov@�0U@��@��a@�o�@��]@��@�l"@�M�@��@�bN@��@�&�@�7�@�/@܌�@�&@ڞ�@�u%@�ݘ@�<6@�Ɇ@׬q@��@�V@��)@Ր�@�|�@Ӭq@��[@Ұ�@ҥz@ҍ�@��@�a�@Ч@�J@ύP@��@�Ĝ@�+k@�c�@̬@ˮ@ʝI@��.@��@�@O@�+@�Ɇ@�-�@�o @��@�:*@�?}@�ں@Ĉ�@ç�@�4@�&�@��@�Y@��@¾�@�V@���@��F@��"@�X@�-w@��p@�w�@�:*@��@��@�)_@��P@���@�g8@��@���@���@�]�@�F�@���@�l"@�6�@��X@���@�\�@�O@��@��F@�1�@���@�x@���@���@���@���@���@�<6@���@��@���@�Y@�ѷ@���@�`�@�,=@��z@���@���@�O@��q@�|@�?}@�͟@�y>@��@��@�a@��@��u@�v�@��d@�0�@���@�p;@�$@��@��@���@��f@�S�@��@���@�Ft@��@��M@�F@��P@��@��6@�H�@��@���@�u�@�F�@��p@�	@��)@�ƨ@�x�@��S@�0�@��e@��.@��F@�k�@�e�@�Mj@��@��@���@��@��@���@�6z@�IR@�=�@�>�@��@�q@��@���@��@�~�@�YK@�n�@�q@��m@��@�n/@�O@�9�@�-w@�C@���@���@���@��o@�7@�u@���@��@�m]@��@��/@���@�|�@�L0@��@�� @���@���@�!�@�� @���@��@�g�@�6z@��P@��<@���@�l"@�1�@�!@��@��@�/@��@��U@�z@���@��7@�k�@�+�@��[@��@�z�@�4n@�@��@��[@�X�@��@��y@��X@���@�YK@���@���@���@��P@�W?@�O@�L�@�$t@���@��@���@���@��@���@�M@��@��]@��g@��~@�S�@�<6@� \@���@�`�@�  @��@@�T�@�7L@�;@��b@��+@�I�@�@��}@�iD@���@���@�|�@�	@��r@��h@��@���@���@�9X@�'R@��@��@��)@�خ@��K@�y�@��@���@���@���@���@�`�@�;�@�	@���@���@�b�@���@�A�@��}@��q@��k@�X�@�.I@�ߤ@��@�H�@�$@�O@�]@e�@~p;@~^5@~+k@}��@}p�@|�D@|'R@{��@{{J@{t�@z��@zW�@z
�@y�@y0�@x��@x��@x�p@x[�@x �@x�@w��@v��@vp;@vW�@v.�@u�T@u��@u@t��@s�+@s��@s��@s�V@sA�@r�x@rGE@r($@q��@q��@q��@qw2@q4@pɆ@o�;@o33@n��@mԕ@m�"@ma�@mG�@m%F@l�	@lѷ@l�.@k�m@kdZ@j�@j��@j��@jh
@i�t@iT�@i&�@h��@hĜ@h�e@gy�@gW?@gF�@g,�@gS@f��@f��@fkQ@fTa@f0U@eϫ@e�@e(�@d��@d7�@cخ@cA�@b��@b$�@a2a@`�@_X�@^W�@]�@]G�@\�v@\��@\Ft@[�r@[�@[��@[8@Z�"@Z�!@Z��@Z��@Y��@X�/@XS�@XA�@X4n@X@WA�@W(@V�<@V�F@U��@Tѷ@S��@R��@R$�@Q��@Q�@Q��@Q!�@P�_@P�@O��@O�@N�A@M�)@L�`@L[�@L>B@K�@K9�@J�!@J5?@I��@Iq@H�@HA�@H�@G�w@Ga@G@F�@Fa|@F �@E�j@E�@E^�@E	l@D�@D�Y@D�@DQ�@D2�@C��@C+@B�H@BOv@A�o@A��@AVm@@�@@��@@�@?��@?�P@?\)@?@>ں@>�@>&�@=�^@=o @=Dg@<�`@<I�@;�@;y�@;�@:�<@:$�@9��@9��@9|@9rG@9!�@8ѷ@8?�@7�}@7iD@7�@6��@6��@6_�@6$�@6�@5�.@5��@5�#@5��@5u�@54@4��@4�_@4'R@3�Q@3�V@3�4@3S�@3'�@3�@2��@2��@2��@2�\@2?@2�@1�>@1��@1�@1��@1x�@1p�@1m]@1[W@1B�@1�@0��@0�@0��@0w�@02�@/�@/� @/�@/��@/�	@/x@/]�@/U�@/6z@/+@/�@.�"@.͟@.{�@.H�@-��@-f�@,�v@,��@,�D@,!@+�{@+a@+F�@*��@*��@*n�@*YK@*3�@)��@)�@)Q�@)�@(��@(�@(j@(b@'��@'F�@'@&�B@&�@&xl@&!�@%�>@%�@%�h@%[W@%2a@$�@$g8@$1@#�0@#;d@"�"@"�B@"� @"i�@"4@!�@!��@!�@!e,@!�@ �@ �@ :�@��@�@qv@O@Y@ i@�y@�x@GE@�@ԕ@�X@x�@-w@��@��@(�@x@��@ƨ@��@��@K�@�@�@��@�s@��@c @:*@��@�9@s�@4@�@�@��@�@��@~(@$@�@�{@y�@>�@�H@�x@{�@c @Ov@:*@�H@��@�"@�~@}�@f�@=�@��@�@�`@��@ѷ@��@g8@>B@M@�g@��@o�@K�@33@ i@��@��@}V@_�@.�@�D@ϫ@�7@B�@(�@�@��@�u@c�@4n@�]@��@��@a@P�@�@�@�<@�h@q�@.�@�@��@�-@w2@S&@?}@V@�@�`@֡@�$@�z@oi@7@�}@�@�F@qv@4�@+@�@
҉@
��@
��@
��@
Ta@
6�@
1�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
U2B
T�B
T�B
T�B
T�B
TaB
T�B
TFB
T�B
U2B
UgB
U2B
U�B
T�B
U�B
V�B
V�B
W�B
\�B
b4B
gB
i�B
jKB
mCB
��B
�FB
�B.B"�B-wBA;BPHBU�Bg�B��B��B0UBJ�BY1Bd�B�B�\B�FB�B��B��B��B�@B��B��B��B�9B�B�OB��B��B~BBxlBwBpoBi*BhXBH�BsB��B�B�B�IB�SB��B�dBb�BS�BT�BUBDMB�B
��B
�QB
�WB
�HB
�8B
�$B
|�B
q'B
^B
2�B
oB	�sB	�
B	ªB	� B	�B	v`B	^B	9�B	�B	�B	�B��B��B�!B�B��B�&B�hB��BܒB�vB�OB�xB	9B	\B	�B	"�B	2|B	:*B	:*B	B�B	T{B	VmB	a�B	m]B	}"B	��B	�<B	�?B	�_B	�B	��B	�LB	��B	�|B	�TB	��B	��B	�B	�B	�B	�B	�
B	��B	�CB	�B	�=B	��B	�XB	�rB	ȴB	�'B	��B	�0B	ňB	͟B	�B	��B	�?B	��B	��B	�hB	�>B	ܒB	յB	�B	ʌB	��B	��B	�B	��B	�YB	�B	�%B	��B	�YB	�1B	�VB	�SB	ևB	֡B	�EB	ޞB	�&B	��B	�FB	��B	��B	�B	�B	�]B	�]B	�B	�B	�B	� B	�B	��B	��B	�B	�CB	��B	�B	��B	�IB	�B	�B	��B	��B	�B	�tB	��B	��B	�B	�LB	��B	��B	��B	�lB	�+B	��B	�FB	�zB	��B	�|B	�B	��B	�IB	�}B	��B	�[B	��B	�jB	�^B	�	B	��B	�GB	�B	�B	�0B	�B	��B	��B	�>B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�RB	�RB	��B	��B	��B	��B	�xB	��B	�XB	��B	��B	��B	�B	�dB	�dB	�B	�^B	��B	��B	�B	�B	�B	�oB	�B	�OB	�cB	��B	��B	�B	�}B	��B	�!B	�B	�B	�OB	�!B	�B	��B	�wB	�B	�B	�B	�B	�6B	�kB	�kB	�kB	�B	�WB	�B	�B	�B	�5B	�B	�OB	�B	�B	�B	�B	��B	��B	��B	�B	�B	�-B	�B	��B	�B	�tB	��B	��B	��B	��B	��B	��B	�+B	�`B	��B	��B	��B	��B	��B	��B	��B	�B	�lB	��B	�	B	��B	�XB	��B	�^B	�^B	�xB	�xB	�dB	�dB	�dB	��B	��B	�PB	�PB	�B	��B	��B	�B	��B	�jB	�jB	�6B	�6B	�B	�jB	��B	��B	��B	��B	��B	��B	�qB	�VB	��B	�wB	�.B	�}B	�cB	�cB	�cB	��B	��B	��B	�}B	��B
 B
 4B
 iB
UB
 OB
 �B
B
GB
�B
�B
�B
SB
9B
mB
�B
YB
�B
�B
tB
�B
�B
�B
�B
EB
zB
zB
_B
�B
�B
3B
�B
�B
�B
�B
�B
gB
GB
�B
�B
B
�B
�B
�B
[B
 �B	�qB	��B	��B	��B	�cB	�HB
  B
B
uB
�B
�B
tB
zB
	lB

rB

�B

rB

�B

�B
xB
�B
�B
�B
�B
B
HB
�B
�B
}B
�B
�B
B
uB
uB
�B
�B
FB
,B
9B
SB
�B
?B
?B
�B
yB
�B
�B
�B
B
B
�B
�B
�B
�B
	B
�B
#B
)B
)B
�B
�B
B
B
dB
5B
�B
B
B
;B
�B
�B
 B
 \B
 \B
!-B
!bB
!bB
!|B
!�B
!�B
!�B
!�B
!�B
"B
"NB
"NB
"NB
"NB
"�B
"�B
"�B
"�B
#nB
#nB
#�B
#nB
#�B
#�B
#�B
$tB
$�B
%,B
%�B
&LB
&B
&�B
'B
'B
'RB
'�B
'�B
(sB
(�B
(�B
)DB
*KB
+6B
+kB
,B
,"B
,=B
,qB
,WB
,qB
,WB
,�B
-)B
-wB
-]B
-�B
.B
.IB
.B
-�B
-�B
-�B
.�B
/�B
0B
0oB
0UB
0UB
0�B
0�B
1'B
1vB
1�B
2-B
1�B
2B
2�B
2�B
2�B
2�B
2�B
3B
2�B
33B
3hB
3�B
3�B
49B
4�B
5B
5�B
6+B
6+B
6+B
6zB
6�B
6�B
6�B
7fB
7�B
8B
8B
8B
8RB
8�B
9$B
9$B
9�B
9�B
9�B
9�B
:B
:�B
;B
;JB
;B
;�B
;�B
;dB
;B
;�B
<�B
<�B
=qB
>B
>]B
>wB
>�B
>�B
>�B
>�B
>�B
?�B
@ B
@�B
@�B
@�B
@�B
A B
AoB
A�B
A�B
A�B
A�B
B�B
B�B
B�B
CB
C-B
CB
CaB
CaB
C{B
C�B
CaB
C{B
C�B
DMB
D3B
DMB
D�B
C�B
CGB
B�B
B�B
B�B
A�B
A�B
BB
BAB
B[B
B[B
B�B
B�B
B�B
B�B
B�B
CB
C-B
CB
C�B
D�B
D�B
D�B
D�B
D�B
ESB
E�B
E�B
F�B
E�B
FB
F�B
G�B
G�B
HKB
H�B
I�B
JXB
J=B
J=B
J�B
J�B
KB
J�B
K)B
KxB
L0B
L�B
L�B
MB
MB
M�B
M�B
N"B
NVB
N<B
N�B
N�B
OBB
O�B
O�B
Q�B
RB
RoB
R�B
S@B
TaB
T�B
V�B
V�B
V�B
W�B
X�B
X�B
Y1B
Y1B
YeB
Y�B
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
[WB
[�B
[�B
\CB
\�B
\�B
\�B
]dB
]�B
]�B
^jB
^�B
^�B
^�B
^�B
_�B
_�B
_�B
`B
`'B
`�B
`�B
aB
aHB
a|B
a�B
bB
bB
bNB
bNB
b4B
bNB
b�B
b�B
b�B
cTB
cnB
c�B
c�B
d@B
d&B
d@B
dtB
dtB
d�B
d�B
d�B
d�B
d�B
eB
eFB
ezB
ezB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
fB
fLB
fLB
fLB
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
g8B
g8B
g8B
gRB
gmB
g�B
g�B
hXB
h�B
iB
iB
i*B
i�B
j0B
j0B
jKB
jB
j�B
kB
kB
kB
k6B
kkB
k�B
k�B
lB
l=B
lWB
l�B
mB
mCB
m]B
m�B
m�B
m�B
n/B
nIB
nIB
n�B
n�B
n�B
o5B
o�B
o�B
o�B
poB
p�B
p�B
p�B
p�B
q'B
q[B
q�B
q�B
q�B
q�B
r-B
raB
r�B
r�B
r�B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
t9B
t9B
tnB
t�B
t�B
uB
u?B
utB
u�B
vB
v+B
v�B
v�B
v�B
v�B
w�B
w�B
x8B
xlB
x8B
xlB
x�B
y$B
y�B
zB
zB
zDB
z�B
z�B
{B
{B
z�B
z�B
z�B
z�B
z�B
zxB
zDB
z*B
z*B
z^B
z�B
z*B
y�B
y�B
y�B
y�B
y�B
z*B
zxB
zxB
z�B
zxB
z�B
z�B
{0B
{JB
{B
{�B
|B
|jB
|PB
|PB
|�B
}B
|�B
|�B
|�B
|�B
|�B
}<B
}�B
}�B
}�B
}�B
}�B
~BB
~wB
~�B
~�B
~�B
.B
HB
�B
�B
� B
�B
�B
�B
��B
��B
��B
��B
� B
�UB
�;B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�-B
�aB
�aB
��B
��B
��B
��B
��B
�gB
�MB
�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
U2B
T�B
T�B
T�B
T�B
TaB
T�B
TFB
T�B
U2B
UgB
U2B
U�B
T�B
U�B
V�B
V�B
W�B
\�B
b4B
gB
i�B
jKB
mCB
��B
�FB
�B.B"�B-wBA;BPHBU�Bg�B��B��B0UBJ�BY1Bd�B�B�\B�FB�B��B��B��B�@B��B��B��B�9B�B�OB��B��B~BBxlBwBpoBi*BhXBH�BsB��B�B�B�IB�SB��B�dBb�BS�BT�BUBDMB�B
��B
�QB
�WB
�HB
�8B
�$B
|�B
q'B
^B
2�B
oB	�sB	�
B	ªB	� B	�B	v`B	^B	9�B	�B	�B	�B��B��B�!B�B��B�&B�hB��BܒB�vB�OB�xB	9B	\B	�B	"�B	2|B	:*B	:*B	B�B	T{B	VmB	a�B	m]B	}"B	��B	�<B	�?B	�_B	�B	��B	�LB	��B	�|B	�TB	��B	��B	�B	�B	�B	�B	�
B	��B	�CB	�B	�=B	��B	�XB	�rB	ȴB	�'B	��B	�0B	ňB	͟B	�B	��B	�?B	��B	��B	�hB	�>B	ܒB	յB	�B	ʌB	��B	��B	�B	��B	�YB	�B	�%B	��B	�YB	�1B	�VB	�SB	ևB	֡B	�EB	ޞB	�&B	��B	�FB	��B	��B	�B	�B	�]B	�]B	�B	�B	�B	� B	�B	��B	��B	�B	�CB	��B	�B	��B	�IB	�B	�B	��B	��B	�B	�tB	��B	��B	�B	�LB	��B	��B	��B	�lB	�+B	��B	�FB	�zB	��B	�|B	�B	��B	�IB	�}B	��B	�[B	��B	�jB	�^B	�	B	��B	�GB	�B	�B	�0B	�B	��B	��B	�>B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�RB	�RB	��B	��B	��B	��B	�xB	��B	�XB	��B	��B	��B	�B	�dB	�dB	�B	�^B	��B	��B	�B	�B	�B	�oB	�B	�OB	�cB	��B	��B	�B	�}B	��B	�!B	�B	�B	�OB	�!B	�B	��B	�wB	�B	�B	�B	�B	�6B	�kB	�kB	�kB	�B	�WB	�B	�B	�B	�5B	�B	�OB	�B	�B	�B	�B	��B	��B	��B	�B	�B	�-B	�B	��B	�B	�tB	��B	��B	��B	��B	��B	��B	�+B	�`B	��B	��B	��B	��B	��B	��B	��B	�B	�lB	��B	�	B	��B	�XB	��B	�^B	�^B	�xB	�xB	�dB	�dB	�dB	��B	��B	�PB	�PB	�B	��B	��B	�B	��B	�jB	�jB	�6B	�6B	�B	�jB	��B	��B	��B	��B	��B	��B	�qB	�VB	��B	�wB	�.B	�}B	�cB	�cB	�cB	��B	��B	��B	�}B	��B
 B
 4B
 iB
UB
 OB
 �B
B
GB
�B
�B
�B
SB
9B
mB
�B
YB
�B
�B
tB
�B
�B
�B
�B
EB
zB
zB
_B
�B
�B
3B
�B
�B
�B
�B
�B
gB
GB
�B
�B
B
�B
�B
�B
[B
 �B	�qB	��B	��B	��B	�cB	�HB
  B
B
uB
�B
�B
tB
zB
	lB

rB

�B

rB

�B

�B
xB
�B
�B
�B
�B
B
HB
�B
�B
}B
�B
�B
B
uB
uB
�B
�B
FB
,B
9B
SB
�B
?B
?B
�B
yB
�B
�B
�B
B
B
�B
�B
�B
�B
	B
�B
#B
)B
)B
�B
�B
B
B
dB
5B
�B
B
B
;B
�B
�B
 B
 \B
 \B
!-B
!bB
!bB
!|B
!�B
!�B
!�B
!�B
!�B
"B
"NB
"NB
"NB
"NB
"�B
"�B
"�B
"�B
#nB
#nB
#�B
#nB
#�B
#�B
#�B
$tB
$�B
%,B
%�B
&LB
&B
&�B
'B
'B
'RB
'�B
'�B
(sB
(�B
(�B
)DB
*KB
+6B
+kB
,B
,"B
,=B
,qB
,WB
,qB
,WB
,�B
-)B
-wB
-]B
-�B
.B
.IB
.B
-�B
-�B
-�B
.�B
/�B
0B
0oB
0UB
0UB
0�B
0�B
1'B
1vB
1�B
2-B
1�B
2B
2�B
2�B
2�B
2�B
2�B
3B
2�B
33B
3hB
3�B
3�B
49B
4�B
5B
5�B
6+B
6+B
6+B
6zB
6�B
6�B
6�B
7fB
7�B
8B
8B
8B
8RB
8�B
9$B
9$B
9�B
9�B
9�B
9�B
:B
:�B
;B
;JB
;B
;�B
;�B
;dB
;B
;�B
<�B
<�B
=qB
>B
>]B
>wB
>�B
>�B
>�B
>�B
>�B
?�B
@ B
@�B
@�B
@�B
@�B
A B
AoB
A�B
A�B
A�B
A�B
B�B
B�B
B�B
CB
C-B
CB
CaB
CaB
C{B
C�B
CaB
C{B
C�B
DMB
D3B
DMB
D�B
C�B
CGB
B�B
B�B
B�B
A�B
A�B
BB
BAB
B[B
B[B
B�B
B�B
B�B
B�B
B�B
CB
C-B
CB
C�B
D�B
D�B
D�B
D�B
D�B
ESB
E�B
E�B
F�B
E�B
FB
F�B
G�B
G�B
HKB
H�B
I�B
JXB
J=B
J=B
J�B
J�B
KB
J�B
K)B
KxB
L0B
L�B
L�B
MB
MB
M�B
M�B
N"B
NVB
N<B
N�B
N�B
OBB
O�B
O�B
Q�B
RB
RoB
R�B
S@B
TaB
T�B
V�B
V�B
V�B
W�B
X�B
X�B
Y1B
Y1B
YeB
Y�B
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
[WB
[�B
[�B
\CB
\�B
\�B
\�B
]dB
]�B
]�B
^jB
^�B
^�B
^�B
^�B
_�B
_�B
_�B
`B
`'B
`�B
`�B
aB
aHB
a|B
a�B
bB
bB
bNB
bNB
b4B
bNB
b�B
b�B
b�B
cTB
cnB
c�B
c�B
d@B
d&B
d@B
dtB
dtB
d�B
d�B
d�B
d�B
d�B
eB
eFB
ezB
ezB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
fB
fLB
fLB
fLB
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
g8B
g8B
g8B
gRB
gmB
g�B
g�B
hXB
h�B
iB
iB
i*B
i�B
j0B
j0B
jKB
jB
j�B
kB
kB
kB
k6B
kkB
k�B
k�B
lB
l=B
lWB
l�B
mB
mCB
m]B
m�B
m�B
m�B
n/B
nIB
nIB
n�B
n�B
n�B
o5B
o�B
o�B
o�B
poB
p�B
p�B
p�B
p�B
q'B
q[B
q�B
q�B
q�B
q�B
r-B
raB
r�B
r�B
r�B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
t9B
t9B
tnB
t�B
t�B
uB
u?B
utB
u�B
vB
v+B
v�B
v�B
v�B
v�B
w�B
w�B
x8B
xlB
x8B
xlB
x�B
y$B
y�B
zB
zB
zDB
z�B
z�B
{B
{B
z�B
z�B
z�B
z�B
z�B
zxB
zDB
z*B
z*B
z^B
z�B
z*B
y�B
y�B
y�B
y�B
y�B
z*B
zxB
zxB
z�B
zxB
z�B
z�B
{0B
{JB
{B
{�B
|B
|jB
|PB
|PB
|�B
}B
|�B
|�B
|�B
|�B
|�B
}<B
}�B
}�B
}�B
}�B
}�B
~BB
~wB
~�B
~�B
~�B
.B
HB
�B
�B
� B
�B
�B
�B
��B
��B
��B
��B
� B
�UB
�;B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�-B
�aB
�aB
��B
��B
��B
��B
��B
�gB
�MB
�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220624034244  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220624034442  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220624034443  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220624034443                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220624124447  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220624124447  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220624040150                      G�O�G�O�G�O�                