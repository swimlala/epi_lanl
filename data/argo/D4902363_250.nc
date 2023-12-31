CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-16T00:35:05Z creation;2018-06-16T00:35:10Z conversion to V3.1;2019-12-19T07:39:50Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180616003505  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_250                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�j�%�~ 1   @�j��>� @:�a@O�dBxF�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @~{@�
=@�
=A�A?�A_�A��\A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�C�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#޸C%޸C'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCF�CG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW޸CY�RC[�RC]�RC_�RCa�RCc�RCe޸Cg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=�{D>{D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��=D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D��=D��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�\)A��A��HA���A��wA�bA��PA�$�A��A���A�n�A���A��RA�r�A��;A���A���A��A��
A��A�jA� �A��-A�dZA�JA�33A�7LA�~�A���A�x�A��uA��A�/A�E�A�7LA��A�ffA�jA��A�z�A��RA��A�hsA���A�ƨA�JA�/A��DA�
=A��\A�;dA� �A�33A�dZA���A�t�A��A���A�~�A��PA�A�A���A�XA���A�ffA�G�A��TA��RA�  A�\)A��A�G�A�K�A��!A�33A��;A�|�AA}O�A|$�Az�AzI�Ax�DAw/At�Ar�+Aq`BAn��Ak�FAi�-Ag"�Af�uAfJAe��Ae/Ac/Abz�AbA�AaAa�Aa?}A`5?A_�wA_33A^�A]`BA\ȴA\1'A[/AY�-AY�AXAV�AVJAT�!ATAS�ARbNAQ+AP1'AO�^AO%ANE�AL1'AJ��AI+AH5?AGO�AD��AC�FACS�AC?}ABv�AA�7A@��A@VA>{A<��A<  A;�PA:��A:��A:I�A9|�A9A8�`A8�\A7�#A7\)A7�A6��A6E�A6JA5��A4��A4��A3��A3�A2ȴA2�DA1��A1K�A0n�A/�A/�PA/;dA.�DA-p�A,�yA,��A+�-A*=qA)A)�-A)"�A&ȴA%p�A$v�A#`BA"z�A"=qA!`BA ��A Q�A �A�-Al�A�A�AƨA"�AQ�A7LA�RA\)A��A�AbNA9XA�TA�-A\)A�9AQ�A$�A`BAjA��A�!AjA�TA\)A��AffAffA^5A{A��A�A�HA��Av�A1'A��AXAoA��A�wA	S�AI�A�mAt�A��A%An�A(�A�wA�A�DA=qA��AK�A I�@��@��D@���@�x�@��9@�l�@�ȴ@��+@��@�~�@�/@�(�@���@�&�@�+@��@��@�+@��@�9X@�+@��@ߍP@��@�X@�C�@ى7@��@�o@�^5@ղ-@ԣ�@�b@ӍP@ҧ�@�@��@�hs@ϥ�@�C�@��@�ȴ@͡�@̋D@˝�@�C�@��y@�n�@ɩ�@�&�@��/@�9X@��@��H@�z�@��m@Ý�@�o@��7@�Z@��@�;d@��y@��\@�V@�S�@���@�/@��D@�ƨ@�33@���@��7@�
=@���@��@�j@�I�@�t�@�
=@���@�/@��/@���@��m@�33@�@�&�@��;@���@��+@��@�1'@��y@�$�@��#@�?}@���@�z�@�r�@�9X@�1@��@�o@��+@���@��D@���@�@��-@��-@�7L@��`@��@�@�-@��7@�`B@��`@�(�@��@�+@���@���@��\@��\@��\@��+@�v�@�^5@��h@��9@�r�@�A�@�b@���@�K�@�
=@���@��H@�ȴ@���@���@��+@�V@��/@���@�v�@�-@���@�G�@�%@���@���@�  @��@��+@�M�@��@��@��T@���@�@���@��h@�x�@�Q�@��P@�+@��y@��!@�5?@��@��#@���@�@�`B@�/@�%@��@��@�bN@�I�@�9X@�9X@�1'@�1'@�1@�@�P@l�@;d@
=@~�R@~v�@~$�@}�T@}�h@}/@|�j@{��@{33@z��@z��@zn�@zJ@y��@y7L@y7L@y%@w�@v{@u�T@u@u�-@u��@u�h@u�@u?}@t�/@tj@s��@s�@sdZ@sC�@s"�@s@r�H@r��@r�\@r=q@qX@pQ�@o��@o\)@o+@o�@o�@n�y@n�y@n�y@nȴ@n�R@n��@n�+@nff@n$�@m�T@m@m�h@m�@mO�@l�@lZ@k��@k�
@k�F@k��@k�@kS�@j��@jM�@i7L@hb@g�P@g+@f�y@f��@fv�@fE�@f5?@f5?@f5?@f5?@f5?@f$�@f$�@f@e�@c�@b��@b�\@b^5@bn�@b~�@b^5@bM�@b^5@bM�@b-@bJ@a��@a&�@`�u@`  @_�w@^�y@]V@\��@\�j@\�@\�D@\��@\I�@[�m@[t�@["�@Z��@Z�@Yhs@X��@XQ�@Xb@W�;@W��@W�w@WK�@Vȴ@VV@U�T@Up�@U�@T��@Tz�@TI�@T1@SC�@R��@Q�#@Q%@PA�@O�;@O+@NE�@M�T@M@M��@M�-@M�-@M��@M�@M��@M�h@Mp�@Mp�@M`B@L��@K�F@J�\@J-@I�@I��@Ihs@I�@H��@H �@G�@G�@Fȴ@Fv�@F{@E�T@D�j@C�F@CdZ@B��@B�\@B~�@BM�@B=q@A��@A�@@Ĝ@@��@@r�@@A�@@  @?��@?�w@?��@?�P@?�P@?��@?��@?��@?��@?�P@?|�@?\)@?\)@?+@?
=@>�y@>ȴ@>ȴ@>�R@>��@>ff@>$�@>$�@>{@=�@=��@=��@=�h@=�h@=��@=p�@=`B@<�/@<��@<�j@<Z@<1@;�m@;t�@;S�@;"�@:��@:-@:-@:�@:J@9��@9��@9��@9x�@9G�@9X@9&�@8A�@7��@7\)@7�@6�@6��@6�+@6V@65?@65?@65?@65?@6$�@6@5�@5�h@5/@4�@4Z@333@2�@1��@1�@0b@/\)@.�@.v�@.@-�@-?}@,�@,�j@+��@+t�@+dZ@+S�@+33@+33@+"�@+o@*�@*��@*�!@*��@*~�@*n�@*n�@*^5@*^5@*=q@*�@)�^@)x�@)x�@)hs@)X@)G�@(��@(��@(Ĝ@(�u@(r�@(1'@'��@&ȴ@%�@%V@$z�@$z�@$Z@$9X@$�@$1@$1@#��@#�m@#�
@#�
@#ƨ@#ƨ@#�F@#��@#t�@#S�@#C�@#"�@#o@#o@"�@"�H@"��@"��@"n�@"=q@"-@!��@!hs@ r�@  �@��@\)@�@��@�@��@v�@V@$�@{@��@@��@p�@?}@V@�@��@��@j@��@"�@o@�@��@^5@-@�@�^@hs@&�@&�@�@��@Ĝ@�u@�@bN@A�@b@��@��@�P@�P@|�@l�@K�@�@�@+@
=@��@��@�@ȴ@ȴ@ȴ@ȴ@�R@��@��@ff@E�@@�@�T@��@�-@�h@�@?}@��@��@�D@�D@Z@(�@�@��@�
@��@dZ@S�@33@@�H@�!@M�@��@��@�7@hs@&�@%@��@1'@�@�P@
=@ȴ@$�@@��@p�@O�@?}@?}@?}@?}@/@�/@��@9X@1@��@�m@ƨ@��@�@�@dZ@o@
�H@
�H@
�H@
��@
�\@
~�@
n�@
M�@
�@	�@	�@	��@	X@	7L@	7L@	G�@	G�@	7L@	G�@	7L@	�@	%@�`@�u@r�@Q�@Q�@A�@1'@b@�@�w@|�@;d@�@�y@�@�@�@�@�R@ȴ@��@ff@��@�-@�h@p�@O�@/@/@�@�@�/@�@z�@I�@(�@(�@(�@(�@�@��@��@��@�m@ƨ@��@dZ@33@o@�@��@~�@n�@^5@�@J@��@�^@��@x�@G�@G�@7L@%@ ��@ ��@ ��@ �@ A�@   ?��;?���?�\)?�;d?�;d1111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�\)A��A��HA���A��wA�bA��PA�$�A��A���A�n�A���A��RA�r�A��;A���A���A��A��
A��A�jA� �A��-A�dZA�JA�33A�7LG�O�G�O�A�x�A��uA��A�/A�E�A�7LA��A�ffA�jA��A�z�A��RA��A�hsA���A�ƨA�JA�/A��DA�
=A��\A�;dA� �A�33A�dZA���A�t�A��A���A�~�A��PA�A�A���A�XA���A�ffA�G�A��TA��RA�  A�\)A��A�G�A�K�A��!A�33A��;A�|�AA}O�A|$�Az�AzI�Ax�DAw/At�Ar�+Aq`BAn��Ak�FAi�-Ag"�Af�uAfJAe��Ae/Ac/Abz�AbA�AaAa�Aa?}A`5?A_�wA_33A^�A]`BA\ȴA\1'A[/AY�-AY�AXAV�AVJAT�!ATAS�ARbNAQ+AP1'AO�^AO%ANE�AL1'AJ��AI+AH5?AGO�AD��AC�FACS�AC?}ABv�AA�7A@��A@VA>{A<��A<  A;�PA:��A:��A:I�A9|�A9A8�`A8�\A7�#A7\)A7�A6��A6E�A6JA5��A4��A4��A3��A3�A2ȴA2�DA1��A1K�A0n�A/�A/�PA/;dA.�DA-p�A,�yA,��A+�-A*=qA)A)�-A)"�A&ȴA%p�A$v�A#`BA"z�A"=qA!`BA ��A Q�A �A�-Al�A�A�AƨA"�AQ�A7LA�RA\)A��A�AbNA9XA�TA�-A\)A�9AQ�A$�A`BAjA��A�!AjA�TA\)A��AffAffA^5A{A��A�A�HA��Av�A1'A��AXAoA��A�wA	S�AI�A�mAt�A��A%An�A(�A�wA�A�DA=qA��AK�A I�@��@��D@���@�x�@��9@�l�@�ȴ@��+@��@�~�@�/@�(�@���@�&�@�+@��@��@�+@��@�9X@�+@��@ߍP@��@�X@�C�@ى7@��@�o@�^5@ղ-@ԣ�@�b@ӍP@ҧ�@�@��@�hs@ϥ�@�C�@��@�ȴ@͡�@̋D@˝�@�C�@��y@�n�@ɩ�@�&�@��/@�9X@��@��H@�z�@��m@Ý�@�o@��7@�Z@��@�;d@��y@��\@�V@�S�@���@�/@��D@�ƨ@�33@���@��7@�
=@���@��@�j@�I�@�t�@�
=@���@�/@��/@���@��m@�33@�@�&�@��;@���@��+@��@�1'@��y@�$�@��#@�?}@���@�z�@�r�@�9X@�1@��@�o@��+@���@��D@���@�@��-@��-@�7L@��`@��@�@�-@��7@�`B@��`@�(�@��@�+@���@���@��\@��\@��\@��+@�v�@�^5@��h@��9@�r�@�A�@�b@���@�K�@�
=@���@��H@�ȴ@���@���@��+@�V@��/@���@�v�@�-@���@�G�@�%@���@���@�  @��@��+@�M�@��@��@��T@���@�@���@��h@�x�@�Q�@��P@�+@��y@��!@�5?@��@��#@���@�@�`B@�/@�%@��@��@�bN@�I�@�9X@�9X@�1'@�1'@�1@�@�P@l�@;d@
=@~�R@~v�@~$�@}�T@}�h@}/@|�j@{��@{33@z��@z��@zn�@zJ@y��@y7L@y7L@y%@w�@v{@u�T@u@u�-@u��@u�h@u�@u?}@t�/@tj@s��@s�@sdZ@sC�@s"�@s@r�H@r��@r�\@r=q@qX@pQ�@o��@o\)@o+@o�@o�@n�y@n�y@n�y@nȴ@n�R@n��@n�+@nff@n$�@m�T@m@m�h@m�@mO�@l�@lZ@k��@k�
@k�F@k��@k�@kS�@j��@jM�@i7L@hb@g�P@g+@f�y@f��@fv�@fE�@f5?@f5?@f5?@f5?@f5?@f$�@f$�@f@e�@c�@b��@b�\@b^5@bn�@b~�@b^5@bM�@b^5@bM�@b-@bJ@a��@a&�@`�u@`  @_�w@^�y@]V@\��@\�j@\�@\�D@\��@\I�@[�m@[t�@["�@Z��@Z�@Yhs@X��@XQ�@Xb@W�;@W��@W�w@WK�@Vȴ@VV@U�T@Up�@U�@T��@Tz�@TI�@T1@SC�@R��@Q�#@Q%@PA�@O�;@O+@NE�@M�T@M@M��@M�-@M�-@M��@M�@M��@M�h@Mp�@Mp�@M`B@L��@K�F@J�\@J-@I�@I��@Ihs@I�@H��@H �@G�@G�@Fȴ@Fv�@F{@E�T@D�j@C�F@CdZ@B��@B�\@B~�@BM�@B=q@A��@A�@@Ĝ@@��@@r�@@A�@@  @?��@?�w@?��@?�P@?�P@?��@?��@?��@?��@?�P@?|�@?\)@?\)@?+@?
=@>�y@>ȴ@>ȴ@>�R@>��@>ff@>$�@>$�@>{@=�@=��@=��@=�h@=�h@=��@=p�@=`B@<�/@<��@<�j@<Z@<1@;�m@;t�@;S�@;"�@:��@:-@:-@:�@:J@9��@9��@9��@9x�@9G�@9X@9&�@8A�@7��@7\)@7�@6�@6��@6�+@6V@65?@65?@65?@65?@6$�@6@5�@5�h@5/@4�@4Z@333@2�@1��@1�@0b@/\)@.�@.v�@.@-�@-?}@,�@,�j@+��@+t�@+dZ@+S�@+33@+33@+"�@+o@*�@*��@*�!@*��@*~�@*n�@*n�@*^5@*^5@*=q@*�@)�^@)x�@)x�@)hs@)X@)G�@(��@(��@(Ĝ@(�u@(r�@(1'@'��@&ȴ@%�@%V@$z�@$z�@$Z@$9X@$�@$1@$1@#��@#�m@#�
@#�
@#ƨ@#ƨ@#�F@#��@#t�@#S�@#C�@#"�@#o@#o@"�@"�H@"��@"��@"n�@"=q@"-@!��@!hs@ r�@  �@��@\)@�@��@�@��@v�@V@$�@{@��@@��@p�@?}@V@�@��@��@j@��@"�@o@�@��@^5@-@�@�^@hs@&�@&�@�@��@Ĝ@�u@�@bN@A�@b@��@��@�P@�P@|�@l�@K�@�@�@+@
=@��@��@�@ȴ@ȴ@ȴ@ȴ@�R@��@��@ff@E�@@�@�T@��@�-@�h@�@?}@��@��@�D@�D@Z@(�@�@��@�
@��@dZ@S�@33@@�H@�!@M�@��@��@�7@hs@&�@%@��@1'@�@�P@
=@ȴ@$�@@��@p�@O�@?}@?}@?}@?}@/@�/@��@9X@1@��@�m@ƨ@��@�@�@dZ@o@
�H@
�H@
�H@
��@
�\@
~�@
n�@
M�@
�@	�@	�@	��@	X@	7L@	7L@	G�@	G�@	7L@	G�@	7L@	�@	%@�`@�u@r�@Q�@Q�@A�@1'@b@�@�w@|�@;d@�@�y@�@�@�@�@�R@ȴ@��@ff@��@�-@�h@p�@O�@/@/@�@�@�/@�@z�@I�@(�@(�@(�@(�@�@��@��@��@�m@ƨ@��@dZ@33@o@�@��@~�@n�@^5@�@J@��@�^@��@x�@G�@G�@7L@%@ ��@ ��@ ��@ �@ A�@   ?��;?���?�\)?�;d?�;d1111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�ZB��B�B�XB��B�wB�}B�RB�dB�-B��BW
B_;BB�#B�B��B��BȴBȴBB�!B��B��B�+B,By�B�Bs�BjBQ�BG�BE�BA�B:^B��B��B1B�B	7B�B�ZB��B�B�oB�1B� BjBdZB?}B+B%�B49B2-B"�BPB
�B
�B
��B
�B
�B
�fB
�B
�'B
�FB
�B
�B
��B
~�B
�B
{�B
w�B
hsB
P�B
H�B
D�B
?}B
8RB
"�B
bB	��B	�fB	�mB	��B	��B	�9B	��B	��B	�}B	�jB	�-B	��B	��B	�B	��B	��B	��B	�{B	��B	�{B	�PB	�B	�+B	�B	s�B	k�B	n�B	jB	\)B	`BB	S�B	XB	VB	J�B	@�B	B�B	E�B	9XB	2-B	�B	hB	VB	PB		7B�B��B	
=B	VB	B��B��B�B��B��B�)B�NB�)B�TB�B�B�
B�;B�B��B��B��B��B��B��BƨB�}B�}B�RB�B�LB�?B�B��B��B��B�B��B��B�uB��B��B�JB|�B�=B�\B}�B_;BbNBn�BgmBk�Bu�BjBk�Bo�Br�Bn�Bo�BjBbNBYBW
BS�BM�BS�BI�BT�BaHB]/B^5BYBYBR�BJ�BO�BO�BB�B:^B@�B:^BI�BC�BD�BA�BE�BO�BL�BF�B@�B@�BE�BE�BC�B@�B;dB6FB5?B+B�B��B�B-B(�B�BhB%�B.B(�B#�B"�B'�B�B�BhBB�B�BVB�B�B�B�B
=B  BJBhB	7B�BB�BhBPB\BhBJBDB\BPB�B
=BVB�BVB�B�B�B �B#�B�B"�B'�B"�B�B&�B)�B%�B�B�B!�B,B+B(�B&�B(�B,B'�B&�B�BuB-B0!B,B �B+B1'B9XB7LB2-B'�B!�B-B5?B8RB7LB8RB5?B-B"�B7LBD�BE�BJ�BD�BH�BE�BH�BR�BQ�BL�BK�BG�BN�BM�BS�B^5BVBT�B\)BffBo�Bn�Bp�Bx�By�Bv�Bt�Br�Bq�Bp�Bm�BiyBm�B�B�oB�bB�=B�JB�1B�+B�hB��B��B��B��B��B��B�B�3B�?B�FB�?B�9B�3B�!B��B�B�dB�qB�qB�qB��BĜBȴBȴBȴBǮBȴBĜB�}B�?B�XB�BB�HB�TB�ZB�B�B�yB�`B�sB��B	  B	B	B	%B	+B	+B	%B	%B	B��B		7B	�B	�B	�B	�B	(�B	.B	/B	.B	,B	1'B	2-B	33B	9XB	;dB	>wB	@�B	A�B	A�B	A�B	@�B	A�B	E�B	E�B	E�B	G�B	G�B	I�B	K�B	M�B	O�B	P�B	R�B	R�B	\)B	^5B	aHB	`BB	`BB	aHB	cTB	ffB	dZB	aHB	ffB	y�B	z�B	{�B	|�B	{�B	{�B	{�B	{�B	|�B	~�B	�B	�%B	�+B	�1B	�7B	�=B	�7B	�1B	�1B	�+B	�DB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�!B	�B	�B	�B	�!B	�XB	�dB	�qB	�wB	��B	��B	B	ÖB	ÖB	ÖB	B	B	B	��B	�qB	�^B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�;B	�NB	�HB	�HB	�HB	�;B	�;B	�BB	�HB	�HB	�BB	�NB	�`B	�mB	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B
B
+B
+B
+B
+B
%B
+B
1B
1B
DB
DB
DB
DB
1B

=B
hB
hB
{B
�B
�B
�B
{B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
"�B
"�B
!�B
#�B
"�B
#�B
!�B
�B
 �B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
)�B
(�B
(�B
'�B
'�B
&�B
$�B
$�B
$�B
#�B
 �B
"�B
(�B
(�B
&�B
+B
.B
/B
0!B
0!B
2-B
2-B
33B
2-B
33B
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
9XB
9XB
8RB
8RB
7LB
9XB
:^B
:^B
:^B
9XB
8RB
:^B
:^B
:^B
9XB
8RB
6FB
33B
6FB
9XB
<jB
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
?}B
>wB
D�B
D�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
H�B
G�B
K�B
N�B
M�B
L�B
M�B
N�B
N�B
N�B
N�B
O�B
Q�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
T�B
T�B
T�B
S�B
T�B
T�B
T�B
VB
VB
VB
T�B
VB
S�B
S�B
VB
XB
XB
W
B
W
B
XB
XB
XB
XB
XB
YB
XB
XB
YB
XB
XB
XB
ZB
ZB
[#B
ZB
ZB
ZB
W
B
YB
\)B
ZB
\)B
\)B
]/B
`BB
`BB
aHB
aHB
bNB
bNB
aHB
aHB
_;B
_;B
`BB
bNB
cTB
cTB
bNB
cTB
dZB
dZB
dZB
cTB
dZB
e`B
e`B
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
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
iyB
jB
k�B
l�B
l�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
m�B
n�B
m�B
l�B
k�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
v�1111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B��B��B��B҉B�B�*B�B�B� B�rB�6B��B�yB_�Be`B��B�=B�yBӏBΊBɺBɆBðB�aB��B�G�O�G�O�B{�B��Bu�Bl�BT�BJ�BH�BC�B<PB-B�B
XB/B
�B��B�LB�YB��B�B��B��Bm]Bf�BC�B.�B(XB4�B2�B$B�B
��B
�B
��B
�B
�B
�B
�YB
��B
��B
��B
��B
��B
�UB
�aB
}"B
x�B
i�B
SuB
J�B
FB
@�B
9rB
$�B
oB
�B	�DB	�DB	� B	��B	��B	��B	�B	�B	�B	�MB	�B	��B	�iB	��B	�sB	�ZB	��B	�1B	�gB	�VB	��B	��B	��B	u%B	mCB	o�B	k�B	]�B	abB	U�B	X�B	V�B	LJB	B'B	C�B	FYB	:xB	3hB	7B	[B	HB	�B	
�B�B�B	
�B	�B	-B�B��B��BҽBԕB�B�B��B�B�	B��BרB�pBخB��BуB�aBбB�VB�VB�_B�iB�4B�>B�;B��B��B�B��B�B��B�CB��B��B��B�QB�EB��B~�B��B��BHBbBd&Bo�Bh�Bl�BvFBk�BlWBp;BsBo5BpBkBc BZQBXBU2BOBBT�BKxBU�Ba|B]�B^�BY�BYBS�BK�BPbBPHBC�B;�BA�B;�BJ#BDgBEmBB[BF?BO�BMBG+BAUBA;BFBF%BC�BAB<B6�B5�B+�BEB �B�B-�B)�B B&B&�B.}B)�B$�B#�B(sB�BVB�B�BeBEB�B/B_BB	BxB�B6B B
�B9B�B+B�BVBbB BjBJBbBVB
B�B\B)B�B5B \BjB!HB$ZB �B#TB($B#�B�B'8B*KB&LB�B �B"hB,qB+kB)yB'�B)yB,WB(�B'mB �BMB-]B0oB,�B!�B+�B1�B9�B7�B2�B)*B#:B-�B5�B8�B7�B8�B5�B./B$�B8BEBF%BJ�BESBIBFYBIRBS@BRTBMjBL~BH�BO�BN�BT�B^�BW
BVB]/Bf�Bo�BoBq'By	By�BwBuBsBr-BqABnIBj�Bn�B��B�TB��B��B��B��B�B�B�1B�B�IB�OB�ZB�XB�WB�MB�ZB�`B�ZB�TB�hB�oB��B��B�B��B��B��B��B��B��B��B��B��B��B��B� B��B��B�\B�B�B��B�B��B��B�2B�yB�B	 4B	GB	gB	?B	_B	_B	YB	YB	aB��B		�B	�B	�B	B	 'B	)*B	./B	/5B	./B	,qB	1AB	2aB	3�B	9rB	;�B	>�B	@�B	A�B	A�B	A�B	@�B	A�B	E�B	E�B	E�B	G�B	G�B	I�B	LB	NB	PB	QB	S&B	SuB	\CB	^jB	abB	`�B	`vB	a|B	c�B	ffB	d�B	a�B	gB	y�B	z�B	|B	}B	|B	|B	|B	|B	}<B	.B	�GB	�?B	�EB	�KB	�lB	�XB	�lB	�fB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�8B	�6B	�5B	�;B	�;B	�AB	�;B	�IB	�iB	��B	��B	��B	��B	��B	��B	��B	��B	ªB	ÖB	ÖB	ÖB	B	ªB	B	��B	��B	�0B	��B	� B	�B	�B	��B	�B	��B	��B	�B	�B	�B	� B	�.B	� B	�,B	�MB	�[B	ѝB	�;B	�NB	�bB	�bB	�HB	�pB	�pB	�vB	�bB	�|B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�%B	�B	�8B	�B	�B
 B
'B
B
-B
B
'B
-B
'B
'B
B
 B	�6B	�^B	�wB
9B
EB
EB
EB
EB
YB
_B
fB
fB
^B
xB
�B
xB
�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
"�B
"�B
!�B
$B
"�B
#�B
!�B
B
 �B
%B
$�B
&B
'B
($B
(
B
($B
)�B
(�B
(�B
($B
(
B
'B
%B
%,B
%,B
$&B
!-B
#:B
)*B
)*B
'RB
+kB
.IB
/OB
0UB
0UB
2GB
2aB
3MB
2|B
3hB
7LB
8RB
8lB
8lB
8RB
8lB
8lB
8lB
9XB
9XB
9�B
9XB
:^B
9XB
9XB
8lB
8lB
7�B
9rB
:^B
:^B
:^B
9�B
8lB
:^B
:xB
:�B
9rB
8lB
6zB
3�B
6�B
9�B
<�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
?�B
>�B
D�B
D�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
H�B
G�B
K�B
N�B
NB
L�B
M�B
OB
OB
N�B
N�B
O�B
RB
QB
Q B
Q B
Q B
Q�B
RB
R B
RB
RB
SB
S�B
S�B
S�B
TB
TB
TB
T�B
T�B
UB
T�B
UB
UB
UB
VB
VB
VB
U2B
UB
T�B
TB
UB
UB
T�B
VB
VB
VB
U2B
VB
T,B
T,B
VB
XB
XB
W$B
W$B
X+B
X+B
X+B
XEB
XEB
YB
X+B
X+B
Y1B
X+B
X+B
X+B
Z7B
Z7B
[=B
Z7B
Z7B
Z7B
WYB
YKB
\CB
ZQB
\CB
\xB
]IB
`BB
`\B
aHB
aHB
bhB
bhB
abB
abB
_VB
_pB
`\B
bhB
cTB
cTB
bhB
cnB
dZB
dZB
dtB
cnB
dtB
e`B
ezB
d�B
e`B
e`B
e`B
ezB
ezB
ezB
e`B
ezB
f�B
gmB
hsB
hsB
hsB
hsB
iyB
iyB
i�B
i�B
i�B
i�B
j�B
k�B
l�B
l�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
m�B
n�B
m�B
l�B
k�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
v�1111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.03(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806200038432018062000384320180620003843201806221243302018062212433020180622124330201806210032432018062100324320180621003243  JA  ARFMdecpA19c                                                                20180616093503  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180616003505  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180616003508  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180616003508  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180616003509  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180616003509  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180616003509  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180616003509  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180616003509  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180616003509  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180616003509  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180616003510                      G�O�G�O�G�O�                JA  ARUP                                                                        20180616005513                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180616153447  CV  JULD            G�O�G�O�F�U�                JM  ARCAJMQC2.0                                                                 20180619153843  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180619153843  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180620153243  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034330  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                