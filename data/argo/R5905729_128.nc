CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-10-28T09:01:13Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `p   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɸ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ͠   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20211028090113  20211028090113  5905729 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @ٞUm�(�1   @ٞV@)/��w�du�S���1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB	33B  B33B   B(ffB0  B8  B@ffBG33BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�B G�B	{B�HB{B�HB(G�B/�HB7�HB@G�BG{BOz�BW�HB_�HBg�HBo�HBw�HB�HB��B��B�#�B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DFzDF�zDF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK�zDLzDL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
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
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
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
D�;�D�
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
D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�G�A�E�A�I�A�A�A�A�A�E�A�I�A�Q�A�ZA�^5A�VA�S�A�n�A�r�A�=qA�/A�7LA�
=AҶFAѓuAΩ�A�+A���A�hsA�r�A��A�E�A�ZA���A�S�A���A�33A�bNA���A���A�dZA���A�+A��+A�C�A�1A� �A�VA��A�1A�ZA���A�33A���A��;A�1'A�G�A��#A�z�A�-A��Avz�Aq"�Al{Ah��Ad�HAd �Ad1'AdM�Ad-Aa�mA^ZA[��AX��AVQ�AU�FAUG�AS�AO�AM�AK�AKAJ�uAIS�AHr�AG"�AF~�AF{AEC�AB��AAoA@bNA?�;A>�A=�mA=l�A<�!A<-A;�A:��A8��A7�A6$�A4�A4�A3%A0�A0Q�A0A�A0  A/t�A/C�A.��A.�9A-��A-VA,n�A,ZA+�TA+G�A+
=A*��A*�+A*I�A*�A)��A)|�A)G�A)C�A)+A(��A(�`A(�/A(�A(�jA(�\A(JA'7LA'"�A'+A'&�A&�yA&�DA&VA&-A&1A%��A%x�A%O�A%�A%VA%%A$�A$�9A$E�A$$�A#�#A#��A#7LA"�`A"bNA"JA!�A!�;A!�A!|�A!hsA!O�A!�A ��A ��A  �A��A��AS�AVA�jAffAQ�AQ�A=qA�mA�FA��AhsA"�AȴA9XA��A�At�AVAȴA�RA��A�+A��Av�AbA��AdZA/A��A��AĜA�9A��A�+A�A�TA�PAO�A��A^5A�A�AXA5?A�7AO�AA�!A�\A^5A �A�wAXA�A�`AQ�A��A|�A7LA�A�A�uA�^An�AM�A1'A�A�A;dA
��A
ffA
bA	��A	G�A	7LA	XA	"�A��A��A�9A�+AE�A�TA��AC�A%A�HAĜA��AbNA�A�A33A��AȴA��A�uAv�A^5AA�A1'AA�A�A�A��A�\Av�AI�A�A��A�A�A jA 5?A �A 1A   @���@�C�@���@�n�@�$�@���@���@�X@���@��@��F@�C�@���@���@�/@���@�9X@���@��R@�E�@��T@���@�`B@��@���@��@�@�@��#@�&�@�j@��;@�dZ@�R@��T@��@�@��m@�"�@�+@��@陚@�7L@�Ĝ@�bN@�@��@�ff@�O�@�Q�@�o@�!@�^5@�@�hs@��/@߾w@�C�@�"�@��@�$�@�j@�K�@��@ى7@ى7@�G�@��`@ش9@�bN@��;@�l�@��@և+@�-@��@�`B@�Z@��@ӶF@�S�@��@ҸR@�5?@ѡ�@���@��m@�t�@�^5@ͺ^@�hs@�%@���@�Z@˥�@�l�@�S�@��@�=q@ɺ^@�G�@���@ȃ@�(�@��m@Ǯ@�K�@��@Ƨ�@�V@ź^@�%@�9X@å�@�S�@�"�@§�@�5?@��^@��h@��/@�b@�C�@�ȴ@��@���@��@�x�@�X@�/@��`@�z�@�ƨ@���@�33@��R@�^5@�J@��^@��h@�hs@�hs@�X@���@�(�@�t�@���@�~�@�=q@��@��-@�G�@��9@�Q�@��@���@�C�@�E�@�p�@��/@���@�r�@��@���@�C�@��y@��+@�5?@�p�@���@�z�@� �@���@��w@�"�@��R@�-@���@�/@��`@�bN@���@��y@�v�@�^5@�=q@�$�@�{@��^@��j@�z�@�Q�@�9X@�1'@��P@�"�@�"�@��R@�E�@�-@��@�J@��#@�hs@��@���@��D@�j@�b@��@�\)@�o@���@��+@�{@��#@��#@���@�@��7@��`@�j@�Q�@�Q�@�I�@�ƨ@�\)@�;d@�+@��y@��!@���@�ff@�{@��T@��7@�X@�&�@���@��`@�I�@���@�C�@���@���@�M�@��T@���@��/@�z�@�A�@���@�33@���@���@�v�@�=q@���@�O�@���@��9@��D@�bN@�(�@�b@��m@��w@�t�@�@��R@���@��+@�^5@�5?@��@���@�`B@�?}@�%@���@��D@�Q�@� �@��;@�33@��@�ȴ@���@�^5@��-@��@��@� �@���@�ƨ@�ƨ@��w@�dZ@��@��\@�M�@��@��#@��-@�p�@�?}@���@�Ĝ@���@�j@�  @��F@�|�@�+@���@��@��y@���@�n�@�{@�hs@�&�@���@���@�j@�I�@� �@�w@\)@~ȴ@}�T@}�h@|�@{�
@{�@{@z��@z=q@yhs@y&�@xb@w��@w+@v�@vv�@u�T@u��@u�-@u�-@u@u@uO�@u/@t�j@t1@s�@sdZ@sS�@sC�@s33@rn�@q��@q�^@q��@qx�@q7L@q%@pĜ@p�@pQ�@p1'@o�@o�@o|�@n�y@nv�@nE�@m�T@m�@m�@m�@mp�@m/@l�@l�@lZ@k�F@kS�@kC�@j��@j�\@j=q@jJ@i��@i�#@i��@i�@h�u@hb@g��@g��@g;d@fȴ@fE�@e�h@eO�@d�/@dz�@d1@cdZ@b^5@a7L@`Ĝ@`r�@`bN@` �@_+@^��@^$�@]�h@]V@\j@[t�@Z=q@Y�#@Y��@Yhs@Y�@X�`@Xr�@XQ�@XQ�@XQ�@XA�@W|�@W
=@V��@V@U�@T��@T�@T�@T�@Tz�@R�@R-@Q��@Q�#@Q��@Q�7@QX@Pr�@O�P@O;d@O
=@N��@N{@M��@M`B@M/@M�@L�@L�D@K��@J��@J=q@J�@I�^@I7L@H��@H�9@H1'@G��@G��@G;d@F��@F�y@Fȴ@F5?@E��@E�h@E�@E?}@DZ@C�
@Cƨ@C��@CS�@CC�@C33@C@B��@B��@BM�@BJ@A��@AG�@@�`@@Ĝ@@bN@?�@?�P@?+@>�y@>v�@>E�@=�@=�h@=�@<�/@<��@<9X@;��@;ƨ@;�@;@:��@:n�@:-@9�^@9G�@8�`@8Q�@8  @7��@7;d@6�@6�R@6v�@5�@5�@5�@4�@4�@4Z@4�@3��@3��@3o@2��@2�!@2^5@1��@1�^@1X@0��@0��@0��@0�u@0Q�@/�@/�w@/|�@/+@.��@.�@.��@.v�@.$�@-��@-�-@-�@-`B@-O�@-/@,�/@,�j@,Z@,(�@,�@+ƨ@+C�@*�@*�\@*�@)�@)�#@)��@)�^@)�7@)x�@)hs@)7L@)&�@)&�@)&�@)�@(Ĝ@(r�@(b@'�;@'�w@'�P@';d@&��@&ff@&$�@%�T@%��@%��@%�h@%�@%p�@%`B@%`B@%?}@%�@%V@$�D@#ƨ@#"�@"�@"~�@"-@!��@!�@!�@!�@!�#@!�^@!x�@!7L@!�@ �9@ A�@ A�@ 1'@��@��@�w@�@��@l�@l�@;d@
=@��@�+@5?@�T@��@�h@?}@V@�j@�D@(�@��@�m@ƨ@��@33@@��@~�@M�@�@��@x�@7L@�9@��@��@�9@�9@�9@��@�@b@�@��@�P@K�@;d@
=@�R@��@ff@E�@�@��@@��@O�@?}@?}@/@�@V@�@��@9X@�@1@�F@t�@dZ@S�@"�@�@�H@��@^5@�@�@��@hs@X@G�@&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�G�A�E�A�I�A�A�A�A�A�E�A�I�A�Q�A�ZA�^5A�VA�S�A�n�A�r�A�=qA�/A�7LA�
=AҶFAѓuAΩ�A�+A���A�hsA�r�A��A�E�A�ZA���A�S�A���A�33A�bNA���A���A�dZA���A�+A��+A�C�A�1A� �A�VA��A�1A�ZA���A�33A���A��;A�1'A�G�A��#A�z�A�-A��Avz�Aq"�Al{Ah��Ad�HAd �Ad1'AdM�Ad-Aa�mA^ZA[��AX��AVQ�AU�FAUG�AS�AO�AM�AK�AKAJ�uAIS�AHr�AG"�AF~�AF{AEC�AB��AAoA@bNA?�;A>�A=�mA=l�A<�!A<-A;�A:��A8��A7�A6$�A4�A4�A3%A0�A0Q�A0A�A0  A/t�A/C�A.��A.�9A-��A-VA,n�A,ZA+�TA+G�A+
=A*��A*�+A*I�A*�A)��A)|�A)G�A)C�A)+A(��A(�`A(�/A(�A(�jA(�\A(JA'7LA'"�A'+A'&�A&�yA&�DA&VA&-A&1A%��A%x�A%O�A%�A%VA%%A$�A$�9A$E�A$$�A#�#A#��A#7LA"�`A"bNA"JA!�A!�;A!�A!|�A!hsA!O�A!�A ��A ��A  �A��A��AS�AVA�jAffAQ�AQ�A=qA�mA�FA��AhsA"�AȴA9XA��A�At�AVAȴA�RA��A�+A��Av�AbA��AdZA/A��A��AĜA�9A��A�+A�A�TA�PAO�A��A^5A�A�AXA5?A�7AO�AA�!A�\A^5A �A�wAXA�A�`AQ�A��A|�A7LA�A�A�uA�^An�AM�A1'A�A�A;dA
��A
ffA
bA	��A	G�A	7LA	XA	"�A��A��A�9A�+AE�A�TA��AC�A%A�HAĜA��AbNA�A�A33A��AȴA��A�uAv�A^5AA�A1'AA�A�A�A��A�\Av�AI�A�A��A�A�A jA 5?A �A 1A   @���@�C�@���@�n�@�$�@���@���@�X@���@��@��F@�C�@���@���@�/@���@�9X@���@��R@�E�@��T@���@�`B@��@���@��@�@�@��#@�&�@�j@��;@�dZ@�R@��T@��@�@��m@�"�@�+@��@陚@�7L@�Ĝ@�bN@�@��@�ff@�O�@�Q�@�o@�!@�^5@�@�hs@��/@߾w@�C�@�"�@��@�$�@�j@�K�@��@ى7@ى7@�G�@��`@ش9@�bN@��;@�l�@��@և+@�-@��@�`B@�Z@��@ӶF@�S�@��@ҸR@�5?@ѡ�@���@��m@�t�@�^5@ͺ^@�hs@�%@���@�Z@˥�@�l�@�S�@��@�=q@ɺ^@�G�@���@ȃ@�(�@��m@Ǯ@�K�@��@Ƨ�@�V@ź^@�%@�9X@å�@�S�@�"�@§�@�5?@��^@��h@��/@�b@�C�@�ȴ@��@���@��@�x�@�X@�/@��`@�z�@�ƨ@���@�33@��R@�^5@�J@��^@��h@�hs@�hs@�X@���@�(�@�t�@���@�~�@�=q@��@��-@�G�@��9@�Q�@��@���@�C�@�E�@�p�@��/@���@�r�@��@���@�C�@��y@��+@�5?@�p�@���@�z�@� �@���@��w@�"�@��R@�-@���@�/@��`@�bN@���@��y@�v�@�^5@�=q@�$�@�{@��^@��j@�z�@�Q�@�9X@�1'@��P@�"�@�"�@��R@�E�@�-@��@�J@��#@�hs@��@���@��D@�j@�b@��@�\)@�o@���@��+@�{@��#@��#@���@�@��7@��`@�j@�Q�@�Q�@�I�@�ƨ@�\)@�;d@�+@��y@��!@���@�ff@�{@��T@��7@�X@�&�@���@��`@�I�@���@�C�@���@���@�M�@��T@���@��/@�z�@�A�@���@�33@���@���@�v�@�=q@���@�O�@���@��9@��D@�bN@�(�@�b@��m@��w@�t�@�@��R@���@��+@�^5@�5?@��@���@�`B@�?}@�%@���@��D@�Q�@� �@��;@�33@��@�ȴ@���@�^5@��-@��@��@� �@���@�ƨ@�ƨ@��w@�dZ@��@��\@�M�@��@��#@��-@�p�@�?}@���@�Ĝ@���@�j@�  @��F@�|�@�+@���@��@��y@���@�n�@�{@�hs@�&�@���@���@�j@�I�@� �@�w@\)@~ȴ@}�T@}�h@|�@{�
@{�@{@z��@z=q@yhs@y&�@xb@w��@w+@v�@vv�@u�T@u��@u�-@u�-@u@u@uO�@u/@t�j@t1@s�@sdZ@sS�@sC�@s33@rn�@q��@q�^@q��@qx�@q7L@q%@pĜ@p�@pQ�@p1'@o�@o�@o|�@n�y@nv�@nE�@m�T@m�@m�@m�@mp�@m/@l�@l�@lZ@k�F@kS�@kC�@j��@j�\@j=q@jJ@i��@i�#@i��@i�@h�u@hb@g��@g��@g;d@fȴ@fE�@e�h@eO�@d�/@dz�@d1@cdZ@b^5@a7L@`Ĝ@`r�@`bN@` �@_+@^��@^$�@]�h@]V@\j@[t�@Z=q@Y�#@Y��@Yhs@Y�@X�`@Xr�@XQ�@XQ�@XQ�@XA�@W|�@W
=@V��@V@U�@T��@T�@T�@T�@Tz�@R�@R-@Q��@Q�#@Q��@Q�7@QX@Pr�@O�P@O;d@O
=@N��@N{@M��@M`B@M/@M�@L�@L�D@K��@J��@J=q@J�@I�^@I7L@H��@H�9@H1'@G��@G��@G;d@F��@F�y@Fȴ@F5?@E��@E�h@E�@E?}@DZ@C�
@Cƨ@C��@CS�@CC�@C33@C@B��@B��@BM�@BJ@A��@AG�@@�`@@Ĝ@@bN@?�@?�P@?+@>�y@>v�@>E�@=�@=�h@=�@<�/@<��@<9X@;��@;ƨ@;�@;@:��@:n�@:-@9�^@9G�@8�`@8Q�@8  @7��@7;d@6�@6�R@6v�@5�@5�@5�@4�@4�@4Z@4�@3��@3��@3o@2��@2�!@2^5@1��@1�^@1X@0��@0��@0��@0�u@0Q�@/�@/�w@/|�@/+@.��@.�@.��@.v�@.$�@-��@-�-@-�@-`B@-O�@-/@,�/@,�j@,Z@,(�@,�@+ƨ@+C�@*�@*�\@*�@)�@)�#@)��@)�^@)�7@)x�@)hs@)7L@)&�@)&�@)&�@)�@(Ĝ@(r�@(b@'�;@'�w@'�P@';d@&��@&ff@&$�@%�T@%��@%��@%�h@%�@%p�@%`B@%`B@%?}@%�@%V@$�D@#ƨ@#"�@"�@"~�@"-@!��@!�@!�@!�@!�#@!�^@!x�@!7L@!�@ �9@ A�@ A�@ 1'@��@��@�w@�@��@l�@l�@;d@
=@��@�+@5?@�T@��@�h@?}@V@�j@�D@(�@��@�m@ƨ@��@33@@��@~�@M�@�@��@x�@7L@�9@��@��@�9@�9@�9@��@�@b@�@��@�P@K�@;d@
=@�R@��@ff@E�@�@��@@��@O�@?}@?}@/@�@V@�@��@9X@�@1@�F@t�@dZ@S�@"�@�@�H@��@^5@�@�@��@hs@X@G�@&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbBhBhBhBbBbBoBuB�B�B�B�B�B0!B;dBXB	�B	�dB
5?B
q�B
� B
�JB
{�B
�=B
��B
�}B
ȴB
��B�B�B-BH�BC�B�{B�DB�hBr�BQ�BJ�B,B"�B�B{BDB  B
�yB
ÖB
�}B
�XB
��B
ZB
Q�B
R�B
O�B
E�B
2-B	�sB	ŢB	��B	�VB	�%B	�hB	��B	��B	�uB	�%B	�B	�uB	��B	��B	��B	��B	�FB	�3B	�B
PB
;dB
K�B
T�B
p�B
�B
��B
��B
��B
��B
�qB
��B
�#B
�B
�BB
�fB
�sB
�B
�B
�NB
��B
��B
��B
��B
��B
��B
�jB
�dB
�qB
�dB
�^B
�wB
�}B
��B
�qB
��B
ƨB
��B
ɺB
ǮB
��B
ɺB
��B
ɺB
ɺB
ȴB
ȴB
ɺB
��B
��B
ɺB
��B
��B
��B
ȴB
ƨB
ÖB
B
ɺB
��B
ȴB
ƨB
ÖB
ĜB
ÖB
ÖB
��B
��B
B
B
B
B
��B
�}B
�qB
��B
�}B
�wB
�wB
�qB
�qB
�}B
�wB
�wB
�jB
�^B
�XB
�LB
�?B
�3B
�3B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�oB
�\B
�DB
�%B
z�B
x�B
y�B
w�B
v�B
w�B
w�B
w�B
u�B
t�B
t�B
r�B
o�B
n�B
n�B
m�B
m�B
k�B
hsB
cTB
[#B
bNB
bNB
aHB
^5B
\)B
[#B
XB
XB
VB
VB
XB
[#B
\)B
\)B
\)B
[#B
ZB
XB
VB
T�B
S�B
S�B
T�B
S�B
R�B
O�B
N�B
M�B
N�B
N�B
N�B
O�B
O�B
N�B
N�B
N�B
M�B
K�B
H�B
G�B
I�B
H�B
I�B
H�B
F�B
E�B
E�B
E�B
B�B
?}B
C�B
E�B
D�B
D�B
B�B
B�B
A�B
A�B
B�B
A�B
A�B
@�B
>wB
;dB
=qB
;dB
:^B
6FB
:^B
9XB
8RB
7LB
5?B
7LB
8RB
8RB
7LB
7LB
5?B
33B
2-B
0!B
0!B
0!B
0!B
.B
.B
-B
,B
+B
,B
)�B
)�B
+B
,B
+B
+B
)�B
(�B
&�B
&�B
%�B
"�B
"�B
"�B
'�B
'�B
&�B
$�B
$�B
"�B
$�B
&�B
$�B
�B
�B
�B
�B
�B
#�B
"�B
 �B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
!�B
�B
�B
!�B
#�B
"�B
"�B
�B
 �B
#�B
"�B
!�B
$�B
%�B
$�B
"�B
!�B
 �B
#�B
$�B
$�B
"�B
#�B
$�B
$�B
%�B
%�B
%�B
'�B
)�B
)�B
(�B
&�B
$�B
%�B
)�B
)�B
(�B
&�B
'�B
)�B
+B
)�B
)�B
+B
+B
)�B
,B
+B
,B
,B
,B
+B
(�B
&�B
(�B
)�B
+B
)�B
)�B
+B
'�B
+B
,B
+B
)�B
-B
/B
0!B
0!B
0!B
/B
/B
2-B
33B
33B
33B
49B
49B
33B
2-B
1'B
33B
49B
5?B
49B
49B
49B
49B
49B
5?B
5?B
49B
7LB
6FB
6FB
5?B
49B
7LB
8RB
8RB
8RB
5?B
7LB
8RB
9XB
;dB
>wB
>wB
=qB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
>wB
>wB
>wB
>wB
?}B
A�B
@�B
?}B
@�B
>wB
=qB
A�B
A�B
A�B
B�B
C�B
E�B
D�B
D�B
D�B
C�B
D�B
A�B
E�B
D�B
D�B
D�B
D�B
C�B
E�B
C�B
F�B
G�B
G�B
H�B
H�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
I�B
I�B
J�B
L�B
L�B
L�B
K�B
J�B
K�B
L�B
M�B
M�B
L�B
M�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
L�B
L�B
N�B
N�B
N�B
O�B
P�B
O�B
O�B
O�B
O�B
N�B
N�B
O�B
P�B
O�B
P�B
Q�B
R�B
R�B
S�B
S�B
R�B
S�B
R�B
S�B
S�B
S�B
R�B
S�B
R�B
T�B
T�B
T�B
S�B
R�B
R�B
Q�B
T�B
T�B
VB
VB
T�B
W
B
W
B
W
B
XB
XB
W
B
W
B
ZB
[#B
[#B
\)B
\)B
[#B
]/B
]/B
]/B
\)B
ZB
[#B
\)B
[#B
ZB
]/B
^5B
]/B
\)B
[#B
YB
[#B
^5B
^5B
^5B
^5B
]/B
[#B
[#B
^5B
_;B
_;B
_;B
aHB
`BB
aHB
aHB
aHB
_;B
_;B
^5B
aHB
cTB
bNB
bNB
cTB
dZB
cTB
cTB
dZB
dZB
e`B
e`B
dZB
cTB
dZB
dZB
e`B
dZB
cTB
e`B
gmB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
jB
iyB
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
jB
k�B
l�B
l�B
k�B
k�B
l�B
l�B
m�B
m�B
n�B
o�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
q�B
q�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
u�B
t�B
t�B
u�B
u�B
t�B
u�B
v�B
u�B
v�B
v�B
v�B
x�B
w�B
x�B
x�B
w�B
w�B
w�B
w�B
x�B
x�B
w�B
w�B
x�B
x�B
y�B
z�B
{�B
{�B
{�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
z�B
z�B
z�B
{�B
{�B
|�B
{�B
{�B
{�B
|�B
}�B
}�B
� B
� B
� B
� B
� B
~�B
~�B
~�B
}�B
|�B
{�B
}�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�%B
�B
�%B
�B
�%B
�%B
�%B
�%B
�B
�%B
�%B
�%B
�%B
�B
�B
�%B
�%B
�B
�+B
�1B
�7B
�7B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�PB
�PB
�PB
�PB
�JB
�JB
�JB
�JB
�PB
�PB
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
�\B
�\B
�\B
�bB
�hB
�hB
�h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BbBhBhBhBbBbBoBuB�B�B�B�B�B0!B;dBXB	�B	�dB
5?B
q�B
� B
�JB
{�B
�=B
��B
�}B
ȴB
��B�B�B-BH�BC�B�{B�DB�hBr�BQ�BJ�B,B"�B�B{BDB  B
�yB
ÖB
�}B
�XB
��B
ZB
Q�B
R�B
O�B
E�B
2-B	�sB	ŢB	��B	�VB	�%B	�hB	��B	��B	�uB	�%B	�B	�uB	��B	��B	��B	��B	�FB	�3B	�B
PB
;dB
K�B
T�B
p�B
�B
��B
��B
��B
��B
�qB
��B
�#B
�B
�BB
�fB
�sB
�B
�B
�NB
��B
��B
��B
��B
��B
��B
�jB
�dB
�qB
�dB
�^B
�wB
�}B
��B
�qB
��B
ƨB
��B
ɺB
ǮB
��B
ɺB
��B
ɺB
ɺB
ȴB
ȴB
ɺB
��B
��B
ɺB
��B
��B
��B
ȴB
ƨB
ÖB
B
ɺB
��B
ȴB
ƨB
ÖB
ĜB
ÖB
ÖB
��B
��B
B
B
B
B
��B
�}B
�qB
��B
�}B
�wB
�wB
�qB
�qB
�}B
�wB
�wB
�jB
�^B
�XB
�LB
�?B
�3B
�3B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�oB
�\B
�DB
�%B
z�B
x�B
y�B
w�B
v�B
w�B
w�B
w�B
u�B
t�B
t�B
r�B
o�B
n�B
n�B
m�B
m�B
k�B
hsB
cTB
[#B
bNB
bNB
aHB
^5B
\)B
[#B
XB
XB
VB
VB
XB
[#B
\)B
\)B
\)B
[#B
ZB
XB
VB
T�B
S�B
S�B
T�B
S�B
R�B
O�B
N�B
M�B
N�B
N�B
N�B
O�B
O�B
N�B
N�B
N�B
M�B
K�B
H�B
G�B
I�B
H�B
I�B
H�B
F�B
E�B
E�B
E�B
B�B
?}B
C�B
E�B
D�B
D�B
B�B
B�B
A�B
A�B
B�B
A�B
A�B
@�B
>wB
;dB
=qB
;dB
:^B
6FB
:^B
9XB
8RB
7LB
5?B
7LB
8RB
8RB
7LB
7LB
5?B
33B
2-B
0!B
0!B
0!B
0!B
.B
.B
-B
,B
+B
,B
)�B
)�B
+B
,B
+B
+B
)�B
(�B
&�B
&�B
%�B
"�B
"�B
"�B
'�B
'�B
&�B
$�B
$�B
"�B
$�B
&�B
$�B
�B
�B
�B
�B
�B
#�B
"�B
 �B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
!�B
�B
�B
!�B
#�B
"�B
"�B
�B
 �B
#�B
"�B
!�B
$�B
%�B
$�B
"�B
!�B
 �B
#�B
$�B
$�B
"�B
#�B
$�B
$�B
%�B
%�B
%�B
'�B
)�B
)�B
(�B
&�B
$�B
%�B
)�B
)�B
(�B
&�B
'�B
)�B
+B
)�B
)�B
+B
+B
)�B
,B
+B
,B
,B
,B
+B
(�B
&�B
(�B
)�B
+B
)�B
)�B
+B
'�B
+B
,B
+B
)�B
-B
/B
0!B
0!B
0!B
/B
/B
2-B
33B
33B
33B
49B
49B
33B
2-B
1'B
33B
49B
5?B
49B
49B
49B
49B
49B
5?B
5?B
49B
7LB
6FB
6FB
5?B
49B
7LB
8RB
8RB
8RB
5?B
7LB
8RB
9XB
;dB
>wB
>wB
=qB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
>wB
>wB
>wB
>wB
?}B
A�B
@�B
?}B
@�B
>wB
=qB
A�B
A�B
A�B
B�B
C�B
E�B
D�B
D�B
D�B
C�B
D�B
A�B
E�B
D�B
D�B
D�B
D�B
C�B
E�B
C�B
F�B
G�B
G�B
H�B
H�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
I�B
I�B
J�B
L�B
L�B
L�B
K�B
J�B
K�B
L�B
M�B
M�B
L�B
M�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
L�B
L�B
N�B
N�B
N�B
O�B
P�B
O�B
O�B
O�B
O�B
N�B
N�B
O�B
P�B
O�B
P�B
Q�B
R�B
R�B
S�B
S�B
R�B
S�B
R�B
S�B
S�B
S�B
R�B
S�B
R�B
T�B
T�B
T�B
S�B
R�B
R�B
Q�B
T�B
T�B
VB
VB
T�B
W
B
W
B
W
B
XB
XB
W
B
W
B
ZB
[#B
[#B
\)B
\)B
[#B
]/B
]/B
]/B
\)B
ZB
[#B
\)B
[#B
ZB
]/B
^5B
]/B
\)B
[#B
YB
[#B
^5B
^5B
^5B
^5B
]/B
[#B
[#B
^5B
_;B
_;B
_;B
aHB
`BB
aHB
aHB
aHB
_;B
_;B
^5B
aHB
cTB
bNB
bNB
cTB
dZB
cTB
cTB
dZB
dZB
e`B
e`B
dZB
cTB
dZB
dZB
e`B
dZB
cTB
e`B
gmB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
jB
iyB
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
jB
k�B
l�B
l�B
k�B
k�B
l�B
l�B
m�B
m�B
n�B
o�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
q�B
q�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
u�B
t�B
t�B
u�B
u�B
t�B
u�B
v�B
u�B
v�B
v�B
v�B
x�B
w�B
x�B
x�B
w�B
w�B
w�B
w�B
x�B
x�B
w�B
w�B
x�B
x�B
y�B
z�B
{�B
{�B
{�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
z�B
z�B
z�B
{�B
{�B
|�B
{�B
{�B
{�B
|�B
}�B
}�B
� B
� B
� B
� B
� B
~�B
~�B
~�B
}�B
|�B
{�B
}�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�%B
�B
�%B
�B
�%B
�%B
�%B
�%B
�B
�%B
�%B
�%B
�%B
�B
�B
�%B
�%B
�B
�+B
�1B
�7B
�7B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�PB
�PB
�PB
�PB
�JB
�JB
�JB
�JB
�PB
�PB
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
�\B
�\B
�\B
�bB
�hB
�hB
�h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211028090113                              AO  ARCAADJP                                                                    20211028090113    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211028090113  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211028090113  QCF$                G�O�G�O�G�O�0               