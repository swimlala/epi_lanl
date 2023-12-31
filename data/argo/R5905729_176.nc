CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-02-20T10:01:38Z creation      
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
resolution        =���   axis      Z        L  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  n�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230220100138  20230220100138  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�V;���1   @�V��@*T��E��d��;dZ1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D��fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��>B��>B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC$�C%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC`�Cb�Cc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCz�C{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC��DDw�DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DXw�DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D���D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D�B=D�
D��pD��p1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�AӃAӁAӁAӅAӝ�Aӣ�Aӝ�Aӗ�AӓuA�|�A�^5A�Q�A�A�A�1'A�oA���A��A��A��yA��`A��`A��HA��/A��/A��/A���A���AҼjA�~�A�C�A��A�A�G�A΋DA�K�A�G�A��;A��
A�/A��hA�G�A�~�A�jA��A�ĜA�A�K�A��wA�"�A�A�A�=qA�hsA��wA�oA�`BA�
=A�&�A�I�A�ffA�A��A���A�bNA�A�5?A�~�A���A��yA�VA��uA�/A�Q�A�n�A���A���A�l�A�-A���A�bA~�!AzJAu33Am&�Ai+Adn�A^��A\VAZĜAVĜAPĜAN��AL�AK�AH�jAF~�ACVAA%A@�A?��A=�wA=
=A;33A:��A:�DA933A8VA7�7A7?}A6~�A4M�A3��A2-A1��A1K�A0�A0I�A01A/��A05?A/�mA/G�A.��A,��A, �A+7LA)�wA(jA'VA%�
A%A$��A$ �A#�A#G�A!��A!��A!A ȴA   A�/An�A(�A  A��A;dA�yA�uA�Az�A^5A��A/A��A9XAƨA��A�A��AA�A�
A�-A�A�A�A�!AE�A�TA��A/AoA��AZAbA�;AdZA��A��Ar�A��A7LA�uAQ�A-A�#A�^A��At�AdZA\)AXAS�AK�AG�A�A�jAffA5?A�A1A�A&�A��AVA�TA7LA
�A	��A	x�AȴAbA�wA�7At�AO�A/AoAȴAZA�A�
A�-A�7Ap�AdZA\)AC�AA��A7LAVA��AjA(�A�AbA  AƨA\)A33A ��A ��A ffA @��y@�S�@��@��@�@��h@�`B@�O�@���@�(�@��m@��w@�
=@��#@���@���@�@�9X@���@�t�@��@�5?@�?}@�j@@�33@�@�+@�@���@�Z@��;@�ƨ@�C�@��@ꗍ@�$�@��/@��m@�\@��@�hs@�X@���@� �@㕁@�5?@���@��@ޗ�@��@���@ܴ9@ܣ�@��m@ڧ�@ٺ^@�ƨ@�G�@�7L@�/@��@��@���@ԓu@ԃ@�A�@ӕ�@���@���@ѩ�@���@ϥ�@��@Ο�@ͺ^@�/@�%@̼j@̃@��@�dZ@ʧ�@�9X@ǶF@ǅ@�C�@�o@���@�~�@�-@��@��#@�@ř�@�hs@���@�Q�@�ƨ@�S�@�@¸R@+@�E�@�{@���@��7@��@���@�9X@��;@��P@��@��R@��!@���@�~�@�n�@�E�@�J@��#@��h@� �@��P@�C�@�@��H@���@��#@���@��@�1'@���@�C�@�;d@�;d@�;d@�
=@�M�@�{@���@�9X@��@�@��H@���@�^5@���@�&�@���@�I�@��@��P@�+@���@�V@�$�@��@���@�A�@��m@�ƨ@��@��y@���@�J@��h@���@���@��D@�z�@�Q�@��@��@�\)@�K�@��H@�-@��^@���@�/@��@��`@��u@�A�@�b@��;@�ƨ@���@�\)@�o@���@�$�@��^@�hs@�%@��u@���@�S�@���@�E�@�$�@���@���@���@���@�x�@�O�@�?}@�&�@��`@���@�Ĝ@��@�1'@�l�@���@��@��^@��@���@���@�j@�(�@�l�@�ff@��-@�`B@��D@��@��
@��w@���@�dZ@�\)@�;d@�+@�o@�
=@���@��#@�/@�r�@��m@�C�@��y@�5?@�?}@���@�1'@�ƨ@�"�@��!@�=q@���@�&�@��9@�I�@��m@��w@�l�@�o@��y@��\@�M�@�=q@�=q@�$�@�{@�@���@��@��T@���@��^@�p�@���@��@�z�@�Q�@�Q�@�Q�@�I�@�9X@�1'@��;@�C�@�o@��@��R@��!@���@�~�@�~�@�~�@�v�@�E�@���@��-@�`B@��`@��/@���@�Ĝ@��@���@��@�j@�I�@� �@�;@l�@+@~��@~�y@~��@}��@}/@|�@|Z@{��@{C�@{o@z�@z��@z^5@z^5@z^5@z=q@zJ@y�^@y��@y�7@yx�@yhs@y�@xbN@w�w@wK�@v��@v$�@u@u��@up�@t�@tj@t1@sƨ@s�@sdZ@s33@so@s@r��@r�\@r=q@r-@q�@q�#@q��@qhs@p�`@pr�@pQ�@pQ�@pA�@p1'@p1'@p1'@p �@pb@o��@o+@n�@nȴ@nȴ@n�R@n�R@n�R@n��@n{@m�-@mV@l��@l��@lI�@k�@kdZ@k"�@k@j��@j�\@j=q@j-@jJ@i��@i&�@h�9@hr�@hb@g�@g+@f�+@e��@ep�@d��@d1@c�
@cƨ@c�F@c��@c�@cS�@c@b��@b��@b�\@b~�@bJ@aX@a&�@`�`@`1'@_�;@^�y@^�+@]�-@]V@\�j@\j@[ƨ@[�@[C�@["�@Z��@Y��@Y%@X��@XĜ@W�;@V��@VE�@U�-@UO�@T9X@Sƨ@St�@SC�@S@R��@R-@Q��@Qx�@PĜ@PQ�@Pb@O�;@O\)@N��@N�R@N{@M�T@MO�@M/@L�@K�m@J�@Jn�@J=q@J-@I��@I�^@HĜ@H �@G�@G\)@FE�@E?}@D�/@D�@D(�@C33@B�H@B�!@B=q@A�^@A�7@A&�@@��@@��@@�9@@��@@�@@bN@@1'@@  @@  @?�@?�w@?|�@?l�@?\)@?K�@>�@>V@>@=/@<�j@<�@;��@;dZ@;C�@;"�@;o@:~�@:-@9&�@8Ĝ@8Q�@7�;@7�w@7\)@7+@6��@6ȴ@6ȴ@6ȴ@6ȴ@6��@6ff@65?@6@5�@5��@5@5�@5`B@5O�@4��@4�@4z�@49X@3�
@3S�@3o@2��@2n�@2=q@2-@2J@1��@1��@17L@0��@0�@0 �@/�@/�;@/��@/��@/��@/l�@/�@.�y@.�@.��@.��@.��@.�+@.v�@.$�@-��@-�@-?}@-/@,�@,��@,z�@,I�@,(�@,(�@,�@,�@+�
@+dZ@*�H@*=q@)��@)��@)hs@)G�@)G�@)7L@(��@(�u@(Q�@( �@'�w@'\)@'�@&��@&��@&5?@%@%p�@%?}@%/@$�/@$�@#�
@#�F@#��@#�@#�@#t�@#S�@#33@"�@"��@"�\@"n�@"=q@!�@!��@!��@!hs@!X@!�@ ��@ �9@ �u@ �@  �@�P@+@�@�R@��@��@��@��@��@v�@$�@�T@O�@��@�j@�@�D@Z@9X@�m@t�@�@��@�\@-@�#@��@&�@%@%@�`@�9@�@�;@�w@�w@�@�P@l�@ȴ@ff@E�@E�@{@@�-@�-@�-@p�@�@Z@9X@��@��@t�@C�@��@��@�!@�\@=q@��@X@&�@%@��@�`@Ĝ@�9@�9@�@  @��@|�@l�@\)@K�@�@�@�R@��@v�@ff@{@p�@/@/@/@�@��@�@�@�@�/@�/@�j@�@�@�@��@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�z�AӃAӁAӁAӅAӝ�Aӣ�Aӝ�Aӗ�AӓuA�|�A�^5A�Q�A�A�A�1'A�oA���A��A��A��yA��`A��`A��HA��/A��/A��/A���A���AҼjA�~�A�C�A��A�A�G�A΋DA�K�A�G�A��;A��
A�/A��hA�G�A�~�A�jA��A�ĜA�A�K�A��wA�"�A�A�A�=qA�hsA��wA�oA�`BA�
=A�&�A�I�A�ffA�A��A���A�bNA�A�5?A�~�A���A��yA�VA��uA�/A�Q�A�n�A���A���A�l�A�-A���A�bA~�!AzJAu33Am&�Ai+Adn�A^��A\VAZĜAVĜAPĜAN��AL�AK�AH�jAF~�ACVAA%A@�A?��A=�wA=
=A;33A:��A:�DA933A8VA7�7A7?}A6~�A4M�A3��A2-A1��A1K�A0�A0I�A01A/��A05?A/�mA/G�A.��A,��A, �A+7LA)�wA(jA'VA%�
A%A$��A$ �A#�A#G�A!��A!��A!A ȴA   A�/An�A(�A  A��A;dA�yA�uA�Az�A^5A��A/A��A9XAƨA��A�A��AA�A�
A�-A�A�A�A�!AE�A�TA��A/AoA��AZAbA�;AdZA��A��Ar�A��A7LA�uAQ�A-A�#A�^A��At�AdZA\)AXAS�AK�AG�A�A�jAffA5?A�A1A�A&�A��AVA�TA7LA
�A	��A	x�AȴAbA�wA�7At�AO�A/AoAȴAZA�A�
A�-A�7Ap�AdZA\)AC�AA��A7LAVA��AjA(�A�AbA  AƨA\)A33A ��A ��A ffA @��y@�S�@��@��@�@��h@�`B@�O�@���@�(�@��m@��w@�
=@��#@���@���@�@�9X@���@�t�@��@�5?@�?}@�j@@�33@�@�+@�@���@�Z@��;@�ƨ@�C�@��@ꗍ@�$�@��/@��m@�\@��@�hs@�X@���@� �@㕁@�5?@���@��@ޗ�@��@���@ܴ9@ܣ�@��m@ڧ�@ٺ^@�ƨ@�G�@�7L@�/@��@��@���@ԓu@ԃ@�A�@ӕ�@���@���@ѩ�@���@ϥ�@��@Ο�@ͺ^@�/@�%@̼j@̃@��@�dZ@ʧ�@�9X@ǶF@ǅ@�C�@�o@���@�~�@�-@��@��#@�@ř�@�hs@���@�Q�@�ƨ@�S�@�@¸R@+@�E�@�{@���@��7@��@���@�9X@��;@��P@��@��R@��!@���@�~�@�n�@�E�@�J@��#@��h@� �@��P@�C�@�@��H@���@��#@���@��@�1'@���@�C�@�;d@�;d@�;d@�
=@�M�@�{@���@�9X@��@�@��H@���@�^5@���@�&�@���@�I�@��@��P@�+@���@�V@�$�@��@���@�A�@��m@�ƨ@��@��y@���@�J@��h@���@���@��D@�z�@�Q�@��@��@�\)@�K�@��H@�-@��^@���@�/@��@��`@��u@�A�@�b@��;@�ƨ@���@�\)@�o@���@�$�@��^@�hs@�%@��u@���@�S�@���@�E�@�$�@���@���@���@���@�x�@�O�@�?}@�&�@��`@���@�Ĝ@��@�1'@�l�@���@��@��^@��@���@���@�j@�(�@�l�@�ff@��-@�`B@��D@��@��
@��w@���@�dZ@�\)@�;d@�+@�o@�
=@���@��#@�/@�r�@��m@�C�@��y@�5?@�?}@���@�1'@�ƨ@�"�@��!@�=q@���@�&�@��9@�I�@��m@��w@�l�@�o@��y@��\@�M�@�=q@�=q@�$�@�{@�@���@��@��T@���@��^@�p�@���@��@�z�@�Q�@�Q�@�Q�@�I�@�9X@�1'@��;@�C�@�o@��@��R@��!@���@�~�@�~�@�~�@�v�@�E�@���@��-@�`B@��`@��/@���@�Ĝ@��@���@��@�j@�I�@� �@�;@l�@+@~��@~�y@~��@}��@}/@|�@|Z@{��@{C�@{o@z�@z��@z^5@z^5@z^5@z=q@zJ@y�^@y��@y�7@yx�@yhs@y�@xbN@w�w@wK�@v��@v$�@u@u��@up�@t�@tj@t1@sƨ@s�@sdZ@s33@so@s@r��@r�\@r=q@r-@q�@q�#@q��@qhs@p�`@pr�@pQ�@pQ�@pA�@p1'@p1'@p1'@p �@pb@o��@o+@n�@nȴ@nȴ@n�R@n�R@n�R@n��@n{@m�-@mV@l��@l��@lI�@k�@kdZ@k"�@k@j��@j�\@j=q@j-@jJ@i��@i&�@h�9@hr�@hb@g�@g+@f�+@e��@ep�@d��@d1@c�
@cƨ@c�F@c��@c�@cS�@c@b��@b��@b�\@b~�@bJ@aX@a&�@`�`@`1'@_�;@^�y@^�+@]�-@]V@\�j@\j@[ƨ@[�@[C�@["�@Z��@Y��@Y%@X��@XĜ@W�;@V��@VE�@U�-@UO�@T9X@Sƨ@St�@SC�@S@R��@R-@Q��@Qx�@PĜ@PQ�@Pb@O�;@O\)@N��@N�R@N{@M�T@MO�@M/@L�@K�m@J�@Jn�@J=q@J-@I��@I�^@HĜ@H �@G�@G\)@FE�@E?}@D�/@D�@D(�@C33@B�H@B�!@B=q@A�^@A�7@A&�@@��@@��@@�9@@��@@�@@bN@@1'@@  @@  @?�@?�w@?|�@?l�@?\)@?K�@>�@>V@>@=/@<�j@<�@;��@;dZ@;C�@;"�@;o@:~�@:-@9&�@8Ĝ@8Q�@7�;@7�w@7\)@7+@6��@6ȴ@6ȴ@6ȴ@6ȴ@6��@6ff@65?@6@5�@5��@5@5�@5`B@5O�@4��@4�@4z�@49X@3�
@3S�@3o@2��@2n�@2=q@2-@2J@1��@1��@17L@0��@0�@0 �@/�@/�;@/��@/��@/��@/l�@/�@.�y@.�@.��@.��@.��@.�+@.v�@.$�@-��@-�@-?}@-/@,�@,��@,z�@,I�@,(�@,(�@,�@,�@+�
@+dZ@*�H@*=q@)��@)��@)hs@)G�@)G�@)7L@(��@(�u@(Q�@( �@'�w@'\)@'�@&��@&��@&5?@%@%p�@%?}@%/@$�/@$�@#�
@#�F@#��@#�@#�@#t�@#S�@#33@"�@"��@"�\@"n�@"=q@!�@!��@!��@!hs@!X@!�@ ��@ �9@ �u@ �@  �@�P@+@�@�R@��@��@��@��@��@v�@$�@�T@O�@��@�j@�@�D@Z@9X@�m@t�@�@��@�\@-@�#@��@&�@%@%@�`@�9@�@�;@�w@�w@�@�P@l�@ȴ@ff@E�@E�@{@@�-@�-@�-@p�@�@Z@9X@��@��@t�@C�@��@��@�!@�\@=q@��@X@&�@%@��@�`@Ĝ@�9@�9@�@  @��@|�@l�@\)@K�@�@�@�R@��@v�@ff@{@p�@/@/@/@�@��@�@�@�@�/@�/@�j@�@�@�@��@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�}B	�}B	�qB	�}B	��B	��B	�B	�#B	�)B	�#B	�B	�#B	�#B	�B	�B	�/B	�BB	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�fB	�mB	�B	�B
%B
��B.B��B�?B��B�#B��B�B+BPB7LB<jBT�BN�BI�BF�B6FB(�B,B{B�B�B{BB��B�B�TB��BŢB�3B��B|�B33B-B�BB
��B
�B
��B
��B
k�B
XB
-B
+B
{B	��B	�yB	��B	�B	{�B	jB	VB	;dB	@�B	6FB	�B��B	�B	\B	)�B	-B	)�B	�B	�B	%�B	A�B	9XB	W
B	bNB	iyB	|�B	�B	�{B	��B	��B	�FB	��B	ƨB	��B	�)B	�B	��B	��B
+B
JB
bB
�B
�B
$�B
�B
49B
7LB
6FB
:^B
?}B
F�B
J�B
Q�B
R�B
T�B
Q�B
L�B
[#B
YB
\)B
YB
YB
cTB
gmB
gmB
ffB
e`B
ffB
gmB
iyB
iyB
ffB
bNB
]/B
aHB
aHB
aHB
dZB
cTB
]/B
[#B
aHB
cTB
cTB
`BB
aHB
aHB
_;B
_;B
`BB
^5B
aHB
_;B
\)B
^5B
_;B
[#B
^5B
_;B
bNB
]/B
^5B
]/B
aHB
aHB
`BB
bNB
bNB
cTB
dZB
e`B
e`B
dZB
e`B
e`B
dZB
cTB
ffB
hsB
hsB
ffB
aHB
_;B
_;B
\)B
[#B
VB
VB
T�B
VB
R�B
Q�B
T�B
VB
W
B
VB
T�B
S�B
Q�B
O�B
N�B
R�B
Q�B
P�B
Q�B
P�B
O�B
L�B
H�B
A�B
C�B
H�B
F�B
F�B
G�B
I�B
I�B
G�B
E�B
C�B
D�B
C�B
B�B
@�B
=qB
8RB
/B
5?B
=qB
=qB
<jB
<jB
;dB
8RB
8RB
9XB
7LB
49B
1'B
2-B
49B
5?B
33B
2-B
1'B
0!B
-B
,B
,B
,B
.B
.B
,B
'�B
(�B
(�B
)�B
+B
(�B
'�B
'�B
$�B
�B
�B
�B
�B
#�B
"�B
�B
�B
�B
�B
�B
{B
{B
hB
�B
�B
�B
{B
VB
hB
DB
1B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
bB
oB
VB
JB
\B
hB
\B
bB
uB
oB
hB
\B
\B
PB
1B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
!�B
�B
�B
�B
�B
�B
�B
�B
"�B
!�B
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
"�B
$�B
$�B
$�B
#�B
"�B
%�B
%�B
"�B
!�B
#�B
%�B
#�B
&�B
&�B
%�B
%�B
'�B
'�B
(�B
(�B
'�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
%�B
(�B
(�B
,B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
/B
0!B
0!B
.B
,B
+B
+B
.B
1'B
1'B
0!B
1'B
0!B
0!B
.B
-B
0!B
2-B
0!B
2-B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
5?B
33B
0!B
2-B
33B
5?B
5?B
7LB
5?B
49B
8RB
7LB
9XB
9XB
:^B
;dB
<jB
;dB
=qB
>wB
>wB
@�B
?}B
?}B
A�B
@�B
A�B
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
A�B
@�B
C�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
D�B
D�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
H�B
G�B
F�B
H�B
G�B
G�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
I�B
J�B
L�B
K�B
K�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
N�B
N�B
O�B
O�B
O�B
N�B
M�B
L�B
M�B
N�B
N�B
M�B
O�B
P�B
P�B
O�B
O�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
R�B
R�B
S�B
T�B
VB
VB
VB
VB
VB
T�B
T�B
S�B
T�B
T�B
VB
W
B
VB
VB
VB
T�B
S�B
S�B
S�B
VB
VB
VB
T�B
XB
XB
XB
XB
XB
XB
YB
XB
W
B
XB
XB
YB
XB
XB
XB
XB
XB
ZB
XB
ZB
\)B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
]/B
\)B
]/B
\)B
^5B
]/B
^5B
_;B
`BB
_;B
aHB
`BB
`BB
_;B
_;B
`BB
aHB
aHB
_;B
_;B
`BB
aHB
aHB
`BB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
bNB
dZB
e`B
e`B
e`B
e`B
e`B
dZB
ffB
e`B
ffB
e`B
cTB
e`B
ffB
hsB
iyB
hsB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
jB
jB
jB
iyB
k�B
l�B
k�B
k�B
l�B
l�B
m�B
n�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
q�B
q�B
q�B
q�B
o�B
p�B
o�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
t�B
u�B
u�B
t�B
s�B
t�B
s�B
s�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
u�B
v�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
y�B
y�B
x�B
x�B
x�B
z�B
y�B
z�B
{�B
{�B
{�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
z�B
z�B
{�B
|�B
}�B
}�B
|�B
|�B
~�B
� B
� B
� B
�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�%B
�%B
�%B
�+B
�1B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�1B
�=B
�DB
�=B
�=B
�=B
�7B
�=B
�DB
�JB
�DB
�DB
�JB
�JB
�JB
�DB
�=B
�PB
�PB
�PB
�PB
�VB
�VB
�PB
�\B
�\B
�\B
�VB
�PB
�\B
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�bB
�bB
�bB
�oB
�oB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�oB
�hB
�hB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�}B	�}B	�qB	�}B	��B	��B	�B	�#B	�)B	�#B	�B	�#B	�#B	�B	�B	�/B	�BB	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�fB	�mB	�B	�B
%B
��B.B��B�?B��B�#B��B�B+BPB7LB<jBT�BN�BI�BF�B6FB(�B,B{B�B�B{BB��B�B�TB��BŢB�3B��B|�B33B-B�BB
��B
�B
��B
��B
k�B
XB
-B
+B
{B	��B	�yB	��B	�B	{�B	jB	VB	;dB	@�B	6FB	�B��B	�B	\B	)�B	-B	)�B	�B	�B	%�B	A�B	9XB	W
B	bNB	iyB	|�B	�B	�{B	��B	��B	�FB	��B	ƨB	��B	�)B	�B	��B	��B
+B
JB
bB
�B
�B
$�B
�B
49B
7LB
6FB
:^B
?}B
F�B
J�B
Q�B
R�B
T�B
Q�B
L�B
[#B
YB
\)B
YB
YB
cTB
gmB
gmB
ffB
e`B
ffB
gmB
iyB
iyB
ffB
bNB
]/B
aHB
aHB
aHB
dZB
cTB
]/B
[#B
aHB
cTB
cTB
`BB
aHB
aHB
_;B
_;B
`BB
^5B
aHB
_;B
\)B
^5B
_;B
[#B
^5B
_;B
bNB
]/B
^5B
]/B
aHB
aHB
`BB
bNB
bNB
cTB
dZB
e`B
e`B
dZB
e`B
e`B
dZB
cTB
ffB
hsB
hsB
ffB
aHB
_;B
_;B
\)B
[#B
VB
VB
T�B
VB
R�B
Q�B
T�B
VB
W
B
VB
T�B
S�B
Q�B
O�B
N�B
R�B
Q�B
P�B
Q�B
P�B
O�B
L�B
H�B
A�B
C�B
H�B
F�B
F�B
G�B
I�B
I�B
G�B
E�B
C�B
D�B
C�B
B�B
@�B
=qB
8RB
/B
5?B
=qB
=qB
<jB
<jB
;dB
8RB
8RB
9XB
7LB
49B
1'B
2-B
49B
5?B
33B
2-B
1'B
0!B
-B
,B
,B
,B
.B
.B
,B
'�B
(�B
(�B
)�B
+B
(�B
'�B
'�B
$�B
�B
�B
�B
�B
#�B
"�B
�B
�B
�B
�B
�B
{B
{B
hB
�B
�B
�B
{B
VB
hB
DB
1B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
bB
oB
VB
JB
\B
hB
\B
bB
uB
oB
hB
\B
\B
PB
1B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
!�B
�B
�B
�B
�B
�B
�B
�B
"�B
!�B
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
"�B
$�B
$�B
$�B
#�B
"�B
%�B
%�B
"�B
!�B
#�B
%�B
#�B
&�B
&�B
%�B
%�B
'�B
'�B
(�B
(�B
'�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
%�B
(�B
(�B
,B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
/B
0!B
0!B
.B
,B
+B
+B
.B
1'B
1'B
0!B
1'B
0!B
0!B
.B
-B
0!B
2-B
0!B
2-B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
5?B
33B
0!B
2-B
33B
5?B
5?B
7LB
5?B
49B
8RB
7LB
9XB
9XB
:^B
;dB
<jB
;dB
=qB
>wB
>wB
@�B
?}B
?}B
A�B
@�B
A�B
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
A�B
@�B
C�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
D�B
D�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
H�B
G�B
F�B
H�B
G�B
G�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
I�B
J�B
L�B
K�B
K�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
N�B
N�B
O�B
O�B
O�B
N�B
M�B
L�B
M�B
N�B
N�B
M�B
O�B
P�B
P�B
O�B
O�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
R�B
R�B
S�B
T�B
VB
VB
VB
VB
VB
T�B
T�B
S�B
T�B
T�B
VB
W
B
VB
VB
VB
T�B
S�B
S�B
S�B
VB
VB
VB
T�B
XB
XB
XB
XB
XB
XB
YB
XB
W
B
XB
XB
YB
XB
XB
XB
XB
XB
ZB
XB
ZB
\)B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
]/B
\)B
]/B
\)B
^5B
]/B
^5B
_;B
`BB
_;B
aHB
`BB
`BB
_;B
_;B
`BB
aHB
aHB
_;B
_;B
`BB
aHB
aHB
`BB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
bNB
dZB
e`B
e`B
e`B
e`B
e`B
dZB
ffB
e`B
ffB
e`B
cTB
e`B
ffB
hsB
iyB
hsB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
jB
jB
jB
iyB
k�B
l�B
k�B
k�B
l�B
l�B
m�B
n�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
q�B
q�B
q�B
q�B
o�B
p�B
o�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
t�B
u�B
u�B
t�B
s�B
t�B
s�B
s�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
u�B
v�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
y�B
y�B
x�B
x�B
x�B
z�B
y�B
z�B
{�B
{�B
{�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
z�B
z�B
{�B
|�B
}�B
}�B
|�B
|�B
~�B
� B
� B
� B
�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�%B
�%B
�%B
�+B
�1B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�1B
�=B
�DB
�=B
�=B
�=B
�7B
�=B
�DB
�JB
�DB
�DB
�JB
�JB
�JB
�DB
�=B
�PB
�PB
�PB
�PB
�VB
�VB
�PB
�\B
�\B
�\B
�VB
�PB
�\B
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�bB
�bB
�bB
�oB
�oB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�oB
�hB
�hB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230220100138                              AO  ARCAADJP                                                                    20230220100138    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230220100138  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230220100138  QCF$                G�O�G�O�G�O�0               